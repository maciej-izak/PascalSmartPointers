program SmartPtr;

{$MODE DELPHI}

type
  TMyObj = class
  public
    constructor Create;
    destructor Destroy; override;
  end;
  
constructor TMyObj.Create;
begin
  WriteLn('TMyObj.Create');
end;

destructor TMyObj.Destroy;
begin
  WriteLn('TMyObj.Destroy');
end;
  
type
  TSmartObj<T: TObject> = record
    // similar as overloading [] operators for property x[v: string]: integer read gx write sx; default;
    Instance: T; //default; // default keyword for non property, can be used only for field of pointer type.
    RefCount: PLongint;

    procedure SmartFinalize();

    class operator Initialize(var aRec: TSmartObj<T>);
    class operator Finalize(var aRec: TSmartObj<T>);
    class operator AddRef(var aRec: TSmartObj<T>);
    class operator Copy(constref aSource: TSmartObj<T>; var aDest: TSmartObj<T>);

    // implicit or explicit operator should be used before "default" field
    procedure Assign(const aValue: T); // special version of Implicit/Explicit is also needed (available only when is used default for field)
    //operator Explicit: TRawSmartPtr;
  end;

procedure TSmartObj<T>.SmartFinalize();
begin
  WriteLn(' SmartFinalize');
  if RefCount <> nil then
    if InterLockedDecrement(RefCount^)=0 then
    begin
      Dispose(RefCount);
      Instance.Free;
      WriteLn('  Dispose :)');
    end;
end;

class operator TSmartObj<T>.Initialize(var aRec: TSmartObj<T>);
begin
  WriteLn('Initialize');
  aRec.RefCount := nil;
end;

class operator TSmartObj<T>.Finalize(var aRec: TSmartObj<T>);
begin
  WriteLn('Finalize');
  aRec.SmartFinalize();
end;

class operator TSmartObj<T>.AddRef(var aRec: TSmartObj<T>);
begin
  WriteLn('AddRef');
  if aRec.RefCount <> nil then
    InterLockedIncrement(aRec.RefCount^);
end;

class operator TSmartObj<T>.Copy(constref aSource: TSmartObj<T>; var aDest: TSmartObj<T>);
begin
  WriteLn('Copy');
  if aDest.RefCount <> nil then
    aDest.SmartFinalize();
  if aSource.RefCount <> nil then
    InterLockedIncrement(aSource.RefCount^);
  aDest.RefCount := aSource.RefCount;
  aDest.Instance := aSource.Instance;
end;

procedure TSmartObj<T>.Assign(const aValue: T);
begin
  if RefCount <> nil then
    SmartFinalize();

  New(RefCount);
  RefCount^ := 0;

  InterLockedIncrement(RefCount^);
  Instance := aValue;
end;

var
  a, b: TSmartObj<TMyObj>;
  dynA, dynB: array of TSmartObj<TMyObj>;
  i: Integer;
begin 
  WriteLn('BEGIN');   
 
  WriteLn('> a.Assign(New(PInteger)) ');   
  a.Assign(TMyObj.Create);
  
  WriteLn('> b := a');
  b := a;
  
  WriteLn('> SetLength(dynA, 5)');
  SetLength(dynA, 5);
  WriteLn('> for i := 0 to High(dynA) do');
  WriteLn('> dynA[i] := b');
  for i := 0 to High(dynA) do
    dynA[i] := b;
    
  WriteLn('> dynB := Copy(dynA)');
  dynB := Copy(dynA);
      
  WriteLn('END.');   
end.
