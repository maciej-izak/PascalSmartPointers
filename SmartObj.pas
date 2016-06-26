program SmartObj;

{$MODE DELPHI}

uses
  SysUtils, Classes;

type
  TMyObj = class(TStringList)
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
    Instance: T default; // default keyword for non property.
    RefCount: PLongint;

    procedure SmartFinalize();

    class operator Initialize(var aRec: TSmartObj<T>);
    class operator Finalize(var aRec: TSmartObj<T>);
    class operator Copy(var aRec: TSmartObj<T>);
    class operator Clone(constref aSource: TSmartObj<T>; var aDest: TSmartObj<T>);

    // implicit or explicit operator is used before "default" field
    class operator Implicit(aObj: T): TSmartObj<T>;
    procedure Assign(const aValue: T);
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

class operator TSmartObj<T>.Copy(var aRec: TSmartObj<T>);
begin
  WriteLn('AddRef');
  if aRec.RefCount <> nil then
    InterLockedIncrement(aRec.RefCount^);
end;

class operator TSmartObj<T>.Clone(constref aSource: TSmartObj<T>; var aDest: TSmartObj<T>);
begin
  WriteLn('Copy');
  if aDest.RefCount <> nil then
    aDest.SmartFinalize();
  if aSource.RefCount <> nil then
    InterLockedIncrement(aSource.RefCount^);
  aDest.RefCount := aSource.RefCount;
  aDest.Instance := aSource.Instance;
end;

class operator TSmartObj<T>.Implicit(aObj: T): TSmartObj<T>;
begin
  WriteLn('Implicit');
  Result.Assign(aObj);
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


procedure Foo;
var
  x: TSmartObj<TObject>;
begin
  x := TList.Create;
end;

var
  a, b: TSmartObj<TMyObj>;
  dynA, dynB: array of TSmartObj<TMyObj>;
  i: Integer;
begin
  Foo;
  WriteLn('BEGIN');   
 
  WriteLn('> a.Assign(TMyObj.Create) ');
  a := TMyObj.Create;
  a.Add('Foo');
  WriteLn('>>> ', a.ClassName);
  WriteLn('>>> ', a.UnitName);

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

{ OUTPUT :

Initialize
Initialize
Initialize
Initialize
Implicit
Finalize
 SmartFinalize
  Dispose :)
BEGIN
> a.Assign(TMyObj.Create)
TMyObj.Create
Implicit
Copy
>>> TMyObj
>>> SmartObj
> b := a
Copy
> SetLength(dynA, 5)
Initialize
Initialize
Initialize
Initialize
Initialize
> for i := 0 to High(dynA) do
> dynA[i] := b
Copy
Copy
Copy
Copy
Copy
> dynB := Copy(dynA)
AddRef
AddRef
AddRef
AddRef
AddRef
END.
Finalize
 SmartFinalize
Finalize
 SmartFinalize
Finalize
 SmartFinalize
Finalize
 SmartFinalize
Finalize
 SmartFinalize
Finalize
 SmartFinalize
Finalize
 SmartFinalize
Finalize
 SmartFinalize
Finalize
 SmartFinalize
Finalize
 SmartFinalize
Finalize
 SmartFinalize
Finalize
 SmartFinalize
Finalize
 SmartFinalize
TMyObj.Destroy
  Dispose :)
  
}
end.
