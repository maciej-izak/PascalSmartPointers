program SmartPtr;

{$MODE DELPHI}

type
  TSmartPtr<T> = record
    // similar as overloading [] operators for property x[v: string]: integer read gx write sx; default;
    Instance: T default; // default keyword for non property.
    RefCount: PLongint;

    procedure SmartFinalize();

    class operator Initialize(var aRec: TSmartPtr<T>);
    class operator Finalize(var aRec: TSmartPtr<T>);
    class operator Copy(var aRec: TSmartPtr<T>);
    class operator Clone(constref aSource: TSmartPtr<T>; var aDest: TSmartPtr<T>);

    // implicit or explicit operator should be used before "default" field
    class operator Implicit(aValue: T): TSmartPtr<T>;
    procedure Assign(const aValue: T); 
  end;

procedure TSmartPtr<T>.SmartFinalize();
begin
  WriteLn(' SmartFinalize');
  if RefCount <> nil then
    if InterLockedDecrement(RefCount^)=0 then
    begin
      Dispose(RefCount);
      Dispose(Instance);
      WriteLn('  Dispose :)');
    end;
end;

class operator TSmartPtr<T>.Initialize(var aRec: TSmartPtr<T>);
begin
  WriteLn('Initialize');
  aRec.RefCount := nil;
end;

class operator TSmartPtr<T>.Finalize(var aRec: TSmartPtr<T>);
begin
  WriteLn('Finalize');
  aRec.SmartFinalize();
end;

class operator TSmartPtr<T>.Copy(var aRec: TSmartPtr<T>);
begin
  WriteLn('AddRef');
  if aRec.RefCount <> nil then
    InterLockedIncrement(aRec.RefCount^);
end;

class operator TSmartPtr<T>.Clone(constref aSource: TSmartPtr<T>; var aDest: TSmartPtr<T>);
begin
  WriteLn('Copy');
  if aDest.RefCount <> nil then
    aDest.SmartFinalize();
  if aSource.RefCount <> nil then
    InterLockedIncrement(aSource.RefCount^);
  aDest.RefCount := aSource.RefCount;
  aDest.Instance := aSource.Instance;
end;

class operator TSmartPtr<T>.Implicit(aValue: T): TSmartPtr<T>;
begin
  WriteLn('Implicit');
  Result.Assign(aValue);
end;

procedure TSmartPtr<T>.Assign(const aValue: T);
begin
  if RefCount <> nil then
    SmartFinalize();

  New(RefCount);
  RefCount^ := 0;

  InterLockedIncrement(RefCount^);
  Instance := aValue;
end;

var
  a, b: TSmartPtr<PInteger>;
  dynA, dynB: array of TSmartPtr<PInteger>;
  i: Integer;
begin 
  WriteLn('BEGIN');   
 
  WriteLn('> a.Assign(New(PInteger)) ');   
  a.Assign(New(PInteger));
  
  WriteLn('> a.Instance^ := 5 ');   
  a.Instance^ := 10;
  WriteLn('a = ', a.Instance^);
  
  WriteLn('> b := a');
  b := a;
  WriteLn('b = ', b.Instance^);
  
  WriteLn('> SetLength(dynA, 5)');
  SetLength(dynA, 5);
  WriteLn('> for i := 0 to High(dynA) do');
  WriteLn('> dynA[i] := b');
  for i := 0 to High(dynA) do
  begin
    dynA[i] := b;
    WriteLn('dynA[', i, '] = ', dynA[i].Instance^);
  end;
    
  WriteLn('> dynB := Copy(dynA)');
  dynB := Copy(dynA);
  for i := 0 to High(dynB) do
    WriteLn('dynB[', i, '] = ', dynB[i].Instance^);
    
  WriteLn('> dynB[0].Instance^ := 5 ');   
  dynB[0].Instance^ := 5;  
  WriteLn('dynB[0] = ', dynB[0].Instance^);
  
  WriteLn('a = ', b.Instance^);
  WriteLn('b = ', b.Instance^);
  WriteLn('END.');   

{ OUTPUT :

Initialize
Initialize
BEGIN
> a.Assign(New(PInteger))
> a.Instance^ := 5
a = 10
> b := a
Copy
b = 10
> SetLength(dynA, 5)
Initialize
Initialize
Initialize
Initialize
Initialize
> for i := 0 to High(dynA) do
> dynA[i] := b
Copy
dynA[0] = 10
Copy
dynA[1] = 10
Copy
dynA[2] = 10
Copy
dynA[3] = 10
Copy
dynA[4] = 10
> dynB := Copy(dynA)
AddRef
AddRef
AddRef
AddRef
AddRef
dynB[0] = 10
dynB[1] = 10
dynB[2] = 10
dynB[3] = 10
dynB[4] = 10
> dynB[0].Instance^ := 5
dynB[0] = 5
a = 5
b = 5
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
  Dispose :)
}
end.
