unit SmartPtr;

{$MODE DELPHI}
{$MACRO ON}

interface

type
  TSmartPtr<T> = proxy
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

implementation

procedure TSmartPtr<T>.SmartFinalize();
begin
  if RefCount <> nil then
    if InterLockedDecrement(RefCount^)=0 then
    begin
      Dispose(RefCount);
      Dispose(Instance);
    end;
end;

class operator TSmartPtr<T>.Initialize(var aRec: TSmartPtr<T>);
begin
  aRec.RefCount := nil;
end;

class operator TSmartPtr<T>.Finalize(var aRec: TSmartPtr<T>);
begin
  aRec.SmartFinalize();
end;

class operator TSmartPtr<T>.Copy(var aRec: TSmartPtr<T>);
begin
  if aRec.RefCount <> nil then
    InterLockedIncrement(aRec.RefCount^);
end;

class operator TSmartPtr<T>.Clone(constref aSource: TSmartPtr<T>; var aDest: TSmartPtr<T>);
begin
  if aDest.RefCount <> nil then
    aDest.SmartFinalize();
  if aSource.RefCount <> nil then
    InterLockedIncrement(aSource.RefCount^);
  aDest.RefCount := aSource.RefCount;
  aDest.Instance := aSource.Instance;
end;

class operator TSmartPtr<T>.Implicit(aValue: T): TSmartPtr<T>;
begin
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

end.
