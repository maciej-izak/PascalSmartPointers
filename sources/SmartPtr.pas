{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2014 by Maciej Izak aka hnb (NewPascal project)

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
 
 unit SmartPtr;

{$MODE DELPHI}

interface

type
  TSmartPtr<T> = record
    // similar as overloading [] operators for property x[v: string]: integer read gx write sx; default;
    Instance: T default; // default keyword for non property.
    RefCount: PLongint;

    procedure SmartFinalize();

    class operator Initialize(var aRec: TSmartPtr<T>);
    class operator Finalize(var aRec: TSmartPtr<T>);
    class operator AddRef(var aRec: TSmartPtr<T>);
    class operator Copy(constref aSource: TSmartPtr<T>; var aDest: TSmartPtr<T>);

    // implicit or explicit operator should be used before "default" field
    class operator Implicit(aValue: T): TSmartPtr<T>;
    procedure Assign(const aValue: T); 
  end;

implementation

{ TSmartPtr }

procedure TSmartPtr<T>.SmartFinalize();
begin
  if RefCount <> nil then
    if InterLockedDecrement(RefCount^)=0 then
    begin
      FreeMem(RefCount);
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

class operator TSmartPtr<T>.AddRef(var aRec: TSmartPtr<T>);
begin
  if aRec.RefCount <> nil then
    InterLockedIncrement(aRec.RefCount^);
end;

class operator TSmartPtr<T>.Copy(constref aSource: TSmartPtr<T>; var aDest: TSmartPtr<T>);
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

  GetMem(RefCount, SizeOf(Longint));
  RefCount^ := 0;

  InterLockedIncrement(RefCount^);
  Instance := aValue;
end;

end.
