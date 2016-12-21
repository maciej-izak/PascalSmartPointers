{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2014 by Maciej Izak aka hnb (NewPascal project)

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
 
 unit SmartObj;

{$MODE DELPHI}

interface

uses
  SysUtils, Classes;

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

implementation

procedure TSmartObj<T>.SmartFinalize();
begin
  if RefCount <> nil then
    if InterLockedDecrement(RefCount^)=0 then
    begin
      FreeMem(RefCount);
      Instance.Free;
    end;
end;

class operator TSmartObj<T>.Initialize(var aRec: TSmartObj<T>);
begin
  aRec.RefCount := nil;
end;

class operator TSmartObj<T>.Finalize(var aRec: TSmartObj<T>);
begin
  aRec.SmartFinalize();
end;

class operator TSmartObj<T>.Copy(var aRec: TSmartObj<T>);
begin
  if aRec.RefCount <> nil then
    InterLockedIncrement(aRec.RefCount^);
end;

class operator TSmartObj<T>.Clone(constref aSource: TSmartObj<T>; var aDest: TSmartObj<T>);
begin
  if aDest.RefCount <> nil then
    aDest.SmartFinalize();
  if aSource.RefCount <> nil then
    InterLockedIncrement(aSource.RefCount^);
  aDest.RefCount := aSource.RefCount;
  aDest.Instance := aSource.Instance;
end;

class operator TSmartObj<T>.Implicit(aObj: T): TSmartObj<T>;
begin
  Result.Assign(aObj);
end;

procedure TSmartObj<T>.Assign(const aValue: T);
begin
  if RefCount <> nil then
    SmartFinalize();

  GetMem(RefCount, SizeOf(LongInt));
  RefCount^ := 0;

  InterLockedIncrement(RefCount^);
  Instance := aValue;
end;

end.
