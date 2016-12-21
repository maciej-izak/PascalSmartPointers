{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2014 by Maciej Izak aka hnb (NewPascal project)

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
 
 unit Nullable;

{$MODE DELPHI}

interface

uses
  SysUtils;

type
  TNullable<T: record> = record
  public type
    PT = ^T;
  strict private
    Instance: ^T default;
    function GetValue: T;
  public
    property Value: T read GetValue;

    function HasValue: Boolean; inline;
    function GetValueOrDefault: T;

    class operator Implicit(A: T): TNullable<T>;
    class operator Implicit(A: TNullable<T>): T;
    class operator Implicit(A: PT): TNullable<T>;

    class operator Equal(A: TNullable<T>; B: PT): Boolean;
    class operator Equal(A: TNullable<T>; B: T): Boolean;

    class operator Initialize(var A: TNullable<T>);
    class operator Finalize(var A: TNullable<T>);
    class operator Clone(constref Src: TNullable<T>; var Dest: TNullable<T>);
    class operator Copy(var Rec: TNullable<T>);
  end;

implementation

function TNullable<T>.GetValue: T;
begin
  Result := Instance^;
end;

function TNullable<T>.HasValue: Boolean;
begin
  Result := Instance <> nil;
end;

function TNullable<T>.GetValueOrDefault: T;
begin
  if Instance <> nil then
    Result := Instance^
  else
    Result := Default(T);
end;

class operator TNullable<T>.Equal(A: TNullable<T>; B: T): Boolean;
begin
  Result := Assigned(A.Instance);
  if Result then
    Result := A.Instance^ = B
end;

class operator TNullable<T>.Equal(A: TNullable<T>; B: PT): Boolean;
begin
  if Assigned(A.Instance) and Assigned(B) then
    Result := A.Instance^ = B^
  else
    Result := not Assigned(A.Instance) and not Assigned(B);
end;

class operator TNullable<T>.Implicit(A: T): TNullable<T>;
begin
  New(Result.Instance);
  Result.Instance^ := A;
end;

class operator TNullable<T>.Implicit(A: TNullable<T>): T;
begin
  Result := A.GetValueOrDefault;
end;

class operator TNullable<T>.Implicit(A: PT): TNullable<T>;
begin
  if Assigned(A) then
  begin
    New(Result.Instance);
    Result.Instance^ := PT((@A)^)^;
  end;
end;

class operator TNullable<T>.Initialize(var A: TNullable<T>);
begin
  A.Instance := nil;
end;

class operator TNullable<T>.Finalize(var A: TNullable<T>);
begin
  if Assigned(A.Instance) then
    Dispose(A.Instance);
end;

class operator TNullable<T>.Clone(constref Src: TNullable<T>; var Dest: TNullable<T>);
begin
  if Assigned(Dest.Instance) and Assigned(Src.Instance) then
    Dest.Instance^ := Src.Instance^
  else
  if Assigned(Dest.Instance) and not Assigned(Src.Instance) then
  begin
    Dispose(Dest.Instance);
    Dest.Instance := nil;
  end
  else
  if not Assigned(Dest.Instance) and Assigned(Src.Instance) then
  begin
    New(Dest.Instance);
    Dest.Instance^ := Src.Instance^;
  end;
end;

class operator TNullable<T>.Copy(var Rec: TNullable<T>);
var
  Tmp: T;
begin
  if Rec.Instance <> nil then
  begin
    Tmp := Rec.Instance^;
    New(Rec.Instance);
    Rec.Instance^ := Tmp;
  end;
end;

end.