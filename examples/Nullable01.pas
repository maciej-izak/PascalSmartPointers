program SmartPtr01;

{$MODE DELPHI}

uses
  heaptrc,
  SysUtils,
  Nullable in '..\sources\Nullable.pas'
  ;


var
  a: TNullable<string>;
  b: TNullable<Integer>;
  nns: TNullable<TNullable<string>>;
begin
  if nns = nil then
    WriteLn('nullable for nullable string works :)');
  
  try
    WriteLn(b.Value);
  except
    on E: EAccessViolation do
      WriteLn(b.HasValue); // print false
  end;
  
  b := 0;

  WriteLn(b.HasValue); // print true
  if b.HasValue then
  begin
    WriteLn(b.Value); // print 0
    WriteLn(b^); // print 0
  end; 
end.