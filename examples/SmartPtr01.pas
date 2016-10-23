program SmartPtr01;

{$MODE DELPHI}

uses
  heaptrc,
  SmartPtr in '..\sources\SmartPtr.pas'
  ;


var
  ps: TSmartPtr<PString>;
  pi: TSmartPtr<PInteger>;
  dynA, dynB: array of TSmartPtr<PInteger>;
  i: Integer;
begin
  ps := New(PString);  
  pi := New(PInteger);

  ps^ := 'hello';
  WriteLn(ps^);

  pi^ := 10;
  WriteLn(pi^);

  SetLength(dynA, 5);
  for i := 0 to High(dynA) do
  begin
    Inc(pi^);
    dynA[i] := pi;
    WriteLn('dynA[', i, '] = ', dynA[i]^);
  end;
    
  dynB := Copy(dynA);
  for i := 0 to High(dynB) do
    WriteLn('dynB[', i, '] = ', dynB[i].Instance^);  
end.