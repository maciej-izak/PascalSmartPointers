program SmartObj01;

{$MODE DELPHI}

uses
  heaptrc,
  Classes,
  SmartObj in '..\sources\SmartObj.pas'
  ;

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
  inherited Destroy;
  WriteLn('TMyObj.Destroy');
end;

procedure Foo;
var
  x: TSmartObj<TObject>;
begin
  x := TMyObj.Create;
end;

var
  a, b: TSmartObj<TMyObj>;
  dynA, dynB: array of TSmartObj<TMyObj>;
  i: Integer;
begin
  Foo;
  WriteLn('... after foo ...');
 
  a := TMyObj.Create;
  a.Add('Foo');
  WriteLn('a.ClassName = ', a.ClassName);
  WriteLn('a.UnitName = ', a.UnitName);
  WriteLn('a[0] = ', a[0]);

  b := a;
  
  SetLength(dynA, 5);
  for i := 0 to High(dynA) do
    dynA[i] := b;
    
  dynB := Copy(dynA);   
end.