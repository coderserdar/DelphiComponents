{$I VQBDEF.INC}
unit VQBReg;

interface

uses
  {Windows, Messages, SysUtils,} Classes, VQBuilder;


procedure Register;

implementation
uses
{$IFNDEF Delphi6}
  DsgnIntf
{$ELSE}
  DesignIntf, DesignEditors
{$ENDIF};

procedure Register;
begin
  RegisterComponents('VQBuilder', [TSQLDialog]);
end;

end.
 
