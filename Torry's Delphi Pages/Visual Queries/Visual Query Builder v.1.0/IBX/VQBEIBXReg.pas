{$I VQBDEF.inc}

unit VQBEIBXReg;

interface

uses
  {Windows, Messages, SysUtils,} Classes, DBEIBX;

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
  RegisterComponents('VQBuilder', [TDBEngineIBX]);
end;

end.
 