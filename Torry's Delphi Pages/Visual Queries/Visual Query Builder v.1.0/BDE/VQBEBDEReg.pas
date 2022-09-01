{$I VQBDEF.inc}

unit VQBEBDEReg;

interface

uses
  Classes, DBEBDE;

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
  RegisterComponents('VQBuilder', [TDBEngineBDE]);
end;

end.
 