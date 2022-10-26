unit rxwizardreg;

interface

procedure Register;

implementation

uses
  rxresexp;

procedure Register;
begin
{$IFDEF RX_D3}
{ Project Resource Expert }
  RegisterResourceExpert;
{$ENDIF}
end;

end.
