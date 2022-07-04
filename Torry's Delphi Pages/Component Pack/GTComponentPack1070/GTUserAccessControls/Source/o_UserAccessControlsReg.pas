unit o_UserAccessControlsReg;

interface
uses
   Classes
  ,LoginDialog
  ;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('GT User Access Controls',[TgtLoginDialog]);
end;



end.
