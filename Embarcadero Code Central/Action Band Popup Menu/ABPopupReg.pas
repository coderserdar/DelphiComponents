unit ABPopupReg;

interface

uses ActnPopupCtrl;

procedure Register;

implementation

uses Classes, DsnConst;

procedure Register;
begin
  RegisterComponents(srAdditional, [TPopupActionBarEx]);
end;

end.
