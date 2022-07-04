unit GTMenuRelatedControlsReg;

interface
uses
   Classes
  ,o_MenuTreeView
  ,o_GTMenuStyler
  ;

procedure Register;

implementation


procedure Register;
begin
  RegisterComponents('GT Menu Related Controls',
                                               [
                                                TgtMenuTreeView
                                               ,TgtMenuStyler
                                               ]);
end;
end.
