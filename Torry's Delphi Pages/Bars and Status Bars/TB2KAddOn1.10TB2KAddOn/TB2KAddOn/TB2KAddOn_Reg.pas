unit TB2KAddOn_Reg;

interface

uses
  Classes, TB2Reg, TB2KAddOnAutoVisibility, TB2KAddOnCustomizeForm,
    TB2KAddOnAutoState, TB2DsgnItemEditor, DesignIntf;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Toolbar2000', [TTB2KToolbarList, TTB2KCustomizeDialog,
    TTB2KAutoStateSave]);
  TBRegisterClasses([TTBAVListItem, TTBCustomizeDialogItem]);
  TBRegisterItemClass(TTBAVListItem, 'New AutoVisibility List Item', HInstance);
  TBRegisterItemClass(TTBCustomizeDialogItem, 'New CustomizeDialog Item',
    HInstance);
end;

end.
 
