unit StdActnCtrlsEx;

interface

uses ActnList, StdActnMenus;

type
  TStandardPopupMenuEx = class(TStandardMenuPopup)
  protected
    procedure ExecAction(Action: TContainedAction); override;
  end;

implementation

uses Menus, Windows, Messages;

{ TStandardPopupMenuEx }

procedure TStandardPopupMenuEx.ExecAction(Action: TContainedAction);
begin
  PostMessage(PopupList.Window, WM_COMMAND, TMenuItem(FSelectedItem.Tag).Command, 0);
end;

end.
