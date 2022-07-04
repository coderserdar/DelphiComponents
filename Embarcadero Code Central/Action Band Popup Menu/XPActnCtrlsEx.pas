unit XPActnCtrlsEx;

interface

uses ActnList, XPActnCtrls;

type
  TXPStylePopupMenuEx = class(TXPStylePopupMenu)
  protected
    procedure ExecAction(Action: TContainedAction); override;
  end;

implementation

uses Menus, Windows, Messages;

{ TXPStylePopupMenuEx }

procedure TXPStylePopupMenuEx.ExecAction(Action: TContainedAction);
begin
  PostMessage(PopupList.Window, WM_COMMAND, TMenuItem(FSelectedItem.Tag).Command, 0);
end;

end.
 