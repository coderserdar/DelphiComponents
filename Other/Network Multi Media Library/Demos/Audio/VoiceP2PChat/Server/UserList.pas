unit UserList;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TfrmUsers = class(TForm)
    Panel1: TPanel;
    lbUsers: TListBox;
    btnSend: TButton;
    edMessage: TEdit;
    Label1: TLabel;
    procedure btnSendClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmUsers: TfrmUsers;

implementation

uses Main;

{$R *.dfm}

procedure TfrmUsers.btnSendClick(Sender: TObject);
begin
 if lbUsers.ItemIndex > -1 then
 begin
   if not frmMain.Server.SendTextMessageToUser(
                             lbUsers.Items.Objects[lbUsers.ItemIndex],
                             edMessage.Text) then
   begin
     ShowMessage('Cannot send the message to user.'+char(VK_RETURN)+
                 'Refresh the user list');
   end;
 end
 else
 begin
   ShowMessage('No user selected');
 end;
end;

end.
