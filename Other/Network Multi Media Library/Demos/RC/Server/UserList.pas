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

end.
