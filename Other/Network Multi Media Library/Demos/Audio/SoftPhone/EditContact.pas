unit EditContact;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons;

type
  TfrmEditContact = class(TForm)
    edName: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    edURL: TEdit;
    bOk: TBitBtn;
    bCancel: TBitBtn;
    procedure edURLExit(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmEditContact: TfrmEditContact;

implementation

uses NMMCommon;

{$R *.dfm}

procedure TfrmEditContact.edURLExit(Sender: TObject);
var LServerHost: String;
    LServerPort: Integer;
begin
  if not ParseHostAndPortString(edURL.Text,LServerHost,LServerPort) then
    ShowMessage('Invalid IP address: "'+edURL.Text+'"');
end;

end.
