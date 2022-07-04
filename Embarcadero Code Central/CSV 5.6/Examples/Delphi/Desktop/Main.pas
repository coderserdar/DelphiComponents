unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, XPMan, StdCtrls, ExtCtrls, CSV;

type
  TFormMain = class(TForm)
    XPManifest: TXPManifest;
    Panel: TPanel;
    ButtonOpenFile: TButton;
    OpenDialog: TOpenDialog;
    Memo: TMemo;
    procedure ButtonOpenFileClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

procedure TFormMain.ButtonOpenFileClick(Sender: TObject);
begin
  if OpenDialog.Execute then
  begin
    Memo.Clear;
    with TCsv.Create do
    try
      LoadUtf8File(OpenDialog.FileName);
      Memo.Text := ToString;
    finally
      Free;
    end;
  end;
end;

end.
