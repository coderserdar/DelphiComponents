unit Main;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, CSV;

type
  TFormMain = class(TForm)
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

{$R *.lfm}

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