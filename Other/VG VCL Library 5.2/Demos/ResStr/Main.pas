unit Main;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, Forms, StdCtrls;

type
  TMainForm = class(TForm)
    cmLocal: TButton;
    cmEnglish: TButton;
    Label1: TLabel;
    procedure cmEnglishClick(Sender: TObject);
    procedure cmLocalClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation
uses ConstsRC, Dialogs;

{$R *.DFM}

procedure TMainForm.cmEnglishClick(Sender: TObject);
begin
  FreeResStringsConsts;
  MessageDlg('Message box has captions loaded from default resource', mtInformation, [mbYes, mbNo, mbCancel], 0);
end;

procedure TMainForm.cmLocalClick(Sender: TObject);
begin
  InitResStringsConsts;
  MessageDlg('Message box has captions loaded from local resource', mtInformation, [mbYes, mbNo, mbCancel], 0);
end;

end.
