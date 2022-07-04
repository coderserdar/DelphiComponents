unit FormRun;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
	Buttons, StdCtrls, ExtCtrls;

type
  TfrRun = class(TForm)
    edRun: TEdit;
    Label1: TLabel;
    btnSrcBrowse: TSpeedButton;
    btnRun: TSpeedButton;
    btnExit: TSpeedButton;
    OpenDialog1: TOpenDialog;
    rgAction: TRadioGroup;
    procedure btnExitClick(Sender: TObject);
    procedure btnSrcBrowseClick(Sender: TObject);
    procedure edRunChange(Sender: TObject);
    procedure btnRunClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure rgActionClick(Sender: TObject);
  private
		fAction: ShortString;
  public
		{ Public declarations }
	end;

var
	frRun: TfrRun;

implementation

uses FormMain, APITool;

{$R *.DFM}

procedure TfrRun.btnExitClick(Sender: TObject);
begin
	Close;
end;

procedure TfrRun.btnSrcBrowseClick(Sender: TObject);
begin
	with OpenDialog1 do
	if Execute then
	begin
		edRun.Text:= FileName;
		rgActionClick(Sender);
	end;
end;

procedure TfrRun.edRunChange(Sender: TObject);
begin
	btnRun.Enabled:= edRun.Text <> '';
end;

procedure TfrRun.btnRunClick(Sender: TObject);
begin
	with frMain.APITools1 do
	ExecuteFile(fAction,edRun.Text,'','',SW_SHOW)
end;

procedure TfrRun.FormShow(Sender: TObject);
begin
	rgAction.ItemIndex:= 0;
	rgActionClick(Sender);
	edRun.Clear;
	edRun.SetFocus;
end;

procedure TfrRun.FormKeyPress(Sender: TObject; var Key: Char);
begin
	if Key = #27 then
	Close;
end;

procedure TfrRun.rgActionClick(Sender: TObject);
begin
	with rgAction do
	fAction:= Items[ItemIndex];
	if fAction = 'Explore' then
	edRun.Text:= ExtractFilePath(edRun.Text);
end;

end.
