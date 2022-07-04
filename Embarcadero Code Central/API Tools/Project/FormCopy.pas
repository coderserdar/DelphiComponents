unit FormCopy;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Buttons, StdCtrls, ComCtrls;

type
  TfrFileCopy = class(TForm)
    btnCopy: TSpeedButton;
    btnExit: TSpeedButton;
    Edit1: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Edit2: TEdit;
    btnSrcBrowse: TSpeedButton;
    OpenDialog1: TOpenDialog;
    cbxOver: TCheckBox;
    procedure btnExitClick(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure btnCopyClick(Sender: TObject);
    procedure btnSrcBrowseClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure Edit2Enter(Sender: TObject);
    procedure Edit1Enter(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frFileCopy: TfrFileCopy;

implementation

uses FormMain;

{$R *.DFM}

procedure TfrFileCopy.btnExitClick(Sender: TObject);
begin
	Close;
end;

procedure TfrFileCopy.Edit1Change(Sender: TObject);
begin
	btnCopy.Enabled:=
		(Edit1.Text <> '') and
		(Edit2.Text <> '');
end;

procedure TfrFileCopy.btnCopyClick(Sender: TObject);
var
	Overwrite: BOOL;
begin
	if cbxOver.Checked then
	Overwrite:= False else
	Overwrite:= True;
	frMain.APITools1.FileCopy(ExpandFileName(Edit1.Text),ExpandFileName(Edit2.Text),Overwrite);
end;

procedure TfrFileCopy.btnSrcBrowseClick(Sender: TObject);
begin
	with OpenDialog1 do
	if Execute then
	begin
		Edit1.SetFocus;
		Edit1.Text:= FileName;
	end;
end;

procedure TfrFileCopy.FormShow(Sender: TObject);
begin
	Edit1.Clear;
	Edit2.Clear;
	cbxOver.Checked:= True;
end;

procedure TfrFileCopy.FormKeyPress(Sender: TObject; var Key: Char);
begin
	if Key = #27 then
	Close;
end;

procedure TfrFileCopy.Edit2Enter(Sender: TObject);
begin
	if Edit1.Text <> '' then
	Edit2.Text:= '\'+ExtractFileName(Edit1.Text);
end;

procedure TfrFileCopy.Edit1Enter(Sender: TObject);
begin
	Edit2.Text:= '';
end;

end.
