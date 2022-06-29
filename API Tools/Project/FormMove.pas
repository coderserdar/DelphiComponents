unit FormMove;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Buttons, StdCtrls, ComCtrls;

type
  TfrFileMove = class(TForm)
    btnRename: TSpeedButton;
    btnExit: TSpeedButton;
    Edit1: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Edit2: TEdit;
    btnSrcBrowse: TSpeedButton;
    OpenDialog1: TOpenDialog;
    cbxOverwrite: TCheckBox;
    procedure btnExitClick(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure btnRenameClick(Sender: TObject);
    procedure btnSrcBrowseClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure Edit1Exit(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frFileMove: TfrFileMove;

implementation

uses FormMain;

{$R *.DFM}

procedure TfrFileMove.btnExitClick(Sender: TObject);
begin
	Close;
end;

procedure TfrFileMove.Edit1Change(Sender: TObject);
begin
	btnRename.Enabled:=
		(Edit1.Text <> '') and
		(Edit2.Text <> '');
end;

procedure TfrFileMove.btnRenameClick(Sender: TObject);
var
	Overwrite: DWORD;
begin
	if cbxOverwrite.Checked then
	Overwrite:= MOVEFILE_COPY_ALLOWED or MOVEFILE_REPLACE_EXISTING else
	Overwrite:= Ord(False);
	frMain.APITools1.FileMove(Edit1.Text,Edit2.Text,Overwrite);
end;

procedure TfrFileMove.btnSrcBrowseClick(Sender: TObject);
begin
	with OpenDialog1 do
	if Execute then
	Edit1.Text:= FileName;
end;

procedure TfrFileMove.FormShow(Sender: TObject);
begin
	Edit1.Clear;
	Edit2.Clear;
	{$ifdef WIN32}
	cbxOverwrite.Enabled:= TRUE;
	cbxOverwrite.Checked:= TRUE;
	{$else}
	cbxOverwrite.Enabled:= FALSE;
	cbxOverwrite.Checked:= FALSE;
	{$endif}
end;

procedure TfrFileMove.FormKeyPress(Sender: TObject; var Key: Char);
begin
	if Key = #27 then
	Close;
end;

procedure TfrFileMove.Edit1Exit(Sender: TObject);
begin
	Edit1.Text:= ExpandFileName(Edit1.Text);
end;

end.
