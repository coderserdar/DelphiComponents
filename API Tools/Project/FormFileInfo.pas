unit FormFileInfo;

interface

uses
	Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
	StdCtrls, Buttons, ExtCtrls, Menus;

type
	TfrFileInfo = class(TForm)
		GroupBox1: TGroupBox;
    gbAttributes: TGroupBox;
		cbxArchive: TCheckBox;
		OpenDialog1: TOpenDialog;
		cbxComp: TCheckBox;
		cbxDirectory: TCheckBox;
    cbxHidden: TCheckBox;
    cbxNormal: TCheckBox;
		cbxOffLine: TCheckBox;
		cbxReadOnly: TCheckBox;
		cbxSystem: TCheckBox;
		cbxTemp: TCheckBox;
		Panel1: TPanel;
		lbFSize: TLabel;
		lbCreated: TLabel;
		lbAccessed: TLabel;
		lbModified: TLabel;
		Label1: TLabel;
		edLocation: TEdit;
    lbSizeOnDisk: TLabel;
    lbShortName: TLabel;
    edShortPath: TEdit;
    Label2: TLabel;
		edFName: TEdit;
    btnBrowse: TSpeedButton;
    pmFileInfo: TPopupMenu;
    SetFileTime: TMenuItem;
    SetFileDate: TMenuItem;
    sfdCreated: TMenuItem;
    sfdModified: TMenuItem;
    sfdAccessed: TMenuItem;
    sftCreated: TMenuItem;
		sftModified: TMenuItem;
		sftAccessed: TMenuItem;
		procedure edFNameExit(Sender: TObject);
		procedure btnBrowseClick(Sender: TObject);
		procedure FormShow(Sender: TObject);
		procedure edFNameKeyPress(Sender: TObject; var Key: Char);
		procedure pmFileInfoPopup(Sender: TObject);
	private
		{ Private declarations }
	public
		function GetCompressedFSize(const FileName: string): DWORD;
	end;

var
	frFileInfo: TfrFileInfo;

implementation

uses FormMain, mcConst;

{$R *.DFM}

function TfrFileInfo.GetCompressedFSize(const FileName: string): DWORD;
var
	fSize,fHighSize: DWORD;
begin
	fSize:= GetCompressedFileSize(PChar(FileName),@fHighSize);
	if (fSize = $FFFFFFFF) and (GetLastError <> NO_ERROR) then	// Size > 4 GB?
	result:= HiWord(fHighSize) else
	result:= fSize;
end;

procedure TfrFileInfo.edFNameExit(Sender: TObject);
begin
	with frMain.APITools1 do
	begin
		lbShortName.Caption:= ' Short name	'+ShortPathName(edFName.Text);
		edShortPath.Text:= ShortPathName(edLocation.Text);
		lbFSize.Caption:= SizeStr(' File size	',GetFSize(edFName.Text), ssType2);
		lbSizeOnDisk.Caption:= SizeStr(' Size on disk	',GetCompressedFSize(edFName.Text),ssType2);
		lbCreated.Caption:= ' Created	'+GetFileTimes(edFName.Text,ftiCreated);
		lbModified.Caption:= ' Modified	'+GetFileTimes(edFName.Text,ftiModified);
		lbAccessed.Caption:= ' Accessed	'+GetFileTimes(edFName.Text,ftiAccessed);

		cbxArchive.Checked:= FileAttribute(edFName.Text,fatArchive);
		cbxComp.Checked:= FileAttribute(edFName.Text,fatCompressed);
		cbxDirectory.Checked:= FileAttribute(edFName.Text,fatDirectory);
		cbxHidden.Checked:= FileAttribute(edFName.Text,fatHidden);
		cbxNormal.Checked:= FileAttribute(edFName.Text,fatNormal);
		cbxOffLine.Checked:= FileAttribute(edFName.Text,fatOffLine);
		cbxReadOnly.Checked:= FileAttribute(edFName.Text,fatReadOnly);
		cbxSystem.Checked:= FileAttribute(edFName.Text,fatSystem);
		cbxTemp.Checked:= FileAttribute(edFName.Text,fatTemporary);
	end;
end;

procedure TfrFileInfo.btnBrowseClick(Sender: TObject);
begin
	edFName.SetFocus;
	with OpenDialog1 do
	if Execute then
	begin
		edFName.Text:= ExtractFileName(FileName);
		edLocation.Text:= ExtractFilePath(FileName);
		edFNameExit(Sender);
	end;
end;

procedure TfrFileInfo.FormShow(Sender: TObject);
begin
	//cbxOffLine.Enabled:= not frMain.APITools1.IsWin95;
	//cbxComp.Enabled:= cbxOffLine.Enabled;
end;

procedure TfrFileInfo.edFNameKeyPress(Sender: TObject; var Key: Char);
begin
	if Key = #27 then
	Close;
end;

procedure TfrFileInfo.pmFileInfoPopup(Sender: TObject);
var
	bFile: boolean;
begin
	bFile:=
		(edFName.Text <> '') and
		(not cbxSystem.Checked) and
		(not cbxHidden.Checked) and
		(not cbxReadOnly.Checked);
	sfdCreated.Enabled:= bFile;
	sfdModified.Enabled:= bFile;
	sfdAccessed.Enabled:= bFile;
	sftCreated.Enabled:= bFile;
	sftModified.Enabled:= bFile;
	sftAccessed.Enabled:= bFile;
end;

end.
