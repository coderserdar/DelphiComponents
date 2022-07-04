unit fmExtract;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Buttons, FileCtrl, ArchiverMisc, unTranslation;

type
  TExtract = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    GroupBox1: TGroupBox;
    DriveComboBox1: TDriveComboBox;
    DirectoryListBox1: TDirectoryListBox;
    Label1: TLabel;
    edExtractPath: TEdit;
    SpeedButton1: TSpeedButton;
    rgFiles: TRadioGroup;
    edFiles: TEdit;
    cbRestorePath: TCheckBox;
    BitBtn3: TBitBtn;
    cbRestore: TComboBox;
    procedure DirectoryListBox1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure edFilesChange(Sender: TObject);
  private
    { Déclarations privées }
    procedure WMTranslate( var Message : TMessage ); message WM_TRANSLATE;
  public
    { Déclarations publiques }
  end;

var
  Extract: TExtract;

implementation
uses fmCreateFolder, Archiver;
{$R *.DFM}

procedure TExtract.DirectoryListBox1Change(Sender: TObject);
begin
  edExtractPath.Text := DirectoryListBox1.Directory;
end;

procedure TExtract.FormCreate(Sender: TObject);
begin
  edExtractPath.Text := DirectoryListBox1.Directory;
end;

procedure TExtract.SpeedButton1Click(Sender: TObject);
begin
  DirectoryListBox1.Directory := edExtractPath.Text;
end;

procedure TExtract.BitBtn3Click(Sender: TObject);
var
  tmp : String;
begin
  CreateFolder.lPath.Caption := DirectoryListBox1.Directory;
  if CreateFolder.ShowModal = mrOk then
    begin
      tmp := AppendSlash(DirectoryListBox1.Directory) + CreateFolder.edPath.Text;
      if CreateDir( tmp ) then
        DirectoryListBox1.Directory := tmp;
    end;
end;

procedure TExtract.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if (ModalResult = mrOk) and (rgFiles.ItemIndex = 2) then
    begin
      CanClose := edFiles.Text <> '';
      if not CanClose then
        begin
          MessageDlg( 'You must enter a filter !', mtWarning, [mbOk], 0 );
          edFiles.SetFocus;
        end;
    end;
end;

procedure TExtract.edFilesChange(Sender: TObject);
begin
  rgFiles.ItemIndex := 2;
end;

procedure TExtract.WMTranslate( var Message : TMessage );
begin
  rgFiles.Items.Text := GetStr(1403);
  cbRestore.Items.Text := GetStr(1405);
end;

end.
