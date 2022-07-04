unit UFormPluginsAdd;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TFormPluginsAdd = class(TForm)
    btnOk: TButton;
    btnCancel: TButton;
    boxPlugins: TGroupBox;
    chkSrcFolder: TRadioButton;
    chkSrcTC: TRadioButton;
    chkSrcFile: TRadioButton;
    edPath1: TEdit;
    labSource: TLabel;
    btnBrowse1: TButton;
    labSource2: TLabel;
    edPath2: TEdit;
    btnBrowse2: TButton;
    OpenDialog1: TOpenDialog;
    chkSrcZip: TRadioButton;
    procedure chkSrcFolderClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnBrowse2Click(Sender: TObject);
    procedure btnBrowse1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    FInstallFolder: string;
  end;

implementation

uses
  ATxMsg, ATxMsgProc,
  ATxTotalCmd, ATxUtils, ATxParamStr, ATxSProc;

{$R *.DFM}

procedure TFormPluginsAdd.chkSrcFolderClick(Sender: TObject);
var
  En: boolean;
begin
  if chkSrcTC.Checked then labSource.Caption:= MsgViewerPluginsLabelTCIni else
   if chkSrcZip.Checked then labSource.Caption:= MsgViewerPluginsLabelFile else
    if chkSrcFile.Checked then labSource.Caption:= MsgViewerPluginsLabelFile else
     if chkSrcFolder.Checked then labSource.Caption:= MsgViewerPluginsLabelFolder;

  if chkSrcTC.Checked then
    begin
    edPath1.Text:= TCDefIni;
    edPath2.Text:= TCDefExe;
    end
  else
    begin
    edPath1.Text:= '';
    edPath2.Text:= '';
    end;

  En:= chkSrcTC.Checked;
  labSource2.Visible:= En;
  edPath2.Visible:= En;
  btnBrowse2.Visible:= En;
end;

procedure TFormPluginsAdd.FormShow(Sender: TObject);
begin
  {$I Lang.FormPluginsAdd.inc}

  chkSrcZip.Checked:= true;
  chkSrcFolderClick(Self);

  FInstallFolder:= 'C:\';
end;

procedure TFormPluginsAdd.btnBrowse2Click(Sender: TObject);
begin
  with OpenDialog1 do
    begin
    FileName:= TCDefExe;
    InitialDir:= ExtractFileDir(TCDefExe);
    Filter:= MsgViewerPluginsFilterExe;
    DefaultExt:= 'exe';
    if Execute then
      edPath2.Text:= FileName;
    end;
end;

procedure TFormPluginsAdd.btnBrowse1Click(Sender: TObject);
begin
  if chkSrcZip.Checked then
    OpenDialog1.Options:= OpenDialog1.Options + [ofAllowMultiSelect]
  else
    OpenDialog1.Options:= OpenDialog1.Options - [ofAllowMultiSelect];

  if chkSrcTC.Checked then
    begin
    with OpenDialog1 do
      begin
      FileName:= TCDefIni;
      InitialDir:= ExtractFileDir(TCDefExe);
      Filter:= MsgViewerPluginsFilterIni;
      DefaultExt:= 'ini';
      if Execute then
        edPath1.Text:= FileName;
      end;
    end
  else
  if chkSrcFile.Checked then
    begin
    with OpenDialog1 do
      begin
      FileName:= '';
      InitialDir:= FInstallFolder;
      Filter:= MsgViewerPluginsFilterWLX;
      DefaultExt:= 'wlx';
      if Execute then
        begin
        edPath1.Text:= FileName;
        ModalResult:= mrOk;
        end;
      end;
    end
  else
  if chkSrcZip.Checked then
    begin
    with OpenDialog1 do
      begin
      FileName:= '';
      InitialDir:= FInstallFolder;
      Filter:= MsgViewerPluginsFilterZip;
      DefaultExt:= '';
      if Execute then
        begin
        edPath1.Text:= FileName;
        ModalResult:= mrOk;
        end;
      end;
    end;
end;

end.
