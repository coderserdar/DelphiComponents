unit Main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, vgCtrls, ComCtrls, ExplCtrl, Explorer, vgStndrt, 
  fFrame;

type
  TMainForm = class(TForm)
    GroupBox1: TGroupBox;
    vgLabel1: TvgLabel;
    Label1: TLabel;
    edName: TEdit;
    Label2: TLabel;
    Label3: TLabel;
    cbRegRoot: TComboBox;
    cmLoad: TButton;
    cmSave: TButton;
    cbIniFileType: TComboBox;
    psForm: TPropStorage;
    ExplorerSource1: TExplorerSource;
    GroupBox2: TGroupBox;
    Label4: TLabel;
    cmLoadForm: TButton;
    cmSaveForm: TButton;
    cmLoadData: TButton;
    cmSaveData: TButton;
    etv: TExplorerTreeView;
    cbFolder: TComboBox;
    GroupBox3: TGroupBox;
    fr: TStoredFrame;
    procedure cmLoadClick(Sender: TObject);
    procedure cmSaveClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure cmLoadFormClick(Sender: TObject);
    procedure cmSaveFormClick(Sender: TObject);
    procedure cmLoadDataClick(Sender: TObject);
    procedure cmSaveDataClick(Sender: TObject);
    procedure cbFolderChange(Sender: TObject);
  private
    { Private declarations }
    procedure SetIniFile;
    procedure UpdateControls;
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

uses dData, ExplShl;

{$R *.DFM}

procedure TMainForm.SetIniFile;
begin
  with dm.afIniFile do
  begin
    IniFileName := edName.Text;
    IniFileType := TIniFileType(cbIniFileType.ItemIndex);
    RegistryRoot := TRegistryRoot(cbRegRoot.ItemIndex);
  end;
end;

procedure TMainForm.UpdateControls;
begin
  edName.Text := dm.afIniFile.IniFileName;
  cbIniFileType.ItemIndex := Integer(dm.afIniFile.IniFileType);
  cbRegRoot.ItemIndex := Integer(dm.afIniFile.RegistryRoot);
  cbFolder.ItemIndex := Integer(dm.enShell.Folder);
end;

procedure TMainForm.cmLoadClick(Sender: TObject);
begin
  SetIniFile;
  dm.afIniFile.Load;
  UpdateControls;
end;

procedure TMainForm.cmSaveClick(Sender: TObject);
begin
  SetIniFile;
  dm.afIniFile.Save;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  UpdateControls;
end;

procedure TMainForm.cmLoadFormClick(Sender: TObject);
begin
  psForm.Load;
  UpdateControls;
end;

procedure TMainForm.cmSaveFormClick(Sender: TObject);
begin
  psForm.Save;
end;

procedure TMainForm.cmLoadDataClick(Sender: TObject);
begin
  dm.psData.Load;
  UpdateControls;
end;

procedure TMainForm.cmSaveDataClick(Sender: TObject);
begin
  dm.psData.Save;
end;

procedure TMainForm.cbFolderChange(Sender: TObject);
begin
  dm.enShell.Folder := TShellFolder(cbFolder.ItemIndex);
  dm.enShell.Expand;
end;

end.
