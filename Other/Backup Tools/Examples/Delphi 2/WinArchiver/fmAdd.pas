unit fmAdd;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, FileCtrl, unTranslation;

type
  TAdd = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    GroupBox1: TGroupBox;
    DriveComboBox1: TDriveComboBox;
    DirectoryListBox1: TDirectoryListBox;
    FileListBox1: TFileListBox;
    edName: TEdit;
    Label1: TLabel;
    GroupBox2: TGroupBox;
    cbRecurse: TCheckBox;
    cbIncludeCurrent: TCheckBox;
    cbPathStorage: TComboBox;
    Label2: TLabel;
    cbEmptyFolders: TCheckBox;
    Label3: TLabel;
    cbComprLevel: TComboBox;
    cbCryptFiles: TCheckBox;
    cbAddToCurrentFolder: TCheckBox;
    procedure FileListBox1DblClick(Sender: TObject);
  private
    { Déclarations privées }
    procedure WMTranslate( var Message : TMessage ); message WM_TRANSLATE;
  public
    { Déclarations publiques }
  end;

var
  Add: TAdd;

implementation

{$R *.DFM}

procedure TAdd.FileListBox1DblClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TAdd.WMTranslate( var Message : TMessage );
begin
  cbPathStorage.Items.Text := GetStr(910);
  cbComprLevel.Items.Text := GetStr(911);
end;

end.
