unit fmAddDropedFiles;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, FileCtrl, unTranslation;

type
  TAddDropedFiles = class(TForm)
    GroupBox1: TGroupBox;
    edName: TEdit;
    Label1: TLabel;
    lbFiles: TListBox;
    Label3: TLabel;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    GroupBox2: TGroupBox;
    Label2: TLabel;
    cbRecurse: TCheckBox;
    cbIncludeCurrent: TCheckBox;
    cbPathStorage: TComboBox;
    cbEmptyFolders: TCheckBox;
    cbComprLevel: TComboBox;
    cbCryptFiles: TCheckBox;
    cbAddToCurrentFolder: TCheckBox;
    cbFilterExtension: TCheckBox;
  private
    { Déclarations privées }
    procedure WMTranslate( var Message : TMessage ); message WM_TRANSLATE;
  public
    { Déclarations publiques }
  end;

var
  AddDropedFiles: TAddDropedFiles;

implementation

{$R *.DFM}

procedure TAddDropedFiles.WMTranslate( var Message : TMessage );
begin
  cbPathStorage.Items.Text := GetStr(910);
  cbComprLevel.Items.Text := GetStr(911);
end;

end.
