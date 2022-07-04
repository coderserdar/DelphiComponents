unit fmSFXConfig;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, Buttons, ArchiverRoot, CustSFXGenerator, SFXGenerator,
  unTranslation;

type
  TSFXConfig = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    btnOk: TBitBtn;
    btnCancel: TBitBtn;
    cbExecFile: TCheckBox;
    cbSelectFilesToExtract: TCheckBox;
    cbSelectOverwriteMode: TCheckBox;
    cbOverwriteMode: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    cbLanguage: TComboBox;
    edCaption: TEdit;
    edCommandLine: TEdit;
    Bevel1: TBevel;
    btnHelp: TBitBtn;
    edExtractPath: TEdit;
    Label6: TLabel;
    edArgs: TEdit;
    Button1: TButton;
    cbDontRun: TCheckBox;
    procedure FormShow(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Déclarations privées }
    procedure WMTranslate( var Message : TMessage ); message WM_TRANSLATE;
  public
    { Déclarations publiques }
  end;

var
  SFXConfig: TSFXConfig;

implementation

uses fmHelpOnSFX, fmMain, fmSFXComments;

{$R *.DFM}

procedure TSFXConfig.FormShow(Sender: TObject);
begin
  with Main.SFXGenerator1 do
    begin
      cbExecFile.Checked := TagInfo.ExecuteFileAfterExtract;
      cbSelectFilesToExtract.Checked := TagInfo.UserChooseFilesToExtract;
      cbSelectOverwriteMode.Checked := TagInfo.UserChooseOverwriteMode;
      cbDontRun.Checked := TagInfo.UserAllowedToDontRunTheFile;
      cbOverwriteMode.ItemIndex := Integer(TagInfo.DefaultOwerwriteMode);
      edCommandLine.Text := TagInfo.CommandLine;
      edCaption.Text := TagInfo.Caption;
      edExtractPath.Text := TagInfo.DefaultExtractPath;
      cbLanguage.ItemIndex := Integer(TagInfo.Language);
    end;
end;

procedure TSFXConfig.btnOkClick(Sender: TObject);
begin
  with Main.SFXGenerator1 do
    begin
      TagInfo.ExecuteFileAfterExtract := cbExecFile.Checked;
      TagInfo.UserChooseFilesToExtract := cbSelectFilesToExtract.Checked;
      TagInfo.UserChooseOverwriteMode := cbSelectOverwriteMode.Checked;
      TagInfo.UserAllowedToDontRunTheFile := cbDontRun.Checked;
      TagInfo.DefaultOwerwriteMode := TOverwritemode(cbOverwriteMode.ItemIndex);
      TagInfo.CommandLine := edCommandLine.Text;
      if edArgs.Text <> '' then
        TagInfo.CommandLine := TagInfo.CommandLine + '|' + edArgs.Text;
      TagInfo.Caption := edCaption.Text;
      TagInfo.DefaultExtractPath := edExtractPath.Text;
      TagInfo.Language := TLanguage(cbLanguage.ItemIndex);
      if (Length(SFXComments.mmBefore.Text)>0) and
         (Length(SFXComments.mmAfter.Text)>0) then
        TagInfo.Comment := both
      else if Length(SFXComments.mmBefore.Text)>0 then
        TagInfo.Comment := before
      else if Length(SFXComments.mmAfter.Text)>0 then
        TagInfo.Comment := after
      else
        TagInfo.Comment := none;
    end;
end;

procedure TSFXConfig.btnHelpClick(Sender: TObject);
begin
  HelpOnSFX.ShowModal;
end;

procedure TSFXConfig.Button1Click(Sender: TObject);
begin
  SFXComments.ShowModal;
end;

procedure TSFXConfig.WMTranslate( var Message : TMessage );
begin
  cbLanguage.Items.Text := GetStr(1018);
  cbOverwriteMode.Items.Text := GetStr(1810);
end;

end.
