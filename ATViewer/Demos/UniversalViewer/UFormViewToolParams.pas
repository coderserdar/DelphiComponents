unit UFormViewToolParams;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls, TntStdCtrls, TntDialogs;

type
  TFormViewToolParams = class(TForm)
    GroupBox1: TGroupBox;
    btnOk: TButton;
    btnCancel: TButton;
    edCommand: TTntEdit;
    labCommand: TLabel;
    btnCommandBrowse: TButton;
    edParams: TTntEdit;
    labParams: TLabel;
    labMacros: TLabel;
    labCaption: TLabel;
    edCaption: TEdit;
    chkSelectAll: TCheckBox;
    chkCopy: TCheckBox;
    labActions: TLabel;
    OpenDialog1: TTntOpenDialog;
    labActions2: TLabel;
    chkExit: TCheckBox;
    labActions1: TLabel;
    labParamsHint: TLabel;
    listMacros: TListBox;
    procedure btnCommandBrowseClick(Sender: TObject);
    procedure listMacros33DblClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

function FGetFileDescription(const FileName: WideString): WideString;


implementation

uses
  ATxMsg, ATxMsgProc, ATxSProc, ATxVersionInfo, ATxUtils,
  TntSysUtils;

{$R *.DFM}

procedure TFormViewToolParams.btnCommandBrowseClick(Sender: TObject);
begin
  with OpenDialog1 do
    begin
    FileName:= SExpandVars(edCommand.Text);
    InitialDir:= SExtractFileDir(FileName);
    if Execute then
      begin
      edCaption.Text:= FGetFileDescription(FileName);
      edCommand.Text:= FileName;
      end;
    end;
end;

procedure TFormViewToolParams.listMacros33DblClick(Sender: TObject);
var
  S: string;
begin
  with listMacros do
    if ItemIndex >= 0 then
      begin
      S:= Items[ItemIndex];
      if edParams.Text = '' then
        edParams.Text:= S
      else
        edParams.Text:= edParams.Text + ' ' + S;
      end;
end;

procedure TFormViewToolParams.FormShow(Sender: TObject);
begin
  {$I Lang.FormViewToolParams.inc}
end;

function FGetFileDescription(const FileName: WideString): WideString;
begin
  Result:= FGetVersionInfo(FileName, vsFileDescription);
  if Result='' then
    Result:= WideChangeFileExt(SExtractFileName(FileName), '');
end;


procedure TFormViewToolParams.FormCreate(Sender: TObject);
begin
  //Fix form font
  //FixFormFont(Self.Font);
end;

end.
