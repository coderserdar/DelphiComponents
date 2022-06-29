{$I ATStreamSearchOptions.inc} //ATStreamSearch options.

unit UFormViewFindText;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, TntStdCtrls;

type
  TFormViewFindText = class(TForm)
    labFind: TLabel;
    btnOk: TButton;
    btnCancel: TButton;
    edText: TTntComboBox;
    btnHelp: TButton;
    boxOptions: TGroupBox;
    chkWords: TCheckBox;
    chkCase: TCheckBox;
    chkHex: TCheckBox;
    chkRegex: TCheckBox;
    boxDirection: TGroupBox;
    chkDirForward: TRadioButton;
    chkDirBackward: TRadioButton;
    boxOrigin: TGroupBox;
    chkOriginCursor: TRadioButton;
    chkOriginEntire: TRadioButton;
    chkMLine: TCheckBox;
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure chkRegexClick(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);
    procedure chkHexClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    BackEnabled: boolean;
    HexEnabled: boolean;
    RegexEnabled: boolean;
    OriginEnabled: boolean;
  end;

implementation

uses
  ATxMsgProc, ATxSHex;

{$R *.DFM}

procedure TFormViewFindText.FormShow(Sender: TObject);
begin
  {$I Lang.FormViewFindText.inc}
  chkRegexClick(Self);
end;

procedure TFormViewFindText.FormCreate(Sender: TObject);
begin
  BackEnabled:= true;
  HexEnabled:= true;
  RegexEnabled:= true;
  OriginEnabled:= true;
end;

procedure TFormViewFindText.chkRegexClick(Sender: TObject);
begin
  if not BackEnabled then chkDirForward.Checked:= true;
  if not HexEnabled then chkHex.Checked:= false;
  if not RegexEnabled then chkRegex.Checked:= false;
  if not OriginEnabled then chkOriginEntire.Checked:= true;

  chkDirForward.Enabled:= BackEnabled and (not chkRegex.Checked);
  chkDirBackward.Enabled:= chkDirForward.Enabled;
  chkOriginCursor.Enabled:= OriginEnabled;
  chkOriginEntire.Enabled:= chkOriginCursor.Enabled;
  chkHex.Enabled:= HexEnabled and (not chkRegex.Checked);
  chkRegex.Enabled:= RegexEnabled;
  chkMLine.Enabled:= chkRegex.Enabled and chkRegex.Checked;
  btnHelp.Enabled:= RegexEnabled;

  //Uncheck Hex and Backward options when Regex checked
  if chkRegex.Checked then
  begin
    if chkHex.Checked then chkHex.Checked:= false;
    chkDirForward.Checked:= true;
  end;
end;

procedure TFormViewFindText.chkHexClick(Sender: TObject);
var
  S: string;
begin
  //Decode text only when form is visible,
  //otherwise chkHex.Checked assignment will break text
  if Visible then
    if (edText.Text <> '') then
    begin
      if chkHex.Checked then
        edText.Text:= SToHex(edText.Text)
      else
        if SHexToNormal(edText.Text, S) then
          edText.Text:= S;
    end;
end;

procedure TFormViewFindText.btnHelpClick(Sender: TObject);
begin
  ShowHelp(Handle, 'RegEx.html');
end;

end.
