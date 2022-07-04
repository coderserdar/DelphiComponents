unit unit_Crypt;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Menus, Spin, ComCtrls,
  CHButton, CHLabel, CHCrypt, CHRadioButton;


type
  TfrmCHCrypt = class(TForm)
    MainMenu1: TMainMenu;
    close1: TMenuItem;
    Info1: TMenuItem;
    CHLabel1: TCHLabel;
    CHLabel2: TCHLabel;
    btnEncrypt: TCHButton;
    CHLabel3: TCHLabel;
    btnDecrypt: TCHButton;
    GroupBox2: TGroupBox;
    edtKey1: TSpinEdit;
    edtKey2: TSpinEdit;
    CHLabel4: TCHLabel;
    CHLabel5: TCHLabel;
    redtText: TRichEdit;
    redtEncrypt: TRichEdit;
    redtDecrypt: TRichEdit;
    edtKey3: TSpinEdit;
    edtKey4: TSpinEdit;
    CHLabel6: TCHLabel;
    CHLabel7: TCHLabel;
    cmbCryptMode: TComboBox;
    cmbCryptChar: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    btnClear: TCHButton;
    CHCrypt1: TCHCrypt;
    GroupBox1: TGroupBox;
    CHLabel8: TCHLabel;
    edtBase: TSpinEdit;
    procedure btnEncryptClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure close1Click(Sender: TObject);
    procedure Info1Click(Sender: TObject);
    procedure btnDecryptClick(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
    procedure cmbCryptCharChange(Sender: TObject);
    procedure cmbCryptModeChange(Sender: TObject);
  private
    FPath : string;
    procedure EnCrypt;
    procedure DeCrypt;
  public
    { Public-Deklarationen }
  end;

var
  frmCHCrypt: TfrmCHCrypt;

implementation

uses unit_About, _CHTypes, CHUnit;

{$R *.dfm}

procedure TfrmCHCrypt.FormCreate(Sender: TObject);
begin
  FPath := ExtractFilePath(Application.ExeName);
  cmbCryptMode.ItemIndex := 1;
  CHCrypt1.CryptMode := cmExtended;

  if frmDemo.bGerman then
  begin
    redtText.Lines[0] := 'Mit CHCrypt können Sie einfach und schnell Text verschlüsseln.' +#10;
    redtText.Lines[1] := 'Die Verschlüsselung generiert KEINE Steuerzeichen.' +#10;
    redtText.Lines[2] := 'Dadurch kann der Text leicht weiterverarbeitet werden.';
  end
  else
  begin
    redtText.Lines[0] := 'With CHCrypt you can encode text simply and fast.' +#10;
    redtText.Lines[1] := 'The encryption does not generate any control characters.' +#10;
    redtText.Lines[2] := 'So the text can be handled by that easily.';
  end;
end;


{ Encrypt }
procedure TfrmCHCrypt.btnEncryptClick(Sender: TObject);
begin
  CHCrypt1.Key1 := edtKey1.Value;
  CHCrypt1.Key2 := edtKey2.Value;
  CHCrypt1.Key3 := edtKey3.Value;
  CHCrypt1.Key4 := edtKey4.Value;
  CHCrypt1.Base := edtBase.Value;

  EnCrypt;

end;

{ Encrypt to Memo}
procedure TfrmCHCrypt.EnCrypt;
var
  I : Integer;
  sCryptText : string;
begin
  redtEncrypt.Clear;
  for I := 0 to redtText.Lines.Count -1 do
  begin
    sCryptText := redtText.Lines[I];
    redtEncrypt.Lines[I] := CHCrypt1.DoEncrypt(sCryptText) +#10;
  end;

  redtEncrypt.SelStart := SendMessage(redtEncrypt.Handle, EM_LINEINDEX, 0, 0);
  Application.ProcessMessages;
  redtEncrypt.SetFocus;
end;

{ Decrypt }
procedure TfrmCHCrypt.btnDecryptClick(Sender: TObject);
begin
  DeCrypt;
end;

{ Decrypt from Memo}
procedure TfrmCHCrypt.DeCrypt;
var
  I : Integer;
  sCryptText, sDeCrypt : string;
begin
  redtDecrypt.Clear;
  for I := 0 to redtEncrypt.Lines.Count -1 do
  begin
    sCryptText := redtEncrypt.Lines[I];
    sDeCrypt := CHCrypt1.DoDecrypt(sCryptText) +#10;
    redtDecrypt.Lines[I] := sDeCrypt;
  end;

  redtDecrypt.SelStart := SendMessage(redtDecrypt.Handle, EM_LINEINDEX, 0, 0);
  Application.ProcessMessages;
  redtDecrypt.SetFocus;
end;

{ EINSTELLUNGEN }
procedure TfrmCHCrypt.cmbCryptCharChange(Sender: TObject);
begin
  redtText.Clear;
  if cmbCryptChar.ItemIndex = 0 then
  begin
    CHCrypt1.CryptChar := ccAll;
    if frmDemo.bGerman then
    begin
      redtText.Lines[0] := '"All" bedeutet, dass zur Verschlüsselung unter anderem Klein- als auch' +#10;
      redtText.Lines[1] := 'Grossbuchstaben verwendet werden.' +#10;
    end
    else
    begin
      redtText.Lines[0] := '"ALL" means that to the encoding small and' +#10;
      redtText.Lines[1] := 'Uppercase letter to be used.' +#10;
    end;
  end
  else if cmbCryptChar.ItemIndex = 1 then
  begin
    CHCrypt1.CryptChar := ccUpper;
    if frmDemo.bGerman then
    begin
      redtText.Lines[0] := '"Upper" bedeutet, dass zur Verschlüsselung unter anderem nur Großbuchstaben' +#10;
      redtText.Lines[1] := 'verwendet werden.' +#10;
      redtText.Lines[2] := 'Dieser Modus eignet sich sehr gut zum Verarbeiten von INI-Dateien.' +#10;
    end
    else
    begin
      redtText.Lines[0] := '"Upper" means that for the encoding only uppercase' +#10;
      redtText.Lines[1] := 'letters are used.' +#10;
      redtText.Lines[2] := 'This mode is suitable very well for processing INI files.' +#10;
    end;
  end
  else if cmbCryptChar.ItemIndex = 2 then
  begin
    CHCrypt1.CryptChar := ccLower;
    if frmDemo.bGerman then
    begin
      redtText.Lines[0] := '"Lower" bedeutet, dass zur Verschlüsselung unter anderem nur Kleinbuchstaben' +#10;
      redtText.Lines[1] := 'verwendet werden.' +#10;
      redtText.Lines[2] := 'Dieser Modus eignet sich sehr gut zum Verarbeiten von INI-Dateien.' +#10;
    end
    else
    begin
      redtText.Lines[0] := '"Lower" means that for the encoding only lowercase' +#10;
      redtText.Lines[1] := 'letters are used.' +#10;
      redtText.Lines[2] := 'This mode is suitable very well for processing INI files.' +#10;
    end;
  end;
end;

procedure TfrmCHCrypt.cmbCryptModeChange(Sender: TObject);
begin
  redtText.Clear;
  if cmbCryptMode.ItemIndex = 0 then
  begin
    CHCrypt1.CryptMode := cmStandard;
    if frmDemo.bGerman then
    begin
      redtText.Lines[0] := 'Der Standardmodus verarbeitet keine Sonderzeichen!!!' +#10;
      redtText.Lines[1] := 'Zur Verschluesselung werden nur die Zeichen "a..z", "A..Z", "0..9"' +#10;
      redtText.Lines[2] := 'sowie alle sonstigen druckbaren ASCII-Zeichen verwendet.' +#10;
      redtText.Lines[3] := 'Der Vorteil liegt darin, dass alle PCs diesen Zeichensatz decodieren koennen.';
    end
    else
    begin
      redtText.Lines[0] := 'The standard mode does not process special characters!!!' +#10;
      redtText.Lines[1] := 'The encoding only the characters "a..z", "A..Z", "0..9"' +#10;
      redtText.Lines[2] := 'as well as all other printable ASCII characters uses.' +#10;
      redtText.Lines[3] := 'The advantage is in the fact that all PC s can decode this character';
    end;
  end
  else if cmbCryptMode.ItemIndex = 1 then
  begin
    CHCrypt1.CryptMode := cmExtended;
    if frmDemo.bGerman then
    begin
      redtText.Lines[0] := 'Im CryptModus "Extended" kommen zur Verschlüsselung der Standardzeichensatz' +#10;
      redtText.Lines[1] := 'sowie der ISO-8859-1 Zeichensatz (auch Latin 1 genannt) zum Einsatz' +#10;
      redtText.Lines[2] := 'Benutzer, die einen anderen Zeichensatz verwenden (z.b. Russland),' +#10;
      redtText.Lines[3] := 'können mit "Extended" codierte Texte nicht decodieren.';
    end
    else
    begin
      redtText.Lines[0] := 'In the CryptModus "Extended" CHCrypt used the standard' +#10;
      redtText.Lines[1] := 'as well as the ISO-8859-1 character set (also Latin 1 called) to encoding.' +#10;
      redtText.Lines[2] := 'Users, who use another character set (e.g. Russia),' +#10;
      redtText.Lines[3] := 'cannot with "Extended" encoded texts decode.';
    end;
  end
  else if cmbCryptMode.ItemIndex = 2 then
  begin
    CHCrypt1.CryptMode := cmIniStandard;
    if frmDemo.bGerman then
    begin
      redtText.Lines[0] := 'Siehe Standard.' +#10;
      redtText.Lines[1] := 'Zudem werden nur Zeichen verwendet, die mit INI-Dateien' +#10;
      redtText.Lines[2] := 'kompatibel sind. Also z.B. kein "=", "[", "]", usw.' +#10;
    end
    else
    begin
      redtText.Lines[0] := 'See standard.' +#10;
      redtText.Lines[1] := 'Additionally only characters are used, which are compatible' +#10;
      redtText.Lines[2] := 'with ini-files. Thus e.g. none "=", "[", "]", etc..' +#10;
    end;
  end
  else if cmbCryptMode.ItemIndex = 3 then
  begin
    CHCrypt1.CryptMode := cmIniExtended;
    if frmDemo.bGerman then
    begin
      redtText.Lines[0] := 'Siehe Extended.' +#10;
      redtText.Lines[1] := 'Zudem werden nur Zeichen verwendet, die mit INI-Dateien' +#10;
      redtText.Lines[2] := 'kompatibel sind. Also z.B. kein "=", "[", "]", usw.' +#10;
    end
    else
    begin
      redtText.Lines[0] := 'See extended.' +#10;
      redtText.Lines[1] := 'Additionally only characters are used, which are compatible' +#10;
      redtText.Lines[2] := 'with ini-files. Thus e.g. none "=", "[", "]", etc..' +#10;
    end;
  end;
end;

procedure TfrmCHCrypt.btnClearClick(Sender: TObject);
begin
  redtText.Clear;
  redtEncrypt.Clear;
  redtDecrypt.Clear;
end;

procedure TfrmCHCrypt.Info1Click(Sender: TObject);
begin
  frmAbout := TfrmAbout.Create(Self);
  frmAbout.ShowModal;
end;


procedure TfrmCHCrypt.close1Click(Sender: TObject);
begin
  Close;
end;

end.
