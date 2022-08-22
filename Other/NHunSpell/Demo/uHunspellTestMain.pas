unit uHunspellTestMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls,
  ShellAPI, CheckLst, NHunspell;

type
  TFrmHunspellTestMain = class(TForm)
    edDict: TEdit;
    Label1: TLabel;
    btnDict: TButton;
    btnReadOXT: TButton;
    dlgLoadDictionary: TOpenDialog;
    Label2: TLabel;
    EdCheckWord: TEdit;
    btnSpell: TButton;
    MemOutput: TMemo;
    btnStem: TButton;
    btnAnalyze: TButton;
    btnHyphenate: TButton;
    btnReadFolder: TButton;
    btnClear: TButton;
    Label3: TLabel;
    Label4: TLabel;
    lbSpellDicts: TCheckListBox;
    lbHyphenDicts: TCheckListBox;
    btnDownload: TButton;
    procedure btnDictClick(Sender: TObject);
    procedure btnReadOXTClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnSpellClick(Sender: TObject);
    procedure btnStemClick(Sender: TObject);
    procedure btnAnalyzeClick(Sender: TObject);
    procedure btnHyphenateClick(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
    procedure btnReadFolderClick(Sender: TObject);
    procedure lbHyphenDictsClickCheck(Sender: TObject);
    procedure lbSpellDictsClickCheck(Sender: TObject);
    procedure btnDownloadClick(Sender: TObject);
  private
  public
    procedure UpdateDicts;
    procedure UpdateButtons;
  end;

var
  FrmHunspellTestMain: TFrmHunspellTestMain;

implementation

uses Math;

{$R *.dfm}

procedure TFrmHunspellTestMain.btnAnalyzeClick(Sender: TObject);
var
  tmpStr: TUnicodeStringList;
begin
  tmpStr := TUnicodeStringList.create;
  try
    TNHSpellDictionary(lbSpellDicts.Items.Objects[lbSpellDicts.Itemindex]).Analyze(EdCheckWord.Text, tmpStr);
    if tmpStr.count = 0 then
      memOutput.Text := 'Not in dictionary'
    else
      memOutput.Text := tmpStr.Text;
  finally
    FreeAndNil(tmpStr);
  end;
end;

procedure TFrmHunspellTestMain.btnDictClick(Sender: TObject);
begin
  dlgLoadDictionary.Filename := EdDict.Text;
  if dlgLoadDictionary.Execute then
    EdDict.Text := dlgLoadDictionary.FileName;

end;

procedure TFrmHunspellTestMain.btnDownloadClick(Sender: TObject);
begin
  ShowMessage('Download more dictionaries from the OpenOffice website and place them in the "\Dictionaries" subfolder. Then click "Read Folder" to update the list.');
  ShellExecute( Application.Handle, 'open', 'http://extensions.services.openoffice.org/en/dictionaries', nil, nil, SW_NORMAL );
end;

procedure TFrmHunspellTestMain.UpdateDicts;
var
  intIndex: integer;
begin
  with lbSpellDicts do
  try
    Items.BeginUpdate;
    clear;
    for intIndex := 0 to Hunspell.SpellDictionaryCount-1 do
      Items.AddObject(Format('%s - %s Version: %s', [Hunspell.SpellDictionaries[intIndex].LanguageName,
                                                     Hunspell.SpellDictionaries[intIndex].DisplayName,
                                                     Hunspell.SpellDictionaries[intIndex].Version]),
                      Hunspell.SpellDictionaries[intIndex]);
  finally
    Items.EndUpdate;
  end;

  with lbHyphenDicts do
  try
    Items.BeginUpdate;
    clear;
    for intIndex := 0 to Hunspell.HyphenDictionaryCount-1 do
      Items.AddObject(Format('%s - %s Version: %s', [Hunspell.HyphenDictionaries[intIndex].LanguageName,
                                                     Hunspell.HyphenDictionaries[intIndex].DisplayName,
                                                     Hunspell.HyphenDictionaries[intIndex].Version]),
                      Hunspell.HyphenDictionaries[intIndex]);
  finally
    Items.EndUpdate;
  end;
end;

procedure TFrmHunspellTestMain.btnReadOXTClick(Sender: TObject);
begin
  Hunspell.ReadOXT(EdDict.Text);
  UpdateDicts;
end;

procedure TFrmHunspellTestMain.btnHyphenateClick(Sender: TObject);
begin
  memOutput.Text := TNHHyphenDictionary(lbHyphenDicts.Items.Objects[lbHyphenDicts.Itemindex]).Hyphenate(EdCheckWord.Text);
end;

procedure TFrmHunspellTestMain.btnStemClick(Sender: TObject);
var
  tmpStr: TUnicodeStringList;
begin
    tmpStr := TUnicodeStringList.create;
    TNHSpellDictionary(lbSpellDicts.Items.Objects[lbSpellDicts.Itemindex]).Stem(EdCheckWord.Text, tmpStr);

    if tmpStr.count = 0 then
      memOutput.Text := 'Not in dictionary'
    else
      memOutput.Text := tmpStr.Text;
    FreeAndNil(tmpStr);
end;

procedure TFrmHunspellTestMain.btnClearClick(Sender: TObject);
begin
  Hunspell.ClearDictionaries;
  UpdateDicts;
  UpdateButtons;
end;

procedure TFrmHunspellTestMain.btnReadFolderClick(Sender: TObject);
begin
  Hunspell.ReadFolder(ExtractFilePath(EdDict.Text));
  UpdateDicts;
  UpdateButtons;
end;

procedure TFrmHunspellTestMain.FormCreate(Sender: TObject);
begin
  EdDict.Text := ExtractFilePath(application.Exename) + 'Dictionaries\dict-en.oxt';
end;

procedure TFrmHunspellTestMain.lbHyphenDictsClickCheck(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to lbHyphenDicts.Items.Count-1 do
  begin
    lbHyphenDicts.Checked[i] := i = lbHyphenDicts.ItemIndex;
    Hunspell.HyphenDictionaries[i].Active := lbHyphenDicts.Checked[i];
  end;

  Hunspell.UpdateAndLoadDictionaries;
  UpdateButtons;
end;

procedure TFrmHunspellTestMain.lbSpellDictsClickCheck(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to lbSpellDicts.Items.Count-1 do
  begin
    lbSpellDicts.Checked[i] := i = lbSpellDicts.ItemIndex;
    Hunspell.SpellDictionaries[i].Active := lbSpellDicts.Checked[i];
  end;

  Hunspell.UpdateAndLoadDictionaries;
  UpdateButtons;
end;

procedure TFrmHunspellTestMain.UpdateButtons;
begin
  btnSpell.Enabled := (lbSpellDicts.ItemIndex > -1) and TNHSpellDictionary(lbSpellDicts.Items.Objects[lbSpellDicts.ItemIndex]).Loaded;
  btnStem.Enabled := btnSpell.Enabled;
  btnAnalyze.Enabled := btnSpell.Enabled;
  btnHyphenate.Enabled := (lbHyphenDicts.ItemIndex > -1) and TNHHyphenDictionary(lbHyphenDicts.Items.Objects[lbHyphenDicts.ItemIndex]).Loaded;
end;

procedure TFrmHunspellTestMain.btnSpellClick(Sender: TObject);
var
  tmpStr: TUnicodeStringList;
begin

  if TNHSpellDictionary(lbSpellDicts.Items.Objects[lbSpellDicts.Itemindex]).Spell(EdCheckWord.Text) then
    memOutput.Text := 'Correct'
  else
  begin
    tmpStr := TUnicodeStringList.create;
    TNHSpellDictionary(lbSpellDicts.Items.Objects[lbSpellDicts.Itemindex]).Suggest(EdCheckWord.Text, tmpStr);

    if tmpStr.count = 0 then
      memOutput.Text := 'No suggestions'
    else
      memOutput.Text := tmpStr.Text;
    FreeAndNil(tmpStr);
  end;

end;

end.
