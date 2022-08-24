unit RibbonDemoMainForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, ImgList, Ribbon, RibbonStyleActnCtrls, ActnList, ActnMan,
  ActnMenus, RibbonActnMenus, StdActns, ExtActns, ToolWin, ActnCtrls, StdCtrls,
  ComCtrls, RibbonActnCtrls, RibbonLunaStyleActnCtrls, RibbonObsidianStyleActnCtrls,
  RibbonSilverStyleActnCtrls, ScreenTips, pngimage, plsController,
  Actions;

type
  TFileFormat = (ffDefault, ffRTF, ffTXT);

  TfrmRibbonDemo = class(TForm)
    ActionManager1: TActionManager;
    Ribbon1: TRibbon;
    GridPanel1: TGridPanel;
    RichEdit1: TRichEdit;
    rpHome: TRibbonPage;
    rgHomeClipboard: TRibbonGroup;
    rgHomeFont: TRibbonGroup;
    rgHomeParagraph: TRibbonGroup;
    FileOpen1: TFileOpen;
    FileSaveAs1: TFileSaveAs;
    FilePrintSetup1: TFilePrintSetup;
    FilePageSetup1: TFilePageSetup;
    FileExit1: TFileExit;
    ilGFX16: TImageList;
    ilGFX16_d: TImageList;
    ilGFX32: TImageList;
    ilGFX32_d: TImageList;
    EditCut1: TEditCut;
    EditCopy1: TEditCopy;
    EditPaste1: TEditPaste;
    EditSelectAll1: TEditSelectAll;
    EditUndo1: TEditUndo;
    EditDelete1: TEditDelete;
    FileSaveActn: TAction;
    FileNewActn: TAction;
    RichEditBold1: TRichEditBold;
    RichEditItalic1: TRichEditItalic;
    RichEditUnderline1: TRichEditUnderline;
    RichEditStrikeOut1: TRichEditStrikeOut;
    RichEditBullets1: TRichEditBullets;
    RichEditAlignLeft1: TRichEditAlignLeft;
    RichEditAlignRight1: TRichEditAlignRight;
    RichEditAlignCenter1: TRichEditAlignCenter;
    SearchFind1: TSearchFind;
    SearchReplace1: TSearchReplace;
    rgHomeEditing: TRibbonGroup;
    rpControlTypes: TRibbonPage;
    rpStyle: TRibbonPage;
    rpStyleRibbonStyle: TRibbonGroup;
    LunaStyleActn: TAction;
    ObsidianStyleActn: TAction;
    SilverStyleActn: TAction;
    RibbonApplicationMenuBar1: TRibbonApplicationMenuBar;
    RibbonQuickAccessToolbar1: TRibbonQuickAccessToolbar;
    FileCloseActn: TAction;
    FileSaveAsText: TAction;
    FileSaveAsRTF: TAction;
    FileQuickPrint: TAction;
    FilePrintPreview: TAction;
    rcbFonts: TRibbonComboBox;
    rcbFontSize: TRibbonComboBox;
    FontGrowSizeActn: TAction;
    FontShrinkSizeActn: TAction;
    FontSubscriptActn: TAction;
    FontSuperScriptActn: TAction;
    ChangeCaseSentenceActn: TAction;
    ChangeCaseLowerActn: TAction;
    ChangeCaseUpperActn: TAction;
    ChangeCaseCapitalizeActn: TAction;
    ChangeCaseToggleActn: TAction;
    FontHighlightActn: TAction;
    FontColorActn: TAction;
    ChangeCaseActn: TAction;
    EditPasteSpecial: TAction;
    EditPasteHyperlink: TAction;
    FileRun1: TFileRun;
    rgSpinEdit: TRibbonGroup;
    RibbonSpinEdit1: TRibbonSpinEdit;
    RibbonSpinEdit2: TRibbonSpinEdit;
    RibbonSpinEdit3: TRibbonSpinEdit;
    RibbonSpinEdit4: TRibbonSpinEdit;
    FontEdit1: TFontEdit;
    PrintDlg1: TPrintDlg;
    rgRadioButtons: TRibbonGroup;
    RadioAction1: TAction;
    RadioAction2: TAction;
    RadioAction3: TAction;
    rgCheckBoxes: TRibbonGroup;
    CheckboxAction1: TAction;
    CheckboxAction2: TAction;
    CheckboxAction3: TAction;
    rgOtherControls: TRibbonGroup;
    DateTimePicker1: TDateTimePicker;
    TreeView1: TTreeView;
    alBulletNumberGallery: TActionList;
    ScreenTipsManager1: TScreenTipsManager;
    NumberingActn: TAction;
    ilBulletNumberGallery: TImageList;
    NumberNoneActn: TAction;
    NumberArabicDotActn: TAction;
    NumberArabicParenActn: TAction;
    NumberUpperRomanActn: TAction;
    NumberUpperActn: TAction;
    NumberLowerParenActn: TAction;
    NumberLowerDotActn: TAction;
    NumberLowerRomanActn: TAction;
    plsController1: TplsController;
    cbLangs: TComboBox;
    procedure FileSaveAs1Accept(Sender: TObject);
    procedure FileSaveAs1Update(Sender: TObject);
    procedure FileSaveActnExecute(Sender: TObject);
    procedure FilePrintActnExecute(Sender: TObject);
    procedure FileNewActnExecute(Sender: TObject);
    procedure FileCloseActnExecute(Sender: TObject);
    procedure FileSaveAsRTFExecute(Sender: TObject);
    procedure FileSaveAsTextExecute(Sender: TObject);
    procedure FileQuickPrintExecute(Sender: TObject);
    procedure FontGrowSizeActnExecute(Sender: TObject);
    procedure FontShrinkSizeActnExecute(Sender: TObject);
    procedure SetFontName(Sender: TObject);
    procedure SetFontSize(NewSize: Integer);
    procedure ActionManager1StyleChanged(Sender: TCustomActionManager);
    procedure LunaStyleActnExecute(Sender: TObject);
    procedure ObsidianStyleActnExecute(Sender: TObject);
    procedure SilverStyleActnExecute(Sender: TObject);
    procedure FontEdit1Accept(Sender: TObject);
    procedure FontEdit1BeforeExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure RichEdit1Change(Sender: TObject);
    procedure RichEdit1KeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure RichEdit1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure rcbFontSizeChange(Sender: TObject);
    procedure ClearFormattingActnExecute(Sender: TObject);
    procedure ChangeCaseLowerActnExecute(Sender: TObject);
    procedure CheckboxAction1Execute(Sender: TObject);
    procedure RadioAction1Execute(Sender: TObject);
    procedure FontCalibriActnExecute(Sender: TObject);
    procedure FontSubscriptActnUpdate(Sender: TObject);
    procedure FontSubscriptActnExecute(Sender: TObject);
    procedure FontSuperScriptActnExecute(Sender: TObject);
    procedure FontSuperScriptActnUpdate(Sender: TObject);
    procedure NumberingActnExecute(Sender: TObject);
    procedure NumberNoneActnExecute(Sender: TObject);
    procedure NumberArabicDotActnExecute(Sender: TObject);
    procedure NumberArabicParenActnExecute(Sender: TObject);
    procedure NumberUpperRomanActnExecute(Sender: TObject);
    procedure NumberUpperActnExecute(Sender: TObject);
    procedure NumberLowerParenActnExecute(Sender: TObject);
    procedure NumberLowerDotActnExecute(Sender: TObject);
    procedure NumberLowerRomanActnExecute(Sender: TObject);
    procedure NumberingActnUpdate(Sender: TObject);
    procedure FileOpen1Accept(Sender: TObject);
    procedure plsController1InitLangManager(Sender: TObject);
    procedure plsController1LanguageChanged(Sender: TObject);
    procedure cbLangsSelect(Sender: TObject);
  private
    FFileName: string;
    procedure SaveFile(const FileName: string; const Format: TFileFormat = ffDefault);
    procedure LoadFile(const FileName: string);
    procedure CloseFile;
    function GetIsFileOpen: Boolean;
    procedure UpdateUI;
    procedure LoadRecentFiles;
    property IsFileOpen: Boolean read GetIsFileOpen;
    procedure CreateNewFile;
    procedure EnableEmptyActions;
    procedure SetNumberStyle(Numbering, Style : Word);
    procedure ReadAvailableLanguages;
  public
    { Public declarations }
  end;

var
  frmRibbonDemo: TfrmRibbonDemo;

implementation

uses
  RichEdit;

{$R *.dfm}


procedure TfrmRibbonDemo.plsController1InitLangManager(Sender: TObject);
begin
  ReadAvailableLanguages;
end;

procedure TfrmRibbonDemo.ReadAvailableLanguages;
var
  i:Integer;
  cblos:TNotifyEvent;
begin
  // fill menu with available languages
  cbLangs.Items.BeginUpdate;
  cblos := cbLangs.OnSelect;
  cbLangs.OnSelect := nil;
  try
    cbLangs.Items.Clear;
    for i := 0 To plsController1.LanguagesCount - 1 do
      cbLangs.Items.Add(plsController1.LanguageNames[i]+' ('+plsController1.LanguageCodes[i]+')');
  finally
    cbLangs.Items.EndUpdate;
    cbLangs.OnSelect := cblos;
  end;
end;

procedure TfrmRibbonDemo.plsController1LanguageChanged(Sender: TObject);
var
  cblos:TNotifyEvent;
begin
  cblos := cbLangs.OnSelect;
  cbLangs.OnSelect := nil;
  try
    cbLangs.ItemIndex := cbLangs.Items.IndexOf(TplsController(Sender).LanguageName+' ('+TplsController(Sender).LanguageCode+')');
  finally
    cbLangs.OnSelect := cblos;
  end;
end;

procedure TfrmRibbonDemo.cbLangsSelect(Sender: TObject);
var
  tmp:string;
  i:Integer;
begin
  if cbLangs.ItemIndex>=0 then
  begin
    tmp:=cbLangs.Items[cbLangs.ItemIndex];
    i:=LastDelimiter('(',tmp);
    if i>0 then
      plsController1.LanguageCode:=Copy(tmp,i+1,Length(tmp)-i-1);
  end;
end;


const
  FormatExt: array[TFileFormat] of string = ('.rtf', '.rtf', '.txt');

procedure TfrmRibbonDemo.SaveFile(const FileName: string; const Format: TFileFormat);
var
  lNewFileName: string;
begin
  if IsFileOpen then
  begin
    if FileName = '' then
    begin
      FileSaveAs1.Execute;
      exit;
    end;
    lNewFileName := FileName;
    if Format = ffTXT then
    begin
      RichEdit1.PlainText := True;
      lNewFileName := ChangeFileExt(FileName, FormatExt[Format]);
    end;
    RichEdit1.Lines.SaveToFile(lNewFileName);
    FFileName := FileName;
  end;
end;

procedure TfrmRibbonDemo.LoadFile(const FileName: string);
begin
  FileNewActn.Execute;
  RichEdit1.Lines.LoadFromFile(FileName);
  FFileName := FileName;
  Ribbon1.DocumentName := ChangeFileExt(ExtractFileName(FileName), '');
  RichEdit1.PlainText := SameText(ExtractFileExt(FileName), FormatExt[ffTXT]);
  RichEdit1.Modified := False;
end;

procedure TfrmRibbonDemo.NumberingActnExecute(Sender: TObject);
begin
  if TAction(Sender).checked then
    NumberNoneActn.Execute
  else
    NumberArabicDotActn.Execute;
end;

procedure TfrmRibbonDemo.NumberingActnUpdate(Sender: TObject);
var AParaFormat2 : TParaFormat2;
begin
  AParaFormat2.cbSize := SizeOf(AParaFormat2);
  AParaFormat2.dwMask := PFM_NUMBERING or PFM_NUMBERINGSTYLE;
  SendMessage(RichEdit1.Handle, EM_GETPARAFORMAT, 0, LParam(@AParaFormat2));
  TAction(Sender).Checked := AParaFormat2.wNumbering > 1;
  with AParaFormat2 do
  begin
    NumberNoneActn.Checked        := (wNumbering = 0);
    NumberArabicDotActn.Checked   := (wNumbering = 2) and (wNumberingStyle = $200);
    NumberArabicParenActn.Checked := (wNumbering = 2) and (wNumberingStyle = $000);
    NumberUpperRomanActn.Checked  := (wNumbering = 6) and (wNumberingStyle = $200);
    NumberUpperActn.Checked       := (wNumbering = 4) and (wNumberingStyle = $200);
    NumberLowerParenActn.Checked  := (wNumbering = 3) and (wNumberingStyle = $000);
    NumberLowerDotActn.Checked    := (wNumbering = 3) and (wNumberingStyle = $200);
    NumberLowerRomanActn.Checked  := (wNumbering = 5) and (wNumberingStyle = $200);
  end;
  TAction(Sender).Enabled := ActiveControl = RichEdit1;
end;

procedure TfrmRibbonDemo.ActionManager1StyleChanged(Sender: TCustomActionManager);
begin
  if Sender.Style = RibbonLunaStyle then
    GridPanel1.Color := clSkyBlue
  else if Sender.Style = RibbonObsidianStyle then
    GridPanel1.Color := $00444444
  else
    GridPanel1.Color := $00CBC2BF;
end;

procedure TfrmRibbonDemo.ChangeCaseLowerActnExecute(Sender: TObject);
begin
  RichEdit1.SelText := AnsiLowerCase(RichEdit1.SelText);
end;

procedure TfrmRibbonDemo.CheckboxAction1Execute(Sender: TObject);
begin
  // check box execute handlers
end;

procedure TfrmRibbonDemo.ClearFormattingActnExecute(Sender: TObject);
begin
  RichEdit1.SelAttributes.Assign(RichEdit1.DefAttributes)
end;

procedure TfrmRibbonDemo.CloseFile;
begin
  if IsFileOpen and RichEdit1.Modified then
  begin
    if FFileName = '' then
      FileSaveAs1.Execute
    else
      FileSaveActn.Execute;
  end;
  FFileName := '';
  Ribbon1.DocumentName := '';
  RichEdit1.Visible := False;
  RichEdit1.Lines.Clear;
end;

function TfrmRibbonDemo.GetIsFileOpen: Boolean;
begin
  result := RichEdit1.Visible;
end;

procedure TfrmRibbonDemo.UpdateUI;
var
  LAttribs: TTextAttributes;
begin
  LAttribs := RichEdit1.SelAttributes;
  rcbFontSize.Text := IntToStr(LAttribs.Size);
  rcbFonts.Text := LAttribs.Name;
end;

procedure TfrmRibbonDemo.LoadRecentFiles;
var
  LSearchRec: TSearchRec;
  LFolder: string;
begin
  Ribbon1.ClearRecentItems;
  LFolder := 'C:\Temp\Filenames\';
  if DirectoryExists(LFolder) then
  begin
    if FindFirst(LFolder + '*.txt', faAnyFile, LSearchRec) = 0 then
    begin
      Ribbon1.AddRecentItem(LFolder + LSearchRec.Name);
      while FindNext(LSearchRec) = 0 do
        Ribbon1.AddRecentItem(LFolder + LSearchRec.Name);
    end;
  end;
end;

procedure TfrmRibbonDemo.LunaStyleActnExecute(Sender: TObject);
begin
  Ribbon1.Style := RibbonLunaStyle;
end;

procedure TfrmRibbonDemo.NumberArabicDotActnExecute(Sender: TObject);
begin
  SetNumberStyle(2, $200);
end;

procedure TfrmRibbonDemo.NumberArabicParenActnExecute(Sender: TObject);
begin
  SetNumberStyle(2, 0);
end;

procedure TfrmRibbonDemo.NumberLowerDotActnExecute(Sender: TObject);
begin
  SetNumberStyle(3, $200);
end;

procedure TfrmRibbonDemo.NumberLowerParenActnExecute(Sender: TObject);
begin
  SetNumberStyle(3, 0);
end;

procedure TfrmRibbonDemo.NumberLowerRomanActnExecute(Sender: TObject);
begin
  SetNumberStyle(5, $200);
end;

procedure TfrmRibbonDemo.NumberNoneActnExecute(Sender: TObject);
begin
  SetNumberStyle(0, 0);
end;

procedure TfrmRibbonDemo.NumberUpperActnExecute(Sender: TObject);
begin
  SetNumberStyle(4, $200);
end;

procedure TfrmRibbonDemo.NumberUpperRomanActnExecute(Sender: TObject);
begin
  SetNumberStyle(6, $200);
end;

procedure TfrmRibbonDemo.ObsidianStyleActnExecute(Sender: TObject);
begin
  Ribbon1.Style := RibbonObsidianStyle;
end;

procedure TfrmRibbonDemo.RadioAction1Execute(Sender: TObject);
begin
  // Radio Button execute handlers
end;

procedure TfrmRibbonDemo.rcbFontSizeChange(Sender: TObject);
begin
  SetFontSize(StrToIntDef(TCustomEdit(Sender).Text, 8));
end;

procedure TfrmRibbonDemo.RichEdit1Change(Sender: TObject);
begin
  UpdateUI
end;

procedure TfrmRibbonDemo.RichEdit1KeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  UpdateUI
end;

procedure TfrmRibbonDemo.RichEdit1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  UpdateUI
end;

procedure TfrmRibbonDemo.CreateNewFile;
begin
  if RichEdit1.Visible then
  begin
    if RichEdit1.Modified then
      SaveFile(FFileName);
  end;
  RichEdit1.PlainText := False;
  RichEdit1.Visible := True;
  RichEdit1.Lines.Clear;
  ActiveControl := RichEdit1;
end;

procedure TfrmRibbonDemo.EnableEmptyActions;
begin
  ChangeCaseActn.DisableIfNoHandler := False;
  ChangeCaseSentenceActn.DisableIfNoHandler := False;
  ChangeCaseLowerActn.DisableIfNoHandler := False;
  ChangeCaseUpperActn.DisableIfNoHandler := False;
  ChangeCaseCapitalizeActn.DisableIfNoHandler := False;
  ChangeCaseToggleActn.DisableIfNoHandler := False;
  FontSubscriptActn.DisableIfNoHandler := False;
  FontSuperScriptActn.DisableIfNoHandler := False;
  FontHighlightActn.DisableIfNoHandler := False;
  FontColorActn.DisableIfNoHandler := False;
end;

procedure TfrmRibbonDemo.FileCloseActnExecute(Sender: TObject);
begin
  CloseFile;
end;

procedure TfrmRibbonDemo.FileNewActnExecute(Sender: TObject);
begin
  CreateNewFile;
end;

procedure TfrmRibbonDemo.FileOpen1Accept(Sender: TObject);
begin
  LoadFile(FileOpen1.Dialog.FileName);
end;

procedure TfrmRibbonDemo.FilePrintActnExecute(Sender: TObject);
begin
  if PrintDlg1 .Execute then
    RichEdit1.Print(Format('Printing %s...', [Ribbon1.DocumentName]));
end;

procedure TfrmRibbonDemo.FileQuickPrintExecute(Sender: TObject);
begin
  RichEdit1.Print(Format('Printing %s...', [Ribbon1.DocumentName]));
end;

procedure TfrmRibbonDemo.FileSaveActnExecute(Sender: TObject);
begin
  SaveFile(FFileName);
end;

procedure TfrmRibbonDemo.FileSaveAs1Accept(Sender: TObject);
begin
  SaveFile(FileSaveAs1.Dialog.FileName);
end;

procedure TfrmRibbonDemo.FileSaveAs1Update(Sender: TObject);
begin
  TAction(Sender).Enabled := IsFileOpen;
end;

procedure TfrmRibbonDemo.FileSaveAsRTFExecute(Sender: TObject);
begin
  SaveFile(FFileName, ffRTF);
end;

procedure TfrmRibbonDemo.FileSaveAsTextExecute(Sender: TObject);
begin
  SaveFile(FFileName, ffTXT);
end;

procedure TfrmRibbonDemo.FontCalibriActnExecute(Sender: TObject);
var
  LAttribs: TTextAttributes;
begin
  LAttribs := RichEdit1.SelAttributes;
  LAttribs.Name := TAction(Sender).Caption;
end;

procedure TfrmRibbonDemo.FontEdit1Accept(Sender: TObject);
begin
  RichEdit1.SelAttributes.Assign(FontEdit1.Dialog.Font);
end;

procedure TfrmRibbonDemo.FontEdit1BeforeExecute(Sender: TObject);
begin
  FontEdit1.Dialog.Font.Assign(RichEdit1.SelAttributes);
end;

procedure TfrmRibbonDemo.FontGrowSizeActnExecute(Sender: TObject);
begin
  RichEdit1.SelAttributes.Size := RichEdit1.SelAttributes.Size + 1;
end;

procedure TfrmRibbonDemo.FontShrinkSizeActnExecute(Sender: TObject);
begin
  RichEdit1.SelAttributes.Size := RichEdit1.SelAttributes.Size - 1;
end;

procedure TfrmRibbonDemo.FontSubscriptActnExecute(Sender: TObject);
var aCharFormat2 : TCharFormat2;
begin
  aCharFormat2.cbSize := SizeOf(aCharFormat2);
  aCharFormat2.dwMask :=  CFM_SUBSCRIPT;
  if TAction(Sender).Checked then
    aCharFormat2.dwEffects := CFE_SUBSCRIPT;
  SendMessage(RichEdit1.Handle, EM_SETCHARFORMAT, SCF_SELECTION, LParam(@aCharFormat2));
end;

procedure TfrmRibbonDemo.FontSubscriptActnUpdate(Sender: TObject);
var aCharFormat2 : TCharFormat2;
begin
  aCharFormat2.cbSize := SizeOf(aCharFormat2);
  aCharFormat2.dwMask :=  CFM_SUBSCRIPT;
  SendMessage(RichEdit1.Handle, EM_GETCHARFORMAT, SCF_SELECTION, LParam(@aCharFormat2));
  TAction(Sender).Checked := (aCharFormat2.dwEffects and CFE_SUBSCRIPT) = CFE_SUBSCRIPT;
end;

procedure TfrmRibbonDemo.FontSuperScriptActnExecute(Sender: TObject);
var aCharFormat2 : TCharFormat2;
begin
  aCharFormat2.cbSize := SizeOf(aCharFormat2);
  aCharFormat2.dwMask :=  CFM_SUPERSCRIPT;
  if TAction(Sender).Checked then
    aCharFormat2.dwEffects := CFE_SUPERSCRIPT;
  SendMessage(RichEdit1.Handle, EM_SETCHARFORMAT, SCF_SELECTION, LParam(@aCharFormat2));
end;

procedure TfrmRibbonDemo.FontSuperScriptActnUpdate(Sender: TObject);
var aCharFormat2 : TCharFormat2;
begin
  aCharFormat2.cbSize := SizeOf(aCharFormat2);
  aCharFormat2.dwMask :=  CFM_SUPERSCRIPT;
  SendMessage(RichEdit1.Handle, EM_GETCHARFORMAT, SCF_SELECTION, LParam(@aCharFormat2));
  TAction(Sender).Checked := (aCharFormat2.dwEffects and CFE_SUPERSCRIPT) = CFE_SUPERSCRIPT;
end;

procedure TfrmRibbonDemo.FormCreate(Sender: TObject);
var
  LRect: TRect;
begin
  inherited;
  DoubleBuffered:=true;
  Ribbon1.DoubleBuffered:=True;
  ReportMemoryLeaksOnShutdown := True;
  EnableEmptyActions;
  if (ParamCount > 0) and FileExists(ParamStr(1)) then
    LoadFile(ParamStr(1))
  else
    CreateNewFile;
  LoadRecentFiles;

  LRect := RichEdit1.ClientRect;
  InflateRect(LRect, -25, -25);
  RichEdit1.Perform(EM_SETRECT, 0, Integer(@LRect));
end;

procedure TfrmRibbonDemo.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (ssCtrl in Shift) and (Key = VK_F1) then
  begin
    Ribbon1.Minimized := not Ribbon1.Minimized;
    Key := 0;
  end;
end;

procedure TfrmRibbonDemo.SetFontName(Sender: TObject);
begin
  RichEdit1.SelAttributes.Name := TAction(Sender).caption;
  UpdateUI;
end;

procedure TfrmRibbonDemo.SetFontSize(NewSize: Integer);
var
  LAttribs: TTextAttributes;
begin
  LAttribs := RichEdit1.SelAttributes;
  LAttribs.Size := NewSize;
  UpdateUI;
end;

procedure TfrmRibbonDemo.SetNumberStyle(Numbering, Style: Word);
var aparaformat2 : TParaFormat2;
begin
  aparaformat2.cbSize := SizeOf(aparaformat2);
  aparaformat2.dwMask := PFM_NUMBERING or PFM_NUMBERINGSTART or PFM_NUMBERINGSTYLE;
  aparaformat2.wNumbering := Numbering;
  aparaformat2.wNumberingStart := 1;
  aparaformat2.wNumberingStyle := Style;

  SendMessage(richedit1.Handle, EM_SETPARAFORMAT, 0, LParam(@aparaformat2));
end;

procedure TfrmRibbonDemo.SilverStyleActnExecute(Sender: TObject);
begin
  Ribbon1.Style := RibbonSilverStyle;
end;

end.
