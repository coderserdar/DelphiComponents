unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Menus, ImgList, ActnList, ComCtrls,
  {$IFNDEF UNICODE}
  TntActnList, TntDialogs, TntStdCtrls, TntSystem, TntForms,
  {$ENDIF}
  { TB2K }
  TB2Dock, TB2Toolbar, TB2Item, TB2ExtItems,
  SpTBXSkins, SpTBXItem, SpTBXControls, SpTBXDkPanels, SpTBXTabs, SpTBXEditors,
  SpTBXExtEditors, SpTBXCustomizer,
  { gettext }
  gnugettext;

type
  TForm1 = class(TForm)
    ImageList1: TImageList;
    SpTBXDock1: TSpTBXDock;
    SpTBXMultiDock1: TSpTBXMultiDock;
    SpTBXMultiDock2: TSpTBXMultiDock;
    SpTBXDock2: TSpTBXDock;
    dpLog: TSpTBXDockablePanel;
    dpHelp: TSpTBXDockablePanel;
    tbStandard: TSpTBXToolbar;
    tbFormat: TSpTBXToolbar;
    tbMenuBar: TSpTBXToolbar;
    tbNavigation: TSpTBXToolbar;
    mFile: TSpTBXSubmenuItem;
    mEdit: TSpTBXSubmenuItem;
    mView: TSpTBXSubmenuItem;
    mHelp: TSpTBXSubmenuItem;
    mNew: TSpTBXItem;
    mOpen: TSpTBXItem;
    mSave: TSpTBXItem;
    SpTBXSeparatorItem1: TSpTBXSeparatorItem;
    mExit: TSpTBXItem;
    mCut: TSpTBXItem;
    mCopy: TSpTBXItem;
    mPaste: TSpTBXItem;
    SpTBXSeparatorItem2: TSpTBXSeparatorItem;
    mSelectAll: TSpTBXItem;
    SpTBXSeparatorItem3: TSpTBXSeparatorItem;
    mFind: TSpTBXItem;
    mLeftJustify: TSpTBXItem;
    mUnderline: TSpTBXItem;
    mItalic: TSpTBXItem;
    mBold: TSpTBXItem;
    SpTBXSeparatorItem5: TSpTBXSeparatorItem;
    mRightJustify: TSpTBXItem;
    mCentered: TSpTBXItem;
    SpTBXSeparatorItem6: TSpTBXSeparatorItem;
    mBullets: TSpTBXItem;
    mNumberedBullets: TSpTBXItem;
    mSidebar: TSpTBXSubmenuItem;
    mOptions: TSpTBXItem;
    mmHelp: TSpTBXItem;
    mToolbars: TSpTBXSubmenuItem;
    SpTBXPopupMenu1: TSpTBXPopupMenu;
    mStandardToolbar: TSpTBXItem;
    mFormattingToolbar: TSpTBXItem;
    mNavigationToolbar: TSpTBXItem;
    mCommandsLog: TSpTBXItem;
    dpOptions: TSpTBXDockablePanel;
    mmmHelp: TSpTBXItem;
    mAbout: TSpTBXItem;
    tNew: TSpTBXItem;
    tOpen: TSpTBXItem;
    tSave: TSpTBXItem;
    tCut: TSpTBXItem;
    tCopy: TSpTBXItem;
    tPaste: TSpTBXItem;
    tBold: TSpTBXItem;
    SpTBXSeparatorItem8: TSpTBXSeparatorItem;
    tItalic: TSpTBXItem;
    tUnderline: TSpTBXItem;
    mPrint: TSpTBXItem;
    SpTBXSeparatorItem7: TSpTBXSeparatorItem;
    SpTBXSeparatorItem9: TSpTBXSeparatorItem;
    tFind: TSpTBXItem;
    tStop: TSpTBXItem;
    tRefresh: TSpTBXItem;
    tForward: TSpTBXItem;
    tBack: TSpTBXItem;
    tSearch: TSpTBXItem;
    pGroupItem1: TTBGroupItem;
    SpTBXSeparatorItem10: TSpTBXSeparatorItem;
    pCustomize: TSpTBXItem;
    tbLayouts: TSpTBXToolbar;
    tLayoutSave: TSpTBXItem;
    SpTBXLabelItem1: TSpTBXLabelItem;
    tLayoutsToolbar: TSpTBXItem;
    SpTBXCustomizer1: TSpTBXCustomizer;
    cPrint: TSpTBXItem;
    cLeftJustify: TSpTBXItem;
    cCentered: TSpTBXItem;
    cNumBullets: TSpTBXItem;
    cBullets: TSpTBXItem;
    cRightJustify: TSpTBXItem;
    cExit: TSpTBXItem;
    cSelectAll: TSpTBXItem;
    Memo1: TMemo;
    SpTBXSubmenuItem1: TSpTBXSubmenuItem;
    SpTBXTabControl1: TSpTBXTabControl;
    SpTBXTabItem1: TSpTBXTabItem;
    SpTBXTabSheet1: TSpTBXTabSheet;
    SpTBXTabItem2: TSpTBXTabItem;
    SpTBXTabSheet2: TSpTBXTabSheet;
    Memo2: TMemo;
    Memo3: TMemo;
    SpTBXSeparatorItem12: TSpTBXSeparatorItem;
    tSkins: TSpTBXSubmenuItem;
    SpTBXSkinGroupItem1: TSpTBXSkinGroupItem;
    pEmbeddedCustomize: TSpTBXItem;
    SpTBXLabel1: TSpTBXLabel;
    SpTBXSeparatorItem13: TSpTBXSeparatorItem;
    SpTBXTabItem3: TSpTBXTabItem;
    SpTBXTabSheet3: TSpTBXTabSheet;
    Memo4: TMemo;
    SpTBXSplitter1: TSpTBXSplitter;
    SpTBXSplitter2: TSpTBXSplitter;
    tLanguages: TSpTBXComboBox;
    TBControlItem1: TTBControlItem;
    tFont: TSpTBXComboBox;
    TBControlItem2: TTBControlItem;
    tFontSize: TSpTBXComboBox;
    TBControlItem3: TTBControlItem;
    tLayouts: TSpTBXComboBox;
    TBControlItem4: TTBControlItem;
    SpTBXStatusBar1: TSpTBXStatusBar;
    SpTBXColorEdit1: TSpTBXColorEdit;
    TBControlItem5: TTBControlItem;
    procedure ActionsExecute(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure tLayoutSaveClick(Sender: TObject);
    procedure tLayoutsItemClick(Sender: TObject);
    procedure aCustomizeExecute(Sender: TObject);
    procedure aEmbeddedCustomizeExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure tLanguagesItemClick(Sender: TObject);
    procedure SpTBXCustomizer1CreateCustomizeForm(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    FAppPath: string;
    FIniPath: string;
    procedure FillLayoutList(CurrentLayout: string = '');
  end;

var
  Form1: TForm1;

implementation

uses
  Unit2;

{$R *.dfm}

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ Form }

procedure TForm1.FormShow(Sender: TObject);
begin
  FAppPath := IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName));
  FIniPath := FAppPath + 'Options.ini';

  // Load the text files
  Memo2.Lines.LoadFromFile(FAppPath + 'faq.txt');              
  Memo3.Lines.LoadFromFile(FAppPath + 'advanced.txt');
  Memo4.Lines.LoadFromFile(FAppPath + 'translations.txt');

  // Load the items positions and the last layout from the ini file
  SpTBXCustomizer1.Load(FIniPath);

  // Load the layout list
  FillLayoutList('LastLayout');

  SpTBXCustomizer1.MenuBar := tbMenuBar;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  // Save the items positions and the current layout to the Ini file
  SpTBXCustomizer1.Save(FIniPath);
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ Layouts UI }

procedure TForm1.FillLayoutList(CurrentLayout: string);
var
  I: integer;
begin
  // Fill the tLayouts combobox
  tLayouts.Items.Clear;
  for I := 0 to SpTBXCustomizer1.Layouts.Count - 1 do
    tLayouts.Items.Add(SpTBXCustomizer1.Layouts[I]);
  I := tLayouts.Items.IndexOf(CurrentLayout);
  if I > -1 then
    tLayouts.ItemIndex := I;
end;

procedure TForm1.tLayoutsItemClick(Sender: TObject);
begin
  if tLayouts.ItemIndex > -1 then
    SpTBXCustomizer1.LoadLayout(FIniPath, tLayouts.Items[tLayouts.ItemIndex]);
end;

procedure TForm1.tLayoutSaveClick(Sender: TObject);
var
  S: string;
begin
  {$IFNDEF UNICODE}
  S := TntDialogs.WideInputBox(_('Save Layout'), _('Save current layout as:'), '');
  {$ELSE}
  S := InputBox(_('Save Layout'), _('Save current layout as:'), '');
  {$ENDIF}
  if S <> '' then begin
    SpTBXCustomizer1.SaveLayout(FIniPath, S);
    FillLayoutList(S);
  end;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ Actions }

procedure TForm1.ActionsExecute(Sender: TObject);
begin
  if Sender is TSpTBXItem then
    Memo1.Lines.Add(TSpTBXItem(Sender).Caption);
end;

procedure TForm1.aCustomizeExecute(Sender: TObject);
begin
  SpTBXCustomizer1.Show;
end;

procedure TForm1.aEmbeddedCustomizeExecute(Sender: TObject);
begin
  if not Form2.Visible and not SpTBXCustomizer1.Showing then begin
    SpTBXCustomizer1.ShowEmbedded(Form2.ClientPanel);
    Form2.Show;
    Form2.tCustomize.Click;
  end;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ Languages }

function MyWideCustomLoadResString(ResStringRec: PResStringRec; var Value: WideString): Boolean;
begin
  Result := True;
  Value := GnuGetText.LoadResStringW(ResStringRec);
end;

procedure SpDxGetTextInitialize(LanguageCode: string; AComponents: array of TComponent; ShellFont, UnicodeResourceStrings: Boolean);
// LanguageCode can be an ISO language code: 'en', 'es', 'ko'
// And also can be the ISO code plus a description: '[en] English', '[es] Spanish', '[ko] Korean'
var
  I, L: Integer;
begin
  // Get the ISO language code
  L := Length(LanguageCode);
  if (L > 2) and (LanguageCode[1] = '[') then begin
    I := Pos(']', LanguageCode);
    if (I > 0) then
      LanguageCode := Copy(LanguageCode, 2, I - 2);
  end;

  {$IFNDEF UNICODE}
  // Override Delphi's automatic ResourceString conversion to Ansi
  if UnicodeResourceStrings then begin
    TntSystem.InstallTntSystemUpdates;
    // Override TNT's LoadResString function
    // This is necessary because dxGetText uses a different
    // way to access the translated ResourceStrings.
    TntSystem.WideCustomLoadResString := MyWideCustomLoadResString;
  end;
  {$ENDIF}  

  if ShellFont then begin
    if  (Win32Platform = VER_PLATFORM_WIN32_NT) and (Win32MajorVersion >= 5) then
      DefFontData.Name := 'MS Shell Dlg 2'
    else
      DefFontData.Name := 'MS Shell Dlg';
  end;

  gnugettext.TP_GlobalIgnoreClassProperty(TAction,'Category');
  gnugettext.TP_GlobalIgnoreClassProperty(TControl,'HelpKeyword');
  gnugettext.TP_GlobalIgnoreClassProperty(TControl,'ImeName');
  gnugettext.TP_GlobalIgnoreClass(Graphics.TFont);
  gnugettext.TP_GlobalIgnoreClass(TSpTBXTabSheet);

  gnugettext.UseLanguage(LanguageCode);
  for I := Low(AComponents) to High(AComponents) do
    gnugettext.TranslateComponent(AComponents[I]);
end;

procedure SpDxGetTextChangeLanguage(LanguageCode: string; AComponents: array of TComponent);
// LanguageCode can be an ISO language code: 'en', 'es', 'ko'
// And also can be the ISO code plus a description: '[en] English', '[es] Spanish', '[ko] Korean'
var
  I, L: Integer;
  C: TComponent;
begin
  // Get the ISO language code
  L := Length(LanguageCode);
  if (L > 2) and (LanguageCode[1] = '[') then begin
    I := Pos(']', LanguageCode);
    if (I > 0) then
      LanguageCode := Copy(LanguageCode, 2, I - 2);
  end;

  if LanguageCode <> gnugettext.GetCurrentLanguage then begin
    gnugettext.UseLanguage(LanguageCode);
    for I := Low(AComponents) to High(AComponents) do begin
      C := AComponents[I];
      SpBeginUpdateAllToolbars(C);
      try
        gnugettext.ReTranslateComponent(C);
      finally
        SpEndUpdateAllToolbars(C);
      end;
    end;
  end;
end;

procedure TForm1.tLanguagesItemClick(Sender: TObject);
var
  I: integer;
begin
  I := tLanguages.ItemIndex;
  if I > -1 then begin
    tLanguages.Text := tLanguages.Items[I];
    // Change language and retranslate
    if SpTBXCustomizer1.Showing then
      SpDxGetTextChangeLanguage(tLanguages.Text, [Self, Form2, SpTBXCustomizer1.CustomizeForm])
    else
      SpDxGetTextChangeLanguage(tLanguages.Text, [Self, Form2]);
  end;
end;

procedure TForm1.SpTBXCustomizer1CreateCustomizeForm(Sender: TObject);
begin
  // Don't translate the skins combobox
  gnugettext.TP_Ignore(SpTBXCustomizer1.CustomizeForm, 'cbSkins');
  // Make sure the Customizer form is translated
  gnugettext.TranslateComponent(SpTBXCustomizer1.CustomizeForm);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  SpDxGetTextInitialize('en', [Self], True, True);
  tLanguages.Items.LoadFromFile('langcodes.txt');
  tLanguages.ItemIndex := 2;
end;

end.
