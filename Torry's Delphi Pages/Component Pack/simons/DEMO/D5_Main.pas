unit D5_Main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  RackCtls, StdCtrls, Spin, ExtCtrls, ComCtrls, ColorGrd, Buttons, SRWave,
  EnhEdit, OvalBtn, SRCal, Grids, SRColBtn, SRGrad, SRLabel, SRDlgs,
  SRChkBox, Srclock, IniList, SRValEdt, FileBtn, SRFntCtl;

type
  TMainForm = class(TForm)
    PC: TPageControl;
    TSLEDDisplay: TTabSheet;
    ScrewPanel1: TScrewPanel;
    LEDDisplay1: TLEDDisplay;
    LEDDisplay2: TLEDDisplay;
    Label1: TLabel;
    EditValue: TEdit;
    Label2: TLabel;
    SpinEditNumDigits: TSpinEdit;
    Label3: TLabel;
    SpinEditFractionDigits: TSpinEdit;
    CBLeadingZeros: TCheckBox;
    Label4: TLabel;
    ComboBoxSS: TComboBox;
    Label5: TLabel;
    ComboBoxBevel: TComboBox;
    Label6: TLabel;
    ComboBoxDecSeparator: TComboBox;
    TSLEDButton: TTabSheet;
    LEDButton1: TLEDButton;
    ButtonPanel2: TButtonPanel;
    ButtonPanel3: TButtonPanel;
    LEDButton2: TLEDButton;
    ButtonPanel4: TButtonPanel;
    ButtonPanel1: TButtonPanel;
    TestButton: TLEDButton;
    ColorGrid: TColorGrid;
    Label7: TLabel;
    ComboBoxBD: TComboBox;
    Label8: TLabel;
    CBSwitching: TCheckBox;
    Label9: TLabel;
    ComboBoxTP: TComboBox;
    CBBeveled: TCheckBox;
    CBShowLED: TCheckBox;
    LabelClick: TLabel;
    Bevel1: TBevel;
    TSLEDMeter: TTabSheet;
    LabelPosition: TLabel;
    Label11: TLabel;
    TBPosition: TTrackBar;
    LEDMeter1: TLEDMeter;
    RGDirection: TRadioGroup;
    LEDMeter2: TLEDMeter;
    SpinEditSection2Value: TSpinEdit;
    Label10: TLabel;
    Label12: TLabel;
    SpinEditSection3Value: TSpinEdit;
    TSEnhancedEdit: TTabSheet;
    CBSingleLED: TCheckBox;
    TSOvalButton: TTabSheet;
    TSSRGradient: TTabSheet;
    TSSRCalendar: TTabSheet;
    TSSRClock: TTabSheet;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    EnhEdit: TEnhancedEdit;
    RGEnhAlignment: TRadioGroup;
    SpinEnhDigits: TSpinEdit;
    EditEnhValueInt: TEdit;
    EditEnhValue: TEdit;
    CBEnhEnabled: TCheckBox;
    CBEnhGrayDisabled: TCheckBox;
    ComboEnhFormat: TComboBox;
    Hintergrund: TImage;
    OvalButton1: TOvalButton;
    OvalButton2: TOvalButton;
    OvalButton3: TOvalButton;
    OvalButton4: TOvalButton;
    OvalButton5: TOvalButton;
    OvalButton6: TOvalButton;
    OvalButton7: TOvalButton;
    OvalButton8: TOvalButton;
    Label19: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Uhr: TSRClock;
    Label23: TLabel;
    Label24: TLabel;
    Label25: TLabel;
    EditZeit: TEdit;
    ComboClockStyle: TComboBox;
    ComboClockKind: TComboBox;
    CBAutoUpdate: TCheckBox;
    SBStart: TSpeedButton;
    SBStop: TSpeedButton;
    SBReset: TSpeedButton;
    LblDatum: TLabel;
    Label30: TLabel;
    Label31: TLabel;
    LblFeiertag: TLabel;
    LblSternzeichen: TLabel;
    Label32: TLabel;
    LblTagImJahr: TLabel;
    LblWocheImJahr: TLabel;
    LblMonat: TLabel;
    LblJahr: TLabel;
    Label33: TLabel;
    Label34: TLabel;
    SpinMonat: TSpinButton;
    SpinJahr: TSpinButton;
    ComboAstroData: TComboBox;
    TSExtDialogs: TTabSheet;
    SBCSVOpenDlg: TSpeedButton;
    SBCSVSaveDlg: TSpeedButton;
    SBExtSaveDlg: TSpeedButton;
    SBExtOpenDlg: TSpeedButton;
    SBSliderSaveDlg: TSpeedButton;
    SBSliderOpenDlg: TSpeedButton;
    ExtOpenDlg: TExtOpenDialog;
    ExtSaveDlg: TExtSaveDialog;
    CSVSaveDlg: TCSVSaveDialog;
    SliderOpenDlg: TSliderOpenDialog;
    SliderSaveDlg: TSliderSaveDialog;
    CBDlgShowHelp: TCheckBox;
    CBDlgHideReadOnly: TCheckBox;
    Label37: TLabel;
    SpinLEDDisplayContrast: TSpinEdit;
    Label38: TLabel;
    SpinLEDMeterContrast: TSpinEdit;
    Label39: TLabel;
    SpinClockContrast: TSpinEdit;
    CSVOpenDlg: TCSVOpenDialog;
    TSSRCheckBox: TTabSheet;
    TSSRColorButton: TTabSheet;
    SRColorButton1: TSRColorButton;
    SRColorButton2: TSRColorButton;
    SpinContrastShadow: TSpinEdit;
    SpinContrastHighlight: TSpinEdit;
    Label40: TLabel;
    Label41: TLabel;
    Label42: TLabel;
    SpinBevelWidth: TSpinEdit;
    TSSRLabel: TTabSheet;
    SRLabel1: TSRLabel;
    Label35: TLabel;
    Label36: TLabel;
    CBLblShowHighlight: TCheckBox;
    SpinLblHighlightOffset: TSpinEdit;
    CBLblShowShadow: TCheckBox;
    SpinLblShadowOffset: TSpinEdit;
    SRLabel2: TSRLabel;
    Bevel2: TBevel;
    CBLblLinkActive: TCheckBox;
    CBLblUnderlineOnEnter: TCheckBox;
    CBLblUnderlined: TCheckBox;
    CBLblEnabled: TCheckBox;
    SRLabel3: TSRLabel;
    RGLblLayout: TRadioGroup;
    RGLblAlignment: TRadioGroup;
    TSInfo: TTabSheet;
    LblTitel: TSRLabel;
    Bevel4: TBevel;
    LblAutor: TSRLabel;
    LbleMail: TSRLabel;
    LblInternet: TSRLabel;
    LblInfo: TSRLabel;
    Label43: TLabel;
    ComboDrawStyle: TComboBox;
    SRColorButton3: TSRColorButton;
    SRColorButton4: TSRColorButton;
    Label44: TLabel;
    Label45: TLabel;
    PanelVersion: TPanel;
    Shape: TShape;
    Label46: TLabel;
    Label47: TLabel;
    Label48: TLabel;
    SRCheckBox: TSRCheckBox;
    CBAllowGrayed: TCheckBox;
    ComboCBStyle: TComboBox;
    SpinCheckSize: TSpinEdit;
    CBAutoSize: TCheckBox;
    CBTransparent: TCheckBox;
    SpinSpacing: TSpinEdit;
    CBWordWrap: TCheckBox;
    CBEnhAcceptChars: TCheckBox;
    SRGradient1: TSRGradient;
    SRGradient2: TSRGradient;
    SRGradient3: TSRGradient;
    SRGradient4: TSRGradient;
    Label26: TLabel;
    Label27: TLabel;
    Label28: TLabel;
    Label29: TLabel;
    Label18: TLabel;
    Label49: TLabel;
    SRGradient5: TSRGradient;
    SRGradient6: TSRGradient;
    CBAllowAllUp1: TCheckBox;
    CBAllowAllUp2: TCheckBox;
    CBAstroDaten: TCheckBox;
    CBDeleteMarks: TCheckBox;
    CBShowMarks: TCheckBox;
    CBShowHolidays: TCheckBox;
    CBGridLines: TCheckBox;
    CBFrameSelection: TCheckBox;
    SRLabel4: TSRLabel;
    SpinLabelWidth: TSpinButton;
    TSSRValueEdit: TTabSheet;
    TSIniList: TTabSheet;
    LblSeparator: TLabel;
    SBSeparator: TSpeedButton;
    LblSection: TLabel;
    LblKey: TLabel;
    LblValue: TLabel;
    LblSectionCount: TLabel;
    LblDescrSectionCount: TLabel;
    LblDescrKeyCount: TLabel;
    LblKeyCount: TLabel;
    Bevel3: TBevel;
    BtnLoadFromFile: TButton;
    EditSeparator: TEdit;
    EditSection: TEdit;
    EditKey: TEdit;
    Edit1: TEdit;
    BtnWriteString: TButton;
    BtnDeleteKey: TButton;
    BtnEraseSection: TButton;
    ListBox: TListBox;
    Panel1: TPanel;
    CBOnSectionChange: TCheckBox;
    CBOnKeyChange: TCheckBox;
    CBOnValueChange: TCheckBox;
    OpenDlg: TOpenDialog;
    Timer: TTimer;
    Panel2: TPanel;
    SRValueEdit2: TSRValueEdit;
    SRValueEdit3: TSRValueEdit;
    SRValueEdit1: TSRValueEdit;
    Label50: TLabel;
    ComboHighlightPos: TComboBox;
    ComboBevelStyle: TComboBox;
    SpinLblBevelWidth: TSpinEdit;
    Label51: TLabel;
    CBHoverActive: TCheckBox;
    CBUnderlineOnEnter: TCheckBox;
    ComboCBAlignment: TComboBox;
    Label52: TLabel;
    ComboCBLayout: TComboBox;
    Label53: TLabel;
    Kalender: TSRCalendar;
    ComboGermanState: TComboBox;
    ComboStartTag: TComboBox;
    CBSatAsSun: TCheckBox;
    LblSeason: TLabel;
    TSSRWavePlayer: TTabSheet;
    TSFileButton: TTabSheet;
    SBWaveStart: TSpeedButton;
    SBWaveStop: TSpeedButton;
    CBWaveAsync: TCheckBox;
    CBWaveLoop: TCheckBox;
    FileButton: TFileButton;
    CBFBAsynchronous: TCheckBox;
    CBFBFlat: TCheckBox;
    CBFBShowFilename: TCheckBox;
    EditFBFileName: TEdit;
    Label54: TLabel;
    SEFBBevelContrast: TSpinEdit;
    Label55: TLabel;
    Label56: TLabel;
    SEFBBevelWidth: TSpinEdit;
    CBFBShowIconFrame: TCheckBox;
    CBFBTransparent: TCheckBox;
    LblIniFileName: TSRLabel;
    TSFontControls: TTabSheet;
    SRFontComboBox: TSRFontComboBox;
    SRFontListBox: TSRFontListBox;
    Label57: TLabel;
    Label58: TLabel;
    CBFCUseItemFont: TCheckBox;
    CBFCShowTTSymbol: TCheckBox;
    GBFCFontMask: TGroupBox;
    CBFCVariablePitch: TCheckBox;
    CBFCFixedPitch: TCheckBox;
    CBFCNoTrueType: TCheckBox;
    CBFCTrueType: TCheckBox;
    CBDrawDigitShapes: TCheckBox;
    Label59: TLabel;
    Label60: TLabel;
    Label61: TLabel;
    ComboClockBevel: TComboBox;
    SEClockBevel: TSpinEdit;
    ComboSeparator: TComboBox;
    SRWavePlayer: TSRWavePlayer;
    procedure FormCreate(Sender: TObject);
    procedure EditValueChange(Sender: TObject);
    procedure SpinEditNumDigitsChange(Sender: TObject);
    procedure SpinEditFractionDigitsChange(Sender: TObject);
    procedure ComboBoxSSChange(Sender: TObject);
    procedure ComboBoxBevelChange(Sender: TObject);
    procedure CBLeadingZerosClick(Sender: TObject);
    procedure ComboBoxDecSeparatorChange(Sender: TObject);
    procedure TestButtonClick(Sender: TObject);
    procedure ColorGridChange(Sender: TObject);
    procedure ComboBoxBDChange(Sender: TObject);
    procedure ComboBoxTPChange(Sender: TObject);
    procedure CBShowLEDClick(Sender: TObject);
    procedure CBBeveledClick(Sender: TObject);
    procedure CBSwitchingClick(Sender: TObject);
    procedure LEDMeter1Change(Sender: TObject);
    procedure TBPositionChange(Sender: TObject);
    procedure RGDirectionClick(Sender: TObject);
    procedure SpinEditSection2ValueChange(Sender: TObject);
    procedure SpinEditSection3ValueChange(Sender: TObject);
    procedure CBSingleLEDClick(Sender: TObject);
    procedure SBWaveStartClick(Sender: TObject);
    procedure SBWaveStopClick(Sender: TObject);
    procedure SRWavePlayerAfterPlay(Sender: TObject);
    procedure CBWaveAsyncClick(Sender: TObject);
    procedure CBWaveLoopClick(Sender: TObject);
    procedure SRWavePlayerBeforePlay(Sender: TObject);
    procedure EnhEditKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure CBEnhEnabledClick(Sender: TObject);
    procedure CBEnhGrayDisabledClick(Sender: TObject);
    procedure RGEnhAlignmentClick(Sender: TObject);
    procedure EditEnhValueKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure EditEnhValueIntKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure SpinEnhDigitsChange(Sender: TObject);
    procedure ComboEnhFormatChange(Sender: TObject);
    procedure OvalButton5Click(Sender: TObject);
    procedure EditZeitChange(Sender: TObject);
    procedure CBAutoUpdateClick(Sender: TObject);
    procedure ComboClockStyleChange(Sender: TObject);
    procedure ComboClockKindChange(Sender: TObject);
    procedure SBStartClick(Sender: TObject);
    procedure SBStopClick(Sender: TObject);
    procedure SBResetClick(Sender: TObject);
    procedure SpinMonatDownClick(Sender: TObject);
    procedure SpinMonatUpClick(Sender: TObject);
    procedure SpinJahrDownClick(Sender: TObject);
    procedure SpinJahrUpClick(Sender: TObject);
    procedure ComboStartTagChange(Sender: TObject);
    procedure CBShowHolidaysClick(Sender: TObject);
    procedure CBShowMarksClick(Sender: TObject);
    procedure CBDeleteMarksClick(Sender: TObject);
    procedure CBAstroDatenClick(Sender: TObject);
    procedure KalenderDblClick(Sender: TObject);
    procedure KalenderChange(Sender: TObject);
    procedure CBLblShowHighlightClick(Sender: TObject);
    procedure CBLblShowShadowClick(Sender: TObject);
    procedure SpinLblHighlightOffsetChange(Sender: TObject);
    procedure SpinLblShadowOffsetChange(Sender: TObject);
    procedure SBCSVOpenDlgClick(Sender: TObject);
    procedure SBCSVSaveDlgClick(Sender: TObject);
    procedure SBExtOpenDlgClick(Sender: TObject);
    procedure SBExtSaveDlgClick(Sender: TObject);
    procedure SBSliderOpenDlgClick(Sender: TObject);
    procedure SBSliderSaveDlgClick(Sender: TObject);
    procedure CBDlgShowHelpClick(Sender: TObject);
    procedure CBDlgHideReadOnlyClick(Sender: TObject);
    procedure SpinLEDDisplayContrastChange(Sender: TObject);
    procedure SpinLEDMeterContrastChange(Sender: TObject);
    procedure SpinClockContrastChange(Sender: TObject);
    procedure SpinContrastHighlightChange(Sender: TObject);
    procedure SpinContrastShadowChange(Sender: TObject);
    procedure SpinBevelWidthChange(Sender: TObject);
    procedure CBLblLinkActiveClick(Sender: TObject);
    procedure CBLblUnderlineOnEnterClick(Sender: TObject);
    procedure CBLblUnderlinedClick(Sender: TObject);
    procedure CBLblEnabledClick(Sender: TObject);
    procedure RGLblAlignmentClick(Sender: TObject);
    procedure RGLblLayoutClick(Sender: TObject);
    procedure ComboDrawStyleChange(Sender: TObject);
    procedure SRColorButton3Click(Sender: TObject);
    procedure CBAllowGrayedClick(Sender: TObject);
    procedure CBTransparentClick(Sender: TObject);
    procedure CBAutoSizeClick(Sender: TObject);
    procedure CBWordWrapClick(Sender: TObject);
    procedure ComboCBStyleChange(Sender: TObject);
    procedure SpinCheckSizeChange(Sender: TObject);
    procedure SpinSpacingChange(Sender: TObject);
    procedure SRCheckBoxClick(Sender: TObject);
    procedure CBEnhAcceptCharsClick(Sender: TObject);
    procedure CBAllowAllUp1Click(Sender: TObject);
    procedure CBAllowAllUp2Click(Sender: TObject);
    procedure CBGridLinesClick(Sender: TObject);
    procedure CBFrameSelectionClick(Sender: TObject);
    procedure SpinLabelWidthDownClick(Sender: TObject);
    procedure SpinLabelWidthUpClick(Sender: TObject);
    procedure BtnLoadFromFileClick(Sender: TObject);
    procedure EditSeparatorChange(Sender: TObject);
    procedure SBSeparatorClick(Sender: TObject);
    procedure EditSeparatorKeyPress(Sender: TObject; var Key: Char);
    procedure EditSectionChange(Sender: TObject);
    procedure EditSectionKeyPress(Sender: TObject; var Key: Char);
    procedure EditKeyChange(Sender: TObject);
    procedure BtnEraseSectionClick(Sender: TObject);
    procedure BtnDeleteKeyClick(Sender: TObject);
    procedure BtnWriteStringClick(Sender: TObject);
    procedure ListBoxClick(Sender: TObject);
    procedure CBOnSectionChangeClick(Sender: TObject);
    procedure IniListChange(Sender: TObject);
    procedure IniListKeyChange(Sender: TObject);
    procedure IniListSectionChange(Sender: TObject);
    procedure IniListValueChange(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure ComboHighlightPosChange(Sender: TObject);
    procedure ComboBevelStyleChange(Sender: TObject);
    procedure SpinLblBevelWidthChange(Sender: TObject);
    procedure ComboCBAlignmentChange(Sender: TObject);
    procedure ComboCBLayoutChange(Sender: TObject);
    procedure CBHoverActiveClick(Sender: TObject);
    procedure CBUnderlineOnEnterClick(Sender: TObject);
    procedure CBSatAsSunClick(Sender: TObject);
    procedure ComboGermanStateChange(Sender: TObject);
    procedure EditFBFileNameChange(Sender: TObject);
    procedure CBFBAsynchronousClick(Sender: TObject);
    procedure CBFBFlatClick(Sender: TObject);
    procedure CBFBShowFilenameClick(Sender: TObject);
    procedure CBFBShowIconFrameClick(Sender: TObject);
    procedure CBFBTransparentClick(Sender: TObject);
    procedure SEFBBevelContrastChange(Sender: TObject);
    procedure SEFBBevelWidthChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure CBFCUseItemFontClick(Sender: TObject);
    procedure CBFCShowTTSymbolClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure CBFCTrueTypeClick(Sender: TObject);
    procedure CBDrawDigitShapesClick(Sender: TObject);
    procedure ComboSeparatorChange(Sender: TObject);
    procedure ComboClockBevelChange(Sender: TObject);
    procedure SEClockBevelChange(Sender: TObject);
  private
    { Private-Deklarationen }
    Counter : integer;
    IniList : TIniList;
    procedure SetCheckBoxSize;
    procedure ShowIniListInMemo;
    procedure UpdateButtonStates;
  public
    { Public-Deklarationen }
  end;

var
  MainForm : TMainForm;

implementation

{$R *.DFM}

uses SRUtils;

const Timeout = 10;

function GetPackageVersionNr:string;
var AText : TStringList;
    ALine,
    APath : string;
    i,P   : integer;
begin
  Result:='Unbekannt';
  AText:=TStringList.Create;
  try
    APath:=ExtractFileDir(Application.ExeName);
    P:=LastDelimiter('\', APath);
    if P>0 then begin
      delete(APath, P+1, length(APath)-P);
      APath:=APath+'Liesmich.txt';
      if FileExists(APath) then begin
        AText.LoadFromFile(APath);
        i:=0;
        while (i<AText.Count) and (Pos('Version', AText[i])=0) do
          inc(i);
        if Pos('Version', AText[i])>0 then begin
          ALine:=AText[i];
          P:=Pos(':', ALine);
          if P>0 then
            delete(ALine, 1, P);
          ALine:=Trim(ALine);
          if ALine<>'' then
            Result:=ALine;
        end;
      end;
    end;
  finally
    AText.Free;
  end;
end;

function GetInfoFileName:string;
var APath : string;
    P     : integer;
begin
  Result:='';
  APath:=ExtractFileDir(Application.ExeName);
  P:=LastDelimiter('\', APath);
  if P>0 then begin
    delete(APath, P+1, length(APath)-P);
    Result:=APath+'Liesmich.txt';
  end;
end;

procedure TMainForm.SetCheckBoxSize;
begin
  if not CBAutoSize.Checked then begin
    if CBWordWrap.Checked then
      SRCheckBox.SetBounds(112, 8, 120, 70)
    else
      SRCheckBox.SetBounds(112, 8, 240, 70);
  end
  else begin
    SRCheckBox.AutoSize:=false;
    SRCheckBox.Width:=200;
    SRCheckBox.AutoSize:=true;
  end;
end;

{ TEnhancedEdit }
procedure TMainForm.EnhEditKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  try
    EditEnhValue.Text:=FloatToStr(EnhEdit.Value);
    EditEnhValueInt.Text:=IntToStr(EnhEdit.ValueInt);
  except
  end;
end;

procedure TMainForm.CBEnhEnabledClick(Sender: TObject);
begin
  EnhEdit.Enabled:=CBEnhEnabled.Checked;
end;

procedure TMainForm.CBEnhGrayDisabledClick(Sender: TObject);
begin
  EnhEdit.GrayDisabled:=CBEnhGrayDisabled.Checked;
end;

procedure TMainForm.RGEnhAlignmentClick(Sender: TObject);
begin
  case RGEnhAlignment.ItemIndex of
    0 : EnhEdit.Alignment:=taLeftJustify;
    1 : EnhEdit.Alignment:=taCenter;
    2 : EnhEdit.Alignment:=taRightJustify;
  end;
end;

procedure TMainForm.EditEnhValueKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  try
    EnhEdit.Value:=StrToFloat(EditEnhValue.Text);
    EditEnhValueInt.Text:=IntToStr(EnhEdit.ValueInt);
  except
  end;
end;

procedure TMainForm.EditEnhValueIntKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  try
    EnhEdit.ValueInt:=StrToInt(EditEnhValueInt.Text);
    EditEnhValue.Text:=FloatToStr(EnhEdit.Value);
  except
  end;
end;

procedure TMainForm.SpinEnhDigitsChange(Sender: TObject);
begin
  EnhEdit.Digits:=SpinEnhDigits.Value;
end;

procedure TMainForm.CBEnhAcceptCharsClick(Sender: TObject);
begin
  EnhEdit.AcceptChars:=CBEnhAcceptChars.Checked;
end;

procedure TMainForm.ComboEnhFormatChange(Sender: TObject);
begin
  case ComboEnhFormat.ItemIndex of
    0 : EnhEdit.Format:=ffCurrency;
    1 : EnhEdit.Format:=ffExponent;
    2 : EnhEdit.Format:=ffFixed;
    3 : EnhEdit.Format:=ffGeneral;
    4 : EnhEdit.Format:=ffNumber;
  end;
end;

{ TSRLabel }
procedure TMainForm.CBLblShowHighlightClick(Sender: TObject);
begin
  SRLabel1.ShowHighlight:=CBLblShowHighlight.Checked;
end;

procedure TMainForm.CBLblShowShadowClick(Sender: TObject);
begin
  SRLabel1.ShowShadow:=CBLblShowShadow.Checked;
end;

procedure TMainForm.SpinLblHighlightOffsetChange(Sender: TObject);
begin
  SRLabel1.HighlightOffset:=SpinLblHighlightOffset.Value;
end;

procedure TMainForm.SpinLblShadowOffsetChange(Sender: TObject);
begin
  SRLabel1.ShadowOffset:=SpinLblShadowOffset.Value;
end;

procedure TMainForm.ComboHighlightPosChange(Sender: TObject);
begin
  if ComboHighlightPos.ItemIndex>=0 then
    SRLabel1.HighlightPos:=THighLightPos(ComboHighlightPos.ItemIndex);
end;

procedure TMainForm.ComboBevelStyleChange(Sender: TObject);
begin
  if ComboBevelStyle.ItemIndex>=0 then
    SRLabel1.BevelStyle:=TLabelBevel(ComboBevelStyle.ItemIndex);
end;

procedure TMainForm.SpinLblBevelWidthChange(Sender: TObject);
begin
  SRLabel1.BevelWidth:=SpinLblBevelWidth.Value;
end;

procedure TMainForm.CBLblEnabledClick(Sender: TObject);
begin
  SRLabel1.Enabled:=CBLblEnabled.Checked;
end;

procedure TMainForm.CBLblLinkActiveClick(Sender: TObject);
begin
  SRLabel2.LinkActive:=CBLblLinkActive.Checked;
end;

procedure TMainForm.CBLblUnderlineOnEnterClick(Sender: TObject);
begin
  SRLabel2.UnderlineOnEnter:=CBLblUnderlineOnEnter.Checked;
end;

procedure TMainForm.CBLblUnderlinedClick(Sender: TObject);
begin
  if CBLblUnderlined.Checked then
    SRLabel2.Font.Style:=[fsBold,fsUnderline]
  else
    SRLabel2.Font.Style:=[fsBold];
end;

procedure TMainForm.RGLblAlignmentClick(Sender: TObject);
begin
  case RGLblAlignment.ItemIndex of
    0 : SRLabel3.Alignment:=taLeftJustify;
    1 : SRLabel3.Alignment:=taCenter;
    2 : SRLabel3.Alignment:=taRightJustify;
  end;
end;

procedure TMainForm.RGLblLayoutClick(Sender: TObject);
begin
  case RGLblLayout.ItemIndex of
    0 : SRLabel3.Layout:=tlTop;
    1 : SRLabel3.Layout:=tlCenter;
    2 : SRLabel3.Layout:=tlBottom;
  end;
end;

procedure TMainForm.SpinLabelWidthDownClick(Sender: TObject);
begin
  if SRLabel4.Width>80 then
    SRLabel4.Width:=SRLabel4.Width-10;
end;

procedure TMainForm.SpinLabelWidthUpClick(Sender: TObject);
begin
  if SRLabel4.Width<(ClientWidth-47) then
    SRLabel4.Width:=SRLabel4.Width+10;
end;

{ TLEDButton }
procedure TMainForm.TestButtonClick(Sender: TObject);
begin
  LabelClick.Visible:=not LabelClick.Visible;
end;

procedure TMainForm.ColorGridChange(Sender: TObject);
begin
  TestButton.Color:=ColorGrid.ForegroundColor;
end;

procedure TMainForm.ComboBoxBDChange(Sender: TObject);
begin
  case ComboBoxBD.ItemIndex of
    0 : TestButton.ButtonDirection:=bdBottomUp;
    1 : TestButton.ButtonDirection:=bdLeftUp;
    2 : TestButton.ButtonDirection:=bdNone;
    3 : TestButton.ButtonDirection:=bdRightUp;
    4 : TestButton.ButtonDirection:=bdTopUp;
  end;
end;

procedure TMainForm.ComboBoxTPChange(Sender: TObject);
begin
  case ComboBoxTP.ItemIndex of
    0 : TestButton.TextPosition:=tpAbove;
    1 : TestButton.TextPosition:=tpBelow;
    2 : TestButton.TextPosition:=tpNone;
    3 : TestButton.TextPosition:=tpOnButton;
  end;
end;

procedure TMainForm.CBShowLEDClick(Sender: TObject);
begin
  TestButton.ShowLED:=CBShowLED.Checked;
end;

procedure TMainForm.CBBeveledClick(Sender: TObject);
begin
  TestButton.Beveled:=CBBeveled.Checked;
end;

procedure TMainForm.CBSwitchingClick(Sender: TObject);
begin
  TestButton.Switching:=CBSwitching.Checked;
end;

{ TLEDDisplay }
procedure TMainForm.EditValueChange(Sender: TObject);
begin
  try
    LEDDisplay1.Value:=StrToFloat(EditValue.Text);
    LEDDisplay2.Value:=StrToFloat(EditValue.Text);
  except
  end;
end;

procedure TMainForm.SpinEditNumDigitsChange(Sender: TObject);
begin
  LEDDisplay1.NumDigits:=SpinEditNumDigits.Value;
  LEDDisplay2.NumDigits:=SpinEditNumDigits.Value;
end;

procedure TMainForm.SpinEditFractionDigitsChange(Sender: TObject);
begin
  LEDDisplay1.FractionDigits:=SpinEditFractionDigits.Value;
  LEDDisplay2.FractionDigits:=SpinEditFractionDigits.Value;
end;

procedure TMainForm.CBLeadingZerosClick(Sender: TObject);
begin
  LEDDisplay1.LeadingZeros:=CBLeadingZeros.Checked;
  LEDDisplay2.LeadingZeros:=CBLeadingZeros.Checked;
end;

procedure TMainForm.ComboBoxSSChange(Sender: TObject);
begin
  if ComboBoxSS.ItemIndex>=0 then begin
    LEDDisplay1.SegmentStyle:=TSegmentStyle(ComboBoxSS.ItemIndex);
    LEDDisplay2.SegmentStyle:=TSegmentStyle(ComboBoxSS.ItemIndex);
  end;
end;

procedure TMainForm.ComboBoxBevelChange(Sender: TObject);
begin
  if ComboBoxBevel.ItemIndex>=0 then begin
    LEDDisplay1.BevelStyle:=TBevelCut(ComboBoxBevel.ItemIndex);
    LEDDisplay2.BevelStyle:=TBevelCut(ComboBoxBevel.ItemIndex);
  end;
end;

procedure TMainForm.ComboBoxDecSeparatorChange(Sender: TObject);
begin
  if ComboBoxDecSeparator.ItemIndex>=0 then begin
    LEDDisplay1.DecSeparator:=TDecSeparator(ComboBoxDecSeparator.ItemIndex);
    LEDDisplay2.DecSeparator:=TDecSeparator(ComboBoxDecSeparator.ItemIndex);
  end;
end;

procedure TMainForm.SpinLEDDisplayContrastChange(Sender: TObject);
begin
  LEDDisplay1.LEDContrast:=SpinLEDDisplayContrast.Value;
  LEDDisplay2.LEDContrast:=SpinLEDDisplayContrast.Value;
end;

procedure TMainForm.CBDrawDigitShapesClick(Sender: TObject);
begin
  LEDDisplay1.DrawDigitShapes:=CBDrawDigitShapes.Checked;
  LEDDisplay2.DrawDigitShapes:=CBDrawDigitShapes.Checked;
end;

{ TLEDMeter }
procedure TMainForm.LEDMeter1Change(Sender: TObject);
begin
  LabelPosition.Caption:=IntToStr(LEDMeter1.Position);
end;

procedure TMainForm.TBPositionChange(Sender: TObject);
begin
  LEDMeter1.Position:=TBPosition.Position;
  LEDMeter2.Position:=TBPosition.Position;
end;

procedure TMainForm.RGDirectionClick(Sender: TObject);
begin
  if RGDirection.ItemIndex=0 then begin
    LEDMeter1.Direction:=mdRight;
    LEDMeter2.Direction:=mdUp;
  end
  else begin
    LEDMeter1.Direction:=mdLeft;
    LEDMeter2.Direction:=mdDown;
  end;
end;

procedure TMainForm.CBSingleLEDClick(Sender: TObject);
begin
  LEDMeter1.SingleLED:=CBSingleLED.Checked;
  LEDMeter2.SingleLED:=CBSingleLED.Checked;
end;

procedure TMainForm.SpinLEDMeterContrastChange(Sender: TObject);
begin
  LEDMeter1.LEDContrast:=SpinLEDMeterContrast.Value;
  LEDMeter2.LEDContrast:=SpinLEDMeterContrast.Value;
end;

procedure TMainForm.SpinEditSection2ValueChange(Sender: TObject);
begin
  LEDMeter1.Section2Value:=SpinEditSection2Value.Value;
  LEDMeter2.Section2Value:=SpinEditSection2Value.Value;
  SpinEditSection2Value.Value:=LEDMeter1.Section2Value;
end;

procedure TMainForm.SpinEditSection3ValueChange(Sender: TObject);
begin
  LEDMeter1.Section3Value:=SpinEditSection3Value.Value;
  LEDMeter2.Section3Value:=SpinEditSection3Value.Value;
  SpinEditSection3Value.Value:=LEDMeter1.Section3Value;
end;

{ TSRWavePlayer }
procedure TMainForm.SBWaveStartClick(Sender: TObject);
begin
  SRWavePlayer.Play;
end;

procedure TMainForm.SBWaveStopClick(Sender: TObject);
begin
  SRWavePlayer.Stop;
  CBWaveLoop.Enabled:=true;
  CBWaveAsync.Enabled:=true;
end;

procedure TMainForm.SRWavePlayerAfterPlay(Sender: TObject);
begin
  SBWaveStart.Down:=false;
  SBWaveStop.Down:=true;
  SBWaveStop.Enabled:=true;
  if not SRWavePlayer.Loop then begin
    CBWaveAsync.Enabled:=true;
    CBWaveLoop.Enabled:=true;
  end;
end;

procedure TMainForm.SRWavePlayerBeforePlay(Sender: TObject);
begin
  CBWaveAsync.Enabled:=false;
  CBWaveLoop.Enabled:=false;
  SBWaveStop.Enabled:=false;
end;

procedure TMainForm.CBWaveAsyncClick(Sender: TObject);
begin
  CBWaveLoop.Enabled:=CBWaveAsync.Checked;
  SRWavePlayer.Async:=CBWaveAsync.Checked;
end;

procedure TMainForm.CBWaveLoopClick(Sender: TObject);
begin
  SRWavePlayer.Loop:=CBWaveLoop.Checked;
end;

{ TSRCheckBox }
procedure TMainForm.SRCheckBoxClick(Sender: TObject);
begin
  MessageBeep(0);
end;

procedure TMainForm.CBAllowGrayedClick(Sender: TObject);
begin
  SRCheckBox.AllowGrayed:=CBAllowGrayed.Checked;
end;

procedure TMainForm.CBTransparentClick(Sender: TObject);
begin
  SRCheckBox.Transparent:=CBTransparent.Checked;
end;

procedure TMainForm.CBAutoSizeClick(Sender: TObject);
begin
  SRCheckBox.AutoSize:=CBAutoSize.Checked;
  SetCheckBoxSize;
end;

procedure TMainForm.CBWordWrapClick(Sender: TObject);
begin
  SRCheckBox.WordWrap:=CBWordWrap.Checked;
  SetCheckBoxSize;
end;

procedure TMainForm.CBHoverActiveClick(Sender: TObject);
begin
  SRCheckBox.HoverActive:=CBHoverActive.Checked;
end;

procedure TMainForm.CBUnderlineOnEnterClick(Sender: TObject);
begin
  SRCheckBox.UnderlineOnEnter:=CBUnderlineOnEnter.Checked;
end;

procedure TMainForm.ComboCBStyleChange(Sender: TObject);
begin
  SRCheckBox.Style:=TCheckStyle(ComboCBStyle.ItemIndex);
end;

procedure TMainForm.ComboCBAlignmentChange(Sender: TObject);
begin
  SRCheckBox.Alignment:=TLeftRight(ComboCBAlignment.ItemIndex);
end;

procedure TMainForm.ComboCBLayoutChange(Sender: TObject);
begin
  SRCheckBox.Layout:=TCheckboxLayout(ComboCBLayout.ItemIndex);
end;

procedure TMainForm.SpinCheckSizeChange(Sender: TObject);
begin
  SRCheckBox.CheckSize:=SpinCheckSize.Value;
end;

procedure TMainForm.SpinSpacingChange(Sender: TObject);
begin
  SRCheckBox.Spacing:=SpinSpacing.Value;
end;

{ TOvalButton }
procedure TMainForm.OvalButton5Click(Sender: TObject);
begin
  MessageBeep(0);
end;

{ TSRColorButton }
procedure TMainForm.CBAllowAllUp1Click(Sender: TObject);
begin
  SRColorButton1.AllowAllUp:=CBAllowAllUp1.Checked;
end;

procedure TMainForm.CBAllowAllUp2Click(Sender: TObject);
begin
  SRColorButton2.AllowAllUp:=CBAllowAllUp2.Checked;
end;

procedure TMainForm.SpinContrastHighlightChange(Sender: TObject);
begin
  SRColorButton1.ContrastHighlight:=SpinContrastHighlight.Value;
  SRColorButton2.ContrastHighlight:=SpinContrastHighlight.Value;
end;

procedure TMainForm.SpinContrastShadowChange(Sender: TObject);
begin
  SRColorButton1.ContrastShadow:=SpinContrastShadow.Value;
  SRColorButton2.ContrastShadow:=SpinContrastShadow.Value;
end;

procedure TMainForm.SpinBevelWidthChange(Sender: TObject);
begin
  SRColorButton1.BevelWidth:=SpinBevelWidth.Value;
  SRColorButton2.BevelWidth:=SpinBevelWidth.Value;
end;

procedure TMainForm.SRColorButton3Click(Sender: TObject);
begin
  MessageBeep(0);
end;

{ TSRCalendar }
procedure TMainForm.SpinMonatDownClick(Sender: TObject);
begin
  Kalender.PrevMonth;
end;

procedure TMainForm.SpinMonatUpClick(Sender: TObject);
begin
  Kalender.NextMonth;
end;

procedure TMainForm.SpinJahrDownClick(Sender: TObject);
begin
  Kalender.PrevYear;
end;

procedure TMainForm.SpinJahrUpClick(Sender: TObject);
begin
  Kalender.NextYear;
end;

procedure TMainForm.ComboGermanStateChange(Sender: TObject);
begin
  if ComboGermanState.ItemIndex>=0 then
    Kalender.GermanState:=TGermanState(ComboGermanState.ItemIndex);
end;

procedure TMainForm.ComboStartTagChange(Sender: TObject);
begin
  Kalender.StartOfWeek:=TDayOfWeek(ComboStartTag.ItemIndex);
end;

procedure TMainForm.ComboDrawStyleChange(Sender: TObject);
begin
  Kalender.DrawStyle:=TCalendarDrawStyle(ComboDrawStyle.ItemIndex);
end;

procedure TMainForm.CBSatAsSunClick(Sender: TObject);
begin
  Kalender.SaturdayAsSunday:=CBSatAsSun.Checked;
end;

procedure TMainForm.CBShowHolidaysClick(Sender: TObject);
begin
  if CBShowHolidays.Checked then
    Kalender.CalendarOptions:=Kalender.CalendarOptions+[coCalcHolidays]
  else
    Kalender.CalendarOptions:=Kalender.CalendarOptions-[coCalcHolidays];
  LblFeiertag.Caption:=Kalender.Holiday;
end;

procedure TMainForm.CBGridLinesClick(Sender: TObject);
begin
  if CBGridLines.Checked then
    Kalender.CalendarOptions:=Kalender.CalendarOptions+[coGridLines]
  else
    Kalender.CalendarOptions:=Kalender.CalendarOptions-[coGridLines];
end;

procedure TMainForm.CBFrameSelectionClick(Sender: TObject);
begin
  if CBFrameSelection.Checked then
    Kalender.CalendarOptions:=Kalender.CalendarOptions+[coFrameSelection]
  else
    Kalender.CalendarOptions:=Kalender.CalendarOptions-[coFrameSelection];
end;

procedure TMainForm.CBShowMarksClick(Sender: TObject);
begin
  if CBShowMarks.Checked then
    Kalender.CalendarOptions:=Kalender.CalendarOptions+[coShowMarks]
  else
    Kalender.CalendarOptions:=Kalender.CalendarOptions-[coShowMarks];
end;

procedure TMainForm.CBDeleteMarksClick(Sender: TObject);
begin
  if CBDeleteMarks.Checked then
    Kalender.CalendarOptions:=Kalender.CalendarOptions+[coAutoDeleteMarks]
  else
    Kalender.CalendarOptions:=Kalender.CalendarOptions-[coAutoDeleteMarks];
end;

procedure TMainForm.CBAstroDatenClick(Sender: TObject);
begin
  if CBAstroDaten.Checked then
    Kalender.CalendarOptions:=Kalender.CalendarOptions+[coCalcAstroData]
  else
    Kalender.CalendarOptions:=Kalender.CalendarOptions-[coCalcAstroData];
  ComboAstroData.Visible:=CBAstroDaten.Checked;
end;

procedure TMainForm.KalenderDblClick(Sender: TObject);
begin
  Kalender.Marked[Kalender.Day]:=not Kalender.Marked[Kalender.Day];
end;

procedure TMainForm.KalenderChange(Sender: TObject);
begin
  LblMonat.Caption:=LongMonthNames[Kalender.Month];
  LblJahr.Caption:=IntToStr(Kalender.Year);
  LblDatum.Caption:=FormatDateTime('dddd, dd. mmmm yyyy',Kalender.Date);
  LblTagImJahr.Caption:=IntToStr(Kalender.DayOfYear)+'. Tag im Jahr';
  LblWocheImJahr.Caption:=IntToStr(Kalender.WeekOfYear)+'. Woche im Jahr';
  if Kalender.HolidayNr<0 then
    LblFeiertag.Font.Color:=clNavy
  else
    LblFeiertag.Font.Color:=clRed;
  LblFeiertag.Caption:=Kalender.Holiday;
  LblSeason.Caption:=Jahreszeit[ord(Kalender.Season)];
  LblSternzeichen.Caption:='Sternzeichen: '+Sternzeichen[ord(Kalender.ZodiacSign)];
  with ComboAstroData.Items do begin
    Clear;
    Add('Mondaufgang: '+TimeToStr(Kalender.MoonRise)+' h');
    Add('Mondhöchststand: '+TimeToStr(Kalender.MoonTransit)+' h');
    Add('Monduntergang: '+TimeToStr(Kalender.MoonSet)+' h');
    Add('Entfernung zum Mond: '+FloatToStrF(Kalender.MoonDistance,ffNumber,8,0)+' km');
    Add(MondPhase[ord(Kalender.MoonPhase)]);
    Add('Sonnenaufgang: '+TimeToStr(Kalender.SunRise)+' h');
    Add('Sonnenhöchststand: '+TimeToStr(Kalender.SunTransit)+' h');
    Add('Sonnenuntergang: '+TimeToStr(Kalender.SunSet)+' h');
    Add('Entfernung zur Sonne: '+FloatToStrF(Kalender.SunDistance/1000000,ffNumber,8,2)+' mio km');
  end;
  if ComboAstroData.Items.Count>0 then
    ComboAstroData.ItemIndex:=0;
end;

{ TSRClock }
procedure TMainForm.EditZeitChange(Sender: TObject);
begin
  try
    Uhr.Time:=StrToFloat(EditZeit.Text)/24;
  except
  end;
end;

procedure TMainForm.CBAutoUpdateClick(Sender: TObject);
begin
  Uhr.AutoUpdate:=CBAutoUpdate.Checked;
  EditZeit.Enabled:=not CBAutoUpdate.Checked;
end;

procedure TMainForm.ComboClockStyleChange(Sender: TObject);
begin
  if ComboClockStyle.ItemIndex>=0 then
    Uhr.Style:=TClockStyle(ComboClockStyle.ItemIndex);
  if Uhr.Style=csDigital then begin
    Uhr.Left:=9;
    Uhr.Height:=56;
    Uhr.Width:=169;
    SpinClockContrast.Visible:=true;
    Label39.Visible:=true;
  end
  else begin
    Uhr.Left:=29;
    Uhr.Height:=130;
    Uhr.Width:=130;
    SpinClockContrast.Visible:=false;
    Label39.Visible:=false;
  end;
  ComboSeparator.Enabled:=Uhr.Style=csDigital;
end;

procedure TMainForm.ComboClockKindChange(Sender: TObject);
begin
  case ComboClockKind.ItemIndex of
    0 : Uhr.Kind:=ckRealTime;
    1 : Uhr.Kind:=ckStopWatch;
  end;
  SBStart.Visible:=ComboClockKind.ItemIndex=1;
  SBStop.Visible:=ComboClockKind.ItemIndex=1;
  SBReset.Visible:=ComboClockKind.ItemIndex=1;
end;

procedure TMainForm.SpinClockContrastChange(Sender: TObject);
begin
  Uhr.LEDContrast:=SpinClockContrast.Value;
  Uhr.LEDContrast:=SpinClockContrast.Value;
end;

procedure TMainForm.ComboSeparatorChange(Sender: TObject);
begin
  if ComboSeparator.ItemIndex>=0 then
    Uhr.Separator:=TSeparator(ComboSeparator.ItemIndex);
end;

procedure TMainForm.ComboClockBevelChange(Sender: TObject);
begin
  if ComboClockBevel.ItemIndex>=0 then
    Uhr.BevelStyle:=TClockBevel(ComboClockBevel.ItemIndex);
end;

procedure TMainForm.SEClockBevelChange(Sender: TObject);
begin
  Uhr.BevelWidth:=SEClockBevel.Value;
end;

procedure TMainForm.SBStartClick(Sender: TObject);
begin
  Uhr.Start;
end;

procedure TMainForm.SBStopClick(Sender: TObject);
begin
  Uhr.Stop;
end;

procedure TMainForm.SBResetClick(Sender: TObject);
begin
  Uhr.Reset;
end;

procedure TMainForm.SBCSVOpenDlgClick(Sender: TObject);
begin
  CSVOpenDlg.Execute;
end;

procedure TMainForm.SBCSVSaveDlgClick(Sender: TObject);
begin
  CSVSaveDlg.Execute;
end;

procedure TMainForm.SBExtOpenDlgClick(Sender: TObject);
begin
  ExtOpenDlg.Execute;
end;

procedure TMainForm.SBExtSaveDlgClick(Sender: TObject);
begin
  ExtSaveDlg.Execute;
end;

procedure TMainForm.SBSliderOpenDlgClick(Sender: TObject);
begin
  SliderOpenDlg.Execute;
end;

procedure TMainForm.SBSliderSaveDlgClick(Sender: TObject);
begin
  SliderSaveDlg.Execute;
end;

procedure TMainForm.CBDlgShowHelpClick(Sender: TObject);
var Opt : TOpenOptions;
begin
  if CBDlgShowHelp.Checked then
    Opt:=CSVOpenDlg.Options+[ofShowHelp]
  else
    Opt:=CSVOpenDlg.Options-[ofShowHelp];
  CSVOpenDlg.Options:=Opt;
  CSVSaveDlg.Options:=Opt;
  ExtOpenDlg.Options:=Opt;
  ExtSaveDlg.Options:=Opt;
  SliderOpenDlg.Options:=Opt;
  SliderSaveDlg.Options:=Opt;
end;

procedure TMainForm.CBDlgHideReadOnlyClick(Sender: TObject);
var Opt : TOpenOptions;
begin
  if CBDlgShowHelp.Checked then
    Opt:=CSVOpenDlg.Options+[ofHideReadOnly]
  else
    Opt:=CSVOpenDlg.Options-[ofHideReadOnly];
  CSVOpenDlg.Options:=Opt;
  CSVSaveDlg.Options:=Opt;
  ExtOpenDlg.Options:=Opt;
  ExtSaveDlg.Options:=Opt;
  SliderOpenDlg.Options:=Opt;
  SliderSaveDlg.Options:=Opt;
end;

{ TIniList }

procedure TMainForm.ShowIniListInMemo;
var AStream : TMemoryStream;
begin
  AStream:=TMemoryStream.Create;
  try
    IniList.SaveToStream(AStream);
    AStream.Position:=0;
    ListBox.Items.LoadFromStream(AStream);
  finally
    AStream.Free;
  end;
end;

procedure TMainForm.UpdateButtonStates;
begin
  BtnEraseSection.Enabled:=(EditSection.Text<>'');
  BtnDeleteKey.Enabled:=(EditSection.Text<>'') and (EditKey.Text<>'');
  BtnWriteString.Enabled:=(EditSection.Text<>'') and (EditKey.Text<>'') and (EditValue.Text<>'');
end;

procedure TMainForm.BtnLoadFromFileClick(Sender: TObject);
begin
  if OpenDlg.Execute then begin
    IniList.IniFilename:=OpenDlg.Filename;
    IniList.LoadFromFile;
    LblIniFilename.Caption:=IniList.IniFilename;
    ShowIniListInMemo;
  end;
end;

procedure TMainForm.EditSeparatorChange(Sender: TObject);
begin
  SBSeparator.Enabled:=true;
end;

procedure TMainForm.EditSeparatorKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key=#13) and (SBSeparator.Enabled) then
    SBSeparatorClick(nil);
end;

procedure TMainForm.SBSeparatorClick(Sender: TObject);
var Temp : string;
begin
  Temp:=EditSeparator.Text;
  if Temp<>'' then begin
    IniList.Separator:=Temp[1];
    ShowIniListInMemo;
  end;
  SBSeparator.Enabled:=false;
end;

procedure TMainForm.EditSectionChange(Sender: TObject);
begin
  LblDescrKeyCount.Caption:='KeyCount('+EditSection.Text+'): ';
  LblKeyCount.Left:=LblDescrKeyCount.Left+LblDescrKeyCount.Width;
  LblKeyCount.Caption:=IntToStr(IniList.KeyCount(EditSection.Text));
  UpdateButtonStates;
end;

procedure TMainForm.EditSectionKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key=#13) and BtnWriteString.Enabled then
    BtnWriteStringClick(nil);
end;

procedure TMainForm.EditKeyChange(Sender: TObject);
begin
  UpdateButtonStates;
end;

procedure TMainForm.BtnEraseSectionClick(Sender: TObject);
begin
  IniList.EraseSection(EditSection.Text);
  ShowIniListInMemo;
end;

procedure TMainForm.BtnDeleteKeyClick(Sender: TObject);
begin
  IniList.DeleteKey(EditSection.Text, EditKey.Text);
  ShowIniListInMemo;
end;

procedure TMainForm.BtnWriteStringClick(Sender: TObject);
begin
  IniList.WriteString(EditSection.Text, EditKey.Text, EditValue.Text);
  ShowIniListInMemo;
end;

procedure TMainForm.ListBoxClick(Sender: TObject);
var ALine : string;
    P     : integer;
begin
  if ListBox.ItemIndex>=0 then begin
    ALine:=Trim(ListBox.Items[ListBox.ItemIndex]);
    if ALine<>'' then begin
      if ALine[1]='[' then
        EditSection.Text:=copy(ALine, 2, length(ALine)-2)
      else begin
        P:=Pos(EditSeparator.Text, ALine);
        if P>0 then begin
          EditKey.Text:=Copy(ALine, 1, P-1);
          EditValue.Text:=Copy(ALine, P+1, length(ALine)-P);
        end;
      end;
    end;
    EditSectionChange(nil);
  end;
end;

procedure TMainForm.CBOnSectionChangeClick(Sender: TObject);
begin
  if (Sender is TCheckBox) and TCheckBox(Sender).Checked then begin
    Counter:=Timeout;
    Timer.Enabled:=true;
  end;
end;

procedure TMainForm.IniListChange(Sender: TObject);
begin
  LblSectionCount.Caption:=IntToStr(IniList.SectionCount);
end;

procedure TMainForm.IniListKeyChange(Sender: TObject);
begin
  CBOnKeyChange.Checked:=true;
end;

procedure TMainForm.IniListSectionChange(Sender: TObject);
begin
  CBOnSectionChange.Checked:=true;
end;

procedure TMainForm.IniListValueChange(Sender: TObject);
begin
  CBOnValueChange.Checked:=true;
end;

{ TFileButton }

procedure TMainForm.EditFBFileNameChange(Sender: TObject);
begin
  FileButton.FileName:=EditFBFileName.Text;
end;

procedure TMainForm.CBFBAsynchronousClick(Sender: TObject);
begin
  FileButton.Asynchronous:=CBFBAsynchronous.Checked;
end;

procedure TMainForm.CBFBFlatClick(Sender: TObject);
begin
  FileButton.Flat:=CBFBFlat.Checked;
end;

procedure TMainForm.CBFBShowFilenameClick(Sender: TObject);
begin
  FileButton.ShowFilename:=CBFBShowFilename.Checked;
end;

procedure TMainForm.CBFBShowIconFrameClick(Sender: TObject);
begin
  FileButton.ShowIconFrame:=CBFBShowIconFrame.Checked;
end;

procedure TMainForm.CBFBTransparentClick(Sender: TObject);
begin
  FileButton.Transparent:=CBFBTransparent.Checked;
end;

procedure TMainForm.SEFBBevelContrastChange(Sender: TObject);
begin
  FileButton.BevelContrast:=SEFBBevelContrast.Value;
end;

procedure TMainForm.SEFBBevelWidthChange(Sender: TObject);
begin
  FileButton.BevelWidth:=SEFBBevelWidth.Value;
end;

{ FontControls }

procedure TMainForm.CBFCTrueTypeClick(Sender: TObject);
begin
  if CBFCTrueType.Checked then
    SRFontComboBox.FontMask:=SRFontComboBox.FontMask+[fmTrueType]
  else
    SRFontComboBox.FontMask:=SRFontComboBox.FontMask-[fmTrueType];
  if CBFCNoTrueType.Checked then
    SRFontComboBox.FontMask:=SRFontComboBox.FontMask+[fmNoTrueType]
  else
    SRFontComboBox.FontMask:=SRFontComboBox.FontMask-[fmNoTrueType];
  if CBFCFixedPitch.Checked then
    SRFontComboBox.FontMask:=SRFontComboBox.FontMask+[fmFixedPitch]
  else
    SRFontComboBox.FontMask:=SRFontComboBox.FontMask-[fmFixedPitch];
  if CBFCVariablePitch.Checked then
    SRFontComboBox.FontMask:=SRFontComboBox.FontMask+[fmVariablePitch]
  else
    SRFontComboBox.FontMask:=SRFontComboBox.FontMask-[fmVariablePitch];
  SRFontListBox.FontMask:=SRFontComboBox.FontMask;
end;

procedure TMainForm.CBFCShowTTSymbolClick(Sender: TObject);
begin
  SRFontComboBox.ShowTTSymbol:=CBFCShowTTSymbol.Checked;
  SRFontListBox.ShowTTSymbol:=CBFCShowTTSymbol.Checked;
end;

procedure TMainForm.CBFCUseItemFontClick(Sender: TObject);
begin
  SRFontComboBox.UseItemFont:=CBFCUseItemFont.Checked;
  SRFontListBox.UseItemFont:=CBFCUseItemFont.Checked;
end;

procedure TMainForm.TimerTimer(Sender: TObject);
begin
  dec(Counter);
  if Counter<=0 then begin
    CBOnSectionChange.Checked:=false;
    CBOnKeyChange.Checked:=false;
    CBOnValueChange.Checked:=false;
    Timer.Enabled:=false;
  end;
end;

{ Misc }

procedure TMainForm.FormCreate(Sender: TObject);
var i     : byte;
    AYear : word;
begin
  IniList:=TIniList.Create;
  PC.ActivePage:=TSInfo;
  ComboEnhFormat.ItemIndex:=4;
  ComboBoxBD.ItemIndex:=0;
  ComboBoxTP.ItemIndex:=1;
  ComboBoxDecSeparator.ItemIndex:=ord(LEDDisplay1.DecSeparator);
  ComboBoxSS.ItemIndex:=ord(LEDDisplay1.SegmentStyle);
  ComboBoxBevel.ItemIndex:=ord(LEDDisplay1.BevelStyle);
  SRWavePlayer.WaveName:=ExtractFilePath(Application.ExeName)+'Maus.wav';
  ComboCBStyle.ItemIndex:=ord(SRCheckBox.Style);
  ComboCBAlignment.ItemIndex:=ord(SRCheckBox.Alignment);
  ComboCBLayout.ItemIndex:=ord(SRCheckBox.Layout);
  KalenderChange(nil);
  ComboGermanState.ItemIndex:=ord(Kalender.GermanState);
  for i:=1 to 7 do
    ComboStartTag.Items.Add(LongDayNames[i]);
  ComboStartTag.ItemIndex:=ord(Kalender.StartOfWeek);
  ComboDrawStyle.ItemIndex:=ord(Kalender.DrawStyle);
  Kalender.Date:=Date;
  KalenderChange(nil);
  ComboClockStyle.ItemIndex:=0;
  ComboClockKind.ItemIndex:=0;
  ComboSeparator.ItemIndex:=ord(Uhr.Separator);
  ComboClockBevel.ItemIndex:=ord(Uhr.BevelStyle);
  SRLabel3.Caption:='SRLabel3:'+#13#10+'Beispieltext'+#13#10+'mit Zeilenumbrüchen';
  ComboHighlightPos.ItemIndex:=integer(SRLabel1.HighlightPos);
  ComboBevelStyle.ItemIndex:=integer(SRLabel1.BevelStyle);
  EditFBFileName.Text:=GetInfoFileName;
  CBFCShowTTSymbol.Checked:=SRFontComboBox.ShowTTSymbol;
  CBFCUseItemFont.Checked:=SRFontComboBox.UseItemFont;
  PanelVersion.Caption:='Version '+GetPackageVersionNr;
  AYear:=GetYearFromDate(FileDateToDateTime(FileAge(Application.ExeName)));
  LblInfo.Caption:='(C)opyright '+IntToStr(AYear)+#13#10#13#10+
                   'Die Komponenten sind Public Domain'+#13#10+
                   'das Urheberrecht liegt aber beim Autor.';
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  IniList.Free;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  if SRFontComboBox.Items.Count>0 then
    SRFontComboBox.ItemIndex:=0;
end;

end.
