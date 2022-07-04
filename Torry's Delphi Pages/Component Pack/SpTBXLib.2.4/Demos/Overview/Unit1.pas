unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, ActnList, ImgList, ExtDlgs, ComCtrls,
  // TB2K
  TB2Item, TB2Toolbar, TB2Dock, TB2ExtItems,
  // SpTBXLib
  SpTBXSkins, SpTBXItem, SpTBXDkPanels, SpTBXTabs, SpTBXEditors, SpTBXControls,
  SpTBXExtEditors;

type
  TForm1 = class(TForm)
    SpTBXDock1: TSpTBXDock;
    SpTBXMultiDock1: TSpTBXMultiDock;
    SpTBXMultiDock3: TSpTBXMultiDock;
    SpTBXDockablePanel1: TSpTBXDockablePanel;
    SpTBXToolbar1: TSpTBXToolbar;
    Panel1: TPanel;
    SpTBXTabControl1: TSpTBXTabControl;
    SpTBXRightAlignSpacerItem1: TSpTBXRightAlignSpacerItem;
    tabClose: TSpTBXItem;
    SpTBXSubmenuItem1: TSpTBXSubmenuItem;
    SpTBXSubmenuItem2: TSpTBXSubmenuItem;
    SpTBXItem2: TSpTBXItem;
    tabRight: TSpTBXItem;
    tabLeft: TSpTBXItem;
    SpTBXToolbar3: TSpTBXToolbar;
    SpTBXLabelItem3: TSpTBXLabelItem;
    SpTBXItem7: TSpTBXItem;
    subColor: TSpTBXSubmenuItem;
    SpTBXLabelItem4: TSpTBXLabelItem;
    subLang: TSpTBXSubmenuItem;
    subSkins: TSpTBXSubmenuItem;
    SpTBXSkinGroupItem1: TSpTBXSkinGroupItem;
    SpTBXTabControl2: TSpTBXTabControl;
    SpTBXTabItem1: TSpTBXTabItem;
    SpTBXTabSheet1: TSpTBXTabSheet;
    ActionList1: TActionList;
    Action1: TAction;
    Action2: TAction;
    SpTBXButton1: TSpTBXButton;
    SpTBXStatusBar1: TSpTBXStatusBar;
    SpTBXSeparatorItem2: TSpTBXSeparatorItem;
    hintLabel: TSpTBXLabelItem;
    SpTBXSeparatorItem3: TSpTBXSeparatorItem;
    ImageList1: TImageList;
    SpTBXLabelItem6: TSpTBXLabelItem;
    SpTBXSeparatorItem4: TSpTBXSeparatorItem;
    subLang2: TSpTBXSubmenuItem;
    Image1: TImage;
    SpTBXSeparatorItem5: TSpTBXSeparatorItem;
    SpTBXSeparatorItem8: TSpTBXSeparatorItem;
    SpTBXTabItem3: TSpTBXTabItem;
    SpTBXTabSheet3: TSpTBXTabSheet;
    SpTBXGroupBox5: TSpTBXGroupBox;
    SpTBXLabel2: TSpTBXLabel;
    SpTBXLabel3: TSpTBXLabel;
    SpTBXLabel4: TSpTBXLabel;
    SpTBXLabel7: TSpTBXLabel;
    SpTBXLabel5: TSpTBXLabel;
    SpTBXToolbar2: TSpTBXToolbar;
    SpTBXItem1: TSpTBXItem;
    SpTBXSeparatorItem6: TSpTBXSeparatorItem;
    SpTBXItem3: TSpTBXItem;
    SpTBXItem4: TSpTBXItem;
    SpTBXSeparatorItem7: TSpTBXSeparatorItem;
    SpTBXLabel6: TSpTBXLabel;
    DP1: TSpTBXDockablePanel;
    DP2: TSpTBXDockablePanel;
    DP3: TSpTBXDockablePanel;
    SpTBXLabel1: TSpTBXLabel;
    SpTBXLabel8: TSpTBXLabel;
    SpTBXLabel9: TSpTBXLabel;
    SpTBXLabel10: TSpTBXLabel;
    SpTBXLabel11: TSpTBXLabel;
    SpTBXLabel12: TSpTBXLabel;
    SpTBXLabel13: TSpTBXLabel;
    SpTBXLabel15: TSpTBXLabel;
    SpTBXLabel16: TSpTBXLabel;
    SpTBXLabel17: TSpTBXLabel;
    Timer1: TTimer;
    SpTBXGroupBox6: TSpTBXGroupBox;
    progressDec: TSpTBXButton;
    progressInc: TSpTBXButton;
    progressAnimate: TSpTBXButton;
    SpTBXProgressBar1: TSpTBXProgressBar;
    progressFiles: TSpTBXCheckBox;
    SpTBXProgressBar2: TSpTBXProgressBar;
    TBControlItem2: TTBControlItem;
    SpTBXTrackBar1: TSpTBXTrackBar;
    SpTBXComboBox1: TSpTBXComboBox;
    TBControlItem3: TTBControlItem;
    SpTBXPanel2: TSpTBXPanel;
    Memo2: TMemo;
    SpTBXSplitter1: TSpTBXSplitter;
    SpTBXSplitter2: TSpTBXSplitter;
    SpTBXTabItem6: TSpTBXTabItem;
    SpTBXTabSheet6: TSpTBXTabSheet;
    SpTBXButtonEdit1: TSpTBXButtonEdit;
    SpTBXComboBox2: TSpTBXComboBox;
    SpTBXEdit1: TSpTBXEdit;
    SpTBXSpinEdit1: TSpTBXSpinEdit;
    LangListBox: TSpTBXListBox;
    rgSkinType: TSpTBXRadioGroup;
    SpTBXGroupBox1: TSpTBXGroupBox;
    SpTBXGroupBox2: TSpTBXGroupBox;
    trackTickmarks: TSpTBXRadioGroup;
    skinButton: TSpTBXSpeedButton;
    SpTBXDockablePanel2: TSpTBXDockablePanel;
    OpenDialog1: TOpenDialog;
    SpTBXSeparatorItem1: TSpTBXSeparatorItem;
    SpTBXColorPalette1: TSpTBXColorPalette;
    SpTBXItem5: TSpTBXItem;
    SpTBXSpinEditItem1: TSpTBXSpinEditItem;
    Panel2: TPanel;
    SpTBXSpeedButton1: TSpTBXSpeedButton;
    SpTBXSpeedButton2: TSpTBXSpeedButton;
    SpTBXSpeedButton3: TSpTBXSpeedButton;
    SpTBXColorEdit1: TSpTBXColorEdit;
    SpTBXFontComboBox1: TSpTBXFontComboBox;
    procedure FormShow(Sender: TObject);
    procedure tabCloseClick(Sender: TObject);
    procedure ActionList1Update(Action: TBasicAction;
      var Handled: Boolean);
    procedure Action1Execute(Sender: TObject);
    procedure Action2Execute(Sender: TObject);
    procedure SpTBXTabControl2Resize(Sender: TObject);
    procedure rgSkinTypeClick(Sender: TObject);
    procedure hintLabelDrawHint(Sender: TObject;
      AHintBitmap: TBitmap; var AHint: WideString;
      var PaintDefault: Boolean);
    procedure progressDecClick(Sender: TObject);
    procedure progressIncClick(Sender: TObject);
    procedure progressAnimateClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure SpTBXProgressBar1ProgressChange(Sender: TObject;
      NewPosition: Integer);
    procedure progressFilesClick(Sender: TObject);
    procedure TrackbarTickMarksRadioClick(Sender: TObject);
    procedure skinButtonClick(Sender: TObject);
    procedure SpTBXSpeedButton1Draw(Sender: TObject; ACanvas: TCanvas;
      ARect: TRect; const PaintStage: TSpTBXPaintStage;
      var PaintDefault: Boolean);
  private
    { Private declarations }
    procedure LangClick(Sender: TObject);
  public
    AppPath: String;
  end;

var
  Form1: TForm1;

implementation

uses
  Registry;

{$R *.dfm}

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ Utility functions }

function SpGetWindowsDir: String;
var
  Z: Cardinal;
begin
  Z := GetWindowsDirectory(nil, 0);
  if Z > 0 then begin
    SetLength(Result, Z);
    GetWindowsDirectory(PChar(Result), Z);
    Result := IncludeTrailingPathDelimiter(Result);
  end
  else
    Result := '';
end;

function SpGetWinAmpDir: String;
var
  Registry: TRegistry;
begin
  Registry := TRegistry.Create;
  try
    Registry.RootKey := HKEY_CURRENT_USER;
    // False because we do not want to create it if it doesn't exist
    Registry.OpenKey('\Software\Winamp', False);
    Result := Registry.ReadString('');
    if Length(Result) > 0 then
      Result := IncludeTrailingPathDelimiter(Result);
  finally
    Registry.Free;
  end;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ Form }

procedure TForm1.FormShow(Sender: TObject);
var
  A: TSpTBXItem;
  D: String;
  I: Integer;
begin
  // Set skin type to sknTBX
  SkinManager.ChangeControlSkinType(Self, sknSkin);

  // Add the Languages to the Languages menu item and TabControl
  for I := 0 to LangListBox.Items.Count - 1 do begin
    A := TSpTBXItem.Create(nil);
    try
      A.Caption := LangListBox.Items[I];
      A.GroupIndex := 100;
      A.AutoCheck := True;
      A.Tag := I;
      A.OnClick := LangClick;
      subLang.Add(A);
      with SpTBXTabControl1.Add(LangListBox.Items[I]) do
        Tag := I;
    except
      A.Free;
    end;
  end;

  // Select the first Language
  subLang.Items[0].Click;
  SpTBXTabControl1.ActiveTabIndex := 0;
  SpTBXTabControl2.ActiveTabIndex := 0;

  // Load default button Skin
  AppPath := IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName));

  // Initialize the link labels
  D := SpGetWindowsDir;
  if Length(D) > 0 then begin
    SpTBXLabel4.LinkText := D;
    SpTBXLabel5.LinkText := 'explorer.exe';
    SpTBXLabel5.LinkTextParams := '/e, ' + D;
  end;
  D := SpGetWinAmpDir;
  if Length(D) > 0 then
    SpTBXLabel6.LinkText := D + 'winamp.exe';
  if FileExists(AppPath + 'unit1.pas') then
    SpTBXLabel7.LinkTextParams := '"' + AppPath + 'unit1.pas' + '"';
end;

procedure TForm1.LangClick(Sender: TObject);
var
  A: TSpTBXItem;
begin
  A := Sender as TSpTBXItem;
  subLang.Caption := A.Caption;
  subLang2.Caption := A.Caption;
  SpTBXTabControl1.ActiveTabIndex := subLang.IndexOf(A);
end;

procedure TForm1.tabCloseClick(Sender: TObject);
begin
  SpTBXTabControl1.Visible := False;
end;

procedure TForm1.ActionList1Update(Action: TBasicAction; var Handled: Boolean);
var
  L, R: Boolean;
begin
  SpTBXTabControl1.ScrollState(L, R);
  Action1.Enabled := L;
  Action2.Enabled := R;
end;

procedure TForm1.Action1Execute(Sender: TObject);
begin
  SpTBXTabControl1.ScrollLeft;
end;

procedure TForm1.Action2Execute(Sender: TObject);
begin
  SpTBXTabControl1.ScrollRight;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ Options Panel }

procedure TForm1.rgSkinTypeClick(Sender: TObject);
begin
  SkinManager.ChangeControlSkinType(Self, TSpTBXSkinType(rgSkinType.ItemIndex));
end;

procedure TForm1.skinButtonClick(Sender: TObject);
var
  S: string;
  I: Integer;
begin
  S := AppPath + 'Skins';
  if DirectoryExists(S) then
    OpenDialog1.InitialDir := S;

  if OpenDialog1.Execute then
    if FileExists(OpenDialog1.FileName) then begin
      // Load the skin file and add it to the SkinList
      I := SkinManager.SkinsList.AddSkinFromFile(OpenDialog1.FileName);
      if I > -1 then begin
        // Set the new skin
        SkinManager.SetSkin(SkinManager.SkinsList[I]);
        // Recreate the SkinGroupItem
        SpTBXSkinGroupItem1.Recreate;
      end;
    end;
end;

procedure TForm1.SpTBXTabControl2Resize(Sender: TObject);
begin
  SpTBXButton1.Left := (SpTBXTabControl2.Width - SpTBXButton1.Width) div 2;
  SpTBXProgressBar1.Left := (SpTBXTabControl2.Width - SpTBXProgressBar1.Width) div 2;
  SpTBXTrackBar1.Left := (SpTBXTabControl2.Width - SpTBXTrackBar1.Width) div 2;
end;

procedure TForm1.SpTBXSpeedButton1Draw(Sender: TObject; ACanvas: TCanvas;
  ARect: TRect; const PaintStage: TSpTBXPaintStage; var PaintDefault: Boolean);
var
  SB: TSpTBXSpeedButton;
begin
  if PaintStage = pstPrePaint then begin
    PaintDefault := False;
    SB := TSpTBXSpeedButton(Sender);
    SpDrawXPHeader(ACanvas, ARect, SB = SpTBXSpeedButton1, SB.MouseInControl, SB.Pushed, sknSkin);
  end;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ StatusBar }

procedure TForm1.hintLabelDrawHint(Sender: TObject;
  AHintBitmap: TBitmap; var AHint: WideString; var PaintDefault: Boolean);
var
  R, GR, TR: TRect;
  WS: WideString;
begin
  PaintDefault := False;
  AHintBitmap.Width := Image1.Picture.Bitmap.Width + 115;
  AHintBitmap.Height := Image1.Picture.Bitmap.Height + 30;
  with AHintBitmap.Canvas do begin
    Brush.Color := clInfoBk;
    Font.Color := clInfoText;
    R := Rect(0, 0, AHintBitmap.Width, AHintBitmap.Height);
    FillRect(R);

    GR := Bounds(5, 5, Image1.Picture.Bitmap.Width, Image1.Picture.Bitmap.Height);
    Draw(GR.Left, GR.Top, Image1.Picture.Bitmap);

    WS := 'Language: ' + subLang.Caption + #13#10 +
          'Skin: ' + SkinManager.CurrentSkinName + #13#10 +
          'Time: ' + TimeToStr(Now);
    TR := Rect(GR.Right + 5, 10, R.Right, R.Bottom);
    SpDrawXPText(AHintBitmap.Canvas, WS, TR, DT_WORDBREAK);

    Font.Color := clBlue;
    Font.Style := [fsUnderline];
    TR := Rect(GR.Left, GR.Bottom + 5, R.Right, R.Bottom);
    WS := 'http://www.silverpointdevelopment.com';
    SpDrawXPText(AHintBitmap.Canvas, WS, TR, DT_WORDBREAK);
  end;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ ProgressBar }

procedure TForm1.progressDecClick(Sender: TObject);
begin
  SpTBXProgressBar1.StepIt(-10);
  SpTBXProgressBar2.StepIt(-10);
end;

procedure TForm1.progressIncClick(Sender: TObject);
begin
  SpTBXProgressBar1.StepIt(10);
  SpTBXProgressBar2.StepIt(10);
end;

procedure TForm1.progressAnimateClick(Sender: TObject);
begin
  SpTBXProgressBar1.Position := 0;
  SpTBXProgressBar2.Position := 0;
  Timer1.Enabled := True;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  SpTBXProgressBar1.StepIt;
  SpTBXProgressBar2.StepIt;
  if SpTBXProgressBar1.Position >= SpTBXProgressBar1.Max then
    Timer1.Enabled := False;
end;

procedure TForm1.SpTBXProgressBar1ProgressChange(Sender: TObject;
  NewPosition: Integer);
var
  I: Integer;
  WS: WideString;
begin
  if progressFiles.Checked then begin
    I := (NewPosition div 10) - 1;
    if I < 0  then I := 0;
    if I > LangListBox.Items.Count - 1 then I := LangListBox.Items.Count - 1;
    WS := 'C:\Lang\' + LangListBox.Items[I] + '.txt';
    SpTBXProgressBar1.Caption := WS;
    SpTBXProgressBar2.Caption := WS;
  end;
end;

procedure TForm1.ProgressFilesClick(Sender: TObject);
begin
  if progressFiles.Checked then
    SpTBXProgressBar1.CaptionType := pctDefault
  else
    SpTBXProgressBar1.CaptionType := pctPercentage;
  SpTBXProgressBar2.CaptionType := SpTBXProgressBar1.CaptionType;
  SpTBXProgressBar1ProgressChange(SpTBXProgressBar1, SpTBXProgressBar1.Position);
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TrackBar }

procedure TForm1.TrackbarTickMarksRadioClick(Sender: TObject);
begin
  SpTBXTrackBar1.TickMarks := TSpTBXTickMark(trackTickmarks.ItemIndex);
end;

end.
