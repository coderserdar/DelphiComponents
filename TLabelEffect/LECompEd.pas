unit LECompEd;

{
  A component editor for the TLabelEffect component as a whole.
  Allows all of the important properties of this component to be
  manipuated with the effects being immediately visible.
  Written by Keith Wood - 13 Jan 1997.
}

interface

uses
  Classes, Graphics, Forms, Dialogs,
{$IFDEF VER140}  { Delphi 6 }
  DesignIntf, DesignEditors,
{$ELSE}
  DsgnIntf,
{$ENDIF}
  LblEffct, Controls, StdCtrls, Buttons, ExtCtrls, Spin, ComCtrls, TabNotBk;

type
  { The component editor for the TLabelEffect component }
  TLabelEffectCompEditor = class(TComponentEditor)
  public
    procedure Edit; override;
  end;

  { The interface form for the TLabelEffect component editor }
  TLabelEffectEditor = class(TForm)
    Label14: TLabel;                        { Overall properties }
    cmbEffectStyle: TComboBox;
    Label15: TLabel;
    cmbColourScheme: TComboBox;
    Label17: TLabel;
    edtCaption: TEdit;
    btnFont: TButton;
    GroupBox2: TGroupBox;
      lefSample: TLabelEffect;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    tnbComponents: TTabbedNotebook;
      Label11: TLabel;                      { Face page }
      pnlFaceColour: TPanel;
      btnFaceColour: TButton;
      Label16: TLabel;
      cmbGraduateOption: TComboBox;
      Label12: TLabel;
      pnlFaceGraduate: TPanel;
      btnFaceGraduate: TButton;
      cbxAngle: TCheckBox;
      grpAngle: TGroupBox;
        imgAngle: TImage;
        lblAngle: TLabel;
        spnAngle: TSpinEdit;
        cbxKeepVertical: TCheckBox;
      ragHighlightDirection: TRadioGroup;   { Highlight page }
      Label6: TLabel;
      spnHighlightDepth: TSpinEdit;
      Label8: TLabel;
      pnlHighlightColour: TPanel;
      btnHighlightColour: TButton;
      Label9: TLabel;
      pnlHighlightGraduate: TPanel;
      btnHighlightGraduate: TButton;
      Label7: TLabel;
      cmbHighlightStyle: TComboBox;
      Label10: TLabel;
      cmbHighlightResize: TComboBox;
      ragShadowDirection: TRadioGroup;      { Shadow page }
      Label5: TLabel;
      spnShadowDepth: TSpinEdit;
      Label3: TLabel;
      pnlShadowColour: TPanel;
      btnShadowColour: TButton;
      Label4: TLabel;
      pnlShadowGraduate: TPanel;
      btnShadowGraduate: TButton;
      Label1: TLabel;
      cmbShadowStyle: TComboBox;
      Label2: TLabel;
      cmbShadowResize: TComboBox;
      grpBitmap: TGroupBox;                 { Bitmaps }
        Label18: TLabel;
        imgBitmap: TImage;
        btnLoad: TButton;
        btnClear: TButton;
      grpAnimation: TGroupBox;
        cbxEnabled: TCheckBox;
        cbxContinuous: TCheckBox;
        lblFrames: TLabel;
        cmbFrames: TComboBox;
        lblIndex: TLabel;
        spnIndex: TSpinEdit;
        lblInterval: TLabel;
        spnInterval: TSpinEdit;
        cbxExternal: TCheckBox;
        lblStyle: TLabel;
        cmbStyle: TComboBox;
    dlgFont: TFontDialog;                   { Dialogs }
    dlgOpen: TOpenDialog;
    dlgColour: TColorDialog;
    procedure cmbEffectStyleChange(Sender: TObject);        { Overall properties }
    procedure cmbColourSchemeChange(Sender: TObject);
    procedure edtCaptionChange(Sender: TObject);
    procedure btnFontClick(Sender: TObject);
    procedure btnFaceColourClick(Sender: TObject);          { Face page }
    procedure cmbGraduateOptionChange(Sender: TObject);
    procedure btnFaceGraduateClick(Sender: TObject);
    procedure cbxAngleClick(Sender: TObject);
    procedure imgAngleMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure spnAngleChange(Sender: TObject);
    procedure cbxKeepVerticalClick(Sender: TObject);
    procedure ragHighlightDirectionClick(Sender: TObject);  { Highlight page }
    procedure spnHighlightDepthChange(Sender: TObject);
    procedure btnHighlightColourClick(Sender: TObject);
    procedure btnHighlightGraduateClick(Sender: TObject);
    procedure cmbHighlightStyleChange(Sender: TObject);
    procedure cmbHighlightResizeChange(Sender: TObject);
    procedure ragShadowDirectionClick(Sender: TObject);     { Shadow page }
    procedure spnShadowDepthChange(Sender: TObject);
    procedure btnShadowColourClick(Sender: TObject);
    procedure btnShadowGraduateClick(Sender: TObject);
    procedure cmbShadowStyleChange(Sender: TObject);
    procedure cmbShadowResizeChange(Sender: TObject);
    procedure btnLoadClick(Sender: TObject);                { Bitmaps page }
    procedure btnClearClick(Sender: TObject);
    procedure cbxEnabledClick(Sender: TObject);
    procedure cbxContinuousClick(Sender: TObject);
    procedure cmbFramesChange(Sender: TObject);
    procedure spnIndexChange(Sender: TObject);
    procedure spnIntervalChange(Sender: TObject);
    procedure cbxExternalClick(Sender: TObject);
    procedure cmbStyleChange(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
  private
    lefOriginal: TLabelEffect;
    bExternalTiming: Boolean;
    bInitialising: Boolean;
    procedure InitControls;
    function ChangeColour(clrOrig: TColor; pnlShow: TPanel): TColor;
  public
    constructor CreateFor(AOwner: TComponent; lefLabel: TLabelEffect);
  end;

implementation

{$R *.DFM}

{ Show the editor for the component }
procedure TLabelEffectCompEditor.Edit;
var
  dlgLabelEffectEditor: TLabelEffectEditor;
begin
  dlgLabelEffectEditor :=
    TLabelEffectEditor.CreateFor(Application, TLabelEffect(Component));
  try
    if dlgLabelEffectEditor.ShowModal = mrOK then
    begin
      TLabelEffect(Component).Invalidate;
      Designer.Modified;
    end;
  finally
    dlgLabelEffectEditor.Free;
  end;
end;

const
  { Convert between TEffectDirections and integers (not straight Ord() mapping) }
  edDirections: array [0..8] of TEffectDirection =
    (edUpLeft, edLeft, edDownLeft, edUp, edNone, edDown, edUpRight, edRight, edDownRight);
  iDirections: array [TEffectDirection] of 0..8 =
    (4, 3, 6, 7, 8, 5, 2, 1, 0);

{ Create a new editor and initialise it for the specified label }
constructor TLabelEffectEditor.CreateFor(AOwner: TComponent; lefLabel: TLabelEffect);
var
  i: Integer;
  cmpOwner: TComponent;
begin
  inherited Create(AOwner);

  bInitialising := False;
  { Attach to original label }
  lefOriginal := lefLabel;
  lefSample.Assign(lefOriginal);
  with imgAngle.Canvas do
  begin
    Brush.Color := clBtnFace;
    FillRect(ClientRect);
    Brush.Color := clWhite;
  end;
  cmbFrames.Items.AddObject('<none>', nil);
  cmpOwner := lefOriginal.Owner;
  with cmpOwner do
    for i := 0 to ComponentCount - 1 do
      if Components[i] is TImageList then
        cmbFrames.Items.AddObject(Components[i].Name, Components[i]);
  bExternalTiming := lefSample.Animation.ExternalTiming;
  lefSample.Animation.ExternalTiming := False;
  lefSample.Animation.Enabled        := not lefSample.Animation.Enabled;
  lefSample.Animation.Enabled        := not lefSample.Animation.Enabled;

  InitControls;
end;

{ Set controls based on properties of sample }
procedure TLabelEffectEditor.InitControls;
const
  clrFont: array [False..True] of TColor = (clGrayText, clWindowText);
begin
  if bInitialising then
    Exit;

  bInitialising := True;
  cmbEffectStyle.ItemIndex := Ord(lefSample.EffectStyle);
  cmbColourScheme.ItemIndex := Ord(lefSample.ColourScheme);
  edtCaption.Text := lefSample.Caption;
  pnlFaceColour.Color := lefSample.ColourFace;
  cmbGraduateOption.ItemIndex := Ord(lefSample.GraduateFace);
  pnlFaceGraduate.Color := lefSample.GraduateFrom;
  spnAngle.Value := lefSample.Angle;
  if lefSample.Angle = 0 then
    spnAngle.Value := 360;
  cbxKeepVertical.Checked := lefSample.KeepLettersVertical;
  cbxAngle.Checked := (lefSample.Angle > 0);
  lblAngle.Enabled := cbxAngle.Checked;
  spnAngle.Enabled := cbxAngle.Checked;
  spnAngle.Font.Color := clrFont[cbxAngle.Checked];
  cbxKeepVertical.Enabled := cbxAngle.Checked;
  with imgAngle.Canvas do
  begin
    Ellipse(0, 0, 129, 129);
    MoveTo(64, 64);
    LineTo(Round(Sin((spnAngle.Value + 90) * PI / 180) * 64) + 64,
      Round(Cos((spnAngle.Value + 90) * PI / 180) * 64) + 64);
  end;

  ragHighlightDirection.ItemIndex := iDirections[lefSample.DirectionHighlight];
  spnHighlightDepth.Value := lefSample.DepthHighlight;
  pnlHighlightColour.Color := lefSample.ColourHighlight;
  pnlHighlightGraduate.Color := lefSample.GraduateHighlight;
  cmbHighlightStyle.ItemIndex := Ord(lefSample.StyleHighlight);
  cmbHighlightResize.ItemIndex := Ord(lefSample.ResizeHighlight);

  ragShadowDirection.ItemIndex := iDirections[lefSample.DirectionShadow];
  spnShadowDepth.Value := lefSample.DepthShadow;
  pnlShadowColour.Color := lefSample.ColourShadow;
  pnlShadowGraduate.Color := lefSample.GraduateShadow;
  cmbShadowStyle.ItemIndex := Ord(lefSample.StyleShadow);
  cmbShadowResize.ItemIndex := Ord(lefSample.ResizeShadow);

  imgBitmap.Picture.Bitmap.Assign(lefSample.Bitmap);
  grpAnimation.Enabled := (cmbFrames.Items.Count > 1);
  cbxEnabled.Enabled := grpAnimation.Enabled;
  cbxEnabled.Checked := lefSample.Animation.Enabled;
  cbxContinuous.Enabled := grpAnimation.Enabled;
  cbxContinuous.Checked := lefSample.Animation.Continuous;
  lblFrames.Enabled := grpAnimation.Enabled;
  cmbFrames.Enabled := grpAnimation.Enabled;
  if Assigned(lefSample.Animation.Frames) then
  begin
    cmbFrames.ItemIndex := cmbFrames.Items.IndexOf(lefSample.Animation.Frames.Name);
    spnIndex.MaxValue := lefSample.Animation.Frames.Count - 1;
  end
  else
  begin
    cmbFrames.ItemIndex := 0;
    spnIndex.MaxValue := 0;
  end;
  lblIndex.Enabled := grpAnimation.Enabled;
  spnIndex.Enabled := grpAnimation.Enabled;
  spnIndex.Value := lefSample.Animation.FrameIndex;
  lblInterval.Enabled := grpAnimation.Enabled;
  spnInterval.Enabled := grpAnimation.Enabled;
  spnInterval.Value := lefSample.Animation.Interval;
  cbxExternal.Enabled := grpAnimation.Enabled;
  cbxExternal.Checked := bExternalTiming;
  lblStyle.Enabled := grpAnimation.Enabled;
  cmbStyle.Enabled := grpAnimation.Enabled;
  cmbStyle.ItemIndex := Ord(lefSample.Animation.Style);
  bInitialising := False;
end;

{ Present colour dialog and accept change of colour }
function TLabelEffectEditor.ChangeColour(clrOrig: TColor; pnlShow: TPanel): TColor;
begin
  with dlgColour do
  begin
    Color := clrOrig;
    if Execute then
    begin
      Result := Color;
      pnlShow.Color := Color;
    end
    else
      Result := clrOrig;
  end;
end;

{ Change effect style }
procedure TLabelEffectEditor.cmbEffectStyleChange(Sender: TObject);
begin
  lefSample.EffectStyle := TEffectStyle(cmbEffectStyle.ItemIndex);
  InitControls;
end;

{ Change colour scheme }
procedure TLabelEffectEditor.cmbColourSchemeChange(Sender: TObject);
begin
  lefSample.ColourScheme := TColourScheme(cmbColourScheme.ItemIndex);
  InitControls;
end;

{ Change label caption }
procedure TLabelEffectEditor.edtCaptionChange(Sender: TObject);
begin
  lefSample.Caption := edtCaption.Text;
  InitControls;
end;

{ Change font }
procedure TLabelEffectEditor.btnFontClick(Sender: TObject);
begin
  with dlgFont do
  begin
    Font.Assign(lefSample.Font);
    if Execute then
    begin
      lefSample.Font.Assign(Font);
      InitControls;
    end;
  end;
end;

{ Change face colour }
procedure TLabelEffectEditor.btnFaceColourClick(Sender: TObject);
begin
  lefSample.ColourFace := ChangeColour(lefSample.ColourFace, pnlFaceColour);
  InitControls;
end;

{ Change face graduation option }
procedure TLabelEffectEditor.cmbGraduateOptionChange(Sender: TObject);
begin
  lefSample.GraduateFace := TGraduateOption(cmbGraduateOption.ItemIndex);
  InitControls;
end;

{ Change face graduation colour }
procedure TLabelEffectEditor.btnFaceGraduateClick(Sender: TObject);
begin
  lefSample.GraduateFrom := ChangeColour(lefSample.GraduateFrom, pnlFaceGraduate);
  InitControls;
end;

{ Allow/disallow angled text }
procedure TLabelEffectEditor.cbxAngleClick(Sender: TObject);
begin
  with lefSample do
    if cbxAngle.Checked then
    begin
      Angle := spnAngle.Value;
      KeepLettersVertical := cbxKeepVertical.Checked;
    end
    else
      Angle := 0;
  InitControls;
end;

{ Select angle graphically }
procedure TLabelEffectEditor.imgAngleMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  iAngle: Integer;
begin
  if not cbxAngle.Checked then
    Exit;

  if X = 64 then
  begin
    if Y > 64 then
      iAngle := 270
    else
      iAngle := 90;
  end
  else
  begin
    iAngle := Round(ArcTan((Y - 64) / (X - 64)) * 180 / PI);
    if X < 64 then
      iAngle := 180 - iAngle
    else if Y < 64 then
      iAngle := -iAngle
    else
      iAngle := 360 - iAngle;
  end;
  spnAngle.Value := iAngle;
end;

{ Change text angle }
procedure TLabelEffectEditor.spnAngleChange(Sender: TObject);
begin
  if cbxAngle.Checked then
  begin
    lefSample.Angle := spnAngle.Value;
    InitControls;
  end;
end;

{ Change vertical text flag }
procedure TLabelEffectEditor.cbxKeepVerticalClick(Sender: TObject);
begin
  lefSample.KeepLettersVertical := cbxKeepVertical.Checked;
  InitControls;
end;

{ Change highlight direction }
procedure TLabelEffectEditor.ragHighlightDirectionClick(Sender: TObject);
begin
  lefSample.DirectionHighlight := edDirections[ragHighlightDirection.ItemIndex];
  InitControls;
end;

{ Change highlight depth }
procedure TLabelEffectEditor.spnHighlightDepthChange(Sender: TObject);
begin
  lefSample.DepthHighlight := spnHighlightDepth.Value;
  InitControls;
end;

{ Change highlight colour }
procedure TLabelEffectEditor.btnHighlightColourClick(Sender: TObject);
begin
  lefSample.ColourHighlight :=
    ChangeColour(lefSample.ColourHighlight, pnlHighlightColour);
  InitControls;
end;

{ Change highlight graduation colour }
procedure TLabelEffectEditor.btnHighlightGraduateClick(Sender: TObject);
begin
  lefSample.GraduateHighlight :=
    ChangeColour(lefSample.GraduateHighlight, pnlHighlightGraduate);
  InitControls;
end;

{ Change highlight style }
procedure TLabelEffectEditor.cmbHighlightStyleChange(Sender: TObject);
begin
  lefSample.StyleHighlight := TEffectOption(cmbHighlightStyle.ItemIndex);
  InitControls;
end;

{ Change highlight resize option }
procedure TLabelEffectEditor.cmbHighlightResizeChange(Sender: TObject);
begin
  lefSample.ResizeHighlight := TResizeOption(cmbHighlightResize.ItemIndex);
  InitControls;
end;

{ Change shadow direction }
procedure TLabelEffectEditor.ragShadowDirectionClick(Sender: TObject);
begin
  lefSample.DirectionShadow := edDirections[ragShadowDirection.ItemIndex];
  InitControls;
end;

{ Change shadow depth }
procedure TLabelEffectEditor.spnShadowDepthChange(Sender: TObject);
begin
  lefSample.DepthShadow := spnShadowDepth.Value;
  InitControls;
end;

{ Change shadow colour }
procedure TLabelEffectEditor.btnShadowColourClick(Sender: TObject);
begin
  lefSample.ColourShadow :=
    ChangeColour(lefSample.ColourShadow, pnlShadowColour);
  InitControls;
end;

{ Change shadow graduation colour }
procedure TLabelEffectEditor.btnShadowGraduateClick(Sender: TObject);
begin
  lefSample.GraduateShadow :=
    ChangeColour(lefSample.GraduateShadow, pnlShadowGraduate);
  InitControls;
end;

{ Change shadow style }
procedure TLabelEffectEditor.cmbShadowStyleChange(Sender: TObject);
begin
  lefSample.StyleShadow := TEffectOption(cmbShadowStyle.ItemIndex);
  InitControls;
end;

{ Change shadow resize option }
procedure TLabelEffectEditor.cmbShadowResizeChange(Sender: TObject);
begin
  lefSample.ResizeShadow := TResizeOption(cmbShadowResize.ItemIndex);
  InitControls;
end;

{ Load a new bitmap }
procedure TLabelEffectEditor.btnLoadClick(Sender: TObject);
begin
  with dlgOpen do
    if Execute then
    begin
      imgBitmap.Picture.Bitmap.LoadFromFile(FileName);
      lefSample.Bitmap.LoadFromFile(FileName);
      InitControls;
    end;
end;

{ Clear current bitmap }
procedure TLabelEffectEditor.btnClearClick(Sender: TObject);
begin
  imgBitmap.Picture := nil;
  lefSample.Bitmap := nil;
  dlgOpen.FileName := '';
  InitControls;
end;

{ Change animation enabled flag }
procedure TLabelEffectEditor.cbxEnabledClick(Sender: TObject);
begin
  lefSample.Animation.Enabled := cbxEnabled.Checked;
  InitControls;
end;

{ Change animation continuous flag }
procedure TLabelEffectEditor.cbxContinuousClick(Sender: TObject);
begin
  lefSample.Animation.Continuous := cbxContinuous.Checked;
  InitControls;
end;

{ Change animation images }
procedure TLabelEffectEditor.cmbFramesChange(Sender: TObject);
begin
  lefSample.Animation.Frames :=
    TImageList(cmbFrames.Items.Objects[cmbFrames.ItemIndex]);
  InitControls;
end;

{ Change animation image index }
procedure TLabelEffectEditor.spnIndexChange(Sender: TObject);
begin
  lefSample.Animation.FrameIndex := spnIndex.Value;
  InitControls;
end;

{ Change animation interval }
procedure TLabelEffectEditor.spnIntervalChange(Sender: TObject);
begin
  lefSample.Animation.Interval := spnInterval.Value;
  InitControls;
end;

{ Change animation external timing flag }
procedure TLabelEffectEditor.cbxExternalClick(Sender: TObject);
begin
  bExternalTiming := cbxExternal.Checked;
  InitControls;
end;

{ Change animation style }
procedure TLabelEffectEditor.cmbStyleChange(Sender: TObject);
begin
  lefSample.Animation.Style := TLEAnimationStyle(cmbStyle.ItemIndex);
  InitControls;
end;

{ Copy changes back to original label }
procedure TLabelEffectEditor.btnOKClick(Sender: TObject);
begin
  lefSample.Animation.ExternalTiming := bExternalTiming;
  lefOriginal.Assign(lefSample);
end;

end.
