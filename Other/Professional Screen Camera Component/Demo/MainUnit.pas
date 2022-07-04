{
Professional Screen Camera Component (Delphi 7 to above)
Developed 2008 by Mohammad Reza Hanifeh Pour (MRH Software Co.)
Author E-Mail: mrh.info2007@gmail.com
Centeral Office Tel: +98(21)(7764)(4130).   Everyday 9AM ~ 4PM.
Office Address: F2 29 Rezai St. Namjo Av. Tehran-Iran.
................................................................................
Version history:

v4.7.1.0: Updated 01/01/2009
    New features:
      1) Add unit hightimar.pas for a threaded timer in preview or recording.
      2) Add canvas.trylock and canvas.unlock for all parts of image processing.
      3) Included all necessary units of Wave Audio Package and TEffect in my component. 
    Modify features:
      1) Fixed some routines in function PowerDeleteFile, Because long time waiting for deleting a file.
    Remove features:
      No thing
 
v4.4.1.1: Updated 12/11/2008
    New features:
      1) Screen Camera Unit converted to component packege (Delphi 7 to above)
      2) Add info frame rate to preview routine
    Modify features:
      1) Replaced PreviewScreenFrame routine with CaptureScreenFrame routine in preview mode
    Remove features:
      1) Delete PreviewScreenFrame routine, Because between record and preview
         eventuate to memory stack overflow

v4.2.2.1: Updated 12/03/2008
    New features:
      1) Add recording from multi monitor
      2) Add Noise effect to image effects
    Modify features:
      1) Fixed some errors
      2) Fixed memory overflow in low frame rate
    Remove features:
      1) Remove solarize filter effect from image effects

v4.0.1.0: Updated 11/18/2008
    New features:
      1) Add grayscale drawing (Capture And Preview)
      2) Add some image effects (Rotation, Brightness, Contrast, Color Adjusting, Saturation, Solarize)
    Modify features:
      1) Fixed some errors
    Remove features:
      No thing

v3.8.2.0: Updated 04/03/2008
    New features:
      No thing
    Modify features:
      1) Fixed error on selecting audio input.
    Remove features:
      No thing

v3.8.1.0: Updated 03/18/2008
    New features:
      1) Add overlay event for draw objects, picture, text and more over image.
      2) Add deleting event.
      3) Add correct frame rate info.
    Modify features:
      1) correction elapsed timer.
    Remove features:
      No thing

v3.5.3.2: Updated 03/07/2008
    New features:
      No thing
    Modify features:
      1) Canceling select region from object and windows on start record, that correct.
      2) Not synchronized record time with play time in full auto mode, that correct.
      3) Corrected some internal errors.
    Remove features:
      1) Remove capture timer and elapsed timer and add into record routin.
      2) Remove sleep timer on record (For full motion).

v3.5.0.1: Updated 02/28/2008
    New features:
      1) Upper interval TTimer (Because, sometimes system error).
      2) Lower sleep on upper frame rate during record (Softer motion).
      3) Not delete already temp audio/video files from temp directory, But can now.
      4) Add freehand window for free select region.
      5) Add select object window for select region from object or
          windows under mouse pointer.
    Modify features:
      No thing
    Remove features:
      1) Remove recompressing after record (Because, Some codecs, more the size of file).

v3.0.0.0: Released 11/20/2007
    First release.
................................................................................
}

unit MainUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ScrCam, ExtCtrls, ComCtrls, XPMan, Buttons;

{$WARNINGS OFF}
{$HINTS OFF}
{$RANGECHECKS OFF}

type
  TfrmMain = class(TForm)
    RecBut: TButton;
    StopBut: TButton;
    AudVidOptBut: TButton;
    CheckBox1: TCheckBox;
    CheckBox3: TCheckBox;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
    CheckBox6: TCheckBox;
    CheckBox8: TCheckBox;
    GroupBox1: TGroupBox;
    Edit5: TEdit;
    UpDown1: TUpDown;
    UpDown2: TUpDown;
    Edit6: TEdit;
    UpDown3: TUpDown;
    Edit7: TEdit;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    RegSelect: TRadioGroup;
    ExitBut: TButton;
    Panel1: TPanel;
    Image1: TImage;
    Label9: TLabel;
    ComboBox1: TComboBox;
    Label10: TLabel;
    ComboBox2: TComboBox;
    Label11: TLabel;
    XPManifest1: TXPManifest;
    Edit8: TEdit;
    Label12: TLabel;
    Panel2: TPanel;
    Progress: TProgressBar;
    BCancel: TButton;
    LStatus: TLabel;
    UpDownTop: TUpDown;
    UpDownLeft: TUpDown;
    UpDownWidth: TUpDown;
    UpDownHeight: TUpDown;
    ComboBox3: TComboBox;
    Label13: TLabel;
    GroupBox2: TGroupBox;
    VideoCodec: TLabel;
    ElapsedTime: TLabel;
    RealCapturing: TLabel;
    CurrentCaptured: TLabel;
    FramesCaptured: TLabel;
    DropedFrames: TLabel;
    CheckBox9: TCheckBox;
    AboutBut: TButton;
    CheckBox2: TCheckBox;
    BitBtn1: TBitBtn;
    SaveDialog: TSaveDialog;
    FilterEffectsBut: TButton;
    UpDown4: TUpDown;
    Edit9: TEdit;
    Label1: TLabel;
    CheckBox7: TCheckBox;
    Label14: TLabel;
    ComboBox4: TComboBox;
    CheckBox10: TCheckBox;
    VideoCodecValue: TLabel;
    ElapsedTimeValue: TLabel;
    RealCapturingValue: TLabel;
    CurrentCapturedValue: TLabel;
    FramesCapturedValue: TLabel;
    DropedFramesValue: TLabel;
    ScrCamera: TScreenCamera;
    procedure FormCreate(Sender: TObject);
    procedure StopButClick(Sender: TObject);
    procedure RecButClick(Sender: TObject);
    procedure AudVidOptButClick(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure CheckBox3Click(Sender: TObject);
    procedure CheckBox4Click(Sender: TObject);
    procedure CheckBox5Click(Sender: TObject);
    procedure CheckBox6Click(Sender: TObject);
    procedure CheckBox8Click(Sender: TObject);
    procedure CheckBox9Click(Sender: TObject);
    procedure Edit5Change(Sender: TObject);
    procedure RegSelectClick(Sender: TObject);
    procedure ExitButClick(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure ComboBox2Change(Sender: TObject);
    procedure BCancelClick(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure Edit1KeyPress(Sender: TObject; var Key: Char);
    procedure ComboBox3Change(Sender: TObject);
    procedure Edit2Change(Sender: TObject);
    procedure Edit3Change(Sender: TObject);
    procedure Edit4Change(Sender: TObject);
    procedure AboutButClick(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure FilterEffectsButClick(Sender: TObject);
    procedure Edit9Change(Sender: TObject);
    procedure Edit7KeyPress(Sender: TObject; var Key: Char);
    procedure CheckBox7Click(Sender: TObject);
    procedure ComboBox4Change(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure CheckBox10Click(Sender: TObject);
    procedure ScrCameraUpdate(Sender: TObject);
    procedure ScrCameraError(Sender: TObject; ErrorMessage: String);
    procedure ScrCameraStart(Sender: TObject);
    procedure ScrCameraStop(Sender: TObject);
    procedure ScrCameraPreview(Sender: TObject; PreviewBitmap: TBitmap;
      Preview, Recording: Boolean);
    procedure ScrCameraSaving(Sender: TObject; Percent: Integer;
      StatusCaption: String; var Continue: Boolean);
    procedure ScrCameraDeleting(Sender: TObject; Percent: Integer;
      StatusCaption: String; var Continue: Boolean);
    procedure ScrCameraOverlay(Sender: TObject; HDCBitmap: HDC; bmpWidth,
      bmpHeight: Integer);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
    TimerON : TTimerRecord;
    FCancel : Boolean;
  end;

var
  frmMain: TfrmMain;

implementation

uses OptDlg, About, FilterEffects;

{$R *.dfm}

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  FCancel := False;
end;

procedure TfrmMain.StopButClick(Sender: TObject);
begin
  if ScrCamera.IsRecording then
    ScrCamera.StopRecording;
end;

procedure TfrmMain.RecButClick(Sender: TObject);
begin
  if Edit8.Text <> '' then
    ScrCamera.StartRecording(Edit8.Text);
end;

procedure TfrmMain.AudVidOptButClick(Sender: TObject);
begin
  frmOption.ShowModal;
end;

procedure TfrmMain.CheckBox1Click(Sender: TObject);
begin
  ScrCamera.RecordCursor := CheckBox1.Checked;
end;

procedure TfrmMain.CheckBox3Click(Sender: TObject);
begin
  ScrCamera.DrawAreaCapture := CheckBox3.Checked;
end;                              

procedure TfrmMain.CheckBox4Click(Sender: TObject);
begin
  ScrCamera.LineRectClear := CheckBox4.Checked;
end;

procedure TfrmMain.CheckBox5Click(Sender: TObject);
begin
  ScrCamera.MinimizeAppOnStart := CheckBox5.Checked;
end;

procedure TfrmMain.CheckBox6Click(Sender: TObject);
begin
  ScrCamera.RestoreAppOnStop := CheckBox6.Checked;
end;

procedure TfrmMain.CheckBox8Click(Sender: TObject);
begin
  RegSelectClick(Self);
  ScrCamera.ShowPreview := CheckBox8.Checked;
end;

procedure TfrmMain.CheckBox9Click(Sender: TObject);
begin
  TimerON.TimerON    := CheckBox9.Checked;
  TimerON.Hour       := StrToInt(Edit5.Text);
  TimerON.Min        := StrToInt(Edit6.Text);
  TimerON.Sec        := StrToInt(Edit7.Text);
  ScrCamera.SetTimer := TimerON;
  if CheckBox9.Checked then begin
    Label6.Enabled  := True;
    Label7.Enabled  := True;
    Label8.Enabled  := True;
    Edit5.Enabled   := True;
    UpDown1.Enabled := True;
    Edit6.Enabled   := True;
    UpDown2.Enabled := True;
    Edit7.Enabled   := True;
    UpDown3.Enabled := True;
    end
  else begin
    Label6.Enabled  := False;
    Label7.Enabled  := False;
    Label8.Enabled  := False;
    Edit5.Enabled   := False;
    UpDown1.Enabled := False;
    Edit6.Enabled   := False;
    UpDown2.Enabled := False;
    Edit7.Enabled   := False;
    UpDown3.Enabled := False;
    end;
end;

procedure TfrmMain.Edit5Change(Sender: TObject);
begin
  if TEdit(Sender).Text = '' then
    TEdit(Sender).Text := '0';

  TimerON.TimerON    := CheckBox9.Checked;
  TimerON.Hour       := StrToInt(Edit5.Text);
  TimerON.Min        := StrToInt(Edit6.Text);
  TimerON.Sec        := StrToInt(Edit7.Text);
  ScrCamera.SetTimer := TimerON;
end;

procedure TfrmMain.RegSelectClick(Sender: TObject);
begin
  if not ScrCamera.IsRecording then begin
    case ComboBox1.ItemIndex of
      0: ScrCamera.VideoPriority := tpIdle;
      1: ScrCamera.VideoPriority := tpLowest;
      2: ScrCamera.VideoPriority := tpLower;
      3: ScrCamera.VideoPriority := tpNormal;
      4: ScrCamera.VideoPriority := tpHigher;
      5: ScrCamera.VideoPriority := tpHighest;
      6: ScrCamera.VideoPriority := tpTimeCritical;
    end;
    ScrCamera.FilterColor := ComboBox2.ItemIndex;
    case ComboBox3.ItemIndex of
      0: ScrCamera.Colors := pf8bit;
      1: ScrCamera.Colors := pf16bit;
      2: ScrCamera.Colors := pf24bit;
      3: ScrCamera.Colors := pf32bit;
    end;
    ScrCamera.RecordCursor       := CheckBox1.Checked;
    ScrCamera.OvelayDrawing      := CheckBox2.Checked;
    ScrCamera.DrawAreaCapture    := CheckBox3.Checked;
    ScrCamera.LineRectClear      := CheckBox4.Checked;
    ScrCamera.MinimizeAppOnStart := CheckBox5.Checked;
    ScrCamera.RestoreAppOnStop   := CheckBox6.Checked;
    ScrCamera.RecordAllMonitors  := CheckBox7.Checked;
    ScrCamera.ShowPreview        := CheckBox8.Checked;
    ScrCamera.EffectToOvelayDraw := CheckBox10.Checked;

    TimerON.TimerON    := CheckBox9.Checked;
    TimerON.Hour       := StrToInt(Edit5.Text);
    TimerON.Min        := StrToInt(Edit6.Text);
    TimerON.Sec        := StrToInt(Edit7.Text);
    ScrCamera.SetTimer := TimerON;

    case RegSelect.ItemIndex of
      0: begin
           Edit1.Enabled := False;
           Edit2.Enabled := False;
           Edit3.Enabled := False;
           Edit4.Enabled := False;

           CheckBox3.Enabled := True;
           CheckBox4.Enabled := True;

           ScrCamera.ScreenRegion := SelObject;
         end;
      1: begin
           Edit1.Enabled := False;
           Edit2.Enabled := False;
           Edit3.Enabled := False;
           Edit4.Enabled := False;

           CheckBox3.Enabled := True;
           CheckBox4.Enabled := True;

           ScrCamera.ScreenRegion := FreeHand;
         end;
      2: begin
           ScrCamera.ScreenTop    := StrToInt(Edit1.Text);
           ScrCamera.ScreenLeft   := StrToInt(Edit2.Text);
           ScrCamera.ScreenWidth  := StrToInt(Edit3.Text);
           ScrCamera.ScreenHeight := StrToInt(Edit4.Text);

           Edit1.Enabled := False;
           Edit2.Enabled := False;
           Edit3.Enabled := True;
           Edit4.Enabled := True;

           CheckBox3.Enabled := True;
           CheckBox4.Enabled := True;

           ScrCamera.ScreenRegion := FixedMoving;
         end;
      3: begin
           ScrCamera.ScreenTop    := StrToInt(Edit1.Text);
           ScrCamera.ScreenLeft   := StrToInt(Edit2.Text);
           ScrCamera.ScreenWidth  := StrToInt(Edit3.Text);
           ScrCamera.ScreenHeight := StrToInt(Edit4.Text);

           Edit1.Enabled := True;
           Edit2.Enabled := True;
           Edit3.Enabled := True;
           Edit4.Enabled := True;

           CheckBox3.Enabled := True;
           CheckBox4.Enabled := True;

           ScrCamera.ScreenRegion := FixedStable;
         end;
      4: begin
           Edit1.Text := IntToStr(ScrCamera.ScreenTop);
           Edit2.Text := IntToStr(ScrCamera.ScreenLeft);
           Edit3.Text := IntToStr(ScrCamera.ScreenWidth);
           Edit4.Text := IntToStr(ScrCamera.ScreenHeight);

           Edit1.Enabled := False;
           Edit2.Enabled := False;
           Edit3.Enabled := False;
           Edit4.Enabled := False;

           CheckBox3.Enabled := False;
           CheckBox4.Enabled := False;

           ScrCamera.SetSizeFullScreen;

           ScrCamera.ScreenRegion := FullScreen;
         end;
    end;
    UpDownTop.Enabled    := Edit1.Enabled;
    UpDownLeft.Enabled   := Edit2.Enabled;
    UpDownWidth.Enabled  := Edit3.Enabled;
    UpDownHeight.Enabled := Edit4.Enabled;
    end;
end;

procedure TfrmMain.ExitButClick(Sender: TObject);
begin
  if ScrCamera.IsRecording then
    ScrCamera.StopRecording;
  if ScrCamera.ShowPreview then
    ScrCamera.ShowPreview := False;
  Close;
end;

procedure TfrmMain.ComboBox1Change(Sender: TObject);
begin
  case ComboBox1.ItemIndex of
    0: ScrCamera.VideoPriority := tpIdle;
    1: ScrCamera.VideoPriority := tpLowest;
    2: ScrCamera.VideoPriority := tpLower;
    3: ScrCamera.VideoPriority := tpNormal;
    4: ScrCamera.VideoPriority := tpHigher;
    5: ScrCamera.VideoPriority := tpHighest;
    6: ScrCamera.VideoPriority := tpTimeCritical;
  end;
end;

procedure TfrmMain.ComboBox2Change(Sender: TObject);
begin
  ScrCamera.FilterColor := ComboBox2.ItemIndex;
end;

procedure TfrmMain.BCancelClick(Sender: TObject);
begin
  FCancel := True;
end;

procedure TfrmMain.Edit1Change(Sender: TObject);
var
  W, H: Integer;
begin
  ScrCamera.GetMinimumScreenSize(W, H);
  UpDownTop.Min    := W;
  UpDownLeft.Min   := H;
  UpDownWidth.Min  := W;
  UpDownHeight.Min := H;

  ScrCamera.GetMaximumScreenSize(W, H);
  UpDownTop.Max    := W;
  UpDownLeft.Max   := H;
  UpDownWidth.Max  := W;
  UpDownHeight.Max := H;

  if TEdit(Sender).Text = '' then
    TEdit(Sender).Text := '0';

  if StrToInt(Edit1.Text) > UpDownTop.Max then
    Edit1.Text := IntToStr(UpDownTop.Max);

  ScrCamera.ScreenTop := StrToInt(Edit1.Text);

  if not CheckBox8.Checked then
    RegSelectClick(Self);
end;

procedure TfrmMain.Edit1KeyPress(Sender: TObject; var Key: Char);
begin
  if Sender is TEdit then
    begin
      if Key in [#8, '0'..'9'] then
         Exit;
      Key := #0;
    end;
end;

procedure TfrmMain.ComboBox3Change(Sender: TObject);
begin
  case ComboBox3.ItemIndex of
    0: ScrCamera.Colors := pf8bit;
    1: ScrCamera.Colors := pf16bit;
    2: ScrCamera.Colors := pf24bit;
    3: ScrCamera.Colors := pf32bit;
  end;
end;

procedure TfrmMain.Edit2Change(Sender: TObject);
var
  W, H: Integer;
begin
  ScrCamera.GetMinimumScreenSize(W, H);
  UpDownTop.Min    := W;
  UpDownLeft.Min   := H;
  UpDownWidth.Min  := W;
  UpDownHeight.Min := H;

  ScrCamera.GetMaximumScreenSize(W, H);
  UpDownTop.Max    := W;
  UpDownLeft.Max   := H;
  UpDownWidth.Max  := W;
  UpDownHeight.Max := H;

  if TEdit(Sender).Text = '' then
    TEdit(Sender).Text := '0';

  if StrToInt(Edit2.Text) > UpDownLeft.Max then
    Edit2.Text := IntToStr(UpDownLeft.Max);

  ScrCamera.ScreenLeft := StrToInt(Edit2.Text);

  if not CheckBox8.Checked then
    RegSelectClick(Self);
end;

procedure TfrmMain.Edit3Change(Sender: TObject);
var
  W, H: Integer;
begin
  ScrCamera.GetMinimumScreenSize(W, H);
  UpDownTop.Min    := W;
  UpDownLeft.Min   := H;
  UpDownWidth.Min  := W;
  UpDownHeight.Min := H;

  ScrCamera.GetMaximumScreenSize(W, H);
  UpDownTop.Max    := W;
  UpDownLeft.Max   := H;
  UpDownWidth.Max  := W;
  UpDownHeight.Max := H;

  if TEdit(Sender).Text = '' then
    TEdit(Sender).Text := '0';

  if StrToInt(Edit3.Text) > UpDownWidth.Max then
    Edit3.Text := IntToStr(UpDownWidth.Max);

  ScrCamera.ScreenWidth := StrToInt(Edit3.Text);

  if not CheckBox8.Checked then
    RegSelectClick(Self);
end;

procedure TfrmMain.Edit4Change(Sender: TObject);
var
  W, H: Integer;
begin
  ScrCamera.GetMinimumScreenSize(W, H);
  UpDownTop.Min    := W;
  UpDownLeft.Min   := H;
  UpDownWidth.Min  := W;
  UpDownHeight.Min := H;

  ScrCamera.GetMaximumScreenSize(W, H);
  UpDownTop.Max    := W;
  UpDownLeft.Max   := H;
  UpDownWidth.Max  := W;
  UpDownHeight.Max := H;

  if TEdit(Sender).Text = '' then
    TEdit(Sender).Text := '0';

  if StrToInt(Edit4.Text) > UpDownHeight.Max then
    Edit4.Text := IntToStr(UpDownHeight.Max);

  ScrCamera.ScreenHeight := StrToInt(Edit4.Text);

  if not CheckBox8.Checked then
    RegSelectClick(Self);
end;

procedure TfrmMain.AboutButClick(Sender: TObject);
begin
  ScrCamera.ShowAbout;
end;

procedure TfrmMain.CheckBox2Click(Sender: TObject);
begin
  ScrCamera.OvelayDrawing := CheckBox2.Checked;
end;

procedure TfrmMain.BitBtn1Click(Sender: TObject);
begin
  SaveDialog.InitialDir := ExtractFilePath(Edit8.Text);
  SaveDialog.FileName   := ExtractFileName(Edit8.Text);
  if SaveDialog.Execute then
    Edit8.Text := SaveDialog.FileName;
end;

procedure TfrmMain.FilterEffectsButClick(Sender: TObject);
begin
  frmFilterEffects.ShowModal;
end;

procedure TfrmMain.Edit9Change(Sender: TObject);
begin
  if TEdit(Sender).Text = '' then
    TEdit(Sender).Text := '0';
  if StrToInt(Edit9.Text) > UpDown4.Max then
    Edit9.Text      := IntToStr(UpDown4.Max);
  ScrCamera.UpdateRate := StrToInt(Edit9.Text);
end;

procedure TfrmMain.Edit7KeyPress(Sender: TObject; var Key: Char);
begin
  if Sender is TEdit then
    begin
      if Key in [#8, '0'..'9'] then
         Exit;
      Key := #0;
    end;
end;

procedure TfrmMain.CheckBox7Click(Sender: TObject);
begin
  ScrCamera.RecordAllMonitors := CheckBox7.Checked;
  if not CheckBox7.Checked then begin
    ComboBox4.Enabled := True;
    Label14.Enabled := True;
    end
  else begin
    ComboBox4.Enabled := False;
    Label14.Enabled := False;
    end;
end;

procedure TfrmMain.ComboBox4Change(Sender: TObject);
begin
  ScrCamera.CurrentMonitor := ComboBox4.ItemIndex + 1;
end;

procedure TfrmMain.FormShow(Sender: TObject);
var
  I: Integer;
begin
  Edit8.Text := ExtractFilePath(Application.ExeName) + 'CaptureDemo.avi';
  for I := 1 to ScrCamera.MonitorsCount do
    ComboBox4.Items.Add(IntToStr(I));
  ComboBox4.ItemIndex := ScrCamera.CurrentMonitor - 1;
  Edit1.Text := '0';
  Edit2.Text := '0';
  Edit3.Text := '300';
  Edit4.Text := '300';
end;

procedure TfrmMain.CheckBox10Click(Sender: TObject);
begin
  ScrCamera.EffectToOvelayDraw := CheckBox10.Checked;
end;

procedure TfrmMain.ScrCameraUpdate(Sender: TObject);
begin
  VideoCodecValue.Caption      := Format('%s', [ScrCamera.VideoCodecName]);
  ElapsedTimeValue.Caption     := Format('%s', [ScrCamera.ElapsedTime]);
  RealCapturingValue.Caption   := Format('%f', [ScrCamera.RealCapturingFPS]);
  CurrentCapturedValue.Caption := Format('%f', [ScrCamera.CurrentCapturedFPS]);
  FramesCapturedValue.Caption  := Format('%d', [ScrCamera.CapturedFrames]);
  DropedFramesValue.Caption    := Format('%d', [ScrCamera.DropedFrames]);
end;

procedure TfrmMain.ScrCameraError(Sender: TObject; ErrorMessage: String);
begin
  MessageBox(Application.MainForm.Handle, PChar(ErrorMessage), 'Error', MB_OK);
end;

procedure TfrmMain.ScrCameraStart(Sender: TObject);
begin
  ComboBox3.Enabled    := False;
  Edit8.Enabled        := False;
  BitBtn1.Enabled      := False;
  RecBut.Enabled       := False;
  StopBut.Enabled      := True;
  AudVidOptBut.Enabled := False;
  RegSelect.Enabled    := False;
  Edit1.Enabled        := False;
  Edit2.Enabled        := False;
  Edit3.Enabled        := False;
  Edit4.Enabled        := False;
  UpDownTop.Enabled    := Edit1.Enabled;
  UpDownLeft.Enabled   := Edit2.Enabled;
  UpDownWidth.Enabled  := Edit3.Enabled;
  UpDownHeight.Enabled := Edit4.Enabled;
end;

procedure TfrmMain.ScrCameraStop(Sender: TObject);
begin
  ComboBox3.Enabled    := True;
  Edit8.Enabled        := True;
  BitBtn1.Enabled      := True;
  RecBut.Enabled       := True;
  StopBut.Enabled      := False;
  AudVidOptBut.Enabled := True;
  RegSelect.Enabled    := True;
  RegSelectClick(Self);
end;

procedure TfrmMain.ScrCameraPreview(Sender: TObject; PreviewBitmap: TBitmap;
  Preview, Recording: Boolean);
begin
  if Preview then begin
    if not Recording then begin
      Label9.Caption := 'Preview... Cancel: Esc';
      end
    else begin
      Label9.Caption := 'Recording... Cancel: Esc -- Stop: Shift+Esc';
      end;
    end
  else begin
    if not Recording then
      Label9.Caption := 'Preview Off...'
    else
      Label9.Caption := 'Recording... Cancel: Esc -- Stop: Shift+Esc';
    end;

  Image1.Picture.Assign(PreviewBitmap);

  Edit1.Text := IntToStr(ScrCamera.ScreenTop);
  Edit2.Text := IntToStr(ScrCamera.ScreenLeft);
  Edit3.Text := IntToStr(ScrCamera.ScreenWidth);
  Edit4.Text := IntToStr(ScrCamera.ScreenHeight);

  CheckBox8.Checked := Preview;
end;

procedure TfrmMain.ScrCameraSaving(Sender: TObject; Percent: Integer;
  StatusCaption: String; var Continue: Boolean);
begin
  Application.ProcessMessages;

  if not FCancel then begin

    if not BCancel.Enabled then begin
      BCancel.Enabled := True;
      LStatus.Enabled := True;
      LStatus.Caption := StatusCaption;
      end;

    Progress.Position := Percent;

    if Percent = 100 then begin
      FCancel         := False;
      LStatus.Enabled := False;
      BCancel.Enabled := False;
      LStatus.Caption := 'No Status';
      Progress.Position := 0;
      end;

    Continue          := True;
    end
  else begin
    FCancel           := False;
    LStatus.Enabled   := False;
    BCancel.Enabled   := False;
    LStatus.Caption   := 'No Status';
    Progress.Position := 0;
    Continue          := False;
    end;
end;

procedure TfrmMain.ScrCameraDeleting(Sender: TObject; Percent: Integer;
  StatusCaption: String; var Continue: Boolean);
begin
  Application.ProcessMessages;

  if not FCancel then begin

    if not BCancel.Enabled then begin
      BCancel.Enabled := True;
      LStatus.Enabled := True;
      LStatus.Caption := StatusCaption;
      end;

    Progress.Position := Percent;

    if Percent = 100 then begin
      FCancel         := False;
      LStatus.Enabled := False;
      BCancel.Enabled := False;
      LStatus.Caption := 'No Status';
      Progress.Position := 0;
      end;

    Continue          := True;
    end
  else begin
    FCancel           := False;
    LStatus.Enabled   := False;
    BCancel.Enabled   := False;
    LStatus.Caption   := 'No Status';
    Progress.Position := 0;
    Continue          := False;
    end;
end;

procedure TfrmMain.ScrCameraOverlay(Sender: TObject; HDCBitmap: HDC; bmpWidth,
  bmpHeight: Integer);
var
  AppName,
  Ver    : String;
  Size   : TSize;
  Bmp    : TBitmap;
  Canvas : TCanvas;
begin
  AppName := 'Professional Screen Camera Demo';
  Ver     := 'v' + ScrCamera.PSC_Version;

  // Set transparent background image
  SetBkMode(HDCBitmap, TRANSPARENT);

  // Draw app name over image
  GetTextExtentPoint32(HDCBitmap, PChar(AppName), Length(AppName), Size);
  SetTextColor(HDCBitmap, ColorToRGB(clBlack));
  TextOut(HDCBitmap, (bmpWidth div 2) - (Size.cx div 2) + 2, (bmpHeight div 2) + 2, PChar(AppName), Length(AppName));
  SetTextColor(HDCBitmap, ColorToRGB(clLime));
  TextOut(HDCBitmap, (bmpWidth div 2) - (Size.cx div 2), (bmpHeight div 2), PChar(AppName), Length(AppName));

  // Draw version text over image
  GetTextExtentPoint32(HDCBitmap, PChar(Ver), Length(Ver), Size);
  SetTextColor(HDCBitmap, ColorToRGB(clBlack));
  TextOut(HDCBitmap, (bmpWidth div 2) - (Size.cx div 2) + 2, (bmpHeight div 2) + 22, PChar(Ver), Length(Ver));
  SetTextColor(HDCBitmap, ColorToRGB(clRed));
  TextOut(HDCBitmap, (bmpWidth div 2) - (Size.cx div 2), (bmpHeight div 2) + 20, PChar(Ver), Length(Ver));

  // Draw logo trademark over image
  Bmp := TBitmap.Create;
  try
    Bmp.LoadFromFile(ExtractFilePath(Application.ExeName) + 'Logo.bmp');
    Bmp.Transparent := True;
    Bmp.Canvas.Lock;
    Canvas := TCanvas.Create;
    try
      Canvas.Handle := HDCBitmap;
      Canvas.Lock;
      Canvas.Draw(((bmpWidth div 2) - (Bmp.Width div 2)),
                  ((bmpHeight div 2) - (Bmp.Height)),
                  Bmp);
      Canvas.Unlock;
    finally
      Canvas.Free;
      end;
  finally
    Bmp.Canvas.Unlock;
    Bmp.FreeImage;
    Bmp.Free;
  end;
end;

procedure TfrmMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

end.
