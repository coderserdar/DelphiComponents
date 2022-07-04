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

unit FilterEffects;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, ExtCtrls, MainUnit, ScrCam;

type
  TfrmFilterEffects = class(TForm)
    BrightnessBox: TGroupBox;
    BrightnessTrackBar: TTrackBar;
    BrightnessLabel: TLabel;
    ContrastBox: TGroupBox;
    ContrastLabel: TLabel;
    ContrastTrackBar: TTrackBar;
    ColorAdjustingBox: TGroupBox;
    RedLabel: TLabel;
    RedTrackBar: TTrackBar;
    GreenTrackBar: TTrackBar;
    GreenLabel: TLabel;
    BlueTrackBar: TTrackBar;
    BlueLabel: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    SaturationBox: TGroupBox;
    SaturationLabel: TLabel;
    SaturationTrackBar: TTrackBar;
    SolarizeBox: TGroupBox;
    NoiseLabel: TLabel;
    NoiseTrackBar: TTrackBar;
    CheckGrayScale: TCheckBox;
    ScreenRotation: TRadioGroup;
    OkBtn: TButton;
    UseBrightnessEffect: TCheckBox;
    UseContrastEffect: TCheckBox;
    UseColorAdjustingEffect: TCheckBox;
    UseSaturationEffect: TCheckBox;
    UseNoiseEffect: TCheckBox;
    UseScreenRotation: TCheckBox;
    DefaultValueBtn: TButton;
    procedure CheckGrayScaleClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure OkBtnClick(Sender: TObject);
    procedure UseBrightnessEffectClick(Sender: TObject);
    procedure BrightnessTrackBarChange(Sender: TObject);
    procedure ContrastTrackBarChange(Sender: TObject);
    procedure RedTrackBarChange(Sender: TObject);
    procedure GreenTrackBarChange(Sender: TObject);
    procedure BlueTrackBarChange(Sender: TObject);
    procedure SaturationTrackBarChange(Sender: TObject);
    procedure NoiseTrackBarChange(Sender: TObject);
    procedure UseContrastEffectClick(Sender: TObject);
    procedure UseColorAdjustingEffectClick(Sender: TObject);
    procedure UseSaturationEffectClick(Sender: TObject);
    procedure UseNoiseEffectClick(Sender: TObject);
    procedure UseScreenRotationClick(Sender: TObject);
    procedure ScreenRotationClick(Sender: TObject);
    procedure DefaultValueBtnClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmFilterEffects: TfrmFilterEffects;

implementation

{$R *.dfm}

procedure TfrmFilterEffects.CheckGrayScaleClick(Sender: TObject);
begin
  frmMain.ScrCamera.GrayScale := CheckGrayScale.Checked;
end;

procedure TfrmFilterEffects.FormShow(Sender: TObject);
begin
  UseBrightnessEffect.Checked     := frmMain.ScrCamera.UseBrightness;
  BrightnessTrackBar.Position     := frmMain.ScrCamera.EffectBrightness;
  BrightnessLabel.Caption         := IntToStr(frmMain.ScrCamera.EffectBrightness);

  UseContrastEffect.Checked       := frmMain.ScrCamera.UseContrast;
  ContrastTrackBar.Position       := frmMain.ScrCamera.EffectContrast;
  ContrastLabel.Caption           := IntToStr(frmMain.ScrCamera.EffectContrast);

  UseColorAdjustingEffect.Checked := frmMain.ScrCamera.UseColorAdjusting;
  RedTrackBar.Position            := frmMain.ScrCamera.EffectRedValue;
  RedLabel.Caption                := IntToStr(frmMain.ScrCamera.EffectRedValue);
  GreenTrackBar.Position          := frmMain.ScrCamera.EffectGreenValue;
  GreenLabel.Caption              := IntToStr(frmMain.ScrCamera.EffectGreenValue);
  BlueTrackBar.Position           := frmMain.ScrCamera.EffectBlueValue;
  BlueLabel.Caption               := IntToStr(frmMain.ScrCamera.EffectBlueValue);

  UseSaturationEffect.Checked     := frmMain.ScrCamera.UseSaturation;
  SaturationTrackBar.Position     := frmMain.ScrCamera.EffectSaturation;
  SaturationLabel.Caption         := IntToStr(frmMain.ScrCamera.EffectSaturation);

  UseNoiseEffect.Checked          := frmMain.ScrCamera.UseNoise;
  NoiseTrackBar.Position          := frmMain.ScrCamera.EffectNoise;
  NoiseLabel.Caption              := IntToStr(frmMain.ScrCamera.EffectNoise);

  UseScreenRotation.Checked       := frmMain.ScrCamera.UseRotateImage;
  case frmMain.ScrCamera.EffectScreenRotate of
    R90D      : ScreenRotation.ItemIndex := 0;
    R180D     : ScreenRotation.ItemIndex := 1;
    R270D     : ScreenRotation.ItemIndex := 2;
    Mirror0D  : ScreenRotation.ItemIndex := 3;
    Mirror180D: ScreenRotation.ItemIndex := 4;
  end;

  CheckGrayScale.Checked          := frmMain.ScrCamera.GrayScale;
end;

procedure TfrmFilterEffects.OkBtnClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmFilterEffects.UseBrightnessEffectClick(Sender: TObject);
begin
  frmMain.ScrCamera.UseBrightness := UseBrightnessEffect.Checked;
end;

procedure TfrmFilterEffects.BrightnessTrackBarChange(Sender: TObject);
begin
  frmMain.ScrCamera.EffectBrightness := BrightnessTrackBar.Position;
  BrightnessLabel.Caption := IntToStr(frmMain.ScrCamera.EffectBrightness);
end;

procedure TfrmFilterEffects.ContrastTrackBarChange(Sender: TObject);
begin
  frmMain.ScrCamera.EffectContrast := ContrastTrackBar.Position;
  ContrastLabel.Caption := IntToStr(frmMain.ScrCamera.EffectContrast);
end;

procedure TfrmFilterEffects.RedTrackBarChange(Sender: TObject);
begin
  frmMain.ScrCamera.EffectRedValue := RedTrackBar.Position;
  RedLabel.Caption      := IntToStr(frmMain.ScrCamera.EffectRedValue);
end;

procedure TfrmFilterEffects.GreenTrackBarChange(Sender: TObject);
begin
  frmMain.ScrCamera.EffectGreenValue := GreenTrackBar.Position;
  GreenLabel.Caption      := IntToStr(frmMain.ScrCamera.EffectGreenValue);
end;

procedure TfrmFilterEffects.BlueTrackBarChange(Sender: TObject);
begin
  frmMain.ScrCamera.EffectBlueValue := BlueTrackBar.Position;
  BlueLabel.Caption      := IntToStr(frmMain.ScrCamera.EffectBlueValue);
end;

procedure TfrmFilterEffects.SaturationTrackBarChange(Sender: TObject);
begin
  frmMain.ScrCamera.EffectSaturation := SaturationTrackBar.Position;
  SaturationLabel.Caption := IntToStr(frmMain.ScrCamera.EffectSaturation);
end;

procedure TfrmFilterEffects.NoiseTrackBarChange(Sender: TObject);
begin
  frmMain.ScrCamera.EffectNoise := NoiseTrackBar.Position;
  NoiseLabel.Caption := IntToStr(frmMain.ScrCamera.EffectNoise);
end;

procedure TfrmFilterEffects.UseContrastEffectClick(Sender: TObject);
begin
  frmMain.ScrCamera.UseContrast := UseContrastEffect.Checked;
end;

procedure TfrmFilterEffects.UseColorAdjustingEffectClick(Sender: TObject);
begin
  frmMain.ScrCamera.UseColorAdjusting := UseColorAdjustingEffect.Checked;
end;

procedure TfrmFilterEffects.UseSaturationEffectClick(Sender: TObject);
begin
  frmMain.ScrCamera.UseSaturation := UseSaturationEffect.Checked;
end;

procedure TfrmFilterEffects.UseNoiseEffectClick(Sender: TObject);
begin
  frmMain.ScrCamera.UseNoise := UseNoiseEffect.Checked;
end;

procedure TfrmFilterEffects.UseScreenRotationClick(Sender: TObject);
begin
  frmMain.ScrCamera.UseRotateImage := UseScreenRotation.Checked;
end;

procedure TfrmFilterEffects.ScreenRotationClick(Sender: TObject);
begin
  case ScreenRotation.ItemIndex of
    0: frmMain.ScrCamera.EffectScreenRotate := R90D;
    1: frmMain.ScrCamera.EffectScreenRotate := R180D;
    2: frmMain.ScrCamera.EffectScreenRotate := R270D;
    3: frmMain.ScrCamera.EffectScreenRotate := Mirror0D;
    4: frmMain.ScrCamera.EffectScreenRotate := Mirror180D;
  end;
end;

procedure TfrmFilterEffects.DefaultValueBtnClick(Sender: TObject);
begin
  BrightnessTrackBar.Position     := 0;
  ContrastTrackBar.Position       := 0;
  RedTrackBar.Position            := 0;
  GreenTrackBar.Position          := 0;
  BlueTrackBar.Position           := 0;
  SaturationTrackBar.Position     := 0;
  NoiseTrackBar.Position          := 0;
  ScreenRotation.ItemIndex        := 0;
  UseBrightnessEffect.Checked     := False;
  UseContrastEffect.Checked       := False;
  UseColorAdjustingEffect.Checked := False;
  UseSaturationEffect.Checked     := False;
  UseNoiseEffect.Checked          := False;
  UseScreenRotation.Checked       := False;
  CheckGrayScale.Checked          := False;
end;

end.
