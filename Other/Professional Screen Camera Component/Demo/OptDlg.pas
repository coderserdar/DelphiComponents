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

unit OptDlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, MainUnit, Math;

{$WARNINGS OFF}
{$RANGECHECKS OFF}

type
  TfrmOption = class(TForm)
    Compressor: TGroupBox;
    VideoCompressor: TComboBox;
    AboutB: TButton;
    VideoQuality: TTrackBar;
    Label1: TLabel;
    LabelQuality: TLabel;
    GroupBox1: TGroupBox;
    ConfigB: TButton;
    Button2: TButton;
    Button3: TButton;
    EditKeyFrames: TEdit;
    Label3: TLabel;
    Label2: TLabel;
    TrackBarRecord: TTrackBar;
    Label4: TLabel;
    Label6: TLabel;
    labelmspFrecord: TLabel;
    AudioFormat: TComboBox;
    Label8: TLabel;
    Label9: TLabel;
    AudioVolume: TTrackBar;
    Label10: TLabel;
    AudioInput: TComboBox;
    Label11: TLabel;
    LabelVolume: TLabel;
    Label12: TLabel;
    TrackBarPlayback: TTrackBar;
    Label7: TLabel;
    labelFPSPlayback: TLabel;
    Label5: TLabel;
    Label13: TLabel;
    AutoMode: TCheckBox;
    AudioRecord: TCheckBox;
    procedure AboutBClick(Sender: TObject);
    procedure ConfigBClick(Sender: TObject);
    procedure TrackBarPlaybackChange(Sender: TObject);
    procedure VideoQualityChange(Sender: TObject);
    procedure AudioVolumeChange(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure AudioInputClick(Sender: TObject);
    procedure TrackBarRecordChange(Sender: TObject);
    procedure AutoModeClick(Sender: TObject);
    procedure AudioRecordClick(Sender: TObject);
    procedure VideoCompressorClick(Sender: TObject);
  private
    { Private declarations }
    procedure RefreshCompressorButtons;
    procedure UpdateAdjustSliderVal;
  public
    { Public declarations }
    Auto: Boolean;
  end;

var
  frmOption: TfrmOption;

implementation

{$R *.dfm}

procedure TfrmOption.AboutBClick(Sender: TObject);
var
  Idx: Integer;
begin
  Idx := VideoCompressor.ItemIndex;
  frmMain.ScrCamera.CompressorAbout(Idx, WindowHandle);
end;

procedure TfrmOption.ConfigBClick(Sender: TObject);
var
  Idx: Integer;
begin
  Idx := VideoCompressor.ItemIndex;
  frmMain.ScrCamera.CompressorConfigure(Idx, WindowHandle);
end;

procedure TfrmOption.RefreshCompressorButtons;
var
  Idx: Integer;
  About,
  Config: Boolean;
begin
  Idx                  := VideoCompressor.ItemIndex;
  frmMain.ScrCamera.CompressorHasFeatures(Idx, About, Config);
  AboutB.Enabled       := About;
  ConfigB.Enabled      := Config;
  VideoQuality.Enabled := VideoCompressor.ItemIndex > -1;
  Label1.Enabled       := VideoCompressor.ItemIndex > -1;
  LabelQuality.Enabled := VideoCompressor.ItemIndex > -1;
end;


procedure TfrmOption.UpdateAdjustSliderVal;
var
  LmspFRecord,
  LFPSPlayback: Integer;
begin
  LmspFRecord              := TrackBarRecord.Position;
  LFPSPlayback             := TrackBarPlayback.Position;
  LabelmspfRecord.Caption  := Format('%d Millisecond', [LmspFRecord]);
  LabelFPSPlayback.Caption := Format('%d fps', [LFPSPlayback]);
  LabelQuality.Caption     := IntToStr(VideoQuality.Position);
  LabelVolume.Caption      := IntToStr(AudioVolume.Position);

  frmMain.ScrCamera.SetAudioInputVolume(AudioInput.ItemIndex, AudioVolume.Position);
end;


procedure TfrmOption.TrackBarPlaybackChange(Sender: TObject);
begin
  if (Sender = TrackBarPlayback) and (AutoMode.Checked) then
    TrackBarRecord.Position := 1000 div TrackBarPlayback.Position;
  UpdateAdjustSliderVal;
end;

procedure TfrmOption.VideoQualityChange(Sender: TObject);
begin
  UpdateAdjustSliderVal;
end;

procedure TfrmOption.AudioVolumeChange(Sender: TObject);
begin
  UpdateAdjustSliderVal;
end;

procedure TfrmOption.Button2Click(Sender: TObject);
begin
  UpdateAdjustSliderVal;
  frmMain.ScrCamera.SelectedCompressor := VideoCompressor.ItemIndex;
  frmMain.ScrCamera.CompressionQuality := VideoQuality.Position * 1000;
  frmMain.ScrCamera.KeyFramesEvery     := StrToInt(EditKeyFrames.Text);
  frmMain.ScrCamera.Record_MSPF        := TrackBarRecord.Position;
  frmMain.ScrCamera.PlayBack_FPS       := TrackBarPlayback.Position;
  frmMain.ScrCamera.UseAudioRecord     := AudioRecord.Checked;
  frmMain.ScrCamera.AudioFormatsIndex  := AudioFormat.ItemIndex;
end;

procedure TfrmOption.FormShow(Sender: TObject);
begin
  VideoCompressor.Items.Assign(frmMain.ScrCamera.VideoCodecsList);
  AudioFormat.Items.Assign(frmMain.ScrCamera.AudioFormatsList);
  AudioInput.Items.Assign(frmMain.ScrCamera.GetAudioInputInfo.AudioInputNames);

  VideoCompressor.ItemIndex := frmMain.ScrCamera.SelectedCompressor;
  AudioFormat.ItemIndex     := frmMain.ScrCamera.AudioFormatsIndex;
  AudioInput.ItemIndex      := frmMain.ScrCamera.GetAudioInputInfo.AudioInputIndex;

  if frmMain.ScrCamera.GetAudioInputInfo.AudioInputEnabled then begin
    if AudioRecord.Checked then begin
      AudioVolume.Enabled   := True;
      LabelVolume.Enabled   := True;
      end;
    AudioVolume.Position    := frmMain.ScrCamera.GetAudioInputInfo.AudioInputVolume;
    end
  else begin
    AudioVolume.Position    := 0;
    AudioVolume.Enabled     := False;
    LabelVolume.Enabled     := False;
    end;

  TrackBarRecord.Position   := frmMain.ScrCamera.Record_MSPF;
  TrackBarPlayback.Position := frmMain.ScrCamera.PlayBack_FPS;

  EditKeyFrames.Text        := Format('%d', [frmMain.ScrCamera.KeyFramesEvery]);
  VideoQuality.Position     := frmMain.ScrCamera.CompressionQuality div 100;

  RefreshCompressorButtons;
end;

procedure TfrmOption.Button3Click(Sender: TObject);
begin
  Close;
end;

procedure TfrmOption.AudioInputClick(Sender: TObject);
begin
  frmMain.ScrCamera.SetAudioInputIndex(AudioInput.ItemIndex);
  if frmMain.ScrCamera.GetAudioInputInfo.AudioInputEnabled then begin
    AudioVolume.Enabled  := True;
    LabelVolume.Enabled  := True;
    AudioVolume.Position := frmMain.ScrCamera.GetAudioInputInfo.AudioInputVolume;
    end
  else begin
    AudioVolume.Position := 0;
    AudioVolume.Enabled  := False;
    LabelVolume.Enabled  := False;
    end;
end;

procedure TfrmOption.TrackBarRecordChange(Sender: TObject);
begin
  if (Sender = TrackBarRecord) and (AutoMode.Checked) then
    TrackBarPlayback.Position := 1000 div TrackBarRecord.Position;
  UpdateAdjustSliderVal;
end;

procedure TfrmOption.AutoModeClick(Sender: TObject);
begin
  if AutoMode.Checked then
    TrackBarRecord.Position := 1000 div TrackBarPlayback.Position;
end;

procedure TfrmOption.AudioRecordClick(Sender: TObject);
begin
  if AudioRecord.Checked then begin
    Label9.Enabled         := True;
    AudioFormat.Enabled    := True;
    Label11.Enabled        := True;
    AudioInput.Enabled     := True;
    Label10.Enabled        := True;
    if frmMain.ScrCamera.GetAudioInputInfo.AudioInputEnabled then begin
      AudioVolume.Enabled  := True;
      LabelVolume.Enabled  := True;
      AudioVolume.Position := frmMain.ScrCamera.GetAudioInputInfo.AudioInputVolume;
      end
    else begin
      AudioVolume.Enabled  := False;
      LabelVolume.Enabled  := False;
      AudioVolume.Position := 0;
      end;
    end
  else begin
    Label9.Enabled         := False;
    AudioFormat.Enabled    := False;
    Label11.Enabled        := False;
    AudioInput.Enabled     := False;
    Label10.Enabled        := False;
    AudioVolume.Enabled    := False;
    LabelVolume.Enabled    := False;
    end;
end;

procedure TfrmOption.VideoCompressorClick(Sender: TObject);
begin
	RefreshCompressorButtons;
end;

end.
