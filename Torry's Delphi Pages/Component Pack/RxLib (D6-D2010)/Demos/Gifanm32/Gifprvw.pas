{*******************************************************}
{                                                       }
{     Delphi VCL Extensions (RX) demo program           }
{                                                       }
{     Copyright (c) 1997 Master-Bank                    }
{                                                       }
{*******************************************************}

unit GIFPrvw;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, ExtCtrls, StdCtrls, rxSpeedBar, RxGIF, rxGIFCtrl, rxPlacemnt,
  RXSlider, rxAnimate;

type
  TPreviewForm = class(TForm)
    SpeedBar: TSpeedBar;
    SpeedbarSection1: TSpeedbarSection;
    PlayBtn: TSpeedItem;
    StopBtn: TSpeedItem;
    RewindBtn: TSpeedItem;
    BackBtn: TSpeedItem;
    NextBtn: TSpeedItem;
    ForwardBtn: TSpeedItem;
    Label1: TLabel;
    FrameNo: TLabel;
    SliderPanel: TPanel;
    Slider: TRxSlider;
    ImagePanel: TPanel;
    Image: TRxGIFAnimator;
    procedure ImageFrameChanged(Sender: TObject);
    procedure SliderChange(Sender: TObject);
    procedure PlayBtnClick(Sender: TObject);
    procedure StopBtnClick(Sender: TObject);
    procedure RewindBtnClick(Sender: TObject);
    procedure BackBtnClick(Sender: TObject);
    procedure NextBtnClick(Sender: TObject);
    procedure ForwardBtnClick(Sender: TObject);
    procedure ImageChanged(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure ImageStartStop(Sender: TObject);
  private
    { Private declarations }
    procedure EnableButtons;
    procedure CalculateFormSize;
  public
    { Public declarations }
  end;

procedure PreviewGIF(AImage: TGIFImage);

implementation

uses
  ShellAPI, rxMaxMin;

{$R *.DFM}

procedure PreviewGIF(AImage: TGIFImage);
begin
  with TPreviewForm.Create(Application) do
  try
    Image.Image := AImage;
    Image.Loop := AImage.Looping;
    if not AImage.Empty then Image.Image.FrameIndex := 0;
    if PlayBtn.Enabled then PlayBtnClick(nil);      
    ShowModal;
  finally
    Free;
  end;
end;

{ TPreviewForm }

procedure TPreviewForm.CalculateFormSize;
var
  NewSize: TPoint;
begin
  NewSize.X := Max(Image.Image.ScreenWidth + 4, 270);
  Inc(NewSize.X, (ImagePanel.BorderWidth + ImagePanel.BevelWidth) * 2);
  NewSize.Y := Max(Image.Image.ScreenHeight + 4, 70);
  Inc(NewSize.Y, (ImagePanel.BorderWidth + ImagePanel.BevelWidth) * 2 +
    SpeedBar.Height + SliderPanel.Height);
  NewSize.X := Min(NewSize.X, Screen.Width);
  NewSize.Y := Min(NewSize.Y, Screen.Height);
  ClientWidth := NewSize.X;
  ClientHeight := NewSize.Y;
end;

procedure TPreviewForm.EnableButtons;
begin
  PlayBtn.Enabled := not Image.Animate and (Image.Image.Count > 1);
  StopBtn.Enabled := Image.Animate;
  RewindBtn.Enabled := Image.FrameIndex > 0;
  BackBtn.Enabled := ((Image.FrameIndex > 0) or Image.Loop)
    and (Image.Image.Count > 1);
  NextBtn.Enabled := ((Image.FrameIndex < Image.Image.Count - 1) or Image.Loop)
    and (Image.Image.Count > 1);
  ForwardBtn.Enabled := Image.FrameIndex < Image.Image.Count - 1;
end;

procedure TPreviewForm.ImageFrameChanged(Sender: TObject);
begin
  Slider.Value := Image.FrameIndex;
  FrameNo.Caption := IntToStr(Image.FrameIndex + 1);
  EnableButtons;
end;

procedure TPreviewForm.SliderChange(Sender: TObject);
begin
  Image.FrameIndex := Slider.Value;
  Slider.Value := Image.FrameIndex;
end;

procedure TPreviewForm.PlayBtnClick(Sender: TObject);
begin
  Image.Animate := True;
  EnableButtons;
end;

procedure TPreviewForm.StopBtnClick(Sender: TObject);
begin
  Image.Animate := False;
  EnableButtons;
end;

procedure TPreviewForm.RewindBtnClick(Sender: TObject);
begin
  Image.FrameIndex := 0;
end;

procedure TPreviewForm.BackBtnClick(Sender: TObject);
var
  NewIndex: Integer;
begin
  NewIndex := Image.FrameIndex - 1;
  if NewIndex < 0 then begin
    if Image.Loop then NewIndex := Image.Image.Count + NewIndex
    else NewIndex := 0;
  end;
  Image.FrameIndex := NewIndex;
end;

procedure TPreviewForm.NextBtnClick(Sender: TObject);
var
  NewIndex: Integer;
begin
  NewIndex := Image.FrameIndex + 1;
  if NewIndex >= Image.Image.Count then begin
    if Image.Loop then NewIndex := NewIndex - Image.Image.Count
    else NewIndex := Image.Image.Count - 1;
  end;
  Image.FrameIndex := NewIndex;
end;

procedure TPreviewForm.ForwardBtnClick(Sender: TObject);
begin
  Image.FrameIndex := Image.Image.Count - 1;
end;

procedure TPreviewForm.ImageChanged(Sender: TObject);
begin
  Slider.MaxValue := Max(1, Image.Image.Count - 1);
  FrameNo.Caption := IntToStr(Image.FrameIndex + 1);
  EnableButtons;
  Slider.Enabled := Image.Image.Count > 1;
  SpeedBar.Enabled := Slider.Enabled;
  CalculateFormSize;
end;

procedure TPreviewForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_F9) and (PlayBtn.Enabled) then begin
    PlayBtnClick(nil);
    Key := 0;
  end;
end;

procedure TPreviewForm.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #27 then begin
    if Image.Animate then StopBtnClick(nil)
    else ModalResult := mrCancel;
  end
  else if Key = Char(VK_RETURN) then begin
    if not Image.Animate then PlayBtnClick(nil);
  end;
end;

procedure TPreviewForm.ImageStartStop(Sender: TObject);
begin
  EnableButtons;
end;

end.
