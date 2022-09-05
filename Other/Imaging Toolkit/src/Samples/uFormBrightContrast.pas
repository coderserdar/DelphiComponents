// $HDR$
//----------------------------------------------------------------------------//
// MCM DESIGN                                                                 //  
//                                                                            //  
// For further information / comments, visit our WEB site at                  //  
//   www.mcm-design.com                                                       //  
// or e-mail to                                                               //  
//   CustomerCare@mcm-design.dk                                               //  
//----------------------------------------------------------------------------//
//
// $Log:  21058: uFormBrightContrast.pas 
//
//    Rev 1.3    2014-02-02 21:10:08  mcm    Version: IMG 4.0
// Added support for Delphi XE2, 3, 4 and 5
//
//   Rev 1.2    20-12-2004 22:58:08  mcm
// Modified to use TmcmInt

//
//   Rev 1.1    29-09-2003 18:41:56  mcm    Version: IMG 1.6
// Added option to disable Range check.

//
//   Rev 1.0    29-07-2003 11:43:38  mcm

unit uFormBrightContrast;

{$Include 'mcmDefines.pas'}

interface

uses {$IFNDEF GE_DXE2}
      Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
      StdCtrls, ExtCtrls, Spin,
     {$ELSE}
      WinApi.Windows, WinApi.Messages, System.SysUtils, System.Classes, Vcl.Controls,
      Vcl.Graphics, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Samples.Spin, 
     {$ENDIF}
     mcmImage,
     mcmImageTypeDef,
     mcmImageDualView,
     mcmImageKernel,
     mcmImageColor,
     mcmShape,
     umcmIntE;

type
  TFormBrightContrast = class(TForm)
    DualView: TmcmImageDualView;
    gbBrightContrast  : TGroupBox;
    lBrightness       : TLabel;
    lContrast         : TLabel;
    btnOK             : TButton;
    btnCancel         : TButton;
    mcmImageColor     : TmcmImageColor;
    HistogramView: TmcmImageCtrl;
    seBrightness: TmcmIntSpin;
    seContrast: TmcmIntSpin;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure seBrightnessChange(Sender: TObject);
    procedure seContrastChange(Sender: TObject);
  private
    { Private declarations }
    FCreatedResult : boolean;
    {$IFDEF MCMDELPHI3}
    FStartTime       : TLargeInteger;
    FEndTime         : TLargeInteger;
    {$ELSE}
    FStartTime       : int64;
    FEndTime         : int64;
    {$ENDIF}
    FBrightness      : integer;
    FContrast        : integer;
    procedure SetBrightness(Value : integer);
    procedure SetContrast(Value : integer);
    function  GetResultImage : TmcmImage;
    function  GetSourceImage : TmcmImage;
    procedure SetResultImage(AImage : TmcmImage);
    procedure SetSourceImage(AImage : TmcmImage);
    procedure ShowHistogram;
    procedure UpdateResult;
  public
    { Public declarations }
    property Brightness : integer
      read   FBrightness
      write  SetBrightness;
    property Contrast : integer
      read   FContrast
      write  SetContrast;
    property ResultImage : TmcmImage
      read   GetResultImage
      write  SetResultImage;
    property SourceImage : TmcmImage
      read   GetSourceImage
      write  SetSourceImage;
    property TimeStart : {$IFDEF MCMDELPHI3} TLargeInteger {$ELSE} int64 {$ENDIF}
      read   FStartTime;
    property TimeEnd : {$IFDEF MCMDELPHI3} TLargeInteger {$ELSE} int64 {$ENDIF}
      read   FEndTime;
  end;

var FormBrightContrast : TFormBrightContrast;

implementation

{$IFDEF GE_DXE2}
  uses System.Types;
{$ELSE}
 {$IFDEF GE_D2009}
   uses Types;
 {$ENDIF}
{$ENDIF}

{$R *.DFM}

procedure TFormBrightContrast.FormCreate(Sender : TObject);
begin
  mcmImageColor.SourceImage[0] := Nil;
  FCreatedResult := False;
  HistogramView.Image.ImageFormat := IF_RGB24;
  // The boarder of HistogramView is 2 pixels heigh/wide, therefore we'll set
  // the histogram image 4 pixels less in height and width
  HistogramView.Image.Height := HistogramView.Height - 4;
  HistogramView.Image.Width  := HistogramView.Width - 4;
  // and we'll move the histogram image 2 pixels in the down and right.
  HistogramView.Image.SetOrigo(Point(2,2));
end; // TFormBrightContrast.FormCreate.


procedure TFormBrightContrast.FormDestroy(Sender : TObject);
begin
;
end; // TFormBrightContrast.FormDestroy.


procedure TFormBrightContrast.FormShow(Sender : TObject);
begin
  UpdateResult;
end; // TFormBrightContrast.FormShow.


procedure TFormBrightContrast.btnCancelClick(Sender : TObject);
begin
  if FCreatedResult
  then mcmImageColor.ResultImage.Free;
end; // TFormBrightContrast.btnCancelClick.


procedure TFormBrightContrast.SetBrightness(Value : integer);
begin
  FBrightness := Value;
  seBrightness.Value := Value;
end; // TFormBrightContrast.SetBrightness.


procedure TFormBrightContrast.SetContrast(Value : integer);
begin
  FContrast   := Value;
  seContrast.Value := Value;
end; // TFormBrightContrast.SetContrast.


function TFormBrightContrast.GetResultImage : TmcmImage;
begin
  Result := mcmImageColor.ResultImage;
end; // TFormBrightContrast.GetResultImage.


function TFormBrightContrast.GetSourceImage : TmcmImage;
begin
  Result := DualView.SourceImage;
end; // TFormBrightContrast.GetSourceImage.


procedure TFormBrightContrast.SetResultImage(AImage : TmcmImage);
begin
  //DualView.ResultImage := AImage;
end; // TFormBrightContrast.SetResultImage.


procedure TFormBrightContrast.SetSourceImage(AImage : TmcmImage);
begin
  DualView.SourceImage := AImage;
  mcmImageColor.SourceImage[0] := AImage;
end; // TFormBrightContrast.SetSourceImage.


procedure TFormBrightContrast.ShowHistogram;
var i         : word;
    pHist     : PVectorC;
    NoColors  : cardinal;
    MaxValue  : cardinal;
    NextValue : cardinal;
    hScale    : double;
    vScale    : double;
begin
  // Show the histogram of the result image after modifying brightness & contrast.
  // First clear the histogram view.
  HistogramView.Image.FillAll(255);
  HistogramView.Image.Canvas.Pen.Style := PSSOLID;

  // Obtain the intensity histogram.
  mcmImageColor.SourceImage[0] := mcmImageColor.ResultImage;
  mcmImageColor.GetHistogram;
  mcmImageColor.SourceImage[0] := DualView.SourceImage;

  pHist := mcmImageColor.IntHistogram;
  HistogramView.Image.Canvas.Pen.Color := RGB(0, 0, 0);

  if Assigned(pHist)
  then begin
       // Get horizontal and vertical scale factors, so that we can fit the
       // entire histogram into our image.
       NoColors := 256;
       // Get largest histogram value
       MaxValue := 0;
       NextValue := 0;
       for i := 0 to (NoColors - 1)
       do begin
          if (MaxValue < pHist^[i])
          then MaxValue := pHist^[i];
          if (NextValue < pHist^[i]) and (MaxValue > pHist^[i])
          then NextValue := pHist^[i];
       end;
       // Calculate scale factors
       vScale := HistogramView.Image.Height / MaxValue;
       hScale := HistogramView.Image.Width / NoColors;

       // Plot the histogram.
       for i := 0 to (NoColors - 1)
       do begin
          HistogramView.Image.Canvas.MoveTo(Round(i * hScale), HistogramView.Image.Height - 1);
          HistogramView.Image.Canvas.LineTo(Round(i * hScale), HistogramView.Image.Height - 1 - Round(vScale * pHist^[i]));
       end;
       HistogramView.DrawImage;
  end;
end; // TFormBrightContrast.ShowHistogram.


procedure TFormBrightContrast.UpdateResult;
begin
  if Assigned(mcmImageColor)
  then if Assigned(mcmImageColor.SourceImage[0])
       then begin
            // Brightness is the amount the intensity values are shifted.
            // FBrightness = 0 -> no intensity shift
            // FBrightness > 0 -> Shift towards brighter intensities
            // FBrightness < 0 -> Shift towards darker intensities
            FBrightness := seBrightness.Value;
            // FContrast is the amount used to stretch or contract the contrast.
            // FContrast = 0 -> no change in contrast
            // FContrast > 0 -> The contrast is stretched by the factor = 255 / (255 - Width).
            // FContrast < 0 -> The contrast is contracted by the factor = (255.0 + Width) / 255.0
            FContrast  := Round(2.55 * seContrast.Value);

            if Not(Assigned(mcmImageColor.ResultImage))
            then begin
                 mcmImageColor.ResultImage := TmcmImage.Create;
                 mcmImageColor.ResultImage.CopyFormat(mcmImageColor.SourceImage[0]);
                 {
                 mcmImageColor.ResultImage.Width  := mcmImageColor.SourceImage[0].Width;
                 mcmImageColor.ResultImage.Height := mcmImageColor.SourceImage[0].Height;
                 mcmImageColor.ResultImage.ImageFormat := mcmImageColor.SourceImage[0].ImageFormat;
                 mcmImageColor.ResultImage.Palette := mcmImageColor.SourceImage[0].Palette;
                 }
                 DualView.ResultImage := mcmImageColor.ResultImage;
                 FCreatedResult := True;
            end;

            // Get start time
            QueryPerformanceCounter(FStartTime);
            mcmImageColor.BrightContrast(FBrightness, FContrast);
            QueryPerformanceCounter(FEndTime);

            DualView.UpdateResultView;
            ShowHistogram;
       end;
end; // TFormBrightContrast.UpdateResult.


procedure TFormBrightContrast.seBrightnessChange(Sender : TObject);
begin
  if (seBrightness.Text <> '')
  then UpdateResult;
end; // TFormBrightContrast.seBrightnessChange.


procedure TFormBrightContrast.seContrastChange(Sender : TObject);
begin
  if (seContrast.Text <> '')
  then UpdateResult;
end; // TFormBrightContrast.seContrastChange.


{$IFDEF RANGE_OFF}{$UNDEF RANGE_OFF}{$R+}{$ENDIF}

end.
