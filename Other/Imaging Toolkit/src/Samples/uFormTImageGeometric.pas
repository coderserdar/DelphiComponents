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
// $Log:  21511: uFormTImageGeometric.pas 
//
//   Rev 1.0    25-09-2003 23:25:48  mcm    Version: IMG 1.5

unit uFormTImageGeometric;

interface

{$IFDEF VER100} {$DEFINE MCMDELPHI3} {$ENDIF} { DELPHI 3  = VER100 }
{$IFDEF VER110} {$DEFINE MCMDELPHI3} {$ENDIF} { BUILDER 3 = VER110 }
uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls,
  mcmImageTypeDef, mcmImage, mcmImageKernel, mcmImageTransform;

type
  TFormGeoMain = class(TForm)
    SourceImage: TImage;
    lSource: TLabel;
    lResult: TLabel;
    ResultImage: TmcmImageCtrl;
    btnRotate45: TButton;
    btnScalex2: TButton;
    btnShear: TButton;
    btnStretch: TButton;
    ImageTransform: TmcmImageTransform;
    btnFun: TButton;
    lFunStep: TLabel;
    lTime: TLabel;
    cbInterpolate: TComboBox;
    lInterpolate: TLabel;
    procedure btnRotate45Click(Sender: TObject);
    procedure btnScalex2Click(Sender: TObject);
    procedure btnShearClick(Sender: TObject);
    procedure btnStretchClick(Sender: TObject);
    procedure btnFunClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure cbInterpolateChange(Sender: TObject);
  private
    { Private declarations }
    FInterpolate     : TmcmInterpolate;
    FBKColor         : TColorRef;

    FDuration        : double;
    {$IFDEF MCMDELPHI3}
    FStartTime       : TLargeInteger;
    FEndTime         : TLargeInteger;
    FPerformanceFreq : TLargeInteger;
    {$ELSE}
    FStartTime       : int64;
    FEndTime         : int64;
    FPerformanceFreq : int64;
    {$ENDIF}
  public
    { Public declarations }
  end;

var
  FormGeoMain: TFormGeoMain;

implementation

{$R *.DFM}

procedure TFormGeoMain.btnRotate45Click(Sender : TObject);
var TempImage : TmcmImage;
begin
  TempImage := TmcmImage.Create;
  TempImage.DibHandle := SourceImage.Picture.Bitmap.Handle;

  ImageTransform.SourceImage[0] := TempImage;
  ImageTransform.ResultImage := ResultImage.Image;
  ImageTransform.BKColor := FBKColor;

  // Get start time of processing.
  QueryPerformanceCounter(FStartTime);

  // Rotate image 45 degree
  ImageTransform.Rotate(True, 45);

  // Get end time and display processing time.
  QueryPerformanceCounter(FEndTime);
  {$IFDEF MCMDELPHI3}
    FDuration := (FEndTime.QuadPart - FStartTime.QuadPart) / FPerformanceFreq.QuadPart;
  {$ELSE}
    FDuration := (FEndTime - FStartTime) / FPerformanceFreq;
  {$ENDIF}
  lTime.Caption := 'Time: ' + FloatToStrF(1000.0 * FDuration, ffFixed, 15, 4) + ' ms';

  // Return rotated image back to Source.
  TempImage.ReleaseHandle;
  TempImage.Free;

  ResultImage.DrawImage;
end;

procedure TFormGeoMain.btnScalex2Click(Sender: TObject);
var TempImage : TmcmImage;
begin
  TempImage := TmcmImage.Create;
  TempImage.DibHandle := SourceImage.Picture.Bitmap.Handle;

  ImageTransform.SourceImage[0] := TempImage;
  ImageTransform.ResultImage := ResultImage.Image;

  // Scale image by a factor 2.
  ResultImage.Image.Width  := 2 * SourceImage.Picture.Bitmap.Width;
  ResultImage.Image.Height := 2 * SourceImage.Picture.Bitmap.Height;

  // Get start time of processing.
  QueryPerformanceCounter(FStartTime);

  ImageTransform.Resize(FInterpolate);

  // Get end time and display processing time.
  QueryPerformanceCounter(FEndTime);
  {$IFDEF MCMDELPHI3}
    FDuration := (FEndTime.QuadPart - FStartTime.QuadPart) / FPerformanceFreq.QuadPart;
  {$ELSE}
    FDuration := (FEndTime - FStartTime) / FPerformanceFreq;
  {$ENDIF}
  lTime.Caption := 'Time: ' + FloatToStrF(1000.0 * FDuration, ffFixed, 15, 4) + ' ms';

  TempImage.ReleaseHandle;
  TempImage.Free;

  ResultImage.DrawImage;
end;

procedure TFormGeoMain.btnShearClick(Sender: TObject);
var TempImage : TmcmImage;
begin
  TempImage := TmcmImage.Create;
  TempImage.DibHandle := SourceImage.Picture.Bitmap.Handle;

  ImageTransform.SourceImage[0] := TempImage;
  ImageTransform.ResultImage := ResultImage.Image;

  ImageTransform.Degree := 0.0;
  ImageTransform.xScale := 1.0;
  ImageTransform.yScale := 1.0;
  ImageTransform.xShear := 125.0 / (SourceImage.Picture.Bitmap.Width);
  ImageTransform.yShear := ImageTransform.xShear;
  ImageTransform.xDist  := 0;
  ImageTransform.yDist  := 0;
  ImageTransform.BKColor := FBKColor;

  // Get start time of processing.
  QueryPerformanceCounter(FStartTime);

  ImageTransform.Affine;

  // Get end time and display processing time.
  QueryPerformanceCounter(FEndTime);
  {$IFDEF MCMDELPHI3}
    FDuration := (FEndTime.QuadPart - FStartTime.QuadPart) / FPerformanceFreq.QuadPart;
  {$ELSE}
    FDuration := (FEndTime - FStartTime) / FPerformanceFreq;
  {$ENDIF}
  lTime.Caption := 'Time: ' + FloatToStrF(1000.0 * FDuration, ffFixed, 15, 4) + ' ms';

  TempImage.ReleaseHandle;
  TempImage.Free;

  ResultImage.DrawImage;
end;

procedure TFormGeoMain.btnStretchClick(Sender : TObject);
var TempImage : TmcmImage;
begin
  TempImage := TmcmImage.Create;
  TempImage.DibHandle := SourceImage.Picture.Bitmap.Handle;

  ImageTransform.SourceImage[0] := TempImage;
  ImageTransform.ResultImage := ResultImage.Image;

  ImageTransform.Degree := 0.0;
  ImageTransform.xScale := 4.0;
  ImageTransform.yScale := 2.0;
  ImageTransform.xShear := 0.0;
  ImageTransform.yShear := 0.0;
  ImageTransform.xDist  := 0;
  ImageTransform.yDist  := 0;

  // Get start time of processing.
  QueryPerformanceCounter(FStartTime);

  ImageTransform.Affine;

  // Get end time and display processing time.
  QueryPerformanceCounter(FEndTime);
  {$IFDEF MCMDELPHI3}
    FDuration := (FEndTime.QuadPart - FStartTime.QuadPart) / FPerformanceFreq.QuadPart;
  {$ELSE}
    FDuration := (FEndTime - FStartTime) / FPerformanceFreq;
  {$ENDIF}
  lTime.Caption := 'Time: ' + FloatToStrF(1000.0 * FDuration, ffFixed, 15, 4) + ' ms';

  TempImage.ReleaseHandle;
  TempImage.Free;

  ResultImage.DrawImage;
end;

procedure TFormGeoMain.btnFunClick(Sender: TObject);
var i         : integer;
    TempImage : TmcmImage;
begin
  TempImage := TmcmImage.Create;
  TempImage.DibHandle := SourceImage.Picture.Bitmap.Handle;
  ImageTransform.SourceImage[0] := TempImage;

  ResultImage.Image.Width := 425;
  ResultImage.Image.Height := 425;
  ResultImage.Image.ImageFormat := TempImage.ImageFormat;

  ImageTransform.KeepResultSize := True;

  ImageTransform.ResultImage := ResultImage.Image;
  ImageTransform.xShear := 0.0;
  ImageTransform.yShear := 0.0;
  ImageTransform.xDist  := 0;
  ImageTransform.yDist  := 0;
  ImageTransform.BKColor := FBKColor;
  for i := 1 to 299
  do begin
     // Get start time of processing.
     QueryPerformanceCounter(FStartTime);


     // Transform image.
     ImageTransform.Degree := (i * 3.59 / 3.0) - 359;
     ImageTransform.xScale := i / 100.0;
     ImageTransform.yScale := i / 100.0;
     ImageTransform.xDist  := -175 + i;
     if (ImageTransform.xDist > 0)
     then ImageTransform.xDist := 0;
     ImageTransform.yDist  := ImageTransform.xDist;
     ImageTransform.Affine;


     // Get end time and display processing time.
     QueryPerformanceCounter(FEndTime);
     {$IFDEF MCMDELPHI3}
       FDuration := (FEndTime.QuadPart - FStartTime.QuadPart) / FPerformanceFreq.QuadPart;
     {$ELSE}
       FDuration := (FEndTime - FStartTime) / FPerformanceFreq;
     {$ENDIF}
     lTime.Caption := 'Time: ' + FloatToStrF(1000.0 * FDuration, ffFixed, 15, 4) + ' ms';
     lFunStep.Caption := 'Step ' + IntToStr(i) + ' of 300';


     // Update result image on screen.
     ResultImage.DrawImage;
     ResultImage.Update;
  end;

  TempImage.ReleaseHandle;
  TempImage.Free;

  ImageTransform.KeepResultSize := False;
end;

procedure TFormGeoMain.FormCreate(Sender: TObject);
begin
  // Set-up time measurement of processes.
  QueryPerformanceFrequency(FPerformanceFreq);

  cbInterpolate.ItemIndex := 0;

  FBKColor := GetSysColor(COLOR_BTNFACE);
  FBKColor := RGB(GetBValue(FBKColor), GetGValue(FBKColor), GetRValue(FBKColor));
end;

procedure TFormGeoMain.cbInterpolateChange(Sender: TObject);
begin
  FInterpolate := TmcmInterpolate(cbInterpolate.ItemIndex);
  ImageTransform.Interpolate := FInterpolate;
end;

end.
