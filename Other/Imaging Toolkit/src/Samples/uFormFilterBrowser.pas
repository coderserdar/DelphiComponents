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
// $Log:  17792: uFormFilterBrowser.pas 
//
//    Rev 1.7    2014-02-02 21:10:08  mcm    Version: IMG 4.0
// Added support for Delphi XE2, 3, 4 and 5
//
//    Rev 1.6    18-02-2006 20:05:54  mcm
// Added MeanHarmonic and MeanContraHarmonic filters.
//
//    Rev 1.5    22/11/2005 20:27:20  mcm    Version: IMG 2.10
// Modified dialogue to use TmcmImageDualView.
// Added support for Shen-Castan edge filter.
//
//   Rev 1.4    23-05-2005 22:02:56  mcm    Version: IMG 2.9
// Added Marr-Hildreth and Canny filter.

//
//   Rev 1.3    20-12-2004 22:59:36  mcm
// Modified to use TmcmIntSpin

//
//   Rev 1.2    27-01-2003 13:47:36  mcm

//
//   Rev 1.1    27-09-2002 13:18:30  mcm    Version: IMG 1.2
// Added additional matrix filters: Degrain, MedianMaxMin, MedianMinMax,
// Unsharpmask.
// Added ability in user interface to change filter size and hysteresis.

//
//   Rev 1.0    12-07-2002 10:48:12  mcm

unit uFormFilterBrowser;

{$IFDEF VER100} {$DEFINE MCMDELPHI3} {$ENDIF} { DELPHI 3  = VER100 }
{$IFDEF VER110} {$DEFINE MCMDELPHI3} {$ENDIF} { BUILDER 3 = VER110 }

{$IFDEF VER150} // Don't show "Unsafe code type and cast warnings".
{$WARN UNSAFE_TYPE OFF}
{$WARN UNSAFE_CODE OFF}
{$WARN UNSAFE_CAST OFF}
{$ENDIF}

interface

{$Include 'mcmDefines.pas'}

uses {$IFNDEF GE_DXE2}
      Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
      StdCtrls, ComCtrls, Spin,
     {$ELSE}
      WinApi.Windows, WinApi.Messages, System.SysUtils, System.Classes, Vcl.Controls,
      Vcl.Graphics, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls, Vcl.Samples.Spin,
     {$ENDIF}
     mcmImage,
     mcmImageFilter,
     mcmImageDualView,
     umcmIntE;

type
  TFormFilterBrowser = class(TForm)
    btnCancel        : TButton;
    btnOK            : TButton;
    DualView         : TmcmImageDualView;

    gbMatrixFilter   : TGroupBox;
    lFilterSize      : TLabel;
    seFilterSize     : TmcmIntSpin;
    lHysteresis      : TLabel;
    seHysteresis     : TmcmIntSpin;

    gbMarrHildreth   : TGroupBox;
    lDeviation       : TLabel;
    rsGaussSD        : TmcmRealSpin;
    lDelta           : TLabel;
    rsDeltaSD        : TmcmRealSpin;

    gbCanny          : TGroupBox;
    lCannyDeviation  : TLabel;
    rsCannyDeviation : TmcmRealSpin;
    lPercentage      : TLabel;
    sePercentage     : TmcmIntSpin;
    gbCannyTrace     : TGroupBox;
    cbTraceLevels    : TCheckBox;
    lLow             : TLabel;
    lHigh            : TLabel;
    isLow            : TmcmIntSpin;
    isHigh           : TmcmIntSpin;

    lSCSmoothFct     : TLabel;
    rsSCSmoothFct    : TmcmRealSpin;
    gbFilter         : TGroupBox;
    cbFilter         : TComboBox;
    lFilter          : TLabel;
    btnHelp          : TButton;
    lHarmonicOrder   : TLabel;
    rsHarmonicOrder  : TmcmRealSpin;

    procedure FormCreate(Sender : TObject);
    procedure FormDestroy(Sender : TObject);
    procedure btnOKClick(Sender : TObject);
    procedure btnCancelClick(Sender : TObject);
    procedure lbFilterClick(Sender : TObject);
    procedure seFilterSizeChange(Sender : TObject);
    procedure seHysteresisChange(Sender : TObject);
    procedure rsGaussSDChange(Sender : TObject);
    procedure rsDeltaSDChange(Sender : TObject);
    procedure cbTraceLevelsClick(Sender : TObject);
    procedure isLowChange(Sender : TObject);
    procedure isHighChange(Sender : TObject);
    procedure sePercentageChange(Sender : TObject);
    procedure rsSCSmoothFctChange(Sender : TObject);
    procedure cbFilterDropDown(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);
    procedure rsHarmonicOrderChange(Sender: TObject);
  private
    { Private declarations }
    FCreatedResult  : boolean;
    FLoading        : boolean;
    FImageFilter    : TmcmImageFilter;
    {$IFDEF DCB3}
    FStartTime     : TLargeInteger;
    FEndTime       : TLargeInteger;
    {$ELSE}
    FStartTime     : int64;
    FEndTime       : int64;
    {$ENDIF}

    function  GetDeltaSD : double;
    function  GetFilter : TmcmFilter;
    function  GetFilterSize : word;
    function  GetGaussSD : double;
    function  GetHarmonicOrder : double;
    function  GetHysteresis : word;
    function  GetResultImage : TmcmImage;
    function  GetSourceImage : TmcmImage;
    function  GetTraceAuto : boolean;
    function  GetTraceLow : word;
    function  GetTraceHigh : word;
    function  GetTracePercentage : word;
    procedure SetResultImage(AImage : TmcmImage);
    procedure SetSourceImage(AImage : TmcmImage);
    procedure PopulateFilterList;
    procedure UpdateResult;
  public
    { Public declarations }
    property DeltaSD : double
      read   GetDeltaSD;
    property Filter : TmcmFilter
      read   GetFilter;
    property FilterSize : word
      read   GetFilterSize;
    property GaussSD : double
      read   GetGaussSD;
    property HarmonicOrder : double
      read   GetHarmonicOrder;
    property Hysteresis : word
      read   GetHysteresis;
    property ResultImage : TmcmImage
      read   GetResultImage
      write  SetResultImage;
    property SourceImage : TmcmImage
      read   GetSourceImage
      write  SetSourceImage;
    property TraceAuto : boolean
      read   GetTraceAuto;
    property TraceLow : word
      read   GetTraceLow;
    property TraceHigh : word
      read   GetTraceHigh;
    property TracePercentage : word
      read   GetTracePercentage;
    property TimeStart : {$IFDEF DCB3} TLargeInteger {$ELSE} int64 {$ENDIF}
      read   FStartTime;
    property TimeEnd : {$IFDEF DCB3} TLargeInteger {$ELSE} int64 {$ENDIF}
      read   FEndTime;
  end;

var FormFilterBrowser : TFormFilterBrowser;

implementation

{$R *.DFM}

uses mcmImageTypeDef;

procedure TFormFilterBrowser.FormCreate(Sender : TObject);
begin
  FLoading := True;
  FCreatedResult := False;

  FImageFilter := TmcmImageFilter.Create(Self);
  lFilterSize.Enabled  := False;
  seFilterSize.Enabled := False;
  lHysteresis.Enabled  := False;
  seHysteresis.Enabled := False;

  gbMatrixFilter.Visible := False;
  gbMarrHildreth.Visible := False;
  gbCanny.Visible        := False;
  gbCannyTrace.Visible   := False;

  rsGaussSD.Value        := FImageFilter.GaussSD;
  rsDeltaSD.Value        := FImageFilter.DeltaSD;
  rsCannyDeviation.Value := FImageFilter.GaussSD;
  cbTraceLevels.Checked  := Not(FImageFilter.TraceAuto);
  isHigh.Value           := FImageFilter.TraceHigh;
  isLow.Value            := FImageFilter.TraceLow;
  sePercentage.Value     := FImageFilter.TracePercent;

  gbMatrixFilter.Visible    := True;

  FLoading := False;
end; // TFormFilterBrowser.FormCreate.


procedure TFormFilterBrowser.FormDestroy(Sender : TObject);
begin
  if Assigned(FImageFilter)
  then FImageFilter.Free;
end; // TFormFilterBrowser.FormDestroy.


function TFormFilterBrowser.GetResultImage : TmcmImage;
begin
  Result := DualView.ResultImage;
end; // TFormFilterBrowser.GetResultImage.


function TFormFilterBrowser.GetSourceImage : TmcmImage;
begin
  Result := DualView.SourceImage;
end; // TFormFilterBrowser.GetSourceImage.


procedure TFormFilterBrowser.SetSourceImage(AImage : TmcmImage);
begin
  DualView.SourceImage := AImage;
  FImageFilter.SourceImage[0] := AImage;
  PopulateFilterList;
end; // TFormFilterBrowser.SetSourceImage.


procedure TFormFilterBrowser.SetResultImage(AImage : TmcmImage);
begin
  DualView.ResultImage := AImage;
end; // TFormFilterBrowser.SetResultImage.


procedure TFormFilterBrowser.PopulateFilterList;
begin
  cbFilter.Clear;

  if Not(Assigned(FImageFilter))
  then Exit;

  cbFilter.Items.AddObject('Select a Filter', Pointer($7FFFFFFF));
  cbFilter.Items.AddObject('Average', Pointer(FLT_AVERAGE));
  cbFilter.Items.AddObject('Average more', Pointer(FLT_AVERAGEHEAVY));
  cbFilter.Items.AddObject('Blur', Pointer(FLT_BLUR));
  cbFilter.Items.AddObject('Blur more', Pointer(FLT_BLURHEAVY));
  cbFilter.Items.AddObject('Degrain', Pointer(FLT_DEGRAIN));
  cbFilter.Items.AddObject('Degrain (Bright grains)', Pointer(FLT_MEDIANMAXMIN));
  cbFilter.Items.AddObject('Degrain (Dark grains)', Pointer(FLT_MEDIANMINMAX));
  cbFilter.Items.AddObject('Edge', Pointer(FLT_EDGE));
  cbFilter.Items.AddObject('Edge, Prewitt', Pointer(FLT_EDGEPREWITT));
  if (FImageFilter.SourceImage[0].ImageFormat = IF_GREY8)
  then begin
       cbFilter.Items.AddObject('Edge, Marr-Hildreth', Pointer(FLT_MARRHILDRETH));
       cbFilter.Items.AddObject('Edge, Canny', Pointer(FLT_CANNY));
       cbFilter.Items.AddObject('Edge, Shen-Castan', Pointer(FLT_SHENCASTAN));
  end;
  cbFilter.Items.AddObject('Emboss', Pointer(FLT_EMBOSS));
  cbFilter.Items.AddObject('Gauss blur', Pointer(FLT_GAUSSBLUR));
  cbFilter.Items.AddObject('Highpass', Pointer(FLT_HIGHPASS));
  if (FImageFilter.SourceImage[0].ImageFormat = IF_GREY8)
  then cbFilter.Items.AddObject('Infinite Symmetric Exponential', Pointer(FLT_ISEF));
  cbFilter.Items.AddObject('Maximum', Pointer(FLT_MAXIMUM));
  cbFilter.Items.AddObject('Max Min', Pointer(FLT_MAXMIN));
  cbFilter.Items.AddObject('Median', Pointer(FLT_MEDIAN));
  cbFilter.Items.AddObject('Mean Contra Harmonic', Pointer(FLT_MEANCONTRAHARMONIC));
  cbFilter.Items.AddObject('Mean Harmonic', Pointer(FLT_MEANHARMONIC));
  cbFilter.Items.AddObject('Minimum', Pointer(FLT_MINIMUM));
  cbFilter.Items.AddObject('Mosaic', Pointer(FLT_MOSAIC));
  cbFilter.Items.AddObject('Laplacian', Pointer(FLT_LAPLACIAN));
  cbFilter.Items.AddObject('Prewitt North South', Pointer(FLT_PREWITTNS));
  cbFilter.Items.AddObject('Prewitt East West', Pointer(FLT_PREWITTEW));
  cbFilter.Items.AddObject('Sharpen', Pointer(FLT_SHARPEN));
  cbFilter.Items.AddObject('Sharpen more', Pointer(FLT_SHARPENHEAVY));
  cbFilter.Items.AddObject('Smooth', Pointer(FLT_SMOOTH));
  cbFilter.Items.AddObject('Smooth Circular', Pointer(FLT_SMOOTHCIRCLE));
  cbFilter.Items.AddObject('Smooth Cone', Pointer(FLT_SMOOTHCONE));
  cbFilter.Items.AddObject('Smooth Pyramidal', Pointer(FLT_SMOOTHPYRAMIDAL));
  cbFilter.Items.AddObject('Sobel North South', Pointer(FLT_SOBELNS));
  cbFilter.Items.AddObject('Sobel East West', Pointer(FLT_SOBELEW));
  cbFilter.Items.AddObject('Horizontal Line Mask', Pointer(FLT_LINEMASKHORZ));
  cbFilter.Items.AddObject('Vertical Line Mask', Pointer(FLT_LINEMASKVERT));
  cbFilter.Items.AddObject('Unsharp Mask', Pointer(FLT_UNSHARPMASK));
  cbFilter.ItemIndex := 0;
end; // TFormFilterBrowser.PopulateFilterList.


procedure TFormFilterBrowser.btnOKClick(Sender : TObject);
begin
 ;
end; // TFormFilterBrowser.btnOKClick.


procedure TFormFilterBrowser.btnCancelClick(Sender : TObject);
begin
  if FCreatedResult
  then FImageFilter.ResultImage.Free;
end; // TFormFilterBrowser.btnCancelClick.


procedure TFormFilterBrowser.lbFilterClick(Sender : TObject);
begin
  if (cbFilter = Nil)
  then Exit;
  if (cbFilter.ItemIndex < 0)
  then Exit;

  lFilterSize.Parent  := gbMatrixFilter;
  lFilterSize.Top     := 32;
  seFilterSize.Parent := gbMatrixFilter;
  seFilterSize.Top    := 24;

  case TmcmFilter(cbFilter.Items.Objects[cbFilter.ItemIndex]) of
  FLT_MAXMIN          : begin
                          gbMatrixFilter.Visible := True;
                          gbMarrHildreth.Visible := False;
                          gbCanny.Visible        := False;
                          gbCannyTrace.Visible   := False;
                          lFilterSize.Enabled    := True;
                          seFilterSize.Enabled   := True;
                          lHysteresis.Enabled    := True;
                          seHysteresis.Enabled   := True;
                          rsHarmonicOrder.Enabled := False;
                          try
                            FImageFilter.FilterSize := seFilterSize.Value;
                          except
                          end;
                          try
                            FImageFilter.Hysteresis := seHysteresis.Value;
                          except
                          end;
                        end;
  FLT_DEGRAIN,
  FLT_MAXIMUM,
  FLT_MEDIAN,
  FLT_MEDIANMAXMIN,
  FLT_MEDIANMINMAX,
  FLT_MINIMUM,
  FLT_MOSAIC,
  FLT_MEANHARMONIC    : begin
                          gbMatrixFilter.Visible := True;
                          gbMarrHildreth.Visible := False;
                          gbCanny.Visible        := False;
                          gbCannyTrace.Visible   := False;
                          lFilterSize.Enabled    := True;
                          seFilterSize.Enabled   := True;
                          lHysteresis.Enabled    := False;
                          seHysteresis.Enabled   := False;
                          rsHarmonicOrder.Enabled := False;
                          try
                            FImageFilter.FilterSize := seFilterSize.Value;
                          except
                          end;
                       end;
  FLT_MEANCONTRAHARMONIC
                      : begin
                          gbMatrixFilter.Visible := True;
                          gbMarrHildreth.Visible := False;
                          gbCanny.Visible        := False;
                          gbCannyTrace.Visible   := False;
                          lFilterSize.Enabled    := True;
                          seFilterSize.Enabled   := True;
                          lHysteresis.Enabled    := False;
                          seHysteresis.Enabled   := False;
                          rsHarmonicOrder.Enabled := True;
                          try
                            FImageFilter.FilterSize := seFilterSize.Value;
                          except
                          end;
                        end;
  FLT_AVERAGEHEAVY,
  FLT_SMOOTHCIRCLE,
  FLT_SMOOTHPYRAMIDAL,
  FLT_SMOOTHCONE,
  FLT_BLURHEAVY,
  FLT_GAUSSBLUR,
  FLT_HIGHPASS2       : begin
                          gbMatrixFilter.Visible := True;
                          gbMarrHildreth.Visible := False;
                          gbCanny.Visible        := False;
                          gbCannyTrace.Visible   := False;
                          lFilterSize.Enabled    := False;
                          seFilterSize.Enabled   := False;
                          lHysteresis.Enabled    := False;
                          seHysteresis.Enabled   := False;
                          rsHarmonicOrder.Enabled := False;
                          seFilterSize.Value      := 5;
                          FImageFilter.FilterSize := 5;
                        end;
  FLT_MARRHILDRETH    : begin
                          rsGaussSD.Value := FImageFilter.GaussSD;
                          gbMarrHildreth.Visible := True;
                          gbMatrixFilter.Visible := False;
                          gbCanny.Visible        := False;
                          gbCannyTrace.Visible   := False;
                        end;
  FLT_CANNY           : begin
                          gbCanny.Width          := 169;
                          gbCanny.Caption        := 'Canny';
                          gbCanny.Visible        := True;
                          gbCannyTrace.Visible   := True;
                          gbMatrixFilter.Visible := False;
                          gbMarrHildreth.Visible := False;

                          lCannyDeviation.Visible  := True;
                          rsCannyDeviation.Visible := True;
                          lSCSmoothFct.Visible  := False;
                          rsSCSmoothFct.Visible := False;
                          lPercentage.Visible   := True;
                          sePercentage.Visible  := True;
                        end;
  FLT_SHENCASTAN      : begin
                          gbCanny.Width          := 169;
                          lFilterSize.Parent     := gbCanny;
                          lFilterSize.Top        := 96;
                          seFilterSize.Parent    := gbCanny;
                          seFilterSize.Top       := 88;

                          gbCanny.Caption        := 'Shen-Castan';
                          gbCanny.Visible        := True;
                          gbCannyTrace.Visible   := True;
                          gbMatrixFilter.Visible := False;
                          gbMarrHildreth.Visible := False;

                          lFilterSize.Enabled    := True;
                          seFilterSize.Enabled   := True;

                          lCannyDeviation.Visible  := False;
                          rsCannyDeviation.Visible := False;
                          lSCSmoothFct.Visible  := True;
                          rsSCSmoothFct.Visible := True;
                          lPercentage.Visible   := True;
                          sePercentage.Visible  := True;
                        end;
  FLT_ISEF            : begin
                          gbCanny.Caption        := 'Infinite Symmetric Exponential';
                          gbCanny.Visible        := True;
                          gbCanny.Width          := gbMatrixFilter.Width;
                          gbCannyTrace.Visible   := False;
                          gbMatrixFilter.Visible := False;
                          gbMarrHildreth.Visible := False;

                          lFilterSize.Enabled    := False;
                          seFilterSize.Enabled   := False;

                          lCannyDeviation.Visible  := False;
                          rsCannyDeviation.Visible := False;
                          lSCSmoothFct.Visible  := True;
                          rsSCSmoothFct.Visible := True;
                          lPercentage.Visible   := False;
                          sePercentage.Visible  := False;
                        end;
  else begin
       gbMatrixFilter.Visible  := True;
       gbMarrHildreth.Visible  := False;
       gbCanny.Visible         := False;
       gbCannyTrace.Visible    := False;
       lFilterSize.Enabled     := False;
       seFilterSize.Enabled    := False;
       lHysteresis.Enabled     := False;
       seHysteresis.Enabled    := False;
       rsHarmonicOrder.Enabled := False;
       seFilterSize.Value      := 3;
       FImageFilter.FilterSize := 3;
  end;
  end;
  Update;
  UpdateResult;
end; // TFormFilterBrowser.lbFilterClick.


function TFormFilterBrowser.GetFilter : TmcmFilter;
begin
  Result := TmcmFilter(cbFilter.Items.Objects[cbFilter.ItemIndex]);
end; // TFormFilterBrowser.GetFilter.


function TFormFilterBrowser.GetFilterSize : word;
begin
  Result := seFilterSize.Value;
end; // TFormFilterBrowser.GetFilterSize.


function TFormFilterBrowser.GetDeltaSD : double;
begin
  Result := rsDeltaSD.Value;
end; // TFormFilterBrowser.GetDeltaSD.


function TFormFilterBrowser.GetGaussSD : double;
begin
  if (TmcmFilter(cbFilter.Items.Objects[cbFilter.ItemIndex]) = FLT_MARRHILDRETH)
  then Result := rsGaussSD.Value
  else Result := rsCannyDeviation.Value;
end; // TFormFilterBrowser.GetGaussSD.


function TFormFilterBrowser.GetHarmonicOrder : double;
begin
  Result := rsHarmonicOrder.Value;
end; // TFormFilterBrowser.GetHarmonicOrder.


function TFormFilterBrowser.GetTraceAuto : boolean;
begin
  Result := Not(cbTraceLevels.Checked);
end; // TFormFilterBrowser.GetTraceAuto.


function TFormFilterBrowser.GetTraceLow : word;
begin
  Result := isLow.Value;
end; // TFormFilterBrowser.GetTraceLow.


function TFormFilterBrowser.GetTraceHigh : word;
begin
  Result := isHigh.Value;
end; // TFormFilterBrowser.GetTraceHigh.


function TFormFilterBrowser.GetTracePercentage : word;
begin
  Result := sePercentage.Value;
end; // TFormFilterBrowser.GetTracePercentage.


function TFormFilterBrowser.GetHysteresis : word;
begin
  Result := seHysteresis.Value;
end; // TFormFilterBrowser.GetHysteresis.


procedure TFormFilterBrowser.seFilterSizeChange(Sender : TObject);
begin
  try
    if (seFilterSize.Text <> '')
    then UpdateResult;
  except
  end;
end; // TFormFilterBrowser.seFilterSizeChange.


procedure TFormFilterBrowser.seHysteresisChange(Sender : TObject);
begin
  try
    if (seHysteresis.Text <> '')
    then UpdateResult;
  except
  end;
end; // TFormFilterBrowser.seHysteresisChange.


procedure TFormFilterBrowser.rsGaussSDChange(Sender : TObject);
begin
  try
    if (rsGaussSD.Text <> '')
    then UpdateResult;
  except
  end;
end; // TFormFilterBrowser.rsGaussSDChange.


procedure TFormFilterBrowser.rsDeltaSDChange(Sender: TObject);
begin
  try
    if (FImageFilter <> Nil) and (rsDeltaSD.Text <> '')
    then begin
         if (FImageFilter.DeltaSD <> rsDeltaSD.Value)
         then begin
              FImageFilter.DeltaSD := rsDeltaSD.Value;
              rsDeltaSD.Value := FImageFilter.DeltaSD;
              UpdateResult;
         end;
    end;
  except
  end;
end; // TFormFilterBrowser.rsDeltaSDChange.


procedure TFormFilterBrowser.isLowChange(Sender : TObject);
begin
  try
    if (isLow.Text <> '')
    then begin
         if (isLow.Value <= isHigh.Value)
         then UpdateResult
         else isLow.Value := isHigh.Value;
    end;
  except
  end;
end; // TFormFilterBrowser.isLowChange.


procedure TFormFilterBrowser.isHighChange(Sender : TObject);
begin
  try
    if (isHigh.Text <> '')
    then begin
         if (isHigh.Value >= isLow.Value)
         then UpdateResult
         else isHigh.Value := isLow.Value;
    end;
  except
  end;
end; // TFormFilterBrowser.isHighChange.


procedure TFormFilterBrowser.sePercentageChange(Sender : TObject);
begin
  try
    if (sePercentage.Text <> '')
    then UpdateResult;
  except
  end;
end; // TFormFilterBrowser.seLevelChange.


procedure TFormFilterBrowser.cbTraceLevelsClick(Sender : TObject);
begin
  if cbTraceLevels.Checked
  then begin
       lLow.Enabled    := True;
       isLow.Enabled   := True;
       lHigh.Enabled   := True;
       isHigh.Enabled  := True;
       lPercentage.Enabled  := False;
       sePercentage.Enabled := False;
  end
  else begin
       lLow.Enabled    := False;
       isLow.Enabled   := False;
       lHigh.Enabled   := False;
       isHigh.Enabled  := False;
       lPercentage.Enabled  := True;
       sePercentage.Enabled := True;
  end;
  UpdateResult;
end; // TFormFilterBrowser.cbTraceLevelsClick.


procedure TFormFilterBrowser.rsSCSmoothFctChange(Sender : TObject);
begin
  try
    if (rsSCSmoothFct.Text <> '')
    then UpdateResult;
  except
  end;
end; // TFormFilterBrowser.rsSCSmoothFctChange.


procedure TFormFilterBrowser.UpdateResult;
var SaveCursor : TCursor;
begin
  if FLoading
  then Exit;
  if (cbFilter = Nil)
  then Exit;
  if (cbFilter.ItemIndex < 0)
  then Exit;

  if (FImageFilter <> Nil)
  then begin
       FImageFilter.FilterSize := seFilterSize.Value;
       FImageFilter.Hysteresis := seHysteresis.Value;
       case TmcmFilter(cbFilter.Items.Objects[cbFilter.ItemIndex]) of
       FLT_MARRHILDRETH    : begin
                               FImageFilter.GaussSD := rsGaussSD.Value;
                               FImageFilter.DeltaSD := rsDeltaSD.Value;
                             end;
       FLT_CANNY           : begin
                               FImageFilter.GaussSD   := rsCannyDeviation.Value;
                               FImageFilter.TraceAuto := Not(cbTraceLevels.Checked);
                               FImageFilter.TraceHigh := isHigh.Value;
                               FImageFilter.TraceLow  := isLow.Value;
                               FImageFilter.TracePercent := sePercentage.Value;
                             end;
       FLT_SHENCASTAN      : begin
                               FImageFilter.SmoothFactor := rsSCSmoothFct.Value;
                               FImageFilter.TraceAuto := Not(cbTraceLevels.Checked);
                               FImageFilter.TraceHigh := isHigh.Value;
                               FImageFilter.TraceLow  := isLow.Value;
                               FImageFilter.TracePercent := sePercentage.Value;
                             end;
       FLT_ISEF            : begin
                               FImageFilter.SmoothFactor := rsSCSmoothFct.Value;
                             end;
       end;
       SaveCursor := Screen.Cursor;
       try
         Screen.Cursor := crHourglass;

         if Assigned(FImageFilter.SourceImage[0])
         then begin
              if Not(Assigned(FImageFilter.ResultImage))
              then begin
                   FImageFilter.ResultImage := TmcmImage.Create;
                   FImageFilter.ResultImage.Width  := FImageFilter.SourceImage[0].Width;
                   FImageFilter.ResultImage.Height := FImageFilter.SourceImage[0].Height;
                   FImageFilter.ResultImage.ImageFormat := FImageFilter.SourceImage[0].ImageFormat;
                   FImageFilter.ResultImage.Palette := FImageFilter.SourceImage[0].Palette;
                   DualView.ResultImage := FImageFilter.ResultImage;
                   FCreatedResult := True;
              end;
              // Get start time
              QueryPerformanceCounter(FStartTime);

              FImageFilter.Filter(TmcmFilter(cbFilter.Items.Objects[cbFilter.ItemIndex]));

              QueryPerformanceCounter(FEndTime);

              DualView.UpdateResultView;
         end;
       finally
         Screen.Cursor := SaveCursor;
       end;

       if FImageFilter.TraceAuto
       then begin
            case TmcmFilter(cbFilter.Items.Objects[cbFilter.ItemIndex]) of
            FLT_SHENCASTAN,
            FLT_CANNY      : begin
                               isHigh.OnChange := Nil;
                               isLow.OnChange  := Nil;
                               isHigh.Value := FImageFilter.TraceHigh;
                               isLow.Value  := FImageFilter.TraceLow;
                               isHigh.OnChange := isHighChange;
                               isLow.OnChange  := isLowChange;
                             end;
            end;
       end;
  end;
end; // TFormFilterBrowser.UpdateResult.


procedure TFormFilterBrowser.cbFilterDropDown(Sender : TObject);
begin
  // Remove first entry, if Objects returns -1.
  // This is the "Select a Filter" message.
  if (integer(cbFilter.Items.Objects[0]) = integer($7FFFFFFF))
  then cbFilter.Items.Delete(0);
end; // TFormFilterBrowser.cbFilterDropDown.


procedure TFormFilterBrowser.btnHelpClick(Sender : TObject);
begin
  Application.HelpContext(HelpContext);
end; // TFormFilterBrowser.btnHelpClick.


procedure TFormFilterBrowser.rsHarmonicOrderChange(Sender : TObject);
begin
  try
    if (FImageFilter <> Nil) and (rsHarmonicOrder.Text <> '')
    then begin
         if (FImageFilter.HarmonicOrder <> rsHarmonicOrder.Value)
         then begin
              FImageFilter.HarmonicOrder := rsHarmonicOrder.Value;
              rsHarmonicOrder.Value := FImageFilter.HarmonicOrder;
              UpdateResult;
         end;
    end;
  except
  end;
end; // TFormFilterBrowser.rsHarmonicOrderChange.

end.
