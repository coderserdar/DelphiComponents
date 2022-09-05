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
// $Log:  17619: uFormThreshold.pas
//
//    Rev 1.8    2014-02-02 21:10:12  mcm    Version: IMG 4.0
// Added support for Delphi XE2, 3, 4 and 5
//
//   Rev 1.7    23-05-2005 22:01:52  mcm    Version: IMG 2.9
// Added Trace threshold method.

//
//   Rev 1.6    15-05-2005 23:45:04  mcm    Version: IMG 2.9
// Added Trace threshold method.

//
//   Rev 1.5    13-02-2005 19:58:16  mcm
// Added Triangular threshold method.

//
//   Rev 1.4    20-12-2004 22:58:10  mcm
// Modified to use TmcmInt

//
//   Rev 1.3    30-05-2003 13:05:38  mcm    Version: IMG 1.3.4
// Enabled this form to threshold 8, 24 and 32 bit color images by creating a
// luminance image.

//
//   Rev 1.2    08-03-2003 23:38:36  mcm    Version: IMG 1.3.2
// Modified to use TmcmImageDualView, ie. present a preview of the threshold
// process.

//
//   Rev 1.1    27-09-2002 13:35:32  mcm    Version: IMG 1.2
// Added ISO data and Background Symmetry thresholds.

//
//   Rev 1.0    27-05-2002 16:22:42  mcm

unit uFormThreshold;

interface

{$Include 'mcmDefines.pas'}

uses {$IFNDEF GE_DXE2}
      Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
      StdCtrls, Spin, ExtCtrls,
     {$ELSE}
      WinApi.Windows, WinApi.Messages, System.SysUtils, System.Classes, Vcl.Controls,
      Vcl.Graphics, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Samples.Spin, 
     {$ENDIF}
     mcmImage,
     mcmImageColor,
     mcmImageDualView,
     mcmImageKernel,
     umcmIntE;

type
  TFormThreshold  = class(TForm)
    mcmImageColor : TmcmImageColor;
    btnOK         : TButton;
    btnCancel     : TButton;
    gbThreshold   : TGroupBox;
    cbMethod      : TComboBox;
    lMethod       : TLabel;
    lLevel        : TLabel;
    seLevel       : TmcmIntSpin;
    DualView      : TmcmImageDualView;
    gbSegments    : TGroupBox;
    lCols         : TLabel;
    seCols        : TmcmIntSpin;
    lRows         : TLabel;
    seRows        : TmcmIntSpin;
    gbTrace       : TGroupBox;
    cbTraceLevels : TCheckBox;
    lLow          : TLabel;
    isLow         : TmcmIntSpin;
    lHigh         : TLabel;
    isHigh        : TmcmIntSpin;
    procedure FormCreate(Sender : TObject);
    procedure FormShow(Sender : TObject);
    procedure ValueChange(Sender : TObject);
    procedure cbMethodChange(Sender : TObject);
    procedure btnCancelClick(Sender : TObject);
    procedure FormDestroy(Sender: TObject);
    procedure cbTraceLevelsClick(Sender: TObject);
  private
    { Private declarations }
    FCreatedResult : boolean;
    FLumImage      : TmcmImage;
    FLevel         : integer;
    FPercentage    : integer;
    {$IFDEF DCB3}
    FStartTime     : TLargeInteger;
    FEndTime       : TLargeInteger;
    {$ELSE}
    FStartTime     : int64;
    FEndTime       : int64;
    {$ENDIF}

    function  GetCols : integer;
    procedure SetCols(Value : integer);
    function  GetRows : integer;
    procedure SetRows(Value : integer);
    function  GetMethod : integer;
    function  GetLevel : integer;
    function  GetPercentage : integer;
    function  GetResultImage : TmcmImage;
    function  GetSourceImage : TmcmImage;
    procedure SetMethod(Value : integer);
    procedure SetLevel(Value : integer);
    procedure SetPercentage(Value : integer);
    procedure SetResultImage(AImage : TmcmImage);
    procedure SetSourceImage(AImage : TmcmImage);
    procedure UpdateResult;
  public
    { Public declarations }
    property Cols : integer
      read   GetCols
      write  SetCols;
    property Level : integer
      read   GetLevel
      write  SetLevel;
    property Method : integer
      read   GetMethod
      write  SetMethod;
    property Percentage : integer
      read   GetPercentage
      write  SetPercentage;
    property Rows : integer
      read   GetRows
      write  SetRows;

    property ResultImage : TmcmImage
      read   GetResultImage
      write  SetResultImage;
    property SourceImage : TmcmImage
      read   GetSourceImage
      write  SetSourceImage;
    property TimeStart : {$IFDEF DCB3} TLargeInteger {$ELSE} int64 {$ENDIF}
      read   FStartTime;
    property TimeEnd : {$IFDEF DCB3} TLargeInteger {$ELSE} int64 {$ENDIF}
      read   FEndTime;
  end;

var FormThreshold : TFormThreshold;

implementation

uses mcmImageTypeDef;

{$R *.DFM}

procedure TFormThreshold.FormCreate(Sender : TObject);
begin
  cbMethod.ItemIndex := 0;
  FCreatedResult := False;
  FLumImage := Nil;
  cbTraceLevelsClick(Self);
end; // TFormThreshold.FormCreate.


procedure TFormThreshold.FormDestroy(Sender : TObject);
begin
  if Assigned(FLumImage)
  then FLumImage.Free;
  FLumImage := Nil;
end; // TFormThreshold.FormDestroy.


procedure TFormThreshold.FormShow(Sender : TObject);
begin
  UpdateResult;
end; // TFormThreshold.FormShow.


procedure TFormThreshold.btnCancelClick(Sender : TObject);
begin
  if FCreatedResult
  then mcmImageColor.ResultImage.Free;
end; // TFormThreshold.btnCancelClick.


procedure TFormThreshold.ValueChange(Sender : TObject);
begin
  if (TmcmThreshold(cbMethod.ItemIndex) = TH_TRACE) or
     (TmcmThreshold(cbMethod.ItemIndex) = TH_SYMMETRY)
  then FPercentage := seLevel.Value
  else FLevel := seLevel.Value;

  if Assigned(mcmImageColor)
  then if Assigned(mcmImageColor.SourceImage[0])
       then UpdateResult;
end; // TFormThreshold.ValueChange.


function TFormThreshold.GetMethod : integer;
begin
  Result := cbMethod.ItemIndex;
end; // TFormThreshold.GetMethod.


function TFormThreshold.GetLevel : integer;
begin
  Result := seLevel.Value;
end; // TFormThreshold.GetLevel.


function TFormThreshold.GetPercentage : integer;
begin
  Result := FPercentage;
end; // TFormThreshold.GetPercentage.


procedure TFormThreshold.SetMethod(Value : integer);
begin
  cbMethod.ItemIndex := Value;
end; // TFormThreshold.SetMethod.


procedure TFormThreshold.SetLevel(Value : integer);
begin
  FLevel := Value;
  if (TmcmThreshold(cbMethod.ItemIndex) <> TH_TRACE) and
     (TmcmThreshold(cbMethod.ItemIndex) <> TH_SYMMETRY)
  then seLevel.Value := FLevel;
end; // TFormThreshold.SetLevel.


procedure TFormThreshold.SetPercentage(Value : integer);
begin
  FPercentage := Value;
  if (TmcmThreshold(cbMethod.ItemIndex) = TH_TRACE) or
     (TmcmThreshold(cbMethod.ItemIndex) = TH_SYMMETRY)
  then seLevel.Value := FPercentage;
end; // TFormThreshold.SetPercentage.


function TFormThreshold.GetCols : integer;
begin
  Result := seCols.Value;
end; // TFormThreshold.GetCols.


procedure TFormThreshold.SetCols(Value : integer);
begin
  seCols.Value := Value;
end; // TFormThreshold.SetCols.


function TFormThreshold.GetRows : integer;
begin
  Result := seRows.Value;
end; // TFormThreshold.GetRows.


procedure TFormThreshold.SetRows(Value : integer);
begin
  seRows.Value := Value;
end; // TFormThreshold.SetRows.


procedure TFormThreshold.cbMethodChange(Sender : TObject);
begin
  lCols.Enabled := False;
  lRows.Enabled := False;
  seCols.Enabled := False;
  seRows.Enabled := False;
  cbTraceLevels.Enabled := False;
  case TmcmThreshold(cbMethod.ItemIndex) of
  TH_EDGE,
  TH_OPTIMIZED,
  TH_STATISTIC  : begin
                    // Enable is method is TH_EDGE, TH_OPTIMIZED or TH_STATISTIC.
                    seLevel.MaxValue := 255;
                    lCols.Enabled   := True;
                    lRows.Enabled   := True;
                    seCols.Enabled  := True;
                    seRows.Enabled  := True;
                    gbTrace.Visible := False;
                    lLevel.Caption  := '&Level:';
                    lLevel.Enabled  := False;
                    seLevel.Enabled := False;
                  end;
  TH_TRACE      : begin
                    gbTrace.Visible  := True;
                    seLevel.Value    := FPercentage;
                    seLevel.MaxValue := 100;
                    cbTraceLevels.Enabled := True;
                    cbTraceLevelsClick(Self);
                    lLevel.Caption  := '&Percentage:';
                  end;
  TH_SYMMETRY   : begin
                    lLevel.Enabled   := True;
                    seLevel.Enabled  := True;
                    lLevel.Caption   := '&Percentage:';
                    seLevel.Value    := FPercentage;
                    seLevel.MaxValue := 100;
                    gbTrace.Visible  := False;
                  end;
  TH_ISODATA,
  TH_TRIANGULAR : begin
                    seLevel.MaxValue := 255;
                    lLevel.Caption   := '&Level:';
                    lLevel.Enabled   := False;
                    seLevel.Enabled  := False;
                  end;

  else begin
       lLevel.Caption   := '&Level:';
       lLevel.Enabled   := True;
       seLevel.Enabled  := True;
       seLevel.Value    := Level;
       seLevel.MaxValue := 255;
       gbTrace.Visible  := False;
  end;
  end;
  UpdateResult;
end; // TFormThreshold.cbMethodChange.


function TFormThreshold.GetResultImage : TmcmImage;
begin
  Result := DualView.ResultImage;
end; // TFormThreshold.GetResultImage.


function TFormThreshold.GetSourceImage : TmcmImage;
begin
  Result := DualView.SourceImage;
end; // TFormThreshold.GetSourceImage.


procedure TFormThreshold.SetResultImage(AImage : TmcmImage);
begin
  DualView.ResultImage := AImage;
end; // TFormThreshold.SetResultImage.


procedure TFormThreshold.SetSourceImage(AImage : TmcmImage);
var SaveImage : TmcmImage;
begin
  DualView.SourceImage := AImage;
  mcmImageColor.SourceImage[0] := AImage;
  if (AImage.ImageFormat <> IF_GREY8)
  then begin
       SaveImage := mcmImageColor.ResultImage;
       mcmImageColor.GetLuminanceChannel;
       FLumImage := mcmImageColor.ResultImage;
       mcmImageColor.SourceImage[0] := FLumImage;
       mcmImageColor.ResultImage := SaveImage;
  end;
end; // TFormThreshold.SetSourceImage.


procedure TFormThreshold.UpdateResult;
var Level : word;
begin
  if Assigned(mcmImageColor.SourceImage[0])
  then begin
       if Not(Assigned(mcmImageColor.ResultImage))
       then begin
            mcmImageColor.ResultImage := TmcmImage.Create;
            mcmImageColor.ResultImage.Width  := mcmImageColor.SourceImage[0].Width;
            mcmImageColor.ResultImage.Height := mcmImageColor.SourceImage[0].Height;
            mcmImageColor.ResultImage.ImageFormat := IF_GREY8; // ImageFormat could also be IF_BW;
            mcmImageColor.ResultImage.CreateGreyPalette;
            DualView.ResultImage := mcmImageColor.ResultImage;
            FCreatedResult := True;
       end;
       // Get start time
       QueryPerformanceCounter(FStartTime);
       Level := seLevel.Value;

       mcmImageColor.TraceAuto := Not(cbTraceLevels.Checked);
       mcmImageColor.TraceLow  := isLow.Value;
       mcmImageColor.TraceHigh := isHigh.Value;

       mcmImageColor.Threshold(TmcmThreshold(FormThreshold.Method),
                               Level,
                               FormThreshold.Cols,
                               FormThreshold.Rows);
       QueryPerformanceCounter(FEndTime);

       if mcmImageColor.TraceAuto
       then begin
            isLow.Value  := mcmImageColor.TraceLow;
            isHigh.Value := mcmImageColor.TraceHigh;
       end;

       if (seLevel.Value <> Level) and
          ((TmcmThreshold(FormThreshold.Method) = TH_ISODATA) or
           (TmcmThreshold(FormThreshold.Method) = TH_TRIANGULAR))
       then begin
            seLevel.OnChange := Nil;
            seLevel.Value := Level;
            seLevel.OnChange := ValueChange;
       end;
       DualView.UpdateResultView;
  end;
end; // TFormThreshold.UpdateResult.


procedure TFormThreshold.cbTraceLevelsClick(Sender : TObject);
begin
  if cbTraceLevels.Checked
  then begin
       lLow.Enabled    := True;
       isLow.Enabled   := True;
       lHigh.Enabled   := True;
       isHigh.Enabled  := True;
       lLevel.Enabled  := False;
       seLevel.Enabled := False;
  end
  else begin
       lLow.Enabled    := False;
       isLow.Enabled   := False;
       lHigh.Enabled   := False;
       isHigh.Enabled  := False;
       lLevel.Enabled  := True;
       seLevel.Enabled := True;
  end;
  UpdateResult;
end; // TFormThreshold.cbTraceLevelsClick.

end.
