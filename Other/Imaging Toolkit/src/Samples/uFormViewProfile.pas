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
// $Log:  23266: uFormViewProfile.pas 
//
//    Rev 1.3    2014-02-02 21:10:12  mcm    Version: IMG 4.0
// Added support for Delphi XE2, 3, 4 and 5
//
//   Rev 1.2    15-05-2005 19:46:44  mcm    Version: IMG 2.9
// Corrected setting average on a profile.

//
//   Rev 1.1    20-12-2004 22:58:10  mcm
// Modified to use TmcmInt

//
//   Rev 1.0    19-05-2004 22:02:28  mcm    Version: IMG 2.4

unit uFormViewProfile;

interface

{$Include 'mcmDefines.pas'}

uses {$IFNDEF GE_DXE2}
      Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
      StdCtrls, ExtCtrls, Spin, ComCtrls, Grids,
     {$ELSE}
      WinApi.Windows, WinApi.Messages, System.SysUtils, System.Classes, Vcl.Controls,
      Vcl.Graphics, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls, Vcl.ExtCtrls,
      Vcl.Samples.Spin, Vcl.Grids, 
     {$ENDIF}
     umcmIntE,
     mcmImageTypeDef,
     mcmShape,
     mcmGraphics;

type
  TFormViewProfile = class(TForm)
    btnClose      : TButton;
    PageControl   : TPageControl;
    tsScanProp    : TTabSheet;

    tsDetect      : TTabSheet;
    gbScanProp    : TGroupBox;
    lInterpolate  : TLabel;
    lAverage      : TLabel;
    lScanWidth    : TLabel;
    cbInterpolate : TComboBox;
    lAngle        : TLabel;
    rsAngle       : TmcmRealSpin;
    lLength       : TLabel;
    rsLength      : TmcmRealSpin;

    gbDetect      : TGroupBox;
    lDevirative   : TLabel;
    lHysteresis   : TLabel;

    gbTransitions : TGroupBox;
    cbPeak        : TCheckBox;
    cbPositive    : TCheckBox;
    cbNegative    : TCheckBox;
    cbValley      : TCheckBox;
    cbCenterPeaks : TCheckBox;

    tsResults     : TTabSheet;
    sgResults     : TStringGrid;

    Panel1        : TPanel;
    lValues       : TLabel;
    sbProfile     : TScrollBox;
    mcmShape1     : TmcmShape;
    mcmShape2     : TmcmShape;
    seScanWidth   : TmcmIntSpin;
    seAverage     : TmcmIntSpin;
    seDevirative  : TmcmIntSpin;
    seHysteresis  : TmcmIntSpin;
    procedure FormCreate(Sender : TObject);
    procedure FormCloseQuery(Sender : TObject; var CanClose : Boolean);
    procedure FormClose(Sender : TObject; var Action : TCloseAction);
    procedure btnCloseClick(Sender : TObject);
    procedure FormDestroy(Sender : TObject);

    procedure cbInterpolateChange(Sender : TObject);
    procedure seAverageChange(Sender : TObject);
    procedure seScanWidthChange(Sender : TObject);
    procedure seDevirativeChange(Sender : TObject);
    procedure seHysteresisChange(Sender : TObject);
    procedure cbPositiveClick(Sender : TObject);
    procedure cbNegativeClick(Sender : TObject);
    procedure cbPeakClick(Sender : TObject);
    procedure cbValleyClick(Sender : TObject);
    procedure rsAngleChange(Sender : TObject);
    procedure rsLengthChange(Sender : TObject);
    procedure sbProfileMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure cbCenterPeaksClick(Sender: TObject);
  private
    { Private declarations }
    {$IFDEF DCB3}
    FStartTime       : TLargeInteger;
    FEndTime         : TLargeInteger;
    FPerformanceFreq : TLargeInteger;
    {$ELSE}
    FStartTime       : int64;
    FEndTime         : int64;
    FPerformanceFreq : int64;
    {$ENDIF}
    FOnProcessTime   : TOnProcessTime;
    FProfile         : TmcmProfile;
    procedure SetProfile(Value : TmcmProfile);
    procedure OnProfileChange(Sender : TObject);
    procedure UpdateProfile;
    procedure ClearResults;
  public
    { Public declarations }
    property Profile : TmcmProfile
      read   FProfile
      write  SetProfile;
    property OnProcessTime : TOnProcessTime
      read   FOnProcessTime
      write  FOnProcessTime;
  end;

var FormViewProfile : TFormViewProfile;

implementation

{$R *.DFM}

procedure TFormViewProfile.FormCreate(Sender : TObject);
begin
  ClearResults;
  PageControl.ActivePage := tsScanProp;
  QueryPerformanceFrequency(FPerformanceFreq);
  QueryPerformanceCounter(FStartTime);
  QueryPerformanceCounter(FEndTime);
  FProfile := Nil;
  cbInterpolate.ItemIndex := 0;
end; // TFormViewProfile.FormCreate.


procedure TFormViewProfile.FormDestroy(Sender : TObject);
begin
  FormViewProfile := Nil;
end; // TFormViewProfile.FormDestroy.


procedure TFormViewProfile.FormCloseQuery(Sender : TObject; var CanClose : Boolean);
begin
  CanClose := True;
end; // TFormViewProfile.FormCloseQuery.


procedure TFormViewProfile.FormClose(Sender : TObject; var Action : TCloseAction);
begin
  if Assigned(FProfile)
  then begin
       FProfile.Enabled := False;
       FProfile.OnChange := Nil;
       FProfile.SendToBack;
  end;
  Action := caFree;
end; // TFormViewProfile.FormClose.


procedure TFormViewProfile.btnCloseClick(Sender : TObject);
begin
  Close;
end; // TFormViewProfile.btnCloseClick.


procedure TFormViewProfile.SetProfile(Value : TmcmProfile);
begin
  FProfile := Value;
  if Assigned(FProfile)
  then begin
       // Get and show profile properties.
       cbInterpolate.ItemIndex := integer(FProfile.Interpolate);
       seScanWidth.Value  := FProfile.ScanWidth;
       seAverage.Value    := FProfile.Average;
       seDevirative.Value := FProfile.MinDerivative;
       seHysteresis.Value := FProfile.Hysteresis;
       rsAngle.Value      := FProfile.Angle;
       rsLength.Value     := FProfile.Length;

       FProfile.BringToFront;
       //FProfile.Align    := alClient;
       FProfile.Enabled  := True;
       FProfile.Visible  := True;
       FProfile.OnChange := OnProfileChange;

       cbPositive.Checked := (PT_POSITIVE in FProfile.Transitions);
       cbNegative.Checked := (PT_NEGATIVE in FProfile.Transitions);
       cbPeak.Checked     := (PT_PEAK in FProfile.Transitions);
       cbValley.Checked   := (PT_VALLEY in FProfile.Transitions);

       UpdateProfile;
  end;
end; // TFormViewProfile.SetProfile.


procedure TFormViewProfile.UpdateProfile;
var i, j       : integer;
    PolyPoints : array[0..4096] of TPoint;
begin
  // Scan intensity profile and detect transitions.
  QueryPerformanceCounter(FStartTime);
  if (FProfile.Scan = EC_OK)
  then FProfile.LocateTransitions;
  QueryPerformanceCounter(FEndTime);

  // Report scan time.
  if Assigned(FOnProcessTime)
  then FOnProcessTime(Self, FStartTime, FEndTime);

  // Resize profile view's width to scan length.
  mcmShape1.Width := FProfile.ScanLength + 2;
  mcmShape2.Width := FProfile.ScanLength + 2;

  j := FProfile.ScanLength - 1;
  if (j > 4096)
  then j := 4096;

  // Display intensity profile.
  for i := 0 to j
  do begin
     PolyPoints[i].x := i;
     PolyPoints[i].y := 256 - FProfile.ScanData[i];
  end;
  mcmShape1.SetPoints(Slice(PolyPoints, FProfile.ScanLength));

  // Display 1st derivative.
  if (FProfile.Transitions <> [])
  then begin
       mcmShape2.Visible := True;
       for i := 0 to j
       do begin
          PolyPoints[i].x := i;
          PolyPoints[i].y := (128 - FProfile.Derivative[i]);
       end;
       mcmShape2.SetPoints(Slice(PolyPoints, FProfile.ScanLength));
  end
  else mcmShape2.Visible := False;

  FProfile.Invalidate;
  
  ClearResults;
  for i := 0 to (FProfile.NoTransitions - 1)
  do begin
     sgResults.Cells[0, i+1] := FloatToStrF(FProfile.TransitionX[i], ffFixed, 7, 1);
     sgResults.Cells[1, i+1] := FloatToStrF(FProfile.TransitionY[i], ffFixed, 7, 1);
     j := FProfile.TransitionIndex[i];
     sgResults.Cells[2, i+1] := IntToStr(FProfile.ScanData[j]);
     case FProfile.TransitionType[i] of
     PT_POSITIVE : sgResults.Cells[3, i+1] := 'Positive';
     PT_NEGATIVE : sgResults.Cells[3, i+1] := 'Negative';
     PT_PEAK     : sgResults.Cells[3, i+1] := 'Peak';
     PT_VALLEY   : sgResults.Cells[3, i+1] := 'Valley';
     else sgResults.Cells[3, i+1] := '';
     end;
     sgResults.RowCount := FProfile.NoTransitions + 1;
  end;
end; // TFormViewProfile.UpdateProfile.


procedure TFormViewProfile.OnProfileChange(Sender : TObject);
begin
  // Re-scan profile if moving/moved.
  if (rsAngle.Value <> FProfile.Angle)
  then begin
       rsAngle.OnChange := Nil;
       rsAngle.Value := FProfile.Angle;
       rsAngle.OnChange := rsAngleChange;
  end;
  if (rsLength.Value <> FProfile.Length)
  then begin
       rsLength.OnChange := Nil;
       rsLength.Value := FProfile.Length;
       rsLength.OnChange := rsLengthChange;
  end;
  UpdateProfile;
end; // TFormViewProfile.OnProfileChange.


procedure TFormViewProfile.cbInterpolateChange(Sender : TObject);
begin
  if Assigned(FProfile)
  then begin
       FProfile.Interpolate := TmcmProfInterpolate(cbInterpolate.ItemIndex);
       UpdateProfile;
  end;
end; // TFormViewProfile.cbInterpolateChange.


procedure TFormViewProfile.seAverageChange(Sender : TObject);
begin
  if Assigned(FProfile)
  then if (seAverage.Text <> '')
       then begin
            if Odd(seAverage.Value)
            then FProfile.Average := seAverage.Value
            else if (FProfile.Average < seAverage.Value)
                 then seAverage.Value := seAverage.Value + 1
                 else seAverage.Value := seAverage.Value - 1;
       end;
end; // TFormViewProfile.seAverageChange.


procedure TFormViewProfile.seScanWidthChange(Sender : TObject);
begin
  if Assigned(FProfile)
  then if (seScanWidth.Text <> '')
       then FProfile.ScanWidth := seScanWidth.Value;
end; // TFormViewProfile.seScanWidthChange.


procedure TFormViewProfile.rsAngleChange(Sender : TObject);
begin
  if Assigned(FProfile)
  then FProfile.Angle := rsAngle.Value;
end; // TFormViewProfile.rsAngleChange.


procedure TFormViewProfile.rsLengthChange(Sender : TObject);
begin
  if Assigned(FProfile)
  then begin
       if (rsLength.Value >= 1.0)
       then FProfile.Length := rsLength.Value;
  end;
end; // TFormViewProfile.rsLengthChange.


procedure TFormViewProfile.seDevirativeChange(Sender : TObject);
begin
  if (seDevirative.Text <> '')
  then begin
       FProfile.MinDerivative := seDevirative.Value;
  end;
end; // TFormViewProfile.seDevirativeChange.


procedure TFormViewProfile.seHysteresisChange(Sender : TObject);
begin
  if (seHysteresis.Text <> '')
  then begin
       FProfile.Hysteresis := seHysteresis.Value;
  end;
end; // TFormViewProfile.seHysteresisChange.


procedure TFormViewProfile.cbPositiveClick(Sender : TObject);
begin
  if Assigned(FProfile)
  then begin
       if cbPositive.Checked
       then FProfile.Transitions := FProfile.Transitions + [PT_POSITIVE]
       else FProfile.Transitions := FProfile.Transitions - [PT_POSITIVE];
  end;
end; // TFormViewProfile.cbPositiveClick.


procedure TFormViewProfile.cbNegativeClick(Sender : TObject);
begin
  if Assigned(FProfile)
  then begin
       if cbNegative.Checked
       then FProfile.Transitions := FProfile.Transitions + [PT_NEGATIVE]
       else FProfile.Transitions := FProfile.Transitions - [PT_NEGATIVE];
  end;
end; // TFormViewProfile.cbNegativeClick.


procedure TFormViewProfile.cbPeakClick(Sender : TObject);
begin
  if Assigned(FProfile)
  then begin
       if cbPeak.Checked
       then FProfile.Transitions := FProfile.Transitions + [PT_PEAK]
       else FProfile.Transitions := FProfile.Transitions - [PT_PEAK];
  end;
end; // TFormViewProfile.cbPeakClick.


procedure TFormViewProfile.cbValleyClick(Sender : TObject);
begin
  if Assigned(FProfile)
  then begin
       if cbValley.Checked
       then FProfile.Transitions := FProfile.Transitions + [PT_VALLEY]
       else FProfile.Transitions := FProfile.Transitions - [PT_VALLEY];
  end;
end; // TFormViewProfile.cbValleyClick.


procedure TFormViewProfile.cbCenterPeaksClick(Sender : TObject);
begin
  if Assigned(FProfile)
  then begin
       FProfile.CenterPeaks := cbCenterPeaks.Checked;
  end;
end; // TFormViewProfile.cbCenterPeaksClick.


procedure TFormViewProfile.sbProfileMouseMove(Sender : TObject;
                                              Shift  : TShiftState;
                                              X, Y   : Integer);
begin
  lValues.BringToFront;
  if Assigned(FProfile)
  then begin
       if (FProfile.Transitions <> [])
       then lValues.Caption := 'Intensity: ' + IntToStr(FProfile.ScanData[x]) +
                               '  Derivative: ' + IntToStr(FProfile.Derivative[x])
       else lValues.Caption := 'Intensity: ' + IntToStr(FProfile.ScanData[x]);
  end;
end; // TFormViewProfile.sbProfileMouseMove.


//------------------------------------------------------------------------------
// Result page
//------------------------------------------------------------------------------

procedure TFormViewProfile.ClearResults;
begin
  sgResults.RowCount := 2;
  sgResults.DefaultColWidth := 43;
  sgResults.ColWidths[2] := 26;
  sgResults.ColWidths[3] := 48;

  // Set header.
  sgResults.Cells[0,0] := 'x';
  sgResults.Cells[1,0] := 'y';
  sgResults.Cells[2,0] := 'int';
  sgResults.Cells[3,0] := 'type';

  // Clear first row.
  sgResults.Cells[0,1] := '';
  sgResults.Cells[1,1] := '';
  sgResults.Cells[2,1] := '';
  sgResults.Cells[3,1] := '';

  Update;
end; // TFormViewProfile.ClearResults.



initialization
  FormViewProfile := Nil;
end.
