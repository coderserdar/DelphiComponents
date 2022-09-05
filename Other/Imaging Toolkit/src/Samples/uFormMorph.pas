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
// $Log:  20269: uFormMorph.pas 
//
//    Rev 1.3    2014-02-02 21:10:10  mcm    Version: IMG 4.0
// Added support for Delphi XE2, 3, 4 and 5
//
//   Rev 1.2    20-12-2004 22:58:10  mcm
// Modified to use TmcmInt

//
//   Rev 1.1    15-06-2003 13:20:20  mcm    Version: IMG 1.3.4

//
//   Rev 1.0    12-05-2003 15:34:00  mcm    Version: IMG 1.3.4
// Initial code.

unit uFormMorph;

interface

{$Include 'mcmDefines.pas'}

uses {$IFNDEF GE_DXE2}
      Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
      StdCtrls, Spin,
     {$ELSE}
      WinApi.Windows, WinApi.Messages, System.SysUtils, System.Classes, Vcl.Controls,
      Vcl.Graphics, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Samples.Spin,
     {$ENDIF}
     mcmImage,
     mcmImageDualView,
     mcmImageKernel,
     mcmImageMorph,
     umcmIntE;

type
  TFormMorph = class(TForm)
    mcmImageMorph : TmcmImageMorph;
    DualView      : TmcmImageDualView;
    gbOperation   : TGroupBox;
    btnOK         : TButton;
    btnCancel     : TButton;
    gbFeatures    : TGroupBox;
    rbBlack       : TRadioButton;
    rbWhite       : TRadioButton;
    cbKeepSep     : TCheckBox;
    cbMethod      : TComboBox;
    lIterations   : TLabel;
    lCoefEven     : TLabel;
    lCoefOdd      : TLabel;
    seIterations  : TmcmIntSpin;
    seCoefEven    : TmcmIntSpin;
    seCoefOdd     : TmcmIntSpin;
    procedure FormCreate(Sender : TObject);
    procedure btnOKClick(Sender : TObject);
    procedure btnCancelClick(Sender : TObject);
    procedure FormShow(Sender : TObject);
    procedure cbMethodChange(Sender : TObject);
    procedure SpinChange(Sender: TObject);
    procedure cbKeepSepClick(Sender: TObject);
    procedure rbBlackClick(Sender: TObject);
    procedure rbWhiteClick(Sender: TObject);
  private
    { Private declarations }
    FCreatedResult : boolean;
    {$IFDEF DCB3}
    FStartTime       : TLargeInteger;
    FEndTime         : TLargeInteger;
    {$ELSE}
    FStartTime       : int64;
    FEndTime         : int64;
    {$ENDIF}

    function  GetResultImage : TmcmImage;
    function  GetSourceImage : TmcmImage;
    procedure SetResultImage(AImage : TmcmImage);
    procedure SetSourceImage(AImage : TmcmImage);
    procedure UpdateResult;
  public
    { Public declarations }
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

var FormMorph : TFormMorph;

implementation

{$R *.DFM}

uses mcmImageTypeDef;

procedure TFormMorph.FormCreate(Sender : TObject);
begin
  FCreatedResult := False;
  cbMethod.ItemIndex := 0;
  cbKeepSep.Checked := mcmImageMorph.KeepSeparate;
  if mcmImageMorph.FeaturesAreWhite
  then rbWhite.Checked := True
  else rbBlack.Checked := True;
end; // TFormMorph.FormCreate.


procedure TFormMorph.btnOKClick(Sender : TObject);
begin
;
end; // TFormMorph.btnOKClick.


procedure TFormMorph.btnCancelClick(Sender : TObject);
begin
  if FCreatedResult
  then mcmImageMorph.ResultImage.Free;
  mcmImageMorph.ResultImage := Nil;
end; // TFormMorph.btnCancelClick.


procedure TFormMorph.FormShow(Sender : TObject);
begin
  UpdateResult;
end; // TFormMorph.FormShow.


procedure TFormMorph.cbMethodChange(Sender : TObject);
begin
  case cbMethod.ItemIndex of
  0..3 : begin
           seCoefEven.Enabled := True;
           seCoefOdd.Enabled := True;
           seIterations.Enabled := True;
           cbKeepSep.Enabled := True;
         end;
  else begin
         seCoefEven.Enabled := False;
         seCoefOdd.Enabled := False;
         seIterations.Enabled := False;
         cbKeepSep.Enabled := False;
  end;
  end;
  UpdateResult;
end; // TFormMorph.cbMethodChange.


function TFormMorph.GetResultImage : TmcmImage;
begin
  Result := DualView.ResultImage;
end; // TFormMorph.GetResultImage.


function TFormMorph.GetSourceImage : TmcmImage;
begin
  Result := DualView.SourceImage;
end; // TFormMorph.GetSourceImage.


procedure TFormMorph.SetResultImage(AImage : TmcmImage);
begin
  DualView.ResultImage := AImage;
end; // TFormMorph.SetResultImage.


procedure TFormMorph.SetSourceImage(AImage : TmcmImage);
begin
  DualView.SourceImage := AImage;
  mcmImageMorph.SourceImage[0] := AImage;
end; // TFormMorph.SetSourceImage.


procedure TFormMorph.UpdateResult;
var Iterations : word;
    SaveCursor : TCursor;
begin
  if Assigned(mcmImageMorph)
  then begin
       if Assigned(mcmImageMorph.SourceImage[0])
       then begin
            if Not(Assigned(mcmImageMorph.ResultImage))
            then begin
                 mcmImageMorph.ResultImage := TmcmImage.Create;
                 mcmImageMorph.ResultImage.Width  := mcmImageMorph.SourceImage[0].Width;
                 mcmImageMorph.ResultImage.Height := mcmImageMorph.SourceImage[0].Height;
                 mcmImageMorph.ResultImage.ImageFormat := IF_GREY8;
                 mcmImageMorph.ResultImage.Palette := mcmImageMorph.SourceImage[0].Palette;
                 mcmImageMorph.ResultImage.XResolution := mcmImageMorph.SourceImage[0].XResolution;
                 mcmImageMorph.ResultImage.YResolution := mcmImageMorph.SourceImage[0].YResolution;
                 DualView.ResultImage := mcmImageMorph.ResultImage;
                 FCreatedResult := True;
            end;

            Iterations := seIterations.Value;
            mcmImageMorph.Coefficient[0] := seCoefEven.Value;
            mcmImageMorph.Coefficient[1] := seCoefOdd.Value;

            // Get start time
            SaveCursor := Screen.Cursor;
            Screen.Cursor := crHourGlass;
            try
              QueryPerformanceCounter(FStartTime);
              case cbMethod.ItemIndex of
              0 : mcmImageMorph.Close(Iterations);
              1 : mcmImageMorph.Dilate(Iterations);
              2 : mcmImageMorph.Erode(Iterations);
              3 : mcmImageMorph.Open(Iterations);
              4 : mcmImageMorph.Outline;
              5 : mcmImageMorph.Shrink(65535);
              6 : mcmImageMorph.Skeleton;
              7 : mcmImageMorph.Skeleton2(65535);
              8 : mcmImageMorph.Thin(65535);
              end;
              QueryPerformanceCounter(FEndTime);
              DualView.UpdateResultView;
            finally
              Screen.Cursor := SaveCursor;
            end;
       end;
  end;
end; // TFormMorph.UpdateResult.


procedure TFormMorph.SpinChange(Sender : TObject);
var v : integer;
begin
  try
    v := (Sender as TmcmIntSpin).Value;
    if (v >= 0)
    then UpdateResult;
  except
  end;
end;

procedure TFormMorph.cbKeepSepClick(Sender : TObject);
begin
  mcmImageMorph.KeepSeparate := cbKeepSep.Checked;
  UpdateResult;
end;

procedure TFormMorph.rbBlackClick(Sender : TObject);
begin
  mcmImageMorph.FeaturesAreWhite := False;
  UpdateResult;
end;

procedure TFormMorph.rbWhiteClick(Sender: TObject);
begin
  mcmImageMorph.FeaturesAreWhite := True;
  UpdateResult;
end;

end.
