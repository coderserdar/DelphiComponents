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
// $Log:  17615: uFormStretch.pas 
//
//    Rev 1.4    2014-02-02 21:10:12  mcm    Version: IMG 4.0
// Added support for Delphi XE2, 3, 4 and 5
//
//   Rev 1.3    23-01-2005 09:35:36  mcm
// Modified to support BW images.

//
//   Rev 1.2    20-12-2004 22:58:08  mcm
// Modified to use TmcmInt

//
//   Rev 1.1    25-09-2003 23:36:26  mcm    Version: IMG 1.5
// Included interpolation.

//
//   Rev 1.0    27-05-2002 16:22:38  mcm

unit uFormStretch;

interface

{$Include 'mcmDefines.pas'}

uses {$IFNDEF GE_DXE2}
      Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
      StdCtrls, Spin,
     {$ELSE}
      WinApi.Windows, WinApi.Messages, System.SysUtils, System.Classes, Vcl.Controls,
      Vcl.Graphics, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Samples.Spin,
     {$ENDIF}
     umcmIntE,
     mcmImageTypeDef,
     mcmImage;

type
  TFormResize = class(TForm)
    gbPixelSize     : TGroupBox;
    lPixelWidth     : TLabel;
    lPixelHeight    : TLabel;
    gbPercentage    : TGroupBox;
    lPercentWidth   : TLabel;
    lPercentHeight  : TLabel;
    btnOK           : TButton;
    btnCancel       : TButton;
    gbActualSize    : TGroupBox;
    lActualWidth    : TLabel;
    lActualHeight   : TLabel;
    lActualRes      : TLabel;
    rsActualWidth   : TmcmRealSpin;
    rsActualHeight  : TmcmRealSpin;
    rsActualRes     : TmcmRealSpin;
    cbActualRes     : TComboBox;
    cbMeasure       : TComboBox;
    gbMethod        : TGroupBox;
    lResizeMethod   : TLabel;
    lto1            : TLabel;
    cbAspectRatio   : TCheckBox;
    cbMethod        : TComboBox;
    rsAspectRatio   : TmcmRealSpin;
    sePixelWidth    : TmcmIntSpin;
    sePixelHeight   : TmcmIntSpin;
    rsPercentWidth  : TmcmRealSpin;
    rsPercentHeight : TmcmRealSpin;
    procedure sePixelWidthChange(Sender: TObject);
    procedure sePixelHeightChange(Sender: TObject);
    procedure rsPercentWidthChange(Sender: TObject);
    procedure rsPercentHeightChange(Sender: TObject);
    procedure cbMethodChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure cbAspectRatioClick(Sender: TObject);
    procedure rsActualWidthChange(Sender: TObject);
    procedure rsActualHeightChange(Sender: TObject);
    procedure cbMeasureChange(Sender: TObject);
    procedure rsActualResChange(Sender: TObject);
    procedure cbActualResChange(Sender: TObject);
    procedure rsAspectRatioChange(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
  private
    { Private declarations }
    FReady          : boolean;
    FOldPixelWidth  : integer;
    FOldPixelHeight : integer;
    FNewPixelWidth  : integer;
    FNewPixelHeight : integer;
    FAspectRatio    : double;
    Fdpi            : double;

    FMethod         : TmcmResize;
    FHorizScale     : double;
    FVertScale      : double;
    FImage          : TmcmImage;
    function  GetPixelHeight : integer;
    function  GetPixelWidth : integer;
    function  GetResolution : integer;
    procedure SetImage(Value : TmcmImage);
    procedure SetPixelHeight(Value : integer);
    procedure SetPixelWidth(Value : integer);
    procedure SetInterpolate(Value : TmcmResize);
    procedure SetResolution(Value : integer);
    procedure SetHorizScale(Value : double);
    procedure SetVertScale(Value : double);
    procedure UpdateAllControls(Sender : TObject);
  public
    { Public declarations }
    property HorizScale : double
      read   FHorizScale
      write  SetHorizScale;
    property Image : TmcmImage
      read   FImage
      write  SetImage;
    property Interpolate : TmcmResize
      read   FMethod
      write  SetInterpolate;
    property PixelHeight : integer
      read   GetPixelHeight
      write  SetPixelHeight;
    property PixelWidth : integer
      read   GetPixelWidth
      write  SetPixelWidth;
    property Resolution : integer
      read   GetResolution
      write  SetResolution;
    property VertScale : double
      read   FVertScale
      write  SetVertScale;
  end;

var FormResize : TFormResize;

implementation

{$R *.DFM}

procedure TFormResize.FormCreate(Sender : TObject);
begin
  FMethod     := ST_BILINEAR;
  FHorizScale := 100.0;
  FVertScale  := 100.0;
  cbMeasure.ItemIndex := 1;
  cbActualRes.ItemIndex := 1;
  FReady := True;
  FImage := Nil;
end; // TFormResize.FormCreate.


procedure TFormResize.SetImage(Value : TmcmImage);
begin
  FImage := Value;
  if Assigned(FImage)
  then begin
       if (FImage.ImageFormat = IF_BW) or
          (FImage.ImageFormat = IF_PAL8)
       then begin
            cbMethod.ItemIndex := integer(ST_NEAREST);
            cbMethod.Enabled := False;
       end;
       FOldPixelHeight := FImage.Height;
       FOldPixelWidth  := FImage.Width;
       FAspectRatio    := FOldPixelWidth / FOldPixelHeight;

       FNewPixelHeight := FOldPixelHeight;
       FNewPixelWidth  := FOldPixelWidth;

       Fdpi := 2.54 * FImage.XResolution / 100.0;

     //  sePixelHeight.Value := FOldPixelHeight;
     //  sePixelWidth.Value  := FOldPixelWidth;

       rsAspectRatio.Value := FOldPixelWidth / FOldPixelHeight;
       rsActualRes.Value := Fdpi;
  end;
end; // TFormResize.SetImage.


procedure TFormResize.UpdateAllControls(Sender : TObject);
begin
  if FReady
  then begin
       sePixelWidth.OnChange := Nil;
       sePixelHeight.OnChange := Nil;
       rsPercentWidth.OnChange := Nil;
       rsPercentHeight.OnChange := Nil;
       rsActualWidth.OnChange := Nil;
       rsActualHeight.OnChange := Nil;
       rsAspectRatio.OnChange := Nil;

       FHorizScale := 100.0 * FNewPixelWidth / FOldPixelWidth;
       FVertScale  := 100.0 * FNewPixelHeight / FOldPixelHeight;

       if Assigned(cbAspectRatio)
       then if cbAspectRatio.Checked
            then FHorizScale := FVertScale;

       if (Sender <> sePixelWidth)
       then sePixelWidth.Value := FNewPixelWidth;
       if (Sender <> sePixelHeight)
       then sePixelHeight.Value := FNewPixelHeight;
       if (Sender <> rsPercentWidth)
       then rsPercentWidth.Value := FHorizScale;
       if (Sender <> rsPercentHeight)
       then rsPercentHeight.Value := FVertScale;

       if (Sender <> rsActualWidth) and (Fdpi > 0.0)
       then begin
            case cbMeasure.ItemIndex of
            0 : rsActualWidth.Value := 2.54 * FNewPixelWidth / Fdpi;
            1 : rsActualWidth.Value := FNewPixelWidth / Fdpi;
            end;
       end;
       if (Sender <> rsActualHeight) and (Fdpi > 0.0)
       then begin
            case cbMeasure.ItemIndex of
            0 : rsActualHeight.Value := 2.54 * FNewPixelHeight / Fdpi;
            1 : rsActualHeight.Value := FNewPixelHeight / Fdpi;
            end;
       end;

       {
       if (Sender <> rsAspectRatio)
       then rsAspectRatio.Value := FNewPixelWidth / FNewPixelHeight;
       }

       sePixelWidth.OnChange := sePixelWidthChange;
       sePixelHeight.OnChange := sePixelHeightChange;
       rsPercentWidth.OnChange := rsPercentWidthChange;
       rsPercentHeight.OnChange := rsPercentHeightChange;
       rsActualWidth.OnChange := rsActualWidthChange;
       rsActualHeight.OnChange := rsActualHeightChange;
       rsAspectRatio.OnChange := rsAspectRatioChange;
  end;
end; // TFormResize.UpdateAllControls.


function TFormResize.GetPixelHeight : integer;
begin
  Result := FNewPixelHeight;
end; // TFormResize.GetPixelHeight.


function TFormResize.GetPixelWidth : integer;
begin
  Result := FNewPixelWidth;
end; // TFormResize.GetPixelWidth.


function TFormResize.GetResolution : integer;
begin
  Result := Round(100.0 * Fdpi / 2.54);
end; // TFormResize.GetResolution.


procedure TFormResize.SetPixelHeight(Value : integer);
begin
  FOldPixelHeight := Value;
  FNewPixelHeight := Value;
  sePixelHeight.Value := Value;
end; // TFormResize.SetPixelHeight.


procedure TFormResize.SetPixelWidth(Value : integer);
begin
  FOldPixelWidth := Value;
  FNewPixelWidth := Value;
  sePixelWidth.Value := Value;
end; // TFormResize.SetPixelWidth.


procedure TFormResize.SetHorizScale(Value : double);

begin
  rsPercentWidth.OnChange := Nil;

  FHorizScale  := Value;
  rsPercentWidth.Value := Value;
  FNewPixelWidth := Round(FHorizScale * FOldPixelWidth / 100.0);

  sePixelWidth.OnChange := Nil;
  sePixelWidth.Value := FNewPixelWidth;
  sePixelWidth.OnChange := sePixelWidthChange;

  rsPercentWidth.OnChange := rsPercentWidthChange;
end; // TFormResize.SetHorizScale.


procedure TFormResize.SetVertScale(Value : double);
begin
  rsPercentHeight.OnChange := Nil;

  FVertScale  := Value;
  rsPercentHeight.Value := Value;
  FNewPixelHeight := Round(FVertScale * FOldPixelHeight / 100.0);

  sePixelHeight.OnChange := Nil;
  sePixelHeight.Value := FNewPixelHeight;
  sePixelHeight.OnChange := sePixelHeightChange;

  rsPercentHeight.OnChange := rsPercentHeightChange;
end; // TFormResize.SetVertScale.


procedure TFormResize.SetInterpolate(Value : TmcmResize);
begin
  if cbMethod.Enabled
  then begin
       FMethod := Value;
       cbMethod.ItemIndex := integer(Value);
  end;
end; // TFormResize.SetInterpolate.


procedure TFormResize.SetResolution(Value : integer);
begin
  Fdpi := 2.54 * Value / 100.0;
end; // TFormResize.SetResolution.


procedure TFormResize.sePixelWidthChange(Sender : TObject);
begin
  if (sePixelWidth.Text <> '') and (Sender = sePixelWidth)
  then begin
       FNewPixelWidth := sePixelWidth.Value;
       if Assigned(cbAspectRatio)
       then if cbAspectRatio.Checked
            then FNewPixelHeight := Round(FNewPixelWidth / FAspectRatio);
       UpdateAllControls(Sender);
  end;
end; // TFormResize.sePixelWidthChange.


procedure TFormResize.sePixelHeightChange(Sender : TObject);
begin
  if (sePixelHeight.Text <> '') and (Sender = sePixelHeight)
  then begin
       FNewPixelHeight := sePixelHeight.Value;
       if Assigned(cbAspectRatio)
       then if cbAspectRatio.Checked
            then FNewPixelWidth := Round(FNewPixelHeight * FAspectRatio);
       UpdateAllControls(Sender);
  end;
end; // TFormResize.sePixelHeightChange.


procedure TFormResize.rsPercentWidthChange(Sender : TObject);
begin
  if (rsPercentWidth.Text <> '') and (Sender = rsPercentWidth)
  then begin
       FHorizScale  := rsPercentWidth.Value;
       FNewPixelWidth := Round(FHorizScale * FOldPixelWidth / 100.0);
       if Assigned(cbAspectRatio)
       then if cbAspectRatio.Checked
            then FNewPixelHeight := Round(FNewPixelWidth / FAspectRatio);
       UpdateAllControls(Sender);
  end;
end; // TFormResize.rsPercentWidthChange.


procedure TFormResize.rsPercentHeightChange(Sender : TObject);
begin
  if (rsPercentHeight.Text <> '') and (Sender = rsPercentHeight)
  then begin
       FVertScale := rsPercentHeight.Value;
       FNewPixelHeight := Round(FVertScale * FOldPixelHeight / 100.0);
       if Assigned(cbAspectRatio)
       then if cbAspectRatio.Checked
            then FNewPixelWidth := Round(FNewPixelHeight * FAspectRatio);
       UpdateAllControls(Sender);
  end;
end; // TFormResize.rsPercentHeightChange.


procedure TFormResize.rsActualWidthChange(Sender : TObject);
begin
  if (rsActualWidth.Text <> '')
  then begin
       if Assigned(cbMeasure)
       then begin
            case cbMeasure.ItemIndex of
            0 : FNewPixelWidth  := Round(rsActualWidth.Value * Fdpi / 2.54);
            1 : FNewPixelWidth  := Round(rsActualWidth.Value * Fdpi);
            end;
       end;

       if Assigned(cbAspectRatio)
       then if cbAspectRatio.Checked
            then FNewPixelHeight := Round(FNewPixelWidth / FAspectRatio);
       UpdateAllControls(Sender);
  end;
end;


procedure TFormResize.rsActualHeightChange(Sender : TObject);
begin
  if (rsActualHeight.Text <> '')
  then begin
       if Assigned(cbMeasure)
       then begin
            case cbMeasure.ItemIndex of
            0 : FNewPixelHeight := Round(rsActualHeight.Value * Fdpi / 2.54);
            1 : FNewPixelHeight := Round(rsActualHeight.Value * Fdpi);
            end;
       end;
       if Assigned(cbAspectRatio)
       then if cbAspectRatio.Checked
            then FNewPixelWidth := Round(FNewPixelHeight * FAspectRatio);
       UpdateAllControls(Sender);
  end;
end;


procedure TFormResize.cbMeasureChange(Sender : TObject);
begin
  UpdateAllControls(Sender);
end;


procedure TFormResize.rsActualResChange(Sender : TObject);
begin
  if (rsActualRes.Text <>  '')
  then begin
       if Assigned(cbActualRes)
       then begin
            case cbActualRes.ItemIndex of
            0 : Fdpi := rsActualRes.Value * 2.54;
            1 : Fdpi := rsActualRes.Value;
            end;
            UpdateAllControls(Sender);
       end;
  end;
end;


procedure TFormResize.cbActualResChange(Sender : TObject);
begin
  case cbActualRes.ItemIndex of
  0 : rsActualRes.Value := Fdpi / 2.54;
  1 : rsActualRes.Value := Fdpi;
  end;
end;


procedure TFormResize.cbMethodChange(Sender : TObject);
begin
  FMethod := TmcmResize(cbMethod.ItemIndex);
end; // TFormResize.cbMethodChange.


procedure TFormResize.cbAspectRatioClick(Sender : TObject);
begin
  if cbAspectRatio.Checked
  then begin
       rsAspectRatio.Value := FNewPixelWidth / FNewPixelHeight;
  end;
end; // TFormResize.cbAspectRatioClick.


procedure TFormResize.rsAspectRatioChange(Sender : TObject);
begin
  if (rsAspectRatio.Text <> '')
  then begin
       FAspectRatio := rsAspectRatio.Value;
       FNewPixelHeight := Round(FNewPixelWidth / FAspectRatio);
       UpdateAllControls(Sender);
  end;
end; // TFormResize.rsAspectRatioChange.


procedure TFormResize.btnOKClick(Sender : TObject);
begin
  if Assigned(FImage)
  then begin

  end;
end;

end.
