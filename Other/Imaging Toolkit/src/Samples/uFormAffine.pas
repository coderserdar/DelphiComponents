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
// $Log:  21505: uFormAffine.pas 
//
//    Rev 1.4    2014-02-02 21:10:08  mcm    Version: IMG 4.0
// Added support for Delphi XE2, 3, 4 and 5
//
//   Rev 1.3    23-01-2005 09:35:34  mcm
// Modified to support BW images.

//
//   Rev 1.2    20-12-2004 22:58:08  mcm
// Modified to use TmcmInt

//
//   Rev 1.1    06-11-2003 18:00:48  mcm    Version: IMG 2.0
// Changed image background color.

//
//   Rev 1.0    25-09-2003 23:25:46  mcm    Version: IMG 1.5

unit uFormAffine;

interface

{$Include 'mcmDefines.pas'}

uses {$IFNDEF GE_DXE2}
      Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
      StdCtrls, Spin,
     {$ELSE}
      WinApi.Windows, WinApi.Messages, System.SysUtils, System.Classes,
      Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
      Vcl.Samples.Spin,
     {$ENDIF}
     mcmImage, mcmImageTransform, mcmImageDualView, mcmImageTypeDef,
     mcmImageKernel, umcmIntE;

type
  TFormAffine = class(TForm)
    gbStretch        : TGroupBox;
    gbRotate         : TGroupBox;
    gbShear          : TGroupBox;
    gbTranslate      : TGroupBox;
    DualView         : TmcmImageDualView;
    btnOK            : TButton;
    btnCancel        : TButton;
    lHorizScale      : TLabel;
    LVertScale       : TLabel;
    LHorizShear      : TLabel;
    LVertShear       : TLabel;
    LHorizTrans      : TLabel;
    LVertTrans       : TLabel;
    LRotate          : TLabel;
    gbInterpolate    : TGroupBox;
    cbMethod         : TComboBox;
    mcmImageTransform: TmcmImageTransform;
    rsxTrans         : TmcmRealSpin;
    rsyTrans         : TmcmRealSpin;
    sexScale         : TmcmIntSpin;
    seyScale         : TmcmIntSpin;
    sexShear         : TmcmIntSpin;
    seyShear         : TmcmIntSpin;
    seRotate         : TmcmIntSpin;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure AffineChange(Sender: TObject);
    procedure DualViewImageMoved(Sender: TObject);
    procedure cbMethodChange(Sender: TObject);
    procedure AffineDblChange(Sender: TObject);
  private
    { Private declarations }
    FImage           : TmcmImage;

    procedure   UpdateView;
    procedure   SetImage(Value : TmcmImage);
    function    GetInterpolate : TmcmInterpolate;
    procedure   SetInterpolate(Value : TmcmInterpolate);
    function    GetDegree : double;
    procedure   SetDegree(Value : double);
    function    GetxDist : double;
    procedure   SetxDist(Value : double);
    function    GetxScale : double;
    procedure   SetxScale(Value : double);
    function    GetxShear : double;
    procedure   SetxShear(Value : double);
    function    GetyDist : double;
    procedure   SetyDist(Value : double);
    function    GetyScale : double;
    procedure   SetyScale(Value : double);
    function    GetyShear : double;
    procedure   SetyShear(Value : double);
  public
    { Public declarations }
    property    Image : TmcmImage
      read      FImage
      write     SetImage;
    property    Interpolate : TmcmInterpolate
      read      GetInterpolate
      write     SetInterpolate;

    property    Degree : double
      read      GetDegree
      write     SetDegree;
    property    xDist : double
      read      GetxDist
      write     SetxDist;
    property    xScale : double
      read      GetxScale
      write     SetxScale;
    property    xShear : double
      read      GetxShear
      write     SetxShear;
    property    yDist : double
      read      GetyDist
      write     SetyDist;
    property    yScale : double
      read      GetyScale
      write     SetyScale;
    property    yShear : double
      read      GetyShear
      write     SetyShear;
  end;

var FormAffine : TFormAffine;

implementation

{$R *.DFM}


procedure TFormAffine.FormCreate(Sender : TObject);
begin
  FImage := Nil;
  btnOK.Enabled  := False;

  sexScale.Value := 100;
  seyScale.Value := 100;
  sexShear.Value := 0;
  seyShear.Value := 0;
  rsxTrans.Value := 0;
  rsyTrans.Value := 0;
  seRotate.Value := 0;

  FImage := TmcmImage.Create;
  FImage.Width  := 153;
  FImage.Height := 153;

  mcmImageTransform.SourceImage[0] := FImage;
  mcmImageTransform.ResultImage := TmcmImage.Create;
  mcmImageTransform.ResultImage.Width  := FImage.Width;
  mcmImageTransform.ResultImage.Height := FImage.Height;
end; // TFormAffine.FormCreate.


procedure TFormAffine.FormDestroy(Sender : TObject);
begin
  FImage.Free;
  FImage := Nil;
  mcmImageTransform.ResultImage.Free;
  mcmImageTransform.ResultImage := Nil;
  DualView.ResultImage := Nil;
end; // TFormAffine.FormDestroy.


procedure TFormAffine.FormShow(Sender : TObject);
begin
  UpdateView;
end; // TFormAffine.FormShow.


procedure TFormAffine.UpdateView;
var dx, dy     : double;
begin
  if Assigned(DualView.SourceImage)
  then begin
       mcmImageTransform.KeepResultSize := True;
       mcmImageTransform.Degree := seRotate.Value;

       dx := sexScale.Value / 100.0;
       dy := seyScale.Value / 100.0;
       mcmImageTransform.xScale := dx;
       mcmImageTransform.yScale := dy;

       // The following lines are only added here to visually compensate for the
       // autro-adjustment of the image centre when rotating!!!
       if seRotate.Value = 0
       then begin
            if (dx <> 1.0)
            then dx := 0.5 * (dx - 1.0) * mcmImageTransform.ResultImage.Width;
            if (dy <> 1.0)
            then dy := 0.5 * (dy - 1.0) * mcmImageTransform.ResultImage.Height;
       end
       else begin
            dx := 0.0;
            dy := 0.0;
       end;

       mcmImageTransform.xDist  := rsxTrans.Value - dx;
       mcmImageTransform.yDist  := rsyTrans.Value - dy;
       
       mcmImageTransform.xShear := sexShear.Value / DualView.SourceImage.Width;
       mcmImageTransform.yShear := seyShear.Value / DualView.SourceImage.Height;
       mcmImageTransform.BKColor := RGB(255,255,255);
       mcmImageTransform.Interpolate := Interpolate;
       mcmImageTransform.Affine;

       DualView.UpdateResultView;
  end;
end; // TFormAffine.UpdateView.


procedure TFormAffine.AffineChange(Sender : TObject);
begin
  if ((Sender as TmcmIntSpin).Text <> '')
  then begin
       if (DualView.SourceImage <> Nil) and
          ((sexScale.Value <> 100) or
           (seyScale.Value <> 100) or
           (sexShear.Value <> 0) or
           (seyShear.Value <> 0) or
           (rsxTrans.Value <> 0) or
           (rsyTrans.Value <> 0) or
           (seRotate.Value <> 0))
       then btnOK.Enabled := True
       else btnOK.Enabled := False;
       UpdateView;
  end;
end; // TFormAffine.AffineChange.


procedure TFormAffine.AffineDblChange(Sender : TObject);
begin
  if ((Sender as TmcmRealSpin).Text <> '')
  then begin
       if Assigned(DualView)
       then begin
            if (DualView.SourceImage <> Nil) and
               ((sexScale.Value <> 100) or
                (seyScale.Value <> 100) or
                (sexShear.Value <> 0) or
                (seyShear.Value <> 0) or
                (rsxTrans.Value <> 0) or
                (rsyTrans.Value <> 0) or
                (seRotate.Value <> 0))
            then btnOK.Enabled := True
            else btnOK.Enabled := False;
            UpdateView;
       end;
  end;
end; // TFormAffine.AffineDblChange.

procedure TFormAffine.cbMethodChange(Sender : TObject);
begin
  UpdateView;
end; // TFormAffine.cbMethodChange.


procedure TFormAffine.SetImage(Value : TmcmImage);
var xScale      : double;
    yScale      : double;
begin
  if (Value <> Nil)
  then begin
       DualView.SourceImage := Value;

       if (Value.ImageFormat = IF_BW) or
          (Value.ImageFormat = IF_PAL8)
       then begin
            cbMethod.ItemIndex := integer(ST_NEAREST);
            cbMethod.Enabled := False;
       end;

       FImage.ImageFormat := Value.ImageFormat;
       FImage.Palette := Value.Palette;

       mcmImageTransform.ResultImage.ImageFormat := Value.ImageFormat;
       mcmImageTransform.ResultImage.Palette := Value.Palette;
       mcmImageTransform.ResultImage.FillAll(255);

       // Create and draw thumb-nail.
       xScale := FImage.Width / Value.Width;
       yScale := FImage.Height / Value.Height;
       if (xScale > yScale)
       then xScale := yScale;
       DualView.SourceImage.Draw(FImage.Canvas.Handle, xScale);

       DualView.ResultImage := mcmImageTransform.ResultImage;
       DualView.UpdateResultView;
  end
  else DualView.SourceImage := Nil;
end; // TFormAffine.SetImage.


function TFormAffine.GetInterpolate : TmcmInterpolate;
begin
  Result := TmcmInterpolate(cbMethod.ItemIndex);
end; // TFormAffine.GetInterpolate.


procedure TFormAffine.SetInterpolate(Value : TmcmInterpolate);
begin
  if cbMethod.Enabled
  then cbMethod.ItemIndex := integer(Value);
end; // TFormAffine.SetInterpolate.


function TFormAffine.GetDegree : double;
begin
  Result := seRotate.Value;
end; // TFormAffine.GetDegree.


procedure TFormAffine.SetDegree(Value : double);
begin
  seRotate.Value := Round(Value);
end; // TFormAffine.SetDegree.


function TFormAffine.GetxDist : double;
begin
  Result := rsxTrans.Value;
end; // TFormAffine.GetxDist.


procedure TFormAffine.SetxDist(Value : double);
begin
  rsxTrans.Value := Value;
end; // TFormAffine.SetxDist.


function TFormAffine.GetxScale : double;
begin
  Result := sexScale.Value;
end; // TFormAffine.GetxScale.


procedure TFormAffine.SetxScale(Value : double);
begin
  sexScale.Value := Round(Value);
end; // TFormAffine.SetxScale.


function TFormAffine.GetxShear : double;
begin
  Result := sexShear.Value;
end; // TFormAffine.GetxShear.


procedure TFormAffine.SetxShear(Value : double);
begin
  sexShear.Value := Round(Value);
end; // TFormAffine.SetxShear.


function TFormAffine.GetyDist : double;
begin
  Result := rsyTrans.Value;
end; // TFormAffine.GetyDist.


procedure TFormAffine.SetyDist(Value : double);
begin
  rsyTrans.Value := Value;
end; // TFormAffine.SetyDist.


function TFormAffine.GetyScale : double;
begin
  Result := seyScale.Value;
end; // TFormAffine.GetyScale.


procedure TFormAffine.SetyScale(Value : double);
begin
  seyScale.Value := Round(Value);
end; // TFormAffine.SetyScale.


function TFormAffine.GetyShear : double;
begin
  Result := seyShear.Value;
end; // TFormAffine.GetyShear.


procedure TFormAffine.SetyShear(Value : double);
begin
  seyShear.Value := Round(Value);
end; // TFormAffine.SetyShear.


procedure TFormAffine.DualViewImageMoved(Sender : TObject);
begin
  if Not(FImage.Empty)
  then begin
       FImage.FillAll(255);
       DualView.PaintSourceView(FImage);
       UpdateView;
  end;
end; // TFormAffine.DualViewImageMoved.


end.
