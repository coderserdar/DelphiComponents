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
// $Log:  19043: uFormMath.pas 
//
//    Rev 1.4    2014-02-02 21:10:10  mcm    Version: IMG 4.0
// Added support for Delphi XE2, 3, 4 and 5
//
//   Rev 1.3    15-05-2005 19:48:22  mcm    Version: IMG 2.9
// Added support for Magnitude and Orientation operations.

//
//   Rev 1.2    11-02-2003 12:25:34  mcm
// Added Mul & Div image capability.

//
//   Rev 1.1    05-02-03 16:42:08  mcm    Version: IMG 1.3
// Adjusted to actual functionality of TmcmImageMath.

//
//   Rev 1.0    29-01-2003 16:16:30  mcm

unit uFormMath;

interface

{$Include 'mcmDefines.pas'}

{$IFDEF VER150} // Don't show "Unsafe code type and cast warnings".
{$WARN UNSAFE_TYPE OFF}
{$WARN UNSAFE_CODE OFF}
{$WARN UNSAFE_CAST OFF}
{$ENDIF}

uses {$IFNDEF GE_DXE2}
      Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
      StdCtrls,
     {$ELSE}
      WinApi.Windows, WinApi.Messages, System.SysUtils, System.Classes, Vcl.Controls,
      Vcl.Graphics, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
     {$ENDIF}
     mcmImageTypeDef, mcmImage, mcmImageMath, umcmIntE;

type
  TFormMath = class(TForm)
    btnOK          : TButton;
    btnCancel      : TButton;
    gbSource1      : TGroupBox;
    cbImage1       : TComboBox;
    icSource1      : TmcmImageCtrl;
    gbSource2      : TGroupBox;
    cbImage2       : TComboBox;
    icSource2      : TmcmImageCtrl;
    gbResult       : TGroupBox;
    cbMethod       : TComboBox;
    icResult       : TmcmImageCtrl;
    rsBlendFactor1 : TmcmRealSpin;
    rsBlendFactor2 : TmcmRealSpin;
    lBlendFactor1  : TLabel;
    lBlendFactor2  : TLabel;
    procedure FormCreate(Sender : TObject);
    procedure FormDestroy(Sender : TObject);
    procedure FormActivate(Sender : TObject);
    procedure cbImage1Change(Sender : TObject);
    procedure cbImage2Change(Sender : TObject);
    procedure cbMethodChange(Sender : TObject);
    procedure rsBlendFactorChange(Sender : TObject);
  private
    { Private declarations }
    FMethod    : TmcmImageMathematics;
    FImageMath : TmcmImageMath;

    function  GetBlendFactor(Index : byte) : double;
    function  GetMethod : TmcmImageMathematics;
    function  GetSourceImage(Index : byte) : TmcmImage;
    procedure SetBlendFactor(Index : byte; Value : double);
    procedure SetMethod(Value : TmcmImageMathematics);
    procedure SetSourceImage(Value : TmcmImage);
  public
    { Public declarations }
    property AddImage : TmcmImage
      write  SetSourceImage;
    property Method : TmcmImageMathematics
      read   GetMethod
      write  SetMethod;
    property Source[Index : byte] : TmcmImage
      read   GetSourceImage;
    property BlendFactor[Index : byte] : double
      read   GetBlendFactor
      write  SetBlendFactor;
  end;

var FormMath : TFormMath;

implementation

{$R *.DFM}

uses uChildWin;

procedure TFormMath.FormCreate(Sender : TObject);
begin
  FImageMath := TmcmImageMath.Create(Self);

  lBlendFactor1.Enabled  := False;
  rsBlendFactor1.Enabled := False;
  lBlendFactor2.Enabled  := False;
  rsBlendFactor2.Enabled := False;
  
  cbMethod.Items.AddObject('Select Method', Nil);
  cbMethod.Items.AddObject('Add',         Pointer(IM_ADD));
  cbMethod.Items.AddObject('And',         Pointer(IM_AND));
  cbMethod.Items.AddObject('Average',     Pointer(IM_AVE));
  cbMethod.Items.AddObject('Bitwise And', Pointer(IM_ANDBW));
  cbMethod.Items.AddObject('Bitwise Or',  Pointer(IM_ORBW));
  cbMethod.Items.AddObject('Bitwise Xor', Pointer(IM_XORBW));
  cbMethod.Items.AddObject('Blend',       Pointer(IM_BLEND));
  cbMethod.Items.AddObject('Difference',  Pointer(IM_DIFF));
  cbMethod.Items.AddObject('Divide',      Pointer(IM_DIV));
  cbMethod.Items.AddObject('Equal',       Pointer(IM_EQU));
  cbMethod.Items.AddObject('Greater-Than',Pointer(IM_GT));
  cbMethod.Items.AddObject('Magnitude',   Pointer(IM_MAG));
  cbMethod.Items.AddObject('Multiply',    Pointer(IM_MUL));
  cbMethod.Items.AddObject('Orientation', Pointer(IM_ORI));
  cbMethod.Items.AddObject('Subtract',    Pointer(IM_SUB));
  cbMethod.ItemIndex := 0;

  cbImage1.Clear;
  cbImage2.Clear;
end; // TFormMath.FormCreate.


procedure TFormMath.FormDestroy(Sender : TObject);
begin
  FImageMath.Free;
end; // TFormMath.FormDestroy.


procedure TFormMath.FormActivate(Sender : TObject);
begin
  cbImage1.ItemIndex := 0;
  cbImage1Change(Self);
end; // TFormMath.FormActivate.


procedure TFormMath.cbImage1Change(Sender : TObject);
var xScale      : double;
    yScale      : double;
    SelImage    : TmcmImage;
    S2Image     : TmcmImage;
    i           : integer;
begin
  if (cbImage1.ItemIndex >= 0) and (cbImage1.Items.Objects[cbImage1.ItemIndex] <> Nil)
  then begin
       // Get Source image 1, and down-scale this to thumb size.
       SelImage := TmcmImage(cbImage1.Items.Objects[cbImage1.ItemIndex]);
       xScale := (icSource1.Width - 4) / SelImage.Width;
       yScale := (icSource1.Height - 4) / SelImage.Height;
       if (xScale > yScale)
       then xScale := yScale;
       if (xScale > 1.0)
       then begin
            xScale := 1.0;
            icSource1.Image.Height := SelImage.Height;
            icSource1.Image.Width  := SelImage.Width;
       end
       else begin
            icSource1.Image.Height := Trunc(SelImage.Height * xScale);
            icSource1.Image.Width  := Trunc(SelImage.Width * xScale);
       end;
       icSource1.Image.ImageFormat := SelImage.ImageFormat;
       icSource1.Image.Palette := SelImage.Palette;

       SelImage.Draw(icSource1.Image.Canvas.Handle, xScale);
       icSource1.Invalidate;

       // Fill Source 2 listbox with physicaly matching images.
       cbImage2.Clear;
       for i := 0 to (cbImage1.Items.Count - 1)
       do begin
          if (SelImage <> cbImage1.Items.Objects[i])
          then begin
               S2Image := TmcmImage(cbImage1.Items.Objects[i]);
               if (SelImage.Height      = S2Image.Height) and
                  (SelImage.Width       = S2Image.Width) and
                  (SelImage.ImageFormat = S2Image.ImageFormat)
               then cbImage2.Items.AddObject(ExtractFileName(S2Image.ImageInfo.FileName), S2Image);
          end;
       end;
       cbImage2Change(Sender);
  end
  else begin
       icSource1.Clear;
       icSource1.Invalidate;
  end;
end; // TFormMath.cbImage1Change.


procedure TFormMath.cbImage2Change(Sender : TObject);
var xScale      : double;
    yScale      : double;
    SelImage    : TmcmImage;
begin
  if (cbImage2.ItemIndex >= 0) and (cbImage2.Items.Objects[cbImage2.ItemIndex] <> Nil)
  then begin
       // Get Source image 1, and down-scale this to thumb size.
       SelImage := TmcmImage(cbImage2.Items.Objects[cbImage2.ItemIndex]);
       xScale := (icSource2.Width - 4) / SelImage.Width;
       yScale := (icSource2.Height - 4) / SelImage.Height;
       if (xScale > yScale)
       then xScale := yScale;
       if (xScale > 1.0)
       then begin
            xScale := 1.0;
            icSource2.Image.Height := SelImage.Height;
            icSource2.Image.Width  := SelImage.Width;
       end
       else begin
            icSource2.Image.Height := Trunc(SelImage.Height * xScale);
            icSource2.Image.Width  := Trunc(SelImage.Width * xScale);
       end;
       icSource2.Image.ImageFormat := SelImage.ImageFormat;
       icSource2.Image.Palette := SelImage.Palette;

       SelImage.Draw(icSource2.Image.Canvas.Handle, xScale);
       icSource2.Invalidate;

       cbMethodChange(Sender);
  end
  else begin
       icSource2.Clear;
       icSource2.Invalidate;
  end;
end; // TFormMath.cbImage2Change.


procedure TFormMath.cbMethodChange(Sender : TObject);
begin
  if (cbMethod.ItemIndex > 0)
  then begin
       FMethod := TmcmImageMathematics(cbMethod.Items.Objects[cbMethod.ItemIndex]);

       rsBlendFactor1.MaxValue := 255;
       rsBlendFactor2.MaxValue := 255;
       case FMethod of
       IM_BLEND : begin
                    gbSource2.Enabled := True;
                    lBlendFactor1.Enabled := True;
                    lBlendFactor2.Enabled := True;
                    rsBlendFactor1.MaxValue := 100;
                    rsBlendFactor2.MaxValue := 100;
                    rsBlendFactor1.Increment := 1.0;
                  end;
       IM_DIV   : begin
                    gbSource2.Enabled := False;
                    lBlendFactor1.Enabled := True;
                    lBlendFactor2.Enabled := False;
                    rsBlendFactor1.Increment := 0.1;
                  end;
       IM_MUL   : begin
                    gbSource2.Enabled := False;
                    lBlendFactor1.Enabled := True;
                    lBlendFactor2.Enabled := False;
                    rsBlendFactor1.Increment := 0.1;
                  end;
       else begin
            rsBlendFactor1.Increment := 1.0;
            gbSource2.Enabled := True;
            lBlendFactor1.Enabled := False;
            lBlendFactor2.Enabled := False;
       end;
       end;
       cbImage2.Enabled := gbSource2.Enabled;
       rsBlendFactor1.Enabled := lBlendFactor1.Enabled;
       rsBlendFactor2.Enabled := lBlendFactor2.Enabled;


       FImageMath.SourceImage[0] := icSource1.Image;
       FImageMath.SourceImage[1] := icSource2.Image;
       icResult.Image.Width  := icSource1.Image.Width;
       icResult.Image.Height := icSource1.Image.Height;
       icResult.Image.ImageFormat := icSource1.Image.ImageFormat;
       icResult.Image.Palette := icSource1.Image.Palette;

       FImageMath.Scale[0] := rsBlendFactor1.Value;
       FImageMath.Scale[1] := rsBlendFactor2.Value;
       FImageMath.ResultImage := icResult.Image;
       FImageMath.Execute(FMethod);

       icResult.Invalidate;
  end
  else icResult.Clear;
end; // TFormMath.cbMethodChange.


procedure TFormMath.rsBlendFactorChange(Sender : TObject);
begin
  if Assigned(cbMethod)
  then cbMethodChange(Sender);
end; // TFormMath.rsBlendFactorChange.


function TFormMath.GetBlendFactor(Index : byte) : double;
begin
  case Index of
  0 : Result := rsBlendFactor1.Value;
  1 : Result := rsBlendFactor2.Value;
  else Result := 0.0;
  end;
end; // TFormMath.GetBlendFactor.


procedure TFormMath.SetBlendFactor(Index : byte; Value : double);
begin
  case Index of
  0 : rsBlendFactor1.Value := Value * 100.0;
  1 : rsBlendFactor2.Value := Value * 100.0;
  end;
end; // TFormMath.SetBlendFactor.


function TFormMath.GetMethod : TmcmImageMathematics;
begin
  Result := FMethod;
end; // TFormMath.GetMethod.


procedure TFormMath.SetMethod(Value : TmcmImageMathematics);
begin
  FMethod := Value;
end; // TFormMath.SetMethod.


function TFormMath.GetSourceImage(Index : byte) : TmcmImage;
begin
  case Index of
  0 : if (cbImage1.ItemIndex >= 0)
      then Result := TmcmImage(cbImage1.Items.Objects[cbImage1.ItemIndex])
      else Result := Nil;
  1 : if (cbImage2.ItemIndex >= 0)
      then Result := TmcmImage(cbImage2.Items.Objects[cbImage2.ItemIndex])
      else Result := Nil;
  else Result := Nil;
  end;
end; // TFormMath.GetSourceImage.


procedure TFormMath.SetSourceImage(Value : TmcmImage);
begin
  cbImage1.Items.AddObject(ExtractFileName(Value.ImageInfo.FileName), Value);
end; // TFormMath.SetSourceImage.


end.
