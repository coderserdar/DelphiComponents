unit ATxImageProc;

interface

uses
  Windows, Graphics, ExtCtrls;

type
  TATImageEffect = (
    aieCorrectOnly,
    aieRotate90,
    aieRotate270,
    aieGrayscale,
    aieNegative,
    aieFlipVertical,
    aieFlipHorizontal
    );

function PictureEffect(
  APicture: TPicture;
  AEffect: TATImageEffect;
  ABackColor: TColor): Boolean;

procedure FixIcon(AIcon: TIcon);


implementation

uses
  SysUtils, Classes, Jpeg, ATxBitmapProc;

//------------------------------------------------------------------------------
{
Need to "fix" icon sizes. Icon should be drawn once before its sizes are to be read:
http://qc.codegear.com/wc/qcmain.aspx?d=6018
}
procedure FixIcon(AIcon: TIcon);
var
  Bmp: TBitmap;
begin
  try
    Bmp := TBitmap.Create;
    try
      Bmp.PixelFormat := pf24bit;
      Bmp.Canvas.Draw(0, 0, AIcon);
    finally
      Bmp.Free;
    end;
  except
  end;
end;

//------------------------------------------------------------------------------
//From VirtualShellTools, added BackColor parameter by AT
procedure SpStretchDraw(G: TGraphic; OutBitmap: TBitmap; DestR: TRect; UseSubsampling: Boolean; BackColor: TColor);
// Canvas.StretchDraw is NOT THREADSAFE!!!
// Use StretchBlt instead, we have to use a worker bitmap to do so
var
  Work: TBitmap;
begin
  Work := TBitmap.Create;
  Work.Canvas.Lock;
  try
    // Paint the Picture in Work
    if (G is TJpegImage) or (G is TBitmap) then
      Work.Assign(G) //assign works in this case
    else begin
      Work.Width := G.Width;
      Work.Height := G.Height;

      Work.Canvas.Brush.Color := BackColor;
      Work.Canvas.FillRect(Rect(0, 0, Work.Width, Work.Height));

      Work.Canvas.Draw(0, 0, G);
    end;

    if UseSubsampling then
    begin
      SetStretchBltMode(OutBitmap.Canvas.Handle, STRETCH_HALFTONE);
      SetBrushOrgEx(OutBitmap.Canvas.Handle, 0, 0, nil);
    end
    else
    begin
      SetStretchBltMode(OutBitmap.Canvas.Handle, STRETCH_DELETESCANS);
    end;

    StretchBlt(OutBitmap.Canvas.Handle,
      DestR.Left, DestR.Top, DestR.Right - DestR.Left, DestR.Bottom - DestR.Top,
      Work.Canvas.Handle, 0, 0, G.Width, G.Height, SRCCopy);
  finally
    Work.Canvas.Unlock;
    Work.Free;
  end;
end;

//------------------------------------------------------------------------------
function CorrectPictureToBitmap(APicture: TPicture; AOutBmp: TBitmap; ABackColor: TColor): Boolean;
var
  DestR: TRect;
begin
  Result := True;

  Assert(
    Assigned(AOutBmp),
    'CorrectImageToBitmap: bitmap not assigned.');

  with APicture do
    try
      AOutBmp.PixelFormat := pf24Bit;
      DestR := Rect(0, 0, 0, 0);
      DestR.Right := Graphic.Width;
      DestR.Bottom := Graphic.Height;
      AOutBmp.Width := DestR.Right;
      AOutBmp.Height := DestR.Bottom;
      AOutBmp.Canvas.Brush.Color := ABackColor;
      AOutBmp.Canvas.FillRect(DestR);
      SpStretchDraw(Graphic, AOutBmp, DestR, False, ABackColor);
    except
      Result := False;
    end;

  Assert(
    (AOutBmp.Width > 0) and (AOutBmp.Height > 0),
    'CorrectImageToBitmap: result bitmap is empty.');
end;


//------------------------------------------------------------------------------
function CorrectPicture(APicture: TPicture; ABackColor: TColor): Boolean;
var
  bmp: TBitmap;
begin
  Result := True;
  with APicture do
    if (not (Graphic is TBitmap)) or ((Graphic is TBitmap) and (TBitmap(Graphic).PixelFormat<>pf24Bit)) then
      try
        bmp := TBitmap.Create;
        try
          Result := CorrectPictureToBitmap(APicture, bmp, ABackColor);
          Graphic := bmp;
        finally
          bmp.Free;
        end;
      except
        Result := False;
      end;
end;


//------------------------------------------------------------------------------
function FlipBitmap(ABitmap: TBitmap; AVerticalFlip: Boolean): TBitmap;
var
  ResWidth, ResHeight,
  ResX, ResY: Integer;
begin
  Result:= TBitmap.Create;
  try
    Result.PixelFormat := ABitmap.PixelFormat;
    Result.Width := ABitmap.Width;
    Result.Height := ABitmap.Height;

    if AVerticalFlip then
      begin
      ResWidth := ABitmap.Width;
      ResHeight := -ABitmap.Height;
      ResX := 0;
      ResY := ABitmap.Height - 1;
      end
    else
      begin
      ResWidth := -ABitmap.Width;
      ResHeight := ABitmap.Height;
      ResX := ABitmap.Width - 1;
      ResY := 0;
      end;

    StretchBlt(
      Result.Canvas.Handle,
      ResX, ResY, ResWidth, ResHeight,
      ABitmap.Canvas.Handle,
      0, 0, ABitmap.Width, ABitmap.Height,
      SRCCOPY);
    GDIFlush;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

//------------------------------------------------------------------------------
function PictureEffect(
  APicture: TPicture;
  AEffect: TATImageEffect;
  ABackColor: TColor): Boolean;
var
  bmp: TBitmap;
begin
  Result := CorrectPicture(APicture, ABackColor);
  if Result then
    if AEffect <> aieCorrectOnly then
    begin
      bmp := nil;
      try
        case AEffect of
          aieRotate90: bmp := RotateBitmap90(APicture.Bitmap);
          aieRotate270: bmp := RotateBitmap270(APicture.Bitmap);
          aieGrayscale: bmp := ConvertToGrayscale(APicture.Bitmap);
          aieNegative: bmp := ConvertToNegative(APicture.Bitmap);
          aieFlipVertical: bmp := FlipBitmap(APicture.Bitmap, True);
          aieFlipHorizontal: bmp := FlipBitmap(APicture.Bitmap, False);
        end;
        APicture.Bitmap.FreeImage;
        APicture.Bitmap.Assign(bmp);
      finally
        if Assigned(bmp) then
          bmp.Free;
      end;
    end;
end;


end.
