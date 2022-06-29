unit sThirdParty;
{$I sDefs.inc}

interface

uses
  Messages, SysUtils, Classes, Windows, Graphics, Controls, Forms, Dialogs, ActnList, imglist,
    comctrls, StdCtrls, sCommonData, sConst, sBitBtn, sSpeedButton{$IFNDEF ALITE}, sToolBar{$ENDIF}
    {$IFDEF USEPNG}, PngImageList, PngFunctions, PngImage{$ENDIF};

function GetImageCount(ImgList : TCustomImageList) : integer;
procedure CopyGlyph(Control : TControl; SkinData : TsCommonData; FImageIndex : integer; Images : TCustomImageList; Glyph : TBitmap; DisabledGlyphKind : TsDisabledGlyphKind; var NumGlyphs : integer); // Preparing of the glyph for standard mode from Images
procedure DrawBitBtnGlyph(Button : TsBitBtn);
procedure DrawSpeedButtonGlyph(Button : TsSpeedButton);
procedure DoActionChanging(Button : TsBitBtn; Action : TCustomAction); overload;
procedure DoActionChanging(Button : TsSpeedButton; Action : TCustomAction); overload;
{$IFNDEF ALITE}
procedure CopyToolBtnGlyph(ToolBar : TsToolBar; Button : TToolButton; State: TCustomDrawState; Stage: TCustomDrawStage; var Flags: TTBCustomDrawFlags; BtnBmp : TBitmap);
{$ENDIF}

implementation

uses sGraphUtils;

function GetImageCount(ImgList : TCustomImageList) : integer;
begin
  Result := 0;
  if ImgList = nil then Exit;
{$IFDEF USEPNG}
  if ImgList is TPngImageList then begin
    Result := TPngImageList(ImgList).PngImages.Count;
  end
  else
{$ENDIF}
  Result := ImgList.Count;
end;

procedure CopyGlyph(Control : TControl; SkinData : TsCommonData; FImageIndex : integer; Images : TCustomImageList; Glyph : TBitmap; DisabledGlyphKind : TsDisabledGlyphKind; var NumGlyphs : integer); // Preparing of the glyph for standard mode from Images
var
  Bmp : TBitmap;
  MaskColor : TsColor;
  pf : TPixelFormat;
{$IFDEF USEPNG}
  R : TRect;
  PngCopy: TPNGObject;
{$ENDIF}
begin
  // Copying a picture from Images to Glyph for standard mode
  if ((csLoading in Control.ComponentState) and (csDesigning in Control.ComponentState)) or SkinData.Skinned or (FImageIndex < 0) then Exit;
{$IFDEF USEPNG}
  if Images is TPngImageList then begin
    if (GetImageCount(Images) > FImageIndex) then begin
      PngCopy := TPngImageCollectionItem(TPngImageList(Images).PngImages.Items[FImageIndex]).PngImage;
      Glyph.Width := PngCopy.Width * 2;
      Glyph.Height := PngCopy.Height;
      Glyph.Canvas.Brush.Color := clBtnFace;
      R := Rect(0, 0, Glyph.Width, PngCopy.Height);
      Glyph.Canvas.FillRect(R);
      R.Right := PngCopy.Width;
      PngCopy.Draw(Glyph.Canvas, R);

      // Disabled piece drawing
      R.Left := R.Right;
      R.Right := Glyph.Width;

      PngCopy := nil;
      if dgBlended in DisabledGlyphKind then begin
        PngCopy := TPNGObject.Create;
        PngCopy.Assign(TPngImageCollectionItem(TPngImageList(Images).PngImages.Items[FImageIndex]).PngImage);
        MakeImageBlended(PngCopy);
      end;
      if dgGrayed in DisabledGlyphKind then begin
        if PngCopy = nil then begin
          PngCopy := TPNGObject.Create;
          PngCopy.Assign(TPngImageCollectionItem(TPngImageList(Images).PngImages.Items[FImageIndex]).PngImage);
        end;
        MakeImageGrayscale(PngCopy);
      end;
      if PngCopy = nil then begin
        PngCopy := TPngImageCollectionItem(TPngImageList(Images).PngImages.Items[FImageIndex]).PngImage;
        PngCopy.Draw(Glyph.Canvas, R);
      end
      else begin
        PngCopy.Draw(Glyph.Canvas, R);
        FreeAndNil(PngCopy);
      end;

      NumGlyphs := 2;
    end;
  end
  else
{$ENDIF}
  if Images.Count > FImageIndex then begin
    Glyph.Assign(nil);
    CopyImage(Glyph, Images, FImageIndex);

    if DisabledGlyphKind <> [] then begin // If changes are needed
      Bmp := TBitmap.Create;
      Bmp.Assign(Glyph);
      Bmp.PixelFormat := pf24bit;
      pf := Glyph.PixelFormat; // v4.23
      Glyph.PixelFormat := pf24bit;
      if dgGrayed in DisabledGlyphKind then begin
        if Images.BkColor = clNone then MaskColor.C := clFuchsia else MaskColor.C := Images.BkColor;//TsColor(Bmp.Canvas.Pixels[0, Bmp.Height - 1]);
        GrayScaleTrans(Bmp, MaskColor);
      end;
      Glyph.Width := Images.Width * 2;
      Glyph.Canvas.Brush.Color := clBtnFace;
//      if Images.BkColor = clNone then Glyph.Canvas.Brush.Color := clFuchsia else Glyph.Canvas.Brush.Color := Images.BkColor;//TsColor(Bmp.Canvas.Pixels[0, Bmp.Height - 1]);
      Glyph.Canvas.FillRect(Rect(Images.Width, 0, Glyph.Width, Glyph.Height));

      if dgBlended in DisabledGlyphKind then begin
//        MaskColor := TsColor(Bmp.Canvas.Pixels[0, Bmp.Height - 1]);
        if Images.BkColor = clNone then MaskColor.C := clFuchsia else MaskColor.C := Images.BkColor;//TsColor(Bmp.Canvas.Pixels[0, Bmp.Height - 1]);
        BlendTransRectangle(Glyph, Images.Width, 0, Bmp,
                              Rect(0, 0, Bmp.Width, Bmp.Height),
                              0.5, MaskColor);
      end
      else begin
        if Images.BkColor = clNone then MaskColor.C := clFuchsia else MaskColor.C := Images.BkColor;//TsColor(Bmp.Canvas.Pixels[0, Bmp.Height - 1]);
//        MaskColor := TsColor(Bmp.Canvas.Pixels[Images.Width, Bmp.Height - 1]);
        CopyTransBitmaps(Glyph, Bmp, Images.Width, 0, MaskColor);

        Glyph.Canvas.Brush.Color := clYellow;
        Glyph.Canvas.Pixels[Images.Width, Glyph.Height - 1] := clFuchsia
      end;

      Glyph.PixelFormat := pf; //v4.23
      Glyph.TransparentColor := MaskColor.C;
      Glyph.Transparent := True;
      FreeAndNil(Bmp);
    end else begin
      Glyph.Width := Images.Width * 2;
      BitBlt(Glyph.Canvas.Handle, Images.Width, 0, Glyph.Width, Glyph.Height, Glyph.Canvas.Handle, 0, 0, SRCCOPY);
    end;
    NumGlyphs := 2;
  end;
end;

procedure DrawBitBtnGlyph(Button : TsBitBtn);
var
  IRect : TRect;
  Bmp : TBitmap;
  MaskColor: TsColor;
{$IFDEF USEPNG}
  PngCopy: TPNGObject;
{$ENDIF}
  procedure PrepareGlyph; begin
    with Button do begin
      Bmp.Width := Images.Width;
      Bmp.Height := Images.Height;
      Bmp.PixelFormat := pf24bit;
      if Button.Images.BkColor <> clNone then MaskColor.C := Button.Images.BkColor else MaskColor.C := clFuchsia;
      Bmp.Canvas.Brush.Color := MaskColor.C;
      Bmp.Canvas.FillRect(Rect(0, 0, Bmp.Width, Bmp.Height));
      Images.GetBitmap(ImageIndex, Bmp);
    end;
  end;
begin
  with Button do begin
    if (Glyph <> nil) and (Glyph.Width > 0) then begin
      sGraphUtils.DrawGlyphEx(Glyph, SkinData.FCacheBmp, ImgRect, NumGlyphs, Enabled, Grayed, DisabledGlyphKind, integer(ControlIsActive(SkinData)), Blend);
    end
    else if Assigned(Images) and (ImageIndex > -1) and (GetImageCount(Images) > ImageIndex) then begin
      IRect := ImgRect;
  {$IFDEF USEPNG}
      if Images is TPngImageList then begin
        PngCopy := nil;
        if Enabled then begin
          if ControlIsActive(SkinData) or ((Blend = 0) and not Grayed) then begin
            PngCopy := TPngImageCollectionItem(TPngImageList(Images).PngImages.Items[ImageIndex]).PngImage;
            PngCopy.Draw(SkinData.FCacheBmp.Canvas, IRect);
          end
          else begin
            if Blend > 0 then begin
              PngCopy := TPNGObject.Create;
              PngCopy.Assign(TPngImageCollectionItem(TPngImageList(Images).PngImages.Items[ImageIndex]).PngImage);
              MakeImageBlended(PngCopy);
            end;
            if Grayed then begin
              if PngCopy = nil then begin
                PngCopy := TPNGObject.Create;
                PngCopy.Assign(TPngImageCollectionItem(TPngImageList(Images).PngImages.Items[ImageIndex]).PngImage);
              end;
              MakeImageGrayscale(PngCopy);
            end;
            PngCopy.Draw(SkinData.FCacheBmp.Canvas, IRect);
            FreeAndNil(PngCopy);
          end;
        end
        else begin
          if dgBlended in DisabledGlyphKind then begin
            PngCopy := TPNGObject.Create;
            PngCopy.Assign(TPngImageCollectionItem(TPngImageList(Images).PngImages.Items[ImageIndex]).PngImage);
            MakeImageBlended(PngCopy);
          end;
          if dgGrayed in DisabledGlyphKind then begin
            if PngCopy = nil then begin
              PngCopy := TPNGObject.Create;
              PngCopy.Assign(TPngImageCollectionItem(TPngImageList(Images).PngImages.Items[ImageIndex]).PngImage);
            end;
            MakeImageGrayscale(PngCopy);
          end;
          if PngCopy = nil then begin
            PngCopy := TPngImageCollectionItem(TPngImageList(Images).PngImages.Items[ImageIndex]).PngImage;
            PngCopy.Draw(SkinData.FCacheBmp.Canvas, IRect);
          end
          else begin
            PngCopy.Draw(SkinData.FCacheBmp.Canvas, IRect);
            FreeAndNil(PngCopy);
          end;
        end;
      end
      else
  {$ENDIF}
      begin
        Bmp := TBitmap.Create;
        try
          PrepareGlyph;
          if not Enabled then begin
            if dgGrayed in DisabledGlyphKind then begin
              GrayScale(Bmp);
            end;
            if dgBlended in DisabledGlyphKind then begin
//              if Button.Images.BkColor <> clNone then MaskColor.C := Button.Images.BkColor else MaskColor.C := clFuchsia;
//              MaskColor := TsColor(Bmp.Canvas.Pixels[0, Bmp.Height - 1]);
              BlendTransRectangle(SkinData.FCacheBmp, IRect.Left, IRect.Top, Bmp,
                                    Rect(0,
                                         0,
                                         Bmp.Width,
                                         Bmp.Height),
                                    0.5, MaskColor);
            end
            else begin
//              if Button.Images.BkColor <> clNone then MaskColor.C := Button.Images.BkColor else MaskColor.C := clFuchsia;
//              MaskColor := TsColor(Bmp.Canvas.Pixels[0, Bmp.Height - 1]);
              CopyTransBitmaps(SkinData.FCacheBmp, Bmp, IRect.Left, IRect.Top, MaskColor);
            end;
          end
          else begin
            if not ControlIsActive(SkinData) and Grayed then begin
              GrayScale(Bmp);
            end;
//            if Button.Images.BkColor <> clNone then MaskColor.C := Button.Images.BkColor else MaskColor.C := clFuchsia;
//            MaskColor := TsColor(Bmp.Canvas.Pixels[0, Bmp.Height - 1]);

            if not ControlIsActive(SkinData) and (Blend > 0) then begin
              BlendTransRectangle(SkinData.FCacheBmp, IRect.Left, IRect.Top, Bmp,
                                  Rect(0,
                                       0,
                                       Bmp.Width,
                                       Bmp.Height),
                                  Blend / 100, MaskColor);
            end
            else begin
              CopyTransBitmaps(SkinData.FCacheBmp, Bmp, IRect.Left, IRect.Top, MaskColor);
            end;
          end;
        finally
          FreeAndNil(Bmp);
        end;
      end;
    end;
  end;
end;

procedure DrawSpeedButtonGlyph(Button : TsSpeedButton);
var
  IRect : TRect;
  Bmp : TBitmap;
  MaskColor: TsColor;
{$IFDEF USEPNG}
  PngCopy: TPNGObject;
{$ENDIF}
  procedure PrepareGlyph; begin
    with Button do begin
      Bmp.Width := Images.Width;
      Bmp.Height := Images.Height;
      Bmp.PixelFormat := pf24bit;
      if Button.Images.BkColor <> clNone then MaskColor.C := Button.Images.BkColor else MaskColor.C := clFuchsia;
      Bmp.Canvas.Brush.Color := MaskColor.C;
      Bmp.Canvas.FillRect(Rect(0, 0, Bmp.Width, Bmp.Height));
      Images.GetBitmap(ImageIndex, Bmp);
    end;
  end;
begin
  with Button do begin
    if (Glyph <> nil) and (Glyph.Width > 0) then begin
      sGraphUtils.DrawGlyphEx(Glyph, SkinData.FCacheBmp, ImgRect, NumGlyphs, Enabled, Grayed, DisabledGlyphKind, integer(ControlIsActive(SkinData)), Blend);
    end
    else if Assigned(Images) and (ImageIndex > -1) and (GetImageCount(Images) > ImageIndex) then begin
      IRect := ImgRect;
  {$IFDEF USEPNG}
      if Images is TPngImageList then begin
        PngCopy := nil;
        if Enabled then begin
          if ControlIsActive(SkinData) or ((Blend = 0) and not Grayed) then begin
            PngCopy := TPngImageCollectionItem(TPngImageList(Images).PngImages.Items[ImageIndex]).PngImage;
            PngCopy.Draw(SkinData.FCacheBmp.Canvas, IRect);
          end
          else begin
            if Blend > 0 then begin
              PngCopy := TPNGObject.Create;
              PngCopy.Assign(TPngImageCollectionItem(TPngImageList(Images).PngImages.Items[ImageIndex]).PngImage);
              MakeImageBlended(PngCopy);
            end;
            if Grayed then begin
              if PngCopy = nil then begin
                PngCopy := TPNGObject.Create;
                PngCopy.Assign(TPngImageCollectionItem(TPngImageList(Images).PngImages.Items[ImageIndex]).PngImage);
              end;
              MakeImageGrayscale(PngCopy);
            end;
            PngCopy.Draw(SkinData.FCacheBmp.Canvas, IRect);
            FreeAndNil(PngCopy);
          end;
        end
        else begin
          if dgBlended in DisabledGlyphKind then begin
            PngCopy := TPNGObject.Create;
            PngCopy.Assign(TPngImageCollectionItem(TPngImageList(Images).PngImages.Items[ImageIndex]).PngImage);
            MakeImageBlended(PngCopy);
          end;
          if dgGrayed in DisabledGlyphKind then begin
            if PngCopy = nil then begin
              PngCopy := TPNGObject.Create;
              PngCopy.Assign(TPngImageCollectionItem(TPngImageList(Images).PngImages.Items[ImageIndex]).PngImage);
            end;
            MakeImageGrayscale(PngCopy);
          end;
          if PngCopy = nil then begin
            PngCopy := TPngImageCollectionItem(TPngImageList(Images).PngImages.Items[ImageIndex]).PngImage;
            PngCopy.Draw(SkinData.FCacheBmp.Canvas, IRect);
          end
          else begin
            PngCopy.Draw(SkinData.FCacheBmp.Canvas, IRect);
            FreeAndNil(PngCopy);
          end;
        end;
      end
      else
  {$ENDIF}
      begin
        Bmp := TBitmap.Create;
        try
          PrepareGlyph;
          if not Enabled then begin
            if dgGrayed in DisabledGlyphKind then begin
              GrayScale(Bmp);
            end;
            if dgBlended in DisabledGlyphKind then begin
//              MaskColor := TsColor(Bmp.Canvas.Pixels[0, Bmp.Height - 1]);
              BlendTransRectangle(SkinData.FCacheBmp, IRect.Left, IRect.Top, Bmp,
                                    Rect(0, 0, Bmp.Width, Bmp.Height),
                                    0.5, MaskColor);
            end
            else begin
//              MaskColor := TsColor(Bmp.Canvas.Pixels[0, Bmp.Height - 1]);
              CopyTransBitmaps(SkinData.FCacheBmp, Bmp, IRect.Left, IRect.Top, MaskColor);
            end;
          end
          else begin
            if not ControlIsActive(SkinData) and Grayed then begin
              GrayScale(Bmp);
            end;
//            MaskColor := TsColor(Bmp.Canvas.Pixels[0, Bmp.Height - 1]);

            if not ControlIsActive(SkinData) and (Blend > 0) then begin
              BlendTransRectangle(SkinData.FCacheBmp, IRect.Left, IRect.Top, Bmp,
                                  Rect(0,
                                       0,
                                       Bmp.Width,
                                       Bmp.Height),
                                  Blend / 100, MaskColor);
            end
            else begin
              CopyTransBitmaps(SkinData.FCacheBmp, Bmp, IRect.Left, IRect.Top, MaskColor);
            end;
          end;
        finally
          FreeAndNil(Bmp);
        end;
      end;
    end;
  end;
end;

{$IFNDEF ALITE}
procedure CopyToolBtnGlyph(ToolBar : TsToolBar; Button : TToolButton; State: TCustomDrawState; Stage: TCustomDrawStage; var Flags: TTBCustomDrawFlags; BtnBmp : TBitmap);
var
  IRect : TRect;
  Mode : integer;
  Bmp : TBitmap;
  MaskColor : TsColor;
{$IFDEF USEPNG}
  PngCopy: TPNGObject;
{$ENDIF}
  function AddedWidth : integer; begin
    Result := integer(Button.Style = tbsDropDown) * 8;
  end;
  function ImgRect : TRect;
  begin
    with ToolBar do begin
      if not List then begin
        Result.Left := (Button.Width - Images.Width) div 2 + 1 - AddedWidth;
        Result.Top := (Button.Height - Images.Height - integer(ShowCaptions) * (SkinData.FCacheBMP.Canvas.TextHeight('A') + 3)) div 2;
        Result.Right := Result.Left + Images.Width;
        Result.Bottom := Result.Bottom + Images.Height;
      end
      else begin
        Result.Left := 5;
        Result.Top := (Button.Height - Images.Height) div 2;
        Result.Right := Result.Left + Images.Width;
        Result.Bottom := Result.Bottom + Images.Height;
      end;
      if (Mode = 2) //or (cdsChecked in State)
        then OffsetRect(Result, 1, 1);
    end;
  end;
    procedure PrepareGlyph;
      function Imges : TCustomImageList; begin
        with ToolBar do
        if (Mode <> 0) and Assigned(HotImages) and (Button.ImageIndex < HotImages.Count) then begin
          Result := HotImages;
        end
        else Result := Images;
      end;
    begin
      Bmp.Width := Imges.Width;
      Bmp.Height := Imges.Height;
      Bmp.PixelFormat := pf24bit;
      if ToolBar.Images.BkColor <> clNone then MaskColor.C := ToolBar.Images.BkColor else MaskColor.C := clFuchsia;
      Bmp.Canvas.Brush.Color := MaskColor.C;
      Bmp.Canvas.FillRect(Rect(0, 0, Bmp.Width, Bmp.Height));
      Imges.GetBitmap(Button.ImageIndex, Bmp);
    end;
begin
  with ToolBar do begin
    if (State = []) or (State = [cdsDisabled]) then Mode := 0 else if cdsSelected in State
      then Mode := 2
      else Mode := 1;
    IRect := ImgRect;
{$IFDEF USEPNG}
    if Images is TPngImageList then begin
      PngCopy := nil;
      if Enabled then begin
        PngCopy := TPngImageCollectionItem(TPngImageList(Images).PngImages.Items[Button.ImageIndex]).PngImage;
        PngCopy.Draw(BtnBmp.Canvas, IRect);
      end
      else begin
        PngCopy := TPNGObject.Create;
        PngCopy.Assign(TPngImageCollectionItem(TPngImageList(Images).PngImages.Items[Button.ImageIndex]).PngImage);
        MakeImageBlended(PngCopy);
        if PngCopy = nil then begin
          PngCopy := TPngImageCollectionItem(TPngImageList(Images).PngImages.Items[Button.ImageIndex]).PngImage;
          PngCopy.Draw(BtnBmp.Canvas, IRect);
        end
        else begin
          PngCopy.Draw(BtnBmp.Canvas, IRect);
          FreeAndNil(PngCopy);
        end;
      end;
    end
    else
{$ENDIF}
    begin
      Bmp := TBitmap.Create;
      try
        PrepareGlyph;
        CopyTransBitmaps(BtnBmp, Bmp, ImgRect.left, ImgRect.Top{IRect.Left + integer(Mode = 2), IRect.Top + integer(Mode = 2)}, MaskColor);
      finally
        FreeAndNil(Bmp);
      end;
    end;
  end;
end;
{$ENDIF}

procedure DoActionChanging(Button : TsBitBtn; Action : TCustomAction); 
begin
{$IFDEF USEPNG}
  if (Action.ActionList <> nil) and (Action.ActionList.Images <> nil) and (Action.ActionList.Images is TPNGImageList) and
       (Action.ImageIndex >= 0) {and (ImageIndex < TCustomAction(Sender).ActionList.Images.Count)} then begin
    Button.Glyph.Assign(nil);
    Button.Images := Action.ActionList.Images;
    Button.ImageIndex := Action.ImageIndex;
  end;
{$ENDIF}
end;

procedure DoActionChanging(Button : TsSpeedButton; Action : TCustomAction); overload;
begin
{$IFDEF USEPNG}
  if (Action.ActionList <> nil) and (Action.ActionList.Images <> nil) and (Action.ActionList.Images is TPNGImageList) and
       (Action.ImageIndex >= 0) {and (ImageIndex < TCustomAction(Sender).ActionList.Images.Count)} then begin
    Button.Glyph.Assign(nil);
    Button.Images := Action.ActionList.Images;
    Button.ImageIndex := Action.ImageIndex;
  end;
{$ENDIF}
end;

end.
