{   Component(s):
    tcySpeedButton

    Description:
    Simple SpeedButton with color feature

    $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
    $  €€€ Accept any PAYPAL DONATION $$$  €
    $      to: mauricio_box@yahoo.com      €
    €€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€

    * ***** BEGIN LICENSE BLOCK *****
    *
    * Version: MPL 1.1
    *
    * The contents of this file are subject to the Mozilla Public License Version
    * 1.1 (the "License"); you may not use this file except in compliance with the
    * License. You may obtain a copy of the License at http://www.mozilla.org/MPL/
    *
    * Software distributed under the License is distributed on an "AS IS" basis,
    * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
    * the specific language governing rights and limitations under the License.
    *
    * The Initial Developer of the Original Code is Mauricio
    * (https://sourceforge.net/projects/tcycomponents/).
    *
    * Donations: see Donation section on Description.txt
    *
    * Alternatively, the contents of this file may be used under the terms of
    * either the GNU General Public License Version 2 or later (the "GPL"), or the
    * GNU Lesser General Public License Version 2.1 or later (the "LGPL"), in which
    * case the provisions of the GPL or the LGPL are applicable instead of those
    * above. If you wish to allow use of your version of this file only under the
    * terms of either the GPL or the LGPL, and not to allow others to use your
    * version of this file under the terms of the MPL, indicate your decision by
    * deleting the provisions above and replace them with the notice and other
    * provisions required by the LGPL or the GPL. If you do not delete the
    * provisions above, a recipient may use your version of this file under the
    * terms of any one of the MPL, the GPL or the LGPL.
    *
    * ***** END LICENSE BLOCK *****}

unit cySpeedButton;

{$I ..\Core\cyCompilerDefines.inc}

interface

uses Graphics, Classes, Windows, Themes, Controls, {$IFDEF UNICODE} pngimage, {$ENDIF}
      Buttons, Messages, VCL.cyTypes, VCL.cyClasses, VCL.cyGraphics, cyBaseSpeedButton;

type
  TcyCustomSpeedButton = class;

  TProcBeforePaint = procedure (Sender: TObject; var DrawBorders, DrawBackground, DrawWallPaper, DrawCaption, DrawGlyph: Boolean) of object;

  TcyCustomSpeedButton = class(TcyBaseSpeedButton)
  private
    bDrawBorders, bDrawBackground, bDrawWallpaper, bDrawCaption, bDrawGlyph: Boolean;
    FDegrade: TcyGradient;
    FHotFromColor: TColor;
    FHotToColor: TColor;
    FHotBalanceMode: TDgradBalanceMode;
    FDownFromColor: TColor;
    FDownToColor: TColor;
    FDisabledFromColor: TColor;
    FDisabledToColor: TColor;
    FMetroFrameColor: TColor;
    FImagelistOptions: TcyImagelistOptions;
    FWallpaper: TcyBgPicture;
    FBeforePaint: TProcBeforePaint;
    FMetroFrameWidth: Word;
    FFlatBorderColor: TColor;
    FDownFontColor: TColor;
    FHotFontColor: TColor;
    procedure SetDegrade(const Value: TcyGradient);
    procedure SetImagelistOptions(const Value: TcyImagelistOptions);
    procedure SetWallpaper(const Value: TcyBgPicture);
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
  protected
    procedure DrawButton(aState: TButtonState; Hot: Boolean); override;
    procedure DrawBackground(var Rect: TRect; aState: TButtonState; Hot: Boolean); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure DegradeChanged(Sender: TObject);
    procedure WallpaperChanged(Sender: TObject);
    procedure ImagelistOptionsChanged(Sender: TObject);
    // To be public :
    property DefaultFlatBorderColor: TColor read FFlatBorderColor write FFlatBorderColor;
    property DefaultHotFontColor: TColor read FHotFontColor write FHotFontColor;
    property DefaultHotFromColor: TColor read FHotFromColor write FHotFromColor;
    property DefaultHotToColor: TColor read FHotToColor write FHotToColor;
    property DefaultDownFontColor: TColor read FDownFontColor write FDownFontColor;
    property DefaultDownFromColor: TColor read FDownFromColor write FDownFromColor;
    property DefaultDownToColor: TColor read FDownToColor write FDownToColor;
    property DefaultDisabledFromColor: TColor read FDisabledFromColor write FDisabledFromColor;
    property DefaultDisabledToColor: TColor read FDisabledToColor write FDisabledToColor;
    property DefaultMetroFrameColor: TColor read FMetroFrameColor write FMetroFrameColor;
    property DefaultMetroFrameWidth: Word read FMetroFrameWidth write FMetroFrameWidth;
    // To be published :
    property Degrade: TcyGradient read FDegrade write SetDegrade;
    property HotBalanceMode: TDgradBalanceMode read FHotBalanceMode write FHotBalanceMode default bmReverseFromColor;
    property ImagelistOptions: TcyImagelistOptions read FImagelistOptions write SetImagelistOptions;
    property Wallpaper: TcyBgPicture read FWallpaper write SetWallpaper;
    property BeforePaint: TProcBeforePaint read FBeforePaint write FBeforePaint;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
  end;

  TcySpeedButton = class(TcyCustomSpeedButton)
  private
  protected
  public
    property MouseOver;
    // Herited from TcyCustomSpeedButton :
    property DefaultFlatBorderColor;
    property DefaultHotFromColor;
    property DefaultHotToColor;
    property DefaultHotFontColor;
    property DefaultDownFromColor;
    property DefaultDownToColor;
    property DefaultDownFontColor;
    property DefaultDisabledFromColor;
    property DefaultDisabledToColor;
    property DefaultMetroFrameColor;
    property DefaultMetroFrameWidth;
  published
    // Herited from TcyCustomSpeedButton :
    property DisabledGlyphDrawing;
    property DisabledColor;
    property MonochromeGlyphColor;
    property Degrade;
    property HotBalanceMode;
    property ImagelistOptions;
    property Wallpaper;
    // Herited from TcyBaseSpeedButton :
    property CaptionOrientation;
    property FlatDisabledStyle;
    property FlatDownStyle;
    property FlatHotStyle;
    property FlatNormalStyle;
    property GlyphX;
    property WordWrap;
    property OnMouseEnter;
    property OnMouseLeave;
    property BeforePaint;
    property OnPaint;
  end;

const
  FFramePercent = -20;

implementation

{ TcyCustomSpeedButton }
procedure TcyCustomSpeedButton.CMFontChanged(var Message: TMessage);
begin
  Invalidate;

  FDownFontColor := Font.Color;
  FHotFontColor := Font.Color;
end;

constructor TcyCustomSpeedButton.Create(AOwner: TComponent);
begin
  inherited;
  FDegrade := TcyGradient.Create(Self);
  FDegrade.OnChange := DegradeChanged;
  FDegrade.FromColor := clYellow;
  FDegrade.ToColor := $000080FF;  // Orange color ...
  FDegrade.SpeedPercent := 90;
  FMetroFrameWidth := 2;
  FWallpaper := TcyBgPicture.Create(self);
  FWallpaper.OnChange := WallpaperChanged;
  FHotBalanceMode := bmReverseFromColor;
  DegradeChanged(Self);
  FImagelistOptions := TcyImagelistOptions.Create(Self);
  FImagelistOptions.OnChange := ImagelistOptionsChanged;
end;

destructor TcyCustomSpeedButton.Destroy;
begin
  FWallpaper.Free;
  FDegrade.Free;
  FImagelistOptions.Free;
  inherited;
end;

procedure TcyCustomSpeedButton.SetImagelistOptions(const Value: TcyImagelistOptions);
begin
  FImagelistOptions.Assign(Value);
end;

procedure TcyCustomSpeedButton.ImagelistOptionsChanged(Sender: TObject);
begin
  Invalidate;
end;

procedure TcyCustomSpeedButton.SetDegrade(const Value: TcyGradient);
begin
  FDegrade := Value;
end;

procedure TcyCustomSpeedButton.SetWallpaper(const Value: TcyBgPicture);
begin
  FWallpaper := Value;
end;

procedure TcyCustomSpeedButton.WallpaperChanged(Sender: TObject);
begin
  Invalidate;
end;

procedure TcyCustomSpeedButton.DegradeChanged(Sender: TObject);
begin
  if FDegrade.FromColor = FDegrade.ToColor
  then begin
    FHotFromColor := ColorSetPercentBrightness(FDegrade.FromColor, 20);
    FHotToColor := FHotFromColor;
  end
  else begin
    FHotFromColor := FDegrade.FromColor;
    FHotToColor := FDegrade.ToColor;
  end;

  FDownFromColor := ColorSetPercentBrightness(FDegrade.FromColor, -20);
  FDownToColor := ColorSetPercentBrightness(FDegrade.ToColor, -20);
  FDisabledFromColor := ColorSetPercentBrightness(FDegrade.FromColor, 30);
  FDisabledToColor := ColorSetPercentBrightness(FDegrade.ToColor, 30);
  FMetroFrameColor := ColorSetPercentBrightness(FDegrade.FromColor, 40);
  FFlatBorderColor := FDownToColor;
  Invalidate;
end;

procedure TcyCustomSpeedButton.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;

  // Careful, FImageListOptions may be not created already, check operation first!
  if Operation = opRemove
  then
    if FImagelistOptions.ImageList <> nil
    then
      if AComponent = FImagelistOptions.ImageList
      then begin
        FImagelistOptions.ImageList := nil;
        Invalidate;
      end;
end;

procedure TcyCustomSpeedButton.DrawButton(aState: TButtonState; Hot: Boolean);
var
  ImageHandled: Boolean;
  Rect, TextRect: TRect;
  aGraphicWidth, aGraphicHeight, aGraphicX, aGraphicY: Integer;
  ImageListIndex: Integer;
  ImageListEnabled: Boolean;
begin
  // Let user changes drawing behavior :
  bDrawBorders := true;
  bDrawBackground := (not Flat) or (not Transparent);
  bDrawWallpaper := true;
  bDrawCaption := true;
  bDrawGlyph := Enabled or (DisabledGlyphDrawing <> dgDoNotDraw);

  Canvas.Font := Self.Font;

  if not Enabled then
  begin
    Canvas.Font.Color := DisabledColor;
    MonochromeGlyphColor := DisabledColor;
  end;

  if aState = bsUp then
    if Hot then
      Canvas.Font.Color := FHotFontColor;

  if aState in [bsDown, bsExclusive] then
    Canvas.Font.Color := FDownFontColor;

  if Assigned(FBeforePaint) then
    FBeforePaint(Self, bDrawBorders, bDrawBackground, bDrawWallpaper, bDrawCaption, bDrawGlyph);

  Rect := ClientRect;
  ImageHandled := false;

  DrawBackground(Rect, aState, Hot);

  // Draw caption/glyph :
  if Assigned(FImagelistOptions.ImageList) then
  begin
    FImagelistOptions.GetDrawingParams(aState, ImageListIndex, ImageListEnabled);

    if ImageListIndex > -1
    then begin
      aGraphicWidth := FImagelistOptions.ImageList.Width;
      aGraphicHeight := FImagelistOptions.ImageList.Height;
      CalcLayout(Rect, aGraphicWidth, aGraphicHeight, aGraphicX, aGraphicY, TextRect);
      if bDrawGlyph then
        FImagelistOptions.ImageList.Draw(Canvas, aGraphicX, aGraphicY, ImageListIndex, ImageListEnabled);
      ImageHandled := true;
    end;
  end
  else
    if ValidGraphic(GlyphX.Graphic) then
    begin
      if GlyphX.Graphic is graphics.TBitmap
      then aGraphicWidth := GlyphX.Bitmap.Width div NumGlyphs
      else aGraphicWidth := GlyphX.Width;

      aGraphicHeight := GlyphX.Height;
      CalcLayout(Rect, aGraphicWidth, aGraphicHeight, aGraphicX, aGraphicY, TextRect);
      if bDrawGlyph then
        DrawGlyph(aGraphicX, aGraphicY, aState);
      ImageHandled := true;
    end;

  if not ImageHandled
  then begin
    aGraphicWidth := 0;
    aGraphicHeight := 0;
    CalcLayout(Rect, aGraphicWidth, aGraphicHeight, aGraphicX, aGraphicY, TextRect);
  end;

  if bDrawCaption then
  begin
    Canvas.Brush.Style := bsClear;
    DrawCaption(TextRect, aState, Hot, aGraphicWidth <> 0);
    Canvas.Brush.Style := bsSolid;
  end;
end;

procedure TcyCustomSpeedButton.DrawBackground(var Rect: TRect; aState: TButtonState; Hot: Boolean);
var
  fromColor, toColor, FrameColor, BorderLeftTopColor, BorderRightBottomColor: TColor;
  balanceMode: TDgradBalanceMode;
  Details: TThemedElementDetails;
  i: Integer;
begin
  // inherited;
  case aState of
    bsUp:
      begin
        if Hot and (FlatHotStyle <> hsHidden) then
        begin
          // *** hot *** //
          balanceMode := FHotBalanceMode;   // 2014-09-19 Added !!!

          if (FlatHotStyle = hsMetro) and Flat then
          begin
            fromColor := FDegrade.FromColor;
            toColor := FDegrade.ToColor;
            BorderLeftTopColor := FMetroFrameColor;
          end
          else begin
            fromColor := FHotFromColor;
            toColor := FHotToColor;
            BorderLeftTopColor := ColorSetPercentBrightness(fromColor, 40);
          end;
        end
        else begin
          // *** not hot or not flat *** //
          fromColor := FDegrade.FromColor;
          toColor := FDegrade.ToColor;
          balanceMode := FDegrade.BalanceMode;

          if (FlatNormalStyle = nsMetro) and Flat
          then BorderLeftTopColor := FMetroFrameColor
          else BorderLeftTopColor := ColorSetPercentBrightness(fromColor, 40);
        end;


        BorderRightBottomColor := MediumColor(fromColor, ToColor);
        if not Flat then
          FrameColor := ColorSetPercentBrightness(toColor, FFramePercent);
      end;

    bsDisabled:
      begin
        fromColor := FDisabledFromColor;
        toColor := FDisabledToColor;
        balanceMode := FDegrade.BalanceMode;

        if (FlatDisabledStyle = disMetro) and Flat then
        begin
          BorderLeftTopColor := FMetroFrameColor;
        end
        else begin
          BorderLeftTopColor := ColorSetPercentBrightness(fromColor, 40);
          BorderRightBottomColor := MediumColor(fromColor, ToColor);
        end;

        if not Flat then
          FrameColor := ColorSetPercentBrightness(FDisabledToColor, FFramePercent);
      end;

    bsDown, bsExclusive:
      begin
        if (FlatDownStyle = dsMetro) and Flat then
        begin
          fromColor := FDegrade.FromColor;
          toColor := FDegrade.ToColor;
          balanceMode := bmMirror;
          BorderLeftTopColor := FMetroFrameColor;
        end
        else begin
          fromColor := FDownFromColor;
          toColor := FDownToColor;
          balanceMode := bmMirror;
          BorderLeftTopColor := MediumColor(fromColor, ToColor);
          BorderRightBottomColor := ColorSetPercentBrightness(fromColor, 40);
          if not Flat then
            FrameColor := ColorSetPercentBrightness(FDownToColor, FFramePercent);
        end;
      end;
  end;

  // Draw button body :
  if bDrawBackground then
  begin
    cyGradientFill(Canvas, Rect, fromColor, toColor,  FDegrade.Orientation, FDegrade.Balance,
                     FDegrade.AngleDegree, balanceMode, FDegrade.MaxDegrade, FDegrade.SpeedPercent);
  end;

  // Draw Wallpaper :
  if bDrawWallpaper then
    cyDrawBgPicture(Canvas, Rect, FWallpaper);

  // Draw frame :
  if Flat then
  begin
    case aState of
      bsUp:
      begin
        if Hot then    // Show hot state on flat buttons :
        begin
          if ThemeServices.ThemesEnabled and (FlatHotStyle = hsWindowsTheme)
          then begin
            Details := ThemeServices.GetElementDetails(ttbButtonHot);
            ThemeServices.DrawElement(Canvas.Handle, Details, Rect);
            Rect := ThemeServices.ContentRect(Canvas.Handle, Details, Rect);
          end
          else
            case FlatHotStyle of
              hsClassic:
                if bDrawBorders then
                  DrawBorders(Rect, BorderLeftTopColor, ColorSetPercentBrightness(BorderRightBottomColor, -40));

              hsMetro:
              begin
                Canvas.Brush.Color := BorderLeftTopColor;

                if bDrawBorders then
                  for i := 1 to FMetroFrameWidth do
                  begin
                    Canvas.FrameRect(Rect);
                    InflateRect(Rect, -1, -1);
                  end;
              end;
            end;
        end
        else begin
          case FlatNormalStyle of
            nsMetro:
            begin
              Canvas.Brush.Color := BorderLeftTopColor;

              if bDrawBorders then
                for i := 1 to FMetroFrameWidth do
                begin
                  Canvas.FrameRect(Rect);
                  InflateRect(Rect, -1, -1);
                end;
            end;

            nsShowBorders:
            begin
              if bDrawBorders then
                DrawBorders(Rect, FFlatBorderColor, FFlatBorderColor);
            end;
          end;
        end;
      end;

      bsDisabled:
      begin
        case FlatDisabledStyle of
          disMetro:
          begin
            Canvas.Brush.Color := BorderLeftTopColor;

            if bDrawBorders then
              for i := 1 to FMetroFrameWidth do
              begin
                Canvas.FrameRect(Rect);
                InflateRect(Rect, -1, -1);
              end;
          end;

          disShowBorders:
          begin
            if bDrawBorders then
              DrawBorders(Rect, FFlatBorderColor, FFlatBorderColor);
          end;
        end;
      end;

      bsDown, bsExclusive:         // Show down state on flat buttons :
      begin
        // if ThemeControl(Self) and (FlatDownStyle = dsWindowsTheme)
        if ThemeServices.ThemesEnabled and (FlatDownStyle = dsWindowsTheme)
        then begin
          Details := ThemeServices.GetElementDetails(ttbButtonPressed);
          ThemeServices.DrawElement(Canvas.Handle, Details, Rect);
          Rect := ThemeServices.ContentRect(Canvas.Handle, Details, Rect);
        end
        else
          case FlatDownStyle of
            dsClassic:
              if bDrawBorders then
                DrawBorders(Rect, ColorSetPercentBrightness(BorderLeftTopColor, -40), BorderRightBottomColor);

            dsMetro:
            begin
              Canvas.Brush.Color := BorderLeftTopColor;
              Canvas.FrameRect(Rect);

              if bDrawBorders then
                for i := 1 to FMetroFrameWidth do
                begin
                  Canvas.FrameRect(Rect);
                  InflateRect(Rect, -1, -1);
                end;
            end;
          end;

        OffsetRect(Rect, 1, 1);  // Will offset caption + glyph!
      end;
    end;
  end
  else begin
    if bDrawBorders then
      DrawBorders(Rect, FrameColor, FrameColor);

    if bDrawBorders then
      if aState = bsDown
      then DrawBorders(Rect, BorderLeftTopColor, BorderRightBottomColor)
      else DrawInnerBorders(Rect, BorderLeftTopColor, BorderRightBottomColor);
  end;
end;

end.
