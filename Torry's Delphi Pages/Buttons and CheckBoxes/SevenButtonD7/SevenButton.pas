{*******************************************************************************
SevenButton
Copyright (C) 2010 Misugi
http://zyroom.misulud.fr
contact@misulud.fr

Button with the look of Windows Seven for Delphi 7.

SevenButton is free software: you can redistribute it and/or modify
it under the terms of the GNU Affero General Public License as
published by the Free Software Foundation, either version 3 of the
License, or (at your option) any later version.

SevenButton is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Affero General Public License for more details.

You should have received a copy of the GNU Affero General Public License
along with SevenButton. If not, see http://www.gnu.org/licenses.
*******************************************************************************}

{*******************************************************************************
History

# version 1.1 (2010-07-14)
Change: temporary bitmap to draw button (to stop the flashing)
Change: ignore message CM_MOUSELEAVE if button is disabled
Change: ignore message WM_LBUTTONUP if button is disabled

# version 1.0 (2010-07-11)
New: first version !
*******************************************************************************}
unit SevenButton;

interface

uses
  Classes, Controls, Graphics, Messages, StdCtrls, Forms, Types, Windows;

type
  TSevenButtonState = (fbsNormal, fbsHot, fbsDown, fbsFocused, fbsDisabled);
  TSevenButtonBorderWidth = 1..10;
  
  TSevenButton = class;

  TSevenButtonBorder = class(TPersistent)
  private
    FButton: TSevenButton;
    FColorNormal: TColor;
    FColorHot: TColor;
    FColorDown: TColor;
    FColorDisabled: TColor;
    FColorFocused: TColor;
    FWidthNormal: TSevenButtonBorderWidth;
    FWidthHot: TSevenButtonBorderWidth;
    FWidthDown: TSevenButtonBorderWidth;
    FWidthDisabled: TSevenButtonBorderWidth;
    FWidthFocused: TSevenButtonBorderWidth;
    procedure SetWidthNormal(const Value: TSevenButtonBorderWidth);
    procedure SetColorNormal(const Value: TColor);
  public
    constructor Create(AButton: TSevenButton);
  published
    property ColorNormal: TColor read FColorNormal write SetColorNormal;
    property ColorHot: TColor read FColorHot write FColorHot;
    property ColorDown: TColor read FColorDown write FColorDown;
    property ColorDisabled: TColor read FColorDisabled write FColorDisabled;
    property ColorFocused: TColor read FColorFocused write FColorFocused;
    property WidthNormal: TSevenButtonBorderWidth read FWidthNormal write SetWidthNormal;
    property WidthHot: TSevenButtonBorderWidth read FWidthHot write FWidthHot;
    property WidthDown: TSevenButtonBorderWidth read FWidthDown write FWidthDown;
    property WidthDisabled: TSevenButtonBorderWidth read FWidthDisabled write FWidthDisabled;
    property WidthFocused: TSevenButtonBorderWidth read FWidthFocused write FWidthFocused;
  end;

  TSevenButtonFonts = class(TPersistent)
  private
    FButton: TSevenButton;
    FFontHot: TFont;
    FFontDown: TFont;
    FFontDisabled: TFont;
    FFontFocused: TFont;
  public
    constructor Create(AButton: TSevenButton);
    destructor  Destroy; override;
  published
    property FontHot: TFont read FFontHot write FFontHot;
    property FontDown: TFont read FFontDown write FFontDown;
    property FontDisabled: TFont read FFontDisabled write FFontDisabled;
    property FontFocused: TFont read FFontFocused write FFontFocused;
  end;

  TSevenButtonColors = class(TPersistent)
  private
    FButton: TSevenButton;
    FColorNormalFrom: TColor;
    FColorNormalTo: TColor;
    FColorHotFrom: TColor;
    FColorHotTo: TColor;
    FColorDownFrom: TColor;
    FColorDownTo: TColor;
    FColorDisabledFrom: TColor;
    FColorDisabledTo: TColor;
    FColorFocusedFrom: TColor;
    FColorFocusedTo: TColor;
    procedure SetColorNormalFrom(const Value: TColor);
    procedure SetColorNormalTo(const Value: TColor);
  public
    constructor Create(AButton: TSevenButton);
  published
    property ColorNormalFrom: TColor read FColorNormalFrom write SetColorNormalFrom;
    property ColorNormalTo: TColor read FColorNormalTo write SetColorNormalTo;
    property ColorHotFrom: TColor read FColorHotFrom write FColorHotFrom;
    property ColorHotTo: TColor read FColorHotTo write FColorHotTo;
    property ColorDownFrom: TColor read FColorDownFrom write FColorDownFrom;
    property ColorDownTo: TColor read FColorDownTo write FColorDownTo;
    property ColorDisabledFrom: TColor read FColorDisabledFrom write FColorDisabledFrom;
    property ColorDisabledTo: TColor read FColorDisabledTo write FColorDisabledTo;
    property ColorFocusedFrom: TColor read FColorFocusedFrom write FColorFocusedFrom;
    property ColorFocusedTo: TColor read FColorFocusedTo write FColorFocusedTo;
  end;

  TSevenButtonPictures = class(TPersistent)
  private
    FButton: TSevenButton;
    FAlignment: TLeftRight;
    FTransparent: Boolean;
    FPictureNormal: TPicture;
    FPictureHot: TPicture;
    FPictureDown: TPicture;
    FPictureDisabled: TPicture;
    FPictureFocused: TPicture;
    procedure SetPictureNormal(const Value: TPicture);
    procedure SetPictureDisabled(const Value: TPicture);
    procedure SetPictureDown(const Value: TPicture);
    procedure SetPictureFocused(const Value: TPicture);
    procedure SetPictureHot(const Value: TPicture);
    procedure SetAlignment(const Value: TLeftRight);
    procedure SetTransparent(const Value: Boolean);
  public
    constructor Create(AButton: TSevenButton);
    destructor  Destroy; override;
  published
    property PictureNormal: TPicture read FPictureNormal write SetPictureNormal;
    property PictureHot: TPicture read FPictureHot write SetPictureHot;
    property PictureDown: TPicture read FPictureDown write SetPictureDown;
    property PictureDisabled: TPicture read FPictureDisabled write SetPictureDisabled;
    property PictureFocused: TPicture read FPictureFocused write SetPictureFocused;
    property Alignment: TLeftRight read FAlignment write SetAlignment default taLeftJustify;
    property Transparent: Boolean read FTransparent write SetTransparent default False;
  end;

  TSevenButton = class(TButtonControl)
  private
    FBorder: TSevenButtonBorder;
    FState: TSevenButtonState;
    FColors: TSevenButtonColors;
    FText: TSevenButtonFonts;
    FModalResult: TModalResult;
    FDefault: Boolean;
    FCancel: Boolean;
    FAlignment: TAlignment;
    FPictures: TSevenButtonPictures;
    FSpacing: Integer;
    FMarging: Integer;

    procedure Gradient(ACanvas: TCanvas; Arect: Trect; AColorFrom, AColorTo: TColor);
    procedure SetAlignment(const Value: TAlignment);
    procedure SetSpacing(const Value: Integer);
    procedure SetMarging(const Value: Integer);
  protected
    procedure WndProc(var Message: TMessage); override;
    procedure Paint;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;

    procedure   Click; override;
  published
    property Anchors;
    property Caption;
    property Constraints;
    property Cursor;
    property Enabled;
    property Font;
    property Height;
    property Hint;
    property Left;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop default True;
    property Tag;
    property Top;
    property Visible;
    property Width;

    property OnClick;
    property OnContextPopup;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;

    property Border: TSevenButtonBorder read FBorder write FBorder;
    property Colors: TSevenButtonColors read FColors write FColors;
    property Fonts: TSevenButtonFonts read FText write FText;
    property ModalResult: TModalResult read FModalResult write FModalResult default 0;
    property Default: Boolean read FDefault write FDefault default False;
    property Cancel: Boolean read FCancel write FCancel default False;
    property Pictures: TSevenButtonPictures read FPictures write FPictures;
    property Alignment: TAlignment read FAlignment write SetAlignment default taCenter;
    property Spacing: Integer read FSpacing write SetSpacing;
    property Marging: Integer read FMarging write SetMarging;
  end;

  procedure Register;

implementation

{$R *.res}

procedure Register;
begin
  RegisterComponents('Zyroom', [TSevenButton]);
end;

{ TSevenButton }

{*******************************************************************************
Create
*******************************************************************************}
constructor TSevenButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FBorder := TSevenButtonBorder.Create(Self);
  FColors := TSevenButtonColors.Create(Self); 
  FText := TSevenButtonFonts.Create(Self);
  FPictures := TSevenButtonPictures.Create(Self);

  // Button
  FColors.ColorNormalFrom := $00FCFCFC;
  FColors.ColorNormalTo := $00CFCFCF;
  FColors.ColorHotFrom := $00FCFCFC;
  FColors.ColorHotTo := $00F5D9A7;
  FColors.ColorDownFrom := $00FCFCFC;
  FColors.ColorDownTo := $00DBB368;
  FColors.ColorDisabledFrom := $00F4F4F4;
  FColors.ColorDisabledTo := $00F4F4F4;
  FColors.ColorFocusedFrom := $00FCFCFC;
  FColors.ColorFocusedTo := $00CFCFCF;

  // Text
  FText.FontDisabled.Color := clGrayText;

  // Border
  FBorder.ColorNormal := $00707070;
  FBorder.ColorHot := $00B17F3C;
  FBorder.ColorDown := $008B622C;
  FBorder.ColorDisabled := $00B5B2AD;
  FBorder.ColorFocused := $00B17F3C;
  FBorder.WidthNormal := 1;
  FBorder.WidthHot := 1;
  FBorder.WidthDown := 1;
  FBorder.WidthDisabled := 1;
  FBorder.WidthFocused := 1;

  // Other
  FSpacing := 5;
  FMarging := 5;
  FAlignment := taCenter;
  FState := fbsNormal;
  TabStop := True;
  FModalResult := 0;
  FCancel := False;
  FDefault := False;
  Width := 85;
  Height := 24;
end;

{*******************************************************************************
Destroy
*******************************************************************************}
destructor TSevenButton.Destroy;
begin
  FPictures.Free;
  FText.Free;
  FColors.Free;
  FBorder.Free;
  inherited;
end;

{*******************************************************************************
Paint
*******************************************************************************}
procedure TSevenButton.Paint;
var
  wCanvas: TCanvas;
  wBitmap: Graphics.TBitmap;
  wRect: TRect;
  wBorderColor: TColor;
  wBorderWidth: Integer;
  wColorFrom: TColor;
  wColorTo: TColor;
  wTextFont: TFont;
  wPicture: TPicture;
  wPicLeft, wPicTop: Integer;
  wAlignment: Integer;
  i: Integer;
begin
  if GetParentForm(Self) = nil then Exit;

  // Bitmap
  wBitmap := Graphics.TBitmap.Create;
  wBitmap.Width := Self.Width;
  wBitmap.Height := Self.Height;
  wCanvas := wBitmap.Canvas;

  // State
  wBorderColor := Border.ColorNormal;
  wColorFrom := Colors.ColorNormalFrom;
  wColorTo := Colors.ColorNormalTo;
  wTextFont := Font;
  wBorderWidth := Border.WidthNormal;
  wPicture := FPictures.PictureNormal;

  if not(csDesigning in ComponentState) then begin
    case FState of
      fbsFocused: begin
        wBorderColor := FBorder.ColorFocused;
        wColorFrom := FColors.ColorFocusedFrom;
        wColorTo := FColors.ColorFocusedTo;
        wTextFont := FText.FontFocused;
        wBorderWidth := FBorder.WidthFocused;
        if FPictures.PictureFocused.Graphic <> nil then
          wPicture := FPictures.PictureFocused;
      end;
      fbsHot: begin
        wBorderColor := FBorder.ColorHot;
        wColorFrom := FColors.ColorHotFrom;
        wColorTo := FColors.ColorHotTo;
        wTextFont := FText.FontHot;
        wBorderWidth := FBorder.WidthHot;
        if FPictures.PictureHot.Graphic <> nil then
          wPicture := FPictures.PictureHot;
      end;
      fbsDown: begin
        wBorderColor := FBorder.ColorDown;
        wColorFrom := FColors.ColorDownFrom;
        wColorTo := FColors.ColorDownTo;
        wTextFont := FText.FontDown;
        wBorderWidth := FBorder.WidthDown;
        if FPictures.PictureDown.Graphic <> nil then
          wPicture := FPictures.PictureDown;
      end;
      fbsDisabled: begin
        wBorderColor := FBorder.ColorDisabled;
        wColorFrom := FColors.ColorDisabledFrom;
        wColorTo := FColors.ColorDisabledTo;
        wTextFont := FText.FontDisabled;
        wBorderWidth := FBorder.WidthDisabled;
        if FPictures.PictureDisabled.Graphic <> nil then
          wPicture := FPictures.PictureDisabled;
      end;
    end;
  end;

  // Button
  wRect.Left := 0;
  wRect.Right := Self.Width;
  wRect.Top := wBorderWidth;
  wRect.Bottom := Self.Height - wBorderWidth;
  Gradient(wCanvas, wRect, wColorFrom, wColorTo);

  // Image
  if wPicture.Graphic <> nil then begin
    wPicture.Graphic.Transparent := FPictures.Transparent;
    case FPictures.Alignment of
      taLeftJustify: wPicLeft := Border.WidthNormal + FMarging;
      taRightJustify: wPicLeft := Self.Width - Border.WidthNormal - FMarging - wPicture.Graphic.Width;
    end;
    wPicTop := ((Self.Height - wPicture.Height) div 2);
    wCanvas.Draw(wPicLeft, wPicTop, wPicture.Graphic);
  end;

  // Border
  wCanvas.Pen.Width := 1;
  wCanvas.Pen.Color := wBorderColor;
  for i := 1 to wBorderWidth do begin
    wCanvas.MoveTo(i-1,i-1);
    wCanvas.LineTo(Self.Width-i, i-1);
    wCanvas.LineTo(Self.Width-i, Self.Height-i);
    wCanvas.LineTo(i-1, Self.Height-i);
    wCanvas.LineTo(i-1, i-1);
  end;

  // Corner
  wCanvas.Pixels[0,0] := Color;
  wCanvas.Pixels[wBorderWidth,wBorderWidth] := wBorderColor;
  wCanvas.Pixels[Self.Width-1,0] := Color;
  wCanvas.Pixels[Self.Width-1-wBorderWidth,wBorderWidth] := wBorderColor;
  wCanvas.Pixels[0,Self.Height-1] := Color;
  wCanvas.Pixels[wBorderWidth,Self.Height-1-wBorderWidth] := wBorderColor;
  wCanvas.Pixels[Self.Width-1,Self.Height-1] := Color;
  wCanvas.Pixels[Self.Width-1-wBorderWidth,Self.Height-1-wBorderWidth] := wBorderColor;

  // Text
  wCanvas.Pen.Width := 1;
  wCanvas.Brush.Style := bsClear;
  wCanvas.Font.Assign(wTextFont);
  case FAlignment of
    taCenter: wAlignment := DT_CENTER;
    taLeftJustify: begin
      wRect.Left := wRect.Left + FBorder.WidthNormal + FMarging;
      wAlignment := DT_LEFT;
    end;
    taRightJustify: begin
      wRect.Right := wRect.Right - FBorder.WidthNormal - FMarging;
      wAlignment := DT_RIGHT;
    end;
  end;
  if (wPicture.Graphic <> nil) and (Alignment <> taCenter) then begin
    case FPictures.Alignment of
      taLeftJustify: wRect.Left := wPicLeft + wPicture.Graphic.Width + FSpacing;
      taRightJustify: wRect.Right := wPicLeft - FSpacing;
    end;
  end;
  DrawText(wCanvas.Handle, PChar(Caption), -1, wRect, wAlignment or DT_NOPREFIX or DT_VCENTER or DT_SINGLELINE);

  // Draw the button
  with TControlCanvas.Create do begin
    Control := Self;
    Draw(0, 0, wBitmap);
  end;

  wBitmap.Free;
end;

{*******************************************************************************
Click
*******************************************************************************}
procedure TSevenButton.Click;
var
  Form: TCustomForm;
begin
  Form := GetParentForm(Self);
  if Form <> nil then Form.ModalResult := FModalResult;
  inherited;
end;

{*******************************************************************************
Messages
*******************************************************************************}
procedure TSevenButton.WndProc(var Message: TMessage);
begin
  if Message.Msg = WM_PAINT then Paint;
  if Message.Msg = CM_TEXTCHANGED then Paint;
  if Message.Msg = CM_COLORCHANGED then Paint;
  if Message.Msg = CM_BORDERCHANGED then Paint;
  if Message.Msg = WM_ERASEBKGND then Exit;

  if not(csDesigning in ComponentState) then begin
    case Message.Msg of
      WM_CHAR: begin
        if (Message.WParam = VK_RETURN) or
           (Message.WParam = VK_SPACE) then Click;
      end;
      CM_MOUSEENTER: begin
        FState := fbsHot;
        Paint;
      end;
      CM_MOUSELEAVE: begin
        if (FState <> fbsDisabled) then begin
          if Focused then FState := fbsFocused else FState := fbsNormal;
          Paint;
        end;
      end;
      CM_DIALOGKEY: begin
        if (Message.WParam = VK_RETURN) and FDefault and (not Focused) then Click;
        if (Message.WParam = VK_ESCAPE) and FCancel then Click;
      end;
      CM_FOCUSCHANGED: begin
        if Focused and (FState = fbsNormal) then FState := fbsFocused;
        if (not Focused) and (FState = fbsFocused) then FState := fbsNormal;
        Paint;
      end;
      CM_ENABLEDCHANGED: begin
        if not Enabled then FState := fbsDisabled else FState := fbsNormal;
        Paint;
      end;
      WM_LBUTTONDOWN: begin
        FState := fbsDown;
        Paint;
      end;
      WM_LBUTTONUP: begin
        if (FState <> fbsNormal) and (FState <> fbsFocused) and (FState <> fbsDisabled) then begin
          FState := fbsHot;
          Paint;
        end;
      end;
      WM_LBUTTONDBLCLK: begin
        FState := fbsDown;
        Paint;
        Click;
      end;
    end;
  end;

  inherited;
end;

{*******************************************************************************
Gradient
*******************************************************************************}
procedure TSevenButton.Gradient(ACanvas: TCanvas; Arect: Trect; AColorFrom, AColorTo: TColor);
var
  i: integer;
  R1, G1, B1  : integer;
  R, G, B     : integer;
  R2, G2, B2  : integer;
  ah : integer;
  wTop, wNewTop: Integer;
begin
  R1 := GetRValue(ColorToRGB(AColorFrom));
  G1 := GetGValue(ColorToRGB(AColorFrom));
  B1 := GetBValue(ColorToRGB(AColorFrom));
  R2 := GetRValue(ColorToRGB(AColorTo)) - R1;
  G2 := GetgValue(ColorToRGB(AColorTo)) - G1;
  B2 := GetbValue(ColorToRGB(AColorTo)) - B1;
  
  ah := Arect.Bottom - Arect.Top;

  wTop := -1;
  for i:= 0 to 255 do begin
    wNewTop := Arect.Top+(i * ah) div 256;
    if wTop = wNewTop then Continue;
    wTop := wNewTop;

    R := (R1+ (i * R2) div 255) mod 256;
    G := (G1+ (i * G2) div 255) mod 256;
    B := (B1+ (i * B2) div 255) mod 256;
    
    // Draw line
    ACanvas.Pen.Color := RGB(R, G, B);
    ACanvas.MoveTo(Arect.Left, wTop);
    ACanvas.LineTo(Arect.Right, wTop);
  end;
end;

procedure TSevenButton.SetAlignment(const Value: TAlignment);
begin
  FAlignment := Value;
  Paint;
end;

procedure TSevenButton.SetSpacing(const Value: Integer);
begin
  FSpacing := Value;
  Paint;
end;

procedure TSevenButton.SetMarging(const Value: Integer);
begin
  FMarging := Value;
  Paint;
end;

{ TSevenButtonFonts }

constructor TSevenButtonFonts.Create(AButton: TSevenButton);
begin
  inherited Create;
  FButton := AButton;
  FFontHot := TFont.Create;
  FFontDown := TFont.Create;
  FFontDisabled := TFont.Create;
  FFontFocused := TFont.Create;
end;

destructor TSevenButtonFonts.Destroy;
begin
  FFontHot.Free;
  FFontDown.Free;
  FFontDisabled.Free;
  FFontFocused.Free;
  inherited;
end;

{ TSevenButtonBorder }

constructor TSevenButtonBorder.Create(AButton: TSevenButton);
begin
  inherited Create;
  FButton := AButton;
end;

procedure TSevenButtonBorder.SetColorNormal(const Value: TColor);
begin
  FColorNormal := Value;
  FButton.Paint;
end;

procedure TSevenButtonBorder.SetWidthNormal(
  const Value: TSevenButtonBorderWidth);
begin
  FWidthNormal := Value;
  FButton.Paint;
end;

{ TSevenButtonColors }

constructor TSevenButtonColors.Create(AButton: TSevenButton);
begin
  inherited Create;
  FButton := AButton;
end;

procedure TSevenButtonColors.SetColorNormalFrom(const Value: TColor);
begin
  FColorNormalFrom := Value;
  FButton.Paint;
end;

procedure TSevenButtonColors.SetColorNormalTo(const Value: TColor);
begin
  FColorNormalTo := Value;
  FButton.Paint;
end;

{ TSevenButtonPictures }

constructor TSevenButtonPictures.Create(AButton: TSevenButton);
begin
  inherited Create;
  FButton := AButton;
  FAlignment := taLeftJustify;
  FTransparent := False;
  FPictureNormal := TPicture.Create;
  FPictureHot := TPicture.Create;
  FPictureDown := TPicture.Create;
  FPictureDisabled := TPicture.Create;
  FPictureFocused := TPicture.Create;
end;

destructor TSevenButtonPictures.Destroy;
begin
  FPictureNormal.Free;
  FPictureHot.Free;
  FPictureDown.Free;
  FPictureDisabled.Free;
  FPictureFocused.Free;
  inherited;
end;

procedure TSevenButtonPictures.SetAlignment(const Value: TLeftRight);
begin
  FAlignment := Value;
  FButton.Paint;
end;

procedure TSevenButtonPictures.SetPictureDisabled(const Value: TPicture);
begin
  FPictureDisabled.Assign(Value);
end;

procedure TSevenButtonPictures.SetPictureDown(const Value: TPicture);
begin
  FPictureDown.Assign(Value);
end;

procedure TSevenButtonPictures.SetPictureFocused(const Value: TPicture);
begin
  FPictureFocused.Assign(Value);
end;

procedure TSevenButtonPictures.SetPictureHot(const Value: TPicture);
begin
  FPictureHot.Assign(Value);
end;

procedure TSevenButtonPictures.SetPictureNormal(const Value: TPicture);
begin
  FPictureNormal.Assign(Value);
  FButton.Paint;
end;

procedure TSevenButtonPictures.SetTransparent(const Value: Boolean);
begin
  FTransparent := Value;
  FButton.Paint;
end;

end.
 
