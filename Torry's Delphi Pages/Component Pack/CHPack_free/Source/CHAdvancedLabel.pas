unit CHAdvancedLabel;

{ ##############################################################################
  TCHAdvancedLabel

  Version   		:   1.1.1
  Delphi    		:   5, 6, 7
  Author     		:   Christian Hämmerle
  eMail     		:   chaemmerle@Blue-Xplosion.de
  Internet  		:   http://www.Blue-Xplosion.de (German/English)

  History:
  1.0.0 - 15.11.2002    - First Release
  1.1.0 - 26.12.2002    - NEW: Captioneffects (multigradient and texture - font)
                        - BUG: "Destroy" don´t get free some resources
                        - BUG: "Autosize" don´t work correct, if caption is blank
                        - BUG: "Transparent" work now correctly at Designtime
  1.1.1 - 09.03.2003    - reorganize "uses" for more performance and less memory needed

  ############################################################################ }

interface

uses
  Windows, Forms, Messages, Classes, Controls, Graphics, ShellApi,
  _CHClassProperty, _CHClassFunction;

type

  TCHCustomAdvancedLabel = class(TGraphicControl)
  private
    FCaptionLayout : TCHCaptionLayout;
    FCaptionEffect : TCHCaptionEffect;
    FBitmap: TCHBitmap;
    FFill: TCHFill;
    FGradient: TCHGradient;

    FOnClick : TNotifyEvent;
    FOnMouseEnter : TNotifyEvent;
    FOnMouseLeave : TNotifyEvent;

    FFocusControl : TWinControl;
    FExecuteType : TExecuteType;
    FBackgroundBmp : TBitmap;
    FEffectBitmap : TBitmap;
    FExecuteActive : Boolean;
    FShowAccelChar: Boolean;
    FEffect : Boolean;
    FAutosize: Boolean;
    FExecuteText : string;
    FExecuteParameter : string;
    FClientRect : TRect;
    FWorkRect : TRect;
    FCaptionRect : TRect;

    procedure GetRectSize;
    procedure MakeForeground;

    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure UpdateChanges(Sender: TObject);
    procedure SetTextStyle(const Value: TTextStyle);
    procedure SetFocusControl(const Value: TWinControl);
    procedure SetShowAccelChar(const Value: Boolean);
    procedure SetNewAutosize(const Value: Boolean);
  protected
    procedure Click; override;
    procedure Paint; override;
    procedure Resize; override;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
    property Canvas;
    property Caption;
  published
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;

    property Autosize : Boolean read FAutosize Write SetNewAutosize;
    property Bitmap : TCHBitmap read FBitmap Write FBitmap;
    property CaptionEffect : TCHCaptionEffect read FCaptionEffect write FCaptionEffect;
    property CaptionLayout : TCHCaptionLayout read FCaptionLayout write FCaptionLayout;
    property ExecuteActive : Boolean read FExecuteActive write FExecuteActive;
    property ExecuteText : string read FExecuteText write FExecuteText;
    property ExecuteParameter : string read FExecuteParameter write FExecuteParameter;
    property ExecuteType : TExecuteType read FExecuteType write FExecuteType;
    property Fill : TCHFill read FFill write FFill;
    property FocusControl: TWinControl read FFocusControl write SetFocusControl;
    property Gradient : TCHGradient read FGradient Write FGradient;
    property ShowAccelChar: Boolean read FShowAccelChar write SetShowAccelChar;
  end;

  TCHAdvancedLabel = class(TCHCustomAdvancedLabel)
  published
    property Anchors;
    property AutoSize;
    property BiDiMode;
    property Bitmap;
    property Caption;
    property CaptionEffect;
    property CaptionLayout;
    property Color;
    property Constraints;
    property Cursor;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ExecuteActive;
    property ExecuteText;
    property ExecuteParameter;
    property ExecuteType;
    property Fill;
    property FocusControl;
    property Font;
    property Gradient;
    property Hint;
    property Left;
    property Name;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowAccelChar;
    property ShowHint;
    property Tag;
    property Top;
    property Visible;
    property Width;

    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnEndDock;
    property OnStartDock;
    property OnStartDrag;

  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('CH Pack', [TCHAdvancedLabel]);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
constructor TCHCustomAdvancedLabel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCaptionLayout := TCHCaptionLayout.Create;
  FCaptionLayout.OnChange := UpdateChanges;
  FCaptionEffect := TCHCaptionEffect.Create;
  FCaptionEffect.OnChange := UpdateChanges;
  FGradient := TCHGradient.Create;
  FGradient.OnChange := UpdateChanges;
  FFill := TCHFill.Create;
  FFill.OnChange := UpdateChanges;
  FBitmap := TCHBitmap.Create;
  FBitmap.OnChange := UpdateChanges;

  Width := 63;
  Height := 13;
  FEffectBitmap := TBitmap.Create;
  FBackgroundBmp := TBitmap.Create;
  Font.Color := clBlack;
  FExecuteType := etWWW;
  FExecuteText := 'http://www.Blue-Xplosion.de';
  FExecuteActive := False;
  FAutosize := True;
  FShowAccelChar := True;
  ControlStyle := ControlStyle + [csOpaque, csReplicatable];

  Invalidate;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCustomAdvancedLabel.Loaded;
begin
  inherited Loaded;
  UpdateChanges(Self);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
destructor TCHCustomAdvancedLabel.Destroy;
begin
  FBackgroundBmp.Free;
  FEffectBitmap.Free;
  FCaptionLayout.Free;
  FCaptionEffect.Free;
  FGradient.Free;
  FFill.Free;
  FBitmap.Free;
  inherited Destroy;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCustomAdvancedLabel.UpdateChanges(Sender: TObject);
begin
  if csLoading in ComponentState then
    Exit;
  // ++++ DRAW TRANSPARENT BACKGOUND ++++
  // transparents must set befor do invalitate !!!
  if FFill.Transparent  then
  	ControlStyle := ControlStyle - [csOpaque]
  else
    ControlStyle := ControlStyle + [csOpaque];

  Invalidate;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCustomAdvancedLabel.Click;
var
  sExecute, sParam : string;
begin
  if Assigned(FOnClick) then
    FOnClick(Self);

  if Enabled = True then
  begin
    if FExecuteActive and (FExecuteType <> etNone) and (FExecuteText <> '') then
    begin
      sParam := FExecuteParameter;

      if FExecuteType = etEMail then
        sExecute := 'mailto:' + FExecuteText + sParam
      else if FExecuteType = etNews then
        sExecute := 'news:' + FExecuteText
      else if FExecuteType = etWWW then
        sExecute := FExecuteText
      else if FExecuteType = etEXE then
        sExecute := FExecuteText;

      ShellExecute(0, 'open', PChar(sExecute), PChar(sParam), nil, SW_ShowNormal);
    end;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCustomAdvancedLabel.CMMouseEnter(var Message: TMessage);
begin
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(self);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCustomAdvancedLabel.CMMouseLeave(var Message: TMessage);
begin
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(self);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCustomAdvancedLabel.CMDialogChar(var Message: TCMDialogChar);
begin
  if (FFocusControl <> nil) and Enabled and ShowAccelChar and
    IsAccel(Message.CharCode, Caption) then
  begin
    with FFocusControl do
    begin
      if CanFocus then
      begin
        SetFocus;
        Message.Result := 1;
      end;
    end;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCustomAdvancedLabel.CMFontChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCustomAdvancedLabel.CMTextChanged(var Message: TMessage);
begin
  Invalidate;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCustomAdvancedLabel.Resize;
begin
  inherited Resize;
  Invalidate;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCustomAdvancedLabel.Paint;
var
  nX_BackgroundBmp, nY_BackgroundBmp : Integer;
begin
  inherited Paint;
  Canvas.Font.Assign(Self.Font);
  FEffect := False;

  // get size of ClientRect, WorkRect and TextRect
  GetRectSize;

  // ++++ SET TEXTSTYLE ++++
  if (FFill.Style = fsNormal) or (FFill.Style = fsGradient) or (FFill.Style = fsEffect) then
  begin
    SetTextStyle(FCaptionLayout.TextStyle);
  end;

  // ++++ DRAW NORMAL BACKGROUND
  if not FFill.Transparent then
    DrawNormal(Self.Canvas, FWorkRect, Self.Color);

  // ++++ DRAW GRADIENT BACKGROUND ++++
  if (FFill.Style = fsGradient) then
  begin
    // to draw gradient, min. two colors must ACTIVE
    if Length(FGradient.FGradientColorArray) > 1 then
    begin
      DrawGradient(Canvas, FClientRect, FGradient.FGradientColorArray,
        FGradient.Style, FGradient.Rotation);
    end
    else
    begin
      // only one color is ACTICE
      if (Length(FGradient.FGradientColorArray) = 1) then
        Canvas.Brush.Color := FGradient.FGradientColorArray[0]
      // no color is ACTIVE
      else
        Canvas.Brush.Color := Color;

      Canvas.Brush.Style := bsSolid;
      Canvas.FillRect(FClientRect);
    end;
  end
  // ++++ DRAW CAPTION-EFFECT ++++
  else if (FFill.Style = fsEffect) then
  begin
    FEffect := True;

    // draw caption-gradinet
    if (FCaptionEffect.Effect = ceGradient) then
    begin
      FEffectBitmap.Width := Self.Width;
      FEffectBitmap.Height := Self.Height;

      // to draw gradient, min. two colors must ACTIVE
      if Length(FGradient.FGradientColorArray) > 1 then
      begin
        DrawGradient(FEffectBitmap.Canvas, FClientRect, FGradient.FGradientColorArray,
          FGradient.Style, FGradient.Rotation);
      end
      else
      begin
        // only one color is ACTICE
        if (Length(FGradient.FGradientColorArray) = 1) then
          FEffectBitmap.Canvas.Brush.Color := FGradient.FGradientColorArray[0]
        // no color is ACTIVE
        else
          FEffectBitmap.Canvas.Brush.Color := Color;

        FEffectBitmap.Canvas.Brush.Style := bsSolid;
        FEffectBitmap.Canvas.FillRect(FClientRect);
      end;
    end
    // set fontbitmap; bitmap will not scale
    else if (FCaptionEffect.Effect = ceBitmap) then
    begin
      FEffectBitmap.Width := Self.Width;
      FEffectBitmap.Height := Self.Height;
      FEffectBitmap.Assign(FCaptionEffect.FontBitmap);
    end;
  end
  // ++++ DRAW BACKGROUND BITMAP ++++
  else if (FFill.Style = fsBitmap) then
  begin
    if not FBitmap.Bitmap.Empty then
    begin
      // build bitmap
      MakeForeground;

      // draw bitmap onto the canvas
      with Canvas do
      begin
        // Normal
        if FBitmap.Mode = bmNormal then
        begin
          FBackgroundBmp.Canvas.Draw(0, 0, FBitmap.Bitmap);
          nX_BackgroundBmp := FWorkRect.Left;
          nY_BackgroundBmp := FWorkRect.Top;
          Draw(nX_BackgroundBmp, nY_BackgroundBmp, FBackgroundBmp);
        end
        // Stretch
        else if FBitmap.Mode = bmStretch then
        begin
          FBackgroundBmp.Canvas.Draw(0, 0, FBitmap.Bitmap);
          StretchDraw(FWorkRect, FBackgroundBmp);
        end
        // Tile
        else if FBitmap.Mode = bmTile then
        begin
          FBackgroundBmp.Canvas.Draw(0, 0, FBitmap.Bitmap);
          Brush.Bitmap := FBackgroundBmp;
          FillRect(FWorkRect);
        end
        // Center
        else if FBitmap.Mode = bmCenter then
        begin
          // Center X
          if (FWorkRect.Right - FWorkRect.Left) > FBitmap.Bitmap.Width then
            nX_BackgroundBmp := ((FWorkRect.Right - FWorkRect.Left) div 2) -
              (FBitmap.Bitmap.Width div 2)
          else
            nX_BackgroundBmp := -(FBitmap.Bitmap.Width - (FWorkRect.Right - FWorkRect.Left)) div 2;

          // Center Y
          if (FWorkRect.Bottom - FWorkRect.Top) > FBitmap.Bitmap.Height then
            nY_BackgroundBmp := ((FWorkRect.Bottom - FWorkRect.Top) div 2) -
              (FBitmap.Bitmap.Height div 2)
          else
            nY_BackgroundBmp := -(FBitmap.Bitmap.Height - (FWorkRect.Bottom - FWorkRect.Top)) div 2;

          FBackgroundBmp.Canvas.Draw(nX_BackgroundBmp, nY_BackgroundBmp, FBitmap.Bitmap);

          Draw(0, 0, FBackgroundBmp);
        end;
      end;
    end;
  end;

  // draw caption with setting parameters
  if (FFill.Style = fsNormal) or (FFill.Style = fsGradient) or (FFill.Style = fsEffect) then
  begin
    DrawCaption(
      Self,
      Self.Canvas,
      FCaptionRect, FWorkRect, FClientRect,
      FCaptionLayout.Angle,
      FCaptionLayout.HighlightDepth, FCaptionLayout.ShadowDepth,
      FCaptionLayout.HighlightDirection, FCaptionLayout.ShadowDirection,
      FCaptionLayout.HighlightColor, FCaptionLayout.ShadowColor, Self.Color,
      FCaptionLayout.DisableColor, Self.Font.Color, clNone,
      Self.Caption,
      FEffectBitmap,
      FCaptionLayout.TextStyle,
      FCaptionLayout.Alignment,
      FFill.Style,
      tmAuto,
      gmCaption,
      gaLeft,
      geNone,
      nil,
      0, 0, 0,
      FCaptionLayout.PosX, FCaptionLayout.PosY,
      FAutosize, False, FCaptionLayout.Antialiasing, ShowAccelChar, FEffect,
      ctLabel);
  end;

end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCustomAdvancedLabel.SetTextStyle(const Value: TTextStyle);
begin
  with FCaptionLayout do
  begin
    TextStyle := Value;
    { ssNone }
    if Value = tsNone then
    begin
      ShadowDirection := drNone;
      ShadowDepth := 0;
      ShadowColor := clBtnShadow;

      HighlightDirection := drNone;
      HighlightDepth := 0;
      HighlightColor := clBtnHighlight;
    end
    { ssRaised }
    else if Value = tsRaised then
    begin
      ShadowDirection := drNone;
      ShadowDepth := 0;
      ShadowColor := clBtnShadow;

      HighlightDirection := drUpLeft;
      HighlightDepth := 1;
      HighlightColor := clBtnHighlight;
    end
    { ssRaisedColor }
    else if Value = tsRaisedColor then
    begin
      ShadowDirection := drDownRight;
      ShadowDepth := 1;
      ShadowColor := clBtnShadow;
      Self.Font.Color := clBtnFace;

      HighlightDirection := drUpLeft;
      HighlightDepth := 1;
      HighlightColor := clBtnHighlight;
    end
    { ssRaisedShadow }
    else if Value = tsRaisedShadow then
    begin
      ShadowDirection := drDownRight;
      ShadowDepth := 1;
      ShadowColor := clBtnShadow;

      HighlightDirection := drUpLeft;
      HighlightDepth := 1;
      HighlightColor := clBtnHighlight;
    end
    { ssRecessed }
    else if Value = tsRecessed then
    begin
      ShadowDirection := drNone;
      ShadowDepth := 0;
      ShadowColor := clBtnShadow;

      HighlightDirection := drDownRight;
      HighlightDepth := 1;
      HighlightColor := clBtnHighlight;
    end
    { ssRecessedColor }
    else if Value = tsRecessedColor then
    begin
      ShadowDirection := drUpLeft;
      ShadowDepth := 1;
      ShadowColor := clBtnShadow;
      Self.Font.Color := clBtnFace;

      HighlightDirection := drDownRight;
      HighlightDepth := 1;
      HighlightColor := clBtnHighlight;
    end
    { ssRecessedShadow }
    else if Value = tsRecessedShadow then
    begin
      ShadowDirection := drUpLeft;
      ShadowDepth := 1;
      ShadowColor := clBtnShadow;

      HighlightDirection := drDownRight;
      HighlightDepth := 1;
      HighlightColor := clBtnHighlight;
    end
    { ssShadow }
    else if Value = tsShadow then
    begin
      ShadowDirection := drDownRight;
      ShadowDepth := 2;
      ShadowColor := clBtnShadow;

      HighlightDirection := drNone;
      HighlightDepth := 0;
      HighlightColor := clBtnHighlight;
    end;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCustomAdvancedLabel.SetFocusControl(const Value: TWinControl);
begin
  if FFocusControl <> Value then
  begin
    FFocusControl := Value;
    if Value <> nil then
      Value.FreeNotification(Self);
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCustomAdvancedLabel.SetShowAccelChar(const Value: Boolean);
begin
  if FShowAccelChar <> Value then
  begin
    FShowAccelChar := Value;
    Invalidate;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCustomAdvancedLabel.GetRectSize;
begin
  // ClientRect
  FClientRect := GetClientRect;

  // WorkRect
  FWorkRect.Left := FClientRect.Left;
  FWorkRect.Top := FClientRect.Top;
  FWorkRect.Right := FClientRect.Right;
  FWorkRect.Bottom := FClientRect.Bottom;

  // TextRect
  FCaptionRect.Left := 0;
  FCaptionRect.Top := 0;
  FCaptionRect.Right := 0;
  FCaptionRect.Bottom := 0;

end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCustomAdvancedLabel.MakeForeground;
var
  BRect : TRect;
begin
  BRect := Bounds(0, 0, Self.Width, Self.Height);
  FBackgroundBmp.Canvas.Brush.Color := clWhite;
  FBackgroundBmp.Canvas.Brush.Style := bsSolid;
  FBackgroundBmp.Canvas.FillRect(BRect);

  if (FBitmap.Mode = bmNormal) or (FBitmap.Mode = bmCenter) then
  begin
    FBackgroundBmp.Width := (FWorkRect.Right - FWorkRect.Left);
    FBackgroundBmp.Height := (FWorkRect.Bottom - FWorkRect.Top);
  end
  else if (FBitmap.Mode = bmStretch) or (FBitmap.Mode = bmTile) then
  begin
    FBackgroundBmp.Width := FBitmap.Bitmap.Width;
    FBackgroundBmp.Height := FBitmap.Bitmap.Height;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCustomAdvancedLabel.SetNewAutosize(const Value: Boolean);
begin
  if FAutosize <> Value then
  begin
    FAutosize := Value;
    Invalidate;
  end;
end;




end.
