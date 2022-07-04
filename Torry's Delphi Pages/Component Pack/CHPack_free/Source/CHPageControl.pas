unit CHPageControl;

{ ##############################################################################
  TCHPageControl

  Version   		:   1.0.0
  Delphi    		:   5, 6, 7
  Author     		:   Christian Hämmerle
  eMail     		:   chaemmerle@Blue-Xplosion.de
  Internet  		:   http://www.Blue-Xplosion.de (German/English)

  History:
  1.0.0 - 13.06.2004    - First Release


  ############################################################################ }

interface

uses
  Windows, Messages, Classes, Controls, ComCtrls, StdCtrls, CommCtrl, Graphics,
  SysUtils, Forms;

type
  TTabDraw = (tdNormal, tdColor, tdGraphic);
  TCHPageControl = class;

  TCHActiveSetting = class(TPersistent)
  private
    FOwner : TCHPageControl;
    FActiveTabFont: TFont;
    procedure SetActiveTabFont(const Value: TFont);
    procedure FontChanged(Sender: TObject);

  public
    constructor Create(AOwner: TCHPageControl); virtual;
  published
    property Font : TFont read FActiveTabFont Write SetActiveTabFont;
  end;


  TCHPageControl = class(TPageControl)
  private
    sTabEnable : string;
    FActiveSetting : TCHActiveSetting;
    FBuffered: Boolean;
    FTabDraw: TTabDraw;
    FBackground: TPicture;
    FBackgroundForRest : Boolean;
    procedure SetBuffered(const Value: Boolean);
    procedure SetTabDraw(const Value: TTabDraw);
    procedure SetBackground(const Value: TPicture);
    procedure BackGroundChange(Sender: TObject);
    procedure PaintBackground(ACanvas: TCanvas; ARect: TRect);
    procedure SetBackgroundForRest(const Value: Boolean);
  protected
    procedure DrawTab(TabIndex: Integer; const Rect: TRect; Active: Boolean); override;
  public
    FLastTab : TTabSheet;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
    procedure Change; override;
    function CanChange: Boolean; override;
    procedure EnablePage(TabSheet: TTabSheet; Aktive : Boolean); overload;

  published
    property ActiveSetting : TCHActiveSetting read FActiveSetting Write FActiveSetting;
    property Buffered : Boolean read FBuffered Write SetBuffered;
    property TabDraw : TTabDraw read FTabDraw Write SetTabDraw;
    property TabBackground: TPicture read FBackground write SetBackground;
    property TabBackgroundAll: Boolean read FBackgroundForRest write SetBackgroundForRest default TRUE;

  end;


procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('CH Pack', [TCHPageControl]);
end;

{ TCHPageControl }
{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
constructor TCHPageControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csOpaque];
  OwnerDraw := True;
  FActiveSetting := TCHActiveSetting.Create(Self);
  FTabDraw := tdColor;
  FBackground := TPicture.Create;
  FBackground.OnChange := BackgroundChange;

  FBackgroundForRest := True;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
destructor TCHPageControl.Destroy;
begin
  FBackground.Free;
  inherited Destroy
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHPageControl.Loaded;
begin
  inherited;
  FLastTab := Pages[0];
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHPageControl.DrawTab(TabIndex: Integer; const Rect: TRect;
  Active: Boolean);
var
  AText: string;
  APoint: TPoint;
  CR, R, RestRect: TRect;
  //TS: TTabSheet;
begin

  inherited DrawTab(TabIndex, Rect, Active);

  if FTabDraw = tdColor then
  begin
    R := Rect;
    with Canvas do
    begin
      If not Pages[TabIndex].Enabled Then
        Font.Color := clGray
      else
      begin
        Font.Color := clBlack;
        if (Active) Then
          Font := FActiveSetting.FActiveTabFont
        else
          Font := Self.Font;
      end;

      if Active then
        R.Bottom := R.Bottom - 4;

      Brush.Color := Parent.Brush.Color;
      FillRect(Rect);
      AText := Pages[TabIndex].Caption;
      APoint.x := (Rect.Right - Rect.Left) div 2 - TextWidth(AText) div 2;
      APoint.y := (Rect.Bottom - Rect.Top) div 2 - TextHeight(AText) div 2;
      TextRect(Rect, Rect.Left + APoint.x, Rect.Top + APoint.y, AText);
    end;
  end
  else if FTabDraw = tdGraphic then
  begin
    //TS := Pages[TabIndex];
    CR := GetClientRect;
    R := Rect;
    with Canvas do
    begin
      If not Pages[TabIndex].Enabled Then
        Font.Color := clGray
      else
      begin
        Font.Color := clBlack;
        if (Active) Then
          Font := FActiveSetting.FActiveTabFont
        else
          Font := Self.Font;
      end;


      if FBackgroundForRest and (TabIndex = PageCount - 1) then
      begin
        RestRect := classes.Rect(R.Right, CR.Top, CR.Right+3, R.Bottom);
        if Active then
          RestRect.Bottom := RestRect.Bottom-4;
        PaintBackground(Canvas, RestRect);
      end;

      if Active then
        R.Bottom := R.Bottom - 4;

      PaintBackground(Canvas, R);

      Brush.Style := bsClear;
      AText := Pages[TabIndex].Caption;
      TextOut(Rect.Left+2, Rect.Top+2, AText);
      //APoint.x := (Rect.Right - Rect.Left) div 2 - TextWidth(AText) div 2;
      //APoint.y := (Rect.Bottom - Rect.Top) div 2 - TextHeight(AText) div 2;
      //TextRect(Rect, Rect.Left + APoint.x, Rect.Top + APoint.y, AText);
    end;
  end;

  
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHPageControl.EnablePage(TabSheet: TTabSheet; Aktive: Boolean);
var
  nPos, nPosLen : Integer;
begin
  if Aktive = False then
  begin
    TabSheet.Enabled := False;
    nPos := Pos(TabSheet.Name, sTabEnable);
    if nPos = 0 then
      sTabEnable := sTabEnable + TabSheet.Name + ',';
  end
  else
  begin
    TabSheet.Enabled := True;
    nPos := Pos(TabSheet.Name, sTabEnable);
    if nPos > 0 then
    begin
      nPosLen := Length(TabSheet.Name) +1;
      Delete(sTabEnable, nPos, nPosLen);
    end;
  end;
  Invalidate;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHPageControl.Change;
begin
  if not Pages[TabIndex].Enabled then
    ActivePage := FLastTab
  else
    inherited;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
function TCHPageControl.CanChange: Boolean;
begin
  FLastTab := Pages[TabIndex];
  Result := inherited CanChange;
end;


{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHPageControl.SetBuffered(const Value: Boolean);
begin
  if FBuffered <> Value then
  begin
    FBuffered := Value;
    if (FBuffered = True) then
      Self.DoubleBuffered := True
    else
      Self.DoubleBuffered := False;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
constructor TCHActiveSetting.Create(AOwner: TCHPageControl);
begin
  inherited Create;
  FOwner := AOwner;

  FActiveTabFont := TFont.Create;
  FActiveTabFont.Assign(Self.Font);
  FActiveTabFont.OnChange := FontChanged;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHActiveSetting.FontChanged(Sender: TObject);
begin
  FOwner.Invalidate;
  FOwner.Repaint;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHActiveSetting.SetActiveTabFont(const Value: TFont);
begin
  FActiveTabFont.Assign(Value);
end;

procedure TCHPageControl.SetTabDraw(const Value: TTabDraw);
begin
  FTabDraw := Value;
  Repaint
end;

procedure TCHPageControl.SetBackground(const Value: TPicture);
begin
  FBackground.Assign(Value);
  Repaint
end;

procedure TCHPageControl.BackGroundChange(Sender: TObject);
begin
  Repaint;
end;

procedure TCHPageControl.PaintBackground(ACanvas: TCanvas; ARect: TRect);
VAR
  CR, R: TRect;
  B, H, I, J: Integer;
  P : TPicture;
  Pos1: TRect; {Position des gesamten Pattern im Canvas}
  Pos2: TRect; {Position des darzustellenden Teilpattern im Pattern}
  Malen: Boolean;
BEGIN
  IF Assigned(FBackGround) AND Assigned(FBackGround.Graphic) THEN BEGIN
    P := TPicture.Create;
    TRY
      P.Assign(FBackGround);
      CR := ARect;
      R.Left := 0;
      R.Top := 0;
      R.Right := ARect.Right - ARect.Left;
      R.Bottom := ARect.Bottom - ARect.Top;
      H := P.Graphic.Height;
      B := P.Graphic.Width;
      IF (H>0) AND (B>0) THEN BEGIN

        FOR I := 0 TO ((CR.Right - CR.Left) DIV B) DO
        FOR J := 0 TO ((CR.Bottom - CR.Top) DIV H) DO BEGIN
          Pos1 := Rect(I*B, J*H, (I+1)*B, (J+1)*H);
          Malen := (Pos1.Left < R.Right) AND (Pos1.Right > R.Left) AND
                   (Pos1.Top < R.Bottom) AND (Pos1.Bottom > R.Top);

          IF Malen THEN BEGIN
            Pos2 := Rect(R.Left-I*B, R.Top-J*H, R.Right-I*B, R.Bottom-J*H);
            IF Pos2.Left   < 0 THEN Pos2.Left   := 0;
            IF Pos2.Top    < 0 THEN Pos2.Top    := 0;
            IF Pos2.Right  > B THEN Pos2.Right  := B;
            IF Pos2.Bottom > H THEN Pos2.Bottom := H;

            IF (Pos2.Left > 0) OR (Pos2.Top > 0) THEN
            BitBlt(P.Bitmap.Canvas.Handle, 0, 0,
                  (Pos2.Right-Pos2.Left),
                  (Pos2.Bottom-Pos2.Top),
                   P.Bitmap.Canvas.Handle, Pos2.Left, Pos2.Top,
                   srcCopy);

            P.Graphic.Width := Pos2.Right-Pos2.Left;
            P.Graphic.Height := Pos2.Bottom-Pos2.Top;
            ACanvas.Draw(Pos1.Left + Pos2.Left + ARect.Left,
                         Pos1.Top  + Pos2.Top  + ARect.Top,
                         P.Graphic);
            P.Assign(FBackGround);
          END;
        END;

      END;  {IF (H>0) AND ...}
    FINALLY
      P.Free;
    END;
  end;

end;

procedure TCHPageControl.SetBackgroundForRest(const Value: Boolean);
begin
  FBackgroundForRest := Value;
  Repaint
end;

end.
