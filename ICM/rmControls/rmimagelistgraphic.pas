{================================================================================
Copyright (C) 1997-2002 Mills Enterprise

Unit     : rmImageListGraphic
Purpose  : This is a visual interface for a TimageList.  Could be used for simple
           animation purposes or for displaying different images from an
           imagelist via an imageindex.
Date     : 05-03-2000
Author   : Ryan J. Mills
Version  : 1.90
================================================================================}

unit rmImageListGraphic;

interface

{$I CompilerDefines.INC}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  imglist, buttons, StdCtrls;

type
  TrmCustomImageListGraphic = class(TGraphicControl)
  private
    { Private declarations }
    FImageChangeLink: TChangeLink;
    FImages: TCustomImageList;
    fImageIndex: integer;
    fAutosize: boolean;
    fCentered: boolean;
    procedure ImageListChange(Sender: TObject);
    procedure SetImages(Value: TCustomImageList);
    procedure SetImageIndex(const Value: integer);
    procedure SetCentered(const Value: boolean);
  protected
    { Protected declarations }
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    property ImageIndex : integer read fImageIndex write SetImageIndex default -1;
    property Images: TCustomImageList read FImages write SetImages;
    property AutoSize : boolean read fAutosize write fautosize default true;
    property Centered : boolean read fCentered write SetCentered default false;
  public
    { Public declarations }
    constructor create(AOwner:TComponent); override;
    destructor destroy; override;
  published
    { Published declarations }
  end;

  TrmImageListGraphic = class(TrmCustomImageListGraphic)
  protected
    { Protected declarations }
    procedure Paint; override;
  published
    { Published declarations }
    property Align;
    property Anchors;
    property Autosize;
    property Centered;
    property Enabled;
    property ImageIndex;
    property Images;
  end;

  TGlyphLayout = (glGlyphLeft, glGlyphRight, glGlyphTop, glGlyphBottom);

  TrmCustomImageListGlyph = class(TrmCustomImageListGraphic)
  private
    fCaption: string;
    fGLayout: TGlyphLayout;
    procedure SetCaption(const Value: string);
    procedure SetGlyphLayout(const Value: TGlyphLayout);
    procedure CalcLayout( const Client: TRect; const Caption: string; Layout: TGlyphLayout;
                                Margin, Spacing: Integer; var GlyphPos: TPoint;
                                var TextBounds: TRect; BiDiFlags: LongInt );
    function TextFlags : integer;
  protected
    { Protected declarations }
    procedure InternalDrawGlyph(const GlyphPos: TPoint); virtual;
    procedure InternalDrawText(const Caption: string; TextBounds: TRect; BiDiFlags: Integer); virtual;
    procedure Paint; override;
    property Caption : string read fCaption write SetCaption;
    property GlyphLayout : TGlyphLayout read fGLayout write SetGlyphLayout;
  public
    { Public declarations }
    constructor create(AOwner:TComponent); override;
  end;

  TrmImageListGlyph = class(TrmCustomImageListGlyph)
  published
    { Published declarations }
    property Caption;
    property GlyphLayout;
    property BiDiMode;
    property Enabled;
    property Font;
    property Align;
    property Anchors;
    property ImageIndex;
    property Images;
  end;

implementation

{ TrmCustomImageListGraphic }

procedure TrmCustomImageListGraphic.ImageListChange(Sender: TObject);
begin
   Invalidate;
end;

procedure TrmCustomImageListGraphic.SetImages(Value: TCustomImageList);
begin
  if Images <> nil then
    Images.UnRegisterChanges(FImageChangeLink);
  FImages := Value;
  if Images <> nil then
  begin
    if fAutosize then
       SetBounds(left, top, Images.Width, Images.Height);
    Images.RegisterChanges(FImageChangeLink);
    Images.FreeNotification(Self);
  end;
  invalidate;
end;

procedure TrmCustomImageListGraphic.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = Images) then
    Images := nil;
end;

procedure TrmCustomImageListGraphic.SetImageIndex(const Value: integer);
begin
  if (value < -1) then
     fImageIndex := -1
  else
     fImageIndex := Value;
  RePaint;
end;

constructor TrmCustomImageListGraphic.create(AOwner: TComponent);
begin
  inherited;
  height := 16;
  width := 16;
  fImageIndex := -1;
  fAutoSize := true;
  fCentered := false;
  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := ImageListChange;
end;

procedure TrmCustomImageListGraphic.SetCentered(const Value: boolean);
begin
  if fCentered <> Value then
     fCentered := Value;
  Invalidate;
end;

destructor TrmCustomImageListGraphic.destroy;
begin
  FImageChangeLink.Free;
  inherited;
end;

{ TrmImageListGraphic }

procedure TrmImageListGraphic.Paint;
var
   xPos, yPos : integer;
begin
  inherited;
  if assigned(fimages) then
  begin
     if fCentered then
     begin
        xPos := (Width div 2) - (FImages.Width div 2);
        yPos := (Height div 2) - (fImages.Height div 2);
     end
     else
     begin
        xPos := 0;
        yPos := 0;
     end;
     if (fimageindex > -1) and (fImageIndex < FImages.Count) then
        fimages.Draw(canvas, xPos, yPos, fimageindex, enabled)
     else
     begin
        with canvas do
        begin
           brush.style := bsclear;
           fillrect(clientrect);
        end;
     end;
  end;

  if csdesigning in componentstate then
  begin
     with canvas do
     begin
        brush.Style := bsclear;
        pen.style := psDash;
        pen.color := clWindowtext;
        rectangle(clientrect);
     end;
  end;
end;

{ TrmCustomImageListGlyph }

procedure TrmCustomImageListGlyph.Paint;
var
   wrect : TRect;
   GlyphPos: TPoint;
begin
  inherited;
  CalcLayout(ClientRect, Caption, GlyphLayout, -1, 4, GlyphPos, wRect, DrawTextBiDiModeFlags(0));
  InternalDrawGlyph(GlyphPos);
  InternalDrawText(Caption, wRect, DrawTextBiDiModeFlags(0));

  if csdesigning in componentstate then
  begin
     with canvas do
     begin
        brush.Style := bsclear;
        pen.style := psDash;
        pen.color := clWindowtext;
        rectangle(clientrect);
     end;
  end;
end;

procedure TrmCustomImageListGlyph.SetCaption(const Value: string);
begin
  fCaption := Value;
  repaint;
end;

procedure TrmCustomImageListGlyph.SetGlyphLayout(const Value: TGlyphLayout);
begin
  fGLayout := Value;
  invalidate;
end;

procedure TrmCustomImageListGlyph.CalcLayout( const Client: TRect;
          const Caption: string; Layout: TGlyphLayout; Margin, Spacing: Integer;
          var GlyphPos: TPoint; var TextBounds: TRect; BiDiFlags: LongInt );
var
  TextPos: TPoint;
  ClientSize, GlyphSize, TextSize: TPoint;
  TotalSize: TPoint;
begin
  if (BiDiFlags and DT_RIGHT) = DT_RIGHT then
    if Layout = glGlyphLeft then Layout := glGlyphRight
    else
      if Layout = glGlyphRight then Layout := glGlyphLeft;
  { calculate the item sizes }
  ClientSize := Point(Client.Right - Client.Left, Client.Bottom -
    Client.Top);

  if assigned(FImages) and (fimageindex > -1) and (fimageindex < fimages.count) then
    GlyphSize := Point(fimages.Width, fimages.Height)
  else
    GlyphSize := Point(0, 0);

  if Length(Caption) > 0 then
  begin
    TextBounds := Rect(0, 0, Client.Right - Client.Left, 0);
    DrawText(Canvas.Handle, PChar(Caption), Length(Caption), TextBounds,
      DT_CALCRECT or TextFlags or BiDiFlags);
    TextSize := Point(TextBounds.Right - TextBounds.Left, TextBounds.Bottom -
      TextBounds.Top);
  end
  else
  begin
    TextBounds := Rect(0, 0, 0, 0);
    TextSize := Point(0,0);
  end;

  { If the layout has the glyph on the right or the left, then both the
    text and the glyph are centered vertically.  If the glyph is on the top
    or the bottom, then both the text and the glyph are centered horizontally.}
  if Layout in [glGlyphLeft, glGlyphRight] then
  begin
    GlyphPos.Y := (ClientSize.Y - GlyphSize.Y + 1) div 2;
    TextPos.Y := (ClientSize.Y - TextSize.Y + 1) div 2;
  end
  else
  begin
    GlyphPos.X := (ClientSize.X - GlyphSize.X + 1) div 2;
    TextPos.X := (ClientSize.X - TextSize.X + 1) div 2;
  end;

  { if there is no text or no bitmap, then Spacing is irrelevant }
  if (TextSize.X = 0) or (GlyphSize.X = 0) then
    Spacing := 0;

  { adjust Margin and Spacing }
  if Margin = -1 then
  begin
    if Spacing = -1 then
    begin
      TotalSize := Point(GlyphSize.X + TextSize.X, GlyphSize.Y + TextSize.Y);
      if Layout in [glGlyphLeft, glGlyphRight] then
        Margin := (ClientSize.X - TotalSize.X) div 3
      else
        Margin := (ClientSize.Y - TotalSize.Y) div 3;
      Spacing := Margin;
    end
    else
    begin
      TotalSize := Point(GlyphSize.X + Spacing + TextSize.X, GlyphSize.Y +
        Spacing + TextSize.Y);
      if Layout in [glGlyphLeft, glGlyphRight] then
        Margin := (ClientSize.X - TotalSize.X + 1) div 2
      else
        Margin := (ClientSize.Y - TotalSize.Y + 1) div 2;
    end;
  end
  else
  begin
    if Spacing = -1 then
    begin
      TotalSize := Point(ClientSize.X - (Margin + GlyphSize.X), ClientSize.Y -
        (Margin + GlyphSize.Y));
      if Layout in [glGlyphLeft, glGlyphRight] then
        Spacing := (TotalSize.X - TextSize.X) div 2
      else
        Spacing := (TotalSize.Y - TextSize.Y) div 2;
    end;
  end;

  case GlyphLayout of
    glGlyphLeft:
      begin
        GlyphPos.X := Margin;
        TextPos.X := GlyphPos.X + GlyphSize.X + Spacing;
      end;
    glGlyphRight:
      begin
        GlyphPos.X := ClientSize.X - Margin - GlyphSize.X;
        TextPos.X := GlyphPos.X - Spacing - TextSize.X;
      end;
    glGlyphTop:
      begin
        GlyphPos.Y := Margin;
        TextPos.Y := GlyphPos.Y + GlyphSize.Y + Spacing;
      end;
    glGlyphBottom:
      begin
        GlyphPos.Y := ClientSize.Y - Margin - GlyphSize.Y;
        TextPos.Y := GlyphPos.Y - Spacing - TextSize.Y;
      end;
  end;
  OffsetRect(TextBounds, TextPos.X + Client.Left, TextPos.Y + Client.Top);
end;

procedure TrmCustomImageListGlyph.InternalDrawGlyph(const GlyphPos: TPoint);
begin
  if Not (assigned(fimages) and (fimageindex > -1) and (fimageindex < fimages.count)) then exit;
  FImages.draw(Canvas, GlyphPos.X, GlyphPos.Y, fimageindex, enabled);
end;

procedure TrmCustomImageListGlyph.InternalDrawText(const Caption: string;
  TextBounds: TRect; BiDiFlags: LongInt);
begin
  with Canvas do
  begin
    Brush.Style := bsClear;
    if not enabled then
    begin
      OffsetRect(TextBounds, 1, 1);
      Font.Color := clBtnHighlight;
      DrawText(Handle, PChar(Caption), Length(Caption), TextBounds, TextFlags or BiDiFlags);
      OffsetRect(TextBounds, -1, -1);
      Font.Color := clBtnShadow;
      DrawText(Handle, PChar(Caption), Length(Caption), TextBounds, TextFlags or BiDiFlags);
    end
    else
    begin
      Font.Color := clBtnText;
      DrawText(Handle, PChar(Caption), Length(Caption), TextBounds, TextFlags or BiDiFlags);
    end;
  end;
end;

constructor TrmCustomImageListGlyph.create(AOwner: TComponent);
begin
  inherited;
  AutoSize := false;
  Centered := false;
end;

function TrmCustomImageListGlyph.TextFlags: integer;
begin
  Result := DT_CENTER or DT_SINGLELINE;
end;

end.
