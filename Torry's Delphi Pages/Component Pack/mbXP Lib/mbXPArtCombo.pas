unit mbXPArtCombo;

interface

uses
  Messages, SysUtils, Classes, Controls, ExtCtrls, Windows, Graphics, StdCtrls, ImgList;

type
  TDrawItemLabelEvent = procedure (Sender: TObject; Index: integer; Font: TFont; State: TOwnerDrawState) of object;
  TmbXPArtCombo = class(TComboBox)
  private
   FImages: TImageList;
   FAutoHeight: boolean;
   FNone: string;
   FDrawLabel: TDrawItemLabelEvent;

   procedure SetImages(i: TImageList);
   procedure SetAutoHeight(a: boolean);
   procedure SetNone(n: string);
  protected
   procedure DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState); override;
   procedure CreateWnd; override;
   procedure EnabledChanged;
  public
   constructor Create(AOwner: TComponent); override;
   destructor Destroy; override;
  published
   property Images: TImageList read FImages write SetImages;
   property ItemIndex default 0;
   property AutoHeight: boolean read FAutoHeight write SetAutoHeight default true;
   property NoneText: string read FNone write SetNone;
   property Color;
   property DragMode;
   property DragCursor;
   property DropDownCount;
   property Enabled;
   property ItemHeight;
   property MaxLength;
   property ParentColor default false;
   property ParentFont;
   property ParentShowHint;
   property PopupMenu;
   property ShowHint;
   property TabOrder;
   property TabStop;
   property Visible;
   property Font;

   property OnDrawItemLabel: TDrawItemLabelEvent read FDrawLabel write FDrawLabel;
   property OnChange;
   property OnClick;
   property OnDblClick;
   property OnDragDrop;
   property OnDragOver;
   property OnDrawItem;
   property OnDropDown;
   property OnEndDrag;
   property OnEnter;
   property OnExit;
   property OnKeyDown;
   property OnKeyPress;
   property OnKeyUp;
   property OnMeasureItem;
   property OnStartDrag;
  end;

implementation

constructor TmbXPArtCombo.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);
ParentColor := false;
 DoubleBuffered := true;
 Style := csOwnerDrawFixed;
 FAutoHeight := true;
 FNone := '-- none --';
end;

destructor TmbXPArtCombo.Destroy;
begin
 inherited Destroy;
end;

procedure TmbXPArtCombo.DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
 R: TRect;
 w, i: integer;
begin
 Canvas.Font := Font;
 Canvas.Brush.Style := bsSolid;
 if odSelected in State then
  begin
   Canvas.Brush.Color := clHighlight;
   Canvas.Pen.Color := clHighlightText;
   Canvas.Font.Color := clHighlightText;
  end
 else
  begin
   Canvas.Brush.Color := Color;
   Canvas.Pen.Color := clWindowText;
   Canvas.Font.Color := clWindowText;
  end;
 Canvas.FillRect(Rect);
 if Index = 0 then
  begin
   Canvas.Brush.Style := bsClear;
   if Assigned(FDrawLabel) then FDrawLabel(Self, Index, Canvas.Font, State);
   DrawText(Canvas.Handle, PChar(FNone), Length(FNone), Rect, DT_CENTER or DT_VCENTER or DT_SINGLELINE);
  end;
 Canvas.Brush.Style := bsSolid;
 if (Index > 0) and (FImages <> nil) then
  begin
   // set icon rect
   R := Rect;
   w := R.Right - R.Left;
   if DropDownCount < Items.Count then
    Dec(w, GetSystemMetrics(SM_CXVSCROLL));
   R.Left := ROUND((w - ((w div (FImages.Width + 2)) * FImages.Width)) / 2);
   R.Top := R.Top + ((R.Bottom - R.Top) - FImages.Height) div 2;
   R.Bottom := R.Top + FImages.Height;
   // draw images
   for i := 0 to (w div (FImages.Width + 2)) - 1 do
    begin
     FImages.Draw(Canvas, R.Left, R.Top, Index, Enabled);
     Inc(R.Left, FImages.Width + 2);
    end;
  end;
 // fire event
 if Assigned(OnDrawItem) then
  OnDrawItem(Self, Index, Rect, State);
end;

procedure TmbXPArtCombo.CreateWnd;
begin
 inherited CreateWnd;
 ItemIndex := 0;
end;

procedure TmbXPArtCombo.SetImages(I: TImageList);
var
 c: integer;
begin
 FImages := i;
 Items.Clear;
 if FImages <> nil then
  for c := 0 to FImages.Count - 1 do
   Items.Add('');
 if FAutoHeight and (FImages <> nil) then
  ItemHeight := FImages.Height + 2;
 Invalidate;
 ItemIndex := 0;
end;

procedure TmbXPArtCombo.SetAutoHeight(a: boolean);
begin
 FAutoHeight := a;
 if FImages <> nil then
  SetImages(FImages);
end;

procedure TmbXPArtCombo.SetNone(n: string);
begin
 FNone := n;
 if FImages <> nil then
  SetImages(FImages);
end;

procedure TmbXPArtCombo.EnabledChanged;
begin
 inherited;
 Invalidate;
end;

end.
