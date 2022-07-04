unit mbXPImageListCombo;

interface

uses
  Messages, SysUtils, Classes, Controls, ExtCtrls, Windows, Graphics, StdCtrls, ImgList;

type
  TDrawItemLabelEvent = procedure (Sender: TObject; Index: integer; Font: TFont; State: TOwnerDrawState) of object;
  TmbXPImageListCombo = class(TComboBox)
  private
   FImages: TImageList;
   FAutoHeight: boolean;
   FDrawLabel: TDrawItemLabelEvent;

   procedure SetImages(i: TImageList);
   procedure SetAutoHeight(a: boolean);
  protected
   procedure DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState); override;
   procedure CreateWnd; override;
   procedure EnabledChanged;
  public
   constructor Create(AOwner: TComponent); override;
   destructor Destroy; override;
  published
   property Images: TImageList read FImages write SetImages;
   property ItemIndex;
   property AutoHeight: boolean read FAutoHeight write SetAutoHeight default true;
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

constructor TmbXPImageListCombo.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);
ParentColor := false;
 DoubleBuffered := true;
 Style := csOwnerDrawFixed;
 FAutoHeight := true;
end;

destructor TmbXPImageListCombo.Destroy;
begin
 inherited Destroy;
end;

procedure TmbXPImageListCombo.DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
 R: TRect;
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
 if FImages <> nil then
  begin
   // draw image
   R := Rect;
   R.Left := 4;
   R.Top := R.Top + ((R.Bottom - R.Top) - FImages.Height) div 2;
   R.Bottom := R.Top + FImages.Height;
   FImages.Draw(Canvas, R.Left, R.Top, Index, Enabled);
   // draw text
   Inc(R.Left, FImages.Width + 4);
   R.Right := R.Left + Canvas.TextWidth(Items.Strings[Index]);
   Canvas.Brush.Style := bsClear;
   if Assigned(FDrawLabel) then FDrawLabel(Self, Index, Canvas.Font, State);
   DrawText(Canvas.Handle, PChar(Items.Strings[Index]), Length(Items.Strings[Index]), R, DT_SINGLELINE or DT_VCENTER);
  end;
 // fire event
 if Assigned(OnDrawItem) then
  OnDrawItem(Self, Index, Rect, State);
end;

procedure TmbXPImageListCombo.CreateWnd;
begin
 inherited CreateWnd;
end;

procedure TmbXPImageListCombo.SetImages(I: TImageList);
var
 c: integer;
begin
 FImages := i;
 Items.Clear;
 if FImages <> nil then
  for c := 0 to FImages.Count - 1 do
   Items.Add(IntToStr(c));
 if FAutoHeight and (FImages <> nil) then
  ItemHeight := FImages.Height + 2;
 Invalidate;
end;

procedure TmbXPImageListCombo.SetAutoHeight(a: boolean);
begin
 FAutoHeight := a;
 if FImages <> nil then
  SetImages(FImages);
end;

procedure TmbXPImageListCombo.EnabledChanged;
begin
 inherited;
 Invalidate;
end;

end.
