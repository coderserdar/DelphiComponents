unit mbXPImageComboBox;

interface

uses
  Messages, SysUtils, Classes, Controls, Windows, Graphics, ImgList, StdCtrls;

type
  TDrawItemLabelEvent = procedure (Sender: TObject; Index: integer; Font: TFont; State: TOwnerDrawState) of object;
  TmbXPImageComboBox = class(TComboBox)
  private
   FImages: TImageList;
   FHotImages: TImageList;
   FDrawLabel: TDrawItemLabelEvent;

   procedure SetImages(i: TImageList);
   procedure SetHotImages(i: TImageList);
  protected
   procedure DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState); override;
   procedure EnabledChanged; 
  public
   constructor Create(AOwner: TComponent); override;
   destructor Destroy; override;
  published
   property Images: TImageList read FImages write SetImages;
   property HotImages: TImageList read FHotImages write SetHotImages;
   {$IFNDEF VER130}
   property AutoDropDown default False;
   property AutoCloseUp default False;
   {$ENDIF}
   property Anchors;
   property BiDiMode;
   property CharCase;
   property Color;
   property Constraints;
   property DragCursor;
   property DragKind;
   property DragMode;
   property DropDownCount;
   property Enabled;
   property Font;
   property ImeMode;
   property ImeName;
   property ItemHeight default 17;
   property ItemIndex default -1;
   property MaxLength;
   property ParentColor default false;
   property ParentFont;
   property ParentShowHint;
   property PopupMenu;
   property ShowHint;
   property Sorted;
   property TabOrder;
   property TabStop;
   property Visible;

   property OnDrawItemLabel: TDrawItemLabelEvent read FDrawLabel write FDrawLabel;
   property OnChange;
   property OnClick;
   {$IFNDEF VER130}
   property OnCloseUp;
   {$ENDIF}
   property OnContextPopup;
   property OnDblClick;
   property OnDragDrop;
   property OnDragOver;
   property OnDrawItem;
   property OnDropDown;
   property OnEndDock;
   property OnEndDrag;
   property OnEnter;
   property OnExit;
   property OnKeyDown;
   property OnKeyPress;
   property OnKeyUp;
   property OnMeasureItem;
   {$IFNDEF VER130}
   property OnSelect;
   {$ENDIF}
   property OnStartDock;
   property OnStartDrag;
   property Items;
  end;

implementation

constructor TmbXPImageComboBox.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);
 ParentColor := false;
 Style := csOwnerDrawFixed;
 ItemHeight := 17;
end;

destructor TmbXPImageComboBox.Destroy;
begin
 inherited Destroy;
end;

procedure TmbXPImageComboBox.DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState);
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
   if odSelected in State then
    begin
     if FHotImages <> nil then
      FHotImages.Draw(Canvas, Rect.Left + 4, Rect.Top + ((Rect.Bottom - Rect.Top) - FHotImages.Height) div 2, Index, true)
     else
      FImages.Draw(Canvas, Rect.Left + 4, Rect.Top + ((Rect.Bottom - Rect.Top) - FImages.Height) div 2, Index, true);
    end
   else
    FImages.Draw(Canvas, Rect.Left + 4, Rect.Top + ((Rect.Bottom - Rect.Top) - FImages.Height) div 2, Index, true);
   Inc(Rect.Left, 21);
  end
 else
  Inc(Rect.Left, 4);
 if Assigned(FDrawLabel) then FDrawLabel(Self, Index, Canvas.Font, State);
 DrawText(Canvas.Handle, PChar(Items[Index]), -1, Rect, DT_SINGLELINE or DT_VCENTER or DT_LEFT);
 if Assigned(OnDrawItem) then
  OnDrawItem(Self, Index, Rect, State);
end;

procedure TmbXPImageComboBox.SetImages(i: TImageList);
begin
 FImages := i;
 Invalidate;
end;

procedure TmbXPImageComboBox.SetHotImages(i: TImageList);
begin
 FHotImages := i;
 Invalidate;
end;

procedure TmbXPImageComboBox.EnabledChanged;
begin
 inherited;
 Invalidate;
end;

end.
