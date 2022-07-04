unit mbXPBooleanCombo;

interface

uses
  Messages, SysUtils, Classes, Controls, ExtCtrls, Windows, Graphics, StdCtrls, Themes;

type
  TDrawItemLabelEvent = procedure (Sender: TObject; Index: integer; Font: TFont; State: TOwnerDrawState) of object;
  TmbXPBooleanCombo = class(TComboBox)
  private
   FPaintHotBox: boolean;
   FDrawLabel: TDrawItemLabelEvent;

   procedure SetPaintHot(h: boolean);
  protected
   procedure DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState); override;
   procedure CreateWnd; override;
   function GetSelection: boolean;
   procedure SetSelection(s: boolean);
   procedure EnabledChanged; 
  public
   constructor Create(AOwner: TComponent); override;
   destructor Destroy; override;
  published
   property Color;
   property DragMode;
   property DragCursor;
   property Enabled;
   property Font;
   property ItemHeight;
   property MaxLength;
   property ParentFont;
   property ParentColor default false;
   property ParentShowHint;
   property PopupMenu;
   property ShowHint;
   property TabOrder;
   property TabStop;
   property Visible;
   property PaintHotBox: boolean read FPaintHotBox write SetPaintHot default false;
   property Value: boolean read GetSelection write SetSelection default true;

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

constructor TmbXPBooleanCombo.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);
ParentColor := false;
 DoubleBuffered := true;
 Style := csOwnerDrawFixed;
 FPaintHotBox := false;
end;

destructor TmbXPBooleanCombo.Destroy;
begin
 inherited Destroy;
end;

function TmbXPBooleanCombo.GetSelection: Boolean;
begin
 Result := (Items.Strings[ItemIndex] = 'True');
end;

procedure TmbXPBooleanCombo.SetSelection(s: boolean);
begin
 if s then
  ItemIndex := 0
 else
  ItemIndex := 1;
end;

procedure TmbXPBooleanCombo.DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
 R, IR: TRect;
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
 // set checkmark rect
 R := Rect;
 Inc(R.Left, 1);
 Inc(R.Top, 1);
 Dec(R.Bottom, 1);
 R.Right := R.Left + (R.Bottom - R.Top);
 InflateRect(R, -1, -1);
 if ThemeServices.ThemesEnabled then
  begin
   if Enabled then
    case Index of
     0: ThemeServices.DrawElement(Canvas.Handle, ThemeServices.GetElementDetails(tbCheckBoxCheckedNormal), R);
     1: ThemeServices.DrawElement(Canvas.Handle, ThemeServices.GetElementDetails(tbCheckBoxUncheckedNormal), R);
    end
   else
    ThemeServices.DrawElement(Canvas.Handle, ThemeServices.GetElementDetails(tbCheckBoxUncheckedNormal), R);
  end
 else
  if Enabled then
    case Index of
     0: DrawFrameControl(Canvas.Handle, R, DFC_BUTTON, DFCS_BUTTONCHECK or DFCS_CHECKED);
     1: DrawFrameControl(Canvas.Handle, R, DFC_BUTTON, DFCS_BUTTONCHECK);
    end
   else
    DrawFrameControl(Canvas.Handle, R, DFC_BUTTON, DFCS_BUTTONCHECK or DFCS_INACTIVE);
 // draw text
 Canvas.Brush.Style := bsClear;
 IR := Rect;
 IR.Left := R.Right + 6;
 if Assigned(FDrawLabel) then FDrawLabel(Self, Index, Canvas.Font, State);
 DrawText(Canvas.Handle, PChar(Items.Strings[index]), Length(Items.Strings[index]), IR, DT_VCENTER or DT_SINGLELINE);
 if Assigned(OnDrawItem) then
  OnDrawItem(Self, Index, Rect, State);
end;

procedure TmbXPBooleanCombo.CreateWnd;
begin
 inherited CreateWnd;
 Items.Clear;
 Items.Add('True');
 Items.Add('False');
 ItemIndex := 0;
end;

procedure TmbXPBooleanCombo.SetPaintHot(h: boolean);
begin
 FPaintHotBox := h;
 Invalidate;
end;

procedure TmbXPBooleanCombo.EnabledChanged;
begin
 inherited;
 Invalidate;
end;

end.
