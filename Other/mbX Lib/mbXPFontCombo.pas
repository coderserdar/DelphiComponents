unit mbXPFontCombo;

interface

uses
  Messages, SysUtils, Classes, Controls, ExtCtrls, Windows, Graphics, StdCtrls, Forms;

type
  TDrawItemLabelEvent = procedure (Sender: TObject; Index: integer; Font: TFont; State: TOwnerDrawState) of object;
  TmbXPFontCombo = class(TComboBox)
  private
   FGlyph: TBitmap;
   FPreview: boolean;
   FSelected: TFontName;
   FDrawLabel: TDrawItemLabelEvent;

   procedure SetPreview(p: boolean);
   procedure SetSelected(s: TFontName);
  protected
   procedure DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState); override;
   procedure CreateWnd; override;
   procedure FontChanged(Sender: TObject);
   procedure EnabledChanged;
  public
   constructor Create(AOwner: TComponent); override;
   destructor Destroy; override;
  published
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
   property Font;
   property ShowPreview: boolean read FPreview write SetPreview default true;
   property Selected: TFontName read FSelected write SetSelected;
   property Visible;

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

{$R FCDefGlyph.res}

constructor TmbXPFontCombo.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);
ParentColor := false;
 DoubleBuffered := true;
 Style := csOwnerDrawFixed;
 FGlyph := TBitmap.Create;
 FGlyph.Handle := LoadBitmap(HInstance, 'DEFAULTGLYPH');
 FPreview := true;
 Font.OnChange := FontChanged;
end;

destructor TmbXPFontCombo.Destroy;
begin
 FGlyph.Free;
 inherited Destroy;
end;

procedure TmbXPFontCombo.DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
 R, IR: TRect;
 NewFont: TFont;
begin
 Canvas.Font := Font;
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
 // set icon rect
 R := Rect;
 R.Left := 4;
 R.Right := R.Left + FGlyph.Width;
 R.Top := R.Top + ((R.Bottom - R.Top) - FGlyph.Height) div 2;
 R.Bottom := R.Top + FGlyph.Height;
 FGlyph.Transparent := true;
 Canvas.Draw(R.Left, R.Top, FGlyph);
 //DrawGlyph(Canvas.Handle, R.Left, R.Top);
 // draw text
 Canvas.Brush.Style := bsClear;
 Canvas.Font := Font;
 if FPreview then
  begin
   NewFont := TFont.Create;
   Canvas.Font := NewFont; // needed to fix bug when some symbol fonts are used (wingdings, webdings, ...)
   Canvas.Font.Name := Items.Strings[Index];
   NewFont.Free;
  end;
 IR := Rect;
 IR.Left := R.Right + 4;
 Canvas.Font.Color := clWindowText;
 if odSelected in State then
  Canvas.Font.Color := clHighlightText;
 if Assigned(FDrawLabel) then FDrawLabel(Self, Index, Canvas.Font, State);
 DrawText(Canvas.Handle, PChar(Items.Strings[index]), Length(Items.Strings[index]), IR, DT_VCENTER or DT_SINGLELINE);
 if Assigned(OnDrawItem) then
  OnDrawItem(Self, Index, Rect, State);
end;

procedure TmbXPFontCombo.CreateWnd;
var
 c: integer;
begin
 inherited CreateWnd;
 Items.Clear;
 Screen.ResetFonts; 
 for c:=0 to Screen.Fonts.Count - 1 do
  Items.Add(Screen.Fonts.Strings[c]);
 SetSelected('Times New Roman');
end;

procedure TmbXPFontCombo.SetPreview(p: boolean);
begin
 FPreview := p;
 Invalidate;
end;

procedure TmbXPFontCombo.SetSelected(s: TFontName);
var
 i: integer;
begin
 FSelected := s;
 if s = '' then
  ItemIndex := -1
 else
  for i := 0 to Items.Count - 1 do
   if SameText(s, Items.Strings[i]) then
    begin
     ItemIndex := i;
     Exit;
    end;
end;

procedure TmbXPFontCombo.FontChanged(Sender: TObject);
begin
 if not FPreview then
  Invalidate;
end;

procedure TmbXPFontCombo.EnabledChanged;
begin
 inherited;
 Invalidate;
end;

end.
