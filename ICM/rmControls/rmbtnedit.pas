{================================================================================
Copyright (C) 1997-2002 Mills Enterprise

Unit     : rmBtnEdit
Purpose  : An edit control with a combo type button (or two).  Also used as a
           basis for a couple of the "rm" combo boxes.
Date     : 03-15-1999
Author   : Ryan J. Mills
Version  : 1.90
================================================================================}

unit rmBtnEdit;

interface

{$I CompilerDefines.INC}

uses Windows, Classes, StdCtrls, Controls, Messages, SysUtils,
  Forms, Graphics, Buttons, rmSpeedBtns, rmBaseEdit;

type

{ TrmCustomBtnEdit }

  TrmCustomBtnEdit = class(TrmCustomEdit)
  private
    fundo : string;
    FButton1, FButton2: TrmSpeedButton;
    fOnBtn1Click,
    fOnBtn2Click : TNotifyEvent;
    FEditorEnabled: Boolean;
    FBtnWidth : integer;
    fBtn2IsVisible,
    fBtn1IsEnabled,
    fBtn2IsEnabled : boolean;
    fBtn1DefaultGlyph,
    fBtn2DefaultGlyph : boolean;
    fUseDefaultGlyphs : boolean;
    procedure SetEditRect;
    procedure SetBtnWidth(value:integer);
    function GetBtn1Enabled: boolean;
    function GetBtn1Glyph:TBitMap;
    function GetBtn1NumGlyphs:TNumGlyphs;
    function GetBtn1Visible:boolean;
    function GetBtn2Enabled: boolean;
    function GetBtn2Glyph:TBitMap;
    function GetBtn2NumGlyphs:TNumGlyphs;
    function GetBtn2Visible: boolean;
    procedure SetBtn1Enabled(const Value: boolean);
    procedure SetBtn1Glyph(value:TBitMap);
    procedure SetBtn1NumGlyphs(value:TNumGlyphs);
    procedure SetBtn1Visible(value:boolean);
    procedure SetBtn2Enabled(const Value: boolean);
    procedure SetBtn2Glyph(value:TBitMap);
    procedure SetBtn2NumGlyphs(value:TNumGlyphs);
    procedure SetBtn2Visible(const Value: boolean);
    procedure ResetDefaultGlyphs;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure CMEnter(var Message: TCMGotFocus); message CM_ENTER;
    procedure WMPaste(var Message: TWMPaste);   message WM_PASTE;
    procedure WMCut(var Message: TWMCut);   message WM_CUT;
    procedure CMSysColorChange(var Message:TMessage); message CM_SYSCOLORCHANGE;
    {$ifdef D4_OR_HIGHER}
    procedure SetEnabled(value:Boolean); reintroduce; (* reintroduce is D4 Modification *)
    function GetEnabled:Boolean; reintroduce; (* reintroduce is D4 Modification *)
    {$else}
    procedure SetEnabled(value:Boolean);
    function GetEnabled:Boolean;
    {$endif}
    procedure SetUseDefaultGlyphs(const Value: boolean);
  protected
    procedure BtnClick (Sender: TObject); virtual;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;

    function GetButton(index:integer):TrmSpeedButton;
    property BtnWidth:integer read FBtnWidth write setBtnWidth stored true default 16;
    property Btn1Enabled : boolean read GetBtn1Enabled write SetBtn1Enabled stored true default true;
    property Btn1Glyph:TBitmap read GetBtn1Glyph write SetBtn1Glyph stored True;
    property Btn1NumGlyphs: TNumGlyphs read GetBtn1NumGlyphs write SetBtn1NumGlyphs stored true;
    property Btn1Visible:boolean read GetBtn1Visible write SetBtn1Visible stored true default true;
    property Btn2Enabled : boolean read GetBtn2Enabled write SetBtn2Enabled stored true default true;
    property Btn2Glyph:TBitmap read GetBtn2Glyph write SetBtn2Glyph stored True;
    property Btn2NumGlyphs: TNumGlyphs read GetBtn2NumGlyphs write SetBtn2NumGlyphs stored true;
    Property Btn2Visible:boolean read GetBtn2Visible write SetBtn2Visible stored true default false;
    property UseDefaultGlyphs : boolean read fUseDefaultGlyphs write SetUseDefaultGlyphs default true;
    property EditorEnabled: Boolean read FEditorEnabled write FEditorEnabled default True;
    property Enabled: Boolean read GetEnabled write SetEnabled default True;
    property OnBtn1Click:TNotifyEvent read fOnBtn1Click write fOnBtn1Click;
    property OnBtn2Click:TNotifyEvent read fOnBtn2Click write fOnBtn2Click;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetFocus; override;
  end;

  TrmBtnEdit = class(TrmCustomBtnEdit)
  published
    property Align; 
    {$ifdef D4_OR_HIGHER}
    property Anchors;
    property Constraints;
    {$endif}
    property AutoSelect;
    property AutoSize;
    property BtnWidth;
    property Btn1Enabled;
    property Btn1Glyph;
    property Btn1NumGlyphs;
    property Btn1Visible;
    property Btn2Enabled;
    property Btn2Glyph;
    property Btn2NumGlyphs;
    Property Btn2Visible;
    property BorderStyle;
    property Color;
    property Ctl3D;
    property DragCursor;
    property DragMode;
    property EditorEnabled;
    property Enabled;
    property Font;
    property MaxLength;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
    property OnBtn1Click;
    property OnBtn2Click;
  end;

implementation

{$R rmBtnEdit.res}

uses rmLibrary;

{ TrmCustomBtnEdit }

constructor TrmCustomBtnEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  fUseDefaultGlyphs := true;

  FButton1 := TrmSpeedButton.Create (Self);
  with FButton1 do
  begin
     Visible := True;
     Style := sbsComboButton;
     Cursor := crArrow;
     Parent := Self;
     Align := alRight;
     OnClick := BtnClick;
     enabled := true;
     Layout := blGlyphTop;
  end;
  fBtn1IsEnabled := true;
  fBtn1DefaultGlyph := true;
  Btn1Glyph := nil;

  FButton2 := TrmSpeedButton.Create (Self);
  with FButton2 do
  begin
     Visible := false;
     Style := sbsComboButton;
     Cursor := crArrow;
     Parent := Self;
     Align := alRight;
     OnClick := BtnClick;
     Layout := blGlyphTop;
     enabled := true;
  end;
  fBtn2IsEnabled := true;
  fBtn2IsVisible := false;
  fBtn2DefaultGlyph := true;
  Btn2Glyph := nil;

  BtnWidth := 16;
  Text := '';
  ControlStyle := ControlStyle - [csSetCaption];
  FEditorEnabled := True;
end;

destructor TrmCustomBtnEdit.Destroy;
begin
  FButton1.free; // := nil;
  FButton2.free; // := nil;
  inherited Destroy;
end;

procedure TrmCustomBtnEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_RETURN) then
  begin
       if ([ssCtrl] = Shift) then BtnClick (FButton1)
       else
       if ([ssctrl, ssShift] = Shift) then BtnClick (FButton2)
       else
       if (shift = []) then
       begin
            setfocus;
            inherited KeyDown(key, shift);
       end;
  end
  else
  if (key = vk_escape) then
  begin
       if (shift = []) and (text <> fundo) then
       begin
            text := fundo;
            selectall;
       end;
  end
  else
  inherited KeyDown(Key, Shift);
end;

procedure TrmCustomBtnEdit.setfocus;
begin
     fundo := text;
     inherited;
end;

procedure TrmCustomBtnEdit.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
{  Params.Style := Params.Style and not WS_BORDER;  }
  Params.Style := Params.Style or WS_CLIPCHILDREN or ES_MULTILINE;
end;

procedure TrmCustomBtnEdit.CreateWnd;
begin
  inherited CreateWnd;
  SetEditRect;
end;

procedure TrmCustomBtnEdit.SetEditRect;
var
  R: TRect;
begin
  SendMessage(Handle, EM_GETRECT, 0, LongInt(@R));
  if FButton1.visible then
     R.Right := ClientWidth - fBtnWidth - 1
  else
     R.right := ClientWidth;

  if FButton2.visible then
     R.Right := R.right - fBtnWidth;

  R.Top := 0;
  R.Left := 0;
  SendMessage(Handle, EM_SETRECTNP, 0, LongInt(@R));
  SendMessage(Handle, EM_GETRECT, 0, LongInt(@R));  {debug}
end;

procedure TrmCustomBtnEdit.WMSize(var Message: TWMSize);
begin
  inherited;
  if NewStyleControls and Ctl3D then
  begin
       if fButton2.Visible then
       begin
            FButton1.SetBounds((width - (fBtnWidth shl 1)) - 4, 0, fBtnWidth, Height - 4);
            FButton2.SetBounds((width - fBtnWidth) - 4, 0, fBtnWidth, Height - 4);
       end
       else
           FButton1.SetBounds((width - fBtnWidth) - 4, 0, fBtnWidth, Height - 4);
  end
  else
  begin
       if fButton2.Visible then
       begin
            FButton1.SetBounds (width - (fBtnWidth shl 1), 1, fBtnWidth, Height - 2);
            FButton2.SetBounds (width - fBtnWidth, 1, fBtnWidth, Height - 2);
       end
       else
           FButton1.SetBounds (width - fBtnWidth, 1, fBtnWidth, Height - 2);
  end;
  if csdesigning in componentstate then
  begin
       if not fbutton1.visible then fbutton1.width := 0;
       if not fbutton2.visible then fbutton2.width := 0;
  end;
  SetEditRect;
end;

procedure TrmCustomBtnEdit.BtnClick (Sender: TObject);
begin
     SetFocus;
     if (Sender is TrmSpeedButton) and (TrmSpeedButton(Sender) = fbutton1) and assigned(fOnBtn1Click) then
        fOnBtn1Click(self)
     else
     if (Sender is TrmSpeedButton) and (TrmSpeedButton(Sender) = fbutton2) and assigned(fOnBtn2Click) then
        fOnBtn2Click(self)
end;

procedure TrmCustomBtnEdit.WMPaste(var Message: TWMPaste);
begin
  if not FEditorEnabled or ReadOnly then Exit;
  inherited;
end;

procedure TrmCustomBtnEdit.WMCut(var Message: TWMPaste);   
begin
  if not FEditorEnabled or ReadOnly then Exit;
  inherited;
end;

procedure TrmCustomBtnEdit.CMEnter(var Message: TCMGotFocus);
begin
  if AutoSelect and not (csLButtonDown in ControlState) then
    SelectAll;
  inherited;
end;

procedure TrmCustomBtnEdit.SetBtn1Glyph(value:TBitMap);
var
   bmp : TBitmap;
begin
   FButton1.glyph := value;
   fBtn1DefaultGlyph := false;
   if fUseDefaultGlyphs and (value = nil) then
   begin
      fBtn1DefaultGlyph := true;
      bmp := tbitmap.create;
      try
         bmp.LoadFromResourceName(HInstance,'RM_ELLIPSIS');
         ReplaceColors(bmp, clBtnFace, clBtnText);
         fButton1.Glyph.Assign(bmp);
      finally
         bmp.free;
      end;
   end;
end;

function TrmCustomBtnEdit.GetBtn1Glyph:TBitMap;
begin
     result := FButton1.glyph;
end;

procedure TrmCustomBtnEdit.SetBtn2Glyph(value:TBitMap);
var
   bmp : TBitmap;
begin
     FButton2.glyph := value;
     fBtn2DefaultGlyph := false;

     if fUseDefaultGlyphs and (value = nil) then
     begin
        fBtn2DefaultGlyph := true;

        bmp := tbitmap.create;
        try
           bmp.LoadFromResourceName(HInstance,'RM_ELLIPSIS');
           ReplaceColors(bmp, clBtnFace, clBtnText);
           fButton2.Glyph.Assign(bmp);
        finally
           bmp.free;
        end;
     end;
end;

function TrmCustomBtnEdit.GetBtn2Glyph:TBitMap;
begin
     result := FButton2.glyph;
end;

procedure TrmCustomBtnEdit.SetBtn1Visible(value:boolean);
begin
   FButton1.visible := value;
   fButton2.Visible := FBtn2IsVisible and fButton1.visible;
   if fButton2.Visible then
      fButton2.Left := fButton1.left+1;
   recreatewnd;
end;

function TrmCustomBtnEdit.GetBtn1Visible:boolean;
begin
     result := FButton1.visible;
end;

procedure TrmCustomBtnEdit.SetEnabled(value:Boolean);
begin
     inherited enabled := value;
     FButton1.enabled := fBtn1IsEnabled and value;
     FButton2.Enabled := fBtn2IsEnabled and value;
end;

procedure TrmCustomBtnEdit.KeyPress(var Key: Char);
begin
     if key in [#10,#13] then key := #0;
     inherited;
end;

function TrmCustomBtnEdit.GetEnabled:Boolean;
begin
     result := inherited Enabled;
end;

procedure TrmCustomBtnEdit.SetBtnWidth(value:integer);
begin
     if value <> FBtnWidth then
     begin
          FBtnWidth := value;
          FButton1.width := FBtnWidth;
          FButton2.Width := FBtnWidth;
          recreatewnd;
     end;
end;

procedure TrmCustomBtnEdit.SetBtn2Visible(const Value: boolean);
begin
     fBtn2IsVisible := Value;
     fButton2.Visible := FBtn2IsVisible and FButton1.visible;
     if fButton2.Visible then
        fButton2.Left := fButton1.left+1;
     if csdesigning in componentstate then
        fButton2.Visible := FBtn2IsVisible and FButton1.visible;
     recreatewnd;
end;

function TrmCustomBtnEdit.GetBtn2Visible: boolean;
begin
     result := fBtn2IsVisible;
end;

function TrmCustomBtnEdit.GetBtn1Enabled: boolean;
begin
     result := fBtn1IsEnabled;
end;

function TrmCustomBtnEdit.GetBtn2Enabled: boolean;
begin
     result := fBtn2IsEnabled;
end;

procedure TrmCustomBtnEdit.SetBtn1Enabled(const Value: boolean);
begin
     fbutton1.enabled := enabled and value;
     fBtn1IsEnabled := value;
end;

procedure TrmCustomBtnEdit.SetBtn2Enabled(const Value: boolean);
begin
     fbutton2.enabled := enabled and value;
     fBtn2IsEnabled := value;
end;

function TrmCustomBtnEdit.GetBtn1NumGlyphs: TNumGlyphs;
begin
     result := fbutton1.NumGlyphs;
end;

function TrmCustomBtnEdit.GetBtn2NumGlyphs: TNumGlyphs;
begin
     result := fbutton2.NumGlyphs;
end;

procedure TrmCustomBtnEdit.SetBtn1NumGlyphs(value: TNumGlyphs);
begin
     FButton1.numglyphs := value;
end;

procedure TrmCustomBtnEdit.SetBtn2NumGlyphs(value: TNumGlyphs);
begin
     FButton2.numGlyphs := value;
end;

function TrmCustomBtnEdit.GetButton(index: integer): TrmSpeedButton;
begin
     case index of
          1: result := FButton1;
          2: result := FButton2;
     else
         result := nil;
     end;
end;

procedure TrmCustomBtnEdit.CMSysColorChange(var Message: TMessage);
begin
   ResetDefaultGlyphs;  
end;

procedure TrmCustomBtnEdit.SetUseDefaultGlyphs(const Value: boolean);
begin
  fUseDefaultGlyphs := Value;
  ResetDefaultGlyphs;
end;

procedure TrmCustomBtnEdit.ResetDefaultGlyphs;
begin
   if fBtn1DefaultGlyph then
      SetBtn1Glyph(nil);

   if fBtn2DefaultGlyph then
      SetBtn2Glyph(nil);
end;

end.
