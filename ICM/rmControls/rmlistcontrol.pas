{================================================================================
Copyright (C) 1997-2002 Mills Enterprise

Unit     : rmListControl
Purpose  : This unit was created for use in the rmDiff controls and has been
           found to be usefull in other areas.  Basically it's a listbox with
           a few interesting properties.
Date     : 06-24-2000
Author   : Ryan J. Mills
Version  : 1.90
================================================================================}

unit rmListControl;

interface

{$I CompilerDefines.INC}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, rmScrollableControl;

type
  TFormatDrawingEvent = procedure(Sender:TObject; Canvas: TCanvas; Selected: boolean; var str: string) of object;

  TrmListControl = class(TrmCustomScrollableControl)
  private
    { Private declarations }
    fItems: TStringList;
    fLongestLine: integer;
    fFormatDrawing: TFormatDrawingEvent;
    fShowFocusRect: boolean;
    fColor: tColor;
    procedure SetItems(const Value: TStringList);
    procedure ItemsChanged(Sender: TObject);

    procedure cmFOCUSCHANGED(var MSG: TMessage); message CM_FOCUSCHANGED;
    procedure cmFontChanged(var Msg:TMessage); message cm_fontchanged;
    procedure wmSize(var MSG: TWMSize); message wm_size;
    procedure wmEraseBKGrnd(var msg: tmessage); message wm_erasebkgnd;
    procedure SetLongestLineLength(const Value: integer);
    procedure SetColor(const Value: tColor);
  protected
    procedure paint; override;

    function MaxItemLength: integer; override;
    function MaxItemCount:integer; override;
    function VisibleItems: integer; override;
    function MaxItemHeight: integer; override;
    function MaxItemWidth: integer; override;
  public
    { Public declarations }
    constructor create(aowner: TComponent); override;
    destructor destroy; override;
    property VScrollPos;
    property HScrollPos;
    property VScrollSize;
    property HScrollSize;
    property LongestLineLength : integer read MaxItemLength write SetLongestLineLength;
    property SelStart;
    property SelCount;
    property ItemIndex;
  published
    property Align;
    property Anchors;
    property BorderStyle;
    property Constraints;
    property Ctl3D;
    property Color : tColor read fColor write SetColor default clWindow;
    property Font;
    property Enabled;
    property HideSelection;
    property Items: TStringList read fItems write SetItems;
    property MultiSelect;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ScrollBars;
    property ShowFocusRect;
    property ShowHint;
    property Visible;


    property OnFormatDrawing: TFormatDrawingEvent read fFormatDrawing write fFormatDrawing;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnContextPopup;
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnScroll;
  end;

implementation

uses Math, rmlibrary;

{ TrmListControl }

constructor TrmListControl.create(aowner: TComponent);
begin
  inherited;

  ControlStyle := controlstyle + [csopaque];
  height := 200;
  width := 400;
  color := clWindow;
  fLongestLine :=-1;
  fItems := TStringList.Create;
  fItems.OnChange := ItemsChanged;
end;

destructor TrmListControl.destroy;
begin
  fItems.free;
  inherited;
end;

procedure TrmListControl.ItemsChanged(Sender: TObject);
begin
  ItemIndex := 0;
end;

procedure TrmListControl.paint;
var
  lcount, loop: integer;
  wRect: TRect;
  wstr: string;
begin
  Canvas.Font.assign(font);
  Canvas.Brush.color := color;

  wRect := rect(0, 0, ClientWidth, Canvas.textheight('X'));
  if fitems.count > 0 then
  begin
    lcount := VisibleItems;
    if lcount + InternalTopIndex > fitems.Count then
      lcount := fitems.count - InternalTopIndex;
    loop := InternalTopIndex;
    while loop < InternalTopIndex + lcount do
    begin
      if not HideSelection then
      begin
         if ((loop >= SelStart) and (loop <= (SelStart+SelCount))) and (focused) then
         begin
            Canvas.brush.Color := clHighlight;
            Canvas.Font.color := clHighlightText;
         end
         else
         begin
            Canvas.brush.Color := color;
            Canvas.Font.color := font.color;
         end;
      end;

      wstr := fItems[loop];

      if Assigned(fFormatDrawing) then
        fFormatDrawing(Self, Canvas, ((loop >= SelStart) and (loop <= (SelStart+SelCount))) and not HideSelection, wstr);

      Canvas.TextRect(wRect, 2+(-HScrollPos), wRect.top, wstr);

      if Focused and fShowFocusRect and (loop = ItemIndex) then
        Canvas.DrawFocusRect(wRect);

      offsetrect(wrect, 0, canvas.textheight('X'));

      Canvas.brush.Color := color;
      Canvas.Font.color := font.color;

      inc(loop);
    end;
  end;
  wRect.Bottom := ClientHeight;
  Canvas.FillRect(wRect);
end;

procedure TrmListControl.SetItems(const Value: TStringList);
begin
  fItems.assign(Value);
end;

procedure TrmListControl.wmEraseBKGrnd(var msg: tmessage);
begin
  msg.result := 1;
end;

procedure TrmListControl.wmSize(var MSG: TWMSize);
begin
  UpdatevScrollBar;
  UpdateHScrollBar;
  inherited;
end;

procedure TrmListControl.cmFOCUSCHANGED(var MSG: TMessage);
begin
  inherited;
  invalidate;
end;

procedure TrmListControl.cmFontChanged(var Msg: TMessage);
begin
   inherited;
   Canvas.font.Assign(font);
end;

function TrmListControl.MaxItemCount: integer;
begin
   result := Items.Count;
end;

function TrmListControl.MaxItemHeight: integer;
begin
   result := canvas.TextHeight('X');
end;

function TrmListControl.MaxItemLength: integer;
var
  loop: integer;
begin
  if (fLongestLine = -1) and (fItems.count > 0) then
  begin
    for loop := 0 to fItems.count - 1 do
      fLongestLine := Max(fLongestLine, Canvas.TextWidth(fItems[loop]));
  end;
  result := fLongestLine;
end;

function TrmListControl.MaxItemWidth: integer;
begin
   result := 1;
end;

function TrmListControl.VisibleItems: integer;
begin
  result := ClientHeight div canvas.TextHeight('X');
end;

procedure TrmListControl.SetLongestLineLength(const Value: integer);
begin
   fLongestLine := value;
end;

procedure TrmListControl.SetColor(const Value: tColor);
begin
  if fColor <> value then
  begin
     fColor := Value;
     invalidate;
  end;
end;

end.

