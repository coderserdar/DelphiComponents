{================================================================================
Copyright (C) 1997-2002 Mills Enterprise

Unit     : rmDayView
Purpose  : This is a very basic PIM component.
Date     : 04-14-2002
Author   : Ryan J. Mills
Version  : 1.90
================================================================================}

unit rmDayView;

interface

{$I CompilerDefines.INC}

uses windows, Messages, controls, classes, graphics, extctrls, menus,
     forms, stdctrls, sysutils, rmLibrary, rmLabel, rmScrollableControl, math;

const
  wm_PostedUpdate = wm_user+501;
  wm_DayViewItemDBLClicked = wm_user+502;
  wm_DayViewItemClicked = wm_User+503;

  wm_UpdateOverlaps = wm_user+504;
  wm_UpdateTops = wm_user+505;
  wm_updateHeights = wm_user+506;
  wm_updateHints = wm_user+507;
  wm_updateAll = wm_user+508;

  HintStr = '%s at %s for %d minute(s)'#13#10#13#10'%s';

type
  TrmHourBreakdown = (hb5Minutes, hb10Minutes, hb15Minutes, hb20Minutes, hb30Minutes, hb1Hour);
  TrmPercent = 1..100;

  TrmDayViewCollection = class;
  TrmCustomDayView = class;
  TrmDayView = class;
  TrmDayViewCollectionItem = class;

  TrmCustomDayViewItem = class(TCustomControl)
  private
    fCollectionItem : TrmDayViewCollectionItem;
    fLabel: TrmLabel; 
    fNote: TrmLabel;
    fStartTime: TTime;
    fDayView: TrmDayView;
    fDuration: integer;
    fItemDesc : string;
    function GetItemColor: TColor;
    procedure SetItemColor(const Value: TColor);
    function GetCaption: TCaption;
    procedure SetCaption(const Value: TCaption);
    procedure SetStartTime(const Value: TTime);
    procedure SetDayView(const Value: TrmDayView);
    procedure SetDuration(const Value: integer);
    function GetPopup: TPopupMenu;
    procedure SetPopup(const Value: TPopupMenu);
    function GetItemFont: TFont;
    procedure SetItemFont(const Value: TFont);
    function Getcolor: TColor;
    procedure SetColor(const Value: TColor);
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus(var Message: TWMSetFocus); message WM_KILLFOCUS;
    procedure wmupdateoverlaps(var message:Tmessage); message wm_UpdateOverlaps;
    procedure wmupdatetops(var message:Tmessage); message wm_updateTops;
    procedure wmupdateheights(var message:tmessage); message wm_updateheights;
    procedure wmupdateall(var message:tmessage); message wm_updateall;

    function GetItemDesc: string;
    procedure SetItemDesc(const Value: string);
  protected
    procedure HandleSetFocus;
    procedure DoClick(sender : TObject);
    procedure DoDblClick(Sender : TObject);
    property Font : TFont read GetItemFont write SetItemFont;
    property ItemColor : TColor read GetItemColor write SetItemColor default clBlue;
    property ItemDescription : string read GetItemDesc write SetItemDesc;
    property Color : TColor read Getcolor write SetColor;
    property Caption : TCaption read GetCaption write SetCaption;
    property StartTime : TTime read fStartTime write SetStartTime;
    property Duration : integer read fDuration write SetDuration;
    property DayView : TrmDayView read fDayView write SetDayView;
    property PopupMenu : TPopupMenu read GetPopup write SetPopup;
    property CollectionItem : TrmDayViewCollectionItem read fCollectionItem write fCollectionItem;
    procedure DoIntMouseEnter(sender:TObject);
    procedure DoIntMouseLeave(sender:TObject);
  public
    constructor create(AOwner:TComponent); override;
    procedure UpdateTop;
    procedure UpdateHeight;
    procedure UpdateOverlap;
    procedure UpdateHint;
  published
  end;

  TrmDayViewItem = class(TrmCustomDayViewItem)
  published
     property Caption;
     property Color;
     property Font;
     property DayView;
     property Duration;
     property ItemColor;
     property ItemDescription;
     property PopupMenu;
     property StartTime;
     property OnClick;
     property OnDblClick;
  end;

  TrmDayViewCollectionItem = class(TCollectionItem)
  private
    fDayViewItem : TrmDayViewItem;
    fTag: integer;
    function GetCaption: TCaption;
    function Getcolor: TColor;
    function GetDuration: integer;
    function GetItemColor: TColor;
    function GetItemFont: TFont;
    function GetPopup: TPopupMenu;
    function GetStartTime: TTime;
    procedure SetCaption(const Value: TCaption);
    procedure SetColor(const Value: TColor);
    procedure SetDuration(const Value: integer);
    procedure SetItemColor(const Value: TColor);
    procedure SetItemFont(const Value: TFont);
    procedure SetPopup(const Value: TPopupMenu);
    procedure SetStartTime(const Value: TTime);
    function GetShowHint: boolean;
    procedure SetShowHint(const Value: boolean);
    function GetItemDesc: string;
    procedure SetItemDesc(const Value: string);
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure UpdateViewDetails;
    procedure UpdateTop;
    procedure UpdateOverlap;
  published
    property Font : TFont read GetItemFont write SetItemFont;
    property ItemColor : TColor read GetItemColor write SetItemColor;
    property ItemDescription : string read GetItemDesc write SetItemDesc;
    property Color : TColor read Getcolor write SetColor;
    property Caption : TCaption read GetCaption write SetCaption;
    property StartTime : TTime read GetStartTime write SetStartTime;
    property Duration : integer read GetDuration write SetDuration;
    property PopupMenu : TPopupMenu read GetPopup write SetPopup;
    property ShowHint : boolean read GetShowHint write SetShowHint;
    property Tag : integer read fTag write fTag default 0;
  end;

  TrmDayViewCollection = class(TCollection)
  private
    FDayView: TrmDayView;
    FOnUpdate: TNotifyEvent;
    function GetItem(Index: Integer): TrmDayViewCollectionItem;
    procedure SetItem(Index: Integer; Value: TrmDayViewCollectionItem);
  protected
    function GetOwner: TPersistent; override;
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(DayView: TrmCustomDayView);
    function Add: TrmDayViewCollectionItem;
    procedure Delete(Index: Integer);
    function Insert(Index: Integer): TrmDayViewCollectionItem;
    property Items[Index: Integer]: TrmDayViewCollectionItem read GetItem write SetItem; default;
    property OnCollectionUpdate: TNotifyEvent read fOnUpdate write fOnUpdate;
  end;

  TrmCustomDayView = class(TrmCustomScrollableControl)
  private
    fItems : TrmDayViewCollection;
    fRowHeight:integer;
    fHourlyBreakdown: TrmHourBreakDown;

    fDrawBuffer : TBitmap;
    fHourColWidth: integer;
    fDayStart: TTime;
    fDayEnd: TTime;
    fSelColor: tcolor;
    fColorDiff: TrmPercent;
    fOnItemClick: TNotifyEvent;
    fOnItemDblClick: TNotifyEvent;
    fSelectedItem: TrmDayViewCollectionItem;
    fSelItemChange: TNotifyEvent;

    procedure SetHourlyBreakdown(const Value: TrmHourBreakdown);
    procedure SetHourColWidth(const Value: integer);
    procedure setDayEnd(const Value: TTime);
    procedure SetDayStart(const Value: TTime);

    procedure SetSelColor(const Value: tcolor);
    function GetItemTime: TTime;
    procedure SetItemTime(const Value: TTime);
    procedure SetColorDiff(const Value: TrmPercent);
    function GetSelTimeCount: Integer;
    function GetSelTimeStart: TTime;
    procedure SetSelTimeCount(const Value: Integer);
    procedure SetSelTimeStart(const Value: TTime);
    procedure SetItems(const Value: TrmDayViewCollection);
    function GetTopIndex: integer;
    function GetTopTime: TTime;
    procedure SetTopIndex(const Value: integer);
    procedure SetTopTime(const Value: TTime);
  protected
    procedure CMBorderChanged(var Message: TMessage); message CM_BORDERCHANGED;
    procedure CMFontChanged(var Message: TMessagE); message cm_FontChanged;
    procedure CMFocusChanged(var Message: TCMFocusChanged); message CM_FOCUSCHANGED;
    procedure cmColorChanged(var Message:TMessage); message cm_colorchanged;
    procedure wmSize(var msg: twmSize); message wm_size;
    procedure wmEraseBKGrnd(var msg: tmessage); message wm_erasebkgnd;
    procedure wmDayViewItemDBLClicked(var msg:Tmessage); message wm_DayViewItemDBLClicked;
    procedure wmDayViewItemClicked(var msg:Tmessage); message wm_DayViewItemClicked;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure Paint; override;
    procedure Loaded; override;
    procedure DoItemClick(sender:TObject);
    procedure DoItemDblclick(sender:TObject);
    procedure DoItemsUpdate(sender:TObject);
    procedure SetSelectedItem(item : TrmDayViewCollectionItem);
    function IsTimeModIndex(aTime:TTime): boolean;
    procedure DoItemIndexChange; override;

    property HourlyBreakdown : TrmHourBreakdown read fHourlyBreakdown write SetHourlyBreakdown default hb30Minutes;
    property Color default clSkyBlue;
    property SelectedColor : tcolor read fSelColor write SetSelColor default clHighlight;
    property HourColWidth : integer read fHourColWidth write SetHourColWidth default 50;
    property OnItemClick : TNotifyEvent read fOnItemClick write fOnItemClick;
    property OnItemDblClick : TNotifyEvent read fOnItemDblClick write fOnItemDblClick;

    procedure VerticalScrollChange(sender:TObject); override;
    function MaxItemLength: integer; override;
    function MaxItemCount:integer; override;
    function VisibleItems: integer; override;
    function MaxItemHeight: integer; override;

    property DayStart:TTime read fDayStart write SetDayStart;
    property DayEnd:TTime read fDayEnd write setDayEnd;
    property ItemTime:TTime read GetItemTime write SetItemTime;
    property ColorDifference:TrmPercent read fColorDiff write SetColorDiff default 20;
    property Items : TrmDayViewCollection read fItems write SetItems;
    property InternalTopIndex;

    property OnSelectedItemChange : TNotifyEvent read fSelItemChange write fSelItemChange;
  public
    constructor create(AOwner: TComponent); override;
    destructor destroy; override;

    function RowTime(aRow:integer):TTime;
    function RowIndex(aTime:TTime):integer;

    function BlockTime:integer;
    procedure UpdateViewItems;
    procedure UpdateTops;
    procedure UpdateOverlaps;

    property SelTimeStart : TTime read GetSelTimeStart write SetSelTimeStart;
    property SelTimeCount : Integer read GetSelTimeCount write SetSelTimeCount;
    property SelStart;
    property SelCount;
    property SelectedItem : TrmDayViewCollectionItem read fSelectedItem;

    property TopTime: TTime read GetTopTime write SetTopTime;
    property TopIndex: integer read GetTopIndex write SetTopIndex;

    property Rowcount:integer read MaxItemCount;
  end;

  TrmDayView = class(TrmCustomDayView)
  published
    property Align;
    property BorderStyle;
    property DoubleBuffered;
    property SelectedColor;
    property DayStart;
    property DayEnd;
    property ColorDifference;
    property HourlyBreakdown;
    property Color;
    property Font;
    property HideSelection;
    property ShowFocusRect;
    property HourColWidth;
    property ItemIndex;
    property ItemTime;
    property Items;
    property MultiSelect;
    property TabStop;
    property PopupMenu;
    property ClientHeight;
    property ClientWidth;
    property Ctl3D;
    property Cursor;
    property Enabled;
    property Hint;
    property ParentFont;
    property ShowHint;
    property Visible;

    property OnItemClick;
    property OnItemDblClick;
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
    property OnSelectedItemChange;
  end;

implementation

const
   fHoursPerDay = 24;
   fMinsPerDay = 1440;

function AdjustMinutes(OldTime:TTime; MinToAdd:integer):TTime;
begin
   Result := ((OldTime * fMinsPerDay) + MinToAdd) / fMinsPerDay;
end;

{ TrmCustomDayView }

procedure TrmCustomDayView.CMBorderChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;

procedure TrmCustomDayView.cmColorChanged(var Message: TMessage);
begin
   inherited;
   invalidate;
end;

procedure TrmCustomDayView.CMFontChanged(var Message: TMessagE);
begin
  inherited;
  canvas.font.size := canvas.Font.size + 9;
  try
     fRowHeight := Canvas.TextHeight('X');
  finally
     canvas.font.size := font.size;
  end;
end;

constructor TrmCustomDayView.create(AOwner: TComponent);
begin
  inherited;
  Height := 250;
  Width := 150;
  fRowHeight := 24;
  fHourColWidth := 50;
  TabStop := true;
  fColorDiff := 20;
  fHourlyBreakdown := hb30Minutes;
  fDrawBuffer := TBitmap.Create;
  color := clSkyBlue;
  fSelColor := clHighlight;
  BorderStyle := bsNone;
  ScrollBars := ssVertical;
  fDayStart := EncodeTime(9, 0, 0, 0);
  fDayEnd := EncodeTime(17, 0, 0, 0);
  fItems := TrmDayViewCollection.Create(self);
  fItems.OnCollectionUpdate := DoItemsUpdate;
end;

destructor TrmCustomDayView.destroy;
begin
  fDrawBuffer.free;
  fItems.free;
  inherited;
end;

function TrmCustomDayView.MaxItemCount: integer;
begin
   case fHourlyBreakdown of
     hb5Minutes : result := fHoursPerDay * 12;
     hb10Minutes : result := fHoursPerDay * 6;
     hb15Minutes : result := fHoursPerDay * 4;
     hb20Minutes : result := fHoursPerDay * 3;
     hb30Minutes : result := fHoursPerDay * 2;
     hb1Hour : result := fHoursPerDay;
   else
      result := 0;
   end;
end;

function TrmCustomDayView.MaxItemHeight: integer;
begin
   result := fRowHeight;
end;

function TrmCustomDayView.MaxItemLength: integer;
begin
   result := 0;
end;

procedure TrmCustomDayView.Paint;
var
   loop : integer;
   wFocusRect, wTimeRect, wDataRect : TRect;
   wh, wm, ws, wms : word;
   wData : string;
   wDrawHourLine : boolean;
   wCanvas : TCanvas;
   windex : integer;
begin
   if FDoubleBuffered then
   begin
      //set some of the defaults
      fDrawBuffer.Width := ClientWidth;
      fDrawBuffer.height := clientheight;

      wCanvas := fDrawBuffer.Canvas;
   end
   else
      wCanvas := Canvas;

   wCanvas.font.assign(font);

   wCanvas.Brush.Style := bsSolid;
   wCanvas.Brush.color := clBtnFace;
   wCanvas.FillRect(rect(0,0,fHourColWidth,clientheight));

   //Set row times
   wTimeRect := rect(0, 0, fHourColWidth, fRowHeight);
   wDataRect := rect(fHourColWidth, 0, clientwidth, fRowHeight);
   wFocusRect := rect(-1,-1,-1,-1);
   for loop := 0 to VisibleItems-1 do
   begin
      if loop = 1 then
         inc(wDataRect.Top);
      wIndex := InternalTopIndex+loop;
      if wIndex >= MaxItemCount then
         break;

      decodetime(rowtime(wIndex), wh, wm, ws, wms);

      if ((windex >= rowindex(fDayStart)) and (wIndex < rowindex(fdayend))) then
        wCanvas.Brush.Color := color
      else
         wCanvas.Brush.color := DarkenColor(color, fColorDiff);

      if ((wIndex >= SelStart) and (wIndex <= SelStart+SelCount)) then
      begin
         if (Focused or not HideSelection) and not assigned(fSelectedItem) then
            wCanvas.Brush.Color := fSelColor;
      end;

      wCanvas.FillRect(wDataRect);
      wCanvas.Brush.Style := bsClear;

      wCanvas.Pen.Color := clBtnShadow;
      wCanvas.MoveTo(fHourColWidth, wTimeRect.top);
      wCanvas.lineto(fHourColWidth, wTimeRect.Bottom);

      if ShowFocusRect and Focused and (windex = ItemIndex) then
         wFocusRect := wDataRect;

      case fHourlyBreakdown of
        hb5Minutes: wDrawHourLine := (wm = 55);
        hb10Minutes: wDrawHourLine := (wm = 50);
        hb15Minutes: wDrawHourLine := (wm = 45);
        hb20Minutes: wDrawHourLine := (wm = 40);
        hb30Minutes: wDrawHourLine := (wm = 30);
        hb1Hour : wDrawHourLine := true;
      else
        wDrawHourLine := false;
      end;

      if wDrawHourLine then
         wCanvas.MoveTo(0, wTimeRect.Top+rectheight(wTimeRect))
      else
         wCanvas.MoveTo(rectWidth(wTimeRect) div 2, wTimeRect.Top+rectheight(wTimeRect));

      wCanvas.Pen.Color := DarkenColor(clDkGray, 50);
      wCanvas.LineTo(clientwidth, wTimeRect.Top+rectheight(wTimeRect));

      if wm = 0 then
      begin
         wCanvas.font.style := [fsBold];
         wCanvas.Font.size := wCanvas.Font.size + 8;

         if wh = 0 then
         begin
            wCanvas.TextRect(wTimeRect, wTimeRect.left+2, wTimeRect.top+((rectheight(wTimeRect) div 2) - (wCanvas.Textheight('12') div 2)), '12');
            wdata := TimeAMString;
         end
         else if wh = 12 then
         begin
            wCanvas.TextRect(wTimeRect, wTimeRect.left+2, wTimeRect.top+((rectheight(wTimeRect) div 2) - (wCanvas.Textheight('12') div 2)), '12');
            wdata := TimePMString;
         end
         else
         begin
            if wh < 12 then
            begin
               wData := inttostr(wh);
               if wh < 10 then
                  wData := ' '+wData;
               wCanvas.TextRect(wTimeRect, wTimeRect.left+2, wTimeRect.top+((rectheight(wTimeRect) div 2) - (wCanvas.Textheight(wdata) div 2)), wdata);
               wdata := TimeAMString;
            end
            else
            begin
               wData := inttostr(wh-12);
               if wh-12 < 10 then
                  wData := ' '+wData;
               wCanvas.TextRect(wTimeRect, wTimeRect.left+2, wTimeRect.top+((rectheight(wTimeRect) div 2) - (wCanvas.Textheight(wdata) div 2)), wdata);
               wdata := TimePMString;
            end;
         end;
      end
      else
      begin
         if wm < 10 then
            wdata := TimeSeparator+'0'+inttostr(wm)
         else
            wdata := TimeSeparator+inttostr(wm);
      end;

      wCanvas.Font.assign(font);
      wCanvas.TextRect(wTimeRect, wTimeRect.left+rectwidth(wTimeRect)-wCanvas.TextWidth(wdata)-4, wTimeRect.top+((rectheight(wTimeRect) div 2) - (wCanvas.Textheight(wdata) div 2)), wdata);

      OffsetRect(wTimeRect, 0, fRowheight);
      OffsetRect(wDataRect, 0, fRowheight);
   end;

   if wTimeRect.Top < ClientHeight then
   begin
      wCanvas.Brush.color := clbtnface;
      wCanvas.FillRect(rect(0, wTimeRect.top+1, clientwidth, clientheight));
   end;

   case fHourlyBreakdown of
     hb5Minutes: wDrawHourLine := (wm = 55);
     hb10Minutes: wDrawHourLine := (wm = 50);
     hb15Minutes: wDrawHourLine := (wm = 45);
     hb20Minutes: wDrawHourLine := (wm = 40);
     hb30Minutes: wDrawHourLine := (wm = 30);
     hb1Hour : wDrawHourLine := true;
   else
     wDrawHourLine := false;
   end;

   wCanvas.Brush.color := DarkenColor(color, 20);
   if wDrawHourLine then
      wCanvas.MoveTo(0, wTimeRect.Top)
   else
      wCanvas.MoveTo(rectWidth(wTimeRect) div 2, wTimeRect.Top);
   wCanvas.LineTo(clientwidth, wTimeRect.Top);

   if not EqualRect(wFocusRect, rect(-1,-1,-1,-1)) then
      wCanvas.DrawFocusRect(wFocusRect);

   if FDoubleBuffered then
      BitBlt(Canvas.Handle, 0, 0, clientwidth, clientheight, fDrawBuffer.Canvas.Handle, 0, 0, SRCCOPY);
end;

function TrmCustomDayView.RowIndex(aTime: TTime): integer;
var
   h, m, s, ms:word;
   minutes : integer;
begin
   DecodeTime(aTime, h, m, s, ms);
   minutes := (h * 60) + m;
   case fHourlyBreakdown of
     hb5Minutes :  result := minutes div 5;
     hb10Minutes : result := minutes div 10;
     hb15Minutes : result := minutes div 15;
     hb20Minutes : result := minutes div 20;
     hb30Minutes : result := minutes div 30;
     hb1Hour : result := minutes div 60;
   else
     result := 0; 
   end;

   result := SetInRange(result, 0, MaxItemCount-1);
end;

function TrmCustomDayView.RowTime(aRow: integer): TTime;
begin
   case fHourlyBreakdown of
     hb5Minutes : result := AdjustMinutes(0, arow * 5);
     hb10Minutes : result := AdjustMinutes(0, arow * 10);
     hb15Minutes : result := AdjustMinutes(0, arow * 15);
     hb20Minutes : result := AdjustMinutes(0, arow * 20);
     hb30Minutes : result := AdjustMinutes(0, arow * 30);
     hb1Hour : result := AdjustMinutes(0, arow * 60);
   else
      result := 0;
   end;
   if result < 0 then
      result := 0
   else if result > 1 then
     result := 1;
end;

procedure TrmCustomDayView.setDayEnd(const Value: TTime);
begin
  if (fDayEnd <> Value) and (fDayStart < Value) then
  begin
     fDayEnd := Value;
     invalidate;
  end;
end;

procedure TrmCustomDayView.SetDayStart(const Value: TTime);
begin
  if (fDayStart <> Value) and (fDayEnd > Value) then
  begin
     fDayStart := Value;
     invalidate;
  end;
end;

procedure TrmCustomDayView.SetHourColWidth(const Value: integer);
begin
  if fHourColWidth <> Value then
  begin
     fHourColWidth := Value;
     UpdateViewItems;
     invalidate;
  end;
end;

procedure TrmCustomDayView.SetHourlyBreakdown(const Value: TrmHourBreakdown);
var
   wOldTime : TTime;
begin
  if value <> fHourlyBreakdown then
  begin
     wOldTime := ItemTime;
     fHourlyBreakdown := Value;
     ItemTime := wOldTime;
{//rjm
     UpdateViewItems;
     UpdateVScrollBar;
     Invalidate;}
  end;
end;

function TrmCustomDayView.VisibleItems: integer;
begin
  Result := ClientHeight div fRowHeight;
end;

procedure TrmCustomDayView.wmEraseBKGrnd(var msg: tmessage);
begin
  msg.result := 1;
end;

procedure TrmCustomDayView.SetSelColor(const Value: tcolor);
begin
  if fSelColor <> Value then
  begin
     fSelColor := Value;
     invalidate;
  end;
end;

function TrmCustomDayView.GetItemTime: TTime;
begin
   result := RowTime(ItemIndex);
end;

procedure TrmCustomDayView.SetItemTime(const Value: TTime);
begin
   if ItemIndex <> RowIndex(value) then
      ItemIndex := RowIndex(value);
end;

procedure TrmCustomDayView.SetColorDiff(const Value: TrmPercent);
begin
  if fColorDiff <> Value then
  begin
     fColorDiff := Value;
     invalidate;
  end;
end;

procedure TrmCustomDayView.CMFocusChanged(var Message: TCMFocusChanged);
begin
   inherited;
   invalidate;
end;

function TrmCustomDayView.GetSelTimeCount:integer;
begin
   result := ((SelStart+(SelCount+1)) - SelStart);
   case fHourlyBreakdown of
     hb5Minutes : result := result*5;
     hb10Minutes : result := result*10;
     hb15Minutes : result := result*15;
     hb20Minutes : result := result*20;
     hb30Minutes : result := result*30;
     hb1Hour : result := result*60;
   else
      result := 0;
   end;
end;

function TrmCustomDayView.GetSelTimeStart: TTime;
begin
  result := RowTime(SelStart);
end;

procedure TrmCustomDayView.SetSelTimeCount(const Value: integer);
begin
   case fHourlyBreakdown of
     hb5Minutes : SelCount := value div 5;
     hb10Minutes : SelCount := value div 10;
     hb15Minutes : SelCount := value div 15;
     hb20Minutes : SelCount := value div 20;
     hb30Minutes : SelCount := value div 30;
     hb1Hour : SelCount := value div 60;
   else
      SelCount := 0;
   end;
end;

procedure TrmCustomDayView.SetSelTimeStart(const Value: TTime);
begin
  SelStart := RowIndex(Value);
end;

procedure TrmCustomDayView.Loaded;
begin
  inherited;
  SetTopIndex(ItemIndex);
  UpdateVScrollBar;
end;

procedure TrmCustomDayView.UpdateViewItems;
var
   loop : integer;
begin
   for loop := 0 to Items.Count-1 do
      Items[loop].UpdateViewDetails;
end;

function TrmCustomDayView.BlockTime: integer;
begin
   case fHourlyBreakdown of
     hb5Minutes : result := 5;
     hb10Minutes : result := 10;
     hb15Minutes : result := 15;
     hb20Minutes : result := 20;
     hb30Minutes : result := 30;
     hb1Hour : result := 60;
   else
      result := 0;
   end;
end;

procedure TrmCustomDayView.SetItems(const Value: TrmDayViewCollection);
begin
  fItems := Value;
  UpdateViewItems;
end;

procedure TrmCustomDayView.VerticalScrollChange(sender: TObject);
begin
   UpdateTops;
end;

Procedure TrmCustomDayView.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if (x > fHourColWidth) then
     inherited;
end;

procedure TrmCustomDayView.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if (x > fHourColWidth) then
     inherited;
end;

procedure TrmCustomDayView.DoItemClick(sender: TObject);
begin
   if assigned(fonitemclick) then
      fOnItemClick(sender);
end;

procedure TrmCustomDayView.DoItemDblclick(sender: TObject);
begin
   if assigned(fonitemdblclick) then
      fonitemdblclick(self);
end;

function TrmCustomDayView.IsTimeModIndex(aTime:TTime): boolean;
var
   h, m, s, ms:word;
   minutes : integer;
begin
   DecodeTime(aTime, h, m, s, ms);
   minutes := (h * 60) + m;
   case fHourlyBreakdown of
     hb5Minutes : result := (minutes mod 5) > 0;
     hb10Minutes : result := (minutes mod 10) > 0;
     hb15Minutes : result := (minutes mod 15) > 0;
     hb20Minutes : result := (minutes mod 20) > 0;
     hb30Minutes : result := (minutes mod 30) > 0;
     hb1Hour : result := (minutes mod 60) > 0;
   else
      result := false;
   end;
end;

procedure TrmCustomDayView.DoItemsUpdate(sender: TObject);
begin
  // UpdateViewItems;
end;

procedure TrmCustomDayView.UpdateTops;
var
   loop : integer;
begin
   for loop := 0 to Items.Count-1 do
      Items[loop].UpdateTop;
end;

procedure TrmCustomDayView.SetSelectedItem(item: TrmDayViewCollectionItem);
begin
   If fSelectedItem <> item then
   begin
      fSelectedItem := item;
      invalidate;
      if assigned(fSelItemChange) then
         fSelItemChange(self);
   end;
end;

procedure TrmCustomDayView.DoItemIndexChange;
begin
  inherited;
  SetSelectedItem(nil);
end;

procedure TrmCustomDayView.wmSize(var msg: twmSize);
begin
   inherited;
   if msg.width <> width then
      updateoverlaps;
   invalidate;
end;

procedure TrmCustomDayView.wmDayViewItemClicked(var msg: Tmessage);
begin
   DoItemClick(self);
end;

procedure TrmCustomDayView.wmDayViewItemDBLClicked(var msg: Tmessage);
begin
   DoItemDblclick(self);
end;

function TrmCustomDayView.GetTopIndex: integer;
begin
   result := InternalTopIndex;
end;

function TrmCustomDayView.GetTopTime: TTime;
begin
   result := RowTime(InternalTopIndex);
end;

procedure TrmCustomDayView.SetTopIndex(const Value: integer);
begin
   InternalTopIndex := Value;
end;

procedure TrmCustomDayView.SetTopTime(const Value: TTime);
begin
   InternalTopIndex := RowIndex(Value);
end;

procedure TrmCustomDayView.UpdateOverlaps;
var
   loop : integer;
begin
   for loop := 0 to Items.Count-1 do
      Items[loop].UpdateOverlap;
end;

{ TrmCustomDayViewItem }

constructor TrmCustomDayViewItem.create(AOwner: TComponent);
begin
  inherited;

  ControlStyle := ControlStyle - [csAcceptsControls];
  height := 45;
  width := 100;

  ParentShowHint := false;

  fItemDesc := 'Unknown Type';

  fLabel := TrmLabel.create(self);
  fLabel.parent := self;
  fLabel.BorderStyle := rmbsNone;
  fLabel.align := alLeft;
  fLabel.width := 10;
  fLabel.Color := clBlue;
  fLabel.caption := '';
  fLabel.AutoSize := false;
  fLabel.OnClick := DoClick;
  fLabel.OnDblClick := DoDblClick;
  fLabel.OnMouseEnter := DoIntMouseEnter;
  fLabel.OnMouseLeave := doIntMouseLeave;

  fNote := TrmLabel.create(self);
  fNote.Parent := self;
  fNote.Align := alClient;
  fNote.BorderStyle := rmbsSingle;
  fNote.Color := clBtnFace;
  fNote.Font.Color := clBtnText;
  fNote.WordWrap := true;
  fNote.OnClick := DoClick;
  fNote.OnDblClick := DoDblClick;
  fNote.OnMouseEnter := DoIntMouseEnter;
  fNote.OnMouseLeave := doIntMouseLeave;
end;

procedure TrmCustomDayViewItem.DoClick(sender: TObject);
begin
   HandleSetFocus;

   PostMessage(fDayView.Handle, wm_DayViewItemClicked, 0, fCollectionItem.Tag);
end;

procedure TrmCustomDayViewItem.DoDblClick(Sender: TObject);
begin
   HandleSetFocus;

   PostMessage(fDayView.Handle, wm_DayViewItemDBLClicked, 0, fCollectionItem.Tag);
end;

function TrmCustomDayViewItem.GetCaption: TCaption;
begin
   result := fNote.Caption;
   updateHint;
end;

function TrmCustomDayViewItem.Getcolor: TColor;
begin
   result := fNote.Color;
end;

function TrmCustomDayViewItem.GetItemColor: TColor;
begin
   Result := fLabel.Color;
end;

function TrmCustomDayViewItem.GetItemFont: TFont;
begin
   result := fNote.Font;
end;

function TrmCustomDayViewItem.GetPopup: TPopupMenu;
begin
   Result := fLabel.PopupMenu;
end;

procedure TrmCustomDayViewItem.UpdateTop;
begin
   if Assigned(fDayView) then
      top := (fDayView.MaxItemHeight * (fDayView.RowIndex(fStartTime)-fDayView.InternalTopIndex))+5;
end;

procedure TrmCustomDayViewItem.SetCaption(const Value: TCaption);
begin
   fNote.Caption := value;
   UpdateHint;
end;

procedure TrmCustomDayViewItem.SetColor(const Value: TColor);
begin
   fnote.color := value;
end;

procedure TrmCustomDayViewItem.SetDayView(const Value: TrmDayView);
begin
  if fDayView <> value then
  begin
     fDayView := Value;
     if assigned(fDayView) then
        FreeNotification(fDayView);
  end;
end;

procedure TrmCustomDayViewItem.SetDuration(const Value: integer);
begin
  if fDuration <> value then
  begin
     fDuration := Value;
     UpdateHint;
     fDayView.UpdateViewItems;
  end;
end;

procedure TrmCustomDayViewItem.SetItemColor(const Value: TColor);
begin
   fLabel.Color := Value;
end;

procedure TrmCustomDayViewItem.SetItemFont(const Value: TFont);
begin
   fNote.Font.Assign(value);
end;

procedure TrmCustomDayViewItem.SetPopup(const Value: TPopupMenu);
begin
   fLabel.PopupMenu := value;
   fNote.PopupMenu := value;
end;

procedure TrmCustomDayViewItem.SetStartTime(const Value: TTime);
begin
  if fStartTime <> value then
  begin
     fStartTime := Value;
     UpdateHint;
     fDayView.UpdateViewItems;
  end;
end;

procedure TrmCustomDayViewItem.UpdateHint;
begin
   fNote.Hint := format(HintStr, [fItemDesc, timetostr(fStartTime), fduration, fNote.Caption]);
   fLabel.Hint := fNote.Hint;
end;

procedure TrmCustomDayViewItem.UpdateHeight;
var
   wHeight : integer;
   wBlockCount : integer;
begin
   if Assigned(fDayView) and (fduration > 0) then
   begin
      wBlockCount := fDayView.RowIndex(AdjustMinutes(fStartTime, fDuration)) - fDayView.RowIndex(fStartTime);
      if fDayView.IsTimeModIndex(AdjustMinutes(fStartTime, fDuration)) then
         inc(wBlockCount);

      wHeight := (fDayView.MaxItemHeight * wBlockCount);
      Height := SetInRange(wHeight, fDayView.MaxItemHeight, 10000)-10;
   end;
end;

procedure TrmCustomDayViewItem.UpdateOverlap;
var
   loop : integer;
   wColl : TrmDayViewCollection;
   wItem : TrmDayViewItem;
   wStart, wEnd : integer;
   wMyStart, wMyEnd : integer;
   wCount : integer;
   wPosition : integer;
   wPassedSelf : boolean;
begin
   wMyStart := fDayView.RowIndex(fStartTime);
   wMyEnd := fDayView.RowIndex(AdjustMinutes(fStartTime, fDuration));
   if fDayView.IsTimeModIndex(AdjustMinutes(fStartTime, fDuration)) then
      inc(wMyEnd);

   wCount := 1;
   wPosition := 0;
   wPassedSelf := false;
   wColl := TrmDayViewCollection(fCollectionItem.Collection);
   for loop := 0 to wColl.Count-1 do
   begin
      wItem := wColl[loop].fDayViewItem;
      if wItem <> self then
      begin
         wStart := fDayView.RowIndex(wItem.StartTime);
         wEnd := fDayView.RowIndex(AdjustMinutes(wItem.StartTime, wItem.Duration));
         if fDayView.IsTimeModIndex(AdjustMinutes(wItem.StartTime, wItem.Duration)) then
            inc(wEnd);
         if ((wStart >= wMyStart) and (wStart <= wMyEnd)) or
            ((wEnd >= wMyStart) and (wEnd <= wMyEnd)) then
         begin
            if not wPassedSelf then
               Inc(wPosition);
            inc(wCount);
         end;
      end
      else
      wPassedSelf := true;
   end;

   Width := (((fDayView.ClientWidth-fDayView.HourColWidth)-5)div wCount)-5;
   Left := fDayView.HourColWidth+(wPosition * width)+(5 * (wPosition+1));
end;

procedure TrmCustomDayViewItem.WMKillFocus(var Message: TWMSetFocus);
begin
   inherited;
   fNote.Color := clBtnFace;
   fNote.Font.Color := clBtnText;
end;

procedure TrmCustomDayViewItem.WMSetFocus(var Message: TWMSetFocus);
var
   wClientArea : TRect;
begin
   inherited;
   fNote.Color := clHighlight;
   fNote.Font.Color := clHighlightText;
   wClientArea := fDayView.ClientRect;
   wClientArea := rect(0,0,fDayView.ClientWidth, fDayView.ClientHeight);
//   OffsetRect(wClientArea, 0, fDayView.InternalTopIndex*fDayView.MaxItemHeight);

   if (Top < wClientarea.Top) then
   begin
      fdayview.InternalTopIndex := fDayView.RowIndex(fStartTime);
      fDayView.UpdateViewItems;
   end
   else if (top < wClientArea.bottom) and ((top+height) > wClientArea.Bottom) then
   begin
      fdayview.InternalTopIndex := fdayview.InternalTopIndex + ((height+5) div  fdayview.maxitemheight);
      fDayView.UpdateViewItems;
   end
   else
   if (top > wClientArea.bottom) then
   begin
      fdayview.InternalTopIndex := fdayview.InternalTopIndex + (((top+(height*2))- wClientArea.bottom) div fDayView.MaxItemHeight);
   end;


end;

procedure TrmCustomDayViewItem.HandleSetFocus;
begin
   if CanFocus then
      SetFocus;

   fDayView.SetSelectedItem(fCollectionItem);
end;

function TrmCustomDayViewItem.GetItemDesc: string;
begin
   result := fItemDesc;
end;

procedure TrmCustomDayViewItem.SetItemDesc(const Value: string);
begin
  if fItemDesc <> value then
  begin
     fItemDesc := value;
     UpdateHint;
  end;
end;

procedure TrmCustomDayViewItem.DoIntMouseEnter(sender: TObject);
begin
   Application.HintHidePause := 300000;
end;

procedure TrmCustomDayViewItem.DoIntMouseLeave(sender: TObject);
begin
   Application.HintHidePause := 2500;
end;

procedure TrmCustomDayViewItem.wmupdateall(var message: tmessage);
begin
   UpdateTop;
   UpdateHeight;
   UpdateOverlap;
   updateHint;
end;

procedure TrmCustomDayViewItem.wmupdateheights(var message: tmessage);
begin
   UpdateHeight;
end;

procedure TrmCustomDayViewItem.wmupdateoverlaps(var message: Tmessage);
begin
   UpdateOverlap;
end;

procedure TrmCustomDayViewItem.wmupdatetops(var message: Tmessage);
begin
   UpdateTop;
end;

{ TrmDayViewCollectionItem }

procedure TrmDayViewCollectionItem.Assign(Source: TPersistent);
begin
  inherited;

end;

constructor TrmDayViewCollectionItem.Create(Collection: TCollection);
begin
  inherited create(Collection);
  fDayViewItem := TrmDayViewItem.create(TrmDayViewCollection(Collection).FDayView);
  fDayViewItem.Parent := TrmDayViewCollection(Collection).FDayView;
  fDayViewItem.DayView := TrmDayViewCollection(Collection).FDayView;
  fDayViewItem.OnClick := TrmDayViewCollection(Collection).FDayView.DoItemClick;
  fDayViewItem.OnDblClick := TrmDayViewCollection(Collection).FDayView.DoItemDblclick;
  fDayViewItem.CollectionItem := self;
  fTag := 0;
end;

destructor TrmDayViewCollectionItem.Destroy;
begin
  if fDayViewItem.DayView.SelectedItem = self then
     fDayViewItem.DayView.SetSelectedItem(nil);

  fDayViewItem.CollectionItem := nil;
  fDayViewItem.DayView := nil;
  fDayViewItem.Free;
  inherited;
end;

function TrmDayViewCollectionItem.GetCaption: TCaption;
begin
   if fDayViewItem <> nil then
      result := fDayViewItem.Caption
   else
      result := '';
end;

function TrmDayViewCollectionItem.Getcolor: TColor;
begin
   if fDayViewItem <> nil then
      result := fDayViewItem.Color
   else
      result := clBtnFace;
end;

function TrmDayViewCollectionItem.GetDuration: integer;
begin
   if fDayViewItem <> nil then
      result := fDayViewItem.Duration
   else
      result := 0;
end;

function TrmDayViewCollectionItem.GetItemColor: TColor;
begin
   if fDayViewItem <> nil then
      result := fDayViewItem.ItemColor
   else
      result := clBlue;
end;

function TrmDayViewCollectionItem.GetItemDesc: string;
begin
   if fDayViewItem <> nil then
      result := fDayViewItem.ItemDescription
   else
      result := '';
end;

function TrmDayViewCollectionItem.GetItemFont: TFont;
begin
   if fDayViewItem <> nil then
      result := fDayViewItem.Font
   else
      result := nil;
end;

function TrmDayViewCollectionItem.GetPopup: TPopupMenu;
begin
   if fDayViewItem <> nil then
      result := fDayViewItem.PopupMenu
   else
      result := nil;
end;

function TrmDayViewCollectionItem.GetShowHint: boolean;
begin
   if fDayViewItem <> nil then
      result := fDayViewItem.ShowHint
   else
      result := false;
end;

function TrmDayViewCollectionItem.GetStartTime: TTime;
begin
   if fDayViewItem <> nil then
      result := fDayViewItem.StartTime
   else
      result := 0;
end;

procedure TrmDayViewCollectionItem.SetCaption(const Value: TCaption);
begin
   if fDayViewItem <> nil then
      fDayViewItem.Caption := value;
end;

procedure TrmDayViewCollectionItem.SetColor(const Value: TColor);
begin
   if fDayViewItem <> nil then
      fDayViewItem.color := value;
end;

procedure TrmDayViewCollectionItem.SetDuration(const Value: integer);
begin
   if fDayViewItem <> nil then
      fDayViewItem.Duration := Value;
end;

procedure TrmDayViewCollectionItem.SetItemColor(const Value: TColor);
begin
   if fDayViewItem <> nil then
      fDayViewItem.ItemColor := value
end;

procedure TrmDayViewCollectionItem.SetItemDesc(const Value: string);
begin
   if fDayViewItem <> nil then
      fDayViewItem.ItemDescription := value;
end;

procedure TrmDayViewCollectionItem.SetItemFont(const Value: TFont);
begin
   if fDayViewItem <> nil then
      fDayViewItem.font.assign(value);
end;

procedure TrmDayViewCollectionItem.SetPopup(const Value: TPopupMenu);
begin
   if fDayViewItem <> nil then
      fDayViewItem.PopupMenu := value;
end;

procedure TrmDayViewCollectionItem.SetShowHint(const Value: boolean);
begin
   if fDayViewItem <> nil then
      fDayViewItem.ShowHint := value;
end;

procedure TrmDayViewCollectionItem.SetStartTime(const Value: TTime);
begin
   if fDayViewItem <> nil then
      fDayViewItem.StartTime := value;
end;

procedure TrmDayViewCollectionItem.UpdateOverlap;
begin
   PostMessage(fDayViewItem.Handle, wm_UpdateOverlaps, 0, 0);
end;

procedure TrmDayViewCollectionItem.UpdateTop;
begin
   PostMessage(fDayViewItem.Handle, wm_updateTops, 0, 0);
end;

procedure TrmDayViewCollectionItem.UpdateViewDetails;
begin
   PostMessage(fDayViewItem.Handle, wm_updateAll, 0, 0);
end;

{ TrmDayViewCollection }

function TrmDayViewCollection.Add: TrmDayViewCollectionItem;
begin
  Result := TrmDayViewCollectionItem(inherited Add);
end;

constructor TrmDayViewCollection.Create(DayView: TrmCustomDayView);
begin
  inherited create(TrmDayViewCollectionItem);
  FDayView := TrmDayView(DayView);
end;

procedure TrmDayViewCollection.Delete(Index: Integer);
begin
  inherited Delete(index);
end;

function TrmDayViewCollection.GetItem(
  Index: Integer): TrmDayViewCollectionItem;
begin
  Result := TrmDayViewCollectionItem(inherited GetItem(Index));
end;

function TrmDayViewCollection.GetOwner: TPersistent;
begin
   result := FDayView;
end;

function TrmDayViewCollection.Insert(
  Index: Integer): TrmDayViewCollectionItem;
begin
  Result := TrmDayViewCollectionItem(inherited Insert(index));
end;

procedure TrmDayViewCollection.SetItem(Index: Integer;
  Value: TrmDayViewCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

procedure TrmDayViewCollection.Update(Item: TCollectionItem);
begin
  inherited;
  if assigned(fOnUpdate) then
    fOnUpdate(item);
end;

end.
