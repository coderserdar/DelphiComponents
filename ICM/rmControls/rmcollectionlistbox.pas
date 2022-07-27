{================================================================================
Copyright (C) 1997-2002 Mills Enterprise

Unit     : rmCollectionListBox
Purpose  : Allow for multi-line/multi-height "listbox" type functionality.  Also
           allowing for Icons to be specified for each item.
Date     : 04-24-2000
Author   : Ryan J. Mills
Version  : 1.90
================================================================================}

unit rmCollectionListBox;

interface

{$I CompilerDefines.INC}

uses Messages, Windows, Forms, Controls, ImgList, Graphics, Classes, sysutils;

type
  TrmCollectionListBox = class;

  TrmListBoxCollectionItem = class(TCollectionItem)
  private
    FTextData: TStringList;
    FLCount: integer; //Number of lines of text with the current font and display rect;
    fLStart: integer; //Calculated line start of the current record;
    fLRect: TRect; //Calculated Lines display Rect;
    FAlignment: TAlignment;
    fImageIndex: integer;
    fCenterImage: boolean;
    FData: TObject;
    procedure SetAlignment(Value: TAlignment);
    procedure SetTextData(const Value: TStringList);
    procedure SetImageIndex(Value: Integer);
    procedure SetCenterImage(const Value: boolean);
    function GetText: string;
    procedure SetText(const Value: string);
    property Text: string read GetText write SetText;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property Data: TObject read fData write fData;
    property LCount: integer read fLCount write fLCount;
    property LStart: integer read fLStart write fLStart;
    property LRect: TRect read fLRect write fLRect;
  published
    property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
    property TextData: TstringList read FTextData write SetTextData;
    property ImageIndex: Integer read fImageIndex write SetImageIndex default -1;
    property CenterImage: boolean read fCenterImage write SetCenterImage default false;
  end;

  TrmListBoxCollection = class(TCollection)
  private
    FCollectionListBox: TrmCollectionListBox;
    FOnUpdate: TNotifyEvent;
    function GetItem(Index: Integer): TrmListBoxCollectionItem;
    procedure SetItem(Index: Integer; Value: TrmListBoxCollectionItem);
  protected
    function GetOwner: TPersistent; override;
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(CollectionListBox: TrmCollectionListBox);
    function Add: TrmListBoxCollectionItem;
    procedure Delete(Index: Integer);
    function Insert(Index: Integer): TrmListBoxCollectionItem;
    property Items[Index: Integer]: TrmListBoxCollectionItem read GetItem write SetItem; default;
    property OnCollectionUpdate: TNotifyEvent read fOnUpdate write fOnUpdate;
  end;

  TrmCollectionListBox = class(TCustomControl)
  private
    fItems: TrmListBoxCollection;
    fSelectedItemIndex: integer;
    fFocusedItemIndex: integer;
    fTopIndex : integer;
    fVScrollSize: longint;
    fVScrollPos: longint;
    fTotalLineCount : integer;

    fFocusRect: TRect;

    FBorderStyle: TBorderStyle;
    FImageChangeLink: TChangeLink;
    FImages: TCustomImageList;
    fOddColor: TColor;
    fClick: TNotifyEvent;
    fAutoSelect: boolean;
    procedure ImageListChange(Sender: TObject);

    procedure SetImages(Value: TCustomImageList);
    procedure SetItems(const Value: TrmListBoxCollection);
    procedure SetOddColor(const Value: TColor);
    procedure SetItemIndex(const Value: integer);
    procedure SetBorderStyle(const Value: TBorderStyle);

    procedure cmFOCUSCHANGED(var MSG:TMessage); message CM_FOCUSCHANGED;
    procedure CMBorderChanged(var Message: TMessage); message CM_BORDERCHANGED;
    procedure CMCtl3DChanged(var Message: TMessage); message CM_CTL3DCHANGED;

    procedure CalcVScrollSize(startIndex: integer);
    procedure UpdateVScrollSize;
    function UpdateVScrollPos(newPos: integer): boolean;

    procedure WMSize(var Msg: TWMSize); message WM_SIZE;
    procedure WMVScroll(var Msg: TWMVScroll); message WM_VSCROLL;

    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Paint; override;
    function VisibleLineCount: integer;
    function LineHeight: integer;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    function TopItemIndex: integer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor destroy; override;
    procedure loaded; override;
    function Add(aText: string; aImageIndex: integer; aData: TObject): integer;
    function Insert(Index: integer; aText: string; aImageIndex: integer; aData: TObject): integer;
    procedure Delete(Index: integer);
    property ItemIndex: integer read fselectedItemIndex write SetItemIndex;
  published
    property AutoSelect : boolean read fAutoSelect write fAutoSelect default false;
    property Collection: TrmListBoxCollection read fItems write setItems;
    property Images: TCustomImageList read FImages write SetImages;
    property OddColor: TColor read fOddColor write SetOddColor default clInfoBk;
    property Align;
    property Anchors;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsSingle;
    property Color default clWindow;
    property Constraints;
    property Ctl3D;
    property Font;
    property ParentColor default False;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop default true;
    property Visible;

    property OnClick: TNotifyEvent read fClick write fClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;

implementation

uses rmMsgList, rmLibrary;

{ TrmListBoxCollectionItem }

constructor TrmListBoxCollectionItem.Create(Collection: TCollection);
begin
  fImageIndex := -1;
  FTextData := TStringList.create;
  inherited Create(Collection);
end;

procedure TrmListBoxCollectionItem.Assign(Source: TPersistent);
begin
  if Source is TrmListBoxCollectionItem then
  begin
    TextData.Assign(TrmListBoxCollectionItem(Source).TextData);
    Alignment := TrmListBoxCollectionItem(Source).Alignment;
    ImageIndex := TrmListBoxCollectionItem(Source).ImageIndex;
  end
  else
    inherited Assign(Source);
end;

procedure TrmListBoxCollectionItem.SetAlignment(Value: TAlignment);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    Changed(False);
  end;
end;

procedure TrmListBoxCollectionItem.SetTextData(const Value: TStringList);
begin
  fTextData.Assign(Value);
  Changed(False);
end;

procedure TrmListBoxCollectionItem.SetImageIndex(Value: Integer);
begin
  if FImageIndex <> Value then
  begin
    FImageIndex := Value;
    Changed(False);
  end;
end;

procedure TrmListBoxCollectionItem.SetCenterImage(const Value: boolean);
begin
  if fCenterImage <> value then
  begin
    fCenterImage := value;
    changed(false);
  end;
end;

function TrmListBoxCollectionItem.GetText: string;
begin
  result := FTextData.Text;
end;

destructor TrmListBoxCollectionItem.Destroy;
begin
  FTextData.free;
  inherited;
end;

procedure TrmListBoxCollectionItem.SetText(const Value: string);
begin
  FTextData.text := Value;

end;

{ TrmListBoxCollection }

constructor TrmListBoxCollection.Create(CollectionListBox: TrmCollectionListBox);
begin
  inherited Create(TrmListBoxCollectionItem);
  FCollectionListBox := CollectionListBox;
end;

function TrmListBoxCollection.Add: TrmListBoxCollectionItem;
begin
  Result := TrmListBoxCollectionItem(inherited Add);
end;

function TrmListBoxCollection.GetItem(Index: Integer): TrmListBoxCollectionItem;
begin
  Result := TrmListBoxCollectionItem(inherited GetItem(Index));
end;

function TrmListBoxCollection.GetOwner: TPersistent;
begin
  Result := FCollectionListBox;
end;

procedure TrmListBoxCollection.SetItem(Index: Integer; Value: TrmListBoxCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

procedure TrmListBoxCollection.Update(Item: TCollectionItem);
begin
  inherited;
  if assigned(fOnUpdate) then
    fOnUpdate(item);
end;

procedure TrmListBoxCollection.Delete(Index: Integer);
begin
  inherited Delete(index);
end;

function TrmListBoxCollection.Insert(
  Index: Integer): TrmListBoxCollectionItem;
begin
  Result := TrmListBoxCollectionItem(inherited Insert(index));
end;

{ TrmCollectionListBox }

constructor TrmCollectionListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  ControlStyle := [csClickEvents, csDoubleClicks, csOpaque];

  if not NewStyleControls then
    ControlStyle := ControlStyle + [csFramed];


  Width := 121;
  Height := 97;
  TabStop := True;
  ParentColor := False;
  fVScrollPos := 0;
  fVScrollSize := 0;
  fselectedItemIndex := -1;
  fTopIndex := 0;
  ffocusedItemIndex := 0;
  fOddColor := clInfoBk;
  Color := clWindow;
  fborderstyle := bsSingle;
  fAutoSelect := false;

  fItems := TrmListBoxCollection.create(self);

  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := ImageListChange;
end;

procedure TrmCollectionListBox.CreateParams(var Params: TCreateParams);
const
  BorderStyles: array[TBorderStyle] of DWORD = (0, WS_BORDER);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := Style or WS_VSCROLL;
    WindowClass.style := CS_DBLCLKS;

    Style := Style or BorderStyles[FBorderStyle];
    if NewStyleControls and Ctl3D and (FBorderStyle = bsSingle) then
    begin
      Style := Style and not WS_BORDER;
      ExStyle := ExStyle or WS_EX_CLIENTEDGE;
    end;
    WindowClass.style := WindowClass.style and not (CS_HREDRAW or CS_VREDRAW);
  end;
end;

procedure TrmCollectionListBox.SetImages(Value: TCustomImageList);
begin
  if Images <> nil then
    Images.UnRegisterChanges(FImageChangeLink);
  FImages := Value;
  if Images <> nil then
  begin
    Images.RegisterChanges(FImageChangeLink);
    Images.FreeNotification(Self);
    CalcVScrollSize(-1);
    invalidate;
  end;
end;

procedure TrmCollectionListBox.ImageListChange(Sender: TObject);
begin
  Invalidate;
end;

procedure TrmCollectionListBox.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = Images) then
    Images := nil;
end;

procedure TrmCollectionListBox.setItems(const Value: TrmListBoxCollection);
begin
  if fitems <> value then
  begin
    fItems.assign(Value);
    invalidate;
  end;
end;

destructor TrmCollectionListBox.destroy;
begin
  FImageChangeLink.Free;
  fItems.Free;
  inherited Destroy;
end;

procedure TrmCollectionListBox.CalcVScrollSize(startIndex: integer);
var
  loop: integer;
  wCalcSize : integer;
  wImageRect, wTextRect, wCalcRect: TRect;
  wFHeight: integer;
  wStr : string;
begin
  if csloading in componentstate then exit;
  if csDestroying in ComponentState then exit;

  if assigned(fImages) then
  begin
    wImageRect := Rect(0, 0, FImages.Width, FImages.Height);
    wTextRect := Rect(fImages.Width + 2, 0, clientwidth - (fImages.Width + 2), FImages.Height);
  end
  else
  begin
    wImageRect := Rect(0, 0, 0, 0);
    wTextRect := Rect(2, 0, clientwidth, 0);
  end;

  if startindex = -1 then
  begin
    loop := 0;
    wCalcSize := 0;
  end
  else
  begin
     loop := startindex;
     if startindex >= 1 then
       wcalcSize := fitems.Items[startindex-1].lstart + fitems.Items[startindex-1].lcount
     else
       wCalcSize := 0;
  end;

  wFHeight := LineHeight;

  while loop < fItems.Count do
  begin
    with fItems.Items[loop] do
    begin
      wCalcRect := wTextRect;
      wstr := trim(Text);
      DrawText(Canvas.Handle, pchar(wStr), length(wStr), wCalcRect, DT_WORDBREAK or DT_CALCRECT);

      if (wCalcRect.Bottom - wCalcRect.Top) < (wImageRect.Bottom-wImageRect.Top) then
         wCalcRect.Bottom := wImageRect.Bottom;

      LCount := (wCalcRect.Bottom - wCalcRect.Top) div wFHeight;

      if LCount = 0 then
        lCount := 1;

      if (((wCalcRect.Bottom - wCalcRect.Top) mod wFHeight) > (wFHeight div 2)) then
        lCount := lCount + 1;

      LStart := wCalcSize;
      LRect := rect(wCalcRect.left, wCalcRect.Top, clientwidth, wCalcRect.bottom);
      inc(wCalcSize, LCount);
    end;
    inc(loop);
  end;

  fTotalLineCount := wCalcSize;
  UpdateVScrollSize;
end;

procedure TrmCollectionListBox.loaded;
begin
  inherited;
  CalcVScrollSize(-1);
end;

procedure TrmCollectionListBox.WMSize(var Msg: TWMSize);
begin
  inherited;
  CalcVScrollSize(-1);
  UpdateVScrollPos(fVScrollPos + 1);
  UpdateVScrollPos(fVScrollPos - 1);
  Invalidate;
end;

procedure TrmCollectionListBox.Paint;
var
  index: integer;
  wImageRect, wCalcRect: TRect;
  wFHeight, cheight, wcalcheight: integer;
  DrawFlags: integer;
  wStr : string;
  wImageAdjust : integer;
begin
  if csDestroying in ComponentState then exit;

  wFHeight := LineHeight;

  index := TopItemIndex;
  if index = -1 then
  begin
    fFocusRect := rect(0, 0, clientwidth, wFHeight);
    if Focused then
       Canvas.DrawFocusRect(fFocusRect)
    else
    begin
       Canvas.Brush.Color := Color;
       Canvas.FillRect(rect(0, 0, clientwidth, wFHeight));
    end;
    exit;
  end;

  if assigned(fImages) then
    wImageRect := Rect(0, 0, FImages.Width, FImages.Height)
  else
    wImageRect := Rect(0, 0, 0, 0);

  wimageAdjust := 0;

  cheight := VisibleLineCount;

  while (cheight > 0) and (index < fItems.count) do
  begin
    with fItems.Items[index] do
    begin
      DrawFlags := DT_WORDBREAK;

      if FAlignment = taCenter then
        DrawFlags := DrawFlags or DT_CENTER;

      if FAlignment = taRightJustify then
        DrawFlags := DrawFlags or DT_RIGHT;

      wCalcRect := LRect;

      OffsetRect(wCalcRect, 0, ((LStart - fVScrollPos) * wFHeight)+wImageAdjust);

      if assigned(fimages) and ((lcount*wfHeight) < fimages.height) then
         inc(wimageAdjust, fimages.height - (lcount*wfHeight));

      if wCalcRect.Top < 0 then
         wcalcheight := (LStart - fVScrollPos) - lCount
      else
         wCalcheight := lcount;

      dec(cheight, wcalcheight);

      if index = fSelectedItemIndex then
      begin
        if assigned(fimages) then
        begin
          Canvas.Brush.Color := clHighlight;
          Canvas.fillrect(rect(0, wCalcRect.top, wImageRect.right, wCalcRect.Bottom));

          if odd(index) then
            canvas.brush.color := color
          else
            canvas.brush.color := oddcolor;

          Canvas.fillrect(rect(wImageRect.right, wCalcRect.top, wCalcRect.right, wCalcRect.Bottom));
        end
        else
        begin
          Canvas.Font.Color := clHighLightText;
          Canvas.Brush.Color := clHighlight;
          wCalcRect.Right := ClientWidth;
          Canvas.fillrect(rect(0, wCalcRect.top, wCalcRect.right, wCalcRect.Bottom));
        end;
      end
      else
      begin
        Canvas.Font.Color := Font.Color;
        if odd(index) then
          canvas.brush.color := color
        else
          canvas.brush.color := oddcolor;

        wCalcRect.Right := ClientWidth;
        Canvas.fillrect(rect(0, wCalcRect.top, wCalcRect.right, wCalcRect.Bottom));
      end;

      if assigned(FImages) then
      begin
        if fCenterImage then
          FImages.Draw(canvas, 0, wCalcRect.Top + (((wCalcRect.Bottom - wCalcRect.Top) div 2) - (fImages.Height div 2)), fImageIndex)
        else
          FImages.Draw(canvas, 0, wCalcRect.Top, fImageIndex);
      end;

      wstr := Trim(Text);
      DrawText(Canvas.Handle, pchar(wstr), length(wstr), wCalcRect, DrawFlags);

      if (index = fFocusedItemIndex) then
      begin
        fFocusRect := rect(0, wCalcRect.Top, clientwidth, wCalcRect.bottom);
        if focused then
          Canvas.DrawFocusRect(fFocusRect);
      end;
    end;
    inc(index);
  end;

  if cHeight > 0 then
  begin
    Canvas.Brush.Color := color;
    Canvas.FillRect(rect(0, wCalcRect.Bottom, clientwidth, wCalcRect.Bottom + ((cHeight + 1) * wFHeight)));
  end;
end;

procedure TrmCollectionListBox.UpdateVScrollSize;
var
  wScrollInfo: TScrollInfo;
begin
  fVScrollSize := fTotalLineCount - VisibleLineCount;
  with wScrollInfo do
  begin
    cbSize := sizeof(TScrollInfo);
    fMask := SIF_RANGE or SIF_DISABLENOSCROLL;
    nMin := 0;
    nMax := fVScrollSize;
  end;

  SetScrollInfo(Handle, SB_VERT, wScrollInfo, True);
end;

procedure TrmCollectionListBox.WMVScroll(var Msg: TWMVScroll);
var
  newPos: integer;
begin
  inherited;
  case Msg.ScrollCode of
    SB_BOTTOM: newPos := fItems.Items[fItems.Count - 1].LStart;
    SB_LINEDOWN: newPos := fVScrollPos + 1;
    SB_LINEUP: newPos := fVScrollPos - 1;
    SB_TOP: newPos := 0;
    SB_PAGEDOWN: newPos := fVScrollPos + VisibleLineCount;
    SB_PAGEUP: newPos := fVScrollPos - VisibleLineCount;
    SB_THUMBPOSITION: newPos := Msg.Pos;
    SB_THUMBTRACK: newPos := msg.Pos;
  else
    exit;
  end;

  if UpdateVScrollPos(newPos) then
    Invalidate;
end;

function TrmCollectionListBox.UpdateVScrollPos(newPos: integer): Boolean;
var
  wScrollInfo: TScrollInfo;
begin
  result := false;

  if (newPos <= 0) and (fVScrollPos = 0) then
    exit;

  if (newPos > fVScrollSize) and (fVScrollPos = fVScrollSize) then
    exit;

  if (newPos = fVscrollPos) then
    exit;

  result := true;

  if newpos < 0 then
    fVScrollPos := 0
  else if newpos > fVscrollSize then
    fVScrollPos := fVScrollSize
  else
    fVScrollPos := newPos;

  if fVScrollPos < 0 then
     fVScrollPos := 0;

  with wScrollInfo do
  begin
    cbSize := sizeof(TScrollInfo);
    fMask := SIF_POS;
    nPos := fVScrollPos;
  end;
  SetScrollInfo(Handle, SB_VERT, wScrollInfo, True);
end;

function TrmCollectionListBox.VisibleLineCount: integer;
begin
  result := (clientheight div LineHeight);
end;

function TrmCollectionListBox.LineHeight: integer;
var
  TM: tagTextMetricA;
begin
  GetTextMetrics(Canvas.Handle, TM);
  result := TM.tmHeight;
end;

function TrmCollectionListBox.Add(aText: string; aImageIndex: integer; aData: TObject): integer;
begin
  with fItems.Add do
  begin
    TextData.text := aText;
    ImageIndex := aImageIndex;
    Data := aData;
    result := ItemIndex;
  end;
  CalcVScrollSize(fItems.count-1);
  UpdateVScrollSize;
  invalidate;
end;

procedure TrmCollectionListBox.Delete(Index: integer);
begin
  fItems.delete(index);
  CalcVScrollSize(index-1);
  UpdateVScrollSize;
  invalidate;
end;

function TrmCollectionListBox.Insert(Index: integer; aText: string; aImageIndex: integer;
  aData: TObject): integer;
begin
  with fItems.Insert(Index) do
  begin
    TextData.text := aText;
    ImageIndex := aImageIndex;
    Data := aData;
    result := ItemIndex;
  end;
  CalcVScrollSize(-1);
  UpdateVScrollSize;
  invalidate;
end;

procedure TrmCollectionListBox.SetOddColor(const Value: TColor);
begin
  if fOddColor <> Value then
  begin
    fOddColor := Value;
    invalidate;
  end;
end;

procedure TrmCollectionListBox.CMFontChanged(var Message: TMessage);
begin
  inherited;
  Canvas.Font.assign(font);
  CalcVScrollSize(-1);
  invalidate;
end;

procedure TrmCollectionListBox.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  index: integer;
  found: boolean;
  wTextRect, wCalcRect: TRect;
  wFHeight, cheight: integer;
  DrawFlags: integer;
  wStr : String;
begin
  inherited;

  if Button = mbLeft then
  begin
    if CanFocus then
       setfocus;
    try
      index := TopItemIndex;
      if index <> -1 then
      begin
        wFHeight := LineHeight;
        wTextRect := Rect(0, 0, clientwidth, 0);
        cheight := VisibleLineCount;

        found := false;
        while (cheight > 0) and (index < fItems.count) and not found do
        begin
          with fItems.Items[index] do
          begin
            DrawFlags := DT_WORDBREAK;

            if FAlignment = taCenter then
              DrawFlags := DrawFlags or DT_CENTER;

            if FAlignment = taRightJustify then
              DrawFlags := DrawFlags or DT_RIGHT;

            wCalcRect := wTextRect;
            wStr := trim(Text);
            DrawText(Canvas.Handle, pchar(wstr), length(wstr), wCalcRect, DrawFlags or DT_CALCRECT);
            wCalcRect.Right := clientwidth;

            OffsetRect(wCalcRect, 0, (LStart - fVScrollPos) * wFHeight);

            found := ptinrect(wCalcRect, Point(x, y));

            if found then
              break;

            dec(cheight, (LStart - fVScrollPos) - lCount);
          end;
          inc(index);
        end;

        if found then
        begin
          ItemIndex := index;
          fFocusedItemIndex := index;
        end
        else
          ItemIndex := -1;
      end
      else
        ItemIndex := -1;
    finally
      if assigned(fClick) then
        fClick(self);
    end;
  end;
end;

procedure TrmCollectionListBox.SetItemIndex(const Value: integer);
begin
  if fSelectedItemIndex <> value then
  begin
    fSelectedItemIndex := Value;
    invalidate;
  end;
end;

procedure TrmCollectionListBox.SetBorderStyle(const Value: TBorderStyle);
begin
  if FBorderStyle <> Value then
  begin
    FBorderStyle := Value;
    RecreateWnd;
  end;
end;

procedure TrmCollectionListBox.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  Message.Result := DLGC_WANTARROWS;
end;

procedure TrmCollectionListBox.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  if fItems.Count > 0 then
  begin
     case key of
       vk_end:  fFocusedItemIndex := fItems.count-1;
       vk_DOWN: inc(fFocusedItemIndex);
       vk_up:   dec(fFocusedItemIndex);
       vk_home: fFocusedItemIndex := 0;
       vk_space, vk_Return:
         begin
           fSelectedItemIndex := fFocusedItemIndex;
           invalidate;
           exit;
         end;
     else
       exit;
     end;

     if fFocusedItemIndex < 0 then
        fFocusedItemIndex := 0;

     if fFocusedItemIndex >= fItems.count then
        fFocusedItemIndex := fItems.Count-1;

     UpdateVScrollPos(fItems[fFocusedItemIndex].fLStart);

     if fAutoSelect then
        fSelectedItemIndex := fFocusedItemIndex;
        
     Invalidate;
  end;
end;

function TrmCollectionListBox.TopItemIndex: integer;
{var
  index: integer;
  found: boolean;}
begin
{  index := 0;
  found := false;

  if fVScrollSize > -1 then
  begin
     while (not found) and (index < fItems.Count) do
     begin
       found := (fVScrollPos >= fItems.Items[index].LStart) and (fVScrollPos <= (fItems.Items[index].lStart + fItems.Items[index].LCount));
       if not found then
         inc(index);
     end;
  end
  else
  begin
     if fItems.count > 0 then
        index := 0
     else
        index := -1;

     found := true;
  end;

  if not found then
    result := -1
  else
    result := index;}

  result := fTopIndex;
end;

procedure TrmCollectionListBox.cmFOCUSCHANGED(var MSG: TMessage);
begin
   inherited;
   invalidate;  
end;

procedure TrmCollectionListBox.CMBorderChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;

procedure TrmCollectionListBox.CMCtl3DChanged(var Message: TMessage);
begin
  if NewStyleControls and (FBorderStyle = bsSingle) then RecreateWnd;
  inherited;
end;

end.

