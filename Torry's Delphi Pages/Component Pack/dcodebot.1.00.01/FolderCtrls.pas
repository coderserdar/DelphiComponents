
(********************************************************)
(*                                                      *)
(*  Codebot Class Library @ www.codebot.org/delphi      *)
(*                                                      *)
(*  1.00.01 Open Source Released 2006                   *)
(*                                                      *)
(********************************************************)

unit FolderCtrls;

interface

{$I STD.INC}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, ImgList,
  CommCtrl, MathTools, GraphTools, Dialogs;

const
  CM_FOLDERBASE       = $BD00;
  CM_FOLDERIMAGES     = CM_FOLDERBASE + 1;
  CM_ITEMIMAGES       = CM_FOLDERIMAGES + 1;
  CM_ITEMDETAILS      = CM_ITEMIMAGES + 1;
  CN_ITEMCLICK        = CM_ITEMDETAILS + 1;

{ TFolderItem }

type
  TFolderItems = class;
  TFolderBar = class;
  TFolderBars = class;

  TFolderItemDetails = record
    BorderStyle: TborderStyle;
    Selected: TFolderBar;
    ClientRect: TRect;
    ItemHeight: Integer;
    FolderHeight: Integer;
  end;
  PFolderItemDetails = ^TFolderItemDetails;

  TFolderItem = class(TCollectionItem)
  private
    FCaption: string;
    FEnabled: Boolean;
    FData: Pointer;
    FDataObject: Boolean;
    FImageIndex: Integer;
    FVisible: Boolean;
    procedure SetCaption(const Value: string);
    procedure SetEnabled(Value: Boolean);
    function GetDisplayRect: TRect;
    procedure SetImageIndex(Value: Integer);
    procedure SetVisible(Value: Boolean);
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Click;
    property DisplayRect: TRect read GetDisplayRect;
    property Data: Pointer read FData write FData;
    property DataObject: Boolean read FDataObject write FDataObject;
  published
    property Caption: string read FCaption write SetCaption;
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    property ImageIndex: Integer read FImageIndex write SetImageIndex;
    property Visible: Boolean read FVisible write SetVisible;
  end;

{ TFolderItems }

  TFolderItems = class(TCollection)
  private
    FFolder: TFolderBar;
    FControl: TControl;
  protected
    procedure Update(Item: TCollectionItem); override;
    function Get(Index: Integer): TFolderItem;
    procedure Put(Index: Integer; Value: TFolderItem);
  public
    constructor Create(Control: TControl; Folder: TFolderBar);
    procedure Assign(Source: TPersistent); override;
    function Add: TFolderItem;
    function Insert(Index: Integer): TFolderItem;
    property Items[Index: Integer]: TFolderItem read Get write Put; default;
    property Folder: TFolderBar read FFolder;
    property Control: TControl read FControl;
  end;

{ TFolderBar }

  TFolderBar = class(TCollectionItem)
  private
    FCaption: string;
    FData: Pointer;
    FDataObject: Boolean;
    FImageIndex: Integer;
    FItems: TFolderItems;
    FSelectedIndex: Integer;
    FTopIndex: Integer;
    FVisible: Boolean;
    procedure SetCaption(const Value: string);
    function GetDisplayRect: TRect;
    procedure SetImageIndex(Value: Integer);
    procedure SetItems(Value: TFolderItems);
    procedure SetSelectedIndex(Value: Integer);
    procedure SetTopIndex(Value: Integer);
    procedure SetVisible(Value: Boolean);
  protected
    function GetNearestTop: Integer;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property Data: Pointer read FData write FData;
    property DataObject: Boolean read FDataObject write FDataObject;
    property DisplayRect: TRect read GetDisplayRect;
    property TopIndex: Integer read FTopIndex write SetTopIndex;
  published
    property Caption: string read FCaption write SetCaption;
    property ImageIndex: Integer read FImageIndex write SetImageIndex;
    property Items: TFolderItems read FItems write SetItems;
    property SelectedIndex: Integer read FSelectedIndex write SetSelectedIndex;
    property Visible: Boolean read FVisible write SetVisible;
  end;

{ TFolderBars }

  TFolderBars = class(TCollection)
  private
    FControl: TControl;
  protected
    procedure Update(Item: TCollectionItem); override;
    function Get(Index: Integer): TFolderBar;
    procedure Put(Index: Integer; Value: TFolderBar);
  public
    constructor Create(Control: TControl);
    procedure Assign(Source: TPersistent); override;
    function Add: TFolderBar;
    function Insert(Index: Integer): TFolderBar;
    property Items[Index: Integer]: TFolderBar read Get write Put; default;
    property Control: TControl read FControl;
  end;

{ TCustomFolderView }

  TFolderItemEvent = procedure(Sender: TObject; Item: TFolderItem) of object;

  TFolderScrollButton = (fbScrollUp, fbScrollDown);
  TFolderScrollButtons = set of TFolderScrollButton;

  TCustomFolderView = class(TCustomControl)
  private
    FActiveIndex: Integer;
    FBorderStyle: TBorderStyle;
    FButtons: TFolderScrollButtons;
    FCaptureItem: TFolderItem;
    FChangeLink: TChangeLink;
    FFolderHeight: Integer;
    FFolderImages: TCustomImageList;
    FFolders: TFolderBars;
    FItemImages: TCustomImageList;
    FItemHeight: Integer;
    FMouseItem: TFolderItem;
    FOverlay: TPicture;
    FSelected: TFolderBar;
    FTextHeight: Integer;
    FOnChange: TNotifyEvent;
    FOnItemClick: TFolderItemEvent;
    procedure ImagesChange(Sender: TObject);
    procedure OverlayChange(Sender: TObject);
    procedure SetActiveIndex(Value: Integer);
    procedure SetBorderStyle(Value: TBorderStyle);
    function GetButtonRect(Button: TFolderScrollButton): TRect;
    procedure SetCaptureItem(Value: TFolderItem);
    procedure SetFolderHeight(Value: Integer);
    procedure SetFolderImages(Value: TCustomImageList);
    procedure SetFolders(Value: TFolderBars);
    procedure SetItemImages(Value: TCustomImageList);
    procedure SetOverlay(Value: TPicture);
    procedure SetSelected(Value: TFolderBar);
    function GetSelectedRect: TRect;
    procedure CMDesignHitTest(var Message: TCMDesignHitTest); message CM_DESIGNHITTEST;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMFolderImages(var Message: TMessage); message CM_FOLDERIMAGES;
    procedure CMItemImages(var Message: TMessage); message CM_ITEMIMAGES;
    procedure CMItemDetails(var Message: TMessage); message CM_ITEMDETAILS;
    procedure CNItemClick(var Message: TMessage); message CN_ITEMCLICK;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMMouseWheel(var Message: TWMMouseWheel); message WM_MOUSEWHEEL;
    procedure WMTimer(var Message: TWMTimer); message WM_TIMER;
  protected
    procedure CreateHandle; override;
    procedure DoItemClick(Item: TFolderItem); dynamic;
    function ItemFromPoint(X, Y: Integer): TFolderItem;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure Loaded; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Paint; override;
    procedure UpdateImages(var InternalImages: TCustomImageList;
      ExternalImages: TCustomImageList);
    property ActiveIndex: Integer read FActiveIndex write SetActiveIndex;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle;
    property ButtonRect[Button: TFolderScrollButton]: TRect read GetButtonRect;
    property CaptureItem: TFolderItem read FCaptureItem write SetCaptureItem;
    property FolderHeight: Integer read FFolderHeight write SetFolderHeight;
    property FolderImages: TCustomImageList read FFolderImages write SetFolderImages;
    property Overlay: TPicture read FOverlay write SetOverlay;
    property Folders: TFolderBars read FFolders write SetFolders;
    property ItemImages: TCustomImageList read FItemImages write SetItemImages;
    property Selected: TFolderBar read FSelected write SetSelected;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnItemClick: TFolderItemEvent read FOnItemClick write FOnItemClick;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Update; override;
    function FolderFromPoint(X, Y: Integer): TFolderBar;
  end;

{ TFolderView }

  TFolderView = class(TCustomFolderView)
  public
    property Selected;
    property FolderHeight;
  published
    property Align;
    property ActiveIndex;
    property Anchors;
    property BorderStyle;
    property Color;
    property DesktopFont;
    property DragKind;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property FolderImages;
    property Folders;
    property ItemImages;
    property Overlay;
    property OnItemClick;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property TabOrder;
    property TabStop;
    property OnCanResize;
    property OnDblClick;
    property OnChange;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
  end;

implementation

{$R SHADOW.RES}

{ TFolderItem }

constructor TFolderItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FEnabled := True;
  FVisible := True;
  FImageIndex := -1;
end;

destructor TFolderItem.Destroy;
begin
  if FDataObject and (FData <> nil) then
    TObject(FData).Free;
  inherited Destroy;
end;

procedure TFolderItem.Assign(Source: TPersistent);
var
  Item: TFolderItem absolute Source;
begin
  if Source is TFolderItem then
  begin
    Caption := Item.Caption;
    ImageIndex := Item.ImageIndex;
    Enabled := Item.Enabled;
    Visible := Item.Visible;
  end
  else
    inherited Assign(Source);
end;

procedure TFolderItem.Click;
begin
  (Collection as TFolderItems).Control.Perform(CN_ITEMCLICK, Integer(Self), 0);
end;

procedure TFolderItem.SetCaption(const Value: string);
begin
  if Value <> FCaption then
  begin
    FCaption := Value;
    Changed(True);
  end;
end;

procedure TFolderItem.SetEnabled(Value: Boolean);
begin
  if Value <> FEnabled then
  begin
    FEnabled := Value;
    Changed(True);
  end;
end;

function TFolderItem.GetDisplayRect: TRect;
var
  Folder: TFolderBar;
  TopIndex: Integer;
  Details: TFolderItemDetails;
  VisibleIndex: Integer;
  I: Integer;
begin
  Folder := (Collection as TFolderItems).Folder;
  TopIndex := (Collection as TFolderItems).Control.Perform(CM_ITEMDETAILS,
    Integer(@Details), 0);
  if TopIndex = 0 then
  begin
    SetRectEmpty(Result);
    Exit;
  end;
  TopIndex := Folder.GetNearestTop;
  if (Folder = Details.Selected) and (TopIndex < Index + 1) and FVisible then
  begin
    Result := Folder.DisplayRect;
    OffsetRect(Result, 0, Details.FolderHeight);
    VisibleIndex := 0;
    for I := Folder.GetNearestTop to Index - 1 do
      if Folder.Items[I].Visible then
        Inc(VisibleIndex);
    Result.Top := Result.Top + (Details.ItemHeight * VisibleIndex);
    Result.Bottom := Result.Top + Details.ItemHeight;
  end
  else
    SetRectEmpty(Result);
end;

procedure TFolderItem.SetImageIndex(Value: Integer);
begin
  if Value <> FImageIndex then
  begin
    FImageIndex := Value;
    Changed(True);
  end;
end;

procedure TFolderItem.SetVisible(Value: Boolean);
begin
  if Value <> FVisible then
  begin
    FVisible := Value;
    Changed(True);
  end;
end;

{ TFolderItems }

constructor TFolderItems.Create(Control: TControl; Folder: TFolderBar);
begin
  inherited Create(TFolderItem);
  FFolder := Folder;
  FControl := Control;
end;

procedure TFolderItems.Assign(Source: TPersistent);
var
  FolderItems: TFolderItems absolute Source;
  I: Integer;
begin
  if Source is TFolderItems then
  begin
    BeginUpdate;
    Clear;
    for I := 0 to FolderItems.Count - 1 do
      Add.Assign(FolderItems[I]);
    EndUpdate;
  end
  else
    inherited Assign(Source);
end;

function TFolderItems.Add: TFolderItem;
begin
  Result := inherited Add as TFolderItem;
end;

function TFolderItems.Insert(Index: Integer): TFolderItem;
begin
  Result := inherited Insert(Index) as TFolderItem;
end;

procedure TFolderItems.Update(Item: TCollectionItem);
begin
  inherited Update(Item);
  TFolderBars(FFolder.Collection).Changed;
end;

function TFolderItems.Get(Index: Integer): TFolderItem;
begin
  Result := GetItem(Index) as TFolderItem;
end;

procedure TFolderItems.Put(Index: Integer; Value: TFolderItem);
begin
  SetItem(Index, Value);
end;

{ TFolderBar }

var
  CheckeredBitmap: TBitmap = nil;

constructor TFolderBar.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FVisible := True;
  FItems := TFolderItems.Create((Collection as TFolderBars).Control, Self);
  FImageIndex := -1;
  FSelectedIndex := -1;
end;

procedure TFolderBar.Assign(Source: TPersistent);
var
  Bar: TFolderBar absolute Source;
begin
  if Source is TFolderBar then
  begin
    Caption := Bar.Caption;
    ImageIndex := Bar.ImageIndex;
    SelectedIndex := Bar.SelectedIndex;
    Visible := Bar.Visible;
    Items.Assign(Bar.Items);
  end
  else
    inherited Assign(Source);
end;

destructor TFolderBar.Destroy;
begin
  FItems.Free;
  if FDataObject and (FData <> nil) then
    TObject(FData).Free;
  inherited Destroy;
end;

procedure TFolderBar.SetCaption(const Value: string);
begin
  if Value <> FCaption then
  begin
    FCaption := Value;
    Changed(True);
  end;
end;

function TFolderBar.GetDisplayRect: TRect;
var
  Folders: TFolderBars;
  Details: TFolderItemDetails;
  Rect: TRect;
  I: Integer;
begin
  Folders := Collection as TFolderBars;
  if Folders.Control.Perform(CM_ITEMDETAILS, Integer(@Details), 0) = 0 then
  begin
    SetRectEmpty(Result);
    Exit;
  end;
  if Visible then
  begin
    Result := Details.ClientRect;
    if (Details.Selected <> nil) and (Index > Details.Selected.Index) then
      Result.Top := Result.Bottom - Details.FolderHeight * (Folders.Count - Index)
    else
      Result.Top := Index * Details.FolderHeight;
    Result.Bottom := Result.Top + Details.FolderHeight;
    if Details.BorderStyle = bsSingle then
    begin
      InflateRect(Result, -GetBorder, 0);
      if (Details.Selected <> nil) and (Index > Details.Selected.Index) then
        OffsetRect(Result, 0, -GetBorder)
      else
        OffsetRect(Result, 0, GetBorder);
    end;
    for I := Index - 1 downto 0 do
      if Folders[I].Visible then
      begin
        Rect := Folders[I].DisplayRect;
        if Result.Top < Rect.Bottom then
          OffsetRect(Result, 0, Rect.Bottom - Result.Top)
        else if (Details.Selected = nil) or (Index <= Details.Selected.Index) then
          OffsetRect(Result, 0, Rect.Bottom - Result.Top);
        Break;
      end
      else if I = 0 then
        OffsetRect(Result, 0, -Result.Top);
  end
  else
    SetRectEmpty(Result);
end;

procedure TFolderBar.SetImageIndex(Value: Integer);
begin
  if Value <> FImageIndex then
  begin
    FImageIndex := Value;
    Changed(True);
  end;
end;

procedure TFolderBar.SetItems(Value: TFolderItems);
begin
  if Value <> FItems then
    FItems.Assign(Value);
end;

function TFolderBar.GetNearestTop: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Items.Count - 1 do
    if Items[I].Visible then
      if Result = FTopIndex then
      begin
        Result := I;
        Break;
      end
      else
        Inc(Result);
end;

procedure TFolderBar.SetSelectedIndex(Value: Integer);
begin
  if Value <> FSelectedIndex then
  begin
    FSelectedIndex := Value;
    Changed(True);
  end;
end;

procedure TFolderBar.SetTopIndex(Value: Integer);
begin
  if Value > Items.Count - 1 then
    Value := Items.Count - 1;
  if Value < 0 then
    Value := 0;
  if Value <> FTopIndex then
  begin
    FTopIndex := Value;
    Changed(True);
  end;
end;

procedure TFolderBar.SetVisible(Value: Boolean);
begin
  if Value <> FVisible then
  begin
     FVisible := Value;
     Changed(True);
  end;
end;

{ TFolderBars }

constructor TFolderBars.Create(Control: TControl);
begin
  inherited Create(TFolderBar);
  FControl := Control;
end;

procedure TFolderBars.Assign(Source: TPersistent);
var
  Bars: TFolderBars absolute Source;
  I: Integer;
begin
  if Source is TFolderBars then
  begin
    BeginUpdate;
    Clear;
    for I := 0 to Bars.Count - 1 do
      Add.Assign(Bars[I]);
    EndUpdate;
  end
  else
    inherited Assign(Source);
end;

function TFolderBars.Add: TFolderBar;
begin
  Result := inherited Add as TFolderBar;
end;

function TFolderBars.Insert(Index: Integer): TFolderBar;
begin
  Result := inherited Insert(Index) as TFolderBar;
end;

procedure TFolderBars.Update(Item: TCollectionItem);
begin
  inherited Update(Item);
  if FControl <> nil then
    FControl.Update;
end;

function TFolderBars.Get(Index: Integer): TFolderBar;
begin
  if (Index > -1) and (Index < Count) then
    Result := GetItem(Index) as TFolderBar
  else
    Result := nil;
end;

procedure TFolderBars.Put(Index: Integer; Value: TFolderBar);
begin
  SetItem(Index, Value);
end;

{ TCustomFolderView }

constructor TCustomFolderView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Color := clAppWorkspace;
  Height := 250;
  Width := 150;
  FActiveIndex := -1;
  FBorderStyle := bsSingle;
  FFolderHeight := 25;
  FItemHeight := 32;
  FFolders := TFolderBars.Create(Self);
  FOverlay := TPicture.Create;
  FOverlay.OnChange := OverlayChange;
  FChangeLink := TChangeLink.Create;
  FChangeLink.OnChange := ImagesChange;
end;

destructor TCustomFolderView.Destroy;
begin
  UpdateImages(FFolderImages, nil);
  UpdateImages(FItemImages, nil);
  FFolders.Free;
  FChangeLink.Free;
  FOverlay.OnChange := nil;
  FOverlay.Free;
  inherited Destroy;
end;

procedure TCustomFolderView.ImagesChange(Sender: TObject);
var
  DC: HDC;
  F: HFont;
  ImageHeight: Integer;
begin
  DC := GetDC(0);
  F := SelectObject(DC, Font.Handle);
  FTextHeight := CalculateCaptionSize(DC, ' ').cY;
  SelectObject(DC, F);
  ReleaseDC(0, DC);
  if FFolderImages <> nil then
    ImageHeight := FFolderImages.Height
  else
    ImageHeight := 0;
  if ImageHeight > FTextHeight then
    FFolderHeight := ImageHeight
  else
    FFolderHeight := FTextHeight;
  Inc(FFolderHeight, 9);
  if FItemImages <> nil then
    ImageHeight := FItemImages.Height
  else
    ImageHeight := 0;
  if ImageHeight > FTextHeight then
    FItemHeight := ImageHeight
  else
    FItemHeight := FTextHeight;
  Inc(FItemHeight, FTextHeight * 2);
  Invalidate;
end;

procedure TCustomFolderView.DoItemClick(Item: TFolderItem);
begin
  CaptureItem := nil;
  if Assigned(FOnItemClick) then
    FOnItemClick(Self, Item);
end;

procedure TCustomFolderView.CreateHandle;
begin
  inherited CreateHandle;
  if FTextHeight = 0 then
    FTextHeight := Canvas.TextHeight(' ');
end;

function TCustomFolderView.FolderFromPoint(X, Y: Integer): TFolderBar;
var
  P: TPoint;
  I: Integer;
begin
  P := Point(X, Y);
  Result := nil;
  for I := 0 to FFolders.Count - 1 do
    if PtInRect(Folders[I].DisplayRect, P) then
    begin
      Result := FFolders[I];
      Break;
    end;
end;

function TCustomFolderView.ItemFromPoint(X, Y: Integer): TFolderItem;
var
  P: TPoint;
  I: Integer;
begin
  P := Point(X, Y);
  Result := nil;
  if FolderFromPoint(X, Y) <> nil then Exit;
  if Selected <> nil then
    for I := 0 to Selected.Items.Count - 1 do
      if PtInRect(Selected.Items[I].DisplayRect, P) then
      begin
        Result := Selected.Items[I];
        Break;
      end;
end;

procedure TCustomFolderView.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  case Key of
    VK_UP:
      if (Selected <> nil) and (Selected.Index > 0) then
        Selected := FFolders[Selected.Index - 1];
    VK_DOWN:
      if (Selected <> nil) and (Selected.Index < FFolders.Count - 1) then
        Selected := FFolders[Selected.Index + 1];
  end;
end;

procedure TCustomFolderView.Loaded;
begin
  inherited Loaded;
  ActiveIndex := FActiveIndex;
end;

procedure TCustomFolderView.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  Point: TPoint;
  Folder: TFolderBar;
  ScrollButton: TFolderScrollButton;
  Rect: TRect;
begin
  inherited MouseDown(Button, Shift, X, Y);
  if Button = mbLeft then
  begin
    SetFocus;
    Point := GetPoint(X, Y);
    FButtons := [];
    for ScrollButton := Low(TFolderScrollButton) to High(TFolderScrollButton) do
      if PtInRect(ButtonRect[ScrollButton], Point) then
      begin
        Include(FButtons, ScrollButton);
        Rect := ButtonRect[ScrollButton];
        InvalidateRect(Handle, @Rect, False);
        if CaptureItem <> nil then
        begin
          Rect := CaptureItem.DisplayRect;
          InvalidateRect(Handle, @Rect, False);
        end;
        CaptureItem := nil;
        SetTimer(Handle, 1, 125, nil);
        Exit;
      end;
    Folder := FolderFromPoint(X, Y);
    if Folder <> nil then
    begin
      Selected := Folder;
      Exit;
    end;
    CaptureItem := ItemFromPoint(X, Y);
    if CaptureItem <> nil then
    begin
      Rect := CaptureItem.DisplayRect;
      InvalidateRect(Handle, @Rect, False);
    end;
  end;
end;

procedure TCustomFolderView.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);
  if not MouseCapture then
    CaptureItem := ItemFromPoint(X, Y);
end;

procedure TCustomFolderView.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  Point: TPoint;
  ScrollButton: TFolderScrollButton;
  Rect: TRect;
begin
  inherited MouseUp(Button, Shift, X, Y);
  if not (csDesigning in ComponentState) then
    if Button = mbLeft then
    begin
      Point := GetPoint(X, Y);
      if FButtons <> [] then
        for ScrollButton := Low(TFolderScrollButton) to High(TFolderScrollButton) do
          if PtInRect(ButtonRect[ScrollButton], Point) then
            if ScrollButton in FButtons then
            begin
              CaptureItem := nil;
              FButtons := [];
              Exit;
            end;
      if FButtons <> [] then
      begin
        FButtons := [];
        Invalidate;
      end;
      if CaptureItem <> nil then
      begin
        Rect := CaptureItem.DisplayRect;
        InvalidateRect(Handle, @Rect, False);
        if CaptureItem = ItemFromPoint(X, Y) then
          DoItemClick(CaptureItem);
      end;
      with Mouse.CursorPos do
        CaptureItem := ItemFromPoint(X, Y);
    end;
end;

procedure TCustomFolderView.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent <> nil) then
    if AComponent = FFolderImages then
      UpdateImages(FFolderImages, nil)
    else if AComponent = FItemImages then
      UpdateImages(FItemImages, nil);
end;

procedure TCustomFolderView.Update;
begin
  ActiveIndex := FActiveIndex;
end;

procedure TCustomFolderView.UpdateImages(var InternalImages: TCustomImageList;
  ExternalImages: TCustomImageList);
begin
  if InternalImages <> nil then
  begin
    InternalImages.UnRegisterChanges(FChangeLink);
    InternalImages.RemoveFreeNotification(Self);
  end;
  InternalImages := ExternalImages;
  if InternalImages <> nil then
  begin
    InternalImages.RegisterChanges(FChangeLink);
    InternalImages.FreeNotification(Self);
  end;
  Invalidate;
end;

procedure TCustomFolderView.OverlayChange(Sender: TObject);
begin
  FOverlay.Bitmap.Transparent := True;
  FOverlay.Bitmap.TransparentMode := tmAuto;
  Invalidate;
end;

procedure TCustomFolderView.Paint;
const
  Shadow: HICON = 0;

  procedure DrawButtons(DC: HDC; Brush: HBRUSH);
  const
    Arrows: array[TFolderScrollButton] of TDirection = (drUp, drDown);
  var
    Button: TFolderScrollButton;
    Rect: TRect;
    Down: Boolean;
    State: TDrawState;
  begin
    for Button := Low(TFolderScrollButton) to High(TFolderScrollButton) do
    begin
      Rect := ButtonRect[Button];
      if not IsRectEmpty(Rect) then
      begin
        if not ThemePainter.Available then
          FillRect(DC, Rect, COLOR_3DFACE + 1);
        Down := MouseCapture and (Button in FButtons);
        if Down then
          State := [dsPressed]
        else
          State := [dsHot];
        DrawThemeScroll(DC, Arrows[Button], Rect, State);
        SelectClipRect(DC, Rect, RGN_DIFF);
      end;
    end;
  end;

var
  DC: HDC;
  Rect: TRect;
  DrawRect: TRect;
  Brush: HBRUSH;
  CheckeredBrush: HBRUSH;
  Folder: TFolderBar;
  Item: TFolderItem;
  Point: TPoint;
  OverlayDrawn: Boolean;
  I: Integer;
  J: Integer;
begin
  inherited Paint;
  if (Shadow = 0) and ThemePainter.Enabled then
    Shadow := LoadIcon(MainInstance, 'SHADOW');
  Rect := ClientRect;
  DC := Canvas.Handle;
  if FBorderStyle = bsSingle then
  begin
    DrawThemeBorder(DC, Color, Rect, []);
    InflateRect(Rect, -GetBorder, -GetBorder);
    SelectClipRect(DC, Rect, RGN_AND);
  end;
  Brush := GetSysColorBrush(COLOR_BTNFACE);
  CheckeredBrush := GetBrush(CheckeredBitmap);
  for I := 0 to FFolders.Count -1 do
  begin
    Folder := FFolders[I];
    if not Folder.Visible then Continue;
    DrawRect := Folder.DisplayRect;
    if ThemePainter.Enabled then
    begin
      FillRect(DC, DrawRect, ColorToRGB(Color));
			Inc(DrawRect.Right, 10);
      if Folder = Selected then
        ThemePainter.DrawElement(DC, ThemePainter.GetDetails(thHeaderItemHot), DrawRect)
      else
        ThemePainter.DrawElement(DC, ThemePainter.GetDetails(thHeaderItemNormal), DrawRect);
			Dec(DrawRect.Right, 10);
    end
    else
    begin
      DrawFrame(DC, DrawRect, dfRaised);
      InflateRect(DrawRect, -1, -1);
      if Folder = Selected then
        FillRect(DC, DrawRect, CheckeredBrush)
      else
        FillRect(DC, DrawRect, Brush);
    end;
    InflateRect(DrawRect, -6, 0);
    with Folder do
      if FFolderImages <> nil then
      begin
        if (Folder = Selected) and (SelectedIndex > -1) then
        begin
          ImageList_DrawEx(FFolderImages.Handle, Folder.SelectedIndex, DC, DrawRect.Left,
            DrawRect.Top + (FFolderHeight - FFolderImages.Height) div 2, 0, 0,
            CLR_NONE, CLR_NONE, ILD_NORMAL);
        end
        else if Folder.ImageIndex > -1 then
          ImageList_DrawEx(FFolderImages.Handle, ImageIndex, DC, DrawRect.Left,
            DrawRect.Top + (FFolderHeight - FFolderImages.Height) div 2, 0, 0,
            CLR_NONE, CLR_NONE, ILD_NORMAL);
        Inc(DrawRect.Left, FFolderImages.Width + 2);
      end;
    InflateRect(DrawRect, -1, -1);
    SetTextColor(DC, GetSysColor(COLOR_WINDOWTEXT));
    SetBkMode(DC, TRANSPARENT);
    DrawCaption(DC, Folder.Caption, DrawRect, drLeft);
    InflateRect(DrawRect, 2, 2);
    if FFolderImages <> nil then
      Dec(DrawRect.Left, FFolderImages.Width + 2);
    DrawRect := Folder.DisplayRect;
    SelectClipRect(DC, DrawRect, RGN_DIFF);
    if FOverlay <> nil then
      with DrawRect, FOverlay.Bitmap do
      BitBlt(Dc, Left, Bottom, Width, Height, Canvas.Handle, 0, 0, SRCCOPY);
  end;
  DeleteObject(Brush);
  Folder := Selected;
  if Folder <> nil then
  begin
    DrawButtons(DC, CheckeredBrush);
    Brush := CreateSolidBrush(ColorToRGB(Color));
    for J := Folder.GetNearestTop to Folder.Items.Count - 1 do
    begin
      Item := Folder.Items[J];
      if not Item.Visible then Continue;
      DrawRect := Item.DisplayRect;
      if FItemImages <> nil then
        Point.X := (DrawRect.Right - DrawRect.Left) div 2 - FItemImages.Width div 2;
      Point.Y := DrawRect.Top + FTextHeight div 2;
      if (FCaptureItem <> nil) and (FCaptureItem = Item) then
      begin
        if ThemePainter.Enabled then
          with DrawRect do
          begin
            DrawIconEx(DC, Left, Top, Shadow, 1, 1, 0 , 0, DI_NORMAL);
            DrawIconEx(DC, Left, Top, Shadow, WidthOf(DrawRect),
              HeightOf(DrawRect), 0 , 0, DI_NORMAL);
            if MouseCapture then
            begin
              DrawIconEx(DC, Right - 1, Top, Shadow, 1, HeightOf(DrawRect), 0 , 0, DI_NORMAL);
              DrawIconEx(DC, Left + 1, Bottom - 1, Shadow, WidthOf(DrawRect) - 2, 1, 0 , 0, DI_NORMAL);
              DrawIconEx(DC, Left, Top, Shadow, 2, HeightOf(DrawRect), 0 , 0, DI_NORMAL);
              DrawIconEx(DC, Left + 2, Top, Shadow, WidthOf(DrawRect) - 4, 2, 0 , 0, DI_NORMAL);
              DrawIconEx(DC, Left, Top, Shadow, WidthOf(DrawRect),
                HeightOf(DrawRect), 0 , 0, DI_NORMAL);
            end;
          end
        else
        if MouseCapture then
          DrawThemeThinButton(DC, DrawRect, [dsPressed])
        else
          DrawThemeThinButton(DC, DrawRect, [dsHot]);
        InflateRect(DrawRect, -1, -1);
        if (not ThemePainter.Enabled) and (Overlay = nil) then
          FillRect(DC, DrawRect, Brush);
      end
      else if Overlay = nil then
        FillRect(DC, DrawRect, Brush);
      Dec(DrawRect.Right, 4);
      if (FCaptureItem <> nil) and (FCaptureItem = Item) then
        if MouseCapture then
        begin
          OffsetRect(DrawRect, 1, 1);
          Inc(Point.X);
          Inc(Point.Y);
        end;
      if (FItemImages <> nil) and (Item.ImageIndex > -1) then
      begin
        ImageList_DrawEx(FItemImages.Handle, Item.ImageIndex, DC, Point.X,
          Point.Y, 0, 0, CLR_NONE, CLR_NONE, ILD_NORMAL);
        if (not Item.Enabled) and (FOverlay.Height > 0) then
          with FOverlay, DrawRect do
            Canvas.Draw(Left + (Right - Left - Width) div 2 - FTextHeight div 4,
              Top + (Bottom - Top - Height) div 2 - FTextHeight div 2, Bitmap);
      end;
      SetTextColor(DC, GetTextColor(Color));
      SetBkMode(DC, TRANSPARENT);
      if (FCaptureItem <> nil) and (FCaptureItem = Item) then
        OffsetRect(DrawRect, 0, 1);
      Dec(DrawRect.Right, 4);
      DrawRect.Top := DrawRect.Bottom - Trunc(FTextHeight * 1.5);
      DrawCaption(DC, Item.Caption, DrawRect, drCenter);
      if (FCaptureItem <> nil) and (FCaptureItem = Item) then
      begin
        InflateRect(DrawRect, 1, 1);
        if MouseCapture then
          OffsetRect(DrawRect, -1, -1);
      end;
    end;
    DeleteObject(Brush);
  end;
  DeleteObject(CheckeredBrush);
  Item := nil;
  if Selected <> nil then
    for I := Selected.Items.Count - 1 downto 0 do
      if Selected.Items[I].Visible then
      begin
        Item := Selected.Items[I];
        Break;
      end;
  if Item <> nil then
  begin
    Rect.Top := Item.DisplayRect.Bottom;
    I := Selected.Index + 1;
    for J := I to Folders.Count - 1 do
      if Folders[J].Visible then
      begin
        Rect.Bottom := Folders[J].DisplayRect.Top;
        Break;
      end;
    if Rect.Bottom < Rect.Top then
      Rect.Bottom := Rect.Top;
  end
  else if Selected <> nil then
  begin
    Rect.Top := Selected.DisplayRect.Top + FFolderHeight;
    I := Selected.Index + 1;
    for J := Folders.Count - 1 downto I do
      if Folders[J].Visible then
      begin
        Rect.Bottom := Folders[J].DisplayRect.Top;
        Break;
      end;
  end
  else
  begin
    Rect.Top := 0;
    for I := Folders.Count - 1 downto 0 do
      if Folders[I].Visible then
      begin
        Rect.Top := Folders[I].DisplayRect.Bottom;
        Break;
      end;
  end;
  if (Overlay = nil) or (Selected = nil) then
  begin
  	Brush := CreateSolidBrush(ColorToRGB(Color));
    FillRect(DC, Rect, Brush);
  	DeleteObject(Brush);
  end;
end;

procedure TCustomFolderView.SetActiveIndex(Value: Integer);
var
  Form: TCustomForm;
begin
  FActiveIndex := Value;
  if csLoading in ComponentState then Exit;
  if FActiveIndex < -1 then
    FActiveIndex := -1;
  if FActiveIndex > FFolders.Count - 1 then
    FActiveIndex := FFolders.Count - 1;
  if (FActiveIndex > -1) and (FFolders[FActiveIndex].Visible) then
    FSelected := FFolders[FActiveIndex]
  else
    FSelected := nil;
  Invalidate;
  if csDesigning in ComponentState then
  begin
    Form := GetParentForm(Self);
    if Form <> nil then
      Form.Designer.Modified;
  end;
end;

procedure TCustomFolderView.SetBorderStyle(Value: TBorderStyle);
begin
  if Value <> FBorderStyle then
  begin
    FBorderStyle := Value;
    Invalidate;
  end;
end;

function TCustomFolderView.GetButtonRect(Button: TFolderScrollButton): TRect;
var
	ButtonHeight: Integer;
  Rect: TRect;
  Counter: Integer;
  I: Integer;
begin
  ButtonHeight := GetSystemMetrics(SM_CXVSCROLL) + 4;
  SetRectEmpty(Result);
  if Selected <> nil then
  begin
    Rect := GetSelectedRect;
    if HeightOf(Rect) > ButtonHeight * 2 + 8 then
      case Button of
        fbScrollUp:
          if Selected.TopIndex > 0 then
            with Rect do
              Result := GetRect(Right - ButtonHeight - 4,
                Top + 4, Right - 4, Top + ButtonHeight + 4);
        fbScrollDown:
          begin
            Counter := 0;
            for I := Selected.Items.Count - 1 downto 1 do
              if Selected.Items[I].Visible then
              begin
                if Counter = 1 then
                  Break;
                with Rect do
                  if Selected.Items[I].DisplayRect.Bottom > Bottom then
                    Result := GetRect(Right - ButtonHeight - 4,
                      Bottom - ButtonHeight - 4, Right - 4,
                      Bottom - 4);
                Inc(Counter);
              end;
            if Counter = 0 then
              SetRectEmpty(Result);
          end;
      end;
  end;
end;

procedure TCustomFolderView.SetCaptureItem(Value: TFolderItem);
var
  Rect: tRect;
begin
  if FButtons <> [] then
    Value := nil;
  if Value <> FCaptureItem then
  begin
    if FCaptureItem <> nil then
    begin
      Rect := FCaptureItem.DisplayRect;
      InvalidateRect(Handle, @Rect, False);
    end;
    FCaptureItem := Value;
    if Value <> nil then
      if FCaptureItem.Enabled then
      begin
        Rect := FCaptureItem.DisplayRect;
        InvalidateRect(Handle, @Rect, False);
      end
      else
        FCaptureItem := nil;
  end;
end;

procedure TCustomFolderView.SetFolderHeight(Value: Integer);
begin
  if Value <> FFolderHeight then
  begin
    FFolderHeight := Value;
    Invalidate;
  end;
end;

procedure TCustomFolderView.SetFolderImages(Value: TCustomImageList);
begin
  if Value <> FFolderImages then
  begin
    UpdateImages(FFolderImages, Value);
    ImagesChange(FFolderImages);
  end;
end;

procedure TCustomFolderView.SetFolders(Value: TFolderBars);
begin
  if Value <> FFolders then
    FFolders.Assign(Value);
end;

procedure TCustomFolderView.SetItemImages(Value: TCustomImageList);
begin
  if Value <> FItemImages then
  begin
    UpdateImages(FItemImages, Value);
    ImagesChange(FItemImages);
  end;
end;

procedure TCustomFolderView.SetOverlay(Value: TPicture);
begin
  FOverlay.Assign(Value);
  Invalidate;
end;

procedure TCustomFolderView.SetSelected(Value: TFolderBar);
begin
  if Value <> FSelected then
  begin
    FSelected := Value;
    if FSelected = nil then
      FActiveIndex := -1
    else
      FActiveIndex := FSelected.Index;
    Invalidate;
    if Assigned(FOnChange) then
      FOnChange(Self);
  end;
end;

function TCustomFolderView.GetSelectedRect: TRect;
var
  I: Integer;
begin
  Result := ClientRect;
  if FBorderStyle = bsSingle then
    InflateRect(Result, -GetBorder, -GetBorder);
  if Selected <> nil then
  begin
    Result.Top := Selected.DisplayRect.Bottom;
    for I := Selected.Index + 1 to Folders.Count - 1 do
      if Folders[I].Visible then
      begin
        Result.Bottom := Folders[I].DisplayRect.Top;
        Break;
      end;
  end
  else
    SetRectEmpty(Result);
end;

procedure TCustomFolderView.CMDesignHitTest(var Message: TCMDesignHitTest);
const
  HitTests: array[Boolean] of Integer = (0, 1);
begin
  inherited;
  with Message do
    Result := HitTests[FolderFromPoint(XPos, YPos) <> nil];
end;

procedure TCustomFolderView.CMFontChanged(var Message: TMessage);
begin
  inherited;
  Canvas.Font := Font;
  FTextHeight := Canvas.TextHeight(' ');
  if FFolderImages <> nil then
    ImagesChange(FFolderImages);
  if FItemImages <> nil then
    ImagesChange(FItemImages);
end;

procedure TCustomFolderView.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  CaptureItem := FMouseItem;
  FMouseItem := nil;
end;

procedure TCustomFolderView.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if MouseCapture then
    FMouseItem := CaptureItem
  else
    FMouseItem := nil;
  CaptureItem := nil;
end;

procedure TCustomFolderView.CMFolderImages(var Message: TMessage);
begin
	Message.Result := Integer(FFolderImages);
end;

procedure TCustomFolderView.CMItemImages(var Message: TMessage);
begin
	Message.Result := Integer(FItemImages);
end;

procedure TCustomFolderView.CMItemDetails(var Message: TMessage);
var
  Details: PFolderItemDetails;
begin
  Details := PFolderItemDetails(Message.WParam);
  Details.BorderStyle := BorderStyle;
  Details.Selected := Selected;
  Details.ClientRect := ClientRect;
  Details.ItemHeight := FItemHeight;
  Details.FolderHeight := FFolderHeight;
  Message.Result := 1;
end;

procedure TCustomFolderView.CNItemClick(var Message: TMessage);
begin
  DoItemClick(TFolderItem(Message.WParam));
  Message.Result := 1;
end;

procedure TCustomFolderView.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  Message.Result := 1;
end;

procedure TCustomFolderView.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  Message.Result := DLGC_WANTARROWS;
end;

procedure TCustomFolderView.WMMouseWheel(var Message: TWMMouseWheel);
var
  Delta: Integer;
begin
  if Selected <> nil then
  begin
    Delta := -Message.WheelDelta div 120;
    if (not IsRectEmpty(ButtonRect[fbScrollUp])) and (Delta = -1) then
      Selected.TopIndex := Selected.TopIndex - 1
    else if (not IsRectEmpty(ButtonRect[fbScrollDown])) and (Delta = 1) then
      Selected.TopIndex := Selected.TopIndex + 1;
    if not MouseCapture then
      with Message, ScreenToClient(Point(XPos, YPos)) do
          CaptureItem := ItemFromPoint(X, Y);
  end;
  inherited;
end;

procedure TCustomFolderView.WMTimer(var Message: TWMTimer);
begin
  if FButtons = [] then
  begin
    KillTimer(Handle, 1);
    Exit;
  end;
  with Selected do
    if fbScrollUp in FButtons then
    begin
      TopIndex := TopIndex - 1;
      if IsRectEmpty(ButtonRect[fbScrollUp]) then
        FButtons := [];
    end
    else
    begin
      TopIndex := TopIndex + 1;
      if IsRectEmpty(ButtonRect[fbScrollDown]) then
        FButtons := [];
    end;
  if FButtons = [] then
    KillTimer(Handle, 1);
end;

initialization
  CheckeredBitmap := GetBitmap(clBtnFace, clBtnHighlight);
finalization
  CheckeredBitmap.Free;
end.
