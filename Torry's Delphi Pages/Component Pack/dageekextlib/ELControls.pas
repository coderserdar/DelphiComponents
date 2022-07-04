{*******************************************************}
{                                                       }
{       Extension Library                               }
{       Controls Unit                                   }
{                                                       }
{       (c) 1999 - 2002, Balabuyev Yevgeny              }
{       E-mail: stalcer@rambler.ru                      }
{                                                       }
{*******************************************************}

unit ELControls;

interface

uses
  Windows, Classes, ShellAPI, Graphics, Menus, Messages, SysUtils, Forms,
  Dialogs, Controls, ExtCtrls, ELSConsts, ELUtils;


  { TELObjectList }

type
  EELObjectList = class(Exception);
  TELItemByProc = procedure(AItem: TObject; AData: Pointer; var AResult: Boolean) of object;

  TELObjectList = class
  private
    FItems: TList;
    FChangingCount: Boolean;
    function GetItems(AIndex: Integer): TObject;
    function GetCount: Integer;
    procedure SetCount(const Value: Integer);
  protected
    function CreateItem: TObject; virtual;
    procedure ValidateAddition; virtual;
    procedure ValidateDeletion; virtual;
    procedure Change; virtual;
    procedure Added; virtual;
    procedure Deleted; virtual;
    function DoItemBy(AData: Pointer; AItemByProc: TELItemByProc): TObject;
    function DoFind(AData: Pointer; AItemByProc: TELItemByProc): TObject;
    function DoSearch(AData: Pointer; AItemByProc: TELItemByProc): TObject;
  public
    constructor Create;
    destructor Destroy; override;
    function Add: Integer;
    procedure Remove(AItem: TObject);
    procedure Delete(AIndex: Integer);
    procedure Clear;
    function IndexOf(AItem: TObject): Integer;
    property Items[AIndex: Integer]: TObject read GetItems; default;
    property Count: Integer read GetCount write SetCount;
  end;


  { TELStringList }

type
  TELStringList = class(TComponent)
  private
    FLines: TStrings;
    FOnChange: TNotifyEvent;
    FOnChanging: TNotifyEvent;
    procedure SetLines(const Value: TStrings);
    function GetSorted: Boolean;
    procedure SetSorted(const Value: Boolean);
    function GetCapacity: Integer;
    procedure SetCapacity(const Value: Integer);
    procedure FLinesOnChange(Sender: TObject);
    procedure FLinesOnChanging(Sender: TObject);
  protected
    procedure Change; virtual;
    procedure Changing; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Capacity: Integer read GetCapacity write SetCapacity;
  published
    property Lines: TStrings read FLines write SetLines;
    property Sorted: Boolean read GetSorted write SetSorted default False;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnChanging: TNotifyEvent read FOnChanging write FOnChanging;
  end;


  { TELEventSender, TELEvent }

type
  TELEvent = class;

  TELEventSender = class(TComponent)
  private
    FEvents: TList;
    FSendingEvents: TList;
    function GetEvents(AI: Integer): TELEvent;
    function GetEventCount: Integer;
    procedure SortEvents;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SendEvent(AParam: Pointer);
    procedure AddEvent(AValue: TELEvent);
    procedure RemoveEvent(AValue: TELEvent);
    property Events[AI: Integer]: TELEvent read GetEvents;
    property EventCount: Integer read GetEventCount;
  end;

  TELEventOnEventEvent = procedure(Sender: TObject; AEventSender: TELEventSender;
    AParam: Pointer) of object;

  TELEvent = class(TComponent)
  private
    FGroup: Integer;
    FSource: TELEventSender;
    FOnEvent: TELEventOnEventEvent;
    procedure SetGroup(const Value: Integer);
    procedure SetSource(const Value: TELEventSender);
  protected
    procedure Event(AEventSender: TELEventSender; AParam: Pointer); virtual;
  public
    destructor Destroy; override;
  published
    property Group: Integer read FGroup write SetGroup default 0;
    property Source: TELEventSender read FSource write SetSource;
    property OnEvent: TELEventOnEventEvent read FOnEvent write FOnEvent;
  end;


  { TELTrayIcon }

type
  EELTrayIcon = class(Exception);

  TELTrayIcon = class;

  TELTrayIconAnimationsItem = class(TCollectionItem)
  private
    FCircular: Boolean;
    FInterval: Integer;
    FImageList: TImageList;
    FRestoreOldIcon: Boolean;
    function GetActive: Boolean;
    procedure SetActive(const Value: Boolean);
    procedure SetImageList(const Value: TImageList);
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
    procedure Start;
    procedure Stop;
  published
    property Active: Boolean read GetActive write SetActive default False;
    property ImageList: TImageList read FImageList write SetImageList;
    property Interval: Integer read FInterval write FInterval default 1000;
    property Circular: Boolean read FCircular write FCircular;
    property RestoreOldIcon: Boolean read FRestoreOldIcon write FRestoreOldIcon default True;
  end;

  TELTrayIconAnimations = class(TCollection)
  private
    FOwnerTrayIcon: TELTrayIcon;
    FActiveItem: TELTrayIconAnimationsItem;
    FTimer: TTimer;
    FActiveImageIndex: Integer;
    FOldIcon: TIcon;
    function GetItems(AIndex: Integer): TELTrayIconAnimationsItem;
    procedure SetItems(AIndex: Integer; const Value: TELTrayIconAnimationsItem);
    procedure FTimerOnTimer(Sender: TObject);
    procedure RemoveImageList(AImageList: TImageList);
    procedure SetActiveItem(AItem: TELTrayIconAnimationsItem);
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(AOwnerTrayIcon: TELTrayIcon);
    destructor Destroy; override;
    property Items[AIndex: Integer]: TELTrayIconAnimationsItem read GetItems write SetItems; default;
    property ActiveItem: TELTrayIconAnimationsItem read FActiveItem;
  end;

  TELTrayIconState = set of (tisIconAdded, tisAnimationSetingIcon);

  TELTrayIcon = class(TComponent)
  private
    FState: TELTrayIconState;
    FHWnd: THandle;
    FActive: Boolean;
    FHint: string;
    FIcon: TIcon;
    FOnClick: TNotifyEvent;
    FOnDblClick: TNotifyEvent;
    FPopupMenu: TPopupMenu;
    FOnContextPopup: TContextPopupEvent;
    FHideTaskBarButton: Boolean;
    FAnimations: TELTrayIconAnimations;
    procedure SetActive(const Value: Boolean);
    procedure SetHint(const Value: string);
    procedure SetIcon(const Value: TIcon);
    procedure SetPopupMenu(const Value: TPopupMenu);
    procedure SetHideTaskBarButton(const Value: Boolean);
    procedure SetAnimations(const Value: TELTrayIconAnimations);
    procedure FIconOnChange(Sender: TObject);
    procedure UpdateTaskBarButtonVisible;
    procedure DoContextPopup;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Loaded; override;
    procedure NotifyIcon(AMessage: Cardinal);
    procedure WndProc(var AMessage: TMessage); virtual;
    function GetPopupMenu: TPopupMenu; virtual;
    procedure Click; virtual;
    procedure DblClick; virtual;
    procedure ContextPopup(const MousePos: TPoint; var Handled: Boolean); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Active: Boolean read FActive write SetActive default False;
    property Hint: string read FHint write SetHint;
    property Icon: TIcon read FIcon write SetIcon;
    property PopupMenu: TPopupMenu read FPopupMenu write SetPopupMenu;
    property HideTaskBarButton: Boolean read FHideTaskBarButton write SetHideTaskBarButton default False;
    property Animations: TELTrayIconAnimations read FAnimations write SetAnimations;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
    property OnDblClick: TNotifyEvent read FOnDblClick write FOnDblClick;
    property OnContextPopup: TContextPopupEvent read FOnContextPopup write FOnContextPopup;
  end;


  { TELDataStorage }

type
  PELDataStorageDataBlock = ^TELDataStorageDataBlock;
  TELDataStorageDataBlock = record
    Index: Integer;
    Data: string;
  end;

  TELDataStorage = class
  private
    FDataList: TList;
  public
    constructor Create;
    destructor Destroy; override;
    function AddDataBlock(const AData: string): Integer;
    function GetDataBlock(AHandle: Integer): string;
    procedure DeleteDataBlock(AHandle: Integer);
  end;


  { TELInstanceChecker }

type
  TELInstanceCheckerOnResieveDataEvent = procedure(Sender: TObject;
    const AData: string) of object;

  EELInstanceChecker = class(Exception);

  TELInstanceChecker = class
  private
    FObjectName: string;
    FHandle: THandle;
    FFileMapObjHandle: THandle;
    FOnResieveData: TELInstanceCheckerOnResieveDataEvent;
    FDataStorage: TELDataStorage;
    FInstanceRegistered: Boolean;
  protected
    procedure WndProc(var AMessage: TMessage); virtual;
    procedure ResieveData(const AData: string); virtual;
    function GetRegisteredHandle: THandle;
  public
    constructor Create(const AObjectName: string);
    destructor Destroy; override;
    function RegisterInstance: Boolean;
    procedure UnregisterInstance;
    function RegisteredInstanceExist: Boolean;
    function PostData(const AData: string): Boolean;
    property ObjectName: string read FObjectName;
    property InstanceRegistered: Boolean read FInstanceRegistered;
    property OnResieveData: TELInstanceCheckerOnResieveDataEvent read FOnResieveData write FOnResieveData;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents(SELComponentPage, [TELStringList, TELEventSender,
    TELEvent, TELTrayIcon]);
end;

{ TELStringList }

procedure TELStringList.FLinesOnChange(Sender: TObject);
begin
  Change;
end;

procedure TELStringList.FLinesOnChanging(Sender: TObject);
begin
  Changing;
end;

constructor TELStringList.Create(AOwner: TComponent);
begin
  inherited;
  FLines := TStringList.Create;
  TStringList(FLines).Duplicates := dupAccept;
  TStringList(FLines).OnChange := FLinesOnChange;
  TStringList(FLines).OnChanging := FLinesOnChanging;
end;

destructor TELStringList.Destroy;
begin
  FLines.Free;
  inherited;
end;

function TELStringList.GetCapacity: Integer;
begin
  Result := TStringList(Flines).Capacity;
end;

function TELStringList.GetSorted: Boolean;
begin
  Result := TStringList(FLines).Sorted;
end;

procedure TELStringList.SetCapacity(const Value: Integer);
begin
  TStringList(Flines).Capacity := Value;
end;

procedure TELStringList.SetLines(const Value: TStrings);
begin
  FLines.Assign(Value);
  if Sorted then TStringList(FLines).Sort;
end;

procedure TELStringList.SetSorted(const Value: Boolean);
begin
  TStringList(FLines).Sorted := Value;
end;

{ TELEventSender }

function _ELEventSourceListSortCompareProc(Item1, Item2: Pointer): Integer;
begin
  Result := TELEvent(Item1).Group - TELEvent(Item2).Group;
end;

procedure TELEventSender.AddEvent(AValue: TELEvent);
begin
  if FEvents.IndexOf(AValue) = -1 then
  begin
    if AValue.Source <> nil then AValue.Source.RemoveEvent(AValue);
    FEvents.Add(AValue);
    AValue.FSource := Self;
    SortEvents;
  end;
end;

constructor TELEventSender.Create(AOwner: TComponent);
begin
  inherited;
  FEvents := TList.Create;
  FSendingEvents := TList.Create;
end;

destructor TELEventSender.Destroy;
begin
  while FEvents.Count > 0 do
    RemoveEvent(TELEvent(FEvents[0]));
  FEvents.Free;
  FSendingEvents.Free;
  inherited;
end;

function TELEventSender.GetEventCount: Integer;
begin
  Result := FEvents.Count;
end;

function TELEventSender.GetEvents(AI: Integer): TELEvent;
begin
  Result := TELEvent(FEvents[AI]);
end;

procedure TELEventSender.RemoveEvent(AValue: TELEvent);
var
  LI: Integer;
begin
  if AValue.Source = Self then
  begin
    FEvents.Remove(AValue);
    LI := FSendingEvents.IndexOf(AValue);
    if LI <> -1 then FSendingEvents[LI] := nil;
    AValue.FSource := nil;
  end;
end;

procedure TELEventSender.SendEvent(AParam: Pointer);
var
  LI: Integer;
  LCurEvent: TELEvent;
begin
  FSendingEvents.Clear;
  for LI := 0 to FEvents.Count - 1 do FSendingEvents.Add(FEvents[LI]);
  for LI := 0 to FSendingEvents.Count - 1 do
  begin
    LCurEvent := FSendingEvents[LI];
    if LCurEvent <> nil then LCurEvent.Event(Self, AParam);
  end;
  FSendingEvents.Clear;
end;

procedure TELEventSender.SortEvents;
begin
  FEvents.Sort(_ELEventSourceListSortCompareProc);
end;

{ TELEvent }

destructor TELEvent.Destroy;
begin
  if Source <> nil then Source.RemoveEvent(Self);
  inherited;
end;

procedure TELEvent.Event(AEventSender: TELEventSender; AParam: Pointer);
begin
  if Assigned(OnEvent) then OnEvent(Self, AEventSender, AParam);
end;

procedure TELEvent.SetGroup(const Value: Integer);
begin
  if FGroup = Value then Exit;
  FGroup := Value;
  if Source <> nil then Source.SortEvents;
end;

procedure TELEvent.SetSource(const Value: TELEventSender);
begin
  if FSource = Value then Exit;
  if FSource <> nil then FSource.RemoveEvent(Self);
  if Value <> nil then Value.AddEvent(Self);
end;

{ TELTrayIcon }

procedure TELTrayIcon.Click;
begin
  if Assigned(OnClick) then OnClick(Self);
end;

constructor TELTrayIcon.Create(AOwner: TComponent);
begin
  inherited;
  {$IFDEF VER140}
  FHWnd := Classes.AllocateHWnd(WndProc);
  {$ELSE}
  FHWnd := AllocateHWnd(WndProc);
  {$ENDIF}
  FIcon := TIcon.Create;
  FIcon.OnChange := FIconOnChange;
  FAnimations := TELTrayIconAnimations.Create(Self);
end;

procedure TELTrayIcon.DblClick;
begin
  if Assigned(OnDblClick) then OnDblClick(Self);
end;

destructor TELTrayIcon.Destroy;
begin
  Active := False;
  FAnimations.Free;
  FIcon.Free;
  {$IFDEF VER140}
  Classes.DeallocateHWnd(FHWnd);
  {$ELSE}
  DeallocateHWnd(FHWnd);
  {$ENDIF}
  inherited;
end;

procedure TELTrayIcon.FIconOnChange(Sender: TObject);
var
  LOldRestoreOldIcon: Boolean;
begin
  if tisIconAdded in FState then
  begin
    if not (tisAnimationSetingIcon in FState) and
      (Animations.ActiveItem <> nil) then
      with Animations.ActiveItem do
      begin
        LOldRestoreOldIcon := RestoreOldIcon;
        RestoreOldIcon := False;
        try
          Stop;
        finally
          RestoreOldIcon := LOldRestoreOldIcon;
        end;
      end;
    NotifyIcon(NIM_MODIFY);
  end;
end;

procedure TELTrayIcon.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
  begin
    if AComponent = FPopupMenu then PopupMenu := nil;
    if (AComponent is TImageList) and
      (csFreeNotification in AComponent.ComponentState) then
      Animations.RemoveImageList(TImageList(AComponent)); 
  end;
end;

procedure TELTrayIcon.NotifyIcon(AMessage: Cardinal);
var
  LNID: _NOTIFYICONDATA;
  LS: string;
begin
  with LNID do
  begin
    cbSize := SizeOf(LNID);
    Wnd := FHWnd;
    uID := 0;
    uFlags := NIF_ICON or NIF_MESSAGE or NIF_TIP;
    uCallbackMessage := WM_USER;
    if FIcon.Handle <> 0 then
      hIcon := FIcon.Handle
    else
      hIcon := Application.Icon.Handle;
    if Hint <> '' then LS := Hint
      else LS := Application.Title;
    StrLCopy(szTip, PChar(LS), 63);
  end;
  Shell_NotifyIcon(AMessage, @LNID);
end;

procedure TELTrayIcon.DoContextPopup;

  procedure _DoPopup(APopupMenu: TPopupMenu; AX, AY: Integer);
  begin
    if GetCapture <> 0 then SendMessage(GetCapture, WM_CANCELMODE, 0, 0);
    SetForegroundWindow(Application.Handle);
    Application.ProcessMessages;
    APopupMenu.Popup(AX, AY);
  end;

var
  LPt: TPoint;
  LHandled: Boolean;
  LPopupMenu: TPopupMenu;

begin
  if csDesigning in ComponentState then Exit;
  GetCursorPos(LPt);
  LHandled := False;
  ContextPopup(LPt, LHandled);
  if LHandled then Exit;
  LPopupMenu := GetPopupMenu;
  if (LPopupMenu <> nil) and LPopupMenu.AutoPopup then
  begin
    LPopupMenu.PopupComponent := Self;
    _DoPopup(LPopupMenu, LPt.X, LPt.Y);
  end;
end;

procedure TELTrayIcon.SetActive(const Value: Boolean);
const
  NOTIFYICONFLAG: array[Boolean] of Integer = (NIM_DELETE, NIM_ADD);
begin
  FActive := Value;
  if not (csDesigning in ComponentState) and
    not (csLoading in ComponentState) then
  begin
    UpdateTaskBarButtonVisible;
    if Value then
    begin
      if Animations.ActiveItem <> nil then
        Animations.SetActiveItem(Animations.ActiveItem);
    end
    else
      Animations.SetActiveItem(nil);
    NotifyIcon(NOTIFYICONFLAG[Value]);
    if Value then
      Include(FState, tisIconAdded) else Exclude(FState, tisIconAdded);
  end;
end;

procedure TELTrayIcon.SetHint(const Value: string);
begin
  FHint := Value;
  if tisIconAdded in FState then NotifyIcon(NIM_MODIFY);
end;

procedure TELTrayIcon.SetIcon(const Value: TIcon);
begin
  FIcon.Assign(Value);
end;

procedure TELTrayIcon.SetPopupMenu(const Value: TPopupMenu);
begin
  FPopupMenu := Value;
end;

procedure TELTrayIcon.WndProc(var AMessage: TMessage);
begin
  if AMessage.Msg = WM_USER then
  begin
    case AMessage.LParam of
      WM_RBUTTONUP: DoContextPopup;
      WM_RBUTTONDOWN:
        Application.BringToFront;
      WM_LBUTTONDOWN:
      begin
        Application.BringToFront;
        Click;
      end;
      WM_LBUTTONDBLCLK:
      begin
        Application.BringToFront;
        DblClick;
      end;
    end;
    AMessage.Result := 0;
  end
  else
    with AMessage do Result := DefWindowProc(FHWnd, Msg, wParam, lParam);
end;

{ TELInstanceChecker }

const
  ICH_FILEMAPPING_SUFFIX = 'FM';
  ICH_MUTEX_SUFFIX       = 'MX';

function TELInstanceChecker.RegisteredInstanceExist: Boolean;
begin
  Result := GetRegisteredHandle <> 0;
end;

constructor TELInstanceChecker.Create(const AObjectName: string);
begin
  FObjectName := AObjectName;
  FDataStorage := TELDataStorage.Create;
  {$IFDEF VER140}
  FHandle := Classes.AllocateHWnd(WndProc);
  {$ELSE}
  FHandle := AllocateHWnd(WndProc);
  {$ENDIF}
end;

destructor TELInstanceChecker.Destroy;
begin
  UnregisterInstance;
  if FHandle <> 0 then
    {$IFDEF VER140}
    Classes.DeallocateHWnd(FHandle);
    {$ELSE}
    DeallocateHWnd(FHandle);
    {$ENDIF}
  FDataStorage.Free;
  inherited;
end;

function TELInstanceChecker.PostData(const AData: string): Boolean;
var
  LBufHandle: THandle;
  LCds: TCopyDataStruct;
begin
  if FInstanceRegistered then
    raise EELInstanceChecker.Create(
      'InstanceChecker can not post data to self'
    );
  LBufHandle := GetRegisteredHandle;
  Result := False;
  if LBufHandle <> 0 then
  begin
    LCds.dwData := 0; // not used
    LCds.cbData := Length(AData);
    GetMem(LCds.lpData, LCds.cbData);
    Move(Pointer(AData)^, LCds.lpData^, LCds.cbData);
    Result := SendMessage(LBufHandle, WM_COPYDATA, FHandle, Integer(@LCds)) = 1;
    FreeMem(LCds.lpData, LCds.cbData);
  end;
end;

procedure TELInstanceChecker.ResieveData(const AData: string);
begin
  if Assigned(OnResieveData) then OnResieveData(Self, AData);
end;

procedure TELInstanceChecker.WndProc(var AMessage: TMessage);
var
  LS: string;
  LCds: PCopyDataStruct;
begin
  case AMessage.Msg of
    WM_COPYDATA:
    begin
      LCds := Pointer(AMessage.LParam);
      SetString(LS, nil, LCds.cbData);
      Move(LCds.lpData^, Pointer(LS)^, LCds.cbData);
      PostMessage(FHandle, WM_USER, FDataStorage.AddDataBlock(LS), 0);
      AMessage.Result := 1;
    end;
    WM_USER:
    begin
      LS := FDataStorage.GetDataBlock(AMessage.WParam);
      FDataStorage.DeleteDataBlock(AMessage.WParam);
      ResieveData(LS);
    end;
  else
    with AMessage do Result := DefWindowProc(FHandle, Msg, WParam, LParam);
  end;
end;

function TELInstanceChecker.RegisterInstance: Boolean;
var
  LMutexHandle: THandle;
  LFilePtr: Pointer;
  LBufFileMapHandle: THandle;
begin
  if not FInstanceRegistered then
  begin
    { Use mutex object to get exlusive access to init file mapping }
    LMutexHandle := CreateMutex(nil, False,
      PChar(ObjectName + ICH_MUTEX_SUFFIX));
    WaitForSingleObject(LMutexHandle, INFINITE);
    try
      { Initialize file mapping }
      LBufFileMapHandle := CreateFileMapping($FFFFFFFF, nil, PAGE_READWRITE, 0,
        SizeOf(Integer), PChar(ObjectName + ICH_FILEMAPPING_SUFFIX));
      Result := (LBufFileMapHandle <> 0) and
        (GetLastError <> ERROR_ALREADY_EXISTS);
      if Result then
      begin
        FFileMapObjHandle := LBufFileMapHandle;
        LFilePtr := MapViewOfFile(FFileMapObjHandle, FILE_MAP_WRITE, 0,
          0, SizeOf(Integer));
        try
          Integer(LFilePtr^) := FHandle;
        finally
          UnmapViewOfFile(LFilePtr);
        end;
      end
      else CloseHandle(LBufFileMapHandle);
    finally
      ReleaseMutex(LMutexHandle);
      CloseHandle(LMutexHandle);
    end;
    FInstanceRegistered := Result;
  end else Result := True;
end;

procedure TELInstanceChecker.UnregisterInstance;
var
  LMutexHandle: THandle;
begin
  if FInstanceRegistered then
  begin
    { Use mutex object to get exlusive access to uninit file mapping }
    LMutexHandle := CreateMutex(nil, False,
      PChar(ObjectName + ICH_MUTEX_SUFFIX));
    WaitForSingleObject(LMutexHandle, INFINITE);
    try
      { Uninitialize file mapping }
      CloseHandle(FFileMapObjHandle);
    finally
      ReleaseMutex(LMutexHandle);
      CloseHandle(LMutexHandle);
    end;
    FInstanceRegistered := False;
  end;
end;

function TELInstanceChecker.GetRegisteredHandle: THandle;
var
  LMutexHandle: THandle;
  LFilePtr: Pointer;
  LBufHandle: THandle;
begin
  { Use mutex object to get exlusive access to init file mapping }
  LMutexHandle := CreateMutex(nil, False,
    PChar(ObjectName + ICH_MUTEX_SUFFIX));
  WaitForSingleObject(LMutexHandle, INFINITE);
  Result := 0;
  try
    { Initialize file mapping }
    LBufHandle := OpenFileMapping(FILE_MAP_READ, False,
      PChar(ObjectName + ICH_FILEMAPPING_SUFFIX));
      if LBufHandle <> 0 then
      begin
        LFilePtr := MapViewOfFile(LBufHandle, FILE_MAP_READ, 0,
          0, SizeOf(Integer));
        try
          Result := Integer(LFilePtr^);
        finally
          UnmapViewOfFile(LFilePtr);
          CloseHandle(LBufHandle);
        end;
      end
  finally
    ReleaseMutex(LMutexHandle);
    CloseHandle(LMutexHandle);
  end;
end;

{ TELDataStorage }

function TELDataStorage.AddDataBlock(const AData: string): Integer;
begin
  New(PELDataStorageDataBlock(Result));
  PELDataStorageDataBlock(Result).Data := AData;
  PELDataStorageDataBlock(Result).Index := FDataList.Add(
    PELDataStorageDataBlock(Result));
end;

constructor TELDataStorage.Create;
begin
  FDataList := TList.Create;
end;

procedure TELDataStorage.DeleteDataBlock(AHandle: Integer);
var
  LI: Integer;
begin
  LI := PELDataStorageDataBlock(AHandle).Index;
  FDataList[LI] := FDataList[FDataList.Count - 1];
  PELDataStorageDataBlock(FDataList[LI]).Index := LI;
  FDataList.Count := FDataList.Count - 1;
  Dispose(PELDataStorageDataBlock(AHandle));
end;

destructor TELDataStorage.Destroy;
var
  LI: Integer;
begin
  for LI := 0 to FDataList.Count - 1 do
    Dispose(PELDataStorageDataBlock(FDataList[LI]));
  FDataList.Free;
  inherited;
end;

function TELDataStorage.GetDataBlock(AHandle: Integer): string;
begin
  Result := PELDataStorageDataBlock(AHandle).Data;
end;

procedure TELStringList.Change;
begin
  if Assigned(OnChange) then OnChange(Self);
end;

procedure TELStringList.Changing;
begin
  if Assigned(OnChanging) then OnChanging(Self);
end;

procedure TELTrayIcon.ContextPopup(const MousePos: TPoint;
  var Handled: Boolean);
begin
  if Assigned(OnContextPopup) then OnContextPopup(Self, MousePos, Handled);
end;

function TELTrayIcon.GetPopupMenu: TPopupMenu;
begin
  Result := FPopupMenu;
end;

procedure TELTrayIcon.SetHideTaskBarButton(const Value: Boolean);
begin
  FHideTaskBarButton := Value;
  if not (csDesigning in ComponentState) and
    not (csLoading in ComponentState) then
    UpdateTaskBarButtonVisible;
end;

procedure TELTrayIcon.UpdateTaskBarButtonVisible;
var
  LAppWindowVisible, LVisible: Boolean;
  LWndLong: Longint;
begin
  LVisible := not HideTaskBarButton or not Active;
  LWndLong := GetWindowLong(Application.Handle, GWL_EXSTYLE);
  if (not LVisible) xor ((LWndLong and WS_EX_TOOLWINDOW) <> 0) then
  begin
    LAppWindowVisible := IsWindowVisible(Application.Handle);
    if LAppWindowVisible then ShowWindow(Application.Handle, SW_HIDE);
    SetWindowLong(
      Application.Handle,
      GWL_EXSTYLE,
      (LWndLong and not WS_EX_TOOLWINDOW) or
        (WS_EX_TOOLWINDOW * Ord(not LVisible))
    );
    if LAppWindowVisible then ShowWindow(Application.Handle, SW_SHOWNA);
  end;
end;

{ TELTrayIconAnimations }

constructor TELTrayIconAnimations.Create(AOwnerTrayIcon: TELTrayIcon);
begin
  inherited Create(TELTrayIconAnimationsItem);
  FOwnerTrayIcon := AOwnerTrayIcon;
end;

procedure TELTrayIcon.SetAnimations(const Value: TELTrayIconAnimations);
begin
  FAnimations.Assign(Value);
end;

destructor TELTrayIconAnimations.Destroy;
begin
  SetActiveItem(nil);
  inherited;
end;

procedure TELTrayIconAnimations.FTimerOnTimer(Sender: TObject);
var
  LNeedStop: Boolean;
begin
  if FActiveItem = nil then Exit;
  LNeedStop := False;
  Inc(FActiveImageIndex);
  if FActiveImageIndex > FActiveItem.ImageList.Count - 1 then
    if FActiveItem.Circular then FActiveImageIndex := 0
      else LNeedStop := True;
  if LNeedStop then
    SetActiveItem(nil)
  else
  begin
    Include(FOwnerTrayIcon.FState, tisAnimationSetingIcon);
    try
      FActiveItem.ImageList.GetIcon(FActiveImageIndex, FOwnerTrayIcon.FIcon);
    finally
      Exclude(FOwnerTrayIcon.FState, tisAnimationSetingIcon);
    end;
    FTimer.Interval := FActiveItem.Interval;
    FTimer.Enabled := True;
  end;
end;

function TELTrayIconAnimations.GetItems(
  AIndex: Integer): TELTrayIconAnimationsItem;
begin
  Result := TELTrayIconAnimationsItem(inherited Items[AIndex]);
end;

function TELTrayIconAnimations.GetOwner: TPersistent;
begin
  Result := FOwnerTrayIcon;
end;

procedure TELTrayIconAnimations.RemoveImageList(AImageList: TImageList);
var
  LI: Integer;
begin
  for LI := 0 to Count - 1 do
    if Items[LI].ImageList = AImageList then
    begin
      Items[LI].Stop;
      Items[LI].ImageList := nil;
      Break;
    end;
end;

procedure TELTrayIconAnimations.SetActiveItem(AItem: TELTrayIconAnimationsItem);

  procedure _StartAnimation;
  begin
    if FOldIcon = nil then FOldIcon := TIcon.Create;
    FOldIcon.Assign(FOwnerTrayIcon.Icon);
    if FTimer = nil then
    begin
      FTimer := TTimer.Create(nil);
      FTimer.OnTimer := FTimerOnTimer;
    end;
    FActiveImageIndex := -1; // Next - 0
    FTimerOnTimer(FTimer);
  end;

  procedure _StopAnimation(ARestoreOldIcon: Boolean);
  begin
    FTimer.Free;
    FTimer := nil;
    if ARestoreOldIcon then
    begin
      Include(FOwnerTrayIcon.FState, tisAnimationSetingIcon);
      try
        FOwnerTrayIcon.Icon := FOldIcon;
      finally
        Exclude(FOwnerTrayIcon.FState, tisAnimationSetingIcon);
      end;
    end;
    FOldIcon.Free;
    FOldIcon := nil;
  end;

begin
  if not (csLoading in FOwnerTrayIcon.ComponentState) then
  begin
    if (AItem <> nil) and ((AItem.ImageList = nil) or ((AItem.ImageList <> nil)
      and (AItem.ImageList.Count = 0))) then
      raise EELTrayIcon.Create('Can not start animation without images');
    if not (csDesigning in FOwnerTrayIcon.ComponentState) and
      (FActiveItem <> nil) then
      _StopAnimation(FActiveItem.RestoreOldIcon and (AItem = nil));
    FActiveItem := AItem;
    if not (csDesigning in FOwnerTrayIcon.ComponentState) and
      FOwnerTrayIcon.Active and (FActiveItem <> nil) then
        _StartAnimation;
  end
  else FActiveItem := AItem;
end;

procedure TELTrayIconAnimations.SetItems(AIndex: Integer;
  const Value: TELTrayIconAnimationsItem);
begin
  inherited Items[AIndex] := Value;
end;

{ TELTrayIconAnimationsItem }

constructor TELTrayIconAnimationsItem.Create(Collection: TCollection);
begin
  inherited;
  FInterval := 1000;
  FRestoreOldIcon := True;
end;

function TELTrayIconAnimationsItem.GetActive: Boolean;
begin
  Result := TELTrayIconAnimations(Collection).ActiveItem = Self;
end;

function TELTrayIconAnimationsItem.GetDisplayName: string;
begin
  Result := 'Animation' + IntToStr(ID);
end;

procedure TELTrayIconAnimationsItem.SetActive(const Value: Boolean);
begin
  if Value = GetActive then Exit;
  if Value then Start else Stop;
end;

procedure TELTrayIconAnimationsItem.SetImageList(const Value: TImageList);
begin
  if FImageList = Value then Exit;
  if not (csLoading in
    TELTrayIconAnimations(Collection).FOwnerTrayIcon.ComponentState) then
    Active := False;
  if FImageList <> nil then
    FImageList.RemoveFreeNotification(
    TELTrayIconAnimations(Collection).FOwnerTrayIcon);
  FImageList := Value;
  if FImageList <> nil then
    FImageList.FreeNotification(
    TELTrayIconAnimations(Collection).FOwnerTrayIcon);
end;

procedure TELTrayIcon.Loaded;
begin
  inherited;
  if FActive then Active := True;
end;

procedure TELTrayIconAnimationsItem.Start;
begin
  TELTrayIconAnimations(Collection).SetActiveItem(Self);
end;

procedure TELTrayIconAnimationsItem.Stop;
begin
  if Active then TELTrayIconAnimations(Collection).SetActiveItem(nil);
end;

{ TELObjectList }

function TELObjectList.Add: Integer;
begin
  ValidateAddition;
  Result := FItems.Add(CreateItem);
  Added;
  if not FChangingCount then Change;
end;

constructor TELObjectList.Create;
begin
  FItems := TList.Create;
end;

function TELObjectList.CreateItem: TObject;
begin
  Result := nil;
end;

procedure TELObjectList.Delete(AIndex: Integer);
begin
  ValidateDeletion;
  TObject(FItems[AIndex]).Free;
  FItems.Delete(AIndex);
  Deleted;
  if not FChangingCount then Change;
end;

destructor TELObjectList.Destroy;
begin
  Clear;
  FItems.Free;
  inherited;
end;

function TELObjectList.DoFind(AData: Pointer; AItemByProc: TELItemByProc): TObject;
var
  LI: Integer;
  LResult: Boolean;
begin
  Result := nil;
  for LI := 0 to Count - 1 do
  begin
    LResult := False;
    AItemByProc(Items[LI], AData, LResult);
    if LResult then
    begin
      Result := Items[LI];
      Break;
    end;
  end;
end;

function TELObjectList.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TELObjectList.GetItems(AIndex: Integer): TObject;
begin
  Result := FItems[AIndex];
end;

function TELObjectList.IndexOf(AItem: TObject): Integer;
begin
  Result := FItems.IndexOf(AItem);
end;

function TELObjectList.DoItemBy(AData: Pointer; AItemByProc: TELItemByProc): TObject;
begin
  Result := DoFind(AData, AItemByProc);
  if Result = nil then raise EELObjectList.Create('Item not found');
end;

procedure TELObjectList.Remove(AItem: TObject);
var
  LI: Integer;
begin
  LI := FItems.IndexOf(AItem);
  if LI <> -1 then Delete(LI);
end;

procedure TELObjectList.SetCount(const Value: Integer);
begin
  FChangingCount := True;
  try
    if Value > Count then
      while Count < Value do Add
    else if Value < Count then
      while Count > Value do Delete(Count - 1);
  finally
    FChangingCount := False;
  end;
  Change;
end;

procedure TELObjectList.Clear;
begin
  Count := 0;
end;

function TELObjectList.DoSearch(AData: Pointer;
  AItemByProc: TELItemByProc): TObject;
var
  LI: Integer;
begin
  Result := DoFind(AData, AItemByProc);
  if Result = nil then
    for LI := 0 to Count - 1 do
      if (Items[LI] <> nil) and (Items[LI] is TELObjectList) then
      begin
        Result := TELObjectList(Items[LI]).DoSearch(AData, AItemByProc);
        if Result <> nil then Break;
      end;
end;

procedure TELObjectList.ValidateAddition;
begin
  // Do nothing
end;

procedure TELObjectList.ValidateDeletion;
begin
  // Do nothing
end;

procedure TELObjectList.Change;
begin
  // Do nothing
end;

procedure TELObjectList.Added;
begin
  // Do nothing
end;

procedure TELObjectList.Deleted;
begin
  // Do nothing
end;

end.
