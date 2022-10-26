{*******************************************************}
{                                                       }
{         Delphi VCL Extensions (RX)                    }
{                                                       }
{         Copyright (c) 1996 AO ROSNO                   }
{         Copyright (c) 1998 Master-Bank                }
{                                                       }
{*******************************************************}

unit rxPageMngr;

{$I RX.INC}

interface

uses
  Classes, Controls, ExtCtrls;

type
  TPageNotifyEvent = procedure(Next: Boolean) of object;
  TPageRequestEvent = procedure(CurrentPage: Integer;
    var NewPage: Integer) of object;

  TPageOwner = TNotebook;
  TPageItem = TPage;
  TPageProxy = class;
  TPageHistory = class;
  TPageHistoryItem = class;
  TPageHistoryCommand = (hcNone, hcAdd, hcBack, hcForward, hcGoto);

  TPageManager = class(TComponent)
  private
    FPageOwner: TPageOwner;
    FPageProxies: TList;
    FSetStartPage: Boolean;
    FDestroyHandles: Boolean;
    FButtons: array [Boolean] of TControl;
    FSaveBtnClick: array [Boolean] of TNotifyEvent;
    FChangeHelpContext: Boolean;
    FPageHistory: TPageHistory;
    FUseHistory: Boolean;
    FHistoryCommand: TPageHistoryCommand;
    FOnGetPriorPage: TPageRequestEvent;
    FOnGetNextPage: TPageRequestEvent;
    FOnCheckButtons: TNotifyEvent;
    FOnCheckProxy: TNotifyEvent;
    FOnPageChanged: TNotifyEvent;
    procedure SetPageOwner(Value: TPageOwner);
    function GetProxyIndex(const PageName: string): Integer;
    procedure AddProxy(Proxy: TPageProxy);
    procedure RemoveProxy(Proxy: TPageProxy);
    procedure DestroyProxies;
    procedure PageEnter(Page: Integer; Next: Boolean);
    procedure PageLeave(Page: Integer; Next: Boolean);
    procedure PageShow(Page: Integer; Next: Boolean);
    procedure PageHide(Page: Integer; Next: Boolean);
    procedure PageChanged;
    function GetNextEnabled: Boolean;
    function GetPriorEnabled: Boolean;
    function GetPageIndex: Integer;
    procedure SetPageIndex(Value: Integer);
    function GetPageCount: Integer;
    function GetPageName(Index: Integer): string;
    function FindFreePage: string;
    procedure SetPageProxies(Value: TList);
    function GetButton(Index: Integer): TControl;
    procedure SetButton(Index: Integer; Value: TControl);
    procedure SetDestroyHandles(Value: Boolean);
    procedure SyncBtnClick(Index: Integer; Sync: Boolean);
    procedure BtnClick(Sender: TObject);
    procedure DormantPages;
  protected
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent;
      AOperation: TOperation); override;
    procedure GetChildren(Proc: TGetChildProc {$IFDEF RX_D3};
      Root: TComponent {$ENDIF}); override;
    procedure ChangePage(Next: Boolean); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CheckBtnEnabled;
    procedure Resync;
    function GetPriorPageIndex(Page: Integer): Integer; virtual;
    function GetNextPageIndex(Page: Integer): Integer; virtual;
    procedure NextPage;
    procedure PriorPage;
    procedure GotoHistoryPage(HistoryIndex: Integer);
    procedure SetPage(NewPageIndex: Integer; Next: Boolean);
    property PageNames[Index: Integer]: string read GetPageName;
    property PageCount: Integer read GetPageCount;
    property PageIndex: Integer read GetPageIndex;
    property NextEnabled: Boolean read GetNextEnabled;
    property PriorEnabled: Boolean read GetPriorEnabled;
    property PageHistory: TPageHistory read FPageHistory;
    property HistoryCommand: TPageHistoryCommand read FHistoryCommand
      write FHistoryCommand;
    property OnCheckProxy: TNotifyEvent read FOnCheckProxy write FOnCheckProxy; { for internal use only }
  published
    property PageOwner: TPageOwner read FPageOwner write SetPageOwner;
    property PageProxies: TList read FPageProxies write SetPageProxies;
    property NextBtn: TControl index 1 read GetButton write SetButton;
    property PriorBtn: TControl index 0 read GetButton write SetButton;
    property SetStartPage: Boolean read FSetStartPage write FSetStartPage default True;
    property DestroyHandles: Boolean read FDestroyHandles write SetDestroyHandles default False;
    property UseHistory: Boolean read FUseHistory write FUseHistory default False;
    property OnGetPriorPage: TPageRequestEvent read FOnGetPriorPage
      write FOnGetPriorPage;
    property OnGetNextPage: TPageRequestEvent read FOnGetNextPage
      write FOnGetNextPage;
    property OnCheckButtons: TNotifyEvent read FOnCheckButtons
      write FOnCheckButtons;
    property OnPageChanged: TNotifyEvent read FOnPageChanged write FOnPageChanged;
  end;

  TPageProxy = class(TComponent)
  private
    FPageManager: TPageManager;
{$IFDEF RX_D4}   // Polaris
    FPageName: String;
{$ELSE}
    FPageName: PString;
{$ENDIF}
    FOnEnter: TPageNotifyEvent;
    FOnLeave: TPageNotifyEvent;
    FOnShow: TPageNotifyEvent;
    FOnHide: TPageNotifyEvent;
    function GetPageName: string;
    procedure SetPageName(const Value: string);
    procedure SetPageManager(Value: TPageManager);
    procedure PageEnter(Next: Boolean);
    procedure PageLeave(Next: Boolean);
    procedure PageShow(Next: Boolean);
    procedure PageHide(Next: Boolean);
  protected
    procedure SetParentComponent(Value: TComponent); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function HasParent: Boolean; override;
    function GetParentComponent: TComponent; override;
    property PageManager: TPageManager read FPageManager write SetPageManager;
  published
    property PageName: string read GetPageName write SetPageName;
    property OnEnter: TPageNotifyEvent read FOnEnter write FOnEnter;
    property OnLeave: TPageNotifyEvent read FOnLeave write FOnLeave;
    property OnShow: TPageNotifyEvent read FOnShow write FOnShow;
    property OnHide: TPageNotifyEvent read FOnHide write FOnHide;
  end;

  TPageHistoryItem = class(TObject)
  public
    Index: Integer;
  end;

  TPageHistory = class(TList)
  private
    FCurrent: Integer;
    FHistoryCapacity: Integer;
    procedure SetCurrent(Value: Integer);
    procedure SetHistoryCapacity(Value: Integer);
    function GetPageIndex(Index: Integer): Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddPageIndex(PageIndex: Integer);
    procedure DeleteHistoryItem(Index: Integer);
    procedure ResetHistory;
    property Current: Integer read FCurrent write SetCurrent;
    property HistoryCapacity: Integer read FHistoryCapacity
      write SetHistoryCapacity;
    property PageIndexes[Index: Integer]: Integer read GetPageIndex;
  end;

const
  pageNull = -1;

implementation

uses
  SysUtils, Forms, StdCtrls {$IFDEF RX_D4}, ActnList {$ENDIF};

const
  Registered: Boolean = False;

{ TPageProxy }

constructor TPageProxy.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{$IFDEF RX_D4}   // Polaris
  FPageName := EmptyStr;
{$ELSE}
  FPageName := NullStr;
{$ENDIF}
end;

destructor TPageProxy.Destroy;
begin
  if FPageManager <> nil then
    FPageManager.RemoveProxy(Self);
{$IFNDEF RX_D4}   // Polaris
  DisposeStr(FPageName);
{$ENDIF}
  inherited Destroy;
end;

function TPageProxy.GetPageName: string;
begin
{$IFDEF RX_D4}   // Polaris
  Result := FPageName;
{$ELSE}
  Result := FPageName^;
{$ENDIF}
end;

procedure TPageProxy.SetPageName(const Value: string);
begin
  if (FPageManager <> nil) and (FPageManager.PageOwner <> nil) then
  begin
{$IFDEF RX_D4}   // Polaris
    if (FPageManager.PageOwner.Pages.IndexOf(Value) >= 0) then
      FPageName := Value
    else
      FPageName := '';
  end
  else FPageName := Value;
{$ELSE}
    if (FPageManager.PageOwner.Pages.IndexOf(Value) >= 0) then
      AssignStr(FPageName, Value)
    else
      AssignStr(FPageName, '');
  end
  else
    AssignStr(FPageName, Value);
{$ENDIF}
end;

procedure TPageProxy.SetPageManager(Value: TPageManager);
begin
  if FPageManager <> nil then
    FPageManager.RemoveProxy(Self);
  if Value <> nil then
    Value.AddProxy(Self);
end;

function TPageProxy.HasParent: Boolean;
begin
  Result := True;
end;

function TPageProxy.GetParentComponent: TComponent;
begin
  Result := FPageManager;
end;

procedure TPageProxy.SetParentComponent(Value: TComponent);
begin
  if FPageManager <> nil then
    FPageManager.RemoveProxy(Self);
  if (Value <> nil) and (Value is TPageManager) then
    PageManager := TPageManager(Value);
end;

procedure TPageProxy.PageEnter(Next: Boolean);
begin
  if Assigned(FOnEnter) then
    FOnEnter(Next);
end;

procedure TPageProxy.PageLeave(Next: Boolean);
begin
  if Assigned(FOnLeave) then
    FOnLeave(Next);
end;

procedure TPageProxy.PageShow(Next: Boolean);
begin
  if Assigned(FOnShow) then
    FOnShow(Next);
end;

procedure TPageProxy.PageHide(Next: Boolean);
begin
  if Assigned(FOnHide) then
    FOnHide(Next);
end;

{ TPageManager }

constructor TPageManager.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPageProxies := TList.Create;
  FPageHistory := TPageHistory.Create;
  FHistoryCommand := hcAdd;
  FSetStartPage := True;
  FChangeHelpContext := True;
  FUseHistory := False;
  if not Registered then
  begin
    RegisterClasses([TPageProxy]);
    Registered := True;
  end;
end;

destructor TPageManager.Destroy;
begin
  DestroyProxies;
  FPageProxies.Free;
  FPageHistory.Free;
  inherited Destroy;
end;

procedure TPageManager.Loaded;
var
  Loading: Boolean;
begin
  Loading := csLoading in ComponentState;
  inherited Loaded;
  if not (csDesigning in ComponentState) and Loading then
  begin
    SyncBtnClick(0, True);
    SyncBtnClick(1, True);
  end;
  if FSetStartPage and not (csDesigning in ComponentState) and
    (FPageOwner <> nil) and (FPageProxies.Count > 0) then
    if (FPageProxies.Items[0] <> nil) and
      (TPageProxy(FPageProxies.Items[0]).PageName <> '') then
      FPageOwner.ActivePage := TPageProxy(FPageProxies.Items[0]).PageName;
  if DestroyHandles then
    DormantPages;
  if (FPageOwner <> nil) and (FPageHistory.Count = 0) then
    FPageHistory.AddPageIndex(FPageOwner.PageIndex);
  CheckBtnEnabled;
end;

procedure TPageManager.Notification(AComponent: TComponent; AOperation: TOperation);
begin
  inherited Notification(AComponent, AOperation);
  if AOperation = opRemove then
  begin
    if AComponent = PageOwner then
      PageOwner := nil
    else
    if AComponent = FButtons[False] then
      FButtons[False] := nil
    else
    if AComponent = FButtons[True] then
      FButtons[True] := nil;
  end;
end;

function TPageManager.GetButton(Index: Integer): TControl;
begin
  Result := FButtons[Boolean(Index)];
end;

procedure TPageManager.SetButton(Index: Integer; Value: TControl);
begin
  if GetButton(Index) <> Value then
  begin
    if not (csLoading in ComponentState) then
      SyncBtnClick(Index, False);
    FButtons[Boolean(Index)] := Value;
    if Value <> nil then
      Value.FreeNotification(Self);
    if not (csLoading in ComponentState) then
      SyncBtnClick(Index, True);
  end;
end;

procedure TPageManager.SyncBtnClick(Index: Integer; Sync: Boolean);
begin
  if (GetButton(Index) <> nil) and not (csDesigning in ComponentState) then
    if Sync then
    begin
      FSaveBtnClick[Boolean(Index)] := TButton(GetButton(Index)).OnClick;
      TButton(GetButton(Index)).OnClick := BtnClick;
    end
    else
    begin
      TButton(GetButton(Index)).OnClick := FSaveBtnClick[Boolean(Index)];
      FSaveBtnClick[Boolean(Index)] := nil;
    end;
end;

procedure TPageManager.BtnClick(Sender: TObject);
var
  Next: Boolean;
begin
  for Next := False to True do
    if Sender = FButtons[Next] then
    begin
      ChangePage(Next);
      if Assigned(FSaveBtnClick[Next]) then
        FSaveBtnClick[Next](Sender);
    end;
end;

procedure TPageManager.CheckBtnEnabled;
begin
  if not (csDesigning in ComponentState) then
  begin
{$IFDEF RX_D4}
    if GetButton(0) <> nil then
    begin
      if GetButton(0).Action <> nil then
        TAction(GetButton(0).Action).Enabled := PriorEnabled
      else
        GetButton(0).Enabled := PriorEnabled;
    end;
    if GetButton(1) <> nil then
    begin
      if GetButton(1).Action <> nil then
        TAction(GetButton(1).Action).Enabled := NextEnabled
      else
        GetButton(1).Enabled := NextEnabled;
    end;
{$ELSE}
    if GetButton(0) <> nil then
      GetButton(0).Enabled := PriorEnabled;
    if GetButton(1) <> nil then
      GetButton(1).Enabled := NextEnabled;
{$ENDIF}
    if Assigned(FOnCheckButtons) then
      FOnCheckButtons(Self);
  end;
end;

procedure TPageManager.GetChildren(Proc: TGetChildProc {$IFDEF RX_D3};
  Root: TComponent {$ENDIF});
var
  I: Integer;
begin
  inherited GetChildren(Proc{$IFDEF RX_D3}, Root {$ENDIF});
  for I := 0 to FPageProxies.Count - 1 do
    Proc(TPageProxy(FPageProxies.Items[I]));
end;

procedure TPageManager.SetDestroyHandles(Value: Boolean);
begin
  if Value <> FDestroyHandles then
  begin
    FDestroyHandles := Value;
    if not (csLoading in ComponentState) and FDestroyHandles then
      DormantPages;
  end;
end;

procedure TPageManager.SetPageOwner(Value: TPageOwner);
begin
  if FPageOwner <> Value then
  begin
    FPageOwner := Value;
    if Value <> nil then
      Value.FreeNotification(Self);
    if not (csLoading in ComponentState) then
    begin
      Resync;
      if FDestroyHandles then
        DormantPages;
      if (FPageOwner <> nil) and (FPageHistory.Count = 0) then
        FPageHistory.AddPageIndex(FPageOwner.PageIndex);
    end;
  end;
end;

procedure TPageManager.SetPageProxies(Value: TList);
begin
end;

function TPageManager.GetProxyIndex(const PageName: string): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to FPageProxies.Count - 1 do
    if TPageProxy(FPageProxies.Items[I]).PageName = PageName then
    begin
      Result := I;
      Exit;
    end;
end;

procedure TPageManager.Resync;
var
  I: Integer;
  Index: Integer;
  NewCount: Integer;
  NewProxy: TPageProxy;
begin
  if FPageOwner = nil then
    Exit;
  if PageCount > FPageProxies.Count then
  begin
    NewCount := PageCount - FPageProxies.Count;
    for I := 1 to NewCount do
    begin
      NewProxy := TPageProxy.Create(Owner);
      AddProxy(NewProxy);
      if Assigned(FOnCheckProxy) then FOnCheckProxy(NewProxy);
      {NewProxy.Name := GetUniqueName(NewProxy);}
      NewProxy.PageName := FindFreePage;
    end;
  end;
  for I := FPageProxies.Count - 1 downto 0 do
  begin
    if FPageProxies.Count > PageCount then
    begin
      if (TPageProxy(FPageProxies.Items[I]).PageName <> '') and
        (FPageOwner.Pages.IndexOf(TPageProxy(FPageProxies.Items[I]).PageName) = -1) then
        TPageProxy(FPageProxies.Items[I]).Free;
    end
    else
      Break;
  end;
  for I := 0 to FPageProxies.Count - 1 do
    if Assigned(FOnCheckProxy) then
      FOnCheckProxy(TObject(FPageProxies.Items[I]));
  for I := 0 to PageCount - 1 do
  begin
    Index := GetProxyIndex(PageNames[I]);
    if Index <> -1 then
      FPageProxies.Move(Index, I);
  end;
end;

procedure TPageManager.AddProxy(Proxy: TPageProxy);
begin
  FPageProxies.Add(Proxy);
  Proxy.FPageManager := Self;
end;

procedure TPageManager.RemoveProxy(Proxy: TPageProxy);
begin
  Proxy.FPageManager := nil;
  FPageProxies.Remove(Proxy);
end;

procedure TPageManager.DestroyProxies;
var
  Proxy: TPageProxy;
begin
  while FPageProxies.Count > 0 do
  begin
    Proxy := FPageProxies.Last;
    RemoveProxy(Proxy);
    Proxy.Free;
  end;
end;

function TPageManager.GetPageCount: Integer;
begin
  Result := 0;
  if FPageOwner <> nil then
    Result := FPageOwner.Pages.Count;
end;

function TPageManager.GetPageName(Index: Integer): string;
begin
  Result := '';
  if (FPageOwner <> nil) and (Index < PageCount) then
    Result := FPageOwner.Pages[Index];
end;

function TPageManager.FindFreePage: string;
var
  I: Integer;
begin
  Result := '';
  if PageOwner <> nil then
    for I := 0 to PageOwner.Pages.Count - 1 do
      if GetProxyIndex(PageOwner.Pages[I]) = -1 then
      begin
        Result := PageOwner.Pages[I];
        Exit;
      end;
end;

function TPageManager.GetPageIndex: Integer;
begin
  if PageOwner <> nil then
    Result := PageOwner.PageIndex
  else
    Result := pageNull;
end;

procedure TPageManager.SetPageIndex(Value: Integer);
var
  Page: TPageItem;
  OldPageIndex: Integer;
begin
  if PageOwner <> nil then
  begin
    OldPageIndex := PageOwner.PageIndex;
    PageOwner.PageIndex := Value;
    if DestroyHandles then DormantPages;
    if OldPageIndex <> PageOwner.PageIndex then
    begin
      if not FUseHistory then
        PageHistory.AddPageIndex(PageOwner.PageIndex)
      else
      begin
        case HistoryCommand of
          hcNone: ;
          hcAdd: PageHistory.AddPageIndex(PageOwner.PageIndex);
          hcBack: PageHistory.Current := PageHistory.Current - 1;
          hcForward: PageHistory.Current := PageHistory.Current + 1;
          hcGoto: ;
        end;
      end;
    end;
    HistoryCommand := hcAdd;
    CheckBtnEnabled;
    { update owner form help context }
    if FChangeHelpContext and (Owner <> nil) and (Owner is TForm) and
      ((Owner as TForm).HelpContext = 0) then
    begin
      Page := TPageItem(PageOwner.Pages.Objects[PageIndex]);
      if Page <> nil then
        (Owner as TForm).HelpContext := Page.HelpContext;
    end;
  end;
end;

function TPageManager.GetNextEnabled: Boolean;
begin
  Result := GetNextPageIndex(PageIndex) >= 0;
end;

function TPageManager.GetPriorEnabled: Boolean;
begin
  Result := GetPriorPageIndex(PageIndex) >= 0;
end;

procedure TPageManager.NextPage;
begin
  ChangePage(True);
end;

procedure TPageManager.PriorPage;
begin
  ChangePage(False);
end;

procedure TPageManager.GotoHistoryPage(HistoryIndex: Integer);
var
  SaveCurrent: Integer;
begin
  SaveCurrent := PageHistory.Current;
  HistoryCommand := hcGoto;
  PageHistory.Current := HistoryIndex;
  try
    SetPage(PageHistory.PageIndexes[HistoryIndex], False);
  finally
    if PageOwner.PageIndex <> PageHistory.PageIndexes[HistoryIndex] then
      PageHistory.Current := SaveCurrent;
  end;
end;

procedure TPageManager.PageEnter(Page: Integer; Next: Boolean);
var
  ProxyIndex: Integer;
begin
  ProxyIndex := GetProxyIndex(PageOwner.Pages.Strings[Page]);
  if ProxyIndex <> pageNull then
    TPageProxy(FPageProxies.Items[ProxyIndex]).PageEnter(Next);
end;

procedure TPageManager.PageLeave(Page: Integer; Next: Boolean);
var
  ProxyIndex: Integer;
begin
  ProxyIndex := GetProxyIndex(PageOwner.Pages.Strings[Page]);
  if ProxyIndex <> pageNull then
    TPageProxy(FPageProxies.Items[ProxyIndex]).PageLeave(Next);
end;

procedure TPageManager.PageShow(Page: Integer; Next: Boolean);
var
  ProxyIndex: Integer;
begin
  ProxyIndex := GetProxyIndex(PageOwner.Pages.Strings[Page]);
  if ProxyIndex <> pageNull then
    TPageProxy(FPageProxies.Items[ProxyIndex]).PageShow(Next);
end;

procedure TPageManager.PageHide(Page: Integer; Next: Boolean);
var
  ProxyIndex: Integer;
begin
  ProxyIndex := GetProxyIndex(PageOwner.Pages.Strings[Page]);
  if ProxyIndex <> pageNull then
    TPageProxy(FPageProxies.Items[ProxyIndex]).PageHide(Next);
end;

procedure TPageManager.PageChanged;
begin
  if Assigned(FOnPageChanged) then
    FOnPageChanged(Self);
end;

function TPageManager.GetPriorPageIndex(Page: Integer): Integer;
begin
  if not FUseHistory then
  begin
    if Page < 1 then
      Result := pageNull
    else
      Result := Page - 1;
    end
  else
  begin
    if PageHistory.Current < 1 then
      Result := pageNull
    else
      Result := PageHistory.PageIndexes[PageHistory.Current - 1];
  end;
  if Assigned(FOnGetPriorPage) then
    FOnGetPriorPage(Page, Result);
end;

function TPageManager.GetNextPageIndex(Page: Integer): Integer;
begin
  if not FUseHistory then
  begin
    if Page >= PageCount - 1 then
      Result := pageNull
    else
      Result := Page + 1;
    end
  else
  begin
    if PageHistory.Current >= PageHistory.Count - 1 then
      Result := pageNull
    else
      Result := PageHistory.PageIndexes[PageHistory.Current + 1];
  end;
  if Assigned(FOnGetNextPage) then
    FOnGetNextPage(Page, Result);
end;

procedure TPageManager.SetPage(NewPageIndex: Integer; Next: Boolean);
var
  OldPageIndex: Integer;
begin
  if (NewPageIndex >=0) and (NewPageIndex < PageCount) then
  begin
    OldPageIndex := PageIndex;
    PageLeave(OldPageIndex, Next);
    PageEnter(NewPageIndex, Next);
    SetPageIndex(NewPageIndex);
    if NewPageIndex = PageIndex then
    begin
      PageHide(OldPageIndex, Next);
      PageShow(NewPageIndex, Next);
      PageChanged;
    end;
  end;
end;

procedure TPageManager.ChangePage(Next: Boolean);
var
  NewPageIndex: Integer;
begin
  if Next then
  begin
    NewPageIndex := GetNextPageIndex(PageIndex);
    HistoryCommand := hcForward;
  end
  else
  begin
    NewPageIndex := GetPriorPageIndex(PageIndex);
    HistoryCommand := hcBack;
  end;
  SetPage(NewPageIndex, Next);
end;

type
  THack = class(TWinControl);

procedure TPageManager.DormantPages;
var
  I: Integer;
begin
  if Assigned(FPageOwner) then
    with PageOwner do
      for I := 0 to Pages.Count - 1 do
        if PageIndex <> I then
          THack(Pages.Objects[I]).DestroyHandle;
end;

{ TPageHistory }

constructor TPageHistory.Create;
begin
  inherited Create;
  FCurrent := -1;
  FHistoryCapacity := 10;
end;

destructor TPageHistory.Destroy;
begin
  ResetHistory;
  inherited Destroy;
end;

procedure TPageHistory.SetCurrent(Value: Integer);
begin
  if Value < 0 then
    Value := -1;
  if Value > Count - 1 then
    Value := Count - 1;
  FCurrent := Value;
end;

procedure TPageHistory.SetHistoryCapacity(Value: Integer);
var
  I: Integer;
begin
  if Value < FHistoryCapacity then
    for I := 0 to Count - Value do
      DeleteHistoryItem(0);
  FHistoryCapacity := Value;
end;

function TPageHistory.GetPageIndex(Index: Integer): Integer;
begin
  Result := TPageHistoryItem(Items[Index]).Index;
end;

procedure TPageHistory.AddPageIndex(PageIndex: Integer);
var
  I: Integer;
  Item: TPageHistoryItem;
begin
  for I := Count - 1 downto Current + 1 do
    DeleteHistoryItem(I);
  for I := 0 to Count - HistoryCapacity do
    DeleteHistoryItem(0);
  if Count < HistoryCapacity then
  begin
    Item := TPageHistoryItem.Create;
    Item.Index := PageIndex;
    Add(Item);
  end;
  Current := Count - 1;
end;

procedure TPageHistory.DeleteHistoryItem(Index: Integer);
var
  Item: TPageHistoryItem;
begin
  if (Index >= 0) and (Index < Count) then
  begin
    Item := TPageHistoryItem(Items[Index]);
    Delete(Index);
    Item.Free;
    if Current > Count - 1 then
      Current := Count - 1;
  end;
end;

procedure TPageHistory.ResetHistory;
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    DeleteHistoryItem(I);
end;

end.
