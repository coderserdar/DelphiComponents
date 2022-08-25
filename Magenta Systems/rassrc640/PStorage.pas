unit PStorage;

// http://www.cobans.net/pslib.php

interface

uses
  Windows, PStorageIntfs, Classes, ActiveX;

type
  // The PStoreCreateInstance function retrieves an interface pointer to a storage provider
  TPStoreCreateInstance = function(var ppProvider: IPStore; pProviderID: PGUID; pReserved: Pointer; dwFlags: DWORD): HRESULT; stdcall;

  PProviderInfo = ^TProviderInfo;
  TProviderInfo = record
    GUID: TGUID;
    Capabilities: LongWord;
    ProviderName: ShortString;
  end;

  TIDList = class;
  TProviderList = class;
  TItemList = class;

  TPStorage = class(TObject)
  private
    FKey: Integer;
    FConnected: Boolean;
    FProvider: IPStore;
    function GetInitialized: Boolean;
    function GetProviderInfo: TProviderInfo;
  public
    constructor Create;
    destructor Destroy; override;
    function Connect(ProviderID: PGUID): Boolean;
    procedure Disconnect;
    property Initialized: Boolean read GetInitialized;
    property Connected: Boolean read FConnected;
    property ProviderInfo: TProviderInfo read GetProviderInfo;

    function GetTypes: TIDList;
    function GetSubtypes(pType: TGUID): TIDList;
    function GetItems(pType, pSubtype: TGUID): TItemList;
    function GetTypeName(pGUID: TGUID): String;
    function GetSubtypeName(pType, pSubtype: TGUID): String;
    function GetProviders: TProviderList;

    function DeleteType(pType: TGUID): Boolean;
    function DeleteSubtype(pType, pSubtype: TGUID): Boolean;

    function ReadItemData(pType, pSubtype: TGUID; pItem: ShortString; var Data: Pointer; var DataLen: LongWord): Boolean;
    function DeleteItem(pType, pSubtype: TGUID; pItem: ShortString): Boolean;
    function WriteItemData(pType, pSubtype: TGUID; pItem: ShortString; Data: Pointer; DataLen: LongWord; Prompt: Boolean; PromptInfo: String): Boolean;

    function CreateType(pType: TGUID; const DisplayName: String): Boolean;
    function CreateSubtype(pType, pSubtype: TGUID; const DisplayName: String): Boolean;
  end;

  TEnumList = class(TObject)
  private
    FList: TList;
    function GetCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Remove(Index: Integer);
    property Count: Integer read GetCount;
  end;

  TIDList = class(TEnumList)
  public
    procedure Add(GUID: TGUID);
    function Get(Index: Integer): TGUID;
  end;

  TProviderList = class(TEnumList)
  public
    procedure Add(ProvInfo: TProviderInfo);
    function Get(Index: Integer): TProviderInfo;
  end;

  TItemList = class(TEnumList)
  public
    procedure Add(ItemName: ShortString);
    function Get(Index: Integer): ShortString;
  end;

  function FillPromptInfoStruct: _PST_PROMPTINFO;

const
  pstorec = 'pstorec.dll';
  PST_KEY_CURRENT_USER = 0;     // Specifies that the storage is maintained in the current user section of the registry.
  PST_KEY_LOCAL_MACHINE = 1;    // Specifies that the storage is maintained in the local machine section of the registry.

  // Specifies whether the prompt dialog is shown. This flag is ignored in Windows XP.
  PST_PF_ALWAYS_SHOW = 1;       // Requests that the provider show the prompt dialog to the user even if not required for this access.
  PST_PF_NEVER_SHOW = 2;        // Do not show the prompt dialog to the user.
  PST_PF_SHOW_ON_REQUEST = 4;

var
  PStoreCreateInstance: TPStoreCreateInstance;
  FInitialized: Boolean;
  FLibrary: THandle;

implementation

{ TPStorage }
constructor TPStorage.Create;
begin
  FKey := PST_KEY_CURRENT_USER;
  FConnected := False;
end;

destructor TPStorage.Destroy;
begin
  Disconnect;
  inherited;
end;

function TPStorage.GetInitialized: Boolean;
begin
  Result := FInitialized;
end;

function TPStorage.GetProviderInfo: TProviderInfo;
var
  ppInfo: PUserType1;
begin
  if not FConnected then Exit;
  if FProvider.GetInfo(ppInfo) = S_OK then begin
    Result.GUID := ppInfo.ID;
    Result.Capabilities := ppInfo.Capabilities;
    Result.ProviderName := String(ppInfo.szProviderName);
  end;
end;

function TPStorage.Connect(ProviderID: PGUID): Boolean;
begin
  Result := False;
  if not FInitialized then Exit;
  if FConnected then Disconnect;

  // Connect to storage provider
  if (PStoreCreateInstance(FProvider, ProviderID, nil, 0) <> S_OK) or (FProvider = nil) then begin
    FProvider := nil;
    Exit;
  end else begin
    FConnected := True;
    Result := True;
  end;
end;

procedure TPStorage.Disconnect;
begin
  if FConnected then begin
    FProvider := nil;
    FConnected := False;
  end;
end;

function TPStorage.GetTypes: TIDList;
var
  ppEnum: IEnumPStoreTypes;
  GUIDBuf: array[0..15] of TGUID;
  ItemsRead, i: Cardinal;
begin
  Result := TIDList.Create;
  if not FConnected then Exit;

  // Get IEnumPStoreTypes interface
  ppEnum := nil;
  if (FProvider.EnumTypes(FKey, 0, ppEnum) <> S_OK) or (ppEnum = nil) then begin
    Exit;
    ppEnum := nil;
  end;

  // Finally read GUIDs
  ItemsRead := 0;
  repeat
    ppEnum.Next(SizeOf(GUIDBuf) div SizeOf(GUIDBuf[0]), GUIDBuf[0], ItemsRead);
    if ItemsRead > 0 then
      for i := 0 to ItemsRead-1 do
        Result.Add(GUIDBuf[i]);
  until ItemsRead = 0;

  // Release interface
  ppEnum := nil;
end;

function TPStorage.GetSubtypes(pType: TGUID): TIDList;
var
  ppEnum: IEnumPStoreTypes;
  GUIDBuf: array[0..15] of TGUID;
  ItemsRead, i: Cardinal;
begin
  Result := TIDList.Create;

  // Get IEnumPStoreTypes interface
  ppEnum := nil;
  if (FProvider.EnumSubTypes(FKey, pType, 0, ppEnum) <> S_OK) or (ppEnum = nil) then begin
    Exit;
    ppEnum := nil;
  end;

  // Finally read GUIDs
  ItemsRead := 0;
  repeat
    ppEnum.Next(SizeOf(GUIDBuf) div SizeOf(GUIDBuf[0]), GUIDBuf[0], ItemsRead);
    if ItemsRead > 0 then
      for i := 0 to ItemsRead-1 do
        Result.Add(GUIDBuf[i]);
  until ItemsRead = 0;

  // Release interface
  ppEnum := nil;
end;

function TPStorage.GetItems(pType, pSubtype: TGUID): TItemList;
var
  ppEnum: IEnumPStoreItems;
  ItemBuf: array[0..15] of PWideChar;
  ItemsRead, i: Cardinal;
begin
  Result := TItemList.Create;

  // Get IEnumPStoreItems interface
  ppEnum := nil;
  if (FProvider.EnumItems(FKey, pType, pSubType, 0, ppEnum) <> S_OK) or (ppEnum = nil) then begin
    Exit;
    ppEnum := nil;
  end;

  // Finally read item strings
  ItemsRead := 0;
  repeat
    ppEnum.Next(SizeOf(ItemBuf) div SizeOf(ItemBuf[0]), ItemBuf[0], ItemsRead);
    if ItemsRead > 0 then
      for i := 0 to ItemsRead-1 do begin
        Result.Add(String(ItemBuf[i]));
        CoTaskMemFree(ItemBuf[i]);
      end;
  until ItemsRead = 0;

  // Release interface
  ppEnum := nil;
end;

function TPStorage.GetTypeName(pGUID: TGUID): String;
var
  pst: PUserType3;
begin
  if not FConnected then Exit;
  pst := nil;
  if (FProvider.GetTypeInfo(FKey, pGUID, pst, 0) = S_OK) and (pst <> nil) then begin
    Result := String(pst^.szDisplayName);
    CoTaskMemFree(pst);
  end;
end;

function TPStorage.GetSubtypeName(pType, pSubtype: TGUID): String;
var
  pst: PUserType3;
begin
  if not FConnected then Exit;
  pst := nil;
  if (FProvider.GetSubtypeInfo(FKey, pType, pSubType, pst, 0) = S_OK) and (pst <> nil) then begin
    Result := String(pst^.szDisplayName);
    CoTaskMemFree(pst);
  end;
end;

function TPStorage.GetProviders: TProviderList;
var
  ppEnum: IEnumPStoreProviders;
  ProvBuf: array[0..15] of PUserType1;
  ItemsRead, i: Cardinal;
  tpi: TProviderInfo;
begin
  Result := TProviderList.Create;
  if not FConnected then Exit;
  if FProvider.QueryInterface(IID_IEnumPStoreProviders, ppEnum) <> S_OK then Exit;

  ItemsRead := 0;
  repeat
    ppEnum.Next(SizeOf(ProvBuf) div SizeOf(ProvBuf[0]), ProvBuf[0], ItemsRead);
    if ItemsRead > 0 then
      for i := 0 to ItemsRead - 1 do begin
        tpi.GUID := ProvBuf[i].ID;
        tpi.Capabilities := ProvBuf[i].Capabilities;
        tpi.ProviderName := String(ProvBuf[i].szProviderName);
        Result.Add(tpi);
        CoTaskMemFree(ProvBuf[i]);
      end;
  until ItemsRead = 0;

  ppEnum := nil;
end;

function TPStorage.DeleteType(pType: TGUID): Boolean;
begin
  Result := False;
  if not FConnected then Exit;
  Result := FProvider.DeleteType(FKey, pType, 0) = S_OK;
end;

function TPStorage.DeleteSubtype(pType, pSubtype: TGUID): Boolean;
begin
  Result := False;
  if not FConnected then Exit;
  Result := FProvider.DeleteSubtype(FKey, pType, pSubType, 0) = S_OK;
end;

function TPStorage.ReadItemData(pType, pSubtype: TGUID; pItem: ShortString; var Data: Pointer; var DataLen: LongWord): Boolean;
var
  pspi: _PST_PROMPTINFO;
begin
  Result := False;
  if not FConnected then Exit;
  pspi := FillPromptInfoStruct;
  DataLen := 0;
  Data := nil;
  Result := FProvider.ReadItem(FKey, pType, pSubtype, StringToOleStr(pItem), DataLen, Data, pspi, 0) = S_OK;
end;

function TPStorage.DeleteItem(pType, pSubtype: TGUID; pItem: ShortString): Boolean;
var
  pspi: _PST_PROMPTINFO;
begin
  Result := False;
  if not FConnected then Exit;
  pspi := FillPromptInfoStruct;
  Result := FProvider.DeleteItem(FKey, pType, pSubtype, StringToOleStr(pItem), pspi, 0) = S_OK;
end;

function TPStorage.WriteItemData(pType, pSubtype: TGUID; pItem: ShortString; Data: Pointer; DataLen: LongWord; Prompt: Boolean; PromptInfo: String): Boolean;
var
  pspi: _PST_PROMPTINFO;
begin
  Result := False;
  if not FConnected then Exit;
  pspi := FillPromptInfoStruct;
  pspi.dwPromptFlags := PST_PF_SHOW_ON_REQUEST;
  pspi.szPrompt := StringToOleStr(PromptInfo);
  if Prompt then
    Result := FProvider.WriteItem(FKey, pType, pSubtype, StringToOleStr(pItem), DataLen, PByte(Data)^, pspi, 0, 0) = S_OK
  else
    Result := FProvider.WriteItem(FKey, pType, pSubtype, StringToOleStr(pItem), DataLen, PByte(Data)^, pspi, 1, 0) = S_OK;
end;

function TPStorage.CreateType(pType: TGUID; const DisplayName: String): Boolean;
var
  pInfo: _PST_TYPEINFO;
begin
  pInfo.cbSize := SizeOf(_PST_TYPEINFO);
  pInfo.szDisplayName := StringToOleStr(DisplayName);
  Result := FProvider.CreateType(FKey, pType, pInfo, 0) = S_OK;
end;

function TPStorage.CreateSubtype(pType, pSubtype: TGUID; const DisplayName: String): Boolean;
var
  pRules: _PST_ACCESSRULESET;
  pInfo: _PST_TYPEINFO;
begin
  pRules.cbSize := SizeOf(_PST_ACCESSRULESET);
  pRules.cRules := 0;
  pRules.rgRules := nil;
  pInfo.cbSize := SizeOf(_PST_TYPEINFO);
  pInfo.szDisplayName := StringToOleStr(DisplayName);
  Result := FProvider.CreateSubtype(FKey, pType, pSubtype, pInfo, pRules, 0) = S_OK;
end;

{------------------------------------------------------------------------}
{ TEnumList }
constructor TEnumList.Create;
begin
  FList := TList.Create;
end;

destructor TEnumList.Destroy;
var
  i: Integer;
begin
  for i := 0 to FList.Count - 1 do
    Dispose(FList.Items[i]);
  FList.Free;
  inherited;
end;

function TEnumList.GetCount: Integer;
begin
  Result := FList.Count;
end;

procedure TEnumList.Remove(Index: Integer);
begin
  Dispose(FList.Items[Index]);
  FList.Delete(Index);
end;

{------------------------------------------------------------------------}
{ TIDList }

procedure TIDList.Add(GUID: TGUID);
var
  ppGUID: PGUID;
begin
  New(ppGUID);
  ppGUID^ := GUID;
  FList.Add(ppGUID);
end;

function TIDList.Get(Index: Integer): TGUID;
begin
  Result := PGUID(FList.Items[Index])^;
end;

{------------------------------------------------------------------------}
{ TProviderList }

procedure TProviderList.Add(ProvInfo: TProviderInfo);
var
  ppProvInfo: PProviderInfo;
begin
  New(ppProvInfo);
  ppProvInfo^ := ProvInfo;
  FList.Add(ppProvInfo);
end;

function TProviderList.Get(Index: Integer): TProviderInfo;
begin
  Result := PProviderInfo(FList.Items[Index])^;
end;

{------------------------------------------------------------------------}
{ TItemList }

procedure TItemList.Add(ItemName: ShortString);
var
  pItem: PShortString;
begin
  New(pItem);
  pItem^ := ItemName;
  FList.Add(pItem);
end;

function TItemList.Get(Index: Integer): ShortString;
begin
  Result := PShortString(FList.Items[Index])^;
end;

function FillPromptInfoStruct: _PST_PROMPTINFO;
begin
  Result.cbSize := SizeOf(_PST_PROMPTINFO);
  Result.dwPromptFlags := PST_PF_SHOW_ON_REQUEST;
  Result.hwndApp := 0;
  Result.szPrompt := '';
end;

procedure InitLib;
begin
  FInitialized := False;

  // Load pstorec.dll library
  FLibrary := LoadLibrary(pstorec);
  if FLibrary = 0 then Exit;

  // Get pointer to PStoreCreateInstance funtion
  PStoreCreateInstance := GetProcAddress(FLibrary, 'PStoreCreateInstance');
  if @PStoreCreateInstance = nil then
    FreeLibrary(FLibrary)
  else
    FInitialized := True;
end;

procedure FreeLib;
begin
  if FInitialized then begin
    FreeLibrary(FLibrary);
    FInitialized := False;
  end;
end;

initialization
  InitLib;

finalization
  FreeLib;

end.
