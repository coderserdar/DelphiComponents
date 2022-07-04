{*******************************************************}
{                                                       }
{         Vladimir Gaitanoff Delphi VCL Library         }
{         Explorer library: shell objects               }
{                                                       }
{         Copyright (c) 1997, 2000                      }
{                                                       }
{*******************************************************}

{$I VG.INC }
{$D-,L-}

unit ExplShl;

interface
{$IFDEF _D3_}
uses Windows, ShlObj, Classes, Controls, vgTools, {$IFDEF _D4_}ImgList,{$ENDIF}
  Explorer, vgShlObj;

type
  TShellBrowseOption  = (sboNodes, sboFolders, sboHidden);
  TShellBrowseOptions = set of TShellBrowseOption;

{ TExplorerShellNode }
  TExplorerShellNode = class (TExplorerNode)
  private
    FAttrs: UINT;
    FDisplayName: TShellDisplayName;
    FFileName: string;
    FShellFolder: IShellFolder;
    FEnumIndex: Integer;
    FItemIDLists: array[0..1] of PItemIDList;
    FBrowseOptions: TShellBrowseOptions;
    procedure SetBrowseOptions(Value: TShellBrowseOptions);
    procedure SetDisplayName(Value: TShellDisplayName);
    procedure SetShellFolder(Value: IShellFolder);
  protected
    procedure DoDblClick; override;
    function GetLargeImages: TExplorerImageList; override;
    function GetSmallImages: TExplorerImageList; override;
    function GetStateImages: TExplorerImageList; override;
    function GetNodeType: TExplorerNodeType; override;
    procedure InternalExpand; override;
    procedure SetItemIDLists(ValueRel, ValueAbs: PItemIDList);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    { Overriden methods }
    function CanExpand(NodeTypes: TExplorerNodeTypes): Boolean; override;
    function ClipboardMode: TExplorerClipboardMode; override;
    function IsRunTime: Boolean; override;
    { Capabilities }
    function AcceptsNodes(ExplorerNodes: TExplorerNodes): Boolean; override;
    function CanCopy: Boolean; override;
    function CanDelete: Boolean; override;
    function CanDrag: Boolean; override;
    function CanDrop(List: TExplorerNodesList): Boolean; override;
    function CanEdit: Boolean; override;
    procedure ClipboardNotification(Operation: TOperation); override;
    procedure DefaultSort; override;
    procedure DoCopyList(List: TExplorerNodesList); override;
    procedure DoCutList(List: TExplorerNodesList); override;
    procedure DoDelete; override;
    procedure DoEdit(var Text: string); override;
    procedure DoPaste(List: TExplorerNodesList); override;
    procedure DragDrop(List: TExplorerNodesList); override;
   { Helper functions }
    function GetParentShellFolder: IShellFolder; virtual;
    function GetEnumObjects: IEnumIDList;
    function ShellExecute(Verb: PChar; ShowCmd: Integer): BOOL;
    property Attrs: UINT read FAttrs;
    property DisplayName: TShellDisplayName read FDisplayName write SetDisplayName default sdnInFolder;
    property BrowseOptions: TShellBrowseOptions read FBrowseOptions write SetBrowseOptions
      default [sboNodes, sboFolders, sboHidden];
    property FileName: string read FFileName;
    property ItemIDListRel: PItemIDList read FItemIDLists[0];
    property ItemIDListAbs: PItemIDList read FItemIDLists[1];
    property EnumIndex: Integer read FEnumIndex;
    property ShellFolder: IShellFolder read FShellFolder write SetShellFolder;
  end;

  TShellFolder =(sfDesktop, sfInternet, sfPrograms, sfControls, sfPrinters,
    sfPersonal, sfFavorites, sfStartUp, sfRecent, sfSendTo, sfBitBucket,
    sfStartMenu, sfDesktopDir, sfDrives, sfNetwork, sfNetHood, sfFonts,
    sfTemplates, sfCommonStartMenu, sfCommonPrograms, sfCommonStartUp,
    sfCommonDesktopDir, sfAppData, sfPrintHood, sfAltStartUp,
    sfCommonAltStartUp, sfCommonFavorites, sfCache, sfCookies, sfHistory);

{ TExplorerShellFolderNode }
  TExplorerShellFolderNode = class(TExplorerShellNode)
  private
    FFolder: TShellFolder;
    procedure CreateShellFolder;
    procedure SetFolder(Value: TShellFolder);
  public
    constructor Create(AOwner: TComponent); override;
    procedure DefaultSort; override;
    function GetParentShellFolder: IShellFolder; override;
  published
    property DisplayName;
    property BrowseOptions;
    property Folder: TShellFolder read FFolder write SetFolder default sfDesktop;
    property Index;
    property Sorted;
  end;
{$ENDIF}

implementation
{$IFDEF _D3_}

uses ActiveX, ShellApi, SysUtils, ComObj, vgUtils, Forms, Clipbrd, vgVCLUtl;

var
  ShellCount: Integer = 0;
  LargeImages: TExplorerImageList = nil;
  SmallImages: TExplorerImageList = nil;
  StateImages: TExplorerImageList = nil;

procedure AddExplorerShell;
var
  psfi: TSHFileInfo;
  ppidl: PItemIDList;
begin
  if ShellCount = 0 then
  begin
    SHGetSpecialFolderLocation(0, CSIDL_DESKTOP, ppidl);
    LargeImages := TExplorerImageList.Create(nil);
    LargeImages.ShareImages := True;
    LargeImages.Handle := SHGetFileInfo(PChar(ppidl), 0, psfi, SizeOf(psfi),
      SHGFI_PIDL or SHGFI_ICON or SHGFI_LARGEICON or SHGFI_SYSICONINDEX);
    SmallImages := TExplorerImageList.Create(nil);
    SmallImages.ShareImages := True;
    SmallImages.Handle := SHGetFileInfo(PChar(ppidl), 0, psfi, SizeOf(psfi),
      SHGFI_PIDL or SHGFI_ICON or SHGFI_SMALLICON or SHGFI_SYSICONINDEX or
      SHGFI_DISPLAYNAME);
    StateImages := TExplorerImageList.Create(nil);
  end else begin
    SHGetSpecialFolderLocation(0, CSIDL_DESKTOP, ppidl);
    SHGetFileInfo(PChar(ppidl), 0, psfi, SizeOf(psfi),
      SHGFI_PIDL or SHGFI_ICON or SHGFI_SMALLICON or SHGFI_SYSICONINDEX or
      SHGFI_DISPLAYNAME);
  end;
  Inc(ShellCount);
end;

procedure DeleteExplorerShell;
begin
  Dec(ShellCount);
  if ShellCount = 0 then
  begin
    LargeImages.Free;
    LargeImages := nil;
    SmallImages.Free;
    SmallImages := nil;
    StateImages.Free;
    StateImages := nil;
  end;
end;

function AllocFileStr(const S: string): PChar;
var
  P: PChar;
begin
  Result := nil;
  if S <> '' then begin
    Result := StrCopy(StrAlloc(Length(S) + 2), PChar(S));
    P := Result;
    while P^ <> #0 do begin
      if (P^ = ';') or (P^ = '|') then P^ := #0;
      Inc(P);
    end;
    Inc(P);
    P^ := #0;
  end;
end;

function AllocShellNodesFileStr(List: TExplorerNodesList; var Size: Integer): PChar;
var
  I: Integer;
  FileNames: string;
begin
  FileNames := '';
  for I := 0 to List.Count - 1 do
    AddDelimeted(FileNames, TExplorerShellNode(List[I]).FileName, ';');
  Size := Length(FileNames) + 2;
  Result := AllocFileStr(FileNames);
end;

function DoSHFileOperation(var sfop: TSHFileOpStruct): Integer;
var
  ActiveWindow: HWnd;
  WindowList: Pointer;
begin
  ActiveWindow := GetActiveWindow;
  WindowList := DisableTaskWindows(0);
  try
    sfop.Wnd := ActiveWindow;
    Result := SHFileOperation(sfop);
  finally
    EnableTaskWindows(WindowList);
    SetActiveWindow(ActiveWindow);
  end;
end;

{ TExplorerShellNode }
constructor TExplorerShellNode.Create(AOwner: TComponent);
begin
  inherited;
  AddExplorerShell;
  FBrowseOptions := [sboNodes, sboFolders, sboHidden];
  FDisplayName := sdnInFolder;
end;

destructor TExplorerShellNode.Destroy;
begin
  SetShellFolder(nil);
  FreePIDL(FItemIDLists[0]);
  FreePIDL(FItemIDLists[1]);
  DeleteExplorerShell;
  inherited;
end;

function TExplorerShellNode.ClipboardMode: TExplorerClipboardMode;
begin
  Result := cmParent;
end;

function TExplorerShellNode.CanExpand(NodeTypes: TExplorerNodeTypes): Boolean;
begin
  Result := (FAttrs and SFGAO_FOLDER) <> 0;
end;

function TExplorerShellNode.GetLargeImages: TExplorerImageList;
begin
  Result := LargeImages;
end;

function TExplorerShellNode.GetSmallImages: TExplorerImageList;
begin
  Result := SmallImages;
end;

function TExplorerShellNode.GetStateImages: TExplorerImageList;
begin
  Result := StateImages;
end;

procedure TExplorerShellNode.DoDblClick;
begin
  if FAttrs and SFGAO_FOLDER = 0 then
    ShellExecute(nil, SW_NORMAL);
end;

function TExplorerShellNode.GetEnumObjects: IEnumIDList;
var
  Flags: ULONG;
begin
  if Assigned(FShellFolder) then
  begin
    if sboNodes in FBrowseOptions then
      Flags := SHCONTF_NONFOLDERS else Flags := 0;
    if sboFolders in FBrowseOptions then
      Flags := Flags or SHCONTF_FOLDERS;
    if sboHidden in FBrowseOptions then
      Flags := Flags or SHCONTF_INCLUDEHIDDEN;
    OleCheck(FShellFolder.EnumObjects(0, Flags, Result));
  end else
    Result := nil;
end;

function TExplorerShellNode.GetNodeType: TExplorerNodeType;
begin
  if Assigned(FShellFolder) then
    Result := ntFolder else Result := ntNode;
end;

procedure TExplorerShellNode.SetShellFolder(Value: IShellFolder);
begin
  Clear;
  FShellFolder := Value;
end;

procedure TExplorerShellNode.SetItemIDLists(ValueRel, ValueAbs: PItemIDList);
var
  psfi: TSHFileInfo;
  ShFolder: IShellFolder;
begin
  FreePIDL(FItemIDLists[0]);
  FreePIDL(FItemIDLists[1]);
  if Assigned(ValueRel) then
  begin
    BeginUpdate;
    try
      FItemIDLists[0] := ValueRel;
      FItemIDLists[1] := ValueAbs;
      ShFolder := GetParentShellFolder;
      FAttrs := SFGAO_CAPABILITYMASK or SFGAO_DISPLAYATTRMASK or
        SFGAO_FILESYSTEM or SFGAO_FOLDER or SFGAO_FILESYSANCESTOR or $FF0000;
      ShFolder.GetAttributesOf(1, FItemIDLists[0], FAttrs);
      Text := GetDisplayName(ShFolder, ItemIDListRel, DisplayName);
      FFileName := GetDisplayName(ShFolder, ItemIDListRel, sdnForParsing);

      if SHGetFileInfo(PChar(ItemIDListAbs), 0, psfi, SizeOf(psfi),
         SHGFI_ICON or SHGFI_SMALLICON or SHGFI_SYSICONINDEX or SHGFI_DISPLAYNAME or SHGFI_PIDL) <> 0 then
      begin
        ImageIndex := psfi.iIcon;
        SHGetFileInfo(PChar(ItemIDListAbs), 0, psfi, SizeOf(psfi),
          SHGFI_ICON or SHGFI_SMALLICON or SHGFI_SYSICONINDEX or SHGFI_OPENICON or SHGFI_PIDL);
        SelectedIndex := psfi.iIcon;
        if (FAttrs and SFGAO_SHARE) <> 0 then
          OverlayIndex := 0
        else if (FAttrs and SFGAO_LINK) <> 0 then
          OverlayIndex := 1
        else
          OverlayIndex := -1;
        Changed;
      end;
    finally
      EndUpdate;
    end;
  end;
end;

procedure TExplorerShellNode.SetBrowseOptions(Value: TShellBrowseOptions);
begin
  if FBrowseOptions <> Value then
  begin
    FBrowseOptions := Value;
    Clear;
  end;
end;

procedure TExplorerShellNode.SetDisplayName(Value: TShellDisplayName);
begin
  if FDisplayName <> Value then
  begin
    FDisplayName := Value;
    Clear;
  end;
end;

function TExplorerShellNode.GetParentShellFolder: IShellFolder;
begin
  Result := TExplorerShellNode(Parent).ShellFolder;
end;

function TExplorerShellNode.ShellExecute(Verb: PChar; ShowCmd: Integer): BOOL;
var
  sei: TShellExecuteInfo;
begin
  ZeroMemory(@sei, SizeOf(sei));
  with sei do
  begin
    cbSize := SizeOf(sei);
    fMask := SEE_MASK_INVOKEIDLIST or SEE_MASK_NOCLOSEPROCESS;
    Wnd := HWND_DESKTOP;
    lpVerb := Verb;
    lpIDList := ItemIDListAbs;
    nShow := ShowCmd;
    Result := ShellExecuteEx(@sei);
  end;
end;

procedure TExplorerShellNode.DefaultSort;

  function Compare(Item1, Item2: Pointer): Integer;
  var
    Attr1, Attr2: ULONG;
  begin
    if Item1 = Item2 then
      Result := 0
    else begin
      Attr1 := TExplorerShellNode(Item1).Attrs;
      Attr2 := TExplorerShellNode(Item2).Attrs;
      Result := CompareInteger(Attr1 and SFGAO_LINK, Attr2 and SFGAO_LINK);
      if Result <> 0 then Exit;
      Result := CompareInteger(Attr2 and SFGAO_FILESYSANCESTOR, Attr1 and SFGAO_FILESYSANCESTOR);
      if Result <> 0 then Exit;
      Result := CompareInteger(Attr2 and SFGAO_FOLDER, Attr1 and SFGAO_FOLDER);
      if Result <> 0 then Exit;
      Result := AnsiCompareText(TExplorerShellNode(Item1).FileName, TExplorerShellNode(Item2).FileName);
      if Result <> 0 then Exit;
      Result := CompareInteger(TExplorerShellNode(Item1).EnumIndex, TExplorerShellNode(Item2).EnumIndex);
    end;
 end;

begin
  Sort(@Compare);
end;

procedure TExplorerShellNode.InternalExpand;
var
  TmpShellFolder: IShellFolder;
  rgelt: PItemIDList;
  pceltFetched: ULONG;
  Node: TExplorerShellNode;
  Unk: IUnknown;
  EnumObjects: IEnumIDList;
begin
  if not Assigned(FShellFolder) then Exit;
  AppSetCursor(crHourglass);
  try
    EnumObjects := GetEnumObjects;
    EnumObjects.Reset;
    while EnumObjects.Next(1, rgelt, pceltFetched) = NOERROR do
    try
      Node := TExplorerShellNode.Create(Self);
      try
        if (ShellFolder.BindToObject(rgelt, nil, IID_IShellFolder, Pointer(Unk)) = S_OK) and
          (Unk.QueryInterface(IShellFolder, TmpShellFolder) = S_OK) then
          Node.ShellFolder := TmpShellFolder;
        Node.Parent := Self;
        Node.DisplayName := Self.DisplayName;
        Node.SetItemIDLists(rgelt, ConcatPIDLs(ItemIDListAbs, rgelt));
        Node.BrowseOptions := Self.BrowseOptions;
        Node.FEnumIndex := Count;
        Node.Sorted := Self.Sorted;
      except
        Node.Free;
        raise;
      end;
    except
      FreePIDL(rgelt);
      raise;
    end;
  finally
    AppRestoreCursor;
  end;
end;

function TExplorerShellNode.IsRuntime: Boolean;
begin
  Result := not (Self is TExplorerShellFolderNode);
end;

function TExplorerShellNode.AcceptsNodes(ExplorerNodes: TExplorerNodes): Boolean;
begin
  Result := ExplorerNodes is TExplorerShellNode;
end;

function TExplorerShellNode.CanCopy: Boolean;
begin
  Result := FAttrs and SFGAO_CANCOPY <> 0;
end;

function TExplorerShellNode.CanDelete: Boolean;
begin
  Result := FAttrs and SFGAO_CANDELETE <> 0;
end;

function TExplorerShellNode.CanDrag: Boolean;
begin
  Result := FAttrs and SFGAO_CANMOVE <> 0;
end;

function TExplorerShellNode.CanDrop(List: TExplorerNodesList): Boolean;
begin
  Result := FAttrs and SFGAO_DROPTARGET <> 0;
end;

function TExplorerShellNode.CanEdit: Boolean;
begin
  Result := (FAttrs and SFGAO_CANRENAME <> 0) and (FDisplayName = sdnInFolder);
end;

procedure TExplorerShellNode.ClipboardNotification(Operation: TOperation);
begin
end;

procedure TExplorerShellNode.DoCopyList(List: TExplorerNodesList);
var
  Size: Integer;
  hClip: THandle;
  pFileNames: PChar;
  pFiles: pDROPFILES;
begin
  pFileNames := AllocShellNodesFileStr(List, Size);
  try
    hClip := GlobalAlloc(GPTR, SizeOf(TDROPFILES) + Size);
    try
      pFiles := GlobalLock(hClip);
      pFiles.pFiles := SizeOf(TDROPFILES);
      Move(pFileNames^, (PChar(pFiles) + SizeOf(TDROPFILES))^, Size);
      GlobalUnlock(hClip);
      Clipboard.SetAsHandle(CF_HDROP, hClip);
    except
      GlobalFree(hClip);
      raise;
    end;
  finally
    StrDispose(pFileNames);
  end;
  inherited;
end;

procedure TExplorerShellNode.DoCutList(List: TExplorerNodesList);
begin
  DoCopyList(List);
end;

procedure TExplorerShellNode.DoDelete;
var
  sfop: TSHFileOpStruct;
begin
  ZeroMemory(@sfop, SizeOf(sfop));
  AppSetCursor(crHourglass);
  with sfop do
  try
    wFunc := FO_DELETE;
    pFrom := AllocFileStr(FileName);
    fFlags := FOF_SIMPLEPROGRESS;
    SetShellFolder(nil);  // Release file lock
    if DoSHFileOperation(sfop) = 0 then
      inherited;
  finally
    StrDispose(pFrom);
    StrDispose(pTo);
    AppRestoreCursor;
  end;
end;

procedure TExplorerShellNode.DoPaste(List: TExplorerNodesList);
begin
  DragDrop(List);
  ExplorerClipboard.Clear;
end;

procedure TExplorerShellNode.DragDrop(List: TExplorerNodesList);
var
  I, Size: Integer;
  DoClear: Boolean;
  sfop: TSHFileOpStruct;
  SaveParent: TExplorerNodes;
begin
  ZeroMemory(@sfop, SizeOf(sfop));
  AppSetCursor(crHourglass);
  with sfop do
  try
    if (GetKeyState(VK_CONTROL) and not $7FFF) <> 0 then
      wFunc := FO_COPY else
      wFunc := FO_MOVE;
    pFrom := AllocShellNodesFileStr(List, Size);
    pTo := AllocFileStr(FileName);
    fFlags := FOF_SIMPLEPROGRESS;

    SaveParent := List[0].Parent;
    SaveParent.BeginExpand;
    try
      for I := 0 to List.Count - 1 do
        TExplorerShellNode(List[I]).SetShellFolder(nil);
      DoClear := not SaveParent.IsParentOf(Self);
      SaveParent.Clear;
      if DoClear then Clear;
      DoSHFileOperation(sfop);
    finally
      SaveParent.EndExpand;
    end;
  finally
    StrDispose(pFrom);
    StrDispose(pTo);
    AppRestoreCursor;
  end;
end;

procedure TExplorerShellNode.DoEdit(var Text: string);
var
  sfop: TSHFileOpStruct;
  NewFileName: string;
begin
  ZeroMemory(@sfop, SizeOf(sfop));
  AppSetCursor(crHourglass);
  with sfop do
  try
    wFunc := FO_RENAME;
    pFrom := AllocFileStr(FileName);
    NewFileName := ExtractFilePath(FileName) + Text;
    if (Pos('.', Text) = 0) then
      NewFileName := NewFileName + ExtractFileExt(FileName);
    pTo := AllocFileStr(NewFileName);
    fFlags := FOF_SIMPLEPROGRESS;
    if DoSHFileOperation(sfop) = 0 then
    begin
      FFileName := NewFileName;
      inherited;
    end;
  finally
    StrDispose(pFrom);
    StrDispose(pTo);
    AppRestoreCursor;
  end;
end;

{ TExplorerShellFolderNode }
constructor TExplorerShellFolderNode.Create(AOwner: TComponent);
begin
  inherited;
  CreateShellFolder;
end;

function TExplorerShellFolderNode.GetParentShellFolder: IShellFolder;
begin
  OleCheck(SHGetDesktopFolder(Result));
end;

procedure TExplorerShellFolderNode.DefaultSort;
begin
  if Folder <> sfDesktop then inherited;
end;

procedure TExplorerShellFolderNode.CreateShellFolder;
const
{$IFNDEF _D4_}
  CSIDL_INTERNET                      = $0001;
  CSIDL_ALTSTARTUP                = $001d;         // DBCS
  CSIDL_COMMON_ALTSTARTUP         = $001e;         // DBCS
  CSIDL_COMMON_FAVORITES          = $001f;
  CSIDL_INTERNET_CACHE            = $0020;
  CSIDL_COOKIES                   = $0021;
  CSIDL_HISTORY                   = $0022;
{$ENDIF}

  FolderMap: array [TShellFolder] of DWord = (
    CSIDL_DESKTOP, CSIDL_INTERNET, CSIDL_PROGRAMS, CSIDL_CONTROLS,
    CSIDL_PRINTERS, CSIDL_PERSONAL, CSIDL_FAVORITES, CSIDL_STARTUP,
    CSIDL_RECENT, CSIDL_SENDTO, CSIDL_BITBUCKET, CSIDL_STARTMENU,
    CSIDL_DESKTOPDIRECTORY, CSIDL_DRIVES, CSIDL_NETWORK, CSIDL_NETHOOD,
    CSIDL_FONTS, CSIDL_TEMPLATES, CSIDL_COMMON_STARTMENU, CSIDL_COMMON_PROGRAMS,
    CSIDL_COMMON_STARTUP, CSIDL_COMMON_DESKTOPDIRECTORY, CSIDL_APPDATA,
    CSIDL_PRINTHOOD, CSIDL_ALTSTARTUP, CSIDL_COMMON_ALTSTARTUP,
    CSIDL_COMMON_FAVORITES, CSIDL_INTERNET_CACHE, CSIDL_COOKIES, CSIDL_HISTORY);

var
  DesktopShellFolder, TmpShellFolder: IShellFolder;
  TmpIDList, DesktopIDList, AbsIDList: PItemIDList;
  Unk: IUnknown;
begin
  SHGetSpecialFolderLocation(0, FolderMap[FFolder], TmpIDList);
  OleCheck(SHGetDesktopFolder(DesktopShellFolder));
  if FFolder = sfDesktop then
    ShellFolder := DesktopShellFolder
  else if (DesktopShellFolder.BindToObject(TmpIDList, nil, IID_IShellFolder, Pointer(Unk)) = S_OK)
    and (Unk.QueryInterface(IShellFolder, TmpShellFolder) = S_OK) then
    ShellFolder := TmpShellFolder;
  SHGetSpecialFolderLocation(0, CSIDL_DESKTOP, DesktopIDList);
  try
    if FFolder = sfDesktop then
      AbsIDList := CopyPIDL(DesktopIDList) else
      AbsIDList := ConcatPIDLs(DesktopIDList, TmpIDList);
    SetItemIDLists(TmpIDList, AbsIDList);
  finally
    FreePIDL(DesktopIDList);
  end;
end;

procedure TExplorerShellFolderNode.SetFolder(Value: TShellFolder);
begin
  if (FFolder <> Value) then
  begin
    ShellFolder := nil;
    FFolder := Value;
    CreateShellFolder;
  end;
end;
{$ENDIF}

end.
