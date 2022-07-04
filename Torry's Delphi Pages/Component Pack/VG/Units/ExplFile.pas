{*******************************************************}
{                                                       }
{         Vladimir Gaitanoff Delphi VCL Library         }
{         Explorer library: file explorer               }
{                                                       }
{         Copyright (c) 1997, 2000                      }
{                                                       }
{*******************************************************}

{$I VG.INC }
{$D-,L-}

unit ExplFile;

interface
uses Windows, SysUtils, Classes, Controls, vgTools, {$IFDEF _D4_}ImgList,{$ENDIF}
  Explorer, FileCtrl;

type
{ TExplorerDrivesNode }
{ TExplorerDrivesNode incapsulates My computer shell folder }
  TExplorerDrivesNode = class(TExplorerNode)
  protected
    function GetLargeImages: TExplorerImageList; override;
    { Returns system image list with large images }
    function GetSmallImages: TExplorerImageList; override;
    { Returns system image list with small images }
    function GetStateImages: TExplorerImageList; override;
    { Returns image list with state images }
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function AcceptsNodes(ExplorerNodes: TExplorerNodes): Boolean; override;
    procedure DefaultSort; override;
  published
    property Index;
    property PopupMenu;
    property SubItems;
    property Sorted;
    property Visible;
  end;

{ TExplorerFileNode }
{ TExplorerFileNode is a basic class for drive, directory, and file }
  TExplorerFileNode = class(TExplorerNode)
  private
    FFileName: TFileName;
    procedure SetFileName(Value: TFileName);
  protected
    procedure DoDblClick; override;
    { Called when user double-clicks on node in TExplorerTreeView or TExplorerListView }
    { Method calls ShellExecute function to run selected file }
    procedure DoEdit(var Text: String); override;
    { Called when user edits text. Method renames edited file to new name  }
    function GetLargeImages: TExplorerImageList; override;
    { Returns system image list with large images }
    function GetNodeType: TExplorerNodeType; override;
    function GetSmallImages: TExplorerImageList; override;
    { Returns system image list with small images }
    function GetStateImages: TExplorerImageList; override;
    { Returns image list with state images }
    procedure UpdateDirFiles; virtual;
    { Method seeks for folders and files to know, when it can be expanded }
    { i.e. does it has children folders or files. }
    procedure UpdateFileInfo;
    { Method extracts file information: file size, image indexes and description. }
  public
    function CanCopy: Boolean; override;
    function CanCut: Boolean; override;
    function CanDelete: Boolean; override;
    function CanDrag: Boolean; override;
    function CanEdit: Boolean; override;
    function CanExpand(NodeTypes: TExplorerNodeTypes): Boolean; override;
    procedure ClipboardNotification(Operation: TOperation); override;
    { Called when users copies or cuts selected node into internal clipboard. }
    { Method changes StateImage to show that it have been copied or cutted. }
    function IsStored: Boolean; override;
    property FileName: TFileName read FFileName write SetFileName;
  end;

{ TExplorerDirNode }
{ TExplorerDirNode is a basic class for directory and file }
  TDirFile = (dfFiles, dfDirs);
  TDirFiles = set of TDirFile;

  TExplorerDirNode = class(TExplorerFileNode)
  private
    FDirFiles: TDirFiles;
  protected
    procedure InternalChangedChildren; override;
    { Called when Items property is changed (nodes are inserted or deleted). }
    { Method calls Sort method to sort children directories and files.       }
    procedure UpdateDirFiles; override;
  public
    function AcceptsNodes(ExplorerNodes: TExplorerNodes): Boolean; override;
    procedure DefaultSort; override;
    procedure DoPaste(List: TExplorerNodesList); override;
    { Called when nodes are pasted into directory from internal clipboard. }
    { Method renames each item in List and sets their Parent property to Self. }
    { Method clears internal clipboard. }
    { Note that inherited DoPaste may call .DoDelete method for each cutted node. }
    { But there is no .DoDelete implementation for nodes (normally it should delete file) }
    { so inherited DoPaste doesn't called. }
    procedure DragDrop(List: TExplorerNodesList); override;
    { Called when nodes are dropped into directory. }
    { Method renames each item in List and sets their Parent property to Self. }
    function CanExpand(NodeTypes: TExplorerNodeTypes): Boolean; override;
  end;

{ TExplorerDriveNode }
{ TExplorerDriveNode incapsulates drive }
  TExplorerDriveNode = class(TExplorerDirNode)
  private
    FDriveType: TDriveType;
  public
    property DriveType: TDriveType read FDriveType;
  end;

implementation
uses ShellAPI, {$IFDEF _D3_}ShlObj{$ELSE}vgShlObj{$ENDIF}, Graphics, vgVCLUtl;

{$R ExplFile.res}

var
  DrivesCount: Integer = 0;
  LargeImages: TExplorerImageList = nil;
  SmallImages: TExplorerImageList = nil;
  StateImages: TExplorerImageList = nil;

procedure AddExplorerDrives(Drives: TExplorerDrivesNode);
var
  psfi: TSHFileInfo;
  ppidl: PItemIDList;
begin
  if DrivesCount = 0 then
  begin
    SHGetSpecialFolderLocation(0, CSIDL_DRIVES, ppidl);
    LargeImages := TExplorerImageList.Create(nil);
    LargeImages.ShareImages := True;
    LargeImages.Handle := SHGetFileInfo(PChar(ppidl), 0, psfi, SizeOf(psfi),
      SHGFI_PIDL or SHGFI_ICON or SHGFI_LARGEICON or SHGFI_SYSICONINDEX);
    SmallImages := TExplorerImageList.Create(nil);
    SmallImages.ShareImages := True;
    SmallImages.Handle := SHGetFileInfo(PChar(ppidl), 0, psfi, SizeOf(psfi),
      SHGFI_PIDL or SHGFI_ICON or SHGFI_SMALLICON or SHGFI_SYSICONINDEX or SHGFI_DISPLAYNAME);
    StateImages := TExplorerImageList.Create(nil);
    StateImages.ResourceLoad(rtBitmap, 'IDB_STATES', clFuchsia);
  end else begin
    SHGetSpecialFolderLocation(0, CSIDL_DRIVES, ppidl);
    SHGetFileInfo(PChar(ppidl), 0, psfi, SizeOf(psfi),
      SHGFI_PIDL or SHGFI_ICON or SHGFI_SMALLICON or SHGFI_SYSICONINDEX or SHGFI_DISPLAYNAME);
  end;
  Drives.Text := psfi.szDisplayName;
  Drives.ImageIndex := psfi.iIcon;
  Drives.SelectedIndex := psfi.iIcon;
  Inc(DrivesCount);
end;

procedure DeleteExplorerDrives;
begin
  Dec(DrivesCount);
  if DrivesCount = 0 then
  begin
    LargeImages.Free;
    LargeImages := nil;
    SmallImages.Free;
    SmallImages := nil;
    StateImages.Free;
    StateImages := nil;
  end;
end;

{ TExplorerDrivesNode }
constructor TExplorerDrivesNode.Create(AOwner: TComponent);
begin
  inherited;
  AddExplorerDrives(Self);
end;

destructor TExplorerDrivesNode.Destroy;
begin
  inherited;
  DeleteExplorerDrives;
end;

function TExplorerDrivesNode.AcceptsNodes(ExplorerNodes: TExplorerNodes): Boolean;
begin
  Result := False;
end;

function TExplorerDrivesNode.GetLargeImages: TExplorerImageList;
begin
  Result := LargeImages;
end;

function TExplorerDrivesNode.GetSmallImages: TExplorerImageList;
begin
  Result := SmallImages;
end;

function TExplorerDrivesNode.GetStateImages: TExplorerImageList;
begin
  Result := StateImages;
end;

procedure TExplorerDrivesNode.DefaultSort;
var
  DriveNum: Integer;
  DriveChar: Char;
  DriveBits: set of 0..25;
begin
  { fill list }
  Integer(DriveBits) := GetLogicalDrives;
  for DriveNum := 0 to 25 do
  begin
    if not (DriveNum in DriveBits) then Continue;
    DriveChar := Char(DriveNum + Ord('a'));
    with TExplorerDriveNode.Create(Self) do
    try
      FileName := Upcase(DriveChar) + ':';
      FDriveType := TDriveType(GetDriveType(PChar(FileName + '\')));
      Parent := Self;
      Sorted := Self.Sorted;
    except
      Free;
      raise;
    end;
  end;
end;

{ TExplorerFileNode }
function TExplorerFileNode.CanCopy: Boolean;
begin
  Result := not (Self is TExplorerDriveNode);
end;

function TExplorerFileNode.CanCut: Boolean;
begin
  Result := CanCopy;
end;

function TExplorerFileNode.CanDelete: Boolean;
begin
  Result := False;
end;

function TExplorerFileNode.CanDrag: Boolean;
begin
  Result := not (csDesigning in ComponentState) and not (Parent is TExplorerDrivesNode);
end;

function TExplorerFileNode.CanEdit: Boolean;
begin
  Result := CanDrag;
end;

function TExplorerFileNode.CanExpand(NodeTypes: TExplorerNodeTypes): Boolean;
begin
  Result := False;
end;

procedure TExplorerFileNode.ClipboardNotification(Operation: TOperation);
begin
  BeginUpdate;
  try
    if Operation = opInsert then
    begin
      if ExplorerClipboard.State = csCopy then
        StateIndex := 1 else StateIndex := 2;
    end else
      StateIndex := -1;
  finally
    EndUpdate;
  end;
end;

procedure TExplorerFileNode.DoDblClick;
begin
  if not (Self is TExplorerDirNode) then
    ShellExecute(0, 'open', PChar(FileName), '', PChar(ExtractFilePath(FileName)), SW_SHOW);
end;

procedure TExplorerFileNode.DoEdit(var Text: String);
var
  NewFileName: String;
begin
  NewFileName := ExtractFilePath(FFileName) + Text;
  if (Pos('.', Text) = 0) and not (Self is TExplorerDirNode) then
    NewFileName := NewFileName + ExtractFileExt(FFileName);
  if RenameFile(FFileName, NewFileName) then FileName := NewFileName;
end;

function TExplorerFileNode.GetLargeImages: TExplorerImageList;
begin
  Result := LargeImages;
end;

function TExplorerFileNode.GetNodeType: TExplorerNodeType;
begin
  if Self is TExplorerDirNode then Result := ntFolder else Result := ntNode;
end;

function TExplorerFileNode.GetSmallImages: TExplorerImageList;
begin
  Result := SmallImages;
end;

function TExplorerFileNode.GetStateImages: TExplorerImageList;
begin
  Result := StateImages;
end;

function TExplorerFileNode.IsStored: Boolean;
begin
  Result := False;
end;

procedure TExplorerFileNode.SetFileName(Value: TFileName);
begin
  if (FFileName <> Value) then
  begin
    FFileName := Value;
    UpdateFileInfo;
    UpdateDirFiles;
  end;
end;

procedure TExplorerFileNode.UpdateDirFiles;
begin
  Clear;
end;

procedure TExplorerFileNode.UpdateFileInfo;
var
  S: String;
  psfi: TSHFileInfo;
begin
  BeginUpdate;
  try
    if Self is TExplorerDriveNode then
      S := FileName + '\' else S := FileName;
    SHGetFileInfo(PChar(S), 0, psfi, SizeOf(psfi),
      SHGFI_ICON or SHGFI_SMALLICON or SHGFI_SYSICONINDEX or SHGFI_DISPLAYNAME or
      SHGFI_TYPENAME);
    Text := psfi.szDisplayName;
    SubItems.Add(psfi.szTypeName);
    ImageIndex := psfi.iIcon;
    if Self is TExplorerDirNode then
      SHGetFileInfo(PChar(S), 0, psfi, SizeOf(psfi),
        SHGFI_ICON or SHGFI_SMALLICON or SHGFI_OPENICON or SHGFI_SYSICONINDEX);
    SelectedIndex := psfi.iIcon;
    if AnsiCompareText(ExtractFileExt(FileName), '.lnk') = 0 then
      OverlayIndex := 1 else OverlayIndex := -1;
  finally
    EndUpdate;
  end;
end;

{ TExplorerDirNode }
function TExplorerDirNode.AcceptsNodes(ExplorerNodes: TExplorerNodes): Boolean;
begin
  Result := (ExplorerNodes is TExplorerFileNode) and (ExplorerNodes <> Self) and
    (Self <> ExplorerNodes.Parent) and not ExplorerNodes.IsParentOf(Self);
end;

procedure TExplorerDirNode.DragDrop(List: TExplorerNodesList);
var
  I: Integer;
  Nodes: TExplorerNodes;
  NewFileName: String;
  SaveParent: TExplorerNodes;
begin
  BeginExpand;
  try
    if List.Count > 0 then
      SaveParent := List[0].Parent else
      SaveParent := nil;
    if Assigned(SaveParent) then SaveParent.BeginExpand;
    try
      for I := 0 to List.Count - 1 do
      begin
        Nodes := List[I];
        NewFileName := FFileName + '\' + ExtractFileName(TExplorerFileNode(Nodes).FileName);
        if RenameFile(TExplorerFileNode(Nodes).FileName, NewFileName) then
        begin
          TExplorerFileNode(Nodes).FileName := NewFileName;
          TExplorerFileNode(TExplorerFileNode(Nodes).Parent).UpdateDirFiles;
          Nodes.Parent := Self;
        end;
      end;
    finally
      if Assigned(SaveParent) then SaveParent.EndExpand;
    end;
  finally
    EndExpand;
  end;
end;

procedure TExplorerDirNode.DoPaste(List: TExplorerNodesList);
begin
  DragDrop(List);
  ExplorerClipboard.Clear;
end;

function TExplorerDirNode.CanExpand(NodeTypes: TExplorerNodeTypes): Boolean;
begin
  Result := (ntFolder in NodeTypes) and (dfDirs in FDirFiles) or
    (ntNode in NodeTypes) and (dfFiles in FDirFiles);
end;

procedure TExplorerDirNode.DefaultSort;
const
  Classes: array [Boolean] of TExplorerNodesClass = (TExplorerFileNode, TExplorerDirNode);
var
  Found: Integer;
  SearchRec: TSearchRec;
  Node: TExplorerNodes;
begin
  AppSetCursor(crHourglass);
  try
    Found := FindFirst(FFileName + '\*.*', faReadOnly or faArchive or faDirectory , SearchRec);
    try
      while (Found = 0) do
      begin
        if (Pos('.', SearchRec.Name) <> 1) then
        begin
          Node := Classes[(faDirectory and SearchRec.Attr) <> 0].Create(Self);
          try
            Node.SubItems.Add(Format('%d kb', [SearchRec.Size div 1024]));
            TExplorerFileNode(Node).FileName := Self.FileName + '\' + SearchRec.Name;
            Node.Parent := Self;
          except
            Node.Free;
            raise;
          end;
        end;
        Found := FindNext(SearchRec);
      end;
    finally
      FindClose(SearchRec);
    end;
  finally
    AppRestoreCursor;
  end;
end;

procedure TExplorerDirNode.InternalChangedChildren;
  function Compare(Item1, Item2: Pointer): Integer;
  begin
    if Item1 = Item2 then
      Result := 0
    else if (TExplorerNodes(Item1).ClassType = TExplorerFileNode) and (TExplorerNodes(Item2) is TExplorerDirNode) then
      Result := 1
    else if (TExplorerNodes(Item2).ClassType = TExplorerFileNode) and (TExplorerNodes(Item1) is TExplorerDirNode) then
      Result := -1
    else
      Result := AnsiCompareText(TExplorerNodes(Item1).Text, TExplorerNodes(Item2).Text);
  end;
begin
  Sort(@Compare);
end;

procedure TExplorerDirNode.UpdateDirFiles;
var
  Found: Integer;
  SearchRec: TSearchRec;
begin
  Clear;
  Found := FindFirst(FFileName + '\*.*', faReadOnly or faArchive or faDirectory, SearchRec);
  try
    while (Found = 0) do
    begin
      if (Pos('.', SearchRec.Name) <> 1) then
      begin
        if (faDirectory and SearchRec.Attr) <> 0 then
          FDirFiles := FDirFiles + [dfDirs] else FDirFiles := FDirFiles + [dfFiles];
        if FDirFiles = [dfDirs, dfFiles] then Exit;
      end;
      Found := FindNext(SearchRec);
    end;
  finally
    FindClose(SearchRec);
  end;
end;

end.


