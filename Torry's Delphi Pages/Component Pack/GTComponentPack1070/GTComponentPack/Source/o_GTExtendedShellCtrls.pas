{*******************************************************}
{                                                       }
{       GT Delphi Components                            }
{                                                       }
{       Copyright (c) GT Delphi Components              }
{       http://www.gtdelphicomponents.gr                }
{                                                       }
{                                                       }
{*******************************************************}
unit o_GTExtendedShellCtrls;
{$WARN UNIT_PLATFORM OFF}

interface
uses
   Classes
  ,Controls
  ,ComCtrls
  ,ShellCtrls
  ,StdCtrls
  ,Messages
  ;
type
{------------------------------------------------------------------------------}
  TgtShellTreeView = class(TShellTreeView);
{------------------------------------------------------------------------------}
  TgtShellItemAction = (
                         siaCopy
                        ,siaMove
                        ,siaDelete
                        ,siaRename
                        ,siaShred
                        ,siaCreateShortCut
                        );
{------------------------------------------------------------------------------}
  TgtShellListViewOnDropFile = procedure (Sender : TObject ; AFileName : string) of Object;
{------------------------------------------------------------------------------}
  TgtShellItem = class(TPersistent)
  private
    { Private declarations }
    FShellFolder : TShellFolder;
    FSize        : Int64;
    FFullName    : string;
    FPath        : string;
    FName        : string;
    FCreationDate: TDateTime;
    FModDate     : TDateTime;
    FFullPath: string;
    FFileType: string;
    function GetFullNameAsPChar: PChar;
    function GetNameAsPChar: PChar;
    function GetPathAsPChar: PChar;
    function GetIsFolder: Boolean;
  protected
    { Protected declarations }
    FInitialized : Boolean;
    procedure Initialize;
    function GetFileSize(FileName : string):Int64;
    function GetFileModDate(FileName : string):TDateTime;
    function GetFileCreDate(FileName : string):TDateTime;
  public
    { Public declarations }
    constructor Create(ShellFolder : TShellFolder);
    destructor  Destroy;override;
    procedure Assign(Source : TPersistent);override;
    procedure AssignShellFolder(AShellFolder : TShellFolder);
    function  GetFullInfo : string;
    function  Rename(NewName : string):Boolean;
  public
    property NameAsPChar     : PChar     read GetNameAsPChar;
    property PathAsPChar     : PChar     read GetPathAsPChar;
    property FullNameAsPChar : PChar     read GetFullNameAsPChar;
    property IsFolder        : Boolean   read GetIsFolder;
  published
    { Published declarations}
    property Name            : string    read FName;
    property Path            : string    read FPath;
    property FullName        : string    read FFullPath;
    property Size            : Int64     read FSize;
    property ModifiedDate    : TDateTime read FModDate;
    property CreationDate    : TDateTime read FCreationDate;
    property FileType        : string    read FFileType;
  end;
{------------------------------------------------------------------------------}
  TgtShellComboBox = class(TShellComboBox)
  private
    { Private declarations }
  protected
    { Protected declarations }
    procedure KeyDown(var Key: Word; Shift: TShiftState);override;
  public
    { Public declarations }
    constructor Create(AOwner:TComponent);override;
    destructor  Destroy;override;
  published
    { Published declarations}
  end;
{------------------------------------------------------------------------------}
  TgtShellListView = class(TShellListView)
  private
    FFileMask: string;
    FCurrentShellItem: TgtShellItem;
    FOnDropFile: TgtShellListViewOnDropFile;
    procedure SetFileMask(const Value: string);
    { Private declarations }
  protected
    { Protected declarations }
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function  OwnerDataFetch(Item: TListItem; Request: TItemRequest): Boolean; override;
    procedure DblClick; override;
    procedure Click;override;
    procedure WndProc(var Message: TMessage); override;
    procedure SetParent(AParent: TWinControl);override;
  public
    { Public declarations }
    constructor Create(AOwner:TComponent);override;
    destructor  Destroy;override;
  public
    property  CurrentShellItem : TgtShellItem read FCurrentShellItem;
  published
    { Published declarations}
    property  FileMask       : string                       read FFileMask       write SetFileMask;
    property  OnDropFile     : TgtShellListViewOnDropFile   read FOnDropFile     write FOnDropFile;
  end;
{------------------------------------------------------------------------------}

implementation
uses
   SysUtils
  ,ShellApi
  ,Windows
  ;

{ TgtShellListView }
{------------------------------------------------------------------------------}
constructor TgtShellListView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFileMask := '*.*';
  if not (csDesigning in ComponentState) then
    FCurrentShellItem := TgtShellItem.Create(nil);
end;
{------------------------------------------------------------------------------}
destructor TgtShellListView.Destroy;
begin
  if Assigned(FCurrentShellItem) then
    FCurrentShellItem.Free;
  inherited;
end;
{------------------------------------------------------------------------------}
procedure TgtShellListView.Notification(AComponent: TComponent;Operation: TOperation);
begin
  inherited Notification(AComponent,Operation);
end;
{------------------------------------------------------------------------------}
procedure TgtShellListView.WndProc(var Message: TMessage);
var
  pcFileName: PChar;
  i, iSize, iFileCount: integer;
begin

  case Message.Msg of
    {DropFiles taken from http://www.swissdelphicenter.ch/en/showcode.php?id=493}
    WM_DROPFILES :
      begin
        pcFileName := nil; // to avoid compiler warning message
        try
          iFileCount := DragQueryFile(Message.wParam, $FFFFFFFF, pcFileName, 255);
          for i := 0 to iFileCount - 1 do
          begin
            iSize := DragQueryFile(Message.wParam, i, nil, 0) + 1;
            pcFileName := StrAlloc(iSize);
            DragQueryFile(Message.wParam, i, pcFileName, iSize);
            if FileExists(pcFileName) then
              if Assigned(FOnDropFile) then
                FOnDropFile(Self,pcFileName);
            StrDispose(pcFileName);
          end;
          finally
            DragFinish(Message.wParam);
            StrDispose(pcFileName);
          end;
      end;
  end;
  inherited WndProc(Message);
end;
{------------------------------------------------------------------------------}
function TgtShellListView.OwnerDataFetch(Item: TListItem; Request: TItemRequest): Boolean;
{Taken from http://www.swissdelphicenter.ch/en/showcode.php?id=1259}
var
  XFolder: TShellFolder;
  XExt, XFltExt: string;
begin
  if (Length(FFileMask) > 0) and (FFileMask <> '*.*') then
  begin
    XFolder := Folders[Item.Index];
    if Assigned(XFolder) and not XFolder.IsFolder then
    begin
      XExt := ExtractFileExt(XFolder.PathName);
      XFltExt := ExtractFileExt(FFileMask);
      if CompareText(XExt, XFltExt) <> 0 then
      begin
        Result := False;
        Exit;
      end;
    end;
  end;
  Result := inherited OwnerDataFetch(Item, Request);
end;
{------------------------------------------------------------------------------}
procedure TgtShellListView.Click;
begin
  if Assigned(Selected) then
    FCurrentShellItem.AssignShellFolder(Folders[Selected.Index]);
  inherited Click;
end;
{------------------------------------------------------------------------------}
procedure TgtShellListView.DblClick;
begin
  inherited DblClick;
end;
{------------------------------------------------------------------------------}



//Getters - Setters \\
{------------------------------------------------------------------------------}
procedure TgtShellListView.SetFileMask(const Value: string);
{Taken from http://www.swissdelphicenter.ch/en/showcode.php?id=1259}
begin
 if FFileMask <> Value then
  begin
    FFileMask := Value;
    Invalidate;
  end;
end;
{------------------------------------------------------------------------------}
procedure TgtShellListView.SetParent(AParent: TWinControl);
begin
  inherited SetParent(AParent);
end;
{------------------------------------------------------------------------------}





{ TgtShellComboBox }
{------------------------------------------------------------------------------}
constructor TgtShellComboBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;
{------------------------------------------------------------------------------}
destructor TgtShellComboBox.Destroy;
begin
  inherited;
end;
{------------------------------------------------------------------------------}
procedure TgtShellComboBox.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if Key = 13 then
    if DirectoryExists(Text) then
    begin
      Path := Text;
      if Assigned(Self.ShellTreeView) then
        Self.ShellTreeView.Path := Path;
    end;
  inherited KeyDown(Key,Shift);
end;
{------------------------------------------------------------------------------}





{ TgtShellItem }
{------------------------------------------------------------------------------}
constructor TgtShellItem.Create(ShellFolder: TShellFolder);
begin
  inherited Create;
  FInitialized := False;
  FShellFolder := ShellFolder;
  Initialize;
end;
{------------------------------------------------------------------------------}
destructor TgtShellItem.Destroy;
begin
  inherited;
end;
{------------------------------------------------------------------------------}
procedure TgtShellItem.Initialize;
var
  FileInfo : SHFILEINFO;
begin
  if Assigned(FShellFolder) then
  begin
  if FShellFolder.IsFolder then
    begin
      FName := FShellFolder.DisplayName;
      FPath := FName;
      FFullName := FShellFolder.PathName;
    end
    else
    begin
      FName := ExtractFileName(FShellFolder.PathName);
      FPath := IncludeTrailingPathDelimiter(ExtractFilePath(FShellFolder.PathName));
      FFullName := FPath+FName;
    end;
    FCreationDate := GetFileCreDate(FFullName);
    FModDate      := GetFileModDate(FFullName);
    FSize         := GetFileSize(FFullName);
    SHGetFileInfo(PChar(FFullName), 0, FileInfo,SizeOf(FileInfo), SHGFI_TYPENAME);
    FFileType     := FileInfo.szTypeName;
    FInitialized  := True;
  end;
end;
{------------------------------------------------------------------------------}
procedure TgtShellItem.Assign(Source: TPersistent);
begin
  if Assigned(Source) then
    if Source is TgtShellItem then
    begin
      FName         := TgtShellItem(Source).Name;
      FPath         := TgtShellItem(Source).Path;
      FFullName     := TgtShellItem(Source).FullName;
      FCreationDate := TgtShellItem(Source).CreationDate;
      FModDate      := TgtShellItem(Source).ModifiedDate;
      FSize         := TgtShellItem(Source).Size;
    end
    else
      inherited Assign(Source);
end;
{------------------------------------------------------------------------------}
procedure TgtShellItem.AssignShellFolder(AShellFolder: TShellFolder);
begin
  FShellFolder := AShellFolder;
  Initialize;
end;
{------------------------------------------------------------------------------}
function TgtShellItem.Rename(NewName: string):Boolean;
begin
  Result := False;
  if Assigned(FShellFolder) then
    Result:= FShellFolder.Rename(NewName);
end;
{------------------------------------------------------------------------------}

//Getters - Setters\\
{------------------------------------------------------------------------------}
function TgtShellItem.GetFileCreDate(FileName: string): TDateTime;
var
   W32Data : TWin32FindData;
   Handle  : Cardinal;
   LocalFileTime: TFileTime;
   Age : integer;
begin
  Result := 0;
  FillChar(W32Data, SizeOf(W32Data), 0);
  Handle := Windows.FindFirstFile(PChar(FileName),W32Data);
  try
    if Handle <> INVALID_HANDLE_VALUE then
    begin
      if (W32Data.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY) = 0 then
      begin
        FileTimeToLocalFileTime(W32Data.ftCreationTime, LocalFileTime);
        if FileTimeToDosDateTime(LocalFileTime, LongRec(Age).Hi,
          LongRec(Age).Lo) then
        begin
          Result := FileDateToDateTime(Age);
        end;
      end;
    end;
  finally
    Windows.FindClose(Handle);
  end;
end;
{------------------------------------------------------------------------------}
function TgtShellItem.GetFileModDate(FileName: string): TDateTime;
var
   W32Data : TWin32FindData;
   Handle  : Cardinal;
   LocalFileTime: TFileTime;
   Age : integer;
begin
  Result := 0;
  FillChar(W32Data, SizeOf(W32Data), 0);
  Handle := Windows.FindFirstFile(PChar(FileName),W32Data);
  try
    if Handle <> INVALID_HANDLE_VALUE then
    begin
      if (W32Data.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY) = 0 then
      begin
        FileTimeToLocalFileTime(W32Data.ftLastWriteTime, LocalFileTime);
        if FileTimeToDosDateTime(LocalFileTime, LongRec(Age).Hi,
          LongRec(Age).Lo) then
        begin
          Result := FileDateToDateTime(Age);
        end;
      end;
    end;
  finally
    Windows.FindClose(Handle);
  end;
end;
{------------------------------------------------------------------------------}
function TgtShellItem.GetFileSize(FileName: string): Int64;
var
   W32Data : TWin32FindData;
   Handle  : Cardinal;
begin
  Result := 0;
  FillChar(W32Data, SizeOf(W32Data), 0);
  Handle := Windows.FindFirstFile(PChar(FileName),W32Data);
  try
    if Handle <> INVALID_HANDLE_VALUE then
    begin
      if (W32Data.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY) = 0 then
        Result := W32Data.nFileSizeHigh * MAXDWORD + W32Data.nFileSizeLow
      else
        Result := -1;
    end;
  finally
    Windows.FindClose(Handle);
  end;
end;
{------------------------------------------------------------------------------}
function TgtShellItem.GetFullInfo: string;
const
  FILE_FULL_INFO =
  'Size :%.2f KB | Type:%s | LastModified Date : %s';
begin
  Result := '';
  if not FInitialized then
    Initialize;
  if Assigned(FShellFolder) then
  begin
    if not FShellFolder.IsFolder then
      Result := Format(FILE_FULL_INFO,[(Self.Size/1024),Self.FileType,FormatDateTime('yyyy-mm-dd HH:mm:ss',Self.ModifiedDate)])
    else
      Result := FFullName;
  end;
end;
{------------------------------------------------------------------------------}
function TgtShellItem.GetFullNameAsPChar: PChar;
begin
  Result := PChar(FullName);
end;
{------------------------------------------------------------------------------}
function TgtShellItem.GetNameAsPChar: PChar;
begin
  Result := PChar(Name);
end;
{------------------------------------------------------------------------------}
function TgtShellItem.GetPathAsPChar: PChar;
begin
  Result := PChar(Path);
end;
{------------------------------------------------------------------------------}
function TgtShellItem.GetIsFolder: Boolean;
begin
  Result := False;
  if Assigned(FShellFolder) then
   Result := FShellFolder.IsFolder;
end;
{------------------------------------------------------------------------------}
























end.
