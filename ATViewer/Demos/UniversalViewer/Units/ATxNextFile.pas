unit ATxNextFile;

interface

uses
  Windows, TntClasses;

type
  TATNextFile = (nfNext, nfPrev, nfCurrent);
  TATFileSort = (fsName, fsNameDesc, fsExt, fsExtDesc);

type
  TATFileList = class
  private
    FList: TTntStringList;
    FListCountLimit: integer;
    FDirectory: WideString;
    FSortOrder: TATFileSort;
    FLocked: boolean;
    FLockedDir: boolean;
    FListIndex: integer;
    FSkipHidden: boolean;
    procedure SetLocked(AValue: boolean);
    function GetCount: integer;
    function GetItem(AIndex: integer): WideString;
  public
    constructor Create;
    destructor Destroy; override;
    function ReadDirectory(const ADirName: WideString): boolean;
    function ReadFileList(const AFileName: WideString): boolean;
    function GetFirst: WideString;
    function GetNext(const AFileName: WideString; ANext: TATNextFile; AMessage: boolean = false): WideString;
    procedure Delete(const AFileName: WideString);
    procedure ShowList;
    property SortOrder: TATFileSort read FSortOrder write FSortOrder;
    property Locked: boolean read FLocked write SetLocked;
    property LockedDir: boolean read FLockedDir write FLockedDir;
    property Count: integer read GetCount;
    property ListIndex: integer read FListIndex;
    property SkipHidden: boolean read FSkipHidden write FSkipHidden;
  end;


implementation

uses
  SysUtils, Classes, Forms,
  ATxFProc, ATxSProc, ATViewerMsg, ATxMsg;


{ Helper functions }

const
  cListCountLimit = 200; //Limit of files num (used for nfCurrent)
var
  FGlobalSortOrder: TATFileSort = fsExt;


//get: s1 - filename without number and ext,
//  s2 - number (at the end), sExt - extension.
procedure SFilenameParts(const fn: WideString; var s1, s2, sExt: WideString);
begin
  sExt:= SExtractFileExt(fn);
  s1:= Copy(fn, 1, Length(fn) - Length(sExt));
  s2:= '';
  while (s1 <> '') and (Char(s1[Length(s1)]) in ['0'..'9']) do
    begin
    s2:= s1[Length(s1)] + s2;
    Delete(s1, Length(s1), 1);
    end;
end;

function CompareFNames(const fn1, fn2: WideString): Integer;
var
  ss1, ss2, sn1, sn2, sExt1, sExt2: WideString;
begin
  SFilenameParts(fn1, ss1, sn1, sExt1);
  SFilenameParts(fn2, ss2, sn2, sExt2);
  //MsgInfo(Format('1: %s %s %s, 2: %s %s %s', [ss1, sn1, sExt1, ss2, sn2, sExt2]));

  Result:= SCompareIW(ss1, ss2);
  if Result = 0 then
    Result:= StrToIntDef(sn1, 0) - StrToIntDef(sn2, 0);
  if Result = 0 then
    Result:= SCompareIW(sExt1, sExt2);
end;


function ListCompare(List: TTntStringList; Index1, Index2: integer): integer;
var
  fn1, fn2: WideString;
begin
  fn1:= List[Index1];
  fn2:= List[Index2];
  case FGlobalSortOrder of
    fsExt,
    fsExtDesc:
      begin
      Result:= SCompareIW(SExtractFileExt(fn1), SExtractFileExt(fn2));
      if Result = 0 then
        Result:= CompareFNames(fn1, fn2);
      if FGlobalSortOrder = fsExtDesc then
        Result:= -Result;
      end;
    fsName,
    fsNameDesc:
      begin
      Result:= CompareFNames(fn1, fn2);
      if FGlobalSortOrder = fsNameDesc then
        Result:= -Result;
      end;
    else
      Result:= 0;
  end;
end;


{ TATFileList }

constructor TATFileList.Create;
begin
  inherited;
  FList:= TTntStringList.Create;
  FListCountLimit:= 0;
  with FList do
    begin
    Duplicates:= dupIgnore;
    Sorted:= false;
    CaseSensitive:= false;
    end;
  FDirectory:= '';
  FSortOrder:= fsExt;
  FLocked:= false;
  FLockedDir:= false;
  FListIndex:= -1;
  FSkipHidden:= true;
end;

destructor TATFileList.Destroy;
begin
  FreeAndNil(FList);
  inherited;
end;

function TATFileList.GetItem(AIndex: integer): WideString;
begin
  Result:= '';
  if (AIndex >= 0) and (AIndex < FList.Count) then
    begin
    FListIndex:= AIndex;
    if FLocked and not (FLockedDir) then
      Result:= FList[AIndex]
    else
      Result:= FDirectory + '\' + FList[AIndex];
    end;
end;

function TATFileList.GetFirst: WideString;
begin
  Result:= GetItem(0);
end;


function TATFileList.ReadDirectory(const ADirName: WideString): boolean;
var
  h: THandle;
  fdA: TWin32FindDataA;
  fdW: TWin32FindDataW;
  fn, dir: string;
  fnW: WideString;
  IsDir, IsHidden, IsSystem: boolean;
begin
  Result:=
    IsDirExist(ADirName) or
    IsFileOrDirExist(ADirName + '\*.*');
  if not Result then Exit;

  FDirectory:= ADirName;
  FList.Clear;
  h:= INVALID_HANDLE_VALUE;

  try
    if Win32Platform=VER_PLATFORM_WIN32_NT then
      begin
      h:= FindFirstFileW(PWChar(ADirName+'\*.*'), fdW);
      if h<>INVALID_HANDLE_VALUE then
        repeat
          fnW:= fdW.cFileName;
          //if Length(fnW)>MAX_PATH then SetLength(fnW, MAX_PATH);
          //if (fnW='.') or (fnW='..') then Continue;

          IsDir:= (fdW.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY)<>0;
          IsHidden:= (fdW.dwFileAttributes and FILE_ATTRIBUTE_HIDDEN)<>0;
          IsSystem:= (fdW.dwFileAttributes and FILE_ATTRIBUTE_SYSTEM)<>0;

          if (not IsDir) and ((not FSkipHidden) or (not IsHidden)) and (not IsSystem) then
            FList.Add(fnW);

          if (FListCountLimit > 0) and
            (FList.Count >= FListCountLimit) then Break;

        until not FindNextFileW(h, fdW);
      end
    else
      begin
      dir:= string(ADirName);
      h:= FindFirstFileA(PAnsiChar(dir+'\*.*'), fdA);
      if h<>INVALID_HANDLE_VALUE then
        repeat
          fn:= fdA.cFileName;
          //if Length(fn)>MAX_PATH then SetLength(fn, MAX_PATH);
          //if (fn='.') or (fn='..') then Continue;

          IsDir:= (fdA.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY)<>0;
          IsHidden:= (fdA.dwFileAttributes and FILE_ATTRIBUTE_HIDDEN)<>0;
          IsSystem:= (fdA.dwFileAttributes and FILE_ATTRIBUTE_SYSTEM)<>0;

          if (not IsDir) and ((not FSkipHidden) or (not IsHidden)) and (not IsSystem) then
            FList.Add(fn);

          if (FListCountLimit > 0) and
            (FList.Count >= FListCountLimit) then Break;

        until not FindNextFileA(h, fdA);
      end;
  finally
    Windows.FindClose(h);
  end;

  FGlobalSortOrder:= FSortOrder;
  FList.CustomSort(ListCompare);
end;


function FReadToString(const FileName: WideString; var S: string): boolean;
const
  cMaxFilelistSize = 1024*1024; //Maximal size of a file list
var
  Buffer: PChar;
  BufferSize, ReadSize: DWORD;
  Handle: THandle;
begin
  Result:= false;
  S:= '';

  Handle:= FFileOpen(FileName);
  if Handle<>INVALID_HANDLE_VALUE then
    try
      Buffer:= nil;
      BufferSize:= DWORD(FGetFileSize(Handle));
      if BufferSize > cMaxFilelistSize then Exit;
      GetMem(Buffer, BufferSize);

      if ReadFile(Handle, Buffer^, BufferSize, ReadSize, nil) then
        begin
        SetString(S, Buffer, ReadSize);
        Result:= true;
        end;
    finally
      if Assigned(Buffer) then
        FreeMem(Buffer);
      CloseHandle(Handle);
    end;
end;

function SGetItem(var SList: string): string;
const
  CR1 = #13#10;
  CR2 = #13;
var
  k: integer;
  len: integer;
begin
  k:= Pos(CR1, SList);
  if k>0 then
    len:= Length(CR1)
  else
    begin
    k:= Pos(CR2, SList);
    if k>0 then
      len:= Length(CR2)
    else
      begin
      k:= MaxInt;
      len:= 0;
      end;
    end;

  Result:= Copy(SList, 1, k-1);
  Delete(SList, 1, k+len-1);
end;


function TATFileList.ReadFileList(const AFileName: WideString): boolean;
var
  S, Item: string;
begin
  Result:= false;

  FDirectory:= '';
  FList.Clear;

  if not FReadToString(AFileName, S) then
    begin
    MsgError(SFormatW(MsgViewerErrInvalidFilelist, [AFileName]));
    Exit;
    end;

  repeat
    Item:= SGetItem(S);
    
    if Item = '' then Break;

    if (FList.Count = 0) and (not IsFileExist(Item)) then 
      begin
      MsgWarning(SFormatW(MsgViewerErrCannotFindFile, [Item]));        
      Exit;
      end;

    FList.Add(Item);
  until false;

  //Delete filelist only when success
  //(filelist contains at last 1 valid file name):
  FDeleteToRecycle(Application.Handle, AFileName, false);

  Locked:= true;
  Result:= true;
end;


function TATFileList.GetNext(
  const AFileName: WideString;
  ANext: TATNextFile;
  AMessage: boolean = false): WideString;
var
  fnW, dirW: WideString;
  n: integer;
begin
  Result:= '';
  FListIndex:= -1;

  //Limit file count when ANext=nfCurrent
  if ANext = nfCurrent then
    FListCountLimit:= cListCountLimit
  else
    FListCountLimit:= 0;

  fnW:= SExtractFileName(AFileName);
  dirW:= SExtractFileDir(AFileName);
  SDelLastSlashW(dirW);         //Case of root folder
  if dirW = '' then dirW:= '.'; //Case of filename without path

  if not FLocked then
    if not ReadDirectory(dirW) then
      begin
      if AMessage then
        MsgWarning(MsgViewerJumpDirEmpty);
      Exit
      end;

  //debug
  //MsgInfo('Debug:'#13'dir: '+dirW+#13'file: '+fnW+#13'files count: '+IntToStr(FList.Count));

  if FList.Count = 0 then
    begin
    if AMessage then
      MsgWarning(MsgViewerJumpDirEmpty);
    Exit
    end;

  if FLocked and (not FLockedDir) then
    n:= FList.IndexOf(AFileName)
  else
    n:= FList.IndexOf(fnW);

  //File not found in directory
  if n < 0 then
    begin
    if ANext = nfCurrent then Exit;
    if (not AMessage) or (MsgBox(SFormatW(MsgViewerJumpNotFound, [fnW]), MsgViewerCaption, MB_OKCANCEL or MB_ICONWARNING)=IDOK)
      then Result:= GetFirst else Exit;
    end
  else
    begin
    //File is single in directory
    if FList.Count = 1 then
      begin
      if AMessage then
        MsgWarning(SFormatW(MsgViewerJumpSingleFile, [fnW]));
      Exit;
      end;

    //Some files in directory
    case ANext of
      nfNext:
        begin
        Inc(n);
        if n > FList.Count-1 then
          begin
          if (not AMessage) or (MsgBox(MsgViewerJumpToFirst, MsgViewerCaption, MB_OKCANCEL or MB_ICONWARNING)=IDOK)
            then n:= 0 else Exit;
          end;
        Result:= GetItem(n);
        end;

      nfPrev:
        begin
        Dec(n);
        if n < 0 then
          begin
          if (not AMessage) or (MsgBox(MsgViewerJumpToLast, MsgViewerCaption, MB_OKCANCEL or MB_ICONWARNING)=IDOK)
            then n:= FList.Count-1 else Exit;
          end;
        Result:= GetItem(n);
        end;

      nfCurrent:
        begin
        //For current file: just update FListIndex (callGetItem)
        Result:= GetItem(n);
        end;
    end;
    end;
end;


procedure TATFileList.ShowList;
var
  S: WideString;
  i: integer;
begin
  S:= '';
  for i:= 0 to FList.Count-1 do
    S:= S+GetItem(i)+#13;
  MsgInfo('File list:'#13#13+S);
end;


procedure TATFileList.SetLocked(AValue: boolean);
begin
  FLocked:= AValue;
  FLockedDir:= false;
  if not FLocked then
    begin
    FList.Clear;
    FListIndex:= -1;
    end;
end;

function TATFileList.GetCount: integer;
begin
  Result:= FList.Count;
end;


procedure TATFileList.Delete(const AFileName: WideString);
var
  i: integer;
begin
  for i:= 0 to FList.Count - 1 do
    if SCompareIW(AFileName, GetItem(i)) = 0 then
      begin
      FList.Delete(i);
      Break;
      end;
end;

end.
