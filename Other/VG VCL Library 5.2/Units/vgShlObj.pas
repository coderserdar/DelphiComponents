{*******************************************************}
{                                                       }
{         Vladimir Gaitanoff Delphi VCL Library         }
{         ShellApi support                              }
{                                                       }
{         Copyright (c) 1997, 2000                      }
{                                                       }
{*******************************************************}

{$I VG.INC }
{$D-,L-}

unit vgShlObj;

interface
uses Windows, Classes {$IFDEF _D3_}, ShlObj, ActiveX{$ENDIF};

type
  TShowCommand = (soHide, soShowNormal, soShowMinimized, soShowMaximized,
    soShowNoActivate, soShow, soMinimize, soShowMinNoActive, soShowNA,
    soRestore, soShowDefault);

{ TShellExecParams }
  TShellExecParams = class(TPersistent)
  private
    FhWnd: HWND;
    FParameters: string;
    FOperation: string;
    FFileName: string;
    FShowCommand: TShowCommand;
    FDirectory: string;
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
    function Execute: HINST;
    function ShowCmd: Integer;
  published
    property hWnd: HWND read FhWnd write FhWnd default 0;
    property Directory: string read FDirectory write FDirectory;
    property FileName: string read FFileName write FFileName;
    property Operation: string read FOperation write FOperation;
    property Parameters: string read FParameters write FParameters;
    property ShowComand: TShowCommand read FShowCommand write FShowCommand default soShow;
  end;

{$IFDEF _D3_}
type
  TShellDisplayName   = (sdnNormal, sdnInFolder, sdnForParsing);

function Malloc: IMalloc;
{ Creates and returns new IMalloc interface }

function CreatePIDL(Size: Integer): PItemIDList;
{ Creates a new itemIDs }

procedure FreePIDL(var ppidl: PItemIDList);
{ Frees allocated ppidl }

function GetPIDLSize(IDList: PItemIDList): Integer;
{ Returns a size of itemIDs }

function CopyPIDL(ID: PItemIDList): PItemIDList;
{ Creates a copy of itemIDs }

function NextPIDL(IDList: PItemIDList): PItemIDList;
{ Returns a next item of itemIDs }

function ConcatPIDLs(IDList1, IDList2: PItemIDList): PItemIDList;
{ Concatenates two itemIDs }

function GetDisplayName(ShellFolder: IShellFolder; PIDL: PItemIDList;
  DisplayName: TShellDisplayName): string;
{ Returns a display name if item }

{$ENDIF}

{$IFNDEF _D3_}
type
{ TSHItemID -- Item ID }
 PSHItemID = ^TSHItemID;
 TSHItemID = packed record           { mkid }
   cb: Word;                         { Size of the ID (including cbitself) }
   abID: array[0..0] of Byte;        { The item ID (variable length)}
 end;

{ TItemIDList -- List if item IDs (combined with 0-terminator) }
 PItemIDList = ^TItemIDList;
 TItemIDList = packed record         { idl }
    mkid: TSHItemID;
  end;

const
  CSIDL_DRIVES             = $0011;
  Shell32                  = 'shell32.dll';

function SHGetPathFromIDList(pidl: PItemIDList; pszPath: LPSTR): BOOL; stdcall;
 external Shell32 name 'SHGetPathFromIDList';
function SHGetSpecialFolderLocation(hwndOwner: HWND; nFolder: Integer;
 var ppidl: PItemIDList): HResult; stdcall; far; external Shell32
 name 'SHGetSpecialFolderLocation';
{$ENDIF}

implementation
uses ShellApi {$IFDEF _D3_}, ComObj, vgUtils{$ENDIF};

{$IFDEF _D3_}
function Malloc: IMalloc;
begin
  OleCheck(SHGetMalloc(Result));
end;

function CreatePIDL(Size: Integer): PItemIDList;
begin
  Result := Malloc.Alloc(Size);
  if Assigned(Result) then
    ZeroMem(Result, Size);
end;

procedure FreePIDL(var ppidl: PItemIDList);
begin
  Malloc.Free(ppidl);
  ppidl := nil;
end;

function GetPIDLSize(IDList: PItemIDList): Integer;
begin
  Result := 0;
  if Assigned(IDList) then
  begin
    Result := SizeOf(IDList^.mkid.cb);
    while IDList^.mkid.cb <> 0 do
    begin
      Result := Result + IDList^.mkid.cb;
      IDList := NextPIDL(IDList);
    end;
  end;
end;

function CopyPIDL(ID: PItemIDList): PItemIDList;
begin
  Result := Malloc.Alloc(ID^.mkid.cb + SizeOf(ID^.mkid.cb));
  CopyMemory(Result, ID, ID^.mkid.cb + SizeOf(ID^.mkid.cb));
end;

function NextPIDL(IDList: PItemIDList): PItemIDList;
begin
  Result := IDList;
  Inc(PChar(Result), IDList^.mkid.cb);
end;

function ConcatPIDLs(IDList1, IDList2: PItemIDList): PItemIDList;
var
  cb1, cb2: Integer;
begin
  if Assigned(IDList1) then
    cb1 := GetPIDLSize(IDList1) - SizeOf(IDList1^.mkid.cb) else
    cb1 := 0;

  cb2 := GetPIDLSize(IDList2);

  Result := CreatePIDL(cb1 + cb2);
  if Assigned(Result) then
  begin
    if Assigned(IDList1) then
      CopyMemory(Result, IDList1, cb1);
    CopyMemory(PChar(Result) + cb1, IDList2, cb2);
  end;
end;

function GetDisplayName(ShellFolder: IShellFolder; PIDL: PItemIDList;
  DisplayName: TShellDisplayName): string;
const
  DisplayNameMap: array[TShellDisplayName] of Integer = (
    SHGDN_NORMAL, SHGDN_INFOLDER, SHGDN_FORPARSING);
var
  StrRet: TStrRet;
  P: PChar;
begin
  Result := '';
  ShellFolder.GetDisplayNameOf(PIDL, DisplayNameMap[DisplayName], StrRet);
  case StrRet.uType of
    STRRET_CSTR:
      SetString(Result, StrRet.cStr, lStrLen(StrRet.cStr));
    STRRET_OFFSET:
      begin
        P := @PIDL.mkid.abID[StrRet.uOffset - SizeOf(PIDL.mkid.cb)];
        SetString(Result, P, PIDL.mkid.cb - StrRet.uOffset);
      end;
    STRRET_WSTR:
      Result := StrRet.pOleStr;
  end;
end;
{$ENDIF}

{ TShellExecParams }
constructor TShellExecParams.Create;
begin
  inherited Create;
  FShowCommand := soShow;
end;

procedure TShellExecParams.Assign(Source: TPersistent);
begin
  if Source is TShellExecParams then
    with TShellExecParams(Source) do
    begin
      Self.FhWnd := FhWnd;
      Self.FDirectory := FDirectory;
      Self.FParameters := FParameters;
      Self.FOperation := FOperation;
      Self.FFileName := FFileName;
      Self.FShowCommand := FShowCommand;
    end
  else
    inherited;
end;

function TShellExecParams.Execute: HINST;
begin
  Result := ShellExecute(hWnd, PChar(Operation), PChar(FileName), PChar(Parameters),
    PChar(Directory), ShowCmd);
end;

function TShellExecParams.ShowCmd: Integer;
begin
  Result := ord(FShowCommand);
end;

end.
