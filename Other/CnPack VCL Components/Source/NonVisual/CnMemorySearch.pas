{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     �й����Լ��Ŀ���Դ�������������                         }
{                   (C)Copyright 2001-2020 CnPack ������                       }
{                   ------------------------------------                       }
{                                                                              }
{            ���������ǿ�Դ��������������������� CnPack �ķ���Э������        }
{        �ĺ����·�����һ����                                                }
{                                                                              }
{            ������һ��������Ŀ����ϣ�������ã���û���κε���������û��        }
{        �ʺ��ض�Ŀ�Ķ������ĵ���������ϸ���������� CnPack ����Э�顣        }
{                                                                              }
{            ��Ӧ���Ѿ��Ϳ�����һ���յ�һ�� CnPack ����Э��ĸ��������        }
{        ��û�У��ɷ������ǵ���վ��                                            }
{                                                                              }
{            ��վ��ַ��http://www.cnpack.org                                   }
{            �����ʼ���master@cnpack.org                                       }
{                                                                              }
{******************************************************************************}

unit CnMemorySearch;
{* |<PRE>
================================================================================
* ������ƣ������ӹ��������
* ��Ԫ���ƣ�������������������ʵ�ֵ�Ԫ
* ��Ԫ���ߣ�CodeGame
* ��    ע���������������ļ������ڴ�������������ַ�������Ϣ��һ�ȶ��ڴ档
* ��    ע����֪���⣺����ģ��ʱֻ�ѱ�����ģ�飬δ�ܱ����������̵�ģ��
* ����ƽ̨��PWinXP + Delphi 2007
* ���ݲ��ԣ�����
* �޸ļ�¼��2013.09.02 v1.0
*               ��ֲ��Ԫ
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Classes, SysUtils, Tlhelp32, Windows, CnNativeDecl;

type
  TSearchMethodList = (smlSearchMemory, smlSearchFile); //��������
  TErrList = (elFileErr, elModuleErr); //��������

  TModuleInfo = packed record
    CodeHeadSize: Integer;
    CodeStartAddr: DWORD;
    CodeSize: Integer;
    CodeBaseAddr: DWORD;
  end;

  TResultType = (rtPointer, rtPointerData, rtPointerCall); //������������
  TtagData= array[0..15] of Byte; {��־���� 16byte}
  PModuleTag = ^TModuleTag;
  TModuleTag = packed record
    TagData:TtagData;{��־���� 16byte}
    Offset: Integer; {ƫ�Ƶ�ַ}
    Len: Integer; {��־����}
  end;

  PDataItem = ^TDataItem;
  TDataItem = packed record
    FileName: string[255];
    ConstStr: string[255];
    ModuleTag: array[0..4] of TModuleTag; {5���־}
    TagCount: Integer; {ModuleTag��־����}
    FileOffset: DWORD; {�ļ�ƫ�Ƶ�ַ}
    MemoryOffset: DWORD; {�ڴ�ƫ�Ƶ�ַ}
    ResultType: TResultType; {������������ 0:����PointerData, 1:����PointerCall}
    ResultData: DWORD; {���ص��ڴ�����}
  end;

  TDataItemList = array of TDataItem;

  TSearchFindEvent = procedure(const Index: Integer; pData: TDataItem) of object;

  TSearchCompleteEvent = procedure(const Status: Boolean) of object;

  TSearchErrEvent = procedure(const Status: TErrList; Msg: string) of object;

  TCnMemorySearchThread = class;

  TCnMemorySearch = class(TComponent)
  private
    FSearchCallTH: TCnMemorySearchThread;
    FStartSearch: Boolean;
    FDirectory: string;
    FDataList: TDataItemList;
    FSearchMethod: TSearchMethodList;
    FSearchFind: TSearchFindEvent;
    FSearchComplete: TSearchCompleteEvent;
    FSearchErrEvent: TSearchErrEvent;
    procedure SetStartSearch(const Val: Boolean);
  protected
    procedure DoSearchFindEvent(const Index: Integer; pData: TDataItem);
    procedure DoSearchCompleteEvent(const Status: Boolean);
    procedure DoErrEvent(const Status: TErrList; Msg: string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property DataList: TDataItemList read FDataList;
    procedure SetCount(const Count: Integer);
    function GetCount: Integer;
    property StartSearch: Boolean read FStartSearch write SetStartSearch;
  published
    property Directory: string read FDirectory write FDirectory;
    property SearchMethod: TSearchMethodList read FSearchMethod write FSearchMethod;
    property OnSearchFind: TSearchFindEvent read FSearchFind write FSearchFind;
    property OnSearchComplete: TSearchCompleteEvent read FSearchComplete write FSearchComplete;
    property OnSearchError: TSearchErrEvent read FSearchErrEvent write FSearchErrEvent;
  end;

  TCnMemorySearchThread = class(TThread)
  private
    FOwner: TCnMemorySearch;
    function GetModuleInfo(const aModuleName: string): TModuleInfo;
    function GetFileInfo(const aFileStream: TMemoryStream): TModuleInfo;
    function MemorySearch: Boolean;
    function FileSearch: Boolean;
  protected
    procedure Execute; override;
  public
    constructor Create(Suspended: Boolean; AOwner: TCnMemorySearch);
    destructor Destroy; override;
  end;

implementation

{ TCnMemorySearchThread }

procedure TCnMemorySearchThread.Execute;
var
  _SearchRet: Boolean;
begin
  { Place thread code here }
  _SearchRet := False;
  while not Terminated do
  begin
    Sleep(1);
    if not FOwner.FStartSearch then
      Continue;

    try
      case FOwner.SearchMethod of
        smlSearchMemory: _SearchRet := Self.MemorySearch; //�����ڴ�ģ��ģʽ
        smlSearchFile: _SearchRet := Self.FileSearch; //�����ļ�ģʽ
      end;
    finally
      FOwner.DoSearchCompleteEvent(_SearchRet);
      Self.Terminate; //�������
      FOwner.FStartSearch := False;
    end;
  end;
end;

function TCnMemorySearchThread.GetFileInfo(const aFileStream: TMemoryStream): TModuleInfo;
var
  _DosHead: IMAGE_DOS_HEADER;
  _NtHead: IMAGE_NT_HEADERS;
begin
  Result.CodeHeadSize := 0;
  Result.CodeStartAddr := 0;
  Result.CodeSize := 0;
  if not Assigned(aFileStream) then
    Exit;
  if aFileStream.Size < 0 then
    Exit;

  CopyMemory(@_DosHead, aFileStream.Memory, Sizeof(_DosHead));
  if _DosHead.e_magic = IMAGE_DOS_SIGNATURE then
  begin
    CopyMemory(@_NtHead, Pointer(DWORD(aFileStream.Memory) + _DosHead._lfanew), SizeOf(_NtHead));
    Result.CodeHeadSize := _NtHead.OptionalHeader.SizeOfHeaders;
    Result.CodeStartAddr := _NtHead.OptionalHeader.SizeOfHeaders + 1;
    Result.CodeSize := _NtHead.OptionalHeader.SizeOfCode;
    Result.CodeBaseAddr := _NtHead.OptionalHeader.ImageBase + _NtHead.OptionalHeader.BaseOfCode;
  end;
end;

function TCnMemorySearchThread.GetModuleInfo(const aModuleName: string): TModuleInfo;
var
  _ModuleSnap: Cardinal;
  _PId: DWORD;
  _Me32: MODULEENTRY32;
  _Handle: THandle;
  _lpr: TCnNativeUInt;
  _DosHead: IMAGE_DOS_HEADER;
  _NtHead: IMAGE_NT_HEADERS;
  LowModuleName: string;
begin
  Result.CodeHeadSize := 0;
  Result.CodeStartAddr := 0;
  Result.CodeSize := 0;
  _PId := 0;
  _ModuleSnap := CreateToolhelp32Snapshot(TH32CS_SNAPMODULE, _PId);
  if (_ModuleSnap <> INVALID_HANDLE_VALUE) then
  begin
    ZeroMemory(@_Me32, sizeof(MODULEENTRY32));
    _Me32.dwSize := sizeof(MODULEENTRY32);
    if (Module32First(_ModuleSnap, _Me32)) then
    begin
      LowModuleName := LowerCase(aModuleName);
      repeat
        if LowerCase(_Me32.szModule) = LowModuleName then
        begin
          _Handle := OpenProcess(PROCESS_VM_READ, True, GetCurrentProcessID);

          ReadProcessMemory(_Handle,
            _Me32.modBaseAddr,
            @_DosHead, Sizeof(_DosHead),
            _lpr);

          if _DosHead.e_magic = IMAGE_DOS_SIGNATURE then
          begin

            ReadProcessMemory(_Handle,
              Pointer(DWORD(_Me32.modBaseAddr) + _DosHead._lfanew),
              @_NtHead, SizeOf(_NtHead),
              _lpr);

            Result.CodeHeadSize := _NtHead.OptionalHeader.SizeOfHeaders;
            Result.CodeStartAddr := _NtHead.OptionalHeader.ImageBase + _NtHead.OptionalHeader.BaseOfCode;
            Result.CodeSize := _NtHead.OptionalHeader.BaseOfData;
          end;
          Break;
        end;
      until (Module32Next(_ModuleSnap, _Me32) = False)
    end;
  end;
end;

function TCnMemorySearchThread.MemorySearch: Boolean;
var
  _ItemIndex, _CodePosition, _TagIndex, _FindCount: Integer;
  _ModuleInfo: TModuleInfo;
  _FileName: string;
begin
  Result := False;
  if FOwner.FDataList = nil then
    Exit;
  if FOwner.GetCount < 1 then
    Exit;

  for _ItemIndex := 0 to FOwner.GetCount - 1 do //ö�����д������ݱ�������
  begin
    _FileName := FOwner.FDataList[_ItemIndex].FileName; //�ڴ���������Ҫȫ·����
    _ModuleInfo := GetModuleInfo(_FileName); //ȡ��ģ����Ϣ
    if (_ModuleInfo.CodeStartAddr <= 0) or (_ModuleInfo.CodeSize <= 0) then
    begin
      FOwner.DoErrEvent(elModuleErr, Format('%s,ģ��򿪴���!', [FOwner.FDataList[_ItemIndex].FileName]));
      Continue; //��һ������
    end;

    for _CodePosition := 0 to _ModuleInfo.CodeSize - 1 do //���ļ���ʼ����
    begin
      _FindCount := 0; //�鵽��־����
      for _TagIndex := 0 to FOwner.FDataList[_ItemIndex].TagCount - 1 do
      begin
        if CompareMem(Pointer(_ModuleInfo.CodeStartAddr + FOwner.FDataList[_ItemIndex].ModuleTag[_TagIndex].Offset + _CodePosition),
          @FOwner.FDataList[_ItemIndex].ModuleTag[_TagIndex], FOwner.FDataList[_ItemIndex].ModuleTag[_TagIndex].Len) then Inc(_FindCount);
      end;

      if _FindCount = FOwner.FDataList[_ItemIndex].TagCount then
      begin
        FOwner.FDataList[_ItemIndex].MemoryOffset := _ModuleInfo.CodeStartAddr + _CodePosition;
        FOwner.FDataList[_ItemIndex].FileOffset := FOwner.FDataList[_ItemIndex].MemoryOffset - _ModuleInfo.CodeStartAddr + _ModuleInfo.CodeHeadSize; //�ļ���ַ
        case FOwner.FDataList[_ItemIndex].ResultType of
          rtPointer: FOwner.FDataList[_ItemIndex].ResultData := FOwner.FDataList[_ItemIndex].MemoryOffset;
          rtPointerData: FOwner.FDataList[_ItemIndex].ResultData := PDword(FOwner.FDataList[_ItemIndex].MemoryOffset)^;
          rtPointerCall: FOwner.FDataList[_ItemIndex].ResultData := FOwner.FDataList[_ItemIndex].MemoryOffset + PInteger(FOwner.FDataList[_ItemIndex].MemoryOffset)^ + 4;
        end;
        FOwner.DoSearchFindEvent(_ItemIndex, FOwner.FDataList[_ItemIndex]);
        Break; //��������
      end;
    end;
  end;
  Result := True;
end;

function TCnMemorySearchThread.FileSearch: Boolean;
var
  _ItemIndex, _CodePosition, _TagIndex, _FindCount: Integer;
  _MemoryFile: TMemoryStream;
  _ModuleInfo: TModuleInfo;
  _FileName: string;
begin
  Result := False;
  if FOwner.FDataList = nil then
    Exit;
  if FOwner.GetCount < 1 then
    Exit;

  _MemoryFile := TMemoryStream.Create;
  try
    for _ItemIndex := 0 to FOwner.GetCount - 1 do //ö�����д������ݱ�������
    begin
      _FileName := FOwner.Directory + FOwner.FDataList[_ItemIndex].FileName;
      if not FileExists(_FileName) then //�ļ�������
      begin
        FOwner.DoErrEvent(elFileErr, Format('%s,�ļ��򿪴���!', [FOwner.FDataList[_ItemIndex].FileName]));
        Continue; //��һ������
      end;
      _MemoryFile.LoadFromFile(_FileName);
      _ModuleInfo := GetFileInfo(_MemoryFile); //ȡ��ģ����Ϣ
      if (_ModuleInfo.CodeSize = 0) or (_ModuleInfo.CodeStartAddr = 0) then
      begin
        FOwner.DoErrEvent(elFileErr, Format('%s,�ļ���ʽ����!', [FOwner.FDataList[_ItemIndex].FileName]));
        Continue; //��һ������
      end;
      _MemoryFile.Position := 0;

      for _CodePosition := 0 to _ModuleInfo.CodeSize - 1 do //���ļ���ʼ����
      begin
        _FindCount := 0; //�鵽��־����
        for _TagIndex := 0 to FOwner.FDataList[_ItemIndex].TagCount - 1 do
        begin
          if CompareMem(Pointer(DWORD(_MemoryFile.Memory) + _ModuleInfo.CodeStartAddr + FOwner.FDataList[_ItemIndex].ModuleTag[_TagIndex].Offset + _CodePosition),
            @FOwner.FDataList[_ItemIndex].ModuleTag[_TagIndex], FOwner.FDataList[_ItemIndex].ModuleTag[_TagIndex].Len) then Inc(_FindCount);
        end;

        if _FindCount = FOwner.FDataList[_ItemIndex].TagCount then
        begin
          FOwner.FDataList[_ItemIndex].FileOffset := _ModuleInfo.CodeStartAddr + _CodePosition;
          FOwner.FDataList[_ItemIndex].MemoryOffset := FOwner.FDataList[_ItemIndex].FileOffset + _ModuleInfo.CodeBaseAddr - _ModuleInfo.CodeHeadSize;

          case FOwner.FDataList[_ItemIndex].ResultType of
            rtPointer: FOwner.FDataList[_ItemIndex].ResultData := FOwner.FDataList[_ItemIndex].MemoryOffset;
            rtPointerData: FOwner.FDataList[_ItemIndex].ResultData := PDWord(FOwner.FDataList[_ItemIndex].FileOffset + DWORD(_MemoryFile.Memory))^;
            rtPointerCall: FOwner.FDataList[_ItemIndex].ResultData := FOwner.FDataList[_ItemIndex].MemoryOffset + PInteger(FOwner.FDataList[_ItemIndex].FileOffset + DWORD(_MemoryFile.Memory))^ + 4;
          end;
          FOwner.DoSearchFindEvent(_ItemIndex, FOwner.FDataList[_ItemIndex]);
          Break; //��������
        end;
      end;
    end;

    Result := True;
  finally
    FreeAndNil(_MemoryFile);
  end;

end;

constructor TCnMemorySearchThread.Create(Suspended: Boolean; AOwner: TCnMemorySearch);
begin
  inherited Create(Suspended);
  FreeOnTerminate := True;
  FOwner := AOwner;
end;

destructor TCnMemorySearchThread.Destroy;
begin
  {.....}
  FOwner := nil;
  inherited Destroy;
end;

{ TCnMemorySearch }

constructor TCnMemorySearch.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

end;

destructor TCnMemorySearch.Destroy;
begin

  inherited Destroy;
end;

procedure TCnMemorySearch.DoErrEvent(const Status: TErrList; Msg: string);
begin
  if Assigned(FSearchErrEvent) then
  try
    FSearchErrEvent(Status, Msg);
  except
    ;
  end;
end;

procedure TCnMemorySearch.DoSearchCompleteEvent(const Status: Boolean);
begin
  if Assigned(FSearchComplete) then
  try
    FSearchComplete(Status);
  except
    ;
  end;
end;

procedure TCnMemorySearch.DoSearchFindEvent(const Index: Integer; pData: TDataItem);
begin
  if Assigned(FSearchFind) then
  try
    FSearchFind(Index, pData);
  except
    ;
  end;
end;

function TCnMemorySearch.GetCount: Integer;
begin
  Result := Length(FDataList);
end;

procedure TCnMemorySearch.SetCount(const Count: Integer);
begin
  SetLength(FDataList, Count);
end;

procedure TCnMemorySearch.SetStartSearch(const Val: Boolean);
begin
//  if FStartSearch = Val then Exit;
  if Val then
  begin
    if Assigned(FSearchCallTH) then
      FSearchCallTH.Terminate;
    FSearchCallTH := TCnMemorySearchThread.Create(False, Self);
  end
  else
  begin
    if Assigned(FSearchCallTH) then
      FSearchCallTH.Terminate;
    FSearchCallTH := nil;
  end;
  FStartSearch := Val;
end;

end.

