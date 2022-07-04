{ Copyright (C) 1998-2008, written by Mike Shkolnik, Scalabium Software
  E-Mail:  mshkolnik@scalabium.com
           mshkolnik@yahoo.com
  WEB: http://www.scalabium.com
}
unit SMCVersInfo;

interface

uses Classes;

const
  VI_MAJOR_VERSION = 1;
  VI_MINOR_VERSION = 2;
  VI_RELEASE       = 3;
  VI_BUILD         = 4;

  VI_COMPANY_NAME      = 1;
  VI_FILE_DESCRIPTION  = 2;
  VI_FILE_VERSION      = 3;
  VI_INTERNAL_NAME     = 4;
  VI_LEGAL_COPYRIGHT   = 5;
  VI_ORIGINAL_FILENAME = 6;
  VI_PRODUCT_NAME      = 7;
  VI_PRODUCT_VERSION   = 8;
  VI_COMMENTS          = 9;
  VI_LEGAL_TRADEMARKS  = 10;

type
  TSMVersionInfo = class(TComponent)
  private
    FFileName: string;
    iDataSize: Integer;
    pData: Pointer;

    function iGetVersionInfo(Index: Integer): Integer;
    function sGetVersionInfo(Index: Integer): string;
    function GetVersionDateTime: TDateTime;
    procedure SetFileName(Value: string);
  public
    constructor CreateFile(AFileName: string);
    destructor Destroy; override;
    function GetVersionString(Key: string): string;
  published
    property FileName: string read FFileName write SetFileName;
    property MajorVersion: Integer index VI_MAJOR_VERSION read iGetVersionInfo;
    property MinorVersion: Integer index VI_MINOR_VERSION read iGetVersionInfo;
    property Release: Integer index VI_RELEASE read iGetVersionInfo;
    property Build: Integer index VI_BUILD read iGetVersionInfo;

    property DateTime: TDateTime read GetVersionDateTime;

    property CompanyName: string index VI_COMPANY_NAME read sGetVersionInfo;
    property FileDescription: string index VI_FILE_DESCRIPTION read sGetVersionInfo;
    property FileVersion: string index VI_FILE_VERSION read sGetVersionInfo;
    property InternalName: string index VI_INTERNAL_NAME read sGetVersionInfo;
    property LegalCopyright: string index VI_LEGAL_COPYRIGHT read sGetVersionInfo;
    property OriginalFilename: string index VI_ORIGINAL_FILENAME read sGetVersionInfo;
    property ProductName: string index VI_PRODUCT_NAME read sGetVersionInfo;
    property ProductVersion: string index VI_PRODUCT_VERSION read sGetVersionInfo;
    property Comments: string index VI_COMMENTS read sGetVersionInfo;
    property LegalTrademarks: string index VI_LEGAL_TRADEMARKS read sGetVersionInfo;
  end;

procedure Register;

implementation

uses Windows, SysUtils;

procedure Register;
begin
  RegisterComponents('SMComponents', [TSMVersionInfo]);
end;

{ TSMVersionInfo }
constructor TSMVersionInfo.CreateFile(AFileName: string);
begin
  inherited;

  FileName := AFileName;
end;

destructor TSMVersionInfo.Destroy;
begin
  if iDataSize > 0 then
    FreeMem(pData, iDataSize);

  inherited;
end;

procedure TSMVersionInfo.SetFileName(Value: string);
var
  lpdwHandle: dWord;
begin
  if (FFileName <> Value) then
  begin
    FFileName := Value;

    if (Value <> '') then
    begin
      iDataSize := GetFileVersionInfoSize(PChar(FileName), lpdwHandle);
      if iDataSize > 0 then
      begin
        GetMem(pData, iDataSize);
        Win32Check(GetFileVersionInfo(PChar(FileName), 0, iDataSize, pData));
      end
    end;
  end;
end;

function TSMVersionInfo.iGetVersionInfo(Index: Integer): Integer;
var
  FixedFileInfo: PVSFixedFileInfo;
  lpdwHandle: dWord;
begin
  Result := -1;
  if iDataSize > 0 then
  begin
    VerQueryValue(pData, '\', Pointer(FixedFileInfo), lpdwHandle);
    with FixedFileInfo^ do
      case Index of
        VI_MAJOR_VERSION: Result := HiWord(dwFileVersionMS);
        VI_MINOR_VERSION: Result := LoWord(dwFileVersionMS);
        VI_RELEASE: Result := HiWord(dwFileVersionLS);
        VI_BUILD: Result := LoWord(dwFileVersionLS);
      end;
  end;
end;

function TSMVersionInfo.GetVersionString(Key: string): string;
var
  lpdwHandle: dWord;
  P: Pointer;
  S: string;
  Buffer: PChar;
begin
  Result := '';
  if iDataSize > 0 then
  begin
    VerQueryValue(pData, '\VarFileInfo\Translation', P, lpdwHandle);
    S := Format('\StringFileInfo\%.4x%.4x\%s',
                [LoWord(Integer(P^)), HiWord(Integer(P^)), Key]);
    if VerQueryValue(pData, PChar(S), Pointer(Buffer), lpdwHandle) then
      Result := StrPas(Buffer);
  end;
end;

function TSMVersionInfo.GetVersionDateTime: TDateTime;
var
  FixedFileInfo: PVSFixedFileInfo;
  lpdwHandle: dWord;
  FileTime: TFileTime;
  SystemTime: TSystemTime;
begin
  Result := 0;
  if iDataSize > 0 then
  begin
    VerQueryValue(pData, '\', Pointer(FixedFileInfo), lpdwHandle);
    with FixedFileInfo^ do
    begin
      FileTime.dwLowDateTime := dwFileDateLS;
      FileTime.dwHighDateTime := dwFileDateMS;
      FileTimeToSystemTime(FileTime, SystemTime);
      with SystemTime do
        Result := EncodeDate(wYear, wMonth, wDay) +
                  EncodeTime(wHour, wMinute, wSecond, wMilliSeconds);
    end;
  end;
end;

function TSMVersionInfo.sGetVersionInfo(Index: Integer): string;
var
  KeyName: string;
begin
  Result := '';
  case Index of
    VI_COMPANY_NAME: KeyName := 'CompanyName';
    VI_FILE_DESCRIPTION: KeyName := 'FileDescription';
    VI_FILE_VERSION: KeyName := 'FileVersion';
    VI_INTERNAL_NAME: KeyName := 'InternalName';
    VI_LEGAL_COPYRIGHT: KeyName := 'LegalCopyright';
    VI_ORIGINAL_FILENAME: KeyName := 'OriginalFilename';
    VI_PRODUCT_NAME: KeyName := 'ProductName';
    VI_PRODUCT_VERSION: KeyName := 'ProductVersion';
    VI_COMMENTS: KeyName := 'Comments';
    VI_LEGAL_TRADEMARKS: KeyName := 'LegalTrademarks';
  end;
  Result := GetVersionString(KeyName);
end;

end.
