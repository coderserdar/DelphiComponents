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

unit CnZip;
{* |<PRE>
================================================================================
* ������ƣ�CnPack �����
* ��Ԫ���ƣ�CnPack ����� Zip ʵ�ֵ�Ԫ
* ��Ԫ���ߣ�CnPack������ Liu Xiao
* ��    ע��ʹ�� Delphi �Դ��� Zlib ʵ��ѹ����ѹ�봫ͳ����֧�֡�
*           �� XE2 ���ϵ� Zlib ��֧�� WindowBits �������ż��ݴ�ͳ�� ZIP ���
* ����ƽ̨��PWinXP + Delphi 5
* ���ݲ��ԣ�PWinXP/7 + Delphi 5 ~ XE
* �� �� �����õ�Ԫ�е��ַ��������ϱ��ػ�����ʽ
* �޸ļ�¼��2018.08.26 V1.3
*                �洢/Deflate ģʽ��֧�� Zip ��ͳ������ѹ����ѹ���㷨
*           2018.08.22 V1.2
*                �洢ģʽ��֧�� Zip ��ͳ������ѹ����ѹ���㷨���� Deflate ģʽ�Բ�֧������
*           2018.08.07 V1.1
*                ʹ�� ZLib ʵ�ּ�ѹ��ѹ�� XE2 ������ Zip ��ѹʱ�������⣬XE2 �����Ͽɼ��� Zip
*           2018.08.05 V1.0
*                ������Ԫ
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

{$IFNDEF SUPPORT_ZLIB_WINDOWBITS}
//  {$MESSAGE WARN 'NOT Compatable with WinZip/WinRAR etc.'}
{$ENDIF}

// {$DEFINE DEBUGZIP}

uses
  SysUtils, Classes, Windows, Contnrs, FileCtrl, CnCRC32, ZLib
  {$IFNDEF DISABLE_DIRECTORY_SUPPORT}, CnCommon {$ELSE}
  {$IFNDEF COMPILER6_UP} , CnCommon {$ENDIF} {$ENDIF};
  // D5 ����Ҫ�õ� CnCommon ��Ԫ�� UTF8 ֧��

type
  ECnZipException = class(Exception);

  TCnZipCompressionMethod = (
    zcStored, zcShrunk, zcReduce1, zcReduce2, zcReduce3, zcReduce4, zcImplode,
    zcTokenize, zcDeflate, zcDeflate64, zcPKImplode, zcReserved11, zcBZIP2,
    zcReserved13, zcLZMA, zcReserved15, zcReserved16, zcReserved17, zcTERSE,
    zcLZ77
//    zcWavePack = 97,
//    zcPPMdI1
  );

  TCnZipHeader = packed record
    MadeByVersion:      Word;     // Start of Central Header
    RequiredVersion:    Word;     // Start of Local Header
    Flag:               Word;
    CompressionMethod:  Word;
    ModifiedDateTime:   LongWord;
    CRC32:              LongWord;
    CompressedSize:     LongWord;
    UncompressedSize:   LongWord;
    FileNameLength:     Word;
    ExtraFieldLength:   Word;     // End of Local Header
    FileCommentLength:  Word;
    DiskNumberStart:    Word;
    InternalAttributes: Word;
    ExternalAttributes: LongWord;
    LocalHeaderOffset:  LongWord; // End of Central Header
    FileName:           AnsiString;
    ExtraField:         AnsiString;
    FileComment:        AnsiString;
  end;
  PCnZipHeader = ^TCnZipHeader;

  TCnZipEndOfCentralHeader = packed record
    DiskNumber:          Word;
    CentralDirStartDisk: Word;
    NumEntriesThisDisk:  Word;
    CentralDirEntries:   Word;
    CentralDirSize:      LongWord;
    CentralDirOffset:    LongWord;
    CommentLength:       Word;
    {Comment: RawByteString}
  end;
  PCnZipEndOfCentralHeader = ^TCnZipEndOfCentralHeader;

  TCnZipBase = class(TObject)
  {* Zip ���������}
  private
    FUtf8: Boolean;
    FFileList: TList;
    FComment: AnsiString;
    FPassword: AnsiString;
    procedure SetUtf8(const Value: Boolean);
    function GetComment: string;
    function GetFileComment(Index: Integer): string;
    function GetFileCount: Integer;
    function GetFileInfo(Index: Integer): PCnZipHeader;
    function GetFileName(Index: Integer): string;
    procedure SetComment(const Value: string);
    procedure SetFileComment(Index: Integer; const Value: string);
  protected
    FStartFileData: Int64;
    FEndFileData: Int64;
    procedure ClearFiles;
    function RawToString(Raw: AnsiString): string;
    function StringToRaw(Str: string): AnsiString;
    function GetHasPassword: Boolean; virtual;
    procedure SetPassword(const Value: AnsiString); virtual;

    property Password: AnsiString read FPassword write SetPassword;
    {* �� Zip �ļ�������}
    property HasPassword: Boolean read GetHasPassword;
    {* �� Zip �ļ��Ƿ�������}
  public
    constructor Create; virtual;
    destructor Destroy; override;

    function IndexOf(const FileName: string): Integer;

    property FileCount: Integer read GetFileCount;
    {* �� Zip �ļ��������ļ�����}
    property FileName[Index: Integer]: string read GetFileName;
    {* �� Zip �ļ��������ļ���}
    property FileInfo[Index: Integer]: PCnZipHeader read GetFileInfo;
    {* �� Zip �ļ��������ļ���Ϣ��������Ŀ¼������}
    property FileComment[Index: Integer]: string read GetFileComment write SetFileComment;
    {* �� Zip �ļ��������ļ�ע��}
    property Comment: string read GetComment write SetComment;
    {* �� Zip �ļ�������ע��}
    property Utf8: Boolean read FUtf8 write SetUtf8;
    {* �� Zip �ļ��Ƿ�֧�� Utf8}
  end;

  TCnZipAbstractCompressionHandler = class(TObject)
  {* ѹ�����͵�ʵ�ֻ���}
  private

  public
    class function CanHandleCompressionMethod(AMethod: TCnZipCompressionMethod): Boolean; virtual; abstract;
    class function CreateCompressionStream(AMethod: TCnZipCompressionMethod;
      OutStream: TStream; const Item: PCnZipHeader; Zip: TCnZipBase): TStream; virtual; abstract;
    class function CreateDecompressionStream(AMethod: TCnZipCompressionMethod;
      InStream: TStream; const Item: PCnZipHeader; Zip: TCnZipBase): TStream; virtual; abstract;
  end;

  TCnZipCompressionHandlerClass = class of TCnZipAbstractCompressionHandler;

  TCnZipReader = class(TCnZipBase)
  {* ������ Zip �ļ��ɽ�ѹ�Ĺ�����}
  private
    FInStream: TStream;
    procedure OpenZipStream;
    procedure ReadCentralHeader;
    function PrepareStream(Index: Integer; LocalHeader: PCnZipHeader): TStream;
  protected
    function GetHasPassword: Boolean; override;
    function SearchEndOfCentralHeader(Stream: TStream;
      Header: PCnZipEndOfCentralHeader): Boolean;

    procedure SetPassword(const Value: AnsiString); override;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure OpenZipFile(const ZipFileName: string);
    {* ��һ�� Zip �ļ�}
    procedure ExtractAllTo(const Path: string);
    {* ���򿪵� Zip �ļ�ȫ����ѹ��ָ��Ŀ¼}
    procedure ExtractTo(Index: Integer; const Path: string; CreateSubdirs: Boolean = True);
    {* ��ѹָ����ŵĵ����ļ���ָ��Ŀ¼}
    procedure ExtractByFileName(const FileName: string; const Path: string; CreateSubdirs: Boolean = True);
    {* ��ѹָ���ļ���ָ��Ŀ¼}
    procedure Close;
    {* �رո� Zip �ļ�}

    property Password;
    {* ��ѹ�� Zip �ļ����������}
    property HasPassword;
    {* �� Zip �ļ�ͷ��ı�־�Ƿ���Ҫ������ܽ�ѹ}
  end;

  TCnZipWriter = class(TCnZipBase)
  {* �������� Zip �ļ��Ĺ�����}
  private
    FOutStream: TStream;
    FRemovePath: Boolean;
{$IFNDEF DISABLE_DIRECTORY_SUPPORT}
    FDirFiles: TStrings;
    procedure FindFileCallback(const FileName: string; const Info: TSearchRec;
      var Abort: Boolean);
{$ENDIF}
  protected
    procedure AddStream(Data: TStream; LocalHeader: PCnZipHeader);
  public
    destructor Destroy; override;

    procedure CreateZipFile(const ZipFileName: string);
    {* ����һ���հ׵� Zip �ļ�}
    procedure AddFile(const FileName: string; const ArchiveFileName: string = '';
      Compression: TCnZipCompressionMethod = zcDeflate);
    {* �� Zip �ļ������ָ�����ݣ�FileName Ϊ�����ļ���ArchiveFileName ΪҪд��
      Zip �ڲ����ļ���}
{$IFNDEF DISABLE_DIRECTORY_SUPPORT}
    procedure AddDirectory(const DirName: string; Compression: TCnZipCompressionMethod = zcDeflate);
    {* �� Zip �ļ������ָ��Ŀ¼�µ������ļ�}
{$ENDIF}    
    procedure Save;
    {* ��ѹ�����ݱ����� Zip �ļ�}
    procedure Close;
    {* �ر�ѹ���ļ�}

    property RemovePath: Boolean read FRemovePath write FRemovePath;
    {* �Ƿ�ȥ��ÿ���ļ���·����Ϣֻ���ļ�����Ϣ��ֻ�� AddFile �� ArchiveFileName
      Ϊ�յ��������Ч}

    property Password;
    {* ���ø� Zip �ļ���ѹ�����룬��������Ч}
    property HasPassword;
    {* �Ƿ����������룬�����ж� Password �����Ƿ�Ϊ��}
  end;

procedure RegisterCompressionHandlerClass(AClass: TCnZipCompressionHandlerClass);
{* ������ṩ���µ�ѹ����ʽ��֧��}

function CnZipFileIsValid(const FileName: string): Boolean;
{* �ж� Zip �ļ��Ƿ�Ϸ�}

{$IFNDEF DISABLE_DIRECTORY_SUPPORT}

function CnZipDirectory(const DirName: string; const FileName: string;
  Compression: TCnZipCompressionMethod = zcDeflate; const Password: string = ''): Boolean;
{* ��ָ��Ŀ¼ѹ��Ϊһ�� Zip �ļ�}

{$ENDIF}

function CnZipExtractTo(const FileName: string; const DirName: string;
  const Password: string = ''): Boolean;
{* ��ָ�� Zip �ļ���ѹ����ָ��Ŀ¼}

implementation

{$IFDEF DEBUGZIP}
uses
  CnDebug;
{$ENDIF}

const
  CN_SIGNATURE_ZIPENDOFHEADER: LongWord = $06054B50;
  CN_SIGNATURE_CENTRALHEADER:  LongWord = $02014B50;
  CN_SIGNATURE_LOCALHEADER:    LongWord = $04034B50;

  CN_KEY0_INIT: LongWord = 305419896;
  CN_KEY1_INIT: LongWord = 591751049;
  CN_KEY2_INIT: LongWord = 878082192;
  CN_KEY_UPDATE: LongWord = 134775813;

  CN_LOCAL_HEADERSIZE = 26;
  CN_CENTRAL_HEADERSIZE = 42;
  CN_UTF8_MASK = $0800;  // 1 shl 11
  CN_ZIP_CRYPT_HEAD_SIZE = 12;

resourcestring
  SZipErrorRead = 'Error Reading Zip File';
  SZipErrorWrite = 'Error Writing Zip File';
  SZipInvalidLocalHeader   = 'Invalid Zip Local Header';
  SZipInvalidCentralHeader = 'Invalid Zip Central Header';
  SFileNotFound = 'Error Finding File';
  SZipNotSupport = 'Zip Compression Method NOT Support';
  SZipInvalidPassword = 'Invalid Password';
  SZipNotImplemented = 'Feature NOT Implemented';
  SZipUtf8NotSupport = 'UTF8 NOT Support';

var
  FZipCompressionHandlers: TClassList = nil;

type
  TCnZipDefaultCompressionHandler = class(TCnZipAbstractCompressionHandler)
  {* Ĭ��ʵ���� Stored �� Deflate ģʽ��֧�ִ�ͳ�ӽ��ܵĴ�����}
  private

  public
    class function CanHandleCompressionMethod(AMethod: TCnZipCompressionMethod): Boolean; override;
    {* �Ƿ�֧���ض���ѹ������}
    class function CreateCompressionStream(AMethod: TCnZipCompressionMethod;
      OutStream: TStream; const Item: PCnZipHeader; Zip: TCnZipBase): TStream; override;
    {* ��������ض���������ѹ������ѹ�����ĸ����ǣ�ѹ�����и������������ѹ����д������ʱ��
      ���Զ���ѹ���������д�������������ѹ����Ҫʵ�� Write ����д���ģ��ڲ�ѹ�����ܺ�д�����}
    class function CreateDecompressionStream(AMethod: TCnZipCompressionMethod;
      InStream: TStream; const Item: PCnZipHeader; Zip: TCnZipBase): TStream; override;
    {* ��������ض��������Ľ�ѹ��������ѹ�����ĸ����ǣ���ѹ�����и������������ӽ�ѹ����������ʱ��
      ���Զ��ѽ�ѹ����������ṩ������ Buffer�����Խ�ѹ����Ҫʵ�� Read �����������ģ��ڲ���������������ѹ������֮���}
  end;

  TCnStoredStream = class(TStream)
  {* �洢��ʽ��ѹ�������ѹ����}
  private
    FStream: TStream;
  protected
    function GetSize: Int64; // override;
  public
    constructor Create(Stream: TStream);

    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
  end;

  TCnZipCryptKeys = class(TObject)
  {* ������ Zip ���������ܽ��ܵĹ�����}
  private
    FKey0, FKey1, FKey2: LongWord;
  protected
    function CalcDecryptByte: Byte;
  public
    procedure InitKeys(const Password: AnsiString);
    procedure UpdateKeys(C: Byte);

    procedure DecryptByte(var Value: Byte);
    procedure EncryptByte(var Value: Byte);
  end;

  TCnEncryptStoredStream = class(TStream)
  {* �洢Ҳ���Ƿ�ѹ���ļ�������ʵ�֣�Write ʱ����}
  private
    FKeys: TCnZipCryptKeys;
    FPassword: AnsiString;
    FOutStream: TStream;
    FZipHeader: PCnZipHeader;
  protected

  public
    constructor Create(OutStream: TStream; const APassword: AnsiString; const AZipHeader: PCnZipHeader);
    destructor Destroy; override;

    function Read(var Buffer; Count: Integer): Integer; override;    // ������ʵ��
    function Seek(Offset: Longint; Origin: Word): Longint; override; // ������ʵ��
    function Write(const Buffer; Count: Integer): Integer; override;
  end;

  TCnDecryptStoredStream = class(TStream)
  {* �洢Ҳ���Ƿ�ѹ���Ľ�������ʵ�֣�Read ʱ����}
  private
    FZip: TStream;
    FKeys: TCnZipCryptKeys;
    FPassword: AnsiString;
    FStream: TStream;
    FZipHeader: PCnZipHeader;
  protected

  public
    constructor Create(AStream: TStream; const APassword: AnsiString;
      const AZipHeader: PCnZipHeader);
    destructor Destroy; override;

    function Read(var Buffer; Count: Integer): Integer; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override; // ������ʵ��
    function Write(const Buffer; Count: Integer): Integer; override; // ������ʵ��
  end;

  TCnEncryptZipCompressStream = class(TStream)
  {* Deflate ѹ�����ұ�׼ Zip ���ܵ�ѹ����ʵ�֣�ʹ���ڴ���ѹ���������ļ����ܻ� OOM
    ʵ��˳�������д��ʱ��ѹ���� FZipped������ʱ�ٽ� FZipped ���ֽڼ���д��}
  private
    FZip: TStream;
    FZipped: TMemoryStream; // �洢ѹ����δ���ܵ���ʱ����
    FKeys: TCnZipCryptKeys;
    FPassword: AnsiString;
    FOutStream: TStream;
    FZipHeader: PCnZipHeader;
  public
    constructor Create(OutStream: TStream; const APassword: AnsiString;
      const AZipHeader: PCnZipHeader);
    destructor Destroy; override;

    function Read(var Buffer; Count: Integer): Integer; override;    // ������ʵ��
    function Seek(Offset: Longint; Origin: Word): Longint; override; // ������ʵ��
    function Write(const Buffer; Count: Integer): Integer; override;
  end;

  TCnDecryptZipCompressStream = class(TStream)
  {* Deflate ѹ�����ұ�׼ Zip ���ܵĽ�ѹ����ʵ�֣�ʹ���ڴ�����ѹ���������ļ����ܻ� OOM
    ʵ��˳���ڴ���ʱ�Ƚ��������� FDecrypted������ȡʱ�ٽ� FDecrypted ��ѹ��������}
  private
    FUnzip: TStream;
    FDecrypted: TMemoryStream; // �洢Ԥ�����ֽڽ��ܳ�����ѹ������
    FKeys: TCnZipCryptKeys;
    FPassword: AnsiString;
    FInStream: TStream;
    FZipHeader: PCnZipHeader;
  public
    constructor Create(InStream: TStream; const APassword: AnsiString;
      const AZipHeader: PCnZipHeader);
    destructor Destroy; override;

    function Read(var Buffer; Count: Integer): Integer; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override; // ������ʵ��
    function Write(const Buffer; Count: Integer): Integer; override; // ������ʵ��
  end;

procedure RegisterCompressionHandlerClass(AClass: TCnZipCompressionHandlerClass);
begin
  if FZipCompressionHandlers.IndexOf(AClass) < 0 then
    FZipCompressionHandlers.Add(AClass);
end;

// �Ƿ�֧��ָ����ѹ����ʽ
function SupportCompressionMethod(AMethod: TCnZipCompressionMethod): Boolean;
var
  I: Integer;
  AComp: TCnZipCompressionHandlerClass;
begin
  Result := False;
  for I := 0 to FZipCompressionHandlers.Count - 1 do
  begin
    AComp := TCnZipCompressionHandlerClass(FZipCompressionHandlers[I]);
    if AComp <> nil then
    begin
      if AComp.CanHandleCompressionMethod(AMethod) then
      begin
        Result := True;
        Exit;
      end;
    end;
  end;
end;

function CreateCompressStreamFromHandler(AMethod: TCnZipCompressionMethod;
  OutStream: TStream; const Item: PCnZipHeader; Zip: TCnZipBase): TStream;
var
  I: Integer;
  AComp: TCnZipCompressionHandlerClass;
begin
  Result := nil;
  for I := 0 to FZipCompressionHandlers.Count - 1 do
  begin
    AComp := TCnZipCompressionHandlerClass(FZipCompressionHandlers[I]);
    if AComp <> nil then
    begin
      if AComp.CanHandleCompressionMethod(AMethod) then
      begin
        Result := AComp.CreateCompressionStream(AMethod, OutStream, Item, Zip);
        Exit;
      end;
    end;
  end;
end;

function CreateDecompressStreamFromHandler(AMethod: TCnZipCompressionMethod;
  InStream: TStream; const Item: PCnZipHeader; Zip: TCnZipBase): TStream;
var
  I: Integer;
  AComp: TCnZipCompressionHandlerClass;
begin
  Result := nil;
  for I := 0 to FZipCompressionHandlers.Count - 1 do
  begin
    AComp := TCnZipCompressionHandlerClass(FZipCompressionHandlers[I]);
    if AComp <> nil then
    begin
      if AComp.CanHandleCompressionMethod(AMethod) then
      begin
        Result := AComp.CreateDecompressionStream(AMethod, InStream, Item, Zip);
        Exit;
      end;
    end;
  end;
end;

function ZipUtf8ToString(const Text: AnsiString): string;
begin
{$IFDEF UNICODE}
  Result := UTF8ToUnicodeString(PAnsiChar(Text));
{$ELSE}
  {$IFDEF COMPILER6_UP}
  Result := Utf8ToAnsi(Text);
  {$ELSE}
  // raise ECnZipException.CreateRes(@SZipUtf8NotSupport);
  Result := AnsiString(CnUtf8DecodeToWideString(Text));
  {$ENDIF}
{$ENDIF}
end;

function ZipStringToUtf8(const Text: string): AnsiString;
begin
{$IFDEF UNICODE}
  Result := Utf8Encode(Text);
{$ELSE}
  {$IFDEF COMPILER6_UP}
  Result := AnsiToUtf8(Text);
  {$ELSE}
  // raise ECnZipException.CreateRes(@SZipUtf8NotSupport);
  Result := CnUtf8EncodeWideString(WideString(Text));
  {$ENDIF}
{$ENDIF}
end;

function CnZipFileIsValid(const FileName: string): Boolean;
var
  Z: TCnZipReader;
  Stream: TStream;
  Header: TCnZipEndOfCentralHeader;
begin
  Result := False;
  try
    Stream := nil;
    Z := nil;
    try
      Z := TCnZipReader.Create;
      Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
      Result := Z.SearchEndOfCentralHeader(Stream, @Header);
    finally
      Stream.Free;
      Z.Free;
    end;
  except on E: EStreamError do
    ;
  end;
end;

{$IFNDEF DISABLE_DIRECTORY_SUPPORT}

function CnZipDirectory(const DirName: string; const FileName: string;
  Compression: TCnZipCompressionMethod; const Password: string): Boolean;
var
  Zip: TCnZipWriter;
begin
  Result := False;
  if not DirectoryExists(DirName) then
    Exit;

  Zip := TCnZipWriter.Create;

  try
    Zip.CreateZipFile(FileName);
    Zip.AddDirectory(DirName);
    Zip.Save;
    Result := True;
  finally
    Zip.Free;
  end;
end;

{$ENDIF}

function CnZipExtractTo(const FileName: string; const DirName: string;
  const Password: string): Boolean;
var
  Zip: TCnZipReader;
begin
  Result := False;
  if not FileExists(FileName) then
    Exit;

  Zip := TCnZipReader.Create;
  try
    Zip.OpenZipFile(FileName);
    Zip.ExtractAllTo(DirName);
    Result := True;
  finally
    Zip.Free;
  end;
end;

procedure VerifyRead(Stream: TStream; var Buffer; Count: Integer);
begin
  if Stream.Read(Buffer, Count) <> Count then
    raise ECnZipException.CreateRes(@SZipErrorRead);
end;

procedure VerifyWrite(Stream: TStream; var Buffer; Count: Integer);
begin
  if Stream.Write(Buffer, Count) <> Count then
    raise ECnZipException.CreateRes(@SZipErrorWrite);
end;

{ TCnZipBase }

procedure TCnZipBase.ClearFiles;
var
  I: Integer;
begin
  for I := FFileList.Count - 1 downto 0 do
    Dispose(FFileList[I]);
  FFileList.Clear;
end;

constructor TCnZipBase.Create;
begin
  inherited;
  FFileList := TList.Create;
  FUtf8 := False;
end;

destructor TCnZipBase.Destroy;
begin
  ClearFiles;
  FFileList.Free;
  inherited;
end;

function TCnZipBase.GetComment: string;
begin
  Result := RawToString(FComment);
end;

function TCnZipBase.GetFileComment(Index: Integer): string;
begin
  Result := RawToString(FileInfo[Index]^.FileComment);
end;

function TCnZipBase.GetFileCount: Integer;
begin
  Result := FFileList.Count;
end;

function TCnZipBase.GetFileInfo(Index: Integer): PCnZipHeader;
begin
  Result := PCnZipHeader(FFileList[Index]);
end;

function TCnZipBase.GetFileName(Index: Integer): string;
begin
  Result := RawToString(FileInfo[Index]^.FileName);
end;

function TCnZipBase.GetHasPassword: Boolean;
begin
  Result := FPassword <> '';
end;

function TCnZipBase.IndexOf(const FileName: string): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to FFileList.Count - 1 do
  begin
    if SameText(RawToString(FileInfo[I].FileName), FileName) then
    begin
      Result := I;
      Exit;
    end;
  end;
end;

function TCnZipBase.RawToString(Raw: AnsiString): string;
begin
  if FUtf8 then
    Result := ZipUtf8ToString(Raw)
  else
    Result := string(Raw);
end;

procedure TCnZipBase.SetComment(const Value: string);
begin
  FComment := StringToRaw(Value);
end;

procedure TCnZipBase.SetFileComment(Index: Integer; const Value: string);
begin
  FileInfo[Index]^.FileComment := StringToRaw(Value);
end;

procedure TCnZipBase.SetPassword(const Value: AnsiString);
begin
  FPassword := Value;
end;

procedure TCnZipBase.SetUtf8(const Value: Boolean);
begin
  FUtf8 := Value;
end;

function TCnZipBase.StringToRaw(Str: string): AnsiString;
begin
  if FUtf8 then
    Result := ZipStringToUtf8(Str)
  else
    Result := AnsiString(Str);
end;

{ TCnZipReader }

procedure TCnZipReader.Close;
begin
  ClearFiles;
  FreeAndNil(FInStream);
end;

function TCnZipReader.PrepareStream(Index: Integer;
  LocalHeader: PCnZipHeader): TStream;
var
  Sig: LongWord;
begin
  if (Index < 0) or (Index > FileCount) then
    raise ECnZipException.CreateRes(@SFileNotFound);

  LocalHeader^.MadeByVersion := 0;
  SetLength(LocalHeader^.FileComment, 0);
  LocalHeader^.FileCommentLength  := 0;
  LocalHeader^.DiskNumberStart    := 0;
  LocalHeader^.InternalAttributes := 0;
  LocalHeader^.ExternalAttributes := 0;
  LocalHeader^.LocalHeaderOffset  := 0;

  FInStream.Position := FileInfo[Index].LocalHeaderOffset + FStartFileData;
  FInStream.Read(Sig, Sizeof(Sig));
  if Sig <> CN_SIGNATURE_LOCALHEADER then
    raise ECnZipException.CreateRes(@SZipInvalidLocalHeader);

  FInStream.Read(LocalHeader^.RequiredVersion,    Sizeof(Word));
  FInStream.Read(LocalHeader^.Flag,               Sizeof(Word));
  FInStream.Read(LocalHeader^.CompressionMethod,  Sizeof(Word));
  FInStream.Read(LocalHeader^.ModifiedDateTime,   Sizeof(LongWord));
  FInStream.Read(LocalHeader^.CRC32,              Sizeof(LongWord));
  FInStream.Read(LocalHeader^.CompressedSize,     Sizeof(LongWord));
  FInStream.Read(LocalHeader^.UncompressedSize,   Sizeof(LongWord));
  FInStream.Read(LocalHeader^.FileNameLength,     Sizeof(Word));
  FInStream.Read(LocalHeader^.ExtraFieldLength,   Sizeof(Word));

  SetLength(LocalHeader^.FileName, LocalHeader^.FileNameLength);
  FInStream.Read(LocalHeader^.FileName[1], LocalHeader^.FileNameLength);
  if LocalHeader^.ExtraFieldLength > 0 then
  begin
    SetLength(LocalHeader^.ExtraField, LocalHeader^.ExtraFieldLength);
    FInStream.Read(LocalHeader^.ExtraField[1], LocalHeader^.ExtraFieldLength);
  end;

{$IFDEF DEBUGZIP}
  CnDebugger.LogMsg('Reader: Dump a Local Header for FileName: ' + LocalHeader^.FileName);
  CnDebugger.LogFmt('  RequiredVersion: %4.4d', [LocalHeader^.RequiredVersion]);
  CnDebugger.LogFmt('  Flag: $%4.4x', [LocalHeader^.Flag]);
  CnDebugger.LogFmt('  CompressionMethod: %4.4d', [LocalHeader^.CompressionMethod]);
  CnDebugger.LogFmt('  ModifiedDateTime: $%8.8x', [LocalHeader^.ModifiedDateTime]);
  CnDebugger.LogFmt('  CRC32: $%8.8x', [LocalHeader^.CRC32]);
  CnDebugger.LogFmt('  CompressedSize: %d', [LocalHeader^.CompressedSize]);
  CnDebugger.LogFmt('  UncompressedSize: %d', [LocalHeader^.UncompressedSize]);
  CnDebugger.LogFmt('  FileNameLength: %d', [LocalHeader^.FileNameLength]);
  CnDebugger.LogFmt('  ExtraFieldLength: %d', [LocalHeader^.ExtraFieldLength]);
  CnDebugger.LogFmt('  FileCommentLength: %d', [LocalHeader^.FileCommentLength]);
  CnDebugger.LogFmt('  DiskNumberStart: %d', [LocalHeader^.DiskNumberStart]);
  CnDebugger.LogFmt('  InternalAttributes: %d', [LocalHeader^.InternalAttributes]);
  CnDebugger.LogFmt('  ExternalAttributes: %d', [LocalHeader^.ExternalAttributes]);
  CnDebugger.LogFmt('  LocalHeaderOffset:  %8.8x', [LocalHeader^.LocalHeaderOffset]);
{$ENDIF}

  Result := CreateDecompressStreamFromHandler(TCnZipCompressionMethod(LocalHeader^.CompressionMethod),
    FInStream, LocalHeader, Self);
end;

destructor TCnZipReader.Destroy;
begin
  FreeAndNil(FInStream);
  inherited;
end;

procedure TCnZipReader.ExtractAllTo(const Path: string);
var
  I: Integer;
begin
  for I := 0 to FFileList.Count - 1 do
    ExtractTo(I, Path);
end;

procedure TCnZipReader.ExtractByFileName(const FileName, Path: string;
  CreateSubdirs: Boolean);
begin
  ExtractTo(IndexOf(FileName), Path, CreateSubdirs);
end;

procedure TCnZipReader.ExtractTo(Index: Integer; const Path: string;
  CreateSubdirs: Boolean);
var
  CompressionStream, OutStream: TStream;
  LocalHeader: TCnZipHeader;
  Dir, AFileName: string;
begin
  CompressionStream := PrepareStream(Index, @LocalHeader);
  if CompressionStream = nil then
    raise ECnZipException.CreateRes(@SZipNotSupport);

  try
    AFileName := RawToString(FileInfo[Index].FileName);
    if AFileName = '' then
      Exit;

{$IFDEF MSWINDOWS}
    AFileName := StringReplace(AFileName, '/', '\', [rfReplaceAll]);
{$ENDIF}

    if CreateSubdirs then
      AFileName := IncludeTrailingBackslash(Path) + AFileName
    else
      AFileName := IncludeTrailingBackslash(Path) + ExtractFileName(AFileName);

    Dir := ExtractFileDir(AFileName);
    if CreateSubdirs and (Dir <> '') then
      ForceDirectories(Dir);

    if AFileName[Length(AFileName) - 1] in ['\', '/'] then
      Exit;

    OutStream := TFileStream.Create(AFileName, fmCreate);
    try
      if (LocalHeader.Flag and (1 shl 3)) = 0 then
      begin
        if FileInfo[Index].UncompressedSize > 0 then
          OutStream.CopyFrom(CompressionStream, FileInfo[Index].UncompressedSize);
      end
      else
      begin
        OutStream.CopyFrom(CompressionStream, FileInfo[Index].UncompressedSize);
      end;
    finally
      OutStream.Free;
    end;
  finally
    CompressionStream.Free;
  end;
end;

procedure TCnZipReader.OpenZipFile(const ZipFileName: string);
begin
  Close;

  FInStream := TFileStream.Create(ZipFileName, fmOpenRead or fmShareDenyWrite);
  try
    OpenZipStream;
  except
    FreeAndNil(FInStream);
    raise;
  end;
end;

procedure TCnZipReader.OpenZipStream;
begin
  FStartFileData := FInStream.Position;
  ReadCentralHeader;
end;

procedure TCnZipReader.ReadCentralHeader;
var
  I: Integer;
  Signature: LongWord;
  EndHeader: TCnZipEndOfCentralHeader;
  Header: PCnZipHeader;
begin
  ClearFiles;
  if FInStream.Size = 0 then
    Exit;

  if not SearchEndOfCentralHeader(FInStream, @EndHeader) then
    raise ECnZipException.CreateRes(@SZipErrorRead);

  FInStream.Position := EndHeader.CentralDirOffset;
  FEndFileData := EndHeader.CentralDirOffset;

  for I := 0 to EndHeader.CentralDirEntries - 1 do
  begin
    FInStream.Read(Signature, Sizeof(Signature));
    if Signature <> CN_SIGNATURE_CENTRALHEADER then
      raise ECnZipException.CreateRes(@SZipInvalidCentralHeader);

    New(Header);
    try
      VerifyRead(FInStream, Header^.MadeByVersion,      Sizeof(Word));
      VerifyRead(FInStream, Header^.RequiredVersion,    Sizeof(Word));
      VerifyRead(FInStream, Header^.Flag,               Sizeof(Word));
      VerifyRead(FInStream, Header^.CompressionMethod,  Sizeof(Word));
      VerifyRead(FInStream, Header^.ModifiedDateTime,   Sizeof(LongWord));
      VerifyRead(FInStream, Header^.CRC32,              Sizeof(LongWord));
      VerifyRead(FInStream, Header^.CompressedSize,     Sizeof(LongWord));
      VerifyRead(FInStream, Header^.UncompressedSize,   Sizeof(LongWord));
      VerifyRead(FInStream, Header^.FileNameLength,     Sizeof(Word));
      VerifyRead(FInStream, Header^.ExtraFieldLength,   Sizeof(Word));
      VerifyRead(FInStream, Header^.FileCommentLength,  Sizeof(Word));
      VerifyRead(FInStream, Header^.DiskNumberStart,    Sizeof(Word));
      VerifyRead(FInStream, Header^.InternalAttributes, Sizeof(Word));
      VerifyRead(FInStream, Header^.ExternalAttributes, Sizeof(LongWord));
      VerifyRead(FInStream, Header^.LocalHeaderOffset,  Sizeof(LongWord));

      if Header^.FileNameLength > 0 then
      begin
        SetLength(Header^.FileName, Header^.FileNameLength);
        VerifyRead(FInStream, Header^.FileName[1], Header^.FileNameLength);
      end;
      if Header^.ExtraFieldLength > 0 then
      begin
        SetLength(Header^.ExtraField, Header^.ExtraFieldLength);
        VerifyRead(FInStream, Header^.ExtraField[1], Header^.ExtraFieldLength);
      end;
      if Header^.FileCommentLength > 0 then
      begin
        SetLength(Header^.FileComment, Header^.FileCommentLength);
        VerifyRead(FInStream, Header^.FileComment[1], Header^.FileCommentLength);
      end;

      if (Header^.Flag and CN_UTF8_MASK) = 0 then
        FUtf8 := False;
    except
      Dispose(Header);
    end;
    FFileList.Add(Header);
  end;
end;

function TCnZipReader.SearchEndOfCentralHeader(Stream: TStream;
  Header: PCnZipEndOfCentralHeader): Boolean;
var
  I: Integer;
  BackRead, ReadSize, MaxBack: Longint;
  BackBuf: array of Byte;
begin
  if Stream.Size < $FFFF then
    MaxBack := Stream.Size
  else
    MaxBack := $FFFF;

  BackRead := 4;
  SetLength(BackBuf, $404 - 1);
  while BackRead < MaxBack do
  begin
    if BackRead + Longint(Length(BackBuf) - 4) > MaxBack then
      BackRead := MaxBack
    else
      Inc(BackRead, Length(BackBuf) - 4);

    Stream.Position := Stream.Size - BackRead;
    if Length(BackBuf) < (Stream.Size - Stream.Position) then
      ReadSize := Length(BackBuf)
    else
      ReadSize := Stream.Size - Stream.Position;

    VerifyRead(Stream, BackBuf[0], ReadSize);
    for I := ReadSize - 4 downto 0 do
    begin
      if (BackBuf[I]     = ((CN_SIGNATURE_ZIPENDOFHEADER       ) and $FF)) and
         (BackBuf[I + 1] = ((CN_SIGNATURE_ZIPENDOFHEADER shr  8) and $FF)) and
         (BackBuf[I + 2] = ((CN_SIGNATURE_ZIPENDOFHEADER shr 16) and $FF)) and
         (BackBuf[I + 3] = ((CN_SIGNATURE_ZIPENDOFHEADER shr 24) and $FF)) then
      begin
        Move(BackBuf[I + 4], Header^, SizeOf(Header^));
        if Header^.CommentLength > 0 then
        begin
          Stream.Position := Stream.Size - BackRead + I + 4 + SizeOf(Header^);
          SetLength(FComment, Header^.CommentLength);
          Stream.Read(FComment[1], Header^.CommentLength);
        end
        else
          SetLength(FComment, 0);

        Result := True;
        Exit;
      end;
    end;
  end;
  Result := False;
end;

function TCnZipReader.GetHasPassword: Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to GetFileCount - 1 do
  begin
    if GetFileInfo(I)^.Flag and 1 = 1 then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

procedure TCnZipReader.SetPassword(const Value: AnsiString);
begin
  inherited;

end;

constructor TCnZipReader.Create;
begin
  inherited;

end;

{ TCnZipDefaultCompressionHandler }

class function TCnZipDefaultCompressionHandler.CanHandleCompressionMethod(
  AMethod: TCnZipCompressionMethod): Boolean;
begin
  Result := AMethod in [zcStored, zcDeflate];
end;

class function TCnZipDefaultCompressionHandler.CreateCompressionStream(
  AMethod: TCnZipCompressionMethod; OutStream: TStream; const Item: PCnZipHeader;
  Zip: TCnZipBase): TStream;
var
  HasPas: Boolean;
begin
  Result := nil;
  HasPas := (Item^.Flag and 1) = 1;

  if AMethod = zcStored then
  begin
    if HasPas then
      Result := TCnEncryptStoredStream.Create(OutStream, Zip.Password, Item)
    else
      Result := TCnStoredStream.Create(OutStream)
  end
  else if AMethod = zcDeflate then
  begin
    if HasPas then
    begin
      Result := TCnEncryptZipCompressStream.Create(OutStream, Zip.Password, Item);
    end
    else
    begin
{$IFDEF SUPPORT_ZLIB_WINDOWBITS}
      Result := TCompressionStream.Create(OutStream, zcDefault, -15);
{$ELSE}
      Result := TCompressionStream.Create(clDefault, OutStream);
{$ENDIF}
    end;
  end;
end;

class function TCnZipDefaultCompressionHandler.CreateDecompressionStream(
  AMethod: TCnZipCompressionMethod; InStream: TStream; const Item: PCnZipHeader;
  Zip: TCnZipBase): TStream;
var
  HasPas: Boolean;
begin
  Result := nil;
  HasPas := (Item^.Flag and 1) = 1;

  if AMethod = zcStored then
  begin
    if HasPas then
      Result := TCnDecryptStoredStream.Create(InStream, Zip.Password, Item)
    else
      Result := TCnStoredStream.Create(InStream);
  end
  else if AMethod = zcDeflate then
  begin
    if HasPas then
    begin
      Result := TCnDecryptZipCompressStream.Create(InStream, Zip.Password, Item);
    end
    else
    begin
{$IFDEF SUPPORT_ZLIB_WINDOWBITS}
      Result := TDecompressionStream.Create(InStream, -15);
{$ELSE}
      Result := TDecompressionStream.Create(InStream);
{$ENDIF}
    end;
  end;
end;

{ TCnStoredStream }

constructor TCnStoredStream.Create(Stream: TStream);
begin
  inherited Create;
  FStream := Stream;
end;

function TCnStoredStream.GetSize: Int64;
begin
  Result := FStream.Size;
end;

function TCnStoredStream.Read(var Buffer; Count: Integer): Longint;
begin
  Result := FStream.Read(Buffer, Count);
end;

function TCnStoredStream.Seek(Offset: Longint; Origin: Word): Longint;
begin
  Result := FStream.Seek(Offset, Origin);
end;

function TCnStoredStream.Write(const Buffer; Count: Integer): Longint;
begin
  Result := FStream.Write(Buffer, Count);
end;

{ TCnZipWriter }

{$IFNDEF DISABLE_DIRECTORY_SUPPORT}

procedure TCnZipWriter.AddDirectory(const DirName: string;
  Compression: TCnZipCompressionMethod);
var
  I: Integer;
  Path, AFile: string;
begin
  if FDirFiles = nil then
    FDirFiles := TStringList.Create
  else
    FDirFiles.Free;

  FindFile(DirName, '*.*', FindFileCallback);

  for I := 0 to FDirFiles.Count - 1 do
  begin
    Path := IncludeTrailingBackslash(DirName);
{$IFDEF MSWINDOWS}
    AFile := StringReplace(Copy(FDirFiles[I], Length(Path) + 1, Length(FDirFiles[I])), '\', '/', [rfReplaceAll]);
{$ELSE}
    AFile := Copy(FDirFiles[I], Length(Path) + 1, Length(FDirFiles[I]));
{$ENDIF}
    AddFile(FDirFiles[I], AFile, Compression);
  end;
end;

{$ENDIF}

procedure TCnZipWriter.AddFile(const FileName, ArchiveFileName: string;
  Compression: TCnZipCompressionMethod);
var
  InStream: TStream;
  LocalHeader: PCnZipHeader;
  Archive: string;

  function GetFileDateTime(const FileName: string): TDateTime;
  var
    Handle: THandle;
    FindData: TWin32FindData;
    SystemTime: TSystemTime;
  begin
    Result := 0.0;
    Handle := FindFirstFile(PChar(FileName), FindData);
    if Handle <> INVALID_HANDLE_VALUE then
    begin
      Windows.FindClose(Handle);
      if (FindData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY) = 0 then
      begin
        FileTimeToLocalFileTime(FindData.ftLastWriteTime, FindData.ftLastWriteTime);
        FileTimeToSystemTime(FindData.ftLastWriteTime, SystemTime);
        with SystemTime do
          Result := EncodeDate(wYear, wMonth, wDay) + EncodeTime(wHour, wMinute,
            wSecond, wMilliseconds);
      end;
    end;
  end;

begin
  if Trim(FileName) = '' then
    Exit;

  if not SupportCompressionMethod(Compression) then
    raise ECnZipException.CreateRes(@SZipNotSupport);

  New(LocalHeader);
  FillChar(LocalHeader^, SizeOf(LocalHeader^), 0);

  InStream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    LocalHeader^.Flag := 0;
    LocalHeader^.CompressionMethod := Word(Compression);
    LocalHeader^.ModifiedDateTime := DateTimeToFileDate(GetFileDateTime(FileName) );
    LocalHeader^.UncompressedSize := InStream.Size;
    LocalHeader^.InternalAttributes := 0;
    LocalHeader^.ExternalAttributes := 0;
    if ArchiveFileName <> '' then
      Archive := ArchiveFileName
    else if FRemovePath then
      Archive := ExtractFileName(FileName)
    else
      Archive := FileName;

    if FUtf8 then
      LocalHeader^.Flag := LocalHeader^.Flag or CN_UTF8_MASK;
    if HasPassword then
      LocalHeader^.Flag := LocalHeader^.Flag or 1;

    LocalHeader^.FileName := StringToRaw(Archive);
    LocalHeader^.FileNameLength := Length(LocalHeader^.FileName);

    LocalHeader^.ExtraFieldLength := 0;
    AddStream(InStream, LocalHeader);
  finally
    InStream.Free;
  end;
end;

procedure TCnZipWriter.AddStream(Data: TStream; LocalHeader: PCnZipHeader);
var
  DataStart: Int64;
  CompressStream: TStream;
  Signature: LongWord;
  LStartPos: Int64;
  C: Integer;
  Buffer: array of Byte;
begin
  FOutStream.Position := FEndFileData;
  LocalHeader^.LocalHeaderOffset := FEndFileData;

  if LocalHeader^.MadeByVersion < 20 then
    LocalHeader^.MadeByVersion := 20;
  if LocalHeader^.RequiredVersion < 20 then
    LocalHeader^.RequiredVersion := 20;

  LocalHeader^.FileNameLength   := Length(LocalHeader^.FileName);
  LocalHeader^.ExtraFieldLength := Length(LocalHeader^.ExtraField);

  Signature := CN_SIGNATURE_LOCALHEADER;
  VerifyWrite(FOutStream, Signature, SizeOf(Signature));

  VerifyWrite(FOutStream, LocalHeader^.RequiredVersion,    Sizeof(Word));
  VerifyWrite(FOutStream, LocalHeader^.Flag,               Sizeof(Word));
  VerifyWrite(FOutStream, LocalHeader^.CompressionMethod,  Sizeof(Word));
  VerifyWrite(FOutStream, LocalHeader^.ModifiedDateTime,   Sizeof(LongWord));
  VerifyWrite(FOutStream, LocalHeader^.CRC32,              Sizeof(LongWord));
  VerifyWrite(FOutStream, LocalHeader^.CompressedSize,     Sizeof(LongWord));
  VerifyWrite(FOutStream, LocalHeader^.UncompressedSize,   Sizeof(LongWord));
  VerifyWrite(FOutStream, LocalHeader^.FileNameLength,     Sizeof(Word));
  VerifyWrite(FOutStream, LocalHeader^.ExtraFieldLength,   Sizeof(Word));

  VerifyWrite(FOutStream, LocalHeader^.FileName[1], LocalHeader^.FileNameLength);
  if LocalHeader^.ExtraFieldLength > 0 then
    VerifyWrite(FOutStream, LocalHeader^.ExtraField[1], LocalHeader^.ExtraFieldLength);

  LStartPos := FOutStream.Position;
  DataStart := Data.Position;
  LocalHeader^.UncompressedSize := Data.Size - DataStart;

  // ����ԭʼ�� CRC32 ֵ
  SetLength(Buffer, $4000);
  while Data.Position < Longint(LocalHeader^.UncompressedSize) do
  begin
    C := Data.Read(Buffer[0], Length(Buffer));
    LocalHeader^.CRC32 := CRC32Calc(LocalHeader^.CRC32, Buffer[0], C);
  end;

  // ���»ص�ԭλ��ѹ��
  Data.Position := DataStart;
  CompressStream := CreateCompressStreamFromHandler(TCnZipCompressionMethod(LocalHeader^.CompressionMethod),
    FOutStream, LocalHeader, Self);
  try
    CompressStream.CopyFrom(Data, LocalHeader^.UncompressedSize);
  finally
    CompressStream.Free;
  end;
  LocalHeader^.CompressedSize := FOutStream.Position - LStartPos;

  FEndFileData := FOutStream.Position;
  FOutStream.Position := LocalHeader^.LocalHeaderOffset + SizeOf(LongWord);
  VerifyWrite(FOutStream, LocalHeader^.RequiredVersion,    Sizeof(Word));
  VerifyWrite(FOutStream, LocalHeader^.Flag,               Sizeof(Word));
  VerifyWrite(FOutStream, LocalHeader^.CompressionMethod,  Sizeof(Word));
  VerifyWrite(FOutStream, LocalHeader^.ModifiedDateTime,   Sizeof(LongWord));
  VerifyWrite(FOutStream, LocalHeader^.CRC32,              Sizeof(LongWord));
  VerifyWrite(FOutStream, LocalHeader^.CompressedSize,     Sizeof(LongWord));
  VerifyWrite(FOutStream, LocalHeader^.UncompressedSize,   Sizeof(LongWord));
  VerifyWrite(FOutStream, LocalHeader^.FileNameLength,     Sizeof(Word));
  VerifyWrite(FOutStream, LocalHeader^.ExtraFieldLength,   Sizeof(Word));

  FFileList.Add(LocalHeader);
end;

procedure TCnZipWriter.Close;
begin
  ClearFiles;
  FreeAndNil(FOutStream);
end;

procedure TCnZipWriter.CreateZipFile(const ZipFileName: string);
begin
  Close;

  FOutStream := TFileStream.Create(ZipFileName, fmCreate);
  FStartFileData := FOutStream.Position;
end;

destructor TCnZipWriter.Destroy;
begin
  FreeAndNil(FOutStream);
{$IFNDEF DISABLE_DIRECTORY_SUPPORT}
  FDirFiles.Free;
{$ENDIF}
  inherited;
end;

{$IFNDEF DISABLE_DIRECTORY_SUPPORT}

procedure TCnZipWriter.FindFileCallback(const FileName: string;
  const Info: TSearchRec; var Abort: Boolean);
begin
  if (FileName <> '.') and (FileName <> '..') then
    FDirFiles.Add(FileName);
end;

{$ENDIF}

procedure TCnZipWriter.Save;
var
  Header: PCnZipHeader;
  EndOfHeader: TCnZipEndOfCentralHeader;
  I: Integer;
  Sig: LongWord;
begin
  FOutStream.Position := FEndFileData;
  Sig := CN_SIGNATURE_CENTRALHEADER;

  for I := 0 to FileCount - 1 do
  begin
    Header := FileInfo[I];
    VerifyWrite(FOutStream, Sig, SizeOf(Sig));
    VerifyWrite(FOutStream, Header^.MadeByVersion,      Sizeof(Word));
    VerifyWrite(FOutStream, Header^.RequiredVersion,    Sizeof(Word));
    VerifyWrite(FOutStream, Header^.Flag,               Sizeof(Word));
    VerifyWrite(FOutStream, Header^.CompressionMethod,  Sizeof(Word));
    VerifyWrite(FOutStream, Header^.ModifiedDateTime,   Sizeof(LongWord));
    VerifyWrite(FOutStream, Header^.CRC32,              Sizeof(LongWord));
    VerifyWrite(FOutStream, Header^.CompressedSize,     Sizeof(LongWord));
    VerifyWrite(FOutStream, Header^.UncompressedSize,   Sizeof(LongWord));
    VerifyWrite(FOutStream, Header^.FileNameLength,     Sizeof(Word));
    VerifyWrite(FOutStream, Header^.ExtraFieldLength,   Sizeof(Word));
    VerifyWrite(FOutStream, Header^.FileCommentLength,  Sizeof(Word));
    VerifyWrite(FOutStream, Header^.DiskNumberStart,    Sizeof(Word));
    VerifyWrite(FOutStream, Header^.InternalAttributes, Sizeof(Word));
    VerifyWrite(FOutStream, Header^.ExternalAttributes, Sizeof(LongWord));
    VerifyWrite(FOutStream, Header^.LocalHeaderOffset,  Sizeof(LongWord));

    if Header^.FileNameLength <> 0 then
      VerifyWrite(FOutStream, Header^.FileName[1], Header^.FileNameLength);
    if Header^.ExtraFieldLength <> 0 then
      VerifyWrite(FOutStream, Header^.ExtraField[1], Header^.ExtraFieldLength);
    if Header^.FileCommentLength <> 0 then
      VerifyWrite(FOutStream, Header^.FileComment[1], Header^.FileCommentLength);
  end;

  FillChar(EndOfHeader, Sizeof(EndOfHeader), 0);
  EndOfHeader.CentralDirEntries := FileCount;
  EndOfHeader.NumEntriesThisDisk := FileCount;
  EndOfHeader.CentralDirSize := FOutStream.Position - FEndFileData;
  EndOfHeader.CentralDirOffset := FEndFileData;

  if Length(FComment) > $FFFF then
    SetLength(FComment, $FFFF);
  EndOfHeader.CommentLength := Length(FComment);

  Sig := CN_SIGNATURE_ZIPENDOFHEADER;
  VerifyWrite(FOutStream, Sig, SizeOf(Sig));
  VerifyWrite(FOutStream, EndOfHeader.DiskNumber,          SizeOf(Word));
  VerifyWrite(FOutStream, EndOfHeader.CentralDirStartDisk, SizeOf(Word));
  VerifyWrite(FOutStream, EndOfHeader.NumEntriesThisDisk,  SizeOf(Word));
  VerifyWrite(FOutStream, EndOfHeader.CentralDirEntries,   SizeOf(Word));
  VerifyWrite(FOutStream, EndOfHeader.CentralDirSize,      SizeOf(LongWord));
  VerifyWrite(FOutStream, EndOfHeader.CentralDirOffset,    SizeOf(LongWord));
  VerifyWrite(FOutStream, EndOfHeader.CommentLength,       SizeOf(Word));

  if EndOfHeader.CommentLength > 0 then
    VerifyWrite(FOutStream, FComment[1], EndOfHeader.CommentLength);
end;

{ TCnZipCryptKeys }

function TCnZipCryptKeys.CalcDecryptByte: Byte;
var
  T: Word;
begin
  T := FKey2 or 2;
  Result := Word(T * (T xor 1)) shr 8;
end;

procedure TCnZipCryptKeys.DecryptByte(var Value: Byte);
begin
  Value := Value xor CalcDecryptByte;
  UpdateKeys(Value);
end;

procedure TCnZipCryptKeys.EncryptByte(var Value: Byte);
var
  T: Byte;
begin
  T := CalcDecryptByte;
  UpdateKeys(Value);
  Value := Value xor T;
end;

procedure TCnZipCryptKeys.InitKeys(const Password: AnsiString);
var
  I: Integer;
begin
  FKey0 := CN_KEY0_INIT;
  FKey1 := CN_KEY1_INIT;
  FKey2 := CN_KEY2_INIT;

  for I := 1 to Length(Password) do
    UpdateKeys(Ord(Password[I]));
end;

procedure TCnZipCryptKeys.UpdateKeys(C: Byte);
begin
  FKey0 := CalcCRC32Byte(FKey0, C);
  FKey1 := FKey1 + (FKey0 and $FF);
  FKey1 := FKey1 * CN_KEY_UPDATE + 1;
  FKey2 := CalcCRC32Byte(FKey2, FKey1 shr 24);
end;

{ TCnDecryptStoredStream }

constructor TCnDecryptStoredStream.Create(AStream: TStream;
  const APassword: AnsiString; const AZipHeader: PCnZipHeader);
var
  I: Integer;
  C: Byte;
  H: array [0..CN_ZIP_CRYPT_HEAD_SIZE - 1] of Byte;
begin
  inherited Create;
  FStream := AStream;
  FPassword := APassword;
  FZipHeader := AZipHeader;
  FKeys := TCnZipCryptKeys.Create;
  FKeys.InitKeys(FPassword);

  // �� 12 �ֽ�ͷ��������ȶ� CRC ���ж������Ƿ���ȷ��ע����Щ zip �ļ����ȶ� CRC
  FStream.Read(H, Sizeof(H));

  for I := 0 to CN_ZIP_CRYPT_HEAD_SIZE - 1 do
  begin
    C := H[I] xor FKeys.CalcDecryptByte;
    FKeys.UpdateKeys(C);
    H[I] := C;
  end;

  if H[CN_ZIP_CRYPT_HEAD_SIZE - 1] <> (FZipHeader^.CRC32 shr 24) then
    raise ECnZipException.CreateRes(@SZipInvalidPassword);
end;

destructor TCnDecryptStoredStream.Destroy;
begin
  FZip.Free;
  FKeys.Free;
  inherited;
end;

function TCnDecryptStoredStream.Read(var Buffer; Count: Integer): Integer;
var
  P: PByte;
  I: Integer;
begin
  Result := FStream.Read(Buffer, Count);
  P := @Buffer;
  for I := 1 to Result do
  begin
    FKeys.DecryptByte(P^);
    Inc(P);
  end;
end;

function TCnDecryptStoredStream.Seek(Offset: Longint; Origin: Word): Longint;
begin
  raise ECnZipException.CreateRes(@SZipNotImplemented);
end;

function TCnDecryptStoredStream.Write(const Buffer; Count: Integer): Integer;
begin
  raise ECnZipException.CreateRes(@SZipNotImplemented);
end;

{ TCnEnryptStoredStream }

constructor TCnEncryptStoredStream.Create(OutStream: TStream;
  const APassword: AnsiString; const AZipHeader: PCnZipHeader);
var
  H: array[0..CN_ZIP_CRYPT_HEAD_SIZE - 1] of Byte;
  I: Integer;
begin
  inherited Create;
  FOutStream := OutStream;
  FPassword := APassword;
  FZipHeader := AZipHeader;
  FKeys := TCnZipCryptKeys.Create;
  FKeys.InitKeys(FPassword);

  // ����� 12 ���ֽڵ�ͷ
  for I := 0 to CN_ZIP_CRYPT_HEAD_SIZE - 2 do
    H[I] := Random(256);
  H[CN_ZIP_CRYPT_HEAD_SIZE - 1] := (FZipHeader^.CRC32 shr 24);

  // ���ܲ�д��
  for I := 0 to CN_ZIP_CRYPT_HEAD_SIZE - 1 do
    FKeys.EncryptByte(H[I]);
  FOutStream.Write(H, Sizeof(H));
end;

destructor TCnEncryptStoredStream.Destroy;
begin
  FKeys.Free;
  inherited;
end;

function TCnEncryptStoredStream.Read(var Buffer; Count: Integer): Integer;
begin
  raise ECnZipException.CreateRes(@SZipNotImplemented);
end;

function TCnEncryptStoredStream.Seek(Offset: Integer;
  Origin: Word): Longint;
begin
  raise ECnZipException.CreateRes(@SZipNotImplemented);
end;

function TCnEncryptStoredStream.Write(const Buffer; Count: Integer): Integer;
const
  MaxBufSize = $F000;
var
  B: array of Byte;
  C, I: Integer;
  P: PByte;
begin
  Result := 0;
  if Count < MaxBufSize then
    SetLength(B, Count)
  else
    SetLength(B, MaxBufSize);

  P := @Buffer;
  while Count > 0 do
  begin
    C := Length(B);
    if Count < C then
      C := Count;

    Move(P^, B[0], C);
    Inc(P, C);
    for I := 0 to C - 1 do
      FKeys.EncryptByte(B[I]);

    Result := Result + FOutStream.Write(B[0], C);
    Count := Count - C;
  end;
end;

{ TCnEncryptZipCompressStream }

constructor TCnEncryptZipCompressStream.Create(OutStream: TStream;
  const APassword: AnsiString; const AZipHeader: PCnZipHeader);
var
  H: array[0..CN_ZIP_CRYPT_HEAD_SIZE - 1] of Byte;
  I: Integer;
begin
  inherited Create;
  FOutStream := OutStream;
  FPassword := APassword;
  FZipHeader := AZipHeader;
  FKeys := TCnZipCryptKeys.Create;
  FKeys.InitKeys(FPassword);

  FZipped := TMemoryStream.Create;
{$IFDEF SUPPORT_ZLIB_WINDOWBITS}
  FZip := TCompressionStream.Create(FZipped, zcDefault, -15);
{$ELSE}
  FZip := TCompressionStream.Create(clDefault, FZipped);
{$ENDIF}

  // ����� 12 ���ֽڵ�ͷ
  for I := 0 to CN_ZIP_CRYPT_HEAD_SIZE - 2 do
    H[I] := Random(256);
  H[CN_ZIP_CRYPT_HEAD_SIZE - 1] := (FZipHeader^.CRC32 shr 24);

  // ���ܲ�д��
  for I := 0 to CN_ZIP_CRYPT_HEAD_SIZE - 1 do
    FKeys.EncryptByte(H[I]);
  FOutStream.Write(H, Sizeof(H));
end;

destructor TCnEncryptZipCompressStream.Destroy;
var
  I: Integer;
  P: PByte;
begin
  FZip.Free;

  // FZipped ������ѹ�����ݣ���Ҫ���ܲ�д���� FOutStream
  P := FZipped.Memory;
  if (P <> nil) and (FZipped.Size > 0) then
  begin
    for I := 0 to FZipped.Size - 1 do
    begin
      FKeys.EncryptByte(P^);
      FOutStream.Write(P^, 1);
      Inc(P);
    end;
  end;

  FZipped.Free;
  FKeys.Free;
  inherited;
end;

function TCnEncryptZipCompressStream.Read(var Buffer;
  Count: Integer): Integer;
begin
  raise ECnZipException.CreateRes(@SZipNotImplemented);
end;

function TCnEncryptZipCompressStream.Seek(Offset: Integer;
  Origin: Word): Longint;
begin
  raise ECnZipException.CreateRes(@SZipNotImplemented);
end;

function TCnEncryptZipCompressStream.Write(const Buffer;
  Count: Integer): Integer;
begin
  // ���д���ԭʼ����������Ҫתд�� FZip ѹ������ѹ���������� FZip д�� FZipped
  Result := FZip.Write(Buffer, Count);
end;

{ TCnDecryptZipCompressStream }

constructor TCnDecryptZipCompressStream.Create(InStream: TStream;
  const APassword: AnsiString; const AZipHeader: PCnZipHeader);
var
  I: Integer;
  P: PByte;
  C: Byte;
  H: array [0..CN_ZIP_CRYPT_HEAD_SIZE - 1] of Byte;
begin
  inherited Create;
  FInStream := InStream;
  FPassword := APassword;
  FZipHeader := AZipHeader;
  FKeys := TCnZipCryptKeys.Create;
  FKeys.InitKeys(FPassword);

  // �� 12 �ֽ�ͷ��������ȶ� CRC ���ж������Ƿ���ȷ��ע����Щ zip �ļ����ȶ� CRC
  FInStream.Read(H, Sizeof(H));

  for I := 0 to CN_ZIP_CRYPT_HEAD_SIZE - 1 do
  begin
    C := H[I] xor FKeys.CalcDecryptByte;
    FKeys.UpdateKeys(C);
    H[I] := C;
  end;

  if H[CN_ZIP_CRYPT_HEAD_SIZE - 1] <> (FZipHeader^.CRC32 shr 24) then
    raise ECnZipException.CreateRes(@SZipInvalidPassword);

  // �ȴ� FInStream �������ж������ݵ� FDecrypted �����ܵõ�ѹ��������
  FDecrypted := TMemoryStream.Create;
  FDecrypted.CopyFrom(FInStream, FZipHeader^.CompressedSize - CN_ZIP_CRYPT_HEAD_SIZE);
  P := FDecrypted.Memory;
  for I := 1 to FDecrypted.Size do
  begin
    FKeys.DecryptByte(P^);
    Inc(P);
  end;

  FDecrypted.Position := 0;
{$IFDEF SUPPORT_ZLIB_WINDOWBITS}
  FUnzip := TDecompressionStream.Create(FDecrypted, -15);
{$ELSE}
  FUnzip := TDecompressionStream.Create(FDecrypted);
{$ENDIF}
end;

destructor TCnDecryptZipCompressStream.Destroy;
begin
  FUnzip.Free;      // FUnzip ���ͷ�ʱ�ڲ�Ҫ���������õ� FDecrypted ��˱������ͷ� FUnzip
  FDecrypted.Free;
  FKeys.Free;
  inherited;
end;

function TCnDecryptZipCompressStream.Read(var Buffer;
  Count: Integer): Integer;
begin
  // ��� Read ʱ�� FDecrypted ������ѹ��������ݲ�����
  Result := FUnzip.Read(Buffer, Count);
end;

function TCnDecryptZipCompressStream.Seek(Offset: Integer;
  Origin: Word): Longint;
begin
  raise ECnZipException.CreateRes(@SZipNotImplemented);
end;

function TCnDecryptZipCompressStream.Write(const Buffer;
  Count: Integer): Integer;
begin
  raise ECnZipException.CreateRes(@SZipNotImplemented);
end;

initialization
  FZipCompressionHandlers := TClassList.Create;
  RegisterCompressionHandlerClass(TCnZipDefaultCompressionHandler);

finalization
  FZipCompressionHandlers.Free;

end.
