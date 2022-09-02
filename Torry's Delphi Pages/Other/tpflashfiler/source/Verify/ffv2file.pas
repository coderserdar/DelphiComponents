{*********************************************************}
{* FlashFiler: FF 2 file & block interface classes       *}
{*********************************************************}

(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is TurboPower FlashFiler
 *
 * The Initial Developer of the Original Code is
 * TurboPower Software
 *
 * Portions created by the Initial Developer are Copyright (C) 1996-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

{$I ffdefine.inc}

unit ffv2file;

interface

uses
  Classes,
  FFFileInt,
  FFLLBase,
  FFSrBase,
  FFTbDict;

type
  Tffv2FileInterface = class(TffFileInterface)
    { Implements the interface for FF 2.xx tables. }
  protected
    FBufMgr : TffBufferManager;
      { Buffer manager used to manage file blocks. }
    FDict : TffServerDataDict;
      { Server data dictionary. }
    FDictBlocks : TInterfaceList;
      { List of data dictionary blocks. }
    FFileInfo : PffFileInfo;
      { Structure used to store information about file being verified. }
    FFileHeaderBlock : IFileHeaderBlock;
    FIndexHeaderBlock : IIndexHeaderBlock;
    FOnReportError : TffReportErrorEvent;
    FOnReportFix : TffReportFixEvent;

    FTI : PffTransInfo;
      { Fake transaction information. }

    procedure CloseCurrentFile;
      { If a file is open, this method closes the file. }

    function GetDictBlockCount : Integer; override;
    function GetDictBlocks(const Inx : Longint) : IStreamBlock; override;
    function GetOnReportError : TffReportErrorEvent; override;
    function GetOnReportFix : TffReportFixEvent; override;

    procedure SetOnReportError(Value : TffReportErrorEvent); override;
    procedure SetOnReportFix(Value : TffReportFixEvent); override;
    procedure SetOutputVersion(const Value : Longint); override;

  public
    destructor Destroy; override;

    procedure Initialize; override;

    procedure Close; override;
      { Close the currently opened file. }

    function GetBlock(const BlockNumber : Longint) : ICommonBlock; override;
      { Returns a specific block from the file. }

    function GetFileHeaderBlock : IFileHeaderBlock; override;
      { Returns the file header block. }

    function GetFileInfo : TffGeneralFileInfo; override;
      { Returns general file information that is made available to blocks. }

    function GetIndexHeaderBlock : IIndexHeaderBlock; override;
      { Returns the index header block. }

    procedure Open(const Filename : string); override;
      { Open a file for analysis. }

    procedure Pack; override;

  end;

  Tffv2FileBlock = class(TffFileBlock)
  protected
    FIsModified : Boolean;
      { Set to True when BeginUpdate is called.
        Set to False when EndUpdate is called. }
  public
    constructor Create(BufMgr : TffBufferManager;
                       FileInfo : PffFileInfo;
                       TI : PffTransInfo;
                 const BlockNum : TffWord32); override;

    procedure BeginUpdate; override;
    procedure EndUpdate; override;
  end;

  TffFileHeaderBlock = class(Tffv2FileBlock, IFileHeaderBlock)
  protected
    function GetAvailBlocks : Longint; virtual;
    function GetBLOBCount : TffWord32; virtual;
    function GetBlockSize : Longint; virtual;
    function GetDataDictBlockNum : TffWord32; virtual;
    function GetDeletedBLOBHead : TffInt64; virtual;
    function GetDeletedBLOBTail : TffInt64; virtual;
    function GetDeletedRecordCount : Longint; virtual;
    function GetEncrypted : Longint; virtual;
    function GetEstimatedUsedBlocks : TffWord32; virtual;
    function GetFFVersion : Longint; virtual;
    function GetFieldCount : Longint; virtual;
    function GetFirstDataBlock : TffWord32; virtual;
    function GetFirstDeletedRecord : TffInt64; virtual;
    function GetFirstFreeBlock : TffWord32; virtual;
    function GetHasSequentialIndex : Longint; virtual;
    function GetIndexCount : Longint; virtual;
    function GetIndexHeaderBlockNum : TffWord32; virtual;
    function GetLastAutoIncValue : TffWord32; virtual;
    function GetLastDataBlock : TffWord32; virtual;
    function GetLog2BlockSize : TffWord32; virtual;
    function GetRecLenPlusTrailer : Longint; virtual;
    function GetRecordCount : Longint; virtual;
    function GetRecordLength : Longint; virtual;
    function GetRecordsPerBlock : Longint; virtual;
    function GetUsedBlocks : TffWord32; virtual;
    function GetPropertyCell(const Row, Column : Integer) : string; override;
    function GetPropertyRowCount : Integer; override;

    procedure SetFirstDataBlock(const Value : TffWord32); virtual;
    procedure SetFirstFreeBlock(const Value : TffWord32); virtual;
    procedure SetHasSequentialIndex(const Value : Longint); virtual;
    procedure SetLastDataBlock(const Value : TffWord32); virtual;
    procedure SetLog2BlockSize(const Value : TffWord32); virtual;
    procedure SetUsedBlocks(const Value : TffWord32); virtual;

    procedure VerifyRepair(const Repair : Boolean); override;
      { This method is used by both Verify & Repair. It carries out the actual
        verification &, if specified, repairing of problems. }
  public

    { Properties }
    property AvailBlocks : Longint
      read GetAvailBlocks;
      { The number of free blocks in the file. }

    property BLOBCount : TffWord32
      read GetBLOBCount;
      { The number of BLOBs in the table. }

    property BlockSize : Longint
      read GetBlockSize;
      { Size of blocks in bytes (e.g., 4k, 8k, 16k, 32k, 64k) }

    property DataDictBlockNum : TffWord32
      read GetDataDictBlockNum;
      { The block number of the data dictionary. If there is no data
        dictionary then this property returns the value zero. }

    property DeletedBLOBHead : TffInt64
      read GetDeletedBLOBHead;
      { The file-relative offset of the first segment in the deleted BLOB
        chain. }

    property DeletedBLOBTail : TffInt64
      read GetDeletedBLOBTail;
      { The file-relative offset of the last segment in the deleted BLOB
        chain. }

    property DeletedRecordCount : Longint
      read GetDeletedRecordCount;
      { The number of deleted records in the table. }

    property Encrypted : Longint
      read GetEncrypted;
      { 0 = not encrypted, 1 = encrypted }

    property EstimatedUsedBlocks : TffWord32
      read GetEstimatedUsedBlocks;
      { For cases where the UsedBlocks counter is invalid, use this property
        to estimate the number of used blocks in the file. This only works
        in cases where the BlockSize is valid. }

    property FFVersion : Longint
      read GetFFVersion;
      { The version of FlashFiler with which this table was created. }

    property FieldCount : Longint
      read GetFieldCount;
      { The number of fields in a record. }

    property FirstDataBlock : TffWord32
      read GetFirstDataBlock write SetFirstDataBlock;
      { The first data block in the chain of data blocks. }

    property FirstDeletedRecord : TffInt64
      read GetFirstDeletedRecord;
      { The offset of the first record in the deleted record chain. }

    property FirstFreeBlock : TffWord32
      read GetFirstFreeBlock;
      { The block number of the first free block in the deleted block chain. }

    property HasSequentialIndex : Longint
      read GetHasSequentialIndex write SetHasSequentialIndex;
      { Identifies whether the table has a sequential index. A value of zero
        means the table does not have a sequential index. A value of 1
        means the table does have a sequential index. }

    property IndexCount : Longint
      read GetIndexCount;
      { The number of indexes in the table. }

    property IndexHeaderBlockNum : TffWord32
      read GetIndexHeaderBlockNum;
      { The block number of the index header. }

    property LastAutoIncValue : TffWord32
      read GetLastAutoIncValue;
      { The last autoincrement value assigned to a record in the table. }

    property LastDataBlock : TffWord32
      read GetLastDataBlock write SetLastDataBlock;
      { The last data block in the chain of data blocks. }

    property Log2BlockSize : TffWord32
      read GetLog2BlockSize write SetLog2BlockSize;
      { log base 2 of BlockSize (e.g., 12, 13, 14, 15, or 16) }

    property RecordCount : Longint
      read GetRecordCount;
      { The number of records in the table. }

    property RecordLength : Longint
      read GetRecordLength;
      { The length of the record in bytes. }

    property RecordLengthPlusTrailer : Longint
      read GetRecLenPlusTrailer;
      { The length of the record plus the deletion link. }

    property RecordsPerBlock : Longint
      read GetRecordsPerBlock;
      { The number of records per data block. }

    property UsedBlocks : TffWord32
      read GetUsedBlocks write SetUsedBlocks;
      { The number of blocks in the file. }

  end;

  TffIndexBlock = class(Tffv2FileBlock, IIndexBlock)
  protected
    function GetIndexBlockType : Byte; virtual;
    function GetIsLeafPage : Boolean; virtual;
    function GetNodeLevel : Byte; virtual;
    function GetKeysAreRefs : Boolean; virtual;
    function GetIndexNum : Word; virtual;
    function GetKeyLength : Word; virtual;
    function GetKeyCount : Longint; virtual;
    function GetMaxKeyCount : Longint; virtual;
    function GetPrevPageRef : TffWord32; virtual;
    function GetPropertyCell(const Row, Column : Integer) : string; override;
    function GetPropertyRowCount : Integer; override;

    procedure VerifyRepair(const Repair : Boolean); override;
      { This method is used by both Verify & Repair. It carries out the actual
        verification &, if specified, repairing of problems. }
  public
    property IndexBlockType : Byte
      read GetIndexBlockType;
      { The type of index block. Header blocks have value 0, B-Tree pages
        have value 1. }
    property IsLeafPage : Boolean
      read GetIsLeafPage;
      { Returns False if this is an internal B-Tree page or True if this is
        a leaf B-Tree page. }
    property NodeLevel : Byte
      read GetNodeLevel;
      { Returns the node level. Leaves have value 1, increments. }
    property KeysAreRefs : Boolean
      read GetKeysAreRefs;
      { Returns the value True if the keys in the index are record reference
        numbers. }
    property IndexNum : Word
      read GetIndexNum;
      { The index number with which the index page is associated. }
    property KeyLength : Word
      read GetKeyLength;
      { The length of each key. }
    property KeyCount : Longint
      read GetKeyCount;
      { The number of keys currently in the page. }
    property MaxKeyCount : Longint
      read GetMaxKeyCount;
      { The maximum number of keys that may be placed within the page. }
    property PrevPageRef : TffWord32
      read GetPrevPageRef;
      { Block number of the previous page. }
  end;

  TffIndexHeaderBlock = class(TffIndexBlock, IIndexBlock, IIndexHeaderBlock)
  protected
    FDataColumns : Integer;
    FIndexHead : PffIndexHeader;
    procedure VerifyRepair(const Repair : Boolean); override;
      { This method is used by both Verify & Repair. It carries out the actual
        verification &, if specified, repairing of problems. }
  public
    constructor Create(BufMgr : TffBufferManager;
                       FileInfo : PffFileInfo;
                       TI : PffTransInfo;
                 const BlockNum : TffWord32); override;

    { Data access }
    function GetDataCell(const Row, Column : Integer) : string; override;
    function GetDataColCaption(const Index : Integer) : string; override;
    function GetDataColCount : Integer; override;
    function GetDataColWidth(const Index : Integer) : Integer; override;
    function GetDataRowCount : Integer; override;
  end;

  TffDataBlock = class(Tffv2FileBlock, IDataBlock)
  protected

    FNumDataColumns : Integer;
      { The number of columns calculated for the data view. }

    function GetNextDataBlock : TffWord32; virtual;
    function GetPrevDataBlock : TffWord32; virtual;
    function GetRecCount : Longint; virtual;
    function GetRecLen : Longint; virtual;

    function IsEmptyLookupEntry(Entry : PffBLOBLookupEntry) : Boolean; virtual;
    
    { Property access }
    function GetPropertyCell(const Row, Column : Integer) : string; override;
    function GetPropertyRowCount : Integer; override;

    { Data access }
    function GetDataCell(const Row, Column : Integer) : string; override;
    function GetDataColCaption(const Index : Integer) : string; override;
    function GetDataColCount : Integer; override;
    function GetDataColWidth(const Index : Integer) : Integer; override;
    function GetDataRowCount : Integer; override;

    procedure SetNextDataBlock(const Value : TffWord32); virtual;
    procedure SetPrevDataBlock(const Value : TffWord32); virtual;
    procedure SetRecCount(const Value : Longint); virtual;
    procedure SetRecLen(const Value : Longint); virtual;

    procedure VerifyBLOB(const BLOBNr : TffInt64;
                           var ErrCode : Integer); virtual;
  public
    procedure VerifyRepair(const Repair : Boolean); override;

    property RecordCount : Longint
      read GetRecCount write SetRecCount;
      { The maximum number of records in the block. }
    property RecordLen : Longint
      read GetRecLen write SetRecLen;
      { The length of each record. }
    property NextDataBlock : TffWord32
      read GetNextDataBlock write SetNextDataBlock;
      { The block # of the next data block. }
    property PrevDataBlock : TffWord32
      read GetPrevDataBlock write SetPrevDataBlock;
      { The block # of the previous data block. }
  end;

  TffBLOBBlock = class(Tffv2FileBlock, IBLOBBlock)
  protected
  public
    procedure VerifyRepair(const Repair : Boolean); override;
  end;

  TffStreamBlock = class(Tffv2FileBlock, IStreamBlock)
  protected
    function GetNextStrmBlock : TffWord32; virtual;
    function GetStreamType : Longint; virtual;
    function GetStreamLength : Longint; virtual;
    function GetOwningStream : Longint; virtual;
    function GetPropertyCell(const Row, Column : Integer) : string; override;
    function GetPropertyRowCount : Integer; override;
  public
    procedure VerifyRepair(const Repair : Boolean); override;
  end;

implementation

uses
  FFDbBase,
  FFFile,
  FFRepCnst,
  FFSrBDE,
  FFSrBLOB,
  FFSrEng,
  FFTbData,
  FFTbIndx,
  FFUtil,
  SysUtils,
  Windows;

const
  FFStartVersion : Longint = 20000; {2.00.00}
  FFEndVersion   : Longint = 29999; {2.99.99, all FF 2 versions }

  ciDataBlockRows = 4;
  ciFileHeaderRows = 24;
  ciIndexBlockRows = 9;
  ciIndexHeaderDataColumns = 6;
  ciStreamRows = 4;

  csAlias = 'FFVerify';

{ The following constants were copied from the implementation section of unit
  FFTBBLOB. }
const
  ffc_FileBLOB = -1;
  ffc_BLOBLink = -2;

{ The following types were copied from the implementation section of unit
  FFTBINDX. }

type
  PRef = ^TRef;
  TRef = TffInt64;
  PPageNum = ^TpageNum;
  TPageNum = TffWord32;

const
  SizeOfRef = sizeof(TRef);
  SizeOfPageNum = sizeof(TPageNum);

type
  PRefBlock = ^TRefBlock;
  TRefBlock = array [0..($FFFFFFF div SizeOfRef)-1] of TRef;

  PPageNumBlock = ^TPageNumBlock;
  TPageNumBlock = array [0..($FFFFFFF div SizeOfPageNum)-1] of TPageNum;


{===TffFileInterface=================================================}
destructor Tffv2FileInterface.Destroy;
begin
  { If a file is open then close it. }
  CloseCurrentFile;

  FTI^.tirTrans.Free;
  FFFreeMem(FTI, SizeOf(TffTransInfo));

  if Assigned(FBufMgr) then
    FBufMgr.free;

  FDictBlocks.Free;

  inherited;
end;
{--------}
procedure Tffv2FileInterface.Initialize;
begin
  inherited;
  FBufMgr := TffBufferManager.Create(GetCurrentDir, 1);
  FBufMgr.MaxRAM := 20;
  fileProcsInitialize;
  FFGetMem(FTI, SizeOf(TffTransInfo));
  FOutputVersion := FFVersionNumber;
  FTI^.tirLockMgr := nil;
  FTI^.tirTrans := TffSrTransaction.Create(1000, False, False);

  FEndFFVersion := FFEndVersion;
  FStartFFVersion := FFStartVersion;

  FDictBlocks := TInterfaceList.Create;
end;
{--------}
procedure Tffv2FileInterface.Close;
begin
  CloseCurrentFile;
end;
{--------}
procedure Tffv2FileInterface.CloseCurrentFile;
var
  Inx : Integer;
begin
  if Assigned(FDict) then begin
    FDict.Free;
    FDict := nil;
  end;

  if FFileHeaderBlock <> nil then
    FFileHeaderBlock := nil;
      { No need to free since it will be autofreed. }

  if FIndexHeaderBlock <> nil then
    FIndexHeaderBlock := nil;

  { Free the list of dictionary blocks. }
  for Inx := Pred(FDictBlocks.Count) downto 0 do
    FDictBlocks.Delete(Inx);

  if FFileInfo <> nil then begin
    { Close the file. }
    FBufMgr.RemoveFile(FFileInfo);
    FFCloseFilePrim(FFileInfo);
    FFFreeFileInfo(FFileInfo);
    FFileInfo := nil;
  end;

end;
{--------}
function Tffv2FileInterface.GetBlock(const BlockNumber : Longint) : ICommonBlock;
var
  Block : ICommonBlock;
begin
  Block := TffFileBlock.Create(FBufMgr, FFileInfo, FTI, BlockNumber);
  case Block.BlockType of
    btUnknown    : Result := Tffv2FileBlock.Create(FBufMgr, FFileInfo, FTI, BlockNumber);
    btFileHeader : Result := TffFileHeaderBlock.Create(FBufMgr, FFileInfo, FTI, BlockNumber);
    btIndexHeader : Result := TffIndexHeaderBlock.Create(FBufMgr, FFileInfo, FTI, BlockNumber);
    btData       : Result := TffDataBlock.Create(FBufMgr, FFileInfo, FTI, BlockNumber);
    btIndex      : Result := TffIndexBlock.Create(FBufMgr, FFileInfo, FTI, BlockNumber);
    btBLOB       : Result := TffBLOBBlock.Create(FBufMgr, FFileInfo, FTI, BlockNumber);
    btStream     : Result := TffStreamBlock.Create(FBufMgr, FFileInfo, FTI, BlockNumber);
    btFree       : Result := Block;
  end;  { case }
  Result.OnReportError := FOnReportError;
  Result.OnReportFix := FOnReportFix;
end;
{--------}
function Tffv2FileInterface.GetDictBlockCount : Longint;
begin
  Result := FDictBlocks.Count;
end;
{--------}
function Tffv2FileInterface.GetDictBlocks(const Inx : Longint) : IStreamBlock;
begin
  Result := IStreamBlock(FDictBlocks[Inx]);
  Result.OnReportError := FOnReportError;
  Result.OnReportFix := FOnReportFix;
end;
{--------}
function Tffv2FileInterface.GetFileHeaderBlock : IFileHeaderBlock;
begin
  Result := FFileHeaderBlock;
  if Result <> nil then begin
    Result.OnReportError := FOnReportError;
    Result.OnReportFix := FOnReportFix;
  end;
end;
{--------}
function Tffv2FileInterface.GetFileInfo : TffGeneralFileInfo;
begin
  { TODO:: File must be opened. }
  Result := TffGeneralFileInfo.Create(FDict, FFileHeaderBlock);
end;
{--------}
function Tffv2FileInterface.GetIndexHeaderBlock : IIndexHeaderBlock;
begin
  Result := FIndexHeaderBlock;
  if Result <> nil then begin
    Result.OnReportError := FOnReportError;
    Result.OnReportFix := FOnReportFix;
  end;
end;
{--------}
function Tffv2FileInterface.GetOnReportError : TffReportErrorEvent;
begin
  Result := FOnReportError;
end;
{--------}
function Tffv2FileInterface.GetOnReportFix : TffReportFixEvent;
begin
  Result := FOnReportFix;
end;
{--------}
procedure Tffv2FileInterface.Open(const Filename : string);
var
  FileBlock : PffBlock;
  Block : ICommonBlock;
  DictBlock : IStreamBlock;
  FileVersion : Longint;
  RelMethod : TffReleaseMethod;
begin
  CloseCurrentFile;

  { Set up the info for the file. }
  FFileInfo := FFAllocFileInfo(FileName, ffc_extForData, FBufMgr);

  { Open the file. }
  FFOpenFile(FFileInfo, omReadWrite, smExclusive, False, False);

  { Read the header record to see if this is a FF data file supported by this
    interface. First, add the file to the buffer manager. }
  FileBlock := FBufMgr.AddFile(FFileInfo, FTI, False, RelMethod);
  try
    { Get the header block. }
    FFileHeaderBlock := TffFileHeaderBlock.Create(FBufMgr, FFileInfo, FTI, 0);
    try
      if ICommonBlock(FFileHeaderBlock).BlockType <> btFileHeader then
        raise Exception.CreateFmt('"%s" is not a FlashFiler table.', [FileName])
      else begin
        { Get the version. }
        FileVersion := FFileHeaderBlock.FFVersion;
        { Does this interface handle the version? }
        if (FileVersion < FFStartVersion) or (FileVersion > FFEndVersion) then
          raise Exception.CreateFmt
                  ('Table "%s" was created with version %s ' +
                   'of FlashFiler but this interface only supports versions ' +
                   '%s through %s',
                   [FileName, VersionToStr(FileVersion),
                    VersionToStr(FFStartVersion),
                    VersionToStr(FFEndVersion)]);

        { Get the data dictionary blocks. }
        Block := GetBlock(FFileHeaderBlock.DataDictBlockNum);
        if Supports(Block, IStreamBlock, DictBlock) then
          FDictBlocks.Add(DictBlock)
        else
          raise Exception.CreateFmt('Block %d is not a dictionary block as expected.',
                                    [FFileHeaderBlock.DataDictBlockNum]);
        while DictBlock.NextBlock <> ffc_W32NoValue do begin
          Block := GetBlock(DictBlock.NextBlock);
          if Supports(Block, IStreamBlock, DictBlock) then
            FDictBlocks.Add(DictBlock)
          else
            raise Exception.CreateFmt('Block %d is not a dictionary block as expected.',
                                      [DictBlock.NextBlock]);
        end;  { while }

        { Get the index header block. }
        FIndexHeaderBlock := TffIndexHeaderBlock.Create
                               (FBufMgr, FFileInfo, FTI,
                                FFileHeaderBlock.IndexHeaderBlockNum);

        { Read the dictionary. }
        FDict := TffServerDataDict.Create(4096);
        FDict.ReadFromFile(FFileInfo, FTI);
      end;  { if }
    except
      CloseCurrentFile;
      raise;
    end;
  finally
    RelMethod(FileBlock);
  end;
end;
{--------}
type
  SrDBCracker = class(TffSrDatabase);
{--------}
procedure Tffv2FileInterface.Pack;
const
  ciTimeout = 10000;
var
  FileName,
  FileDir : string;
  Engine : TffServerEngine;
  RebuildID,
  MaxPos : Integer;
  ClientID : TffClientID;
  SessionID : TffSessionID;
  DatabaseID : TffDatabaseID;
  Result : TffResult;
  PwdHash : TffWord32;
  TableName : string;
  IsPresent : Boolean;
  Status : TffRebuildStatus;
  E : EffDatabaseError;
begin
  { Get location & name of table. }
  FileName := FFileInfo^.fiName^;
  FileDir := ExtractFilePath(FileName);
  TableName := ChangeFileExt(ExtractFileName(FileName), '');

  { Close the file. }
  CloseCurrentFile;

  { Future:: Backup location specified? }

  { Init client, session, database IDs. }
  ClientID := ffc_W32NoValue;
  SessionID := ffc_W32NoValue;
  DatabaseID := ffc_W32NoValue;

  { Pack the table via a temporary embedded server engine. }
  Engine := TffServerEngine.Create(nil);
  try
    try
      { Initialize. }
      Engine.Startup;
      Engine.IsReadOnly := True;
      Engine.MaxRAM := 50;

      { Obtain a client connection. }
      Result := Engine.ClientAdd(ClientID, '', '', ciTimeout, PwdHash);
      if Result <> DBIERR_NONE then
        raise Exception.CreateFmt('Pack error: Could not add client, error code %d',
                                  [Result]);

      { Open a session. }
      Result := Engine.SessionAdd(ClientID, ciTimeout, SessionID);
      if Result <> DBIERR_NONE then
        raise Exception.CreateFmt('Pack error: Could not add session, error code %d',
                                  [Result]);

      { Add an alias for the current directory. }
      Result := Engine.DatabaseAddAlias(csAlias, FileDir, False, ClientID);
      if Result <> DBIERR_NONE then
        raise Exception.CreateFmt('Pack error: Could not add alias, error code %d',
                                  [Result]);

      { Open a database. }
      Result := Engine.DatabaseOpen(ClientID, csAlias, omReadWrite, smShared,
                                    ciTimeout, DatabaseID);
      if Result <> DBIERR_NONE then
        raise Exception.CreateFmt('Pack error: Could not add session, error code %d',
                                  [Result]);

      { Set the output version for the new table. }
      SrDBCracker(DatabaseID).dbSetNewTableVersion(FOutputVersion);
      
      { Calculate Max position for progress. }
      MaxPos := 100;

      { Start the pack. This is asynchronous so wait for the pack to finish. }
      Engine.IsReadOnly := False;
      Result := Engine.TablePack(DatabaseID, TableName, RebuildID);
      if Result <> DBIERR_NONE then
        raise Exception.CreateFmt('Pack error: Could not initiate pack, ' +
                                  'error code %d', [Result]);

      repeat
        Sleep(100);
        Result := Engine.RebuildGetStatus(RebuildID, ClientID, IsPresent, Status);
        if Assigned(FRebuildProgress) then
          FRebuildProgress(Self, Status.rsPercentDone, MaxPos);
      until (Result = DBIERR_OBJNOTFOUND) or Status.rsFinished;

      if (Status.rsErrorCode <> DBIERR_NONE) and
         Assigned(FOnReportError) then begin
        E := EffDatabaseError.CreateViaCode(Status.rsErrorCode, False);
        try
          FOnReportError(nil, rciPackFailure,
                         Format(rcErrStr[rciPackFailure],
                                [E.Message]));
        finally
          E.Free;
        end;
      end;  { if }
    except
      on E:Exception do begin
        FOnReportError(nil, rciPackFailure,
                       Format(rcErrStr[rciPackFailure],
                              [E.Message]));
      end;
    end;
  finally
    if DatabaseID <> ffc_W32NoValue then
      Engine.DatabaseClose(DatabaseID);

    if SessionID <> ffc_W32NoValue then
      Engine.SessionRemove(ClientID, SessionID);

    if ClientID <> ffc_W32NoValue then
      Engine.ClientRemove(ClientID);

    Engine.Free;

    { Re-open the file. }
    Open(FileName);
  end;

end;
{--------}
procedure Tffv2FileInterface.SetOnReportError(Value : TffReportErrorEvent);
begin
  FOnReportError := Value;
end;
{--------}
procedure Tffv2FileInterface.SetOnReportFix(Value : TffReportFixEvent);
begin
  FOnReportFix := Value;
end;
{--------}
procedure Tffv2FileInterface.SetOutputVersion(const Value : Longint);
begin
  { Validate the version. }
  if (Value >= ffVersion2_10) and (Value <= ffVersionNumber) then
    FOutputVersion := Value
  else
    raise Exception.Create(Format('The output version must be >= %d and <= %d',
                                  [ffVersion2_10, ffVersionNumber]));
end;
{====================================================================}

{===Tffv2FileBlock===================================================}
constructor Tffv2FileBlock.Create(BufMgr : TffBufferManager;
                                  FileInfo : PffFileInfo;
                                  TI : PffTransInfo;
                            const BlockNum : TffWord32);
begin
  inherited;
  FIsModified := False;
end;
{--------}
procedure Tffv2FileBlock.BeginUpdate;
begin
  if not FIsModified then begin
    { We need to change the block. Release the read-only copy & grab a modifiable
      copy. }
    FRelMethod(FBlock);
    FBufMgr.StartTransaction(FTI.tirTrans, False, '');
    FBlock := FBufMgr.GetBlock(FFileInfo, FBlockNum, FTI, ffc_MarkDirty,
                               FRelMethod);
    FIsModified := True;
  end;
end;
{--------}
procedure Tffv2FileBlock.EndUpdate;
begin
  { Release the dirty copy, commit the change, & get a read-only copy. }
  FRelMethod(FBlock);
  FBufMgr.CommitTransaction(FTI.tirTrans);
  FBlock := FBufMgr.GetBlock(FFileInfo, FBlockNum, FTI, ffc_ReadOnly,
                             FRelMethod);
  FIsModified := False;
end;
{====================================================================}

{===TffFileHeaderBlock===============================================}
function TffFileHeaderBlock.GetAvailBlocks : Longint;
begin
  Result := PffBlockHeaderFile(FBlock)^.bhfAvailBlocks;
end;
{--------}
function TffFileHeaderBlock.GetBLOBCount : TffWord32;
begin
  Result := PffBlockHeaderFile(FBlock)^.bhfBLOBCount;
end;
{--------}
function TffFileHeaderBlock.GetBlockSize : Longint;
begin
  Result := PffBlockHeaderFile(FBlock)^.bhfBlockSize;
end;
{--------}
function TffFileHeaderBlock.GetDataDictBlockNum : TffWord32;
begin
  Result := PffBlockHeaderFile(FBlock)^.bhfDataDict;
end;
{--------}
function TffFileHeaderBlock.GetDeletedBLOBHead : TffInt64;
begin
  Result := PffBlockHeaderFile(FBlock)^.bhfDelBLOBHead;
end;
{--------}
function TffFileHeaderBlock.GetDeletedBLOBTail : TffInt64;
begin
  Result := PffBlockHeaderFile(FBlock)^.bhfDelBLOBTail;
end;
{--------}
function TffFileHeaderBlock.GetDeletedRecordCount : Longint;
begin
  Result := PffBlockHeaderFile(FBlock)^.bhfDelRecCount;
end;
{--------}
function TffFileHeaderBlock.GetEncrypted : Longint;
begin
  Result := PffBlockHeaderFile(FBlock)^.bhfEncrypted;
end;
{--------}
function TffFileHeaderBlock.GetEstimatedUsedBlocks : TffWord32;
var
  CalcInt64Value : TffInt64;
begin
  ffI64DivInt(FFGetFileSize(FFileInfo), BlockSize, CalcInt64Value);
  Result := CalcInt64Value.iLow;
end;
{--------}
function TffFileHeaderBlock.GetFFVersion : Longint;
begin
  Result := PffBlockHeaderFile(FBlock)^.bhfFFVersion;
end;
{--------}
function TffFileHeaderBlock.GetFieldCount : Longint;
begin
  Result := PffBlockHeaderFile(FBlock)^.bhfFieldCount;
end;
{--------}
function TffFileHeaderBlock.GetFirstDataBlock : TffWord32;
begin
  Result := PffBlockHeaderFile(FBlock)^.bhf1stDataBlock;
end;
{--------}
function TffFileHeaderBlock.GetFirstDeletedRecord : TffInt64;
begin
  Result := PffBlockHeaderFile(FBlock)^.bhf1stDelRec;
end;
{--------}
function TffFileHeaderBlock.GetFirstFreeBlock : TffWord32;
begin
  Result := PffBlockHeaderFile(FBlock)^.bhf1stFreeBlock;
end;
{--------}
function TffFileHeaderBlock.GetHasSequentialIndex : Longint;
begin
  Result := PffBlockHeaderFile(FBlock)^.bhfHasSeqIndex;
end;
{--------}
function TffFileHeaderBlock.GetIndexCount : Longint;
begin
  Result := PffBlockHeaderFile(FBlock)^.bhfIndexCount;
end;
{--------}
function TffFileHeaderBlock.GetIndexHeaderBlockNum : TffWord32;
begin
  Result := PffBlockHeaderFile(FBlock)^.bhfIndexHeader;
end;
{--------}
function TffFileHeaderBlock.GetLastAutoIncValue : TffWord32;
begin
  Result := PffBlockHeaderFile(FBlock)^.bhfAutoIncValue;
end;
{--------}
function TffFileHeaderBlock.GetLastDataBlock : TffWord32;
begin
  Result := PffBlockHeaderFile(FBlock)^.bhfLastDataBlock;
end;
{--------}
function TffFileHeaderBlock.GetLog2BlockSize : TffWord32;
begin
  Result := PffBlockHeaderFile(FBlock)^.bhfLog2BlockSize;
end;
{--------}
function TffFileHeaderBlock.GetRecLenPlusTrailer : Longint;
begin
  Result := PffBlockHeaderFile(FBlock)^.bhfRecLenPlusTrailer;
end;
{--------}
function TffFileHeaderBlock.GetRecordCount : Longint;
begin
  Result := PffBlockHeaderFile(FBlock)^.bhfRecordCount;
end;
{--------}
function TffFileHeaderBlock.GetRecordLength : Longint;
begin
  Result := PffBlockHeaderFile(FBlock)^.bhfRecordLength;
end;
{--------}
function TffFileHeaderBlock.GetRecordsPerBlock : Longint;
begin
  Result := PffBlockHeaderFile(FBlock)^.bhfRecsPerBlock;
end;
{--------}
function TffFileHeaderBlock.GetUsedBlocks : TffWord32;
begin
  Result := PffBlockHeaderFile(FBlock)^.bhfUsedBlocks;
end;
{--------}
function TffFileHeaderBlock.GetPropertyCell(const Row, Column : Integer) : string;
begin
  if Column > Pred(ciFileBlockColumns) then
    raise Exception.CreateFmt
            ('Cannot ask for cell in column %d when there are only %d columns in the view',
             [Column, ciFileBlockColumns]);

  { Does this cell come from the common block view? }
  if Row < ciFileBlockRows then
    Result := inherited GetPropertyCell(Row, Column)
  else
    case Row of
      5 : if Column = 0 then
            Result := 'Block size'
          else
            Result := IntToStr(GetBlockSize);
      6 : if Column = 0 then
            Result := 'Encrypted?'
          else
            Result := YesNoValue(GetEncrypted);
      7 : if Column = 0 then
            Result := 'Log 2 block size'
          else
            Result := IntToStr(GetLog2BlockSize);
      8 : if Column = 0 then
            Result := 'Used blocks'
          else
            Result := IntToStr(GetUsedBlocks);
      9 : if Column = 0 then
            Result := 'Available blocks'
          else
            Result := IntToStr(GetAvailBlocks);
      10: if Column = 0 then
            Result := '1st free block'
          else
            Result := IntToStr(GetFirstFreeBlock);
      11: if Column = 0 then
            Result := 'Record count'
          else
            Result := IntToStr(GetRecordCount);
      12: if Column = 0 then
            Result := 'Deleted record count'
          else
            Result := IntToStr(GetDeletedRecordCount);
      13: if Column = 0 then
            Result := '1st deleted record'
          else
            Result := Int64ToStr(GetFirstDeletedRecord);
      14: if Column = 0 then
            Result := 'Record length'
          else
            Result := IntToStr(GetRecordLength);
      15: if Column = 0 then
            Result := 'Record length plus trailer'
          else
            Result := IntToStr(GetRecLenPlusTrailer);
      16: if Column = 0 then
            Result := 'Records per block'
          else
            Result := IntToStr(GetRecordsPerBlock);
      17: if Column = 0 then
            Result := '1st data block'
          else
            Result := IntToStr(GetFirstDataBlock);
      18: if Column = 0 then
            Result := 'Last data block'
          else
            Result := IntToStr(GetLastDataBlock);
      19: if Column = 0 then
            Result := 'BLOB count'
          else
            Result := IntToStr(GetBLOBCount);
      20: if Column = 0 then
            Result := 'Deleted BLOB head'
          else
            Result := Int64ToStr(GetDeletedBLOBHead);
      21: if Column = 0 then
            Result := 'Deleted BLOB tail'
          else
            Result := Int64ToStr(GetDeletedBLOBTail);
      22: if Column = 0 then
            Result := 'Last autoinc value'
          else
            Result := IntToStr(GetLastAutoIncValue);
      23: if Column = 0 then
            Result := 'Index count'
          else
            Result := IntToStr(GetIndexCount);
      24: if Column = 0 then
            Result := 'Sequential index?'
          else
            Result := YesNoValue(GetHasSequentialIndex);
      25: if Column = 0 then
            Result := 'Index header block number'
          else
            Result := IntToStr(GetIndexHeaderBlockNum);
      26: if Column = 0 then
            Result := 'Field count'
          else
            Result := IntToStr(GetFieldCount);
      27: if Column = 0 then
            Result := 'Data dictionary block number'
          else
            Result := IntToStr(GetDataDictBlockNum);
      28: if Column = 0 then
            Result := 'FF version'
          else
            Result := VersionToStr(GetFFVersion);
    else
      raise Exception.CreateFmt
              ('Cannot ask for cell in row %d when there are only %d rows in the view',
               [Row, ciFileBlockRows + ciFileHeaderRows]);
    end;  { case }
end;
{--------}
function TffFileHeaderBlock.GetPropertyRowCount : Integer;
begin
  Result := ciFileBlockRows + ciFileHeaderRows;
end;
{--------}
procedure TffFileHeaderBlock.SetFirstDataBlock(const Value : TffWord32);
begin
  PffBlockHeaderFile(FBlock)^.bhf1stDataBlock := Value;
end;
{--------}
procedure TffFileHeaderBlock.SetFirstFreeBlock(const Value : TffWord32);
begin
  PffBlockHeaderFile(FBlock)^.bhf1stFreeBlock := Value;
end;
{--------}
procedure TffFileHeaderBlock.SetHasSequentialIndex(const Value : Longint);
begin
  PffBlockHeaderFile(FBlock)^.bhfHasSeqIndex := Value;
end;
{--------}
procedure TffFileHeaderBlock.SetLastDataBlock(const Value : TffWord32);
begin
  PFfBlockHeaderFile(FBlock)^.bhfLastDataBlock := Value;
end;
{--------}
procedure TffFileHeaderBlock.SetLog2BlockSize(const Value : TffWord32);
begin
  PFfBlockHeaderFile(FBlock)^.bhfLog2BlockSize := Value;
end;
{--------}
procedure TffFileHeaderBlock.SetUsedBlocks(const Value : TffWord32);
begin
  PFfBlockHeaderFile(FBlock)^.bhfUsedBlocks := Value;
end;
{--------}
procedure TffFileHeaderBlock.VerifyRepair(const Repair : Boolean);
var
  Block : PffBlock;
  RelMethod : TffReleaseMethod;
  BlockSizeValid : Boolean;
  Log2BlockSizeValid : Boolean;
  CalcValue : TffWord32;
  Modified : Boolean;
begin
  inherited;
  Modified := False;
  Log2BlockSizeValid := False;
  try

    { TODO: AvailBlocks will be checked by repair logic once the number of deleted
      blocks has been determined. }

    { BLOBCount is not verified at this time. }

    { Verify block size is one of the accepted values. }
    BlockSizeValid := ((BlockSize = 4096) or
                       (BlockSize = 8192) or
                       (BlockSize = 16384) or
                       (BlockSize = 32768) or
                       (BlockSize = 65536));
    if not BlockSizeValid then
      DoReportError(rciInvalidBlockSize, [BlockSize]);
      { Future: Implement logic that tests for block size, perhaps by looking for
                valid signatures at specific block boundaries, & self repairs
                block size. }

    { Verify log2 block size. }
    if BlockSizeValid then begin
      CalcValue := FFCalcLog2BlockSize(BlockSize);
      Log2BlockSizeValid := (Log2BlockSize = CalcValue);
      if not Log2BlockSizeValid then begin
        DoReportError(rciInvalidLog2BlockSize,
                      [BlockSize, CalcValue, Log2BlockSize]);
        if Repair then begin
          BeginUpdate;
          Modified := True;
          Log2BlockSize := CalcValue;
          DoReportFix(rciInvalidLog2BlockSize, [CalcValue]);
        end;  { if }
      end;  { if }
    end;  { if }

    { Verify the reference to the data dictionary block. }
    if DataDictBlockNum <> ffc_W32NoValue then
      try
        Block := FBufMgr.GetBlock(FFileInfo, DataDictBlockNum, FTI, ffc_ReadOnly,
                                  RelMethod);
        try
          { Is it a stream block? }
          if (PffBlockHeaderStream(Block)^.bhsSignature <> ffc_SigStreamBlock) or
             (PffBlockHeaderStream(Block)^.bhsStreamType <> ffc_SigDictStream) then
            DoReportError(rciInvalidBlockRefDict, [DataDictBlockNum]);
        finally
          RelMethod(Block);
        end;
      except
        DoReportError(rciInvalidBlockRefDict, [DataDictBlockNum]);
      end
    else
      DoReportError(rciNoDictBlock, [DataDictBlockNum]);

    { Is the deleted BLOB head valid?
      Future: Determine if it is a BLOB segment. }
    if (DeletedBLOBHead.iLow <> ffc_W32NoValue) and
       Log2BlockSizeValid and
       (not FFVerifyBLOBNr(DeletedBLOBHead, Log2BlockSize)) then
      DoReportError(rciInvalidInt64, ['Deleted BLOB head', DeletedBLOBHead.iHigh,
                                      DeletedBLOBHead.iLow]);

    { Is the deleted BLOB tail valid?
      Future: Determine if it is a BLOB segment. }
    if (DeletedBLOBTail.iLow <> ffc_W32NoValue) and
       Log2BlockSizeValid and
       (not FFVerifyBLOBNr(DeletedBLOBTail, Log2BlockSize)) then
      DoReportError(rciInvalidInt64, ['Deleted BLOB tail', DeletedBLOBTail.iHigh,
                                      DeletedBLOBTail.iLow]);


    { Future: Verify deleted record count. }

    { Future: Verify encrypted flag. }

    { Future: Verify FF version. }

    { Future: Verify field count. }

    { Is FirstDataBlock valid? }
    if FirstDataBlock <> ffc_W32NoValue then
      try
        Block := FBufMgr.GetBlock(FFileInfo, FirstDataBlock, FTI, ffc_ReadOnly,
                                  RelMethod);
        try
          { Is it a data block? }
          if (PffBlockHeaderData(Block)^.bhdsignature <> ffc_SigDataBlock) then
            DoReportError(rciInvalidBlockRefFirstData, [FirstDataBlock]);
        finally
          RelMethod(Block);
        end;
      except
        DoReportError(rciInvalidBlockRefFirstData, [FirstDataBlock]);
      end
    else if RecordCount > 0 then
      DoReportError(rciNoDataBlockForRecs, [RecordCount]);

    { Verify ref to 1st deleted record.
      Future: Determine if it really is a deleted record. }
    if (DeletedRecordCount = 0) then begin
      if FirstDeletedRecord.iLow <> ffc_W32NoValue then
        DoReportError(rciInvalidInt64, ['First Deleted Record',
                                        FirstDeletedRecord.iHigh,
                                        FirstDeletedRecord.iLow]);
    end
    else if Log2BlockSizeValid and
            (not FFVerifyRefNr(FirstDeletedRecord, Log2BlockSize,
                               RecordLengthPlusTrailer)) then
      DoReportError(rciInvalidInt64, ['First Deleted Record',
                                      FirstDeletedRecord.iHigh,
                                      FirstDeletedRecord.iLow]);

    { Verify ref to first free block. }
    if FirstFreeBlock <> ffc_W32NoValue then
      try
        Block := FBufMgr.GetBlock(FFileInfo, FirstFreeBlock, FTI, ffc_ReadOnly,
                                  RelMethod);
        try
          { Is it a free block? }
          if (PffBlockCommonHeader(Block)^.bchsignature <> ffc_SigFreeBlock) then
            DoReportError(rciInvalidBlockRefFirstFree, [FirstFreeBlock]);
        finally
          RelMethod(Block);
        end;
      except
        DoReportError(rciInvalidBlockRefFirstFree, [FirstFreeBlock]);
      end;

    { For FF 2.x, each table should have a sequential index. }
    if HasSequentialIndex <> 1 then begin
      DoReportError(rciInvalidSeqIndexFlag, [HasSequentialIndex]);
      if Repair then begin
        BeginUpdate;
        Modified := True;
        HasSequentialIndex := 1;
        DoReportFix(rciInvalidSeqIndexFlag, [1]);
      end;  { if }
    end;  { if }

    { Future: Does the index count match the dictionary. }

    { Verify ref to index header. }
    if IndexHeaderBlockNum <> ffc_W32NoValue then
      try
        Block := FBufMgr.GetBlock(FFileInfo, IndexHeaderBlockNum, FTI,
                                  ffc_ReadOnly, RelMethod);
        try
          { Is it an index block & is its block type set to zero indicating
            a header block? }
          if (PffBlockHeaderIndex(Block)^.bhisignature <> ffc_SigIndexBlock) or
             (PffBlockHeaderIndex(Block)^.bhiBlockType <> 0) then
            DoReportError(rciInvalidBlockRefIndexHead, [IndexHeaderBlockNum]);
        finally
          RelMethod(Block);
        end;
      except
        DoReportError(rciInvalidBlockRefIndexHead, [IndexHeaderBlockNum]);
      end;

    { Future: Verify last autoinc value. }

    { Verify ref to last data block. }
    if LastDataBlock <> ffc_W32NoValue then
      try
        Block := FBufMgr.GetBlock(FFileInfo, LastDataBlock, FTI, ffc_ReadOnly,
                                  RelMethod);
        try
          { Is it a data block? }
          if (PffBlockHeaderData(Block)^.bhdsignature <> ffc_SigDataBlock) then
            DoReportError(rciInvalidBlockRefLastData, [LastDataBlock]);
        finally
          RelMethod(Block);
        end;
      except
        DoReportError(rciInvalidBlockRefLastData, [LastDataBlock]);
      end
    else if RecordCount > 0 then
      DoReportError(rciNoLastDataBlockForRecs, [RecordCount]);

    { Future: Verify record length plus trailer. }

    { Future: Verify record count. }

    { Future: Verify record length. }
    { TODO:: Can now get this information from the data dictionary. ]

    { Future: Verify records per block. }

    { Verify that used blocks matches the size of the file. }
    if BlockSizeValid then begin
      CalcValue := EstimatedUsedBlocks;
      if CalcValue <> UsedBlocks then begin
        DoReportError(rciInvalidUsedBlocks, [CalcValue, UsedBlocks]);
        if Repair then begin
          BeginUpdate;
          Modified := True;
          UsedBlocks := CalcValue;
          DoReportFix(rciInvalidUsedBlocks, [CalcValue]);
        end;  { if }
      end;  { if }
    end;  { if }
  finally
    if Modified then
      EndUpdate;
  end;
end;
{====================================================================}

{====================================================================}
function TffStreamBlock.GetNextStrmBlock : TffWord32;
begin
  Result := PffBlockHeaderStream(FBlock)^.bhsNextStrmBlock;
end;
{--------}
function TffStreamBlock.GetStreamType : Longint;
begin
  Result := PffBlockHeaderStream(FBlock)^.bhsStreamType;
end;
{--------}
function TffStreamBlock.GetStreamLength : Longint;
begin
  Result := PffBlockHeaderStream(FBlock)^.bhsStreamLength;
end;
{--------}
function TffStreamBlock.GetOwningStream : Longint;
begin
  Result := PffBlockHeaderStream(FBlock)^.bhsOwningStream;
end;
{--------}
function TffStreamBlock.GetPropertyCell(const Row, Column : Integer) : string;
begin
  if Column > Pred(ciFileBlockColumns) then
    raise Exception.CreateFmt
            ('Cannot ask for cell in column %d when there are only %d columns in the view',
             [Column, ciFileBlockColumns]);

  { Does this cell come from the common block view? }
  if Row < ciFileBlockRows then
    Result := inherited GetPropertyCell(Row, Column)
  else
    case Row of
      5 : if Column = 0 then
            Result := 'Next stream block'
          else
            Result := IntToStr(GetNextStrmBlock);
      6 : if Column = 0 then
            Result := 'Stream type'
          else
            Result := MapSigToStr(GetStreamType);
      7 : if Column = 0 then
            Result := 'Stream length'
          else
            Result := IntToStr(GetStreamLength);
      8 : if Column = 0 then
            Result := 'Owning stream'
          else
            Result := IntToStr(GetOwningStream);
    else
      raise Exception.CreateFmt
              ('Cannot ask for cell in row %d when there are only %d rows in the view',
               [Row, ciFileBlockRows + ciStreamRows]);
    end;  { case }
end;
{--------}
function TffStreamBlock.GetPropertyRowCount : Integer;
begin
  Result := ciFileBlockRows + ciStreamRows;
end;
{====================================================================}

{===TffIndexBlock====================================================}
function TffIndexBlock.GetIndexBlockType : Byte;
begin
  Result := PffBlockHeaderIndex(FBlock)^.bhiBlockType;
end;
{--------}
function TffIndexBlock.GetIsLeafPage : Boolean;
begin
  Result := PffBlockHeaderIndex(FBlock)^.bhiIsLeafPage;
end;
{--------}
function TffIndexBlock.GetNodeLevel : Byte;
begin
  Result := PffBlockHeaderIndex(FBlock)^.bhiNodeLevel;
end;
{--------}
function TffIndexBlock.GetKeysAreRefs : Boolean;
begin
  Result := PffBlockHeaderIndex(FBlock)^.bhiKeysAreRefs;
end;
{--------}
function TffIndexBlock.GetIndexNum : Word;
begin
  Result := PffBlockHeaderIndex(FBlock)^.bhiIndexNum;
end;
{--------}
function TffIndexBlock.GetKeyLength : Word;
begin
  Result := PffBlockHeaderIndex(FBlock)^.bhiKeyLength;
end;
{--------}
function TffIndexBlock.GetKeyCount : Longint;
begin
  Result := PffBlockHeaderIndex(FBlock)^.bhiKeyCount;
end;
{--------}
function TffIndexBlock.GetMaxKeyCount : Longint;
begin
  Result := PffBlockHeaderIndex(FBlock)^.bhiMaxKeyCount;
end;
{--------}
function TffIndexBlock.GetPrevPageRef : TffWord32;
begin
  Result := PffBlockHeaderIndex(FBlock)^.bhiPrevPageRef;
end;
{--------}
function TffIndexBlock.GetPropertyCell(const Row, Column : Integer) : string;
begin
  if Column > Pred(ciFileBlockColumns) then
    raise Exception.CreateFmt
            ('Cannot ask for cell in column %d when there are only %d columns in the view',
             [Column, ciFileBlockColumns]);

  { Does this cell come from the common block view? }
  if Row < ciFileBlockRows then
    Result := inherited GetPropertyCell(Row, Column)
  else
    case Row of
      5 : if Column = 0 then
            Result := 'Index block type'
          else
            Result := FlagStr(GetIndexBlockType, 'Header', 'B-Tree page');
      6 : if Column = 0 then
            Result := 'Is leaf page'
          else
            Result := BooleanValue('Yes', 'No', GetIsLeafPage);
      7 : if Column = 0 then
            Result := 'Node level'
          else
            Result := IntToStr(GetNodeLevel);
      8 : if Column = 0 then
            Result := 'Keys are refs'
          else
            Result := BooleanValue('Yes', 'No', GetKeysAreRefs);
      9 : if Column = 0 then
            Result := 'Index number'
          else
            Result := IntToStr(GetIndexNum);
      10: if Column = 0 then
            Result := 'Key length'
          else
            Result := IntToStr(GetKeyLength);
      11: if Column = 0 then
            Result := 'Key count'
          else
            Result := IntToStr(GetKeyCount);
      12: if Column = 0 then
            Result := 'Max key count'
          else
            Result := IntToStr(GetMaxKeyCount);
      13: if Column = 0 then
            Result := 'Previous page reference'
          else
            Result := IntToStr(GetPrevPageRef);
   else
      raise Exception.CreateFmt
              ('Cannot ask for cell in row %d when there are only %d rows in the view',
               [Row, ciFileBlockRows + ciIndexBlockRows]);
    end;  { case }
end;
{--------}
function TffIndexBlock.GetPropertyRowCount : Integer;
begin
  Result := ciFileBlockRows + ciIndexBlockRows;
end;
{--------}
procedure TffIndexBlock.VerifyRepair(const Repair : Boolean);
var
  Inx : Integer;
  InxBlockNum,
  DataBlockNum : TffWord32;
  RefNum, TempI64 : TffInt64;
  PageNumBlock : PPageNumBlock;
  Modified : Boolean;
  Block : PffBlock;
  RelMethod : TffReleaseMethod;
  DataRefBlock : PRefBlock;
  ValidStr : string;
  Info : TffGeneralFileInfo;
begin
  inherited;
  Modified := False;
  try

    { Get the previous page & verify it is an index block. }
    if PrevPageRef <> ffc_W32NoValue then
      try
        Block := FBufMgr.GetBlock(FFileInfo, PrevPageRef, FTI, ffc_ReadOnly,
                                  RelMethod);
        try
          if PffBlockHeaderIndex(Block)^.bhiSignature <> ffc_SigIndexBlock then
            DoReportError(rciInvalidInxPrefPageRef, [PrevPageRef]);
        finally
          RelMethod(Block);
        end;
      except
        DoReportError(rciInvalidInxPrefPageRef, [PrevPageRef]);
      end;

    { Get the general file info. }
    if Assigned(FOnGetInfo) then
      FOnGetInfo(Info)
    else
      raise Exception.Create('File interface must provide OnGetInfo handler.');

    { Is this a leaf page? }
    if IsLeafPage then begin
      { Yes.  Verify that all reference numbers point to data pages & to
        valid records. }
      DataRefBlock := PRefBlock(@FBlock^[ffc_BlockHeaderSizeIndex]);

      { Loop through the existing keys. }
      for Inx := 0 to pred(KeyCount) do begin
        { Get the block number. }
        RefNum := DataRefBlock^[Inx];
        ffShiftI64R(RefNum, FFileInfo^.fiLog2BlockSize, TempI64);
        DataBlockNum := TempI64.iLow;

        { Load the page.  Is it a data block? }
        try
          Block := FBufMgr.GetBlock(FFileInfo, DataBlockNum, FTI, ffc_ReadOnly,
                                    RelMethod);
          try
            if PffBlockHeaderData(Block)^.bhdSignature <> ffc_SigDataBlock then begin
              { It is not a data block. Determine the validity of the key's
                ref number. If it is valid then it will point to the
                start of a record given the block size & record length. }
              if FFVerifyRefNr(RefNum, Info.Log2BlockSize,
                               Info.RecLenPlusTrailer) then
                ValidStr := 'The RefNum is valid.'
              else
                ValidStr := 'The RefNum is invalid.';
              DoReportError(rciInvalidLeafKeyBlockRef,
                            [Inx, BlockNum, IndexNum, DataBlockNum,
                             RefNum.iHigh, RefNum.iLow, ValidStr]);
            end
            else begin
              { It is a data block. Verify the key in the index page points
                to a valid record. }
              if not FFVerifyRefNr(RefNum, Info.Log2BlockSize,
                                   Info.RecLenPlusTrailer) then
                DoReportError(rciInvalidLeafKeyRefNum,
                              [Inx, BlockNum, IndexNum, DataBlockNum,
                               RefNum.iHigh, RefNum.iLow]);
            end;  { if..else }
          finally
            RelMethod(Block);
          end;
        except
          ValidStr := 'The RefNum validity is undetermined.';
          DoReportError(rciInvalidLeafKeyBlockRef,
                        [Inx, BlockNum, IndexNum, DataBlockNum,
                         RefNum.iHigh, RefNum.iLow, ValidStr]);
        end;
      end;  { for }
    end
    else begin
      { This is an internal page.  Verify the following:
        1. The referenced parent page actually exists and is an index block.
        2. Each referenced subpage actually exists and is an index block.

        First, get a handle on the page numbers and reference numbers.
        Page numbers point to an index page (used if the key searched for is
          less than the key at this spot).
        Reference numbers point to a data page (use if we have found the key
          we are searching for in the node page). }
      PageNumBlock := PPageNumBlock(@FBlock^[ffc_BlockHeaderSizeIndex]);
      DataRefBlock := PRefBlock(@FBlock^[ffc_BlockHeaderSizeIndex +
                                         (MaxKeyCount * SizeOfPageNum)]);

      { Now loop through the existing keys. }
      for Inx := 0 to pred(KeyCount) do begin
        { Get the index block number. }
        InxBlockNum := PageNumBlock^[Inx];
        RefNum := DataRefBlock^[Inx];
        try
          { Load the referenced index block. Is it an index page? }
          Block := FBufMgr.GetBlock(FFileInfo, InxBlockNum, FTI, ffc_ReadOnly,
                                    RelMethod);
          try
            if PffBlockHeaderIndex(Block)^.bhiSignature <> ffc_SigIndexBlock then begin
              { No, it is not an index page. Determine the validity of the
                reference number. It is valid if it points to the start of a
                record given the block size & record length. }
              if FFVerifyRefNr(RefNum, Info.Log2BlockSize,
                               Info.RecLenPlusTrailer) then
                ValidStr := 'The RefNum is valid.'
              else
                ValidStr := 'The RefNum is invalid.';
              DoReportError(rciInvalidIntrnalKeyBlockRef,
                            [Inx, BlockNum, IndexNum, InxBlockNum,
                             RefNum.iHigh, RefNum.iLow, ValidStr]);
            end
            else begin
              { Yes, the target page is an index page.  Now verify this key points
                to a valid record. }
              if not FFVerifyRefNr(RefNum, Info.Log2BlockSize,
                                   Info.RecLenPlusTrailer) then
                DoReportError(rciInvalidIntrnalKeyRefNum,
                              [Inx, BlockNum, IndexNum, InxBlockNum,
                               RefNum.iHigh, RefNum.iLow]);
            end;  { if }
          finally
            RelMethod(Block);
          end;
        except
          ValidStr := 'The RefNum validity is undetermined.';
          DoReportError(rciInvalidIntrnalKeyBlockRef,
                        [Inx, BlockNum, IndexNum, InxBlockNum,
                         RefNum.iHigh, RefNum.iLow, ValidStr]);
        end;
      end;  { for }
    end;  { if..else }
  finally
    if Modified then
      EndUpdate;
  end;
end;
{====================================================================}

{===TffIndexHeaderBlock==============================================}
constructor TffIndexHeaderBlock.Create(BufMgr : TffBufferManager;
                                       FileInfo : PffFileInfo;
                                       TI : PffTransInfo;
                                 const BlockNum : TffWord32);
begin
  inherited;
  FDataColumns := -1;
  FIndexHead := PffIndexHeader(@FBlock^[ffc_BlockHeaderSizeIndex]);
end;
{--------}
function TffIndexHeaderBlock.GetDataCell(const Row, Column : Integer) : string;
begin
  if Column > Pred(ciIndexBlockRows) then
    raise Exception.CreateFmt
            ('Cannot ask for cell in column %d when there are only %d columns in the view',
             [Column, ciFileBlockColumns]);

  case Column of
    0 : Result := IntToStr(Row + 1);
    1 : Result := IntToStr(FIndexHead^.bihIndexKeyLen[Row]);
    2 : Result := IntToStr(FIndexHead^.bihIndexKeyCount[Row]);
    3 : Result := IntToStr(FIndexHead^.bihIndexRoot[Row]);
    4 : Result := IntToStr(FIndexHead^.bihIndexPageCount[Row]);
    5 : Result := MapFlagsToStr(FIndexHead^.bihIndexFlags[Row]);
  else
    raise Exception.CreateFmt
            ('Cannot ask for cell in row %d when there are only %d rows in the view',
             [Row, ffcl_MaxIndexes]);
  end;  { case }
end;
{--------}
function TffIndexHeaderBlock.GetDataColCaption(const Index : Integer) : string;
begin
  case Index of
    0 : Result := 'Index';
    1 : Result := 'Key length';
    2 : Result := '# keys';
    3 : Result := 'Root page';
    4 : Result := '# pages';
    5 : Result := 'Flags';
  else
    raise Exception.CreateFmt
            ('Cannot ask for caption %d when there are only %d columns in the view',
             [Index, ciIndexHeaderDataColumns]);
  end;  { case }
end;
{--------}
function TffIndexHeaderBlock.GetDataColCount : Integer;
begin
  Result := ciIndexHeaderDataColumns;
end;
{--------}
function TffIndexHeaderBlock.GetDataColWidth(const Index : Integer) : Integer;
begin
  case Index of
    0 : Result := 50;
    1 : Result := 65;
    2 : Result := 65;
    3 : Result := 75;
    4 : Result := 65;
    5 : Result := 90;
  else
    raise Exception.CreateFmt
            ('Cannot ask for width %d when there are only %d columns in the view',
             [Index, ciIndexHeaderDataColumns]);
  end;  { case }
end;
{--------}
function TffIndexHeaderBlock.GetDataRowCount : Integer;
var
  Inx : Integer;
begin
  if FDataColumns < 0 then begin
    FDataColumns := 0;
    for Inx := 0 to Pred(ffcl_MaxIndexes) do
      if FIndexHead^.bihIndexKeyLen[Inx] > 0 then
        inc(FDataColumns)
      else
        Break;
  end;  { if }
  Result := FDataColumns;
    { Future: Obtain # of indices from dictionary or file header block. }
end;
{--------}
procedure TffIndexHeaderBlock.VerifyRepair(const Repair : Boolean);
var
  Block : PffBlock;
  Modified : Boolean;
  Row,
  Rows : Integer;
  Info : TffGeneralFileInfo;
  RelMethod : TffReleaseMethod;
begin
  { Verify an OnGetInfo handler has been specified. }
  if Assigned(FOnGetInfo) then
    FOnGetInfo(Info)
  else
    raise Exception.Create('File interface must provide OnGetInfo handler.');

  Modified := False;
  try
    { Get the # of rows in the header. The # of rows should equal the # of
      indices defined in the dictionary. }
    Rows := GetDataRowCount;
    if Rows <> Info.Dict.IndexCount then
      DoReportError(rciInxHeaderInvalidRowCount,
                    [Rows, Info.Dict.IndexCount])
    else begin
      { Walk through each row. }
      for Row := 0 to Pred(Rows) do begin
        { Verify the index key length. }
        if FIndexHead^.bihIndexKeyLen[Row] <> Info.Dict.IndexKeyLength[Row] then
          DoReportError(rciInxHeaderInvalidKeyLen,
                        [Row, FIndexHead^.bihIndexKeyLen[Row],
                         Info.Dict.IndexKeyLength[Row]]);

        { Verify the index key count matches the number of records in the
          table.
          Future: This test would change if there were ever a type of index
          that filtered out keys. }
        if FIndexHead^.bihIndexKeyCount[Row] <> Info.RecordCount then
          DoReportError(rciInxHeaderInvalidKeyCount,
                        [Row, FIndexHead^.bihIndexKeyCount[Row],
                         Info.RecordCount]);

        { There are no records in the table. Verify the index map does not
          point to an index page. }
        if (Info.RecordCount = 0) then begin
          if FIndexHead^.bihIndexRoot[Row] <> ffc_W32NoValue then
            DoReportError(rciInxHeaderInvalidRootPage,
                          [Row, FIndexHead^.bihIndexRoot[Row]]);
        end
        else if (FIndexHead^.bihIndexRoot[Row] <> ffc_W32NoValue) then
          { There are records. Verify the index root page is really an index
            block. }
          try
            Block := FBufMgr.GetBlock(FFileInfo, FIndexHead^.bihIndexRoot[Row],
                                      FTI, ffc_ReadOnly, RelMethod);
            try
              { Is it an index block? }
              if (PffBlockHeaderIndex(Block)^.bhisignature <> ffc_SigIndexBlock) then
                DoReportError(rciInxHeaderInvalidRootPage,
                              [Row, FIndexHead^.bihIndexRoot[Row]]);
            finally
              RelMethod(Block);
            end;
          except
            DoReportError(rciInxHeaderInvalidRootPage, [FIndexHead^.bihIndexRoot[Row]]);
          end
        else
          DoReportError(rciInxHeaderNoRootPage, [Row]);

        { Future: Verify index page count. }

        { Verify index flags. If this is the first row then it should indicate
          that keys are refs. }
        if (Row = 0) then
          if (FIndexHead^.bihIndexFlags[Row] and ffc_InxFlagKeysAreRefs) <>
             ffc_InxFlagKeysAreRefs then
            DoReportError(rciInxHeaderNoRefsFlag, []);

        if Info.Dict.IndexDescriptor[Row].idDups then
          if (FIndexHead^.bihIndexFlags[Row] and ffc_InxFlagAllowDups) <>
             ffc_InxFlagAllowDups then
            DoReportError(rciInxHeaderNoDupsFlag, [Row, Row]);


      end;  { for }
    end;
  finally
    if Modified then
      EndUpdate;
  end;
end;
{====================================================================}

{===TffDataBlock=====================================================}
function TffDataBlock.GetDataCell(const Row, Column : Integer) : string;
var
  FieldValue : TffVCheckValue;
  IsNull : Boolean;
  Info : TffGeneralFileInfo;
  RecPtrDel,
  RecPtrData : PffByteArray;
  Offset : Integer;
begin
  if Row > Pred(GetRecCount) then
    raise Exception.CreateFmt
            ('Cannot ask for cell in row %d when there are only %d records in the view',
             [Row, GetRecCount]);

  { Get the general file info. }
  if Assigned(FOnGetInfo) then
    FOnGetInfo(Info)
  else
    raise Exception.Create('File interface must provide OnGetInfo handler.');

  if Column < FNumDataColumns then begin
    Result := '-';
    FillChar(FieldValue, SizeOf(FieldValue), 0);
    { Position two pointers to the beginning of the record. The first points
      to the deleted flag. The second points to the start of the record. }
    Offset := ffc_BlockHeaderSizeData + (Info.RecLenPlusTrailer * Row);
    RecPtrDel := @FBlock[Offset];
    RecPtrData := @FBlock[Offset + 1];

    { Is the record deleted? }
    if Column = 0 then
      Result := IntToStr(Row)
    else if PByte(RecPtrDel)^ = $FF then begin
      if Column = 1 then
        Result := 'Y';
    end
    else if Column > 1 then begin
      Info.Dict.GetRecordField(Column - 2, RecPtrData, IsNull, @FieldValue);
      if IsNull then
        Result := '<null>'
      else
        Result := FFVCheckValToString(FieldValue,
                                      Info.Dict.FieldType[Column - 2]);
    end;  { if..else }
  end
  else
    raise Exception.CreateFmt
            ('Cannot ask for cell in column %d when there are only %d columns in the view',
             [Column, FNumDataColumns]);
end;
{--------}
function TffDataBlock.GetDataColCaption(const Index : Integer) : string;
var
  Info : TffGeneralFileInfo;
begin
  if Index < FNumDataColumns then begin
    { Get the general file info. }
    if Assigned(FOnGetInfo) then
      FOnGetInfo(Info)
    else
      raise Exception.Create('File interface must provide OnGetInfo handler.');
    if Index = 0 then
      Result := 'Slot'
    else if Index = 1 then
      Result := 'Deleted?'
    else
      Result := Info.Dict.FieldName[Index - 2];
  end
  else
    raise Exception.CreateFmt
            ('Cannot ask for caption %d when there are only %d columns in the view',
             [Index, ciIndexHeaderDataColumns]);
end;
{--------}
function TffDataBlock.GetDataColCount : Integer;
var
  Info : TffGeneralFileInfo;
begin
  { Get the general file info. }
  if Assigned(FOnGetInfo) then
    FOnGetInfo(Info)
  else
    raise Exception.Create('File interface must provide OnGetInfo handler.');
  Result := Info.Dict.FieldCount + 2;
    { The first extra column is the slot # of the record (base 0) & the second
      extra column used to indicate whether the record is deleted. }
  FNumDataColumns := Result;
end;
{--------}
function TffDataBlock.GetDataColWidth(const Index : Integer) : Integer;
begin
  if Index < FNumDataColumns then begin
    if Index = 1 then
      Result := 70
    else
      Result := 50
  end
  else
    raise Exception.CreateFmt
            ('Cannot ask for width %d when there are only %d columns in the view',
             [Index, FNumDataColumns]);
end;
{--------}
function TffDataBlock.GetDataRowCount : Integer;
begin
  Result := GetRecCount;
end;
{--------}
function TffDataBlock.GetNextDataBlock : TffWord32;
begin
  Result := PffBlockHeaderData(FBlock)^.bhdNextDataBlock;
end;
{--------}
function TffDataBlock.GetPrevDataBlock : TffWord32;
begin
  Result := PffBlockHeaderData(FBlock)^.bhdPrevDataBlock;
end;
{--------}
function TffDataBlock.GetPropertyCell(const Row, Column : Integer) : string;
begin
  if Column > Pred(ciFileBlockColumns) then
    raise Exception.CreateFmt
            ('Cannot ask for cell in column %d when there are only %d columns in the view',
             [Column, ciFileBlockColumns]);

  { Does this cell come from the common block view? }
  if Row < ciFileBlockRows then
    Result := inherited GetPropertyCell(Row, Column)
  else
    case Row of
      5 : if Column = 0 then
            Result := 'Record count'
          else
            Result := IntToStr(GetRecCount);
      6 : if Column = 0 then
            Result := 'Record length'
          else
            Result := IntToStr(GetRecLen);
      7 : if Column = 0 then
            Result := 'Next data block'
          else
            Result := IntToStr(GetNextDatablock);
      8 : if Column = 0 then
            Result := 'Previous data block'
          else
            Result := IntToStr(GetPrevDataBlock);
   else
      raise Exception.CreateFmt
              ('Cannot ask for cell in row %d when there are only %d rows in the view',
               [Row, ciFileBlockRows + ciDataBlockRows]);
    end;  { case }
end;
{--------}
function TffDataBlock.GetPropertyRowCount : Integer;
begin
  Result := ciFileBlockRows + ciDataBlockRows;
end;
{--------}
function TffDataBlock.GetRecCount : Longint;
begin
  Result := PffBlockHeaderData(FBlock)^.bhdRecCount;
end;
{--------}
function TffDataBlock.GetRecLen : Longint;
begin
  Result := PffBlockHeaderData(FBlock)^.bhdRecLength;
end;
{--------}
{ The following code was copied from unit FFTBBLOB. }
function TffDataBlock.IsEmptyLookupEntry(Entry : PffBLOBLookupEntry) : Boolean;
const
  ciEmptyVal1 = 808464432;
    { This is because the lookup segments are fillchar'd with 'O' instead of 0.
      We have to check all 3 fields in the lookup entry for this value so that
      we avoid a case where the value is valid. }
  ciEmptyVal2 = 1179010630;
    { Another value that indicates an empty lookup entry. }
begin
  Result := ((Entry^.bleSegmentOffset.iLow = ciEmptyVal1) and
             (Entry^.bleSegmentOffset.iHigh = ciEmptyVal1) and
             (Entry^.bleContentLength = ciEmptyVal1)) or
            ((Entry^.bleSegmentOffset.iLow = ciEmptyVal2) and
             (Entry^.bleSegmentOffset.iHigh = ciEmptyVal2) and
             (Entry^.bleContentLength = ciEmptyVal2));
end;
{--------}
procedure TffDataBlock.SetNextDataBlock(const Value : TffWord32);
begin
  PffBlockHeaderData(FBlock)^.bhdNextDataBlock := Value;
end;
{--------}
procedure TffDataBlock.SetPrevDataBlock(const Value : TffWord32);
begin
  PffBlockHeaderData(FBlock)^.bhdPrevDataBlock := Value;
end;
{--------}
procedure TffDataBlock.SetRecCount(const Value : Longint);
begin
  PffBlockHeaderData(FBlock)^.bhdRecCount := Value;
end;
{--------}
procedure TffDataBlock.SetRecLen(const Value : Longint);
begin
  PffBlockHeaderData(FBlock)^.bhdRecLength := Value;
end;
{--------}
procedure TffDataBlock.VerifyBLOB(const BLOBNr : TffInt64;
                                    var ErrCode : Integer);
var
  BLOBBlock     : PffBlock;
  BLOBBlockHdr  : PffBlockHeaderBLOB absolute BLOBBlock;
  BLOBBlockNum  : TffWord32;
  BLOBHeader    : PffBLOBHeader;
  EntryCount    : Integer;
  LookupBlock, ContentBlock   : TffWord32;
  LookupEntry   : PffBLOBLookupEntry;
  ContentEntry : PffBLOBSegmentHeader;
  LookupSegBlk, ContentSegBlk  : PffBlock;
  LookupSegPtr  : PffBLOBSegmentHeader;
  NextSeg       : TffInt64;
  OffsetInBlock, ContentOffsetInBlock : TffWord32;
  aLkpRelMethod,
  aContRelMethod,                                                    
  aHdRelMethod  : TffReleaseMethod;
  ByteCount,
  CurByteCount : Longint;
begin
  ErrCode := 0;
  CurByteCount := 0;
  LookupSegBlk := nil;

  { Read and verify the BLOB header block for this BLOB number. }
  try
    BLOBBlock := ReadVfyBlobBlock2(FFileInfo,
                                   FTI,
                                   ffc_ReadOnly,
                                   BLOBNr,
                                   BLOBBlockNum,
                                   OffsetInBlock,
                                   aHdRelMethod);
  except
    ErrCode := rciBLOBInvalidRefNr;
    Exit;
  end;

  BLOBHeader := @BLOBBlock^[OffsetInBlock];

  { Verify the BLOB has not been deleted. }
  if (BLOBHeader^.bbhSignature = ffc_SigBLOBSegDeleted) then begin
    ErrCode := rciBLOBDeleted;
    Exit;
  end
  else if (BLOBHeader^.bbhSignature <> ffc_SigBLOBSegHeader) then begin
    { The BLOB header has an invalid signature. }
    ErrCode := rciBLOBHeaderSignature;
    Exit;
  end
  else if BLOBHeader^.bbh1stLookupSeg.iLow = ffc_W32NoValue then
    { The BLOB has been truncated to length zero. This is a valid situation &
      there is nothing else to do. }
    Exit
  else if (BLOBHeader^.bbhSegCount = ffc_FileBLOB) or
          (BLOBHeader^.bbhSegCount = ffc_BLOBLink) then
    { This is a file BLOB or a BLOB link. There is nothing else to do so
      exit the routine. }
    Exit;

  ByteCount := BLOBHeader^.bbhBLOBLength;
  try
    { Get the lookup segment block and set up offset for 1st lookup entry. }
    try
      LookupSegBlk := ReadVfyBlobBlock2(FFileInfo, FTI, ffc_ReadOnly,
                                        BLOBHeader^.bbh1stLookupSeg,
                                        LookupBlock, OffsetInBlock,
                                        aLkpRelMethod);
    except
      ErrCode := rciBLOBInvalidLookupRefNr;
      Exit;
    end;
    LookupSegPtr := @LookupSegBlk^[OffsetInBlock];
    OffsetInBlock := OffsetInBlock + sizeof(TffBLOBSegmentHeader);

    EntryCount := 0;
    while True do begin
      inc(EntryCount);
      LookupEntry := @LookupSegBlk^[OffsetInBlock];
      { If there are no more lookup entries then verification has finished. }
      if (CurByteCount >= ByteCount) or
         IsEmptyLookupEntry(LookupEntry) then
        Exit;

      inc(CurByteCount, LookupEntry^.bleContentLength);

      { Verify the segment is valid. }
      ContentSegBlk := nil;
      aContRelMethod := nil;
      try
        ContentSegBlk := ReadVfyBlobBlock2(FFileInfo, FTI, ffc_ReadOnly,
                                           LookupEntry^.bleSegmentOffset,
                                           ContentBlock, ContentOffsetInBlock,
                                           aContRelMethod);
      except
        ErrCode := rciBLOBInvalidContentRefNr;
        Exit;
      end;

      try
        ContentEntry := @ContentSegBlk^[ContentOffsetInBlock];
        if PffBlockHeaderBLOB(ContentSegBlk)^.bhbSignature <> ffc_SigBLOBBlock then begin
          ErrCode := rciBLOBContentBlockSignature;
          Exit;
        end
        else if ContentEntry^.bshSignature <> ffc_SigBLOBSegContent then begin
          ErrCode := rciBLOBContentSegSignature;
          Exit;
        end
        else begin
          { See if we're at the end of the lookup segment. }
          if (LookupSegPtr^.bshSegmentLen <
             (sizeof(TffBLOBSegmentHeader) +
             (succ(EntryCount) * sizeof(TffBLOBLookupEntry)))) then begin
            NextSeg := LookupSegPtr^.bshNextSegment;
            if NextSeg.iLow <> ffc_W32NoValue then begin
              aLkpRelMethod(LookupSegBlk);
              try
                LookupSegBlk := ReadVfyBlobBlock2(FFileInfo, FTI, ffc_ReadOnly,
                                                  NextSeg,
                                                  LookupBlock, OffsetInBlock,
                                                  aLkpRelMethod);
              except
                ErrCode := rciBLOBInvalidLookupRefNr;
                Exit;
              end;
              LookupSegPtr := @LookupSegBlk^[OffsetInBlock];
              OffsetInBlock := OffsetInBlock + sizeof(TffBLOBSegmentHeader);
              EntryCount := 0;
            end
            else
              break;
          end else
            OffsetInBlock := OffsetInBlock + sizeof(TffBLOBLookupEntry);
        end;
      finally
        if Assigned(aContRelMethod) then
          aContRelMethod(ContentSegBlk);
      end;
    end; {while}
  finally
    if assigned(LookupSegBlk) then
      aLkpRelMethod(LookupSegBlk);
    aHdRelMethod(BLOBBlock);
  end;
end;
{--------}
procedure TffDataBlock.VerifyRepair(const Repair : Boolean);
var
  BLOBInx,
  Inx : Integer;
  IsNull,
  Modified : Boolean;
  Block : PffBlock;
  RelMethod : TffReleaseMethod;
  Info : TffGeneralFileInfo;
  RecPtrDel,
  RecPtrData : PffByteArray;
  Offset : Longint;
  BLOBNr : TffInt64;
  ErrCode : Integer;
begin
  inherited;
  Modified := False;
  try
    { Get the general file info. }
    if Assigned(FOnGetInfo) then
      FOnGetInfo(Info)
    else
      raise Exception.Create('File interface must provide OnGetInfo handler.');

    { Does the record count match the file header? }
    if RecordCount <> Info.RecordsPerBlock then begin
      DoReportError(rciInvalidDataBlockRecCount,
                    [BlockNum, RecordCount, Info.RecordsPerBlock]);
      if Repair then begin
        BeginUpdate;
        Modified := True;
        RecordCount := Info.RecordCount;
        DoReportFix(rciInvalidDataBlockRecCount, [BlockNum, RecordCount]);
      end;
    end;

    { Does the record length match? }
    if RecordLen <> Info.Dict.RecordLength then begin
      DoReportError(rciInvalidDataBlockRecLen,
                    [BlockNum, RecordLen, Info.Dict.RecordLength]);
      if Repair then begin
        BeginUpdate;
        Modified := True;
        RecordLen := Info.Dict.RecordLength;
        DoReportFix(rciInvalidDataBlockRecLen, [BlockNum, RecordLen]);
      end;
    end;

    { Verify the next data block is really a data block. }
    if NextDataBlock <> ffc_W32NoValue then begin
      try
        Block := FBufMgr.GetBlock(FFileInfo, NextDataBlock, FTI, ffc_ReadOnly,
                                  RelMethod);
        try
          if PffBlockHeaderData(Block)^.bhdSignature <> ffc_SigDataBlock then
            DoReportError(rciInvalidNextDataBlock, [BlockNum, NextDataBlock]);
        finally
          RelMethod(Block);
        end;
      except
        DoReportError(rciInvalidNextDataBlock, [BlockNum, NextDataBlock]);
      end;
    end;  { if }

    { Verify the previous data block is really a data block. }
    if PrevDataBlock <> ffc_W32NoValue then begin
      try
        Block := FBufMgr.GetBlock(FFileInfo, PrevDataBlock, FTI, ffc_ReadOnly,
                                  RelMethod);
        try
          if PffBlockHeaderData(Block)^.bhdSignature <> ffc_SigDataBlock then
            DoReportError(rciInvalidPrevDataBlock, [BlockNum, PrevDataBlock]);
        finally
          RelMethod(Block);
        end;
      except
        DoReportError(rciInvalidPrevDataBlock, [BlockNum, PrevDataBlock]);
      end;
    end;  { if }

    { If this table has BLOB fields & there is only 1 file then verify the
      BLOBs. }
    { Future: Handle BLOBs that are in a separate file. }
    if Info.Dict.HasBLOBFields and (Info.Dict.FileCount = 1) then begin
      { Loop through the records in the block. If the record is not deleted
        then check its BLOB references.
        If verifying then suggested repair method is to pack.
        However, when repairing, any invalid BLOB references will be nulled.
        Packing the table then removes the invalid BLOBs from the table. }
      Offset := ffc_BlockHeaderSizeData;
      for Inx := 0 to Pred(RecordCount) do begin
        RecPtrDel := @FBlock[Offset];
        RecPtrData := @FBlock[Offset + 1];
          { Note: Adding +1 to offset skips the leading deleted flag. }
        { Has the record been deleted? }
        if PByte(RecPtrDel)^ <> $FF then begin
          { No. Check each BLOB field. }
          for BLOBInx := 0 to Pred(Info.BLOBFieldCount) do begin
            Info.Dict.GetRecordField(Info.BLOBFields[BLOBInx],
                                     RecPtrData, IsNull, @BLOBNr);
            if not IsNull then begin
              { If have a BLOB reference then verify the BLOB. }
              VerifyBLOB(BLOBNr, ErrCode);
              { If there is an error then report it. }
              if ErrCode <> 0 then begin
                DoReportError(ErrCode,
                              [Info.BLOBFieldNames[BLOBInx],
                               BLOBNr.iHigh, BLOBNr.iLow,
                               Info.KeyFieldValues(RecPtrData),
                               Inx, BlockNum]);
                { If repairing then null out the BLOB reference. }
                if Repair then begin
                  BeginUpdate;
                  Modified := True;
                  RecPtrData := @FBlock[Offset + 1];
                  Info.Dict.SetRecordFieldNull(Info.BLOBFields[BLOBInx],
                                               RecPtrData, True);
                  DoReportFix(ErrCode, [Info.BLOBFieldNames[BLOBInx],
                                        Info.KeyFieldValues(RecPtrData),
                                        Inx, BlockNum]);
                end;
              end;
            end;  { if }
          end;  { for }
        end;  { if }
        { Move to next record. }
        inc(Offset, Info.RecLenPlusTrailer);
      end;  { for }
    end;  { if }

  finally
    if Modified then
      EndUpdate;
  end;
end;
{====================================================================}

{===TffBLOBBlock=====================================================}
procedure TffBLOBBlock.VerifyRepair(const Repair : Boolean);
begin
  inherited;
end;
{====================================================================}

{===TffStreamBlock===================================================}
procedure TffStreamBlock.VerifyRepair(const Repair : Boolean);
begin
  inherited;
end;
{====================================================================}
initialization

  Tffv2FileInterface.Register('FlashFiler 2 repair interface');

finalization

  Tffv2FileInterface.Unregister;

end.

