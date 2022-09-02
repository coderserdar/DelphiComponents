{*********************************************************}
{* FlashFiler: Temporary Storage classes                 *}
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

unit fflltemp;

interface

uses
  Windows,
  ffllbase;


type
  TffTempStorageClass = class of TffBaseTempStorage;
  TffBaseTempStorage = class(TffObject)
  protected

    tsBlockSize : TffWord32;
      {-Size of the blocks used by the temporary storage. }

    tsNumBlocks : TffWord32;
      {-The number of blocks that can be held. }

    tsSize : TffWord32;
      {-The size of the temporary storage instance, in bytes. }

    function btsGetBlockCount : TffWord32; virtual;
      {-Returns the total number of blocks that the temporary storage
        instance can hold. }

    function btsGetSize : TffWord32; virtual;
      {-Returns the size, in bytes, of the temporary storage instance. }

  public

    { Methods }

    constructor Create(configDir : TffPath; aSize : TffWord32;
                       blockSize : integer); virtual; abstract;
      { Creates an instance of temporary storage. aSize is the size of the
        storage space in bytes. Blocksize is the size of the blocks, in bytes,
        to be allocated by temporary storage. }

    function Full : boolean; virtual; abstract;
      { Returns True if temporary storage is full otherwise returns False. }

    function HasSpaceFor(const numBlocks : TffWord32) : boolean; virtual; abstract;
      { Returns True if temporary storage has space for the specified number
        of blocks. }

    procedure ReadBlock(const aBlockNum : TffWord32; aBlock : PffBlock); virtual; abstract;
      { Reads the block specified by aBlockNum from temporary storage
        & copies the block's data into aBlock. The block in the memory map
        file is unallocated and made available to another caller. }

    procedure ReleaseBlock(const aBlockNum : TffWord32); virtual; abstract;
      { Use this method to release a block previously stored via WriteBlock.
        The space occupied by the block is made available. The data written
        to the block is no longer accessible. }

    function WriteBlock(aBlock : PffBlock) : TffWord32; virtual; abstract;
      { Write a block to temporary stroage. aBlock is the block to be written.
        Returns the block number to which the block was written. When
        retrieving the block via ReadBlock, use the block number returned
        by this function. }

    {Properties}

    property BlockCount : TffWord32 read btsGetBlockCount;
      { The total number of blocks that can be held in temporary storage. }

    property Size : TffWord32 read btsGetSize;
      { The size of the temporary storage, in bytes. }

  end;

  { This class implements temporary storage using VirtualAlloc and VirtualFree.
    It maintains an internal array denoting block availability. }
  TffTempStorageVA = class(TffBaseTempStorage)
  protected

    mmArraySize : TffWord32;
      {-Size of the mmBlocks & mmCommits arrays. }

    mmAddress : PffByteArray;
      {-Pointer to the allocated region. }

    mmBlocks : PffByteArray;
      {-Array of bits used to denote block availability. One bit per 64k block. }

    mmCommits : PffByteArray;
      {-Indicates which blocks in mmBlocks have been committed in virtual
        memory. The bits in this array have a one-to-one correspondence to the
        bits in the mmBlocks array. }

    mmNextAvailBlock : TffWord32;
      {-Position of the next available block in the bit array. This value
        indicates a particular byte not a bit. }

    mmPadLock : TffPadLock;
      {-Controls access to the storage. }

    mmUseCount : TffWord32;
      {-Number of blocks used. Bounds: 0..tsNumBlocks }

    { Protected methods }
    procedure tsReleaseBlockPrim(const aBlockNum : TffWord32);

  public

    { Methods }

    constructor Create(configDir : TffPath; aSize : TffWord32;
                       aBlockSize : integer); override;

    destructor Destroy; override;

    function Full : boolean; override;
      { Returns True if temporary storage is full otherwise returns False. }

    function HasSpaceFor(const numBlocks : TffWord32) : boolean; override;
      { Returns True if temporary storage has space for the specified number
        of blocks. }

    procedure ReadBlock(const aBlockNum : TffWord32; aBlock : PffBlock); override;
      { Reads the block specified by aBlockNum from the memory map file
        and copies the block's data into aBlock. The block in the memory map
        file is unallocated and made available to another caller. }

    procedure ReleaseBlock(const aBlockNum : TffWord32); override;
      { Use this method to release a block previously stored via WriteBlock.
        The space occupied by the block is made available. The data written
        to the block is no longer accessible. }

    function WriteBlock(aBlock : PffBlock) : TffWord32; override;
      { Write a block to the file. aBlock is the block to be written.
        Returns the block number to which the block was written. When
        retrieving the block via ReadBlock, use the block number returned
        by this function. }

  end;

  { This class implements temporary storage using a memory mapped file. It
    divides itself up into blocks that are 64k bytes in size and maintains
    an internal array denoting block availability.

    This class expects to create a file on the disk. It does not support
    mapping to the Windows page file. }
  TffTempStorageMM = class(TffBaseTempStorage)
  protected
    mmArraySize : TffWord32;
      {-Size of the mmBlocks array. }

    mmBlocks : PffByteArray;
      {-Array of bits used to denote block availability. One bit per 64k block. }

    mmFileHandle : THandle;
      {-The handle to the file. }

    mmFileName : PffShStr;
      {-The path and name of the file. }

    mmMapHandle : THandle;
      {-The handle to the memory mapped file. }

    mmNextAvailBlock : TffWord32;
      {-Position of the next available block in the bit array. This value
        indicates a particular byte not a bit. }

    mmPadLock : TffPadLock;
      {-Controls access to file. }

    mmUseCount : TffWord32;
      {-Number of blocks used. Bounds: 0..tsNumBlocks }

    { Protected methods }
    function mmGetFileName : string;
      {-Returns the name of the memory mapped file. }

    procedure mmOpenFile;
      {-Creates & opens the memory mapped file. }

    procedure mmReleaseBlockPrim(const aBlockNum : TffWord32);
      {-Marks a block as available. }

  public

    { Methods }

    constructor Create(configDir : TffPath; aSize : TffWord32;
                       aBlockSize : integer); override;

    destructor Destroy; override;

    function ActualSize : DWORD;
      { Returns the actual size of the file. }

    function Full : boolean; override;
      { Returns True if temporary storage is full otherwise returns False. }

    function HasSpaceFor(const numBlocks : TffWord32) : boolean; override;
      { Returns True if temporary storage has space for the specified number
        of blocks. }

    procedure ReadBlock(const aBlockNum : TffWord32; aBlock : PffBlock); override;
      { Reads the block specified by aBlockNum from the memory map file
        and copies the block's data into aBlock. The block in the memory map
        file is unallocated and made available to another caller. }

    procedure ReleaseBlock(const aBlockNum : TffWord32); override;
      { Use this method to release a block previously stored via WriteBlock.
        The space occupied by the block is made available. The data written
        to the block is no longer accessible. }

    function WriteBlock(aBlock : PffBlock) : TffWord32; override;
      { Write a block to the file. aBlock is the block to be written.
        Returns the block number to which the block was written. When
        retrieving the block via ReadBlock, use the block number returned
        by this function. }

    {Properties}

    property BlockCount : TffWord32 read tsNumBlocks;
      { The number of blocks available in the file. }

    property Name : string read mmGetFileName;
      { The path and name of the file. }

    property Size : TffWord32 read tsSize;
      { The size of the file. }

  end;

var
  ffcTempStorageClass : TffTempStorageClass = TffTempStorageMM;
    { Identifies which type of temporary storage is to be used. }

implementation

uses
  SysUtils,
  FFLLExcp,
  FFSrBase,
  {$IFDEF SecureTempStorage}                                          {!!.06}
  fftbcryp,                                                           {!!.06}
  {$ENDIF}                                                            {!!.06}
  FFConst;

{$IFDEF SecureTempStorage}                                            {!!.06}
var                                                                   {!!.06}
  EncryptBuffer : PffByteArray; {for encryption}                      {!!.06}
{$ENDIF}                                                              {!!.06}

{===TffBaseTempStorage===============================================}
function TffBaseTempStorage.btsGetBlockCount : TffWord32;
begin
  Result := tsNumBlocks;
end;
{--------}
function TffBaseTempStorage.btsGetSize : TffWord32;
begin
  Result := tsSize;
end;
{====================================================================}

{===TffTempStorageVA==================================================}
constructor TffTempStorageVA.Create(configDir : TffPath; aSize : TffWord32;
                                    aBlockSize : integer);
var
  ErrCode : DWORD;
begin
  tsBlockSize := aBlockSize;
  mmNextAvailBlock := 0;
  mmPadLock := TffPadLock.Create;
  tsSize := aSize;
  mmUseCount := 0;

  { Allocate the virtual memory region. Memory is reserved but not
    committed. }
  mmAddress := VirtualAlloc(nil, tsSize, MEM_RESERVE or MEM_TOP_DOWN,
                            PAGE_READWRITE);
  if not assigned(mmAddress) then begin
    ErrCode := GetLastError;
    raise EffException.CreateEx(ffStrResGeneral, fferrTmpStoreCreateFail,
                                [aSize, ErrCode, ErrCode,
                                 SysErrorMessage(ErrCode)]);
  end;

  { Round up the storage size to the nearest 8 * 64k boundary. This makes
    things easier. }
  if tsSize mod (8 * ffcl_64k) <> 0 then begin
    tsNumBlocks := tsSize div (8 * ffcl_64k);
    tsSize := (tsNumBlocks + 1) * 8 * ffcl_64k;
  end;

  { Set up the block array. Array size is calculated as:
    # blocks = <file size> div <block size>
    # bytes = <# blocks> div 8. }
  tsNumBlocks := tsSize div tsBlockSize;
  mmArraySize := tsNumBlocks div 8;
  FFGetMem(mmBlocks, mmArraySize);
  FillChar(mmBlocks^, mmArraySize, 0);

  FFGetMem(mmCommits, mmArraySize);
  FillChar(mmCommits^, mmArraySize, 0);

end;
{--------}
destructor TffTempStorageVA.Destroy;
begin

  mmPadLock.Free;

  { Free the allocated memory. }
  if assigned(mmAddress) then
    VirtualFree(mmAddress, 0, MEM_RELEASE);

  { Free the block & commit arrays. }
  FFFreeMem(mmBlocks, mmArraySize);
  FFFreeMem(mmCommits, mmArraySize);

  inherited Destroy;
end;
{--------}
function TffTempStorageVA.Full : boolean;
begin
  Result := (mmUseCount = tsNumBlocks);
end;
{--------}
function TffTempStorageVA.HasSpaceFor(const numBlocks : TffWord32) : boolean;
begin
  Result := ((tsNumBlocks - mmUseCount) >= numBlocks);
end;
{--------}
procedure TffTempStorageVA.ReadBlock(const aBlockNum : TffWord32; aBlock : PffBlock);
var
  BlockPtr : PffByteArray;
begin

  { Requirement: Block number must be less than upper block boundary (because
      blocks are base zero). }
  Assert(aBlockNum < tsNumBlocks);

  mmPadLock.Lock;
  try
    { Calculate the location of the block. }
    BlockPtr := @mmAddress^[aBlockNum * tsBlockSize];

    { Move data from the file into the block. }
    Move(BlockPtr^, aBlock^, tsBlockSize);

    { Erase the block in the file. }
    FillChar(BlockPtr^, tsBlockSize, 0);

    { Mark the block as available. }
    tsReleaseBlockPrim(aBlockNum);
  finally
    mmPadLock.Unlock;                                                  {!!.03}
  end;

end;
{--------}
procedure TffTempStorageVA.ReleaseBlock(const aBlockNum : TffWord32);
begin
  { Requirement: Block number must be less than upper block boundary (because
      blocks are base zero). }
  Assert(aBlockNum < tsNumBlocks);

  mmPadLock.Lock;
  try
    { Mark the block as available. }
    tsReleaseBlockPrim(aBlockNum);
  finally
    mmPadLock.Unlock;                                                  {!!.03}
  end;

end;
{--------}
procedure TffTempStorageVA.tsReleaseBlockPrim(const aBlockNum : TffWord32);
var
  BitIndex : Byte;
  Index : TffWord32;
begin
  { Mark the block as available. First, find the position within the block
    array. }
  Index := aBlockNum div 8;

  mmNextAvailBlock := Index;

  { Now determine which bit to clear within that position. }
  BitIndex := aBlockNum - (Index * 8);

  { Clear the bit. }
  FFClearBit(@mmBlocks^[Index], BitIndex);

  { Decrement the usage count. }
  dec(mmUseCount);
  
end;
{--------}
function TffTempStorageVA.WriteBlock(aBlock : PffBlock) : TffWord32;
var
  AllBlocksChecked : boolean;
  BitIndex : Byte;
  BlockPtr : PffByteArray;
  Index, StartPoint : TffWord32;
begin

  Result := ffc_W32NoValue;
  BitIndex := 0;

  mmPadLock.Lock;
  try
    { Find an available block. We will be scanning through the entire block
      array so find our starting point & mark it as our ending point. }
    Index := mmNextAvailBlock;
    StartPoint := Index;
    AllBlocksChecked := False;

    repeat
      { Is there a block available in this byte? }
      if mmBlocks^[Index] < $FF then begin
        { Determine which bit is set. }
        for BitIndex := 0 to 7 do
          if not FFIsBitSet(@mmBlocks^[Index], BitIndex) then begin
            Result := (Index * 8) + BitIndex;
            break;
          end;
      end
      else begin
        { Move to the next position. }
        inc(Index);
        { Have we reached our start point? }
        AllBlocksChecked := (Index = StartPoint);
        if (not AllBlocksChecked) and (Index = mmArraySize) then begin
          Index := 0;
          AllBlocksChecked := (Index = StartPoint);
            { Catches case where StartPoint = 0. }
        end;
      end;
    until (Result <> ffc_W32NoValue) or AllBlocksChecked;

    { Was a free block found? }
    if Result <> ffc_W32NoValue then begin
      { Yes. Reset high water mark. }
      mmNextAvailBlock := Index;

      { Calculate the pointer to the block. }
      BlockPtr := @mmAddress^[Result * tsBlockSize];

      { Has that memory been committed? }
      if not FFIsBitSet(@mmCommits^[Index], BitIndex) then
        { No. Commit the memory. }
        VirtualAlloc(BlockPtr, tsBlockSize, MEM_COMMIT, PAGE_READWRITE);

      { Write the block to the file's block. }
      Move(aBlock^, BlockPtr^, tsBlockSize);

      { Mark the block as unavailable. }
      FFSetBit(@mmBlocks^[Index], BitIndex);
      FFSetBit(@mmCommits^[Index], BitIndex);

      inc(mmUseCount);
    end
    else
      raise EffException.CreateEx(ffStrResGeneral, fferrTmpStoreFull, [tsSize]);

  finally
    mmPadLock.Unlock;
  end;

end;
{====================================================================}

{===TffTempStorageMM=================================================}
constructor TffTempStorageMM.Create(configDir : TffPath; aSize : TffWord32;
                                    aBlockSize : integer);
begin
  tsBlockSize := aBlockSize;
  mmFileHandle := 0;
  mmMapHandle := 0;
  mmNextAvailBlock := 0;
  mmPadLock := TffPadLock.Create;
  tsSize := aSize;
  mmUseCount := 0;

  { Build the filename of the temporary storage file. }
  mmFileName := FFShStrAlloc(
                  FFExpandFileName(
                    FFMakeFullFileName(configDir, IntToStr(Longint(Self)) +
                                       '.TMP')));

  { The file won't be opened until it is needed. }

  { Round up the storage size to the nearest 8 * 64k boundary. This makes
    things easier. }
  if tsSize mod (8 * ffcl_64k) <> 0 then begin
    tsNumBlocks := tsSize div (8 * ffcl_64k);
    tsSize := (tsNumBlocks + 1) * 8 * ffcl_64k;
  end;

  { Set up the block array. Array size is calculated as:
    # blocks = <file size> div <block size>
    # bytes = <# blocks> div 8. }
  tsNumBlocks := tsSize div tsBlockSize;
  mmArraySize := tsNumBlocks div 8;
  FFGetMem(mmBlocks, mmArraySize);
  FillChar(mmBlocks^, mmArraySize, 0);

end;
{--------}
destructor TffTempStorageMM.Destroy;
begin

  mmPadLock.Free;

  { Close the mem map file if necessary. }
  if mmMapHandle <> 0 then
    CloseHandle(mmMapHandle);

  { Close the file handle if necessary. }
  if mmFileHandle <> 0 then
    CloseHandle(mmFileHandle);

  { Free the block array. }
  FFFreeMem(mmBlocks, mmArraySize);

  { Delete the temporary file. }
  if FFFileExists(mmFileName^) then
    FFDeleteFile(mmFileName^);

  { Deallocate the filename. }
  FFShStrFree(mmFileName);

  inherited Destroy;
end;
{--------}
function TffTempStorageMM.ActualSize : DWORD;
begin
  Result := GetFileSize(mmFileHandle, nil);
end;
{--------}
function TffTempStorageMM.Full : boolean;
begin
  Result := (mmUseCount = tsNumBlocks);
end;
{--------}
function TffTempStorageMM.HasSpaceFor(const numBlocks : TffWord32) : boolean;
begin
  Result := ((tsNumBlocks - mmUseCount) >= numBlocks);
end;
{--------}
function TffTempStorageMM.mmGetFileName : string;
begin
  Result := mmFileName^;
end;
{--------}
procedure TffTempStorageMM.mmOpenFile;
var
  ErrCode : DWORD;
begin

  { Create the temporary storage file. }
  mmFileHandle := FFOpenFilePrim(@mmFileName^[1], omReadWrite, smExclusive,
                                 False, True);
  if mmFileHandle <> 0 then begin

    mmMapHandle := CreateFileMapping(mmFileHandle, nil, PAGE_READWRITE,
                                     0, tsSize, nil);
    if mmMapHandle = 0 then begin
      { Raise exception. }
      ErrCode := GetLastError;
      raise EffException.CreateEx(ffStrResGeneral, fferrMapFileHandleFail,
                                  [mmFileName^, tsSize, ErrCode, ErrCode,
                                   SysErrorMessage(ErrCode)]);
    end;

  end
  else begin
    ErrCode := GetLastError;
    raise EffException.CreateEx(ffStrResGeneral, fferrMapFileCreateFail,
                                [mmFileName^, tsSize, ErrCode, ErrCode,
                                 SysErrorMessage(ErrCode)]);
  end;

end;
{--------}
procedure TffTempStorageMM.ReadBlock(const aBlockNum : TffWord32; aBlock : PffBlock);
var
  BlockPtr : PffByteArray;
  ErrCode : DWORD;
begin

  { Requirement: Block number must be less than upper block boundary (because
      blocks are base zero). }
  Assert(aBlockNum < tsNumBlocks);

  { Requirement: Something must have been written to the file. }
  Assert(mmMapHandle <> 0, 'Temp storage is empty');

  { Note: We don't verify that the block requested was actually written. }

  mmPadLock.Lock;
  try
    { Assumption: Block was previously marked unavailable and data was written
      to that block. }
    BlockPtr := MapViewOfFile(mmMapHandle, FILE_MAP_WRITE,
                              0, aBlockNum * tsBlockSize, tsBlockSize);
    if BlockPtr = nil then begin
      { Raise exception. }
      ErrCode := GetLastError;
      raise EffException.CreateEx(ffStrResGeneral, fferrMapFileViewFail,
                                  ['ReadBlock', aBlockNum, mmGetFileName,
                                   ErrCode, ErrCode, SysErrorMessage(ErrCode)]);
    end;

    {$IFDEF SecureTempStorage}                                        {begin !!.06}
    if (EncryptBuffer = nil) then
      GetMem(EncryptBuffer, tsBlockSize);
    Move(BlockPtr^, EncryptBuffer^, tsBlockSize);
    FFDecodeBlockServer(EncryptBuffer, tsBlockSize, 0);

    { Move data from the decrypted file into the block. }
    Move(EncryptBuffer^, aBlock^, tsBlockSize);
    {$ELSE}

    { Move data from the file into the block. }
    Move(BlockPtr^, aBlock^, tsBlockSize);
    {$ENDIF}                                                          {end !!.06}

    { Erase the block in the file. }
    FillChar(BlockPtr^, tsBlockSize, 0);

    { Unmap the view. }
    UnmapViewOfFile(BlockPtr);

    { Mark the block as available. }
    mmReleaseBlockPrim(aBlockNum);

  finally
    mmPadLock.Unlock;                                                  {!!.03}
  end;

end;
{--------}
procedure TffTempStorageMM.ReleaseBlock(const aBlockNum : TffWord32);
begin
  { Requirement: Block number must be less than upper block boundary (because
      blocks are base zero). }
  Assert(aBlockNum < tsNumBlocks);

  mmPadLock.Lock;
  try
    { Mark the block as available. }
    mmReleaseBlockPrim(aBlockNum);
  finally
    mmPadLock.Unlock;                                                  {!!.03}
  end;

end;
{--------}
procedure TffTempStorageMM.mmReleaseBlockPrim(const aBlockNum : TffWord32);
var
  BitIndex : Byte;
  Index : TffWord32;
begin
  { Mark the block as available. First, find the position within the block
    array. }
  Index := aBlockNum div 8;

  mmNextAvailBlock := Index;

  { Now determine which bit to clear within that position. }
  BitIndex := aBlockNum - (Index * 8);

  { Clear the bit. }
  FFClearBit(@mmBlocks^[Index], BitIndex);

  { Decrement the usage count. }
  dec(mmUseCount);
  
end;
{--------}
function TffTempStorageMM.WriteBlock(aBlock : PffBlock) : TffWord32;
var
  AllBlocksChecked : boolean;
  BitIndex : Byte;
  BlockPtr : PffByteArray;
  ErrCode : DWORD;
  Index, StartPoint : TffWord32;
begin

  Result := ffc_W32NoValue;
  BitIndex := 0;

  { Open the file if it has not already been opened. }
  if mmMapHandle = 0 then
    mmOpenFile;

  mmPadLock.Lock;
  try
    { Find an available block. We will be scanning through the entire block
      array so find our starting point & mark it as our ending point. }
    Index := mmNextAvailBlock;
    StartPoint := Index;
    AllBlocksChecked := False;

    repeat
      { Is there a block available in this byte? }
      if mmBlocks^[Index] < $FF then begin
        { Determine which bit is set. }
        for BitIndex := 0 to 7 do
          if not FFIsBitSet(@mmBlocks^[Index], BitIndex) then begin
            Result := (Index * 8) + BitIndex;
            break;
          end;
      end
      else begin
        { Move to the next position. }
        inc(Index);
        { Have we reached our start point? }
        AllBlocksChecked := (Index = StartPoint);
        if (not AllBlocksChecked) and (Index = mmArraySize) then begin
          Index := 0;
          AllBlocksChecked := (Index = StartPoint);
            { Catches case where StartPoint = 0. }
        end;
      end;
    until (Result <> ffc_W32NoValue) or AllBlocksChecked;

    { Was a free block found? }
    if Result <> ffc_W32NoValue then begin
      { Yes. Reset high water mark. }
      mmNextAvailBlock := Index;

      { Obtain a view on the available block. }
      BlockPtr := MapViewOfFile(mmMapHandle, FILE_MAP_ALL_ACCESS,
                                0, Result * tsBlockSize, tsBlockSize);

      if BlockPtr = nil then begin
        ErrCode := GetLastError;
        raise EffException.CreateEx(ffStrResGeneral, fferrMapFileViewFail,
                                    ['WriteBlock', Result * tsBlockSize,
                                     mmGetFileName,
                                     ErrCode, ErrCode, SysErrorMessage(ErrCode)]);
      end;

      {$IFDEF SecureTempStorage}                                      {begin !!.06}
      if (EncryptBuffer = nil) then
        GetMem(EncryptBuffer, tsBlockSize);
      Move(aBlock^, EncryptBuffer^, tsBlockSize);
      FFCodeBlockServer(EncryptBuffer, tsBlockSize, 0);

      { Write the encrypted block to the file's block. }
      Move(EncryptBuffer^, BlockPtr^, tsBlockSize);
      {$ELSE}

      { Write the block to the file's block. }
      Move(aBlock^, BlockPtr^, tsBlockSize);
      {$ENDIF}                                                        {end !!.06}
      { Release the view on the block. }
      UnmapViewOfFile(BlockPtr);

      { Mark the block as unavailable. }
      FFSetBit(@mmBlocks^[Index], BitIndex);

      inc(mmUseCount);

    end else
      raise EffException.CreateEx(ffStrResGeneral, fferrTmpStoreFull,
                                  [tsSize]);
  finally
    mmPadLock.Unlock;
  end;

end;
{====================================================================}


{===Initialization/Finalization======================================}{begin !!.06}
{$IFDEF SecureTempStorage}
procedure FinalizeUnit;
begin
  if (EncryptBuffer <> nil) then
    FreeMem(EncryptBuffer, 64*1024);
end;
{--------}
procedure InitializeUnit;
begin
  EncryptBuffer := nil;
end;
{--------}
initialization
  InitializeUnit;

finalization
  FinalizeUnit;
{$ENDIF}                                                              {end !!.06}

end.
