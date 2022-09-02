{*********************************************************}
{* FSSQL: Temporary Storage classes                 *}
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

{$I fsdefine.inc}

Unit fslltemp;

Interface

Uses
  Windows,
  fsllbase;

Type
  TfsTempStorageClass = Class Of TfsBaseTempStorage;
  TfsBaseTempStorage = Class(TFSSpecObject)
  Protected

    tsBlockSize: TffWord32;
    {-Size of the blocks used by the temporary storage. }

    tsNumBlocks: TffWord32;
    {-The number of blocks that can be held. }

    tsSize: TffWord32;
    {-The size of the temporary storage instance, in bytes. }
    tsEncrypt: Boolean;
    Function btsGetBlockCount: TffWord32; Virtual;
    {-Returns the total number of blocks that the temporary storage
      instance can hold. }

    Function btsGetSize: TffWord32; Virtual;
    {-Returns the size, in bytes, of the temporary storage instance. }

  Public

    { Methods }

    Constructor Create(configDir: String; aSize: TffWord32;
      blockSize: Integer; aEncrypt: boolean); Virtual; Abstract;
    { Creates an instance of temporary storage. aSize is the size of the
      storage space in bytes. Blocksize is the size of the blocks, in bytes,
      to be allocated by temporary storage. }

    Function Full: boolean; Virtual; Abstract;
    { Returns True if temporary storage is full otherwise returns False. }

    Function HasSpaceFor(Const numBlocks: TffWord32): boolean; Virtual; Abstract;
    { Returns True if temporary storage has space for the specified number
      of blocks. }

    Procedure ReadBlock(Const aBlockNum: TffWord32; aBlock: PffBlock); Virtual; Abstract;
    { Reads the block specified by aBlockNum from temporary storage
      & copies the block's data into aBlock. The block in the memory map
      file is unallocated and made available to another caller. }

    Procedure ReleaseBlock(Const aBlockNum: TffWord32); Virtual; Abstract;
    { Use this method to release a block previously stored via WriteBlock.
      The space occupied by the block is made available. The data written
      to the block is no longer accessible. }

    Function WriteBlock(aBlock: PffBlock): TffWord32; Virtual; Abstract;
    { Write a block to temporary stroage. aBlock is the block to be written.
      Returns the block number to which the block was written. When
      retrieving the block via ReadBlock, use the block number returned
      by this function. }

  {Properties}

    Property BlockCount: TffWord32 Read btsGetBlockCount;
    { The total number of blocks that can be held in temporary storage. }

    Property Size: TffWord32 Read btsGetSize;
    { The size of the temporary storage, in bytes. }

    Property Encrypt: Boolean Read tsEncrypt Write tsEncrypt;

  End;

  { This class implements temporary storage using VirtualAlloc and VirtualFree.
    It maintains an internal array denoting block availability. }
  TffTempStorageVA = Class(TfsBaseTempStorage)
  Protected

    mmArraySize: TffWord32;
    {-Size of the mmBlocks & mmCommits arrays. }

    mmAddress: PffByteArray;
    {-Pointer to the allocated region. }

    mmBlocks: PffByteArray;
    {-Array of bits used to denote block availability. One bit per 64k block. }

    mmCommits: PffByteArray;
    {-Indicates which blocks in mmBlocks have been committed in virtual
      memory. The bits in this array have a one-to-one correspondence to the
      bits in the mmBlocks array. }

    mmNextAvailBlock: TffWord32;
    {-Position of the next available block in the bit array. This value
      indicates a particular byte not a bit. }

    mmPadLock: TfsPadlock;
    {-Controls access to the storage. }

    mmUseCount: TffWord32;
    {-Number of blocks used. Bounds: 0..tsNumBlocks }

  { Protected methods }
    Procedure tsReleaseBlockPrim(Const aBlockNum: TffWord32);

  Public

    { Methods }

    Constructor Create(configDir: String; aSize: TffWord32;
      aBlockSize: Integer; aEncrypt: boolean); Override;

    Destructor Destroy; Override;

    Function Full: boolean; Override;
    { Returns True if temporary storage is full otherwise returns False. }

    Function HasSpaceFor(Const numBlocks: TffWord32): boolean; Override;
    { Returns True if temporary storage has space for the specified number
      of blocks. }

    Procedure ReadBlock(Const aBlockNum: TffWord32; aBlock: PffBlock); Override;
    { Reads the block specified by aBlockNum from the memory map file
      and copies the block's data into aBlock. The block in the memory map
      file is unallocated and made available to another caller. }

    Procedure ReleaseBlock(Const aBlockNum: TffWord32); Override;
    { Use this method to release a block previously stored via WriteBlock.
      The space occupied by the block is made available. The data written
      to the block is no longer accessible. }

    Function WriteBlock(aBlock: PffBlock): TffWord32; Override;
    { Write a block to the file. aBlock is the block to be written.
      Returns the block number to which the block was written. When
      retrieving the block via ReadBlock, use the block number returned
      by this function. }

  End;

  { This class implements temporary storage using a memory mapped file. It
    divides itself up into blocks that are 64k bytes in size and maintains
    an internal array denoting block availability.

    This class expects to create a file on the disk. It does not support
    mapping to the Windows page file. }
  TfsTempStorageMM = Class(TfsBaseTempStorage)
  Protected
    mmArraySize: TffWord32;
    {-Size of the mmBlocks array. }

    mmBlocks: PffByteArray;
    {-Array of bits used to denote block availability. One bit per 64k block. }

    mmFileHandle: THandle;
    {-The handle to the file. }

    mmFileName: PffShStr;
    {-The path and name of the file. }

    mmMapHandle: THandle;
    {-The handle to the memory mapped file. }

    mmNextAvailBlock: TffWord32;
    {-Position of the next available block in the bit array. This value
      indicates a particular byte not a bit. }

    mmPadLock: TfsPadlock;
    {-Controls access to file. }

    mmUseCount: TffWord32;
    {-Number of blocks used. Bounds: 0..tsNumBlocks }

  { Protected methods }
    Function mmGetFileName: String;
    {-Returns the name of the memory mapped file. }

    Procedure mmOpenFile;
    {-Creates & opens the memory mapped file. }

    Procedure mmReleaseBlockPrim(Const aBlockNum: TffWord32);
    {-Marks a block as available. }

  Public

    { Methods }

    Constructor Create(configDir: String; aSize: TffWord32;
      aBlockSize: Integer; aEncrypt: boolean); Override;

    Destructor Destroy; Override;

    Function ActualSize: DWORD;
    { Returns the actual size of the file. }

    Function Full: boolean; Override;
    { Returns True if temporary storage is full otherwise returns False. }

    Function HasSpaceFor(Const numBlocks: TffWord32): boolean; Override;
    { Returns True if temporary storage has space for the specified number
      of blocks. }

    Procedure ReadBlock(Const aBlockNum: TffWord32; aBlock: PffBlock); Override;
    { Reads the block specified by aBlockNum from the memory map file
      and copies the block's data into aBlock. The block in the memory map
      file is unallocated and made available to another caller. }

    Procedure ReleaseBlock(Const aBlockNum: TffWord32); Override;
    { Use this method to release a block previously stored via WriteBlock.
      The space occupied by the block is made available. The data written
      to the block is no longer accessible. }

    Function WriteBlock(aBlock: PffBlock): TffWord32; Override;
    { Write a block to the file. aBlock is the block to be written.
      Returns the block number to which the block was written. When
      retrieving the block via ReadBlock, use the block number returned
      by this function. }

  {Properties}

    Property BlockCount: TffWord32 Read tsNumBlocks;
    { The number of blocks available in the file. }

    Property Name: String Read mmGetFileName;
    { The path and name of the file. }

    Property Size: TffWord32 Read tsSize;
    { The size of the file. }
    Property Encrypt: boolean Read tsEncrypt;
  End;

Var
  fscTempStorageClass: TfsTempStorageClass = TfsTempStorageMM;
  { Identifies which type of temporary storage is to be used. }

Implementation

Uses
  SysUtils,
  FsLLExcp,
  FsSrBase,
  fscryptaccess,
  FsConst;

Var {!!.06}
  EncryptBuffer: PffByteArray; {for encryption}

  {===TfsBaseTempStorage===============================================}

Function TfsBaseTempStorage.btsGetBlockCount: TffWord32;
Begin
  Result := tsNumBlocks;
End;
{--------}

Function TfsBaseTempStorage.btsGetSize: TffWord32;
Begin
  Result := tsSize;
End;
{====================================================================}

{===TffTempStorageVA==================================================}

Constructor TffTempStorageVA.Create(configDir: String; aSize: TffWord32;
  aBlockSize: Integer; aEncrypt: boolean);
Var
  ErrCode: DWORD;
Begin
  tsBlockSize := aBlockSize;
  mmNextAvailBlock := 0;
  mmPadLock := TfsPadlock.Create;
  tsSize := aSize;
  mmUseCount := 0;
  tsEncrypt := aEncrypt;
  { Allocate the virtual memory region. Memory is reserved but not
    committed. }
  mmAddress := VirtualAlloc(Nil, tsSize, MEM_RESERVE Or MEM_TOP_DOWN,
    PAGE_READWRITE);
  If Not assigned(mmAddress) Then
    Begin
      ErrCode := GetLastError;
      Raise EfsException.CreateEx(fsStrResGeneral, fserrTmpStoreCreateFail,
        [aSize, ErrCode, ErrCode,
        SysErrorMessage(ErrCode)]);
    End;

  { Round up the storage size to the nearest 8 * 64k boundary. This makes
    things easier. }
  If tsSize Mod (8 * fscl_64k) <> 0 Then
    Begin
      tsNumBlocks := tsSize Div (8 * fscl_64k);
      tsSize := (tsNumBlocks + 1) * 8 * fscl_64k;
    End;

  { Set up the block array. Array size is calculated as:
    # blocks = <file size> div <block size>
    # bytes = <# blocks> div 8. }
  tsNumBlocks := tsSize Div tsBlockSize;
  mmArraySize := tsNumBlocks Div 8;
  FFGetMem(mmBlocks, mmArraySize);
  FillChar(mmBlocks^, mmArraySize, 0);

  FFGetMem(mmCommits, mmArraySize);
  FillChar(mmCommits^, mmArraySize, 0);

End;
{--------}

Destructor TffTempStorageVA.Destroy;
Begin

  mmPadLock.Free;

  { Free the allocated memory. }
  If assigned(mmAddress) Then
    VirtualFree(mmAddress, 0, MEM_RELEASE);

  { Free the block & commit arrays. }
  FFFreeMem(mmBlocks, mmArraySize);
  FFFreeMem(mmCommits, mmArraySize);

  Inherited Destroy;
End;
{--------}

Function TffTempStorageVA.Full: boolean;
Begin
  Result := (mmUseCount = tsNumBlocks);
End;
{--------}

Function TffTempStorageVA.HasSpaceFor(Const numBlocks: TffWord32): boolean;
Begin
  Result := ((tsNumBlocks - mmUseCount) >= numBlocks);
End;
{--------}

Procedure TffTempStorageVA.ReadBlock(Const aBlockNum: TffWord32; aBlock: PffBlock);
Var
  BlockPtr: PffByteArray;
Begin

  { Requirement: Block number must be less than upper block boundary (because
      blocks are base zero). }
  Assert(aBlockNum < tsNumBlocks);

  mmPadLock.Lock;
  Try
    { Calculate the location of the block. }
    BlockPtr := @mmAddress^[aBlockNum * tsBlockSize];

    { Move data from the file into the block. }
    Move(BlockPtr^, aBlock^, tsBlockSize);

    { Erase the block in the file. }
    FillChar(BlockPtr^, tsBlockSize, 0);

    { Mark the block as available. }
    tsReleaseBlockPrim(aBlockNum);
  Finally
    mmPadLock.Unlock; {!!.03}
  End;

End;
{--------}

Procedure TffTempStorageVA.ReleaseBlock(Const aBlockNum: TffWord32);
Begin
  { Requirement: Block number must be less than upper block boundary (because
      blocks are base zero). }
  Assert(aBlockNum < tsNumBlocks);

  mmPadLock.Lock;
  Try
    { Mark the block as available. }
    tsReleaseBlockPrim(aBlockNum);
  Finally
    mmPadLock.Unlock; {!!.03}
  End;

End;
{--------}

Procedure TffTempStorageVA.tsReleaseBlockPrim(Const aBlockNum: TffWord32);
Var
  BitIndex: Byte;
  Index: TffWord32;
Begin
  { Mark the block as available. First, find the position within the block
    array. }
  Index := aBlockNum Div 8;

  mmNextAvailBlock := Index;

  { Now determine which bit to clear within that position. }
  BitIndex := aBlockNum - (Index * 8);

  { Clear the bit. }
  FFClearBit(@mmBlocks^[Index], BitIndex);

  { Decrement the usage count. }
  dec(mmUseCount);

End;
{--------}

Function TffTempStorageVA.WriteBlock(aBlock: PffBlock): TffWord32;
Var
  AllBlocksChecked: boolean;
  BitIndex: Byte;
  BlockPtr: PffByteArray;
  Index, StartPoint: TffWord32;
Begin

  Result := fsc_W32NoValue;
  BitIndex := 0;

  mmPadLock.Lock;
  Try
    { Find an available block. We will be scanning through the entire block
      array so find our starting point & mark it as our ending point. }
    Index := mmNextAvailBlock;
    StartPoint := Index;
    AllBlocksChecked := False;

    Repeat
      { Is there a block available in this byte? }
      If mmBlocks^[Index] < $FF Then
        Begin
          { Determine which bit is set. }
          For BitIndex := 0 To 7 Do
            If Not FFIsBitSet(@mmBlocks^[Index], BitIndex) Then
              Begin
                Result := (Index * 8) + BitIndex;
                break;
              End;
        End
      Else
        Begin
          { Move to the next position. }
          inc(Index);
          { Have we reached our start point? }
          AllBlocksChecked := (Index = StartPoint);
          If (Not AllBlocksChecked) And (Index = mmArraySize) Then
            Begin
              Index := 0;
              AllBlocksChecked := (Index = StartPoint);
              { Catches case where StartPoint = 0. }
            End;
        End;
    Until (Result <> fsc_W32NoValue) Or AllBlocksChecked;

    { Was a free block found? }
    If Result <> fsc_W32NoValue Then
      Begin
        { Yes. Reset high water mark. }
        mmNextAvailBlock := Index;

        { Calculate the pointer to the block. }
        BlockPtr := @mmAddress^[Result * tsBlockSize];

        { Has that memory been committed? }
        If Not FFIsBitSet(@mmCommits^[Index], BitIndex) Then
          { No. Commit the memory. }
          VirtualAlloc(BlockPtr, tsBlockSize, MEM_COMMIT, PAGE_READWRITE);

        { Write the block to the file's block. }
        Move(aBlock^, BlockPtr^, tsBlockSize);

        { Mark the block as unavailable. }
        FFSetBit(@mmBlocks^[Index], BitIndex);
        FFSetBit(@mmCommits^[Index], BitIndex);

        inc(mmUseCount);
      End
    Else
      Raise EfsException.CreateEx(fsStrResGeneral, fserrTmpStoreFull, [tsSize]);

  Finally
    mmPadLock.Unlock;
  End;

End;
{====================================================================}

{===TfsTempStorageMM=================================================}

Constructor TfsTempStorageMM.Create(configDir: String; aSize: TffWord32;
  aBlockSize: Integer; aEncrypt: boolean);
Begin
  tsBlockSize := aBlockSize;
  mmFileHandle := 0;
  mmMapHandle := 0;
  mmNextAvailBlock := 0;
  mmPadLock := TfsPadlock.Create;
  tsSize := aSize;
  mmUseCount := 0;
  tsEncrypt := aEncrypt;
  { Build the filename of the temporary storage file. }
  mmFileName := FFShStrAlloc(
    FFExpandFileName(
    FFMakeFullFileName(configDir, IntToStr(Longint(Self)) +
    '.TMP')));

  { The file won't be opened until it is needed. }

  { Round up the storage size to the nearest 8 * 64k boundary. This makes
    things easier. }
  If tsSize Mod (8 * fscl_64k) <> 0 Then
    Begin
      tsNumBlocks := tsSize Div (8 * fscl_64k);
      tsSize := (tsNumBlocks + 1) * 8 * fscl_64k;
    End;

  { Set up the block array. Array size is calculated as:
    # blocks = <file size> div <block size>
    # bytes = <# blocks> div 8. }
  tsNumBlocks := tsSize Div tsBlockSize;
  mmArraySize := tsNumBlocks Div 8;
  FFGetMem(mmBlocks, mmArraySize);
  FillChar(mmBlocks^, mmArraySize, 0);

End;
{--------}

Destructor TfsTempStorageMM.Destroy;
Begin

  mmPadLock.Free;

  { Close the mem map file if necessary. }
  If mmMapHandle <> 0 Then
    CloseHandle(mmMapHandle);

  { Close the file handle if necessary. }
  If mmFileHandle <> 0 Then
    CloseHandle(mmFileHandle);

  { Free the block array. }
  FFFreeMem(mmBlocks, mmArraySize);

  { Delete the temporary file. }
  If FFFileExists(mmFileName^) Then
    FFDeleteFile(mmFileName^);

  { Deallocate the filename. }
  FFShStrFree(mmFileName);

  Inherited Destroy;
End;
{--------}

Function TfsTempStorageMM.ActualSize: DWORD;
Begin
  Result := GetFileSize(mmFileHandle, Nil);
End;
{--------}

Function TfsTempStorageMM.Full: boolean;
Begin
  Result := (mmUseCount = tsNumBlocks);
End;
{--------}

Function TfsTempStorageMM.HasSpaceFor(Const numBlocks: TffWord32): boolean;
Begin
  Result := ((tsNumBlocks - mmUseCount) >= numBlocks);
End;
{--------}

Function TfsTempStorageMM.mmGetFileName: String;
Begin
  Result := mmFileName^;
End;
{--------}

Procedure TfsTempStorageMM.mmOpenFile;
Var
  ErrCode: DWORD;
Begin

  { Create the temporary storage file. }
  mmFileHandle := FFOpenFilePrim(@mmFileName^[1], omReadWrite, smExclusive,
    False, True);
  If mmFileHandle <> 0 Then
    Begin

      mmMapHandle := CreateFileMapping(mmFileHandle, Nil, PAGE_READWRITE,
        0, tsSize, Nil);
      If mmMapHandle = 0 Then
        Begin
          { Raise exception. }
          ErrCode := GetLastError;
          Raise EfsException.CreateEx(fsStrResGeneral, fserrMapFileHandleFail,
            [mmFileName^, tsSize, ErrCode, ErrCode,
            SysErrorMessage(ErrCode)]);
        End;

    End
  Else
    Begin
      ErrCode := GetLastError;
      Raise EfsException.CreateEx(fsStrResGeneral, fserrMapFileCreateFail,
        [mmFileName^, tsSize, ErrCode, ErrCode,
        SysErrorMessage(ErrCode)]);
    End;

End;
{--------}

Procedure TfsTempStorageMM.ReadBlock(Const aBlockNum: TffWord32; aBlock: PffBlock);
Var
  BlockPtr: PffByteArray;
  ErrCode: DWORD;
Begin

  { Requirement: Block number must be less than upper block boundary (because
      blocks are base zero). }
  Assert(aBlockNum < tsNumBlocks);

  { Requirement: Something must have been written to the file. }
  Assert(mmMapHandle <> 0, 'Temp storage is empty');

  { Note: We don't verify that the block requested was actually written. }

  mmPadLock.Lock;
  Try
    { Assumption: Block was previously marked unavailable and data was written
      to that block. }
    BlockPtr := MapViewOfFile(mmMapHandle, FILE_MAP_WRITE,
      0, aBlockNum * tsBlockSize, tsBlockSize);
    If BlockPtr = Nil Then
      Begin
        { Raise exception. }
        ErrCode := GetLastError;
        Raise EfsException.CreateEx(fsStrResGeneral, fserrMapFileViewFail,
          ['ReadBlock', aBlockNum, mmGetFileName,
          ErrCode, ErrCode, SysErrorMessage(ErrCode)]);
      End;

    If Encrypt Then
      Begin
        If (EncryptBuffer = Nil) Then
          GetMem(EncryptBuffer, tsBlockSize);
        Move(BlockPtr^, EncryptBuffer^, tsBlockSize);
        FFDecodeBlockServer(EncryptBuffer, tsBlockSize, 0);

        { Move data from the decrypted file into the block. }
        Move(EncryptBuffer^, aBlock^, tsBlockSize);
      End
    Else
      { Move data from the file into the block. }
      Move(BlockPtr^, aBlock^, tsBlockSize);

    { Erase the block in the file. }
    FillChar(BlockPtr^, tsBlockSize, 0);

    { Unmap the view. }
    UnmapViewOfFile(BlockPtr);

    { Mark the block as available. }
    mmReleaseBlockPrim(aBlockNum);

  Finally
    mmPadLock.Unlock; {!!.03}
  End;

End;
{--------}

Procedure TfsTempStorageMM.ReleaseBlock(Const aBlockNum: TffWord32);
Begin
  { Requirement: Block number must be less than upper block boundary (because
      blocks are base zero). }
  Assert(aBlockNum < tsNumBlocks);

  mmPadLock.Lock;
  Try
    { Mark the block as available. }
    mmReleaseBlockPrim(aBlockNum);
  Finally
    mmPadLock.Unlock; {!!.03}
  End;

End;
{--------}

Procedure TfsTempStorageMM.mmReleaseBlockPrim(Const aBlockNum: TffWord32);
Var
  BitIndex: Byte;
  Index: TffWord32;
Begin
  { Mark the block as available. First, find the position within the block
    array. }
  Index := aBlockNum Div 8;

  mmNextAvailBlock := Index;

  { Now determine which bit to clear within that position. }
  BitIndex := aBlockNum - (Index * 8);

  { Clear the bit. }
  FFClearBit(@mmBlocks^[Index], BitIndex);

  { Decrement the usage count. }
  dec(mmUseCount);

End;
{--------}

Function TfsTempStorageMM.WriteBlock(aBlock: PffBlock): TffWord32;
Var
  AllBlocksChecked: boolean;
  BitIndex: Byte;
  BlockPtr: PffByteArray;
  ErrCode: DWORD;
  Index, StartPoint: TffWord32;
Begin

  Result := fsc_W32NoValue;
  BitIndex := 0;

  { Open the file if it has not already been opened. }
  If mmMapHandle = 0 Then
    mmOpenFile;

  mmPadLock.Lock;
  Try
    { Find an available block. We will be scanning through the entire block
      array so find our starting point & mark it as our ending point. }
    Index := mmNextAvailBlock;
    StartPoint := Index;
    AllBlocksChecked := False;

    Repeat
      { Is there a block available in this byte? }
      If mmBlocks^[Index] < $FF Then
        Begin
          { Determine which bit is set. }
          For BitIndex := 0 To 7 Do
            If Not FFIsBitSet(@mmBlocks^[Index], BitIndex) Then
              Begin
                Result := (Index * 8) + BitIndex;
                break;
              End;
        End
      Else
        Begin
          { Move to the next position. }
          inc(Index);
          { Have we reached our start point? }
          AllBlocksChecked := (Index = StartPoint);
          If (Not AllBlocksChecked) And (Index = mmArraySize) Then
            Begin
              Index := 0;
              AllBlocksChecked := (Index = StartPoint);
              { Catches case where StartPoint = 0. }
            End;
        End;
    Until (Result <> fsc_W32NoValue) Or AllBlocksChecked;

    { Was a free block found? }
    If Result <> fsc_W32NoValue Then
      Begin
        { Yes. Reset high water mark. }
        mmNextAvailBlock := Index;

        { Obtain a view on the available block. }
        BlockPtr := MapViewOfFile(mmMapHandle, FILE_MAP_ALL_ACCESS,
          0, Result * tsBlockSize, tsBlockSize);

        If BlockPtr = Nil Then
          Begin
            ErrCode := GetLastError;
            Raise EfsException.CreateEx(fsStrResGeneral, fserrMapFileViewFail,
              ['WriteBlock', Result * tsBlockSize,
              mmGetFileName,
                ErrCode, ErrCode, SysErrorMessage(ErrCode)]);
          End;

        If Encrypt Then
          Begin
            If (EncryptBuffer = Nil) Then
              GetMem(EncryptBuffer, tsBlockSize);
            Move(aBlock^, EncryptBuffer^, tsBlockSize);
            FFCodeBlockServer(EncryptBuffer, tsBlockSize, 0);

            { Write the encrypted block to the file's block. }
            Move(EncryptBuffer^, BlockPtr^, tsBlockSize);
          End
        Else
          { Write the block to the file's block. }
          Move(aBlock^, BlockPtr^, tsBlockSize);
        { Release the view on the block. }
        UnmapViewOfFile(BlockPtr);

        { Mark the block as unavailable. }
        FFSetBit(@mmBlocks^[Index], BitIndex);

        inc(mmUseCount);

      End
    Else
      Raise EfsException.CreateEx(fsStrResGeneral, fserrTmpStoreFull,
        [tsSize]);
  Finally
    mmPadLock.Unlock;
  End;

End;
{====================================================================}

{===Initialization/Finalization======================================}{begin !!.06}

Procedure FinalizeUnit;
Begin
  If (EncryptBuffer <> Nil) Then
    FreeMem(EncryptBuffer, 64 * 1024);
End;
{--------}

Procedure InitializeUnit;
Begin
  EncryptBuffer := Nil;
End;
{--------}
Initialization
  InitializeUnit;

Finalization
  FinalizeUnit;

End.

