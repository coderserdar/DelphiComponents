{*********************************************************}
{* FlashFiler: Table access - general & helper routines  *}
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

unit fftbbase;

interface

uses
  Windows,
  SysUtils,
  Classes,
  ffconst,
  ffllbase,
  ffsrlock,
  ffsrmgr,
  ffllexcp,
  ffsrbase,
  fffile,
  ffsrintf,
  fflldict;

{---Field comparison routine---}
function FFKeyCompareField(const Key1, Key2;
                           FieldType       : TffFieldType;
                           FieldLen        : Integer;
                           NoCase          : Boolean) : integer;
  {-Treat Key1 and Key2 as Filer typed fields, compare}

{---Table helper routines---}
function FFTblHlpGetNewBlock(aFI            : PffFileInfo;
                             aTI            : PffTransInfo;
                         var aReleaseMethod : TffReleaseMethod)
                                            : PffBlock;
  {-Return a new block, pre-marked as dirty.  Exclusively locks the
    file header block and recycled block (if one is available). }
procedure FFTblHlpDelete(const aPath      : TffPath;
                         const aTableName : TffTableName;
                         const aDict      : TffDataDictionary);
  {-Delete all files associated with a table}
procedure FFTblHlpDeleteBlock(aFI         : PffFileInfo;
                              aFileHeader : PffBlockHeaderFile;
                              aBlock      : PffBlock);
  {-Delete the block, add it to the deleted block chain.  Assumes the block
    has been exclusively locked. }

procedure FFTblHlpRename(const aPath      : TffPath;
                         const aTableName : TffTableName;
                         const aNewName   : TffTableName;
                         const aDict      : TffDataDictionary);
  {-Renames all files associated with a table}

{---Buffer manager access routines---}
function FFBMAddBlock(aFI : PffFileInfo;
                      aTI : PffTransInfo;
                const aBlockNum : TffWord32;
                  var aReleaseMethod : TffReleaseMethod) : PffBlock;
  {-Return address of a new block}
procedure FFBMDirtyBlock(aFI : PffFileInfo;
                   const aBlockNum : TffWord32;
                         aTI : PffTransInfo;
                     var aModifiableBlock : PffBlock);
  {-Mark a block dirty. The block *must* be in the buffer already.
    Returns a pointer to the modifiable copy of the block in output variable
    aModifiableBlock. The calling function *MUST*
    use the modifiable copy instead of the read-only copy it currently
    possesses. }
function FFBMGetBlock(aFI : PffFileInfo;
                      aTI : PffTransInfo;
                const aBlockNum : TffWord32;
                const aMarkDirty : Boolean;
                  var aReleaseMethod : TffReleaseMethod) : PffBlock;
  {-Retrieves the specified file block.  If aMarkDirty is False then returns
    a pointer to the read-only copy of the block.  If aMarkDirty is True then
    returns a modifiable copy of the block. }
function FFBMGetFileHeaderBlock(aFI : PffFileInfo;
                                aTI : PffTransInfo;
                          const aMarkDirty : Boolean;
                            var aReleaseMethod : TffReleaseMethod) : PffBlock;
  {-Reads a file header (block 0) into buffer, returns address.
    Note: this routine verifies the header block to be a valid header
          block, so it must be used IMMEDIATELY after opening a file;
          also sets the block size for the file}
procedure FFBMRemoveFile(aFI : PffFileInfo);
  {-Mark a file's blocks in the buffer manager as available for reuse}
procedure FFBMUnlockBlock(aFI : PffFileInfo; aBlockNum : TffWord32);
  {-Unlock a block making it available for reuse immediately.}

{---Lock manager access routines---}
procedure FFAcqRecordLock(aFI       : PffFileInfo;
                          aTI       : PffTransInfo;
                    const aRefNum   : TffInt64;
                    const aLockType : TffSrLockType;
                    const aDatabaseID : TffDatabaseID;                 {!!.10}
                    const aCursorID : TffCursorID;                     {!!.02}
                    const aConditional : Boolean);                     {!!.02}
  { Use this procedure to obtain a lock on a record.  If the lock is not
    granted then an exception is raised. }

{Begin !!.10}
procedure FFRelaxRecordLock(aFI : PffFileInfo;
                            aTI : PffTransInfo;
                      const aCursorID : TffCursorID;
                      const aRefNum : TffInt64);
  { Used by data modification operations to make a modified record available
    to other cursors within the same transaction. }
{End !!.10}

procedure FFRelRecordLock(aFI         : PffFileInfo;
                          aTI         : PffTransInfo;
                    const aRefNum     : TffInt64;
                    const aDatabaseID : TffDatabaseID;                 {!!.10}
                    const aCursorID   : TffCursorID);
  { Use this procedure to release an existing record lock. }

implementation

uses                                                                   {!!.11}
  FFSrEng;                                                             {!!.11}

resourcestring
  ffcRecord = 'record %d:%d (high:low)';

{== Field comparison routine =========================================}
function FFKeyCompareField(const Key1, Key2;
                           FieldType       : TffFieldType;
                           FieldLen        : Integer;
                           NoCase          : Boolean)
                                           : Integer;
var
  CompareData : TffCompareData;
  Ch1, Ch2    : AnsiChar;
begin
  CompareData.cdPartLen := 0;
  {we'll use an ascending type comparison here, our caller will flip
   the sign of our result if required}
  CompareData.cdAscend := True;
  CompareData.cdNoCase := NoCase;
  case FieldType of
    fftBoolean :
      begin
        {assumption: True is greater than False}
        if boolean(Key1) then
          if boolean(Key2) then Result := 0
          else                  Result := 1
        else
          if boolean(Key2) then Result := -1
          else                  Result := 0;
      end;
    fftChar :
      begin
        if NoCase then begin
          Ch1 := upcase(AnsiChar(Key1));
          Ch2 := upcase(AnsiChar(Key2));
          Result := FFCmpB(byte(Ch1), byte(Ch2));
        end
        else
          Result := FFCmpB(byte(Key1), byte(Key2));
      end;
    fftWideChar :
      begin
        CompareData.cdKeyLen := 1;
        Result := FFKeyCompareWideChar(Key1, Key2, @CompareData);
      end;
    fftByte :
      begin
        Result := FFCmpB(byte(Key1), byte(Key2));
      end;
    fftWord16 :
      begin
        Result := FFCmpW(TffWord16(Key1), TffWord16(Key2));
      end;
    fftWord32 :
      begin
        Result := FFCmpDW(TffWord32(Key1), TffWord32(Key2));
      end;
    fftInt8 :
      begin
        Result := FFCmpI8(shortint(Key1), shortint(Key2));
      end;
    fftInt16 :
      begin
        Result := FFCmpI16(smallint(Key1), smallint(Key2));
      end;
    fftInt32 :
      begin
        Result := FFCmpI32(longint(Key1), longint(Key2));
      end;
    fftAutoInc :
      begin
        Result := FFCmpDW(TffWord32(Key1), TffWord32(Key2));
      end;
    fftSingle :
      begin
        if      single(Key1) = single(Key2) then Result := 0
        else if single(Key1) > single(Key2) then Result := 1
        else                                     Result := -1;
      end;
    fftDouble :
      begin
        if      double(Key1) = double(Key2) then Result := 0
        else if double(Key1) > double(Key2) then Result := 1
        else                                     Result := -1;
      end;
    fftExtended :
      begin
        if      extended(Key1) = extended(Key2) then Result := 0
        else if extended(Key1) > extended(Key2) then Result := 1
        else                                         Result := -1;
      end;
    fftComp,
    fftCurrency :
      begin
        if      comp(Key1) = comp(Key2) then Result := 0
        else if comp(Key1) > comp(Key2) then Result := 1
        else                                 Result := -1;
      end;
    fftStDate,
    fftStTime :
      begin
        Result := FFCmpI32(longint(Key1), longint(Key2));
      end;
    fftDateTime :
      begin
        if      double(Key1) = double(Key2) then Result := 0
        else if double(Key1) > double(Key2) then Result := 1
        else                                     Result := -1;
      end;
    fftBLOB,
    fftBLOBMemo,
    fftBLOBFmtMemo,
    fftBLOBOLEObj,
    fftBLOBGraphic,
    fftBLOBDBSOLEObj,
    fftBLOBTypedBin,
    fftBLOBFile :
      begin
        Result := 0; {a spurious value}
        FFRaiseExceptionNoData(EffServerException, ffStrResServer, fferrCannotCompare);
      end;
    fftByteArray :
      begin
        CompareData.cdKeyLen := FieldLen;
        Result := FFKeyCompareBytes(Key1, Key2, @CompareData);
      end;
    fftShortString :
      begin
        CompareData.cdKeyLen := FieldLen - 1;
        Result := FFKeyCompareStr(Key1, Key2, @CompareData);
      end;
    fftShortAnsiStr :
      begin
        CompareData.cdKeyLen := FieldLen - 1;
        Result := FFKeyCompareAnsiStr(Key1, Key2, @CompareData);
      end;
    fftNullString :
      begin
        CompareData.cdKeyLen := FieldLen;
        Result := FFKeyCompareStrZ(Key1, Key2, @CompareData);
      end;
    fftNullAnsiStr :
      begin
        CompareData.cdKeyLen := FieldLen;
        Result := FFKeyCompareAnsiStrZ(Key1, Key2, @CompareData);
      end;
    fftWideString :
      begin
        CompareData.cdKeyLen := FieldLen div sizeof(WideChar);
        Result := FFKeyCompareWideStr(Key1, Key2, @CompareData);
      end;
  else
    Result := 0; {a spurious value}
    FFRaiseExceptionNoData(EffServerException, ffStrResServer, fferrBadFieldType);
  end;{case}
end;
{=====================================================================}

{== Internal Table Helper routines ===================================}
function FFTblHlpGetNewBlock(aFI            : PffFileInfo;
                             aTI            : PffTransInfo;
                         var aReleaseMethod : TffReleaseMethod)        {!!.11}
                                            : PffBlock;
var
  aFileHeader  : PffBlockHeaderFile;
  BlockHdr     : PffBlockHeaderFile absolute Result; {a convenient typecast}
  BlockNumber  : Longint;
  aFHRelMethod : TffReleaseMethod;
begin

  aFileHeader := PffBlockHeaderFile(FFBMGetBlock(aFI,
                                                 aTI,
                                                 0,
                                                 True,
                                                 aFHRelMethod));
  try
    {find a new block}
    with aFileHeader^ do begin
      {if there are no deleted blocks...}
      if (bhfAvailBlocks = 0) then begin
        {Have we reached the max # of blocks? }
        if (bhfUsedBlocks = aFI^.fiMaxBlocks) then
          FFRaiseExceptionNoData(EffServerException,
                                 ffStrResServer,
                                 fferrTableFull);
{Begin !!.11}
        if ((TffSrDatabase(aTI^.tirTrans.DatabaseID).CheckSpace) and
            (not (fffaTemporary in aFI^.fiAttributes)) and             {!!.12}
            (FFGetDiskFreeSpace(ExtractFileDir(aFI^.fiName^)) <
             aFI^.fiBlockSizeK)) then
          FFRaiseExceptionNoData(EffServerException,
                                 ffStrResServer,
                                 fferrDiskFull);
{End !!.11}

        {get a brand new block}
        BlockNumber := bhfUsedBlocks;
        {Note: We don't need to lock the new block because the file's
               header block is exclusively locked.  This prevents
               other threads from adding the same block to the file
               and doesn't allow them to read it since they don't know
               about it yet. }
        Result := FFBMAddBlock(aFI, aTI, BlockNumber, aReleaseMethod);
        inc(bhfUsedBlocks);
        aFI^.fiUsedBlocks := bhfUsedBlocks;
        if not (fffaTemporary in aFI^.fiAttributes) then               {!!.12}
          aTI^.tirTrans.NewSpace := aTI^.tirTrans.NewSpace + aFI^.fiBlockSizeK; {!!.11}
      end
      else {...there are some deleted blocks} begin
        { Reuse the first in the deleted block chain. }
        BlockNumber := bhf1stFreeBlock;
        Result := FFBMGetBlock(aFI,
                               aTI,
                               BlockNumber,
                               True,
                               aReleaseMethod);
        bhf1stFreeBlock := BlockHdr^.bhfNextBlock;
        dec(bhfAvailBlocks);
      end;
    end;
    {set the only field in the block header we can}
    BlockHdr^.bhfThisBlock := BlockNumber;
  finally
    aFHRelMethod(PffBlock(aFileHeader));
  end;
end;
{--------}
procedure FFTblHlpDelete(const aPath      : TffPath;
                         const aTableName : TffTableName;
                         const aDict      : TffDataDictionary);
var
  SL  : TffStringList;
  i   : integer;
  TblName  : TffTableName;
  FullName : TffFullFileName;
begin
  SL := TffStringList.Create;
  try
    if (FFExtractExtension(aTableName) = ffc_ExtForData) then
      TblName := FFExtractFileName(aTableName)
    else
      TblName := aTableName;

    for i := 0 to Pred(aDict.FileCount) do begin
      if i = 0 then
        { Force file extension. }
        FullName := FFMakeFullFileName(aPath,
                                       FFMakeFileNameExt(TblName,
                                                         ffc_ExtForData))
      else
        { Use file extension from data dictionary. }
        FullName := FFMakeFullFileName(aPath,
                                       FFMakeFileNameExt(TblName,
                                                         aDict.FileExt[i]));
      FFDeleteFile(FullName);
    end;
  finally
    SL.Free;
  end;
end;
{--------}
procedure FFTblHlpDeleteBlock(aFI         : PffFileInfo;
                              aFileHeader : PffBlockHeaderFile;
                              aBlock      : PffBlock);
var
  BlockHdr : PffBlockHeaderFile absolute aBlock; {a convenient typecast}
begin
  { Assumption: File header block & aBlock have been exclusively locked. }

  {add it to the deleted block chain}
  with aFileHeader^ do begin
    {destroy the info in the block header}
    BlockHdr^.bhfSignature := ffc_SigFreeBlock;
    BlockHdr^.bhfNextBlock := bhf1stFreeBlock;
    {add it to the deleted block chain}
    bhf1stFreeBlock := BlockHdr^.bhfThisBlock;
    inc(bhfAvailBlocks);
  end;
end;
{--------}
procedure FFTblHlpRename(const aPath      : TffPath;
                         const aTableName : TffTableName;
                         const aNewName   : TffTableName;
                         const aDict      : TffDataDictionary);
var
  SL  : TffStringList;
  i   : integer;
  TblName  : TffTableName;
  NewName  : TffTableName;
  FullName : TffFullFileName;
  FullNewName : TffFullFileName;
begin
  SL := TffStringList.Create;
  try
    if (FFExtractExtension(aTableName) = ffc_ExtForData) then
      TblName := FFExtractFileName(aTableName)
    else
      TblName := aTableName;

    if (FFExtractExtension(aNewName) = ffc_ExtForData) then
      NewName := FFExtractFileName(aNewname)
    else
      NewName := aNewName;

    for i := 0 to Pred(aDict.FileCount) do begin
      if i = 0 then begin
        { Force file extension. }
        FullName := FFMakeFullFileName(aPath,
                                       FFMakeFileNameExt(TblName,
                                                         ffc_ExtForData));
        FullNewName := FFMakeFullFileName(aPath,
                                          FFMakeFileNameExt(NewName,
                                                            ffc_ExtForData));
      end else begin
        { Use file extension from data dictionary. }
        FullName := FFMakeFullFileName(aPath,
                                       FFMakeFileNameExt(TblName,
                                                         aDict.FileExt[i]));
        FullNewName := FFMakeFullFileName(aPath,
                                          FFMakeFileNameExt(NewName,
                                                            aDict.FileExt[i]));
      end;
      FFRenameFile(FullName, FullNewName);
    end;
  finally
    SL.Free;
  end;
end;
{====================================================================}


{===Buffer manager access routines===================================}
function FFBMAddBlock(aFI : PffFileInfo;
                      aTI : PffTransInfo;
                const aBlockNum : TffWord32;
                  var aReleaseMethod : TffReleaseMethod) : PffBlock;
begin
  Assert(assigned(aFI) and assigned(aFI^.fiBufMgr));
  Result := aFI^.fiBufMgr.AddBlock(aFI, aTI, aBlockNum, aReleaseMethod)
end;
{--------}
procedure FFBMDirtyBlock(aFI : PffFileInfo;
                   const aBlockNum : TffWord32;
                         aTI : PffTransInfo;
                     var aModifiableBlock : PffBlock);
begin
  Assert(assigned(aFI));
  aFI^.fiBufMgr.DirtyBlock(aFI, aBlockNum, aTI, aModifiableBlock);
end;
{--------}
function FFBMGetBlock(aFI : PffFileInfo;
                      aTI : PffTransInfo;
                const aBlockNum : TffWord32;
                const aMarkDirty : Boolean;
                  var aReleaseMethod : TffReleaseMethod) : PffBlock;
begin
  Assert(aTI <> nil, 'No transaction specified.');                     {!!.03}
  if Assigned(aFI) and Assigned(aFI^.fiBufMgr) then
    Result := aFI^.fiBufMgr.GetBlock(aFI, aBlockNum, aTI, aMarkDirty,
                                     aReleaseMethod)
  else
    Result := nil;
end;
{--------}
function FFBMGetFileHeaderBlock(aFI : PffFileInfo;
                                aTI : PffTransInfo;
                          const aMarkDirty : Boolean;
                            var aReleaseMethod : TffReleaseMethod) : PffBlock;
begin
  if Assigned(aFI) and Assigned(aFI^.fiBufMgr) then
    Result := aFI^.fiBufMgr.AddFile(aFI, aTI, aMarkDirty, aReleaseMethod)
  else
    Result := nil;
end;
{--------}
procedure FFBMRemoveFile(aFI : PffFileInfo);
begin
  if Assigned(aFI) and Assigned(aFI^.fiBufMgr) then
    aFI^.fiBufMgr.RemoveFile(aFI);
end;
{--------}
procedure FFBMUnlockBlock(aFI : PffFileInfo; aBlockNum : TffWord32);
begin
  if Assigned(aFI) and Assigned(aFI^.fiBufMgr) then
    aFI^.fiBufMgr.UnlockBlock(aFI, aBlockNum);
end;
{====================================================================}

{===Lock manager access routines=====================================}
procedure FFAcqRecordLock(aFI       : PffFileInfo;
                          aTI       : PffTransInfo;
                    const aRefNum   : TffInt64;
                    const aLockType : TffSrLockType;
                    const aDatabaseID : TffDatabaseID;                 {!!.10}
                    const aCursorID : TffCursorID;                     {!!.02}
                    const aConditional : Boolean);                     {!!.02}
var
  LockStatus : TffLockRequestStatus;
  RecStr     : string;
  RetryUntil : DWORD;
  TickCount  : DWORD;
begin

  { We support only exclusive locking of records. }
  if (aFI^.fiExclOwner = aCursorID) or (aLockType <> ffsltExclusive) then
    Exit;

  RetryUntil := FFGetRetry;
  TickCount := GetTickCount;

  { Do we have any time left?  Note that we are doing this check to avoid
    the situation where we ask for a lock and pass a negative timeout. }
  if (RetryUntil > TickCount) and
     ((RetryUntil - TickCount) >= 5) then begin
    { Obtain an exclusive lock on the record. }
    LockStatus := TffLockManager(aTI^.tirLockMgr).AcquireRecordLock
                                            (aRefNum, aFI,
                                             aLockType,
                                             aConditional,             {!!.02}
                                             (RetryUntil - TickCount),
                                             aTI^.tirTrans,
                                             aDatabaseID,              {!!.10}
                                             aCursorID);

    { Raise an exception if something went awry. }
    if LockStatus <> fflrsGranted then
      RecStr := format(ffcRecord,[aRefNum.iHigh, aRefNum.iLow]);
    case LockStatus of
      fflrsTimeout :
        FFRaiseException(EffServerException, ffStrResServer, fferrLockTimeout,
                         [FFMapLockToName(aLockType), RecStr, aFI^.fiName^]);
      fflrsRejected :
        FFRaiseException(EffServerException, ffStrResServer, fferrLockRejected,
                         [FFMapLockToName(aLockType), RecStr, aFI^.fiName^]);
    end;  { case }
  end
  else
    { No.  Assume we will time out waiting for the resource. }
    FFRaiseExceptionNoData(EffServerException, ffStrResServer,
                           fferrGeneralTimeout);
end;
{Begin !!.10}
{--------}
procedure FFRelaxRecordLock(aFI : PffFileInfo;
                            aTI : PffTransInfo;
                      const aCursorID : TffCursorID;
                      const aRefNum : TffInt64);
begin
  if (aFI^.fiExclOwner = aCursorID) then
    Exit;

  TffLockManager(aTI^.tirLockMgr).RelaxRecordLock
    (aRefNum, aFI, aTI^.tirTrans.DatabaseID);
end;
{End !!.10}
{--------}
procedure FFRelRecordLock(aFI : PffFileInfo; aTI : PffTransInfo;
                          const aRefNum : TffInt64;
                          const aDatabaseID : TffDatabaseID;           {!!.10}
                          const aCursorID : TffCursorID);
begin
  if (aFI^.fiExclOwner = aCursorID) then
    Exit;

  TffLockManager(aTI^.tirLockMgr).ReleaseRecordLock
    (aRefNum, aFI, aTI^.tirTrans, aDatabaseID);                        {!!.10}
end;
{====================================================================}

end.
