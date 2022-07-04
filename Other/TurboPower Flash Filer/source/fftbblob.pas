{*********************************************************}
{* FlashFiler: Table BLOB access                         *}
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

{!!.11 - Added logging}
{ Uncomment the following define to enable BLOB tracing. }
{.$DEFINE BLOBTrace}

unit fftbblob;

interface

uses
  Classes,                                                             {!!.03}
  Windows,
  SysUtils,
  ffconst,
  ffllbase,
  ffsrmgr,
  ffllexcp,
  ffsrbase,
  ffsrlock,
  fffile,
  fftbbase;

{---BLOB Link method types---}
type
  TffBLOBLinkGetLength = function(const aTableName : TffTableName;
                                  const aBLOBNr    : TffInt64;
                                    var aLength    : Longint) : TffResult of object;
    { Declaration of method to be called when trying to find the length
      of a BLOB visible through a BLOB link. }

  TffBLOBLinkRead = function(const aTableName : TffTableName;
                             const aBLOBNr    : TffInt64;
                             const aOffset    : TffWord32;             {!!.06}
                             const aLen       : TffWord32;             {!!.06}
                               var aBLOB;
                               var aBytesRead : TffWord32)             {!!.06}
                                              : TffResult of object;
    { Declaration of a method to be called when trying to read a BLOB visible
      through a BLOB link. }

{---BLOB maintenance---}
procedure FFTblAddBLOB(aFI     : PffFileInfo;
                       aTI     : PffTransInfo;
                   var aBLOBNr : TffInt64);
  {-add a new, empty (length 0) BLOB, return new BLOB number}

procedure FFTblAddBLOBLink(aFI          : PffFileInfo;
                           aTI          : PffTransInfo;
                     const aTableName   : TffTableName;
                     const aTableBLOBNr : TffInt64;
                       var aBLOBNr      : TffInt64);
  {-Add a new BLOB link, return new BLOB number. }

procedure FFTblAddFileBLOB(aFI       : PffFileInfo;
                           aTI       : PffTransInfo;
                     const aFileName : TffFullFileName;
                       var aBLOBNr   : TffInt64);
  {-add a new file BLOB, return new BLOB number}

procedure FFTblDeleteBLOB(aFI     : PffFileInfo;
                          aTI     : PffTransInfo;
                    const aBLOBNr : TffInt64);
  {-delete a BLOB; BLOB number will no longer be valid after this}

function FFTblFreeBLOB(aFI     : PffFileInfo;
                       aTI     : PffTransInfo;
                       aBLOBNr : TffInt64) : boolean;
  {-if the BLOB length is zero, delete it; return true if deleted}

function FFTblGetBLOBLength(aFI     : PffFileInfo;
                            aTI     : PffTransInfo;
                            aBLOBNr : TffInt64;
                            aLengthMethod : TffBLOBLinkGetLength;
                        var aFBError: TffResult) : Longint;
  {-return the length of the BLOB}

function FFTblGetFileNameBLOB(aFI       : PffFileInfo;
                              aTI       : PffTransInfo;
                              aBLOBNr   : TffInt64;
                          var aFileName : TffFullFileName ) : Boolean;
  {-return True if the given BLOB nr refers to a file BLOB, and the
    filename is returned in aFileName}

function FFTblIsBLOBLink(aFI             : PffFileInfo;                {!!.11 - New}
                         aTI             : PffTransInfo;
                         aBLOBNr         : TffInt64;
                     var aSrcTableName   : TffTableName;
                     var aSrcTableBLOBNr : TffInt64)
                                         : Boolean;
  { Checks to see if aBLOBNr is a BLOB Link. If it is, it returns the
    the offset of the source as aSrcTableBLOBNr in aSrcTableName

{Begin !!.03}
procedure FFTblListBLOBSegments(aFI : PffFileInfo;
                                aTI : PffTransInfo;
                                aBLOBNr : TffInt64;
                                aStream : TStream);
  { List the segments comprising the BLOB. }
{End !!.03}

{Begin !!.11}
type
  TffBaseBLOBEngine = class;  { foward declaration }
  TffBLOBEngineClass = class of TffBaseBLOBEngine;
  
  TffBaseBLOBEngine = class(TffObject)
    { Base class representing an engine to read, write, & truncate BLOBs. }
  public
    class function GetEngine(aFI : PffFileInfo) : TffBaseBLOBEngine;
      { Returns the engine instance to be used for the specified file. }
      
    procedure Read(aFI         : PffFileInfo;
                   aTI         : PffTransInfo;
                   aBLOBNr     : TffInt64;
                   aOffset     : TffWord32;
                   aLen        : TffWord32;
                   aReadMethod : TffBLOBLinkRead;
               var aBLOB;
               var aBytesRead  : TffWord32;
               var aFBError    : TffResult); virtual; abstract;
      { Read all or part of a BLOB}

    procedure Truncate(aFI     : PffFileInfo;
                       aTI     : PffTransInfo;
                       aBLOBNr : TffInt64;
                       aLen    : TffWord32); virtual; abstract;
      { Truncate the BLOB to the specified length. Does *not* delete BLOB if
        length 0. }

    procedure Write(aFI     : PffFileInfo;
                    aTI     : PffTransInfo;
              const aBLOBNr : TffInt64;
                    aOffset : TffWord32;
                    aLen    : TffWord32;
              const aBLOB); virtual; abstract;
      { Write to or append to a BLOB. }
  end;

  TffBLOBEngine = class(TffBaseBLOBEngine)
    { This class provides an interface to BLOBs in 2.1.0.1 and later. The logic
      supports the improved nesting algorithm that recycles all available
      BLOB segments regardless of size. }
{Begin !!.12}
  protected
    function IsEmptyLookupEntry(Entry : PffBLOBLookupEntry) : Boolean;
{End !!.12}
  public
    procedure Read(aFI         : PffFileInfo;
                   aTI         : PffTransInfo;
                   aBLOBNr     : TffInt64;
                   aOffset     : TffWord32;
                   aLen        : TffWord32;
                   aReadMethod : TffBLOBLinkRead;
               var aBLOB;
               var aBytesRead  : TffWord32;
               var aFBError    : TffResult); override;
      { Read all or part of a BLOB}

    procedure Truncate(aFI     : PffFileInfo;
                       aTI     : PffTransInfo;
                       aBLOBNr : TffInt64;
                       aLen    : TffWord32); override;
      { Truncate the BLOB to the specified length. Does *not* delete BLOB if
        length 0. }

    procedure Write(aFI     : PffFileInfo;
                    aTI     : PffTransInfo;
              const aBLOBNr : TffInt64;
                    aOffset : TffWord32;
                    aLen    : TffWord32;
              const aBLOB); override;
      { Write to or append to a BLOB. }
  end;

  Tff210BLOBEngine = class(TffBaseBLOBEngine)
    { This class provides an interface to BLOBs that is compatible with tables
      created under versions 2.0.0.0 to 2.1.0.0. }
  public
    procedure Read(aFI         : PffFileInfo;
                   aTI         : PffTransInfo;
                   aBLOBNr     : TffInt64;
                   aOffset     : TffWord32;
                   aLen        : TffWord32;
                   aReadMethod : TffBLOBLinkRead;
               var aBLOB;
               var aBytesRead  : TffWord32;
               var aFBError    : TffResult); override;
      { Read all or part of a BLOB}

    procedure Truncate(aFI     : PffFileInfo;
                       aTI     : PffTransInfo;
                       aBLOBNr : TffInt64;
                       aLen    : TffWord32); override;
      { Truncate the BLOB to the specified length. Does *not* delete BLOB if
        length 0. }

    procedure Write(aFI     : PffFileInfo;
                    aTI     : PffTransInfo;
              const aBLOBNr : TffInt64;
                    aOffset : TffWord32;
                    aLen    : TffWord32;
              const aBLOB); override;
      { Write to or append to a BLOB. }
  end;

function  FFTblRebuildLookupSegments(aFI           : PffFileInfo;
                                     aTI           : PffTransInfo;
                                     aNewBLOBSize  : TffWord32;
                                     aOldBLOBSize  : TffWord32;
                               const aBLOBNr       : TffInt64)
                                                   : TffInt64;
  {-rebuilds all lookup segment(s) for a BLOB that is growing}

{End !!.11}

implementation

uses
  fflllog,                                                             {!!.13}
  ffsrbde,
  ffsrblob;

resourcestring
  ffcBLOBSegExpected = ' Expected %s segment but segment marked with ''%s''.';
  ffcBLOBSegHeader   = 'header';

const
  ffc_FileBLOB = -1;
  ffc_BLOBLink = -2;

{Begin !!.11}
var
  FFBLOBEngine : TffBLOBEngine;
  FF210BLOBEngine : Tff210BLOBEngine;
{End !!.11}

{Begin !!.13}
{$IFDEF BLOBTrace}
var
  btLog : TffEventLog;

procedure Logbt(aMsg : string; args : array of const);
begin
  if btLog <> nil then
    btLog.WriteStringFmt(aMsg, args);
end;
{$ENDIF}
{End !!.13}

{== Calculation routines =============================================}
function EstimateSegmentCount(const aBLOBSize, aMaxSegSize : Integer)
                                                           : Integer;
begin
  Result := ((aBLOBSize * 2) div aMaxSegSize) + 1;
end;

{Begin !!.11}
function CalcBLOBSegNumber(const aOffset    : TffWord32;
                           const aBlockSize : TffWord32;
                           var   aOfsInSeg  : TffWord32) : TffWord32;
  {-Calculate the segment number for an offset into a BLOB.}
  {-aOfsInSeg tells us how much of the last segment (result)
    we're using}
var
  MaxSegSize  : TffWord32;
begin
  {offset 0 is in the 1st segment}
  if aOffset = 0 then begin
    Result := 1;
    aOfsInSeg := 0;
  end else begin
    MaxSegSize := (((aBlockSize - ffc_BlockHeaderSizeBLOB)
                    div ffc_BLOBSegmentIncrement) * ffc_BLOBSegmentIncrement) -
                  sizeof(TffBLOBSegmentHeader);
    aOfsInSeg := 0;

    Result := aOffset div MaxSegSize;
    aOfsInSeg := aOffset - (Result * MaxSegSize);
    if aOfsInSeg > 0 then
      inc(Result)
    else if (aOfsInSeg = 0) and
            (aOffset <> 0) then
      aOfsInSeg := MaxSegSize;
  end; {if..else}
end;
{=====================================================================}

{== BLOB link routines ===============================================}
function BLOBLinkGetLength(aBLOBHeader      : PffBLOBHeader;
                           aGetLengthMethod : TffBLOBLinkGetLength;
                       var aLength          : Longint)
                                            : TffResult;
var
  BLOBData     : PffByteArray absolute aBLOBHeader;
  BLOBNr       : TffInt64;
  TableName    : TffFullFileName;
  TableNameLen : Byte;
begin
  { Get the length of the table name. }
  Move(BLOBData^[sizeof(TffBLOBHeader)], TableNameLen, sizeOf(TableNameLen));
  Inc(TableNameLen);
  { Copy the file name to TableName. }
  Move(BLOBData^[sizeof(TffBLOBHeader)], TableName, TableNameLen);
  { Get the table's BLOB number. }
  Move(BLOBData^[SizeOf(TffBLOBHeader) + TableNameLen], BlobNr,
       SizeOf(TffInt64));

  Result := aGetLengthMethod(TableName, BlobNr, aLength);

end;
{--------}
procedure BLOBLinkGetTableNameAndRefNr(aBLOBBlock   : PffBlock;        {!!.11 - New}
                                       aBlockOffset : Integer;
                                   var aTableName   : TffTableName;
                                   var aBLOBNr      : TffInt64);
var
  TableNameLen : Byte;
begin
  { Get the length of the table name. }
  Inc(aBlockOffset, SizeOf(TffBLOBHeader));
  Move(aBLOBBlock^[aBlockOffset], TableNameLen, SizeOf(TableNameLen));
  Inc(TableNameLen);
  { Copy the file name to TableName. }
  Move(aBLOBBlock^[aBlockOffset], aTableName, TableNameLen);
  { Get the table's BLOB number. }
  Move(aBLOBBlock^[aBlockOffset + TableNameLen],
       aBLOBNr,
       SizeOf(TffInt64));
end;
{--------}
function BLOBLinkRead(aFI         : PffFileInfo;
                      aTI         : PffTransInfo;
                      aBLOBNr     : TffInt64;
                      aOffset     : Longint;
                      aLen        : Longint;
                      aReadMethod : TffBLOBLinkRead;
                  var aBLOB;
                  var aBytesRead  : TffWord32)                         {!!.06}
                                  : TffResult;
var
  BLOBBlock     : PffBlock;
  BLOBNr        : TffInt64;
  OffsetInBlock : TffWord32;                                           {!!.11}
  {TableNameLen  : Byte;}                                              {!!.11}
  TableName     : TffTableName;
  aFHRelMethod  : TffReleaseMethod;
begin
  BLOBBlock := ReadVfyBlobBlock(aFI,
                                aTI,
                                ffc_ReadOnly,
                                aBLOBNr,
                                OffsetInBlock,
                                aFHRelMethod);
  try
    BLOBLinkGetTableNameAndRefNr(BLOBBlock,                            {!!.11}
                                 OffsetInBlock,
                                 TableName,
                                 BLOBNr);
    Result := aReadMethod(TableName,
                          BlobNr,
                          aOffset,
                          aLen,
                          aBLOB,
                          aBytesRead);
  finally
    aFHRelMethod(BLOBBlock);
  end;
end;
{=====================================================================}

{== File BLOB routines ===============================================}
function FileBLOBLength(aBLOBHeader : PffBLOBHeader;
                    var aLength     : Longint)
                                    : TffResult;
var
  BLOBFile    : PffFileInfo;
  BLOBData    : PffByteArray absolute aBLOBHeader;
  FileName    : TffFullFileName;
  FileNameLen : Byte;
  TmpLen      : TffInt64;
begin
  Result := 0;

  {Get the length of the file name}
  Move(BLOBData^[sizeof(TffBLOBHeader)], FileNameLen, sizeOf(FileNameLen));
  {copy the file name to FileName}
  Move(BLOBData^[sizeof(TffBLOBHeader)], FileName, succ(FileNameLen));

  try
    BLOBFile := FFAllocFileInfo(FileName, FFExtractExtension(FileName), nil);
    try
      FFOpenFile(BLOBFile, omReadOnly, smShared, false, false);
      try
        TmpLen := FFPositionFileEOF(BLOBFile);
        aLength := TmpLen.iLow;
      finally
        FFCloseFile(BLOBFile);
      end;{try..finally}
    finally
      FFFreeFileInfo(BLOBFile);
    end;{try..finally}
  except
    on E : EffException do begin
      case E.ErrorCode of
        fferrOpenFailed  : Result := DBIERR_FF_FileBLOBOpen;
        fferrCloseFailed : Result := DBIERR_FF_FileBLOBClose;
        fferrReadFailed  : Result := DBIERR_FF_FileBLOBRead;
        fferrSeekFailed  : Result := DBIERR_FF_FileBLOBRead;
      else
        raise
      end;{case}
    end;
  end;{try..except}
end;
{--------}
function FileBLOBRead(aFI        : PffFileInfo;
                      aTI        : PffTransInfo;
                      aBLOBNr    : TffInt64;
                      aOffset    : Longint;
                      aLen       : Longint;
                  var aBLOB;
                  var aBytesRead : TffWord32)                          {!!.06}
                                 : TffResult;
var
  BLOBFile      : PffFileInfo;
  BLOBBlock     : PffBlock;
  OffsetInBlock : TffWord32;                                           {!!.11}
  FileNameLen   : Byte;
  FileName      : TffFullFileName;
  TempI64       : TffInt64;
  aFHRelMethod  : TffReleaseMethod;
begin
  Result := 0;
  BLOBBlock := ReadVfyBlobBlock(aFI,
                                aTI,
                                ffc_ReadOnly,
                                aBLOBNr,
                                OffsetInBlock,
                                aFHRelMethod);
  try
    {Get the length of the file name}
    Move(BLOBBlock^[(OffsetInBlock + sizeof(TffBLOBHeader))],
         FileNameLen, sizeOf(FileNameLen));
    {copy the file name to FileName}
    Move(BLOBBlock^[(OffsetInBlock + sizeof(TffBLOBHeader))],
         FileName, succ(FileNameLen));
    try
      BLOBFile := FFAllocFileInfo(FileName, FFExtractExtension(FileName), nil);
      try
        FFOpenFile(BLOBFile, omReadOnly, smShared, false, false);
        try
          TempI64.iLow  := aOffset;
          TempI64.iHigh := 0;
          FFPositionFile(BLOBFile, TempI64);
          aBytesRead := FFReadFile(BLOBFile, aLen, aBLOB);
        finally
          FFCloseFile(BLOBFile);
        end;{try..finally}
      finally
        FFFreeFileInfo(BLOBFile);
      end;{try..finally}
    except
      on E : EffException do begin
        case E.ErrorCode of
          fferrOpenFailed  : Result := DBIERR_FF_FileBLOBOpen;
          fferrCloseFailed : Result := DBIERR_FF_FileBLOBClose;
          fferrReadFailed  : Result := DBIERR_FF_FileBLOBRead;
          fferrSeekFailed  : Result := DBIERR_FF_FileBLOBRead;
        else
          raise
        end;{case}
      end;
    end;{try..except}
  finally
    aFHRelMethod(BLOBBlock);
  end;
end;
{=====================================================================}

{== BLOB maintenance =================================================}
procedure FFTblAddBLOB(aFI     : PffFileInfo;
                       aTI     : PffTransInfo;
                   var aBLOBNr : TffInt64);
var
  FileHeader    : PffBlockHeaderFile;
  BLOBHeaderPtr : PffBLOBHeader;
  BLOBBlock     : PffBlock;
  SegSize       : TffWord32;                                           {!!.11}
  OffsetInBlock : TffWord32;                                           {!!.11}
  aBlkRelMethod,
  aFHRelMethod  : TffReleaseMethod;
begin
{$IFDEF BLOBTrace}                                                     {!!.11}
  Logbt('FFTblAddBLOB.Begin', []);
{$ENDIF}
  { First get the file header, block 0. }
  FileHeader := PffBlockHeaderFile(FFBMGetBlock(aFI,
                                                aTI,
                                                0,
                                                ffc_MarkDirty,
                                                aFHRelMethod));
  try
    { Create a new BLOB header. }
    SegSize := ffc_BLOBHeaderSize;                                     {!!.11}
    aBLOBNr := aFI^.fiBLOBrscMgr.NewSegment(aFI,
                                            aTI,
                                            SegSize,                   {!!.11}
                                            SegSize);                  {!!.11}
    BLOBBlock := ReadVfyBlobBlock(aFI,
                                  aTI,
                                  ffc_MarkDirty,
                                  aBLOBNr,
                                  OffsetInBlock,
                                  aBlkRelMethod);
    try
      BLOBHeaderPtr := @BLOBBlock^[OffsetInBlock];
      {set up the new BLOB header}
      with BLOBHeaderPtr^ do begin
        bbhSignature := ffc_SigBLOBSegHeader;
        bbhSegmentLen := (((sizeof(TffBLOBHeader) + pred(ffc_BLOBSegmentIncrement)) div
                          ffc_BLOBSegmentIncrement) * ffc_BLOBSegmentIncrement);
        bbhBLOBLength := 0;
        bbhSegCount := 0;
        bbh1stLookupSeg.iLow := ffc_W32NoValue;                        {!!.11}
      end;
      {we've got one more BLOB}
      inc(FileHeader^.bhfBLOBCount);
    finally
      aBlkRelMethod(BLOBBlock);
    end;
  finally
    aFHRelMethod(PffBlock(FileHeader));
  end;
end;
{--------}
procedure FFTblAddBLOBLink(aFI          : PffFileInfo;
                           aTI          : PffTransInfo;
                     const aTableName   : TffTableName;
                     const aTableBLOBNr : TffInt64;
                       var aBLOBNr      : TffInt64);
var
  FileHeader    : PffBlockHeaderFile;
  BLOBBlock     : PffBlock;
  BLOBBlockHdr  : PffBlockHeaderBLOB absolute BLOBBlock;
  SegSize       : TffWord32;                                           {!!.11}
  OffsetInBlock : TffWord32;                                           {!!.11}
  BLOBHeaderPtr : PffBLOBHeader;
  LinkLen,
  NameLen       : TffWord32;                                           {!!.11}
  aBlkRelMethod,
  aFHRelMethod  : TffReleaseMethod;
begin
  { First get the file header, block 0. }
  FileHeader := PffBlockHeaderFile(FFBMGetBlock(aFI,
                                                aTI,
                                                0,
                                                ffc_MarkDirty,
                                                aFHRelMethod));
  try
    { Create a new BLOB header. }
    NameLen := succ(Length(aTableName));
    LinkLen := succ(Length(aTableName) + SizeOf(aTableBLOBNr));
    SegSize := ffc_BLOBHeaderSize + LinkLen;                           {!!.11}
    aBLOBNr := aFI^.fiBLOBrscMgr.NewSegment(aFI,
                                            aTI,
                                            SegSize,                   {!!.11}
                                            SegSize);                  {!!.11}
    if (aBLOBNr.iLow <> ffc_W32NoValue) then begin
      BLOBBlock := ReadVfyBlobBlock(aFI,
                                    aTI,
                                    ffc_MarkDirty,
                                    aBLOBNr,
                                    OffsetInBlock,
                                    aBlkRelMethod);
      BLOBHeaderPtr := @BLOBBlock^[OffsetInBlock];
    end else begin
      aBLOBNr.iLow  := ffc_W32NoValue;
      Exit;
    end;
    { Set up the new BLOB header. }
    with BLOBHeaderPtr^ do begin
      bbhSignature := ffc_SigBLOBSegHeader;
      bbhBLOBLength := 0;
      bbhSegCount := ffc_BLOBLink;
      bbh1stLookupSeg.iLow := ffc_W32NoValue;
    end;
    { Write aTableName & the table's BLOB number after BLOBHeader.  Note that
      length of string is automatically stored as the first byte of the string. }
    Move(aTableName, BLOBBlock^[(OffsetInBlock + sizeof(TffBLOBHeader))],
         NameLen);
    Move(aTableBLOBNr, BLOBBlock^[(OffsetInBlock + SizeOf(TffBLOBHeader) +
                                   NameLen)], SizeOf(TffInt64));
    { We've got one more BLOB. }
    inc(FileHeader.bhfBLOBCount);
    aBlkRelMethod(BLOBBlock);
  finally
    aFHRelMethod(PffBlock(FileHeader));
  end;
end;
{--------}
procedure FFTblAddFileBLOB(aFI       : PffFileInfo;
                           aTI       : PffTransInfo;
                     const aFileName : TffFullFileName;
                       var aBLOBNr   : TffInt64);
var
  FileHeader    : PffBlockHeaderFile;
  BLOBBlock     : PffBlock;
  BLOBBlockHdr  : PffBlockHeaderBLOB absolute BLOBBlock;
  SegSize       : TffWord32;                                           {!!.11}
  OffsetInBlock : TffWord32;                                           {!!.11}
  BLOBHeaderPtr : PffBLOBHeader;
  FileNameLen   : Integer;
  aBlkRelMethod,
  aFHRelMethod  : TffReleaseMethod;
begin
  {first get the file header, block 0}
  FileHeader := PffBlockHeaderFile(FFBMGetBlock(aFI,
                                                aTI,
                                                0,
                                                ffc_MarkDirty,
                                                aFHRelMethod));
  try
    {create a new BLOB header}
    FileNameLen := succ(Length(aFileName));
    SegSize := ffc_BLOBHeaderSize + FileNameLen;                       {!!.11}
    aBLOBNr := aFI^.fiBLOBrscMgr.NewSegment(aFI,
                                            aTI,
                                            SegSize,                   {!!.11}
                                            SegSize);                  {!!.11}
    if (aBLOBNr.iLow <> ffc_W32NoValue) then begin
      BLOBBlock := ReadVfyBlobBlock(aFI,
                                    aTI,
                                    ffc_MarkDirty,
                                    aBLOBNr,
                                    OffsetInBlock,
                                    aBlkRelMethod);
      BLOBHeaderPtr := @BLOBBlock^[OffsetInBlock];
    end else begin
      aBLOBNr.iLow  := ffc_W32NoValue;
      exit;
    end;
    {set up the new BLOB header}
    with BLOBHeaderPtr^ do begin
      bbhSignature := ffc_SigBLOBSegHeader;
      bbhBLOBLength := 0;
      bbhSegCount := ffc_FileBLOB;
      bbh1stLookupSeg.iLow := ffc_W32NoValue;
    end;
    { Write aFileName after BLOBHeader.  Note that length of string is
      automatically stored as the first byte of the string. }
    Move(aFileName, BLOBBlock^[(OffsetInBlock + sizeof(TffBLOBHeader))], FileNameLen);
    {we've got one more BLOB}
    inc(FileHeader.bhfBLOBCount);
    aBlkRelMethod(BLOBBlock);
  finally
    aFHRelMethod(PffBlock(FileHeader));
  end;
end;
{--------}
procedure FFTblDeleteBLOBPrim(aFI        : PffFileInfo;
                              aTI        : PffTransInfo;
                              BLOBHeader : PffBLOBHeader);
var
  OffsetInBlock : TffWord32;                                           {!!.11}
  LookupSegBlk  : PffBlock;
  LookupSegOfs,                                                        {!!.03}
  TmpSegOfs     : TffInt64;                                            {!!.03}
  LookupSegPtr  : PffBLOBSegmentHeader;
  LookupEntOfs  : integer;
  LookupEntPtr  : PffBLOBLookupEntry;
  EntryCount,                                                          {!!.03}
  RemainEntries : Integer;                                             {!!.03}
  i             : Integer;
  aRelMethod    : TffReleaseMethod;
begin
{$IFDEF BLOBTrace}                                                     {!!.11}
  Logbt('FFTblDeleteBLOBPrim.Begin', []);
{$ENDIF}

  { Assumption: File header block is exclusively locked. }

  { Get the BLOB's first lookup segment. }
  LookupSegOfs := BLOBHeader^.bbh1stLookupSeg;

{Begin !!.03}
  { BLOB truncated to length 0? }
  if LookupSegOfs.iLow = ffc_W32NoValue then
    Exit;
{End !!.03}

  LookupSegBlk := ReadVfyBlobBlock(aFI,
                                   aTI,
                                   ffc_MarkDirty,
                                   LookupSegOfs,
                                   OffsetInBlock,
                                   aRelMethod);
  LookupSegPtr := @LookupSegBlk^[OffsetInBlock];
  LookupEntOfs := OffsetInBlock + sizeof(TffBLOBSegmentHeader);

  try
    { Get the first lookup entry in the lookup segment. }
    LookupEntPtr := @LookupSegBlk^[LookupEntOfs];

    { Is this the only lookup segment? }
    if LookupSegPtr^.bshNextSegment.iLow <> ffc_W32NoValue then
      { No.  Figure out number of lookup entries based on segment size. }
      EntryCount := FFCalcMaxLookupEntries(LookupSegPtr)
    else
      { Yes.  Number of lookup entries = number of content segments. }
      EntryCount := BLOBHeader^.bbhSegCount;

    RemainEntries := BLOBHeader^.bbhSegCount;                          {!!.03}

    { Free each content segment. }
    dec(RemainEntries, EntryCount);                                    {!!.03}
    for i := 1 to BLOBHeader^.bbhSegCount do begin
      aFI^.fiBLOBrscMgr.DeleteSegment(aFI, aTI, LookupEntPtr^.bleSegmentOffset);
      dec(EntryCount);

      { Need to move to another lookup segment? }
      if ((EntryCount = 0) and (LookupSegPtr^.bshNextSegment.iLow <> ffc_W32NoValue)) then begin
        {Yes.  Get the location of the next lookup segment and delete the
         existing lookup segment. }
        TmpSegOfs := LookupSegPtr^.bshNextSegment;                     {!!.03}
        aFI^.fiBLOBrscMgr.DeleteSegment(aFI, aTI, LookupSegOfs);
        LookupSegOfs := TmpSegOfs;                                     {!!.03}

        { Grab the next lookup segment. }
        aRelMethod(LookupSegBlk);
        LookupSegBlk := ReadVfyBlobBlock(aFI, aTI, ffc_MarkDirty,
                                         LookupSegOfs, OffsetInBlock,
                                         aRelMethod);
        LookupSegPtr := @LookupSegBlk^[OffsetInBlock];
        LookupEntOfs := OffsetInBlock + sizeof(TffBLOBSegmentHeader);
        EntryCount := FFCalcMaxLookupEntries(LookupSegPtr);
{Begin !!.03}
        if RemainEntries > EntryCount then
          dec(RemainEntries, EntryCount)
        else begin
          EntryCount := RemainEntries;
          RemainEntries := 0;
        end;
      end
      else
        { Grab the next lookup entry. }
        LookupEntOfs := LookupEntOfs + sizeof(TffBLOBLookupEntry);
        
      LookupEntPtr := @LookupSegBlk^[LookupEntOfs];
{End !!.03}
    end; {for}

    { Delete the last lookup segment.}
    aFI^.fiBLOBrscMgr.DeleteSegment(aFI, aTI, LookupSegOfs);
  finally
    aRelMethod(LookupSegBlk);
  end;
end;
{--------}
procedure FFTblDeleteBLOB(aFI     : PffFileInfo;
                          aTI     : PffTransInfo;
                    const aBLOBNr : TffInt64);
var
  FileHeader    : PffBlockHeaderFile;
  BLOBBlock     : PffBlock;
  BLOBBlockHdr  : PffBlockHeaderBLOB absolute BLOBBlock;
  BLOBHeader    : PffBLOBHeader;
  OffsetInBlock : TffWord32;                                           {!!.11}
  aBlkRelMethod,
  aFHRelMethod  : TffReleaseMethod;
begin
{$IFDEF BLOBTrace}                                                     {!!.11}
  Logbt('FFTblDeleteBLOB.Begin', []);
{$ENDIF}
  {first get the file header, block 0}
  FileHeader := PffBlockHeaderFile(FFBMGetBlock(aFI, aTI, 0, ffc_MarkDirty,
                                   aFHRelMethod));
  try
    {read and verify the BLOB header block}
    BLOBBlock := ReadVfyBlobBlock(aFI,
                                  aTI,
                                  ffc_MarkDirty,
                                  aBLOBNr,
                                  OffsetInBlock,
                                  aBlkRelMethod);
    BLOBHeader := @BLOBBlock^[OffsetInBlock];
{Begin !!.01}
    { Verify the BLOB has not been deleted. }
    if (BLOBHeader^.bbhSignature = ffc_SigBLOBSegDeleted) then
      FFRaiseException(EffServerException, ffStrResServer,
                       fferrBLOBDeleted,
                       [aFI^.fiName^, aBLOBNr.iHigh, aBLOBNr.iLow]);
{End !!.01}

    try
      FFTblDeleteBLOBPrim(aFI, aTI, BLOBHeader);

      { Delete the BLOB header}
      aFI^.fiBLOBrscMgr.DeleteSegment(aFI, aTI, aBLOBNr);

      { We've got one less BLOB. }
      dec(FileHeader.bhfBLOBCount);
    finally
      aBlkRelMethod(BLOBBlock);
    end;
  finally
    aFHRelMethod(PffBlock(FileHeader));
  end;
end;
{--------}
function FFTblFreeBLOB(aFI     : PffFileInfo;
                       aTI     : PffTransInfo;
                       aBLOBNr : TffInt64)
                               : Boolean;
var
  BLOBBlock    : PffBlock;
  BLOBBlockHdr : PffBlockHeaderBLOB absolute BLOBBlock;
  BLOBBlockNum : TffWord32;
  BLOBHeader   : PffBLOBHeader;
  FileHeader   : PffBlockHeaderFile;
  OffsetInBlock: TffWord32;                                            {!!.11}
  TempI64      : TffInt64;
  aBlkRelMethod,
  aFHRelMethod : TffReleaseMethod;
begin
{$IFDEF BLOBTrace}                                                     {!!.11}
  Logbt('FFTblFreeBLOB.Begin', []);
  Logbt('  aBLOBNr = %d:%d', [aBLOBNr.iLow, aBLOBNr.iHigh]);
{$ENDIF}
  { Assume we won't delete. }
  Result := false;
  FileHeader := nil;

  {now get the BLOB block}
  ffShiftI64R(aBLOBNr, aFI^.fiLog2BlockSize, TempI64);
  BLOBBlockNum := TempI64.iLow;

  { Read and verify the BLOB header block. }
  BLOBBlock := ReadVfyBlobBlock2(aFI,
                                 aTI,
                                 ffc_ReadOnly,
                                 aBLOBNr,
                                 BLOBBlockNum,
                                 OffsetInBlock,
                                 aBlkRelMethod);
  BLOBHeader := @BLOBBlock^[OffsetInBlock];
{Begin !!.01}
  { Verify the BLOB has not been deleted. }
  if (BLOBHeader^.bbhSignature = ffc_SigBLOBSegDeleted) then
    FFRaiseException(EffServerException, ffStrResServer,
                     fferrBLOBDeleted,
                     [aFI^.fiName^, aBLOBNr.iHigh, aBLOBNr.iLow]);
{End !!.01}

  try
    {don't bother doing anything if the BLOB's length > 0}
    if (BLOBHeader^.bbhBLOBLength > 0) then
      Exit;

    { We don't need to obtain exclusive locks on file header page or BLOB page
      because the BLOB resource manager's DeleteSegment routine will do so. }

    { Delete the BLOB's header. }
    aFI^.fiBLOBrscMgr.DeleteSegment(aFI, aTI, aBLOBNr);

    { One less BLOB. }
    FileHeader := PffBlockHeaderFile(FFBMGetBlock(aFI, aTI, 0, ffc_MarkDirty,
                                                  aFHRelMethod));
    dec(FileHeader.bhfBLOBCount);

    { We did delete. }
    Result := true;
  finally
    if assigned(FileHeader) then
      aFHRelMethod(PffBlock(FileHeader));
    aBlkRelMethod(BLOBBlock);
  end;
end;
{--------}
function FFTblGetBLOBLength(aFI           : PffFileInfo;
                            aTI           : PffTransInfo;
                            aBLOBNr       : TffInt64;
                            aLengthMethod : TffBLOBLinkGetLength;
                        var aFBError      : TffResult)
                                          : Longint;
var
  BLOBBlock     : PffBlock;
  BLOBBlockHdr  : PffBlockHeaderBLOB absolute BLOBBlock;
  BLOBBlockNum  : TffWord32;
  BLOBHeader    : PffBLOBHeader;
  OffsetInBlock : TffWord32;                                           {!!.11}
  aRelMethod    : TffReleaseMethod;
begin
{$IFDEF BLOBTrace}                                                     {!!.11}
  Logbt('FFTblGetBLOBLength.Begin', []);
  Logbt('  aBLOBNr = %d:%d', [aBLOBNr.iLow, aBLOBNr.iHigh]);
{$ENDIF}
  aFBError := DBIERR_NONE;
  { Read and verify the BLOB header block for this BLOB number. }
  BLOBBlock := ReadVfyBlobBlock2(aFI,
                                 aTI,
                                 ffc_ReadOnly,
                                 aBLOBNr,
                                 BLOBBlockNum,
                                 OffsetInBlock,
                                 aRelMethod);
  try
    BLOBHeader := @BLOBBlock^[OffsetInBlock];
{Begin !!.01}
    { Verify the BLOB has not been deleted. }
    if (BLOBHeader^.bbhSignature = ffc_SigBLOBSegDeleted) then
      FFRaiseException(EffServerException, ffStrResServer,
                       fferrBLOBDeleted,
                       [aFI^.fiName^, aBLOBNr.iHigh, aBLOBNr.iLow]);
{End !!.01}
    { Verify this is a header segment. }
    if (BLOBHeader^.bbhSignature <> ffc_SigBLOBSegHeader) then
      FFRaiseException(EffServerException, ffStrResServer, fferrBadBLOBSeg,
                       [aFI^.fiName^, aBLOBNr.iLow, aBLOBNr.iHigh,
                        format(ffcBLOBSegExpected,
                               [ffcBLOBSegHeader,
                                char(BLOBHeader^.bbhSignature)])]);
    { What kind of BLOB are we dealing with? }
    case BLOBHeader^.bbhSegCount of
      ffc_FileBLOB : { File BLOB }
        aFBError := FileBLOBLength(BLOBHeader, Result);
      ffc_BLOBLink : { BLOB link }
        begin
          Assert(assigned(aLengthMethod));
          aFBError := BLOBLinkGetLength(BLOBHeader, aLengthMethod, Result);
        end;
    else   { Standard BLOB }
      Result := BLOBHeader^.bbhBLOBLength;
    end;
  finally
    aRelMethod(BLOBBlock);
  end;
end;
{--------}
function FFTblGetFileNameBLOB(aFI       : PffFileInfo;
                              aTI       : PffTransInfo;
                              aBLOBNr   : TffInt64;
                          var aFileName : TffFullFileName )
                                        : Boolean;
var
  BLOBBlock     : PffBlock;
  BLOBBlockHdr  : PffBlockHeaderBLOB absolute BLOBBlock;
  BLOBBlockNum  : TffWord32;
  BLOBHeader    : PffBLOBHeader;
  FileNameLen   : Integer;
  OffsetInBlock : TffWord32;                                           {!!.11}
  aRelMethod    : TffReleaseMethod;
begin
  {read and verify the BLOB header block for this BLOB number}
  BLOBBlock := ReadVfyBlobBlock2(aFI,
                                 aTI,
                                 ffc_ReadOnly,
                                 aBLOBNr,
                                 BLOBBlockNum,
                                 OffsetInBlock,
                                 aRelMethod);
  BLOBHeader := @BLOBBlock^[OffsetInBlock];
{Begin !!.01}
  { Verify the BLOB has not been deleted. }
  if (BLOBHeader^.bbhSignature = ffc_SigBLOBSegDeleted) then
    FFRaiseException(EffServerException, ffStrResServer,
                     fferrBLOBDeleted,
                     [aFI^.fiName^, aBLOBNr.iHigh, aBLOBNr.iLow]);
{End !!.01}
  Result := BLOBHeader^.bbhSegCount = ffc_FileBLOB;
  if Result then begin
    {get the length of the file name}
    Move(BLOBBlock^[(OffsetInBlock + sizeof(TffBLOBHeader))],
         FileNameLen, 1);
    {move the file name to aFileName}
    Move(BLOBBlock^[(OffsetInBlock + sizeof(TffBLOBHeader))],
         aFileName, succ(FileNameLen));
  end;
  aRelMethod(BLOBBlock);
end;
{--------}
function FFTblIsBLOBLink(aFI             : PffFileInfo;                {!!.11 - Start}
                         aTI             : PffTransInfo;
                         aBLOBNr         : TffInt64;
                     var aSrcTableName   : TffTableName;
                     var aSrcTableBLOBNr : TffInt64)
                                         : Boolean;
var
  BLOBBlock     : PffBlock;
  BLOBHeader    : PffBLOBHeader;
  aHdRelMethod  : TffReleaseMethod;
  BLOBBLockNum  : TffWord32;
  OffsetInBlock : TffWord32;                                           {!!.11}
begin
  { Read and verify the BLOB header block for this BLOB number. }
  BLOBBlock := ReadVfyBlobBlock2(aFI,
                                 aTI,
                                 ffc_ReadOnly,
                                 aBLOBNr,
                                 BLOBBlockNum,
                                 OffsetInBlock,
                                 aHdRelMethod);
  try
    BLOBHeader := @BLOBBlock^[OffsetInBlock];

    Result := BLOBHeader^.bbhSegCount = ffc_BLOBLink;

    if (Result) then
      BLOBLinkGetTableNameAndRefNr(BLOBBlock,
                                   OffsetInBlock,
                                   aSrcTableName,
                                   aSrcTableBLOBNr);
  finally
    aHdRelMethod(BLOBBlock);
  end;
end;
{--------}                                                             {!!.11 - End}
{Begin !!.03}
{--------}
procedure WriteToStream(const aMsg : string; aStream : TStream);
begin
  aStream.Write(aMsg[1], Length(aMsg));
end;
{--------}
procedure FFTblListBLOBSegments(aFI     : PffFileInfo;
                                aTI     : PffTransInfo;
                                aBLOBNr : TffInt64;
                                aStream : TStream);
var
  BLOBBlock     : PffBlock;
  BLOBBlockHdr  : PffBlockHeaderBLOB absolute BLOBBlock;
  BLOBBlockNum  : TffWord32;
  BLOBHeader    : PffBLOBHeader;
  EntryCount    : Integer;
  LookupBlock, ContentBlock   : TffWord32;                             {!!.11}
  LookupEntry   : PffBLOBLookupEntry;
  ContentEntry : PffBLOBSegmentHeader;                                 {!!.11}
  LookupSegBlk, ContentSegBlk  : PffBlock;                             {!!.11}
  LookupSegPtr  : PffBLOBSegmentHeader;
  NextSeg       : TffInt64;
  OffsetInBlock, ContentOffsetInBlock : TffWord32;                     {!!.11}
  aLkpRelMethod,
  aContRelMethod,                                                      {!!.11}
  aHdRelMethod  : TffReleaseMethod;
begin
  LookupSegBlk := nil;

  { Read and verify the BLOB header block for this BLOB number. }
  BLOBBlock := ReadVfyBlobBlock2(aFI,
                                 aTI,
                                 ffc_ReadOnly,
                                 aBLOBNr,
                                 BLOBBlockNum,
                                 OffsetInBlock,
                                 aHdRelMethod);
  BLOBHeader := @BLOBBlock^[OffsetInBlock];

  { Verify the BLOB has not been deleted. }
  if (BLOBHeader^.bbhSignature = ffc_SigBLOBSegDeleted) then
    FFRaiseException(EffServerException, ffStrResServer,
                     fferrBLOBDeleted,
                     [aFI^.fiName^, aBLOBNr.iHigh, aBLOBNr.iLow]);

  { BLOB truncated to length zero? }
  if BLOBHeader^.bbh1stLookupSeg.iLow = ffc_W32NoValue then begin
    WriteToStream('BLOB has been truncated to length zero.', aStream);
    WriteToStream(#0, aStream);
    Exit;
  end;

  try
    { Are we dealing with a file BLOB or a BLOB link? }
    case BLOBHeader^.bbhSegCount of
      ffc_FileBLOB : { file BLOB }
        begin
           WriteToStream('This is a file BLOB.', aStream);
           Exit;
        end;
      ffc_BLOBLink : { BLOB link }
        begin
           WriteToStream('This is a BLOB link.', aStream);
          Exit;
        end;
    end;  { case }

    { Get the lookup segment block and set up offset for 1st lookup entry. }
    LookupSegBlk := ReadVfyBlobBlock2(aFI, aTI, ffc_ReadOnly,
                                      BLOBHeader^.bbh1stLookupSeg,
                                      LookupBlock, OffsetInBlock,
                                      aLkpRelMethod);
    LookupSegPtr := @LookupSegBlk^[OffsetInBlock];
    OffsetInBlock := OffsetInBlock + sizeof(TffBLOBSegmentHeader);

    { Walk through the BLOB segment linked list. }
    WriteToStream(Format('Segment list for BLOB %d:%d '+ #13#10,
                         [aBLOBNr.iHigh, aBLOBNr.iLow]), aStream);
    EntryCount := 0;
    while True do begin
      inc(EntryCount);
      LookupEntry := @LookupSegBlk^[OffsetInBlock];
{Begin !!.11}
      { Verify the segment is valid. }
      ContentSegBlk := ReadVfyBlobBlock2(aFI, aTI, ffc_ReadOnly,
                                         LookupEntry^.bleSegmentOffset,
                                         ContentBlock, ContentOffsetInBlock,
                                         aContRelMethod);

      ContentEntry := @ContentSegBlk^[ContentOffsetInBlock];
      if PffBlockHeaderBLOB(ContentSegBlk)^.bhbSignature <> ffc_SigBLOBBlock then
        raise Exception.CreateFmt
          ('Invalid BLOB block signature, block: %d', [ContentBlock])
      else if ContentEntry^.bshSignature <> ffc_SigBLOBSegContent then
        raise Exception.CreateFmt
          ('Invalid signature for content segment, offset: %d,%d, signature: %s',
           [LookupEntry^.bleSegmentOffset.iHigh,
            LookupEntry^.bleSegmentOffset.iLow,
            char(ContentEntry^.bshSignature)])
      else begin

        WriteToStream(Format('Segment %d, %d:%d, Len %d' + #13#10,
                             [EntryCount, LookupEntry^.bleSegmentOffset.iHigh,
                              LookupEntry^.bleSegmentOffset.iLow,
                              LookupEntry^.bleContentLength]), aStream);

        {see if we're at the end of the lookup segment}
        if (LookupSegPtr^.bshSegmentLen <
           (sizeof(TffBLOBSegmentHeader) +
           (succ(EntryCount) * sizeof(TffBLOBLookupEntry)))) then begin
          NextSeg := LookupSegPtr^.bshNextSegment;
          if NextSeg.iLow <> ffc_W32NoValue then begin
            aLkpRelMethod(LookupSegBlk);
            LookupSegBlk := ReadVfyBlobBlock2(aFI, aTI, ffc_ReadOnly,
                                              NextSeg,                   {!!.11}
                                              LookupBlock, OffsetInBlock,
                                              aLkpRelMethod);
            LookupSegPtr := @LookupSegBlk^[OffsetInBlock];
            OffsetInBlock := OffsetInBlock + sizeof(TffBLOBSegmentHeader);
            EntryCount := 0;
          end
          else
            break;
        end else
          OffsetInBlock := OffsetInBlock + sizeof(TffBLOBLookupEntry);
      end;
{End !!.11}
    end; {while}
  finally
    if assigned(LookupSegBlk) then
      aLkpRelMethod(LookupSegBlk);
    aHdRelMethod(BLOBBlock);
    WriteToStream(#0, aStream);
  end;
end;
{ End !!.03}
{Begin !!.11}
{--------}
function  FFTblRebuildLookupSegments(aFI            : PffFileInfo;
                                     aTI            : PffTransInfo;
                                     aNewBLOBSize   : TffWord32;
                                     aOldBLOBSize   : TffWord32;
                               const aBLOBNr        : TffInt64)
                                                    : TffInt64;
{This function takes an existing lookup segment chain & grows it to
 accomodate a larger BLOB. }
var
  NewBLOBBlock    : PffBlock;
  NewLookupHeader : PffBLOBSegmentHeader;
  OldBLOBBlock    : PffBlock;
  OldLookupHeader : PffBLOBSegmentHeader;
  OldLookupEntry  : PffBLOBLookupEntry;
  OldLookupBlk    : PffBlock;
  OldLookupOfs    : TffWord32;
  OldBLOBHeader   : PffBLOBHeader;
  NewSegCount     : TffWord32;
  OldSegCount     : Longint;
  SegBytesUsed    : TffWord32;
  EntriesToGo     : Longint;
  MaxEntries      : Longint;
  NewOfsInBlock   : TffWord32;
  OldOfsInBlock   : TffWord32;
  EntInOldSeg     : Longint;
  EntInNewSeg     : Longint;
  CurrentCount    : Longint;
  OldHeaderOfs    : TffInt64;
  TempI64         : TffInt64;
  aRelMethod      : TffReleaseMethod;
  aRelList        : TffPointerList;
  SegSize       : TffWord32;                                         
begin
  { We use the following list to track the RAM pages we've accessed and
    the release method associated with each RAM page. At the end of this
    routine, we will call the release method for each RAM page. }
  aRelList := TffPointerList.Create;

  try
    { Get the old lookup header before we replace it with a new one. }
    OldBLOBBlock := ReadVfyBlobBlock(aFI, aTI, ffc_MarkDirty,
                                     aBLOBNr, OldOfsInBlock, aRelMethod);
    aRelList.Append(FFAllocReleaseInfo(OldBLOBBlock, TffInt64(aRelMethod)));
    OldBLOBHeader := PffBLOBHeader(@OldBLOBBlock^[OldOfsInBlock]);
    OldHeaderOfs := OldBLOBHeader^.bbh1stLookupSeg;

    { Determine number of segments needed to hold the entire BLOB. }
    NewSegCount := CalcBLOBSegNumber(aNewBLOBSize, aFI^.fiBlockSize, SegBytesUsed);

    { Can the number of lookup entries required for the number of segments
      fit within one lookup segment? }
    if ((NewSegCount * ffc_BLOBLookupEntrySize) <=
        (aFI^.fiMaxSegSize - ffc_BLOBSegmentHeaderSize)) then begin
      { Yes.  Create a new lookup segment. }
      SegSize := (NewSegCount * ffc_BLOBLookupEntrySize) +
                 ffc_BLOBSegmentHeaderSize;
      Result := aFI^.fiBLOBrscMgr.NewSegment(aFI, aTI, SegSize, SegSize);
      NewBLOBBlock := ReadVfyBlobBlock(aFI, aTI, ffc_MarkDirty, Result,
                                       NewOfsInBlock, aRelMethod);
      aRelList.Append(FFAllocReleaseInfo(NewBLOBBlock, TffInt64(aRelMethod)));
      NewLookupHeader := @NewBLOBBlock^[NewOfsInBlock];

      { Setup our new lookup header. }
      with NewLookupHeader^ do begin
        bshSignature := ffc_SigBLOBSegLookup;
        bshParentBLOB := aBLOBNr;
        bshNextSegment.iLow := ffc_W32NoValue;
      end;
    end else begin
      { No.  We need a chain of lookup segments. }
      EntriesToGo := NewSegCount;
      MaxEntries  := (aFI^.fiMaxSegSize - ffc_BLOBSegmentHeaderSize) div
                     ffc_BLOBLookupEntrySize;
      SegSize := (MaxEntries * ffc_BLOBLookupEntrySize) +
                 ffc_BLOBSegmentHeaderSize;
      Result := aFI^.fiBLOBrscMgr.NewSegment(aFI, aTI, SegSize, SegSize);
      dec(EntriesToGo, MaxEntries);
      NewBLOBBlock := ReadVfyBlobBlock(aFI, aTI, ffc_MarkDirty,
                                       Result, NewOfsInBlock, aRelMethod);
      aRelList.Append(FFAllocReleaseInfo(NewBLOBBlock, TffInt64(aRelMethod)));
      NewLookupHeader := @NewBLOBBlock^[NewOfsInBlock];
      NewLookupHeader^.bshSignature := ffc_SigBLOBSegHeader;
      NewLookupHeader^.bshParentBLOB := aBLOBNr;
      while EntriesToGo > 0 do begin
        if EntriesToGo > MaxEntries then begin
          { We need this lookup segment & at least one more. }
          SegSize := (MaxEntries * ffc_BLOBLookupEntrySize) +
                     ffc_BLOBSegmentHeaderSize;
          NewLookupHeader^.bshNextSegment := aFI^.fiBLOBrscMgr.NewSegment
                                               (aFI, aTI, SegSize, SegSize);
          dec(EntriesToGo, MaxEntries);
          NewBLOBBlock := ReadVfyBlobBlock(aFI, aTI, ffc_MarkDirty,
                                           NewLookupHeader^.bshNextSegment,
                                           NewOfsInBlock, aRelMethod);
          aRelList.Append(FFAllocReleaseInfo(NewBLOBBlock, TffInt64(aRelMethod)));
        end else begin
          { This is the last lookup segment needed. }
          SegSize := (EntriesToGo * ffc_BLOBLookupEntrySize) +
                     ffc_BLOBSegmentHeaderSize;
          NewLookupHeader^.bshNextSegment := aFI^.fiBLOBrscMgr.NewSegment
                                               (aFI, aTI, SegSize, SegSize);
          dec(EntriesToGo, EntriesToGo);
          NewBLOBBlock := ReadVfyBlobBlock(aFI, aTI, ffc_MarkDirty,
                                           NewLookupHeader^.bshNextSegment,
                                           NewOfsInBlock, aRelMethod);
          aRelList.Append(FFAllocReleaseInfo(NewBLOBBlock, TffInt64(aRelMethod)));
        end; {if..else}

        { Initialize the segment. }
        NewLookupHeader := @NewBLOBBlock^[NewOfsInBlock];
        NewLookupHeader^.bshSignature := ffc_SigBLOBSegHeader;
        NewLookupHeader^.bshParentBLOB := aBLOBNr;
        NewLookupHeader^.bshNextSegment.iLow := ffc_W32NoValue;

      end; {while}
      {Reset the new lookup segment to the 1st one in the chain.}
      NewBLOBBlock := ReadVfyBlobBlock(aFI, aTI, ffc_MarkDirty,
                                       Result,
                                       NewOfsInBlock, aRelMethod);
      NewLookupHeader := @NewBLOBBlock^[NewOfsInBlock];

    end; {if..else}

    { Now that we have our newly-sized lookup header(s) and entries, we
      need to copy the old entries into the new header. }
    if aOldBLOBSize = 0 then
      OldSegCount := 0
    else
      OldSegCount := CalcBLOBSegNumber(aOldBLOBSize, aFI^.fiBlockSize,
                                       SegBytesUsed);

    if OldSegCount <> 0 then begin
      OldLookupBlk := ReadVfyBlobBlock(aFI, aTI, ffc_MarkDirty,
                                       OldBLOBHeader^.bbh1stLookupSeg,
                                       OldLookupOfs, aRelMethod);
      aRelList.Append(FFAllocReleaseInfo(OldLookupBlk, TffInt64(aRelMethod)));
      OldLookupHeader := @OldLookupBlk^[OldLookupOfs];
      OldLookupOfs := OldLookupOfs + sizeof(TffBLOBSegmentHeader);
      { Point to the 1st lookup entry. }
      OldLookupEntry := PffBLOBLookupEntry(@OldLookupBlk^[OldLookupOfs]);

      { Get the block offset to where the first new lookup entry goes. }
      NewBLOBBlock := ReadBLOBBlock(aFI, aTI, Result, NewOfsInBlock,
                                    aRelMethod);
      aRelList.Append(FFAllocReleaseInfo(NewBLOBBlock, TffInt64(aRelMethod)));
      NewOfsInBlock := NewOfsInBlock + sizeof(TffBLOBSegmentHeader);

      { Is the old lookup segment followed by another lookup segment? }
      if OldLookupHeader^.bshNextSegment.iLow <> ffc_W32NoValue then
        { Yes.  It must have the maximum number of lookup entries so figure out
          how many that is. }
        EntInOldSeg := FFCalcMaxLookupEntries(OldLookupHeader)
      else
        { No.  The number of lookup entries equals the number of segments in
          the BLOB. }
        EntInOldSeg := OldSegCount;

      { Figure out the maximum number of entries for the new lookup segment. }
      EntInNewSeg := FFCalcMaxLookupEntries(NewLookupHeader);

      CurrentCount := 0;
      while CurrentCount < OldSegCount do begin
        { Move over all lookup entries from the old lookup segment to the new
          lookup segment. }
        Move(OldLookupEntry^, NewBLOBBlock^[NewOfsInBlock],
             EntInOldSeg * sizeof(TffBLOBLookupEntry));
        inc(CurrentCount, EntInOldSeg);
        dec(EntInNewSeg, EntInOldSeg);

        { Save a pointer to the beginning of our old lookup segment.
          We will need it to delete the lookup segment later. }
        TempI64 := OldHeaderOfs;

        { Is there a lookup segment after this one? }
        if OldLookupHeader^.bshNextSegment.iLow <> ffc_W32NoValue then begin
          { Yes.  Move to it. }
          OldHeaderOfs := OldLookupHeader^.bshNextSegment;
          OldBLOBBlock := ReadVfyBlobBlock(aFI, aTI, ffc_MarkDirty,
                                           OldHeaderOfs, OldLookupOfs,
                                           aRelMethod);
          aRelList.Append(FFAllocReleaseInfo(OldBLOBBlock, TffInt64(aRelMethod)));
          OldLookupBlk :=
                 ReadVfyBlobBlock(aFI, aTI, ffc_MarkDirty,
                                  OldLookupHeader^.bshNextSegment,
                                  OldLookupOfs, aRelMethod);
          aRelList.Append(FFAllocReleaseInfo(OldLookupBlk,
                                             TffInt64(aRelMethod)));
          OldLookupHeader := @OldLookupBlk^[OldLookupOfs];
          inc(OldLookupOfs, sizeof(TffBLOBSegmentHeader));
          OldLookupEntry := PffBLOBLookupEntry(@OldBLOBBlock^[OldLookupOfs]);

          { Since the lookup segment was followed by another lookup segment,
            we know this is a max-size segment full of entries. }
          EntInOldSeg := FFCalcMaxLookupEntries(OldLookupHeader);
        end;

        { Delete the old lookup segment now that we have copied all its
          entries. }
        aFI^.fiBLOBrscMgr.DeleteSegment(aFI, aTI, TempI64);

        { Check if we've filled up our current (target) header}
        if (EntInNewSeg = 0) and
           (NewLookupHeader^.bshNextSegment.iLow <> ffc_W32NoValue) then begin
          NewBLOBBlock := ReadBlobBlock(aFI,
                                        aTI,
                                        NewLookupHeader^.bshNextSegment,
                                        NewOfsInBlock,
                                        aRelMethod);

          aRelList.Append(FFAllocReleaseInfo(NewBLOBBlock, TffInt64(aRelMethod)));
          NewLookupHeader := @NewBLOBBlock^[NewOfsInBlock];
          NewOfsInBlock :=
            NewOfsInBlock + sizeof(TffBLOBSegmentHeader);           
          EntInNewSeg := FFCalcMaxLookupEntries(NewLookupHeader);
        end;
      end; {while}
    end; {if}
    OldBLOBHeader^.bbh1stLookupSeg := Result;
  finally
    for CurrentCount := 0 to pred(aRelList.Count) do begin
      FFDeallocReleaseInfo(aRelList[CurrentCount]);
    end;
    aRelList.Free;
  end;
end;
{====================================================================}

{===TffBaseBLOBEngine================================================}
class function TffBaseBLOBEngine.GetEngine(aFI : PffFileInfo) : TffBaseBLOBEngine;
begin
  if aFI.fiFFVersion <= ffVersion2_10 then
    Result := FF210BLOBEngine
  else
    Result := FFBLOBEngine;
end;
{====================================================================}

{===TffBLOBEngine====================================================}
procedure TffBLOBEngine.Read(aFI         : PffFileInfo;
                                aTI         : PffTransInfo;
                                aBLOBNr     : TffInt64;
                                aOffset     : TffWord32;
                                aLen        : TffWord32;
                                aReadMethod : TffBLOBLinkRead;
                            var aBLOB;
                            var aBytesRead  : TffWord32;
                            var aFBError    : TffResult);
var
  aCntRelMethod,
  aLkpRelMethod,
  aHdRelMethod     : TffReleaseMethod;
  BLOBAsBytes      : PffBLOBArray;
  BLOBBlock        : PffBlock;
  BLOBBlockHdr     : PffBlockHeaderBLOB absolute BLOBBlock;
  BLOBBlockNum     : TffWord32;
  BLOBHeader       : PffBLOBHeader;
  BytesToCopy      : TffWord32;
  ContentBlock,
  LookupSegOfs     : TffWord32;
  ContentSegBlk    : PffBlock;
  ContentSegOfs    : TffInt64;
  DestOffset       : TffWord32;
  MaxLookupEntries : Integer;
  LookupBlock      : TffWord32;
  LookupEntry      : PffBLOBLookupEntry;
  LookupSegBlk     : PffBlock;
  LookupSegPtr     : PffBLOBSegmentHeader;
  OffsetInBlock    : TffWord32;
  StartBytesUsed,
  BLOBPos          : TffWord32;
  CurrLookupEntry  : Integer;
{$IFDEF BLOBTrace}
  LookupSegCount   : Integer;
{$ENDIF}
  NextSeg          : TffInt64;                                         {!!.11}
begin
{$IFDEF BLOBTrace}
  Logbt('FFTblReadBLOB.Begin', []);
  Logbt('  aBLOBNr = %d:%d', [aBLOBNr.iLow, aBLOBNr.iHigh]);
  Logbt('  aOffset = %d', [aOffset]);
  Logbt('  aLen    = %d', [aLen]);
  try
{$ENDIF}

  BLOBAsBytes := @aBLOB;
  ContentSegBlk := nil;
  LookupSegBlk := nil;
  DestOffset := 0;

  aFBError := 0;

  {Exit if aLen = 0}
  if aLen = 0 then
    Exit;

  { Read and verify the BLOB header block for this BLOB number. }
  BLOBBlock := ReadVfyBlobBlock2(aFI,
                                 aTI,
                                 ffc_ReadOnly,
                                 aBLOBNr,
                                 BLOBBlockNum,
                                 OffsetInBlock,
                                 aHdRelMethod);
  BLOBHeader := @BLOBBlock^[OffsetInBlock];
  {$IFDEF BLOBTrace}
    Logbt('  BLOB.Length: %d, 1st lookup segment: %d:%d',
          [BLOBHeader^.bbhBLOBLength,
           BLOBHeader^.bbh1stLookupSeg.iLow,
           BLOBHeader^.bbh1stLookupSeg.iHigh]);
  {$ENDIF}

  { Verify the BLOB has not been deleted. }
  if (BLOBHeader^.bbhSignature = ffc_SigBLOBSegDeleted) then
    FFRaiseException(EffServerException,
                     ffStrResServer,
                     fferrBLOBDeleted,
                     [aFI^.fiName^,
                      aBLOBNr.iHigh,
                      aBLOBNr.iLow]);

  try
    { Are we dealing with a file BLOB or a BLOB link? }
    case BLOBHeader^.bbhSegCount of
      ffc_FileBLOB : { file BLOB }
        begin
           aFBError := FileBLOBRead(aFI,
                                    aTI,
                                    aBLOBNr,
                                    aOffset,
                                    aLen,
                                    aBLOB,
                                    aBytesRead);
           Exit;
        end;
      ffc_BLOBLink : { BLOB link }
        begin
          aFBError := BLOBLinkRead(aFI,
                                   aTI,
                                   aBLOBNr,
                                   aOffset,
                                   aLen,
                                   aReadMethod,
                                   aBLOB,
                                   aBytesRead);
          Exit;
        end;
    end;  { case }

    { Make sure that the offset is within BLOB. }
    if (FFCmpDW(aOffset, BLOBHeader^.bbhBLOBLength) >= 0) then begin
      aBytesRead := 0;
      Exit;
    end;
    { Get the lookup segment block and set up offset for 1st lookup entry. }
    LookupSegBlk := ReadVfyBlobBlock2(aFI,
                                      aTI,
                                      ffc_ReadOnly,
                                      BLOBHeader^.bbh1stLookupSeg,
                                      LookupBlock,
                                      LookupSegOfs,
                                      aLkpRelMethod);
    LookupSegPtr := @LookupSegBlk^[LookupSegOfs];
    LookupSegOfs := LookupSegOfs + ffc_BLOBSegmentHeaderSize;

    { Calculate the number of bytes we can (= "are going to") read. }
    aBytesRead := ffMinDW(aLen, BLOBHeader^.bbhBLOBLength - aOffset);

    { How many entries are in the current lookup segment? }
    MaxLookupEntries := FFCalcMaxLookupEntries(LookupSegPtr);
    CurrLookupEntry := 1;
    {$IFDEF BLOBTrace}
      LookupSegCount := 1;
      Logbt('  Lookup segment - Max entries: %d',
            [MaxLookupEntries]);
    {$ENDIF}

    { Position to where we are to start reading. }
    BLOBPos := 0;
    StartBytesUsed := 0;
    while (BLOBPos < aOffset) do begin
      LookupEntry := @LookupSegBlk^[LookupSegOfs];
      {$IFDEF BLOBTrace}
        Logbt('  Lookup entry %d points to ' +
              'segment %d:%d with %d bytes',
              [CurrLookupEntry,
               LookupEntry^.bleSegmentOffset.iHigh,
               LookupEntry^.bleSegmentOffset.iLow,
               LookupEntry^.bleContentLength]);
      {$ENDIF}
      { Does this entry point to the segment where we should start
        copying data? }
      if ((BLOBPos + LookupEntry^.bleContentLength) >= aOffset) then begin
        { Yes. We found the starting point. }
        ContentSegOfs := LookupEntry^.bleSegmentOffset;
        StartBytesUsed := aOffset - BLOBPos;
        { NOTE: We will start reading from this segment, so we don't
                want to move past it. }
        Break;
      end else begin
        { Nope. Update and keep moving. }
        BLOBPos := BLOBPos + LookupEntry^.bleContentLength;
        LookupSegOfs := LookupSegOfs + ffc_BLOBLookupEntrySize;
        CurrLookupEntry := CurrLookupEntry + 1;
      end;

      { Have we reached the end of this lookup segment? }
      if (CurrLookupEntry > MaxLookupEntries) then begin
        { Get the lookup segment block and set up offset for 1st lookup entry. }
        NextSeg := LookupSegPtr^.bshNextSegment;                       {!!.11}
        aLkpRelMethod(LookupSegBlk);
        LookupSegBlk := ReadVfyBlobBlock2(aFI,
                                          aTI,
                                          ffc_ReadOnly,
                                          NextSeg,                     {!!.11}
                                          LookupBlock,
                                          LookupSegOfs,
                                          aLkpRelMethod);
        LookupSegPtr := @LookupSegBlk^[LookupSegOfs];
        LookupSegOfs := LookupSegOfs + ffc_BLOBSegmentHeaderSize;

        { How many entries are in the current lookup segment? }
        MaxLookupEntries := FFCalcMaxLookupEntries(LookupSegPtr);
        CurrLookupEntry := 1;
        {$IFDEF BLOBTrace}
          LookupSegCount := LookupSegCount + 1;
          Logbt('  Moving to lookup segment %d',
                [LookupSegCount]);
          Logbt('  Lookup segment - Max entries: %d',
                [MaxLookupEntries]);
        {$ENDIF}
      end;
    end;

    { Read what we need. }
    BLOBPos := 0;
    while (BLOBPos < aBytesRead) do begin
      { Read the BLOB content segment. }
      if (ContentSegBlk <> nil) then
        aCntRelMethod(ContentSegBlk);
      LookupEntry := @LookupSegBlk^[LookupSegOfs];
      {$IFDEF BLOBTrace}
        Logbt('  Lookup entry %d points to segment %d:%d with %d bytes',
              [CurrLookupEntry,
               LookupEntry^.bleSegmentOffset.iHigh,
               LookupEntry^.bleSegmentOffset.iLow,
               LookupEntry^.bleContentLength]);
      {$ENDIF}
      ContentSegBlk := ReadVfyBlobBlock2(aFI,
                                         aTI,
                                         ffc_ReadOnly,
                                         LookupEntry^.bleSegmentOffset,
                                         ContentBlock,
                                         OffsetInBlock,
                                         aCntRelMethod);
      OffsetInBlock := OffsetInBlock + ffc_BLOBSegmentHeaderSize;

      if (StartBytesUsed > 0) then begin
        { This is the first segment we're reading from. This will
          normally be in the middle of a segment. }
        BytesToCopy := LookupEntry^.bleContentLength - StartBytesUsed;
        OffsetInBlock := OffsetInBlock + StartBytesUsed;
      end else begin
        { copying from middle segments }
        BytesToCopy := LookupEntry^.bleContentLength;
      end;

      BytesToCopy := ffMinL(BytesToCopy, (aBytesRead - BLOBPos));
      Move(ContentSegBlk^[OffsetInBlock],
           BLOBAsBytes^[DestOffset],
           BytesToCopy);
      BLOBPos := BLOBPos + BytesToCopy;
      DestOffset := DestOffset + BytesToCopy;
      {$IFDEF BLOBTrace}
        Logbt('  Read %d bytes from lookup segment %d, entry %d',
              [BytesToCopy, LookupSegCount, CurrLookupEntry]);
      {$ENDIF}
      CurrLookupEntry := CurrLookupEntry + 1;
      StartBytesUsed := 0;

      { Have we reached the end of this lookup segment? }
      if ((BLOBPos < aBytesRead) and
          (CurrLookupEntry > MaxLookupEntries)) then begin
        NextSeg := LookupSegPtr^.bshNextSegment;                       {!!.11}
        aLkpRelMethod(LookupSegBlk);
        { Get the lookup segment block and set up offset for 1st
          lookup entry. }
        LookupSegBlk := ReadVfyBlobBlock2(aFI,
                                          aTI,
                                          ffc_ReadOnly,
                                          NextSeg,                     {!!.11}
                                          LookupBlock,
                                          LookupSegOfs,
                                          aLkpRelMethod);
        LookupSegPtr := @LookupSegBlk^[LookupSegOfs];
        LookupSegOfs := LookupSegOfs + ffc_BLOBSegmentHeaderSize;

        { How many entries are in the current lookup segment? }
        MaxLookupEntries := FFCalcMaxLookupEntries(LookupSegPtr);
        CurrLookupEntry := 1;
        {$IFDEF BLOBTrace}
          LookupSegCount := LookupSegCount + 1;
          Logbt('  Moving to lookup segment %d',
                [LookupSegCount]);
          Logbt('  Lookup segment - Max entries: %d',
                [MaxLookupEntries]);
        {$ENDIF}
      end else begin
        LookupSegOfs := LookupSegOfs + ffc_BLOBLookupEntrySize;
      end;
    end; {while}
  finally
    if assigned(ContentSegBlk) then
      aCntRelMethod(ContentSegBlk);
    if assigned(LookupSegBlk) then
      aLkpRelMethod(LookupSegBlk);
    aHdRelMethod(BLOBBlock);
  end;
{$IFDEF BLOBTrace}
  except
    Logbt('*** FFTblReadBLOB Exception ***', []);
    raise;
  end
{$ENDIF}
end;
{--------}
function TffBLOBEngine.IsEmptyLookupEntry(Entry : PffBLOBLookupEntry) : Boolean;
{ Revised !!.13}
const
  ciEmptyVal1 = 808464432;
    { This is because lookup segments prior to 2.13 were fillchar'd with 'O'
      instead of 0. We have to check all 3 fields in the lookup entry for this
      value so that we avoid a case where the value is valid. }
  ciEmptyVal2 = 1179010630;
    { Another value that indicates an empty lookup entry. }
begin
  Result := (Entry^.bleSegmentOffset.iLow = ffc_W32NoValue) or
            ((Entry^.bleSegmentOffset.iLow = 0) and
             (Entry^.bleSegmentOffset.iHigh = 0)) or
            ((Entry^.bleSegmentOffset.iLow = ciEmptyVal1) and
             (Entry^.bleSegmentOffset.iHigh = ciEmptyVal1) and
             (Entry^.bleContentLength = ciEmptyVal1)) or
            ((Entry^.bleSegmentOffset.iLow = ciEmptyVal2) and
             (Entry^.bleSegmentOffset.iHigh = ciEmptyVal2) and
             (Entry^.bleContentLength = ciEmptyVal2));
end;
{--------}
procedure TffBLOBEngine.Truncate(aFI     : PffFileInfo;
                                 aTI     : PffTransInfo;
                                 aBLOBNr : TffInt64;
                                 aLen    : TffWord32);
{Updated !!.12}
var
  aRelList         : TffPointerList;
//  aLkpRelMethod,                                                     {Deleted !!.13}
  aRelMethod       : TffReleaseMethod;
  NextLookupSeg    : TffInt64;
  ContOffset,                                                          {!!.13}
  BLOBPos,
  CurrLookupEntry,
  LookupBlock,
  MaxLookupEntries,
  OffsetInBlock,
  StartBytesUsed   : TffWord32;
  NewSegCount      : Integer;
  BLOBBlock        : PffBlock;
  ContentSegBlk,                                                       {!!.13}
  LookupSegBlk     : PffBlock;
  BLOBBlockHdr     : PffBlockHeaderBLOB absolute BLOBBlock;
  BLOBHeader       : PffBLOBHeader;
{Begin !!.13}
  ContentSegOfs     : TffInt64;
  ContentSegPtr,
{End !!.13}
  LookupSegPtr     : PffBLOBSegmentHeader;
  LookupEntry      : PffBLOBLookupEntry;
{$IFDEF BLOBTrace}
  LookupSegCount   : Integer;
{$ENDIF}
begin
{$IFDEF BLOBTrace}
  Logbt('Entering FFTblTruncateBLOB', []);
  Logbt('  aBLOBNr = %d:%d', [aBLOBNr.iLow, aBLOBNr.iHigh]);
  Logbt('  aLen    = %d', [aLen]);
  LookupSegCount := 1;
{$ENDIF}

//  aLkpRelMethod := nil;                                              {Deleted !!.13}

  { We use the following list to track the RAM pages we've accessed and
    the release method associated with each RAM page. At the end of this
    routine, we will call the release method for each RAM page. }
  aRelList := TffPointerList.Create;

  try
    { Read and verify the BLOB header block for this BLOB number. }
    BLOBBlock := ReadVfyBlobBlock(aFI,
                                  aTI,
                                  ffc_MarkDirty,
                                  aBLOBNr,
                                  OffsetInBlock,
                                  aRelMethod);
    aRelList.Append(FFAllocReleaseInfo(BLOBBlock,TffInt64(aRelMethod)));

    BLOBHeader := @BLOBBlock^[OffsetInBlock];

    { Check if we're trying to truncate a zero-length BLOB or to the
      BLOB's current length. }
    if (BLOBHeader^.bbhBLOBLength = aLen) then
      Exit;

    { Verify the BLOB has not been deleted. }
    if (BLOBHeader^.bbhSignature = ffc_SigBLOBSegDeleted) then
      FFRaiseException(EffServerException, ffStrResServer,
                       fferrBLOBDeleted,
                       [aFI^.fiName^, aBLOBNr.iHigh, aBLOBNr.iLow]);

    { Verify this is a header segment. }
    if (BLOBHeader^.bbhSignature <> ffc_SigBLOBSegHeader) then
      FFRaiseException(EffServerException,
                       ffStrResServer,
                       fferrBadBLOBSeg,
                       [aFI^.fiName^,
                        aBLOBNr.iLow,
                        aBLOBNr.iHigh,
                        Format(ffcBLOBSegExpected,
                               [ffcBLOBSegHeader,
                                Char(BLOBHeader^.bbhSignature)])]);

    { We can't write to a file BLOB. }
    if (BLOBHeader^.bbhSegCount = -1) then
      FFRaiseException(EffServerException,
                       ffStrResServer,
                       fferrFileBLOBWrite,
                       [aFI^.fiName^,
                        aBLOBNr.iLow,
                        aBLOBNr.iHigh]);

    { Make sure the truncated length <= current BLOB length. }
    if (aLen > BLOBHeader^.bbhBLOBLength) then
      FFRaiseException(EffServerException,
                       ffStrResServer,
                       fferrLenMismatch,
                       [aFI^.fiName^,
                        aBLOBNr.iLow,
                        aBLOBNr.iHigh,
                        aLen,
                        BLOBHeader^.bbhBLOBLength]);

    { If the new length is greater than 0, we will lop off some
      content segments. The content segment that becomes the last
      content segment must be updated. }
    NewSegCount := 0;
    if (aLen > 0) then begin
      { Grab the first lookup segment. }
      NextLookupSeg := BLOBHeader^.bbh1stLookupSeg;
      LookupSegBlk := ReadVfyBlobBlock2(aFI,
                                        aTI,
                                        ffc_MarkDirty,                 {!!.13}
                                        NextLookupSeg,
                                        LookupBlock,
                                        OffsetInBlock,
                                        aRelMethod);
      aRelList.Append(FFAllocReleaseInfo(LookupSegBlk,TffInt64(aRelMethod)));
      LookupSegPtr := PffBLOBSegmentHeader(@LookupSegBlk^[OffsetInBlock]);
      MaxLookupEntries := FFCalcMaxLookupEntries(LookupSegPtr);

      OffsetInBlock := OffsetInBlock +
                       ffc_BLOBSegmentHeaderSize;
      CurrLookupEntry := 1;

      { Position to where we are to start truncating. }
      BLOBPos := 0;
      StartBytesUsed := 0;
      while (BLOBPos < aLen) do begin
        NewSegCount := NewSegCount + 1;
        LookupEntry := @LookupSegBlk^[OffsetInBlock];
        {$IFDEF BLOBTrace}
        Logbt('  Lookup entry %d points to a segment with %d bytes',
              [CurrLookupEntry,
               LookupEntry^.bleContentLength]);
        {$ENDIF}

        if ((BLOBPos + LookupEntry^.bleContentLength) >= aLen) then begin {!!.13}
          { We found the starting point. }
          StartBytesUsed := aLen - BLOBPos;
          Break;
        end else begin
          BLOBPos := BLOBPos + LookupEntry^.bleContentLength;
          CurrLookupEntry := CurrLookupEntry + 1;
        end;

        { Have we reached the end of this lookup segment? }
        if ((BLOBPos < aLen) and
            (CurrLookupEntry > MaxLookupEntries)) then begin
          { Get the lookup segment block and set up offset for 1st
            lookup entry. }
          NextLookupSeg := LookupSegPtr^.bshNextSegment;
//          if Assigned(aLkpRelMethod) then                            {Deleted !!.13}
//            aLkpRelMethod(LookupSegBlk);                             {Deleted !!.13}
          LookupSegBlk := ReadVfyBlobBlock2(aFI,
                                            aTI,
                                            ffc_MarkDirty,             {!!.13}
                                            NextLookupSeg,
                                            LookupBlock,
                                            OffsetInBlock,
                                            aRelMethod);               {!!.13}
          aRelList.Append(FFAllocReleaseInfo(LookupSegBlk,TffInt64(aRelMethod))); {!!.13}
          LookupSegPtr := @LookupSegBlk^[OffsetInBlock];
          OffsetInBlock := OffsetInBlock + ffc_BLOBSegmentHeaderSize;

          { How many entries are in the current lookup segment? }
          MaxLookupEntries := FFCalcMaxLookupEntries(LookupSegPtr);
          CurrLookupEntry := 1;
          {$IFDEF BLOBTrace}
            LookupSegCount := LookupSegCount + 1;
            Logbt('  Moving to lookup segment %d',
                  [LookupSegCount]);
            Logbt('  Lookup segment - Max entries: %d',
                  [MaxLookupEntries]);
          {$ENDIF}
        end
        else
          OffsetInBlock := OffsetInBlock + ffc_BLOBLookupEntrySize;
      end;  { while }

      { We should now be positioned on the last lookup entry to be retained
        by the truncation. Update the length of its content segment. }
      LookupEntry := @LookupSegBlk^[OffsetInBlock];
      BLOBPos := BLOBPos + LookupEntry^.bleContentLength;
      LookupEntry^.bleContentLength := StartBytesUsed;

{Begin !!.13}
      { Update the content segment's NextSegment pointer. }
      ContentSegOfs := LookupEntry^.bleSegmentOffset;
      ContentSegBlk := ReadVfyBlobBlock(aFI,
                                        aTI,
                                        ffc_MarkDirty,
                                        ContentSegOfs,
                                        ContOffset,
                                        aRelMethod);
      aRelList.Append(FFAllocReleaseInfo(ContentSegBlk,TffInt64(aRelMethod)));  {!!.13}
      ContentSegPtr := @ContentSegBlk^[ContOffset];
      ContentSegPtr^.bshNextSegment.iLow := ffc_W32NoValue;
{End !!.13}

      { Delete the content & lookup segments that are no longer needed.
        First, obtain the number of extraneous lookup entries in the
        current lookup segment. }
      while (BLOBPos < BLOBHeader^.bbhBLOBLength) do begin
        CurrLookupEntry := CurrLookupEntry + 1;
        OffsetInBlock := OffsetInBlock + ffc_BLOBLookupEntrySize;
        LookupEntry := @LookupSegBlk^[OffsetInBlock];
        { Have we reached the end of this lookup segment? }
        if (CurrLookupEntry > MaxLookupEntries) then begin
            if LookupSegPtr^.bshNextSegment.iLow = ffc_W32NoValue then
              Break
            else begin
            { Get the lookup segment block and set up offset for 1st
              lookup entry. }
            NextLookupSeg := LookupSegPtr^.bshNextSegment;
//            if Assigned(aLkpRelMethod) then                          {Deleted !!.13}
//              aLkpRelMethod(LookupSegBlk);                           {Deleted !!.13}
            LookupSegBlk := ReadVfyBlobBlock2(aFI,
                                              aTI,
                                              ffc_MarkDirty,
                                              NextLookupSeg,
                                              LookupBlock,
                                              OffsetInBlock,
                                              aRelMethod);             {!!.13}
            aRelList.Append(FFAllocReleaseInfo(LookupSegBlk,TffInt64(aRelMethod))); {!!.13}
            LookupSegPtr^.bshNextSegment.iLow := ffc_W32NoValue;
            LookupSegPtr := @LookupSegBlk^[OffsetInBlock];
            { Move ahead to first lookup entry. }
            OffsetInBlock := OffSetInBlock + ffc_BLOBSegmentHeaderSize;
            LookupEntry := @LookupSegBlk^[OffsetInBlock];

            { How many entries are in the current lookup segment? }
            MaxLookupEntries := FFCalcMaxLookupEntries(LookupSegPtr);
            CurrLookupEntry := 1;

            {$IFDEF BLOBTrace}
              LookupSegCount := LookupSegCount + 1;
              Logbt('  Moving to lookup segment %d',
                    [LookupSegCount]);
              Logbt('  Lookup segment - Max entries: %d',
                    [MaxLookupEntries]);
            {$ENDIF}
          end
        end
        else if IsEmptyLookupEntry(LookupEntry) then
          { Have we encountered an empty lookup segment? If so then this
            indicates the end of the BLOB content. }
          Break;

        if (StartBytesUsed = 0) then
          BLOBPos := BLOBPos + LookupEntry^.bleContentLength
        else
          StartBytesUsed := 0;

        aFI^.fiBLOBrscMgr.DeleteSegment(aFI,
                                        aTI,
                                        LookupEntry^.bleSegmentOffset);
        FillChar(LookupEntry^, ffc_BLOBLookupEntrySize, 0);            {!!.13}

      end;  { while }
      LookupSegPtr^.bshNextSegment.iLow := ffc_W32NoValue;
    end else begin
      { We are truncating to length of 0. }
      FFTblDeleteBLOBPrim(aFI, aTI, BLOBHeader);

      { Reset the lookup segment field and the segment count.
        FFTblFreeBLOB will get rid of the BLOB header if the BLOB is
        still at length 0. }
      BLOBHeader^.bbh1stLookupSeg.iLow := ffc_W32NoValue;
    end;
    { Set the new BLOB length and segment count in the BLOB header. }
    BLOBHeader^.bbhBLOBLength := aLen;

    { Set the new segment count in the BLOB header. }
    BLOBHeader^.bbhSegCount := NewSegCount;
  finally
    for OffsetInBlock := 0 to (aRelList.Count - 1) do
      FFDeallocReleaseInfo(aRelList[OffsetInBlock]);
    aRelList.Free;
  end;
end;
{--------}
procedure TffBLOBEngine.Write(aFI     : PffFileInfo;
                              aTI     : PffTransInfo;
                        const aBLOBNr : TffInt64;
                              aOffset : TffWord32;   {offset in blob to start writing}
                              aLen    : TffWord32;   {bytes from aOffset to stop writing}
                        const aBLOB);
var
  aLkpRelMethod,
  aRelMethod        : TffReleaseMethod;
  aRelList          : TffPointerList;
  ContentSegOfs     : TffInt64;
  BLOBPos,
  BytesCopied,
  BytesToCopy,
  BytesToGo,
  CurrLookupEntry,
  LookupBlock,
  LookupEntOfs,
  LookupSegOfs,
  MaxLookupEntries,
  NewSize,
  OffsetInBlock,
  SegBytesLeft,
  SegSize,
  StartBytesUsed,
  TargetOffset,
  TempWord          : TffWord32;
  MinSegSize        : Integer;
  BLOBBlock,
  ContentSegBlk,
  LookupSegBlk,
  PrevContSegBlk    : PffBlock;
  BLOBBlockHdr      : PffBlockHeaderBLOB absolute BLOBBlock;
  BLOBHeader        : PffBLOBHeader;
  BLOBAsBytes       : PffBLOBArray;
  LookupEntry       : PffBLOBLookupEntry;
  ContentSegPtr,
  LookupSegPtr,
  PrevContentSegPtr,
  TempSegPtr        : PffBLOBSegmentHeader;
  NewSegment        : Boolean;
{$IFDEF BLOBTrace}
  LookupSegCount    : Integer;
{$ENDIF}
  NextSeg           : TffInt64;                                        {!!.11}
begin
{$IFDEF BLOBTrace}
  Logbt('Entering FFTblWriteBLOB', []);
  Logbt('  aBLOBNr = %d:%d', [aBLOBNr.iLow, aBLOBNr.iHigh]);
  Logbt('  aOffset = %d', [aOffset]);
  Logbt('  aLen    = %d', [aLen]);
  try
{$ENDIF}

  BLOBAsBytes := @aBLOB;
  ContentSegOfs.iLow := ffc_W32NoValue;
  LookupSegBlk := nil;

  { We use the following list to track the RAM pages we've accessed and
    the release method associated with each RAM page. At the end of this
    routine, we will call the release method for each RAM page. }
  aRelList := TffPointerList.Create;

  try
    { Read and verify the BLOB header block for this BLOB number. }
    BLOBBlock := ReadVfyBlobBlock(aFI,
                                  aTI,
                                  ffc_MarkDirty,
                                  aBLOBNr,
                                  OffsetInBlock,
                                  aRelMethod);
    aRelList.Append(FFAllocReleaseInfo(BLOBBlock, TffInt64(aRelMethod)));
    BLOBHeader := @BLOBBlock^[OffsetInBlock];

    { Verify the new length (aLen + aOffset) doesn't exceed max. }
    NewSize := FFMaxL(aOffset + aLen, BLOBHeader^.bbhBLOBLength);
    if (NewSize > ffcl_MaxBLOBLength) then
      FFRaiseException(EffServerException,
                       ffStrResServer,
                       fferrBLOBTooBig,
                       [NewSize]);

    { Verify the BLOB has not been deleted. }
    if (BLOBHeader^.bbhSignature = ffc_SigBLOBSegDeleted) then
      FFRaiseException(EffServerException,
                       ffStrResServer,
                       fferrBLOBDeleted,
                       [aFI^.fiName^,
                        aBLOBNr.iHigh,
                        aBLOBNr.iLow]);

    { For a file BLOB raise an error. }
    if (BLOBHeader^.bbhSegCount = -1) then
      FFRaiseException(EffServerException,
                       ffStrResServer,
                       fferrFileBLOBWrite,
                       [aFI^.fiName^,
                        aBLOBNr.iLow,
                        aBLOBNr.iHigh]);

    { Verify the offset is within, or at the end of, the BLOB. }
    if (aOffset > BLOBHeader^.bbhBLOBLength) then
      FFRaiseException(EffServerException,
                       ffStrResServer,
                       fferrOfsNotInBlob,
                       [aFI^.fiName^,
                        aBLOBNr.iLow,
                        aBLOBNr.iHigh,
                        aOffset,
                        BLOBHeader^.bbhBLOBLength]);

    { If there's not one, we'll need a lookup segment. }
    if (BLOBHeader^.bbh1stLookupSeg.iLow = ffc_W32NoValue) then begin
      NewSegment := True;
      TempWord := EstimateSegmentCount(NewSize, aFI^.fiMaxSegSize);
      TempWord := (TempWord * ffc_BLOBLookupEntrySize) + ffc_BLOBSegmentHeaderSize;
      TempWord := FFMinDW(TempWord, aFI^.fiMaxSegSize);
      BLOBHeader^.bbh1stLookupSeg := aFI^.fiBLOBrscMgr.NewSegment(aFI,
                                                                  aTI,
                                                                  TempWord,
                                                                  (TempWord div 2));
      {$IFDEF BLOBTrace}
        Logbt('  Built first lookup segment: %d:%d',
              [BLOBHeader^.bbh1stLookupSeg.iLow,
               BLOBHeader^.bbh1stLookupSeg.iHigh]);
      {$ENDIF}
    end else begin
      NewSegment := False;
      {$IFDEF BLOBTrace}
        Logbt('  First lookup segment established: %d:%d',
              [BLOBHeader^.bbh1stLookupSeg.iLow,
               BLOBHeader^.bbh1stLookupSeg.iHigh]);
      {$ENDIF}
    end;

    { Get the first lookup segment. }
    LookupSegBlk := ReadVfyBlobBlock(aFI,
                                     aTI,
                                     ffc_MarkDirty,
                                     BLOBHeader^.bbh1stLookupSeg,
                                     LookupSegOfs,
                                     aLkpRelMethod);
    LookupSegPtr := @LookupSegBlk^[LookupSegOfs];
    if (NewSegment) then begin
      LookupSegPtr^.bshParentBLOB := aBLOBNr;
      LookupSegPtr^.bshSignature := ffc_SigBLOBSegLookup;
      LookupSegPtr^.bshNextSegment.iLow := ffc_W32NoValue;
    end;
    MaxLookupEntries := FFCalcMaxLookupEntries(LookupSegPtr);

    LookupEntOfs := LookupSegOfs + SizeOf(TffBLOBSegmentHeader);
    CurrLookupEntry := 1;
    {$IFDEF BLOBTrace}
      LookupSegCount := 1;
      Logbt('  Lookup segment - Max entries: %d', [MaxLookupEntries]);
    {$ENDIF}

    { Position to where we are to start writing. }
    BLOBPos := 0;
    LookupEntry := nil;
    StartBytesUsed := 0;
    while (BLOBPos < aOffset) do begin
      LookupEntry := @LookupSegBlk^[LookupEntOfs];
      {$IFDEF BLOBTrace}
        Logbt('  Lookup entry %d points to a segment with %d bytes',
              [CurrLookupEntry,
               LookupEntry^.bleContentLength]);
      {$ENDIF}
      { Does this entry point to the segment where we should start
        copying data? }
      if ((BLOBPos + LookupEntry^.bleContentLength) >= aOffset) then begin
        { Yes. We found the starting point. }
        ContentSegOfs := LookupEntry^.bleSegmentOffset;
        StartBytesUsed := aOffset - BLOBPos;
        { NOTE: We will be making updates to this segment, so we don't
                want to move past it. }
        Break;
      end else begin
        { Nope. Update and keep moving. }
        BLOBPos := BLOBPos + LookupEntry^.bleContentLength;
        LookupEntOfs := LookupEntOfs + ffc_BLOBLookupEntrySize;
        CurrLookupEntry := CurrLookupEntry + 1;
      end;

      { Have we reached the end of this lookup segment? }
      if (CurrLookupEntry > MaxLookupEntries) then begin
        { Get the lookup segment block and set up offset for 1st lookup entry. }
        NextSeg := LookupSegPtr^.bshNextSegment;                       {!!.11}
        aLkpRelMethod(LookupSegBlk);
        LookupSegBlk := ReadVfyBlobBlock2(aFI,
                                          aTI,
                                          ffc_MarkDirty,
                                          NextSeg,                     {!!.11}
                                          LookupBlock,
                                          LookupSegOfs,
                                          aLkpRelMethod);
        LookupSegPtr := @LookupSegBlk^[LookupSegOfs];
        LookupEntOfs := LookupSegOfs + SizeOf(TffBLOBSegmentHeader);

        { How many entries are in the current lookup segment? }
        MaxLookupEntries := FFCalcMaxLookupEntries(LookupSegPtr);
        CurrLookupEntry := 1;
        {$IFDEF BLOBTrace}
          LookupSegCount := LookupSegCount + 1;
          Logbt('  Moving to lookup segment %d',
                [LookupSegCount]);
          Logbt('  Lookup segment - Max entries: %d',
                [MaxLookupEntries]);
        {$ENDIF}
      end;
    end;

    { We may need to initialize the previous content segment so that
      we can maintain the chain. }
    if ((BLOBPos = 0) and
        (BLOBHeader^.bbhBLOBLength > 0)) then begin
      LookupEntry := @LookupSegBlk^[LookupEntOfs];
      ContentSegOfs := LookupEntry^.bleSegmentOffset;
    end;

    ContentSegPtr := nil;
    if (ContentSegOfs.iLow <> ffc_W32NoValue) then begin
      { Get the previous content segment. }
      ContentSegOfs := LookupEntry^.bleSegmentOffset;
      ContentSegBlk := ReadVfyBlobBlock(aFI,
                                        aTI,
                                        ffc_MarkDirty,
                                        ContentSegOfs,
                                        OffsetInBlock,
                                        aRelMethod);
      aRelList.Append(FFAllocReleaseInfo(ContentSegBlk,
                                         TffInt64(aRelMethod)));
      ContentSegPtr := @ContentSegBlk^[OffsetInBlock];
      {$IFDEF BLOBTrace}
        Logbt('  Initialized 1st content segment to write to: %d:%d',
              [ContentSegOfs.iLow, ContentSegOfs.iHigh]);
        Logbt('  Total segment length: %d',
              [ContentSegPtr^.bshSegmentLen]);
        Logbt('  Bytes to keep: %d',
              [StartBytesUsed]);
      {$ENDIF}
    end;

    { I've been using BLOBPos to track where I was at in the existing
      BLOB, if any. Now, I'm going to be using it to track where we
      are in the source (data being added to the BLOB). }
    BLOBPos := 0;

    { Now we're positioned and ready to start copying the source data
      to the BLOB. }
    BytesToGo := aLen;
    while (BytesToGo > 0) do begin
      { Are we overwriting an existing segment? }
      if (ContentSegOfs.iLow <> ffc_W32NoValue) then begin
        { Yes. Get the location of the existing segment so we can
          update it. }
        BytesToCopy := BytesToGo;
        {$IFDEF BLOBTrace}
          Logbt('  Updating existing segment: %d:%d.',
                [ContentSegOfs.iLow, ContentSegOfs.iHigh]);
        {$ENDIF}
      end else begin
        { Nope. We'll have to intialize a new lookup entry and get a
          new content segment. }
        NewSegment := True;
        LookupEntry := @LookupSegBlk^[LookupEntOfs];

        { Update the previous content segment so we can chain it to the
          next one later. }
        PrevContentSegPtr := ContentSegPtr;

        { Figure out how many bytes we "want" to copy. }
        BytesToCopy := ffMinL(aFI^.fiMaxSegSize, BytesToGo);

        { Get a new content segment}
        SegSize := BytesToCopy;
        MinSegSize := ffc_BLOBSegmentIncrement;
        ContentSegOfs := aFI^.fiBLOBrscMgr.NewSegment(aFI,
                                                      aTI,
                                                      SegSize,
                                                      MinSegSize);
        LookupEntry^.bleSegmentOffset := ContentSegOfs;
        LookupEntry^.bleContentLength := 0;

        { Increment the segment count. }
        BLOBHeader^.bbhSegCount := BLOBHeader^.bbhSegCount + 1;

        if (PrevContentSegPtr <> nil) then begin
          PrevContentSegPtr^.bshNextSegment := ContentSegOfs;
        end;
        {$IFDEF BLOBTrace}
          Logbt('  Created new segment: %d:%d.',
                [ContentSegOfs.iLow, ContentSegOfs.iHigh]);
        {$ENDIF}
      end;

      { Get the content segment. }
      ContentSegBlk := ReadVfyBlobBlock(aFI,
                                        aTI,
                                        ffc_MarkDirty,
                                        ContentSegOfs,
                                        OffsetInBlock,
                                        aRelMethod);
      aRelList.Append(FFAllocReleaseInfo(ContentSegBlk,
                                         TffInt64(aRelMethod)));
      ContentSegPtr := @ContentSegBlk^[OffsetInBlock];
      if (NewSegment) then begin
        ContentSegPtr^.bshSignature := ffc_SigBLOBSegContent;
        ContentSegPtr^.bshParentBLOB := aBLOBNr;
        ContentSegPtr^.bshNextSegment.iLow := ffc_W32NoValue;
        NewSegment := False;
      end;

      { We may not have gotten an optimal size segment, so we need
        to update how many bytes we can copy based on the actual
        segment size. }
      StartBytesUsed := StartBytesUsed + ffc_BLOBSegmentHeaderSize;
      TargetOffset := OffsetInBlock + StartBytesUsed;
      SegBytesLeft := ContentSegPtr^.bshSegmentLen - StartBytesUsed;
      BytesToCopy := FFMinL(BytesToCopy, SegBytesLeft);

      { Copy. }
      Move(BLOBAsBytes^[BLOBPos],
           ContentSegBlk^[TargetOffset],
           BytesToCopy);
      BytesToGo := BytesToGo - BytesToCopy;
      BLOBPos := BLOBPos + BytesToCopy;
      Assert(BytesToGo <= aLen, 'BLOB writing is out of whack');

      {$IFDEF BLOBTrace}
        Logbt('  Copied %d bytes to lookup segment %d, entry %d, content segment %d:%d',
              [BytesToCopy,
               LookupSegCount,
               CurrLookupEntry,
               ContentSegOfs.iLow, ContentSegOfs.iHigh]);
      {$ENDIF}

      StartBytesUsed := StartBytesUsed - ffc_BLOBSegmentHeaderSize;
      { Update the content length of the lookup entry. We have several
        cases to account for:
        1. Write X bytes to empty segment. Length = X.
        2. Suffix X bytes to end of segment containing Y bytes.
           Length = X + Y.
        3. Write X bytes to segment containing Y bytes where X <= Y and
           (aOffset + X) <= Y.  Length = Y.
        4. Write X bytes to segment containing Y bytes where X <= Y and
           (aOffset + X) > Y. Length = # untouched bytes + Y.
        These cases are all handled by the following IF statement. }
      if (StartBytesUsed + BytesToCopy >
          LookupEntry^.bleContentLength) then begin
        LookupEntry^.bleContentLength := StartBytesUsed + BytesToCopy;
      end;
      {$IFDEF BLOBTrace}
        Logbt('  Last lookup entry now points to segment with %d bytes',
              [LookupEntry^.bleContentLength]);
      {$ENDIF}

      CurrLookupEntry := CurrLookupEntry + 1;
      StartBytesUsed := 0;

      { Have we reached the end of this lookup segment? }
      if ((BytesToGo > 0) and
          (CurrLookupEntry > MaxLookupEntries)) then begin
        { Is there another lookup segment in this chain? }
        if (LookupSegPtr^.bshNextSegment.iLow = ffc_W32NoValue) then begin
          { No. We'll have to get a new one and add it to the chain. }
          TempWord := EstimateSegmentCount(BytesToGo, aFI^.fiMaxSegSize);
          TempWord := (TempWord * ffc_BLOBLookupEntrySize) + ffc_BLOBSegmentHeaderSize;
          TempWord := FFMinDW(TempWord, aFI^.fiMaxSegSize);

          { Use ContentSegPtr to hold the new lookup segment's offset
            temporarily. }
          ContentSegOfs := aFI^.fiBLOBrscMgr.NewSegment(aFI,
                                                        aTI,
                                                        TempWord,
                                                        TempWord);
          {$IFDEF BLOBTrace}
            Logbt('  Creating new lookup segment: %d:%d.',
                  [ContentSegOfs.iLow, ContentSegOfs.iHigh]);
          {$ENDIF}
        end else begin
          { Yes. Assign it to our temp variable. }
          ContentSegOfs := LookupSegPtr^.bshNextSegment;
          {$IFDEF BLOBTrace}
            Logbt('  Moving to next lookup segment.',
                  [ContentSegOfs.iLow, ContentSegOfs.iHigh]);
          {$ENDIF}
        end;

        { Get the lookup segment block and set up offset for 1st
          lookup entry. }
        aLkpRelMethod(LookupSegBlk);
        LookupSegBlk := ReadVfyBlobBlock2(aFI,
                                          aTI,
                                          ffc_MarkDirty,
                                          ContentSegOfs,
                                          LookupBlock,
                                          LookupSegOfs,
                                          aLkpRelMethod);

        { Intialize the segment on if it's new. }
        if ((LookupSegPtr <> nil) and
            (LookupSegPtr^.bshNextSegment.iLow <> ffc_W32NoValue)) then begin
          LookupSegPtr := @LookupSegBlk^[LookupSegOfs];
          LookupEntOfs := LookupSegOfs + ffc_BLOBSegmentHeaderSize;
          LookupEntry := @LookupSegBlk^[LookupEntOfs];
          ContentSegOfs := LookupEntry^.bleSegmentOffset;
        end else begin
          { Chain the last lookup segment to the new one. }
          LookupSegPtr^.bshNextSegment := ContentSegOfs;

          LookupSegPtr := @LookupSegBlk^[LookupSegOfs];
          LookupSegPtr^.bshParentBLOB := aBLOBNr;
          LookupSegPtr^.bshSignature := ffc_SigBLOBSegLookup;
          LookupSegPtr^.bshNextSegment.iLow := ffc_W32NoValue;

          LookupEntOfs := LookupSegOfs + ffc_BLOBSegmentHeaderSize;
          LookupEntry := @LookupSegBlk^[LookupEntOfs];
          ContentSegOfs.iLow := ffc_W32NoValue;
        end;

        { How many entries are in the current lookup segment? }
        MaxLookupEntries := FFCalcMaxLookupEntries(LookupSegPtr);
        CurrLookupEntry := 1;
        {$IFDEF BLOBTrace}
          LookupSegCount := LookupSegCount + 1;
          Logbt('  Moving to lookup segment %d',
                [LookupSegCount]);
          Logbt('  Lookup segment - Max entries: %d',
                [MaxLookupEntries]);
        {$ENDIF}
      end else begin
        LookupEntOfs := LookupEntOfs + ffc_BLOBLookupEntrySize;
        LookupEntry := @LookupSegBlk^[LookupEntOfs];
        ContentSegOfs := ContentSegPtr^.bshNextSegment;
      end;
    end;

    { If the BLOB has grown, we need to update its length.
      NOTE: BLOBs can't be truncated via a write operation. }
    if (NewSize > BLOBHeader^.bbhBLOBLength) then
      BLOBHeader^.bbhBLOBLength := NewSize;
  finally
    if (LookupSegBlk <> nil) then
      aLkpRelMethod(LookupSegBlk);
    for OffsetInBlock := 0 to (aRelList.Count - 1) do
      FFDeallocReleaseInfo(aRelList[OffsetInBlock]);
    aRelList.Free;
  end;
{$IFDEF BLOBTrace}
  except
    on E:Exception do begin
      Logbt('*** FFTblWriteBLOB Error: %s', [E.Message]);
      raise;
    end;
  end;
{$ENDIF}
end;
{====================================================================}

{===Tff210BLOBEngine=================================================}
procedure Tff210BLOBEngine.Read(aFI         : PffFileInfo;
                                   aTI         : PffTransInfo;
                                   aBLOBNr     : TffInt64;
                                   aOffset     : TffWord32;
                                   aLen        : TffWord32;                     
                                   aReadMethod : TffBLOBLinkRead;
                               var aBLOB;
                               var aBytesRead  : TffWord32;
                               var aFBError    : TffResult);
var
  BLOBAsBytes    : PffBLOBArray;
  BLOBBlock      : PffBlock;
  BLOBBlockHdr   : PffBlockHeaderBLOB absolute BLOBBlock;
  BLOBBlockNum   : TffWord32;
  BLOBHeader     : PffBLOBHeader;
  BytesToCopy    : Longint;
  CmpRes         : integer;
  ContentBlock   : TffWord32;
  ContentSegBlk  : PffBlock;
  ContentSegOfs  : TffWord32;
  DestOffset     : Longint;
  EndBytesUsed   : TffWord32;
  EndSegInx      : Integer;
  EntryCount     : Integer;
  LookupBlock    : TffWord32;
  LookupEntry    : PffBLOBLookupEntry;
  LookupSegBlk   : PffBlock;
  LookupSegPtr   : PffBLOBSegmentHeader;
  NextSeg        : TffInt64;
  OffsetInBlock  : TffWord32;
  SegInx         : Integer;
  StartBytesUsed : TffWord32;
  StartSegInx    : Integer;
  aCntRelMethod,
  aLkpRelMethod,
  aHdRelMethod   : TffReleaseMethod;
begin
  BLOBAsBytes := @aBLOB;
  ContentSegBlk := nil;
  LookupSegBlk := nil;

  aFBError := 0;

  {Exit if aLen = 0}
  if aLen = 0 then
    Exit;

  { Read and verify the BLOB header block for this BLOB number. }
  BLOBBlock := ReadVfyBlobBlock2(aFI, aTI, ffc_ReadOnly, aBLOBNr,
                                 BLOBBlockNum, OffsetInBlock, aHdRelMethod);
  BLOBHeader := @BLOBBlock^[OffsetInBlock];

  { Verify the BLOB has not been deleted. }
  if (BLOBHeader^.bbhSignature = ffc_SigBLOBSegDeleted) then
    FFRaiseException(EffServerException, ffStrResServer,
                     fferrBLOBDeleted,
                     [aFI^.fiName^, aBLOBNr.iHigh, aBLOBNr.iLow]);

  try
    { Are we dealing with a file BLOB or a BLOB link? }
    case BLOBHeader^.bbhSegCount of
      ffc_FileBLOB : { file BLOB }
        begin
           aFBError := FileBLOBRead(aFI, aTI, aBLOBNr, aOffset, aLen, aBLOB,
                                    aBytesRead);
           Exit;
        end;
      ffc_BLOBLink : { BLOB link }
        begin
          aFBError := BLOBLinkRead(aFI, aTI, aBLOBNr, aOffset, aLen,
                                   aReadMethod, aBLOB, aBytesRead);
          Exit;
        end;
    end;  { case }

    { Make sure that the offset is within BLOB. }
    CmpRes := FFCmpDW(aOffset, BLOBHeader^.bbhBLOBLength);
    if (CmpRes >= 0) then begin
      aBytesRead := 0;
      Exit;
    end;
    { Get the lookup segment block and set up offset for 1st lookup entry. }
    LookupSegBlk := ReadVfyBlobBlock2(aFI, aTI, ffc_ReadOnly,
                                      BLOBHeader^.bbh1stLookupSeg,
                                      LookupBlock, OffsetInBlock,
                                      aLkpRelMethod);
    LookupSegPtr := @LookupSegBlk^[OffsetInBlock];
    OffsetInBlock := OffsetInBlock + sizeof(TffBLOBSegmentHeader);

    { Calculate the number of bytes we can (= "are going to") read. }
    aBytesRead := ffMinDW(aLen, BLOBHeader^.bbhBLOBLength - aOffset);

    { Calculate the starting & ending index of the segments to read. }
    if aOffset = 0 then begin
      StartSegInx := 0;
      StartBytesUsed := 0;
    end
    else
      StartSegInx := CalcBLOBSegNumber(aOffset, aFI^.fiBlockSize, StartBytesUsed);
    EndSegInx := CalcBLOBSegNumber(aOffset + aBytesRead,
                                   aFI^.fiBlockSize,
                                   EndBytesUsed);

    { Walk through the BLOB segment linked list, reading segments as we
      go, copying to the BLOB when required. }
    SegInx := 1;
    DestOffset := 0;
    EntryCount := 0;
    ContentBlock := 0;
    while (SegInx <= EndSegInx) do begin
      inc(EntryCount);
      LookupEntry := @LookupSegBlk^[OffsetInBlock];

      {if we should read from this block, do so}
      if (SegInx >= StartSegInx) then begin

        { Read the BLOB content segment. }
        if assigned(ContentSegBlk) then
          aCntRelMethod(ContentSegBlk);
        ContentSegBlk := ReadVfyBlobBlock2(aFI, aTI, ffc_ReadOnly,
                                           LookupEntry^.bleSegmentOffset,
                                           ContentBlock, ContentSegOfs,
                                           aCntRelMethod);
        ContentSegOfs := ContentSegOfs + sizeof(TffBLOBSegmentHeader);

        if SegInx = StartSegInx then begin
          { move from starting offset to dest }
          BytesToCopy := LookupEntry^.bleContentLength - StartBytesUsed;
          ContentSegOfs := ContentSegOfs + StartBytesUsed;
        end else if SegInx = EndSegInx then begin
          { move up to ending offset to dest }
          BytesToCopy := EndBytesUsed;
        end else begin
          { copying from middle segments }
          BytesToCopy := LookupEntry^.bleContentLength;
        end;
        BytesToCopy := ffMinL(BytesToCopy, aBytesRead);
        Move(ContentSegBlk^[ContentSegOfs], BLOBAsBytes^[DestOffset], BytesToCopy);
        inc(DestOffset, BytesToCopy);
      end; { if }

      {see if we're at the end of the lookup segment}
      if ((SegInx <> EndSegInx) and
          (LookupSegPtr^.bshSegmentLen <
           (sizeof(TffBLOBSegmentHeader) +
            (succ(EntryCount) * sizeof(TffBLOBLookupEntry))))) then begin
        NextSeg := LookupSegPtr^.bshNextSegment;
        aLkpRelMethod(LookupSegBlk);
        LookupSegBlk := ReadVfyBlobBlock2(aFI, aTI, ffc_ReadOnly,
                                          NextSeg,                     {!!.11}
                                          LookupBlock, OffsetInBlock,
                                          aLkpRelMethod);
        LookupSegPtr := @LookupSegBlk^[OffsetInBlock];
        OffsetInBlock := OffsetInBlock + sizeof(TffBLOBSegmentHeader);
        EntryCount := 0;
      end else
        OffsetInBlock := OffsetInBlock + sizeof(TffBLOBLookupEntry);
      inc(SegInx);
    end; {while}
  finally
    if assigned(ContentSegBlk) then
      aCntRelMethod(ContentSegBlk);
    if assigned(LookupSegBlk) then
      aLkpRelMethod(LookupSegBlk);
    aHdRelMethod(BLOBBlock);
  end;
end;
{--------}
procedure Tff210BLOBEngine.Truncate(aFI     : PffFileInfo;
                                       aTI     : PffTransInfo;
                                       aBLOBNr : TffInt64;
                                       aLen    : TffWord32);                    
var
  BLOBBlock      : PffBlock;
  BLOBBlockHdr   : PffBlockHeaderBLOB absolute BLOBBlock;
  BLOBHeader     : PffBLOBHeader;
  EntryCount     : TffWord32;
  OffsetInBlock  : TffWord32;
  NewSegCount    : TffWord32;
  OldSegCount    : TffWord32;
  i              : Integer;
  IsNewTailSeg   : Boolean;
  LookupBlock    : TffWord32;
  LookupSegBlk   : PffBlock;
  LookupSegOfs   : TffInt64;
  LookupSegPtr   : PffBLOBSegmentHeader;
  LookupEntOfs   : TffWord32;
  LookupEntPtr   : PffBLOBLookupEntry;
  OldUsedSpace   : TffWord32;
  NewUsedSpace   : TffWord32;
  OldContSegOfs  : TffWord32;
  OldContSegBlk  : PffBlock;
  OldContSegPtr  : PffBLOBSegmentHeader;
  NewContSegOfs  : TffWord32;
  NewContSegBlk  : PffBlock;
  NewContSegPtr  : PffBLOBSegmentHeader;
  NextLookupSeg  : TffInt64;
  UpdatedContSeg : TffInt64;
  SegEntries     : TffWord32;
  TailEntry      : TffWord32;
  TotEntries     : TffWord32;
  aRelList       : TffPointerList;
  aRelMethod     : TffReleaseMethod;
  SegSize        : TffWord32;
begin

  { We use the following list to track the RAM pages we've accessed and
    the release method associated with each RAM page. At the end of this
    routine, we will call the release method for each RAM page. }
  aRelList := TffPointerList.Create;

  try
    { Read and verify the BLOB header block for this BLOB number. }
    BLOBBlock := ReadVfyBlobBlock(aFI, aTI, ffc_MarkDirty, aBLOBNr,
                                  OffsetInBlock, aRelMethod);
    aRelList.Append(FFAllocReleaseInfo(BLOBBlock,TffInt64(aRelMethod)));

    BLOBHeader := @BLOBBlock^[OffsetInBlock];

    { Check if we're trying to truncate a zero-length BLOB or to the
      BLOB's current length. }
    if ((BLOBHeader^.bbhBLOBLength = aLen) or
        ((BLOBHeader^.bbhBLOBLength = 0) and
         (aLen = 0))) then
      Exit;

    { Verify the BLOB has not been deleted. }
    if (BLOBHeader^.bbhSignature = ffc_SigBLOBSegDeleted) then
      FFRaiseException(EffServerException, ffStrResServer,
                       fferrBLOBDeleted,
                       [aFI^.fiName^, aBLOBNr.iHigh, aBLOBNr.iLow]);
    { Verify this is a header segment. }
    if (BLOBHeader^.bbhSignature <> ffc_SigBLOBSegHeader) then
      FFRaiseException(EffServerException, ffStrResServer, fferrBadBLOBSeg,
                       [aFI^.fiName^, aBLOBNr.iLow, aBLOBNr.iHigh,
                        format(ffcBLOBSegExpected,
                               [ffcBLOBSegHeader,
                                char(BLOBHeader^.bbhSignature)])]);

    { We can't write to a file BLOB. }
    if (BLOBHeader^.bbhSegCount = -1) then
      FFRaiseException(EffServerException, ffStrResServer,
                       fferrFileBLOBWrite, [aFI^.fiName^, aBLOBNr.iLow,
                                            aBLOBNr.iHigh]);

    { Make sure the truncated length <= current BLOB length. }
    if (aLen > BLOBHeader^.bbhBLOBLength) then
      FFRaiseException(EffServerException, ffStrResServer, fferrLenMismatch,
                       [aFI^.fiName^, aBLOBNr.iLow, aBLOBNr.iHigh, aLen,
                        BLOBHeader^.bbhBLOBLength]);

    { If the new length is greater than 0, we will lop off some content
      segments.  The content segment that becomes the last content segment
      must be resized. }
    if aLen > 0 then begin

      { Calculate the number of segments for the old and new lengths. }
      OldSegCount := CalcBLOBSegNumber(BLOBHeader^.bbhBLOBLength,
                                       aFI^.fiBlockSize, OldUsedSpace);
      NewSegCount := CalcBLOBSegNumber(aLen, aFI^.fiBlockSize, NewUsedSpace);

      { Grab the first lookup segment. }
      NextLookupSeg := BLOBHeader^.bbh1stLookupSeg;
      LookupSegBlk := ReadVfyBlobBlock2(aFI, aTI, ffc_MarkDirty,       {!!.12}
                                        NextLookupSeg, LookupBlock,
                                        OffsetInBlock, aRelMethod);
      aRelList.Append(FFAllocReleaseInfo(LookupSegBlk,TffInt64(aRelMethod)));
      LookupSegPtr := PffBLOBSegmentHeader(@LookupSegBlk^[OffsetInBlock]);

      TotEntries := 0;

      { Calculate # of entries in this lookup segment. }
      SegEntries := FFCalcMaxLookupEntries(LookupSegPtr);

      { Walk through the lookup segments until we find the lookup segment
        containing the new tail lookup entry. }
      while ((TotEntries + SegEntries) < NewSegCount) do begin

        Inc(TotEntries, SegEntries);

        { Grab the offset of the next lookup segment. }
        NextLookupSeg := LookupSegPtr^.bshNextSegment;

        LookupSegBlk := ReadVfyBlobBlock2(aFI, aTI, ffc_MarkDirty,
                                          NextLookupSeg,               {!!.12}
                                          LookupBlock, OffsetInBlock,
                                          aRelMethod);
        aRelList.Append(FFAllocReleaseInfo(LookupSegBlk,TffInt64(aRelMethod)));
        LookupSegPtr := PffBLOBSegmentHeader(@LookupSegBlk^[OffsetInBlock]);
        SegEntries := FFCalcMaxLookupEntries(LookupSegPtr);
      end;

      { Find the lookup entry that will now point to the new tail content
        segment. }
      TailEntry := pred(NewSegCount - TotEntries);  { base zero }
      LookupEntOfs := (OffsetInBlock + sizeof(TffBLOBSegmentHeader) +
                       (TailEntry * sizeof(TffBLOBLookupEntry)));
      LookupEntPtr := PffBLOBLookupEntry(@LookupSegBlk^[LookupEntOfs]);

      { Grab the content segment pointed to by this lookup entry.  We will copy
        over some of those bytes to the new tail content segment. }
      UpdatedContSeg := LookupEntPtr^.bleSegmentOffset;

      { Obtain the new tail content segment. }
      SegSize := NewUsedSpace + sizeof(TffBLOBSegmentHeader);
      LookupEntPtr^.bleSegmentOffset :=
        aFI^.fiBLOBrscMgr.NewSegment(aFI, aTI, SegSize, SegSize);
      LookupEntPtr^.bleContentLength := NewUsedSpace;

      { Initialize the new content segment header. }
      NewContSegBlk := ReadVfyBlobBlock(aFI, aTI, ffc_MarkDirty,
                                        LookupEntPtr^.bleSegmentOffset,
                                        NewContSegOfs, aRelMethod);
      aRelList.Append(FFAllocReleaseInfo(NewContSegBlk, TffInt64(aRelMethod)));
      NewContSegPtr := PffBLOBSegmentHeader(@NewContSegBlk^[NewContSegOfs]);
      NewContSegPtr^.bshSignature := ffc_SigBLOBSegContent;
      NewContSegPtr^.bshParentBLOB := aBLOBNr;
      NewContSegPtr^.bshNextSegment.iLow := ffc_W32NoValue;

      { If there is more than one content segment in the truncated BLOB,
        make sure the next to last content segment points to the new tail
        content segment. }
      if NewSegCount > 1 then begin
        LookupEntPtr := PffBLOBLookupEntry(@LookupSegBlk^[LookupEntOfs -
                                                          sizeof(TffBLOBLookupEntry)]);
        OldContSegBlk := ReadVfyBlobBlock(aFI, aTI, ffc_MarkDirty,
                                          LookupEntPtr^.bleSegmentOffset,
                                          OldContSegOfs, aRelMethod);
        aRelList.Append(FFAllocReleaseInfo(OldContSegBlk, TffInt64(aRelMethod)));
        OldContSegPtr := PffBLOBSegmentHeader(@OldContSegBlk^[OldContSegOfs]);

        { Restore LookupEntPtr. }
        LookupEntPtr := PffBLOBLookupEntry(@LookupSegBlk^[LookupEntOfs]);
        OldContSegPtr^.bshNextSegment := LookupEntPtr^.bleSegmentOffset;
      end;

      { Copy NewUsedSpace bytes from the old content segment to new tail content
        segment. }
      OldContSegBlk := ReadVfyBlobBlock(aFI, aTI, ffc_MarkDirty,
                                        UpdatedContSeg, OldContSegOfs,
                                        aRelMethod);
      aRelList.Append(FFAllocReleaseInfo(OldContSegBlk, TffInt64(aRelMethod)));
      Move(OldContSegBlk^[OldContSegofs + sizeof(TffBLOBSegmentHeader)],
           NewContSegBlk^[NewContSegOfs + sizeof(TffBLOBSegmentHeader)],
           NewUsedSpace);

      { Get rid of the old content segment. }
      aFI^.fiBLOBrscMgr.DeleteSegment(aFI, aTI, UpdatedContSeg);

      { Delete the content & lookup segments that are no longer needed.
        First, obtain the number of extraneous lookup entries in the
        current lookup segment. }
      EntryCount := FFCalcMaxLookupEntries(LookupSegPtr) - succ(TailEntry);

      { Initialize the lookup entry offset & pointer.  They must
        point to the lookup entry after the new tail lookup entry. }
      LookupEntOfs := (OffsetInBlock + sizeof(TffBLOBSegmentHeader) +
                       (succ(TailEntry) * sizeof(TffBLOBLookupEntry)));
      LookupEntPtr := PffBLOBLookupEntry(@LookupSegBlk^[LookupEntOfs]);

      { Save the offset of the current lookup segment. }
      LookupSegOfs := NextLookupSeg;

      IsNewTailSeg := True;

      { Free each content segment. }
      for i := succ(NewSegCount) to OldSegCount do begin

        aFI^.fiBLOBrscMgr.DeleteSegment(aFI, aTI, LookupEntPtr^.bleSegmentOffset);
        dec(EntryCount);

        { Need to move to another lookup segment? }
        if ((EntryCount = 0) and (LookupSegPtr^.bshNextSegment.iLow <> ffc_W32NoValue)) then begin
          {Yes.  Get the location of the next lookup segment. }
          NextLookupSeg := LookupSegPtr^.bshNextSegment;

          { If this is not the new tail lookup segment then delete the
            lookup segment. }
          if IsNewTailSeg then
            IsNewTailSeg := False
          else
            aFI^.fiBLOBrscMgr.DeleteSegment(aFI, aTI, LookupSegOfs);

          { Grab the next lookup segment. }
          LookupSegBlk := ReadVfyBlobBlock(aFI, aTI, ffc_MarkDirty,
                                           NextLookupSeg, OffsetInBlock,
                                           aRelMethod);
          aRelList.Append(FFAllocReleaseInfo(LookupSegBlk, TffInt64(aRelMethod)));
          LookupSegPtr := @LookupSegBlk^[OffsetInBlock];
          LookupEntOfs := OffsetInBlock + sizeof(TffBLOBSegmentHeader);
          LookupSegOfs := NextLookupSeg;
          EntryCount := FFCalcMaxLookupEntries(LookupSegPtr);
        end;

        { If this is the new tail lookup segment then zero out the lookup
          entry. }
        if IsNewTailSeg then
          FillChar(LookupEntPtr^, sizeof(TffBLOBLookupEntry), 0);      {!!.13}

        { Grab the next lookup entry. }
        LookupEntOfs := LookupEntOfs + sizeof(TffBLOBLookupEntry);
        LookupEntPtr := @LookupSegBlk^[LookupEntOfs];
      end; {for}

      { Delete the last lookup segment if it's not the new tail
        segment.}
      if not IsNewTailSeg then
        aFI^.fiBLOBrscMgr.DeleteSegment(aFI, aTI, LookupSegOfs);

      { Set the new segment count in the BLOB header. }
      BLOBHeader^.bbhSegCount := NewSegCount;

    end else begin {we are truncating to length of 0}

      FFTblDeleteBLOBPrim(aFI, aTI, BLOBHeader);

      { Reset the lookup segment field and the segment count.
        FFTblFreeBLOB will get rid of the BLOB header if the BLOB is
        still at length 0. }
      BLOBHeader^.bbh1stLookupSeg.iLow := ffc_W32NoValue;
      BLOBHeader^.bbhSegCount := 0;
      BLOBHeader^.bbhBLOBLength := 0;
    end;
    {set the new BLOB length and segment count in the BLOB header}
    BLOBHeader^.bbhBLOBLength := aLen;
  finally
    for OffsetInBlock := 0 to pred(aRelList.Count) do
      FFDeallocReleaseInfo(aRelList[OffsetInBlock]);
    aRelList.Free;
  end;
end;
{--------}
procedure Tff210BLOBEngine.Write(aFI     : PffFileInfo;
                                    aTI     : PffTransInfo;
                              const aBLOBNr : TffInt64;
                                    aOffset : TffWord32;   {offset in blob to start writing}
                                    aLen    : TffWord32;   {bytes from aOffset to stop writing}
                              const aBLOB);
var
  AvailSpace        : TffWord32;
  BLOBBlock         : PffBlock;
  BLOBBlockHdr      : PffBlockHeaderBLOB absolute BLOBBlock;
  BLOBHeader        : PffBLOBHeader;
  OffsetInBlock     : TffWord32;
  BLOBAsBytes       : PffBLOBArray;
  StartSegInx       : Integer;
  EndSegInx         : Integer;
  OldEndSeg         : Integer;
  SegInx            : Integer; { index into the blob }
  BytesToCopy       : TffWord32;
  BytesToGo         : TffWord32;
  SrcOffset         : Longint;
  LookupEntOfs      : TffWord32;
  LookupEntPtr      : PffBLOBLookupEntry;
  LookupSegOfs      : TffWord32;
  LookupSegPtr      : PffBLOBSegmentHeader;
  LookupSegBlk      : PffBlock;
  ContentSegOfs     : TffInt64;
  ContentSegBlk     : PffBlock;
  ContentSegPtr     : PffBLOBSegmentHeader;
  PrevContentSegPtr : PffBLOBSegmentHeader;
  StartBytesUsed    : TffWord32;
  EndBytesUsed      : TffWord32;
  EntryCount        : TffWord32;
  SegBytesLeft      : TffWord32;
  SegEntNumber      : TffWord32;         { index into the lookup segment }
  TargetOffset      : TffWord32;
  TempSegOfs        : TffInt64;
  TempSegBlk        : PffBlock;
  TempSegPtr        : PffBLOBSegmentHeader;
  TempOfsInBlk      : TffWord32;
  NewSize           : TffWord32;
  NewSizeW32        : TffWord32;
  BytesCopied       : Longint;
  aRelMethod        : TffReleaseMethod;
  aRelList          : TffPointerList;
  SegSize           : TffWord32;
begin
  BLOBAsBytes := @aBLOB;

  NewSizeW32 := aOffset + aLen;

  { Verify the new length (aLen + aOffset) doesn't exceed max. }
  if (NewSizeW32 > ffcl_MaxBLOBLength) then
    FFRaiseException(EffServerException, ffStrResServer,
                     fferrBLOBTooBig, [aOffset + aLen]);

  { We use the following list to track the RAM pages we've accessed and
    the release method associated with each RAM page. At the end of this
    routine, we will call the release method for each RAM page. }
  aRelList := TffPointerList.Create;

  try
    { Read and verify the BLOB header block for this BLOB number. }
    BLOBBlock := ReadVfyBlobBlock(aFI, aTI, ffc_MarkDirty, aBLOBNr,
                                  OffsetInBlock, aRelMethod);
    aRelList.Append(FFAllocReleaseInfo(BLOBBlock, TffInt64(aRelMethod)));
    BLOBHeader := @BLOBBlock^[OffsetInBlock];

    { Verify the BLOB has not been deleted. }
    if (BLOBHeader^.bbhSignature = ffc_SigBLOBSegDeleted) then
      FFRaiseException(EffServerException, ffStrResServer,
                       fferrBLOBDeleted,
                       [aFI^.fiName^, aBLOBNr.iHigh, aBLOBNr.iLow]);

    NewSize := FFMaxL(aOffset + aLen, BLOBHeader^.bbhBLOBLength);     

    { For a file BLOB raise an error. }
    if (BLOBHeader^.bbhSegCount = -1) then
      FFRaiseException(EffServerException, ffStrResServer,
                       fferrFileBLOBWrite, [aFI^.fiName^, aBLOBNr.iLow,
                                            aBLOBNr.iHigh]);

    { Verify the offset is within, or at the end of, the BLOB. }
    if (aOffset > BLOBHeader^.bbhBLOBLength) then
      FFRaiseException(EffServerException, ffStrResServer,
                       fferrOfsNotInBlob, [aFI^.fiName^, aBLOBNr.iLow,
                                           aBLOBNr.iHigh, aOffset,
                                           BLOBHeader^.bbhBLOBLength]);

    { If the BLOB is growing we need to rebuild the lookup segment(s). }
    if (NewSize > BLOBHeader^.bbhBLOBLength) then
      {the lookup segment(s) have to be rebuilt because the BLOB is growing}
      BLOBHeader^.bbh1stLookupSeg := FFTblRebuildLookupSegments(aFI, aTI,
                                                                NewSize,
                                                                BLOBHeader^.bbhBLOBLength,
                                                                aBLOBNr);

    { Get the first lookup segment. }
    LookupSegBlk := ReadVfyBlobBlock(aFI, aTI, ffc_MarkDirty,
                                     BLOBHeader^.bbh1stLookupSeg,
                                     LookupSegOfs, aRelMethod);
    aRelList.Append(FFAllocReleaseInfo(LookupSegBlk, TffInt64(aRelMethod)));
    LookupSegPtr := @LookupSegBlk^[LookupSegOfs];
    EntryCount := FFCalcMaxLookupEntries(LookupSegPtr);
    { Calculate the last segment in which we will write data. }
    EndSegInx := CalcBLOBSegNumber(aOffset + aLen, aFI^.fiBlockSize,
                                   EndBytesUsed);

    if BLOBHeader^.bbhBLOBLength = 0 then begin
      OldEndSeg := 0;
      StartSegInx := 0;
    end else begin
      { Calculate the number of segments currently used. }
      OldEndSeg := CalcBLOBSegNumber(BLOBHeader^.bbhBLOBLength,
                                     aFI^.fiBlockSize, EndBytesUsed);

      { Calculate the segment in which we will start writing the data. }
      if aOffset = 0 then begin
        StartSegInx := 1;                                             
        StartBytesUsed := 0;
      end                                                              
      else
        StartSegInx := CalcBLOBSegNumber(aOffset, aFI^.fiBlockSize, StartBytesUsed);
    end;

    ContentSegPtr := nil;
    PrevContentSegPtr := nil;
    SrcOffset := 0;
    BytesToGo := aLen;
    SegInx := 0;
    LookupEntOfs := LookupSegOfs + sizeof(TffBLOBSegmentHeader);
    LookupEntPtr := PffBLOBLookupEntry(@LookupSegBlk^[LookupEntOfs]);
    SegEntNumber := 0;

    { Walk through the lookup segments up to the current end segment. }
    while (SegInx < OldEndSeg) and (SegInx < EndSegInx) do begin

      { Is the segment one in which we are to write data? }
      if SegInx >= pred(StartSegInx) then begin

        { Get the content block. }
        ContentSegBlk := ReadVfyBlobBlock(aFI, aTI, ffc_MarkDirty,
                                          LookupEntPtr^.bleSegmentOffset,
                                          OffsetInBlock, aRelMethod);
        aRelList.Append(FFAllocReleaseInfo(ContentSegBlk, TffInt64(aRelMethod)));
        ContentSegPtr := @ContentSegBlk^[OffsetInBlock];

        SegBytesLeft := ContentSegPtr^.bshSegmentLen - ffc_BLOBSegmentHeaderSize;
        TargetOffset := OffsetInBlock + ffc_BLOBSegmentHeaderSize;

        { If this is the first segment to which we are writing, adjust the
          starting points. }
        if SegInx = pred(StartSegInx) then begin
          dec(SegBytesLeft, StartBytesUsed);
          inc(TargetOffset, StartBytesUsed);
        end
        else
          StartBytesUsed := 0;

        { If this old segment is not the largest it could be & is not big enough
          to hold what is left then we need a new segment}
        if StartBytesUsed = 0 then
          AvailSpace := ContentSegPtr^.bshSegmentLen -
                        ffc_BLOBSegmentHeaderSize
        else
          AvailSpace := SegBytesLeft;

        if ((ContentSegPtr^.bshSegmentLen < aFI^.fiMaxSegSize) and
            (AvailSpace < BytesToGo)) then begin

          { Calculate the size of the data in the new segment. }
          BytesToCopy := ffMinL(aFI^.fiMaxSegSize - ffc_BLOBSegmentHeaderSize,
                                BytesToGo + LookupEntPtr^.bleContentLength);

          { Allocate & retrieve the new segment. }
          SegSize := BytesToCopy + ffc_BLOBSegmentHeaderSize;
          TempSegOfs := aFI^.fiBLOBrscMgr.NewSegment(aFI, aTI, SegSize, SegSize);
          TempSegBlk := ReadVfyBlobBlock(aFI, aTI, ffc_MarkDirty,
                                         TempSegOfs, TempOfsInBlk,
                                         aRelMethod);
          aRelList.Append(FFAllocReleaseInfo(TempSegBlk, TffInt64(aRelMethod)));
          TempSegPtr := @TempSegBlk^[TempOfsInBlk];
          TempSegPtr^.bshSignature := ffc_SigBLOBSegContent;
          TempSegPtr^.bshParentBLOB := aBLOBNr;

          { Preserve the existing data in the old content segment. }
          if LookupEntPtr^.bleContentLength > 0 then begin
            assert(LookupEntPtr^.bleContentLength = EndBytesUsed);
            Move(ContentSegBlk^[OffsetInBlock + ffc_BLOBSegmentHeaderSize],
                 TempSegBlk^[TempOfsInBlk + ffc_BLOBSegmentHeaderSize],
                 EndBytesUsed);
            { Decrement EndBytesUsed. }
            dec(EndBytesUsed, LookupEntPtr^.bleContentLength);
          end;
          SegBytesLeft := BytesToCopy - StartBytesUsed;
          TargetOffset := TempOfsInBlk + ffc_BLOBSegmentHeaderSize +
                          StartBytesUsed;
          { Change the previous content segment's NextSegment field. }
          if Assigned(PrevContentSegPtr) then
            PrevContentSegPtr^.bshNextSegment := TempSegOfs;
          aFI^.fiBLOBrscMgr.DeleteSegment(aFI, aTI,
                                          LookupEntPtr^.bleSegmentOffset);
          LookupEntPtr^.bleSegmentOffset := TempSegOfs;
          OffsetInBlock := TempOfsInBlk;
          ContentSegBlk := TempSegBlk;
          ContentSegPtr := TempSegPtr;
        end;

        { Figure out how many bytes to copy. }
        BytesToCopy := ffMinL(SegBytesLeft, BytesToGo);

        { Copy. }
        Move(BLOBAsBytes^[SrcOffset], ContentSegBlk^[TargetOffset], BytesToCopy);
        dec(BytesToGo, BytesToCopy);
        inc(SrcOffset, BytesToCopy);
        { Update the content length of the lookup entry. We have several cases
          to account for:
          1. Write X bytes to empty segment. Length = X.
          2. Suffix X bytes to end of segment containing Y bytes.
             Length = X + Y.
          3. Write X bytes to segment containing Y bytes where X <= Y and
             (aOffset + X) <= Y.  Length = Y.
          4. Write X bytes to segment containing Y bytes where X <= Y and
             (aOffset + X) > Y. Length = # untouched bytes + Y.

          These cases are all handled by the following IF statement.
        }
        if StartBytesUsed + BytesToCopy > LookupEntPtr^.bleContentLength then
          LookupEntPtr^.bleContentLength := StartBytesUsed + BytesToCopy
      end;

      inc(SegEntNumber);
      inc(SegInx);
      PrevContentSegPtr := ContentSegPtr;

      { If we're not done, we may need to move to the next lookup header. }
      if BytesToGo <> 0 then begin
        if SegEntNumber = EntryCount then begin
          { We filled all the segments in this segment, move to next one &
            reset SegInx}
          LookupSegBlk := ReadVfyBlobBlock(aFI, aTI, ffc_MarkDirty,
                                           LookupSegPtr^.bshNextSegment,
                                           LookupSegOfs, aRelMethod);
          aRelList.Append(FFAllocReleaseInfo(LookupSegBlk, TffInt64(aRelMethod)));
          LookupSegPtr := @LookupSegBlk^[LookupSegOfs];
          EntryCount := FFCalcMaxLookupEntries(LookupSegPtr);
          LookupEntOfs := LookupSegOfs + sizeof(TffBLOBSegmentHeader);
          LookupEntPtr := PffBLOBLookupEntry(@LookupSegBlk^[LookupEntOfs]);
          SegEntNumber := 0;
        end else begin
          LookupEntOfs := LookupEntOfs + sizeof(TffBLOBLookupEntry);
          LookupEntPtr := @LookupSegBlk^[LookupEntOfs];
        end;
      end;
    end; {while}

    if (EndSegInx >= OldEndSeg) then begin                            
      { If newly-sized BLOB extends past old BLOB then add new segments. }
      BLOBHeader^.bbhSegCount := OldEndSeg;
      while BytesToGo > 0 do begin

        { Figure out how many bytes to copy. }
        BytesToCopy := ffMinL(BytesToGo, aFI^.fiMaxSegSize - ffc_BLOBSegmentHeaderSize);

        { Get a new content segment}
        SegSize := BytesToCopy + ffc_BLOBSegmentHeaderSize;
        ContentSegOfs := aFI^.fiBLOBrscMgr.NewSegment(aFI, aTI, SegSize, SegSize);

        { If exist, update prev segment to point to this new segment. }
        if Assigned(ContentSegPtr) then begin
          PrevContentSegPtr := ContentSegPtr;
          PrevContentSegPtr^.bshNextSegment := ContentSegOfs;
        end;

        { Increment the segment count & read in the new content segment. }
        inc(BLOBHeader^.bbhSegCount);
        ContentSegBlk := ReadVfyBlobBlock(aFI, aTI, ffc_MarkDirty,
                                          ContentSegOfs, OffsetInBlock,
                                          aRelMethod);
        aRelList.Append(FFAllocReleaseInfo(ContentSegBlk, TffInt64(aRelMethod)));
        ContentSegPtr := @ContentSegBlk^[OffsetInBlock];
        ContentSegPtr^.bshSignature := ffc_sigBLOBSegContent;
        ContentSegPtr^.bshParentBLOB := aBLOBNr;
        ContentSegPtr^.bshNextSegment.iLow := ffc_W32NoValue;

        { Get a new lookup entry. }
        LookupEntOfs := LookupSegOfs + sizeof(TffBLOBSegmentHeader) +
                        (SegEntNumber * sizeof(TffBLOBLookupEntry));
        LookupEntPtr := PffBLOBLookupEntry(@LookupSegBlk^[LookupEntOfs]);
        LookupEntPtr^.bleSegmentOffset := ContentSegOfs;
        LookupEntPtr^.bleContentLength := BytesToCopy;

        { Fill the content segment. }
        Move(BLOBAsBytes^[SrcOffset],
             ContentSegBlk^[OffsetInBlock + sizeof(TffBLOBSegmentHeader)],
             BytesToCopy);
        inc(SrcOffset, BytesToCopy);
        dec(BytesToGo, BytesToCopy);
        inc(SegEntNumber);

        { If we're not done, we may need to move to the next lookup segment. }
        if  BytesToGo <> 0 then begin
          if SegEntNumber = EntryCount then begin
            { We filled all the segments in this segment, move to next one & reset
              SegEntNumber. }
            LookupSegBlk := ReadVfyBlobBlock(aFI, aTI, ffc_MarkDirty,
                                             LookupSegPtr^.bshNextSegment,
                                             LookupSegOfs, aRelMethod);
            aRelList.Append(FFAllocReleaseInfo(LookupSegBlk, TffInt64(aRelMethod)));
            LookupSegPtr := @LookupSegBlk^[LookupSegOfs];
            EntryCount := FFCalcMaxLookupEntries(LookupSegPtr);
            OffsetInBlock := LookupSegOfs + sizeof(TffBLOBSegmentHeader);
            SegEntNumber := 0;
          end; {if}
        end;
      end; {while}
    end; {if}

    { If the BLOB has grown, we need to update its length.
      NOTE: BLOBs can't be truncated via a write operation. }
    if (NewSizeW32 > BLOBHeader^.bbhBLOBLength) then
      BLOBHeader^.bbhBLOBLength := NewSizeW32;                        
  finally
    for OffsetInBlock := 0 to pred(aRelList.Count) do
      FFDeallocReleaseInfo(aRelList[OffsetInBlock]);
    aRelList.Free;
  end;
end;
{====================================================================}
initialization
  FFBLOBEngine := TffBLOBEngine.Create;
  FF210BLOBEngine := Tff210BLOBEngine.Create;

  {$IFDEF BLOBTrace}
  btLog := TffEventLog.Create(nil);
  btLog.FileName := 'BLOBTrace.log';
  btLog.Enabled := True;
  {$ENDIF}

finalization
  FFBLOBEngine.Free;
  FF210BLOBEngine.Free;

  {$IFDEF BLOBTrace}
  btLog.Flush;
  btLog.Free;
  {$ENDIF}

{End !!.11}
end.
