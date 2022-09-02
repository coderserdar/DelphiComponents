{*********************************************************}
{* FSSQL: Table BLOB access                         *}
{*********************************************************}

{$I fsdefine.inc}

{!!.11 - Added logging}
{ Uncomment the following define to enable BLOB tracing. }
{.$DEFINE BLOBTrace}

Unit fsblobaccess;

Interface

Uses
  Classes, {!!.03}
  Windows,
  SysUtils,
  fsconst,
  fsllbase,
  fssrmgr,
  fsllexcp,
  fssrbase,
  fssrlock,
  fsfile,
  fstablehelper;

{---BLOB Link method types---}
Type
  TffBLOBLinkGetLength = Function(Const aTableName: TfsTableName;
    Const aBLOBNr: TffInt64;
    Var aLength: Longint): TffResult Of Object;
  { Declaration of method to be called when trying to find the length
    of a BLOB visible through a BLOB link. }

  TffBLOBLinkRead = Function(Const aTableName: TfsTableName;
    Const aBLOBNr: TffInt64;
    Const aOffset: TffWord32; {!!.06}
    Const aLen: TffWord32; {!!.06}
    Var aBLOB;
    Var aBytesRead: TffWord32) {!!.06}
  : TffResult Of Object;
  { Declaration of a method to be called when trying to read a BLOB visible
    through a BLOB link. }

{---BLOB maintenance---}
Procedure FFTblAddBLOB(aFI: PffFileInfo;
  aTI: PffTransInfo;
  Var aBLOBNr: TffInt64);
{-add a new, empty (length 0) BLOB, return new BLOB number}

Procedure FFTblAddBLOBLink(aFI: PffFileInfo;
  aTI: PffTransInfo;
  Const aTableName: TfsTableName;
  Const aTableBLOBNr: TffInt64;
  Var aBLOBNr: TffInt64);
{-Add a new BLOB link, return new BLOB number. }

Procedure FFTblAddFileBLOB(aFI: PffFileInfo;
  aTI: PffTransInfo;
  Const aFileName: TffFullFileName;
  Var aBLOBNr: TffInt64);
{-add a new file BLOB, return new BLOB number}

Procedure FFTblDeleteBLOB(aFI: PffFileInfo;
  aTI: PffTransInfo;
  Const aBLOBNr: TffInt64);
{-delete a BLOB; BLOB number will no longer be valid after this}

Function FFTblFreeBLOB(aFI: PffFileInfo;
  aTI: PffTransInfo;
  aBLOBNr: TffInt64): boolean;
{-if the BLOB length is zero, delete it; return true if deleted}

Function FFTblGetBLOBLength(aFI: PffFileInfo;
  aTI: PffTransInfo;
  aBLOBNr: TffInt64;
  aLengthMethod: TffBLOBLinkGetLength;
  Var aFBError: TffResult): Longint;
{-return the length of the BLOB}

Function FFTblIsDeletedBlob(aFI: PffFileInfo;
  aTI: PffTransInfo;
  aBLOBNr: TffInt64;
  Var aFBError: TffResult)
  : Boolean;

Function FFTblGetFileNameBLOB(aFI: PffFileInfo;
  aTI: PffTransInfo;
  aBLOBNr: TffInt64;
  Var aFileName: TffFullFileName): Boolean;
{-return True if the given BLOB nr refers to a file BLOB, and the
  filename is returned in aFileName}

Function FFTblIsBLOBLink(aFI: PffFileInfo; {!!.11 - New}
  aTI: PffTransInfo;
  aBLOBNr: TffInt64;
  Var aSrcTableName: TfsTableName;
  Var aSrcTableBLOBNr: TffInt64)
  : Boolean;
{ Checks to see if aBLOBNr is a BLOB Link. If it is, it returns the
  the offset of the source as aSrcTableBLOBNr in aSrcTableName

{Begin !!.03}
Procedure FFTblListBLOBSegments(aFI: PffFileInfo;
  aTI: PffTransInfo;
  aBLOBNr: TffInt64;
  aStream: TStream);
{ List the segments comprising the BLOB. }
{End !!.03}

{Begin !!.11}
Type
  TffBaseBLOBEngine = Class; { foward declaration }
  TffBLOBEngineClass = Class Of TffBaseBLOBEngine;

  TffBaseBLOBEngine = Class(TFSSpecObject)
    { Base class representing an engine to read, write, & truncate BLOBs. }
  Public
    Class Function GetEngine(aFI: PffFileInfo): TffBaseBLOBEngine;
    { Returns the engine instance to be used for the specified file. }

    Procedure Read(aFI: PffFileInfo;
      aTI: PffTransInfo;
      aBLOBNr: TffInt64;
      aOffset: TffWord32;
      aLen: TffWord32;
      aReadMethod: TffBLOBLinkRead;
      Var aBLOB;
      Var aBytesRead: TffWord32;
      Var aFBError: TffResult); Virtual; Abstract;
    { Read all or part of a BLOB}

    Procedure Truncate(aFI: PffFileInfo;
      aTI: PffTransInfo;
      aBLOBNr: TffInt64;
      aLen: TffWord32); Virtual; Abstract;
    { Truncate the BLOB to the specified length. Does *not* delete BLOB if
      length 0. }

    Procedure Write(aFI: PffFileInfo;
      aTI: PffTransInfo;
      Const aBLOBNr: TffInt64;
      aOffset: TffWord32;
      aLen: TffWord32;
      Const aBLOB); Virtual; Abstract;
    { Write to or append to a BLOB. }
  End;

  TffBLOBEngine = Class(TffBaseBLOBEngine)
    { This class provides an interface to BLOBs in 2.1.0.1 and later. The logic
      supports the improved nesting algorithm that recycles all available
      BLOB segments regardless of size. }
{Begin !!.12}
  Protected
    Function IsEmptyLookupEntry(Entry: PffBLOBLookupEntry): Boolean;
    {End !!.12}
  Public
    Procedure Read(aFI: PffFileInfo;
      aTI: PffTransInfo;
      aBLOBNr: TffInt64;
      aOffset: TffWord32;
      aLen: TffWord32;
      aReadMethod: TffBLOBLinkRead;
      Var aBLOB;
      Var aBytesRead: TffWord32;
      Var aFBError: TffResult); Override;
    { Read all or part of a BLOB}

    Procedure Truncate(aFI: PffFileInfo;
      aTI: PffTransInfo;
      aBLOBNr: TffInt64;
      aLen: TffWord32); Override;
    { Truncate the BLOB to the specified length. Does *not* delete BLOB if
      length 0. }

    Procedure Write(aFI: PffFileInfo;
      aTI: PffTransInfo;
      Const aBLOBNr: TffInt64;
      aOffset: TffWord32;
      aLen: TffWord32;
      Const aBLOB); Override;
    { Write to or append to a BLOB. }
  End;

  {End !!.11}

Implementation

Uses
  fslllog, {!!.13}
  fssrbde,
  fssrblob;

Resourcestring
  ffcBLOBSegExpected = ' Expected %s segment but segment marked with ''%s''.';
  ffcBLOBSegHeader = 'header';

Const
  fsc_FileBLOB = -1;
  fsc_BLOBLink = -2;

  {Begin !!.11}
Var
  FFBLOBEngine: TffBLOBEngine;
  {End !!.11}

  {Begin !!.13}
  {$IFDEF BLOBTrace}
Var
  btLog: TffEventLog;

Procedure Logbt(aMsg: String; args: Array Of Const);
Begin
  If btLog <> Nil Then
    btLog.WriteStringFmt(aMsg, args);
End;
{$ENDIF}
{End !!.13}

{== Calculation routines =============================================}

Function EstimateSegmentCount(Const aBLOBSize, aMaxSegSize: Integer)
  : Integer;
Begin
  Result := ((aBLOBSize * 2) Div aMaxSegSize) + 1;
End;

{Begin !!.11}

Function CalcBLOBSegNumber(Const aOffset: TffWord32;
  Const aBlockSize: TffWord32;
  Var aOfsInSeg: TffWord32): TffWord32;
{-Calculate the segment number for an offset into a BLOB.}
{-aOfsInSeg tells us how much of the last segment (result)
  we're using}
Var
  MaxSegSize: TffWord32;
Begin
  {offset 0 is in the 1st segment}
  If aOffset = 0 Then
    Begin
      Result := 1;
      aOfsInSeg := 0;
    End
  Else
    Begin
      MaxSegSize := (((aBlockSize - fsc_BlockHeaderSizeBLOB)
        Div fsc_BLOBSegmentIncrement) * fsc_BLOBSegmentIncrement) -
        sizeof(TffBLOBSegmentHeader);
      aOfsInSeg := 0;

      Result := aOffset Div MaxSegSize;
      aOfsInSeg := aOffset - (Result * MaxSegSize);
      If aOfsInSeg > 0 Then
        inc(Result)
      Else If (aOfsInSeg = 0) And
        (aOffset <> 0) Then
        aOfsInSeg := MaxSegSize;
    End; {if..else}
End;
{=====================================================================}

{== BLOB link routines ===============================================}

Function BLOBLinkGetLength(aBLOBHeader: PffBLOBHeader;
  aGetLengthMethod: TffBLOBLinkGetLength;
  Var aLength: Longint)
  : TffResult;
Var
  BLOBData: PffByteArray Absolute aBLOBHeader;
  BLOBNr: TffInt64;
  TableName: TffFullFileName;
  TableNameLen: Byte;
Begin
  { Get the length of the table name. }
  Move(BLOBData^[sizeof(TffBLOBHeader)], TableNameLen, sizeOf(TableNameLen));
  Inc(TableNameLen);
  { Copy the file name to TableName. }
  Move(BLOBData^[sizeof(TffBLOBHeader)], TableName, TableNameLen);
  { Get the table's BLOB number. }
  Move(BLOBData^[SizeOf(TffBLOBHeader) + TableNameLen], BlobNr,
    SizeOf(TffInt64));

  Result := aGetLengthMethod(TableName, BlobNr, aLength);

End;
{--------}

Procedure BLOBLinkGetTableNameAndRefNr(aBLOBBlock: PffBlock; {!!.11 - New}
  aBlockOffset: Integer;
  Var aTableName: TfsTableName;
  Var aBLOBNr: TffInt64);
Var
  TableNameLen: Byte;
Begin
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
End;
{--------}

Function BLOBLinkRead(aFI: PffFileInfo;
  aTI: PffTransInfo;
  aBLOBNr: TffInt64;
  aOffset: Longint;
  aLen: Longint;
  aReadMethod: TffBLOBLinkRead;
  Var aBLOB;
  Var aBytesRead: TffWord32) {!!.06}
: TffResult;
Var
  BLOBBlock: PffBlock;
  BLOBNr: TffInt64;
  OffsetInBlock: TffWord32; {!!.11}
  {TableNameLen  : Byte;}{!!.11}
  TableName: TfsTableName;
  aFHRelMethod: TffReleaseMethod;
Begin
  BLOBBlock := ReadVfyBlobBlock(aFI,
    aTI,
    fsc_ReadOnly,
    aBLOBNr,
    OffsetInBlock,
    aFHRelMethod);
  Try
    BLOBLinkGetTableNameAndRefNr(BLOBBlock, {!!.11}
      OffsetInBlock,
      TableName,
      BLOBNr);
    Result := aReadMethod(TableName,
      BlobNr,
      aOffset,
      aLen,
      aBLOB,
      aBytesRead);
  Finally
    aFHRelMethod(BLOBBlock);
  End;
End;
{=====================================================================}

{== File BLOB routines ===============================================}

Function FileBLOBLength(aBLOBHeader: PffBLOBHeader;
  Var aLength: Longint)
  : TffResult;
Var
  BLOBFile: PffFileInfo;
  BLOBData: PffByteArray Absolute aBLOBHeader;
  FileName: TffFullFileName;
  FileNameLen: Byte;
  TmpLen: TffInt64;
Begin
  Result := 0;

  {Get the length of the file name}
  Move(BLOBData^[sizeof(TffBLOBHeader)], FileNameLen, sizeOf(FileNameLen));
  {copy the file name to FileName}
  Move(BLOBData^[sizeof(TffBLOBHeader)], FileName, succ(FileNameLen));

  Try
    BLOBFile := FFAllocFileInfo(FileName, FFExtractExtension(FileName), Nil);
    Try
      FFOpenFile(BLOBFile, omReadOnly, smShared, False, False);
      Try
        TmpLen := FFPositionFileEOF(BLOBFile);
        aLength := TmpLen.iLow;
      Finally
        FFCloseFile(BLOBFile);
      End; {try..finally}
    Finally
      FFFreeFileInfo(BLOBFile);
    End; {try..finally}
  Except
    On E: EfsException Do
      Begin
        Case E.ErrorCode Of
          fserrOpenFailed: Result := DBIERR_FS_FileBLOBOpen;
          fserrCloseFailed: Result := DBIERR_FS_FileBLOBClose;
          fserrReadFailed: Result := DBIERR_FS_FileBLOBRead;
          fserrSeekFailed: Result := DBIERR_FS_FileBLOBRead;
          Else
            Raise
        End; {case}
      End;
  End; {try..except}
End;
{--------}

Function FileBLOBRead(aFI: PffFileInfo;
  aTI: PffTransInfo;
  aBLOBNr: TffInt64;
  aOffset: Longint;
  aLen: Longint;
  Var aBLOB;
  Var aBytesRead: TffWord32) {!!.06}
: TffResult;
Var
  BLOBFile: PffFileInfo;
  BLOBBlock: PffBlock;
  OffsetInBlock: TffWord32; {!!.11}
  FileNameLen: Byte;
  FileName: TffFullFileName;
  TempI64: TffInt64;
  aFHRelMethod: TffReleaseMethod;
Begin
  Result := 0;
  BLOBBlock := ReadVfyBlobBlock(aFI,
    aTI,
    fsc_ReadOnly,
    aBLOBNr,
    OffsetInBlock,
    aFHRelMethod);
  Try
    {Get the length of the file name}
    Move(BLOBBlock^[(OffsetInBlock + sizeof(TffBLOBHeader))],
      FileNameLen, sizeOf(FileNameLen));
    {copy the file name to FileName}
    Move(BLOBBlock^[(OffsetInBlock + sizeof(TffBLOBHeader))],
      FileName, succ(FileNameLen));
    Try
      BLOBFile := FFAllocFileInfo(FileName, FFExtractExtension(FileName), Nil);
      Try
        FFOpenFile(BLOBFile, omReadOnly, smShared, False, False);
        Try
          TempI64.iLow := aOffset;
          TempI64.iHigh := 0;
          FFPositionFile(BLOBFile, TempI64);
          aBytesRead := FFReadFile(BLOBFile, aLen, aBLOB);
        Finally
          FFCloseFile(BLOBFile);
        End; {try..finally}
      Finally
        FFFreeFileInfo(BLOBFile);
      End; {try..finally}
    Except
      On E: EfsException Do
        Begin
          Case E.ErrorCode Of
            fserrOpenFailed: Result := DBIERR_FS_FileBLOBOpen;
            fserrCloseFailed: Result := DBIERR_FS_FileBLOBClose;
            fserrReadFailed: Result := DBIERR_FS_FileBLOBRead;
            fserrSeekFailed: Result := DBIERR_FS_FileBLOBRead;
            Else
              Raise
          End; {case}
        End;
    End; {try..except}
  Finally
    aFHRelMethod(BLOBBlock);
  End;
End;
{=====================================================================}

{== BLOB maintenance =================================================}

Procedure FFTblAddBLOB(aFI: PffFileInfo;
  aTI: PffTransInfo;
  Var aBLOBNr: TffInt64);
Var
  FileHeader: PffBlockHeaderFile;
  BLOBHeaderPtr: PffBLOBHeader;
  BLOBBlock: PffBlock;
  SegSize: TffWord32; {!!.11}
  OffsetInBlock: TffWord32; {!!.11}
  aBlkRelMethod,
    aFHRelMethod: TffReleaseMethod;
Begin
  {$IFDEF BLOBTrace} {!!.11}
  Logbt('FFTblAddBLOB.Begin', []);
  {$ENDIF}
  { First get the file header, block 0. }
  FileHeader := PffBlockHeaderFile(FFBMGetBlock(aFI,
    aTI,
    0,
    fsc_MarkDirty,
    aFHRelMethod, fsoNone));
  Try
    { Create a new BLOB header. }
    SegSize := fsc_BLOBHeaderSize; {!!.11}
    aBLOBNr := aFI^.fiBLOBrscMgr.NewSegment(aFI,
      aTI,
      SegSize, {!!.11}
      SegSize); {!!.11}
    BLOBBlock := ReadVfyBlobBlock(aFI,
      aTI,
      fsc_MarkDirty,
      aBLOBNr,
      OffsetInBlock,
      aBlkRelMethod);
    Try
      BLOBHeaderPtr := @BLOBBlock^[OffsetInBlock];
      {set up the new BLOB header}
      With BLOBHeaderPtr^ Do
        Begin
          bbhSignature := fsc_SigBLOBSegHeader;
          bbhSegmentLen := (((sizeof(TffBLOBHeader) + pred(fsc_BLOBSegmentIncrement)) Div
            fsc_BLOBSegmentIncrement) * fsc_BLOBSegmentIncrement);
          bbhBLOBLength := 0;
          bbhSegCount := 0;
          bbh1stLookupSeg.iLow := fsc_W32NoValue; {!!.11}
        End;
      {we've got one more BLOB}
      inc(FileHeader^.bhfBLOBCount);
    Finally
      aBlkRelMethod(BLOBBlock);
    End;
  Finally
    aFHRelMethod(PffBlock(FileHeader));
  End;
End;
{--------}

Procedure FFTblAddBLOBLink(aFI: PffFileInfo;
  aTI: PffTransInfo;
  Const aTableName: TfsTableName;
  Const aTableBLOBNr: TffInt64;
  Var aBLOBNr: TffInt64);
Var
  FileHeader: PffBlockHeaderFile;
  BLOBBlock: PffBlock;
  BLOBBlockHdr: PffBlockHeaderBLOB Absolute BLOBBlock;
  SegSize: TffWord32; {!!.11}
  OffsetInBlock: TffWord32; {!!.11}
  BLOBHeaderPtr: PffBLOBHeader;
  LinkLen,
    NameLen: TffWord32; {!!.11}
  aBlkRelMethod,
    aFHRelMethod: TffReleaseMethod;
Begin
  { First get the file header, block 0. }
  FileHeader := PffBlockHeaderFile(FFBMGetBlock(aFI,
    aTI,
    0,
    fsc_MarkDirty,
    aFHRelMethod, fsoNone));
  Try
    { Create a new BLOB header. }
    NameLen := succ(Length(aTableName));
    LinkLen := succ(Length(aTableName) + SizeOf(aTableBLOBNr));
    SegSize := fsc_BLOBHeaderSize + LinkLen; {!!.11}
    aBLOBNr := aFI^.fiBLOBrscMgr.NewSegment(aFI,
      aTI,
      SegSize, {!!.11}
      SegSize); {!!.11}
    If (aBLOBNr.iLow <> fsc_W32NoValue) Then
      Begin
        BLOBBlock := ReadVfyBlobBlock(aFI,
          aTI,
          fsc_MarkDirty,
          aBLOBNr,
          OffsetInBlock,
          aBlkRelMethod);
        BLOBHeaderPtr := @BLOBBlock^[OffsetInBlock];
      End
    Else
      Begin
        aBLOBNr.iLow := fsc_W32NoValue;
        Exit;
      End;
    { Set up the new BLOB header. }
    With BLOBHeaderPtr^ Do
      Begin
        bbhSignature := fsc_SigBLOBSegHeader;
        bbhBLOBLength := 0;
        bbhSegCount := fsc_BLOBLink;
        bbh1stLookupSeg.iLow := fsc_W32NoValue;
      End;
    { Write aTableName & the table's BLOB number after BLOBHeader.  Note that
      length of string is automatically stored as the first byte of the string. }
    Move(aTableName, BLOBBlock^[(OffsetInBlock + sizeof(TffBLOBHeader))],
      NameLen);
    Move(aTableBLOBNr, BLOBBlock^[(OffsetInBlock + SizeOf(TffBLOBHeader) +
        NameLen)], SizeOf(TffInt64));
    { We've got one more BLOB. }
    inc(FileHeader.bhfBLOBCount);
    aBlkRelMethod(BLOBBlock);
  Finally
    aFHRelMethod(PffBlock(FileHeader));
  End;
End;
{--------}

Procedure FFTblAddFileBLOB(aFI: PffFileInfo;
  aTI: PffTransInfo;
  Const aFileName: TffFullFileName;
  Var aBLOBNr: TffInt64);
Var
  FileHeader: PffBlockHeaderFile;
  BLOBBlock: PffBlock;
  BLOBBlockHdr: PffBlockHeaderBLOB Absolute BLOBBlock;
  SegSize: TffWord32; {!!.11}
  OffsetInBlock: TffWord32; {!!.11}
  BLOBHeaderPtr: PffBLOBHeader;
  FileNameLen: Integer;
  aBlkRelMethod,
    aFHRelMethod: TffReleaseMethod;
Begin
  {first get the file header, block 0}
  FileHeader := PffBlockHeaderFile(FFBMGetBlock(aFI,
    aTI,
    0,
    fsc_MarkDirty,
    aFHRelMethod, fsoNone));
  Try
    {create a new BLOB header}
    FileNameLen := succ(Length(aFileName));
    SegSize := fsc_BLOBHeaderSize + FileNameLen; {!!.11}
    aBLOBNr := aFI^.fiBLOBrscMgr.NewSegment(aFI,
      aTI,
      SegSize, {!!.11}
      SegSize); {!!.11}
    If (aBLOBNr.iLow <> fsc_W32NoValue) Then
      Begin
        BLOBBlock := ReadVfyBlobBlock(aFI,
          aTI,
          fsc_MarkDirty,
          aBLOBNr,
          OffsetInBlock,
          aBlkRelMethod);
        BLOBHeaderPtr := @BLOBBlock^[OffsetInBlock];
      End
    Else
      Begin
        aBLOBNr.iLow := fsc_W32NoValue;
        Exit;
      End;
    {set up the new BLOB header}
    With BLOBHeaderPtr^ Do
      Begin
        bbhSignature := fsc_SigBLOBSegHeader;
        bbhBLOBLength := 0;
        bbhSegCount := fsc_FileBLOB;
        bbh1stLookupSeg.iLow := fsc_W32NoValue;
      End;
    { Write aFileName after BLOBHeader.  Note that length of string is
      automatically stored as the first byte of the string. }
    Move(aFileName, BLOBBlock^[(OffsetInBlock + sizeof(TffBLOBHeader))], FileNameLen);
    {we've got one more BLOB}
    inc(FileHeader.bhfBLOBCount);
    aBlkRelMethod(BLOBBlock);
  Finally
    aFHRelMethod(PffBlock(FileHeader));
  End;
End;
{--------}

Procedure FFTblDeleteBLOBPrim(aFI: PffFileInfo;
  aTI: PffTransInfo;
  BLOBHeader: PffBLOBHeader);
Var
  OffsetInBlock: TffWord32; {!!.11}
  LookupSegBlk: PffBlock;
  LookupSegOfs, {!!.03}
  TmpSegOfs: TffInt64; {!!.03}
  LookupSegPtr: PffBLOBSegmentHeader;
  LookupEntOfs: Integer;
  LookupEntPtr: PffBLOBLookupEntry;
  EntryCount, {!!.03}
  RemainEntries: Integer; {!!.03}
  i: Integer;
  aRelMethod: TffReleaseMethod;
Begin
  {$IFDEF BLOBTrace} {!!.11}
  Logbt('FFTblDeleteBLOBPrim.Begin', []);
  {$ENDIF}

  { Assumption: File header block is exclusively locked. }

  { Get the BLOB's first lookup segment. }
  LookupSegOfs := BLOBHeader^.bbh1stLookupSeg;

  {Begin !!.03}
    { BLOB truncated to length 0? }
  If LookupSegOfs.iLow = fsc_W32NoValue Then
    Exit;
  {End !!.03}

  LookupSegBlk := ReadVfyBlobBlock(aFI,
    aTI,
    fsc_MarkDirty,
    LookupSegOfs,
    OffsetInBlock,
    aRelMethod);
  LookupSegPtr := @LookupSegBlk^[OffsetInBlock];
  LookupEntOfs := OffsetInBlock + sizeof(TffBLOBSegmentHeader);

  Try
    { Get the first lookup entry in the lookup segment. }
    LookupEntPtr := @LookupSegBlk^[LookupEntOfs];

    { Is this the only lookup segment? }
    If LookupSegPtr^.bshNextSegment.iLow <> fsc_W32NoValue Then
      { No.  Figure out number of lookup entries based on segment size. }
      EntryCount := FFCalcMaxLookupEntries(LookupSegPtr)
    Else
      { Yes.  Number of lookup entries = number of content segments. }
      EntryCount := BLOBHeader^.bbhSegCount;

    RemainEntries := BLOBHeader^.bbhSegCount; {!!.03}

    { Free each content segment. }
    dec(RemainEntries, EntryCount); {!!.03}
    For i := 1 To BLOBHeader^.bbhSegCount Do
      Begin
        aFI^.fiBLOBrscMgr.DeleteSegment(aFI, aTI, LookupEntPtr^.bleSegmentOffset);
        dec(EntryCount);

        { Need to move to another lookup segment? }
        If ((EntryCount = 0) And (LookupSegPtr^.bshNextSegment.iLow <> fsc_W32NoValue)) Then
          Begin
            {Yes.  Get the location of the next lookup segment and delete the
             existing lookup segment. }
            TmpSegOfs := LookupSegPtr^.bshNextSegment; {!!.03}
            aFI^.fiBLOBrscMgr.DeleteSegment(aFI, aTI, LookupSegOfs);
            LookupSegOfs := TmpSegOfs; {!!.03}

            { Grab the next lookup segment. }
            aRelMethod(LookupSegBlk);
            LookupSegBlk := ReadVfyBlobBlock(aFI, aTI, fsc_MarkDirty,
              LookupSegOfs, OffsetInBlock,
              aRelMethod);
            LookupSegPtr := @LookupSegBlk^[OffsetInBlock];
            LookupEntOfs := OffsetInBlock + sizeof(TffBLOBSegmentHeader);
            EntryCount := FFCalcMaxLookupEntries(LookupSegPtr);
            {Begin !!.03}
            If RemainEntries > EntryCount Then
              dec(RemainEntries, EntryCount)
            Else
              Begin
                EntryCount := RemainEntries;
                RemainEntries := 0;
              End;
          End
        Else
          { Grab the next lookup entry. }
          LookupEntOfs := LookupEntOfs + sizeof(TffBLOBLookupEntry);

        LookupEntPtr := @LookupSegBlk^[LookupEntOfs];
        {End !!.03}
      End; {for}

    { Delete the last lookup segment.}
    aFI^.fiBLOBrscMgr.DeleteSegment(aFI, aTI, LookupSegOfs);
  Finally
    aRelMethod(LookupSegBlk);
  End;
End;
{--------}

Procedure FFTblDeleteBLOB(aFI: PffFileInfo;
  aTI: PffTransInfo;
  Const aBLOBNr: TffInt64);
Var
  FileHeader: PffBlockHeaderFile;
  BLOBBlock: PffBlock;
  BLOBBlockHdr: PffBlockHeaderBLOB Absolute BLOBBlock;
  BLOBHeader: PffBLOBHeader;
  OffsetInBlock: TffWord32; {!!.11}
  aBlkRelMethod,
    aFHRelMethod: TffReleaseMethod;
Begin
  {$IFDEF BLOBTrace} {!!.11}
  Logbt('FFTblDeleteBLOB.Begin', []);
  {$ENDIF}
  {first get the file header, block 0}
  FileHeader := PffBlockHeaderFile(FFBMGetBlock(aFI, aTI, 0, fsc_MarkDirty,
    aFHRelMethod, fsoNone));
  Try
    {read and verify the BLOB header block}
    BLOBBlock := ReadVfyBlobBlock(aFI,
      aTI,
      fsc_MarkDirty,
      aBLOBNr,
      OffsetInBlock,
      aBlkRelMethod);
    BLOBHeader := @BLOBBlock^[OffsetInBlock];

    { Verify the BLOB has not been deleted. }
    If (BLOBHeader^.bbhSignature = fsc_SigBLOBSegDeleted) Then
      FSRaiseException(EfsServerException, fsStrResServer,
        fserrBLOBDeleted,
        [aFI^.fiName^, aBLOBNr.iHigh, aBLOBNr.iLow]);

    Try
      FFTblDeleteBLOBPrim(aFI, aTI, BLOBHeader);

      { Delete the BLOB header}
      aFI^.fiBLOBrscMgr.DeleteSegment(aFI, aTI, aBLOBNr);

      { We've got one less BLOB. }
      dec(FileHeader.bhfBLOBCount);
    Finally
      aBlkRelMethod(BLOBBlock);
    End;
  Finally
    aFHRelMethod(PffBlock(FileHeader));
  End;
End;
{--------}

Function FFTblFreeBLOB(aFI: PffFileInfo;
  aTI: PffTransInfo;
  aBLOBNr: TffInt64)
  : Boolean;
Var
  BLOBBlock: PffBlock;
  BLOBBlockHdr: PffBlockHeaderBLOB Absolute BLOBBlock;
  BLOBBlockNum: TffWord32;
  BLOBHeader: PffBLOBHeader;
  FileHeader: PffBlockHeaderFile;
  OffsetInBlock: TffWord32; {!!.11}
  TempI64: TffInt64;
  aBlkRelMethod,
    aFHRelMethod: TffReleaseMethod;
Begin
  {$IFDEF BLOBTrace} {!!.11}
  Logbt('FFTblFreeBLOB.Begin', []);
  Logbt('  aBLOBNr = %d:%d', [aBLOBNr.iLow, aBLOBNr.iHigh]);
  {$ENDIF}
  { Assume we won't delete. }
  Result := False;
  FileHeader := Nil;

  {now get the BLOB block}
  ffShiftI64R(aBLOBNr, aFI^.fiLog2BlockSize, TempI64);
  BLOBBlockNum := TempI64.iLow;

  { Read and verify the BLOB header block. }
  BLOBBlock := ReadVfyBlobBlock2(aFI,
    aTI,
    fsc_ReadOnly,
    aBLOBNr,
    BLOBBlockNum,
    OffsetInBlock,
    aBlkRelMethod);
  BLOBHeader := @BLOBBlock^[OffsetInBlock];
  {Begin !!.01}
    { Verify the BLOB has not been deleted. }
  If (BLOBHeader^.bbhSignature = fsc_SigBLOBSegDeleted) Then
    FSRaiseException(EfsServerException, fsStrResServer,
      fserrBLOBDeleted,
      [aFI^.fiName^, aBLOBNr.iHigh, aBLOBNr.iLow]);
  {End !!.01}

  Try
    {don't bother doing anything if the BLOB's length > 0}
    If (BLOBHeader^.bbhBLOBLength > 0) Then
      Exit;

    { We don't need to obtain exclusive locks on file header page or BLOB page
      because the BLOB resource manager's DeleteSegment routine will do so. }

    { Delete the BLOB's header. }
    aFI^.fiBLOBrscMgr.DeleteSegment(aFI, aTI, aBLOBNr);

    { One less BLOB. }
    FileHeader := PffBlockHeaderFile(FFBMGetBlock(aFI, aTI, 0, fsc_MarkDirty,
      aFHRelMethod, fsoNone));
    dec(FileHeader.bhfBLOBCount);

    { We did delete. }
    Result := True;
  Finally
    If assigned(FileHeader) Then
      aFHRelMethod(PffBlock(FileHeader));
    aBlkRelMethod(BLOBBlock);
  End;
End;
{--------}

Function FFTblGetBLOBLength(aFI: PffFileInfo;
  aTI: PffTransInfo;
  aBLOBNr: TffInt64;
  aLengthMethod: TffBLOBLinkGetLength;
  Var aFBError: TffResult)
  : Longint;
Var
  BLOBBlock: PffBlock;
  BLOBBlockHdr: PffBlockHeaderBLOB Absolute BLOBBlock;
  BLOBBlockNum: TffWord32;
  BLOBHeader: PffBLOBHeader;
  OffsetInBlock: TffWord32; {!!.11}
  aRelMethod: TffReleaseMethod;
Begin
  {$IFDEF BLOBTrace} {!!.11}
  Logbt('FFTblGetBLOBLength.Begin', []);
  Logbt('  aBLOBNr = %d:%d', [aBLOBNr.iLow, aBLOBNr.iHigh]);
  {$ENDIF}
  aFBError := DBIERR_NONE;
  { Read and verify the BLOB header block for this BLOB number. }
  BLOBBlock := ReadVfyBlobBlock2(aFI,
    aTI,
    fsc_ReadOnly,
    aBLOBNr,
    BLOBBlockNum,
    OffsetInBlock,
    aRelMethod);
  Try
    BLOBHeader := @BLOBBlock^[OffsetInBlock];
    {Begin !!.01}
        { Verify the BLOB has not been deleted. }
    If (BLOBHeader^.bbhSignature = fsc_SigBLOBSegDeleted) Then
      FSRaiseException(EfsServerException, fsStrResServer,
        fserrBLOBDeleted,
        [aFI^.fiName^, aBLOBNr.iHigh, aBLOBNr.iLow]);
    {End !!.01}
        { Verify this is a header segment. }
    If (BLOBHeader^.bbhSignature <> fsc_SigBLOBSegHeader) Then
      FSRaiseException(EfsServerException, fsStrResServer, fserrBadBLOBSeg,
        [aFI^.fiName^, aBLOBNr.iLow, aBLOBNr.iHigh,
        format(ffcBLOBSegExpected,
          [ffcBLOBSegHeader,
          char(BLOBHeader^.bbhSignature)])]);
    { What kind of BLOB are we dealing with? }
    Case BLOBHeader^.bbhSegCount Of
      fsc_FileBLOB: { File BLOB }
        aFBError := FileBLOBLength(BLOBHeader, Result);
      fsc_BLOBLink: { BLOB link }
        Begin
          Assert(assigned(aLengthMethod));
          aFBError := BLOBLinkGetLength(BLOBHeader, aLengthMethod, Result);
        End;
      Else { Standard BLOB }
        Result := BLOBHeader^.bbhBLOBLength;
    End;
  Finally
    aRelMethod(BLOBBlock);
  End;
End;

Function FFTblIsDeletedBlob(aFI: PffFileInfo;
  aTI: PffTransInfo;
  aBLOBNr: TffInt64;
  Var aFBError: TffResult)
  : Boolean;
Var
  BLOBBlock: PffBlock;
  BLOBBlockHdr: PffBlockHeaderBLOB Absolute BLOBBlock;
  BLOBHeader: PffBLOBHeader;
  OffsetInBlock: TffWord32; {!!.11}
  aRelMethod: TffReleaseMethod;
Begin
  Result:= False;
  aFBError := DBIERR_NONE;
  BLOBBlock := ReadVfyBlobBlock(aFI,
    aTI,
    fsc_ReadOnly,
    aBLOBNr,
    OffsetInBlock,
    aRelMethod);
  Try
    BLOBHeader := @BLOBBlock^[OffsetInBlock];
        { Verify the BLOB has not been deleted. }
    If (BLOBHeader^.bbhSignature = fsc_SigBLOBSegDeleted) Then
      Result:= True;
  Finally
    aRelMethod(BLOBBlock);
  End;
End;

{--------}

Function FFTblGetFileNameBLOB(aFI: PffFileInfo;
  aTI: PffTransInfo;
  aBLOBNr: TffInt64;
  Var aFileName: TffFullFileName)
  : Boolean;
Var
  BLOBBlock: PffBlock;
  BLOBBlockHdr: PffBlockHeaderBLOB Absolute BLOBBlock;
  BLOBBlockNum: TffWord32;
  BLOBHeader: PffBLOBHeader;
  FileNameLen: Integer;
  OffsetInBlock: TffWord32; {!!.11}
  aRelMethod: TffReleaseMethod;
Begin
  {read and verify the BLOB header block for this BLOB number}
  BLOBBlock := ReadVfyBlobBlock2(aFI,
    aTI,
    fsc_ReadOnly,
    aBLOBNr,
    BLOBBlockNum,
    OffsetInBlock,
    aRelMethod);
  BLOBHeader := @BLOBBlock^[OffsetInBlock];
  {Begin !!.01}
    { Verify the BLOB has not been deleted. }
  If (BLOBHeader^.bbhSignature = fsc_SigBLOBSegDeleted) Then
    FSRaiseException(EfsServerException, fsStrResServer,
      fserrBLOBDeleted,
      [aFI^.fiName^, aBLOBNr.iHigh, aBLOBNr.iLow]);
  {End !!.01}
  Result := BLOBHeader^.bbhSegCount = fsc_FileBLOB;
  If Result Then
    Begin
      {get the length of the file name}
      Move(BLOBBlock^[(OffsetInBlock + sizeof(TffBLOBHeader))],
        FileNameLen, 1);
      {move the file name to aFileName}
      Move(BLOBBlock^[(OffsetInBlock + sizeof(TffBLOBHeader))],
        aFileName, succ(FileNameLen));
    End;
  aRelMethod(BLOBBlock);
End;
{--------}

Function FFTblIsBLOBLink(aFI: PffFileInfo; {!!.11 - Start}
  aTI: PffTransInfo;
  aBLOBNr: TffInt64;
  Var aSrcTableName: TfsTableName;
  Var aSrcTableBLOBNr: TffInt64)
  : Boolean;
Var
  BLOBBlock: PffBlock;
  BLOBHeader: PffBLOBHeader;
  aHdRelMethod: TffReleaseMethod;
  BLOBBLockNum: TffWord32;
  OffsetInBlock: TffWord32; {!!.11}
Begin
  { Read and verify the BLOB header block for this BLOB number. }
  BLOBBlock := ReadVfyBlobBlock2(aFI,
    aTI,
    fsc_ReadOnly,
    aBLOBNr,
    BLOBBlockNum,
    OffsetInBlock,
    aHdRelMethod);
  Try
    BLOBHeader := @BLOBBlock^[OffsetInBlock];

    Result := BLOBHeader^.bbhSegCount = fsc_BLOBLink;

    If (Result) Then
      BLOBLinkGetTableNameAndRefNr(BLOBBlock,
        OffsetInBlock,
        aSrcTableName,
        aSrcTableBLOBNr);
  Finally
    aHdRelMethod(BLOBBlock);
  End;
End;
{--------}{!!.11 - End}
{Begin !!.03}
{--------}

Procedure WriteToStream(Const aMsg: String; aStream: TStream);
Begin
  aStream.Write(aMsg[1], Length(aMsg));
End;
{--------}

Procedure FFTblListBLOBSegments(aFI: PffFileInfo;
  aTI: PffTransInfo;
  aBLOBNr: TffInt64;
  aStream: TStream);
Var
  BLOBBlock: PffBlock;
  BLOBBlockHdr: PffBlockHeaderBLOB Absolute BLOBBlock;
  BLOBBlockNum: TffWord32;
  BLOBHeader: PffBLOBHeader;
  EntryCount: Integer;
  LookupBlock, ContentBlock: TffWord32; {!!.11}
  LookupEntry: PffBLOBLookupEntry;
  ContentEntry: PffBLOBSegmentHeader; {!!.11}
  LookupSegBlk, ContentSegBlk: PffBlock; {!!.11}
  LookupSegPtr: PffBLOBSegmentHeader;
  NextSeg: TffInt64;
  OffsetInBlock, ContentOffsetInBlock: TffWord32; {!!.11}
  aLkpRelMethod,
    aContRelMethod, {!!.11}
  aHdRelMethod: TffReleaseMethod;
Begin
  LookupSegBlk := Nil;

  { Read and verify the BLOB header block for this BLOB number. }
  BLOBBlock := ReadVfyBlobBlock2(aFI,
    aTI,
    fsc_ReadOnly,
    aBLOBNr,
    BLOBBlockNum,
    OffsetInBlock,
    aHdRelMethod);
  BLOBHeader := @BLOBBlock^[OffsetInBlock];

  { Verify the BLOB has not been deleted. }
  If (BLOBHeader^.bbhSignature = fsc_SigBLOBSegDeleted) Then
    FSRaiseException(EfsServerException, fsStrResServer,
      fserrBLOBDeleted,
      [aFI^.fiName^, aBLOBNr.iHigh, aBLOBNr.iLow]);

  { BLOB truncated to length zero? }
  If BLOBHeader^.bbh1stLookupSeg.iLow = fsc_W32NoValue Then
    Begin
      WriteToStream('BLOB has been truncated to length zero.', aStream);
      WriteToStream(#0, aStream);
      Exit;
    End;

  Try
    { Are we dealing with a file BLOB or a BLOB link? }
    Case BLOBHeader^.bbhSegCount Of
      fsc_FileBLOB: { file BLOB }
        Begin
          WriteToStream('This is a file BLOB.', aStream);
          Exit;
        End;
      fsc_BLOBLink: { BLOB link }
        Begin
          WriteToStream('This is a BLOB link.', aStream);
          Exit;
        End;
    End; { case }

    { Get the lookup segment block and set up offset for 1st lookup entry. }
    LookupSegBlk := ReadVfyBlobBlock2(aFI, aTI, fsc_ReadOnly,
      BLOBHeader^.bbh1stLookupSeg,
      LookupBlock, OffsetInBlock,
      aLkpRelMethod);
    LookupSegPtr := @LookupSegBlk^[OffsetInBlock];
    OffsetInBlock := OffsetInBlock + sizeof(TffBLOBSegmentHeader);

    { Walk through the BLOB segment linked list. }
    WriteToStream(Format('Segment list for BLOB %d:%d ' + #13#10,
      [aBLOBNr.iHigh, aBLOBNr.iLow]), aStream);
    EntryCount := 0;
    While True Do
      Begin
        inc(EntryCount);
        LookupEntry := @LookupSegBlk^[OffsetInBlock];
        {Begin !!.11}
              { Verify the segment is valid. }
        ContentSegBlk := ReadVfyBlobBlock2(aFI, aTI, fsc_ReadOnly,
          LookupEntry^.bleSegmentOffset,
          ContentBlock, ContentOffsetInBlock,
          aContRelMethod);

        ContentEntry := @ContentSegBlk^[ContentOffsetInBlock];
        If PffBlockHeaderBLOB(ContentSegBlk)^.bhbSignature <> fsc_SigBLOBBlock Then
          Raise Exception.CreateFmt
            ('Invalid BLOB block signature, block: %d', [ContentBlock])
        Else If ContentEntry^.bshSignature <> fsc_SigBLOBSegContent Then
          Raise Exception.CreateFmt
            ('Invalid signature for content segment, offset: %d,%d, signature: %s',
            [LookupEntry^.bleSegmentOffset.iHigh,
            LookupEntry^.bleSegmentOffset.iLow,
              char(ContentEntry^.bshSignature)])
        Else
          Begin

            WriteToStream(Format('Segment %d, %d:%d, Len %d' + #13#10,
              [EntryCount, LookupEntry^.bleSegmentOffset.iHigh,
              LookupEntry^.bleSegmentOffset.iLow,
                LookupEntry^.bleContentLength]), aStream);

            {see if we're at the end of the lookup segment}
            If (LookupSegPtr^.bshSegmentLen <
              (sizeof(TffBLOBSegmentHeader) +
              (succ(EntryCount) * sizeof(TffBLOBLookupEntry)))) Then
              Begin
                NextSeg := LookupSegPtr^.bshNextSegment;
                If NextSeg.iLow <> fsc_W32NoValue Then
                  Begin
                    aLkpRelMethod(LookupSegBlk);
                    LookupSegBlk := ReadVfyBlobBlock2(aFI, aTI, fsc_ReadOnly,
                      NextSeg, {!!.11}
                      LookupBlock, OffsetInBlock,
                      aLkpRelMethod);
                    LookupSegPtr := @LookupSegBlk^[OffsetInBlock];
                    OffsetInBlock := OffsetInBlock + sizeof(TffBLOBSegmentHeader);
                    EntryCount := 0;
                  End
                Else
                  break;
              End
            Else
              OffsetInBlock := OffsetInBlock + sizeof(TffBLOBLookupEntry);
          End;
        {End !!.11}
      End; {while}
  Finally
    If assigned(LookupSegBlk) Then
      aLkpRelMethod(LookupSegBlk);
    aHdRelMethod(BLOBBlock);
    WriteToStream(#0, aStream);
  End;
End;
{ End !!.03}
{Begin !!.11}
{--------}

Function FFTblRebuildLookupSegments(aFI: PffFileInfo;
  aTI: PffTransInfo;
  aNewBLOBSize: TffWord32;
  aOldBLOBSize: TffWord32;
  Const aBLOBNr: TffInt64)
  : TffInt64;
{This function takes an existing lookup segment chain & grows it to
 accomodate a larger BLOB. }
Var
  NewBLOBBlock: PffBlock;
  NewLookupHeader: PffBLOBSegmentHeader;
  OldBLOBBlock: PffBlock;
  OldLookupHeader: PffBLOBSegmentHeader;
  OldLookupEntry: PffBLOBLookupEntry;
  OldLookupBlk: PffBlock;
  OldLookupOfs: TffWord32;
  OldBLOBHeader: PffBLOBHeader;
  NewSegCount: TffWord32;
  OldSegCount: Longint;
  SegBytesUsed: TffWord32;
  EntriesToGo: Longint;
  MaxEntries: Longint;
  NewOfsInBlock: TffWord32;
  OldOfsInBlock: TffWord32;
  EntInOldSeg: Longint;
  EntInNewSeg: Longint;
  CurrentCount: Longint;
  OldHeaderOfs: TffInt64;
  TempI64: TffInt64;
  aRelMethod: TffReleaseMethod;
  aRelList: TfsPointerList;
  SegSize: TffWord32;
Begin
  { We use the following list to track the RAM pages we've accessed and
    the release method associated with each RAM page. At the end of this
    routine, we will call the release method for each RAM page. }
  aRelList := TfsPointerList.Create;

  Try
    { Get the old lookup header before we replace it with a new one. }
    OldBLOBBlock := ReadVfyBlobBlock(aFI, aTI, fsc_MarkDirty,
      aBLOBNr, OldOfsInBlock, aRelMethod);
    aRelList.Append(FFAllocReleaseInfo(OldBLOBBlock, TffInt64(aRelMethod)));
    OldBLOBHeader := PffBLOBHeader(@OldBLOBBlock^[OldOfsInBlock]);
    OldHeaderOfs := OldBLOBHeader^.bbh1stLookupSeg;

    { Determine number of segments needed to hold the entire BLOB. }
    NewSegCount := CalcBLOBSegNumber(aNewBLOBSize, aFI^.fiBlockSize, SegBytesUsed);

    { Can the number of lookup entries required for the number of segments
      fit within one lookup segment? }
    If ((NewSegCount * fsc_BLOBLookupEntrySize) <=
      (aFI^.fiMaxSegSize - fsc_BLOBSegmentHeaderSize)) Then
      Begin
        { Yes.  Create a new lookup segment. }
        SegSize := (NewSegCount * fsc_BLOBLookupEntrySize) +
          fsc_BLOBSegmentHeaderSize;
        Result := aFI^.fiBLOBrscMgr.NewSegment(aFI, aTI, SegSize, SegSize);
        NewBLOBBlock := ReadVfyBlobBlock(aFI, aTI, fsc_MarkDirty, Result,
          NewOfsInBlock, aRelMethod);
        aRelList.Append(FFAllocReleaseInfo(NewBLOBBlock, TffInt64(aRelMethod)));
        NewLookupHeader := @NewBLOBBlock^[NewOfsInBlock];

        { Setup our new lookup header. }
        With NewLookupHeader^ Do
          Begin
            bshSignature := fsc_SigBLOBSegLookup;
            bshParentBLOB := aBLOBNr;
            bshNextSegment.iLow := fsc_W32NoValue;
          End;
      End
    Else
      Begin
        { No.  We need a chain of lookup segments. }
        EntriesToGo := NewSegCount;
        MaxEntries := (aFI^.fiMaxSegSize - fsc_BLOBSegmentHeaderSize) Div
          fsc_BLOBLookupEntrySize;
        SegSize := (MaxEntries * fsc_BLOBLookupEntrySize) +
          fsc_BLOBSegmentHeaderSize;
        Result := aFI^.fiBLOBrscMgr.NewSegment(aFI, aTI, SegSize, SegSize);
        dec(EntriesToGo, MaxEntries);
        NewBLOBBlock := ReadVfyBlobBlock(aFI, aTI, fsc_MarkDirty,
          Result, NewOfsInBlock, aRelMethod);
        aRelList.Append(FFAllocReleaseInfo(NewBLOBBlock, TffInt64(aRelMethod)));
        NewLookupHeader := @NewBLOBBlock^[NewOfsInBlock];
        NewLookupHeader^.bshSignature := fsc_SigBLOBSegHeader;
        NewLookupHeader^.bshParentBLOB := aBLOBNr;
        While EntriesToGo > 0 Do
          Begin
            If EntriesToGo > MaxEntries Then
              Begin
                { We need this lookup segment & at least one more. }
                SegSize := (MaxEntries * fsc_BLOBLookupEntrySize) +
                  fsc_BLOBSegmentHeaderSize;
                NewLookupHeader^.bshNextSegment := aFI^.fiBLOBrscMgr.NewSegment
                  (aFI, aTI, SegSize, SegSize);
                dec(EntriesToGo, MaxEntries);
                NewBLOBBlock := ReadVfyBlobBlock(aFI, aTI, fsc_MarkDirty,
                  NewLookupHeader^.bshNextSegment,
                  NewOfsInBlock, aRelMethod);
                aRelList.Append(FFAllocReleaseInfo(NewBLOBBlock, TffInt64(aRelMethod)));
              End
            Else
              Begin
                { This is the last lookup segment needed. }
                SegSize := (EntriesToGo * fsc_BLOBLookupEntrySize) +
                  fsc_BLOBSegmentHeaderSize;
                NewLookupHeader^.bshNextSegment := aFI^.fiBLOBrscMgr.NewSegment
                  (aFI, aTI, SegSize, SegSize);
                dec(EntriesToGo, EntriesToGo);
                NewBLOBBlock := ReadVfyBlobBlock(aFI, aTI, fsc_MarkDirty,
                  NewLookupHeader^.bshNextSegment,
                  NewOfsInBlock, aRelMethod);
                aRelList.Append(FFAllocReleaseInfo(NewBLOBBlock, TffInt64(aRelMethod)));
              End; {if..else}

            { Initialize the segment. }
            NewLookupHeader := @NewBLOBBlock^[NewOfsInBlock];
            NewLookupHeader^.bshSignature := fsc_SigBLOBSegHeader;
            NewLookupHeader^.bshParentBLOB := aBLOBNr;
            NewLookupHeader^.bshNextSegment.iLow := fsc_W32NoValue;

          End; {while}
        {Reset the new lookup segment to the 1st one in the chain.}
        NewBLOBBlock := ReadVfyBlobBlock(aFI, aTI, fsc_MarkDirty,
          Result,
          NewOfsInBlock, aRelMethod);
        NewLookupHeader := @NewBLOBBlock^[NewOfsInBlock];

      End; {if..else}

    { Now that we have our newly-sized lookup header(s) and entries, we
      need to copy the old entries into the new header. }
    If aOldBLOBSize = 0 Then
      OldSegCount := 0
    Else
      OldSegCount := CalcBLOBSegNumber(aOldBLOBSize, aFI^.fiBlockSize,
        SegBytesUsed);

    If OldSegCount <> 0 Then
      Begin
        OldLookupBlk := ReadVfyBlobBlock(aFI, aTI, fsc_MarkDirty,
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
        If OldLookupHeader^.bshNextSegment.iLow <> fsc_W32NoValue Then
          { Yes.  It must have the maximum number of lookup entries so figure out
            how many that is. }
          EntInOldSeg := FFCalcMaxLookupEntries(OldLookupHeader)
        Else
          { No.  The number of lookup entries equals the number of segments in
            the BLOB. }
          EntInOldSeg := OldSegCount;

        { Figure out the maximum number of entries for the new lookup segment. }
        EntInNewSeg := FFCalcMaxLookupEntries(NewLookupHeader);

        CurrentCount := 0;
        While CurrentCount < OldSegCount Do
          Begin
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
            If OldLookupHeader^.bshNextSegment.iLow <> fsc_W32NoValue Then
              Begin
                { Yes.  Move to it. }
                OldHeaderOfs := OldLookupHeader^.bshNextSegment;
                OldBLOBBlock := ReadVfyBlobBlock(aFI, aTI, fsc_MarkDirty,
                  OldHeaderOfs, OldLookupOfs,
                  aRelMethod);
                aRelList.Append(FFAllocReleaseInfo(OldBLOBBlock, TffInt64(aRelMethod)));
                OldLookupBlk :=
                  ReadVfyBlobBlock(aFI, aTI, fsc_MarkDirty,
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
              End;

            { Delete the old lookup segment now that we have copied all its
              entries. }
            aFI^.fiBLOBrscMgr.DeleteSegment(aFI, aTI, TempI64);

            { Check if we've filled up our current (target) header}
            If (EntInNewSeg = 0) And
              (NewLookupHeader^.bshNextSegment.iLow <> fsc_W32NoValue) Then
              Begin
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
              End;
          End; {while}
      End; {if}
    OldBLOBHeader^.bbh1stLookupSeg := Result;
  Finally
    For CurrentCount := 0 To pred(aRelList.Count) Do
      Begin
        FFDeallocReleaseInfo(aRelList[CurrentCount]);
      End;
    aRelList.Free;
  End;
End;
{====================================================================}

{===TffBaseBLOBEngine================================================}

Class Function TffBaseBLOBEngine.GetEngine(aFI: PffFileInfo): TffBaseBLOBEngine;
Begin
  Result := FFBLOBEngine;
End;
{====================================================================}

{===TffBLOBEngine====================================================}

Procedure TffBLOBEngine.Read(aFI: PffFileInfo;
  aTI: PffTransInfo;
  aBLOBNr: TffInt64;
  aOffset: TffWord32;
  aLen: TffWord32;
  aReadMethod: TffBLOBLinkRead;
  Var aBLOB;
  Var aBytesRead: TffWord32;
  Var aFBError: TffResult);
Var
  aCntRelMethod,
    aLkpRelMethod,
    aHdRelMethod: TffReleaseMethod;
  BLOBAsBytes: PffBLOBArray;
  BLOBBlock: PffBlock;
  BLOBBlockHdr: PffBlockHeaderBLOB Absolute BLOBBlock;
  BLOBBlockNum: TffWord32;
  BLOBHeader: PffBLOBHeader;
  BytesToCopy: TffWord32;
  ContentBlock,
    LookupSegOfs: TffWord32;
  ContentSegBlk: PffBlock;
  ContentSegOfs: TffInt64;
  DestOffset: TffWord32;
  MaxLookupEntries: Integer;
  LookupBlock: TffWord32;
  LookupEntry: PffBLOBLookupEntry;
  LookupSegBlk: PffBlock;
  LookupSegPtr: PffBLOBSegmentHeader;
  OffsetInBlock: TffWord32;
  StartBytesUsed,
    BLOBPos: TffWord32;
  CurrLookupEntry: Integer;
  {$IFDEF BLOBTrace}
  LookupSegCount: Integer;
  {$ENDIF}
  NextSeg: TffInt64; {!!.11}
Begin
  {$IFDEF BLOBTrace}
  Logbt('FFTblReadBLOB.Begin', []);
  Logbt('  aBLOBNr = %d:%d', [aBLOBNr.iLow, aBLOBNr.iHigh]);
  Logbt('  aOffset = %d', [aOffset]);
  Logbt('  aLen    = %d', [aLen]);
  Try
    {$ENDIF}

    BLOBAsBytes := @aBLOB;
    ContentSegBlk := Nil;
    LookupSegBlk := Nil;
    DestOffset := 0;

    aFBError := 0;

    {Exit if aLen = 0}
    If aLen = 0 Then
      Exit;

    { Read and verify the BLOB header block for this BLOB number. }
    BLOBBlock := ReadVfyBlobBlock2(aFI,
      aTI,
      fsc_ReadOnly,
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
    If (BLOBHeader^.bbhSignature = fsc_SigBLOBSegDeleted) Then
      FSRaiseException(EfsServerException,
        fsStrResServer,
        fserrBLOBDeleted,
        [aFI^.fiName^,
        aBLOBNr.iHigh,
          aBLOBNr.iLow]);

    Try
      { Are we dealing with a file BLOB or a BLOB link? }
      Case BLOBHeader^.bbhSegCount Of
        fsc_FileBLOB: { file BLOB }
          Begin
            aFBError := FileBLOBRead(aFI,
              aTI,
              aBLOBNr,
              aOffset,
              aLen,
              aBLOB,
              aBytesRead);
            Exit;
          End;
        fsc_BLOBLink: { BLOB link }
          Begin
            aFBError := BLOBLinkRead(aFI,
              aTI,
              aBLOBNr,
              aOffset,
              aLen,
              aReadMethod,
              aBLOB,
              aBytesRead);
            Exit;
          End;
      End; { case }

      { Make sure that the offset is within BLOB. }
      If (FFCmpDW(aOffset, BLOBHeader^.bbhBLOBLength) >= 0) Then
        Begin
          aBytesRead := 0;
          Exit;
        End;
      { Get the lookup segment block and set up offset for 1st lookup entry. }
      LookupSegBlk := ReadVfyBlobBlock2(aFI,
        aTI,
        fsc_ReadOnly,
        BLOBHeader^.bbh1stLookupSeg,
        LookupBlock,
        LookupSegOfs,
        aLkpRelMethod);
      LookupSegPtr := @LookupSegBlk^[LookupSegOfs];
      LookupSegOfs := LookupSegOfs + fsc_BLOBSegmentHeaderSize;

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
      While (BLOBPos < aOffset) Do
        Begin
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
          If ((BLOBPos + LookupEntry^.bleContentLength) >= aOffset) Then
            Begin
              { Yes. We found the starting point. }
              ContentSegOfs := LookupEntry^.bleSegmentOffset;
              StartBytesUsed := aOffset - BLOBPos;
              { NOTE: We will start reading from this segment, so we don't
                      want to move past it. }
              Break;
            End
          Else
            Begin
              { Nope. Update and keep moving. }
              BLOBPos := BLOBPos + LookupEntry^.bleContentLength;
              LookupSegOfs := LookupSegOfs + fsc_BLOBLookupEntrySize;
              CurrLookupEntry := CurrLookupEntry + 1;
            End;

          { Have we reached the end of this lookup segment? }
          If (CurrLookupEntry > MaxLookupEntries) Then
            Begin
              { Get the lookup segment block and set up offset for 1st lookup entry. }
              NextSeg := LookupSegPtr^.bshNextSegment; {!!.11}
              aLkpRelMethod(LookupSegBlk);
              LookupSegBlk := ReadVfyBlobBlock2(aFI,
                aTI,
                fsc_ReadOnly,
                NextSeg, {!!.11}
                LookupBlock,
                LookupSegOfs,
                aLkpRelMethod);
              LookupSegPtr := @LookupSegBlk^[LookupSegOfs];
              LookupSegOfs := LookupSegOfs + fsc_BLOBSegmentHeaderSize;

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
            End;
        End;

      { Read what we need. }
      BLOBPos := 0;
      While (BLOBPos < aBytesRead) Do
        Begin
          { Read the BLOB content segment. }
          If (ContentSegBlk <> Nil) Then
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
            fsc_ReadOnly,
            LookupEntry^.bleSegmentOffset,
            ContentBlock,
            OffsetInBlock,
            aCntRelMethod);
          OffsetInBlock := OffsetInBlock + fsc_BLOBSegmentHeaderSize;

          If (StartBytesUsed > 0) Then
            Begin
              { This is the first segment we're reading from. This will
                normally be in the middle of a segment. }
              BytesToCopy := LookupEntry^.bleContentLength - StartBytesUsed;
              OffsetInBlock := OffsetInBlock + StartBytesUsed;
            End
          Else
            Begin
              { copying from middle segments }
              BytesToCopy := LookupEntry^.bleContentLength;
            End;

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
          If ((BLOBPos < aBytesRead) And
            (CurrLookupEntry > MaxLookupEntries)) Then
            Begin
              NextSeg := LookupSegPtr^.bshNextSegment; {!!.11}
              aLkpRelMethod(LookupSegBlk);
              { Get the lookup segment block and set up offset for 1st
                lookup entry. }
              LookupSegBlk := ReadVfyBlobBlock2(aFI,
                aTI,
                fsc_ReadOnly,
                NextSeg, {!!.11}
                LookupBlock,
                LookupSegOfs,
                aLkpRelMethod);
              LookupSegPtr := @LookupSegBlk^[LookupSegOfs];
              LookupSegOfs := LookupSegOfs + fsc_BLOBSegmentHeaderSize;

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
            End
          Else
            Begin
              LookupSegOfs := LookupSegOfs + fsc_BLOBLookupEntrySize;
            End;
        End; {while}

    Finally
      If assigned(ContentSegBlk) Then
        aCntRelMethod(ContentSegBlk);
      If assigned(LookupSegBlk) Then
        aLkpRelMethod(LookupSegBlk);
      aHdRelMethod(BLOBBlock);
    End;
    {$IFDEF BLOBTrace}
  Except
    Logbt('*** FFTblReadBLOB Exception ***', []);
    Raise;
  End
  {$ENDIF}
End;
{--------}

Function TffBLOBEngine.IsEmptyLookupEntry(Entry: PffBLOBLookupEntry): Boolean;
{ Revised !!.13}
Const
  ciEmptyVal1 = 808464432;
  { This is because lookup segments prior to 2.13 were fillchar'd with 'O'
    instead of 0. We have to check all 3 fields in the lookup entry for this
    value so that we avoid a case where the value is valid. }
  ciEmptyVal2 = 1179010630;
  { Another value that indicates an empty lookup entry. }
Begin
  Result := (Entry^.bleSegmentOffset.iLow = fsc_W32NoValue) Or
    ((Entry^.bleSegmentOffset.iLow = 0) And
    (Entry^.bleSegmentOffset.iHigh = 0)) Or
    ((Entry^.bleSegmentOffset.iLow = ciEmptyVal1) And
    (Entry^.bleSegmentOffset.iHigh = ciEmptyVal1) And
    (Entry^.bleContentLength = ciEmptyVal1)) Or
    ((Entry^.bleSegmentOffset.iLow = ciEmptyVal2) And
    (Entry^.bleSegmentOffset.iHigh = ciEmptyVal2) And
    (Entry^.bleContentLength = ciEmptyVal2));
End;
{--------}

Procedure TffBLOBEngine.Truncate(aFI: PffFileInfo;
  aTI: PffTransInfo;
  aBLOBNr: TffInt64;
  aLen: TffWord32);
Var
  aRelList: TfsPointerList;
  aRelMethod: TffReleaseMethod;
  NextLookupSeg: TffInt64;
  ContOffset, {!!.13}
  BLOBPos,
    CurrLookupEntry,
    LookupBlock,
    MaxLookupEntries,
    OffsetInBlock,
    StartBytesUsed: TffWord32;
  NewSegCount: Integer;
  BLOBBlock: PffBlock;
  ContentSegBlk, {!!.13}
  LookupSegBlk: PffBlock;
  BLOBBlockHdr: PffBlockHeaderBLOB Absolute BLOBBlock;
  BLOBHeader: PffBLOBHeader;
  {Begin !!.13}
  ContentSegOfs: TffInt64;
  ContentSegPtr,
    {End !!.13}
  LookupSegPtr: PffBLOBSegmentHeader;
  LookupEntry: PffBLOBLookupEntry;
  {$IFDEF BLOBTrace}
  LookupSegCount: Integer;
  {$ENDIF}
Begin
  {$IFDEF BLOBTrace}
  Logbt('Entering FFTblTruncateBLOB', []);
  Logbt('  aBLOBNr = %d:%d', [aBLOBNr.iLow, aBLOBNr.iHigh]);
  Logbt('  aLen    = %d', [aLen]);
  LookupSegCount := 1;
  {$ENDIF}


    { We use the following list to track the RAM pages we've accessed and
      the release method associated with each RAM page. At the end of this
      routine, we will call the release method for each RAM page. }
  aRelList := TfsPointerList.Create;

  Try
    { Read and verify the BLOB header block for this BLOB number. }
    BLOBBlock := ReadVfyBlobBlock(aFI,
      aTI,
      fsc_MarkDirty,
      aBLOBNr,
      OffsetInBlock,
      aRelMethod);
    aRelList.Append(FFAllocReleaseInfo(BLOBBlock, TffInt64(aRelMethod)));

    BLOBHeader := @BLOBBlock^[OffsetInBlock];

    { Check if we're trying to truncate a zero-length BLOB or to the
      BLOB's current length. }
    If (BLOBHeader^.bbhBLOBLength = aLen) Then
      Exit;

    { Verify the BLOB has not been deleted. }
    If (BLOBHeader^.bbhSignature = fsc_SigBLOBSegDeleted) Then
      FSRaiseException(EfsServerException, fsStrResServer,
        fserrBLOBDeleted,
        [aFI^.fiName^, aBLOBNr.iHigh, aBLOBNr.iLow]);

    { Verify this is a header segment. }
    If (BLOBHeader^.bbhSignature <> fsc_SigBLOBSegHeader) Then
      FSRaiseException(EfsServerException,
        fsStrResServer,
        fserrBadBLOBSeg,
        [aFI^.fiName^,
        aBLOBNr.iLow,
          aBLOBNr.iHigh,
          Format(ffcBLOBSegExpected,
          [ffcBLOBSegHeader,
          Char(BLOBHeader^.bbhSignature)])]);

    { We can't write to a file BLOB. }
    If (BLOBHeader^.bbhSegCount = -1) Then
      FSRaiseException(EfsServerException,
        fsStrResServer,
        fserrFileBLOBWrite,
        [aFI^.fiName^,
        aBLOBNr.iLow,
          aBLOBNr.iHigh]);

    { Make sure the truncated length <= current BLOB length. }
    If (aLen > BLOBHeader^.bbhBLOBLength) Then
      FSRaiseException(EfsServerException,
        fsStrResServer,
        fserrLenMismatch,
        [aFI^.fiName^,
        aBLOBNr.iLow,
          aBLOBNr.iHigh,
          aLen,
          BLOBHeader^.bbhBLOBLength]);

    { If the new length is greater than 0, we will lop off some
      content segments. The content segment that becomes the last
      content segment must be updated. }
    NewSegCount := 0;
    If (aLen > 0) Then
      Begin
        { Grab the first lookup segment. }
        NextLookupSeg := BLOBHeader^.bbh1stLookupSeg;
        LookupSegBlk := ReadVfyBlobBlock2(aFI,
          aTI,
          fsc_MarkDirty, {!!.13}
          NextLookupSeg,
          LookupBlock,
          OffsetInBlock,
          aRelMethod);
        aRelList.Append(FFAllocReleaseInfo(LookupSegBlk, TffInt64(aRelMethod)));
        LookupSegPtr := PffBLOBSegmentHeader(@LookupSegBlk^[OffsetInBlock]);
        MaxLookupEntries := FFCalcMaxLookupEntries(LookupSegPtr);

        OffsetInBlock := OffsetInBlock +
          fsc_BLOBSegmentHeaderSize;
        CurrLookupEntry := 1;

        { Position to where we are to start truncating. }
        BLOBPos := 0;
        StartBytesUsed := 0;
        While (BLOBPos < aLen) Do
          Begin
            NewSegCount := NewSegCount + 1;
            LookupEntry := @LookupSegBlk^[OffsetInBlock];
            {$IFDEF BLOBTrace}
            Logbt('  Lookup entry %d points to a segment with %d bytes',
              [CurrLookupEntry,
              LookupEntry^.bleContentLength]);
            {$ENDIF}

            If ((BLOBPos + LookupEntry^.bleContentLength) >= aLen) Then
              Begin {!!.13}
                { We found the starting point. }
                StartBytesUsed := aLen - BLOBPos;
                Break;
              End
            Else
              Begin
                BLOBPos := BLOBPos + LookupEntry^.bleContentLength;
                CurrLookupEntry := CurrLookupEntry + 1;
              End;

            { Have we reached the end of this lookup segment? }
            If ((BLOBPos < aLen) And
              (CurrLookupEntry > MaxLookupEntries)) Then
              Begin
                { Get the lookup segment block and set up offset for 1st
                  lookup entry. }
                NextLookupSeg := LookupSegPtr^.bshNextSegment;
                LookupSegBlk := ReadVfyBlobBlock2(aFI,
                  aTI,
                  fsc_MarkDirty, {!!.13}
                  NextLookupSeg,
                  LookupBlock,
                  OffsetInBlock,
                  aRelMethod); {!!.13}
                aRelList.Append(FFAllocReleaseInfo(LookupSegBlk, TffInt64(aRelMethod))); {!!.13}
                LookupSegPtr := @LookupSegBlk^[OffsetInBlock];
                OffsetInBlock := OffsetInBlock + fsc_BLOBSegmentHeaderSize;

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
              End
            Else
              OffsetInBlock := OffsetInBlock + fsc_BLOBLookupEntrySize;
          End; { while }

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
          fsc_MarkDirty,
          ContentSegOfs,
          ContOffset,
          aRelMethod);
        aRelList.Append(FFAllocReleaseInfo(ContentSegBlk, TffInt64(aRelMethod))); {!!.13}
        ContentSegPtr := @ContentSegBlk^[ContOffset];
        ContentSegPtr^.bshNextSegment.iLow := fsc_W32NoValue;
        {End !!.13}

              { Delete the content & lookup segments that are no longer needed.
                First, obtain the number of extraneous lookup entries in the
                current lookup segment. }
        While (BLOBPos < BLOBHeader^.bbhBLOBLength) Do
          Begin
            CurrLookupEntry := CurrLookupEntry + 1;
            OffsetInBlock := OffsetInBlock + fsc_BLOBLookupEntrySize;
            LookupEntry := @LookupSegBlk^[OffsetInBlock];
            { Have we reached the end of this lookup segment? }
            If (CurrLookupEntry > MaxLookupEntries) Then
              Begin
                If LookupSegPtr^.bshNextSegment.iLow = fsc_W32NoValue Then
                  Break
                Else
                  Begin
                    { Get the lookup segment block and set up offset for 1st
                      lookup entry. }
                    NextLookupSeg := LookupSegPtr^.bshNextSegment;
                    LookupSegBlk := ReadVfyBlobBlock2(aFI,
                      aTI,
                      fsc_MarkDirty,
                      NextLookupSeg,
                      LookupBlock,
                      OffsetInBlock,
                      aRelMethod); {!!.13}
                    aRelList.Append(FFAllocReleaseInfo(LookupSegBlk, TffInt64(aRelMethod))); {!!.13}
                    LookupSegPtr^.bshNextSegment.iLow := fsc_W32NoValue;
                    LookupSegPtr := @LookupSegBlk^[OffsetInBlock];
                    { Move ahead to first lookup entry. }
                    OffsetInBlock := OffSetInBlock + fsc_BLOBSegmentHeaderSize;
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
                  End
              End
            Else If IsEmptyLookupEntry(LookupEntry) Then
              { Have we encountered an empty lookup segment? If so then this
                indicates the end of the BLOB content. }
              Break;

            If (StartBytesUsed = 0) Then
              BLOBPos := BLOBPos + LookupEntry^.bleContentLength
            Else
              StartBytesUsed := 0;

            aFI^.fiBLOBrscMgr.DeleteSegment(aFI,
              aTI,
              LookupEntry^.bleSegmentOffset);
            FillChar(LookupEntry^, fsc_BLOBLookupEntrySize, 0); {!!.13}

          End; { while }
        LookupSegPtr^.bshNextSegment.iLow := fsc_W32NoValue;
      End
    Else
      Begin
        { We are truncating to length of 0. }
        FFTblDeleteBLOBPrim(aFI, aTI, BLOBHeader);

        { Reset the lookup segment field and the segment count.
          FFTblFreeBLOB will get rid of the BLOB header if the BLOB is
          still at length 0. }
        BLOBHeader^.bbh1stLookupSeg.iLow := fsc_W32NoValue;
      End;
    { Set the new BLOB length and segment count in the BLOB header. }
    BLOBHeader^.bbhBLOBLength := aLen;

    { Set the new segment count in the BLOB header. }
    BLOBHeader^.bbhSegCount := NewSegCount;
  Finally
    For OffsetInBlock := 0 To (aRelList.Count - 1) Do
      FFDeallocReleaseInfo(aRelList[OffsetInBlock]);
    aRelList.Free;
  End;
End;
{--------}

Procedure TffBLOBEngine.Write(aFI: PffFileInfo;
  aTI: PffTransInfo;
  Const aBLOBNr: TffInt64;
  aOffset: TffWord32; {offset in blob to start writing}
  aLen: TffWord32; {bytes from aOffset to stop writing}
  Const aBLOB);
Var
  aLkpRelMethod,
    aRelMethod: TffReleaseMethod;
  aRelList: TfsPointerList;
  ContentSegOfs: TffInt64;
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
    TempWord: TffWord32;
  MinSegSize: Integer;
  BLOBBlock,
    ContentSegBlk,
    LookupSegBlk,
    PrevContSegBlk: PffBlock;
  BLOBBlockHdr: PffBlockHeaderBLOB Absolute BLOBBlock;
  BLOBHeader: PffBLOBHeader;
  BLOBAsBytes: PffBLOBArray;
  LookupEntry: PffBLOBLookupEntry;
  ContentSegPtr,
    LookupSegPtr,
    PrevContentSegPtr,
    TempSegPtr: PffBLOBSegmentHeader;
  NewSegment: Boolean;
  {$IFDEF BLOBTrace}
  LookupSegCount: Integer;
  {$ENDIF}
  NextSeg: TffInt64;
  TempaLen: TffWord32; {!!.11}
Begin
  {$IFDEF BLOBTrace}
  Logbt('Entering FFTblWriteBLOB', []);
  Logbt('  aBLOBNr = %d:%d', [aBLOBNr.iLow, aBLOBNr.iHigh]);
  Logbt('  aOffset = %d', [aOffset]);
  Logbt('  aLen    = %d', [aLen]);
  Try
    {$ENDIF}

    BLOBAsBytes := @aBLOB;
    ContentSegOfs.iLow := fsc_W32NoValue;
    LookupSegBlk := Nil;
    TempaLen := aLen;

    { We use the following list to track the RAM pages we've accessed and
      the release method associated with each RAM page. At the end of this
      routine, we will call the release method for each RAM page. }
    aRelList := TfsPointerList.Create;

    Try
      { Read and verify the BLOB header block for this BLOB number. }
      BLOBBlock := ReadVfyBlobBlock(aFI,
        aTI,
        fsc_MarkDirty,
        aBLOBNr,
        OffsetInBlock,
        aRelMethod);
      aRelList.Append(FFAllocReleaseInfo(BLOBBlock, TffInt64(aRelMethod)));
      BLOBHeader := @BLOBBlock^[OffsetInBlock];

      { Verify the new length (aLen + aOffset) doesn't exceed max. }
      NewSize := FFMaxL(aOffset + aLen, BLOBHeader^.bbhBLOBLength);
      If (NewSize > fscl_MaxBLOBLength) Then
        FSRaiseException(EfsServerException,
          fsStrResServer,
          fserrBLOBTooBig,
          [NewSize]);

      { Verify the BLOB has not been deleted. }
      If (BLOBHeader^.bbhSignature = fsc_SigBLOBSegDeleted) Then
        FSRaiseException(EfsServerException,
          fsStrResServer,
          fserrBLOBDeleted,
          [aFI^.fiName^,
          aBLOBNr.iHigh,
            aBLOBNr.iLow]);

      { For a file BLOB raise an error. }
      If (BLOBHeader^.bbhSegCount = -1) Then
        FSRaiseException(EfsServerException,
          fsStrResServer,
          fserrFileBLOBWrite,
          [aFI^.fiName^,
          aBLOBNr.iLow,
            aBLOBNr.iHigh]);

      { Verify the offset is within, or at the end of, the BLOB. }
      If (aOffset > BLOBHeader^.bbhBLOBLength) Then
        FSRaiseException(EfsServerException,
          fsStrResServer,
          fserrOfsNotInBlob,
          [aFI^.fiName^,
          aBLOBNr.iLow,
            aBLOBNr.iHigh,
            aOffset,
            BLOBHeader^.bbhBLOBLength]);

      { If there's not one, we'll need a lookup segment. }
      If (BLOBHeader^.bbh1stLookupSeg.iLow = fsc_W32NoValue) Then
        Begin
          NewSegment := True;
          TempWord := EstimateSegmentCount(NewSize, aFI^.fiMaxSegSize);
          TempWord := (TempWord * fsc_BLOBLookupEntrySize) + fsc_BLOBSegmentHeaderSize;
          TempWord := FFMinDW(TempWord, aFI^.fiMaxSegSize);
          BLOBHeader^.bbh1stLookupSeg := aFI^.fiBLOBrscMgr.NewSegment(aFI,
            aTI,
            TempWord,
            (TempWord Div 2));
          {$IFDEF BLOBTrace}
          Logbt('  Built first lookup segment: %d:%d',
            [BLOBHeader^.bbh1stLookupSeg.iLow,
            BLOBHeader^.bbh1stLookupSeg.iHigh]);
          {$ENDIF}
        End
      Else
        Begin
          NewSegment := False;
          {$IFDEF BLOBTrace}
          Logbt('  First lookup segment established: %d:%d',
            [BLOBHeader^.bbh1stLookupSeg.iLow,
            BLOBHeader^.bbh1stLookupSeg.iHigh]);
          {$ENDIF}
        End;

      { Get the first lookup segment. }
      LookupSegBlk := ReadVfyBlobBlock(aFI,
        aTI,
        fsc_MarkDirty,
        BLOBHeader^.bbh1stLookupSeg,
        LookupSegOfs,
        aLkpRelMethod);
      LookupSegPtr := @LookupSegBlk^[LookupSegOfs];
      If (NewSegment) Then
        Begin
          LookupSegPtr^.bshParentBLOB := aBLOBNr;
          LookupSegPtr^.bshSignature := fsc_SigBLOBSegLookup;
          LookupSegPtr^.bshNextSegment.iLow := fsc_W32NoValue;
        End;
      MaxLookupEntries := FFCalcMaxLookupEntries(LookupSegPtr);

      LookupEntOfs := LookupSegOfs + SizeOf(TffBLOBSegmentHeader);
      CurrLookupEntry := 1;
      {$IFDEF BLOBTrace}
      LookupSegCount := 1;
      Logbt('  Lookup segment - Max entries: %d', [MaxLookupEntries]);
      {$ENDIF}

      { Position to where we are to start writing. }
      BLOBPos := 0;
      LookupEntry := Nil;
      StartBytesUsed := 0;
      While (BLOBPos < aOffset) Do
        Begin
          LookupEntry := @LookupSegBlk^[LookupEntOfs];
          {$IFDEF BLOBTrace}
          Logbt('  Lookup entry %d points to a segment with %d bytes',
            [CurrLookupEntry,
            LookupEntry^.bleContentLength]);
          {$ENDIF}
          { Does this entry point to the segment where we should start
            copying data? }
          If ((BLOBPos + LookupEntry^.bleContentLength) >= aOffset) Then
            Begin
              { Yes. We found the starting point. }
              ContentSegOfs := LookupEntry^.bleSegmentOffset;
              StartBytesUsed := aOffset - BLOBPos;
              { NOTE: We will be making updates to this segment, so we don't
                      want to move past it. }
              Break;
            End
          Else
            Begin
              { Nope. Update and keep moving. }
              BLOBPos := BLOBPos + LookupEntry^.bleContentLength;
              LookupEntOfs := LookupEntOfs + fsc_BLOBLookupEntrySize;
              CurrLookupEntry := CurrLookupEntry + 1;
            End;

          { Have we reached the end of this lookup segment? }
          If (CurrLookupEntry > MaxLookupEntries) Then
            Begin
              { Get the lookup segment block and set up offset for 1st lookup entry. }
              NextSeg := LookupSegPtr^.bshNextSegment; {!!.11}
              aLkpRelMethod(LookupSegBlk);
              LookupSegBlk := ReadVfyBlobBlock2(aFI,
                aTI,
                fsc_MarkDirty,
                NextSeg, {!!.11}
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
            End;
        End;

      { We may need to initialize the previous content segment so that
        we can maintain the chain. }
      If ((BLOBPos = 0) And
        (BLOBHeader^.bbhBLOBLength > 0)) Then
        Begin
          LookupEntry := @LookupSegBlk^[LookupEntOfs];
          ContentSegOfs := LookupEntry^.bleSegmentOffset;
        End;

      ContentSegPtr := Nil;
      If (ContentSegOfs.iLow <> fsc_W32NoValue) Then
        Begin
          { Get the previous content segment. }
          ContentSegOfs := LookupEntry^.bleSegmentOffset;
          ContentSegBlk := ReadVfyBlobBlock(aFI,
            aTI,
            fsc_MarkDirty,
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
        End;

      { I've been using BLOBPos to track where I was at in the existing
        BLOB, if any. Now, I'm going to be using it to track where we
        are in the source (data being added to the BLOB). }
      BLOBPos := 0;

      { Now we're positioned and ready to start copying the source data
        to the BLOB. }
      BytesToGo := aLen;
      While (BytesToGo > 0) Do
        Begin
          { Are we overwriting an existing segment? }
          If (ContentSegOfs.iLow <> fsc_W32NoValue) Then
            Begin
              { Yes. Get the location of the existing segment so we can
                update it. }
              BytesToCopy := BytesToGo;
              {$IFDEF BLOBTrace}
              Logbt('  Updating existing segment: %d:%d.',
                [ContentSegOfs.iLow, ContentSegOfs.iHigh]);
              {$ENDIF}
            End
          Else
            Begin
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
              MinSegSize := fsc_BLOBSegmentIncrement;
              ContentSegOfs := aFI^.fiBLOBrscMgr.NewSegment(aFI,
                aTI,
                SegSize,
                MinSegSize);
              LookupEntry^.bleSegmentOffset := ContentSegOfs;
              LookupEntry^.bleContentLength := 0;

              { Increment the segment count. }
              BLOBHeader^.bbhSegCount := BLOBHeader^.bbhSegCount + 1;

              If (PrevContentSegPtr <> Nil) Then
                Begin
                  PrevContentSegPtr^.bshNextSegment := ContentSegOfs;
                End;
              {$IFDEF BLOBTrace}
              Logbt('  Created new segment: %d:%d.',
                [ContentSegOfs.iLow, ContentSegOfs.iHigh]);
              {$ENDIF}
            End;

          { Get the content segment. }
          ContentSegBlk := ReadVfyBlobBlock(aFI,
            aTI,
            fsc_MarkDirty,
            ContentSegOfs,
            OffsetInBlock,
            aRelMethod);
          aRelList.Append(FFAllocReleaseInfo(ContentSegBlk,
            TffInt64(aRelMethod)));
          ContentSegPtr := @ContentSegBlk^[OffsetInBlock];
          If (NewSegment) Then
            Begin
              ContentSegPtr^.bshSignature := fsc_SigBLOBSegContent;
              ContentSegPtr^.bshParentBLOB := aBLOBNr;
              ContentSegPtr^.bshNextSegment.iLow := fsc_W32NoValue;
              NewSegment := False;
            End;

          { We may not have gotten an optimal size segment, so we need
            to update how many bytes we can copy based on the actual
            segment size. }
          StartBytesUsed := StartBytesUsed + fsc_BLOBSegmentHeaderSize;
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

          StartBytesUsed := StartBytesUsed - fsc_BLOBSegmentHeaderSize;
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
          If (StartBytesUsed + BytesToCopy >
            LookupEntry^.bleContentLength) Then
            Begin
              LookupEntry^.bleContentLength := StartBytesUsed + BytesToCopy;
            End;
          {$IFDEF BLOBTrace}
          Logbt('  Last lookup entry now points to segment with %d bytes',
            [LookupEntry^.bleContentLength]);
          {$ENDIF}

          CurrLookupEntry := CurrLookupEntry + 1;
          StartBytesUsed := 0;

          { Have we reached the end of this lookup segment? }
          If ((BytesToGo > 0) And
            (CurrLookupEntry > MaxLookupEntries)) Then
            Begin
              { Is there another lookup segment in this chain? }
              If (LookupSegPtr^.bshNextSegment.iLow = fsc_W32NoValue) Then
                Begin
                  { No. We'll have to get a new one and add it to the chain. }
                  TempWord := EstimateSegmentCount(BytesToGo, aFI^.fiMaxSegSize);
                  TempWord := (TempWord * fsc_BLOBLookupEntrySize) + fsc_BLOBSegmentHeaderSize;
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
                End
              Else
                Begin
                  { Yes. Assign it to our temp variable. }
                  ContentSegOfs := LookupSegPtr^.bshNextSegment;
                  {$IFDEF BLOBTrace}
                  Logbt('  Moving to next lookup segment.',
                    [ContentSegOfs.iLow, ContentSegOfs.iHigh]);
                  {$ENDIF}
                End;

              { Get the lookup segment block and set up offset for 1st
                lookup entry. }
              aLkpRelMethod(LookupSegBlk);
              LookupSegBlk := ReadVfyBlobBlock2(aFI,
                aTI,
                fsc_MarkDirty,
                ContentSegOfs,
                LookupBlock,
                LookupSegOfs,
                aLkpRelMethod);

              { Intialize the segment on if it's new. }
              If ((LookupSegPtr <> Nil) And
                (LookupSegPtr^.bshNextSegment.iLow <> fsc_W32NoValue)) Then
                Begin
                  LookupSegPtr := @LookupSegBlk^[LookupSegOfs];
                  LookupEntOfs := LookupSegOfs + fsc_BLOBSegmentHeaderSize;
                  LookupEntry := @LookupSegBlk^[LookupEntOfs];
                  ContentSegOfs := LookupEntry^.bleSegmentOffset;
                End
              Else
                Begin
                  { Chain the last lookup segment to the new one. }
                  LookupSegPtr^.bshNextSegment := ContentSegOfs;

                  LookupSegPtr := @LookupSegBlk^[LookupSegOfs];
                  LookupSegPtr^.bshParentBLOB := aBLOBNr;
                  LookupSegPtr^.bshSignature := fsc_SigBLOBSegLookup;
                  LookupSegPtr^.bshNextSegment.iLow := fsc_W32NoValue;

                  LookupEntOfs := LookupSegOfs + fsc_BLOBSegmentHeaderSize;
                  LookupEntry := @LookupSegBlk^[LookupEntOfs];
                  ContentSegOfs.iLow := fsc_W32NoValue;
                End;

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
            End
          Else
            Begin
              LookupEntOfs := LookupEntOfs + fsc_BLOBLookupEntrySize;
              LookupEntry := @LookupSegBlk^[LookupEntOfs];
              ContentSegOfs := ContentSegPtr^.bshNextSegment;
            End;
        End;

      { If the BLOB has grown, we need to update its length.
        NOTE: BLOBs can't be truncated via a write operation. }
      If (NewSize > BLOBHeader^.bbhBLOBLength) Then
        BLOBHeader^.bbhBLOBLength := NewSize;
    Finally
      If (LookupSegBlk <> Nil) Then
        aLkpRelMethod(LookupSegBlk);
      For OffsetInBlock := 0 To (aRelList.Count - 1) Do
        FFDeallocReleaseInfo(aRelList[OffsetInBlock]);
      aRelList.Free;
    End;
    {$IFDEF BLOBTrace}
  Except
    On E: Exception Do
      Begin
        Logbt('*** FFTblWriteBLOB Error: %s', [E.Message]);
        Raise;
      End;
  End;
  {$ENDIF}
End;
{====================================================================}

{====================================================================}
Initialization
  FFBLOBEngine := TffBLOBEngine.Create;

  {$IFDEF BLOBTrace}
  btLog := TffEventLog.Create(Nil);
  btLog.FileName := 'BLOBTrace.lg';
  btLog.Enabled := True;
  {$ENDIF}

Finalization
  FFBLOBEngine.Free;
  {$IFDEF BLOBTrace}
  btLog.Flush;
  btLog.Free;
  {$ENDIF}

  {End !!.11}
End.

