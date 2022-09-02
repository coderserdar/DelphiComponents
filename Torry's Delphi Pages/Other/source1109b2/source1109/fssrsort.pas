{$I fsdefine.inc}

Unit fssrsort;

Interface

Uses
  fsllbase,
  fslldict,
  fssrcur,
  fsserverclass,
  fsindexhelper;

Const
  fscl_MergeSortBufferSize: Longint = fscl_1MB;
  fscl_MergeOrder = 5;
  { # of merge files from which we are retrieving records at any one time. }

Type
  TffSrSortEngineClass = Class Of TffSrBaseSortEngine;

  TffSrSortState = (ffspEmpty, ffspPutting, ffspGetting);

  { The following record type holds the information pertinent to each field
    on which the records are being sorted. }
  TffSrSortFieldInfo = Packed Record
    fldDescriptor: TffFieldDescriptor;
    fldInxHelper: TfsSrIndexHelper;
    fldLength: Longint;
    fldNoCase: boolean;
    fldOrderDir: TfsOrderByDirection;
    fldNullTop: Boolean;
    fldSizeCompare: Integer;
  End;

  PffSrSortArray = ^TffSrSortArray;
  TffSrSortArray = Array[0..fscl_MaxIndexFlds] Of TffSrSortFieldInfo;

  PffSortBuffer = ^TffSortBuffer;
  TffSortBuffer = Array[0..fscl_1MB] Of Byte;

  TffSrBaseSortEngine = Class(TFSSpecObject)
  Protected
    bsDB: TfsSrcDatabase;
    {-The database containing the sorted table. }

    bsDict: TFSInfoDict;
    {-The dictionary describing the sorted table. }

    bsEngine: TFSServer;
    {-The engine managing the database & table. }

    bsNumFields: Integer;
    {-Number of fields on which sort is taking place. }

    bsRecLen: Longint;
    {-The length of each record. }

    bsSortInfo: PffSrSortArray;
    {-The set of sorting information required by the sorting engine. }

  Public
    FUserName: TffName;
    Constructor Create(anEngine: TFSServer;
      aDB: TfsSrcDatabase;
      aFieldsArray: TffFieldList;
      Const aOrderByArray: TfsOrderByArray;
      Const aNumFields: Integer;
      aDict: TFSInfoDict;
      Const aIndexID: Integer); Virtual;
    { Note: Creator is responsible for freeing the memory associated with
      aFieldsArray & aOrderByArray. }

    Destructor Destroy; Override;

    Function Get(aRecord: PffByteArray): boolean; Virtual; Abstract;
    Function Put(aRecord: PffByteArray): TffResult; Virtual; Abstract;
  End;

  { This class performs a merge sort on a set of records. As methods are fed
    to the engine via the Put method, this class places the records within a
    buffer. When the buffer is full, the engine quick sorts the records and
    writes them to a temporary cursor.

    The merge sort does not occur until the Get method is first called.

    When the Get method is first used to retrieve the sorted records, the
    engine sorts the current buffer of records. It then ... }
  TffSrMergeSortEngine = Class(TffSrBaseSortEngine)
  Protected
    msBuffer: PffSortBuffer;
    {-The run buffer used to cache unsorted records during Put phase. }

    msBufferOffset: Longint;
    {-The current offset into the buffer. }

    msCursorList: TfsSrcCursorList;
    {-List of the cursor's containing the sorted buffers. }

    msCursorOnRec: Array[0..pred(fscl_MergeOrder)] Of boolean;
    {-Each element in this array has a one-to-one correspondence with the
      elements in msMergeCursor. If an element in this array is set to True
      then the corresponding cursor is positioned on a record that is to be
      used for comparison. If the element is set to False then the cursor
      is not set on a record to be used for comparison. }

    msMaxRecCount: Longint;
    {-The maximum number of records that may be held in the buffer. }

    msMergeCursor: Array[0..pred(fscl_MergeOrder)] Of TfsSrcSimpleCursor;
    {-Array of cursors used for merging. }

    msMergeCursorCount: Integer;
    {-The number of cursors involved in one stage of merging. }

    msOutputCursor: TfsSrcSimpleCursor;
    {-Cursor to which merged records are written. }

    msOutputStoreSize: TffWord32;
    {-The calculated size for the output cursor's temporary storage file.
      Calculated in msSetMergeCursors. }

    msPivotBuffer: PffByteArray;
    {-Holds the pivot element during the quick sort. }

    msRecBuffer: PffByteArray;
    {-Used to temporarily hold a record while it is being swapped with
      another record. }

    msRecCount: Longint;
    {-The number of records currently in msBuffer. }

    msRunIndex: Longint;
    {-When a small number of records (i.e., fewer than can be stored in the
      run buffer) are added to the engine, this variable serves as an index
      into the run buffer during the Get phase. We retrieve the sorted
      records from the run buffer instead of using any merge files. }

    msState: TffSrSortState;
    {-The state of the sort engine. }

    msTotalCount: TffWord32;
    {-The total number of records added to the sort engine. }

  { Protected methods }
    Function msCompRecs(PRec1, PRec2: PffBytearray): Integer;
    {-Used to compare two records. }

    Function msFinalizeBuffer(Const WriteToCursor: boolean): TffResult;
    {-Called when the in-memory run buffer is ready to be sorted and written
      to a temporary cursor. If the run buffer is to be written to a
      temporary cursor, set WriteToCursor to True. }

    Procedure msGetNextRecord(aRecord: PffByteArray);
    {-Finds the next record that should be written to the
      output cursor. It pulls the records from a number of input cursors
      that are being merged. }

    Procedure msMerge;
    {-Merges all the temporary cursors until there are fscl_MergeOrder or
      fewer temporary cursors left. }

    Procedure msMergeCursors;
    {-Used to merge a number of cursors into an output cursor. }

    Procedure msNextRecord(Const aCursorIndex: Integer);
    {-Positions a specific merge cursor to its next record. If the merge
      cursor reaches EOF then this routine closes the cursor and adjusts
      the msMergeCursor array. }

    Procedure msSetMergeCursors;
    {-Determines which cursors are to be used for merging. }

    Procedure msSortBuffer;
    {-Uses non-recursive quick sort algorithm to sort the in-memory record
      buffer. The quick sort algorithm calculates the Median Of Three method
      to calculate the pivot element. }

    Procedure msSwapRecs(Rec1, Rec2: Longint);
    {-Used to swap two records within the in-memory buffer. }

  Public
    Constructor Create(anEngine: TFSServer;
      aDB: TfsSrcDatabase;
      aFieldsArray: TffFieldList;
      Const aOrderByArray: TfsOrderByArray;
      Const aNumFields: Integer;
      aDict: TFSInfoDict;
      Const aIndexID: Integer); Override;
    { Note: Creator is responsible for freeing the memory associated with
      aIndexHelperArray & aOrderByArray. }

    Destructor Destroy; Override;

    Function Get(aRecord: PffByteArray): boolean; Override;
    Function Put(aRecord: PffByteArray): TffResult; Override;
  End;

Var
  ffcSortEngineClass: TffSrSortEngineClass = TffSrMergeSortEngine;
  { The type of sort engine to be used by the server engine. }

Implementation

Uses
  SysUtils,
  fsllexcp,
  fssrbase,
  fssrbde,
  fssrlock;

{===TffSrBaseSortEngine==============================================}

Constructor TffSrBaseSortEngine.Create(anEngine: TFSServer;
  aDB: TfsSrcDatabase;
  aFieldsArray: TffFieldList;
  Const aOrderByArray: TfsOrderByArray;
  Const aNumFields: Integer;
  aDict: TFSInfoDict;
  Const aIndexID: Integer);
Var
  FldInx, Index: Integer;
Begin
  Inherited Create;
  bsDB := aDB;
  bsDict := aDict;
  bsEngine := anEngine;
  bsNumFields := aNumFields;
  bsRecLen := aDict.RecordLength;

  { Build the set of sorting information. }
  FFGetMem(bsSortInfo, SizeOf(TffSrSortFieldInfo) * bsNumFields);
  For Index := 0 To pred(aNumFields) Do
    Begin
      FldInx := aFieldsArray[Index];
      With bsSortInfo^[Index] Do
        Begin
          fldDescriptor := aDict.FieldDescriptor[FldInx]^;
          fldInxHelper := aDict.IndexHelpers[aIndexID, Index];
          fldLength := aDict.FieldLength[FldInx];
          fldNoCase := Boolean(aDict.IndexDescriptor^[aIndexID]^.idFieldsCase[Index]);
          fldOrderDir := aOrderByArray[Index];
          fldNullTop := Boolean(aDict.IndexDescriptor^[aIndexID]^.idFieldsNullTop[Index]);
          fldSizeCompare := aDict.IndexDescriptor^[aIndexID]^.idFieldsSize[Index];
        End;
    End;
End;
{--------}

Destructor TffSrBaseSortEngine.Destroy;
Begin
  If assigned(bsSortInfo) Then
    FFFreeMem(bsSortInfo, SizeOf(TffSrSortFieldInfo) * bsNumFields);
  Inherited Destroy;
End;
{====================================================================}

{===TffSrMergeSortEngine=============================================}

Constructor TffSrMergeSortEngine.Create(anEngine: TFSServer;
  aDB: TfsSrcDatabase;
  aFieldsArray: TffFieldList;
  Const aOrderByArray: TfsOrderByArray;
  Const aNumFields: Integer;
  aDict: TFSInfoDict;
  Const aIndexID: Integer);
Begin
  Inherited Create(anEngine, aDB, aFieldsArray, aOrderByArray, aNumFields,
    aDict, aIndexID);
  FFGetMem(msBuffer, fscl_MergeSortBufferSize);
  FfGetMem(msPivotBuffer, bsRecLen);
  FFGetMem(msRecBuffer, bsRecLen);
  msBufferOffset := 0;
  msCursorList := TfsSrcCursorList.Create;
  msMaxRecCount := fscl_MergeSortBufferSize Div bsRecLen;
  msOutputStoreSize := fscl_MergeSortBufferSize * fscl_MergeOrder;
  { Default value. Not really used. }
  msRecCount := 0;
  msState := ffspEmpty;
  msTotalCount := 0;
End;
{--------}

Destructor TffSrMergeSortEngine.Destroy;
Var
  aCursor: TfsSrBaseCursor;
  Index: Longint;
Begin
  If assigned(msBuffer) Then
    FFFreeMem(msBuffer, fscl_MergeSortBufferSize);
  If assigned(msPivotBuffer) Then
    FFFreeMem(msPivotBuffer, bsRecLen);
  If assigned(msRecBuffer) Then
    FFFreeMem(msRecBuffer, bsRecLen);
  If assigned(msCursorList) Then
    Begin
      For Index := pred(msCursorList.CursorCount) Downto 0 Do
        Begin
          aCursor := msCursorList.Cursor[ftFromIndex, Index];
          msCursorList.DeleteCursor(aCursor.CursorID);
        End;
      msCursorList.Free;
    End;
  Inherited;
End;
{--------}

Function TffSrMergeSortEngine.Get(aRecord: PffByteArray): boolean;
Var
  aStatus: TffResult;
Begin
  Result := False;

  { Is this the first get? }
  If msState <> ffspGetting Then
    Begin
      { Yes. }
      msState := ffspGetting;
      { Any records in the run buffer? }
      If msRecCount > 0 Then
        Begin
          { Yes. Sort the run buffer. Write to temp cursor only if we have
            written other temporary cursors. This is a performance optimization.
            If there aren't enough records to fill a run buffer then we will
            just quick sort them and retrieve them from the run buffer. }
          aStatus := msFinalizeBuffer(msCursorList.CursorCount > 0);
          If aStatus <> DBIERR_NONE Then
            FSRaiseException(EfsException, fsStrResServer, aStatus,
              ['TffSrMergeSortEngine.Get']);

          { Do we have some temporary cursors? }
          If msCursorList.CursorCount > 0 Then
            { Yes. Merge them until they are whittled down to fscl_MergeOrder files
              in number. }
            msMerge
          Else
            { No. But we do have records in the run buffer. Init an index into the
              run buffer. }
            msRunIndex := 0;
        End
          { Any records at all? }
      Else If msTotalCount = 0 Then
        { No. Nothing to sort. }
        Exit;
    End;

  { Get next record from merge files? }
  If msMergeCursorCount > 0 Then
    Begin
      { Yes. }
      msGetNextRecord(aRecord);
      Result := True;
    End
  Else If msRunIndex < msRecCount Then
    Begin
      { No. Not enough records for a merge file. Retrieve the next record from
        the run buffer. }
      Move(msBuffer^[msRunIndex * bsRecLen], aRecord^, bsRecLen);
      inc(msRunIndex);
      Result := True;
    End;

End;
{--------}

Function TffSrMergeSortEngine.msCompRecs(PRec1, PRec2: PffByteArray): Integer;
Var
  Fld1Null, Fld2Null: boolean;
  Index: Integer;
  Offset, FieldsSize, LenToUse: Longint;
  SortInfo: TffSrSortFieldInfo;
  CurNoCase: boolean;
  NullTop: boolean;
Begin
  Result := 0;
  Index := 0;

  { Compare each field until we see a non-zero result. }
  While (Result = 0) And (Index < bsNumFields) Do
    Begin
      SortInfo := bsSortInfo^[Index];

      { Is either field a null? }
      Fld1Null := bsDict.IsRecordFieldNull(SortInfo.fldDescriptor.fdNumber, PRec1);
      Fld2Null := bsDict.IsRecordFieldNull(SortInfo.fldDescriptor.fdNumber, PRec2);

      CurNoCase := SortInfo.fldNoCase;
      NullTop := SortInfo.fldNullTop;
      FieldsSize := SortInfo.fldSizeCompare;
      If FieldsSize > SortInfo.fldLength Then
        FieldsSize := SortInfo.fldLength;

      If Fld1Null And Fld2Null Then
        Result := 0
      Else
        Begin
          If Fld1Null Then
            Begin
              If Fld2Null Then
                Result := 0
              Else If NullTop Then
                Result := -1
              Else
                Result := 1;
            End
          Else If Fld2Null Then
            Begin
              If NullTop Then
                Result := 1
              Else
                Result := -1;
            End
          Else
            Begin
              Offset := bsSortInfo^[Index].fldDescriptor.fdOffset;

              If (FieldsSize > 0) And (SortInfo.fldDescriptor.fdType In [fstShortString, fstVarNullString,
                fstVarWideString, fstNullString, fstWideString {, fstUnicode}]) Then
                Begin
                  If (SortInfo.fldDescriptor.fdType In [fstWideString, fstVarWideString {, fstUnicode}]) Then
                    LenToUse := sizeof(WideChar) * (FieldsSize)
                  Else If (SortInfo.fldDescriptor.fdType In [fstShortString]) Then
                    LenToUse := FieldsSize + 1
                  Else If (SortInfo.fldDescriptor.fdType In [fstVarNullString, fstNullString]) Then
                    LenToUse := FieldsSize
                  Else
                    LenToUse := -1;
                  Result := bsSortInfo^[Index].fldInxHelper.CompareKey
                    (PRec1^[Offset], PRec2^[Offset],
                    @bsSortInfo^[Index].fldDescriptor,
                    LenToUse, CurNoCase);
                End
              Else
                Result := bsSortInfo^[Index].fldInxHelper.CompareKey
                  (PRec1^[Offset], PRec2^[Offset],
                  @bsSortInfo^[Index].fldDescriptor,
                  bsSortInfo^[Index].fldLength, bsSortInfo^[Index].fldNoCase);
              { The compare function always compares in ascending fashion. If this is
              to be ordered in descending fashion and our result is non-zero, flip
              some bits. }
              If bsSortInfo^[Index].fldOrderDir = fsobDescending Then
                Result := -Result;
            End;
        End;
      inc(Index);
    End;
End;
{--------}

Function TffSrMergeSortEngine.msFinalizeBuffer(Const WriteToCursor: boolean): TffResult;
Var
  aRefNr: TffInt64;
  Cursor: TfsSrcSimpleCursor;
  Index: Longint;
Begin
  Result := DBIERR_NONE;

  { Sort the buffer. }
  msSortBuffer;

  If WriteToCursor Then
    Begin
      { Write the records to a temporary file. }
      Cursor := TfsSrcSimpleCursor.Create(bsEngine, bsDB, FFGetRemainingTime);
      Cursor.Build('', bsDict, omReadWrite, smExclusive, False, True,
        [fffaTemporary, fffaBLOBChainSafe], fscl_MergeSortBufferSize); {!!.05}
      Cursor.CloseTable := True;
      For Index := 0 To pred(msRecCount) Do
        Begin
          Result := Cursor.InsertRecord(@msBuffer^[Index * bsRecLen], ffsltNone, 0, aRefNr);
          If Result <> DBIERR_NONE Then
            Exit;
        End;

      { Add this cursor to our list of temporary files. }
      msCursorList.AddCursor(Cursor);

      { Zero out the buffer. }
      FillChar(msBuffer^, fscl_MergeSortBufferSize, 0);

      msRecCount := 0;
    End;

End;
{--------}

Procedure TffSrMergeSortEngine.msGetNextRecord(aRecord: PffByteArray);
Var
  aFlag: Byte;
  aRefNr: TffInt64;
  aResult: TffResult;
  Index, Index2: Integer;
Begin
  { Assumption: Each cursor in the merge is positioned on a record. If a cursor
    reaches EOF then it is closed before we access it again. }

  { Get record for first cursor. }
  aResult := msMergeCursor[0].GetRecord(aRecord, ffsltNone, tluDatabase, aflag, aRefNr, False);
  Index := 0;

  { Did an error occur? }
  If (aResult <> DBIERR_NONE) Then
    { Yes. Raise an exception. }
    FSRaiseException(EfsException, fsStrResServer, aResult,
      ['msGetNextRecord.1']);

  { Compare the records from the other cursors. }
  For Index2 := 1 To pred(msMergeCursorCount) Do
    Begin
      aResult := msMergeCursor[Index2].GetRecord(msRecBuffer, ffsltNone, tluDatabase, aflag, aRefNr, False);
      { Did an error occur? }
      If (aResult <> DBIERR_NONE) Then
        { Yes. Raise an exception. }
        FSRaiseException(EfsException, fsStrResServer, aResult,
          ['msGetNextRecord.2']);

      { Should this record be before the current record? }
      If msCompRecs(msRecBuffer, aRecord) < 0 Then
        Begin
          { Yes. Copy the record. }
          Move(msRecBuffer^, aRecord^, bsRecLen);
          Index := Index2;
        End;
    End;

  { By this point, we have found the next record. Move the cursor from which
    the record was obtained to its next record. Note that this action may
    result in the closing of the cursor. }
  msNextRecord(Index);

End;
{--------}

Procedure TffSrMergeSortEngine.msMerge;
Begin
  { While we have more cursors to merge than the merge order, do some work. }
  While msCursorList.CursorCount > fscl_MergeOrder Do
    Begin
      { Get some cursors to merge. }
      msSetMergeCursors;
      { Create an output cursor & add it to the cursor list.  The records from
        the merged cursors will go to the output cursor. }
      msOutputCursor := TfsSrcSimpleCursor.Create(bsEngine, bsDB,
        FFGetRemainingTime);
      msOutputCursor.Build('', bsDict, omReadWrite, smExclusive,
        False, True, [fffaTemporary, fffaBLOBChainSafe], {!!.05}
        msOutputStoreSize); {!!.05}
      msOutputCursor.CloseTable := True;
      msCursorList.AddCursor(msOutputCursor);
      { Merge the input cursors into the output cursor. }
      msMergeCursors;
    End;
  msSetMergeCursors;
End;
{--------}

Procedure TffSrMergeSortEngine.msMergeCursors;
Var
  aRefNr: TffInt64;
  aRecord: PffByteArray;
  aStatus: TffResult;
  aStr: String;
Begin
  FFGetMem(aRecord, bsRecLen);
  Try
    Try
      While True Do
        Begin
          { Find next record for output cursor. Did we find a record? }
          msGetNextRecord(aRecord);
          { Send to output cursor. }
          aStatus := msOutputCursor.InsertRecord(aRecord, ffsltNone, 0, aRefNr);
          If aStatus <> DBIERR_NONE Then
            FSRaiseException(EfsException, fsStrResServer, aStatus,
              ['msMergeCursors']);
          { All records merged? }
          If msMergeCursorCount = 0 Then
            break;
        End;
    Except
      On E: Exception Do
        Begin
          aStr := E.message;
        End;
    End;
  Finally
    FFFreeMem(aRecord, bsRecLen);
  End;
End;
{--------}

Procedure TffSrMergeSortEngine.msNextRecord(Const aCursorIndex: Integer);
Var
  aFlag: Byte;
  aRefNr: TffInt64;
  aResult: TffResult;
Begin
  aResult := msMergeCursor[aCursorIndex].GetNextRecord(msRecBuffer, ffsltNone, aflag, aRefNr);
  If aResult = DBIERR_EOF Then
    Begin
      { Close the cursor. }
      msCursorList.DeleteCursor(msMergeCursor[aCursorIndex].CursorID);

      { Move the last cursor to this position. }
      msMergeCursor[aCursorIndex] := msMergeCursor[pred(msMergeCursorCount)];
      dec(msMergeCursorCount);
    End;
End;
{--------}

Procedure TffSrMergeSortEngine.msSetMergeCursors;
Var
  aFlag: Byte;
  aRefNr: TffInt64;
  aCount: LongWord;
  aCursor: TfsSrcSimpleCursor;
  RecsPerBlock: Longint;
Begin
  msMergeCursorCount := 0;
  msOutputStoreSize := 0;

  RecsPerBlock := (64 * 1024) Div bsRecLen;

  { Obtain a merge cursor while we have not exceeded the merge order and
    while we have not exceeded the number of temporary cursors. }
  While (msMergeCursorCount < fscl_MergeOrder) And
    (msMergeCursorCount < msCursorList.CursorCount) Do
    Begin
      inc(msMergeCursorCount);
      aCursor := TfsSrcSimpleCursor(msCursorList.Cursor[ftFromIndex,
        pred(msMergeCursorCount)]);
      { Position to first record in each cursor. }
      aCursor.SetToBegin;
      aCursor.GetNextRecord(msRecBuffer, ffsltNone, aflag, aRefNr);
      msMergeCursor[pred(msMergeCursorCount)] := aCursor;
      msCursorOnRec[pred(msMergeCursorCount)] := False;
      aCursor.GetRecordCount(aCount);
      { Increment temp store size by # blocks needed to hold the data plus 2
        blocks for header and data dictionary. }
      inc(msOutputStoreSize, (((aCount Div RecsPerBlock) + 1) * 64 * 1024) +
        (2 * 64 * 1024));
    End;
End;
{--------}

Procedure TffSrMergeSortEngine.msSortBuffer;
Const
  MedianThreshold = 16;
  StackSize = 32;
Type
  Stack = Array[0..StackSize - 1] Of Longint;
Var
  L: Longint; { The left edge, base zero. }
  R: Longint; { The right edge, base zero. }
  Pl: Longint; { Left edge within current partition, base zero. }
  Pr: Longint; { Right edge within current partition, base zero. }
  Pm: Longint; { Mid-point of current partition. }
  PLen: Longint; { The size of the current partition. }
  StackP: Integer; { Stack pointer. }
  LStack: Stack; { Pending partitions, left edge. }
  RStack: Stack; { Pending partitions, right edge. }
Begin
  { Initialize the stack. }
  StackP := 0;
  LStack[0] := 0;
  RStack[0] := msRecCount - 1;

  { Repeatedly take top partition from the stack. }
  Repeat
    { Pop the stack. }
    L := LStack[StackP];
    R := RStack[StackP];
    Dec(StackP);

    { Sort the current partition. }
    Repeat
      Pl := L;
      Pr := R;
      PLen := Pr - Pl + 1;

      { Calculate the pivot element. }
      Pm := Pl + (PLen Shr 1);
      If PLen >= MedianThreshold Then
        Begin
          { Sort elements P1, Pm, & Pr. }
          If msCompRecs(@msBuffer^[Pm * bsRecLen], @msBuffer^[Pl * bsRecLen]) < 0 Then
            msSwapRecs(Pm, Pl);
          If msCompRecs(@msBuffer^[Pr * bsRecLen], @msBuffer^[Pl * bsRecLen]) < 0 Then
            msSwapRecs(Pr, Pl);
          If msCompRecs(@msBuffer^[Pr * bsRecLen], @msBuffer^[Pm * bsRecLen]) < 0 Then
            msSwapRecs(Pr, Pm);
          { Exchange Pm with Pr - 1 but use Pm's value as the pivot. }
          msSwapRecs(Pm, Pr - 1);
          Pm := Pr - 1;

          { Reduce range of swapping now that Pl and Pr are in the right
            spots. }
          inc(Pl);
          dec(Pr, 2);
        End;

      { Save the pivot element. }
      Move(msBuffer^[Pm * bsRecLen], msPivotBuffer^, bsRecLen);

      { Swap items in sort order around the pivot. }
      Repeat
        While msCompRecs(@msBuffer^[Pl * bsRecLen], msPivotBuffer) < 0 Do
          inc(Pl);
        While msCompRecs(msPivotBuffer, @msBuffer^[Pr * bsRecLen]) < 0 Do
          dec(Pr);

        { Have we reached the pivot? }
        If Pl = Pr Then
          Begin
            Inc(Pl);
            Dec(Pr);
          End
        Else If Pl < Pr Then
          Begin
            { No. Swap elements around the pivot. }
            msSwapRecs(Pl, Pr);
            inc(Pl);
            dec(Pr);
          End;
      Until Pl > Pr;

      { Decide which partition to sort next. Which partition is bigger? }
      If (Pr - L) < (R - Pl) Then
        Begin
          { Left partition is bigger. }
          If Pl < R Then
            Begin
              { Stack the request for sorting right partition. }
              inc(StackP);
              LStack[StackP] := Pl;
              RStack[StackP] := R;
            End;
          { Continue sorting left partion. }
          R := Pr;
        End
      Else
        Begin
          { Right partition is bigger. }
          If L < Pr Then
            Begin
              { Stack the request for sorting left partition. }
              inc(StackP);
              LStack[StackP] := L;
              RStack[StackP] := Pr;
            End;
          { Continue sorting right partition. }
          L := Pl;
        End;
    Until L >= R;
  Until StackP < 0;
End;
{--------}

Procedure TffSrMergeSortEngine.msSwapRecs(Rec1, Rec2: Longint);
Begin
  Move(msBuffer^[Rec1 * bsRecLen], msRecBuffer^, bsRecLen);
  Move(msBuffer^[Rec2 * bsRecLen], msBuffer^[Rec1 * bsRecLen], bsRecLen);
  Move(msRecBuffer^, msBuffer^[Rec2 * bsRecLen], bsRecLen);
End;
{--------}

Function TffSrMergeSortEngine.Put(aRecord: PffByteArray): TffResult;
Begin
  Result := DBIERR_NONE;

  { Did we start retrieving? }
  Assert(Not (msState = ffspGetting));

  msState := ffspPutting;

  { Is the buffer full? }
  If msRecCount = msMaxRecCount Then
    Begin
      Result := msFinalizeBuffer(True);
      msBufferOffset := 0;
    End;

  If Result = DBIERR_NONE Then
    Begin
      { Add the record to the buffer. }
      Move(aRecord^, msBuffer^[msBufferOffset], bsRecLen);
      inc(msBufferOffset, bsRecLen);
      inc(msRecCount);
      inc(msTotalCount);
    End;

End;
{====================================================================}
End.

