{*********************************************************}
{* FlashFiler: Sort Engine classes                       *}
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

unit ffsrsort;

interface

uses
  ffllbase,
  fflldict,
  ffsrcur,
  ffsreng,
  ffsrixhl;

const
  ffcl_MergeSortBufferSize : Longint = ffcl_1MB;
  ffcl_MergeOrder = 5;
    { # of merge files from which we are retrieving records at any one time. }

type
  TffSrSortEngineClass = class of TffSrBaseSortEngine;

  TffSrSortState = (ffspEmpty, ffspPutting, ffspGetting);

  { The following record type holds the information pertinent to each field
    on which the records are being sorted. }
  TffSrSortFieldInfo = packed record
    fldDescriptor : TffFieldDescriptor;
    fldInxHelper  : TffSrIndexHelper;
    fldLength     : Longint;
    fldNoCase     : boolean;
    fldOrderDir   : TffOrderByDirection;
  end;

  PffSrSortArray = ^TffSrSortArray;
  TffSrSortArray = array[0..ffcl_MaxIndexFlds] of TffSrSortFieldInfo;

  PffSortBuffer = ^TffSortBuffer;
  TffSortBuffer = array[0..ffcl_1MB] of byte;

  TffSrBaseSortEngine = class(TffObject)
  protected
    bsDB : TffSrDatabase;
      {-The database containing the sorted table. }

    bsDict : TffDataDictionary;
      {-The dictionary describing the sorted table. }

    bsEngine : TffServerEngine;
      {-The engine managing the database & table. }
      
    bsNumFields : integer;
      {-Number of fields on which sort is taking place. }

    bsRecLen : Longint;
      {-The length of each record. }

    bsSortInfo : PffSrSortArray;
      {-The set of sorting information required by the sorting engine. }

  public
    constructor Create(anEngine : TffServerEngine;
                       aDB : TffSrDatabase;
                       aFieldsArray : TffFieldList;
                 const aOrderByArray : TffOrderByArray;
                 const aNumFields : integer;
                       aDict : TffDataDictionary;
                 const aIndexID : integer); virtual;
      { Note: Creator is responsible for freeing the memory associated with
        aFieldsArray & aOrderByArray. }

    destructor Destroy; override;

    function Get(aRecord : PffByteArray) : boolean; virtual; abstract;
    function Put(aRecord : PffByteArray) : TffResult; virtual; abstract;
  end;

  { This class performs a merge sort on a set of records. As methods are fed
    to the engine via the Put method, this class places the records within a
    buffer. When the buffer is full, the engine quick sorts the records and
    writes them to a temporary cursor.

    The merge sort does not occur until the Get method is first called.

    When the Get method is first used to retrieve the sorted records, the
    engine sorts the current buffer of records. It then ... }
  TffSrMergeSortEngine = class(TffSrBaseSortEngine)
  protected
    msBuffer : PffSortBuffer;
      {-The run buffer used to cache unsorted records during Put phase. }

    msBufferOffset : Longint;
      {-The current offset into the buffer. }

    msCursorList : TffSrCursorList;
      {-List of the cursor's containing the sorted buffers. }

    msCursorOnRec : array[0..pred(ffcl_MergeOrder)] of boolean;
      {-Each element in this array has a one-to-one correspondence with the
        elements in msMergeCursor. If an element in this array is set to True
        then the corresponding cursor is positioned on a record that is to be
        used for comparison. If the element is set to False then the cursor
        is not set on a record to be used for comparison. }

    msMaxRecCount : Longint;
      {-The maximum number of records that may be held in the buffer. }

    msMergeCursor : array[0..pred(ffcl_MergeOrder)] of TffSrSimpleCursor;
      {-Array of cursors used for merging. }

    msMergeCursorCount : integer;
      {-The number of cursors involved in one stage of merging. }

    msOutputCursor : TffSrSimpleCursor;
      {-Cursor to which merged records are written. }

    msOutputStoreSize : TffWord32;
      {-The calculated size for the output cursor's temporary storage file.
        Calculated in msSetMergeCursors. }

    msPivotBuffer : PffByteArray;
      {-Holds the pivot element during the quick sort. }

    msRecBuffer : PffByteArray;
      {-Used to temporarily hold a record while it is being swapped with
        another record. }

    msRecCount : Longint;
      {-The number of records currently in msBuffer. }

    msRunIndex : Longint;
      {-When a small number of records (i.e., fewer than can be stored in the
        run buffer) are added to the engine, this variable serves as an index
        into the run buffer during the Get phase. We retrieve the sorted
        records from the run buffer instead of using any merge files. }

    msState : TffSrSortState;
      {-The state of the sort engine. }

    msTotalCount : TffWord32;
      {-The total number of records added to the sort engine. }

    { Protected methods }
    function msCompRecs(PRec1, PRec2 : PffBytearray) : integer;
      {-Used to compare two records. }

    function msFinalizeBuffer(const WriteToCursor : boolean) : TffResult;
      {-Called when the in-memory run buffer is ready to be sorted and written
        to a temporary cursor. If the run buffer is to be written to a
        temporary cursor, set WriteToCursor to True. }

    procedure msGetNextRecord(aRecord : PffByteArray);
      {-Finds the next record that should be written to the
        output cursor. It pulls the records from a number of input cursors
        that are being merged. }

    procedure msMerge;
      {-Merges all the temporary cursors until there are ffcl_MergeOrder or
        fewer temporary cursors left. }

    procedure msMergeCursors;
      {-Used to merge a number of cursors into an output cursor. }

    procedure msNextRecord(const aCursorIndex : integer);
      {-Positions a specific merge cursor to its next record. If the merge
        cursor reaches EOF then this routine closes the cursor and adjusts
        the msMergeCursor array. }
        
    procedure msSetMergeCursors;
      {-Determines which cursors are to be used for merging. }

    procedure msSortBuffer;
      {-Uses non-recursive quick sort algorithm to sort the in-memory record
        buffer. The quick sort algorithm calculates the Median Of Three method
        to calculate the pivot element. }

    procedure msSwapRecs(Rec1, Rec2 : Longint);
      {-Used to swap two records within the in-memory buffer. }

  public
    constructor Create(anEngine : TffServerEngine;
                       aDB : TffSrDatabase;
                       aFieldsArray : TffFieldList;
                 const aOrderByArray : TffOrderByArray;
                 const aNumFields : integer;
                       aDict : TffDataDictionary;
                 const aIndexID : integer); override;
      { Note: Creator is responsible for freeing the memory associated with
        aIndexHelperArray & aOrderByArray. }

    destructor Destroy; override;

    function Get(aRecord : PffByteArray) : boolean; override;
    function Put(aRecord : PffByteArray) : TffResult; override;
  end;

var
  ffcSortEngineClass : TffSrSortEngineClass = TffSrMergeSortEngine;
    { The type of sort engine to be used by the server engine. }

implementation

uses
  sysutils,
  ffllexcp,
  ffsrbase,
  ffsrbde,
  ffsrlock;

{===TffSrBaseSortEngine==============================================}
constructor TffSrBaseSortEngine.Create(anEngine : TffServerEngine;
                                       aDB : TffSrDatabase;
                                       aFieldsArray : TffFieldList;
                                 const aOrderByArray : TffOrderByArray;
                                 const aNumFields : integer;
                                       aDict : TffDataDictionary;
                                 const aIndexID : integer);
var
  FldInx, Index : integer;
begin
  inherited Create;
  bsDB := aDB;
  bsDict := aDict;
  bsEngine := anEngine;
  bsNumFields := aNumFields;
  bsRecLen := aDict.RecordLength;

  { Build the set of sorting information. }
  FFGetMem(bsSortInfo, SizeOf(TffSrSortFieldInfo) * bsNumFields);
  for Index := 0 to pred(aNumFields) do begin
    FldInx := aFieldsArray[Index];
    with bsSortInfo^[Index] do begin
      fldDescriptor := aDict.FieldDescriptor[FldInx]^;
      fldInxHelper := aDict.IndexHelpers[aIndexID, Index];
      fldLength := aDict.FieldLength[FldInx];
      fldNoCase := aDict.IndexIsCaseInsensitive[aIndexID];
      fldOrderDir := aOrderByArray[Index];
    end;
  end;
end;
{--------}
destructor TffSrBaseSortEngine.destroy;
begin
  if assigned(bsSortInfo) then
    FFFreeMem(bsSortInfo, SizeOf(TffSrSortFieldInfo) * bsNumFields);
  inherited Destroy;
end;
{====================================================================}

{===TffSrMergeSortEngine=============================================}
constructor TffSrMergeSortEngine.Create(anEngine : TffServerEngine;
                                        aDB : TffSrDatabase;
                                        aFieldsArray : TffFieldList;
                                  const aOrderByArray : TffOrderByArray;
                                  const aNumFields : integer;
                                        aDict : TffDataDictionary;
                                  const aIndexID : integer);
begin
  inherited Create(anEngine, aDB, aFieldsArray, aOrderByArray, aNumFields,
                   aDict, aIndexID);
  FFGetMem(msBuffer, ffcl_MergeSortBufferSize);
  FfGetMem(msPivotBuffer, bsRecLen);
  FFGetMem(msRecBuffer, bsRecLen);
  msBufferOffset := 0;
  msCursorList := TffSrCursorList.Create;
  msMaxRecCount := ffcl_MergeSortBufferSize div bsRecLen;
  msOutputStoreSize := ffcl_MergeSortBufferSize * ffcl_MergeOrder;
    { Default value. Not really used. }
  msRecCount := 0;
  msState := ffspEmpty;
  msTotalCount := 0;
end;
{--------}
destructor TffSrMergeSortEngine.Destroy;
var
  aCursor : TffSrBaseCursor;
  Index : Longint;
begin
  if assigned(msBuffer) then
    FFFreeMem(msBuffer, ffcl_MergeSortBufferSize);
  if assigned(msPivotBuffer) then
    FFFreeMem(msPivotBuffer, bsRecLen);
  if assigned(msRecBuffer) then
    FFFreeMem(msRecBuffer, bsRecLen);
  if assigned(msCursorList) then begin
    for Index := pred(msCursorList.CursorCount) downto 0 do begin
      aCursor := msCursorList.Cursor[ftFromIndex, Index];
      msCursorList.DeleteCursor(aCursor.CursorID);
    end;
    msCursorList.Free;
  end;
  inherited;
end;
{--------}
function TffSrMergeSortEngine.Get(aRecord : PffByteArray) : boolean;
var
  aStatus : TffResult;
begin
  Result := false;

  { Is this the first get? }
  if msState <> ffspGetting then begin
    { Yes. }
    msState := ffspGetting;
    { Any records in the run buffer? }
    if msRecCount > 0 then begin
      { Yes. Sort the run buffer. Write to temp cursor only if we have
        written other temporary cursors. This is a performance optimization.
        If there aren't enough records to fill a run buffer then we will
        just quick sort them and retrieve them from the run buffer. }
      aStatus := msFinalizeBuffer(msCursorList.CursorCount > 0);
      if aStatus <> DBIERR_NONE then
        FFRaiseException(EffException, ffStrResServer, aStatus,
                         ['TffSrMergeSortEngine.Get']);

      { Do we have some temporary cursors? }
      if msCursorList.CursorCount > 0 then
        { Yes. Merge them until they are whittled down to ffcl_MergeOrder files
          in number. }
        msMerge
      else
        { No. But we do have records in the run buffer. Init an index into the
          run buffer. }
        msRunIndex := 0;
    end
    { Any records at all? }
    else if msTotalCount = 0 then
      { No. Nothing to sort. }
      Exit;
  end;

  { Get next record from merge files? }
  if msMergeCursorCount > 0 then begin
    { Yes. }
    msGetNextRecord(aRecord);
    Result := True;
  end
  else if msRunIndex < msRecCount then begin
    { No. Not enough records for a merge file. Retrieve the next record from
      the run buffer. }
    Move(msBuffer^[msRunIndex * bsRecLen], aRecord^, bsRecLen);
    inc(msRunIndex);
    Result := True;
  end;

end;
{--------}
function TffSrMergeSortEngine.msCompRecs(PRec1, PRec2 : PffByteArray) : integer;
var
  Fld1Null, Fld2Null : boolean;
  Index : integer;
  Offset : Longint;
  SortInfo : TffSrSortFieldInfo;
begin
  Result := 0;
  Index := 0;
    
  { Compare each field until we see a non-zero result. }
  while (Result = 0) and (Index < bsNumFields) do begin
    SortInfo := bsSortInfo^[Index];

    { Is either field a null? }
    Fld1Null := bsDict.IsRecordFieldNull(SortInfo.fldDescriptor.fdNumber, PRec1);
    Fld2Null := bsDict.IsRecordFieldNull(SortInfo.fldDescriptor.fdNumber, PRec2);
    if Fld1Null then begin
      if Fld2Null then
        Result := 0
      else
        Result := -1;
    end
    else if Fld2Null then
      Result := 1
    else begin
      Offset := bsSortInfo^[Index].fldDescriptor.fdOffset;
      Result := bsSortInfo^[Index].fldInxHelper.CompareKey
                  (PRec1^[Offset], PRec2^[Offset],
                   @bsSortInfo^[Index].fldDescriptor,
                   bsSortInfo^[Index].fldLength, bsSortInfo^[Index].fldNoCase);
    end;

    { The compare function always compares in ascending fashion. If this is
      to be ordered in descending fashion and our result is non-zero, flip
      some bits. }
    if bsSortInfo^[Index].fldOrderDir = ffobDescending then
      Result := -Result;
    inc(Index);
  end;
end;
{--------}
function TffSrMergeSortEngine.msFinalizeBuffer(const WriteToCursor : boolean) : TffResult;
var
  Cursor : TffSrSimpleCursor;
  Index : Longint;
begin
  Result := DBIERR_NONE;

  { Sort the buffer. }
  msSortBuffer;

  if WriteToCursor then begin
    { Write the records to a temporary file. }
    Cursor := TffSrSimpleCursor.Create(bsEngine, bsDB, FFGetRemainingTime);
    Cursor.Build('', bsDict, omReadWrite, smExclusive, false, true,
                 [fffaTemporary, fffaBLOBChainSafe], ffcl_MergeSortBufferSize); {!!.05}
    Cursor.CloseTable := True;
    for Index := 0 to pred(msRecCount) do begin
      Result := Cursor.InsertRecord(@msBuffer^[Index * bsRecLen], ffsltNone);
      if Result <> DBIERR_NONE then
        Exit;
    end;

    { Add this cursor to our list of temporary files. }
    msCursorList.AddCursor(Cursor);

    { Zero out the buffer. }
    FillChar(msBuffer^, ffcl_MergeSortBufferSize, 0);

    msRecCount := 0;
  end;

end;
{--------}
procedure TffSrMergeSortEngine.msGetNextRecord(aRecord : PffByteArray);
var
  aResult : TffResult;
  Index, Index2 : integer;
begin
  { Assumption: Each cursor in the merge is positioned on a record. If a cursor
    reaches EOF then it is closed before we access it again. }

  { Get record for first cursor. }
  aResult := msMergeCursor[0].GetRecord(aRecord, ffsltNone);
  Index := 0;

  { Did an error occur? }
  if (aResult <> DBIERR_NONE) then
    { Yes. Raise an exception. }
    FFRaiseException(EffException, ffStrResServer, aResult,
                     ['msGetNextRecord.1']);

  { Compare the records from the other cursors. }
  for Index2 := 1 to pred(msMergeCursorCount) do begin
    aResult := msMergeCursor[Index2].GetRecord(msRecBuffer, ffsltNone);
    { Did an error occur? }
    if (aResult <> DBIERR_NONE) then
      { Yes. Raise an exception. }
      FFRaiseException(EffException, ffStrResServer, aResult,
                       ['msGetNextRecord.2']);

    { Should this record be before the current record? }
    if msCompRecs(msRecBuffer, aRecord) < 0 then begin
      { Yes. Copy the record. }
      Move(msRecBuffer^, aRecord^, bsRecLen);
      Index := Index2;
    end;
  end;

  { By this point, we have found the next record. Move the cursor from which
    the record was obtained to its next record. Note that this action may
    result in the closing of the cursor. }
  msNextRecord(Index);

end;
{--------}
procedure TffSrMergeSortEngine.msMerge;
begin
  { While we have more cursors to merge than the merge order, do some work. }
  while msCursorList.CursorCount > ffcl_MergeOrder do begin
    { Get some cursors to merge. }
    msSetMergeCursors;
    { Create an output cursor & add it to the cursor list.  The records from
      the merged cursors will go to the output cursor. }
    msOutputCursor := TffSrSimpleCursor.Create(bsEngine, bsDB,
                                               FFGetRemainingTime);
    msOutputCursor.Build('', bsDict, omReadWrite, smExclusive,
                         false, true, [fffaTemporary, fffaBLOBChainSafe],  {!!.05}
                         msOutputStoreSize);                               {!!.05}
    msOutputCursor.CloseTable := True;
    msCursorList.AddCursor(msOutputCursor);
    { Merge the input cursors into the output cursor. }
    msMergeCursors;
  end;
  msSetMergeCursors;
end;
{--------}
procedure TffSrMergeSortEngine.msMergeCursors;
var
  aRecord : PffByteArray;
  aStatus : TffResult;
  aStr : string;
begin
  FFGetMem(aRecord, bsRecLen);
  try
    try
      while True do begin
        { Find next record for output cursor. Did we find a record? }
        msGetNextRecord(aRecord);
        { Send to output cursor. }
        aStatus := msOutputCursor.InsertRecord(aRecord, ffsltNone);
        if aStatus <> DBIERR_NONE then
          FFRaiseException(EffException, ffStrResServer, aStatus,
                           ['msMergeCursors']);
        { All records merged? }
        if msMergeCursorCount = 0 then
          break;
      end;
    except
     on E:Exception do begin
       aStr := E.message;
     end;
    end;
  finally
    FFFreeMem(aRecord, bsRecLen);
  end;
end;
{--------}
procedure TffSrMergeSortEngine.msNextRecord(const aCursorIndex : integer);
var
  aResult : TffResult;
begin
  aResult := msMergeCursor[aCursorIndex].GetNextRecord(msRecBuffer, ffsltNone);
  if aResult = DBIERR_EOF then begin
    { Close the cursor. }
    msCursorList.DeleteCursor(msMergeCursor[aCursorIndex].CursorID);

    { Move the last cursor to this position. }
    msMergeCursor[aCursorIndex] := msMergeCursor[pred(msMergeCursorCount)];
    dec(msMergeCursorCount);
  end;
end;
{--------}
procedure TffSrMergeSortEngine.msSetMergeCursors;
var
  aCount : Longint;
  aCursor : TffSrSimpleCursor;
  RecsPerBlock : Longint;
begin
  msMergeCursorCount := 0;
  msOutputStoreSize := 0;

  RecsPerBlock := (64 * 1024) div bsRecLen;

  { Obtain a merge cursor while we have not exceeded the merge order and
    while we have not exceeded the number of temporary cursors. }
  while (msMergeCursorCount < ffcl_MergeOrder) and
        (msMergeCursorCount < msCursorList.CursorCount) do begin
    inc(msMergeCursorCount);
    aCursor := TffSrSimpleCursor(msCursorList.Cursor[ftFromIndex,
                                                     pred(msMergeCursorCount)]);
    { Position to first record in each cursor. }
    aCursor.SetToBegin;
    aCursor.GetNextRecord(msRecBuffer, ffsltNone);
    msMergeCursor[pred(msMergeCursorCount)] := aCursor;
    msCursorOnRec[pred(msMergeCursorCount)] := false;
    aCursor.GetRecordCount(aCount);
    { Increment temp store size by # blocks needed to hold the data plus 2
      blocks for header and data dictionary. }
    inc(msOutputStoreSize, (((aCount div RecsPerBlock) + 1) * 64 * 1024) +
                            (2 * 64 * 1024));
  end;
end;
{--------}
procedure TffSrMergeSortEngine.msSortBuffer;
const
  MedianThreshold = 16;
  StackSize = 32;
type
  Stack = array[0..StackSize - 1] of Longint;
var
  L : Longint;            { The left edge, base zero. }
  R : Longint;            { The right edge, base zero. }
  Pl : Longint;           { Left edge within current partition, base zero. }
  Pr : Longint;           { Right edge within current partition, base zero. }
  Pm : Longint;           { Mid-point of current partition. }
  PLen : Longint;         { The size of the current partition. }
  StackP : integer;       { Stack pointer. }
  LStack : Stack;         { Pending partitions, left edge. }
  RStack : Stack;         { Pending partitions, right edge. }
begin
  { Initialize the stack. }
  StackP := 0;
  LStack[0] := 0;
  RStack[0] := msRecCount - 1;

  { Repeatedly take top partition from the stack. }
  repeat
    { Pop the stack. }
    L := LStack[StackP];
    R := RStack[StackP];
    Dec(StackP);

    { Sort the current partition. }
    repeat
      Pl := L;
      Pr := R;
      PLen := Pr - Pl + 1;

      { Calculate the pivot element. }
      Pm := Pl + (PLen shr 1);
      if PLen >= MedianThreshold then begin
        { Sort elements P1, Pm, & Pr. }
        if msCompRecs(@msBuffer^[Pm * bsRecLen], @msBuffer^[Pl * bsRecLen]) < 0 then
          msSwapRecs(Pm, Pl);
        if msCompRecs(@msBuffer^[Pr * bsRecLen], @msBuffer^[Pl * bsRecLen]) < 0 then
          msSwapRecs(Pr, Pl);
        if msCompRecs(@msBuffer^[Pr * bsRecLen], @msBuffer^[Pm * bsRecLen]) < 0 then
          msSwapRecs(Pr, Pm);
        { Exchange Pm with Pr - 1 but use Pm's value as the pivot. }
        msSwapRecs(Pm, Pr - 1);
        Pm := Pr - 1;

        { Reduce range of swapping now that Pl and Pr are in the right
          spots. }
        inc(Pl);
        dec(Pr, 2);
      end;

      { Save the pivot element. }
      Move(msBuffer^[Pm * bsRecLen], msPivotBuffer^, bsRecLen);

      { Swap items in sort order around the pivot. }
      repeat
        while msCompRecs(@msBuffer^[Pl * bsRecLen], msPivotBuffer) < 0 do
          inc(Pl);
        while msCompRecs(msPivotBuffer, @msBuffer^[Pr * bsRecLen]) < 0 do
          dec(Pr);

        { Have we reached the pivot? }
        if Pl = Pr then begin
          Inc(Pl);
          Dec(Pr);
        end
        else if Pl < Pr then begin
          { No. Swap elements around the pivot. }
          msSwapRecs(Pl, Pr);
          inc(Pl);
          dec(Pr);
        end;
      until Pl > Pr;

      { Decide which partition to sort next. Which partition is bigger? }
      if (Pr - L) < (R - Pl) then begin
        { Left partition is bigger. }
        if Pl < R then begin
          { Stack the request for sorting right partition. }
          inc(StackP);
          LStack[StackP] := Pl;
          RStack[StackP] := R;
        end;
        { Continue sorting left partion. }
        R := Pr;
      end
      else begin
        { Right partition is bigger. }
        if L < Pr then begin
          { Stack the request for sorting left partition. }
          inc(StackP);
          LStack[StackP] := L;
          RStack[StackP] := Pr;
        end;
        { Continue sorting right partition. }
        L := Pl;
      end;
    until L >= R;
  until StackP < 0;
end;
{--------}
procedure TffSrMergeSortEngine.msSwapRecs(Rec1, Rec2 : Longint);
begin
  Move(msBuffer^[Rec1 * bsRecLen], msRecBuffer^, bsRecLen);
  Move(msBuffer^[Rec2 * bsRecLen], msBuffer^[Rec1 * bsRecLen], bsRecLen);
  Move(msRecBuffer^, msBuffer^[Rec2 * bsRecLen], bsRecLen);
end;
{--------}
function TffSrMergeSortEngine.Put(aRecord : PffByteArray) : TffResult;
begin
  Result := DBIERR_NONE;

  { Did we start retrieving? }
  Assert(not (msState = ffspGetting));

  msState := ffspPutting;

  { Is the buffer full? }
  if msRecCount = msMaxRecCount then begin
    Result := msFinalizeBuffer(True);
    msBufferOffset := 0;
  end;

  if Result = DBIERR_NONE then begin
    { Add the record to the buffer. }
    Move(aRecord^, msBuffer^[msBufferOffset], bsRecLen);
    inc(msBufferOffset, bsRecLen);
    inc(msRecCount);
    inc(msTotalCount);
  end;

end;
{====================================================================}
end.
