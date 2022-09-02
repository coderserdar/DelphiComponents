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
 * The Original Code is TurboPower B-Tree Filer
 *
 * The Initial Developer of the Original Code is
 * TurboPower Software
 *
 * Portions created by the Initial Developer are Copyright (C) 1996-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Based in part on code written by Ralf Nagel
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

{$DEFINE Heap6}

const
  SIZETYPEMAX      = 16380;

type
  SizeType         = Array[1..SIZETYPEMAX] of Word;
  SizeTypePtr      = ^SizeType;

function AnalyzeEMS(ST : SizeTypePtr; TableSize : Word;
                    RunL,RecL : Word; var PagesUsed : Word) : LongInt;
{-Find Size of items that will fit in EMS memory given the SizeTable from }
{AutoSimulate, the Run Length, Record Length, and free EMS pages.}

var
  PagesFree        : Word;
  PagesPerRun      : Word;
  Pg               : Word;
  Count            : Word;
  RunCt            : Word;
  AE               : LongInt;
  BytesPerRun      : LongInt;
  Continue         : Boolean;

begin
  AnalyzeEMS := 0;
  PagesFree := EMSPagesAvail;   {get number of free pages from EMM}
  if PagesFree = 0 then         {if no free pages then exit}
    Exit;

  PagesUsed := 0;

  {calculate the number of pages used by a single run}
  BytesPerRun := LongInt(RecL) * LongInt(RunL);
  PagesPerRun := Word((LongInt(BytesPerRun) + PAGESIZE - 1) DIV
                 LongInt(PAGESIZE));

  {step through the size table until we either run out of EMS pages or }
  { we don't need anymore space.}
  Count    := 1;
  Continue := TRUE;
  AE       := 0;
  while Continue and (Count <= TableSize) do begin
    RunCt := ST^[Count];
    BytesPerRun := LongInt(RecL) * LongInt(RunL) * LongInt(RunCt);
    PagesPerRun := Word((LongInt(BytesPerRun) + PAGESIZE - 1) DIV
                   LongInt(PAGESIZE));

    Pg := PagesUsed + PagesPerRun;
    if Pg <= PagesFree then begin
      AE := AE + BytesPerRun;
      PagesUsed := Pg;
    end
    else
      Continue  := FALSE;
    Inc(Count);
  end;
  AnalyzeEMS := AE;
end;

function AutoSimulate(SizeInBytes : LongInt; WillUseEMS : Boolean;
                      RunLen,RecLen : Word;
                      var EMSPagesUsed : Word; var Hands : Word) : LongInt;
{-Calculates the amount of disk space needed by simulating the merge process.}
{Accounts for EMS usage (by calling AnalyzeEMS). Since the final merge pass  }
{is not merged to disk, it is not counted. This routine sets up a size table }
{on the heap, and simulates the merge process updating the size table as it  }
{goes. The sum of the largest files existing on disk simultaneously is       }
{calculated, and this is the largest amount of disk space needed during the  }
{merge.}

{If an error occurs, this function will return -1, otherwise the amount of   }
{disk space needed in bytes is returned. }

var
  STSize           : Word;
  MaxST            : Word;
  Files,Peak,Curr  : Word;
  F,HighF,CurF     : Word;
  I                : Word;
  NewSize          : Word;
  AS               : LongInt;
  SizeTable        : SizeTypePtr;

begin
  AutoSimulate := 0; {assume we need no disk space}

  {this tells us the number of files created by splitting the input into}
  {separate files of RunLen}
  F := Word((SizeInBytes DIV RecLen) + RunLen - 1) DIV RunLen;
  Files := F;
  HighF := F;
  CurF  := 1;

  {Figure out the number of file handles we need.  This would never be more}
  {than MERGEORDER + 1}
  if F >= MERGEORDER then
    Hands := MERGEORDER
  else
    Hands := F;
  Inc(Hands);   {add one for output file}

  StSize := SIZETYPEMAX * SizeOf(Word);

  if MaxAvail < (SIZETYPEMAX * SizeOf(Word)) then
    StSize := Word(MaxAvail DIV SizeOf(Word)) * SizeOf(Word);

  MaxST := STSize DIV SizeOf(Word);

  GetMem(SizeTable,StSize);
  FillWord(SizeTable^,MaxST,1);

  while Files > MERGEORDER do begin
    Inc(HighF);                             {increment the high file index}
    if HighF > MaxSt then begin
      {we have run out of space in the SizeTable, so exit}
      AutoSimulate := -1; {indicate that we were unable to calculate size}
      Exit;
    end;

    NewSize := 0;

    {calculate the size of the file created by merging these five files}
    for I := CurF to CurF + MERGEORDER - 1 do
      NewSize := NewSize + SizeTable^[I];

    SizeTable^[HighF] := NewSize;           {set the size of the high file}
    Files := (Files - MERGEORDER) + 1;      {decrement the files}
    CurF  := CurF + MERGEORDER;
  end;
  Peak  := 0;

  for I := CurF - MERGEORDER to HighF do
    Peak := Peak + SizeTable^[I];


  AS := LongInt(Peak) * LongInt(RunLen) * LongInt(RecLen);
  if AS < SizeInBytes then
    AS := SizeInBytes;

  if WillUseEMS then begin
    AS := AS - AnalyzeEMS(SizeTable,HighF,RunLen,RecLen,EMSPagesUsed);
    if AS < 0 then
      AS := 0;
  end
  else
    EMSPagesUsed := 0;

  FreeMem(SizeTable,StSize);

  AutoSimulate := AS;
end;

{$IFDEF Heap6}
  Function RoundToGranul ( Value : Word ) : Word;
    {-Rounds Value up to the next multiple of 8}
  Begin
    RoundToGranul := (Value + 7) And $FFF8;
  End;
{$ENDIF}

function AutoSortInfo(FSizeInRecs     : LongInt;
                      RecLength       : Word;
                      var HeapSpace   : LongInt;
                      var DiskSpace   : LongInt;
                      var FileHandles : Word;
                      var EMSPages    : Word;
                      var RunLen      : Word;
                      var FileBufs    : Word;
                      var OutFileBufs : Word;
                      var AllInMem    : Boolean) : MSortStatus;
label
  ExitPoint;
var
  TotalNeeded,Avail,PBufLen,Temp,DiskSpaceNeeded,DiskSpaceFree,MaxAv
                   : LongInt;
  EMSToBeUsed      : Boolean;
  {$IFDEF Heap6}                             {!!.06}
  SaveRecLen       : Word;
  {$ELSE}
  FreeMinSave      : Word;                   {!!.04}
  {$ENDIF}                                   {!!.06}
  FreeListSize     : LongInt;


begin
  AutoSortInfo := MSortSuccess;
{$IFNDEF Heap6}                               {!!.06}
  {Set FreeMin to some reasonable value (preserving its original contents)}
  FreeMinSave := FreeMin;                    {!!.04}
  if FreeMin < UserFreeMin then              {!!.04}
    FreeMin := UserFreeMin;                  {!!.04}
{$ENDIF}                                     {!!.06}

{$IFDEF Heap6}
  SaveRecLen := RecLength;
  RecLength  := RoundToGranul(RecLength);
{$ENDIF}
  EMSToBeUsed  := UseEMS and EMSInstalled and (EMSPagesAvail > 0);
  {calculate optimum buffer sizes for RecBuf and I/O buffers}
  FreeListSize := 0;
  {$IFNDEF BigHeap}
  {$IFNDEF Heap6}
  if Ofs(FreePtr^) = 0 then
    FreeListSize := 0
  else
    FreeListSize := LongInt(65536)-Ofs(FreePtr^);
  {$ENDIF}
  {$ENDIF}
  MaxAv := MaxAvail;
  if MaxAv > MaxHeapToUse then
    MaxAv := MaxHeapToUse;
  HeapSpace   := MaxAv;               {!!.02}
  Avail       := MaxAv
                 {$IFNDEF BigHeap}
                 - FreeListSize       {for free list buffer}
                 {$ENDIF}
                 - (2*RecLength)
                 - SizeOf(InFilePtr^)  {!!.04}
             {$IFNDEF Heap6}                               {!!.06}
                 - UserFreeMin
             {$ENDIF}                                      {!!.06}
                 - UserFree;

  if EMSToBeUsed then
    Avail := Avail - SizeOf(EMSFatType);

  if Avail <= 0 then begin
    AutoSortInfo := MSortOutOfMemory;
    goto ExitPoint;                    {!!.04}
  end;

  PBufLen:= (LongInt(FSizeInRecs) * SizeOf(Pointer)) + 15;  {!!.04}

  TotalNeeded  := (LongInt(FSizeInRecs+MERGEORDER) * RecLength) + PBufLen;

  if (TotalNeeded < Avail) and (FSizeInRecs < BiggestDataItem) then begin
    {entire file will fit in memory, so no need for I/O buffers}
    AllInMem    := TRUE;
    FileBufs    := 0;
    RunLen      := FSizeInRecs;
    Temp        := Avail - TotalNeeded;
    if Temp > BiggestDataItem then
      Temp := BiggestDataItem;

    OutFileBufs := Temp;
  end
  else begin
    {file won't fit in mem, so pick sizes for RunLen and FileBufs}
    AllInMem     := FALSE;
    Temp         := (Avail DIV BUFSLICE);
    if Temp > BiggestDataItem then
      Temp := BiggestDataItem
    else if Temp < RecLength then
      Temp := 0;

    OutFileBufs  := Temp;
    Avail        := Avail - OutFileBufs;
    Temp         := (Avail DIV (4+RecLength));
    if Temp > LongInt(MAXRUNLENGTH) then
      Temp := MAXRUNLENGTH;
    RunLen := Word(Temp);

    Temp         := LongInt(OutFileBufs) * 5;             {!!.02}
    if Temp > BiggestDataItem then
      Temp := BiggestDataItem;
    FileBufs := Temp;
  end;
  {$IFDEF Heap6}
  RecLength := SaveRecLen;
  {$ENDIF}
  DiskSpace := AutoSimulate(FSizeInRecs * RecLength,EMSToBeUsed,
                            RunLen,RecLength,
                            EMSPages,FileHandles);
ExitPoint:
{$IFNDEF Heap6}                               {!!.06}
  FreeMin := FreeMinSave;                     {!!.04}
{$ENDIF}                                      {!!.06}
end;

