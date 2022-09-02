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

{--Conditional defines and compiler options}
  {$I BTDefine.Inc}
  {$F+,V-,B-,S-,I-,R-}
  {$IFDEF CanAllowOverlays}
    {$O+}
  {$ENDIF}
  {$IFDEF CanSetOvrflowCheck}
    {$Q-,T-}          {!!.41}
  {$ENDIF}

{$IFDEF DPMIOrWnd}    {!!.41}
  !! Error: this unit is for DOS real mode only
{$ENDIF}              {!!.41}

{!!.41 All Windows and DPMI support code removed}

{Controls whether heap blocks larger than 64K are allocated. See the manual
 for a discussion}
{.$DEFINE BigHeap}

Unit Msort;

interface
uses
  DOS,
  TPAlloc,
{$IFDEF UseOPEMS}
  OPEMS;
{$ELSE}
  {$IFDEF UseTPEMS}
  TPEMS;
  {$ELSE}
  EMSSupp;
  {$ENDIF}
{$ENDIF}

const
  MERGEORDER       = 5;     {The maximum number of input files for merge}
  BiggestDataItem  = 65521; {largest data item Turbo Pascal can handle}
  UseEMS           : Boolean = TRUE; {tells sort whether to use EMS if }
                                     {available and needed}
  MaxHeapToUse     : LongInt = 655210;
  STemp            : String[5] = 'STEMP';

TYPE

  {these are the possible return values of the sort routines}
  MSortStatus       = (MSortSuccess,     {Successful sort}
                       MSortOutOfMemory, {insufficient memory}
                       MSortDiskError,   {disk I/O error}
                       MSortOutOfDisk,   {insufficient disk space for merge}
                       MSortEMSError,    {EMM error}
                       MSortUserAbort);  {User abort}    {!!.05}

  PathName         = String[79];         {string type for pathnames}

var
  UsingEMS         : Boolean;
  GRunLength       : Word;        {To store the run length}
  MSortIOResult    : Integer;     {Stores last IOResult value}
  LastFileName     : PathName;    {The name of the last file written to}

function DoSort(RunLength     : Word;
                RecLength     : Word;
                InFileBufMax  : Word;
                OutFileBufMax : Word;
                TempPath      : PathName;
                GetElements   : Pointer;
                LessFunc      : Pointer;
                PutElements   : Pointer
               ) : MSortStatus;
{-low level sorting routine}


function AutoSort(FSizeInRecs : LongInt;
                  RecLength   : Word;
                  TempPath    : PathName;
                  GetElements : Pointer;
                  LessFunc    : Pointer;
                  PutElements : Pointer
                 ) : MSortStatus;
{-high level sorting routine}

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


function PutElement(var X) : Boolean;
{-Submits a record to the sort system.  Returns TRUE is record successfully}
{submitted.}

function GetElement(var X) : Boolean;
{-Returns a record from the sort system.  Returns TRUE while there are more}
{ records to return.  When FALSE is returned, the value of the VAR X is    }
{ undefined.}

procedure AbortSort;
{-Prematurely halt the sort from the user defined routines}

implementation

const
  MAXRUNLENGTH     = $FFFF; {maximum number of elements per Run}
  UserFree         = 128;
{$IFDEF Ver55}                                {!!.06}
  UserFreeMin      = 16;
{$ENDIF}                                      {!!.06}
  AllInMem         : Boolean = FALSE; {to indicate whether the entire file}
                                      {to be sorted is in memory}
  GFSizeInRecs     : LongInt = MaxLongInt;

type
  FileName         = String[13];
  BufferType       = Array[1..$FFFF] of Char; {a buffer of Characters}
  HugeArray        = Pointer; {for data structures larger than 64k}
  FDBuffer         = Array[1..MERGEORDER] of File; {the input file list}
  FDBuf            = ^FDBuffer;
  MarkRec          = record
                       Marker : Pointer;
                       FreeBuf : Pointer;
                       OldFreePtr : Pointer;
                       BufSize : Word;
                     end; {used by fast heap routines MarkFL and ReleaseFL}

var
  BufferPos,    {to hold the current position within each of the I/O buffers}
  BufferSize    {to hold the size of each of the buffers}
                   : Array[0..MERGEORDER] of Word;

  {The following var holds pointers to the I/O buffers.  Position 0 }
  {is the output buffer, and positions 1..MERGEORDER are for each of}
  {the input files.}
  Buffer           : Array[0..MERGEORDER] of ^BufferType;

  RecBufSeg        : Word;      {The segment of the address of RecBuf}
  RecBuf           : HugeArray; {This huge array holds pointers to the data.}
                                {It may exceed 64K in size if necessary.}
  RecBufSave       : Pointer;   {to save the original value of RecBuf} {!!.04}
  HeapMark         : MarkRec;
  UserLess         : Pointer;   {points to the user's Less procedure}
  UserGet          : Pointer;   {points to the user's GetElements procedure}
  UserPut          : Pointer;   {points to the user's PutElements procedure}
  GTempPath        : PathName;  {Pathname to store temporary files}
  NumElements,
  ElementCount     : LongInt;   {Used by sort routines to count elements}
  FileBufferMax    : Word;      {The maximum size of an input file buffer}
  InFilePtr           : FDBuf;     {a record to hold all the input files for merge}
  OutFile          : File;      {The output file}
  Pivot            : Pointer;   {pointer to pivot element for QuickSort}
  TempRec          : Pointer;   {pointer to temporary record}
  OutBufSize       : Word;      {size of the output buffer}
  HighF            : Word;
  NumElemInRun     : Word;      {element number within this run}
  PtrBufLength     : LongInt;   {The length of the pointer buffer for data}
  GRecLength       : Word;      {To store the record length of the data items}

  Pl               : Word;      {Left edge within partition}
  Pr               : Word;      {Right edge within partition}

  GBasePtr         : Pointer;
  GBytes           : LongInt;
  MSortResult      : MSortStatus; {global sort status}

function GetDiskInfo(Drive : Byte; var ClustersAvailable, TotalClusters,
                     BytesPerSector, SectorsPerCluster: Word) : Boolean;
begin
Inline(
  $8A/$96/>DRIVE/        {  mov    dl,>Drive[BP]}
  $B4/$36/               {  mov    ah,$36}
  $CD/$21/               {  int    $21}
  $3D/$FF/$FF/           {  cmp    ax,$FFFF}
  $74/$20/               {  je     GDIError}
  $1E/                   {  push   ds}
  $C5/$BE/>SECTORSPERCLUSTER/ {  lds    di,>SectorsPerCluster[BP]}
  $89/$05/               {  mov    [di],ax}
  $C5/$BE/>BYTESPERSECTOR/ {  lds    di,>BytesPerSector[BP]}
  $89/$0D/               {  mov    [di],cx}
  $C5/$BE/>TOTALCLUSTERS/ {  lds    di,>TotalClusters[BP]}
  $89/$15/               {  mov    [di],dx}
  $C5/$BE/>CLUSTERSAVAILABLE/ {  lds    di,>ClustersAvailable[BP]}
  $89/$1D/               {  mov    [di],bx}
  $1F/                   {  pop    ds}
  $C6/$46/<GETDISKINFO/$01/ {  mov    byte ptr <GetDiskInfo[BP],1}
  $EB/$07/               {  jmp    short GDIExit}
                         {GDIError:}
  $C6/$46/<GETDISKINFO/$00/ {  mov    byte ptr <GetDiskInfo[BP],0}
  $E9/$00/$00);          {  jmp    GDIExit}
                         {GDIExit:}
end;

procedure FillWord(var Dest; Count, Filler : Word);
  {-Fill memory starting at Dest with Count instances of Filler}
  inline(
    $58/                     {pop ax    ;AX = Filler}
    $59/                     {pop cx    ;CX = Count}
    $5F/                     {pop di    ;ES:DI => Dest}
    $07/                     {pop es}
    $FC/                     {cld       ;go forward}
    $F2/$AB);                {rep stosw ;fill memory}

  function EmsInstalled : Boolean;
    {-Returns true if the Expanded Memory Manager is installed.}
  var
    F : file;
  begin
    Assign(F, 'EMMXXXX0');
    Reset(F);
    if IoResult = 0 then begin
      EmsInstalled := True;
      Close(F);
    end
    else
      EmsInstalled := False;
  end;

{$I MSortEMS.Inc}

{$IFNDEF BigHeap}
  procedure MarkFL(var HeapMark : MarkRec);
    {-Mark the current top of heap and buffer the free list}
  begin
    with HeapMark do begin
      {$IFDEF Ver55}
      if Ofs(FreePtr^) = 0 then
        {Free list is empty}
        FreeBuf := nil
      else begin
        {Buffer the free list}
        BufSize := LongInt(65536)-Ofs(FreePtr^);
        {Allow for possibility of free list growth}
        if BufSize < $FFF8 then
          inc(BufSize, 8);
        GetMem(FreeBuf, BufSize);
        if FreeBuf <> nil then begin
          OldFreePtr := FreePtr;
          Move(FreePtr^, FreeBuf^, LongInt(65536)-Ofs(FreePtr^));
        end;
      end;
      {$ENDIF}
      {Mark the top of the heap}
      Mark(Marker);
    end;
  end;

  procedure ReleaseFL(HeapMark : MarkRec);
    {-Release the heap at heap mark and restore the free list}
  begin
    with HeapMark do begin
      {Release from the marker}
      Release(Marker);
      {$IFDEF Ver55}
      if FreeBuf <> nil then begin
        {Reset FreePtr}
        FreePtr := OldFreePtr;                                 {!!.10}
        {Restore free list}
        Move(FreeBuf^, FreePtr^, LongInt(65536)-Ofs(FreePtr^));{!!.10}
        {Dispose of the free list buffer}
        FreeMem(FreeBuf, BufSize);
      end;
      {$ENDIF}
    end;
  end;
{$ENDIF}

procedure GetRecBuffer(Size : LongInt);
{-Get the record buffer, and ensure it starts on a 0 offset}

var
  Off              : Word;

begin
  HugeGetMem(RecBuf,Size+15);
  if (RecBuf = NIL)  then begin
    MSortResult := MSortOutOfMemory;
    Exit;
  end;
  RecBufSave := RecBuf;         {!!.04}
  Off := SegOfs(RecBuf).Ofst;
  if (Off <> 0) then begin     {if offset not zero, then make it zero}
    SegOfs(RecBuf).Ofst := 16;
    Pointer(RecBuf) := Normalized(RecBuf);
  end;
  RecBufSeg := SegOfs(RecBuf).Segm;
end;

procedure FreeRecBuffer(Size : LongInt);
{-Free the Record buffer}
begin
  HugeFreeMem(RecBufSave,Size+15);  {!!.04}
end;

function GetPointerPr : Pointer;
{-this macro returns a pointer to the element at index Pr within RecBuf}
Inline(
  $8B/$16/>RECBUFSEG/    {     mov   DX,[>RecBufSeg]}
  $31/$DB/               {     xor   BX,BX}
  $A1/>PR/               {     mov   AX,[>Pr]}
  $48/                   {     dec   AX}
  $D1/$E0/               {     shl   AX,1}
  $D1/$D3/               {     rcl   BX,1}
  $D1/$E0/               {     shl   AX,1}
  $D1/$D3/               {     rcl   BX,1}
  $B1/$04/               {     mov   CL,4}
  $D2/$E3/               {     shl   BL,CL}
  $00/$DE);              {     add   DH,BL}

function GetPointerPl : Pointer;
{-this macro returns a pointer to the element at index Pl within RecBuf}
Inline(
  $8B/$16/>RECBUFSEG/    {     mov   DX,[>RecBufSeg]}
  $31/$DB/               {     xor   BX,BX}
  $A1/>PL/               {     mov   AX,[>Pl]}
  $48/                   {     dec   AX}
  $D1/$E0/               {     shl   AX,1}
  $D1/$D3/               {     rcl   BX,1}
  $D1/$E0/               {     shl   AX,1}
  $D1/$D3/               {     rcl   BX,1}
  $B1/$04/               {     mov   CL,4}
  $D2/$E3/               {     shl   BL,CL}
  $00/$DE);              {     add   DH,BL}

procedure IncPtr(var P : Pointer; Increment : Word);
{-increments a pointer by Increment and normalizes it}
begin
  Inc(SegOfs(P).Ofst,Increment);
  P := Normalized(P);
end;

procedure AllocRunBuffers;
{-allocate space for an entire run's worth of data}
var
  I                : Word;
  P                : Pointer;

begin
{$IFNDEF BigHeap}
  MarkFL(HeapMark);
{$ENDIF}

  GetRecBuffer(PtrBufLength);
  if MSortResult <> MSortSuccess then Exit;
{$IFDEF BigHeap}
  GBytes := LongInt(GRecLength) * GRunLength;  {!!.02}
  HugeGetMem(GBasePtr,GBytes);
  if GBasePtr = NIL then begin                 {!!.02}
    MSortResult := MSortOutOfMemory;           {!!.02}
    Exit;                                      {!!.02}
  end;
  P := GBasePtr;
  for I := 1 to GRunLength do begin
    Pr := I;
    Pointer(GetPointerPr^) := P;
    IncPtr(P,GRecLength);
  end;
{$ELSE}
  for I := 1 to GRunLength do begin
    Pr := I;
    GetMem(Pointer(GetPointerPr^),GRecLength);
  end;
{$ENDIF}
end;

procedure DeallocRunBuffers;
{-deallocate the space used by the run data}

{$IFNDEF BigHeap}
var
  I                : Word;
  P                : Pointer;
{$ENDIF}

begin
{$IFDEF BigHeap}
  HugeFreeMem(GBasePtr,GBytes);
  FreeRecBuffer(PtrBufLength);
{$ELSE}
  ReleaseFL(HeapMark);
{$ENDIF}
end;

procedure AllocIOBuffers;
{-allocate the input file data buffers}

var
  I                : Word;

begin
  {$IFNDEF BigHeap}
  MarkFL(HeapMark);
  {$ENDIF}
  for I := 1 to MERGEORDER do begin
    if FileBufferMax > 0 then begin
      GetMem(Buffer[I],FileBufferMax);
      if Buffer[I] = NIL then begin
        MSortResult := MSortOutOfMemory;
        Exit;
      end;
    end;
    BufferPos[I]  := FileBufferMax + 1;
    BufferSize[I] := FileBufferMax;
  end;
end;

procedure DeallocIOBuffers;
{-deallocate the input file data buffers}

var
  I                : Word;

begin
  {$IFDEF BigHeap}
  for I := 1 to MERGEORDER do
    FreeMem(Buffer[I],FileBufferMax);
  {$ELSE}
  ReleaseFL(HeapMark);
  {$ENDIF}
end;

function MakePathName(N : FileName) : PathName;
{-appends a filename  to the path for temporary files}
begin
  MakePathName := GTempPath + N;
end;

function Min(A, B : Word) : Word;
  {-Returns the smaller of A and B}
  inline(
    $58/                     {pop ax}
    $5B/                     {pop bx}
    $39/$C3/                 {cmp bx,ax}
    $73/$02/                 {jae done}
    $89/$D8);                {mov ax,bx}
                             {done:}

procedure ExchangeWords(var I, J : Word);
  {-Exchange words I and J. Useful in sorts}
  inline(
    $8C/$DB/                 {mov bx,ds       ;save DS}
    $5E/                     {pop si}
    $1F/                     {pop ds          ;DS:SI => J}
    $5F/                     {pop di}
    $07/                     {pop es          ;ES:DI => I}
    $8B/$04/                 {mov ax,[si]     ;AX = J}
    $26/$87/$05/             {xchg ax,es:[di] ;I = J, AX = I}
    $89/$04/                 {mov [si],ax     ;J = I}
    $8E/$DB);                {mov ds,bx       ;restore DS}

procedure ExchangePtr(var A,B);
{-Exchange pointers A and B. Useful in sorts}
Inline(
  $8C/$DA/               {  mov    DX,DS}
  $5E/                   {  pop    SI}
  $1F/                   {  pop    DS}
  $5F/                   {  pop    DI}
  $07/                   {  pop    ES}
  $8B/$04/               {  mov    AX,[SI]}
  $26/                   {  ES:}
  $87/$05/               {  xchg   AX,[DI]}
  $89/$04/               {  mov    [SI],AX}
  $46/                   {  inc    SI}
  $46/                   {  inc    SI}
  $47/                   {  inc    DI}
  $47/                   {  inc    DI}
  $8B/$04/               {  mov    AX,[SI]}
  $26/                   {  ES:}
  $87/$05/               {  xchg   AX,[DI]}
  $89/$04/               {  mov    [SI],AX}
  $8E/$DA);              {  mov    DS,DX}


function ULess(var X, Y) : Boolean;
  {-Call the user routine to perform element comparison}
  inline($FF/$1E/>UserLess); {CALL DWORD PTR [>UserLess]}

procedure CallGetElements;
  {-Call the user GetEl routine}
  inline($FF/$1E/>UserGet); {CALL DWORD PTR [>UserGet]}

procedure CallPutElements;
  {-Call the user PutEl routine}
  inline($FF/$1E/>UserPut); {CALL DWORD PTR [>UserPut]}

procedure GetPivot(L, R : Word);
{-Load the pivot element}
var
  Sav            : Word;

begin
  {Use a random pivot index to help with pre-sorted arrays}
  Sav := Pr;
  Pr  := L+Random(R-L);
  Move(Pointer(GetPointerPr^)^,Pivot^,GRecLength);
  Pr := Sav;
end;

  procedure QuickSort(L, R : Word);
    {-Non-recursive QuickSort per N. Wirth's "Algorithms and Data Structures"}
  const
    StackSize = 20;
  type
    Stack = array[1..StackSize] of Word;
  var
    Lstack : Stack;          {Pending partitions, left edge}
    Rstack : Stack;          {Pending partitions, right edge}
    StackP : Integer;        {Stack pointer}
  begin
    {Initialize the stack}
    StackP := 1;
    Lstack[1] := L;
    Rstack[1] := R;

    {Repeatedly take top partition from stack}
    repeat
      {Pop the stack}
      L := Lstack[StackP];
      R := Rstack[StackP];
      Dec(StackP);

      {Sort current partition}
      repeat
        {Load the pivot element}
        GetPivot(L,R);
        Pl := L;
        Pr := R;
        {Swap items in sort order around the pivot index}
        repeat
          while ULess(Pointer(GetPointerPl^)^,Pivot^) do
            Inc(Pl);
          while ULess(Pivot^, Pointer(GetPointerPr^)^) do
            Dec(Pr);
          if Pl <= Pr then begin
            if Pl <> Pr then
              {Swap the two elements}
              ExchangePtr(Pointer(GetPointerPl^),Pointer(GetPointerPr^));

            if Pl < 65535 then
              Inc(Pl);
            if Pr > 0 then
              Dec(Pr);
          end;
        until Pl > Pr;
        {Decide which partition to sort next}
        if (Pr-L) < (R-Pl) then begin
          {Left partition is bigger}
          if Pl < R then begin
            {Stack the request for sorting right partition}
            Inc(StackP);
            Lstack[StackP] := Pl;
            Rstack[StackP] := R;
          end;
          {Continue sorting left partition}
          R := Pr;
        end else begin
          {Right partition is bigger}
          if L < Pr then begin
            {Stack the request for sorting left partition}
            Inc(StackP);
            Lstack[StackP] := L;
            Rstack[StackP] := Pr;
          end;
          {Continue sorting right partition}
          L := Pl;
        end;

      until L >= R;

    until StackP <= 0;
  end;

procedure QuickSortIt(NLines : Word);
{-calls the non-recursive quicksort}
begin
  QuickSort(1,NLines);
end;

procedure GName(N : Word; var Name : FileName);
{-create a file name in format STEMPxx  where xx is a number}
VAR
  Scratch          : String[8];

begin
  Str(N,Scratch);
  Name := STemp + Scratch;
end;

procedure MakeFile(var F : File; N : Word; Size : LongInt);
{-create a new file}

var
  Name             : PathName;

begin
  GName(N,Name);
  LastFileName := MakePathName(Name);

  Open_OutFile(F,LastFileName,Size);
  BufferPos[0]  := 1;
end;

function GOpen(var InFilePtr : FDBuf; F1,F2 : Word) : LongInt;
{-open files F1 through F2}

var
  Name             : PathName;
  I                : 1..MERGEORDER;
  Size             : LongInt;

begin
  Size := 0;

  for I := 1 to F2-F1+1 do begin
    GName(F1+I-1,Name);
    Name := MakePathName(Name);
    Open_InFile(InFilePtr^[I],Name,I);
    if FileRec(InFilePtr^[I]).Mode = fmEMS then
      Size := Size + FileRec(InFilePtr^[I]).ActualSize

    else begin
      Size := Size + FileSize(InFilePtr^[I]);
(*code commented out as of 5.22                      !!.22
      MSortIOResult := IOResult;
      if MSortIOResult <> 0 then
        MSortResult := MSortDiskError;
*)
    end;
  end;
  BufferPos[I]  := FileBufferMax + 1;
  BufferSize[I] := FileBufferMax;
  GOpen := Size;
end;

procedure CloseOutFile(var F : File);
{-close an output file (flushing buffers if necessary)}

var
  Size,Num         : Word;

begin
  Size := BufferPos[0] - 1;
  if UsingEMS and (FileRec(F).Mode = fmEMS) then begin
    if Size > 0 then
      EMSBlockWrite(F,Buffer[0]^,Size,Num);
    CloseEMSFile(F)
  end
  else begin
    if Size > 0 then begin
      BlockWrite(F,Buffer[0]^,Size,Num);
      MSortIOResult := IOResult;
      if (MSortIOResult <> 0) or (Num <> Size) then begin
        MSortResult := MSortDiskError;
        if (MSortIOResult = 0) then
          MSortIOResult := -1;
      end;
    end;
    Close(F);
    if MSortIOResult = 0 then begin
      MSortIOResult := IOResult;
      if MSortIOResult <> 0 then
        MSortResult := MSortDiskError;
    end
    else
      if IOResult = 0 then ;
  end;
end;

procedure PutARec(var F : File; var S);
{-put a record to the output File specified as F}

var
  Size,Num         : Word;

begin
  if BufferSize[0] = 0 then begin
  {if we're not buffering the output...}

    if UsingEMS and (FileRec(F).Mode = fmEMS) then begin
      EMSBlockWrite(F,S,GRecLength,Num)

    end
    else begin
      BlockWrite(F,S,GRecLength,Num);
      MSortIOResult := IOResult;
      if (MSortIOResult <> 0) or (Num <> GRecLength) then begin
        MSortResult := MSortDiskError;
        if (MSortIOResult = 0) then
          MSortIOResult := -1;
      end;
    end;
  end
  else begin
  {we are buffering the output}
    Size := BufferPos[0] - 1;
    {see if we need to write out the buffer because its full}
    if LongInt(Size) + GRecLength > BufferSize[0] then begin {!!.40}
      if UsingEMS and (FileRec(F).Mode = fmEMS) then begin
        EMSBlockWrite(F,Buffer[0]^,Size,Num);
        {******}
      end
      else begin
        BlockWrite(F,Buffer[0]^,Size,Num);
        MSortIOResult := IOResult;
        if (MSortIOResult <> 0) or (Num <> Size) then begin
          MSortResult := MSortDiskError;
          if (MSortIOResult = 0) then
            MSortIOResult := -1;
          Exit;
        end;
      end;
      BufferPos[0] := 1;
    end;
    {move the record S into the output buffer}
    Size := BufferPos[0];
    Move(S,Buffer[0]^[Size],GRecLength);
    BufferPos[0] := Size + GRecLength;
  end;
end;

function GetARec(var Line; var F : File; Index : Integer) : Boolean;
{-get a record from the input file F (which is InFilePtr^[Index]) }

var
  Reread,Buffered,EMS
                   : Boolean;
  NumRead          : Word;

begin
  GetARec  := TRUE;
  EMS      := UsingEMS and (FileRec(F).Mode = fmEMS); {this file in EMS?}

  {Is the Input buffer exhausted, do we need another buffer full?}
  Reread   := BufferPos[Index] + GRecLength - 1 > BufferSize[Index];

  {Are we utilizing an input buffer?}
  Buffered := BufferSize[Index] > 0;
  if Buffered then begin

    if Reread then begin
      {we need a new buffer full, so check to see if in EMS or on disk}
      if EMS then begin
        if EMS_EOF(F) then begin
          GetARec := FALSE;
          Exit;
        end;
        EMSBlockRead(F,Buffer[Index]^,FileBufferMax,BufferSize[Index]);
        MSortIOResult := IOResult;
        BufferPos[Index] := 1;
      end
      else begin
        if EOF(F) then begin
          GetARec := FALSE;
          Exit;
        end;
        BlockRead(F,Buffer[Index]^,FileBufferMax,BufferSize[Index]);
        MSortIOResult := IOResult;
        BufferPos[Index] := 1;
      end;
    end;

    {get the record from the input buffers}
    Move(Buffer[Index]^[BufferPos[Index]],Line,GRecLength);
    BufferPos[Index] := BufferPos[Index] + GRecLength;
  end
  else
    begin                                                  {!!.42}
    {no buffering, so read directly}
      if EMS then                                          {!!.42}
        begin                                              {!!.42}
          if EMS_EOF(F) then                               {!!.42}
            begin                                          {!!.42}
              GetARec := FALSE;                            {!!.42}
              Exit;                                        {!!.42}
            end;                                           {!!.42}
          EMSBlockRead(F,Line,GRecLength, NumRead);        {!!.42}
          MSortIOResult := IOResult;                       {!!.42}
          if (MSortIOResult <> 0) then                     {!!.42}
            begin                                          {!!.42}
              MSortResult := MSortEMSError;                {!!.42}
              GetARec := FALSE;                            {!!.42}
            end;                                           {!!.42}
        end                                                {!!.42}
      else                                                 {!!.42}
        begin                                              {!!.42}
          if EOF(F) then
            GetARec := FALSE
          else begin
            BlockRead(F,Line,GRecLength);
            MSortIOResult := IOResult;
            if MSortIOResult <> 0 then begin
              MSortResult := MSortDiskError;
              GetARec := FALSE;
            end;
          end;
        end;                                               {!!.42}
  end;
end;

procedure GRemove(var InFilePtr : FDBuf; F1,F2 : Word);
{-close the InFiles specified in InFilePtr from F1, F2, and delete them}

var
  Name             : String;
  I                : 1..MERGEORDER;

begin
  for I := 1 to F2-F1+1 do
    if (not UsingEMS) or (FileRec(InFilePtr^[I]).Mode <> fmEMS) then begin
      Close(InFilePtr^[I]);
      if IOResult <> 0 then ;  {just clear IOResult}
      Erase(InFilePtr^[I]);
      if IOResult <> 0 then ;  {just clear IOResult}
    end;
end;

function GetARun(var Nlines : Word; var InF : File;
                 II : Word) : Boolean;
{- load an entire run into the Run Buffers, return TRUE if entire Run }
{returned, and FALSE if end of input encountered}

var
  NextPos          : Word;
  Done             : Boolean;

begin
  Nlines  := 0;
  repeat
    Done := (GetARec(TempRec^,InF,II) = FALSE);
    if Not Done then begin
      Inc(NLines);
      NextPos := NLines + 1;
      Pr := NLines;
      Move(TempRec^,Pointer(GetPointerPr^)^,GRecLength);
    end;
  until (Done) or (NLines >= GRunLength);
  GetARun := Done;
end;

procedure PutARun(NLines : Word; var OutFile : File);
{-writes a Run to disk (or EMS)}

var
  I                : Word;

begin
  for I := 1 to NLines do begin
    Pr := I;
    PutARec(OutFile,Pointer(GetPointerPr^)^);
  end;
end;


function PutElement(var X) : Boolean;
{-Submits a record to the sort system.  Returns TRUE is record successfully}
{submitted.}


begin
  if (MSortResult = MSortSuccess) and (NumElements < GFSizeInRecs) then
    PutElement := TRUE
  else begin
    PutElement := FALSE;
    Exit;
  end;

  {if there is still room in RAM, add record to the run buffer, otherwise}
  {sort what's in the run buffer and write out the Run first (flush the  }
  {run), and add the record to the run buffer}
  if (NumElemInRun < GRunLength) then begin
    Inc(NumElemInRun);
    Pr := NumElemInRun;

    Move(X,Pointer(GetPointerPr^)^,GRecLength);
  end
  else begin
    QuickSortIt(NumElemInRun);             {non recursive QS}

    Inc(HighF);
    MakeFile(OutFile,HighF,LongInt(GRunLength)*GRecLength);
    PutARun(NumElemInRun,OutFile);
    CloseOutFile(OutFile);
    NumElemInRun := 1;
    Pr := 1;
    Move(X,Pointer(GetPointerPr^)^,GRecLength);
  end;
  Inc(NumElements);
end;

{this type is used by the Merge System}
type
  MergeItem        = record
                       F  : Word;
                       P  : Pointer;
                       HP : Pointer;
                     end;

{used by Merge System}
var
  MergeList        : Array[1..MERGEORDER] of MergeItem;

procedure Exchange(var Item1,Item2 : MergeItem);
{-exchanges two MergeItems in the MergeList.  Note: The field HP is not}
{exchanged!!}

begin
  with Item1 do begin
    ExchangePtr(P,Item2.P);
    ExchangeWords(F,Item2.F);
  end;
end;

procedure SortMergeList(NF : Word);
{-simple bubblesort on MergeList.  The MergeList always contains only }
{MERGEORDER elements, so a bubblesort is adequate.}
var
  I,J              : Word;

begin
  for I := NF Downto 2 do
    for J := 1 to I-1 do
      if ULess(MergeList[J+1].P^,MergeList[J].P^) then
        Exchange(MergeList[J+1],MergeList[J]);
end;

procedure ReHeap(NF : Word);
{-make sure first element in MergeList is the smallest}

var
  I,J              : Word;

begin
  i := 1;
  J := 2 * i;
  while (J <= NF) do begin
    if (J < NF) then
      if not ULess(MergeList[J].P^,MergeList[J+1].P^) then
        Inc(J);

    if ULess(MergeList[I].P^,MergeList[J].P^) then
      I := NF
    else
      Exchange(MergeList[I],MergeList[J]);

    I := J;
    J := 2 * I;
  end;
end;

{variables used by the Merge System}
var
  LowF,Lim         : Word;
  NumFiles         : Word;
  LastMerge        : Boolean;
  BottomLoop       : Boolean;
  MergeDone        : Boolean;

function MoreToMerge : Boolean;
{-this routine returns TRUE while there is more data to be merged.  Sets }
{some important merge System variables}
var
  I                : Word;
  MergeSize        : LongInt;

begin
  MoreToMerge := TRUE;
  if BottomLoop and (MSortResult = MSortSuccess) then
    Exit;
  if LowF < HighF then begin

    Lim := Min(LowF+MergeOrder-1,HighF);
    NumFiles := Lim-LowF+1;
    MergeSize := GOpen(InFilePtr,LowF,Lim);
    if MSortResult <> MSortSuccess then begin              {!!.22}
      MoreToMerge := False;                                {!!.22}
      Exit;                                                {!!.22}
    end;                                                   {!!.22}
    Inc(HighF);
    LastMerge := (LowF + MERGEORDER >= HighF);
    if not LastMerge then
      MakeFile(OutFile,HighF,MergeSize);

    for I := 1 to NumFiles do
      with MergeList[I] do begin
        P := HP;
        if GetARec(P^,InFilePtr^[I],I) then
          F := I;
    end;
    SortMergeList(NumFiles);
    BottomLoop := TRUE;
  end
  else
    MoreToMerge := FALSE;
end;

function GetElement(var X) : Boolean;
{-Returns a record from the sort system.  Returns TRUE while there are more}
{ records to return.  When FALSE is returned, the value of the VAR X is    }
{ undefined.}

{This is a complex routine. The Merge Sort Phase, if it is needed, is      }
{actually performed here. The merging technology used by this program is   }
{somewhat complicated, due to two factors: The fact that the source of the }
{input and destination of the output is unknown to the Sort routines.      }
{The final merge of the data is not performed to disk, but is actually     }
{performed to the output stream (this limits the amount of disk space and  }
{time needed by the merge sort).}


const
  LBP              = 1;

var
  FIndex           : Word;
  Error            : Boolean;

begin
  Error := FALSE;
  if (not MergeDone) and (MSortResult = MSortSuccess) then
    GetElement := TRUE
  else begin
    GetElement := FALSE;
    Exit;
  end;
  if AllInMem then begin
  {if the sort is being performed entirely in memory, then just return the}
  {next element in the run buffer.}
    if ElementCount > 0 then begin
      Pr := NumElements - (ElementCount-1);
      Move(Pointer(GetPointerPr^)^,X,GRecLength);
      Dec(ElementCount);
    end
    else
      GetElement := FALSE;
  end
  else
  {not all in memory, so perform the special merge sort technique}
    repeat
      if MoreToMerge and (MSortResult = MSortSuccess) then begin
        if LastMerge then
          Move(MergeList[LBp].P^,X,GRecLength)
        else
          PutARec(OutFile,MergeList[LBp].P^);
        FIndex := MergeList[LBp].F;
        if not GetARec(TempRec^,InFilePtr^[FIndex],FIndex) then begin
          {InFilePtr^[FIndex] is exhausted, so decrement num files}
          Exchange(MergeList[LBp],MergeList[NumFiles]);
          Dec(NumFiles);
          if NumFiles = 0 then begin
            if LastMerge then
              MergeDone := TRUE
            else begin
              BottomLoop := FALSE;
              CloseOutFile(OutFile);
            end;
            GRemove(InFilePtr,LowF,Lim);
            LowF := LowF + MergeOrder;
          end;
        end
        else
          with MergeList[1] do begin
            Move(TempRec^,P^,GRecLength);
            F := FIndex;
          end;
        ReHeap(NumFiles);
      end
      else
        GRemove(InFilePtr,LowF,Lim);
      Error := (MSortResult <> MSortSuccess);
    until LastMerge or Error;
    if Error then begin
      GetElement := FALSE;
    end;
end;

procedure MergeSort;
{-this procedure calls the user routine to retrieve the sorted elements.  If }
{a merge phase is required, it is performed during the calls to GetElement   }
{(see the comments in GetElement above). }
var
  I                : Word;

begin
  {If not all in memory, then allocate the I/O buffers}
  if not AllinMem then begin
    AllocIOBuffers;

    {allocate the memory for the MergeList data}
    for I := 1 to MERGEORDER do
      with MergeList[I] do
        GetMem(HP,GRecLength);
  end;

  {initialize some important sort system variables}
  LowF := 1;
  LastMerge := FALSE;
  BottomLoop := FALSE;
  MergeDone  := FALSE;

  {call the user routine if everything is kosher}
  if MSortResult = MSortSuccess then
    CallPutElements;

  if not AllInMem then begin
    {free the memory used by the MergeList data}
    for I := 1 to MERGEORDER do
      FreeMem(MergeList[I].HP,GRecLength);

    {if I/O buffers were allocated, then free them}
    DeallocIOBuffers;

    if MSortResult <> MSortSuccess then        {!!.52}
      {close and delete any open merge files}  {!!.52}
      GRemove(InFilePtr,LowF,Lim);             {!!.52}
  end;
end;


function DoSort(RunLength     : Word;
                RecLength     : Word;
                InFileBufMax  : Word;
                OutFileBufMax : Word;
                TempPath      : PathName;
                GetElements   : Pointer;
                LessFunc      : Pointer;
                PutElements   : Pointer
               ) : MSortStatus;

{-the lowest level sorting routine.  This function sorts the elements.  It }
{calls the user GetElements routine to get the elements to be sorted, calls}
{the LessFunc to compare the items, and calls the user PutElements         }
{procedure upon completion.}


Label ExitPoint,FinalExitPoint;

{$IFDEF Ver55}  {!!.06}
var
  FreeMinSave : Word;
{$ENDIF}        {!!.06}
var                           {!!.53}
  RunBuffAllocated : Boolean; {!!.53}

begin
  UsingEMS       := False;     {!!.40}

  {$IFDEF Ver55} {!!.06}
  {Set FreeMin to some reasonable value (preserving its original contents)}
  FreeMinSave := FreeMin;
  if FreeMin < UserFreeMin then
    FreeMin := UserFreeMin;
  {$ENDIF}        {!!.06}

  {initialize EMS related globals}
  UsingEMS       := EMS_Init;
  if UsingEMS then
    GetMem(EMSFat,SizeOf(EMSFat^));            {!!.03}

  {Allocate the InFilePtr data structure}

  GetMem(InFilePtr,SizeOf(InFilePtr^));              {!!.03}

  {assume success}
  MSortResult   :=  MSortSuccess;

  {set some global variables needed by the sort system}
  GRecLength   := RecLength;
  GRunLength   := RunLength;
  if (TempPath <> '') and
     (TempPath[Length(TempPath)] <> '\') then
    GTempPath := TempPath + '\'
  else
    GTempPath := TempPath;

  UserLess     := LessFunc;
  UserGet      := GetElements;
  UserPut      := PutElements;

  {calculate the Run Pointer Buffer size}
  PtrBufLength := LongInt(RunLength) * SizeOf(Pointer);

  {ensure that buffer is an appropriate size}
  OutBufSize   := (OutFileBufMax DIV RecLength) * RecLength;

  {Allocate space for two elements on heap}
  GetMem(Pivot,GRecLength);
  GetMem(TempRec,GRecLength);

  {get memory for output buffer and set buffer variables}
  if OutBufSize > 0 then begin
    GetMem(Buffer[0],OutBufSize);
    if Buffer[0] = NIL then begin
      MSortResult := MSortOutOfMemory;
      Goto FinalExitPoint;
    end;
  end;
  BufferPos[0]  := OutBufSize + 1;
  BufferSize[0] := OutBufSize;

  {figure out InFilePtr buffer size}
  FileBufferMax := ((InFileBufMax DIV MERGEORDER) DIV RecLength)
                   * RecLength;

  NumElements  := 0;
  NumElemInRun := 0;
  HighF        := 0;

  {allocate the memory needed to store a Run in RAM}
  AllocRunBuffers;
  RunBuffAllocated := True; {!!.53}

  {if error then exit}
  if MSortResult <> MSortSuccess then
    Goto FinalExitPoint;

  {call the user routine to get the elements into the sort system}
  CallGetElements;

  {check to see if all the records fit in RAM}
  if (NumElements <= GRunLength) then                        {!!.05}
    AllInMem := TRUE;                                        {!!.05}

  if (MSortResult <> MSortSuccess) or (NumElements = 0) then {!!.04}
    Goto ExitPoint;                      {if error then exit}

  {Sort the run currently in RAM}
  QuickSortIt(NumElemInRun);
  {If there's more than just that run, then output it in preparation for}
  {merge phase}
  if (not AllInMem) and (MSortResult = MSortSuccess) then begin
    Inc(HighF);
    MakeFile(OutFile,HighF,LongInt(GRunLength)*GRecLength);
    PutARun(NumElemInRun,OutFile);
    CloseOutFile(OutFile);
    DeallocRunBuffers;
    RunBuffAllocated := False; {!!.53}
  end;

  if MSortResult <> MSortSuccess then
    Goto ExitPoint;

  ElementCount := NumElements;

  {Do the merge sort logic}
  MergeSort;

ExitPoint:
  if RunBuffAllocated then                    {!!.53}
    DeallocRunBuffers;

  AllInMem := FALSE;


  FreeMem(Buffer[0],OutBufSize);

  FreeMem(TempRec,GRecLength);
  FreeMem(Pivot,GRecLength);
  FreeMem(InFilePtr,SizeOf(InFilePtr^));            {!!.03}

  {if we used EMS, then free all pages}
  if UsingEMS then begin
    EMS_Close_All;
    FreeMem(EMSFat,SizeOf(EMSFat^));          {!!.03}
  end;
FinalExitPoint:
  DoSort := MSortResult;
{$IFDEF Ver55}                               {!!.06}
  FreeMin := FreeMinSave;                     {!!.04}
{$ENDIF}                                      {!!.06}
end;

function FreeDiskSpace(DriveNo : Byte) : LongInt;
{-returns number of bytes free on specified disk (0=default, 1=A:, 2=B:...)}

var
  ClusAvail,TotalClus,BytesPerSect,SectsPerClus
                   : Word;
  BytesPerClus     : LongInt;

begin
  if GetDiskInfo(DriveNo,ClusAvail,TotalClus,
                 BytesPerSect,SectsPerClus) then begin
    BytesPerClus   := LongInt(SectsPerClus) * LongInt(BytesPerSect);
    FreeDiskSpace  := LongInt(ClusAvail) * BytesPerClus;
  end
  else begin
    FreeDiskSpace := -1;
  end;
end;

const
  BUFSLICE         = 14;


{$I MSortInf.Inc} {The AutoSortInfo routine}


function AutoSort(FSizeInRecs : LongInt;
                  RecLength   : Word;
                  TempPath    : PathName;
                  GetElements : Pointer;
                  LessFunc    : Pointer;
                  PutElements : Pointer
                 ) : MSortStatus;

{-this is the high level sort routine. Similar to DoSort, except it}
{calculates optimum sizes for the sort parameters and checks for   }
{sufficient resources before attempting sort.  Calls AutoSortInfo  }
{to calculate sort parameters.}

var
  RunLen,FileBufs,OutFileBufs
                   : Word;
  FileHands,EMSPgs : Word;
  HeapSp,DiskSp    : LongInt;

function ResourcesAvailable : Boolean;

var
  DiskSpaceFree    : LongInt;
  DriveNo          : Byte;

begin
  ResourcesAvailable := FALSE;
  if Pos(':',TempPath) = 2 then
    DriveNo := Ord(UpCase(TempPath[1])) - (Ord('A') - 1)
  else
    DriveNo := 0;
  DiskSpaceFree   := FreeDiskSpace(DriveNo);
  if AllInMem or (DiskSpaceFree >= DiskSp) then
    ResourcesAvailable := TRUE

end;

var
  Res              : MSortStatus;
begin

  Res := AutoSortInfo(FSizeInRecs,RecLength,HeapSp,DiskSp,FileHands,
                      EMSPgs,RunLen,FileBufs,OutFileBufs,AllInMem);

  if Res = MSortSuccess then begin
    if ResourcesAvailable then begin
      GFSizeInRecs := FSizeInRecs;
      AutoSort := DoSort(RunLen,RecLength,FileBufs,OutFileBufs,TempPath,
                  GetElements,LessFunc,PutElements);
      GFSizeInRecs := MaxLongInt;
    end
    else
      AutoSort := MSortOutOfDisk;
  end
  else
    AutoSort := Res;

end;

procedure AbortSort;
{-Prematurely halt the sort from the user defined routines}
begin
   MSortResult := MSortUserAbort;
end;

begin
  {set up an exit procedure to automatically free any EMS pages used by }
  {MSort in the case of a fatal runtime error.}

  EMSSaveExitProc := ExitProc;
  NumInEMSFat     := 0;
  ExitProc        := @EMS_ExitProc;
end.
