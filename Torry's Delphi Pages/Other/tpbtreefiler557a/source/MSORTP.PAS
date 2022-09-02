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

{$F-,V-,B-,S-,I-,R-,X+,A+}

{$I BTDEFINE.INC}

{$IFDEF CanSetOvrflowCheck}
  {$Q-}
{$ENDIF}

{$IFDEF Ver60}
  !! ERROR - cannot be compiled for Turbo Pascal 6.0
{$ENDIF}
{$IFDEF Ver15}
  !! ERROR - cannot be compiled for Turbo Pascal for Windows 1.5
{$ENDIF}
{$IFDEF Win32}
  !! Error - this unit cannot be compiled for 32-bit
{$ENDIF}


unit MSortP;
  {-Merge sort unit. Requires BP7 (rmode, pmode, Windows)}

interface

uses
{$IFDEF Windows}
  WinTypes,
  WinProcs,
{$ENDIF}
{$IFDEF DPMI}
  WinApi,
{$ENDIF}
{$IFDEF Ver80}                                              {!!.51}
  SysUtils;                                                 {!!.51}
{$ELSE}                                                     {!!.51}
  Strings;
{$ENDIF}                                                    {!!.51}

const
  MinRecsPerRun = 4;    {Minimum number of records in run buffer}
  MergeOrder = 5;       {Input files used at a time during merge, >=2, <=10}
  MaxSelectors = 256;   {Maximum number of selectors allocated}
  SwapThreshold = 64;   {RecLen at least this big causes pointer swap}
  MedianThreshold = 16; {Threshold for using median-of-three quicksort}

type
  ElementIOProc = procedure;
  ElementCompareFunc = function (var X, Y) : Boolean;
  MergeNameFunc = function (Dest : PChar; MergeNum : Word) : PChar;

  MergeInfoRec =
    record                       {Record returned by MergeInfo}
      SortStatus   : Word;       {Predicted status of sort, assuming disk ok}
      MergeFiles   : Word;       {Total number of merge files created}
      MergeHandles : Word;       {Maximum file handles used}
      MergePhases  : Word;       {Number of merge phases}
      MaxDiskSpace : LongInt;    {Maximum peak disk space used}
      HeapUsed     : LongInt;    {Heap space actually used}
      SelectorCount: Word;       {Number of selectors allocated}
      RecsPerSel   : Word;       {Records stored in each selector}
    end;

  function MergeSort(MaxHeapToUse : LongInt;
                     RecLen : Word;
                     SendToSortEngine : ElementIOProc;
                     Less : ElementCompareFunc;
                     GetFromSortEngine : ElementIOProc;
                     MergeName : MergeNameFunc) : Word;
    {-Sorts elements of size RecLen. Uses no more than MaxHeapToUse
      bytes of heap space. Elements are passed into MergeSort by the
      user-defined SendToSortEngine routine. Elements are compared by
      the user-defined Less routine. Sorted elements are passed back
      to the program by the user-defined GetFromSortEngine routine.
      When merge files must be used, the name and location of each
      merge file is determined by the user-defined MergeName routine.
      MergeSort returns a status code:
                 0    success
                 1    user abort
                 8    insufficient memory to sort
                106   invalid input parameter
                        (RecLen zero, MaxHeapToUse too small)
                204   invalid pointer returned by GlobalLock, or
                        SelectorInc <> 8
                213   no elements available to sort
                214   more than 65535 merge files
               else   DOS or Turbo Pascal error code}

  function PutElement(var X) : Boolean;
    {-Submits an element to the sort system. Returns True if the record
      is successfully submitted.}

  function GetElement(var X) : Boolean;
    {-Returns next record in sorted order. Returns True while there are
      more records to return. When it returns False, X is uninitialized.}

  function DefaultMergeName(Dest : PChar; MergeNum : Word) : PChar;
    {-Returns a default name for each merge file (SORnnnnn.TMP)}

  procedure AbortSort;
    {-Call this routine from Less, SendToSortEngine, or GetFromSortEngine
      to abort the sort. The Less function must always return False
      if it calls AbortSort.}

  function OptimumHeapToUse(RecLen : Word; NumRecs : LongInt) : LongInt;
    {-Returns the optimum amount of heap space to sort NumRecs records
      of RecLen bytes each. Less heap space causes merging; more heap
      space is partially unused.}

  function MinimumHeapToUse(RecLen : Word) : LongInt;
    {-Returns the absolute minimum heap that allows MergeSort to succeed}

  procedure MergeInfo(MaxHeapToUse : LongInt;
                      RecLen : Word;
                      NumRecs : LongInt;
                      var MI : MergeInfoRec);
    {-Predicts status and resource usage of a merge sort. See
      MergeInfoRec above for the information returned. Returns
      MI.MaxDiskSpace = -1 in the rare case where disk space analysis
      cannot be performed.}


  procedure InitMergeSort(MaxHeapToUse : LongInt;
                          RecLen : Word;
                          Less : ElementCompareFunc;
                          MergeName : MergeNameFunc);
    {-Initialize merge sort, for non-callback method of sorting}

  procedure DoneMergeSort;
    {-Clean up after merge sort, for non-callback method of sorting}

  function GetSortStatus : Word;
    {-Return sort status variable, useful with non-callback sorting}

  {==================================================================}

implementation

type
  OS =
    record                            {Convenient typecast}
      O : Word;
      S : Word;
    end;
  PointerPtr = ^Pointer;              {Pointer to pointer}
  ElementPtrFunc =
    function (ElNum : LongInt) : Pointer; {Return address of given element}
  SwapElementProc =
    procedure (Pl, Pr : LongInt);     {Swap two elements}

  MergeWordArray =
    array[1..MergeOrder] of Word;     {Handles of open merge files}
  MergePtrArray =
    array[1..MergeOrder] of Pointer;  {Used for managing head elements}
  SelectorArray =
    array[0..MaxSelectors-1] of Word; {Used for managing the run buffer}
  PathArray =
    array[0..79] of Char;             {Used for buffering a pathname}

var
  SortStatus : Word;                  {Current status of sort}
  TotalCount : LongInt;               {Total elements sorted}

  {Variables related to memory management}
  Selectors : SelectorArray;          {Selectors for global work area}
  SelectorCount : Word;               {Number of selectors allocated}
  DSelectorCount : Word;              {Number of selectors for run data}
  RecsPerSel : Word;                  {Number of records mapped by one selector}
  RecsShr : Word;                     {SHR count corresponding to RecsPerSel}
  RecsMask : Word;                    {AND mask corresponding to RecsPerSel}
  RecordLen : Word;                   {Bytes in each data record}
  RecordLenAlloc : Word;              {Bytes in each data record buffer}
  SwapPointers : WordBool;            {True when swapping pointers}

  {Variables related to run sorting}
  AllocatedRecs : LongInt;            {Total records allocated in global buffer}
  RunCapacity : LongInt;              {Capacity (in records) of run buffer}
  RunCount : LongInt;                 {Current number of records in run buffer}
  RunElement : LongInt;               {Last run element passed back to user}
  PivotPtr : Pointer;                 {Pointer to pivot record}
  SwapPtr : Pointer;                  {Pointer to record swap area}
  LessF : ElementCompareFunc;         {User less function}
  ElementPtrF : ElementPtrFunc;       {Element pointer function}
  SwapElementP : SwapElementProc;     {Swap element procedure}

  {Variables related to merging}
  MergeNameF : MergeNameFunc;         {User merge filename function}
  MergeFileCount : Word;              {Number of merge files created}
  MergeFileMerged : Word;             {Index of last merge file merged}
  MergeOpenCount : Word;              {Count of open merge files}
  MergeBufSize : Word;                {Usable bytes in merge buffer}
  MergeFileNumber : MergeWordArray;   {File number of each open merge file}
  MergeFiles : MergeWordArray;        {File handles for merge files}
  MergeSelectors : MergeWordArray;    {Selectors for each merge buffer}
  MergeBytesLoaded : MergeWordArray;  {Count of bytes in each merge buffer}
  MergeBytesUsed : MergeWordArray;    {Bytes used in each merge buffer}
  MergePtrs : MergePtrArray;          {Current head elements in each merge buffer}
  OutFile : Word;                     {Output file handle}
  OutSelector : Word;                 {Selector for output buffer}
  OutBytesUsed : Word;                {Number of bytes in output buffer}

  FirstCallToGetElement : Boolean;    {True until GetElement called once}

  {$DEFINE UseAsm}                    {Undefine only for testing}

{$IFNDEF DPMI}
{$IFNDEF Windows}
  {Emulate a couple of memory allocation functions. These
   work only if Bytes < 65511, which is always true here.
   Requires the heap manager of TP6 or later.}

  const
    gmem_Moveable = $0002;     { Allocate moveable memory }

  type
    THandle = Word;

  function HeapFunc(Size : Word) : Integer; far;
    {-Return nil pointer if insufficient memory}
  begin
    if Size <> 0 then
      HeapFunc := 1;
  end;

  function GlobalAlloc(Flags : Word; Bytes : Longint) : THandle;
  var
    Alloc : Longint;
    P : Pointer;
    SaveHeapError : Pointer;
  begin
    Alloc := Bytes+16;
    if Alloc > 65527 then
      GlobalAlloc := 0
    else begin
      SaveHeapError := HeapError;
      HeapError := @HeapFunc;
      GetMem(P, Alloc);
      if P = nil then
        GlobalAlloc := 0
      else begin
        GlobalAlloc := OS(P).S+1;
        Pointer(Ptr(OS(P).S, 8)^) := P;
        LongInt(Ptr(OS(P).S, 12)^) := Alloc;
      end;
      HeapError := SaveHeapError;
    end;
  end;

  function GlobalFree(H : THandle) : THandle;
  var
    Alloc : Longint;
    P : Pointer;
  begin
    if H <> 0 then begin
      dec(H);
      P := Pointer(Ptr(H, 8)^);
      Alloc := LongInt(Ptr(H, 12)^);
      FreeMem(P, Alloc);
    end;
    GlobalFree := 0;
  end;
{$ENDIF}
{$ENDIF}

  function CreateFile(FName : PChar; var Handle : Word) : Word; assembler;
    {-Create a file, returning status code and open handle}
  asm
    push ds
    lds dx,FName
    mov ah,$3C
    xor cx,cx
    int $21
    jc @Done
    les di,Handle
    mov es:[di],ax
    xor ax,ax
@Done:
    pop ds
  end;

  function OpenFile(FName : PChar; var Handle : Word) : Word; assembler;
    {-Open file read-only, returning status code and open handle}
  asm
    push ds
    lds dx,FName
    mov ax,$3D00      {read only}
    int $21
    jc @Done
    les di,Handle
    mov es:[di],ax
    xor ax,ax
@Done:
    pop ds
  end;

  function BlockWriteFile(Handle : Word; var Buf; BufLen : Word) : Word; assembler;
    {-Write buffer to file, returning status}
  asm
    push ds
    mov bx,Handle
    mov cx,BufLen
    lds dx,Buf
    mov ah,$40
    int $21
    jc  @Done
    cmp ax,cx
    mov ax,101   {disk full}
    jne @Done
    xor ax,ax
@Done:
    pop ds
  end;

  function BlockReadFile(Handle : Word; var Buf;
                         BufLen : Word; var Len : Word) : Word; assembler;
    {-Read buffer from file, returning status and bytes read}
  asm
    push ds
    mov bx,Handle
    mov cx,BufLen
    lds dx,Buf
    mov ah,$3F
    int $21
    jc  @Done
    les di,Len
    mov es:[di],ax
    xor ax,ax
@Done:
    pop ds
  end;

  function CloseFile(Handle : Word) : Word; assembler;
    {-Close file, returning status}
  asm
    mov bx,Handle
    mov ah,$3E
    int $21
    jc @Done
    xor ax,ax
@Done:
  end;

  function DeleteFile(FName : PChar) : Word; assembler;
    {-Delete closed file, returning status}
  asm
    push ds
    lds dx,FName
    mov ah,$41
    int $21
    jc @Done
    xor ax,ax
@Done:
    pop ds
  end;

  function ElementPtrDirect(ElNum : LongInt) : Pointer; far;
    {-Return pointer to given element in the global buffer}
  {$IFDEF UseAsm}
  assembler;
  asm
    mov ax,word ptr ElNum
    mov dx,word ptr ElNum+2
    mov si,ax                               {Save low word of ElNum}
    mov cl,byte ptr RecsShr

    {The following stuff circumvents the use of a 32-bit shift}
    cmp cl,8                                {RecordLenAlloc > 256 bytes?}
    jb  @2                                  {Jump if so}
    cmp cl,16                               {RecordLenAlloc = 1 byte?}
    jne @1                                  {Jump if not}
    mov ax,dx                               {RecordLenAlloc = 1 byte}
    jmp @3
@1: mov al,ah                               {RecordLenAlloc <= 256 bytes}
    mov ah,dl
    sub cl,8
@2: shr ax,cl

@3: shl ax,1                                {ax = selector offset}
    mov bx,ax                               {bx = offset into Selectors}
    mov ax,RecsMask                         {ax = offset mask}
    and ax,si                               {ax = OS(ElNum).O and RecsMask}
    mul word ptr RecordLenAlloc             {ax = data offset}
    mov dx,word ptr Selectors[bx]           {dx:ax = address}
  end;
  {$ELSE}
  begin
    ElementPtrDirect := Ptr(Selectors[ElNum shr byte(RecsShr)],
                            (OS(ElNum).O and RecsMask)*RecordLenAlloc);
  end;
  {$ENDIF}

  function ElementPtrIndirect(ElNum : LongInt) : Pointer; far;
    {-Return pointer to element, assuming that first four bytes
      of buffer are another pointer}
  {$IFDEF UseAsm}
  assembler;
  asm
    mov ax,word ptr ElNum
    mov dx,word ptr ElNum+2
    mov si,ax
    mov cl,byte ptr RecsShr
    cmp cl,8
    jb  @2
    cmp cl,16
    jne @1
    mov ax,dx
    jmp @3
@1: mov al,ah
    mov ah,dl
    sub cl,8
@2: shr ax,cl
@3: shl ax,1
    mov bx,ax
    mov ax,RecsMask
    and ax,si
    mul word ptr RecordLenAlloc
    mov di,ax
    mov es,word ptr Selectors[bx]
    les ax,es:[di]
    mov dx,es
  end;
  {$ELSE}
  begin
    ElementPtrIndirect := PointerPtr(Ptr(Selectors[ElNum shr byte(RecsShr)],
                                     (OS(ElNum).O and RecsMask)*RecordLenAlloc))^;
  end;
  {$ENDIF}

  procedure MoveElement(SPtr, DPtr : Pointer); assembler;
    {-Move one element into another. Assumes SPtr <> DPtr}
  asm
    mov dx,ds
    mov cx,RecordLen
    lds si,SPtr
    les di,DPtr
    cld
    shr cx,1
    rep movsw
    rcl cx,1
    rep movsb
    mov ds,dx
  end;

  procedure SwapElementsDirect(Pl, Pr : LongInt); far;
    {-Swap data of elements}
  var
    LPtr : Pointer;
    RPtr : Pointer;
  begin
    LPtr := ElementPtrDirect(Pl);
    RPtr := ElementPtrDirect(Pr);
    MoveElement(LPtr, SwapPtr);
    MoveElement(RPtr, LPtr);
    MoveElement(SwapPtr, RPtr);
  end;

  procedure SwapElementPtrs(Pl, Pr : LongInt); far;
    {-Swap element pointers}
  {$IFDEF UseAsm}
  assembler;
  asm
    push word ptr Pl+2
    push word ptr Pl
    call ElementPtrDirect
    push dx         {Save result}
    push ax
    push word ptr Pr+2
    push word ptr Pr
    call ElementPtrDirect
    mov bx,ds
    mov es,dx
    mov di,ax       {es:di -> RPtr}
    pop si
    pop ds          {ds:si -> LPtr}
    mov ax,es:[di]
    mov dx,es:[di+2]
    xchg ax,ds:[si]
    xchg dx,ds:[si+2]
    mov es:[di],ax
    mov es:[di+2],dx
    mov ds,bx
  end;
  {$ELSE}
  var
    LPtr : PointerPtr;
    RPtr : PointerPtr;
    TPtr : Pointer;
  begin
    LPtr := ElementPtrDirect(Pl);
    RPtr := ElementPtrDirect(Pr);
    TPtr := LPtr^;
    LPtr^ := RPtr^;
    RPtr^ := TPtr;
  end;
  {$ENDIF}

  procedure QuickSort(L, R : LongInt);
    {-Non-recursive in-memory quicksort}
  const
    StackSize = 32;
  type
    Stack = array[1..StackSize] of LongInt;
  var
    Pl : LongInt;            {Left edge within partition}
    Pr : LongInt;            {Right edge within partition}
    Pm : LongInt;            {Mid-point of partition}
    PartitionLen : LongInt;  {Size of current partition}
    StackP : Integer;        {Stack pointer}
    Lstack : Stack;          {Pending partitions, left edge}
    Rstack : Stack;          {Pending partitions, right edge}
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

        {!!.42, partitioning methods improved}

        Pl := L;
        Pr := R;
        PartitionLen := Pr-Pl+1;

        {Select one of the three partitioning methods}
        {Median is on average fastest, and quite safe}
        {Midpoint is simplest, but may degrade on some data sets}
        {Random is the safest, but on average the slowest}
        {$DEFINE Median}
        {.$DEFINE MidPoint}
        {.$DEFINE Random}

        {$IFDEF MidPoint}
        Pm := Pl+(PartitionLen shr 1);
        {$ENDIF}

        {$IFDEF Random}
        Pm := Pl+Random(PartitionLen);
        {$ENDIF}

        {$IFDEF Median}
        Pm := Pl+(PartitionLen shr 1);
        if PartitionLen >= MedianThreshold then begin
          {Sort elements Pl, Pm, Pr}
          if LessF(ElementPtrF(Pm)^, ElementPtrF(Pl)^) then
            SwapElementP(Pm, Pl);
          if LessF(ElementPtrF(Pr)^, ElementPtrF(Pl)^) then
            SwapElementP(Pr, Pl);
          if LessF(ElementPtrF(Pr)^, ElementPtrF(Pm)^) then
            SwapElementP(Pr, Pm);

          {Exchange Pm with Pr-1 but use Pm's value as the pivot}
          SwapElementP(Pm, Pr-1);
          Pm := Pr-1;

          {Reduce range of swapping}
          inc(Pl);
          dec(Pr, 2);
        end;
        {$ENDIF}

        {Save the pivot element}
        MoveElement(ElementPtrF(Pm), PivotPtr);

        {Swap items in sort order around the pivot}
        repeat
          {$IFDEF UseAsm}
          asm
@0:         push word ptr Pl+2
            push word ptr Pl
            call dword ptr ElementPtrF
            push dx
            push ax
            push word ptr PivotPtr+2
            push word ptr PivotPtr
            call dword ptr LessF
            or al,al
            jz @1
            add word ptr Pl,1
            adc word ptr Pl+2,0
            jmp @0
@1:         push word ptr Pr+2
            push word ptr Pr
            call dword ptr ElementPtrF
            push word ptr PivotPtr+2
            push word ptr PivotPtr
            push dx
            push ax
            call dword ptr LessF
            or al,al
            jz @2
            sub word ptr Pr,1
            sbb word ptr Pr+2,0
            jmp @1
@2:       end;
          {$ELSE}
          while LessF(ElementPtrF(Pl)^, PivotPtr^) do
            Inc(Pl);
          while LessF(PivotPtr^, ElementPtrF(Pr)^) do
            Dec(Pr);
          {$ENDIF}

          {Check for user abort}
          if SortStatus <> 0 then
            Exit;

          if Pl = Pr then begin
            {Reached the pivot}
            Inc(Pl);
            Dec(Pr);
          end else if Pl < Pr then begin
            {Swap elements around the pivot}
            SwapElementP(Pl, Pr);
            Inc(Pl);
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

  procedure CreateNewMergeFile(var Handle : Word);
    {-Create a new merge file}
  var
    FName : PathArray;
  begin
    if MergeFileCount = 65535 then begin
      {Too many merge files}
      SortStatus := 214;
      Exit;
    end;

    {Create new merge file}
    inc(MergeFileCount);
    SortStatus := CreateFile(MergeNameF(FName, MergeFileCount), Handle);
    if SortStatus <> 0 then
      dec(MergeFileCount);
  end;

  procedure FlushOutBuffer;
    {-Write the merge output buffer to disk}
  begin
    if OutBytesUsed <> 0 then
      SortStatus := BlockWriteFile(OutFile, Mem[OutSelector:0], OutBytesUsed);
  end;

  procedure StoreElement(ElPtr : Pointer);
    {-Store element in the merge output buffer}
  begin
    if OutBytesUsed >= MergeBufSize then begin
      FlushOutBuffer;
      if SortStatus <> 0 then
        Exit;
      OutBytesUsed := 0;
    end;
    MoveElement(ElPtr, Ptr(OutSelector, OutBytesUsed));
    inc(OutBytesUsed, RecordLen);
  end;

  procedure StoreNewMergeFile;
    {-Create a new merge file and store run buffer to it}
  label
    ExitPoint;
  var
    SelNum : Word;
    BytesLeft : LongInt;
    BytesToWrite : LongInt;
    ElNum : LongInt;
    TempStatus : Word;
  begin
    {Create new merge file}
    CreateNewMergeFile(OutFile);
    if SortStatus <> 0 then
      Exit;

    if SwapPointers then begin
      {Write the run buffer element by element using pointer indirection}
      OutBytesUsed := 0;
      OutSelector := Selectors[DSelectorCount];
      for ElNum := 0 to RunCount-1 do begin
        StoreElement(ElementPtrIndirect(ElNum));
        if SortStatus <> 0 then
          goto ExitPoint;
      end;
      FlushOutBuffer;

    end else begin
      {Write the run buffer by blocks to the merge file}
      BytesLeft := RunCount*RecordLen;
      BytesToWrite := MergeBufSize;
      SelNum := 0;
      while BytesLeft > 0 do begin
        OutSelector := Selectors[SelNum];
        if BytesLeft < BytesToWrite then
          BytesToWrite := BytesLeft;
        SortStatus := BlockWriteFile(OutFile, Mem[OutSelector:0], BytesToWrite);
        if SortStatus <> 0 then
          BytesLeft := 0
          {Note: all merge files are deleted in MergeSort}
        else begin
          dec(BytesLeft, BytesToWrite);
          inc(SelNum);
        end;
      end;
    end;

ExitPoint:
    {Close merge file}
    TempStatus := CloseFile(OutFile);
    if SortStatus = 0 then
      SortStatus := TempStatus;
  end;

  procedure GetMergeElementPtr(M : Word);
    {-Get pointer to next valid element of specified open merge file}
  var
    Len : Word;
    TempStatus : Word;
    FName : PathArray;
  begin
    if MergeBytesUsed[M] >= MergeBytesLoaded[M] then begin
      {Try to load new data into buffer}
      SortStatus := BlockReadFile(MergeFiles[M], Mem[MergeSelectors[M]:0],
                                  MergeBufSize, Len);
      if (SortStatus <> 0) or (Len < RecordLen) then begin
        {Error reading file or end of file. Close and delete it}
        TempStatus := CloseFile(MergeFiles[M]);
        TempStatus := DeleteFile(MergeNameF(FName, MergeFileNumber[M]));
        {Remove file from merge list}
        if M <> MergeOpenCount then begin
          MergeFileNumber[M] := MergeFileNumber[MergeOpenCount];
          MergeFiles[M] := MergeFiles[MergeOpenCount];
          MergeSelectors[M] := MergeSelectors[MergeOpenCount];
          MergeBytesLoaded[M] := MergeBytesLoaded[MergeOpenCount];
          MergeBytesUsed[M] := MergeBytesUsed[MergeOpenCount];
          MergePtrs[M] := MergePtrs[MergeOpenCount];
        end;
        dec(MergeOpenCount);
        Exit;
      end;
      MergeBytesLoaded[M] := Len;
      MergeBytesUsed[M] := 0;
    end;

    OS(MergePtrs[M]).O := MergeBytesUsed[M];
    inc(MergeBytesUsed[M], RecordLen);
  end;

  procedure OpenMergeFiles;
    {-Open next group of merge files (up to MergeOrder of them)}
  var
    FName : PathArray;
  begin
    MergeOpenCount := 0;
    while (MergeOpenCount < MergeOrder) and (MergeFileMerged < MergeFileCount) do begin
      {MergeOpenCount counts the number of open merge files}
      inc(MergeOpenCount);
      {Open associated merge file}
      inc(MergeFileMerged);
      SortStatus := OpenFile(MergeNameF(FName, MergeFileMerged), MergeFiles[MergeOpenCount]);
      if SortStatus <> 0 then begin
        dec(MergeFileMerged);
        dec(MergeOpenCount);
        Exit;
      end;
      {File number of merge file}
      MergeFileNumber[MergeOpenCount] := MergeFileMerged;
      {Selector for merge file}
      MergeSelectors[MergeOpenCount] := Selectors[MergeOpenCount-1];
      {Number of bytes currently in merge buffer}
      MergeBytesLoaded[MergeOpenCount] := 0;
      {Number of bytes used in merge buffer}
      MergeBytesUsed[MergeOpenCount] := 0;
      {Save the segment of the merge pointer}
      OS(MergePtrs[MergeOpenCount]).S := MergeSelectors[MergeOpenCount];
      {Get the first element}
      GetMergeElementPtr(MergeOpenCount);
      if SortStatus <> 0 then
        Exit;
    end;
  end;

  function GetNextElementIndex : Word;
    {-Return merge index of next element in sorted order, nil if error or none}
  {$IFDEF UseAsm}
  assembler;
  var
    MinElPtr : Pointer;
  asm
    {Get out fast if 0 or 1 merge files left open}
    xor ax,ax
    mov cx,MergeOpenCount
    jcxz @3
    inc ax
    cmp cx,2
    jb @3

    {Assume first element is the least}
    les di,dword ptr MergePtrs
    mov word ptr MinElPtr,di
    mov word ptr MinElPtr+2,es
    mov bx,2

    {Loop to find minimum element}
@1: push ax                            {save result}
    push bx                            {save loop index}
    shl bx,1
    shl bx,1
    les di,dword ptr MergePtrs[bx-4]
    push es                            {save MergePtrs[M]}
    push di
    push es
    push di
    les di,MinElPtr
    push es
    push di
    call dword ptr LessF
    or al,al
    pop di
    pop es
    pop bx
    pop ax
    jz @2
    mov ax,bx
    mov word ptr MinElPtr,di
    mov word ptr MinElPtr+2,es
@2: inc bx
    cmp bx,MergeOpenCount
    jbe @1
@3:
  end;
  {$ELSE}
  var
    M : Word;
    MinElPtr : Pointer;
  begin
    if MergeOpenCount = 0 then begin
      {All merge streams are empty}
      GetNextElementIndex := 0;
      Exit;
    end;

    {Assume first element is the least}
    MinElPtr := MergePtrs[1];
    GetNextElementIndex := 1;

    {Scan the other elements}
    for M := 2 to MergeOpenCount do
      if LessF(MergePtrs[M]^, MinElPtr^) then begin
        GetNextElementIndex := M;
        MinElPtr := MergePtrs[M];
      end;
  end;
  {$ENDIF}

  procedure MergeFileGroup;
    {-Merge the opened merge files into the output}
  var
    NextElementIndex : Word;
    TempStatus : Word;
    Done : WordBool;
  begin
    Done := False;
    repeat
      {Find index of minimum element}
      NextElementIndex := GetNextElementIndex;
      if SortStatus <> 0 then
        Done := True
      else if NextElementIndex = 0 then
        Done := True
      else begin
        {Copy element to output}
        StoreElement(MergePtrs[NextElementIndex]);
        if SortStatus <> 0 then
          Done := True
        else
          {Get the next element from its merge stream}
          GetMergeElementPtr(NextElementIndex);
      end;
    until Done;

    {Flush and close the output file}
    if SortStatus = 0 then
      FlushOutBuffer;
    TempStatus := CloseFile(OutFile);
    if SortStatus = 0 then
      SortStatus := TempStatus;
  end;

  procedure PrimaryMerge;
    {-Merge until there are no more than MergeOrder merge files left}
  begin
    OutSelector := Selectors[MergeOrder];
    while (SortStatus = 0) and (MergeFileCount-MergeFileMerged > MergeOrder) do begin
      {Open next group of MergeOrder files}
      OpenMergeFiles;
      if SortStatus = 0 then begin
        {Create new output file}
        CreateNewMergeFile(OutFile);
        if SortStatus = 0 then begin
          {Merge these files into the output}
          OutBytesUsed := 0;
          MergeFileGroup;
        end;
      end;
    end;
  end;

  procedure DeleteRemainingFiles;
    {-Delete any remaining merge files. Needed only in case of error}
  var
    TempStatus : Word;
    I : Word;
    FName : PathArray;
  begin
    {!!.42 Delete any merge files currently open}
    for I := 1 to MergeOpenCount do begin
      TempStatus := CloseFile(MergeFiles[I]);
      TempStatus := DeleteFile(MergeNameF(FName, MergeFileNumber[I]));
    end;

    for I := MergeFileMerged+1 to MergeFileCount do
      TempStatus := DeleteFile(MergeNameF(FName, I));
  end;

  {$IFDEF Windows}
  procedure AHIncr; far; external 'KERNEL' index 114;
    {-Magic routine for getting the constant to add to scan >64K blocks}
  {$ENDIF}

  function ValidateInput(RecLen : Word) : Word;
    {-Validate the input parameters}
  begin
    {Validate SelectorInc (8 assumed throughout)}
    {$IFDEF DPMI}
    if SelectorInc <> 8 then begin
      ValidateInput := 204;
      Exit;
    end;
    {$ENDIF}
    {$IFDEF Windows}
    if Ofs(AHIncr) <> 8 then begin
      ValidateInput := 204;
      Exit;
    end;
    {$ENDIF}

    if RecLen = 0 then begin
      ValidateInput := 106;
      Exit;
    end;

    ValidateInput := 0;
  end;

  procedure FreeAllHandles;
    {-Free all allocated memory (in handle format)}
  var
    SelNum : Word;
  begin
    if SelectorCount > 0 then
      for SelNum := 0 to SelectorCount-1 do
        GlobalFree(Selectors[SelNum]);
  end;

  function HandlesToSelectors : Word;
    {-Convert handles to selectors}
  var
    SelNum : Word;
    SelectorP : Pointer;
    TempSelectors : SelectorArray;
  begin
    {$IFDEF Windows}
    for SelNum := 0 to SelectorCount-1 do begin
      SelectorP := GlobalLock(Selectors[SelNum]);
      if (SelectorP = nil) or (OS(SelectorP).O <> 0) then begin
        FreeAllHandles;
        HandlesToSelectors := 204;
        Exit;
      end;
      TempSelectors[SelNum] := OS(SelectorP).S;
    end;

    {All succeeded}
    move(TempSelectors, Selectors, SelectorCount*SizeOf(Word));
    {$ENDIF}
    HandlesToSelectors := 0;
  end;

  procedure SelectorsToHandles;
  var
    Handle : THandle;
    SelNum : Word;
  begin
    {$IFDEF Windows}
    for SelNum := 0 to SelectorCount-1 do begin
      Handle := Selectors[SelNum];
      GlobalUnlock(Handle);
      Selectors[SelNum] := GlobalHandle(Handle);
    end;
    {$ENDIF}
  end;

  procedure GetMaxRecsPerSel(RecLen : Word);
    {-Compute maximum RecsPerSel and RecsShr for given RecLen}
  var
    R : LongInt;
  begin
    R := 1;
    RecsShr := 0;
    while R*RecLen < 65536 do begin
      R := R shl 1;
      inc(RecsShr);
    end;
    if RecsShr > 0 then begin
      R := R shr 1;
      dec(RecsShr);
    end;
    RecsPerSel := R;
  end;

  function GetHandles(RecLen : Word; MaxHeapToUse : LongInt) : Word;
    {-Compute segment sizes and allocate memory}
  var
    Handle : THandle;
    InitAvail : LongInt;
    SegmentSize : Word;
    TooMuchHeapUsed : WordBool;
  begin
    {Swap elements or pointers?}
    SwapPointers := (RecLen >= SwapThreshold) and
                    (RecLen <= 65535-SizeOf(Pointer));

    {Adjust for pointer swapping}
    RecordLen := RecLen;
    if SwapPointers then begin
      {Allocate an extra pointer for each record and swap just the pointers}
      RecordLenAlloc := RecordLen+SizeOf(Pointer);
      ElementPtrF := ElementPtrIndirect;
      SwapElementP := SwapElementPtrs;
    end else begin
      RecordLenAlloc := RecordLen;
      ElementPtrF := ElementPtrDirect;
      SwapElementP := SwapElementsDirect;
    end;

    {Compute largest power-of-two number of recs that fit into 64K}
    GetMaxRecsPerSel(RecordLenAlloc);

    {Search for valid combinations of selectors}
    repeat
      {Allocate as many handles as possible in memory given}
      SelectorCount := 0;
      InitAvail := MemAvail;
      repeat
        {Allocate next handle}
        Handle := GlobalAlloc(gmem_Moveable, RecsPerSel*RecordLenAlloc);
        Selectors[SelectorCount] := Handle;
        inc(SelectorCount);
        TooMuchHeapUsed := (InitAvail-MemAvail > MaxHeapToUse);
      until (SelectorCount = MaxSelectors) or (Handle = 0) or TooMuchHeapUsed;

      if TooMuchHeapUsed then begin
        {Deallocate last handle to keep within heap quota}
        Handle := GlobalFree(Handle);
        dec(SelectorCount);
        {If we fail, it's because MaxHeapToUse was too small}
        GetHandles := 106;
      end else if Handle = 0 then begin
        {Last handle wasn't allocated}
        dec(SelectorCount);
        {If we fail, it's because there was insufficient heap space}
        GetHandles := 8;
      end;

      if SelectorCount < MergeOrder+1 then begin
        {Not enough selectors, cut segment size in two}
        FreeAllHandles;
        RecsPerSel := RecsPerSel shr 1;
        dec(RecsShr);
      end;
    until (SelectorCount >= MergeOrder+1) or (RecsPerSel = 0);

    if RecsPerSel = 0 then
      {No way to get enough buffers}
      Exit;

    RecsMask := RecsPerSel-1;
    SegmentSize := RecsPerSel*RecordLenAlloc;
    MergeBufSize := (SegmentSize div RecordLen)*RecordLen;

    if SwapPointers then begin
      {Last segment reserved for sorted run output buffer}
      DSelectorCount := SelectorCount-1;
      AllocatedRecs := LongInt(RecsPerSel)*DSelectorCount;
      {PivotPtr := ElementPtrDirect(AllocatedRecs-1);} {!!.50}
      {inc(OS(PivotPtr).O, SizeOf(Pointer));}          {!!.50}
      RunCapacity := AllocatedRecs-1;
    end else begin
      DSelectorCount := SelectorCount;
      AllocatedRecs := LongInt(RecsPerSel)*DSelectorCount;
      {PivotPtr := ElementPtrDirect(AllocatedRecs-1);} {!!.50}
      {SwapPtr := ElementPtrDirect(AllocatedRecs-2);}  {!!.50}
      RunCapacity := AllocatedRecs-2;
    end;

    if RunCapacity < MinRecsPerRun then begin
      {No way to get enough memory in enough buffers}
      FreeAllHandles;
      Exit;
    end;

    GetHandles := 0;
  end;

  procedure AssignPivotAndSwap; {!!.50}
    {-Compute PivotPtr and SwapPtr for the current sort}
  begin
    if SwapPointers then begin
      PivotPtr := ElementPtrDirect(AllocatedRecs-1);
      inc(OS(PivotPtr).O, SizeOf(Pointer));
      SwapPtr := nil;
    end else begin
      PivotPtr := ElementPtrDirect(AllocatedRecs-1);
      SwapPtr := ElementPtrDirect(AllocatedRecs-2);
    end;
  end;

  procedure InitMergeSort(MaxHeapToUse : LongInt;
                          RecLen : Word;
                          Less : ElementCompareFunc;
                          MergeName : MergeNameFunc);
  begin
    {Validate input parameters}
    SortStatus := ValidateInput(RecLen);

    {Compute selector sizes and allocate buffers}
    if SortStatus = 0 then
      SortStatus := GetHandles(RecLen, MaxHeapToUse);

    {Convert handles to selectors}
    if SortStatus = 0 then
      SortStatus := HandlesToSelectors;

    {Get out if any error occurred}
    if SortStatus <> 0 then
      Exit;

    {Compute PivotPtr and SwapPtr}  {!!.50}
    AssignPivotAndSwap;             {!!.50}

    {Copy parameters to global variables and initialize other globals}
    LessF := Less;
    MergeNameF := MergeName;
    RunCount := 0;
    TotalCount := 0;
    MergeFileCount := 0;
    MergeFileMerged := 0;
    FirstCallToGetElement := True;
  end;

  procedure DoneMergeSort;
  begin
    {Assure all merge files are gone}
    DeleteRemainingFiles;

    {Free global data}
    SelectorsToHandles;
    FreeAllHandles;
  end;

  function MergeSort(MaxHeapToUse : LongInt;
                     RecLen : Word;
                     SendToSortEngine : ElementIOProc;
                     Less : ElementCompareFunc;
                     GetFromSortEngine : ElementIOProc;
                     MergeName : MergeNameFunc) : Word;
  begin
    {initialize the merge sort}
    InitMergeSort(MaxHeapToUse, RecLen, Less, MergeName);

    if SortStatus = 0 then begin
      {Get all the elements from the user}
      SendToSortEngine;

      if SortStatus = 0 then
        {Pass elements back to the user}
        GetFromSortEngine;

      {Perform final clean up and shut down the sort}
      DoneMergeSort;
    end;

    {Return status}
    MergeSort := SortStatus;
  end;

  function PutElement(var X) : Boolean;
  var
    SwapPtr : PointerPtr;
    DataPtr : Pointer;
  begin
    if SortStatus <> 0 then begin
      PutElement := False;
      Exit;
    end;

    if RunCount >= RunCapacity then begin
      {Sort run buffer}
      QuickSort(0, RunCount-1);
      {Store to merge file}
      StoreNewMergeFile;
      if SortStatus <> 0 then begin
        {File operation failed}
        PutElement := False;
        Exit;
      end;
      Inc(TotalCount, RunCount);
      RunCount := 0;
    end;

    {Store the element in the run buffer}
    if SwapPointers then begin
      SwapPtr := ElementPtrDirect(RunCount);
      DataPtr := Ptr(OS(SwapPtr).S, OS(SwapPtr).O+SizeOf(Pointer));
      SwapPtr^ := DataPtr;
    end else
      DataPtr := ElementPtrDirect(RunCount);

    MoveElement(@X, DataPtr);
    Inc(RunCount);
    PutElement := True;
  end;

  function GetElement(var X) : Boolean;
  var
    NextElementIndex : Word;
  begin
    {if first call to get element, then perform final sorting}
    if FirstCallToGetElement then begin
      Inc(TotalCount, RunCount);
      if TotalCount = 0 then
        SortStatus := 213;

      if SortStatus = 0 then
        if RunCount > 0 then begin
          {Sort the last run buffer}
          QuickSort(0, RunCount-1);
          if MergeFileCount > 0 then
            {There's already a merge file, create another}
            StoreNewMergeFile;
        end;

      if SortStatus = 0 then
        if MergeFileCount > 0 then begin
          {Perform primary merging}
          PrimaryMerge;
          if SortStatus = 0 then
            {Open the last group of files}
            OpenMergeFiles;
        end else
          {Prepare to return elements from run buffer}
          RunElement := 0;

      {disable first call to GetElement flag}
      FirstCalltoGetElement := false;
    end;

    if SortStatus <> 0 then
      GetElement := False

    else if MergeFileCount = 0 then begin
      {No merging required}
      if RunElement >= RunCount then
        {No more elements}
        GetElement := False
      else begin
        MoveElement(ElementPtrF(RunElement), @X);
        inc(RunElement);
        GetElement := True;
      end;

    end else begin
      {Get next merge element}
      NextElementIndex := GetNextElementIndex;
      if NextElementIndex = 0 then
        {No more elements or error}
        GetElement := False
      else begin
        {Return the element}
        MoveElement(MergePtrs[NextElementIndex], @X);
        {Get pointer to next element in the stream just used}
        GetMergeElementPtr(NextElementIndex);
        GetElement := True;
      end;
    end;
  end;

  function DefaultMergeName(Dest : PChar; MergeNum : Word) : PChar;
  var
    S : array[0..5] of Char;
  begin
    Str(MergeNum, S);
    DefaultMergeName := StrCat(StrCat(StrCopy(Dest, 'SOR'), S), '.TMP');
  end;

  procedure AbortSort;
  begin
    SortStatus := 1;
  end;

  function OptimumHeapToUse(RecLen : Word; NumRecs : LongInt) : LongInt;
  begin
    {Swap elements or pointers?}
    SwapPointers := (RecLen >= SwapThreshold) and
                    (RecLen <= 65535-SizeOf(Pointer));
    if SwapPointers then
      inc(RecLen, SizeOf(Pointer))
    else
      {Account for swap element}
      inc(NumRecs);
    {Account for pivot element}
    inc(NumRecs);

    {Compute largest power-of-two number of recs that fit into 64K}
    GetMaxRecsPerSel(RecLen);

    {Compute number of selectors}
    repeat
      SelectorCount := NumRecs div RecsPerSel;
      if NumRecs mod RecsPerSel <> 0 then
        inc(SelectorCount);
      if SwapPointers then
        {Last selector used for run output buffer when swapping pointers}
        inc(SelectorCount);
      if SelectorCount < MergeOrder+1 then
        RecsPerSel := RecsPerSel shr 1;
    until (SelectorCount >= MergeOrder+1) or (RecsPerSel = 0);

    {!!.42 end of function modified}

    if RecsPerSel = 0 then begin
      {Special case for very small number of records}
      RecsPerSel := 1;
      SelectorCount := MergeOrder+1;
    end;

    if SwapPointers then
      {Last segment reserved for merge output buffer}
      inc(SelectorCount);
    {Assume 32 byte overhead per selector and 2048 byte fixed overhead}
    OptimumHeapToUse := 2048+
                        SelectorCount*(LongInt(RecsPerSel)*RecLen+32);
  end;

  function MinimumHeapToUse(RecLen : Word) : LongInt;
  var
    MinHeapUsed : LongInt;
    HeapToUse : LongInt;
  begin
    {Swap elements or pointers?}
    SwapPointers := (RecLen >= SwapThreshold) and
                    (RecLen <= 65535-SizeOf(Pointer));
    if SwapPointers then
      inc(RecLen, SizeOf(Pointer));

    {Compute largest power-of-two number of recs that fit into 64K}
    GetMaxRecsPerSel(RecLen);

    {Try all valid RecsPerSel}
    MinHeapUsed := MaxLongInt;
    repeat
      {Try minimum number of selectors}
      SelectorCount := MergeOrder+1;
      repeat
        AllocatedRecs := LongInt(RecsPerSel)*SelectorCount;
        if SwapPointers then
          RunCapacity := AllocatedRecs-RecsPerSel-1
        else
          RunCapacity := AllocatedRecs-2;
        if RunCapacity < MinRecsPerRun then
          inc(SelectorCount);
      until RunCapacity >= MinRecsPerRun;
      HeapToUse := 2048+SelectorCount*(LongInt(RecsPerSel)*RecLen+32);
      if HeapToUse < MinHeapUsed then
        MinHeapUsed := HeapToUse;
      RecsPerSel := RecsPerSel shr 1;
    until RecsPerSel = 0;

    MinimumHeapToUse := MinHeapUsed;
  end;

  procedure MergeInfo(MaxHeapToUse : LongInt;
                      RecLen : Word;
                      NumRecs : LongInt;
                      var MI : MergeInfoRec);
  type
    MergeFileSizeArray = array[1..16383] of LongInt;
  var
    InitAvail : LongInt;
    RecordsLeft : LongInt;
    RecordsInFile : LongInt;
    DiskSpace : LongInt;
    OutputSpace : LongInt;
    PeakDiskSpace : LongInt;
    MFileCount : LongInt;
    RecsNeeded : LongInt;
    SizeBufSize : Word;
    MergeFileSizeP : ^MergeFileSizeArray;
  begin
    {Set defaults for the MergeInfoRec}
    FillChar(MI, SizeOf(MergeInfoRec), 0);

    {Validate input parameters}
    SortStatus := ValidateInput(RecLen);
    if SortStatus = 0 then
      if NumRecs = 0 then
        SortStatus := 213;

    {Compute selector sizes and allocate buffers}
    if SortStatus = 0 then begin
      InitAvail := MemAvail;
      SortStatus := GetHandles(RecLen, MaxHeapToUse);
    end;

    {Get out if sort is predicted to fail}
    if SortStatus <> 0 then begin
      MI.SortStatus := SortStatus;
      Exit;
    end;

    {Compute amount of memory used while getting handles}
    dec(InitAvail, MemAvail);
    MI.HeapUsed := InitAvail;

    {Deallocate the memory allocated by GetHandles}
    FreeAllHandles;

    RecsNeeded := NumRecs+1;
    if not SwapPointers then
      inc(RecsNeeded);

    if DSelectorCount*LongInt(RecsPerSel) >= RecsNeeded then begin
      {All the records fit into memory}
      MI.SelectorCount := SelectorCount;
      MI.RecsPerSel := RecsPerSel;
      Exit;
    end;

    {Store the information we already know}
    MI.SelectorCount := SelectorCount;
    MI.RecsPerSel := RecsPerSel;

    {Compute initial number of merge files and disk space}
    MFileCount := NumRecs div RunCapacity;
    if NumRecs mod RunCapacity <> 0 then
      inc(MFileCount);
    if MFileCount > 65535 then begin
      MI.SortStatus := 214;
      Exit;
    end;
    MergeFileCount := MFileCount;
    DiskSpace := NumRecs*RecordLen;

    {At least one merge phase required}
    MI.MergePhases := 1;

    if MergeFileCount <= MergeOrder then begin
      {Only one merge phase, direct to user}
      MI.MergeFiles := MergeFileCount;
      MI.MergeHandles := MergeFileCount;
      MI.MaxDiskSpace := DiskSpace;
      Exit;
    end;

    {Compute total number of merge files and merge phases}
    MergeFileMerged := 0;
    while MergeFileCount-MergeFileMerged > MergeOrder do begin
      inc(MI.MergePhases);
      MergeOpenCount := 0;
      while (MergeOpenCount < MergeOrder) and (MergeFileMerged < MergeFileCount) do begin
        inc(MergeOpenCount);
        inc(MergeFileMerged);
      end;
      inc(MergeFileCount);
    end;

    {Store the information we already know}
    MI.MergeFiles := MergeFileCount;
    MI.MergeHandles := MergeOrder+1; {MergeOrder input files, 1 output file}

    {Determine whether the disk space analysis can proceed}
    SizeBufSize := MergeFileCount*SizeOf(LongInt);
    if (MergeFileCount > 16383) or (MaxAvail < SizeBufSize) then begin
      MI.MaxDiskSpace := -1;
      Exit;
    end;

    {Allocate file size array}
    GetMem(MergeFileSizeP, SizeBufSize);

    {Compute size of initial merge files}
    RecordsLeft := NumRecs;
    MergeFileCount := 0;
    while RecordsLeft > 0 do begin
      inc(MergeFileCount);
      if RecordsLeft >= RunCapacity then
        RecordsInFile := RunCapacity
      else
        RecordsInFile := RecordsLeft;
      MergeFileSizeP^[MergeFileCount] := RecordsInFile*RecordLen;
      dec(RecordsLeft, RecordsInFile);
    end;

    {Carry sizes forward to get disk space used}
    PeakDiskSpace := DiskSpace;
    MergeFileMerged := 0;
    while MergeFileCount-MergeFileMerged > MergeOrder do begin
      MergeOpenCount := 0;
      OutputSpace := 0;
      while (MergeOpenCount < MergeOrder) and (MergeFileMerged < MergeFileCount) do begin
        inc(MergeOpenCount);
        inc(MergeFileMerged);
        inc(OutputSpace, MergeFileSizeP^[MergeFileMerged]);
      end;
      inc(MergeFileCount);
      {Save size of output file}
      MergeFileSizeP^[MergeFileCount] := OutputSpace;
      {Output file and input files coexist temporarily}
      inc(DiskSpace, OutputSpace);
      {Store new peak disk space}
      if DiskSpace > PeakDiskSpace then
        PeakDiskSpace := DiskSpace;
      {Account for deleting input files}
      dec(DiskSpace, OutputSpace);
    end;
    MI.MaxDiskSpace := PeakDiskSpace;

    FreeMem(MergeFileSizeP, SizeBufSize);
  end;

  function GetSortStatus : Word;
  begin
    GetSortStatus := SortStatus;
  end;

end.
