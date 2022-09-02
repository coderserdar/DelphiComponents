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

{$S-,R-,I-,V-,B-,F-}

{won't work under Windows}
{$IFDEF Windows}
  **ERROR** Not compatible with Turbo Pascal for Windows
{$ENDIF}
{$IFDEF DPMI}
  **ERROR** Not compatible with Turbo Pascal DPMI
{$ENDIF}

{$IFNDEF Ver40}
  {Allow overlays}
  {$F+,A-,O+}
{$ENDIF}

{Activates different code for TP6 heap manager}
{$DEFINE Heap6}
{$IFDEF Ver50}
  {$UNDEF Heap6}
{$ENDIF}
{$IFDEF Ver55}
  {$UNDEF Heap6}
{$ENDIF}

unit TpAlloc;
  {-Routines for allocating/deallocating blocks of memory larger than 64K}

interface

type
  SegOfs =                   {structure of a pointer}
    record
      Ofst, Segm : Word;
    end;

  {----- memory management routines -----}

procedure HugeGetMem(var Pt; Bytes : LongInt);
  {-Allocate a block of memory of size Bytes and store pointer to it in
    Pt. Pt is nil if Bytes > MaxAvail}

procedure HugeFreeMem(var Pt; Bytes : LongInt);
  {-Deallocate a block of memory of size Bytes pointed to by Pt, a pointer
    variable. Pt is set to nil on Exit. Does nothing if Pt is nil.}

  {----- pointer manipulation routines -----}

function Linear(P : Pointer) : LongInt;
  {-Converts a pointer to a linear address to allow differences in addresses
    to be calculated. The pointer must be in the range $0:$0 to $FFFF:$000F.}

function LinearToPointer(L : LongInt) : Pointer;
  {-Return linear address L as a normalized pointer}

function PtrDiff(P1, P2 : Pointer) : LongInt;
  {-Return the number of bytes between P1^ and P2^}

function Normalized(P : Pointer) : Pointer;
  {-Return P as a normalized pointer}
  inline(
    $58/                     {pop ax    ;pop offset into AX}
    $5A/                     {pop dx    ;pop segment into DX}
    $89/$C3/                 {mov bx,ax ;BX = Ofs(P^)}
    $B1/$04/                 {mov cl,4  ;CL = 4}
    $D3/$EB/                 {shr bx,cl ;BX = Ofs(P^) div 16}
    $01/$DA/                 {add dx,bx ;add BX to segment}
    $25/$0F/$00);            {and ax,$F ;mask out unwanted bits in offset}

  {=============================================================}

implementation

type
  FreeListRecPtr = ^FreeListRec;
  FreeListRec =              {structure of a free list entry}
    record
      {$IFDEF Heap6}
      Next : FreeListRecPtr; {pointer to next free list record}
      Size : Pointer;        {"normalized pointer" representing size}
      {$ELSE}
      OrgPtr : Pointer;      {pointer to the start of the block}
      EndPtr : Pointer;      {pointer to the end of the block}
      {$ENDIF}
    end;

  function Linear(P : Pointer) : LongInt;
    {-Converts a pointer to a linear address to allow differences in addresses
      to be calculated. The pointer must be in the range $0:$0 to $FFFF:$000F.}
  begin
    with SegOfs(P) do
      Linear := (LongInt(Segm) shl 4)+LongInt(Ofst);
  end;

  function LinearToPointer(L : LongInt) : Pointer;
    {-Return linear address L as a normalized pointer}
  begin
    LinearToPointer := Ptr(Word(L shr 4), Word(L and $0000000F));
  end;

  function PtrDiff(P1, P2 : Pointer) : LongInt;
    {-Return the number of bytes between P1^ and P2^}
  begin
    PtrDiff := Abs(Linear(P1)-Linear(P2));
  end;

{$IFDEF Heap6}

  procedure HugeGetMem(var Pt; Bytes : LongInt);
    {-Allocate a block of memory of size Bytes and store pointer to it in
      Pt. Pt is nil if Bytes > MaxAvail}
  var
    ThisP : Pointer absolute Pt;
    P : FreeListRecPtr;
    Prev : FreeListRecPtr;
    ThisBlock : LongInt;
  begin
    {initialize in case of failure}
    ThisP := nil;

    {round bytes up to multiple of 8}
    Bytes := (Bytes+7) and $FFFFFFF8;

    {scan the free list}
    P := FreeList;
    Prev := nil;
    while P <> HeapPtr do begin
      {get the size of this block}
      ThisBlock := Linear(P^.Size);
      if ThisBlock > Bytes then begin
        {block is bigger than we need, shrink the size}
        dec(ThisBlock, Bytes);
        ThisP := LinearToPointer(Linear(P)+ThisBlock);
        P^.Size := LinearToPointer(ThisBlock);
        Exit;
      end else if ThisBlock = Bytes then begin
        {this block is just right, remove it from list}
        ThisP := P;
        if Prev = nil then
          FreeList := P^.Next
        else
          Prev^.Next := P^.Next;
        Exit;
      end;
      {next free list record}
      Prev := P;
      P := P^.Next;
    end;

    {check block at HeapPtr^}
    if PtrDiff(HeapEnd, HeapPtr) >= Bytes then begin
      {use this block}
      ThisP := HeapPtr;
      {adjust HeapPtr}
      HeapPtr := LinearToPointer(Linear(HeapPtr)+Bytes);
      {adjust free list}
      if Prev = nil then
        FreeList := HeapPtr
      else
        Prev^.Next := HeapPtr;
    end;
  end;

  procedure HugeFreeMem(var Pt; Bytes : LongInt);
    {-Deallocate a block of memory of size Bytes pointed to by Pt, a pointer
      variable. Pt is set to nil on Exit. Does nothing if Pt is nil.}
  label
    ExitPoint;
  var
    ThisP : FreeListRecPtr absolute Pt;
    ThisL : LongInt;
    ThisE : LongInt;
    BytesP : Pointer;
    P : FreeListRecPtr;
    PL : LongInt;
    Prev : FreeListRecPtr;

    procedure MergeThisWithNext;
    var
      PSize : LongInt;
    begin
      if ThisE > PL then
        {this block overlaps next one}
        RunError(204)
      else if ThisE = PL then begin
        {this block merges into next one}
        ThisP^.Next := P^.Next;
        PSize := Linear(P^.Size);
        inc(ThisE, PSize);
        ThisP^.Size := LinearToPointer(PSize+Bytes);
      end else begin
        {link new block into chain}
        ThisP^.Next := P;
        ThisP^.Size := BytesP;
      end;
    end;

    procedure MergePrevWithThis;
    var
      PrevL : LongInt;
      PrevSize : LongInt;
    begin
      PrevSize := Linear(Prev^.Size);
      PrevL := Linear(Prev);
      if PrevL+PrevSize > ThisL then
        {previous block overlaps this one}
        RunError(204)
      else if PrevL+PrevSize = ThisL then begin
        {previous block merges into this one}
        inc(Bytes, PrevSize);
        BytesP := LinearToPointer(Bytes);
        Prev^.Size := BytesP;
        {refer to previous block for further merging}
        ThisP := Prev;
        ThisL := PrevL;
      end else
        {link new block into chain}
        Prev^.Next := ThisP;
    end;

    procedure MergeThisWithFree;
    var
      HL : LongInt;
      P : FreeListRecPtr;
    begin
      HL := Linear(HeapPtr);
      if ThisE > HL then
        {this block overlaps the free heap}
        RunError(204)
      else if ThisE = HL then begin
        {this block merges into HeapPtr}
        HeapPtr := ThisP;
        if (Prev <> nil) and (ThisP <> Prev) then
          {link new block into chain}
          Prev^.Next := HeapPtr
        else if (Prev = nil) or (Prev = FreeList) then
          {no more free blocks}
          FreeList := HeapPtr
        else begin
          {need to find Prev's predecessor}
          P := FreeList;
          while P^.Next <> Prev do
            P := P^.Next;
          P^.Next := Prev;
        end;
      end else begin
        {this is the new top block}
        if Prev = nil then
          FreeList := ThisP
        else
          Prev^.Next := ThisP;
        ThisP^.Next := HeapPtr;
        ThisP^.Size :=BytesP;
      end;
    end;

  begin
    {exit if pointer is nil or no bytes requested}
    if (ThisP = nil) or (Bytes = 0) then
      Exit;

    {error if pointer offset is not 0 or 8}
    if (SegOfs(ThisP).Ofst <> 0) and (SegOfs(ThisP).Ofst <> 8) then
      RunError(204);

    {error if block is below the heap}
    if SegOfs(ThisP).Segm < SegOfs(HeapOrg).Segm then
      RunError(204);
    {block above HeapPtr is checked later}

    {round bytes up to multiple of 8}
    Bytes := (Bytes+7) and $FFFFFFF8;
    BytesP := LinearToPointer(Bytes);

    {get the pointer in linear format}
    ThisL := Linear(ThisP);
    ThisE := ThisL+Bytes;

    {scan the free list}
    P := FreeList;
    Prev := nil;
    while P <> HeapPtr do begin
      PL := Linear(P);
      if PL = ThisL then
        {freeing an already freed block}
        RunError(204)
      else if PL > ThisL then begin
        {passed the new block, time to merge it}
        if Prev = nil then begin
          {new block is lower than any existing block}
          MergeThisWithNext;
          FreeList := ThisP;
        end else begin
          {new block is between two existing blocks}
          MergePrevWithThis;
          MergeThisWithNext;
        end;
        goto ExitPoint;
      end;
      {next free list record}
      Prev := P;
      P := P^.Next;
    end;

    {new block is higher than any existing block}
    if Prev <> nil then
      {this block is after at least one other free block}
      MergePrevWithThis;
    MergeThisWithFree;

ExitPoint:
    ThisP := nil;
  end;

{$ELSE}

  procedure HugeGetMem(var Pt; Bytes : LongInt);
    {-Allocate a block of memory of size Bytes and store pointer to it in
      Pt. Pt is nil if Bytes > MaxAvail}
  var
    ThisP : Pointer absolute Pt;
    P : FreeListRecPtr;
    Top : Pointer;
    ThisBlock : LongInt;
  begin
    ThisP := nil;

    {point to end of free list}
    P := FreePtr;
    if SegOfs(P).Ofst = 0 then
      Inc(SegOfs(P).Segm, $1000);

    {point to top of free memory}
    if FreeMin = 0 then
      Top := Ptr(SegOfs(FreePtr).Segm+$1000, 0)
    else
      Top := Ptr(SegOfs(FreePtr).Segm, -FreeMin);
    if Linear(P) < Linear(Top) then
      Top := P;

    {check block at HeapPtr^}
    if PtrDiff(Top, HeapPtr) >= Bytes then begin
      {use this block}
      ThisP := HeapPtr;

      {adjust HeapPtr}
      HeapPtr := LinearToPointer(Linear(HeapPtr)+Bytes);
    end
    else while SegOfs(P).Ofst <> 0 do begin
      {search the free list for a memory block that is big enough}
      with P^ do begin
        {calculate the size of the block}
        ThisBlock := PtrDiff(EndPtr, OrgPtr);

        if ThisBlock > Bytes then begin
          {bigger than we need--shrink the size of the block}
          ThisP := OrgPtr;
          OrgPtr := LinearToPointer(Linear(OrgPtr)+Bytes);
          Exit;
        end
        else if ThisBlock = Bytes then begin
          {exact size--remove the record from the free list}
          ThisP := OrgPtr;

          {move the entry at the bottom of the free list up}
          P^ := FreeListRecPtr(FreePtr)^;

          {adjust FreePtr}
          with SegOfs(FreePtr) do
            Inc(Ofst, SizeOf(FreeListRec));

          Exit;
        end;
      end;

      {point to next record on free list}
      Inc(SegOfs(P).Ofst, SizeOf(FreeListRec));
    end;
  end;

  procedure HugeFreeMem(var Pt; Bytes : LongInt);
    {-Deallocate a block of memory of size Bytes pointed to by Pt, a pointer
      variable. Pt is set to nil on Exit. Does nothing if Pt is nil.}
  var
    P : Pointer absolute Pt;
    EndP : Pointer;
    FP, SaveFP, NewFreePtr : FreeListRecPtr;
    I : Word;
    Found : Boolean;
  begin
    {exit if P is nil}
    if (P = nil) then
      Exit;

    {calculate pointer to end of block}
    EndP := LinearToPointer(Linear(P)+Bytes);

    {see if this is just below HeapPtr^}
    if EndP = HeapPtr then
      {just reset HeapPtr}
      HeapPtr := P
    else begin
      {search for a free list entry to combine this block with}
      Found := False;
      FP := FreePtr;
      while (SegOfs(FP).Ofst <> 0) and not Found do begin
        with FP^ do
          {does the end of our block match the start of this one?}
          if OrgPtr = EndP then begin
            OrgPtr := P;
            Found := True;
          end
          {does the start of our block match the end of this one?}
          else if EndPtr = P then begin
            EndPtr := EndP;
            Found := True;
          end;

        {point to next record on free list}
        if not Found then
          Inc(SegOfs(FP).Ofst, SizeOf(FreeListRec));
      end;

      if Found then begin
        {save pointer into free list and get pointers to search for}
        SaveFP := FP;
        with FP^ do begin
          P := OrgPtr;
          EndP := EndPtr;
        end;

        {see if we can combine this block with a second}
        Found := False;
        FP := FreePtr;
        while (SegOfs(FP).Ofst <> 0) and not Found do begin
          with FP^ do
            {does the end of our block match the start of this one?}
            if OrgPtr = EndP then begin
              OrgPtr := P;
              Found := True;
            end
            {does the start of our block match the end of this one?}
            else if EndPtr = P then begin
              EndPtr := EndP;
              Found := True;
            end;

          {point to next record on free list}
          if not Found then
            Inc(SegOfs(FP).Ofst, SizeOf(FreeListRec));
        end;

        if Found then begin
          {we combined two blocks--get rid of the 1st free list entry we found}

          {move the entry at the bottom of the free list up into its place}
          SaveFP^ := FreeListRecPtr(FreePtr)^;

          {adjust FreePtr}
          with SegOfs(FreePtr) do
            Inc(Ofst, SizeOf(FreeListRec));
        end;
      end
      else begin
        {can't combine with anything--add an entry to the free list}

        {calculate new FreePtr}
        with SegOfs(FreePtr) do
          NewFreePtr := Ptr(Segm, Ofst-SizeOf(FreeListRec));

        {make sure the free list isn't full}
        with SegOfs(NewFreePtr) do
          if (Linear(NewFreePtr) < Linear(HeapPtr)) or (Ofst = 0) then begin
            {it's full--let real FreeMem generate a runtime error}
            if Bytes > 65521 then
              I := 65521
            else
              I := Bytes;
            FreeMem(P, I);
            Exit;
          end;

        {fill in the new free list entry}
        with NewFreePtr^ do begin
          OrgPtr := P;
          EndPtr := EndP;
        end;

        {adjust FreePtr}
        FreePtr := NewFreePtr;
      end;

      {set P to nil}
      P := nil;
    end;
  end;

{$ENDIF}

end.
