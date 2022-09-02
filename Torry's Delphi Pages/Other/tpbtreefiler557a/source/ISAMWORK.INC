{********************************************************************}
{* ISAMWORK.INC                                                     *}
{********************************************************************}

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

{$IFDEF LengthByteKeys}
procedure IsamMakeStrZ(var UserKey, UserKeyZ : IsamKeyStr);
begin
  Move(UserKey, UserKeyZ, Succ(Word(Length(UserKey))));
end;


procedure IsamMakeStr(var UserKeyZ, UserKey : IsamKeyStr);
begin
  Move(UserKeyZ, UserKey, Succ(Word(Length(UserKeyZ))));
end;
{$ENDIF}

{$IFDEF ASCIIZeroKeys}
procedure IsamMakeStrZ(var UserKey, UserKeyZ : IsamKeyStr);
var
  L : Word;
begin
  L := Length (UserKey);
  if L > MaxKeyLen then
    L := MaxKeyLen;
  Move(UserKey [1], UserKeyZ, L);
  UserKeyZ [L] := #0;
end;


procedure IsamMakeStr(var UserKeyZ, UserKey : IsamKeyStr);
var
  L : Word;
begin
  L := 0;
  while UserKeyZ [L] <> #0 do begin
    Inc (L);
  end;
  if L <> 0 then begin
    Move(UserKeyZ, UserKey [1], L);
  end;
  UserKey [0] := Chr (L);
end;
{$ENDIF}

procedure IsamPack(var Page : IsamPage; KeyL : Word);
var
  I, K, S : Word;
  P       : Array [0..0] Of Byte absolute Page; {Real bounds [0..65535]}
begin
  K := KeyL + 9;
  S := K + 6;
  if KeyL <> MaxKeyLen then begin
    for I := 2 to Page.ItemsOnPage do begin
      Move(Page.ItemArray[I], P[S], K);
      Inc(S, K);
    end;
  end;
end;


procedure IsamUnPack(var Page : IsamPage; KeyL : Word);
var
  I, K, S : Word;
  P       : Array [0..0] Of Byte absolute Page; {Real bounds [0..65535]}
begin
  K := KeyL + 9;
  S := Pred (Page.ItemsOnPage) * K + 6;
  if KeyL <> MaxKeyLen then begin
    for I := Page.ItemsOnPage downto 2 do begin
      Move(P[S], Page.ItemArray[I], K);
      S := S - K;
    end;
  end;
end;


function IsamFirstFreeChanged(IFBPtr : IsamFileBlockPtr;
                                Ind    : Word         ) : Boolean;
begin
  IsamFirstFreeChanged := IFBPtr^.DIDPtr^[Ind]^.FirstFreeChanged;
end;


procedure IsamGetInfoRec(IFBPtr    : IsamFileBlockPtr;
                         KeyNr     : Word;
                     var IndFopen  : Boolean);
var
  IR : IsamInfoRec;
begin
  IsamClearOK;
  with IFBPtr^, DIDPtr^[KeyNr]^, IR, InfoRec do begin
    if KeyNr = 0 then begin
      IsamGetBlock(DatF, 0, SizeOf (IsamSmallInfoRec), IR);
    end
    else begin
      IsamGetBlock(IndF, BlockOfs, SizeOf (IsamInfoRec), IR);
    end;
    if not IsamOK then Exit;
    FirstFree := Gener [1];
    NumberFree := Gener [2];
    NumRec := Gener [3];
    LenRec := Gener [4];
    if KeyNr = 0 then begin
      NrOfKeys := ILI (Gener [5]).Lo;
      IndFopen := ADK;
    end
    else begin
      RootRef := Gener [5];
      AllowDupKeys := ADK;
      NumKeys := KeysUsed;
      UsedPageSize := PageSizeUsed;                            {!!.42}
      if UsedPageSize = 0 then                                 {!!.42}
        UsedPageSize := CreatePageSize;                        {!!.42}
    end;
  end;
end;


procedure IsamCopyInfoRecToIR(IFBPtr : IsamFileBlockPtr;
                              KeyNr  : Word;
                          var IR     : IsamInfoRec);
begin
  with IFBPtr^, DIDPtr^[KeyNr]^, IR, InfoRec do begin
    Gener[1] := FirstFree;
    Gener[2] := NumberFree;
    Gener[3] := NumRec;
    Gener[4] := LenRec;
    if KeyNr = 0 then begin
      Gener[5] := NrOfKeys;
      ADK := False;
    end
    else begin
      Gener[5] := RootRef;
      ADK := AllowDupKeys;
      KeysUsed := NumKeys;
      PageSizeUsed := UsedPageSize;                            {!!.42}
    end;
  end;
end;


procedure IsamPutInfoRec(IFBPtr      : IsamFileBlockPtr;
                         KeyNr       : Word;
                         IndFDamaged : Boolean);
var
  IR   : IsamInfoRec;
begin
  IsamClearOK;
  with IFBPtr^, DIDPtr^[KeyNr]^ do begin
    IsamCopyInfoRecToIR(IFBPtr, KeyNr, IR);
    if KeyNr = 0 then begin
      IR.InfoRec.ADK := IndFDamaged;
      IsamPutBlock(DatF, 0, SizeOf (IsamSmallInfoRec), IR);
    end
    else begin
      IsamPutBlock(IndF, BlockOfs, SizeOf (IsamInfoRec), IR);
    end;
  end;
end;


procedure IsamSaveGivenInfoRec(IFBPtr        : IsamFileBlockPtr;
                                 Ind           : Word;
                                 IR            : IsamInfoRec;
                                 SaveFirstFree : LongInt);
var
  Buffer : IsamSaveInfoRecBuffer;
begin
  with IFBPtr^, Buffer do begin
    IST := 1000;               {Sign for info rec}
    IndNr := Ind;
    BIR := IR;
    SFF := SaveFirstFree;
    IsamBlockWrite(DiaF, Buffer, SizeOf (Buffer));
  end;
end;


function IsamRBufPtrToPgPtr(RBufPtr : IsamRingBufferRecPtr) : IsamPagePtr;
  {-Map of page will and can already be done}
begin
{$IFDEF UseEMSHeap}
  with RBufPtr^ do begin
    if EMSEntry then begin
      if EMSHeapIsUsed then begin
        if UserSaveEMSHandle = 0 then begin
          UserSaveEMSHandle := SaveEMSCtxt;
        {$IFDEF EMSDisturbance}
          RestoreEMSCtxt(OwnSaveEMSHandle);
        {$ENDIF}
        end;
      end;
      IsamRBufPtrToPgPtr := MapEMSPtr(RBufPtr^.PageEntryPtr);
    end
    else begin
      IsamRBufPtrToPgPtr := IsamPagePtr(RBufPtr^.PageEntryPtr);
    end;
  end;
{$else}
  IsamRBufPtrToPgPtr := IsamPagePtr(RBufPtr^.PageEntryPtr);
{$ENDIF}
end;


function IsamPagePtrToRBufPtr(PgPtr : IsamPagePtr) : IsamRingBufferRecPtr;
  {-Map of page must already be done}
begin
  IsamPagePtrToRBufPtr := IsamPageEntryPtr (PgPtr)^.RingBufferPtr;
end;


procedure IsamSavePage(var Pg : IsamPage);
var
  IRBPtr : IsamRingBufferRecPtr;
  IPEPtr : IsamPageEntryPtr;
  P      : Array [0..4] Of Byte Absolute Pg; {May reference 0..65535}
  K      : Word;
  TempLI : LongInt;
begin
  IRBPtr := IsamPagePtrToRBufPtr(Addr (Pg));
  IPEPtr := Addr (Pg);
    {-Map must be already done, so another map is not necessary}
  with IRBPtr^, IFBlPtr^, IPEPtr^, Page do begin
    IsamPack(Page, DIDPtr^[KeyNr]^.KeyLen);
    TempLI := BckwPageRef;

  {--Calculate length of packed page}
    K := ItemsOnPage *(DIDPtr^[KeyNr]^.KeyLen + 9) + 6;
    Move(KeyNr, P [2], 2);       {over BckwPageRef}

  {--Write beyond the end of the packed page (2x)}
    Move(PageRef, P [K], 4);
    Move(TempLI, P [K + 4], 4);  {BckwPageRef at end}
    IsamBlockWrite(IFBlPtr^.DiaF, Pg, K + 8);

    BckwPageRef := TempLI;
    IsamUnPack(Page, DIDPtr^[KeyNr]^.KeyLen);
    SaveBuffered := True;
  end;
end;


procedure IsamOnlyFlushDOSDat(IFBPtr : IsamFileBlockPtr);
var
  Dummy : Boolean;
begin
  IsamFlush(IFBPtr^.DatF, Dummy,
      (IFBPtr^.NSP <> Nil) and (IsamInitializedNet <> NoNet));
end;


procedure IsamFlushDOSDat(IFBPtr : IsamFileBlockPtr);
begin
  with IFBPtr^ do begin
    if DIDPtr^[0]^.InfoRecChanged or not SaveFB then begin
      DIDPtr^[0]^.InfoRecChanged := False;
      DIDPtr^[0]^.IRChangedSaveN := True;
      IsamPutInfoRec(IFBPtr, 0, False);
      if not IsamOK then Exit;
    end;
  end;
  IsamOnlyFlushDOSDat(IFBPtr);
end;


procedure IsamFlushDOSIx(IFBPtr : IsamFileBlockPtr);
var
  Dummy : Boolean;
begin
  IsamFlush(IFBPtr^.IndF, Dummy,
      (IFBPtr^.NSP <> Nil) and (IsamInitializedNet <> NoNet));
end;


procedure IsamFlushDOSDia(IFBPtr : IsamFileBlockPtr; Continue : Boolean);
var
  WithDUP : Boolean;
  Dummy   : DWORD;                                             {!!.54}
begin
  with IFBPtr^ do begin
    IsamFlush(DiaF, WithDUP,
        (NSP <> Nil) and (IsamInitializedNet <> NoNet));
    if not IsamOK then Exit;
    if Continue and not WithDUP then
      IsamLongSeekEOF(DiaF, Dummy);
  end;
end;


procedure IsamMakeDiaFile(var F : IsamFile);
var
  Dummy : Char;
begin
  IsamRewrite(F);
  if not IsamOK then Exit;
  IsamBlockWrite(F, Dummy, 1);
end;


procedure IsamReduceDiaFile(IFBPtr : IsamFileBlockPtr);
var
  Dummy : Char;
begin
  IsamPutBlock(IFBPtr^.DiaF, 1, 0, Dummy);
  if not IsamOK then Exit;
  if(IFBPtr^.NSP = Nil) or IsamNetEmu then begin
    IsamFlushDOSDia(IFBPtr, True);
  end;
end;


procedure IsamSetDataBufferedFlag(IFBPtr : IsamFileBlockPtr);
begin
  with IFBPtr^ do begin
    if DataBuffered then Exit;
    DataBuffered := True;
    if SaveFB then Exit;
    IsamPutBlock(DatF, 20, 1, DataBuffered);
    if IsamForceFlushOfMark and (IsamNetEmu or (NSP = Nil)) then begin
      IsamOnlyFlushDOSDat(IFBPtr);
    end;
  end;
end;


function IsamGetNextUsedAddRecRef(IFBPtr : IsamFileBlockPtr) : LongInt;
begin
  IsamGetNextUsedAddRecRef := 0;
  with IFBPtr^, DIDPtr^[0]^ do begin
    if FirstFree <> -1 then begin
      if (FirstFree <= 0) or (FirstFree > NumRec) then begin
        IsamError := 10170;
        IsamOK := False;
        Exit;
      end;
      IsamGetNextUsedAddRecRef := FirstFree;
    end
    else begin
      IsamGetNextUsedAddRecRef := Succ (NumRec);
    end;
  end;
end;


function IsamGetAfterNextUsedAddRecRef(IFBPtr : IsamFileBlockPtr)
                                                              : LongInt;
var
  NextFree : LongInt;
begin
  IsamGetAfterNextUsedAddRecRef := 0;
  with IFBPtr^, DIDPtr^[0]^ do begin
    if FirstFree <> -1 then begin
      NextFree := FirstFree;
      if (NextFree <= 0) or (NextFree > NumRec) then begin
        IsamError := 10170;
        IsamOK := False;
        Exit;
      end;
      IsamGetBlock(DatF, NextFree * LenRec, SizeOf (LongInt), NextFree);
      if not IsamOK then Exit;
      if (NextFree = 0) or
         (NextFree > NumRec) or
         (NextFree < -1) then begin
        IsamError := 10170;
        IsamOK := False;
        Exit;
      end;
      if NextFree <> -1 then begin
        IsamGetAfterNextUsedAddRecRef := NextFree;
      end
      else begin
        IsamGetAfterNextUsedAddRecRef := Succ (NumRec);
      end;
    end
    else begin
      IsamGetAfterNextUsedAddRecRef := NumRec + 2;
    end;
  end;
end;


procedure IsamNewRec(IFBPtr : IsamFileBlockPtr;
                 var RefNr  : LongInt;
                     Ind    : Word);
var
  NextFree : LongInt;
begin
  with IFBPtr^, DIDPtr^[Ind]^ do begin
    InfoRecChanged := True;
    if FirstFree <> -1 then begin
      if (FirstFree <= 0) or (FirstFree > NumRec) then begin
        IsamError := 10170;
        IsamOK := False;
        Exit;
      end;
      RefNr := FirstFree;
      if Ind = 0 then begin
        IsamGetBlock(DatF, RefNr * LenRec, SizeOf (LongInt), NextFree); 
      end
      else begin
        IsamGetBlock(IndF, RefNr * BlockLen + BlockOfs, SizeOf (LongInt),
            NextFree);                                         
      end;
      if not IsamOK then Exit;
      if (NextFree = 0) or (NextFree > NumRec) or (NextFree < -1) then begin
        IsamError := 10170;
        IsamOK := False;
        Exit;
      end;
      FirstFree := NextFree;
      FirstFreeChanged := True;
      Dec (NumberFree);
    end
    else begin
      FirstFreeChanged := False;
      Inc (NumRec);
      RefNr := NumRec;
      if (Ind <> 0) and (NumRec > MaxPages) then begin
        IsamPutDummyBlock(IndF, Succ (MaxPages) * BlockLen, BlockLen);
        if not IsamOK then begin
          Dec (NumRec);
          Exit;
        end;
        Inc (MaxPages);
      end;
    end;
  end;
end;


procedure IsamDeleteRecOrPage(IFBPtr : IsamFileBlockPtr;
                                RefNr  : LongInt;
                                Ind    : Word);
begin
  with IFBPtr^, DIDPtr^[Ind]^ do begin
    InfoRecChanged := True;
    if Ind = 0 then begin
      IsamPutBlock(DatF, RefNr * LenRec, SizeOf (LongInt), FirstFree);
    end
    else begin
      IsamPutBlock(IndF, RefNr * BlockLen + BlockOfs, SizeOf (LongInt),
          FirstFree);
    end;
    if not IsamOK then Exit;
    FirstFree := RefNr;
    Inc (NumberFree);
  end;
end;


{$IFNDEF Windows}
{$IFNDEF Win32}
function IsamAvailable(Mem : LongInt) : Boolean;
begin
  IsamAvailable := MaxAvail >= Mem;
end;
{$ENDIF}
{$ENDIF}


procedure IsamAddToOpenFileBlockList(IFBPtr : IsamFileBlockPtr);
                                                    {!!.52 rearranged}
var
  T1Ptr,
  T2Ptr : IsamOpenFileBlockListPtr;
begin
  IsamClearOK;
  if not IsamGetMem(T1Ptr, sizeof(IsamOpenFileBlockList)) then begin {!!.52}
    IsamOK := False;
    IsamError := 10100;
    Exit;
  end;
  if IsamOFBLPtr <> Nil then begin
    T2Ptr := IsamOFBLPtr;
    while T2Ptr^.Next <> Nil do begin
      T2Ptr := T2Ptr^.Next;
    end;
    T2Ptr^.Next := T1Ptr;
  end
  else begin
    IsamOFBLPtr := T1Ptr;
  end;
  T1Ptr^.Next := Nil;
  T1Ptr^.OIFBPtr := IFBPtr;
end;


procedure IsamRemoveFromOpenFileBlockList(IFBPtr : IsamFileBlockPtr);
var
  T1Ptr,
  T2Ptr : IsamOpenFileBlockListPtr;
  Found : Boolean;
begin
  IsamClearOK;
  T1Ptr := IsamOFBLPtr;
  if T1Ptr = Nil then Exit;
  if T1Ptr^.OIFBPtr = IFBPtr then begin
    IsamOFBLPtr := T1Ptr^.Next;
  end
  else begin
    T2Ptr := T1Ptr;
    T1Ptr := T1Ptr^.Next;
    Found := False;
    while (T1Ptr <> Nil) and not Found do begin
      Found := T1Ptr^.OIFBPtr = IFBPtr;
      if not Found then begin
        T2Ptr := T1Ptr;
        T1Ptr := T1Ptr^.Next;
      end;
    end;
    if not Found then Exit;
    T2Ptr^.Next := T1Ptr^.Next;
  end;
  FreeMem(T1Ptr, SizeOf (IsamOpenFileBlockList));
end;


function IsamFileBlockIsInOpenList(IFBPtr : IsamFileBlockPtr) : Boolean;
var
  TPtr : IsamOpenFileBlockListPtr;
begin
  IsamFileBlockIsInOpenList := True;
  TPtr := IsamOFBLPtr;
  while TPtr <> Nil do begin
    if TPtr^.OIFBPtr = IFBPtr then Exit;
    TPtr := TPtr^.Next;
  end;
  IsamFileBlockIsInOpenList := False;
end;


procedure IsamFreeKeyDescrMem(IFBPtr : IsamFileBlockPtr; Nr : Integer);
var
  I : Integer;
begin
  with IFBPtr^ do begin
    for I := Nr downto 0 do begin
      FreeMem(DIDPtr^[I], SizeOf (IsamDatIndDescr));
    end;
    FreeMem(DIDPtr, SizeOf (IsamDatIndDescrPtr) * Succ (NrOfKeys));
  end;
end;


procedure IsamGetKeyDescrMem(IFBPtr : IsamFileBlockPtr; Nr : Integer);
                                                        {!!.52 rewritten}
  {----}
  procedure Undo(IFBPtr : IsamFileBlockPtr; Nr : Integer);
  var
    i : Integer;
  begin
    with IFBPtr^ do
      for i := Nr downto 0 do
        FreeMem(DIDPtr^[i], sizeof(IsamDatIndDescr));
  end;
  {----}
  var
    i : Integer;
  begin
    IsamOK := False;
    IsamError := 10030;
    if not IsamGetMem(IFBPtr^.DIDPtr, sizeof(IsamDatIndDescrPtr) * succ(Nr)) then
      Exit;
    for i := 0 to Nr do
      if not IsamGetMem(IFBPtr^.DIDPtr^[i], sizeof(IsamDatIndDescr)) then begin
        Undo(IFBPtr, pred(i));
        FreeMem(IFBPtr^.DIDPtr, sizeof(IsamDatIndDescrPtr) * succ(Nr));
        Exit;
      end;
    IsamClearOK;
  end;


procedure IsamLast(IRBRPtr : IsamRingBufferRecPtr);      {!!.42mod}
begin
  if IRBRPtr <> IsamRBR1Ptr^.Prev then begin
    if IRBRPtr = IsamRBR1Ptr then begin
      IsamRBR1Ptr := IsamRBR1Ptr^.Next
    end
    else begin
      IRBRPtr^.Prev^.Next := IRBRPtr^.Next;
      IRBRPtr^.Next^.Prev := IRBRPtr^.Prev;
      IRBRPtr^.Next := IsamRBR1Ptr;
      IRBRPtr^.Prev := IsamRBR1Ptr^.Prev;
      IsamRBR1Ptr^.Prev^.Next := IRBRPtr;
      IsamRBR1Ptr^.Prev := IRBRPtr;
    end;
  end;
end;


procedure IsamOptimizePageBuffer;
var
  TPtr,
  TBPtr : IsamRingBufferRecPtr;
  W     : Word;
begin
  TPtr := IsamRBR1Ptr;
  for W := 1 to IsamNrOfRingBufferRecs do begin
    TBPtr := TPtr^.Next;
    if TPtr^.IFBlPtr <> Nil then
      IsamLast(TPtr);
    TPtr := TBPtr;
  end;
end;


procedure IsamResetSaveBuffered;
var
  TPtr : IsamRingBufferRecPtr;
begin
  TPtr := IsamRBR1Ptr;
  repeat
    with TPtr^ do begin
      SaveBuffered := False;
      TPtr := Next;
    end;
  until TPtr = IsamRBR1Ptr;
end;


procedure IsamPutPage(IPgPtr      : IsamPagePtr;
                        Destructive : Boolean);
begin
  with IsamPageEntryPtr (IPgPtr)^, RingBufferPtr^, IFBlPtr^, DIDPtr^[KeyNr]^ do begin
    if SaveFB then begin
      if ((NSP = Nil) or IsamNetEmu) and SaveBuffered then begin
        IsamFlushDOSDia(IFBlPtr , True);
        if not IsamOK then Exit;
        IsamResetSaveBuffered;
      end;
    end;
    UpDated := False;
    IsamPack(Page, KeyLen);
    IsamPutBlock(IndF, PageRef * BlockLen + BlockOfs,
        LongInt (6) + LongInt (KeyLen + 9) * LongInt (Page.ItemsOnPage),
        Page);
    if not Destructive then
      IsamUnPack(Page, KeyLen);
  end;
end;


procedure IsamGetPage(    IFBPtr : IsamFileBlockPtr;
                            PRef   : LongInt;
                            Key    : Word;
                        var IPgPtr : IsamPagePtr);
var
  TPtr  : IsamRingBufferRecPtr;
  Found : Boolean;
begin
  TPtr := IsamRBR1Ptr;
  Found := False;
  repeat
    TPtr := TPtr^.Prev;
    with TPtr^ do begin
      if PageRef = PRef then begin
        if KeyNr = Key then begin
          if IFBlPtr = IFBPtr then begin
            Found := True;
            IPgPtr := IsamRBufPtrToPgPtr(TPtr);
          end;
        end;
      end;
    end;
  until Found or (TPtr = IsamRBR1Ptr);
  if not Found then begin
    TPtr := IsamRBR1Ptr;
    IPgPtr := IsamRBufPtrToPgPtr(TPtr);
    with TPtr^ do begin
      if UpDated then begin
        IsamPutPage(IPgPtr, True);
        if not IsamOK then Exit;
      end;
      with IFBPtr^, DIDPtr^[Key]^ do begin
        IsamGetBlock(IndF, PRef * BlockLen + BlockOfs, LenRec, IPgPtr^);
        if not IsamOK then Exit;
        IsamUnPack(IPgPtr^, KeyLen);
        IFBlPtr := IFBPtr;
        PageRef := PRef;
        UpDated := False;
        KeyNr := Key;
      end;
    end;
  end;
  IsamLast(TPtr);
end;


procedure IsamNewPage(    IFBPtr : IsamFileBlockPtr;
                        var PRef   : LongInt;
                        var IPgPtr : IsamPagePtr;
                            Key    : Word);
begin
  IsamClearOK;
  IPgPtr := IsamRBufPtrToPgPtr(IsamRBR1Ptr);
  with IsamRBR1Ptr^ do begin
    if UpDated then begin
      IsamPutPage(IPgPtr, True);
      if not IsamOK then Exit;
    end;
    IsamNewRec(IFBPtr, PRef, Key);
    IFBlPtr := IFBPtr;
    PageRef := PRef;
    KeyNr := Key;
    UpDated := False;
  end;
  IsamLast(IsamRBR1Ptr);
end;


procedure IsamUpdatePage(PgPtr : IsamPagePtr);
begin
  IsamPagePtrToRBufPtr(PgPtr)^.UpDated := True;
end;


procedure IsamReturnPage(var PgPtr : IsamPagePtr);
begin
  with IsamPagePtrToRBufPtr(PgPtr)^ do begin
    IsamDeleteRecOrPage(IFBlPtr, PageRef, KeyNr);
    IFBlPtr := Nil;
    UpDated := False;
    IsamOptimizePageBuffer;
  end;
end;


{$IFDEF LengthByteKeys}
function IsamCompKeys(var Key1, Key2 : IsamKeyStr;
                          DatRef1,
                          DatRef2    : LongInt;
                          Dup        : Boolean   ) : Integer;
  {$IFDEF Win32}
  function CompareStrings(const S1, S2 : IsamKeyStr) : integer; register;
  asm
    push edi
    push esi
    mov edi, edx
    mov esi, eax
    xor eax, eax
    mov dh, [edi]
    mov dl, [esi]
    inc edi
    inc esi
    xor ecx, ecx
    mov cl, dl
    cmp cl, dh
    jb @@CompStrs
    mov cl, dh
  @@CompStrs:
    or ecx, ecx
    jz @@CompLengths
    repe cmpsb
    jb @@LT
    ja @@GT
  @@CompLengths:
    cmp dl, dh
    je @@Exit
    jb @@LT
  @@GT:
    inc eax
    inc eax
  @@LT:
    dec eax
  @@Exit:
    pop esi
    pop edi
  end;
  {----}
begin
  Result := CompareStrings(Key1, Key2);
  if (Result = 0) and Dup then
    Result := DatRef1 - DatRef2;
end;
{$else}
  {------}
  function Sgn(X : LongInt) : Integer;
  begin
    if X = LongInt (0) then begin
      Sgn := 0;
    end
    else begin
      if X < 0 then begin
        Sgn := -1;
      end
      else begin
        Sgn := 1;
      end;
    end;
  end;
  {------}
var
  C,
  Len  : Integer;
begin
  Len := Length (Key1);
  if Length (Key2) < Len then
    Len := Length (Key2);
  Inline (
             $8C / $DA /                    { Mov DX, DS }
             $C4 / $BE / Key1 /             { Les DI, Key1 [BP] }
             $C5 / $B6 / Key2 /             { Lds SI, Key2 [BP] }
             $47 /                          { Inc DI }
             $46 /                          { Inc SI }
             $8B / $8E / Len /              { Mov CX, [BP+Ofs(Len)] }
             $FC /                          { Cld }
             $31 / $C0 /                    { Xor AX, AX }
             $F3 /                          { Repz }
             $A6 /                          { Cmpsb }
             $74 / $05 /                    { Jz OK }
             $40 /                          { Inc AX }
             $72 / $02 /                    { Jc OK }
             $F7 / $D8 /                    { Neg AX }
     { OK: } $8E / $DA /                    { Mov DS, DX }
             $89 / $86 / C                  { Mov [BP+Ofs(C)], AX }
                                                                    );
  IsamCompKeys := C;
  if C <> 0 then Exit;
  if Length (Key1) = Length (Key2) then begin
    if Dup then begin
      IsamCompKeys := Sgn(DatRef1 - DatRef2);
    end;
  end
  else begin
    if Length (Key1) > Length (Key2) then begin
      IsamCompKeys := 1;
    end
    else begin
      IsamCompKeys := -1;
    end;
  end;
end;
{$ENDIF}
{$ENDIF}

{$IFDEF ASCIIZeroKeys}
function IsamCompKeys(var Key1, Key2 : IsamKeyStr;
                            DatRef1,
                            DatRef2    : LongInt;
                            Dup        : Boolean   ) : Integer;
{$IFDEF Win32}
var                                                            {!!.55}
  K1 : array [0..MaxKeyLen] of char absolute Key1;             {!!.55}
  K2 : array [0..MaxKeyLen] of char absolute Key2;             {!!.55}
begin
  Result := StrComp(K1, K2);                                   {!!.55}
  if (Result = 0) and Dup then
    Result := DatRef1 - DatRef2;
end;
{$else}
  function Sgn(X : LongInt) : Integer;
  begin
    if X = LongInt (0) then begin
      Sgn := 0;
    end
    else begin
      if X < LongInt (0) then begin
        Sgn := -1;
      end
      else begin
        Sgn := 1;
      end;
    end;
  end;
var
  C : Integer;
begin
  Inline (
           $C4 / $BE / Key1 /             { Les DI, Key1 [BP] }
           $B9 / >MaxKeyLen /             { Mov CX, MaxKeyLen }
           $41 /                          { Inc CX }
           $89 / $FB /                    { Mov BX, DI }
           $FC /                          { Cld }
           $31 / $C0 /                    { Xor AX, AX }
           $F2 /                          { Repnz }
           $AE /                          { Scasb }
           $29 / $DF /                    { Sub DI, BX }
           $4F /                          { Dec DI }
           $89 / $FB /                    { Mov BX, DI }
           $89 / $D9 /                    { Mov CX, BX }
           $8C / $DA /                    { Mov DX, DS }
           $C4 / $BE / Key1 /             { Les DI, Key1 [BP] }
           $C5 / $B6 / Key2 /             { Lds SI, Key2 [BP] }
           $8A / $18 /                    { Mov BL, [SI+BX] }
           $30 / $FF /                    { Xor BH, BH }
           $F3 /                          { Repz }
           $A6 /                          { Cmpsb }
           $75 / $07 /                    { Jnz Cont }
           $39 / $D8 /                    { Cmp AX, BX }
           $74 / $08 /                    { Jz OK }
           $48 /                          { Dec AX }
           $EB / $05 /                    { Jmp OK }
 { Cont: } $40 /                          { Inc AX }
           $72 / $02 /                    { Jc OK }
           $F7 / $D8 /                    { Neg AX }
   { OK: } $8E / $DA /                    { Mov DS, DX }
           $89 / $86 / C                  { Mov [BP+Ofs(C)], AX }
                                                                  );
  IsamCompKeys := C;
  if (C = 0) and Dup then begin
    IsamCompKeys := Sgn(DatRef1 - DatRef2);
  end;
end;
{$ENDIF}
{$ENDIF}

procedure IsamFlushPageBuffer(IFBPtr      : IsamFileBlockPtr;
                                Destructive : Boolean);
var
  TPtr : IsamRingBufferRecPtr;
begin
  IsamClearOK;
  TPtr := IsamRBR1Ptr;
  repeat
    with TPtr^ do begin
      if IFBlPtr = IFBPtr then begin
        with IFBPtr^ do begin
          if UpDated then begin
            IsamPutPage(IsamRBufPtrToPgPtr(TPtr), Destructive);
            if not IsamOK then Exit;
          end;
        end;
        if Destructive then
          IFBlPtr := Nil;
      end;
    end;
    TPtr := TPtr^.Next;
  until TPtr = IsamRBR1Ptr;
  if Destructive then
    IsamOptimizePageBuffer;
end;


procedure IsamFlushPageInfo(IFBPtr      : IsamFileBlockPtr;
                            Destructive : Boolean);
var
  I : Integer;
begin
  with IFBPtr^ do begin
    IsamFlushPageBuffer(IFBPtr, Destructive);
    if not IsamOK then Exit;
    for I := 1 to NrOfKeys do begin
      with DIDPtr^[I]^ do begin
        if InfoRecChanged then begin
          InfoRecChanged := False;
          IRChangedSaveN := True;
          IsamPutInfoRec(IFBPtr, I, False);
          if not IsamOK then Exit;
        end;
      end;
    end;
  end;
end;


procedure IsamDestroyPages(IFBPtr : IsamFileBlockPtr);
var
  TPtr : IsamRingBufferRecPtr;
begin
  TPtr := IsamRBR1Ptr;
  repeat
    with TPtr^ do begin
      if IFBlPtr = IFBPtr then begin
        SaveBuffered := False;
        UpDated := False;
        IFBlPtr := Nil;
      end;
    end;
    TPtr := TPtr^.Next;
  until TPtr = IsamRBR1Ptr;
end;


procedure IsamDestroyPagesOfKeyNr(IFBPtr : IsamFileBlockPtr;
                                    Key    : Word);
var
  TPtr : IsamRingBufferRecPtr;
begin
  TPtr := IsamRBR1Ptr;
  repeat
    with TPtr^ do begin
      if IFBlPtr = IFBPtr then begin
        if KeyNr = Key then begin
          SaveBuffered := False;
          UpDated := False;
          IFBlPtr := Nil;
        end;
      end;
    end;
    TPtr := TPtr^.Next;
  until TPtr = IsamRBR1Ptr;
end;


procedure IsamRepairFileBlock(IFBPtr : IsamFileBlockPtr);
const
  RepError = 10180;
var
  FrameAlreadySaved : boolean;
  {------}
  procedure Repair;
  var
    IST : Word;
    {----}
    procedure RestoreInfoRec;
    var
      Buffer : IsamSaveInfoRecBuffer;
    begin
      IsamBlockRead(IFBPtr^.DiaF, Buffer.IndNr, SizeOf (Buffer) - SizeOf (Word));
      if not IsamOK then Exit;
      with Buffer, BIR, InfoRec, IFBPtr^, DIDPtr^[IndNr]^ do begin
        if SFF <> -2 then begin
          FirstFree := SFF;
          IsamDeleteRecOrPage(IFBPtr, Gener [1], IndNr);
          if not IsamOK then Exit;
        end;
        FirstFree := Gener [1];
        NumberFree := Gener [2];
        NumRec := Gener [3];
        LenRec := Gener [4];
        if IndNr = 0 then begin
          NrOfKeys := ILI (Gener [5]).Lo;
        end
        else begin
          RootRef := Gener [5];
          AllowDupKeys := ADK;
          NumKeys := KeysUsed;
        end;
        IsamPutInfoRec(IFBPtr, IndNr, False);
        InfoRecChanged := False;
      end;
    end;
    {----}
    procedure RestorePage;
    type
      P = Array [0..4] Of Byte;    {May reference [0..65535]}
    var
      PEPtr  : IsamPageEntryPtr;
      IPgPtr : IsamPagePtr;
      K      : Word;
      PPtr   : ^P;
    begin
      IPgPtr := IsamRBufPtrToPgPtr(IsamRBR1Ptr);
      PEPtr := IsamPageEntryPtr (IPgPtr);
      PPtr := Addr (PEPtr^);
      with IsamRBR1Ptr^, PEPtr^ do begin
        if UpDated then begin
          IsamPutPage(IPgPtr, True);
          if not IsamOK then Exit;
        end;
        IFBlPtr := Nil;
        IsamBlockRead(IFBPtr^.DiaF, PPtr^ [2], 2);
        if not IsamOK then Exit;
        Move(PPtr^ [2], KeyNr, 2);  {Extract KeyNr}
        Page.ItemsOnPage := IST;       {Extract ItemsOnPage}
        K := Page.ItemsOnPage *(IFBPtr^.DIDPtr^[KeyNr]^.KeyLen + 9) + 6;
        IsamBlockRead(IFBPtr^.DiaF, PPtr^ [4], K + 4); {Read rest}
        if not IsamOK then Exit;
        Move(PPtr^ [K], PageRef, 4);
        Move(PPtr^ [K+4], Page.BckwPageRef, 4);
        IFBlPtr := IFBPtr;
        SaveBuffered := False;
        UpDated := False;
        IsamUnPack(Page, IFBPtr^.DIDPtr^[KeyNr]^.KeyLen);
        IsamPutPage(IPgPtr, True);
        IFBlPtr := Nil;
      end;
    end;
    {----}
  begin {Repair}
    IsamClearOK;
    IsamDestroyPages(IFBPtr);
    with IFBPtr^ do begin
      IsamLongSeek(DiaF, 0);
      if not IsamOK then begin
        IsamError := RepError;
        Exit;
      end;
      IsamBlockWrite(DiaF, IST, 1);
      if not IsamOK then begin
        IsamError := RepError;
        Exit;
      end;
      repeat
        IST := 0;
        IsamBlockRead(DiaF, IST, 2);
        if not IsamOK then Exit;
        case IST of
          1..MaxPageSize : RestorePage;                        {!!.42}
          1000           : RestoreInfoRec;
          2000           : Exit;
        else
          IsamOK := False;
          IsamError := RepError;
          Exit;
        end; {Case}
        if not IsamOK then Exit;
      until False;
    end;
  end;
  {------}
begin {IsamRepairFileBlock}
  if IFBPtr^.ReadOnlyFB then begin
    IsamOK := False;
    IsamError := 10430;
    Exit
  end;
{$IFDEF UseEMSHeap}
  if EMSHeapIsUsed then begin
    FrameAlreadySaved := True;
    if UserSaveEMSHandle = 0 then begin
      FrameAlreadySaved := False;
      UserSaveEMSHandle := SaveEMSCtxt;
    {$IFDEF EMSDisturbance}
      RestoreEMSCtxt(OwnSaveEMSHandle);
    {$ENDIF}
    end;
  end;
{$ENDIF}
  Repair;
{$IFDEF UseEMSHeap}
  if EMSHeapIsUsed then begin
    if not FrameAlreadySaved then begin
     {$IFDEF EMSDisturbance}
      OwnSaveEMSHandle := SaveEMSCtxt;
    {$ENDIF}
      RestoreEMSCtxt(UserSaveEMSHandle);
      UserSaveEMSHandle := 0;
    end;
  end;
{$ENDIF}
  if IsamError = 0 then begin
    if (IFBPtr^.NSP = Nil) or IsamNetEmu then begin
      IsamFlushDOSDat(IFBPtr);
      if not IsamOK then begin
        IsamError := RepError;
        Exit;
      end;
      if IFBPtr^.NrOfKeys > 0 then begin
        IsamFlushDOSIx(IFBPtr);
        if not IsamOK then begin
          IsamError := RepError;
          Exit;
        end;
      end;
    end;
    IsamReduceDiaFile(IFBPtr);
  end
  else begin
    if IsamError = 10070 then begin
      IsamClearOK;
      IsamReduceDiaFile(IFBPtr);
    end
    else begin
      IsamError := RepError;
    end;
  end;
end;


procedure IsamFindKey(    IFBPtr     : IsamFileBlockPtr;
                            Key        : Word;
                        var UserDatRef : LongInt;
                        var UserKey    : IsamKeyStr        );
var
  RefNr   : LongInt;
  C,K,L,R : Integer;
  IPgPtr  : IsamPagePtr;
  OK      : Boolean;
begin
  with IFBPtr^, DIDPtr^[Key]^ do begin
    SequentialOK := False;
    OK := False;
    PathInd := 0;
    RefNr := RootRef;
    while (RefNr <> 0) and (not OK) do begin
      Inc (PathInd);
      Path [PathInd].PageRef := RefNr;
      IsamGetPage(IFBPtr, RefNr, Key, IPgPtr);
      if not IsamOK then Exit;
      with IPgPtr^ do begin
        L := 1;
        R := ItemsOnPage;
        repeat
          K := (L + R) Shr 1;
          C := IsamCompKeys(UserKey, ItemArray [K].KeyStr,
                            UserDatRef, ItemArray [K].DataRef,
                            AllowDupKeys);
          if C <= 0 then
            R := Pred (K);
          if C >= 0 then
            L := Succ (K);
        until R < L;
        if L - R > 1 then begin
          UserDatRef := ItemArray [K].DataRef;
          R := K;
          OK := True;
        end
        else begin
          if R = 0 then begin
            RefNr := BckwPageRef;
          end
          else begin
            RefNr := ItemArray [R].PageRef;
          end;
        end;
      end;
      Path [PathInd].ItemArrInd := R;
    end;
    if not OK then begin
      while (PathInd > 0) and (Path [PathInd].ItemArrInd = 0) do begin
        Dec(PathInd);
      end;
    end;
    SequentialOK := True;
  end;
  IsamOK := OK;
end;


procedure IsamAddKey(    IFBPtr     : IsamFileBlockPtr;
                           Key        : Word;
                           UserDatRef : LongInt;
                       var UserKey    : IsamKeyStr        );
var
  IPgRef1,
  IPgRef2  : LongInt;
  C, K, L  : Integer;
  OverRun,
  OK, ADK  : Boolean;
  IPgPtr1,
  IPgPtr2  : IsamPagePtr;
  IItem1,
  IItem2   : IsamItem;
  IR       : IsamInfoRec;
  RefList  : Array [1..30] Of LongInt;  {At least 3 * (MaxHeigth + 1)}
  {------}
  procedure InitRefList;
  begin
    RefList [1] := 0;
  end;
  {------}
  function IsInRefList(Ref : LongInt) : Boolean;
  var
    I : Integer;
  begin
    IsInRefList := True;
    I := 1;
    while RefList [I] <> 0 do begin
      if Ref = RefList [I] then Exit;
      Inc (I);
    end;
    RefList [Succ (I)] := 0;
    RefList [I] := Ref;
    IsInRefList := False;
  end;
  {------}
  procedure Search(IPgRef1 : LongInt; RV : Integer; IPgRef1V : LongInt);
  var
    R : Integer;
    {----}
    function BalanceDone : Boolean;
    var
      IPgRef3           : LongInt;
      IPgPtr1V, IPgPtr3 : IsamPagePtr;
      right             : Boolean;
      {--}
      procedure Balance;
      var
        TRef        : LongInt;
        I, K, M, N  : Integer;
        CurPageSize : Word;                                        {!!.42}
      begin
        CurPageSize := IFBPtr^.DIDPtr^[Key]^.UsedPageSize;         {!!.42}
        with IPgPtr1^ do begin
          if right then begin
            Inc (RV);
            Inc (R);
            TRef := IPgPtr1V^.ItemArray [RV].PageRef;
            IPgPtr1V^.ItemArray [RV].PageRef := IPgPtr3^.BckwPageRef;
            K := 2 + CurPageSize + IPgPtr3^.ItemsOnPage;           {!!.42}
            M := Succ(K Shr 1);
            K := K - M - IPgPtr3^.ItemsOnPage;
            for I := IPgPtr3^.ItemsOnPage downto 1 do begin
              IPgPtr3^.ItemArray [I+K] := IPgPtr3^.ItemArray [I];
            end;
            IPgPtr3^.ItemArray [K] := IPgPtr1V^.ItemArray [RV];
            if M = R then begin
              for I := R to CurPageSize do begin                   {!!.42}
                IPgPtr3^.ItemArray [Succ (I-R)] := ItemArray [I];
              end;
              IPgPtr1V^.ItemArray [RV] := IItem1;
            end
            else begin
              if M > R then begin
                for I := M to CurPageSize do begin                 {!!.42}
                  IPgPtr3^.ItemArray [Succ (I-M)] := ItemArray [I];
                end;
                IPgPtr1V^.ItemArray [RV] := ItemArray [Pred (M)];
                for I := M-2 downto R do begin
                  ItemArray [Succ (I)] := ItemArray [I];
                end;
                ItemArray [R] := IItem1;
              end
              else begin
                for I := R to CurPageSize do begin                 {!!.42}
                  IPgPtr3^.ItemArray [Succ (I-M)] := ItemArray [I];
                end;
                IPgPtr3^.ItemArray [R-M] := IItem1;
                for I := Succ (M) to Pred (R) do begin
                  IPgPtr3^.ItemArray [I-M] := ItemArray [I];
                end;
                IPgPtr1V^.ItemArray [RV] := ItemArray [M];
              end;
            end;
            IPgPtr3^.BckwPageRef := IPgPtr1V^.ItemArray [RV].PageRef;
            IPgPtr1V^.ItemArray [RV].PageRef := TRef;
            ItemsOnPage := Pred (M);
            Inc (IPgPtr3^.ItemsOnPage, K);
          end
          else begin
            Inc (R);
            TRef := IPgPtr1V^.ItemArray [RV].PageRef;
            IPgPtr1V^.ItemArray [RV].PageRef := BckwPageRef;
            N := Succ (IPgPtr3^.ItemsOnPage);
            M := (2 + CurPageSize + N) Shr 1;                      {!!.42}
            K := M - N;
            IPgPtr3^.ItemArray [N] := IPgPtr1V^.ItemArray [RV];
            if K = R then begin
              for I := 1 to Pred (K) do begin
                IPgPtr3^.ItemArray [I+N] := ItemArray [I];
              end;
              for I := R to CurPageSize do begin                   {!!.42}
                ItemArray [Succ (I-R)] := ItemArray [I];
              end;
              IPgPtr1V^.ItemArray [RV] := IItem1;
            end
            else begin
              if K < R then begin
                for I := 1 to Pred (K) do begin
                  IPgPtr3^.ItemArray [I+N] := ItemArray [I];
                end;
                IPgPtr1V^.ItemArray [RV] := ItemArray [K];
                for I := Succ (K) to Pred (R) do begin
                  ItemArray [I-K] := ItemArray [I];
                end;
                ItemArray [R-K] := IItem1;
                for I := R to CurPageSize do begin                 {!!.42}
                  ItemArray [Succ (I-K)] := ItemArray [I];
                end;
              end
              else begin
                for I := 1 to Pred (R) do begin
                  IPgPtr3^.ItemArray [I+N] := ItemArray [I];
                end;
                IPgPtr3^.ItemArray [R+N] := IItem1;
                for I := R to K-2 do begin
                  IPgPtr3^.ItemArray [Succ (I+N)] := ItemArray [I];
                end;
                IPgPtr1V^.ItemArray [RV] := ItemArray [Pred (K)];
                for I := K to CurPageSize do begin                 {!!.42}
                  ItemArray [Succ (I-K)] := ItemArray [I];
                end;
              end;
            end;
            IPgPtr1^.BckwPageRef := IPgPtr1V^.ItemArray [RV].PageRef;
            IPgPtr1V^.ItemArray [RV].PageRef := TRef;
            ItemsOnPage := CurPageSize - Pred (K);                 {!!.42}
            Inc (IPgPtr3^.ItemsOnPage, K);
          end;
        end;
      end;
    {----}
    begin
      BalanceDone := False;
      if RV = -1 then Exit;
      IsamGetPage(IFBPtr, IPgRef1V, Key, IPgPtr1V);
      if not IsamOK then Exit;
      with IPgPtr1V^ do begin
        right := RV < ItemsOnPage;
        if right then begin
          IPgRef3 := ItemArray [Succ (RV)].PageRef;
        end
        else begin
          if RV > 1 then begin
            IPgRef3 := ItemArray [Pred (RV)].PageRef;
          end
          else begin
            IPgRef3 := BckwPageRef;
          end;
        end;
        IsamGetPage(IFBPtr, IPgRef3, Key, IPgPtr3);
        if not IsamOK then Exit;
        if IPgPtr3^.ItemsOnPage =
            IFBPtr^.DIDPtr^[Key]^.UsedPageSize then Exit;          {!!.42}
      end;
      if IFBPtr^.SaveFB then begin
        if not IsInRefList(IPgRef1V) then begin
          IsamSavePage(IPgPtr1V^);
          if not IsamOK then Exit;
        end;
        if not IsInRefList(IPgRef3) then begin
          IsamSavePage(IPgPtr3^);
          if not IsamOK then Exit;
        end;
      end;
      Balance;
      IsamUpDatePage(IPgPtr3);
      IsamUpDatePage(IPgPtr1V);
      OverRun := False;
      BalanceDone := True;
    end;
    {----}
    procedure Insert;
    type
      IsamItemInAr = Array [0..0] Of IsamItem;
    var
      I           : Integer;
      CurPageSize : Word;                                          {!!.42}
    begin
      CurPageSize := IFBPtr^.DIDPtr^[Key]^.UsedPageSize;           {!!.42}
      IsamGetPage(IFBPtr, IPgRef1, Key, IPgPtr1);
      if not IsamOK then Exit;
      with IPgPtr1^ do begin
        if IFBPtr^.SaveFB then begin
          if not IsInRefList(IPgRef1) then begin
            IsamSavePage(IPgPtr1^);
            if not IsamOK then Exit;
          end;
        end;
        if ItemsOnPage < CurPageSize then begin                    {!!.42}
          Inc (ItemsOnPage);
          Move(ItemArray [Succ(R)], ItemArray [R+2],
              (ItemsOnPage - Succ (R)) * SizeOf (IsamItemInAr));
          ItemArray [Succ (R)] := IItem1;
          OverRun := False;
        end
        else begin
          if not BalanceDone then begin
            if not IsamOK then Exit;
            with IFBPtr^ do begin
              IsamNewPage(IFBPtr, IPgRef2, IPgPtr2, Key);
              if not IsamOK then Exit;
              if SaveFB then begin
                if not IsInRefList(-2) then begin
                  if IsamFirstFreeChanged(IFBPtr, Key) then begin
                    IsamSaveGivenInfoRec(IFBPtr, Key, IR,
                        DIDPtr^[Key]^.FirstFree);
                  end
                  else begin
                    IsamSaveGivenInfoRec(IFBPtr, Key, IR, -2);
                  end;
                  if not IsamOK then Exit;
                end;
              end;
            end;
            if R <= (CurPageSize Shr 1) then begin                 {!!.42}
              if R = CurPageSize Shr 1 then begin                  {!!.42}
                IItem2 := IItem1;
              end
              else begin
                IItem2 := ItemArray [CurPageSize Shr 1];           {!!.42}
                for I := CurPageSize Shr 1 downto R + 2 do begin   {!!.42}
                  ItemArray [I] := ItemArray [Pred (I)];
                end;
                ItemArray [Succ (R)] := IItem1;
              end;
              for I := 1 to CurPageSize Shr 1 do begin             {!!.42}
                IPgPtr2^.ItemArray [I] :=
                    ItemArray [I + CurPageSize Shr 1];             {!!.42}
              end;
            end
            else begin
              R := R - CurPageSize Shr 1;                          {!!.42}
              IItem2 := ItemArray [Succ (CurPageSize Shr 1)];      {!!.42}
              for I := 1 to Pred (R) do begin
                IPgPtr2^.ItemArray [I] :=
                    ItemArray [Succ (I + CurPageSize Shr 1)];      {!!.42}
              end;
              IPgPtr2^.ItemArray [R] := IItem1;
              for I := Succ (R) to CurPageSize Shr 1 do begin      {!!.42}
                IPgPtr2^.ItemArray [I] :=
                    ItemArray [I + CurPageSize Shr 1];             {!!.42}
              end;
            end;
            ItemsOnPage := CurPageSize Shr 1;                      {!!.42}
            IPgPtr2^.ItemsOnPage := CurPageSize Shr 1;             {!!.42}
            IPgPtr2^.BckwPageRef := IItem2.PageRef;
            IItem2.PageRef := IPgRef2;
            IItem1 := IItem2;
            IsamUpdatePage(IPgPtr2);
          end;
        end;
      end;
      IsamUpdatePage(IPgPtr1);
    end;
  {------}
  begin {Search}
    if IPgRef1 = 0 then begin
      OverRun := True;
      with IItem1 do begin
        Move(UserKey, KeyStr, SizeOf (IsamKeyStr));
        DataRef := UserDatRef;
        PageRef := 0;
      end;
    end
    else begin
      IsamGetPage(IFBPtr, IPgRef1, Key, IPgPtr1);
      if not IsamOK then Exit;
      with IPgPtr1^ do begin
        L := 1;
        R := ItemsOnPage;
        ADK := IFBPtr^.DIDPtr^[Key]^.AllowDupKeys;
        repeat
          K := (L + R) Shr 1;
          C := IsamCompKeys(UserKey,
                            ItemArray [K].KeyStr,
                            UserDatRef,
                            ItemArray [K].DataRef,
                            ADK                  );
          if C <= 0 then
            R := Pred (K);
          if C >= 0 then
            L := Succ (K);
        until R < L;
        if L - R > 1 then begin
          OK := False;
          OverRun := False;
        end
        else begin
          if R = 0 then begin
            Search(BckwPageRef, R, IPgRef1);
          end
          else begin
            Search(ItemArray [R].PageRef, R, IPgRef1);
          end;
          if not IsamOK then Exit;
          if OverRun then
            Insert;
        end;
      end;
    end;
  end; {Search}
  {------}
begin {IsamAddKey}
  InitRefList;
  IsamSetDataBufferedFlag(IFBPtr);
  if not IsamOK then Exit;
  with IFBPtr^, DIDPtr^[Key]^ do begin
    if SaveFB then
      IsamCopyInfoRecToIR(IFBPtr, Key, IR);
    SequentialOK := False;
    OK := True;
    Search(RootRef, -1, 0);
    if not IsamOK then Exit;
    if OverRun then begin
      IPgRef1 := RootRef;
      IsamNewPage(IFBPtr, RootRef, IPgPtr1, Key);
      if not IsamOK then Exit;
      if SaveFB then begin
        if not IsInRefList(-2) then begin
          if IsamFirstFreeChanged(IFBPtr, Key) then begin
            IsamSaveGivenInfoRec(IFBPtr, Key, IR,
                DIDPtr^[Key]^.FirstFree);
          end
          else begin
            IsamSaveGivenInfoRec(IFBPtr, Key, IR, -2);
          end;
          if not IsamOK then Exit;
        end;
      end;
      with IPgPtr1^ do begin
        ItemsOnPage := 1;
        BckwPageRef := IPgRef1;
        ItemArray [1] := IItem1;
      end;
      IsamUpdatePage(IPgPtr1);
    end;
    if SaveFB then begin
      if not IsInRefList(-2) then begin
        {-if not yet stored, no change was made up to now}
        IsamSaveGivenInfoRec(IFBPtr, Key, IR, -2);
        if not IsamOK then Exit;
      end;
      if (NSP = Nil) or IsamNetEmu then begin
        IsamFlushDOSDia(IFBPtr, False);
        if not IsamOK then Exit;
        IsamResetSaveBuffered;
      end;
      if OK then begin
        Inc (NumKeys);
        InfoRecChanged := True;
      end;
      IsamFlushPageInfo(IFBPtr, False);
      if not IsamOK then Exit;
      if (NSP = Nil) or IsamNetEmu then begin
        IsamFlushDOSIx(IFBPtr);
        if not IsamOK then Exit;
      end;
    end
    else begin
      if OK then begin
        Inc (NumKeys);
        InfoRecChanged := True;
      end;
    end;
  end;
  IsamOK := OK;
end;


procedure IsamDeleteKey(    IFBPtr     : IsamFileBlockPtr;
                              Key        : Word;
                              UserDatRef : LongInt;
                          var UserKey    : IsamKeyStr    );
var
  OK,
  UnderRun : Boolean;
  IPgPtr   : IsamPagePtr;
  RefList  : Array [1..30] Of LongInt;  {At least 3 * (MaxHeigth + 1)}
  IR       : IsamInfoRec;
  {------}
  procedure InitRefList;
  begin
    RefList [1] := 0;
  end;
  {------}
  function IsInRefList(Ref : LongInt) : Boolean;
  var
    I : Integer;
  begin
    IsInRefList := True;
    I := 1;
    while RefList [I] <> 0 do begin
      if Ref = RefList [I] then Exit;
      Inc (I);
    end;
    RefList [Succ (I)] := 0;
    RefList [I] := Ref;
    IsInRefList := False;
  end;
  {------}
  procedure Delete2(RefNr : LongInt);
  type
    IsamItemInAr = Array [0..0] Of IsamItem;
  var
    C, K, L, R : Integer;
    IPgRef00   : LongInt;
    IPgPtr     : IsamPagePtr;
    ADK        : Boolean;
    {----}
    procedure UnderFlow(RefNr, IPgRef2 : LongInt; R : Integer);
    var
      I, K, IItem0             : Integer;
      IPgRef0                  : LongInt;
      IPgPtr, IPgPtr2, IPgPtr0 : IsamPagePtr;
      CurPageSize              : Word;                         {!!.42}
    begin
      CurPageSize := IFBPtr^.DIDPtr^[Key]^.UsedPageSize;       {!!.42}
      IsamGetPage(IFBPtr, RefNr, Key, IPgPtr);
      if not IsamOK then Exit;
      IsamGetPage(IFBPtr, IPgRef2, Key, IPgPtr2);
      if not IsamOK then Exit;
      if R < IPgPtr^.ItemsOnPage then begin
        Inc (R);
        IPgRef0 :=  IPgPtr^.ItemArray [R].PageRef;
        IsamGetPage(IFBPtr, IPgRef0, Key, IPgPtr0);
        if not IsamOK then Exit;
        if IFBPtr^.SaveFB then begin
          if not IsInRefList(RefNr) then begin
            IsamSavePage(IPgPtr^);
            if not IsamOK then Exit;
          end;
          if not IsInRefList(IPgRef2) then begin
            IsamSavePage(IPgPtr2^);
            if not IsamOK then Exit;
          end;
          if not IsInRefList(IPgRef0) then begin
            IsamSavePage(IPgPtr0^);
            if not IsamOK then Exit;
          end;
        end;
        K := (Succ (IPgPtr0^.ItemsOnPage - CurPageSize Shr 1))     {!!.42}
            Shr 1;
        IPgPtr2^.ItemArray [CurPageSize Shr 1] :=                  {!!.42}
            IPgPtr^.ItemArray [R];
        IPgPtr2^.ItemArray [CurPageSize Shr 1].PageRef :=          {!!.42}
            IPgPtr0^.BckwPageRef;
        if K > 0 then begin
          for I := 1 to Pred (K) do begin
            IPgPtr2^.ItemArray [I + CurPageSize Shr 1] :=          {!!.42}
                IPgPtr0^.ItemArray [I];
          end;
          IPgPtr^.ItemArray [R] := IPgPtr0^.ItemArray [K];
          IPgPtr^.ItemArray [R].PageRef := IPgRef0;
          IPgPtr0^.BckwPageRef := IPgPtr0^.ItemArray [K].PageRef;
          IPgPtr0^.ItemsOnPage := IPgPtr0^.ItemsOnPage - K;
          for I := 1 to IPgPtr0^.ItemsOnPage do begin
            IPgPtr0^.ItemArray [I] := IPgPtr0^.ItemArray [I+K];
          end;
          IPgPtr2^.ItemsOnPage := Pred (CurPageSize Shr 1 + K);    {!!.42}
          UnderRun := False;
          IsamUpdatePage (IPgPtr0);
        end
        else begin
          for I := 1 to CurPageSize Shr 1 do begin                 {!!.42}
            IPgPtr2^.ItemArray [I + CurPageSize Shr 1] :=          {!!.42}
                IPgPtr0^.ItemArray [I];
          end;
          for I := R to Pred(IPgPtr^.ItemsOnPage) do begin
            IPgPtr^.ItemArray [I] := IPgPtr^.ItemArray [Succ(I)];
          end;
          IPgPtr2^.ItemsOnPage := CurPageSize;                     {!!.42}
          Dec (IPgPtr^.ItemsOnPage);
          IsamReturnPage (IPgPtr0);
          if not IsamOK then Exit;
          UnderRun := IPgPtr^.ItemsOnPage < CurPageSize Shr 1;     {!!.42}
        end;
        IsamUpdatePage(IPgPtr2);
      end
      else begin
        if R = 1 then begin
          IPgRef0 := IPgPtr^.BckwPageRef;
        end
        else begin
          IPgRef0 := IPgPtr^.ItemArray [Pred(R)].PageRef;
        end;
        IsamGetPage(IFBPtr, IPgRef0, Key, IPgPtr0);
        if not IsamOK then Exit;
        if IFBPtr^.SaveFB then begin
          if not IsInRefList(RefNr) then begin
            IsamSavePage(IPgPtr^);
            if not IsamOK then Exit;
          end;
          if not IsInRefList(IPgRef2) then begin
            IsamSavePage(IPgPtr2^);
            if not IsamOK then Exit;
          end;
          if not IsInRefList(IPgRef0) then begin
            IsamSavePage(IPgPtr0^);
            if not IsamOK then Exit;
          end;
        end;
        IItem0 := Succ (IPgPtr0^.ItemsOnPage);
        K := (IItem0 - CurPageSize Shr 1) Shr 1;                   {!!.42}
        if K > 0 then begin
          for I := Pred (CurPageSize Shr 1) downto 1 do begin      {!!.42}
            IPgPtr2^.ItemArray [I+K] := IPgPtr2^.ItemArray [I];
          end;
          IPgPtr2^.ItemArray [K] := IPgPtr^.ItemArray [R];
          IPgPtr2^.ItemArray [K].PageRef := IPgPtr2^.BckwPageRef;
          Dec (IItem0, K);
          for I := Pred (K) downto 1 do begin
            IPgPtr2^.ItemArray [I] := IPgPtr0^.ItemArray [I+IItem0];
          end;
          IPgPtr2^.BckwPageRef := IPgPtr0^.ItemArray [IItem0].PageRef;
          IPgPtr^.ItemArray [R] := IPgPtr0^.ItemArray [IItem0];
          IPgPtr^.ItemArray [R].PageRef := IPgRef2;
          IPgPtr0^.ItemsOnPage := Pred (IItem0);
          IPgPtr2^.ItemsOnPage := Pred (CurPageSize Shr 1 + K);    {!!.42}
          UnderRun := False;
          IsamUpdatePage(IPgPtr2);
        end
        else begin
          IPgPtr0^.ItemArray [IItem0] := IPgPtr^.ItemArray [R];
          IPgPtr0^.ItemArray [IItem0].PageRef := IPgPtr2^.BckwPageRef;
          for I := 1 to Pred (CurPageSize Shr 1) do begin          {!!.42}
            IPgPtr0^.ItemArray [I+IItem0] := IPgPtr2^.ItemArray [I];
          end;
          IPgPtr0^.ItemsOnPage := CurPageSize;                     {!!.42}
          Dec (IPgPtr^.ItemsOnPage);
          IsamReturnPage(IPgPtr2);
          if not IsamOK then Exit;
          UnderRun := IPgPtr^.ItemsOnPage < CurPageSize Shr 1;     {!!.42}
        end;
        IsamUpdatePage (IPgPtr0);
      end;
      IsamUpdatePage(IPgPtr);
    end;   (* UnderFlow *)
    {----}
    procedure Delete1(IPgRef2 : LongInt);
    var
      C        : Integer;
      IPgRef00 : LongInt;
      IPgPtr2  : IsamPagePtr;
    begin
      IsamGetPage(IFBPtr, IPgRef2, Key, IPgPtr2);
      if not IsamOK then Exit;
      with IPgPtr2^ do begin
        IPgRef00 := ItemArray [ItemsOnPage].PageRef;
        if IPgRef00 <> 0 then begin
          C := ItemsOnPage;
          Delete1(IPgRef00);
          if not IsamOK then Exit;
          if UnderRun then
            UnderFlow(IPgRef2, IPgRef00, C);
        end
        else begin
          IsamGetPage(IFBPtr, RefNr, Key, IPgPtr);
          if not IsamOK then Exit;
          if IFBPtr^.SaveFB then begin
            if not IsInRefList(RefNr) then begin
              IsamSavePage(IPgPtr^);
              if not IsamOK then Exit;
            end;
            if not IsInRefList(IPgRef2) then begin
              IsamSavePage(IPgPtr2^);
              if not IsamOK then Exit;
            end;
          end;
          ItemArray [ItemsOnPage].PageRef :=
              IPgPtr^.ItemArray [K].PageRef;
          IPgPtr^.ItemArray [K] := ItemArray [ItemsOnPage];
          Dec (ItemsOnPage);
          UnderRun := ItemsOnPage <
              IFBPtr^.DIDPtr^[Key]^.UsedPageSize Shr 1;            {!!.42}
          IsamUpdatePage(IPgPtr);
          IsamUpdatePage(IPgPtr2);
        end;
      end;
    end;  (* Delete1 *)
  {------}
  begin  (* Delete2 *)
    if RefNr = 0 then begin
      OK := False;
      UnderRun := False;
    end
    else begin
      IsamGetPage(IFBPtr, RefNr, Key, IPgPtr);
      if not IsamOK then Exit;
      with IPgPtr^ do begin
        L := 1;
        R := ItemsOnPage;
        ADK := IFBPtr^.DIDPtr^[Key]^.AllowDupKeys;
        repeat
          K := (L + R) Shr 1;
          C := IsamCompKeys(UserKey,
                              ItemArray [K].KeyStr,
                              UserDatRef,
                              ItemArray [K].DataRef,
                              ADK                  );
          if C <= 0 then
            R := Pred (K);
          if C >= 0 then
            L := Succ (K);
        until L > R;
        if R = 0 then begin
          IPgRef00 := BckwPageRef;
        end
        else begin
          IPgRef00 := ItemArray [R].PageRef;
        end;
        if L - R > 1 then begin
          UserDatRef := ItemArray [K].DataRef;
          if IPgRef00 = 0 then begin
            if IFBPtr^.SaveFB then begin
              if not IsInRefList(RefNr) then begin
                IsamSavePage(IPgPtr^);
                if not IsamOK then Exit;
              end;
            end;
            Dec (ItemsOnPage);
            UnderRun := ItemsOnPage <                              {!!.42}
                IFBPtr^.DIDPtr^[Key]^.UsedPageSize Shr 1;
            Move(ItemArray [Succ (K)], ItemArray [K],
               (ItemsOnPage - Pred (K)) * SizeOf (IsamItemInAr));
            IsamUpdatePage(IPgPtr);
          end
          else begin
            Delete1(IPgRef00);
            if not IsamOK then Exit;
            if UnderRun then
              UnderFlow(RefNr, IPgRef00, R);
          end;
        end
        else begin
          Delete2(IPgRef00);
          if not IsamOK then Exit;
          if UnderRun then
            UnderFlow(RefNr, IPgRef00, R);
        end;
      end;
    end;
  end;   (* Delete2 *)
  {------}
begin   (* DeleteKey *)
  InitRefList;
  IsamSetDataBufferedFlag(IFBPtr);
  if not IsamOK then Exit;
  with IFBPtr^, DIDPtr^[Key]^ do begin
    if SaveFB then begin
      if not IsInRefList(-2) then begin   {Always True}
        IsamCopyInfoRecToIR(IFBPtr, Key, IR);
        IsamSaveGivenInfoRec(IFBPtr, Key, IR, -2);
        if not IsamOK then Exit;
      end;
    end;
    SequentialOK := False;
    OK := True;
    Delete2(RootRef);
    if not IsamOK then Exit;
    if UnderRun then begin
      IsamGetPage(IFBPtr, RootRef, Key, IPgPtr);
      if not IsamOK then Exit;
      if IPgPtr^.ItemsOnPage = 0 then begin
        if SaveFB then begin
          if not IsInRefList(RootRef) then begin
            IsamSavePage(IPgPtr^);
            if not IsamOK then Exit;
          end;
        end;
        RootRef := IPgPtr^.BckwPageRef;
        IsamReturnPage(IPgPtr);
        if not IsamOK then Exit;
      end;
    end;
    if SaveFB then begin
      if (NSP = Nil) or IsamNetEmu then begin
        IsamFlushDOSDia(IFBPtr, False);
        if not IsamOK then Exit;
        IsamResetSaveBuffered;
      end;
      if OK then begin
        Dec (NumKeys);
        InfoRecChanged := True;
      end;
      IsamFlushPageInfo(IFBPtr, False);
      if not IsamOK then Exit;
      if (NSP = Nil) or IsamNetEmu then begin
        IsamFlushDOSIx(IFBPtr);
        if not IsamOK then Exit;
      end;
    end
    else begin
      if OK then begin
        Dec (NumKeys);
        InfoRecChanged := True;
      end;
    end;
  end;
  IsamOK := OK;
end;
