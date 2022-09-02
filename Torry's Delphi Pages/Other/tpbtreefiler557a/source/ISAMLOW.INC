{********************************************************************}
{* ISAMLOW.INC                                                      *}
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

procedure ISInitIsam;
begin
  IsamClearOK;
  IsamIsCriticalActive := False;
  IsamOFBLPtr := Nil;
  IsamForceFlushOfMark := False;
  {$IFNDEF VER60}                                              {!!.53}
    {$IFNDEF VER15}                                            {!!.53}
  IsamReXUserProcPtr := Nil;                                   {!!.53}
    {$else}                                                    {!!.53}
  FillChar(IsamReXUserProcPtr, sizeof(IsamReXUserProcPtr), 0); {!!.53}
    {$ENDIF}                                                   {!!.53}
  {$else}                                                      {!!.53}
  FillChar(IsamReXUserProcPtr, sizeof(IsamReXUserProcPtr), 0); {!!.53}
  {$ENDIF}                                                     {!!.53}
  UserSaveEMSHandle := 0;
  {$IFNDEF Win32}
  if Swap(CallDosVersion) < $031E then
    IsamFlushDOS33 := False;
    {-Set IsamFlushDOS33 to False if not DOS Version 3.3 or higher}
  {$ENDIF}
end;


procedure IsamAddRec(    IFBPtr : IsamFileBlockPtr;
                       var RefNr  : LongInt;
                       var Source);
  {------}
  procedure AddRec1;
  var
    IR           : IsamInfoRec;
    OldFirstFree : LongInt;
  begin
    IsamSetDataBufferedFlag(IFBPtr);
    if not IsamOK then Exit;
    with IFBPtr^ do begin
      if SaveFB then
        IsamCopyInfoRecToIR(IFBPtr, 0, IR);
      with DIDPtr^[0]^ do begin
        OldFirstFree := FirstFree;
        IsamNewRec(IFBPtr, RefNr, 0);
        if not IsamOK then Exit;
        if SaveFB then begin
          if IsamFirstFreeChanged(IFBPtr, 0) then begin
            IsamSaveGivenInfoRec(IFBPtr, 0, IR, FirstFree);
          end
          else begin
            IsamSaveGivenInfoRec(IFBPtr, 0, IR, -2);
            if not IsamOK then
              Dec(NumRec);
          end;
          if not IsamOK then Exit;
          if (NSP = Nil) or IsamNetEmu then
            IsamFlushDOSDia(IFBPtr, False);
        end;
        IsamPutBlock(DatF, RefNr * LenRec, LenRec, Source);
        if not IsamOK then begin
          if IsamFirstFreeChanged(IFBPtr, 0) then begin
            FirstFree := OldFirstFree;
            Inc(NumberFree);
          end
          else begin
            Dec(NumRec);
          end;
          Exit;
        end;
      end;
      if SaveFB then begin
        if (NSP = Nil) or IsamNetEmu then begin
          IsamFlushDOSDat(IFBPtr);
        end
        else begin
          IsamPutInfoRec(IFBPtr, 0, False);
        end;
      end;
    end;
  end;
  {------}
begin
  IsamClearOK;
  AddRec1;
  if IFBPtr^.SaveFB then begin
    if IsamOK then begin
      IsamReduceDiaFile(IFBPtr);
    end
    else begin
      IsamRepairFileBlock(IFBPtr);
      if IsamOK then begin
        IsamOK := False;
        IsamError := 10001;
      end;
    end;
  end;
end;


procedure IsamDeleteRec(IFBPtr : IsamFileBlockPtr; RefNr : LongInt);
  {------}
  procedure DeleteRec1;
  var
    IR : IsamInfoRec;
  begin
    IsamSetDataBufferedFlag(IFBPtr);
    if not IsamOK then Exit;
    with IFBPtr^ do begin
      if SaveFB then begin
        IsamCopyInfoRecToIR(IFBPtr, 0, IR);
        if IsamFirstFreeChanged(IFBPtr, 0) then begin
          IsamSaveGivenInfoRec(IFBPtr, 0, IR, DIDPtr^[0]^.FirstFree);
        end
        else begin
          IsamSaveGivenInfoRec(IFBPtr, 0, IR, -2);
        end;
        if not IsamOK then Exit;
        if (NSP = Nil) or IsamNetEmu then
          IsamFlushDOSDia(IFBPtr, False);
      end;
      IsamDeleteRecOrPage(IFBPtr, RefNr, 0);
      if not IsamOK then Exit;
      if SaveFB then begin
        if (NSP = Nil) or IsamNetEmu then begin
          IsamFlushDOSDat(IFBPtr);
        end
        else begin
          IsamPutInfoRec(IFBPtr, 0, False);
        end;
      end;
    end;
  end;
  {------}
begin
  IsamClearOK;
  if (RefNr <= 0) or (RefNr > IFBPtr^.DIDPtr^[0]^.NumRec) then begin
    IsamOK := False;
    IsamError := 10135;
    Exit;
  end;
  DeleteRec1;
  if IFBPtr^.SaveFB then begin
    if IsamOK then begin
      IsamReduceDiaFile(IFBPtr);
    end
    else begin
      IsamRepairFileBlock(IFBPtr);
      if IsamOK then begin
        IsamOK := False;
        IsamError := 10002;
      end;
    end;
  end;
end;


procedure IsamPutRec(IFBPtr : IsamFileBlockPtr;              {!!.50}
                     RefNr  : LongInt;
                 var Source);
begin
  if RefNr <> 0 then begin
    with IFBPtr^, DIDPtr^[0]^ do begin
      IsamPutBlock(DatF, RefNr * LenRec, LenRec, Source);
      if IsamOK then begin
        if SaveFB and((NSP = Nil) or IsamNetEmu) then begin
          IsamOnlyFlushDOSDat(IFBPtr);
        end;
      end;
    end;
  end
  else begin
    IsamOK := False;
    IsamError := 10130;
  end;
end;


procedure IsamGetRec(IFBPtr : IsamFileBlockPtr;                {!!.50}
                     RefNr  : LongInt;
                 var Dest);

var                                                            {!!.53}
  RefPtr     : LongPtr;                                        {!!.53}
  Dummy1Ptr,                                                   {!!.53}
  Dummy2Ptr  : IsamLockEntryRecPtr;                            {!!.53}
  IsToUnLock : Boolean;                                        {!!.53}
begin
  {$IFDEF LockBeforeRead}
  IsToUnLock := False;
  with IFBPtr^, DIDPtr^[0]^ do begin
    if IsamOK and(NSP <> Nil) then begin
      IsamIsInLockList(@IFBPtr^.NSP^.LockEntryRec, RefNr, RefPtr,
          False, Dummy1Ptr, Dummy2Ptr);                        {!!.42}
      IsToUnLock := RefPtr = Nil;
      if IsToUnLock then begin
        IsamOK := btfLockMgrAcqLock(DatF.Handle,
                                    RefNr * LenRec, SizeOf(LongInt),
                                    0, 0);
        if not IsamOK then
          IsamError := 10140;
        IsToUnLock := IsamOK;
      end;
    end;
  end;
  {$ENDIF}
  if IsamOK then begin
    with IFBPtr^, DIDPtr^[0]^ do begin
      IsamGetBlock(DatF, RefNr * LenRec, LenRec, Dest);     
    end;
  end;
  {$IFDEF LockBeforeRead}
  if IsToUnLock then begin
    with IFBPtr^, DIDPtr^[0]^ do begin
      if not btfLockMgrRelLock(DatF.Handle, RefNr * LenRec, SizeOf(LongInt)) then begin
        IsamOK := False;
        IsamError := 10341;
      end;
    end;
  end;
  {$ENDIF}
end;


procedure IsamGetRecReadOnly(IFBPtr  : IsamFileBlockPtr;       {!!.50}
                             RefNr   : LongInt;
                         var Dest             );
var
  {$IFDEF VER60}
  TPtr : pointer;
  {$else}
  TPtr : PChar;                                                {!!.52}
  {$ENDIF}
  RefPtr     : LongPtr;                                        {!!.53}
  Dummy1Ptr,                                                   {!!.53}
  Dummy2Ptr  : IsamLockEntryRecPtr;                            {!!.53}
  IsToUnLock : Boolean;                                        {!!.53}

begin
  {$IFDEF LockBeforeRead}
  IsToUnLock := False;
  with IFBPtr^, DIDPtr^[0]^ do begin
    if IsamOK and(NSP <> Nil) then begin
      IsamIsInLockList(@IFBPtr^.NSP^.LockEntryRec, RefNr, RefPtr,
          False, Dummy1Ptr, Dummy2Ptr);                        {!!.42}
      IsToUnLock := RefPtr = Nil;
      if IsToUnLock then begin
        IsamOK := btfLockMgrAcqLock(DatF.Handle,
                                    RefNr * LenRec, SizeOf(LongInt),
                                    0, 0);
      end;
      if IsamOK then begin
        IsamGetBlock(DatF, RefNr * LenRec, LenRec, Dest);  
      end
      else begin
        IsamClearOK;
        {$IFDEF VER60}
        TPtr := Ptr(Seg(Dest), Ofs(Dest) + sizeof(longint));
        {$else}
        TPtr := @Dest;                                         {!!.52}
        inc(TPtr, 4);                                             {!!.52}
        {$ENDIF}
        IsamGetBlock(DatF, RefNr * LenRec + SizeOf(LongInt),
            LenRec - SizeOf(LongInt), TPtr^);                  
        if IsamOK then begin
          IsamOK := False;
          IsamError := 10205;
        end;
      end;
      if IsToUnLock then begin
        if not btfLockMgrRelLock(DatF.Handle, RefNr * LenRec, SizeOf(LongInt)) then begin
        end;
      end;
    end
    else begin
      if IsamOK then begin
        IsamGetBlock(DatF, RefNr * LenRec, LenRec, Dest);  
      end;
    end;
  end;
  {$else}
  if IsamOK then begin
    with IFBPtr^, DIDPtr^[0]^ do begin
      IsamGetBlock(DatF, RefNr * LenRec, LenRec, Dest);    
      if (IsamError = 10140) or (IsamError = 10335) then begin {!!.54}
        IsamClearOK;
        {$IFDEF VER60}
        TPtr := Ptr(Seg(Dest), Ofs(Dest) + sizeof(longint));
        {$else}
        TPtr := @Dest;                                         {!!.52}
        inc(TPtr, 4);                                          {!!.52}
        {$ENDIF}
        IsamGetBlock(DatF, RefNr * LenRec + SizeOf(LongInt),
            LenRec - SizeOf(LongInt), TPtr^);                 
        if IsamOK then begin
          IsamOK := False;
          IsamError := 10205;
        end;
      end;
    end;
  end;
  {$ENDIF}
end;


procedure IsamReleasePageBuffer;
var
  T1Ptr,
  T2Ptr,
  T3Ptr : IsamRingBufferRecPtr;
begin
  if IsamRBR1Ptr = Nil then Exit;
  T1Ptr := IsamRBR1Ptr^.Next;
  repeat
    T2Ptr := T1Ptr;
    T3Ptr := T1Ptr;
    T1Ptr := T1Ptr^.Next;
    with T3Ptr^ do begin
      {$IFDEF UseEMSHeap}
      if EMSEntry then begin
        FreeEMSMem(EMSPointer(PageEntryPtr), SizeOf(IsamPageEntry));
      end
      else begin
      {$ENDIF}
        FreeMem(PageEntryPtr, SizeOf(IsamPageEntry));
      {$IFDEF UseEMSHeap}
      end;
      {$ENDIF}
      IFBlPtr := Nil;
      UpDated := False;
    end;
    FreeMem(T3Ptr, SizeOf(IsamRingBufferRec));
  until T2Ptr = IsamRBR1Ptr;
  IsamRBR1Ptr := Nil;
end;

{$IFDEF UseWindowsInit}
function IsamGetPageBuffer(Pages : integer) : integer;  {!!.52 rewritten}
  {------}
  procedure InitAndInsertInRing(var RBRPtr : IsamRingBufferRecPtr;
                                    PEPtr  : IsamPageEntryPtr);
  begin
    if not IsamGetMem(RBRPtr, sizeof(IsamRingBufferRec)) then
      Exit;
    inc(IsamNrOfRingBufferRecs);
    with RBRPtr^ do begin
      IFBlPtr := nil;
      EMSEntry := False;
      UpDated := False;
      SaveBuffered := False;
      PageEntryPtr := PEPtr;
    end;
    if (IsamNrOfRingBufferRecs = 1) then begin
      IsamRBR1Ptr := RBRPtr;
      with IsamRBR1Ptr^ do begin
        Prev := IsamRBR1Ptr;
        Next := IsamRBR1Ptr;
      end;
    end
    else begin
      RBRPtr^.Prev := IsamRBR1Ptr;
      with IsamRBR1Ptr^ do begin
        RBRPtr^.Next := Next;
        Next^.Prev := RBRPtr;
        Next := RBRPtr;
      end;
    end;
  end;
  {------}
  procedure GetPageBuffer(MaxNrOfRecs : integer);
  var
    T1Ptr : IsamPageEntryPtr;
    T2Ptr : IsamRingBufferRecPtr;
  begin
    while (Pages > IsamNrOfRingBufferRecs) and
          (IsamNrOfRingBufferRecs < MaxNrOfRecs) do begin
      if not IsamGetMem(T1Ptr, sizeof(IsamPageEntry)) then
        Exit;
      InitAndInsertInRing(T2Ptr, T1Ptr);
      if (T2Ptr = nil) then begin
        FreeMem(T1Ptr, sizeof(IsamPageEntry));
        Exit;
      end;
      FillChar(T1Ptr^, sizeof(IsamPageEntry), 0);
      T1Ptr^.RingBufferPtr := T2Ptr;
    end;
  end;
  {------}
begin
  IsamGetPageBuffer := 0;
  IsamNrOfRingBufferRecs := 0;
  IsamRBR1Ptr := nil;
  GetPageBuffer(MaxInt);
  if (IsamNrOfRingBufferRecs < MaxHeight) then begin
    IsamReleasePageBuffer;
    IsamOK := False;
    IsamError := 10000;
  end
  else
    IsamGetPageBuffer := IsamNrOfRingBufferRecs;
end;
{$else}
function IsamGetPageBuffer(Free             : LongInt;
                             NrOfEMSTreePages : Word) : LongInt;
var
  SaveEMSHeapErrorFuncPtr : Pointer;
  UseMinimumNormalHeap    : Boolean;
  MaxNrOfNormalHeapRecs   : Word;
  LResult                 : LongInt;                          {!!.51}
  {------}
  procedure InitAndInsertInRing(var RBRPtr     : IsamRingBufferRecPtr;
                                      IsEMSEntry : Boolean;
                                      PEPtr      : IsamPageEntryPtr);
  begin
    GetMem(RBRPtr, SizeOf(IsamRingBufferRec));
    Inc(IsamNrOfRingBufferRecs);
    with RBRPtr^ do begin
      IFBlPtr := Nil;
      EMSEntry := IsEMSEntry;
      UpDated := False;
      SaveBuffered := False;
      PageEntryPtr := PEPtr;
    end;
    if IsamNrOfRingBufferRecs = 1 then begin
      IsamRBR1Ptr := RBRPtr;
      with IsamRBR1Ptr^ do begin
        Prev := IsamRBR1Ptr;
        Next := IsamRBR1Ptr;
      end;
    end
    else begin
      RBRPtr^.Prev := IsamRBR1Ptr;
      with IsamRBR1Ptr^ do begin
        RBRPtr^.Next := Next;
        Next^.Prev := RBRPtr;
        Next := RBRPtr;
      end;
    end;
  end;
  {------}
  {$IFDEF UseEMSHeap}
  procedure GetEMSPageBuffer;
  var
    T1Ptr : EMSPointer;
    T2Ptr : IsamRingBufferRecPtr;
    T3Ptr : IsamPageEntryPtr;
  begin
    while(MemAvail >(LongInt(RoundToGranul(SizeOf(IsamRingBufferRec)))
        + Free))
        and IsamAvailable(SizeOf(IsamRingBufferRec))
        and(IsamNrOfRingBufferRecs < NrOfEMSTreePages) do begin
      GetEMSMem(T1Ptr, SizeOf(IsamPageEntry));
      if T1Ptr = Nil then Exit;
      InitAndInsertInRing(T2Ptr, True, T1Ptr);
      T3Ptr := IsamPageEntryPtr(IsamRBufPtrToPgPtr(T2Ptr));
      FillChar(T3Ptr^, SizeOf(IsamPageEntry), 0);
      T3Ptr^.RingBufferPtr := T2Ptr;
    end;
  end;
  {$ENDIF}
  {------}
  procedure GetPageBuffer(MaxNrOfRecs : Word);
  var
    T1Ptr : IsamPageEntryPtr;
    T2Ptr : IsamRingBufferRecPtr;
  begin
    Inc(MaxNrOfRecs, IsamNrOfRingBufferRecs);
    while (MemAvail >=(LongInt(RoundToGranul(SizeOf(IsamRingBufferRec))
        + RoundToGranul(SizeOf(IsamPageEntry))) + Free))
        and IsamAvailable(SizeOf(IsamPageEntry))
        and(IsamNrOfRingBufferRecs < MaxNrOfRecs) do begin
      GetMem(T1Ptr, SizeOf(IsamPageEntry));
      if not IsamAvailable(SizeOf(IsamRingBufferRec)) then begin
        FreeMem(T1Ptr, SizeOf(IsamPageEntry));
        Exit;
      end;
      InitAndInsertInRing(T2Ptr, False, T1Ptr);
      FillChar(T1Ptr^, SizeOf(IsamPageEntry), 0);
      T1Ptr^.RingBufferPtr := T2Ptr;
    end;
  end;
  {------}
begin
  IsamNrOfRingBufferRecs := 0;
  IsamRBR1Ptr := Nil;
  UseMinimumNormalHeap := Free >= MinimizeUseOfNormalHeap;
  if UseMinimumNormalHeap then begin
    Free := Free - MinimizeUseOfNormalHeap;
  end;
  {$IFDEF UseEMSHeap}
  if NrOfEMSTreePages > 0 then begin
    SaveEMSHeapErrorFuncPtr := EMSHeapErrorFuncPtr;
    EMSHeapErrorFuncPtr := Nil;
      {-Set default handling for not to use EMSMaxAvail}
    GetEMSPageBuffer;
    EMSHeapErrorFuncPtr := SaveEMSHeapErrorFuncPtr;
  end;
  {$ENDIF}
  LResult := 0;                                                {!!.51}
  ILI(LResult).Hi := IsamNrOfRingBufferRecs;                   {!!.51}
  if UseMinimumNormalHeap then begin
    if IsamNrOfRingBufferRecs < MaxHeight then begin
      MaxNrOfNormalHeapRecs := MaxHeight - IsamNrOfRingBufferRecs;
    end
    else begin
      MaxNrOfNormalHeapRecs := 0;
    end;
  end
  else begin
    MaxNrOfNormalHeapRecs := 65535 - IsamNrOfRingBufferRecs;
  end;
  GetPageBuffer(MaxNrOfNormalHeapRecs);
  ILI(LResult).Lo := IsamNrOfRingBufferRecs - ILI(LResult).Hi; {!!.51}
  IsamGetPageBuffer := LResult;                                {!!.51}
  if IsamNrOfRingBufferRecs < MaxHeight then begin
    IsamReleasePageBuffer;
    IsamOK := False;
    IsamError := 10000;
  end;
end;
{$ENDIF}

procedure IsamGetStartingLong(    IFBPtr  : IsamFileBlockPtr;      {!!.42}
                                    RefNr   : LongInt;
                                var Dest    : LongInt);
var
  RefPtr     : LongPtr;                                        {!!.53}
  Dummy1Ptr,                                                   {!!.53}
  Dummy2Ptr  : IsamLockEntryRecPtr;                            {!!.53}
  IsToUnLock : Boolean;                                        {!!.53}
begin
  {$IFDEF LockBeforeRead}
  IsToUnLock := False;
  with IFBPtr^, DIDPtr^[0]^ do begin
    if NSP <> Nil then begin
      IsamIsInLockList(@IFBPtr^.NSP^.LockEntryRec, RefNr, RefPtr,
          False, Dummy1Ptr, Dummy2Ptr);                          {!!.42}
      IsToUnLock := RefPtr = Nil;
      if IsToUnLock then begin
        IsamOK := btfLockMgrAcqLock(DatF.Handle,
                                    RefNr * LenRec, SizeOf(LongInt),
                                    0, 0);
        if not IsamOK then
          IsamError := 10140;
        IsToUnLock := IsamOK;
      end;
    end;
  end;
  {$ENDIF}
  if IsamOK then begin
    with IFBPtr^, DIDPtr^[0]^ do begin
      IsamGetBlock(DatF, RefNr * LenRec, SizeOf(LongInt), Dest);
    end;
  end;
  {$IFDEF LockBeforeRead}
  if IsToUnLock then begin
    with IFBPtr^, DIDPtr^[0]^ do begin
      if not btfLockMgrRelLock(DatF.Handle, RefNr * LenRec, SizeOf(LongInt)) then begin
        IsamOK := False;
        IsamError := 10341;
      end;
    end;
  end;
  {$ENDIF}
end;


procedure IsamFindRecRef(    IFBPtr                  : IsamFileBlockPtr;
                           var UserDatRef              : LongInt;
                               NotFoundSearchDirection : Integer);{!!.42}
var
  LongDest : LongInt;
  Found    : Boolean;
begin
  Found := False;
  if NotFoundSearchDirection = 0 then begin
    IsamGetStartingLong(IFBPtr, UserDatRef, LongDest);
    if IsamOK then begin
      Found := LongDest = 0;
    end;
  end
  else begin
    if NotFoundSearchDirection > 0 then begin
      if UserDatRef < 1 then begin
        UserDatRef := 0;
      end
      else begin
        Dec(UserDatRef);
      end;
      repeat
        Inc(UserDatRef);
        IsamGetStartingLong(IFBPtr, UserDatRef, LongDest);
        if IsamOK then begin
          Found := LongDest = 0;
        end;
      until Found or not IsamOK
          or(UserDatRef = IFBPtr^.DIDPtr^[0]^.NumRec);
    end
    else begin
      if UserDatRef > IFBPtr^.DIDPtr^[0]^.NumRec then begin
        UserDatRef := Succ(IFBPtr^.DIDPtr^[0]^.NumRec);
      end
      else begin
        Inc(UserDatRef);
      end;
      if UserDatRef > 1 then begin
        repeat
          Dec(UserDatRef);
          IsamGetStartingLong(IFBPtr, UserDatRef, LongDest);
          if IsamOK then begin
            Found := LongDest = 0;
          end;
        until Found or not IsamOK or(UserDatRef = 1);
      end;
    end;
  end;
  if IsamError <> 10140 then begin
    if IsamError <> 9904 then begin
      IsamClearOK;
      if not Found then begin
        IsamOK := False;
        IsamError := 10275;
      end;
    end;
  end
  else begin
    IsamError := 10390;
  end;
end;


function IsamLongIntLessEqualHighBit(L         : LongInt;      {!!.42}
                                     IsLongInt : Boolean) : Word;
var
  LResult : Word;                                              {!!.51}
begin
  if IsLongInt then begin
    LResult := 33;                                             {!!.51}
  end
  else begin
    LResult := 17;                                             {!!.51}
    L := L Shl 16;
  end;
  repeat
    if (L and $80000000) = $80000000 then begin
      IsamLongIntLessEqualHighBit := LResult;                  {!!.51}
      Exit;
    end;
    L := L Shl 1;
    Dec(LResult);                                              {!!.51}
  until LResult = 0;                                           {!!.51}
  IsamLongIntLessEqualHighBit := 0;
end;


procedure IsamGetApprRecPos(IFBPtr     : IsamFileBlockPtr;
                        var RelPos     : Word;
                            Scale      : Word;
                            UserDatRef : LongInt);             {!!.42}
var
  N          : LongInt;
  HighBitSum : Word;
begin
  if Scale = 0 then begin
    IsamOK := False;
    IsamError := 10425;
    Exit;
  end;
  N := Succ(IFBPtr^.DIDPtr^[0]^.NumRec);
  if UserDatRef < 0 then
    UserDatRef := 0;
  if UserDatRef > N then
    UserDatRef := N;
  HighBitSum := IsamLongIntLessEqualHighBit(UserDatRef, True) +
      IsamLongIntLessEqualHighBit(Succ(Scale), False);
  if HighBitSum > 31 then begin
    UserDatRef := UserDatRef Shr(HighBitSum - 31);
    N := N Shr(HighBitSum - 31);
  end;
  RelPos := UserDatRef * Succ(Scale) Div N;
end;


procedure IsamGetApprRecRef(    IFBPtr     : IsamFileBlockPtr;
                                  RelPos     : Word;
                                  Scale      : Word;
                              var UserDatRef : LongInt);          {!!.42}
var
  N,
  D  : LongInt;
begin
  if (Scale = 0) or(RelPos > Scale) then begin
    IsamOK := False;
    IsamError := 10420;
    Exit;
  end;
  N := Succ(IFBPtr^.DIDPtr^[0]^.NumRec);
  if (IsamLongIntLessEqualHighBit(N, True) +
      IsamLongIntLessEqualHighBit(RelPos, False)) > 31 then begin
    D :=(N Div Succ(Scale)) * RelPos;
  end
  else begin
    D :=(N * RelPos) Div Succ(Scale);
  end;
  UserDatRef := D + N Div Succ(Scale) -(N Div Succ(Scale)) Shr 1;
end;


procedure IsamClearKey(IFBPtr : IsamFileBlockPtr; Key : Integer);
begin
  with IFBPtr^.DIDPtr^[Key]^ do begin
    PathInd := 0;
    SequentialOK := True;
  end;
end;


procedure IsamNextKey(    IFBPtr     : IsamFileBlockPtr;
                            Key        : Word;
                        var UserDatRef : LongInt;
                        var UserKey    : IsamKeyStr        );
var
  RefNr  : LongInt;
  IPgPtr : IsamPagePtr;
  {$IFDEF ASCIIZeroKeys}
  TempKeyZ : IsamKeyStr;
  {$ENDIF}
  {$IFDEF LengthByteKeys}
  TempKeyZ : IsamKeyStr absolute UserKey;
  {$ENDIF}
begin
  with IFBPtr^, DIDPtr^[Key]^ do begin
    if not SequentialOK then begin
      if SearchForSequentialEnabled then begin
        {$IFDEF ASCIIZeroKeys}
        IsamMakeStrZ(UserKey, TempKeyZ);
        {$ENDIF}
        IsamFindKey(IFBPtr, Key, UserDatRef, TempKeyZ);
        if IsamError = 0 then begin
          IsamClearOK;
        end
        else begin
          Exit;
        end;
      end
      else begin
        IsamOK := False;
        IsamError := 10255;
        Exit;
      end;
    end;
    SequentialOK := False;
    if PathInd = 0 then begin
      RefNr := RootRef;
    end
    else begin
      with Path [PathInd] do begin
        IsamGetPage(IFBPtr, PageRef, Key, IPgPtr);
        if not IsamOK then Exit;
        RefNr := IPgPtr^.ItemArray [ItemArrInd].PageRef;
      end;
    end;
    while RefNr <> 0 do begin
      Inc(PathInd);
      with Path [PathInd] do begin
        PageRef := RefNr;
        ItemArrInd := 0;
      end;
      IsamGetPage(IFBPtr, RefNr, Key, IPgPtr);
      if not IsamOK then Exit;
      RefNr := IPgPtr^.BckwPageRef;
    end;
    if PathInd <> 0 then begin
      while(PathInd > 1) and
         (Path [PathInd].ItemArrInd = IPgPtr^.ItemsOnPage) do begin
        Dec(PathInd);
        IsamGetPage(IFBPtr, Path [PathInd].PageRef, Key, IPgPtr);
        if not IsamOK then Exit;
      end;
      if Path [PathInd].ItemArrInd < IPgPtr^.ItemsOnPage then begin
        with Path [PathInd] do begin
          Inc(ItemArrInd);
          with IPgPtr^.ItemArray [ItemArrInd] do begin
            IsamMakeStr(KeyStr, UserKey);
            UserDatRef := DataRef;
          end;
        end;
      end
      else begin
        PathInd := 0;
      end;
    end;
    IsamOK := PathInd <> 0;
    if not IsamOK then
      IsamError := 10250;
    SequentialOK := True;
  end;
end;


procedure IsamFindKeyAndRef(    IFBPtr                  : IsamFileBlockPtr;
                                  Key                     : Word;
                              var UserDatRef              : LongInt;
                              var UserKey                 : IsamKeyStr;
                                  NotFoundSearchDirection : Integer);
var
  IPgPtr   : IsamPagePtr;
  {$IFDEF ASCIIZeroKeys}
  TempKeyZ : IsamKeyStr;
  {$ENDIF}
  {$IFDEF LengthByteKeys}
  TempKeyZ : IsamKeyStr absolute UserKey;
  {$ENDIF}
begin
  {$IFDEF ASCIIZeroKeys}
  IsamMakeStrZ(UserKey, TempKeyZ);
  {$ENDIF}
  IsamFindKey(IFBPtr, Key, UserDatRef, TempKeyZ);
  if not IsamOK and(IsamError = 0) then begin
    IsamClearOK;
    if NotFoundSearchDirection > 0 then begin
      IsamNextKey(IFBPtr, Key, UserDatRef, UserKey);
    end
    else begin
      if NotFoundSearchDirection < 0 then begin
        with IFBPtr^, DIDPtr^[Key]^ do begin
          if PathInd = 0 then begin
            IsamOK := False;
            IsamError := 10260;
          end
          else begin
            with Path [PathInd] do begin
              IsamGetPage(IFBPtr, PageRef, Key, IPgPtr);
              if not IsamOK then Exit;
              with IPgPtr^.ItemArray [ItemArrInd] do begin
                UserDatRef := DataRef;
                IsamMakeStr(KeyStr, UserKey);
              end;
            end;
          end;
        end;
      end
      else begin
        IsamOK := False;
        IsamError := 10270;
      end;
    end;
  end;
end;


procedure IsamPrevKey(    IFBPtr     : IsamFileBlockPtr;
                            Key        : Word;
                        var UserDatRef : LongInt;
                        var UserKey    : IsamKeyStr           );
var
  RefNr  : LongInt;
  IPgPtr : IsamPagePtr;
begin
  with IFBPtr^, DIDPtr^[Key]^ do begin
    if not SequentialOK then begin
      if SearchForSequentialEnabled then begin
        IsamFindKeyAndRef(IFBPtr, Key, UserDatRef, UserKey, 0);
        if not IsamOK then begin
          if IsamError = 10270 then begin
            IsamClearOK;
            IsamFindKeyAndRef(IFBPtr, Key, UserDatRef, UserKey, -1);
          end;
          Exit;
        end;
      end
      else begin
        IsamOK := False;
        IsamError := 10265;
        Exit;
      end;
    end;
    SequentialOK := False;
    if PathInd = 0 then begin
      RefNr := RootRef;
    end
    else begin
      with Path [PathInd] do begin
        IsamGetPage(IFBPtr, PageRef, Key, IPgPtr);
        if not IsamOK then Exit;
        Dec(ItemArrInd);
        if ItemArrInd = 0 then begin
          RefNr := IPgPtr^.BckwPageRef;
        end
        else begin
          RefNr := IPgPtr^.ItemArray [ItemArrInd].PageRef;
        end;
      end;
    end;
    while RefNr <> 0 do begin
      IsamGetPage(IFBPtr, RefNr, Key, IPgPtr);
      if not IsamOK then Exit;
      Inc(PathInd);
      with Path [PathInd] do begin
        PageRef := RefNr;
        ItemArrInd := IPgPtr^.ItemsOnPage;
      end;
      with IPgPtr^ do begin
        RefNr := ItemArray [ItemsOnPage].PageRef;
      end;
    end;
    if PathInd <> 0 then begin
      while(PathInd > 1) and(Path [PathInd].ItemArrInd = 0) do begin
        Dec(PathInd);
        IsamGetPage(IFBPtr, Path [PathInd].PageRef, Key, IPgPtr);
        if not IsamOK then Exit;
      end;
      if Path [PathInd].ItemArrInd > 0 then begin
        with IPgPtr^.ItemArray [Path [PathInd].ItemArrInd] do begin
          IsamMakeStr(KeyStr, UserKey);
          UserDatRef := DataRef;
        end;
      end
      else begin
        PathInd := 0;
      end;
    end;
    IsamOK := PathInd <> 0;
    if not IsamOK then
      IsamError := 10260;
    SequentialOK := True;
  end;
end;


procedure IsamSearchKey(    IFBPtr     : IsamFileBlockPtr;
                              Key        : Word;
                          var UserDatRef : LongInt;
                          var UserKey    : IsamKeyStr       );
var
  {$IFDEF ASCIIZeroKeys}
  TempKeyZ : IsamKeyStr;
  {$ENDIF}
  {$IFDEF LengthByteKeys}
  TempKeyZ : IsamKeyStr absolute UserKey;
  {$ENDIF}
begin
  {$IFDEF ASCIIZeroKeys}
  IsamMakeStrZ(UserKey, TempKeyZ);
  {$ENDIF}
  UserDatRef := 0;
  IsamFindKey(IFBPtr, Key, UserDatRef, TempKeyZ);
  if not IsamOK and(IsamError = 0) then begin
    IsamClearOK;
    IsamNextKey(IFBPtr, Key, UserDatRef, UserKey);
  end;
  if not IsamOK then begin
    case IsamError of
      0, 10200..10299 : IsamError := 10210;
    end; {Case}
  end;
end;


procedure IsamDeleteAllKeys(IFBPtr : IsamFileBlockPtr; Key : Word);
var
  IR : IsamInfoRec;
begin
  IsamSetDataBufferedFlag(IFBPtr);
  if not IsamOK then Exit;
  with IFBPtr^ do begin
    if SaveFB then begin
      IsamCopyInfoRecToIR(IFBPtr, Key, IR);
      IsamSaveGivenInfoRec(IFBPtr, Key, IR, -2);
      if not IsamOK then Exit;
      if (NSP = Nil) or IsamNetEmu then begin
        IsamFlushDOSDia(IFBPtr, False);
        if not IsamOK then Exit;
      end;
    end;
    IsamDestroyPagesOfKeyNr(IFBPtr, Key);
    with DIDPtr^[Key]^ do begin
      NumKeys := 0;
      FirstFree := -1;
      NumberFree := 0;
      NumRec := 0;
      RootRef := 0;
      PathInd := 0;
      FirstFreeChanged := True;
      InfoRecChanged := True;
    end;
    if SaveFB then begin
      IsamPutInfoRec(IFBPtr, Key, False);
      if not IsamOK then Exit;
      if (NSP = Nil) or IsamNetEmu then
        IsamFlushDOSIx(IFBPtr);
    end;
  end;
end;


procedure IsamGetApprRelPos(    IFBPtr     : IsamFileBlockPtr;
                                  Key        : Word;
                              var RelPos     : Word;
                                  Scale      : Word;
                                  UserKey    : IsamKeyStr;
                                  UserDatRef : LongInt);
var
  TempL,
  RefNr               : LongInt;
  C, K, L, R          : Integer;
  IPgPtr              : IsamPagePtr;
  Stop                : Boolean;
  EstEltNr,
  EstNrOfElts,
  RootItemNr,
  ItemNrOnSecPage,
  NrOfItemsOnRoot,
  NrOfItemsOnSecPage,
  AvgPageSizeSecLevel : Word;
begin
  if Scale = 0 then begin
    IsamOK := False;
    IsamError := 10425;
    Exit;
  end;
  with IFBPtr^, DIDPtr^[Key]^ do begin
    if RootRef = 0 then begin
      IsamOK := False;
      IsamError := 10285;
      Exit;
    end;
    AvgPageSizeSecLevel := UsedPageSize -(UsedPageSize Shr 2);
     {-75% of real page contents assumed}
    NrOfItemsOnSecPage := AvgPageSizeSecLevel;
    RootItemNr := $FFFF;
    ItemNrOnSecPage := 0;
    IsamMakeStrZ(UserKey, UserKey);
    Stop := False;
    RefNr := RootRef;
    while(RefNr <> 0) and(not Stop) do begin
      IsamGetPage(IFBPtr, RefNr, Key, IPgPtr);
      if not IsamOK then Exit;
      with IPgPtr^ do begin
        L := 1;
        R := ItemsOnPage;
        repeat
          K :=(L + R) Shr 1;
          C := IsamCompKeys(UserKey, ItemArray [K].KeyStr,
                            UserDatRef, ItemArray [K].DataRef,
                            AllowDupKeys);
          if C <= 0 then
            R := Pred(K);
          if C >= 0 then
            L := Succ(K);
        until R < L;
        if L - R > 1 then begin
          Stop := True;
        end
        else begin
          if R = 0 then begin
            RefNr := BckwPageRef;
          end
          else begin
            RefNr := ItemArray [R].PageRef;
          end;
        end;
        if RootItemNr = $FFFF then begin
          NrOfItemsOnRoot := ItemsOnPage;
          if L - R > 1 then begin
            RootItemNr := K;
          end
          else begin
            RootItemNr := R;
          end;
        end
        else begin
          NrOfItemsOnSecPage := ItemsOnPage;
          if L - R > 1 then begin
            ItemNrOnSecPage := K;
          end
          else begin
            ItemNrOnSecPage := R;
          end;
          Stop := True;
        end;
      end;
    end;
  end;
  EstNrOfElts := Succ(AvgPageSizeSecLevel) * Succ(NrOfItemsOnRoot);
    {-Includes backward references on second level(<=> 0''/items on root)}
  if (ItemNrOnSecPage = 1) and(RootItemNr = 0) then begin
    {-Prevent zero position from growing}
    EstEltNr := 1;
  end
  else begin
    TempL :=((LongInt(ItemNrOnSecPage)
        * LongInt(Succ(AvgPageSizeSecLevel))) Shl 1)
        Div LongInt(NrOfItemsOnSecPage);
    EstEltNr := RootItemNr * Succ(AvgPageSizeSecLevel)
        +(TempL - TempL Shr 1);
  end;
  RelPos := Word((LongInt(EstEltNr) * Succ(LongInt(Scale)))
      Div LongInt(EstNrOfElts));
  if RelPos > Scale then
    RelPos := Scale;
    {-Slighly unsymethric, but simple}
end;


procedure IsamGetApprKeyAndRef(    IFBPtr     : IsamFileBlockPtr;
                                     Key,
                                     RelPos     : Word;
                                     Scale      : Word;
                                 var UserKey    : IsamKeyStr;
                                 var UserDatRef : LongInt);
var
  EstNrOfElts,
  EstEltNr,
  EstRootItemNr,
  EstItemNrOnSecPage,
  CorItemNrOnSecPage,
  AvgPageSizeSecLevel : Word;
  IPgPtr              : IsamPagePtr;
  TempL               : LongInt;
begin
  if (Scale = 0) or(RelPos > Scale) then begin
    IsamOK := False;
    IsamError := 10420;
    Exit;
  end;
  with IFBPtr^, DIDPtr^[Key]^ do begin
    if RootRef = 0 then begin
      IsamOK := False;
      IsamError := 10280;
      Exit;
    end;
    IsamGetPage(IFBPtr, RootRef, Key, IPgPtr);
    if not IsamOK then Exit;
    AvgPageSizeSecLevel := UsedPageSize -(UsedPageSize Shr 2);    {!!.42}
     {-75% of real page contents assumed}
  end;
  with IPgPtr^ do begin
    if BckwPageRef = 0 then begin
      {-No second level available}
      AvgPageSizeSecLevel := 0;
    end;                                                           {!!.42}
    EstNrOfElts := Succ(AvgPageSizeSecLevel) * Succ(ItemsOnPage);
      {-Includes backward references on second level(<=> 0''/items on root)}
    EstEltNr :=(LongInt(EstNrOfElts) * LongInt(RelPos))
        Div Succ(LongInt(Scale))
        + EstNrOfElts Div Scale -(EstNrOfElts Div Scale) Shr 1;
      {-Numbering starts at zero}
    if EstEltNr >= EstNrOfElts then
      EstEltNr := Pred(EstNrOfElts);
      {-Correct estimate error}
    EstRootItemNr := EstEltNr Div(Succ(AvgPageSizeSecLevel));
    EstItemNrOnSecPage := EstEltNr Mod(Succ(AvgPageSizeSecLevel));
    if (BckwPageRef = 0) or(EstItemNrOnSecPage = 0) then begin
      {-No second level available}
      if EstRootItemNr = 0 then begin
        {-Build the smallest key}
        UserKey := '';
        UserDatRef := 0;
      end
      else begin
        with ItemArray [EstRootItemNr] do begin
          IsamMakeStr(KeyStr, UserKey);
          UserDatRef := DataRef;
        end;
      end;
      Exit;
    end;
    if EstRootItemNr = 0 then begin
      IsamGetPage(IFBPtr, BckwPageRef, Key, IPgPtr);
    end
    else begin
      IsamGetPage(IFBPtr, ItemArray [EstRootItemNr].PageRef, Key,
          IPgPtr);
    end;
  end;
  if not IsamOK then Exit;
  with IPgPtr^ do begin
    {-Prevent RelPos one from decreasing}
    if (EstItemNrOnSecPage = 2) and(EstRootItemNr = 0) then begin
      CorItemNrOnSecPage := 2;
    end
    else begin
      TempL :=((LongInt(EstItemNrOnSecPage) * LongInt(ItemsOnPage)) Shl 1)
          Div LongInt(AvgPageSizeSecLevel);
      CorItemNrOnSecPage := TempL - TempL Shr 1;
    end;
    if CorItemNrOnSecPage = 0 then
      Inc(CorItemNrOnSecPage);
      {-Slighly unsymethric, but simple}
    with ItemArray [CorItemNrOnSecPage] do begin
      IsamMakeStr(KeyStr, UserKey);
      UserDatRef := DataRef;
    end;
  end;
end;


procedure IsamFlushFileBlock(IFBPtr : IsamFileBlockPtr);
var
  DoIt : Boolean;
begin
  with IFBPtr^ do begin
    if SaveFB or ReadOnlyFB then Exit;                          {!!.50mod}
      {-A save fileblock and a read only fileblock
        never have data buffered}
    if NSP = Nil then begin
      DoIt := True;
    end
    else begin
      DoIt := NSP^.Locked;
    end;
    if DoIt then begin
      if NSP = Nil then begin
        if DataBuffered then
          IsamFlushPageInfo(IFBPtr, False);                 {!!.50mod}
        if not IsamOK then Exit;
        IsamFlushDOSDat(IFBPtr);
      {$IFNDEF NoNet}
      end
      else begin
        if DataBuffered then begin                             {!!.50}
          IsamBuildAndPutFlagSetFlushNewData(IFBPtr);
          if not IsamOK then Exit;
        end;                                                   {!!.50}
        IsamOnlyFlushDOSDat(IFBPtr);
        if not IsamOK then Exit;
        if DataBuffered then                                {!!.50mod}
          IsamFlushDOSDia(IFBPtr, False);                   {!!.50mod}
      {$ENDIF}
      end;
      if not IsamOK then Exit;
      if (NrOfKeys > 0) and DataBuffered then begin         {!!.50mod}
        IsamFlushDOSIx(IFBPtr);
        if not IsamOK then Exit;
      end;
      DataBuffered := False;
    end;
  end;
end;


procedure IsamFlushAllFileBlocks;
var
  TPtr  : IsamOpenFileBlockListPtr;
begin
  TPtr := IsamOFBLPtr;
  while TPtr <> Nil do begin
    IsamFlushFileBlock(TPtr^.OIFBPtr);
    if not IsamOK then Exit;
    TPtr := TPtr^.Next;
  end;
end;


procedure IsamDeleteFileBlock(FName  : IsamFileBlockName);
var
  TF        : IsamFile;
  DummyStr,
  FNameD,
  FNameI    : IsamFileBlockName;
begin
  IsamExtractFileNames(FName, FNameD, FNameI);
  IsamExtractFileNames(FNameI, FNameI, DummyStr);
  IsamAssign(TF, IsamForceExtension(FNameI, IxExtension));
  IsamDelete(TF);
  IsamClearOK;
  IsamAssign(TF, IsamForceExtension(FNameD, DiaExtension));
  IsamDelete(TF);
  IsamClearOK;
  IsamAssign(TF, IsamForceExtension(FNameD, DatExtension));
  IsamDelete(TF);
end;


procedure IsamCreateFileBlock(FName        : IsamFileBlockName;
                                DatSLen      : LongInt;
                                NumberOfKeys : Word;
                                IID          : IsamIndDescr );
                                                    {!!.52 rearranged}
var
  TIFBPtr  : IsamFileBlockPtr;
  I        : Integer;
  DummyStr,
  FNameD,
  FNameI   : IsamFileBlockName;
  {------}
  procedure UnDo(UnDoLevel : Word; Err : Integer);
  begin
    with TIFBPtr^ do begin
      if UnDoLevel >= 3 then begin
        IsamClose(IndF);
        IsamDelete(IndF);
      end;
      if UnDoLevel >= 2 then begin
        IsamClose(DatF);
        IsamDelete(DatF);
      end;
      if UnDoLevel >= 1 then begin
        IsamFreeKeyDescrMem(TIFBPtr, NumberOfKeys);
        FreeMem(TIFBPtr, SizeOf(TIFBPtr^));
      end;
      IsamOK := Err = 0;
      IsamError := Err;
    end;
  end;
  {------}
begin
  if DatSLen < SizeOf(IsamSmallInfoRec) then
    IsamError := 10020;
  if (NumberOfKeys > MaxNrOfKeys) or(NumberOfKeys < 0) then
    IsamError := 10050;
  if not IsamGetMem(TIFBPtr, sizeof(TIFBPtr^)) then
    IsamError := 10090;
  if (IsamError <> 0) then
    begin
    IsamOK := False;
    Exit;
    end;
  IsamDeleteFileBlock(FName);
  IsamClearOK;
  TIFBPtr^.NrOfKeys := NumberOfKeys;
    {-for possible IsamFreeKeyDescrMem below}
  IsamGetKeyDescrMem(TIFBPtr, NumberOfKeys);
  if not IsamOK then begin
    FreeMem(TIFBPtr, SizeOf(TIFBPtr^));
    Exit;
  end;
  IsamExtractFileNames(FName, FNameD, FNameI);
  IsamExtractFileNames(FNameI, FNameI, DummyStr);
  with TIFBPtr^ do begin
    IsamAssign(DatF, IsamForceExtension(FNameD, DatExtension));
    IsamRewrite(DatF);
    if not IsamOK then begin
      UnDo(1, IsamError);
      Exit;
    end;
    with DIDPtr^[0]^ do begin
      FirstFree := -1;
      NumberFree := 0;
      NumRec := 0;
      LenRec := DatSLen;
      InfoRecChanged := True;
    end;
    IsamPutDummyBlock(DatF, 0, DIDPtr^[0]^.LenRec);
    if IsamOK then
      IsamPutInfoRec(TIFBPtr, 0, False);
    if not IsamOK then begin
      UnDo(2, IsamError);
      Exit;
    end;
    IsamClose(DatF);
    if not IsamOK then begin
      UnDo(2, IsamError);
      Exit;
    end;
    if NumberOfKeys > 0 then begin
      IsamAssign(IndF, IsamForceExtension(FNameI, IxExtension));
      IsamRewrite(IndF);
      if not IsamOK then begin
        UnDo(2, IsamError);
        Exit;
      end;
      MaxPages := 0;
      BlockLen := 0;
      for I := 1 to NumberOfKeys do begin
        with DIDPtr^[I]^ do begin
          KeyLen := IID [I].KeyL;
          if (KeyLen < 1) or(KeyLen > MaxKeyLen) then begin
            UnDo(3, 10055);
            Exit;
          end;
          LenRec := LongInt(9 + KeyLen) * LongInt(CreatePageSize){!!.42}
              + LongInt(6);
          BlockOfs := BlockLen;
          BlockLen := BlockLen + LenRec;
          NumKeys := 0;
          UsedPageSize := CreatePageSize;                          {!!.42}
          FirstFree := -1;
          NumberFree := 0;
          NumRec := 0;
          RootRef := 0;
          AllowDupKeys := IID [I].AllowDupK;
          InfoRecChanged := True;
        end;
      end;
      IsamPutDummyBlock(IndF, 0, BlockLen);
      if IsamOK then
        IsamFlushPageInfo(TIFBPtr, False);
      if IsamOK then
        IsamClose(IndF);
      if not IsamOK then begin
        UnDo(3, IsamError);
        Exit;
      end;
    end;
    UnDo(1, 0);
  end;
end;


procedure IsamOpenRawFileBlock(var IFBPtr   : IsamFileBlockPtr;
                                     FNameD,
                                     FNameI   : IsamFileBlockName;
                                     RealNet,
                                     ReadOnly : Boolean);
                                                    {!!.52 rearranged}
var
  I                  : Integer;
  IDID               : IsamDatIndDescr;
  IDIDPtr            : IsamDatIndDescrPtr;
  HeadOrIndCorrupted,
  Dummy              : Boolean;
  CalcKeyL           : LongInt;
  {------}
  procedure UnDo(Level : Word; Error : Integer);
  begin
    with IFBPtr^ do begin
      if Level >= 4 then begin
        if NrOfKeys > 0 then
          IsamClose(IndF);
      end;
      if Level >= 3 then begin
        IsamFreeKeyDescrMem(IFBPtr, NrOfKeys);
      end;
      if Level >= 2 then
        IsamClose(DatF);
      if Level >= 1 then
        FreeMem(IFBPtr, SizeOf(IFBPtr^));
    end;
    IsamError := Error;
    IsamOK := IsamError = 0;
  end;
  {------}
begin
  if not IsamGetMem(IFBPtr, SizeOf(IFBPtr^)) then begin        {!!.52}
    IsamOK := False;
    IsamError := 10100;
    Exit;
  end;
  with IFBPtr^ do begin
    IsamAssign(DatF, IsamForceExtension(FNameD, DatExtension));
    IsamReset(DatF, RealNet, ReadOnly);
    if not IsamOK then begin
      UnDo(1, IsamError);
      Exit;
    end;
    IDIDPtr := Addr(IDID);
    DIDPtr := Addr(IDIDPtr);
      {-First hold info lokally}
    IsamGetInfoRec(IFBPtr, 0, HeadOrIndCorrupted);
    if not IsamOK then begin
      UnDo(2, IsamError);
      Exit;
    end;
    if NrOfKeys > MaxNrOfKeys then begin
      UnDo(2, 10060);
      Exit;
    end;
    IsamGetKeyDescrMem(IFBPtr, NrOfKeys);
    if not IsamOK then begin
      UnDo(2, IsamError);
      Exit;
    end;
    if NrOfKeys > 0 then begin
      IsamAssign(IndF, IsamForceExtension(FNameI, IxExtension));
      IsamReset(IndF, RealNet, ReadOnly);
      if not IsamOK then begin
        if (IsamError <> 9901) and (IsamError <> 10110) then
          IsamError := 10010;
          {-Index file may be ok, if out of handles or drive not ready}
        UnDo(3, IsamError);
        Exit;
      end;
    end;
    MaxPages := 0;
    BlockLen := 0;
    IDID.FirstFreeChanged := False;
    IDID.InfoRecChanged := False;
    DIDPtr^[0]^ := IDID;
      {-Copy lokal info to real}
    for I := 1 to NrOfKeys do begin
      with DIDPtr^[I]^ do begin
        BlockOfs := BlockLen;
        IsamGetInfoRec(IFBPtr, I, Dummy);
        if not IsamOK then begin
          UnDo(4, IsamError);
          Exit;
        end;
        if UsedPageSize > MaxPageSize then begin               {!!.42}
          UnDo(4, 10122);                                      {!!.42}
          Exit;                                                {!!.42}
        end;                                                   {!!.42}
        CalcKeyL :=(LenRec * LongInt(10000) - LongInt(60000))  {!!.42}
            Div LongInt(UsedPageSize) - LongInt(90000);
        if (((CalcKeyL Div LongInt(10000)) * LongInt(10000)) <> CalcKeyL)
            then begin
          UnDo(4, 10120);
          Exit;
        end;
        if (CalcKeyL >(LongInt(MaxKeylen) * LongInt(10000)))
            or(CalcKeyL < LongInt(10000)) then begin
          UnDo(4, 10121);
          Exit;
        end;
        KeyLen := Word(CalcKeyL Div LongInt(10000));
        BlockLen := BlockLen + LenRec;
        if NumRec > MaxPages then
          MaxPages := NumRec;
        PathInd := 0;
        InfoRecChanged := False;
        FirstFreeChanged := False;
        SearchForSequentialEnabled := SearchForSequentialDefault;
        SequentialOK := False;
      end;
    end;
    if HeadOrIndCorrupted then begin
      UnDo(4, 10010);
      Exit;
    end;
    DataBuffered := False;
    CharConvProc := BTNoCharConvert;                           {!!.50}
    CCHookPtr := Nil;                                          {!!.50}
    CCDestrWrite := True;                                      {!!.50}
    IsamAddToOpenFileBlockList(IFBPtr);
    if not IsamOK then begin
      UnDo(4, 10100);
      Exit;
    end;
  end;
end;


procedure IsamCloseRawFileBlock(var IFBPtr : IsamFileBlockPtr);
begin
  with IFBPtr^ do begin
    IsamDestroyPages(IFBPtr);
    if NrOfKeys > 0 then
      IsamClose(IndF);
    IsamClose(DatF);
    IsamFreeKeyDescrMem(IFBPtr, NrOfKeys);
    IsamRemoveFromOpenFileBlockList(IFBPtr);
    ValidSign := 0;
    FreeMem(IFBPtr, SizeOf(IFBPtr^));
    IFBPtr := Nil;
  end;
end;


procedure IsamOpenFileBlock(var IFBPtr      : IsamFileBlockPtr;
                                  FName       : IsamFileBlockName;
                                  ReadOnly,
                                  AllReadOnly,
                                  Save,
                                  Net         : Boolean);
                                                    {!!.52 rearranged}
type
  DialogFileState = (Correct, Raw, ToRepair);
var
  DiaFileState : DialogFileState;
  DiaFile      : IsamFile;
  DiaFOpened,
  DiaFExisted  : Boolean;
  FlagSet      : IsamFlagSet;
  FlagSetLen   : Word;                                         {!!.42}
  DiaFileLen   : LongInt;
  Ok,
  UseDiaFile,
  UseLock,
  RealNet      : Boolean;
  DummyStr,
  FNameD,
  FNameI       : IsamFileBlockName;
  TIFBPtr      : IsamFileBlockPtr;
  {------}
  procedure UnDo(Level : Word; Error : Integer);
  var
    Dummy : Boolean;
  begin
    if Level >= 4 then begin
      if Net then
        FreeMem(TIFBPtr^.NSP, SizeOf(IsamNetSupport));
    end;
    if Level >= 3 then begin
      IsamCloseRawFileBlock(TIFBPtr);
    end;
  {$IFNDEF NoNet}
    if Level >= 2 then begin
      if UseLock then
        Dummy := btfLockMgrRelLock(DiaFile.Handle, 3, 1);
    end;
  {$ENDIF}
    if Level >= 1 then begin
      if DiaFOpened then
        IsamClose(DiaFile);
    end;
    if not DiaFExisted then begin
      IsamDelete(DiaFile);
    end;
    if Net then begin                                         {!!.42mod}
      if Error = 10140 then
        Error := 10355;
    end;
    IsamError := Error;
    IsamOK := IsamError = 0;
  end;
  {------}
  function CheckDiaFile : DialogFileState;
  var
    Len    : DWORD;                                            {!!.54}
    Value  : Word;
    TestNr : Word;                                             {!!.42}
    Ok     : Boolean;                                          {!!.42}
  begin
    CheckDiaFile := ToRepair;                                  {!!.31}
    IsamLongSeekEOF(DiaFile, Len);
    if not IsamOK then Exit;
    if Len = 1 then begin
      CheckDiaFile := Raw;
    end
    else begin
      Value := 0;                                              {!!.42}
      IsamGetBlock(DiaFile, 1, SizeOf(Word), Value);           {!!.53}
      if IsamOK and(Value = 2000) then begin                   {!!.42}
      {--Is Len correct?}                                      {!!.42}
        TestNr :=(Len - 3) Div FlagSetLen;                     {!!.42}
        if RealNet then begin                                  {!!.42}
          Ok := TestNr > 1;                                    {!!.42}
        end
        else begin                                             {!!.42}
          Ok := TestNr = 1;                                    {!!.42}
        end;                                                   {!!.42}
        if Ok then begin                                       {!!.42}
          if LongInt(TestNr) * FlagSetLen =(Len - 3) then begin{!!.42}
            CheckDiaFile := Correct;                           {!!.42}
            DiaFileLen := Len;                                 {!!.42}
            Exit;                                              {!!.42}
          end;                                                 {!!.42}
        end;                                                   {!!.42}
      end;                                                     {!!.42}
    end;
    if ReadOnly and(UseDiaFile or(Len <> 1)) then begin
      {-to use a dialog file in read only mode it must be correct;
        if it is used in read only mode, it's length has to be 1(can
        only occur in single user mode "$Define NoNet")}
      IsamOK := False;
      IsamError := 10440;
    end;
  end;
  {------}
begin
{--Correct parameters}
  if AllReadOnly then
    ReadOnly := True;
  if ReadOnly then
    Save := False;
  {$IFDEF NoNet}
  Net := False;
  {$ENDIF}

{--Set controlling values}
  RealNet :=(IsamInitializedNet <> NoNet) and Net;
  UseLock := RealNet and not AllReadOnly;
  if Net then begin
    UseDiaFile := not AllReadOnly;
  end
  else begin
    UseDiaFile := Save;
  end;

{--Set local values}
  IsamExtractFileNames(FName, FNameD, FNameI);
  IsamExtractFileNames(FNameI, FNameI, DummyStr);

{--Open dialog file in specified mode}
  IsamAssign(DiaFile, IsamForceExtension(FNameD, DiaExtension));
  DiaFExisted := True;
  repeat
    Ok := True;
    IsamReset(DiaFile, RealNet, ReadOnly);
    DiaFOpened := IsamOK;
    if not IsamOK then begin
      if IsamError = 9903 then begin
        DiaFExisted := False;
        IsamClearOK;
        if UseDiaFile then begin
          Ok := False;
          if ReadOnly then begin
            UnDo(0, 10440);
            Exit;
          end
          else begin
            IsamMakeDiaFile(DiaFile);
            if IsamOK then
              IsamClose(DiaFile);
            if not IsamOK then begin
              UnDo(0, IsamError);
              Exit;
            end;
          end;
        end;
      end
      else begin
        UnDo(0, IsamError);
        Exit;
      end;
    end;
  until Ok;

{--Set lock on dialog file station 0 during open}
  {$IFNDEF NoNet}
  if UseLock then begin
    if not btfLockMgrAcqLock(DiaFile.Handle,
                             3,
                             1, {pretend the flag set is one byte}
                             IsamLockTimeOut * IsamFBLockTimeOutFactor,
                             IsamDelayBetwLocks) then begin
      UnDo(1, 10355);
      Exit;
    end;
   (* !!.52 this double locking code has been removed
    if IsamLockRecord(3, 1, DiaFile.Handle, 0, 0) then begin   {!!.42}
      UnDo(2, 10480);                                          {!!.42}
      Exit;                                                    {!!.42}
    end;                                                       {!!.42}
    *)
  end;                                                         {!!.42}
  {$ENDIF}

{--Open data and index files and read headers}
  IsamOpenRawFileBlock(TIFBPtr, FNameD, FNameI, RealNet, ReadOnly);
  if not IsamOK then begin
    UnDo(2, IsamError);
    Exit;
  end;

{--Set additional local values}
  FlagSetLen := Succ(Succ(TIFBPtr^.NrOfKeys) Shr 3);
  DiaFileLen :=
      LongInt(FlagSetLen) * Succ(LongInt(IsamDefNrOfWS)) + 3;  {!!.42}

{--Check dialog file}
  if DiaFExisted then begin
    DiaFileState := CheckDiaFile;
    if not IsamOK then begin
      UnDo(3, IsamError);
      Exit;
    end;
  end
  else begin
    if UseDiaFile then begin
      DiaFileState := Raw;
    end
    else begin
      DiaFileState := Correct;
    end;
  end;

{--if we open a net fileblock with corrupted dialog file,
   then ensure that we are alone}
  {$IFNDEF NoNet}
  if (DiaFileState <> Correct) and UseLock then begin          {!!.42}
    if not IsamEnsureOnlyWS(DiaFile.Handle) then begin         {!!.42}
      UnDo(3, 10360);                                          {!!.42}
      Exit;                                                    {!!.42}
    end;                                                       {!!.42}
  end;                                                         {!!.42}
  {$ENDIF}

{--Set fileblock and net support data}
  with TIFBPtr^ do begin
    DiaF := DiaFile;
    ReadOnlyFB := ReadOnly;
    SaveFB := Save;
    NSP := Nil;
    {$IFNDEF NoNet}
    if Net then begin
      if not IsamGetMem(NSP, SizeOf(IsamNetSupport)) then begin{!!.52}
        UnDo(3, 10356);
        Exit;
      end;
      with NSP^ do begin
        Locked := False;
        ReadLocked := False;
          {-FlagSetReadLocked only set or read by Isam(Entry/Exit)Code}
        FlagSetReadLocked := false;                            {!!.51}
        AllStationsReadOnly := AllReadOnly;
        ReloadAll := False;
        SaveFileBlockRepaired := False;
        DiaLenM3 := DiaFileLen - 3;
        SetLen := FlagSetLen;
        SupNrOfWS := Pred(DiaLenM3 Div SetLen);                {!!.42}
        if RealNet and not AllStationsReadOnly then begin
          IsamDetermineLocalWSNr(DiaF.Handle, LocalWSNr,
              SupNrOfWS);                                      {!!.42}
          if not IsamOK then begin
            UnDo(4, IsamError);
            Exit;
          end;
        end
        else begin
          LocalWSNr := 0;
        end;
        with LockEntryRec do begin
          Count := 0;
          Next := Nil;
        end;
      end;
    end;
    {$ENDIF}
  end;

{--Repair fileblock and/or fill up flag sets if necessary}
  if DiaFileState <> Correct then begin
                                                            {del!!.42}
  {--Repair fileblock if necessary}
    if DiaFileState = ToRepair then begin
      IsamRepairFileBlock(TIFBPtr);
      if not IsamOK then begin
                                                            {del!!.42}
        UnDo(4, IsamError);
        Exit;
      end;
      DiaFileState := Raw;
    end;

  {--Fill up flag sets if necessary}
    {$IFNDEF NoNet}
    if Net then begin
      if DiaFileState = Raw then begin
        IsamFillFlagSet(TIFBPtr, FlagSet);
        IsamPutFlagSet(TIFBPtr, FlagSet);
        if IsamOK and Save and((not Net) or IsamNetEmu) then
            IsamFlushDOSDia(TIFBPtr, False);
        if not IsamOK then begin
                                                            {del!!.42}
          UnDo(4, IsamError);
          Exit;
        end;
      end;
    end;
                                                            {del!!.42}
    {$ENDIF}
  end;

{--Release lock on dialog file}
  {$IFNDEF NoNet}
  if UseLock then begin
    if not btfLockMgrRelLock(TIFBPtr^.DiaF.Handle, 3, 1) then begin
      UnDo(4, 10342);
      Exit;
    end;
  end;
  {$ENDIF}

{--Close opened and not used dialog file}
  if not UseDiaFile and DiaFOpened then begin
    IsamClose(DiaFile);
  end;

{--Delete dialog file if not necessary}
  if not(UseDiaFile or ReadOnly) and DiaFExisted then begin
    IsamDelete(DiaFile);
  end;

{--Final assigns of fileblock pointer}
  IFBPtr := TIFBPtr;
  IFBPtr^.ValidSign := IsamFBValidSign;
end;


procedure IsamCloseFileBlock(var IFBPtr : IsamFileBlockPtr);
var
  Ok,
  UseDiaFile : Boolean;
begin
  if not IsamFileBlockIsInOpenList(IFBPtr) then begin
    IsamOK := False;
    IsamError := 10080;
    Exit;
  end;
  with IFBPtr^ do begin
    {$IFNDEF NoNet}
    if NSP <> Nil then begin
      IsamUnLockAllRecs(IFBPtr);
      if not IsamOK then begin
        IsamError := 10323;
        Exit;
      end;
      with NSP^ do begin
        if Locked or ReadLocked then begin
          ISUnLockFileBlock(IFBPtr);
          if not IsamOK then begin
            IsamError := 10322;
            Exit;
          end;
        end;
      end;
    end;
    {$ENDIF}
    if ((NSP = Nil) or(IsamInitializedNet = NoNet)) and
       (not(SaveFB or ReadOnlyFB)) then begin
      {-Only real net and save fileblocks don't buffer}
      if NrOfKeys > 0 then begin
        IsamFlushPageInfo(IFBPtr, True);
        if not IsamOK then Exit;
      end;
      IsamPutInfoRec(IFBPtr, 0, False);
      if not IsamOK then Exit;
    end;

  {--Point of no return}

    Ok := True;
    if NSP <> Nil then begin
      {$IFNDEF NoNet}
      with NSP^ do begin
        if LocalWSNr <> 0 then begin
          IsamReleaseLocalWSNr(DiaF.Handle, LocalWSNr);
          Ok := Ok and IsamOK;
        end;
        UseDiaFile := not AllStationsReadOnly;
      end;
      FreeMem(NSP, SizeOf(IsamNetSupport));
      {$ENDIF}
    end
    else begin
      UseDiaFile := SaveFB;
    end;
    if UseDiaFile then begin
      IsamClose(DiaF);
      Ok := Ok and IsamOK;
    end;
  end;
  IsamCloseRawFileBlock(IFBPtr);
  IsamOK := Ok and IsamOK;
  if not IsamOK then
    IsamError := 10160;
end;


procedure IsamCloseAllFileBlocks;
var
  T1Ptr,
  TPtr   : IsamOpenFileBlockListPtr;
  TFBPtr : IsamFileBlockPtr;
  Err    : Integer;
begin
  TPtr := IsamOFBLPtr;
  Err := 0;
  while TPtr <> Nil do begin
    with TPtr^ do begin
      T1Ptr := Next;
      TFBPtr := OIFBPtr;
      {-Must(!) be copied for TP6 heap manager compatibility;
        the open fileblock list entry is removed, so the field OIFBPtr is
        no longer valid afterwards(overwritten by heap internal data)}
      IsamCloseFileBlock(TFBPtr);
      if Err = 0 then
        Err := IsamError;
      IsamClearOK;
      TPtr := T1Ptr;
    end;
  end;
  IsamError := Err;
  IsamOK := IsamError = 0;
end;


procedure IsamEntryCode(IFBPtr : IsamFileBlockPtr; Options : Word);
var
  KeyNr : Byte;
begin
  IsamClearOK;
  if not IsamIsInitialized then begin
    IsamOK := False;
    IsamError := 10455;
    Exit;
  end;
  if IsamIsCriticalActive then begin
    IsamOK := False;
    IsamError := 10446;
    Exit;
  end;
  if IFBPtr = Nil then begin
    IsamOK := False;
    IsamError := 10445;
    Exit;
  end;
  if IFBPtr <> Pointer(NotAFileBlockPtr) then begin
    with IFBPtr^ do begin
      if ValidSign <> IsamFBValidSign then begin
        IsamOK := False;
        IsamError := 10445;
        Exit;
      end;
      if ((Options and OptWriteRoutine) <> 0)                  {!!.41}
          and ReadOnlyFB then begin                            {!!.41}
        IsamOK := False;                                       {!!.41}
        IsamError := 10065;                                    {!!.41}
        Exit;                                                  {!!.41}
      end;                                                     {!!.41}
      {$IFNDEF NoNet}
      if (Options and OptCheckLock) <> 0 then begin
        if NSP <> Nil then begin
          if not NSP^.Locked then begin
            IsamOK := False;
            IsamError := 10398;
            Exit;
          end;
        end;
      end;
      {$ENDIF}
      if (Options and OptKeyRoutine) <> 0 then begin
        KeyNr := Options and $00FF;
        if (KeyNr < 1) or(KeyNr > NrOfKeys) then begin
          IsamOK := False;
          IsamError := 10164;
          Exit;
        end;
      end;
      {$IFNDEF NoNet}
      if NSP <> Nil then begin
        with NSP^ do begin
          FlagSetReadLocked := False;
          if (Options and OptReadPrefix) <> 0 then begin
            if not(Locked or ReadLocked or AllStationsReadOnly) then begin
              IsamReadNetPrefix(IFBPtr, False);
              if not IsamOK then begin
                if IsamError = 10140 then
                  IsamError := 10399;
                Exit;
              end;
              FlagSetReadLocked := True;
            end;
          end;
        end;
      end;
      {$ENDIF}
    end;
  end;
  {$IFDEF UseEMSHeap}                                          {!!.41}
  if EMSHeapIsUsed then begin
    if IsamOK then
      IsamIsCriticalActive := True;
  end;
  {$ENDIF}                                                     {!!.41}
end;


procedure IsamExitCode(IFBPtr : IsamFileBlockPtr);
begin
  if not IsamIsInitialized then Exit;
  if IsamError = 10445 then Exit;
  {$IFDEF UseEMSHeap}
  if EMSHeapIsUsed then begin
    if UserSaveEMSHandle <> 0 then begin
      {$IFDEF EMSDisturbance}
      OwnSaveEMSHandle := SaveEMSCtxt;
      {$ENDIF}
      RestoreEMSCtxt(UserSaveEMSHandle);
      UserSaveEMSHandle := 0;
    end;
  end;
  {$ENDIF}
  IsamIsCriticalActive := False;
  if IFBPtr <> Pointer(NotAFileBlockPtr) then begin
    if IsamError = 10140 then begin
      if IFBPtr^.ReadOnlyFB then
        IsamError := 10397;
    end;
    {$IFNDEF NoNet}
    if IFBPtr^.NSP <> Nil then begin
      if IsamError = 10140 then
        IsamError := 10397;
      with IFBPtr^.NSP^ do begin
        if FlagSetReadLocked then begin
          if not IsamUnLockMyFlagSet(IFBPtr) then begin
            IsamOK := False;
            IsamError := 10341;
            Exit;
          end;
          FlagSetReadLocked := False;
        end;
      end;
    end;
    {$ENDIF}
  end;
end;

