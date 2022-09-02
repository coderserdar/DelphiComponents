{********************************************************************}
{* ISAMNWRK.INC - network related routines                          *}
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

procedure IsamResetIRChangedSaveN(IFBPtr : IsamFileBlockPtr);
var
  I : Integer;
begin
  with IFBPtr^ do begin
    for I := 0 to NrOfKeys do begin
      DIDPtr^[I]^.IRChangedSaveN := False;
    end;
  end;
end;


function IsamAddLockToReadLock(IFBPtr : IsamFileBlockPtr) : Boolean;
var
  Dummy : Boolean;
begin
  IsamAddLockToReadLock := False;
  with IFBPtr^, NSP^ do begin
    if LocalWSNr > 0 then begin
      if not btfLockMgrAcqLock(DiaF.Handle,
                               3, LocalWSNr * SetLen,
                               0, 0) then Exit;
    end;
    if LocalWSNr < SupNrOfWS then begin
      if not btfLockMgrAcqLock(DiaF.Handle,
                               Succ(LocalWSNr) * SetLen + 3,
                               (SupNrOfWS - LocalWSNr) * SetLen,
                               0, 0) then begin
        if LocalWSNr > 0 then begin
          Dummy := btfLockMgrRelLock(DiaF.Handle, 3, LocalWSNr * SetLen);
        end;
        Exit;
      end;
    end;
  end;
  IsamAddLockToReadLock := True;
end;


procedure IsamSubLockFromReadLock(IFBPtr : IsamFileBlockPtr);
var
  Dummy : Boolean;
begin
  with IFBPtr^, NSP^ do begin
    if LocalWSNr < SupNrOfWS then begin
      Dummy := btfLockMgrRelLock(DiaF.Handle,
                                 Succ(LocalWSNr) * SetLen + 3,
                                 (SupNrOfWS - LocalWSNr) * SetLen);
    end;
    if LocalWSNr > 0 then begin
      Dummy := btfLockMgrRelLock(DiaF.Handle, 3, LocalWSNr * SetLen);
    end;
  end;
end;


procedure IsamIsInLockList(    FirstLEntryPtr : IsamLockEntryRecPtr;
                                 Ref            : LongInt;
                             var RefPtr         : LongPtr;
                                 SearchEnd      : Boolean;
                             var LEntryPtr      : IsamLockEntryRecPtr;
                             var PrevPtr        : IsamLockEntryRecPtr);
                                                               {!!.42}
var
  Found,
  Stop  : Boolean;
  I     : Integer;
begin
  RefPtr := Nil;
  LEntryPtr := FirstLEntryPtr;                                 {!!.42}
  PrevPtr := Nil;
  Found := False;
  repeat
    with LEntryPtr^ do begin
      I := 1;
      while(I <= Count) and not Found do begin
        Found := EntryArr [I] = Ref;
        if Found then begin
          RefPtr := @EntryArr [I];
        end
        else begin
          Inc(I);
        end;
      end;
    end;
    Stop := LEntryPtr^.Next = Nil;
    if not(Stop or Found) then begin
      PrevPtr := LEntryPtr;
      LEntryPtr := LEntryPtr^.Next;
    end;
  until Found or Stop;
  if Stop or not(Found and SearchEnd) then Exit;
  repeat
    Stop := LEntryPtr^.Next = Nil;
    if not Stop then begin
      PrevPtr := LEntryPtr;
      LEntryPtr := LEntryPtr^.Next;
    end;
  until Stop;
end;


function IsamAddToLockList(FirstLEntryPtr : IsamLockEntryRecPtr;
                             Ref            : LongInt) : Boolean;
                                                                  {!!.42}
var
  DummyPtr,
  ILERPtr  : IsamLockEntryRecPtr;
  RefPtr   : LongPtr;
begin
  IsamAddToLockList := False;
  IsamIsInLockList(FirstLEntryPtr, Ref, RefPtr, False, ILERPtr,
      DummyPtr);                                               {!!.42}
  if RefPtr <> Nil then Exit;
  if ILERPtr^.Count < MaxLockEntries then begin
    Inc(ILERPtr^.Count);
  end
  else begin
    if not IsamGetMem(ILERPtr^.Next, sizeof(IsamLockEntryRec)) then begin {!!.52}
      IsamOK := False;
      IsamError := 10337;
      Exit;
    end;
    ILERPtr := ILERPtr^.Next;
    ILERPtr^.Count := 1;
    ILERPtr^.Next := Nil;
  end;
  ILERPtr^.EntryArr [ILERPtr^.Count] := Ref;
  IsamAddToLockList := True;
end;


function IsamRemoveFromLockList(FirstLEntryPtr : IsamLockEntryRecPtr;
                                  Ref            : LongInt) : Boolean;
                                                               {!!.42}
var
  PrevPtr,
  ILERPtr : IsamLockEntryRecPtr;
  RefPtr  : LongPtr;
begin
  IsamRemoveFromLockList := False;
  IsamIsInLockList(FirstLEntryPtr, Ref, RefPtr, True, ILERPtr,
      PrevPtr);                                                {!!.42}
  if RefPtr = Nil then Exit;
  with ILERPtr^ do begin
    RefPtr^ := EntryArr [Count];
    Dec(Count);
    if(Count = 0) and(PrevPtr <> Nil) then begin
      PrevPtr^.Next := Nil;
      FreeMem(ILERPtr, SizeOf(IsamLockEntryRec));
    end;
  end;
  IsamRemoveFromLockList := True;
end;


procedure ISLockRec(IFBPtr : IsamFileBlockPtr; Ref : LongInt);
begin
  with IFBPtr^, DIDPtr^[0]^ do begin
    if not IsamAddToLockList(@IFBPtr^.NSP^.LockEntryRec, Ref)
        then Exit;                                                {!!.42}
      {-Either record is already locked or heap space doen't suffice}
    IsamOK := btfLockMgrAcqLock(DatF.Handle,
                                Ref * LenRec, SizeOf(LongInt),
                                IsamLockTimeOut, IsamDelayBetwLocks);
    if not IsamOK then begin
      if IsamRemoveFromLockList(@IFBPtr^.NSP^.LockEntryRec, Ref)
          then;                                                   {!!.42}
      IsamError := 10335;
    end;
  end;
end;


procedure ISUnLockRec(IFBPtr : IsamFileBlockPtr; Ref : LongInt);
begin
  with IFBPtr^, DIDPtr^[0]^ do begin
    if not IsamRemoveFromLockList(@IFBPtr^.NSP^.LockEntryRec, Ref)
        then Exit;                                                {!!.42}
      {-Already unlocked}
    IsamOK := btfLockMgrRelLock(DatF.Handle, Ref * LenRec, SizeOf(LongInt));
    if not IsamOK then
      IsamError := 10345;
      {-No attempt is made to mark the lock again, the system forgot this
        lock, what is an hard error}
  end;
end;


procedure IsamUnLockAllRecs(IFBPtr : IsamFileBlockPtr);
begin
  with IFBPtr^, NSP^, LockEntryRec do begin
    while Count > 0 do begin
      ISUnLockRec(IFBPtr, EntryArr [1]);
      if not IsamOK then Exit;
    end;
  end;
end;


function IsamLockAllFlagSets(IFBPtr : IsamFileBlockPtr) : Boolean;
begin
  IsamLockAllFlagSets :=
     btfLockMgrAcqLock(IFBPtr^.DiaF.Handle,
                       3,
                       IFBPtr^.NSP^.DiaLenM3,
                       IsamLockTimeOut * IsamFBLockTimeOutFactor,
                       IsamDelayBetwLocks);
end;


function IsamUnLockAllFlagSets(IFBPtr : IsamFileBlockPtr) : Boolean;
begin
  IsamUnLockAllFlagSets :=
     btfLockMgrRelLock(IFBPtr^.DiaF.Handle, 3, IFBPtr^.NSP^.DiaLenM3);
end;


procedure IsamLockMyFlagSet(IFBPtr : IsamFileBlockPtr);
begin
  with IFBPtr^, NSP^ do begin
    IsamOK := btfLockMgrAcqLock(DiaF.Handle,
                                LongInt(LocalWSNr) * LongInt(SetLen) + LongInt(3),
                                SetLen,
                                IsamLockTimeOut,
                                IsamDelayBetwLocks);
    if not IsamOK then
      IsamError := 10140;
  end;
end;


function IsamUnLockMyFlagSet(IFBPtr : IsamFileBlockPtr) : Boolean;
begin
  with IFBPtr^, NSP^ do begin
    IsamUnLockMyFlagSet :=
       btfLockMgrRelLock(DiaF.Handle,
                         LongInt(LocalWSNr) * LongInt(SetLen) + LongInt(3),
                         SetLen);
  end;
end;


procedure IsamFillFlagSet(IFBPtr  : IsamFileBlockPtr;
                      var FlagSet : IsamFlagSet);
begin
  FillChar(FlagSet, IFBPtr^.NSP^.SetLen, $FF);
end;


procedure IsamResetMyFlagSet(IFBPtr : IsamFileBlockPtr);
var
  FlagSet : IsamFlagSet;
begin
  with IFBPtr^, NSP^ do begin
    FillChar(FlagSet, SetLen, 0);
    IsamPutBlock(DiaF,
        LongInt(LocalWSNr) * LongInt(SetLen) + LongInt(3),
        SetLen, FlagSet);
  end;
end;


procedure IsamBuildFlagSet(IFBPtr  : IsamFileBlockPtr;
                       var FlagSet : IsamFlagSet   );
var
  I : Integer;
begin
  with IFBPtr^, NSP^ do begin
    if SaveFileBlockRepaired then begin
      SaveFileBlockRepaired := False;
      IsamFillFlagSet(IFBPtr, FlagSet);
      Exit;
    end;
    if DataBuffered then begin
      FlagSet := [Succ(NrOfKeys)];
      for I := 0 to NrOfKeys do begin
        with DIDPtr^[I]^ do begin
          if InfoRecChanged or IRChangedSaveN then
            FlagSet := FlagSet + [I];
        end;
      end;
    end
    else begin
      FlagSet := [];
    end;
  end;
end;


procedure IsamPutFlagSet(IFBPtr  : IsamFileBlockPtr;
                     var FlagSet : IsamFlagSet   );
{$IFDEF VER60}                                                 {!!.53}
type                                                           {!!.53}
  SmallInt = integer;                                          {!!.53}
{$ENDIF}                                                       {!!.53}
{$IFDEF VER70}                                                 {!!.53}
type                                                           {!!.53}
  SmallInt = integer;                                          {!!.53}
{$ENDIF}                                                       {!!.53}
{$IFDEF VER15}                                                 {!!.53}
type                                                           {!!.53}
  SmallInt = integer;                                          {!!.53}
{$ENDIF}                                                       {!!.53}
var
  BufRec : packed Record
    IST    : SmallInt; {Integer;}                              {!!.53}
    Buffer : Array [0..512] Of Byte;
  end;
  I,
  BufInd,
  MaxBuf,
  AktWs,
  RestWs     : Word;
  FSComp,
  EndOfPut,
  StartOfPut : Boolean;
  BufCont,
  DiaPos     : LongInt;
  FS         : Array [0..31] Of Byte Absolute FlagSet;
  {------}
  function FlagSetComplete : Boolean;
  var
    I : Integer;
  begin
    FlagSetComplete := False;
    for I := 0 to IFBPtr^.NrOfKeys do begin
      if not (I in FlagSet) then Exit;
    end;
    FlagSetComplete := True;
  end;
  {------}
begin
  with IFBPtr^, NSP^, BufRec do begin
    IST := 2000;       {net info}
    Buffer [0] := 0;   {net emulation}
    BufInd := SetLen;  {at least 1 Byte free}
    StartOfPut := True;
    EndOfPut := False;
    MaxBuf := SizeOf(Buffer) Div SetLen;
    AktWs := 1;
    DiaPos := 3;
    RestWs := Succ(SupNrOfWS);
    FSComp := FlagSetComplete;
    repeat
      if RestWs > MaxBuf then begin
        BufCont := LongInt(MaxBuf) * LongInt(SetLen);
      end
      else begin
        BufCont := LongInt(RestWs) * LongInt(SetLen);
        EndOfPut := True;
      end;
      if not FSComp then begin
        IsamGetBlock(DiaF, DiaPos, BufCont, Buffer);      
        if not IsamOK then Exit;
      end;
      while BufInd < ILI(BufCont).Lo do begin
        if AktWs = LocalWSNr then begin
          FillChar(Buffer [BufInd], SetLen, 0);
        end
        else begin
          for I := BufInd to Pred(BufInd + SetLen) do begin
            Buffer [I] := Buffer [I] or FS [I-BufInd];
          end;
        end;
        BufInd := BufInd + SetLen;
        Inc(AktWs);
      end;
      BufInd := 0;
      if StartOfPut then begin
        StartOfPut := False;
        IsamPutBlock(DiaF, 1, 2, IST);                         {!!.53}
        IsamPutBlock(DiaF, 3, BufCont, Buffer);                {!!.53}
      end
      else begin
        IsamPutBlock(DiaF, DiaPos, BufCont, Buffer);
      end;
      if EndOfPut or not IsamOK then Exit;
      DiaPos := DiaPos + BufCont;
      RestWs := RestWs - MaxBuf;
    until False;
  end;
end;


procedure IsamBuildAndPutFlagSetFlushNewData(IFBPtr  : IsamFileBlockPtr);
var
  FlagSet : IsamFlagSet;
  ModMark : Boolean;
begin
  with IFBPtr^ do begin
    if SaveFB then begin
      IsamFillFlagSet(IFBPtr, FlagSet);
      DIDPtr^[0]^.InfoRecChanged := False;
    end
    else begin
      if not DataBuffered then Exit;
      IsamBuildFlagSet(IFBPtr, FlagSet);
      IsamFlushPageInfo(IFBPtr, False);
      if not IsamOK then Exit;
      if DIDPtr^[0]^.InfoRecChanged or DIDPtr^[0]^.IRChangedSaveN then begin
        IsamPutInfoRec(IFBPtr, 0, False);
        if not IsamOK then Exit;
        DIDPtr^[0]^.InfoRecChanged := False;
      end
      else begin
        ModMark := False;
        IsamPutBlock(DatF, 20, 1, ModMark);
        if not IsamOK then Exit;
      end;
    end;
  end;
  IsamPutFlagSet(IFBPtr, FlagSet);
  if (IFBPtr^.NSP = Nil) or IsamNetEmu then begin              {!!.56}
    IsamFlushDOSDia(IFBPtr, True);                             {!!.56}
  end;                                                         {!!.56}
end;

procedure IsamGetFlagSet(var IFBPtr   : IsamFileBlockPtr;
                         var FlagSet  : IsamFlagSet;
                         var Valid    : Boolean;
                             ReadOnce : Boolean);
var
  Buffer : packed Record
    Dummy : Byte;
    Value : Word;
    Buf   : Array [0..508] Of Byte;
  end;
  NrToRead : Word;
begin
  with IFBPtr^, NSP^ do begin
    if false {ReadOnce} then begin
      NrToRead := Succ(LongInt(LocalWSNr)) * LongInt(SetLen)
          + LongInt(3);
      if NrToRead <= SizeOf(Buffer) then begin
        IsamGetBlock(DiaF, 0, NrToRead, Buffer);          
        if not IsamOK then Exit;
        Valid := Buffer.Value = 2000;
        if not Valid then Exit;
        Move(Buffer.Buf [LongInt(LocalWSNr) * LongInt(SetLen)],
            FlagSet, SetLen);
      end
      else begin
        ReadOnce := False;
      end;
    end;
    if true {not ReadOnce} then begin
      IsamGetBlock(DiaF, 0, 3, Buffer);                    
      if not IsamOK then Exit;
      Valid := Buffer.Value = 2000;
      if not Valid then Exit;
      IsamGetBlock(DiaF, LongInt(LocalWSNr) * LongInt(SetLen)
          + LongInt(3), SetLen, FlagSet);                    
    end;
  end;
end;


procedure IsamLockAndGetFlagSet(IFBPtr  : IsamFileBlockPtr;
                            var FlagSet : IsamFlagSet;
                            var Valid   : Boolean    );
begin
  IsamLockMyFlagSet(IFBPtr);
  if not IsamOK then Exit;
  IsamGetFlagSet(IFBPtr, FlagSet, Valid, False);
  if not IsamOK then                                           {!!.54}
    if not IsamUnLockMyFlagSet(IFBPtr) then                    {!!.54}
      {do nothing};                                            {!!.54}
end;


procedure IsamReactOnFlagSet(IFBPtr  : IsamFileBlockPtr;
                         var FlagSet : IsamFlagSet);
var
  Dummy : Boolean;
  I     : Integer;
begin
  with IFBPtr^ do begin
    if not(Succ(NrOfKeys) in FlagSet) then Exit;
    if not ReadOnlyFB then
      IsamResetMyFlagSet(IFBPtr);
    if not IsamOK then begin
      NSP^.ReloadAll := True;
      Exit;
    end;
    for I := 0 to NrOfKeys do begin
      if I in FlagSet then begin
        IsamGetInfoRec(IFBPtr, I, Dummy);
        if not IsamOK then begin
          NSP^.ReloadAll := True;
          Exit;
        end;
        if I > 0 then begin
          with DIDPtr^[I]^ do begin
            SequentialOK := False;
            if NumRec > MaxPages then begin
              MaxPages := NumRec;
            end;
          end;
          IsamDestroyPagesOfKeyNr(IFBPtr, I);
        end;
      end;
    end;
  end;
end;


procedure IsamReadNetPrefix(IFBPtr : IsamFileBlockPtr;
                            Locked : Boolean  );
var
  Dummy,
  Valid   : Boolean;
  FlagSet : IsamFlagSet;
  {------}
  procedure UnDo(Error : Integer);
  begin
    if not Locked then begin
      Dummy := IsamUnLockMyFlagSet(IFBPtr);
      IsamSubLockFromReadLock(IFBPtr);
    end;
    IsamOK := False;
    IsamError := Error;
  end;
  {------}
begin
  if Locked then begin
    IsamGetFlagSet(IFBPtr, FlagSet, Valid, True);
  end
  else begin
    IsamLockAndGetFlagSet(IFBPtr, FlagSet, Valid);
  end;
  if IsamError = 10140 then begin
    UnDo(IsamError);
    Exit;
  end;
  if not (Valid and IsamOK) then begin
    IsamClearOK;
    if not Locked then begin
      if not IsamAddLockToReadLock(IFBPtr) then begin
        UnDo(10330);
        Exit;
      end;
    end;
    IsamRepairFileBlock(IFBPtr);
    if not IsamOK then begin
      UnDo(IsamError);
      Exit;
    end;
    IsamFillFlagSet(IFBPtr, FlagSet);
    IsamReactOnFlagSet(IFBPtr, FlagSet);
    if not IsamOK then begin
      UnDo(IsamError);
      Exit;
    end;
    if Locked and IFBPtr^.SaveFB then begin
      IFBPtr^.NSP^.SaveFileBlockRepaired := True;
    end
    else begin
      IsamPutFlagSet(IFBPtr, FlagSet);
      if not IsamOK then begin
        UnDo(IsamError);
        Exit;
      end;
    end;
    if not Locked then
      IsamSubLockFromReadLock(IFBPtr);
    Exit;
  end;
  with IFBPtr^.NSP^ do begin
    if ReloadAll then begin
      IsamFillFlagSet(IFBPtr, FlagSet);
      ReloadAll := False;
    end;
  end;
  IsamReactOnFlagSet(IFBPtr, FlagSet);
  if not IsamOK then                                           {!!.54}
    UnDo(IsamError);                                           {!!.54}
end;


procedure ISReadLockFileBlock(IFBPtr : IsamFileBlockPtr);
begin
  if IFBPtr^.NSP <> Nil then begin
    with IFBPtr^, NSP^ do begin
      if not AllStationsReadOnly then begin
        if not ReadLocked then begin
          if Locked then begin
            if not ReadOnlyFB then begin
              IsamBuildAndPutFlagSetFlushNewData(IFBPtr);
            end;
            if IsamOK then begin
              IsamOK := IsamUnLockAllFlagSets(IFBPtr);
              if not IsamOK then begin
                IsamError := 10340;
              end
              else begin
                Locked := False;
                if not IsamNetEmu then
                  DataBuffered := False;
              end;
            end;
          end;
          if IsamOK and not Locked then begin
            IsamReadNetPrefix(IFBPtr, False);
            if IsamOK then begin
              ReadLocked := True;
            end
            else begin
              if IsamError = 10140 then
                IsamError := 10332;
            end;
          end;
        end;
      end;
    end;
  end;
end;


procedure ISLockFileBlock(IFBPtr : IsamFileBlockPtr);
var
  Dummy : Boolean;
begin
  if IFBPtr^.NSP <> Nil then begin
    with IFBPtr^, NSP^ do begin
      if not AllStationsReadOnly then begin
        if not Locked then begin
          if ReadLocked then begin
            if IsamUnLockMyFlagSet(IFBPtr) then begin
              ReadLocked := False;
            end
            else begin
              IsamOK := False;
              IsamError := 10340;
            end;
          end;
          if IsamOK and not ReadLocked then begin
            if IsamLockAllFlagSets(IFBPtr) then begin
              IsamReadNetPrefix(IFBPtr, True);
              if not IsamOK then begin
                Dummy := IsamUnLockAllFlagSets(IFBPtr);
              end;
            end
            else begin
              IsamOK := False;
              IsamError := 10330;
            end;
          end;
          if IsamOK then begin
            if SaveFB then begin
              IsamReduceDiaFile(IFBPtr);
              if not IsamOK then begin
                Dummy := IsamUnLockAllFlagSets(IFBPtr);
              end;
            end;
            if IsamOK then begin
              Locked := True;
              IsamResetIRChangedSaveN(IFBPtr);
            end;
          end;
        end;
      end;
    end;
  end;
end;


procedure ISUnLockFileBlock(IFBPtr : IsamFileBlockPtr);
begin
  if IFBPtr^.NSP <> Nil then begin
    with IFBPtr^, NSP^ do begin
      if not AllStationsReadOnly then begin
        if not Locked then begin
          if ReadLocked then begin
            if IsamUnLockMyFlagSet(IFBPtr) then begin
              ReadLocked := False;
            end
            else begin
              IsamOK := False;
              IsamError := 10340;
            end;
          end;
        end
        else begin
          if not ReadOnlyFB then begin
            IsamBuildAndPutFlagSetFlushNewData(IFBPtr);
          end;
          if IsamOK then begin
            if IsamUnLockAllFlagSets(IFBPtr) then begin
              Locked := False;
              if not IsamNetEmu then
                DataBuffered := False;
            end
            else begin
              IsamOK := False;
              IsamError := 10340;
            end;
          end;
        end;
      end;
    end;
  end;
end;


procedure ISUnLockAllOpenFileBlocks;
var
  TPtr : IsamOpenFileBlockListPtr;
  Err  : Integer;
  OK   : Boolean;
begin
  OK := True;
  TPtr := IsamOFBLPtr;
  while TPtr <> Nil do begin
    ISUnLockFileBlock(TPtr^.OIFBPtr);
    if not IsamOK then
      Err := IsamError;
    OK := OK and IsamOK;
    TPtr := TPtr^.Next;
  end;
  if not OK then begin
    IsamOK := False;
    IsamError := Err;
  end;
end;


procedure IsamReleaseLocalWSNr(FHandle, LocalWSNr : Word);
const
  StartPos = $7FFFFFFF;
begin
  if not btfLockMgrRelLock(FHandle, StartPos - LocalWSNr, 1) then begin
    IsamOK := False;
    IsamError := 10341;
  end;
end;


procedure IsamDetermineLocalWSNr(    FHandle   : Word;
                                   var LocalWSNr : Word;
                                       SupNrOfWS : Word);        {!!.42}
const
  StartPos = $7FFFFFFF;
var
  FirstPos : Word;
begin
  if SupNrOfWS <> 0 then begin                                 {!!.42}
    Randomize;
    FirstPos := Succ(Random(SupNrOfWS));                       {!!.42}
    LocalWSNr := FirstPos;
    repeat
      if btfLockMgrAcqLock(FHandle, StartPos - LocalWSNr, 1, 0, 0) then Exit;
      Dec(LocalWSNr);
      if LocalWSNr = 0 then
        LocalWSNr := SupNrOfWS;                                {!!.42}
    until LocalWSNr = FirstPos;
  end;
  IsamOK := False;
  IsamError := 10306;
end;


function IsamEnsureOnlyWS(FHandle : Word) : Boolean;           {!!.42}
const
  StartPos = $7FFFFFFF;
  MaxWS    = $FFFE;
var
  OnlyWS : Boolean;
begin
  OnlyWS := btfLockMgrAcqLock(FHandle, StartPos - MaxWS, Succ(MaxWS), 0, 0);
  if OnlyWS then
    if btfLockMgrRelLock(FHandle, StartPos - MaxWS, Succ(MaxWS)) then
      {do nothing};
  IsamEnsureOnlyWS := OnlyWS;
end;

