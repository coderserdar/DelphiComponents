{********************************************************************}
{* FILER.INC - high level B-Tree Filer routines                     *}
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

{$IFDEF UseWindowsInit}
function BTInitIsam(ExpectedNet : NetSupportType;        {!!.52}
                    Pages       : integer) : integer;
var
  Dummy  : Boolean;
begin
  BTInitIsam := 0;
  if IsamIsInitialized then begin
    IsamOK := False;
    IsamError := 10450;
    Exit;
  end;
  if (Pages < MaxHeight) then begin
    IsamOK := False;
    IsamError := 10451;
    Exit;
  end;
  ISInitIsam;
  {$IFDEF NoNet}
  ExpectedNet := NoNet;
  IsamNetEmu := False;
  {$ELSE}
  if not IsamInitNet(ExpectedNet) then begin
    if IsamOK then begin
      {-do not overwrite any error code}
      IsamOK := False;
      IsamError := 10310;
    end;
    Exit;
  end;
  IsamNetEmu := ExpectedNet = NoNet;
  {$ENDIF}
  IsamInitializedNet := ExpectedNet;
  BTInitIsam := IsamGetPageBuffer(Pages);
  if not IsamOK then begin
    {$IFNDEF NoNet}
    Dummy := IsamDoneNet;
    {$ENDIF}
    Exit;
  end;
  EMSHeapIsUsed := False;
  IsamIsInitialized := True;
end;
{$ELSE}
function BTInitIsam(ExpectedNet      : NetSupportType;
                      Free             : LongInt;
                      NrOfEMSTreePages : Word) : LongInt;
  {-Initializes B-Tree Isam;
    high word of LResult: pages on EMS heap;
    low word: pages on normal heap}
var
  Dummy  : Boolean;
  LResult : LongInt;                                          {!!.51}
begin
  if IsamIsInitialized then begin
    IsamOK := False;
    IsamError := 10450;
    Exit;
  end;
  ISInitIsam;
  BTInitIsam := 0;
  {$IFDEF NoNet}
  ExpectedNet := NoNet;
  IsamNetEmu := False;
  {$ELSE}
  if not IsamInitNet(ExpectedNet) then begin
    if IsamOK then begin
      {-do not overwrite any error code}
      IsamOK := False;
      IsamError := 10310;
    end;
    Exit;
  end;
  IsamNetEmu := ExpectedNet = NoNet;
  {$ENDIF}
  IsamInitializedNet := ExpectedNet;
  {$IFDEF UseEMSHeap}
  if not EMSHeapInitialized then begin
    NrOfEMSTreePages := 0;
  end;
  {$ELSE}
  NrOfEMSTreePages := 0;
  {$ENDIF}
  {$IFDEF UseEMSHeap}
  if NrOfEMSTreePages > 0 then begin
    if SizeOf (IsamPageEntry) > 16384 then begin
    {$IFNDEF NoNet}
      Dummy := IsamDoneNet;
    {$ENDIF}
      IsamOK := False;
      IsamError := 10435;
      Exit;
    end;
    UserSaveEMSHandle := SaveEMSCtxt;
  end;
  {$ENDIF}
  if Free < 0 then
    Free := 0;
  LResult := IsamGetPageBuffer(Free, NrOfEMSTreePages);    {!!.51}
  BTInitIsam := LResult;                                      {!!.51}
  if not IsamOK then begin
    {$IFDEF UseEMSHeap}
    if NrOfEMSTreePages > 0 then begin
      RestoreEMSCtxt(UserSaveEMSHandle);
      UserSaveEMSHandle := 0;
    end;
    {$ENDIF}
    {$IFNDEF NoNet}
    Dummy := IsamDoneNet;
    {$ENDIF}
    Exit;
  end;
  {$IFDEF UseEMSHeap}
  EMSHeapIsUsed := ILI (LResult).Hi <> 0;                     {!!.51}
  {$IFDEF EMSDisturbance}
  if EMSHeapIsUsed then
    OwnSaveEMSHandle := SaveEMSCtxt;
  {$ENDIF}
  if NrOfEMSTreePages > 0 then begin
    RestoreEMSCtxt(UserSaveEMSHandle);
    UserSaveEMSHandle := 0;
  end;
  {$ELSE}
  EMSHeapIsUsed := False;
  {$ENDIF}
  IsamIsInitialized := True;
end;
{$ENDIF}


procedure BTExitIsam;
  {-Exits use of B-Tree Isam}
begin
  IsamClearOK;
  if not IsamIsInitialized then begin
    IsamOK := False;
    IsamError := 10455;
    Exit;
  end;
  IsamCloseAllFileBlocks;
  if not IsamOK then Exit;
  {$IFDEF UseEMSHeap}
  if EMSHeapIsUsed then begin
    UserSaveEMSHandle := SaveEMSCtxt;
    {$IFDEF EMSDisturbance}
    RestoreEMSCtxt(OwnSaveEMSHandle);
    {$ENDIF}
  end;
  {$ENDIF}
  IsamIsInitialized := False;
  IsamReleasePageBuffer;
  {$IFNDEF NoNet}
  if not IsamDoneNet then begin
    IsamOK := False;
    IsamError := 10315;
  end;
  {$ENDIF}
  {$IFDEF UseEMSHeap}
  if EMSHeapIsUsed then
    RestoreEMSCtxt(UserSaveEMSHandle);
  {$ENDIF}
end;


function BTIsamErrorClass : Integer;
  {-Evaluates IsamError in error classes 0..4}
begin
  case IsamError of
    0 : begin
          BTIsamErrorClass := 0;
        end;
    9903,
    10200..10299,
    10410 : begin
              BTIsamErrorClass := 1;
            end;
    9900,
    10065,
    10110,
    10306,
    10330,
    10332,
    10335,
    10355,
    10360,                                                         {!!.42}
    10390,                                                         {!!.42}
    10397,
    10399 : begin
              BTIsamErrorClass := 2;
            end;
    10001..10009 : begin
                     BTIsamErrorClass := 3;
                   end;
  else
    BTIsamErrorClass := 4;
  end; {case}
end;


function BTNoNetCompiled : Boolean;
  {-Returns True, if B-Tree Isam was compiled without network support}
begin
  IsamEntryCode(Pointer (NotAFileBlockPtr), NoOptions);
  {$IFDEF NoNet}
  BTNoNetCompiled := True;
  {$ELSE}
  BTNoNetCompiled := False;
  {$ENDIF}
  IsamExitCode(Pointer (NotAFileBlockPtr));
end;


function BTPeekNoNetCompiled : Boolean;
  {-Just like the routine without "Peek", but not destroying the previous
    error on success}
var
  ErrSt : ErrStatSaveRec;
begin
  SaveErrorStat(ErrSt);
  BTPeekNoNetCompiled := BTNoNetCompiled;
  if IsamOK then
    RestoreErrorStat(ErrSt);
end;


function BTNetSupported : NetSupportType;
  {-Returns the currently supported network}
begin
  IsamEntryCode(Pointer (NotAFileBlockPtr), NoOptions);
  BTNetSupported := IsamInitializedNet;
  IsamExitCode(Pointer (NotAFileBlockPtr));
end;


function BTPeekNetSupported : NetSupportType;
  {-Just like the routine without "Peek", but not destroying the previous
    error on success}
var
  ErrSt : ErrStatSaveRec;
begin
  SaveErrorStat(ErrSt);
  BTPeekNetSupported := BTNetSupported;
  if IsamOK then
    RestoreErrorStat(ErrSt);
end;


function BTFileBlockIsOpen(IFBPtr : IsamFileBlockPtr) : Boolean;
  {-Returns True, if the fileblock is currently open}
begin
  IsamEntryCode(Pointer (NotAFileBlockPtr), NoOptions);
  BTFileBlockIsOpen := IsamFileBlockIsInOpenList(IFBPtr);
  IsamExitCode(Pointer (NotAFileBlockPtr));
end;


function BTPeekFileBlockIsOpen(IFBPtr : IsamFileBlockPtr) : Boolean;
  {-Just like the routine without "Peek", but not destroying the previous
    error on success}
var
  ErrSt : ErrStatSaveRec;
begin
  SaveErrorStat(ErrSt);
  BTPeekFileBlockIsOpen := BTFileBlockIsOpen(IFBPtr);
  if IsamOK then
    RestoreErrorStat(ErrSt);
end;


function BTIsNetFileBlock(IFBPtr : IsamFileBlockPtr) : Boolean;
  {-Returns True, if the fileblock is a net fileblock}
begin
  IsamEntryCode(IFBPtr, NoOptions);
  {$IFDEF NoNet}
  BTIsNetFileBlock := False;
  {$ELSE}
  BTIsNetFileBlock := IFBPtr^.NSP <> Nil;
  {$ENDIF}
  IsamExitCode(IFBPtr);
end;


function BTPeekIsNetFileBlock(IFBPtr : IsamFileBlockPtr) : Boolean;
  {-Just like the routine without "Peek", but not destroying the previous
    error on success}
var
  ErrSt : ErrStatSaveRec;
begin
  SaveErrorStat(ErrSt);
  BTPeekIsNetFileBlock := BTIsNetFileBlock(IFBPtr);
  if IsamOK then
    RestoreErrorStat(ErrSt);
end;


function BTFileBlockIsLocked(IFBPtr : IsamFileBlockPtr) : Boolean;
  {-Returns True, if the fileblock is currently locked}
begin
  IsamEntryCode(IFBPtr, NoOptions);
  BTFileBlockIsLocked := False;
  {$IFNDEF NoNet}
  if IFBPtr^.NSP <> Nil then
    BTFileBlockIsLocked := IFBPtr^.NSP^.Locked;
  {$ENDIF}
  IsamExitCode(IFBPtr);
end;


function BTPeekFileBlockIsLocked(IFBPtr : IsamFileBlockPtr) : Boolean;
  {-Just like the routine without "Peek", but not destroying the previous
    error on success}
var
  ErrSt : ErrStatSaveRec;
begin
  SaveErrorStat(ErrSt);
  BTPeekFileBlockIsLocked := BTFileBlockIsLocked(IFBPtr);
  if IsamOK then
    RestoreErrorStat(ErrSt);
end;


function BTFileBlockIsReadLocked(IFBPtr : IsamFileBlockPtr) : Boolean;
  {-Returns True, if the fileblock is currently readlocked}
begin
  IsamEntryCode(IFBPtr, NoOptions);
  BTFileBlockIsReadLocked := False;
  {$IFNDEF NoNet}
  if IFBPtr^.NSP <> Nil then
      BTFileBlockIsReadLocked := IFBPtr^.NSP^.ReadLocked;
  {$ENDIF}
  IsamExitCode(IFBPtr);
end;


function BTPeekFileBlockIsReadLocked(IFBPtr : IsamFileBlockPtr) : Boolean;
  {-Just like the routine without "Peek", but not destroying the previous
    error on success}
var
  ErrSt : ErrStatSaveRec;
begin
  SaveErrorStat(ErrSt);
  BTPeekFileBlockIsReadLocked := BTFileBlockIsReadLocked(IFBPtr);
  if IsamOK then
    RestoreErrorStat(ErrSt);
end;


procedure BTReadLockFileBlock(IFBPtr : IsamFileBlockPtr);
  {-Readlocks the fileblock IFBPtr^}
begin
  IsamEntryCode(IFBPtr, NoOptions);
  {$IFNDEF NoNet}
  if IsamOK then
    ISReadLockFileBlock(IFBPtr);
  {$ENDIF}
  IsamExitCode(IFBPtr);
end;


procedure BTLockFileBlock(IFBPtr : IsamFileBlockPtr);
  {-Locks the fileblock IFBPtr^}
begin
  IsamEntryCode(IFBPtr, NoOptions);
  {$IFNDEF NoNet}
  if IsamOK then
    ISLockFileBlock(IFBPtr);
  {$ENDIF}
  IsamExitCode(IFBPtr);
end;


procedure BTUnLockFileBlock(IFBPtr : IsamFileBlockPtr);
  {-Unlocks the fileblock IFBPtr^}
begin
  IsamEntryCode(IFBPtr, NoOptions);
  {$IFNDEF NoNet}
  if IsamOK then
    ISUnLockFileBlock(IFBPtr);
  {$ENDIF}
  IsamExitCode(IFBPtr);
end;


procedure BTUnLockAllOpenFileBlocks;
  {-Unlocks all open fileblocks}
begin
  IsamEntryCode(Pointer (NotAFileBlockPtr), NoOptions);
  {$IFNDEF NoNet}
  if IsamOK then
    ISUnLockAllOpenFileBlocks;
  {$ENDIF}
  IsamExitCode(Pointer (NotAFileBlockPtr));
end;


procedure BTReadLockAllOpenFileBlocks;
  {-Readlocks all open fileblocks}
var
  TPtr : IsamOpenFileBlockListPtr;
  Err  : Integer;
begin
  IsamEntryCode(Pointer (NotAFileBlockPtr), NoOptions);
  {$IFNDEF NoNet}
  if IsamOK then begin
    TPtr := IsamOFBLPtr;
    while (TPtr <> Nil) and IsamOK do begin
      ISReadLockFileBlock(TPtr^.OIFBPtr);
      if not IsamOK then begin
        Err := IsamError;
        ISUnLockAllOpenFileBlocks;
        IsamOK := False;
        IsamError := Err;
      end;
      TPtr := TPtr^.Next;
    end;
  end;
  {$ENDIF}
  IsamExitCode(Pointer (NotAFileBlockPtr));
end;


procedure BTLockAllOpenFileBlocks;
  {-Locks all open fileblocks}
var
  TPtr : IsamOpenFileBlockListPtr;
  Err  : Integer;
begin
  IsamEntryCode(Pointer (NotAFileBlockPtr), NoOptions);
  {$IFNDEF NoNet}
  if IsamOK then begin
    TPtr := IsamOFBLPtr;
    while (TPtr <> Nil) and IsamOK do begin
      ISLockFileBlock(TPtr^.OIFBPtr);
      if not IsamOK then begin
        Err := IsamError;
        ISUnLockAllOpenFileBlocks;
        IsamOK := False;
        IsamError := Err;
      end;
      TPtr := TPtr^.Next;
    end;
  end;
  {$ENDIF}
  IsamExitCode(Pointer (NotAFileBlockPtr));
end;


procedure BTGetRecordInfo(    IFBPtr     : IsamFileBlockPtr;
                                Ref        : LongInt;
                            var Start, Len : LongInt;
                            var Handle     : IsamHandle);
  {-Returns information about record Ref}
begin
  IsamEntryCode(IFBPtr, NoOptions);
  if IsamOK then begin
    with IFBPtr^, DIDPtr^[0]^ do begin
      Start := Ref * LenRec;
      Len := LenRec;
      Handle := DatF.Handle;
    end;
  end;
  IsamExitCode(IFBPtr);
end;


procedure BTPeekGetRecordInfo(    IFBPtr     : IsamFileBlockPtr;
                                    Ref        : LongInt;
                                var Start, Len : LongInt;
                                var Handle     : IsamHandle);
  {-Just like the routine without "Peek", but not destroying the previous
    error on success}
var
  ErrSt : ErrStatSaveRec;
begin
  SaveErrorStat(ErrSt);
  BTGetRecordInfo(IFBPtr, Ref, Start, Len, Handle);
  if IsamOK then
    RestoreErrorStat(ErrSt);
end;


procedure BTLockRec(IFBPtr : IsamFileBlockPtr; Ref : LongInt);
  {-Locks record Ref}
begin
  IsamEntryCode(IFBPtr, NoOptions);
  {$IFNDEF NoNet}
  if IsamOK and (IFBPtr^.NSP <> Nil) then
    ISLockRec(IFBPtr, Ref);
  {$ENDIF}
  IsamExitCode(IFBPtr);
end;


procedure BTUnLockRec(IFBPtr : IsamFileBlockPtr; Ref : LongInt);
  {-Unlocks record Ref}
begin
  IsamEntryCode(IFBPtr, NoOptions);
  {$IFNDEF NoNet}
  if IsamOK and (IFBPtr^.NSP <> Nil) then
    ISUnLockRec(IFBPtr, Ref);
  {$ENDIF}
  IsamExitCode(IFBPtr);
end;


procedure BTUnLockAllRecs(IFBPtr : IsamFileBlockPtr);
  {-Unlocks all locked records of IFBPtr^}
begin
  IsamEntryCode(IFBPtr, NoOptions);
  {$IFNDEF NoNet}
  if IsamOK and (IFBPtr^.NSP <> Nil) then
    IsamUnLockAllRecs(IFBPtr);
  {$ENDIF}
  IsamExitCode(IFBPtr);
end;


function BTRecIsLocked(IFBPtr : IsamFileBlockPtr; Ref : LongInt)
                                                                  : Boolean;
  {-Returns whether the record Ref is locked}
var
  RefPtr    : LongPtr;
  Dummy1Ptr,
  Dummy2Ptr : IsamLockEntryRecPtr;
begin
  IsamEntryCode(IFBPtr, NoOptions);
  BTRecIsLocked  := False;
  {$IFNDEF NoNet}
  if IsamOK and (IFBPtr^.NSP <> Nil) then begin
    IsamIsInLockList(@IFBPtr^.NSP^.LockEntryRec, Ref, RefPtr,
        False, Dummy1Ptr, Dummy2Ptr);                            {!!.42}
    BTRecIsLocked := RefPtr <> Nil;
  end;
  {$ENDIF}
  IsamExitCode(IFBPtr);
end;


function BTPeekRecIsLocked(IFBPtr : IsamFileBlockPtr; Ref : LongInt)
                                                                  : Boolean;
  {-Just like the routine without "Peek", but not destroying the previous
    error on success}
var
  ErrSt : ErrStatSaveRec;
begin
  SaveErrorStat(ErrSt);
  BTPeekRecIsLocked := BTRecIsLocked(IFBPtr, Ref);
  if IsamOK then
    RestoreErrorStat(ErrSt);
end;


function BTaRecIsLocked(IFBPtr : IsamFileBlockPtr) : Boolean;
  {-Returns whether a record is locked}

begin
  IsamEntryCode(IFBPtr, NoOptions);
  BTaRecIsLocked  := False;
  {$IFNDEF NoNet}
  if IsamOK and (IFBPtr^.NSP <> Nil) then begin
    BTaRecIsLocked  := IFBPtr^.NSP^.LockEntryRec.Count > 0;
  end;
  {$ENDIF}
  IsamExitCode(IFBPtr);
end;


function BTPeekaRecIsLocked(IFBPtr : IsamFileBlockPtr) : Boolean;
  {-Just like the routine without "Peek", but not destroying the previous
    error on success}
var
  ErrSt : ErrStatSaveRec;
begin
  SaveErrorStat(ErrSt);
  BTPeekaRecIsLocked := BTaRecIsLocked(IFBPtr);
  if IsamOK then
    RestoreErrorStat(ErrSt);
end;


procedure BTInformTTSAbortSuccessful(IFBPtr : IsamFileBlockPtr);
  {-Resets all internal data to nothing buffered and no lock at all}
begin
  IsamEntryCode(IFBPtr, NoOptions);
  {$IFNDEF NoNet}
  if IsamOK and (IFBPtr^.NSP <> Nil) then begin
    with IFBPtr^.NSP^ do begin
      if Locked then begin
        if IsamUnLockAllFlagSets(IFBPtr) then;
        Locked := False;
      end;
      if ReadLocked then begin
        if IsamUnLockMyFlagSet(IFBPtr) then;
        ReadLocked := False;
      end;
      while LockEntryRec.Count > 0 do begin
        ISUnLockRec(IFBPtr, LockEntryRec.EntryArr [1]);
        IsamClearOK;
      end;
      ReloadAll := True;
    end;
  end;
  {$ENDIF}
  IsamExitCode(IFBPtr);
end;


function BTSetDosRetry(NrOfRetries, WaitTime : Integer) : Boolean;
  {-Sets number of retries and delay time between on locks and accesses
    in a MicroSoft compatible network}
begin
  IsamEntryCode(Pointer (NotAFileBlockPtr), NoOptions);
  {$IFDEF NoNet}
  BTSetDosRetry := IsamOK;
  {$ELSE}
  BTSetDosRetry := IsamSetDosRetry(NrOfRetries, WaitTime);
  {$ENDIF}
  IsamExitCode(Pointer (NotAFileBlockPtr));
end;


procedure BTForceWritingMark(FFM : Boolean);
  {-Forces to write the "modify mark" to disk}
begin
  IsamEntryCode(Pointer (NotAFileBlockPtr), NoOptions);
  IsamForceFlushOfMark := FFM;
  IsamExitCode(Pointer (NotAFileBlockPtr));
end;


procedure BTForceNetBufferWriteThrough(DoIt : Boolean);
  {-with DoIt = True the save mode tries to suppress all buffering in a
    network or multitasking environment}
begin
  IsamEntryCode(Pointer (NotAFileBlockPtr), NoOptions);
  IsamNetEmu := DoIt;
  IsamExitCode(Pointer (NotAFileBlockPtr));
end;


procedure BTGetRec(IFBPtr  : IsamFileBlockPtr;
                   RefNr   : LongInt;
               var Dest;
                   ISOLock : Boolean);
  {-Gets the specified record from the given reference}
var
  Options : Word;
begin
  if ISOLock then begin
    Options := NoOptions;
  end
  else begin
    Options := OptReadPrefix;
  end;
  IsamEntryCode(IFBPtr, Options);
  if IsamOK then
    IsamGetRec(IFBPtr, RefNr, Dest);
  if IsamOK then begin                                         {!!.50}
    IFBPtr^.CharConvProc(@Dest, IFBPtr^.DIDPtr^[0]^.LenRec,    {!!.50}
        True, IFBPtr^.CCHookPtr);                              {!!.50}
  end;                                                         {!!.50}
  IsamExitCode(IFBPtr);
end;


procedure BTGetRecReadOnly(IFBPtr  : IsamFileBlockPtr;
                           RefNr   : LongInt;
                       var Dest             );
  {-Gets the specified record from the given reference even if it's locked}
begin
  IsamEntryCode(IFBPtr, NoOptions);
  if IsamOK then
    IsamGetRecReadOnly(IFBPtr, RefNr, Dest);
  if IsamOK or (IsamError = 10205) then begin                  {!!.50}
    IFBPtr^.CharConvProc(@Dest, IFBPtr^.DIDPtr^[0]^.LenRec,    {!!.50}
        True, IFBPtr^.CCHookPtr);                              {!!.50}
  end;                                                         {!!.50}
  IsamExitCode(IFBPtr);
end;


procedure BTGetStartingLong(IFBPtr  : IsamFileBlockPtr;        {!!.42}
                            RefNr   : LongInt;
                        var Dest    : LongInt);
  {-Gets the first four bytes of the specified record with the given
    reference}
begin
  IsamEntryCode(IFBPtr, NoOptions);
  if IsamOK then
    IsamGetStartingLong(IFBPtr, RefNr, Dest);
  IsamExitCode(IFBPtr);
end;


procedure BTPutRec(IFBPtr  : IsamFileBlockPtr;
                   RefNr   : LongInt;
               var Source;
                   ISOLock : Boolean);
  {-Puts the specified record at the given reference}
var
  Options : Word;
begin
  if ISOLock then begin
    Options := OptWriteRoutine;                                {!!.41}
  end
  else begin
    Options := OptCheckLock or OptWriteRoutine;                {!!.41}
  end;
  IsamEntryCode(IFBPtr, Options);
  if IsamOK then begin
    IFBPtr^.CharConvProc(@Source, IFBPtr^.DIDPtr^[0]^.LenRec,  {!!.50}
        False, IFBPtr^.CCHookPtr);                             {!!.50}
    IsamPutRec(IFBPtr, RefNr, Source);                         {!!.50}
    if not IFBPtr^.CCDestrWrite then begin                     {!!.50}
      IFBPtr^.CharConvProc(@Source, IFBPtr^.DIDPtr^[0]^.LenRec,{!!.50}
          True, IFBPtr^.CCHookPtr);                            {!!.50}
    end;                                                       {!!.50}
  end;
  IsamExitCode(IFBPtr);
end;


procedure BTAddRec(IFBPtr : IsamFileBlockPtr;
               var RefNr  : LongInt;
               var Source );
  {-Adds a record to the given fileblock and returns the reference}
begin
  IsamEntryCode(IFBPtr, OptCheckLock or OptWriteRoutine);      {!!.41}
  if IsamOK then begin
    IFBPtr^.CharConvProc(@Source, IFBPtr^.DIDPtr^[0]^.LenRec,  {!!.50}
        False, IFBPtr^.CCHookPtr);                             {!!.50}
    IsamAddRec(IFBPtr, RefNr, Source);
    if not IFBPtr^.CCDestrWrite then begin                     {!!.50}
      IFBPtr^.CharConvProc(@Source, IFBPtr^.DIDPtr^[0]^.LenRec,{!!.50}
          True, IFBPtr^.CCHookPtr);                            {!!.50}
    end;                                                       {!!.50}
  end;
  IsamExitCode(IFBPtr);
end;


procedure BTDeleteRec(IFBPtr : IsamFileBlockPtr; RefNr : LongInt);
  {-Deletes the record with reference RefNr}
begin
  IsamEntryCode(IFBPtr, OptCheckLock or OptWriteRoutine);      {!!.41}
  if IsamOK then
    IsamDeleteRec(IFBPtr, RefNr);
  IsamExitCode(IFBPtr);
end;


function BTUsedRecs(IFBPtr : IsamFileBlockPtr) : LongInt;
  {-Returns the number of currently used records}
begin
  IsamEntryCode(IFBPtr, OptReadPrefix);
  if IsamOK then begin
    with IFBPtr^.DIDPtr^[0]^ do begin
      BTUsedRecs := NumRec - NumberFree;
    end;
  end
  else begin
    BTUsedRecs := 0;
  end;
  IsamExitCode(IFBPtr);
end;


function BTFreeRecs(IFBPtr : IsamFileBlockPtr) : LongInt;
  {-Returns the number of currently not used records}
begin
  IsamEntryCode(IFBPtr, OptReadPrefix);
  if IsamOK then begin
    BTFreeRecs := IFBPtr^.DIDPtr^[0]^.NumberFree;
  end
  else begin
    BTFreeRecs := 0;
  end;
  IsamExitCode(IFBPtr);
end;


function BTFileLen(IFBPtr : IsamFileBlockPtr) : LongInt;
  {-Returns the number of currently occupied records by this fileblock}
begin
  IsamEntryCode(IFBPtr, OptReadPrefix);
  if IsamOK then begin
    BTFileLen := Succ (IFBPtr^.DIDPtr^[0]^.NumRec);
  end
  else begin
    BTFileLen := 0;
  end;
  IsamExitCode(IFBPtr);
end;


function BTUsedKeys(IFBPtr : IsamFileBlockPtr; Key : Word) : LongInt;
  {-Returns the number of currently used keys of the specified number}
begin
  IsamEntryCode(IFBPtr, OptReadPrefix or OptKeyRoutine or Key);
  if IsamOK then begin
    BTUsedKeys := IFBPtr^.DIDPtr^[Key]^.NumKeys;
  end
  else begin
    BTUsedKeys := 0;
  end;
  IsamExitCode(IFBPtr);
end;


function BTNrOfKeys(IFBPtr : IsamFileBlockPtr) : Word;
  {-Returns the number of defined keys}
begin
  IsamEntryCode(IFBPtr, NoOptions);
  if IsamOK then begin
    BTNrOfKeys := IFBPtr^.NrOfKeys;
  end
  else begin
    BTNrOfKeys := 0;
  end;
  IsamExitCode(IFBPtr);
end;


function BTPeekNrOfKeys(IFBPtr : IsamFileBlockPtr) : Word;
  {-Just like the routine without "Peek", but not destroying the previous
    error on success}
var
  ErrSt : ErrStatSaveRec;
begin
  SaveErrorStat(ErrSt);
  BTPeekNrOfKeys := BTNrOfKeys(IFBPtr);
  if IsamOK then
    RestoreErrorStat(ErrSt);
end;


function BTDatRecordSize(IFBPtr : IsamFileBlockPtr) : LongInt;
  {-Returns the lenght of a data record}
begin
  IsamEntryCode(IFBPtr, NoOptions);
  if IsamOK then begin
    BTDatRecordSize := IFBPtr^.DIDPtr^[0]^.LenRec;
  end
  else begin
    BTDatRecordSize := 0;
  end;
  IsamExitCode(IFBPtr);
end;


function BTPeekDatRecordSize(IFBPtr : IsamFileBlockPtr) : LongInt;
  {-Just like the routine without "Peek", but not destroying the previous
    error on success}
var
  ErrSt : ErrStatSaveRec;
begin
  SaveErrorStat(ErrSt);
  BTPeekDatRecordSize := BTDatRecordSize(IFBPtr);
  if IsamOK then
    RestoreErrorStat(ErrSt);
end;


function BTKeyRecordSize(IFBPtr : IsamFileBlockPtr) : LongInt;
  {-Returns the maximum number of bytes used for a following key adding}
begin
  IsamEntryCode(IFBPtr, NoOptions);
  if IsamOK then begin
    BTKeyRecordSize := IFBPtr^.BlockLen;
  end
  else begin
    BTKeyRecordSize := 0;
  end;
  IsamExitCode(IFBPtr);
end;


function BTPeekKeyRecordSize(IFBPtr : IsamFileBlockPtr) : LongInt;
  {-Just like the routine without "Peek", but not destroying the previous
    error on success}
var
  ErrSt : ErrStatSaveRec;
begin
  SaveErrorStat(ErrSt);
  BTPeekKeyRecordSize := BTKeyRecordSize(IFBPtr);
  if IsamOK then
    RestoreErrorStat(ErrSt);
end;


function BTMinimumDatKeys(IFBPtr : IsamFileBlockPtr; Space : LongInt)
                                                               : LongInt;
  {-Returns the minimum number of data (including keys), that can be placed
    on a disk having Space bytes free Space}
var
  Nr,
  BL,
  LR : LongInt;
  PS : Word;
begin
  IsamEntryCode(IFBPtr, NoOptions);
  if IsamOK then begin
    with IFBPtr^ do begin                                      {!!.42}
      BL := BlockLen;
      LR := DIDPtr^[0]^.LenRec;                                {!!.42}
      if NrOfKeys > 0 then begin                               {!!.42}
        PS := DIDPtr^[1]^.UsedPageSize;                        {!!.42}
      end
      else begin                                               {!!.42}
        PS := CreatePageSize;                                  {!!.42}
      end;                                                     {!!.42}
    end;
    Nr := (Space - 2 * BL - LR)                                {!!.42}
        Div (LR + BL Div LongInt (PS Shr 1));
    if Nr < 0 then begin
      BTMinimumDatKeys := 0;
    end
    else begin
      BTMinimumDatKeys := Nr;
    end;
  end
  else begin
    BTMinimumDatKeys := 0;
  end;
  IsamExitCode(IFBPtr);
end;


function BTPeekMinimumDatKeys(IFBPtr : IsamFileBlockPtr;
                              Space  : LongInt) : LongInt;
  {-Just like the routine without "Peek", but not destroying the previous
    error on success}
var
  ErrSt : ErrStatSaveRec;
begin
  SaveErrorStat(ErrSt);
  BTPeekMinimumDatKeys := BTMinimumDatKeys(IFBPtr, Space);
  if IsamOK then
    RestoreErrorStat(ErrSt);
end;


procedure BTSetSearchForSequential(IFBPtr : IsamFileBlockPtr;
                                   Key    : Word;
                                   ToOn  : Boolean); {!!.TP}
  {-Enables search mode in sequential key operations}
begin
  IsamEntryCode(IFBPtr, OptKeyRoutine or Key);
  if IsamOK then
    IFBPtr^.DIDPtr^[Key]^.SearchForSequentialEnabled := ToOn; {!!.TP}
  IsamExitCode(IFBPtr);
end;


procedure BTGetSearchForSequential(IFBPtr : IsamFileBlockPtr;
                                   Key    : Word;
                               var SFS    : Boolean);
  {-Tests search mode for sequential key operations}
begin
  IsamEntryCode(IFBPtr, OptKeyRoutine or Key);
  if IsamOK then
    SFS := IFBPtr^.DIDPtr^[Key]^.SearchForSequentialEnabled;
  IsamExitCode(IFBPtr);
end;


procedure BTFindRecRef(IFBPtr                  : IsamFileBlockPtr;
                   var UserDatRef              : LongInt;
                       NotFoundSearchDirection : Integer);  {!!.42}
begin
  IsamEntryCode(IFBPtr, OptReadPrefix);
  if IsamOK then begin
    IsamFindRecRef(IFBPtr, UserDatRef, NotFoundSearchDirection);
  end;
  IsamExitCode(IFBPtr);
end;


procedure BTNextRecRef(IFBPtr     : IsamFileBlockPtr;
                   var UserDatRef : LongInt);               {!!.42}
begin
  IsamEntryCode(IFBPtr, OptReadPrefix);
  if IsamOK then begin
    Inc (UserDatRef);
    IsamFindRecRef(IFBPtr, UserDatRef, 1);
  end;
  IsamExitCode(IFBPtr);
end;


procedure BTPrevRecRef(IFBPtr     : IsamFileBlockPtr;
                   var UserDatRef : LongInt);               {!!.42}
begin
  IsamEntryCode(IFBPtr, OptReadPrefix);
  if IsamOK then begin
    Dec (UserDatRef);
    IsamFindRecRef(IFBPtr, UserDatRef, -1);
  end;
  IsamExitCode(IFBPtr);
end;


procedure BTGetApprRecPos(IFBPtr     : IsamFileBlockPtr;
                      var RelPos     : Word;
                          Scale      : Word;
                          UserDatRef : LongInt);            {!!.42}
begin
  IsamEntryCode(IFBPtr, OptReadPrefix);
  if IsamOK then begin
    IsamGetApprRecPos(IFBPtr, RelPos, Scale, UserDatRef);
  end;
  IsamExitCode(IFBPtr);
end;


procedure BTGetApprRecRef(IFBPtr     : IsamFileBlockPtr;
                          RelPos     : Word;
                          Scale      : Word;
                      var UserDatRef : LongInt);            {!!.42}

begin
  IsamEntryCode(IFBPtr, OptReadPrefix);
  if IsamOK then begin
    IsamGetApprRecRef(IFBPtr, RelPos, Scale, UserDatRef);
  end;
  IsamExitCode(IFBPtr);
end;


procedure BTClearKey(IFBPtr : IsamFileBlockPtr; Key : Word);
  {-Places the internal sequential pointer to a null position}
begin
  IsamEntryCode(IFBPtr, OptKeyRoutine or OptReadPrefix or Key);
  if IsamOK then
    IsamClearKey(IFBPtr, Key);
  IsamExitCode(IFBPtr);
end;


procedure BTNextKey(IFBPtr     : IsamFileBlockPtr;
                    Key        : Word;
                var UserDatRef : LongInt;
                var UserKey    : IsamKeyStr   );
  {-Returns in UserKey / UserDatRef the value and reference number of the
    next key}
begin
  IsamEntryCode(IFBPtr, OptKeyRoutine or OptReadPrefix or Key);
  if IsamOK then begin                                         {!!.41}
    if IFBPtr^.DIDPtr^[Key]^.PathInd = 0 then begin            {!!.41}
      {-Key was cleared by a previous operation}               {!!.41}
      IsamClearKey(IFBPtr, Key);                               {!!.41}
        {-Clear it again to allow sequential access}           {!!.41}
    end;                                                       {!!.41}
    IsamNextKey(IFBPtr, Key, UserDatRef, UserKey);
  end;                                                         {!!.41}
  IsamExitCode(IFBPtr);
end;


procedure BTPrevKey(IFBPtr     : IsamFileBlockPtr;
                    Key        : Word;
                var UserDatRef : LongInt;
                var UserKey    : IsamKeyStr      );
  {-Returns in UserKey / UserDatRef the value and reference number of the
    previous key}
begin
  IsamEntryCode(IFBPtr, OptKeyRoutine or OptReadPrefix or Key);
  if IsamOK then begin                                         {!!.41}
    if IFBPtr^.DIDPtr^[Key]^.PathInd = 0 then begin            {!!.41}
      {-Key was cleared by a previous operation}               {!!.41}
      IsamClearKey(IFBPtr, Key);                               {!!.41}
        {-Clear it again to allow sequential access}           {!!.41}
    end;                                                       {!!.41}
    IsamPrevKey(IFBPtr, Key, UserDatRef, UserKey);
  end;                                                         {!!.41}
  IsamExitCode(IFBPtr);
end;


procedure BTFindKey(IFBPtr     : IsamFileBlockPtr;
                    Key        : Word;
                var UserDatRef : LongInt;
                    UserKey    : IsamKeyStr);
  {-Returns in UserDatRef the reference number of the specified key}
var
  TempKey  : IsamKeyStr;
{$IFDEF ASCIIZeroKeys}
  TempKeyZ : IsamKeyStr;
{$ENDIF}
{$IFDEF LengthByteKeys}
  TempKeyZ : IsamKeyStr absolute UserKey;
{$ENDIF}
begin
  IsamEntryCode(IFBPtr, OptKeyRoutine or OptReadPrefix or Key);
  if IsamOK then begin
    {$IFDEF ASCIIZeroKeys}
    IsamMakeStrZ(UserKey, TempKeyZ);
    {$ENDIF}
    UserDatRef := 0;
    IsamFindKey(IFBPtr, Key, UserDatRef, TempKeyZ);
    if (not IsamOK) and (IsamError = 0)
        and IFBPtr^.DIDPtr^[Key]^.AllowDupKeys then begin
      IsamClearOK;
      IsamNextKey(IFBPtr, Key, UserDatRef, TempKey);
      IsamOK := IsamOK and (UserKey = TempKey);
    end;
    if not IsamOK then begin
      case IsamError of
        0, 10200..10299 : IsamError := 10200;
      end; {case}
    end;
  end;
  IsamExitCode(IFBPtr);
end;


procedure BTSearchKey(IFBPtr     : IsamFileBlockPtr;
                      Key        : Word;
                  var UserDatRef : LongInt;
                  var UserKey    : IsamKeyStr);
  {-Returns in UserKey / UserDatRef the value and reference number of the
    via UserKey specified or the following key}
begin
  IsamEntryCode(IFBPtr, OptKeyRoutine or OptReadPrefix or Key);
  if IsamOK then
    IsamSearchKey(IFBPtr, Key, UserDatRef, UserKey);
  IsamExitCode(IFBPtr);
end;


procedure BTFindKeyAndRef(IFBPtr                  : IsamFileBlockPtr;
                          Key                     : Word;
                      var UserDatRef              : LongInt;
                      var UserKey                 : IsamKeyStr;
                          NotFoundSearchDirection : Integer);
  {-Returns in UserKey / UserDatRef the value and reference number of the
    via UserKey / UserDatRef specified combination.
    NotFoundSearchDirection determines whether and where to search for a
    result in case nothing was found}
begin
  IsamEntryCode(IFBPtr, OptKeyRoutine or OptReadPrefix or Key);
  if IsamOK then
    IsamFindKeyAndRef(IFBPtr, Key, UserDatRef, UserKey, NotFoundSearchDirection);
  IsamExitCode(IFBPtr);
end;


procedure BTSearchKeyAndRef(IFBPtr     : IsamFileBlockPtr;
                            Key        : Word;
                        var UserDatRef : LongInt;
                        var UserKey    : IsamKeyStr);
  {-Returns in UserKey / UserDatRef the value and reference number of the
    via UserKey / UserDatRef specified combination.
    in case nothing was found a following result is searched. if this does
    not exist, a previous combination is searched}
begin
  IsamEntryCode(IFBPtr, OptKeyRoutine or OptReadPrefix or Key);
  if IsamOK then begin
    IsamFindKeyAndRef(IFBPtr, Key, UserDatRef, UserKey, 1);
    if IsamError = 10250 then begin
      IsamClearOK;
      IsamPrevKey(IFBPtr, Key, UserDatRef, UserKey);
    end;
  end;
  IsamExitCode(IFBPtr);
end;


function BTKeyExists(IFBPtr     : IsamFileBlockPtr;
                     Key        : Word;
                     UserDatRef : LongInt;
                     UserKey    : IsamKeyStr   ) : Boolean;
  {-Returns True, if the combination UserKey / UserDatRef exists}
var
  C, K, L, R : Integer;
  IPgPtr     : IsamPagePtr;
  ADK        : Boolean;
  {$IFDEF ASCIIZeroKeys}
  TempKeyZ : IsamKeyStr;
  {$ENDIF}
  {$IFDEF LengthByteKeys}
  TempKeyZ : IsamKeyStr absolute UserKey;
  {$ENDIF}
  {------}
  procedure Search(PRef : LongInt);
  begin
    if PRef = 0 then begin
      Exit;
    end
    else begin
      IsamGetPage(IFBPtr, PRef, Key, IPgPtr);
      if not IsamOK then Exit;
      with IPgPtr^ do begin
        L := 1;
        R := ItemsOnPage;
        ADK := IFBPtr^.DIDPtr^[Key]^.AllowDupKeys;
        repeat
          K :=(L + R) Shr 1;
          C := IsamCompKeys(TempKeyZ,
                              ItemArray [K].KeyStr,
                              UserDatRef,
                              ItemArray [K].DataRef,
                              (UserDatRef <> 0) and ADK);
            {-UserDatRef=0 means search independent of the data reference,
              even if ADK is true}
          if C <= 0 then
            R := Pred (K);
          if C >= 0 then
            L := Succ (K);
        until L > R;
        if L - R > 1 then begin
          BTKeyExists := True;
        end
        else begin
          if R = 0 then begin
            Search(BckwPageRef);
          end
          else begin
            Search(ItemArray [R].PageRef);
          end;
        end;
      end;
    end;
  end;
  {------}
begin
  IsamEntryCode(IFBPtr, OptKeyRoutine or OptReadPrefix or Key);
  BTKeyExists := False;
  if IsamOK then begin
    {$IFDEF ASCIIZeroKeys}
    IsamMakeStrZ(UserKey, TempKeyZ);
    {$ENDIF}
    Search(IFBPtr^.DIDPtr^[Key]^.RootRef);
  end;
  IsamExitCode(IFBPtr);
end;


procedure BTNextDiffKey(IFBPtr     : IsamFileBlockPtr;
                        Key        : Word;
                    var UserDatRef : LongInt;
                    var UserKey    : IsamKeyStr   );
  {-Returns in UserKey / UserDatRef the value and reference number of the
    next from UserKey different key}
var
  I : Integer;
begin
  IsamEntryCode(IFBPtr, OptKeyRoutine or OptReadPrefix or Key);
  if IsamOK then begin
    {$IFDEF LengthByteKeys}
    if Length(UserKey) < IFBPtr^.DIDPtr^[Key]^.KeyLen then begin
      UserKey := UserKey + #0;
    end
    else begin
      I := IFBPtr^.DIDPtr^[Key]^.KeyLen;
      repeat
        UserKey [I] := Succ(UserKey [I]);
        Dec (I);
      until(I = 0) or(UserKey [Succ (I)] <> #0);
      if UserKey [Succ (I)] = #0 then begin
        IsamClearKey(IFBPtr, Key);
        IsamOK := False;
        IsamError := 10240;
      end;
    end;
    {$ENDIF}
    {$IFDEF ASCIIZeroKeys}
    if Length(UserKey) < IFBPtr^.DIDPtr^[Key]^.KeyLen then begin
      UserKey := UserKey + #1;
    end
    else begin
      I := IFBPtr^.DIDPtr^[Key]^.KeyLen;
      repeat
        Inc(UserKey [I]);
        if UserKey [I] = #0 then
          Inc (UserKey [I]);
        I := Pred (I);
      until(I = 0) or(UserKey [Succ (I)] <> #1);
      if UserKey [Succ (I)] = #1 then begin
        IsamClearKey(IFBPtr, Key);
        IsamOK := False;
        IsamError := 10240;
        Exit;
      end;
    end;
    {$ENDIF}
  end;
  if IsamOK then begin
    IsamSearchKey(IFBPtr, Key, UserDatRef, UserKey);
    if not IsamOK then begin
      case IsamError of
        0, 10200..10299 : IsamError := 10240;
      end; {case}
    end;
  end;
  IsamExitCode(IFBPtr);
end;


procedure BTPrevDiffKey(IFBPtr     : IsamFileBlockPtr;
                        Key        : Word;
                    var UserDatRef : LongInt;
                    var UserKey    : IsamKeyStr   );
  {-Returns in UserKey / UserDatRef the value and reference number of the
    privious from UserKey different key}
begin
  IsamEntryCode(IFBPtr, OptKeyRoutine or OptReadPrefix or Key);
  if IsamOK then begin
    IsamSearchKey(IFBPtr, Key, UserDatRef, UserKey);
    case IsamError of
      0, 10200..10299 :
        begin
          IsamClearOK;
          IsamPrevKey(IFBPtr, Key, UserDatRef, UserKey);
          if IsamError = 10260 then
            IsamError := 10245;
        end;
    end; {case}
  end;
  IsamExitCode(IFBPtr);
end;


procedure BTAddKey(IFBPtr     : IsamFileBlockPtr;
                   Key        : Word;
                   UserDatRef : LongInt;
                   UserKey    : IsamKeyStr   );
  {-Adds the combination UserKey / UserDatRef to the tree}
var
  {$IFDEF ASCIIZeroKeys}
  TempKeyZ : IsamKeyStr;
  {$ENDIF}
  {$IFDEF LengthByteKeys}
  TempKeyZ : IsamKeyStr absolute UserKey;
  {$ENDIF}
begin
  IsamEntryCode(IFBPtr, OptKeyRoutine or OptCheckLock or Key
      or OptWriteRoutine);                                    {!!.41}
  if IsamOK then begin
    if Length(UserKey) <= IFBPtr^.DIDPtr^[Key]^.KeyLen then begin
      {$IFDEF ASCIIZeroKeys}
      IsamMakeStrZ(UserKey, TempKeyZ);
      {$ENDIF}
      IsamAddKey(IFBPtr, Key, UserDatRef, TempKeyZ);
      if IFBPtr^.SaveFB then begin
        if IsamError = 0 then begin
          IsamReduceDiaFile(IFBPtr);
        end
        else begin
          IsamRepairFileBlock(IFBPtr);
          if IsamOK then begin
            IsamOK := False;
            IsamError := 10003;
          end;
        end;
        IsamResetSaveBuffered;
      end;
      if not IsamOK and (IsamError = 0) then
        IsamError := 10230;
    end
    else begin
      IsamOK := False;
      IsamError := 10125;
    end;
  end;
  IsamExitCode(IFBPtr);
end;


procedure BTDeleteKey(IFBPtr     : IsamFileBlockPtr;
                      Key        : Word;
                      UserDatRef : LongInt;
                      UserKey    : IsamKeyStr   );
  {-Deletes the combination UserKey / UserDatRef in the tree}
var
  {$IFDEF ASCIIZeroKeys}
  TempKeyZ : IsamKeyStr;
  {$ENDIF}
  {$IFDEF LengthByteKeys}
  TempKeyZ : IsamKeyStr absolute UserKey;
  {$ENDIF}
begin
  IsamEntryCode(IFBPtr, OptKeyRoutine or OptCheckLock or Key
      or OptWriteRoutine);                                    {!!.41}
  if IsamOK then begin
    {$IFDEF ASCIIZeroKeys}
    IsamMakeStrZ(UserKey, TempKeyZ);
    {$ENDIF}
    IsamDeleteKey(IFBPtr, Key, UserDatRef, TempKeyZ);
    if IFBPtr^.SaveFB then begin
      if IsamError = 0 then begin
        IsamReduceDiaFile(IFBPtr);
      end
      else begin
        IsamRepairFileBlock(IFBPtr);
        if IsamOK then begin
          IsamOK := False;
          IsamError := 10004;
        end;
      end;
      IsamResetSaveBuffered;
    end;
    if not IsamOK and (IsamError = 0) then
      IsamError := 10220;
  end;
  IsamExitCode(IFBPtr);
end;


procedure BTDeleteAllKeys(IFBPtr : IsamFileBlockPtr; Key : Word);
  {-Deletes all keys with number Key}
begin
  IsamEntryCode(IFBPtr, OptKeyRoutine or OptCheckLock or Key
      or OptWriteRoutine);                                    {!!.41}
  if IsamOK then begin
    IsamDeleteAllKeys(IFBPtr, Key);
    if IFBPtr^.SaveFB then begin
      if IsamOK then begin
        IsamReduceDiaFile(IFBPtr);
      end
      else begin
        IsamRepairFileBlock(IFBPtr);
        if IsamOK then begin
          IsamOK := False;
          IsamError := 10005;
        end;
      end;
    end;
  end;
  IsamExitCode(IFBPtr);
end;


function BTOtherWSChangedKey(IFBPtr : IsamFileBlockPtr;
                             Key    : Word) : Boolean;
  {-Returns True, if it is definite, that a key of number Key was changed
    by another workstation}
var
  FlagSet : IsamFlagSet;
  Dummy,
  Valid   : Boolean;
begin
  IsamEntryCode(IFBPtr, OptKeyRoutine or Key);
  BTOtherWSChangedKey := False;
  {$IFNDEF NoNet}
  if IsamOK then begin
    if IFBPtr^.NSP <> Nil then begin
      with IFBPtr^.NSP^ do begin
        if not (Locked or ReadLocked) then begin
          IsamLockAndGetFlagSet(IFBPtr, FlagSet, Valid);
          if IsamOK and Valid then begin
            BTOtherWSChangedKey  := Key in FlagSet;
            Dummy := IsamUnLockMyFlagSet(IFBPtr);
          end;
        end;
      end;
    end;
  end;
  {$ENDIF}
  IsamExitCode(IFBPtr);
end;


procedure BTGetApprRelPos(IFBPtr     : IsamFileBlockPtr;
                          Key        : Word;
                      var RelPos     : Word;
                          Scale      : Word;
                          UserKey    : IsamKeyStr;
                          UserDatRef : LongInt);
  {-Returns in RelPos the approximate relative position of the combination
    UserKey / UserDatRef in 0..Scale}
begin
  IsamEntryCode(IFBPtr, OptKeyRoutine or OptReadPrefix or Key);
  if IsamOK then
    IsamGetApprRelPos(IFBPtr, Key, RelPos, Scale, UserKey, UserDatRef);
  IsamExitCode(IFBPtr);
end;


procedure BTGetApprKeyAndRef(IFBPtr     : IsamFileBlockPtr;
                             Key,
                             RelPos     : Word;
                             Scale      : Word;
                         var UserKey    : IsamKeyStr;
                         var UserDatRef : LongInt);
  {-Returns the combination UserKey / UserDatRef, that is approximately
    at the position RelPos in 0..Scale}
begin
  IsamEntryCode(IFBPtr, OptKeyRoutine or OptReadPrefix or Key);
  if IsamOK then begin
    IsamGetApprKeyAndRef(IFBPtr, Key, RelPos, Scale, UserKey, UserDatRef);
  end;
  IsamExitCode(IFBPtr);
end;


procedure BTFlushAllFileBlocks;
  {-Flushes all new data of all fileblocks to disk}
begin
  IsamEntryCode(Pointer (NotAFileBlockPtr), NoOptions);
  if IsamOK then
    IsamFlushAllFileBlocks;
  IsamExitCode(Pointer (NotAFileBlockPtr));
end;


procedure BTFlushFileBlock(IFBPtr : IsamFileBlockPtr);
  {-Flushes all new data of this fileblocks to disk}
begin
  IsamEntryCode(IFBPtr, NoOptions);
  if IsamOK then
    IsamFlushFileBlock(IFBPtr);
  IsamExitCode(IFBPtr);
end;


procedure BTDeleteFileBlock(FName : IsamFileBlockName);
  {-Deletes a fileblock}
begin
  IsamEntryCode(Pointer (NotAFileBlockPtr), NoOptions);
  if IsamOK then
    IsamDeleteFileBlock(FName);
  IsamExitCode(Pointer (NotAFileBlockPtr));
end;


procedure BTCreateFileBlock(FName        : IsamFileBlockName;
                            DatSLen      : LongInt;
                            NumberOfKeys : Word;
                            IID          : IsamIndDescr);
  {-Creates a closed fileblock}
begin
  IsamEntryCode(Pointer (NotAFileBlockPtr), NoOptions);
  if IsamOK then
    IsamCreateFileBlock(FName, DatSLen, NumberOfKeys, IID);
  IsamExitCode(Pointer (NotAFileBlockPtr));
end;


procedure BTOpenFileBlock(var IFBPtr      : IsamFileBlockPtr;
                              FName       : IsamFileBlockName;
                              ReadOnly,
                              AllReadOnly,
                              Save,
                              Net         : Boolean);
  {-Opens a fileblock}
begin
  IsamEntryCode(Pointer (NotAFileBlockPtr), NoOptions);
  if IsamOK then
    IsamOpenFileBlock(IFBPtr, FName, ReadOnly, AllReadOnly, Save, Net);
  IsamExitCode(Pointer (NotAFileBlockPtr));
end;


procedure BTCloseFileBlock(var IFBPtr : IsamFileBlockPtr);
  {-Closes a fileblock}
begin
  IsamEntryCode(IFBPtr, NoOptions);
  if IsamOK then
    IsamCloseFileBlock(IFBPtr);
  IsamExitCode(Pointer (NotAFileBlockPtr));
end;


procedure BTCloseAllFileBlocks;
  {-Closes all open fileblocks}
begin
  IsamEntryCode(Pointer (NotAFileBlockPtr), NoOptions);
  if IsamOK then
    IsamCloseAllFileBlocks;
  IsamExitCode(Pointer (NotAFileBlockPtr));
end;


function BTDataFileName(IFBPtr : IsamFileBlockPtr) : IsamFileName;
  {-Returns the name of the data file including path and extension}
begin
  IsamEntryCode(IFBPtr, NoOptions);
  if IsamOK then begin
    BTDataFileName := IsamGetFileName(IFBPtr^.DatF);
  end
  else begin
    BTDataFileName := '';
  end;
  IsamExitCode(IFBPtr);
end;


function BTPeekDataFileName(IFBPtr : IsamFileBlockPtr) : IsamFileName;
  {-Just like the routine without "Peek", but not destroying the previous
    error on success}
var
  ErrSt : ErrStatSaveRec;
begin
  SaveErrorStat(ErrSt);
  BTPeekDataFileName := BTDataFileName(IFBPtr);
  if IsamOK then
    RestoreErrorStat(ErrSt);
end;


function BTIndexFileName(IFBPtr : IsamFileBlockPtr) : IsamFileName;
  {-Returns the name of the index file including path and extension}
begin
  IsamEntryCode(IFBPtr, NoOptions);
  BTIndexFileName := '';
  if IsamOK then begin
    if IFBPtr^.NrOfKeys > 0 then begin
      BTIndexFileName := IsamGetFileName(IFBPtr^.IndF);
    end;
  end;
  IsamExitCode(IFBPtr);
end;


function BTPeekIndexFileName(IFBPtr : IsamFileBlockPtr) : IsamFileName;
  {-Just like the routine without "Peek", but not destroying the previous
    error on success}
var
  ErrSt : ErrStatSaveRec;
begin
  SaveErrorStat(ErrSt);
  BTPeekIndexFileName := BTIndexFileName(IFBPtr);
  if IsamOK then
    RestoreErrorStat(ErrSt);
end;


function BTGetNextUsedAddRecRef(IFBPtr : IsamFileBlockPtr) : LongInt;
  {-Returns the data reference, that is used for adding the next record;
    this function is of a more internal nature}
begin
  IsamEntryCode(IFBPtr, NoOptions);
  BTGetNextUsedAddRecRef := 0;
  if IsamOK then begin
    BTGetNextUsedAddRecRef := IsamGetNextUsedAddRecRef(IFBPtr);
  end;
  IsamExitCode(IFBPtr);
end;


function BTGetAfterNextUsedAddRecRef(IFBPtr : IsamFileBlockPtr) : LongInt;
  {-Returns the data reference, that is used after adding the next record;
    this function is of a more internal nature}
begin
  IsamEntryCode(IFBPtr, NoOptions);
  BTGetAfterNextUsedAddRecRef := 0;
  if IsamOK then begin
    BTGetAfterNextUsedAddRecRef := IsamGetAfterNextUsedAddRecRef(IFBPtr);
  end;
  IsamExitCode(IFBPtr);
end;


function BTGetInternalDialogID(IFBPtr : IsamFileBlockPtr) : Word;
  {-Returns the internal ID used by network accesss to this fileblock}
begin
  IsamEntryCode(IFBPtr, NoOptions);
  BTGetInternalDialogID := 0;
  {$IFNDEF NoNet}
  if IsamOK then begin                                         {!!.42}
    if IFBPtr^.NSP <> Nil then begin
      BTGetInternalDialogID := IFBPtr^.NSP^.LocalWSNr;
    end;
  end;                                                         {!!.42}
  {$ENDIF}
  IsamExitCode(IFBPtr);
end;


function BTGetAllowDupKeys(IFBPtr : IsamFileBlockPtr;            {!!.50}
                           KeyNr  : Word) : Boolean;
begin
  IsamEntryCode(IFBPtr, OptKeyRoutine or KeyNr);
  BTGetAllowDupKeys := False;
  if IsamOK then begin
    BTGetAllowDupKeys := IFBPtr^.DIDPtr^[KeyNr]^.AllowDupKeys;
  end;
  IsamExitCode(IFBPtr);
end;


function BTGetKeyLen(IFBPtr : IsamFileBlockPtr;                  {!!.50}
                       KeyNr  : Word) : Word;
begin
  IsamEntryCode(IFBPtr, OptKeyRoutine or KeyNr);
  BTGetKeyLen := 0;
  if IsamOK then
    BTGetKeyLen := IFBPtr^.DIDPtr^[KeyNr]^.KeyLen;
  IsamExitCode(IFBPtr);
end;


procedure BTNoCharConvert(DataPtr  : Pointer;                  {!!.50}
                          DataLen  : LongInt;
                          PostRead : Boolean;
                          HookPtr  : Pointer);
begin
end;


procedure BTSetCharConvert(IFBPtr     : IsamFileBlockPtr;      {!!.50}
                             CCProc     : ProcBTCharConvert;
                             HookPtr    : Pointer;
                             DestrWrite : Boolean);
begin
  IsamEntryCode(IFBPtr, NoOptions);
  if IsamOK then begin
    with IFBPtr^ do begin
      CharConvProc := CCProc;
      CCHookPtr := HookPtr;
      CCDestrWrite := DestrWrite;
    end;
  end;
  IsamExitCode(IFBPtr);
end;


function BTIsInitialized : Boolean;                                {!!.50}
begin
  BTIsInitialized := IsamIsInitialized;
end;


(*****************************************************************************)
{--Compatibility calls}
function BTIsamLockRecord(Start,
                          Len       : LongInt;
                          Handle    : IsamHandle;
                          TimeOut,
                          DelayTime : Word) : Boolean;
  {-Locks bytes Start to Start - Len + 1 in the file
    with handle Handle}
begin
  IsamEntryCode(Pointer (NotAFileBlockPtr), NoOptions);
  BTIsamLockRecord := True;
  {$IFNDEF NoNet}
  if IsamOK then begin
    BTIsamLockRecord := btfLockMgrAcqLock(Handle, Start, Len, TimeOut, DelayTime);
  end;
  {$ENDIF}
  IsamExitCode(Pointer (NotAFileBlockPtr));
end;


function BTIsamUnLockRecord(Start,
                            Len    : LongInt;
                            Handle : IsamHandle) : Boolean;
  {-Unlocks bytes Start to Start - Len + 1 in the file
    with handle Handle}
begin
  IsamEntryCode(Pointer (NotAFileBlockPtr), NoOptions);
  BTIsamUnLockRecord := True;
  {$IFNDEF NoNet}
  if IsamOK then
    BTIsamUnLockRecord := btfLockMgrRelLock(Handle, Start, Len);
  {$ENDIF}
  IsamExitCode(Pointer (NotAFileBlockPtr));
end;


function BTIsamGetSequentialOK(IFBPtr : IsamFileBlockPtr;        {!!.50}
                               KeyNr  : Word) : Boolean;
begin
  IsamEntryCode(IFBPtr, OptKeyRoutine or KeyNr);
  BTIsamGetSequentialOK := False;
  if IsamOK then begin
    BTIsamGetSequentialOK := IFBPtr^.DIDPtr^[KeyNr]^.SequentialOK;
  end;
  IsamExitCode(IFBPtr);
end;


function IsamGetNumRecAddress(IFBPtr : IsamFileBlockPtr) : Pointer;
                                                                   {!!.50}
begin
  IsamEntryCode(IFBPtr, NoOptions);
  IsamGetNumRecAddress := Nil;
  if IsamOK then begin
    IsamGetNumRecAddress := @IFBPtr^.DIDPtr^[0]^.NumRec;
  end;
  IsamExitCode(IFBPtr);
end;

