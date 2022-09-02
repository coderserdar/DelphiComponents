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
  {$F-,V-,B-,S-,I-,R-}
  {$IFDEF CanSetOvrflowCheck}
    {$Q-}
  {$ENDIF}

Unit ISCompat;

Interface

Uses
  Filer;

type
  IsamPageStackState = (StateUnknown, StateBadDialog,
                        StateInvalid, StateValid);

Var
  IsamFehler : Integer Absolute IsamError;

const
  DynamicNetType : NetSupportType = NoNet;

var
  InternalDosError : word absolute IsamDOSError;
  InternalDosFunction : word absolute IsamDOSFunc;
  IsamDelayForRetry : word absolute IsamDelayBetwLocks;

  Procedure InitIsam;

  Function IsamErrorClass : Integer;

  Procedure ForceWritingMark ( FFM : Boolean );

  Procedure PutRec (     IFBPtr : IsamFileBlockPtr;
                         RefNr  : LongInt;
                     Var Source                 );

  Procedure GetRec (     IFBPtr : IsamFileBlockPtr;
                         RefNr  : LongInt;
                     Var Dest                  );

  Procedure AddRec (     IFBPtr : IsamFileBlockPtr;
                     Var RefNr  : LongInt;
                     Var Source                );

  Function UsedRecs ( IFBPtr : IsamFileBlockPtr ) : LongInt;

  Function UsedKeys ( IFBPtr : IsamFileBlockPtr; Key : Integer ) : LongInt;

  Function FreeRecs ( IFBPtr : IsamFileBlockPtr ) : LongInt;

  Function FileLen ( IFBPtr : IsamFileBlockPtr ) : LongInt;

  Function DatNrOfKeys ( IFBPtr : IsamFileBlockPtr ) : Integer;

  Function DatRecordSize ( IFBPtr : IsamFileBlockPtr ) : LongInt;

  Function KeyRecordSize ( IFBPtr : IsamFileBlockPtr ) : LongInt;

  Function MinimumDatKeys ( IFBPtr : IsamFileBlockPtr; Space : LongInt )
      : LongInt;

  Function MinSpaceForPageStack : LongInt;

  Procedure DeleteRec ( IFBPtr : IsamFileBlockPtr; RefNr : LongInt );

  Function GetPageStack ( Free : LongInt ) : Integer;

  Procedure ReleasePageStack;

  Procedure FlushIsamBuffers;

  Procedure FlushAllFileBlocks;

  Procedure FlushBuffer ( IFBPtr : IsamFileBlockPtr );

  Procedure FlushFileBlock ( IFBPtr : IsamFileBlockPtr );

  Procedure MakeFileBlock ( Var IFBPtr        : IsamFileBlockPtr;
                                FName         : IsamFileBlockName;
                                DatSLen       : LongInt;
                                NumberOfKeys  : Integer;
                                IID           : IsamIndDescr);

  Procedure OpenFileBlock ( Var IFBPtr : IsamFileBlockPtr;
                                FName  : IsamFileBlockName );

  Procedure OpenSaveFileBlock ( Var IFBPtr : IsamFileBlockPtr;
                                    FName  : IsamFileBlockName );

  Procedure CloseFileBlock ( Var IFBPtr : IsamFileBlockPtr );

  Procedure CloseAllFileBlocks;

  Procedure DeleteFileBlock ( FName  : IsamFileBlockName );

  Function FileBlockIsOpen ( IFBPtr : IsamFileBlockPtr ) : Boolean;

  Procedure ClearKey (     IFBPtr : IsamFileBlockPtr; Key : Integer );

  Procedure NextKey (     IFBPtr     : IsamFileBlockPtr;
                          Key        : Integer;
                      Var UserDatRef : LongInt;
                      Var UserKey    : IsamKeyStr    );

  Procedure PrevKey (     IFBPtr     : IsamFileBlockPtr;
                          Key        : Integer;
                      Var UserDatRef : LongInt;
                      Var UserKey    : IsamKeyStr       );

  Procedure FindKey (     IFBPtr     : IsamFileBlockPtr;
                          Key        : Integer;
                      Var UserDatRef : LongInt;
                          UserKey    : IsamKeyStr       );

  Procedure SearchKey (     IFBPtr     : IsamFileBlockPtr;
                            Key        : Integer;
                        Var UserDatRef : LongInt;
                        Var UserKey    : IsamKeyStr    );

  Procedure FindKeyAndRef (     IFBPtr                  : IsamFileBlockPtr;
                                Key                     : Integer;
                            Var UserDatRef              : LongInt;
                            Var UserKey                 : IsamKeyStr;
                                NotFoundSearchDirection : Integer    );

  Procedure SearchKeyAndRef (     IFBPtr                  : IsamFileBlockPtr;
                                  Key                     : Integer;
                              Var UserDatRef              : LongInt;
                              Var UserKey                 : IsamKeyStr );

  Function KeyExists ( IFBPtr     : IsamFileBlockPtr;
                       Key        : Integer;
                       UserDatRef : LongInt;
                       UserKey    : IsamKeyStr    ) : Boolean;

  Procedure NextDiffKey (     IFBPtr     : IsamFileBlockPtr;
                              Key        : Integer;
                          Var UserDatRef : LongInt;
                          Var UserKey    : IsamKeyStr    );

  Procedure PrevDiffKey (     IFBPtr     : IsamFileBlockPtr;
                              Key        : Integer;
                          Var UserDatRef : LongInt;
                          Var UserKey    : IsamKeyStr    );

  Procedure AddKey ( IFBPtr     : IsamFileBlockPtr;
                     Key        : Integer;
                     UserDatRef : LongInt;
                     UserKey    : IsamKeyStr    );

  Procedure DeleteKey ( IFBPtr     : IsamFileBlockPtr;
                        Key        : Integer;
                        UserDatRef : LongInt;
                        UserKey    : IsamKeyStr    );

  Procedure DeleteAllKeys ( IFBPtr : IsamFileBlockPtr;
                            Key    : Integer );

  Procedure EnableSearchForSequential ( IFBPtr : IsamFileBlockPtr;
                                        Key    : Integer         );

  Procedure DisableSearchForSequential ( IFBPtr : IsamFileBlockPtr;
                                         Key    : Integer         );

  Procedure TestSearchForSequential (     IFBPtr : IsamFileBlockPtr;
                                          Key    : Integer;
                                      Var TSFS   : Boolean );

  Procedure GetApprRelPos (     IFBPtr     : IsamFileBlockPtr;
                                Key        : Integer;
                            Var RelPos     : Word;
                                Scale      : Word;
                                UserKey    : IsamKeyStr;
                                UserDatRef : LongInt );

  Procedure GetApprKeyAndRef (     IFBPtr     : IsamFileBlockPtr;
                                   Key        : Integer;
                                   RelPos     : Word;
                                   Scale      : Word;
                               Var UserKey    : IsamKeyStr;
                               Var UserDatRef : LongInt );

  procedure InitNetIsam(NetExpected : Boolean); {!!}

  Procedure ExitNetIsam;

  Procedure ReadLockFileBlock ( IFBPtr : IsamFileBlockPtr );

  Procedure LockFileBlock ( IFBPtr : IsamFileBlockPtr );

  Procedure UnLockFileBlock ( IFBPtr : IsamFileBlockPtr );

  Procedure ReadLockAllOpenFileBlocks;

  Procedure LockAllOpenFileBlocks;

  Procedure UnLockAllOpenFileBlocks;

  Procedure LockRec ( IFBPtr : IsamFileBlockPtr; Ref : LongInt );

  Procedure UnLockRec ( IFBPtr : IsamFileBlockPtr; Ref : LongInt );

  Procedure GetRecordInfo (     IFBPtr     : IsamFileBlockPtr;
                                Ref        : LongInt;
                            Var Start, Len : LongInt;
                            Var Handle     : Word );

  Function FileBlockIsLocked ( IFBPtr : IsamFileBlockPtr ) : Boolean;

  Function FileBlockIsReadLocked ( IFBPtr : IsamFileBlockPtr ) : Boolean;

  Function UsedNetRecs ( IFBPtr : IsamFileBlockPtr ) : LongInt;

  Function UsedNetKeys ( IFBPtr : IsamFileBlockPtr; Key : Integer ) : LongInt;

  Function FreeNetRecs ( IFBPtr : IsamFileBlockPtr ) : LongInt;

  Function NetFileLen ( IFBPtr : IsamFileBlockPtr ) : LongInt;

  Procedure GetNetRec (     IFBPtr : IsamFileBlockPtr;
                            RefNr  : LongInt;
                        Var Dest                );

  Procedure FindNetKey (     IFBPtr     : IsamFileBlockPtr;
                             Key        : Integer;
                         Var UserDatRef : LongInt;
                             UserKey    : IsamKeyStr  );

  Procedure SearchNetKey (     IFBPtr     : IsamFileBlockPtr;
                               Key        : Integer;
                           Var UserDatRef : LongInt;
                           Var UserKey    : IsamKeyStr  );

  Procedure FindNetKeyAndRef (     IFBPtr                  : IsamFileBlockPtr;
                                   Key                     : Integer;
                               Var UserDatRef              : LongInt;
                               Var UserKey                 : IsamKeyStr;
                                   NotFoundSearchDirection : Integer    );

  Procedure SearchNetKeyAndRef (     IFBPtr     : IsamFileBlockPtr;
                                     Key        : Integer;
                                 Var UserDatRef : LongInt;
                                 Var UserKey    : IsamKeyStr );

  Function NetKeyExists ( IFBPtr     : IsamFileBlockPtr;
                          Key        : Integer;
                          UserDatRef : LongInt;
                          UserKey    : IsamKeyStr  ) : Boolean;

  Procedure NextNetKey (     IFBPtr     : IsamFileBlockPtr;
                             Key        : Integer;
                         Var UserDatRef : LongInt;
                         Var UserKey    : IsamKeyStr  );

  Procedure PrevNetKey (     IFBPtr     : IsamFileBlockPtr;
                             Key        : Integer;
                         Var UserDatRef : LongInt;
                         Var UserKey    : IsamKeyStr  );

  Procedure ClearNetKey ( IFBPtr : IsamFileBlockPtr;
                          Key    : Integer      );

  Procedure NextDiffNetKey (     IFBPtr     : IsamFileBlockPtr;
                                 Key        : Integer;
                             Var UserDatRef : LongInt;
                             Var UserKey    : IsamKeyStr  );

  Procedure PrevDiffNetKey (     IFBPtr     : IsamFileBlockPtr;
                                 Key        : Integer;
                             Var UserDatRef : LongInt;
                             Var UserKey    : IsamKeyStr  );

  Procedure GetNetRecInSpiteOfLock (     IFBPtr : IsamFileBlockPtr;
                                         RefNr  : LongInt;
                                     Var Dest                );

  Procedure GetNetRecReadOnly (     IFBPtr : IsamFileBlockPtr;
                                    RefNr  : LongInt;
                                Var Dest                );


  Procedure AddNetRec (     IFBPtr : IsamFileBlockPtr;
                        Var RefNr  : LongInt;
                        Var Source                );

  Procedure PutNetRec (     IFBPtr : IsamFileBlockPtr;
                            RefNr  : LongInt;
                        Var Source                );

  Procedure DeleteNetRec ( IFBPtr : IsamFileBlockPtr;
                           RefNr  : LongInt   );

  Procedure AddNetKey ( IFBPtr     : IsamFileBlockPtr;
                        Key        : Integer;
                        UserDatRef : LongInt;
                        UserKey    : IsamKeyStr  );

  Procedure DeleteNetKey ( IFBPtr     : IsamFileBlockPtr;
                           Key        : Integer;
                           UserDatRef : LongInt;
                           UserKey    : IsamKeyStr  );

  Procedure DeleteAllNetKeys ( IFBPtr : IsamFileBlockPtr;
                               Key    : Integer );

  Procedure PutNetRecInSpiteOfLock (     IFBPtr : IsamFileBlockPtr;
                                         RefNr  : LongInt;
                                     Var Source              );

  Procedure MakeNetFileBlock ( Var IFBPtr  : IsamFileBlockPtr;
                                   FName   : IsamFileBlockName;
                                   DatSLen : LongInt;
                                   AnzKey  : Integer;
                                   IID     : IsamIndDescr  );

  Procedure CloseNetFileBlock ( Var IFBPtr : IsamFileBlockPtr );

  Procedure CloseEachFileBlock;

  Procedure OpenNetFileBlock ( Var IFBPtr : IsamFileBlockPtr;
                                   FName  : IsamFileBlockName );

  Procedure OpenSaveNetFileBlock ( Var IFBPtr : IsamFileBlockPtr;
                                       FName  : IsamFileBlockName );

  Procedure DeleteNetFileBlock ( FName  : IsamFileBlockName );

  Function SetDosRetry ( NrOfRetries, WaitTime : Integer ) : Boolean;

  Function NetSupported : NetSupportType;

  Function NoNetCompiled : Boolean;

  Function IsNetFileBlock ( IFBPtr : IsamFileBlockPtr ) : Boolean;

  Procedure GetApprNetRelPos (     IFBPtr     : IsamFileBlockPtr;
                                   Key        : Integer;
                               Var RelPos     : Word;
                                   Scale      : Word;
                                   UserKey    : IsamKeyStr;
                                   UserDatRef : LongInt );

  Procedure GetApprNetKeyAndRef (     IFBPtr     : IsamFileBlockPtr;
                                      Key        : Integer;
                                      RelPos     : Word;
                                      Scale      : Word;
                                  Var UserKey    : IsamKeyStr;
                                  Var UserDatRef : LongInt );

  Procedure ForceNetBufferWriteThrough ( DoIt : Boolean );

  {!!} {Routines that were in FILER.PAS but not in German version}

  function FileBlockLocked(IFBPtr : IsamFileBlockPtr) : Boolean;

  function FileNameFromIFBPtr(IFBPtr : IsamFileBlockPtr) : IsamFileBlockName;

  function PageStackValid(IFBPtr : IsamFileBlockPtr; KeyNr : Integer) : IsamPageStackState;

  procedure IsamFileSize(F : IsamFile; var Size : LongInt);

  procedure RemoveActiveLocks(IFBPtr : IsamFileBlockPtr);


Implementation

Const
  NetIsExpected : Boolean = False; {!!}


  Procedure InitIsam;
  Begin
    IsamClearOK;
    DynamicNetType := NoNet; {!!}
    NetIsExpected := False;  {!!}
  End;

  Function IsamErrorClass : Integer;
  Begin
    IsamErrorClass := BTIsamErrorClass;
  End;

  Procedure ForceWritingMark ( FFM : Boolean );
  Begin
    BTForceWritingMark ( FFM );
  End;

  Procedure PutRec (     IFBPtr : IsamFileBlockPtr;
                         RefNr  : LongInt;
                     Var Source                 );
  Begin
    BTPutRec ( IFBPtr, RefNr, Source, False );
  End;

  Procedure GetRec (     IFBPtr : IsamFileBlockPtr;
                         RefNr  : LongInt;
                     Var Dest                  );
  Begin
    BTGetRec ( IFBPtr, RefNr, Dest, False );
  End;

  Procedure AddRec (     IFBPtr : IsamFileBlockPtr;
                     Var RefNr  : LongInt;
                     Var Source                );
  Begin
    BTAddRec ( IFBPtr, RefNr, Source );
  End;

  Function UsedRecs ( IFBPtr : IsamFileBlockPtr ) : LongInt;
  Begin
    UsedRecs := BTUsedRecs ( IFBPtr );
  End;

  Function UsedKeys ( IFBPtr : IsamFileBlockPtr; Key : Integer ) : LongInt;
  Begin
    UsedKeys := BTUsedKeys ( IFBPtr, Key );
  End;

  Function FreeRecs ( IFBPtr : IsamFileBlockPtr ) : LongInt;
  Begin
    FreeRecs := BTFreeRecs ( IFBPtr );
  End;

  Function FileLen ( IFBPtr : IsamFileBlockPtr ) : LongInt;
  Begin
    FileLen := BTFileLen ( IFBPtr );
  End;

  Function DatNrOfKeys ( IFBPtr : IsamFileBlockPtr ) : Integer;
  Begin
    DatNrOfKeys := BTNrOfKeys ( IFBPtr );
  End;

  Function DatRecordSize ( IFBPtr : IsamFileBlockPtr ) : LongInt;
  Begin
    DatRecordSize := BTDatRecordSize ( IFBPtr );
  End;

  Function KeyRecordSize ( IFBPtr : IsamFileBlockPtr ) : LongInt;
  Begin
    KeyRecordSize := BTKeyRecordSize ( IFBPtr );
  End;

  Function MinimumDatKeys ( IFBPtr : IsamFileBlockPtr; Space : LongInt )
      : LongInt;
  Begin
    MinimumDatKeys := BTMinimumDatKeys ( IFBPtr, Space );
  End;

  Function MinSpaceForPageStack : LongInt;
  Begin
    IsamClearOK;
    MinSpaceForPageStack :=
        LongInt (RoundToGranul ( SizeOf (IsamRingBufferRec) )
        + RoundToGranul ( SizeOf (IsamPageEntry) ))
        * LongInt (MaxHeight);
  End;

  Procedure DeleteRec ( IFBPtr : IsamFileBlockPtr; RefNr : LongInt );
  Begin
    BTDeleteRec ( IFBPtr, RefNr );
  End;

  Function GetPageStack ( Free : LongInt ) : Integer;
  Var
    LResult : LongInt;                                        {!!.51}
    Net : NetSupportType;                        {!!}
  Begin
    if NetIsExpected then                        {!!}
      Net := DynamicNetType                      {!!}
    else                                         {!!}
      Net := NoNet;                              {!!}
    LResult := BTInitIsam ( Net, Free, 0 );      {!!}         {!!.51}
    GetPageStack := ILI (LResult).Lo;                         {!!.51}
  End;

  Procedure ReleasePageStack;
  Begin
    BTExitIsam;
  End;

  Procedure FlushIsamBuffers;
  Begin
    BTFlushAllFileBlocks;
  End;

  Procedure FlushAllFileBlocks;
  Begin
    BTFlushAllFileBlocks;
  End;

  Procedure FlushBuffer ( IFBPtr : IsamFileBlockPtr );
  Begin
    BTFlushFileBlock ( IFBPtr );
  End;

  Procedure FlushFileBlock ( IFBPtr : IsamFileBlockPtr );
  Begin
    BTFlushFileBlock ( IFBPtr );
  End;

  Procedure MakeFileBlock ( Var IFBPtr        : IsamFileBlockPtr;
                                FName         : IsamFileBlockName;
                                DatSLen       : LongInt;
                                NumberOfKeys  : Integer;
                                IID           : IsamIndDescr);
  Begin
    BTCreateFileBlock ( FName, DatSLen, NumberOfKeys, IID );
    If IsamOK Then Begin
      BTOpenFileBlock ( IFBPtr, FName, False, False, False, False );
    End;
  End;

  Procedure OpenFileBlock ( Var IFBPtr : IsamFileBlockPtr;
                                FName  : IsamFileBlockName );
  Begin
    BTOpenFileBlock ( IFBPtr, FName, False, False, False, False );
  End;

  Procedure OpenSaveFileBlock ( Var IFBPtr : IsamFileBlockPtr;
                                    FName  : IsamFileBlockName );
  Begin
    BTOpenFileBlock ( IFBPtr, FName, False, False, True, False );
  End;

  Procedure CloseFileBlock ( Var IFBPtr : IsamFileBlockPtr );
  Begin
    BTCloseFileBlock ( IFBPtr );
  End;

  Procedure CloseAllFileBlocks;
  Begin
    BTCloseAllFileBlocks;
  End;

  Procedure DeleteFileBlock ( FName  : IsamFileBlockName );
  Begin
    BTDeleteFileBlock ( FName );
  End;

  Function FileBlockIsOpen ( IFBPtr : IsamFileBlockPtr ) : Boolean;
  Begin
    FileBlockIsOpen := BTFileBlockIsOpen ( IFBPtr );
  End;

  Procedure ClearKey ( IFBPtr : IsamFileBlockPtr; Key : Integer );
  Begin
    BTClearKey ( IFBPtr, Key );
  End;

  Procedure NextKey (     IFBPtr     : IsamFileBlockPtr;
                          Key        : Integer;
                      Var UserDatRef : LongInt;
                      Var UserKey    : IsamKeyStr    );
  Begin
    BTNextKey ( IFBPtr, Key, UserDatRef, UserKey );
  End;

  Procedure PrevKey (     IFBPtr     : IsamFileBlockPtr;
                          Key        : Integer;
                      Var UserDatRef : LongInt;
                      Var UserKey    : IsamKeyStr       );
  Begin
    BTPrevKey ( IFBPtr, Key, UserDatRef, UserKey );
  End;

  Procedure FindKey (     IFBPtr     : IsamFileBlockPtr;
                          Key        : Integer;
                      Var UserDatRef : LongInt;
                          UserKey    : IsamKeyStr       );
  Begin
    BTFindKey ( IFBPtr, Key, UserDatRef, UserKey );
  End;

  Procedure SearchKey (     IFBPtr     : IsamFileBlockPtr;
                            Key        : Integer;
                        Var UserDatRef : LongInt;
                        Var UserKey    : IsamKeyStr    );
  Begin
    BTSearchKey ( IFBPtr, Key, UserDatRef, UserKey );
  End;

  Procedure FindKeyAndRef (     IFBPtr                  : IsamFileBlockPtr;
                                Key                     : Integer;
                            Var UserDatRef              : LongInt;
                            Var UserKey                 : IsamKeyStr;
                                NotFoundSearchDirection : Integer    );
  Begin
    BTFindKeyAndRef ( IFBPtr, Key, UserDatRef, UserKey,
        NotFoundSearchDirection );
  End;

  Procedure SearchKeyAndRef (     IFBPtr                  : IsamFileBlockPtr;
                                  Key                     : Integer;
                              Var UserDatRef              : LongInt;
                              Var UserKey                 : IsamKeyStr );
  Begin
    BTSearchKeyAndRef ( IFBPtr, Key, UserDatRef, UserKey );
  End;

  Function KeyExists ( IFBPtr     : IsamFileBlockPtr;
                       Key        : Integer;
                       UserDatRef : LongInt;
                       UserKey    : IsamKeyStr    ) : Boolean;
  Begin
    KeyExists := BTKeyExists ( IFBPtr, Key, UserDatRef, UserKey );
  End;

  Procedure NextDiffKey (     IFBPtr     : IsamFileBlockPtr;
                              Key        : Integer;
                          Var UserDatRef : LongInt;
                          Var UserKey    : IsamKeyStr    );
  Begin
    BTNextDiffKey ( IFBPtr, Key, UserDatRef, UserKey );
  End;

  Procedure PrevDiffKey (     IFBPtr     : IsamFileBlockPtr;
                              Key        : Integer;
                          Var UserDatRef : LongInt;
                          Var UserKey    : IsamKeyStr    );
  Begin
    BTPrevDiffKey ( IFBPtr, Key, UserDatRef, UserKey );
  End;

  Procedure AddKey ( IFBPtr     : IsamFileBlockPtr;
                     Key        : Integer;
                     UserDatRef : LongInt;
                     UserKey    : IsamKeyStr    );
  Begin
    BTAddKey ( IFBPtr, Key, UserDatRef, UserKey );
  End;

  Procedure DeleteKey ( IFBPtr     : IsamFileBlockPtr;
                        Key        : Integer;
                        UserDatRef : LongInt;
                        UserKey    : IsamKeyStr    );
  Begin
    BTDeleteKey ( IFBPtr, Key, UserDatRef, UserKey );
  End;

  Procedure DeleteAllKeys ( IFBPtr : IsamFileBlockPtr;
                            Key    : Integer );
  Begin
    BTDeleteAllKeys ( IFBPtr, Key );
  End;

  Procedure EnableSearchForSequential ( IFBPtr : IsamFileBlockPtr;
                                        Key    : Integer         );
  Begin
    BTSetSearchForSequential ( IFBPtr, Key, True );
  End;

  Procedure DisableSearchForSequential ( IFBPtr : IsamFileBlockPtr;
                                         Key    : Integer         );
  Begin
    BTSetSearchForSequential ( IFBPtr, Key, False );
  End;

  Procedure TestSearchForSequential (     IFBPtr : IsamFileBlockPtr;
                                          Key    : Integer;
                                      Var TSFS   : Boolean );
  Begin
    BTGetSearchForSequential ( IFBPtr, Key, TSFS );
  End;

  Procedure GetApprRelPos (     IFBPtr     : IsamFileBlockPtr;
                                Key        : Integer;
                            Var RelPos     : Word;
                                Scale      : Word;
                                UserKey    : IsamKeyStr;
                                UserDatRef : LongInt );
  Begin
    BTGetApprRelPos ( IFBPtr, Key, RelPos, Scale, UserKey, UserDatRef );
  End;

  Procedure GetApprKeyAndRef (     IFBPtr     : IsamFileBlockPtr;
                                   Key        : Integer;
                                   RelPos     : Word;
                                   Scale      : Word;
                               Var UserKey    : IsamKeyStr;
                               Var UserDatRef : LongInt );
  Begin
    BTGetApprKeyAndRef ( IFBPtr, Key, RelPos, Scale, UserKey, UserDatRef );
  End;

  procedure InitNetIsam(NetExpected : Boolean); {!!}
  Begin
    IsamClearOK;
    NetIsExpected := NetExpected; {!!}
  End;

  Procedure ExitNetIsam;
  Begin
    IsamClearOK;
    NetIsExpected := False; {!!}
  End;

  Procedure ReadLockFileBlock ( IFBPtr : IsamFileBlockPtr );
  Begin
    BTReadLockFileBlock ( IFBPtr );
  End;

  Procedure LockFileBlock ( IFBPtr : IsamFileBlockPtr );
  Begin
    BTLockFileBlock ( IFBPtr );
  End;

  Procedure UnLockFileBlock ( IFBPtr : IsamFileBlockPtr );
  Begin
    BTUnLockFileBlock ( IFBPtr );
  End;

  Procedure ReadLockAllOpenFileBlocks;
  Begin
    BTReadLockAllOpenFileBlocks;
  End;

  Procedure LockAllOpenFileBlocks;
  Begin
    BTLockAllOpenFileBlocks;
  End;

  Procedure UnLockAllOpenFileBlocks;
  Begin
    BTUnLockAllOpenFileBlocks;
  End;

  Procedure LockRec ( IFBPtr : IsamFileBlockPtr; Ref : LongInt );
  Begin
    BTLockRec ( IFBPtr, Ref );
  End;

  Procedure UnLockRec ( IFBPtr : IsamFileBlockPtr; Ref : LongInt );
  Begin
    BTUnLockRec ( IFBPtr, Ref );
  End;

  Procedure GetRecordInfo (     IFBPtr     : IsamFileBlockPtr;
                                Ref        : LongInt;
                            Var Start, Len : LongInt;
                            Var Handle     : Word );
  Begin
    BTGetRecordInfo ( IFBPtr, Ref, Start, Len, Handle );
  End;

  Function FileBlockIsLocked ( IFBPtr : IsamFileBlockPtr ) : Boolean;
  Begin
    FileBlockIsLocked := BTFileBlockIsLocked ( IFBPtr );
  End;

  Function FileBlockIsReadLocked ( IFBPtr : IsamFileBlockPtr ) : Boolean;
  Begin
    FileBlockIsReadLocked := BTFileBlockIsReadLocked ( IFBPtr );
  End;

  Function UsedNetRecs ( IFBPtr : IsamFileBlockPtr ) : LongInt;
  Begin
    UsedNetRecs := BTUsedRecs ( IFBPtr );
  End;

  Function UsedNetKeys ( IFBPtr : IsamFileBlockPtr; Key : Integer ) : LongInt;
  Begin
    UsedNetKeys := BTUsedKeys ( IFBPtr, Key );
  End;

  Function FreeNetRecs ( IFBPtr : IsamFileBlockPtr ) : LongInt;
  Begin
    FreeNetRecs := BTFreeRecs ( IFBPtr );
  End;

  Function NetFileLen ( IFBPtr : IsamFileBlockPtr ) : LongInt;
  Begin
    NetFileLen := BTFileLen ( IFBPtr );
  End;

  Procedure GetNetRec (     IFBPtr : IsamFileBlockPtr;
                            RefNr  : LongInt;
                        Var Dest                );
  Begin
    BTGetRec ( IFBPtr, RefNr, Dest, False );
  End;

  Procedure FindNetKey (     IFBPtr     : IsamFileBlockPtr;
                             Key        : Integer;
                         Var UserDatRef : LongInt;
                             UserKey    : IsamKeyStr  );
  Begin
    BTFindKey ( IFBPtr, Key, UserDatRef, UserKey );
  End;

  Procedure SearchNetKey (     IFBPtr     : IsamFileBlockPtr;
                               Key        : Integer;
                           Var UserDatRef : LongInt;
                           Var UserKey    : IsamKeyStr  );
  Begin
    BTSearchKey ( IFBPtr, Key, UserDatRef, UserKey );
  End;

  Procedure FindNetKeyAndRef (     IFBPtr                  : IsamFileBlockPtr;
                                   Key                     : Integer;
                               Var UserDatRef              : LongInt;
                               Var UserKey                 : IsamKeyStr;
                                   NotFoundSearchDirection : Integer    );
  Begin
    BTFindKeyAndRef ( IFBPtr, Key, UserDatRef, UserKey,
        NotFoundSearchDirection );
  End;

  Procedure SearchNetKeyAndRef (     IFBPtr     : IsamFileBlockPtr;
                                     Key        : Integer;
                                 Var UserDatRef : LongInt;
                                 Var UserKey    : IsamKeyStr );
  Begin
    BTSearchKeyAndRef ( IFBPtr, Key, UserDatRef, UserKey );
  End;

  Function NetKeyExists ( IFBPtr     : IsamFileBlockPtr;
                          Key        : Integer;
                          UserDatRef : LongInt;
                          UserKey    : IsamKeyStr  ) : Boolean;
  Begin
    NetKeyExists := BTKeyExists ( IFBPtr, Key, UserDatRef, UserKey );
  End;

  Procedure NextNetKey (     IFBPtr     : IsamFileBlockPtr;
                             Key        : Integer;
                         Var UserDatRef : LongInt;
                         Var UserKey    : IsamKeyStr  );
  Begin
    BTNextKey ( IFBPtr, Key, UserDatRef, UserKey );
  End;

  Procedure PrevNetKey (     IFBPtr     : IsamFileBlockPtr;
                             Key        : Integer;
                         Var UserDatRef : LongInt;
                         Var UserKey    : IsamKeyStr  );
  Begin
    BTPrevKey ( IFBPtr, Key, UserDatRef, UserKey );
  End;

  Procedure ClearNetKey ( IFBPtr : IsamFileBlockPtr;
                          Key    : Integer      );
  Begin
    BTClearKey ( IFBPtr, Key );
  End;

  Procedure NextDiffNetKey (     IFBPtr     : IsamFileBlockPtr;
                                 Key        : Integer;
                             Var UserDatRef : LongInt;
                             Var UserKey    : IsamKeyStr  );
  Begin
    BTNextDiffKey ( IFBPtr, Key, UserDatRef, UserKey );
  End;

  Procedure PrevDiffNetKey (     IFBPtr     : IsamFileBlockPtr;
                                 Key        : Integer;
                             Var UserDatRef : LongInt;
                             Var UserKey    : IsamKeyStr  );
  Begin
    BTPrevDiffKey ( IFBPtr, Key, UserDatRef, UserKey );
  End;

  Procedure GetNetRecInSpiteOfLock (     IFBPtr : IsamFileBlockPtr;
                                         RefNr  : LongInt;
                                     Var Dest                );
  Begin
    BTGetRec ( IFBPtr, RefNr, Dest, True );
  End;

  Procedure GetNetRecReadOnly (     IFBPtr : IsamFileBlockPtr;
                                    RefNr  : LongInt;
                                Var Dest                );
  Begin
    BTGetRecReadOnly ( IFBPtr, RefNr, Dest );
  End;


  Procedure AddNetRec (     IFBPtr : IsamFileBlockPtr;
                        Var RefNr  : LongInt;
                        Var Source                );
  Begin
    BTAddRec ( IFBPtr, RefNr, Source );
  End;

  Procedure PutNetRec (     IFBPtr : IsamFileBlockPtr;
                            RefNr  : LongInt;
                        Var Source                );
  Begin
    BTPutRec ( IFBPtr, RefNr, Source, False );
  End;

  Procedure DeleteNetRec ( IFBPtr : IsamFileBlockPtr;
                           RefNr  : LongInt   );
  Begin
    BTDeleteRec ( IFBPtr, RefNr );
  End;

  Procedure AddNetKey ( IFBPtr     : IsamFileBlockPtr;
                        Key        : Integer;
                        UserDatRef : LongInt;
                        UserKey    : IsamKeyStr  );
  Begin
    BTAddKey ( IFBPtr, Key, UserDatRef, UserKey );
  End;

  Procedure DeleteNetKey ( IFBPtr     : IsamFileBlockPtr;
                           Key        : Integer;
                           UserDatRef : LongInt;
                           UserKey    : IsamKeyStr  );
  Begin
    BTDeleteKey ( IFBPtr, Key, UserDatRef, UserKey );
  End;

  Procedure DeleteAllNetKeys ( IFBPtr : IsamFileBlockPtr;
                               Key    : Integer );
  Begin
    BTDeleteAllKeys ( IFBPtr, Key );
  End;

  Procedure PutNetRecInSpiteOfLock (     IFBPtr : IsamFileBlockPtr;
                                         RefNr  : LongInt;
                                     Var Source              );
  Begin
    BTPutRec ( IFBPtr, RefNr, Source, True );
  End;

  Procedure MakeNetFileBlock ( Var IFBPtr  : IsamFileBlockPtr;
                                   FName   : IsamFileBlockName;
                                   DatSLen : LongInt;
                                   AnzKey  : Integer;
                                   IID     : IsamIndDescr  );
  Begin
    BTCreateFileBlock ( FName, DatSLen, AnzKey, IID );
    If IsamOK Then Begin
      BTOpenFileBlock ( IFBPtr, FName, False, False, False, True );
    End;
  End;

  Procedure CloseNetFileBlock ( Var IFBPtr : IsamFileBlockPtr );
  Begin
    BTCloseFileBlock ( IFBPtr );
  End;

  Procedure CloseEachFileBlock;
  Begin
    BTCloseAllFileBlocks;
  End;

  Procedure OpenNetFileBlock ( Var IFBPtr : IsamFileBlockPtr;
                                   FName  : IsamFileBlockName );
  Begin
    BTOpenFileBlock ( IFBPtr, FName, False, False, False, True );
  End;

  Procedure OpenSaveNetFileBlock ( Var IFBPtr : IsamFileBlockPtr;
                                       FName  : IsamFileBlockName );
  Begin
    BTOpenFileBlock ( IFBPtr, FName, False, False, True, True );
  End;

  Procedure DeleteNetFileBlock ( FName  : IsamFileBlockName );
  Begin
    BTDeleteFileBlock ( FName );
  End;

  Function SetDosRetry ( NrOfRetries, WaitTime : Integer ) : Boolean;
  Begin
    SetDosRetry := BTSetDosRetry ( NrOfRetries, WaitTime );
  End;

  Function NetSupported : NetSupportType;
  Begin
    NetSupported := BTNetSupported; {!!}
  End;

  Function NoNetCompiled : Boolean;
  Begin
    NoNetCompiled := BTNoNetCompiled;
  End;

  Function IsNetFileBlock ( IFBPtr : IsamFileBlockPtr ) : Boolean;
  Begin
    IsNetFileBlock := BTIsNetFileBlock ( IFBPtr );
  End;

  Procedure GetApprNetRelPos (     IFBPtr     : IsamFileBlockPtr;
                                   Key        : Integer;
                               Var RelPos     : Word;
                                   Scale      : Word;
                                   UserKey    : IsamKeyStr;
                                   UserDatRef : LongInt );
  Begin
    BTGetApprRelPos ( IFBPtr, Key, RelPos, Scale, UserKey, UserDatRef );
  End;

  Procedure GetApprNetKeyAndRef (     IFBPtr     : IsamFileBlockPtr;
                                      Key        : Integer;
                                      RelPos     : Word;
                                      Scale      : Word;
                                  Var UserKey    : IsamKeyStr;
                                  Var UserDatRef : LongInt );
  Begin
    BTGetApprKeyAndRef ( IFBPtr, Key, RelPos, Scale, UserKey, UserDatRef );
  End;

  Procedure ForceNetBufferWriteThrough ( DoIt : Boolean );
  Begin
    BTForceNetBufferWriteThrough ( DoIt );
  End;

  {!!} {Routines that were in FILER.PAS but not in German version}

  function FileBlockLocked(IFBPtr : IsamFileBlockPtr) : Boolean;
  begin
    FileBlockLocked := BTFileBlockIsLocked ( IFBPtr );
  end;

  function FileNameFromIFBPtr(IFBPtr : IsamFileBlockPtr) : IsamFileBlockName;
  begin
    FileNameFromIFBPtr := BTDataFileName(IFBPtr);
  end;

  function PageStackValid(IFBPtr : IsamFileBlockPtr; KeyNr : Integer) : IsamPageStackState;
  begin
    if BTOtherWSChangedKey(IFBPtr, KeyNr) then
      PageStackValid := StateInvalid
    else
      PageStackValid := StateValid;
  end;

  procedure IsamFileSize(F : IsamFile; var Size : LongInt);
  begin
    IsamLongSeekEOF(F, Size);
  end;

  procedure RemoveActiveLocks(IFBPtr : IsamFileBlockPtr);
  begin
    BTUnlockAllRecs(IFBPtr);
    BTUnLockFileBlock(IFBPtr);
  end;

{$IFDEF InitAllUnits}
Begin
{$ENDIF}
End.
