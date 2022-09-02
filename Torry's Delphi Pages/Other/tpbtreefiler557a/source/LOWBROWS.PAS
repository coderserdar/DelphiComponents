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
  {$I BRDefOpt.Inc}
  {$F-,V-,B-,S-,I-,R-}
  {$IFDEF CanAllowOverlays}
    {$O+,F+}
  {$ENDIF}
  {$IFDEF CanSetOvrflowCheck}
    {$Q-}
  {$ENDIF}


Unit LowBrows;

Interface

Uses
  BTBase,
  BTIsBase,
  Filer,     {!!.TP}
{$IFDEF BRUseShell}
  OPSBase,
  OOPShell;
{$ENDIF}
{$IFDEF BRUseIsam}
  VRec;      {!!.TP}
{$ENDIF}

Const
  BRCurrentlyLocked = -1;
  BRNoFilterResult  = -2;
  BRFilterError     = -3;                                              {!!.42}
  BRUserStatStart   = -10;
  MaxCols           = 128;
  BRLMaxRows        = 255;
  MaxEltsPerRow     = 8;
  BRUnbreakedOpers  = 63;
  BRBreakDelay      = 50;
{$IFDEF BRUseShell}
  NoError           = OPSBase.NoError;
  DialogError       = OPSBase.DialogError;
  LockError         = OPSBase.LockError;
  GenMaxKeyLen      = IFDMaxKeyLen;
  GenMaxPosRef      = IFDMaxPossibleRef;
{$ENDIF}
{$IFDEF BRUseIsam}
  NoError           = 0;
  DialogError       = 1;
  LockError         = 2;
  GenMaxKeyLen      = MaxKeyLen;
  GenMaxPosRef      = $7FFFFFFF;
{$ENDIF}

Type
  BRLRowEltString  = String [MaxCols];
  PBRLRowEltString = ^BRLRowEltString;
  RowString        = BRLRowEltString;
  BRLRowEltStrArr  = Array [1..MaxEltsPerRow] Of BRLRowEltString;
  GenKeyStr        = String [GenMaxKeyLen];

  RowRec = Record
  {-Read only part}
    IKS         : GenKeyStr;
    Ref         : LongInt;
    Status      : Integer;
  {-Internal use only}
    RowModified : Boolean;
    RowBuilt    : Boolean;
  {-To manipulate part}
    Case Boolean Of
      True  : (
        Row : RowString;
      );
      False : (
        RowElt : BRLRowEltStrArr;
      );
  End;

  RowRecPtr = ^RowRec;

  BRBrowScreen = Array [1..BRLMaxRows] Of RowRecPtr;

  BRLBrowser = Object
    VarRecMaxReadLen     : Word;
    KeyNr                : Word;
  {--The following fields are set by the constructor only}
    DelayTimeOnGetRec    : Word;
    RetriesOnGetRec      : Word;
  {$IFDEF BRUseShell}
    UsedDriver           : IFDriverPtr;
    SaveRecord           : DStat;
  {$ENDIF}
  {$IFDEF BRUseIsam}
    UsedFileBlock        : IsamFileBlockPtr;
    DataBuffer           : Pointer;
    VariableRecs         : Boolean;
    LastVarRecLen        : Word;
  {$ENDIF}
  {--The following fields are private use only}
    ReadLockOpersCount   : Word;
    NoReadLockCount      : Word;
    UseReadLock          : Boolean;
    BSAPtr               : ^BRBrowScreen;
    AllocNrOfEltsPerRow  : Word;
    AllocNrOfRows        : Word;

    Constructor Init (     DrvOrFileBlockPtr   : Pointer;
                           NumberOfEltsPerRow  : Word;
                           NumberOfRows        : Word;
                           DelayOnGetRec       : Word;
                           RetrOnGetRec        : Word;
                       Var DatS;
                           IsVarRec            : Boolean );
    Destructor Done; virtual;
    Procedure SetDefaults;
    Function BrowserCallAllowed : Boolean;

  {--The following functions may be overwritten in descending objects}
    Function PerformFilter ( Var RR    : RowRec;
                             Var UseIt : Boolean ) : Integer; Virtual;

  {--The following functions have more privat nature, but may be overwritten in
     descending objects}
    Function BRSetReadLockUsage ( Var UseRL : Boolean ) : Integer; Virtual;
    Function BRGetRec ( Var RR       : RowRec;
                            AskUser,
                            ReadFull : Boolean ) : Integer; Virtual;
  {$IFDEF BRUseShell}
    Function BRGetRootRec ( RootDriverPtr : IFDriverPtr;
                            VarRecReadLen : Word;
                            UseSecond     : Boolean ) : Integer; Virtual;
  {$ENDIF}
    Function BRNextKey ( Var RR : RowRec ) : Integer; Virtual;
    Function BRPrevKey ( Var RR : RowRec ) : Integer; Virtual;
    Function BRFindKeyAndRef ( Var RR   : RowRec;
                                   NFSD : Integer ) : Integer; Virtual;
    Function BRFindKeyAndRefNoFilter ( Var RR   : RowRec;
                                           NFSD : Integer ) : Integer;
                                                                       {!!.42}
    Function BRDoReadLock : Integer; Virtual;
    Function BRDoUnLock : Integer; Virtual;

  {--The following routines are internal use only}
    Function KeyInBounds ( Var KeyStr : GenKeyStr ) : Boolean; Virtual;
                                                                       {!!.41}
    Function BRCallFilter ( Var RR    : RowRec;
                            Var UseIt : Boolean ) : Integer;
    Function BRLDoReadLock : Integer;
    Function BRLDoUnLock : Integer;
    Function BRLLockDetected : Boolean;
    Function BRLSaveStatus : Integer; Virtual;
    Function BRLRestoreStatus : Integer; Virtual;
    Function BRLAllocBrowScreen ( NumberOfEltsPerRow : Word;
                                  NumberOfRows       : Word ) : Boolean;
    Procedure BRLDeAllocBrowScreen;
    Function BRLGetApprKeyAndRef (     RelPos     : Word;
                                       Scale      : Word;
                                   Var UserKey    : GenKeyStr;
                                   Var UserDatRef : LongInt ) : Integer;
    Function BRLGetApprRelPos ( Var RelPos     : Word;
                                    Scale      : Word;
                                    UserKey    : GenKeyStr;
                                    UserDatRef : LongInt ) : Integer;
    Function BRLUsedKeys ( Var UK : LongInt ) : Integer;
    Procedure PrivatSetDefaults;
  End;


Implementation

  Constructor BRLBrowser.Init (     DrvOrFileBlockPtr   : Pointer;
                                    NumberOfEltsPerRow  : Word;
                                    NumberOfRows        : Word;
                                    DelayOnGetRec       : Word;
                                    RetrOnGetRec        : Word;
                                Var DatS;
                                    IsVarRec            : Boolean );

  Begin
    PrivatSetDefaults;
  {$IFDEF BRUseShell}
    UsedDriver := IFDriverPtr (DrvOrFileBlockPtr);
    UsedDriver^.IFDOpenFile;
    If MaxErrorClass <> NoError Then Fail;
  {$ENDIF}
  {$IFDEF BRUseIsam}
    UsedFileBlock := IsamFileBlockPtr (DrvOrFileBlockPtr);
    VariableRecs := IsVarRec;
    DataBuffer := @DatS;
  {$ENDIF}
    DelayTimeOnGetRec := DelayOnGetRec;
    RetriesOnGetRec := RetrOnGetRec;
    If Not BRLAllocBrowScreen ( NumberOfEltsPerRow, NumberOfRows )
        Then Fail;
  End;


  Destructor BRLBrowser.Done;

  Begin
    BRLDeAllocBrowScreen;
  End;


  Procedure BRLBrowser.SetDefaults;

  Begin
    PrivatSetDefaults;
  End;


  Procedure BRLBrowser.PrivatSetDefaults;

  Begin
  {$IFDEF BRUseShell}
    VarRecMaxReadLen := IFDMaxVariableRecLength;
  {$ENDIF}
  {$IFDEF BRUseIsam}
    VarRecMaxReadLen := MaxVariableRecLength;
  {$ENDIF}
    KeyNr := 0;
    NoReadLockCount := 0;
    ReadLockOpersCount := 0;
  End;


  Function BRLBrowser.KeyInBounds ( Var KeyStr : GenKeyStr ) : Boolean;
                                                                       {!!.41}
    {-Must internally be overridden in a descending object}
  Begin
    KeyInBounds := True;
  End;


  Function BRLBrowser.BRCallFilter ( Var RR    : RowRec;
                                     Var UseIt : Boolean ) : Integer;

  Var
    LResult : Integer;                                                 {!!.51}

  Begin
    RR.RowBuilt := False;
    If Not KeyInBounds ( RR.IKS ) Then Begin                           {!!.41}
      {-Avoid testing the filter condition, if the key is not in bounds}
      BRCallFilter := DialogError;                                     {!!.41}
      Exit;                                                            {!!.41}
    End;                                                               {!!.41}
    LResult := PerformFilter ( RR, UseIt );                            {!!.51}
    If LResult = NoError Then Begin                                    {!!.51}
      Inc (ReadLockOpersCount);
      If ReadLockOpersCount > BRUnbreakedOpers Then Begin
        {-Give another station a chance to lock the fileblock}
        ReadLockOpersCount := 0;
        If UseReadLock And BRLLockDetected Then Begin
          LResult := BRDoUnLock;                                       {!!.51}
          If LResult = NoError Then Begin                              {!!.51}
            IsamDelay ( BRBreakDelay );
            LResult := BRDoReadLock;                                   {!!.51}
          End;
        End;
      End;
    End;
    BRCallFilter := LResult;                                           {!!.51}
  End;


  Function BRLBrowser.BRLDoReadLock : Integer;

  Begin
    ReadLockOpersCount := 0;
    If BRLLockDetected Then Begin
      Inc (NoReadLockCount);
      BRLDoReadLock := NoError;
    End Else Begin
      BRLDoReadLock := BRDoReadLock;
    End;
  End;


  Function BRLBrowser.BRLDoUnLock : Integer;

  Begin
    If NoReadLockCount = 0 Then Begin
      BRLDoUnLock := BRDoUnLock;
    End Else Begin
      Dec (NoReadLockCount);
      BRLDoUnLock := NoError;
    End;
  End;


  Function BRLBrowser.BRLAllocBrowScreen ( NumberOfEltsPerRow : Word;
                                           NumberOfRows       : Word )
                                                                 : Boolean;

  Var
    I          : Word;
    RowRecSize : Word;


    Procedure UnDo ( NrToDeAlloc : Word );

    Var
      I : Word;

    Begin
      For I := NrToDeAlloc DownTo 1 Do Begin
        FreeMem ( BSAPtr^ [I], RowRecSize );
      End;
      FreeMem ( BSAPtr, SizeOf (Pointer) * NumberOfRows );
    End;


  Begin
    BRLAllocBrowScreen := False;
    if not IsamGetMem ( BSAPtr, SizeOf (Pointer) * NumberOfRows ) then Exit;
    RowRecSize := SizeOf (RowRec)
        - (MaxEltsPerRow - NumberOfEltsPerRow) * SizeOf (BRLRowEltString);
    For I := 1 To NumberOfRows Do Begin
      If not IsamGetMem ( BSAPtr^ [I], RowRecSize ) Then Begin
        UnDo ( Pred (I) );
        Exit;
      End;
    End;
    AllocNrOfEltsPerRow := NumberOfEltsPerRow;
    AllocNrOfRows := NumberOfRows;
    BRLAllocBrowScreen := True;
  End;


  Procedure BRLBrowser.BRLDeAllocBrowScreen;

  Var
    I          : Word;
    RowRecSize : Word;

  Begin
    RowRecSize := SizeOf (RowRec)
        - (MaxEltsPerRow - AllocNrOfEltsPerRow) * SizeOf (BRLRowEltString);
    For I := AllocNrOfRows DownTo 1 Do Begin
      FreeMem ( BSAPtr^ [I], RowRecSize );
    End;
    FreeMem ( BSAPtr, SizeOf (Pointer) * AllocNrOfRows );
    BSAPtr := Nil;
  End;


  Function BRLBrowser.PerformFilter ( Var RR    : RowRec;
                                      Var UseIt : Boolean ) : Integer;

  Begin
    UseIt := True;
    PerformFilter := NoError;
  End;


{$IFDEF BRUseShell}
  {$I BRLShell.Inc}
{$ENDIF}
{$IFDEF BRUseIsam}
  {$I BRLIsam.Inc}
{$ENDIF}

{$IFDEF InitAllUnits}
Begin
{$ENDIF}
End.
