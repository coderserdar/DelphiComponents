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


Unit MedBrows;

Interface

Uses
  LowBrows;

Type
  BrowScreenState = Record
    STCurRow : Word;
    STNrOfRows : Word;
    STLastRow : Word;
  End;

  PBRMBrowser = ^BRMBrowser;
  BRMBrowser = Object ( BRLBrowser )
    NrOfRows        : Word;
    NrOfEltsPerRow  : Word;
    DefNrOfRows     : Word;
    LowKey,
    HighKey         : GenKeyStr;
    CurRow          : Word;
    SaveStatus      : Boolean;

    Constructor Init (     DrvOrFileBlockPtr  : Pointer;
                           NumberOfEltsPerRow : Word;
                           NumberOfRows       : Word;
                           DelayOnGetRec      : Word;
                           RetrOnGetRec       : Word;
                           DefaultNrOfRows    : Word;
                           LKey,
                           HKey               : GenKeyStr;
                           SaveStat           : Boolean;
                       Var DatS;
                           IsVarRec           : Boolean );
    Destructor Done; virtual;
    Procedure SetDefaults;

    Procedure GetBrowScreenState ( Var BST : BrowScreenState );
    Function BrowScreenStateChanged ( Var BST : BrowScreenState ) : Boolean;

  {--The following functions must be overwritten in descending objects}
    Function BuildRow ( Var RR : RowRec ) : Integer; Virtual;

  {--The following functions may be overwritten in descending objects}
    Function PreCompletePage : Integer; Virtual;
    Function PostCompletePage : Integer; Virtual;

  {--The following routines must not be overwritten in descending objects}
    Function CompleteThisPage ( Var Changed  : Boolean;
                                    StartRow,
                                    EndRow   : Word ) : Integer; Virtual;
    Function BuildFirstPage ( Var Changed : Boolean ) : Integer; Virtual;
    Function BuildLastPage ( Var Changed : Boolean ) : Integer; Virtual;
    Function BuildNextPage (     Nr           : Word;
                             Var Moved        : Word;
                                 CompletePage : Boolean;
                                 ModifyCurRow : Word;
                             Var Changed      : Boolean ) : Integer; Virtual;
    Function BuildPrevPage (     Nr           : Word;
                             Var Moved        : Word;
                                 CompletePage : Boolean;
                                 ModifyCurRow : Word;
                             Var Changed      : Boolean ) : Integer; Virtual;
    Function BuildThisPage ( Var Changed : Boolean ) : Integer; Virtual;

  {--The following routines may be called from overridden routines}
    Function ExpandPage ( NewNrOfRows : Word ) : Integer;
    Function ShrinkPage ( NewNrOfRows : Word ) : Integer;
    Function BuildNewPage (     NewKeyNr    : Word;
                                NewKeyStr   : GenKeyStr;
                                NewRef      : LongInt;
                                NewCurRow   : Word;
                                NewNrOfRows : Word;
                            Var Changed     : Boolean ) : Integer;
    Function GetLastRow : Word;                                        {!!.41b}
    Function GetFirstRow : Word;                                       {!!.41b}
    Procedure SetCurRow ( NewValue : Word );
    Function GetCurRow : Word;
    Procedure SetNrOfRows ( NewValue : Word );
    Function GetNrOfRows : Word;
    Function GetNrOfEltsPerRow : Word;
    Function GetCurrentKeyStr : GenKeyStr;
    Function GetCurrentDatRef : LongInt;
    Function GetRowMatchingRec ( Var RR       : RowRec;
                                     AskUser,
                                     ReadFull : Boolean;
                                 Var Match    : Boolean ) : Integer;
    Function BuildBrowScreenRow ( Var RR : RowRec ) : Integer;

  {--The following routines are internal use only}
    Procedure EmptyRowDescr ( Var RR : RowRec );
    Procedure EmptyRowElts ( Var RR : RowRec );
    Procedure EmptyBrowScreen ( Start    : Word;
                                EmptyRow : Boolean );
    Procedure ResetRowModified ( Start : Word );
    Function KeyInBounds ( Var KeyStr : GenKeyStr ) : Boolean; Virtual;
                                                                       {!!.41}
    Procedure BoundForceKeyAndRef ( Var KeyStr : GenKeyStr;
                                    Var Ref    : LongInt );
    Procedure AdjustCurRow ( Var CR : Word; UseLastRow : Boolean );
    Procedure AdjustNrOfRows ( Var NOR : Word );
    Procedure CopyRowRec ( Var RRSource, RRDest : RowRec );
    Procedure CopyRowRecDescr ( Var RRSource, RRDest : RowRec );
    Procedure CopyRowRecDep ( Var RRSource, RRDest : RowRec );
    Procedure PlaceAtEnd ( Var RR : RowRec );
    Procedure PlaceAtTop ( Var RR : RowRec );
    Function RowEltModified ( Var RRMod, RROrg : RowRec ) : Boolean;
    Procedure PrivatSetDefaults;
  End;


Implementation

  Constructor BRMBrowser.Init (     DrvOrFileBlockPtr  : Pointer;
                                    NumberOfEltsPerRow : Word;
                                    NumberOfRows       : Word;
                                    DelayOnGetRec      : Word;
                                    RetrOnGetRec       : Word;
                                    DefaultNrOfRows    : Word;
                                    LKey,
                                    HKey               : GenKeyStr;
                                    SaveStat           : Boolean;
                                Var DatS;
                                    IsVarRec           : Boolean );

  Begin
    If Not BRLBrowser.Init ( DrvOrFileBlockPtr, NumberOfEltsPerRow,
        NumberOfRows, DelayOnGetRec, RetrOnGetRec, DatS, IsVarRec )
        Then Fail;
    If BRSetReadLockUsage ( UseReadLock ) <> NoError Then Fail;
    EmptyBrowScreen ( 1, True );
    DefNrOfRows := DefaultNrOfRows;
    If (DefNrOfRows < 1) Or (DefNrOfRows > AllocNrOfRows) Then Begin
      DefNrOfRows := AllocNrOfRows;
    End;
    LowKey := LKey;
    HighKey := HKey;
    SaveStatus := SaveStat;
    PrivatSetDefaults;
  End;


  Destructor BRMBrowser.Done;

  Begin
    BRLBrowser.Done;
  End;


  Procedure BRMBrowser.SetDefaults;

  Begin
    BRLBrowser.SetDefaults;
    PrivatSetDefaults;
  End;


  Procedure BRMBrowser.PrivatSetDefaults;

  Begin
    NrOfRows := AllocNrOfRows;
    NrOfEltsPerRow := AllocNrOfEltsPerRow;
    CurRow := 1;
  End;


  Procedure BRMBrowser.GetBrowScreenState ( Var BST : BrowScreenState );

  Begin
    With BST Do Begin
      STCurRow := CurRow;
      STNrOfRows := NrOfRows;
      STLastRow := GetLastRow;
    End;
  End;


  Function BRMBrowser.BrowScreenStateChanged ( Var BST : BrowScreenState )
                                                                   : Boolean;

  Begin
    BrowScreenStateChanged := True;
    With BST Do Begin
      If STCurRow <> GetCurRow Then Exit;
      If STNrOfRows <> NrOfRows Then Exit;
      If STLastRow <> GetLastRow Then Exit;
    End;
    BrowScreenStateChanged := False;
  End;


  Function BRMBrowser.PreCompletePage : Integer;

  Begin
    PreCompletePage := NoError;
  End;


  Function BRMBrowser.PostCompletePage : Integer;

  Begin
    PostCompletePage := NoError;
  End;


  Function BRMBrowser.GetLastRow : Word;

  Var
    I : Word;

  Begin
    I := NrOfRows;
    While (I > 0) And (BSAPtr^ [I]^.Ref = 0) Do Begin
      Dec (I);
    End;
    GetLastRow := I;
  End;


  Function BRMBrowser.GetFirstRow : Word;

  Var
    I : Word;

  Begin
    I := 1;
    While (I <= NrOfRows) And (BSAPtr^ [I]^.Ref = 0) Do Begin
      Inc (I);
    End;
    GetFirstRow := I;
  End;


  Procedure BRMBrowser.EmptyRowDescr ( Var RR : RowRec );

  Begin
    With RR Do Begin
      IKS := '';
      Ref := 0;
      Status := 0;
    End;
  End;


  Procedure BRMBrowser.EmptyRowElts ( Var RR : RowRec );

  Var
    I : Word;

  Begin
    With RR Do Begin
      For I := 1 To AllocNrOfEltsPerRow Do Begin
        RowElt [I] := '';
      End;
      RowModified := False;
      RowBuilt := False;
    End;
  End;


  Procedure BRMBrowser.EmptyBrowScreen ( Start    : Word;
                                         EmptyRow : Boolean );

  Var
    I : Word;

  Begin
    For I := Start To AllocNrOfRows Do Begin
      If EmptyRow Then EmptyRowElts ( BSAPtr^ [I]^ );
      EmptyRowDescr ( BSAPtr^ [I]^ );
    End;
  End;


  Procedure BRMBrowser.ResetRowModified ( Start : Word );

  Var
    I : Word;

  Begin
    For I := Start To AllocNrOfRows Do Begin
      BSAPtr^ [I]^.RowModified := False;
    End;
  End;


  Function BRMBrowser.KeyInBounds ( Var KeyStr : GenKeyStr ) : Boolean;

  Begin
    KeyInBounds := False;
    If KeyNr <> 0 Then Begin
      If Copy ( KeyStr, 1, Length (LowKey) ) < LowKey Then Exit;
      If Copy ( KeyStr, 1, Length (HighKey) ) > HighKey Then Exit;
    End;
    KeyInBounds := True;
  End;


  Procedure BRMBrowser.BoundForceKeyAndRef ( Var KeyStr : GenKeyStr;
                                             Var Ref    : LongInt );

  Begin
    If Not KeyInBounds ( KeyStr ) Then Begin
      If Copy ( KeyStr, 1, Length (LowKey) ) < LowKey Then Begin
        KeyStr := LowKey;
        Ref := 0;
      End Else Begin
        KeyStr := HighKey;
        While Length (KeyStr) < GenMaxKeyLen Do Begin
          KeyStr := KeyStr + #255;
        End;
        Ref := GenMaxPosRef;
      End;
    End;
  End;


  Procedure BRMBrowser.AdjustCurRow ( Var CR : Word; UseLastRow : Boolean );

  Begin
    If CR < 1 Then Begin
      CR := 1;
    End Else Begin
      If UseLastRow Then Begin
        If CR > GetLastRow Then CR := GetLastRow;
        If CR = 0 Then CR := 1;
      End Else Begin
        If CR > NrOfRows Then CR := NrOfRows;
      End;
    End;
  End;


  Procedure BRMBrowser.AdjustNrOfRows ( Var NOR : Word );

  Begin
    If (NOR < 1) Or (NOR > AllocNrOfRows) Then Begin
      NOR := AllocNrOfRows;                                            {!!.41}
    End;
  End;


  Procedure BRMBrowser.SetCurRow ( NewValue : Word );

  Begin
    AdjustCurRow ( NewValue, True );
    CurRow := NewValue;
  End;


  Function BRMBrowser.GetCurRow : Word;

  Begin
    GetCurRow := CurRow;
  End;


  Procedure BRMBrowser.SetNrOfRows ( NewValue : Word );

  Begin
    AdjustNrOfRows ( NewValue );
    NrOfRows := NewValue;
  End;


  Function BRMBrowser.GetNrOfRows : Word;

  Begin
    GetNrOfRows := NrOfRows;
  End;


  Function BRMBrowser.GetNrOfEltsPerRow : Word;

  Begin
    GetNrOfEltsPerRow := NrOfEltsPerRow;
  End;


  Procedure BRMBrowser.CopyRowRec ( Var RRSource, RRDest : RowRec );

  Begin
    Move ( RRSource, RRDest, SizeOf (RowRec)
        - (MaxEltsPerRow - AllocNrOfEltsPerRow) * SizeOf (BRLRowEltString) );
  End;


  Procedure BRMBrowser.CopyRowRecDescr ( Var RRSource, RRDest : RowRec );

  Begin
    With RRDest Do Begin
      IKS := RRSource.IKS;
      Ref := RRSource.Ref;
      Status := RRSource.Status;
    End;
  End;


  Procedure BRMBrowser.CopyRowRecDep ( Var RRSource, RRDest : RowRec );

  Begin
    If RRSource.RowBuilt Then Begin
      If Not RowEltModified ( RRSource, RRDest )
          Then RRSource.RowModified := False;
      CopyRowRec ( RRSource, RRDest );
    End Else Begin
      CopyRowRecDescr ( RRSource, RRDest );
    End;
  End;


  Procedure BRMBrowser.PlaceAtEnd ( Var RR : RowRec );

  Var
    I : Word;

  Begin
    I := GetLastRow;
    If I < NrOfRows Then Begin
      CopyRowRecDep ( RR, BSAPtr^ [Succ (I)]^ );
    End Else Begin
      For I := 1 To Pred (NrOfRows) Do Begin
        CopyRowRec ( BSAPtr^ [Succ (I)]^, BSAPtr^ [I]^ );
      End;
      CopyRowRec ( RR, BSAPtr^ [NrOfRows]^ );
    End;
  End;


  Procedure BRMBrowser.PlaceAtTop ( Var RR : RowRec );

  Var
    I : Word;

  Begin
    I := GetFirstRow;
    If I > 1 Then Begin
      CopyRowRecDep ( RR, BSAPtr^ [Pred (I)]^ );
    End Else Begin
      For I := NrOfRows DownTo 2 Do Begin
        CopyRowRec ( BSAPtr^ [Pred (I)]^, BSAPtr^ [I]^ );
      End;
      CopyRowRec ( RR, BSAPtr^ [1]^ );
    End;
  End;


  Function BRMBrowser.RowEltModified ( Var RRMod, RROrg : RowRec ) : Boolean;

  Var
    I : Word;

  Begin
    RowEltModified := True;
    For I := 1 To NrOfEltsPerRow Do Begin
      If RRMod.RowElt [I] <> RROrg.RowElt [I] Then Exit;
    End;
    RowEltModified := False;
  End;


  Function BRMBrowser.CompleteThisPage ( Var Changed  : Boolean;
                                             StartRow,
                                             EndRow   : Word ) : Integer;

  Var
    Dummy,
    LResult : Integer;                                                 {!!.51}


    Function CompleteaPage ( Var Changed : Boolean ) : Integer;

    Var
      Dummy : Integer;


      Function ErrorReturned ( Err : Integer ) : Boolean;

      Begin
        CompleteaPage := Err;
        If Err <> NoError Then Begin
          ErrorReturned := True;
        End Else Begin
          ErrorReturned := False;
        End;
      End;


      Function CompleteaPageInner : Integer;

      Var
        LResult     : Integer;                                         {!!.51}
        I          : Word;
        SaveRowRec : RowRec;


      Begin
        Changed := False;
        For I := StartRow To EndRow Do Begin
          With BSAPtr^ [I]^ Do Begin
            If Ref > 0 Then Begin
              {-Negative values may not be read}
              If Not RowBuilt Then Begin
                If Status > BRUserStatStart Then Begin
                  LResult := BRGetRec ( BSAPtr^ [I]^, False, False );  {!!.51}
                    {-Don't ask user on lock error;
                      read only varrecmaxreadlen bytes}
                  CompleteaPageInner := LResult;                       {!!.51}
                  If LResult > LockError Then Exit;                    {!!.51}
                  If LResult <> NoError Then Status := BRCurrentlyLocked; {!!.51}
                End;
                LResult := BuildBrowScreenRow ( BSAPtr^ [I]^ );        {!!.51}
                  {-Sets RowBuilt to True and RowModified accordingly}
                CompleteaPageInner := LResult;                         {!!.51}
                If LResult <> NoError Then Exit;                       {!!.51}
              End;
              RowBuilt := False;
              Changed := Changed Or RowModified;
            End Else Begin
              EmptyRowElts ( SaveRowRec );
              BSAPtr^ [I]^.RowModified :=
                  RowEltModified ( BSAPtr^ [I]^, SaveRowRec );
              If BSAPtr^ [I]^.RowModified Then Begin
                EmptyRowElts ( BSAPtr^ [I]^ );
                EmptyRowDescr ( BSAPtr^ [I]^ );
                Changed := True;
              End;
            End;
          End;
        End;
        ResetRowModified ( 1 );
        CompleteaPageInner := NoError;
      End;


    Begin
      If UseReadLock Then Begin
        If ErrorReturned ( BRLDoReadLock ) Then Begin
          Exit;
        End;
        If ErrorReturned ( CompleteaPageInner ) Then Begin
          Dummy := BRLDoUnLock;
          Exit;
        End;
        If ErrorReturned ( BRLDoUnLock ) Then Exit;
      End Else Begin
        If ErrorReturned ( CompleteaPageInner ) Then Exit;
      End;
    End;


  Begin
    LResult := PreCompletePage;                                        {!!.51}
    If LResult <> NoError Then Begin                                   {!!.51}
      CompleteThisPage := LResult;                                     {!!.51}
      Exit;
    End;
    If SaveStatus Then Dummy := BRLSaveStatus;
    LResult := CompleteaPage ( Changed );                              {!!.51}
    If SaveStatus Then Dummy := BRLRestoreStatus;
    If LResult <> NoError Then Begin                                   {!!.51}
      CompleteThisPage := LResult;                                     {!!.51}
      Dummy := PostCompletePage;
      Exit;
    End;
    CompleteThisPage := PostCompletePage;
  End;


  Function BRMBrowser.BuildFirstPage ( Var Changed : Boolean ) : Integer;

  Var
    Dummy : Integer;


    Function ErrorReturned ( Err : Integer ) : Boolean;

    Begin
      BuildFirstPage := Err;
      If Err <> NoError Then Begin
        ErrorReturned := True;
      End Else Begin
        ErrorReturned := False;
      End;
    End;


    Function BuildFirstPageInner : Integer;

    Var
      I,
      LResult : Integer;                                               {!!.51}
      TRec   : RowRec;

    Begin
      EmptyBrowScreen ( 1, False );
      CurRow := 1;
      With TRec Do Begin
        EmptyRowElts ( TRec );
        IKS := LowKey;
        Ref := 0;
        Status := 0;
      End;
      LResult := BRFindKeyAndRef ( TRec, 1 );                          {!!.51}
      BuildFirstPageInner := LResult;                                  {!!.51}
      If LResult <> NoError Then Exit;                                 {!!.51}
      If Not KeyInBounds ( TRec.IKS ) Then Begin
        BuildFirstPageInner := DialogError;
        Exit;
      End;
      I := 1;
      While (I <= NrOfRows) And (KeyInBounds ( TRec.IKS ))
          And (LResult = NoError) Do Begin                             {!!.51}
        CopyRowRecDep ( TRec, BSAPtr^ [I]^ );
        EmptyRowElts ( TRec );
        Inc (I);
        If I <= NrOfRows Then Begin
          LResult := BRNextKey ( TRec );                               {!!.51}
          If LResult > DialogError Then Begin                          {!!.51}
            BuildFirstPageInner := LResult;                            {!!.51}
            Exit;
          End;
        End;
      End;
      If I <= NrOfRows Then EmptyBrowScreen ( I, True );
      BuildFirstPageInner := NoError;
    End;


  Begin
    Changed := True;
    If UseReadLock Then Begin
      If ErrorReturned ( BRLDoReadLock ) Then Exit;
      If ErrorReturned ( BuildFirstPageInner ) Then Begin
        Dummy := BRLDoUnLock;
        Exit;
      End;
      If ErrorReturned ( BRLDoUnLock ) Then Exit;
    End Else Begin
      If ErrorReturned ( BuildFirstPageInner ) Then Exit;
    End;
    If ErrorReturned ( CompleteThisPage ( Changed, 1, NrOfRows ) ) Then;
  End;


  Function BRMBrowser.BuildLastPage ( Var Changed : Boolean ) : Integer;

  Var
    Dummy : Integer;


    Function ErrorReturned ( Err : Integer ) : Boolean;

    Begin
      BuildLastPage := Err;
      If Err <> NoError Then Begin
        ErrorReturned := True;
      End Else Begin
        ErrorReturned := False;
      End;
    End;


    Function BuildLastPageInner : Integer;

    Var
      I,
      LResult : Integer;                                               {!!.51}
      FRow,
      LRow   : Word;
      TRec   : RowRec;
      Stop   : Boolean;

    Begin
      LRow := GetLastRow;
      If LRow = 0 Then Inc (LRow);
      EmptyBrowScreen ( 1, False );
      With TRec Do Begin
        EmptyRowElts ( TRec );
        IKS := HighKey;
        While Length (IKS) < GenMaxKeyLen Do Begin
          IKS := IKS + #255;
        End;
        Status := 0;
        Ref := GenMaxPosRef;
        LResult := BRFindKeyAndRef ( TRec, -1 );                       {!!.51}
        BuildLastPageInner := LResult;                                 {!!.51}
        If LResult <> NoError Then Exit;                               {!!.51}
        If Not KeyInBounds ( IKS ) Then Begin
          BuildLastPageInner := DialogError;
          Exit;
        End;
        CopyRowRecDep ( TRec, BSAPtr^ [LRow]^ );
        I := Pred (NrOfRows);
        Repeat
          EmptyRowElts ( TRec );
          LResult := BRPrevKey ( TRec );                               {!!.51}
          If LResult > DialogError Then Begin                          {!!.51}
            BuildLastPageInner := LResult;                             {!!.51}
            Exit;
          End;
          Dec (I);
          If (KeyInBounds ( IKS )) And (LResult = NoError) Then Begin  {!!.51}
            PlaceAtTop ( TRec );
            Stop := False;
          End Else Begin
            Stop := True;
          End;
        Until Stop Or (I = 0);
      End;
      FRow := GetFirstRow;
      If FRow <> 1 Then Begin
        LRow := GetLastRow;
        For I := 1 To Succ (LRow - FRow) Do Begin
          CopyRowRec ( BSAPtr^ [Pred (I + FRow)]^, BSAPtr^ [I]^ );
        End;
        EmptyBrowScreen ( LRow - FRow + 2, False );
      End;
      BuildLastPageInner := NoError;
      CurRow := GetLastRow;
    End;


  Begin
    If UseReadLock Then Begin
      If ErrorReturned ( BRLDoReadLock ) Then Exit;
      If ErrorReturned ( BuildLastPageInner ) Then Begin
        Dummy := BRLDoUnLock;
        Exit;
      End;
      If ErrorReturned ( BRLDoUnLock ) Then Exit;
    End Else Begin
      If ErrorReturned ( BuildLastPageInner ) Then Exit;
    End;
    If ErrorReturned ( CompleteThisPage ( Changed, 1, NrOfRows ) ) Then;
  End;


  Function BRMBrowser.BuildNextPage (     Nr           : Word;
                                      Var Moved        : Word;
                                          CompletePage : Boolean;
                                          ModifyCurRow : Word;
                                      Var Changed      : Boolean ) : Integer;

  Var
    Dummy : Integer;


    Function ErrorReturned ( Err : Integer ) : Boolean;

    Begin
      BuildNextPage := Err;
      If Err <> NoError Then Begin
        ErrorReturned := True;
      End Else Begin
        ErrorReturned := False;
      End;
    End;


    Function BuildNextPageInner : Integer;

    Var
      I,
      LResult : Integer;                                               {!!.51}
      TRec   : RowRec;

    Begin
      Moved := 0;
      I := GetLastRow;
      If I = 0 Then Begin
        BuildNextPageInner := DialogError;
        Exit;
      End;
      CopyRowRec ( BSAPtr^ [I]^, TRec );
      EmptyRowElts ( TRec );
      With TRec Do Begin
        Status := 0;
        LResult := BRFindKeyAndRef ( TRec, 0 );                        {!!.51}
        If LResult > DialogError Then Begin                            {!!.51}
          BuildNextPageInner := LResult;                               {!!.51}
          Exit;
        End;
        LResult := BRNextKey ( TRec );                                 {!!.51}
        If LResult > DialogError Then Begin                            {!!.51}
          BuildNextPageInner := LResult;                               {!!.51}
          Exit;
        End;
        I := 1;
        While (I <= Nr) And (KeyInBounds ( IKS ))
            And (LResult = NoError) Do Begin                           {!!.51}
          PlaceAtEnd ( TRec );
          EmptyRowElts ( TRec );
          Inc (I);
          If I <= Nr Then Begin
            LResult := BRNextKey ( TRec );                             {!!.51}
            If LResult > DialogError Then Begin                        {!!.51}
              BuildNextPageInner := LResult;                           {!!.51}
              Exit;
            End;
          End;
        End;
      End;
      Moved := Pred (I);
      If ModifyCurRow > 0 Then Begin
        If Moved = 0 Then Begin
          CurRow := GetLastRow;
        End Else Begin
          If ModifyCurRow > 1 Then Begin
            CurRow := Succ (GetLastRow) - Moved;
          End;
        End;
      End;
      BuildNextPageInner := NoError;
    End;


  Begin
    Changed := True;
    If UseReadLock Then Begin
      If ErrorReturned ( BRLDoReadLock ) Then Exit;
      If ErrorReturned ( BuildNextPageInner ) Then Begin
        Dummy := BRLDoUnLock;
        Exit;
      End;
      If ErrorReturned ( BRLDoUnLock ) Then Exit;
    End Else Begin
      If ErrorReturned ( BuildNextPageInner ) Then Exit;
    End;
    If CompletePage Then Begin
      If ErrorReturned ( CompleteThisPage ( Changed,
          Succ (GetLastRow - Moved), NrOfRows ) ) Then;
    End;
  End;


  Function BRMBrowser.BuildPrevPage (     Nr           : Word;
                                      Var Moved        : Word;
                                          CompletePage : Boolean;
                                          ModifyCurRow : Word;
                                      Var Changed      : Boolean ) : Integer;


  Var
    Dummy : Integer;


    Function ErrorReturned ( Err : Integer ) : Boolean;

    Begin
      BuildPrevPage := Err;
      If Err <> NoError Then Begin
        ErrorReturned := True;
      End Else Begin
        ErrorReturned := False;
      End;
    End;


    Function BuildPrevPageInner : Integer;

    Var
      I,
      LResult : Integer;                                               {!!.51}
      TRec   : RowRec;

    Begin
      I := GetFirstRow;
      If I > NrOfRows Then Begin
        BuildPrevPageInner := DialogError;
        Exit;
      End;
      CopyRowRec ( BSAPtr^ [I]^, TRec );
      EmptyRowElts ( TRec );
      With TRec Do Begin
        Status := 0;
        LResult := BRFindKeyAndRef ( TRec, -1 );                       {!!.51}
        If LResult = NoError Then Begin                                {!!.51}
          If (BSAPtr^ [I]^.Ref = TRec.Ref)
              And (BSAPtr^ [I]^.IKS = TRec.IKS) Then Begin
            {-Did we find the same key again?}
            LResult := BRPrevKey ( TRec );                             {!!.51}
          End;
        End;
        If LResult > DialogError Then Begin                            {!!.51}
          BuildPrevPageInner := LResult;                               {!!.51}
          Exit;
        End;
        I := 1;
        While (I <= Nr) And (KeyInBounds ( IKS ))
            And (LResult = NoError) Do Begin                           {!!.51}
          PlaceAtTop ( TRec );
          EmptyRowElts ( TRec );
          Inc (I);
          If I <= Nr Then Begin
            LResult := BRPrevKey ( TRec );                             {!!.51}
            If LResult > DialogError Then Begin                        {!!.51}
              BuildPrevPageInner := LResult;                           {!!.51}
              Exit;
            End;
          End;
        End;
      End;
      Moved := Pred (I);
      If ModifyCurRow > 0 Then Begin
        If Moved = 0 Then Begin
          CurRow := 1;
        End Else Begin
          If ModifyCurRow > 1 Then Begin
            CurRow := Moved;
          End;
       End;
      End;
      BuildPrevPageInner := NoError;
    End;


  Begin
    Changed := True;
    If UseReadLock Then Begin
      If ErrorReturned ( BRLDoReadLock ) Then Exit;
      If ErrorReturned ( BuildPrevPageInner ) Then Begin
        Dummy := BRLDoUnLock;
        Exit;
      End;
      If ErrorReturned ( BRLDoUnLock ) Then Exit;
    End Else Begin
      If ErrorReturned ( BuildPrevPageInner ) Then Exit;
    End;
    If CompletePage Then Begin
      If ErrorReturned ( CompleteThisPage ( Changed, 1, Moved ) ) Then;
    End;
  End;


  Function BRMBrowser.BuildThisPage ( Var Changed : Boolean ) : Integer;


    Function ErrorReturned ( Err : Integer ) : Boolean;

    Begin
      BuildThisPage := Err;
      If Err <> NoError Then Begin
        ErrorReturned := True;
      End Else Begin
        ErrorReturned := False;
      End;
    End;


    Function BuildThisPageInner : Integer;

    Var
      I,
      LResult       : Integer;                                         {!!.51}
      Added         : Word;
      SaveRow       : Word;
      LRow,
      FRow          : Word;
      TRec          : RowRec;
      Dummy         : Boolean;
      TooLessBehind : Boolean;


    Begin
      SaveRow := CurRow;
      CopyRowRec ( BSAPtr^ [CurRow]^, TRec );
      EmptyBrowScreen ( 1, False );
      With TRec Do Begin
        Status := 0;
        LResult := BRFindKeyAndRef ( TRec, 0 );                        {!!.51}
        If LResult > DialogError Then Begin                            {!!.51}
          BuildThisPageInner := LResult;                               {!!.51}
          Exit;
        End;
        If LResult = DialogError Then Begin                            {!!.51}
          LResult := BRNextKey ( TRec );                               {!!.51}
          If LResult > DialogError Then Begin                          {!!.51}
            BuildThisPageInner := LResult;                             {!!.51}
            Exit;
          End;
          If (LResult = DialogError) Or (Not KeyInBounds ( IKS )) Then Begin  {!!.51}
            LResult := BRPrevKey ( TRec );                             {!!.51}
            If LResult > DialogError Then Begin                        {!!.51}
              BuildThisPageInner := LResult;                           {!!.51}
              Exit;
            End;
            If (LResult = DialogError) Or (Not KeyInBounds ( IKS ))    {!!.51}
                Then Begin
              EmptyBrowScreen ( 1, True );
              CurRow := 1;
              BuildThisPageInner := DialogError;
              Exit;
            End;
          End;
        End;
      End;
      CopyRowRec ( TRec, BSAPtr^ [CurRow]^ );
      LResult := BuildNextPage ( NrOfRows - CurRow, Added, False, 0, Dummy ); {!!.51}
      If LResult > DialogError Then Begin                              {!!.51}
        BuildThisPageInner := LResult;                                 {!!.51}
        Exit;
      End;
      TooLessBehind := NrOfRows - CurRow <> Added;
      If (CurRow <> 1) Or TooLessBehind Then Begin
        LResult := BuildPrevPage ( NrOfRows - Succ (Added), Added, False, 0,  {!!.51}
            Dummy );
        If LResult > DialogError Then Begin                            {!!.51}
          BuildThisPageInner := LResult;                               {!!.51}
          Exit;
        End;
        CurRow := Succ (Added);
      End;
      If (Not TooLessBehind) And (SaveRow <> CurRow) Then Begin
        LResult := BuildNextPage ( Pred (GetFirstRow), Added, False, 0, {!!.51}
            Dummy );
        If LResult > DialogError Then Begin                            {!!.51}
          BuildThisPageInner := LResult;                               {!!.51}
          Exit;
        End;
      End;

      FRow := GetFirstRow;
      LRow := GetLastRow;
      If FRow <> 1 Then Begin
        For I := 1 To Succ (LRow - FRow) Do Begin
          CopyRowRec ( BSAPtr^ [Pred (I + FRow)]^, BSAPtr^ [I]^ );
        End;
        EmptyBrowScreen ( LRow - FRow + 2, False );
      End Else Begin
        If LRow < NrOfRows Then Begin
          EmptyBrowScreen ( Succ (LRow), False );
        End;
      End;
      BuildThisPageInner := NoError;
    End;


  Begin
    Changed := True;
    If ErrorReturned ( BuildThisPageInner ) Then Exit;
    If ErrorReturned ( CompleteThisPage ( Changed, 1, NrOfRows ) ) Then;
  End;


  Function BRMBrowser.ExpandPage ( NewNrOfRows : Word ) : Integer;

  Var
    Dummy  : Boolean;
    ToAdd,
    Added  : Word;
    LResult : Integer;                                                 {!!.51}

  Begin
    ExpandPage := NoError;
    If NewNrOfRows > AllocNrOfRows Then Exit;
    NrOfRows := NewNrOfRows;
    ToAdd := NrOfRows - GetLastRow;
    LResult := BuildNextPage ( ToAdd, Added, True, 0, Dummy );         {!!.51}
    ExpandPage := LResult;                                             {!!.51}
    If LResult <> NoError Then Exit;                                   {!!.51}
    If Added < ToAdd Then Begin
      LResult := BuildPrevPage ( ToAdd - Added, Added, True, 0, Dummy );  {!!.51}
      ExpandPage := LResult;                                           {!!.51}
      If LResult <> NoError Then Exit;                                 {!!.51}
      Inc (CurRow, Added);
    End;
  End;


  Function BRMBrowser.ShrinkPage ( NewNrOfRows : Word ) : Integer;

  Var
    RemoveAtTop    : Word;
    RemoveAtBottom : Word;
    I              : Word;
    TPtr           : RowRecPtr;

  Begin
    ShrinkPage := NoError;
    If NewNrOfRows = 0 Then Exit;
    If NewNrOfRows < CurRow Then Begin
      RemoveAtBottom := NrOfRows - CurRow;
      RemoveAtTop := NrOfRows - NewNrOfRows - RemoveAtBottom;
    End Else Begin
      RemoveAtTop := 0;
      RemoveAtBottom := NrOfRows - NewNrOfRows;
    End;
    NrOfRows := NewNrOfRows;
    If RemoveAtTop > 0 Then Begin
      Dec (CurRow, RemoveAtTop);
      For I := 1 To NrOfRows Do Begin
        TPtr := BSAPtr^ [I];
        BSAPtr^ [I] := BSAPtr^ [I + RemoveAtTop];
        BSAPtr^ [I + RemoveAtTop] := TPtr;
      End;
    End;
    EmptyBrowScreen ( Succ (NrOfRows), True );
  End;


  Function BRMBrowser.BuildNewPage (     NewKeyNr    : Word;
                                         NewKeyStr   : GenKeyStr;
                                         NewRef      : LongInt;
                                         NewCurRow   : Word;
                                         NewNrOfRows : Word;
                                     Var Changed     : Boolean ) : Integer;

  Begin
    Changed := False;
    BuildNewPage := NoError;
    If NewNrOfRows > AllocNrOfRows Then Exit;
    NrOfRows := NewNrOfRows;
    AdjustCurRow ( NewCurRow, False );
    CurRow := NewCurRow;
    KeyNr := NewKeyNr;
    BoundForceKeyAndRef ( NewKeyStr, NewRef );
    With BSAPtr^ [CurRow]^ Do Begin
      Ref := NewRef;
      IKS := NewKeyStr;
    End;
    BuildNewPage := BuildThisPage ( Changed );
  End;


  Function BRMBrowser.GetCurrentKeyStr : GenKeyStr;

  Begin
    If (CurRow <= GetLastRow) And (CurRow > 0) Then Begin
      GetCurrentKeyStr := BSAPtr^ [CurRow]^.IKS;
    End Else Begin
      GetCurrentKeyStr := '';
    End;
  End;


  Function BRMBrowser.GetCurrentDatRef : LongInt;

  Begin
    If (CurRow <= GetLastRow) And (CurRow > 0) Then Begin
      GetCurrentDatRef := BSAPtr^ [CurRow]^.Ref;
    End Else Begin
      GetCurrentDatRef := 0;
    End;
  End;


  Function BRMBrowser.BuildRow ( Var RR  : RowRec ) : Integer;

  Begin
    RunError (211);
  End;


  Function BRMBrowser.GetRowMatchingRec ( Var RR       : RowRec;
                                              AskUser,
                                              ReadFull : Boolean;
                                          Var Match    : Boolean )
                                                              : Integer;

  Var
    TRowRec : RowRec;


    Function ErrorReturned ( Err : Integer ) : Boolean;

    Begin
      GetRowMatchingRec := Err;
      If Err <> NoError Then Begin
        ErrorReturned := True;
      End Else Begin
        ErrorReturned := False;
      End;
    End;


  Begin
    Match := False;
    If ErrorReturned (BRGetRec ( RR, AskUser, ReadFull )) Then Exit;
    CopyRowRec ( RR, TRowRec );
    If ErrorReturned (BuildRow ( TRowRec )) Then Exit;
    Match := Not RowEltModified ( TRowRec, RR );
  End;


  Function BRMBrowser.BuildBrowScreenRow ( Var RR : RowRec ) : Integer;

  Var
    LResult    : Integer;                                              {!!.51}
    SaveRowRec : RowRec;

  Begin
    CopyRowRec ( RR, SaveRowRec );
    LResult := BuildRow ( RR );                                        {!!.51}
    BuildBrowScreenRow := LResult;                                     {!!.51}
    If LResult <> NoError Then Exit;                                   {!!.51}
    RR.RowModified := RowEltModified ( RR, SaveRowRec );
    RR.RowBuilt := True;
  End;


{$IFDEF InitAllUnits}
Begin
{$ENDIF}
End.
