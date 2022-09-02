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


Unit HiBrows;

Interface

Uses
  LowBrows,
  MedBrows;

Type
  PBRHBrowser = ^BRHBrowser;
  BRHBrowser = Object ( BRMBrowser )
    OtherAction,
    StatusOK    : Boolean;

    Constructor Init (     DrvOrFileBlockPtr  : Pointer;
                           NumberOfEltsPerRow : Word;
                           NumberOfRows       : Word;
                           LKey,
                           HKey               : GenKeyStr;
                           SaveStat           : Boolean;
                       Var DatS;
                           IsVarRec           : Boolean );
    Destructor Done; virtual;
    Procedure SetDefaults;

  {--The following routines may be overwritten in descending objects}
    Procedure ShowErrorOccured ( EClass : Integer ); Virtual;          {!!.51}

  {--The following routines must not be overwritten in descending objects}
    Procedure HBuildFirstPage ( Var Changed : Boolean );
    Procedure HBuildLastPage ( Var Changed : Boolean );
    Procedure HBuildNextPage (     Nr           : Word;
                               Var Moved        : Word;
                                   CompletePage : Boolean;
                                   ModifyCurRow : Word;
                               Var Changed      : Boolean );
    Procedure HBuildPrevPage (     Nr           : Word;
                               Var Moved        : Word;
                                   CompletePage : Boolean;
                                   ModifyCurRow : Word;
                               Var Changed      : Boolean );
    Procedure HBuildThisPage ( Var Changed : Boolean );
    Procedure HExpandPage ( NewNrOfRows : Word );
    Procedure HShrinkPage ( NewNrOfRows : Word );
    Procedure HBuildNewPage (     NewKeyNr    : Word;
                                  NewKeyStr   : GenKeyStr;
                                  NewRef      : LongInt;
                                  NewCurRow   : Word;
                                  NewNrOfRows : Word );
    Procedure HGetApprKeyAndRef (     RelPos     : Word;
                                      Scale      : Word;
                                  Var UserKey    : GenKeyStr;
                                  Var UserDatRef : LongInt );
    Procedure HGetApprRelPos ( Var RelPos     : Word;
                                   Scale      : Word;
                                   UserKey    : GenKeyStr;
                                   UserDatRef : LongInt );

  {--The following routines are internal use only}
    Procedure PrivatSetDefaults;
    Procedure ErrorInspect ( Result : Integer );
  End;


Implementation

Const
  DefDelayOnGetRec = 6;
  DefRetrOnGetRec = 6;
  DefNrOfRowsConst = 20;

  Constructor BRHBrowser.Init (     DrvOrFileBlockPtr  : Pointer;
                                    NumberOfEltsPerRow : Word;
                                    NumberOfRows       : Word;
                                    LKey,
                                    HKey               : GenKeyStr;
                                    SaveStat           : Boolean;
                                Var DatS;
                                    IsVarRec           : Boolean );

  Begin
    If Not BRMBrowser.Init ( DrvOrFileBlockPtr, NumberOfEltsPerRow,
        NumberOfRows, DefDelayOnGetRec, DefRetrOnGetRec, DefNrOfRowsConst,
        LKey, HKey, SaveStat, DatS, IsVarRec ) Then Fail;
    PrivatSetDefaults;
  End;


  Destructor BRHBrowser.Done;

  Begin
    BRMBrowser.Done;
  End;


  Procedure BRHBrowser.SetDefaults;

  Begin
    BRMBrowser.SetDefaults;
    PrivatSetDefaults;
  End;


  Procedure BRHBrowser.PrivatSetDefaults;

  Begin
    StatusOK := False;
    OtherAction := False;
  End;


  Procedure BRHBrowser.ShowErrorOccured ( EClass : Integer );          {!!.51}

  Begin
  End;


  Procedure BRHBrowser.ErrorInspect ( Result : Integer );

  Begin
    OtherAction := False;
    StatusOK := Result = NoError;
    If Not StatusOK Then ShowErrorOccured ( Result );
  End;


  Procedure BRHBrowser.HBuildFirstPage ( Var Changed : Boolean );

  Begin
    ErrorInspect ( BuildFirstPage ( Changed ) );
  End;


  Procedure BRHBrowser.HBuildLastPage ( Var Changed : Boolean );

  Begin
    ErrorInspect ( BuildLastPage ( Changed ) );
  End;


  Procedure BRHBrowser.HBuildNextPage (     Nr           : Word;
                                        Var Moved        : Word;
                                            CompletePage : Boolean;
                                            ModifyCurRow : Word;
                                        Var Changed      : Boolean );

  Var
    OAValue : Boolean;

  Begin
    If Not StatusOK Then Begin
      HBuildThisPage ( Changed );
      OAValue := True;
    End Else Begin
      OAValue := False;
    End;
    If StatusOK Then Begin
      ErrorInspect ( BuildNextPage ( Nr, Moved, CompletePage, ModifyCurRow,
          Changed ) );
    End;
    OtherAction := OAValue;
  End;


  Procedure BRHBrowser.HBuildPrevPage (     Nr           : Word;
                                        Var Moved        : Word;
                                            CompletePage : Boolean;
                                            ModifyCurRow : Word;
                                        Var Changed      : Boolean );

  Var
    OAValue : Boolean;

  Begin
    If Not StatusOK Then Begin
      HBuildThisPage ( Changed );
      OAValue := True;
    End Else Begin
      OAValue := False;
    End;
    If StatusOK Then Begin
      ErrorInspect ( BuildPrevPage ( Nr, Moved, CompletePage, ModifyCurRow,
          Changed ) );
    End;
    OtherAction := OAValue;
  End;


  Procedure BRHBrowser.HBuildThisPage ( Var Changed : Boolean );

  Begin
    Changed := True;
    If Not StatusOK Then Begin
      If (CurRow <= GetLastRow) And (CurRow > 0) Then Begin
        ErrorInspect ( BuildThisPage ( Changed ) );
      End Else Begin
        HBuildFirstPage ( Changed );
      End;
    End Else Begin
      ErrorInspect ( BuildThisPage ( Changed ) );
    End;
  End;


  Procedure BRHBrowser.HExpandPage ( NewNrOfRows : Word );

  Var
    Dummy : Boolean;

  Begin
    If Not StatusOK Then Begin
      If NewNrOfRows > AllocNrOfRows Then Exit;
      NrOfRows := NewNrOfRows;
      HBuildThisPage ( Dummy );
    End Else Begin
      ErrorInspect ( ExpandPage ( NewNrOfRows ) );
    End;
  End;


  Procedure BRHBrowser.HShrinkPage ( NewNrOfRows : Word );

  Var
    Dummy : Boolean;

  Begin
    If Not StatusOK Then Begin
      If NewNrOfRows = 0 Then Exit;
      NrOfRows := NewNrOfRows;
      HBuildThisPage ( Dummy );
    End Else Begin
      ErrorInspect ( ShrinkPage ( NewNrOfRows ) );
    End;
  End;


  Procedure BRHBrowser.HBuildNewPage (     NewKeyNr    : Word;
                                           NewKeyStr   : GenKeyStr;
                                           NewRef      : LongInt;
                                           NewCurRow   : Word;
                                           NewNrOfRows : Word );

  Var
    Dummy : Boolean;

  Begin
    ErrorInspect ( BuildNewPage ( NewKeyNr, NewKeyStr, NewRef, NewCurRow,
        NewNrOfRows, Dummy ) );
  End;


  Procedure BRHBrowser.HGetApprKeyAndRef (     RelPos     : Word;
                                               Scale      : Word;
                                           Var UserKey    : GenKeyStr;
                                           Var UserDatRef : LongInt );

  Var
    TRowRec : RowRec;
    Dummy   : Integer;


    Procedure GetMaxPosKeyStr ( Var KeyStr : GenKeyStr );

    Begin
      KeyStr := '';
      While Length (KeyStr) < GenMaxKeyLen Do Begin
        KeyStr := KeyStr + #255;
      End;
    End;


    Procedure Approximate ( Var FinalKeyStr : GenKeyStr;
                            Var FinalRef    : LongInt );

    Type
      PosPoint = Record
        ResKey     : GenKeyStr;
        ResRef     : LongInt;
        TestRelPos,
        ResRelPos  : Word;
      End;

    Var
      PLow,
      PTest,
      PHigh   : PosPoint;
      Stop    : Boolean;


      Procedure NextStep (     TestRelPos : Word;
                           Var ResKey     : GenKeyStr;
                           Var ResRef     : LongInt;
                           Var ResRelPos  : Word );

      Begin
        ErrorInspect ( BRLGetApprKeyAndRef ( TestRelPos, Scale, ResKey,
            ResRef ) );
        If Not StatusOK Then Exit;
        HGetApprRelPos ( ResRelPos, Scale, ResKey, ResRef );
      End;


      Procedure SetPTestToMin;

      Var
        AbsDiff,
        BestAbsDiff : Word;

      Begin
        BestAbsDiff := Abs (LongInt (PTest.ResRelPos) - LongInt (RelPos));
        AbsDiff := Abs (LongInt (PLow.ResRelPos) - LongInt (RelPos));
        If AbsDiff < BestAbsDiff Then Begin
          BestAbsDiff := AbsDiff;
          PTest := PLow;
        End;
        AbsDiff := Abs (LongInt (PHigh.ResRelPos) - LongInt (RelPos));
        If AbsDiff < BestAbsDiff Then Begin
          PTest := PHigh;
        End;
      End;


    Begin
      PTest.TestRelPos := Scale Div 2;
      With PLow Do Begin
        ResKey := '';
        ResRef := 0;
        TestRelPos := 0;
        ResRelPos := 0;
      End;
      With PHigh Do Begin
        GetMaxPosKeyStr ( ResKey );
        ResRef := GenMaxPosRef;
        TestRelPos := Scale;
        ResRelPos := Scale;
      End;

      Stop := False;
      Repeat
        With PTest Do Begin
          NextStep ( TestRelPos, ResKey, ResRef, ResRelPos );
        End;
        If Not StatusOK Then Exit;
        If PTest.ResRelPos <> RelPos Then Begin
          If PTest.ResRelPos > RelPos Then Begin
            PHigh := PTest;
          End Else Begin
            PLow := PTest;
          End;
          If ((PLow.ResKey = PHigh.ResKey) And (PLow.ResRef = PHigh.ResRef))
              Or (PLow.ResRelPos = PHigh.ResRelPos) Then Begin
            Stop := True;
          End Else Begin
            PTest.TestRelPos := (PLow.TestRelPos + PHigh.TestRelPos) Div 2;
            If (PTest.TestRelPos = PLow.TestRelPos)
                Or (PTest.TestRelPos = PHigh.TestRelPos) Then Begin
              Stop := True;
              SetPTestToMin;
            End;
          End;
        End Else Begin
          Stop := True;
        End;
      Until Stop;

      With PTest Do Begin
        FinalKeyStr := ResKey;
        FinalRef := ResRef;
      End;
    End;


  Begin
    With TRowRec Do Begin
      If RelPos = 0 Then Begin
        IKS := '';
        Ref := 0;
        ErrorInspect ( BRFindKeyAndRefNoFilter ( TRowRec, 1 ) );       {!!.04}
        If StatusOK Then Begin
          UserKey := IKS;
          UserDatRef := Ref;
        End;
        Exit;
      End Else Begin
        If RelPos >= Scale Then Begin
          GetMaxPosKeyStr ( IKS );
          Ref := GenMaxPosRef;
          ErrorInspect ( BRFindKeyAndRefNoFilter ( TRowRec, -1 ) );    {!!.04}
          If StatusOK Then Begin
            UserKey := IKS;
            UserDatRef := Ref;
          End;
          Exit;
        End;
      End;
    End;
    UserKey := '';
    UserDatRef := 0;
    ErrorInspect ( BRLDoReadLock );
    If Not StatusOK Then Exit;
    Approximate ( UserKey, UserDatRef );
    If StatusOK Then Begin
      ErrorInspect ( BRLDoUnLock );
    End Else Begin
      Dummy := BRLDoUnLock;
    End;
  End;


  Procedure BRHBrowser.HGetApprRelPos ( Var RelPos     : Word;
                                            Scale      : Word;
                                            UserKey    : GenKeyStr;
                                            UserDatRef : LongInt );

  Var
    ScaleAdd : Word;
    NewScale : Word;
    SubLow   : Word;
    UsedKeys : LongInt;

  Begin
    RelPos := 0;
    ErrorInspect ( BRLDoReadLock );
    If Not StatusOK Then Exit;
    ErrorInspect ( BRLUsedKeys ( UsedKeys ) );
    If Not StatusOK Then Begin
      ErrorInspect ( BRLDoUnLock );
      Exit;
    End;
    Case UsedKeys Of
      0, 1 :;
      Else Begin
        Case Scale Div UsedKeys Of
          0 : Begin
            ScaleAdd := 2;
          End;
          1 : Begin
            ScaleAdd := Scale Div (Scale Div 4);
          End;
          2 : Begin
            ScaleAdd := Scale Div (Scale Div 8);
          End;
          3, 4 : Begin
            ScaleAdd := Scale Div (Scale Div 10);
          End;
          5..10 : Begin
            ScaleAdd := Scale Div (Scale Div 25);
          End;
          11..20 : Begin
            ScaleAdd := Scale Div (Scale Div 50);
          End;
          21..25 : Begin
            ScaleAdd := Scale;
          End;
          26..33 : Begin
            ScaleAdd := Scale * 2;
          End;
          Else Begin
            ScaleAdd := Scale * 4;
          End;
        End; {Case}
        If ScaleAdd < 2 Then ScaleAdd := 2;
        ScaleAdd := ScaleAdd And $FFFE;
        NewScale := Scale + ScaleAdd;
        ErrorInspect ( BRLGetApprRelPos ( RelPos, NewScale, UserKey,
           UserDatRef ) );
        If StatusOK Then Begin
          SubLow := ScaleAdd Div 2;
          If RelPos <= SubLow Then Begin
            RelPos := 0;
          End Else Begin
            Dec ( RelPos, SubLow );
            If RelPos > Scale Then RelPos := Scale;
          End;
        End;
      End;
    End; {Case}
    ErrorInspect ( BRLDoUnLock );
  End;


{$IFDEF InitAllUnits}
Begin
{$ENDIF}
End.
