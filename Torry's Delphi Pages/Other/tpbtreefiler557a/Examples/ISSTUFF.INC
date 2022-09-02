{*********************************************************}
(*                B-Tree Filer Ver 5.57                  *)
{*     Copyright (c) TurboPower Software Co 1986-99      *}
{*                 All rights reserved.                  *}
(*      Based in part on code written by Ralf Nagel      *)
{*********************************************************}

Var
  LockEnd : Boolean;


  Procedure ShowMessage ( S, Title : PChar );

  Begin
    MessageBox ( GetActiveWindow, S, Title, mb_OK Or mb_TaskModal );
  End;


  Function CreateTheFile : Boolean;

  Var
    IID : IsamIndDescr;

  Begin
    IID [1].Keyl := 30;
    IID [1].AllowDupK := False;
    IID [2].Keyl := 8;
    IID [2].AllowDupK := True;
    BTCreateFileBlock ( TheFileBlockName, SizeOf (PersonDef), 2, IID );
    CreateTheFile := IsamOK;
  End;


  Procedure InformApp ( PRInfo   : PRebuildInfo;
                        FName    : IsamFileBlockname;
                        CallType : ReXCallType ); Far;

  Var
    MsgT      : TMsg;
    SaveTimer : Boolean;

  Begin
    Case CallType Of
      ReXFirst : Begin
        If Application^.MainWindow <> Nil Then Begin
          If (TypeOf (Application^.MainWindow^) = TypeOf (TDemoWin))
              And (Application^.MainWindow^.HWindow <> 0) Then Begin
            With PDemoWin (Application^.MainWindow)^ Do Begin
              EnableWindow ( HWindow, False );
              SaveTimer := SetSuppressTimer ( True );
            End;
          End;
        End;
      End;
      ReXPerform: Begin
        If PRInfo <> Nil Then Begin
          If PRInfo^.HWindow <> 0 Then Begin
            While PeekMessage ( MsgT, 0, 0, 0, pm_Remove ) Do Begin
              If Not IsDialogMessage ( PRInfo^.HWindow, MsgT ) Then Begin
                TranslateMessage ( MsgT );
                DispatchMessage ( MsgT );
              End;
            End;
          End;
        End;
      End;
      ReXFinal : Begin
        If Application^.MainWindow <> Nil Then Begin
          If (TypeOf (Application^.MainWindow^) = TypeOf (TDemoWin))
              And (Application^.MainWindow^.HWindow <> 0) Then Begin
            With PDemoWin ( Application^.MainWindow )^ Do Begin
              EnableWindow ( HWindow, True );
              SetSuppressTimer ( SaveTimer );
            End;
          End;
        End;
      End;
    End;
  End;


  Function RebuildTheFile : Boolean;

  Var
    IID : IsamIndDescr;
    I   : Integer;

  Begin
    IID [1].Keyl := 30;
    IID [1].AllowDupK := False;
    IID [2].Keyl := 8;
    IID [2].AllowDupK := True;
    RebuildAndInform ( TheFileBlockName, SizeOf (PersonDef), False,
        DemoCharConvert, 2, IID, @BuildPersKey, InformApp );      {!!.05mod}
    RebuildTheFile := IsamOK;
  End;


  Function OpenTheFile : Boolean;

  Var
    OK,
    OK1 : Boolean;

  Begin
    OpenTheFile := False;
    OK := False;
    Repeat
      BTOpenFileBlock ( PersFB, TheFileBlockName, False, False, False,
          True );
      OK := IsamOK;
      If Not OK Then Begin
        OK1 := False;
        If IsamError = 10010 Then Begin
          If YesNo ( 'Index file is defective. Rebuild' ) Then Begin {!!.TP}
            OK1 := RebuildTheFile;
          End Else Begin                                             {!!.05}
            Exit;                                                    {!!.05}
          End;
        End Else Begin
          If IsamError = 9903 Then Begin
            If YesNo ( 'Data file does not exist. Create' ) {!!.TP}
                Then Begin
              OK1 := CreateTheFile;
              If Not OK1 Then Exit;
            End Else Begin
              Exit;
            End;
          End Else Begin
            Exit;
          End;
        End;
      End;
    Until OK;
    OpenTheFile := True;
  End;


  Function IsLockError : Boolean;

  Begin
    IsLockError := BTIsamErrorClass = 2;
  End;


  Function LockAbort : Boolean;

  Begin
    LockAbort := False;
    LockEnd := IsLockError;
    If Not LockEnd Then Exit;
    LockAbort := YesNo ( 'Cannot get a lock. Abort' ); {!!.TP}
  End;


  Function AddData ( P : PersonDef; Var Ref : LongInt ) : Boolean;

  Begin
    AddData := False;
    Repeat
      BTAddRec ( PersFB, Ref, P );
      If LockAbort Then Exit;
    Until Not LockEnd;
    AddData := IsamOK;
  End;


  Function ChangeData ( P : PersonDef; Ref : LongInt ) : Boolean;

  Begin
    ChangeData := False;
    Repeat
      BTPutRec ( PersFB, Ref, P, False );
      If LockAbort Then Exit;
    Until Not LockEnd;
    ChangeData := IsamOK;
  End;


  Function DeleteData ( Ref : LongInt ) : Boolean;

  Begin
    DeleteData := False;
    Repeat
      BTDeleteRec ( PersFB, Ref );
      If LockAbort Then Exit;
    Until Not LockEnd;
    DeleteData := IsamOK;
  End;


  Function AddaKey ( K     : IsamKeyStr;
                     Ref   : LongInt;
                     KeyNr : Integer ) : Boolean;

  Begin
    AddaKey := False;
    Repeat
      BTAddKey ( PersFB, KeyNr, Ref, K );
      If LockAbort Then Exit;
    Until Not LockEnd;
    AddaKey := IsamOK;
  End;


  Function DeleteaKey ( K     : IsamKeyStr;
                        Ref   : LongInt;
                        KeyNr : Integer ) : Boolean;

  Begin
    DeleteaKey := False;
    Repeat
      BTDeleteKey ( PersFB, KeyNr, Ref, K );
      If LockAbort Then Exit;
    Until Not LockEnd;
    DeleteaKey := IsamOK;
  End;


  Function ChangeaKey ( OldK,                                {mod !!.03}
                        NewK  : IsamKeyStr;
                        Ref   : LongInt;
                        KeyNr : Integer ) : Boolean;

  Begin
    ChangeaKey := True;
    If OldK = NewK Then Exit;
    If AddaKey ( NewK, Ref, KeyNr ) Then Begin
      If DeleteaKey ( OldK, Ref, KeyNr ) Then;
    End Else Begin
      ChangeaKey := False;
    End;
  End;


  Function AddRecAndKeys ( Var Ref : LongInt ) : Boolean;    {mod !!.03}

  Var
    Key : IsamKeyStr;
    OK  : Boolean;

  Begin
    AddRecAndKeys := False;
    Repeat
      BTLockFileBlock ( PersFB );
      If Not IsamOK Then Begin
        If YesNo ( 'The files are in use. Abort' ) {!!.TP}
            Then Exit;
      End;
    Until IsamOK;
    OK := AddData ( Person, Ref );
    If OK Then Begin
      Key := BuildPersKey ( Person, 1 );
      OK := AddaKey ( Key, Ref, 1 );
      If Not OK Then If DeleteData ( Ref ) Then;
    End;
    If OK Then Begin
      Key := BuildPersKey ( Person, 2 );
      OK := AddaKey ( Key, Ref, 2 );
      If Not OK Then Begin
        If DeleteaKey ( Key, Ref, 1 ) Then;
        If DeleteData ( Ref ) Then;
      End;
    End;
    BTUnLockFileBlock ( PersFB );
    AddRecAndKeys := IsamOK And OK;
  End;


  Function ChangeRecAndKeys ( Var OldPerson : PersonDef;        {mod !!.03}
                                  Ref       : LongInt ) : Boolean;

  Var
    CurrentPerson : PersonDef;
    T,
    Ok            : Boolean;

    Function CompStruc ( Var S1, S2; Len : Word ) : Integer;

    Var
      A1 : Array [1..$FFFF] Of Byte Absolute S1;
      A2 : Array [1..$FFFF] Of Byte Absolute S2;
      I  : Word;

    Begin
      For I := 1 To Len Do Begin
        If A1 [I] < A2 [I] Then Begin
          CompStruc := -1;
          Exit;
        End;
        If A1 [I] > A2 [I] Then Begin
          CompStruc := 1;
          Exit;
        End;
      End;
      CompStruc := 0;
    End;


  Begin
    ChangeRecAndKeys := True;
    If CompStruc ( Person, OldPerson, SizeOf (PersonDef) ) = 0 Then Exit;
    ChangeRecAndKeys := False;
    Repeat
      BTLockFileBlock ( PersFB );
      If Not IsamOK Then Begin
        If YesNo ( 'The files are in use. Abort' ) {!!.TP}
            Then Exit;
      End;
    Until IsamOK;
    BTGetRec ( PersFB, Ref, CurrentPerson, False );
    If Not IsamOK Or (CurrentPerson.Dele <> 0) Then Begin
      BTUnLockFileBlock ( PersFB );
      Exit;
    End;
    If CompStruc ( CurrentPerson, OldPerson, SizeOf (PersonDef) ) <> 0
        Then Begin
      BTUnLockFileBlock ( PersFB );
      ShowMessage ( 'The record has changed in the meantime.', {!!.TP}
          'User Message' );
      Exit;
    End;
    OK := ChangeaKey ( BuildPersKey ( CurrentPerson, 1 ),
        BuildPersKey ( Person, 1 ), Ref, 1 );
    If OK Then Begin
      OK := ChangeaKey ( BuildPersKey ( CurrentPerson, 2 ),
          BuildPersKey ( Person, 2 ), Ref, 2 );
      If Not OK Then Begin
        If ChangeaKey ( BuildPersKey ( Person, 1 ),
            BuildPersKey ( CurrentPerson, 1 ), Ref, 1 ) Then;
      End;
    End;
    If OK Then Begin
      OK := ChangeData ( Person, Ref );
      If Not OK Then Begin
        If ChangeaKey ( BuildPersKey ( Person, 1 ),
            BuildPersKey ( CurrentPerson, 1 ), Ref, 1 ) Then;
        If ChangeaKey ( BuildPersKey ( Person, 2 ),
            BuildPersKey ( CurrentPerson, 2 ), Ref, 2 ) Then;
      End;
    End;
    BTUnLockFileBlock ( PersFB );
    ChangeRecAndKeys :=  IsamOK And OK;
  End;


  Function DeleteRecAndKeys ( Ref : LongInt ) : Boolean;  {mod !!.03}

  Var
    OK : Boolean;

  Begin
    DeleteRecAndKeys := False;
    Repeat
      BTLockFileBlock ( PersFB );
      If Not IsamOK Then Begin
        If YesNo ( 'The files are in use. Abort' ) {!!.TP}
            Then Exit;
      End;
    Until IsamOK;
    OK := DeleteaKey ( BuildPersKey ( Person, 1 ), Ref, 1 );
    OK := DeleteaKey ( BuildPersKey ( Person, 2 ), Ref, 2 ) And OK;
    OK := DeleteData ( Ref ) And OK;
    BTUnLockFileBlock ( PersFB );
    DeleteRecAndKeys := IsamOK And OK;
  End;


  Function SearchForKey ( Var Key    : IsamKeyStr;
                          Var DatRef : LongInt ) : Boolean;


  Begin
    SearchForKey := False;
    Repeat
      BTSearchKey ( PersFB, 1, DatRef, Key );
      If LockAbort Then Exit;
    Until Not LockEnd;
    SearchForKey := IsamOK;
  End;


  Procedure TDemoWin.NewRec ( Var Msg: TMessage);

  Var
    Ref : LongInt;

  Begin
    FillChar ( Person, SizeOf (Person), 0 );
    If EditDataRec ( ' New ' ) Then Begin
      If Not AddRecAndKeys ( Ref ) Then Begin
        DbError ( 'Add operation failed') {!!.TP}
      End Else Begin
        SetAndUpDateBrowserScreen ( BuildPersKey
            ( Person, GetCurrentKeyNr ), Ref );
      End;
    End;
  End;


  Procedure TDemoWin.EditRec ( Var Msg: TMessage);

  Var
    Match      : Boolean;
    SavePerson : PersonDef;

  Begin
    If GetCurrentRec ( Match ) > 0 Then Exit;     {mod !!.03}
    If Not Match Then Exit;                       {mod !!.03}
    SavePerson := Person;
    If EditDataRec ( ' Edit ' ) Then Begin
      If Not ChangeRecAndKeys ( SavePerson, GetCurrentDatRef )
          Then Begin
        DbError ( 'Modify operation failed'); {!!.TP}
      End Else Begin
        SetAndUpDateBrowserScreen ( BuildPersKey
            ( Person, GetCurrentKeyNr ), GetCurrentDatRef );
      End;
    End;
  End;


  Procedure TDemoWin.DelRec ( Var Msg: TMessage);

  Var
    Match      : Boolean;
    SavePerson : PersonDef;

  Begin
    If GetCurrentRec ( Match ) > 0 Then Exit;     {mod !!.03}
    If Not Match Then Exit;                       {mod !!.03}
    SavePerson := Person;
    If EditDataRec ( ' Delete ' ) Then Begin
      Person := SavePerson;
      If Not DeleteRecAndKeys ( GetCurrentDatRef ) Then Begin
        DbError ( 'Delete operation failed'); {!!.TP}
      End Else Begin
        UpDateBrowserScreen;
      End;
    End;
  End;


  Procedure TDemoWin.Search ( Var Msg: TMessage);

  Var
    Key : IsamKeyStr;
    Ref : LongInt;

  Begin
    FillChar ( Person, SizeOf (Person), 0 );
    If EditDataRec ( ' Search ' ) Then Begin
      Key := BuildPersKey ( Person, 1 );
      If Not SearchForKey ( Key, Ref ) Then Begin
        DbError ( 'Search operatin failed') {!!.TP}
      End Else Begin
        SetAndUpDateBrowserScreen ( Key, Ref );
      End;
    End;
  End;
