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
    MessageBox ( GetActiveWindow, S, Title,
        MB_OK Or mb_TaskModal );
  End;


  Function CreateTheFile : Boolean;

  Var
    IID : IsamIndDescr;

  Begin
    IID [1].Keyl := 20;
    IID [1].AllowDupK := True;
    BTCreateFileBlock ( TheFileBlockName, SizeOf (PersonDef), 1, IID );
    CreateTheFile := IsamOK;
  End;


  Function RebuildTheFile : Boolean;

  Var
    IID : IsamIndDescr;
  Begin
    IID [1].Keyl := 20;
    IID [1].AllowDupK := True;
    RebuildFileBlock ( TheFileBlockName, SizeOf (PersonDef), 1, IID,
        BuildPersKey );                                         {!!.52}
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
          If YesNo ( 'Index defective. Rebuild' ) Then Begin
            OK1 := RebuildTheFile;
          End;
        End Else Begin
          If IsamError = 9903 Then Begin
            If YesNo ( 'Data file does not exist. Create' )
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
    LockAbort := YesNo ( 'A lock prevents this operation. Abort' );
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


  Function ChangeaKey ( OldK,
                        NewK  : IsamKeyStr;
                        Ref   : LongInt;
                        KeyNr : Integer ) : Boolean;

  Begin
    ChangeaKey := False;
    If DeleteaKey ( OldK, Ref, KeyNr ) Then Begin
      If AddaKey ( NewK, Ref, KeyNr ) Then Begin
        ChangeaKey := True;
      End;
    End;
  End;


  Function AddRecAndKeys ( Var Key : IsamKeyStr;
                           Var Ref : LongInt ) : Boolean;

  Var
    OK : Boolean;

  Begin
    AddRecAndKeys := False;
    Repeat
      BTLockFileBlock ( PersFB );
      If Not IsamOK Then Begin
        If YesNo ( 'Unable to lock file. Abort' )
            Then Exit;
      End;
    Until IsamOK;
    OK := AddData ( Person, Ref );
    If OK Then Begin
      Key := BuildPersKey ( Person, 1 );
      OK := AddaKey ( Key, Ref, 1 );
    End;
    BTUnLockFileBlock ( PersFB );
    AddRecAndKeys := IsamOK And OK;
  End;


  Function ChangeRecAndKeys ( Var OldPerson : PersonDef;
                                  Ref       : LongInt ) : Boolean;

  Var
    CurrentPerson : PersonDef;
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
        If YesNo ( 'Unable to lock file. Abort' )
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
      ShowMessage ( 'The record has been changed in the meantime.',
          'User Message' );
      Exit;
    End;
    OK := ChangeData ( Person, Ref );
    If OK Then Begin
      If BuildPersKey ( CurrentPerson, 1) <> BuildPersKey ( Person, 1 )
          Then Begin
        OK := ChangeaKey ( BuildPersKey ( CurrentPerson, 1 ),
            BuildPersKey ( Person, 1 ), Ref, 1 );
      End;
    End;
    BTUnLockFileBlock ( PersFB );
    ChangeRecAndKeys :=  IsamOK And OK;
  End;


  Function DeleteRecAndKeys ( Ref : LongInt ) : Boolean;

  Var
    OK : Boolean;

  Begin
    DeleteRecAndKeys := False;
    Repeat
      BTLockFileBlock ( PersFB );
      If Not IsamOK Then Begin
        If YesNo ( 'Unable to lock file. Abort' )
            Then Exit;
      End;
    Until IsamOK;
    OK := DeleteaKey ( BuildPersKey ( Person, 1 ), Ref, 1 );
    If OK Then OK := DeleteData ( Ref );
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
    Key : IsamKeyStr;
    Ref : LongInt;

  Begin
    FillChar ( Person, SizeOf (Person), 0 );
    If EditDataRec ( ' New ' ) Then Begin
      If Not AddRecAndKeys ( Key, Ref ) Then Begin
        DbError ( 'Error adding record')
      End Else Begin
        SetAndUpdateBrowserScreen ( Key, Ref );
      End;
    End;
  End;


  Procedure TDemoWin.EditRec ( Var Msg: TMessage);

  Var
    SavePerson : PersonDef;
    Match      : boolean;  {!!.03}

  Begin
    If  GetCurrentRec ( Match ) > 0 Then Begin                    {mod !!.03}
      OkBox ( 'Reading record.' );
      Exit;
    End;
    If Not Match Then Begin                                       {!!.03}
      OKBox ( 'The record has been changed in the meantime.' );   {!!.03}
      Exit;                                                       {!!.03}
    End;                                                          {!!.03}
    SavePerson := Person;
    If EditDataRec ( ' Edit ' ) Then Begin
      If Not ChangeRecAndKeys ( SavePerson, GetCurrentDatRef )
          Then Begin
        DbError ( 'Error updating record');
      End Else Begin
        SetAndUpDateBrowserScreen ( BuildPersKey ( Person, 1 ),
            GetCurrentDatRef );
      End;
    End;
  End;


  Procedure TDemoWin.DelRec ( Var Msg: TMessage);

  Var
    SavePerson : PersonDef;
    Match      : boolean;  {!!.03}

  Begin
    If  GetCurrentRec ( Match ) > 0 Then Begin                    {mod !!.03}
      OkBox ( 'Reading record.' );
      Exit;
    End;
    If Not Match Then Begin                                       {!!.03}
      OKBox ( 'The record has been changed in the meantime. ');   {!!.03}
      Exit;                                                       {!!.03}
    End;                                                          {!!.03}
    SavePerson := Person;
    If EditDataRec ( ' Delete ' ) Then Begin
      Person := SavePerson;
      If Not DeleteRecAndKeys ( GetCurrentDatRef ) Then Begin
        DbError ( 'Error deleting record');
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
        DbError ( 'Search error')
      End Else Begin
        SetAndUpDateBrowserScreen ( Key, Ref );
      End;
    End;
  End;
