{*********************************************************}
(*                B-Tree Filer Ver 5.57                  *)
{*     Copyright (c) TurboPower Software Co 1986-99      *}
{*                 All rights reserved.                  *}
(*      Based in part on code written by Ralf Nagel      *)
{*********************************************************}

Var
  LockEnd : Boolean;


  Function CreateTheFile : Boolean;

  Var
    IID : IsamIndDescr;

  Begin
    IID [1].Keyl := Key1Len;
    IID [1].AllowDupK := False;
    IID [2].Keyl := Key2Len;
    IID [2].AllowDupK := True;
    BTCreateFileBlock ( TheFileBlockName, SizeOf (PersonDef), 2, IID );
    CreateTheFile := IsamOK;
  End;


  Function RebuildTheFile : Boolean;

  Var
    IID : IsamIndDescr;
    I   : Integer;
    P   : PDialog;

  Procedure PleaseWait;                                           {!!.03mod}
  Var
    R : TRect;
  Begin
    R.Assign ( 0, 0, 30, 3);
    P := New(PDialog, Init(R, ' Reindex ' ));
    If P <> Nil Then With P^ Do Begin
      Options := Options Or ofCentered;
      R.Grow ( -1, -1 );
      Insert ( New ( PStaticText, Init ( R, '   Please wait' ))); {!!.TP}
    End;
    If P <> Nil Then
      DeskTop^.Insert ( P );
  End;


  Begin
    IID [1].Keyl := Key1Len;
    IID [1].AllowDupK := False;
    IID [2].Keyl := Key2Len;
    IID [2].AllowDupK := True;
    P := Nil;
    PleaseWait;
    RebuildFileBlock ( TheFileBlockName, SizeOf (PersonDef), 2, IID,
        BuildPersKey );
    RebuildTheFile := IsamOK;
    If P <> Nil Then Begin
      DeskTop^.Delete ( P );
      Dispose ( P, Done );
    End;
  End;


  Function OpenTheFile ( Save, Net : Boolean ): Boolean;

  Var
    OK,
    OK1 : Boolean;

  Begin
    OpenTheFile := False;
    OK := False;
    Repeat
      BTOpenFileBlock ( PersFB, TheFileBlockName, False, False, Save,
          Net );
      OK := IsamOK;
      If Not OK Then Begin
        OK1 := False;
        If IsamError = 10010 Then Begin
          If YesNo ( 'Index file is defective. Rebuild', mfConfirmation ) {!!.TP}
              Then Begin
            OK1 := RebuildTheFile;
          End;
        End Else Begin
          If IsamError = 9903 Then Begin
            If YesNo ( 'Data file does not exist. Create', {!!.TP}
                mfConfirmation ) Then Begin
              OK1 := CreateTheFile;
              If Not OK1 Then Exit;
            End Else Begin
              Exit;
            End;
          End Else Begin
            If (IsamError = 10480) Or (IsamError = 10355)  {!!.04a}
                Then Begin
              OkBox ( 'The fileblock cannot be opened in network mode.', {!!.TP}
                  mferror);
            End Else Begin
              OkBox ( 'The fileblock cannot be opened.', mferror);       {!!.TP}
            End;
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
    LockAbort := YesNo ( 'A lock prevents the operation. Abbort', {!!.TP}
                          mfConfirmation );
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


  Function ChangeaKey ( OldK,                                     {!!.03mod}
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


  Function AddRecAndKeys ( Var Ref : LongInt ) : Boolean;         {!!.03mod}

  Var
    OK : Boolean;
    Key : IsamKeyStr;

  Begin
    AddRecAndKeys := False;
    Repeat
      BTLockFileBlock ( PersFB );
      If Not IsamOK Then Begin
        If YesNo ( 'The fileblock is in use. Abort', {!!.TP}
                   mfConfirmation )
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


  Function ChangeRecAndKeys ( Var OldPerson : PersonDef;          {!!.03mod}
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
        If YesNo ( 'The fileblock is in use. Abort', {!!.TP}
                   mfConfirmation )
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
      OKBox ( 'The record has been changed in the meantime.', {!!.TP}
               mfInformation);
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


  Function DeleteRecAndKeys ( Ref : LongInt ) : Boolean;          {!!.03mod}

  Var
    OK : Boolean;

  Begin
    DeleteRecAndKeys := False;
    Repeat
      BTLockFileBlock ( PersFB );
      If Not IsamOK Then Begin
        If YesNo ( 'The fileblock is in use. Abort', {!!.TP}
                   mfConfirmation )
            Then Exit;
      End;
    Until IsamOK;
    OK := DeleteaKey ( BuildPersKey ( Person, 1 ), Ref, 1 );
    OK := DeleteaKey ( BuildPersKey ( Person, 2 ), Ref, 2 ) And OK;
    OK := DeleteData ( Ref ) And OK;
    BTUnLockFileBlock ( PersFB );
    DeleteRecAndKeys := IsamOK And OK;
  End;


  Function SearchForKey (     KeyNr  : Word;
                          Var Key    : IsamKeyStr;
                          Var DatRef : LongInt ) : Boolean;


  Begin
    SearchForKey := False;
    Repeat
      BTSearchKey ( PersFB, KeyNr, DatRef, Key );
      If LockAbort Then Exit;
    Until Not LockEnd;
    SearchForKey := IsamOK;
  End;


  Procedure TPersBrowser.NewRec;

  Var
    Key : IsamKeyStr;
    Ref : LongInt;

  Begin
    FillChar ( Person, SizeOf (Person), 0 );
    If EditDataRec ( ' New ', False ) Then Begin  {!!.TP}
      If Not AddRecAndKeys ( Ref ) Then Begin
        OKBox ( 'Add operation failed.', mfError) {!!.TP}
      End Else Begin
        SetAndUpdateBrowserScreen ( BuildPersKey ( Person, GetCurrentKeyNr ), Ref );
      End;
    End;
  End;


  Procedure TPersBrowser.EditRec;

  Var
    SavePerson : PersonDef;
    Refnr : Longint;
    Match : Boolean;                                              {!!.03}

  Begin
    If  GetCurrentRec ( Match ) > 0 Then Begin                    {!!.03mod}
      OkBox ( 'Read operation failed.', mferror); {!!.TP}
      Exit;
    End;
    If Not Match Then Begin                                       {!!.03}
      OKBox ( 'The record has been changed in the meantime.',  {!!.03}{!!.TP}
               mfInformation);                                    {!!.03}
      Exit;                                                       {!!.03}
    End;                                                          {!!.03}
    RefNr := GetCurrentDatRef;
    BtLockRec ( PersFb, refnr );
    If IsamError > 0 Then OkBox ( 'Lock operation failed.', mferror); {!!.TP}
    SavePerson := Person;
    If EditDataRec ( ' Edit ', False ) Then Begin
      If Not ChangeRecAndKeys ( SavePerson, GetCurrentDatRef )
          Then Begin
        OKBox ( 'Modify operation failed.', mfError); {!!.TP}
      End Else Begin
        SetAndUpDateBrowserScreen ( BuildPersKey ( Person, 1 ),
            GetCurrentDatRef );
      End;
    End;
    BtUnLockRec ( PersFb, refnr );
  End;


  Procedure TPersBrowser.DelRec;

  Var
    SavePerson : PersonDef;
    Match : Boolean;                                              {!!.03}

  Begin
    If  GetCurrentRec ( Match ) > 0 Then Begin                    {!!.03mod}
      OkBox ( 'Read operation failed.', mferror); {!!.TP}
      Exit;
    End;
    If Not Match Then Begin                                       {!!.03}
      OKBox ( 'The record has been changed in the meantime.',  {!!.03}{!!.TP}
               mfInformation);                                    {!!.03}
      Exit;                                                       {!!.03}
    End;                                                          {!!.03}    SavePerson := Person;
    If EditDataRec ( ' L”schen ', True ) Then Begin
      Person := SavePerson;
      If Not DeleteRecAndKeys ( GetCurrentDatRef ) Then Begin
        OKBox ( 'Delete operation failed', mfError); {!!.TP}
      End Else Begin
        UpDateBrowserScreen;
      End;
    End;
  End;

  Procedure TPersBrowser.ToggleKey;
  Var
    Key : IsamKeyStr;
    Ref : LongInt;
    Match : Boolean;                                              {!!.03}

  Begin
    If  GetCurrentRec ( Match ) > 0 Then Begin                    {!!.03mod}
      OkBox ( 'Read operation failed.', mferror); {!!.TP}
      Exit;
    End;
    SetKeyNr ( ( GetCurrentKeyNr +1 ) Mod 3 );                    {!!.04mod}
    SetAndUpdateBrowserScreen ( BuildPersKey ( Person, GetCurrentKeyNr ),
       GetCurrentDatRef );
  End;

  Procedure TPersBrowser.Search;

  Var
    UK       : IsamKeyStr;
    Ref      : LongInt;
    NewKeyNr : Integer;

  Begin
    FillChar ( Person, SizeOf (Person), 0 );
    NewKeyNr := 0;
    Ref := 0;
    If EditDataRec ( ' Search ', False ) Then Begin {!!.TP}
      UK := BuildPersKey ( Person, 1 );
      If Length ( UK ) > 0 Then If UK [1] > ' ' Then Begin
        NewKeyNr := 1;
      End Else Begin
        UK := BuildPerskey ( Person, 2 );
        If Length ( UK ) > 0 Then If UK [1] > ' ' Then Begin
          NewKeynr := 2;
        End Else begin
          OKBox ( 'No search criteria given.', mfError ); {!!.TP}
          Exit;
        End;
      End;
      If Not SearchForKey ( NewKeyNr, UK, Ref ) Then Begin
        OKBox ( 'Search operation failed.', MfError ) {!!.TP}
      End Else Begin
        SetKeyNr ( NewKeyNr );
        SetAndUpDateBrowserScreen ( UK, Ref );
      End;
    End;
  End;

  Procedure TPersBrowser.SearchByKey ( C : Char );

  Var
    Key : IsamKeyStr;
    DatRef : LongInt;

  Begin
    Key := C;                                                     {!!.05mod}
    Key := NLStrToKey ( Key,                                      {!!.05mod}
         PTVISDemoApp (Application)^.SourceDestCP,
         PTVISDemoApp (Application)^.OtherCP );
    DatRef := 0;
    If SearchForKey ( GetCurrentKeyNr, Key, DatRef ) Then Begin
      SetAndUpdateBrowserScreen ( Key, 0 )
    End;
  End;



