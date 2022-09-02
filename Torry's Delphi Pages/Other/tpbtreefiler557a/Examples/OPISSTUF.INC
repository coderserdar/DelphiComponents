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
    BTCreateFileBlock ( FName, SizeOf (PersonDef), 2, IID );
    CreateTheFile := IsamOK;
  End;


  Function RebuildTheFile : Boolean;

  Var
    IID : IsamIndDescr;
    I   : Integer;

  Begin
    IID [1].Keyl := Key1Len;
    IID [1].AllowDupK := False;
    IID [2].Keyl := Key2Len;
    IID [2].AllowDupK := True;
    RebuildFileBlock ( FName, SizeOf (PersonDef), 2, IID,
        BuildKey );
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
      BTOpenFileBlock ( PF, FName, False, False, False,
          True );
      OK := IsamOK;
      If Not OK Then Begin
        OK1 := False;
        If IsamError = 10010 Then Begin
          If YesNo ( 'Index file is defective. Rebuild', True ) Then Begin {!!.TP}
            OK1 := RebuildTheFile;
          End;
        End Else Begin
          If IsamError = 9903 Then Begin
            If YesNo ( 'Data file does not exist. Create', True ) {!!.TP}
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
    LockAbort := YesNo ( 'A lock prevents the operation. Abort', {!!.TP}
                          True );
  End;


  Function AddData ( P : PersonDef; Var Ref : LongInt ) : Boolean;

  Begin
    AddData := False;
    Repeat
      BTAddRec ( PF, Ref, P );
      If LockAbort Then Exit;
    Until Not LockEnd;
    AddData := IsamOK;
  End;


  Function ChangeData ( P : PersonDef; Ref : LongInt ) : Boolean;

  Begin
    ChangeData := False;
    Repeat
      BTPutRec ( PF, Ref, P, False );
      If LockAbort Then Exit;
    Until Not LockEnd;
    ChangeData := IsamOK;
  End;


  Function DeleteData ( Ref : LongInt ) : Boolean;

  Begin
    DeleteData := False;
    Repeat
      BTDeleteRec ( PF, Ref );
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
      BTAddKey ( PF, KeyNr, Ref, K );
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
      BTDeleteKey ( PF, KeyNr, Ref, K );
      If LockAbort Then Exit;
    Until Not LockEnd;
    DeleteaKey := IsamOK;
  End;


  Function ChangeaKey ( OldK,                                     {mod !!.03}
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


  Function AddRecAndKeys ( Var Key : IsamKeyStr;                  {mod !!.03}
                           Var Ref : LongInt ) : Boolean;

  Var
    OK : Boolean;

  Begin
    AddRecAndKeys := False;
    Repeat
      BTLockFileBlock ( PF );
      If Not IsamOK Then Begin
        If YesNo ( 'The file is in use. Abort', {!!.TP}
                   True )
            Then Exit;
      End;
    Until IsamOK;
    OK := AddData ( Person, Ref );
    If OK Then Begin
      Key := BuildKey ( Person, 1 );
      OK := AddaKey ( Key, Ref, 1 );
      If Not OK Then If DeleteData ( Ref ) Then;
    End;
    If OK Then Begin
      Key := BuildKey ( Person, 2 );
      OK := AddaKey ( Key, Ref, 2 );
      If Not OK Then Begin
        If DeleteaKey ( Key, Ref, 1 ) Then;
        If DeleteData ( Ref ) Then;
      End;
    End;
    BTUnLockFileBlock ( PF );
    AddRecAndKeys := IsamOK And OK;
  End;


  Function ChangeRecAndKeys ( Var OldPerson : PersonDef;          {mod !!.03}
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
      BTLockFileBlock ( PF );
      If Not IsamOK Then Begin
        If YesNo ( 'The file is in use. Abort', {!!.TP}
                   True )
            Then Exit;
      End;
    Until IsamOK;
    BTGetRec ( PF, Ref, CurrentPerson, False );
    If Not IsamOK Or (CurrentPerson.Dele <> 0) Then Begin
      BTUnLockFileBlock ( PF );
      Exit;
    End;
    If CompStruc ( CurrentPerson, OldPerson, SizeOf (PersonDef) ) <> 0
        Then Begin
      BTUnLockFileBlock ( PF );
      OKBox ( 'Information', 'The record has changed in the meantime.', {!!.TP}
               '');
      Exit;
    End;
    OK := ChangeaKey ( BuildKey ( CurrentPerson, 1 ),
        BuildKey ( Person, 1 ), Ref, 1 );
    If OK Then Begin
      OK := ChangeaKey ( BuildKey ( CurrentPerson, 2 ),
          BuildKey ( Person, 2 ), Ref, 2 );
      If Not OK Then Begin
        If ChangeaKey ( BuildKey ( Person, 1 ),
            BuildKey ( CurrentPerson, 1 ), Ref, 1 ) Then;
      End;
    End;
    If OK Then Begin
      OK := ChangeData ( Person, Ref );
      If Not OK Then Begin
        If ChangeaKey ( BuildKey ( Person, 1 ),
            BuildKey ( CurrentPerson, 1 ), Ref, 1 ) Then;
        If ChangeaKey ( BuildKey ( Person, 2 ),
            BuildKey ( CurrentPerson, 2 ), Ref, 2 ) Then;
      End;
    End;
    BTUnLockFileBlock ( PF );
    ChangeRecAndKeys :=  IsamOK And OK;
  End;


  Function DeleteRecAndKeys ( Ref : LongInt ) : Boolean;          {mod !!.03}

  Var
    OK : Boolean;

  Begin
    DeleteRecAndKeys := False;
    Repeat
      BTLockFileBlock ( PF );
      If Not IsamOK Then Begin
        If YesNo ( 'The file is in use. Abort', {!!.TP}
                   True )
            Then Exit;
      End;
    Until IsamOK;
    OK := DeleteaKey ( BuildKey ( Person, 1 ), Ref, 1 );
    OK := DeleteaKey ( BuildKey ( Person, 2 ), Ref, 2 ) And OK;
    OK := DeleteData ( Ref ) And OK;
    BTUnLockFileBlock ( PF );
    DeleteRecAndKeys := IsamOK And OK;
  End;


  Function SearchForKey (     KeyNr  : Word;
                          Var Key    : IsamKeyStr;
                          Var DatRef : LongInt ) : Boolean;


  Begin
    SearchForKey := False;
    Repeat
      BTSearchKey ( PF, KeyNr, DatRef, Key );
      If LockAbort Then Exit;
    Until Not LockEnd;
    SearchForKey := IsamOK;
  End;


  Procedure AdressBrowser.NewRec;

  Var
    Key : IsamKeyStr;
    Ref : LongInt;

  Begin
    FillChar ( Person, SizeOf (Person), 0 );
    If GetPerson ( False, ' Add ', False ) Then Begin {!!.TP}
      If Not AddRecAndKeys ( Key, Ref ) Then Begin
        OKBox ( 'Error', 'Add operation failed.', '')  {!!.TP}
      End Else Begin
        SetAndUpdateBrowserScreen ( BuildKey ( Person, GetCurrentKeynr ),
                                    Ref );
      End;
    End;
  End;


  Procedure AdressBrowser.EditRec;

  Var
    SavePerson : PersonDef;
    Refnr      : Longint;
    Match      : Boolean;                                         {!!.03}

  Begin
    If  GetCurrentRec ( Match) > 0 Then Begin                     {mod !!.03}
      OkBox ( 'Error', 'Read operation failed.', ''); {!!.TP}
      Exit;
    End;
    If Not Match Then Begin                                       {!!.03}
      OKBox ( 'Information',                                      {!!.03}
              'The record has changed in the meantime.',          {!!.03}{!!.TP}
               '');                                               {!!.03}
      Exit;                                                       {!!.03}
    End;
    RefNr := GetCurrentdatref;
    BtLockRec ( PF, refnr );
    If IsamError > 0 Then OkBox ( 'Error', 'Lock operation failed.', ''); {!!.TP}
    SavePerson := Person;
    If GetPerson ( False, ' Modify ', False ) Then Begin {!!.TP}
      If Not ChangeRecAndKeys ( SavePerson, GetCurrentDatRef )
          Then Begin
        OKBox ( 'Error', 'Modify operation failed.', ''); {!!.TP}
      End Else Begin
        SetAndUpDateBrowserScreen ( BuildKey ( Person, 1 ),
            GetCurrentDatRef );
      End;
    End;
    BtUnLockRec ( PF, refnr );
  End;


  Procedure AdressBrowser.DelRec;

  Var
    SavePerson : PersonDef;
    Match      : Boolean;                                         {!!.03}

  Begin
    If  GetCurrentRec ( Match) > 0 Then Begin                     {mod !!.03}
      OkBox ( 'Error', 'Read operation failed.', '');             {!!.03}{!!.TP}
      Exit;                                                       {!!.03}
    End;                                                          {!!.03}
    If Not Match Then Begin                                       {!!.03}
      OKBox ( 'Information',                                      {!!.03}
              'The record has been changed in the meantime.',     {!!.03}{!!.TP}
               '');                                               {!!.03}
      Exit;                                                       {!!.03}
    End;
    SavePerson := Person;
    If GetPerson ( True, ' Delete ', False ) Then Begin   {!!.TP}
      Person := SavePerson;
      If Not DeleteRecAndKeys ( GetCurrentDatRef ) Then Begin
        OKBox ( 'Error', 'Delete operation failed.', ''); {!!.TP}
      End Else Begin
        UpDateBrowserScreen;
      End;
    End;
  End;


  Procedure AdressBrowser.ToggleKey;

  Var
    Key   : IsamKeyStr;
    Ref   : LongInt;
    Match : Boolean;

  Begin
    If  GetCurrentRec ( Match) > 0 Then Begin                     {mod !!.03}
      OkBox ( 'Error', 'Read operation failed.', '');             {!!.03}{!!.TP}
      Exit;                                                       {!!.03}
    End;                                                          {!!.03}
    SetKeyNr (( GetCurrentKeyNr + 1 ) Mod 3 );                    {!!.04}
    SetAndUpdateBrowserScreen (
        BuildKey ( Person, GetCurrentKeyNr ), GetCurrentDatRef );
  End;

  Procedure AdressBrowser.SearchRec;

  Var
    UK       : IsamKeyStr;
    Ref      : LongInt;
    NewKeyNr : Integer;

  Begin
    FillChar ( Person, SizeOf (Person), 0 );
    NewKeyNr := 0;
    Ref := 0;
    If GetPerson ( False, ' Search ', True ) Then Begin {!!.TP}
      UK := BuildKey ( Person, 1 );
      If Length ( UK ) > 0 Then If UK [1] > ' ' Then Begin
        NewKeyNr := 1;
      End Else Begin
        UK := Buildkey ( Person, 2 );
        If Length ( UK ) > 0 Then If UK [1] > ' ' Then Begin
          NewKeynr := 2;
        End Else begin
          OKBox ( 'Error', 'No search criteria given.','' );{!!.TP}
          Exit;
        End;
      End;
      If Not SearchForKey ( NewKeyNr, UK, Ref ) Then Begin
        OKBox ( 'Error', 'Search operation failed.', '' ) {!!.TP}
      End Else Begin
        SetKeyNr ( NewKeyNr );
        SetAndUpDateBrowserScreen ( UK, Ref );
      End;
    End;
  End;

  Procedure AdressBrowser.SearchByKey ( C : Char );
  Var
    Key : IsamKeyStr;
    DatRef : LongInt;
  Begin
    Key := StUpcase ( C );
    DatRef := 0;
    If SearchForKey ( GetCurrentKeyNr, Key, DatRef ) Then Begin
      SetAndUpdateBrowserScreen ( Key, 0 )
    End;
  End;

  Procedure AdressBrowser.Status;
    {-Show the number of records}

  Var
    F, U, K : LongInt;
    S, S1 : String;
  Begin
    Repeat
      U := BTUsedRecs(Pf);
      If LockAbort then Exit;
    Until Not LockEnd;

    Repeat
      F := BTFreeRecs(Pf);
      If LockAbort Then Exit;
    Until Not LockEnd;
    S := 'Records used :'+Long2Str ( U )+ {!!.TP}
      ', free :'+Long2Str(F);             {!!.TP}
    Case BTNetSupported Of
      NoNet : S1 := 'NoNet';
      Novell : S1 := 'Novell';
      MsNet : S1 := 'MsNet';
    End;
    S1 := 'Network : '+ S1;               {!!.TP}
    OKBox ( 'Status', S, S1 );
  End;


