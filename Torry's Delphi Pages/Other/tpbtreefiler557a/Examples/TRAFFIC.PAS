{*********************************************************}
(*                B-Tree Filer Ver 5.55                  *)
{*     Copyright (c) TurboPower Software Co 1986-99      *}
{*                 All rights reserved.                  *}
(*      Based in part on code written by Ralf Nagel      *)
{*********************************************************}

{-Test program to reach network or multitasking traffic in a given frequency

  Usage:
    Traffic (/N | /M | /O) (/R | /W | /Bx) [/Dy] [/Tz] [/Ff] [/Ll]
  /N : Novell
  /M : MsNet
  /O : without a Net

  /R : read only
  /W : write only
  /B : both read and write (after x reads a write is done)

  /D : delay time of y milliseconds after each operation (default 100ms)

  /T : timeout of z milliseconds for each operation
       (default IsamLockTimeOut ms)

  /F : timeout factor f for fileblock lock in relation to readlock
       (default IsamFBLockTimeOutFactor)

  /L : delay time of l milliseconds after a failed lock attempt
       (default IsamDelayBetwLocks ms)


  If not used with /R the fileblock "TRAFFIC" with TrafficKeys (default 4)
  keys may be created}


{--Conditional defines and compiler options}
  {$I BTDefine.Inc}
  {$F-,V-,B-,S-,I-,R-}
  {$IFDEF CanSetOvrflowCheck}
    {$Q-}
  {$ENDIF}

{$IFDEF Win32}
{$APPTYPE CONSOLE}
{$H-}
{$ENDIF}


Program Traffic;

Uses
{$IFDEF Win32}
  SysUtils, Windows,
{$ELSE}
{$IFDEF Windows}
  {$IFDEF Ver80}
  SysUtils,
  {$ENDIF}
  WinTypes,
  WinProcs,
  WinCrt,
{$ELSE}
  Crt,
{$ENDIF}
{$ENDIF}
  BTBase, BTIsBase,
  Filer;     {!!.TP}

Const
  TrafficFBName   = 'TRAFFIC';
  TrafficKeys     = 4;
    {-Must be in range 4..21}
  TrafficFillSize = 300;
    {-Actual Record size will be 5 + (12 * TrafficKeys) + TrafficFillSize}
  RandomMax = 65535;

Type
  TrafficRecDef = packed Record
    Null : LongInt;
    S    : String [12 * TrafficKeys];
    Fill : Array [1..TrafficFillSize] Of Byte;
  End;

  OperationModeDef = ( ReadOnly, WriteOnly, Both );

  JournalRec = packed Record
    Reads,
    Writes,
    Locks,
    Dups,
    StartTime,
    StopTime  : LongInt;
  End;

Var
  DelayTime          : Word;
  OperationMode      : OperationModeDef;
  NrOfReadsEachWrite : Word;
  NetToInit          : NetSupportType;
  TrafficFB          : IsamFileBlockPtr;
  Journal            : JournalRec;
  SaveExitProc       : Pointer;


  Procedure ShowParams;

  Begin {!!.TP - translations}
    WriteLn;
    WriteLn ('Usage:');

    WriteLn ( '  Traffic (/N | /M | /O) (/R | /W | /Bx) '
        + '[/Dy] [/Tz] [/Ff] [/Ll]' );
    WriteLn;
    WriteLn ( '/N : Novell' );
    WriteLn ( '/M : MsNet (or compatible)' );
    WriteLn ( '/O : No net' );
    WriteLn;
    WriteLn ( '/R : Read only' );
    WriteLn ( '/W : Write only' );
    Writeln ( '/B : both read and write (after x reads a write is done)' );
    WriteLn;
    Writeln ( '/D : delay time of y milliseconds after each operation '
        + '(default 100ms)' );
    WriteLn;
    Writeln ( '/T : timeout of z milliseconds for each operation (default ',
        IsamLockTimeOut, 'ms)' );
    WriteLn;
    Writeln ( '/F : timeout factor f for fileblock lock in relation '
        + 'to readlock (default ', IsamFBLockTimeOutFactor, ')' );
    Writeln;
    Writeln ( '/L : delay time of l milliseconds after a failed '
        + 'lock attempt (default ', IsamDelayBetwLocks, 'ms)' )
  End;


  Function GetParams : Boolean;

  Const
    MaxParams = 6;
    FixParams = 2;

  Var
    P             : String [20];
    Error         : Integer;
    Value         : Word;
    I             : Word;

  Begin
    GetParams := False;

    If Not (ParamCount In [FixParams..MaxParams]) Then Exit;

    P := ParamStr ( 1 );
    If P [1] <> '/' Then Exit;
    Delete ( P, 1, 1);
    If Length ( P ) <> 1 Then Exit;
    Case Upcase (P [1]) Of
      'N' : NetToInit := Novell;
      'M' : NetToInit := MsNet;
      'O' : NetToInit := NoNet;
      Else Exit;
    End; {Case}

    P := ParamStr ( 2 );
    If P [1] <> '/' Then Exit;
    Delete ( P, 1, 1);
    If Length ( P ) = 0 Then Exit;
    Case Upcase (P [1]) Of
      'R' : OperationMode := ReadOnly;
      'W' : OperationMode := WriteOnly;
      'B' : Begin
        Delete ( P, 1, 1);
        Val ( P, NrOfReadsEachWrite, Error );
        If Error <> 0 Then Exit;
        OperationMode := Both;
      End;
      Else Exit;
    End; {Case}

    DelayTime := 100;
    For I := Succ (FixParams) To ParamCount Do Begin
      P := ParamStr ( I );
      If P [1] <> '/' Then Exit;
      Delete ( P, 1, 1);
      If Length ( P ) = 0 Then Exit;
      Case Upcase (P [1]) Of
        'D', 'T', 'F', 'L' : Begin
          Val ( Copy (P, 2, 255), Value, Error );
          If Error <> 0 Then Exit;
          Case Upcase (P [1]) Of
            'D' : DelayTime := Value;
            'T' : IsamLockTimeOut := Value;
            'F' : IsamFBLockTimeOutFactor := Value;
            'L' : IsamDelayBetwLocks := Value;
          End; {Case}
        End;
        Else Exit;
      End; {Case}
    End;

    GetParams := True;
  End;


  Function GetBiosMSCount : LongInt;

  Type
    PLongInt = ^LongInt;

  Begin
  {$IFDEF Win32}
    GetBiosMSCount := GetTickCount;
  {$ELSE}
  {$IFDEF Windows}
    GetBiosMSCount := GetTickCount;
  {$ELSE}
   {$IFDEF DPMI}
    GetBiosMSCount := PLongInt (Ptr (Seg0040, $6C))^ * 55;
   {$ELSE}
    GetBiosMSCount := PLongInt (Ptr ($40, $6C))^ * 55;
   {$ENDIF}
  {$ENDIF}
  {$ENDIF}
  End;

  {$IFDEF Win32}
  function KeyPressed : boolean;
    type
      PInputRecArray = ^TInputRecArray;
      TInputRecArray = array [0..1] of TInputRecord;
    var
      IRA : PInputRecArray;
      Handle : THandle;
      NumEvents : DWORD;
      NumEvRead : DWORD;
      i         : integer;
    begin
      KeyPressed := false;
      Handle := GetStdHandle(STD_INPUT_HANDLE);
      if (Handle = INVALID_HANDLE_VALUE) then
        Exit;
      if not GetNumberOfConsoleInputEvents(Handle, NumEvents) then
        Exit;
      GetMem(IRA, NumEvents * sizeof(TInputRecord));
      try
        if PeekConsoleInput(Handle, IRA^[0], NumEvents, NumEvRead) then
          for i := 0 to pred(NumEvRead) do
            {$IFDEF Delphi4Plus}
            if (IRA^[i].EventType = KEY_EVENT) and
               (IRA^[i].Event.KeyEvent.bKeyDown) then
              begin
                Result := true;
                Exit;
              end;
            {$ELSE}
            if (IRA^[i].EventType = KEY_EVENT) and
               (IRA^[i].KeyEvent.bKeyDown) then
              begin
                Result := true;
                Exit;
              end;
            {$ENDIF}
      finally
        FreeMem(IRA, NumEvents * sizeof(TInputRecord));
      end;
    end;
  function ReadKey : char;
    var
      Handle : THandle;
      NumRead : DWORD;
      IR : TInputRecord;
    begin
      Result := #0;
      Handle := GetStdHandle(STD_INPUT_HANDLE);
      if (Handle = INVALID_HANDLE_VALUE) then
        Exit;
      if not ReadConsoleInput(Handle, IR, 1, NumRead) then
        Exit;
      {$IFDEF Delphi4Plus}
      if (IR.EventType = KEY_EVENT) and
         (IR.Event.KeyEvent.bKeyDown) then
        begin
          Result := char(IR.Event.KeyEvent.AsciiChar);
        end;
      {$ELSE}
      if (IR.EventType = KEY_EVENT) and
         (IR.KeyEvent.bKeyDown) then
        begin
          Result := char(IR.KeyEvent.AsciiChar);
        end;
      {$ENDIF}
    end;
  {$ENDIF}

  Function PrepareFileBlock : Boolean;

  Var
    PSS : LongInt;
    S   : String;

    Function CreateFB : Boolean;

    Var
      IID : IsamIndDescr;
      I   : Word;

    Begin
      CreateFB := False;

      With IID [1] Do Begin
        KeyL := 12;
        AllowDupK := False;
      End;
      For I := 2 To TrafficKeys Do Begin
        With IID [I] Do Begin
          KeyL := 12;
          AllowDupK := True;
        End;
      End;
      BTCreateFileBlock ( TrafficFBName, SizeOf (TrafficRecDef),
          TrafficKeys, IID );
      If Not IsamOK Then Exit;

      CreateFB := True;
    End;


  Begin
    PrepareFileBlock := False;
    WriteLn;
    Randomize;
    {$IFDEF UseWindowsInit}
    PSS := BTInitIsam ( NetToInit, 30 );
    {$ELSE}
    PSS := BTInitIsam ( NetToInit, MemAvail - 100000, 0 );
    {$ENDIF}
    If Not IsamOK Then Exit;
    BTOpenFileBlock ( TrafficFB, TrafficFBName, False, False, False, True );
    If IsamError = 9903 Then Begin
      If OperationMode = ReadOnly Then Exit;
      If Not CreateFB Then Exit;
      BTOpenFileBlock ( TrafficFB, TrafficFBName, False, False, False, True );
      If Not IsamOK Then Exit;
      S := 'created and '; {!!.TP}
    End Else Begin
      If Not IsamOK Then Exit;
      S := '';
    End;

    Writeln ( 'B-Tree Filer initialized with ', PSS Shr 16, '/', {!!.TP}
        PSS And $FFFF, ' page buffers' );                        {!!.TP}
    Writeln ( 'Fileblock TRAFFIC ' + S + 'opened successfully' );{!!.TP}
    Case BTNetSupported Of
      NoNet  : S := 'Supported net: NoNet';                      {!!.TP}
      Novell : S := 'Supported net: Novell';                     {!!.TP}
      MsNet  : S := 'Supported net: MsNet';                      {!!.TP}
    End;
    WriteLn ( ' ==> ' + S + '      Dialog ID: ',                 {!!.TP}
        BTGetInternalDialogID ( TrafficFB ) );
    WriteLn ( 'Abort test with <Ctrl+C>' );                      {!!.TP}
    PrepareFileBlock := True;
  End;


  Function DoTraffic : Boolean;

  Var
    C         : Char;
    ReadCount : Word;
    CurOper   : OperationModeDef;


    Procedure BuildString ( Nr : LongInt; Var DataStr : String );

    Var
      I  : Word;
      St : Array [1..TrafficKeys] Of String [12];

    Begin
      Str ( Nr:12, St [1] );
      St [2] := '            ';
      For I := 1 To 12 Do Begin
        St [2] [I] := St [1] [13 - I];
      End;
      Str ( RandomMax - Nr:12, St [3] );
      For I := 4 To TrafficKeys Do Begin
        Str ( Nr Mod 10 : 12 , St [I] );
      End;
      DataStr := St [2] + St [1];
      For I := 3 to TrafficKeys Do Begin
        DataStr := DataStr + St [I];
      End;
    End;


    Function GetKey ( Var DataStr : String;
                          KeyNr   : Integer ) : String;

    Begin
      GetKey := '';
      Case KeyNr Of
        0:;
        1: GetKey := Copy ( DataStr, 13, 12 );
        2: GetKey := Copy ( DataStr,  1, 12 );
        Else Begin
          GetKey := Copy ( DataStr, Succ (Pred (KeyNr) * 12), 12 );
        End;
      End; {Case}
    End;


    Function AddRecord : Integer;

    Var
      I          : Word;
      RefNr      : LongInt;
      TrafficRec : TrafficRecDef;


    Begin
      TrafficRec.Null := 0;
      BuildString ( Succ (Random (RandomMax)), TrafficRec.S );
      FillChar ( TrafficRec.Fill, SizeOf (TrafficRec.Fill), 0 );
      BTLockFileBlock ( TrafficFB );
      AddRecord := BTIsamErrorClass;
      If Not IsamOK Then Exit;
      If BTKeyExists ( TrafficFB, 1, 0, GetKey ( TrafficRec.S, 1 ) )
          Then Begin
        BTUnLockFileBlock ( TrafficFB );
        If IsamOK Then Begin
          AddRecord := -1;
        End Else Begin
          AddRecord := BTIsamErrorClass;
        End;
        Exit;
      End;
      BTAddRec ( TrafficFB, RefNr, TrafficRec );
      If Not IsamOK Then Begin
        AddRecord := BTIsamErrorClass;
        BTUnLockFileBlock ( TrafficFB );
        If Not IsamOK Then Begin
          AddRecord := BTIsamErrorClass;
        End;
        Exit;
      End;
      For I := 1 To TrafficKeys Do Begin
        BTAddKey ( TrafficFB, I, RefNr,  GetKey ( TrafficRec.S, I ) );
        If Not IsamOK Then Begin
          AddRecord := BTIsamErrorClass;
          BTUnLockFileBlock ( TrafficFB );
          If Not IsamOK Then Begin
            AddRecord := BTIsamErrorClass;
          End;
          Exit;
        End;
      End;
      BTUnLockFileBlock ( TrafficFB );
      AddRecord := BTIsamErrorClass;
    End;


    Function GetRecord : Integer;

    Var
      RefNr      : LongInt;
      TrafficRec : TrafficRecDef;
      MyKey      : IsamKeyStr;
      MyKeyNr    : Word;

    Begin
      BuildString ( Succ (Random (RandomMax)), TrafficRec.S );
      BTReadLockFileBlock ( TrafficFB );
      GetRecord := BTIsamErrorClass;
      If Not IsamOK Then Exit;
      MyKeyNr := Succ (Random (TrafficKeys));
      MyKey := GetKey ( TrafficRec.S, MyKeyNr );
      RefNr := Succ (Random (RandomMax));

      BTSearchKeyAndRef ( TrafficFB, MyKeyNr, RefNr, MyKey );
      If Not IsamOK Then Begin
        GetRecord := BTIsamErrorClass;
        BTUnLockFileBlock ( TrafficFB );
        If Not IsamOK Then Begin
          GetRecord := BTIsamErrorClass;
        End;
        Exit;
      End;
      BTGetRec ( TrafficFB, RefNr, TrafficRec, False );
      GetRecord := BTIsamErrorClass;
      BTUnLockFileBlock ( TrafficFB );
      If Not IsamOK Then Begin
        GetRecord := BTIsamErrorClass;
      End;
    End;


  Begin
    DoTraffic := False;
    ReadCount := 0;
    CurOper := OperationMode;
    {$IFNDEF Win32}
    CheckBreak := False;
    {$ENDIF}
    FillChar (Journal, SizeOf (Journal), 0);
    Journal.StartTime := GetBiosMSCount;
    C := #0;
    Repeat
      If OperationMode = Both Then Begin
        If ReadCount >= NrOfReadsEachWrite Then Begin
          CurOper := WriteOnly;
          ReadCount := 0;
        End Else Begin
          CurOper := ReadOnly;
          Inc (ReadCount);
        End;
      End;
      Case CurOper Of
        WriteOnly : Begin
          Case AddRecord Of
            -1 : Begin
               Write ( 'O' );
               Inc (Journal.Dups);
            End;
            0 : Begin
              Write ( ':' );
              Inc (Journal.Writes);
            End;
            2 : Begin
              Write ( 'X' );
              Inc(Journal.Locks);
            End;
            Else Exit;
          End; {Case}
        End;
        ReadOnly : Begin
          Case GetRecord Of
            0 : Begin
              Write ( '.' );
              Inc (Journal.Reads);
            End;
            1 : Begin
              Write ( 'o' );
            End;
            2 : Begin
              Write ( 'x' );
              Inc (Journal.Locks);
            End;
            Else Exit;
          End; {Case}
        End;
      End; {Case}
      IsamDelay ( DelayTime );
      If KeyPressed Then C := ReadKey;
    Until C = #3;
    WriteLn;
    DoTraffic := True;
  End;


  Procedure DisplayJournal; {!!.TP - translations}

  Var
    CurTime,
    TimeDiff,
    TotalOps : LongInt;

  Begin
    With Journal Do Begin
      StopTime := GetBiosMSCount;
      TimeDiff := (StopTime - StartTime) Div 1000;
      If TimeDiff < 1 Then TimeDiff := 1;

      Case OperationMode Of
        ReadOnly  : TotalOps := Reads;
        WriteOnly : TotalOps := Writes;
        Both      : TotalOps := Reads + Writes;
      End;

      WriteLn;
      WriteLn ( ' Summary ' );
      WriteLn ( ' ======= ' );
      WriteLn ( ' Test running time    : ', TimeDiff, ' seconds' );
      WriteLn;

      Write ( ' Read operations      : ', Reads : 10 );
      If Reads <> 0 Then Begin
        WriteLn ( '     (approx. ', Reads / TimeDiff : 6 : 2, ' per second)' );
      End Else Begin
        WriteLn;
      End;

      Write ( ' Write operations     : ', Writes : 10 );
      If Writes <> 0 Then Begin
        WriteLn ( '     (approx. ', Writes / TimeDiff : 6 : 2, ' per second)' );
        If Dups <> 0 Then Begin
          Write ( '           duplicates : ', Dups : 10 );
          WriteLn ( '     (about every ', Writes Div Dups,
              ' writes)' );
        End;
      End Else Begin
        WriteLn;
      End;

      Write ( ' File lock failures   : ', Locks : 10 );
      If Journal.Locks <> 0 Then Begin
        WriteLn ( '     (about every ', TotalOps Div Locks,
            ' locks)' );
      End Else Begin
        WriteLn;
      End;
    End;
  End;


  {$F+}
  Procedure TrafficExitProc;

  Begin
    If Not IsamOK Then Begin
      WriteLn;
      WriteLn ( 'IsamError: ', IsamError );
    End;
    ExitProc := SaveExitProc;
  End;

{$IFDEF Win32}
var
  Handle : THandle;
{$ENDIF}

Begin {!!.TP - translations}
  {$IFDEF Win32}
  Handle := GetStdHandle(STD_INPUT_HANDLE);
  SetConsoleMode(Handle, 0);
  {$ELSE}
  ClrScr;
  {$ENDIF}

  If BTNoNetCompiled Then Begin
    WriteLn;
    WriteLn ( 'This test program can only be used when B-Tree Filer has' );
    WriteLn ( 'been compiled for network mode.' );
    WriteLn ( 'Please activate one of the network compiler defines in');
    WriteLn ( 'BTDEFINE.INC, rebuild the FILER unit, recompile this');
    WriteLn ( 'program and try again.');

    Halt;
  End;

  If Not GetParams Then Begin
    ShowParams;
    {$IFDEF Win32}
    repeat until KeyPressed;
    {$ENDIF}
    Halt;
  End;

  SaveExitProc := ExitProc;
  ExitProc := @TrafficExitProc;

  If Not PrepareFileBlock Then Halt;

  If DoTraffic Then Begin
    BTCloseFileBlock ( TrafficFB );
    If IsamOK Then BTExitIsam;
  End;

  DisplayJournal;
  {$IFDEF Win32}
  repeat until KeyPressed;
  {$ENDIF}
End.


