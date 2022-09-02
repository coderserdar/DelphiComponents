{$B-,S-,R-,V-,X+}

{*********************************************************}
(*                B-Tree Filer Ver 5.55                  *)
{*     Copyright (c) TurboPower Software Co 1986-99      *}
{*                 All rights reserved.                  *}
(*      Based in part on code written by Ralf Nagel      *)
{*********************************************************}

{$I BTDEFINE.INC}

{$IFDEF Win32}
  !! Error - this unit cannot be compiled for 32-bit
{$ENDIF}

program TTSFiler;
  {-Demo program of how to use TTS with B-Tree Filer.}

uses
  {$IFDEF Windows}
  {$IFDEF Ver80}
  SysUtils,
  {$ENDIF}
  WinCrt,
  WinDOS,
  {$ELSE}
  Crt,
  DOS,
  {$ENDIF}
  BTBase, BTIsBase,
  Filer,
  NWBase,
  NWConn,
  NWFile,
  NWTTS;

type
  TTestRec = record
    DelStatus : longint;
    Name      : string[30]; {Random string to similate a name}
    Node      : string[10]; {Random string to similate a numeric node}
  end;

const
  RecsToAdd = 1000;
  TestFileName = 'TTSTEST';
  CommitRetries = 100;

{$DEFINE UseTTS}  {for speed testing: test with this on and off}

var
  Server : TnwServer;
  NWError : TnwErrorCode;
  TransactionActive : boolean;
  IFB : IsamFileBlockPtr;
  RecCount : longint;
  IDs      : array[1..RecsToAdd] of nwLong;


procedure CheckIsamError(Msg : string);
  begin
    if not IsamOK then
      begin
        writeln(Msg, ', IsamError: ', IsamError);
        Halt(1);
      end;
  end;

procedure CheckNWError(Msg : string);
  begin
    if (NWerror <> 0) then
      begin
        writeln(Msg, ', NetWare Error: ', NWError);
        Halt(1);
      end;
  end;

procedure Error(Msg : string);
  begin
    writeln(Msg);
    Halt(1);
  end;

procedure Warning(Msg : string);
  begin
    writeln(Msg);
  end;

procedure BeginTransaction;
  {-Start a transaction if one is not already active}
  begin
    {$IFDEF UseTTS}
    NWerror := nwTTSBegin(Server);
    CheckNWerror('TTSBegin');
    TransactionActive := true;
    {$ENDIF}
  end;

procedure EndTransaction;
  {-End a transaction and commit it to disk}
  var
    Retries: integer;
    ID     : nwLong;
  begin
    {$IFDEF UseTTS}
    if TransactionActive then
      begin
        TransactionActive := false;
        NWerror := nwTTSEnd(Server, ID);
        CheckNWError('TTSEnd');
        IDs[RecCount] := ID;
      end;
    {$ENDIF}
  end;

procedure AbortTransaction;
  {-Abort a transaction and rollback}
  begin
    {$IFDEF UseTTS}
    if TransactionActive then
      begin
        TransactionActive := false;
        BTUnlockFileBlock(IFB);
        CheckIsamError('BTUnlockFileBlock');
        NWError := nwTTSAbort(Server);
        CheckNWError('TTSAbort failed');
        BTInformTTSAbortSuccessful(IFB);
        CheckIsamError('BTInformTTSAbortSuccessful');
      end;
    {$ENDIF}
  end;

procedure BuildKey(var DatS; KeyNr : integer; var KeyStr : IsamKeyStr);
  var
    P : TTestRec absolute DatS;
  begin
    case KeyNr of
      1 : KeyStr := P.Name;
      2 : KeyStr := P.Node;
    end;{case}
  end;

function AddRecord(var P : TTestRec) : boolean;
  var
    KeyNr : integer;
    RefNr : longint;
    Key   : IsamKeyStr;
  begin
    AddRecord := false;

    {Lock the fileblock}
    BTLockFileBlock(IFB);
    if not IsamOK then
      Exit;

    {Begin transaction}
    BeginTransaction;

    {Add record and keys}
    BTAddRec(IFB, RefNr, P);
    if not IsamOK then
      begin
        Warning('Error adding rec');
        AbortTransaction;
        Exit;
      end;

    for KeyNr := 1 to BTNrOfKeys(IFB) do
      begin
        BuildKey(P, KeyNr, Key);
        BTAddKey(IFB, KeyNr, RefNr, Key);
        if not IsamOK then
          begin
            Warning('Error adding key');
            AbortTransaction;
            Exit;
          end;
      end;

    {Finish the transaction}
    EndTransaction;
    BTUnLockFileBlock(IFB);
    CheckIsamError('AddRecord.BTUnlockFileBlock');

    AddRecord := true;
  end;

procedure MarkForTTS(RootName : string; DoIt : boolean);
  var
    FAttr : byte;
    ExtFAttr : byte;
    FName : string;
  begin
    {$IFDEF UseTTS}
    FName := RootName+'.'+DatExtension;
    NWError := nwGetFileAttr(FName, FAttr, ExtFAttr);
    CheckNWError('nwGetFileAttr.DAT');
    if DoIt then
      ExtFAttr := ExtFAttr or nweaTransactional
    else
      ExtFAttr := ExtFAttr and not nweaTransactional;
    NWError := nwSetFileAttr(FName, FAttr, ExtFAttr);
    CheckNWError('nwSetFileAttr.DAT');

    FName := RootName+'.'+IxExtension;
    NWError := nwGetFileAttr(FName, FAttr, ExtFAttr);
    CheckNWError('nwGetFileAttr.IX');
    if DoIt then
      ExtFAttr := ExtFAttr or nweaTransactional
    else
      ExtFAttr := ExtFAttr and not nweaTransactional;
    NWError := nwSetFileAttr(FName, FAttr, ExtFAttr);
    CheckNWError('nwSetFileAttr.IX');
    {$ENDIF}
  end;

procedure CreateFile;
  var
    IID : IsamIndDescr;
  begin
    IID[1].KeyL := 30;
    IID[1].AllowDupK := True;
    IID[2].KeyL := 10;
    IID[2].AllowDupK := False;

    {create files}
    BTCreateFileBlock(TestFileName, sizeof(TTestRec), 2, IID);
    CheckIsamError('Create');

    {mark files transactional}
    MarkForTTS(TestFileName, true);

    {open the file in network mode}
    BTOpenFileBlock(IFB, TestFileName, False, False, False, True);
    CheckIsamError('Open');
  end;

function RandomStr(MinLen, MaxLen : integer;
                   MinChar, MaxChar : char) : string;
  var
    Len : integer;
    i   : integer;
    Res : string;
  begin
    Len := MinLen + Random(MaxLen-MinLen+1);

    Res[0] := char(Len);
    for i := 1 to Len do
      Res[i] := char(ord(MinChar) + Random(ord(MaxChar)-ord(MinChar)+1));

    RandomStr := Res;
  end;

function GetTimeInSecs : longint;
  var
    H, M, S, T : word;
  begin
    GetTime(H, M, S, T);
    GetTimeInSecs := ((((longint(H) * 60) + M) * 60) + S);
  end;

procedure AddRecords;
  var
    i : integer;
    Retries : integer;
    StartTime, EndTime : longint;
    TR : TTestRec;
    Ch : char;
  begin
    writeln('Adding records and keys. Press Esc to stop.');
    StartTime := GetTimeInSecs;
    RandSeed := 0;

    Ch := #0;
    RecCount := 0;
    while (RecCount < RecsToAdd) and (Ch <> #27) do
      begin
        inc(RecCount);
        write(^M, RecCount:6, '          ');
        FillChar(TR, sizeof(TR), 0);
        TR.Name := RandomStr(10, 30, 'a', 'z');
        TR.Node := RandomStr(8, 10, '0', '9');
        AddRecord(TR);
        if KeyPressed then
          Ch := ReadKey;
      end;
    writeln;

    {$IFDEF UseTTS}
    writeln('Checking transactions to be committed...');
    for i := 1 to RecCount do
      begin
        write(^M, i:7);
        Retries := 0;
        repeat
          if nwTTSIsCommitted(Server, IDs[i]) then
            Retries := CommitRetries
          else
            begin
              inc(Retries);
              IsamDelay(100); {1/10th second}
            end;
        until (Retries >= CommitRetries);
      end;
    writeln;
    {$ENDIF}

    EndTime := GetTimeInSecs;

    writeln('Time to add ', RecCount, ' records: ', EndTime-StartTime, ' seconds');
    if (RecCount > 0) then
      writeln('  ..average: ', (EndTime-StartTime)/RecCount:5:2, ' secs/record');
  end;

var
  ServerName : TnwServerName;
  VolumeName : TnwVolumeName;
  SI         : TnwServerInfo;
  Path       : string;

begin
  if (nwShellType = nsNone) then
    Error('NetWare not loaded');

  Server := nwDefaultServer;
  NWerror := nwGetServerInfo(Server, SI);
  CheckNWerror('Get server info');
  writeln('Default server is ', SI.ServerName);

  NWerror := nwParseFileName(TestFileName, Server, ServerName, VolumeName, Path);
  CheckNWerror('Parse file name');
  if (ServerName = '') then
    Error('Not on NetWare drive');
  writeln('Using server ', ServerName);

  {$ifdef UseTTS}
  if not nwTTSAvailable(Server) then
    Error('Requires TTS');
  {$endif}

  {initialize B-Tree Filer (give it 100000 bytes)}
  {$IFDEF UseWindowsInit}
  BTInitIsam(Novell, 30);
  {$ELSE}
  BTInitIsam(Novell, MemAvail - 100000, 0);
  {$ENDIF}
  CheckIsamError('BTInitIsam');

  {create a new fileblock and add records to it}
  CreateFile;
  AddRecords;
  BTCloseFileBlock(IFB);
  CheckIsamError('BTCloseFileBlock');

  {shut down B-Tree Filer}
  BTExitIsam;

  {mark data and index files normal}
  MarkForTTS(TestFileName, false);
end.
