{$I fsdefine.inc}

{.$DEFINE TranLogging}

Unit fssrtran;

Interface

Uses
  {$IFDEF TranLogging}
  fslllog,
  {$ENDIF}
  Windows,
  Messages,
  SysUtils,
  fsllbase,
  fslleng,
  fssrlock,
  fssrbase;

Type

  { TfsSrcTransactionMgr tracks active transactions.  Each instance of
    TfsSrcFolder has its own instance of TfsSrcTransactionMgr as the scope
    of a transaction is limited to the tables within one database.

    Any number of transactions may be active per TfsSrcFolder.  However, only
    one transaction may be active per logical client database.  This limitation
    is in place to provide for backwards compatibility:

    1. Existing applications call TffDatabase.StartTransaction.
    2. Existing applications can make mods to several cursors within the
       context of one transaction.

    If a client app needs multiple transactions per physical database then
    it should open several TffDatabase objects on the same alias.  The client
    may then have 1 transaction per TffDatabase.

    When freed, the transaction manager stores its NextLSN in a binary config
    file located in the folder.  When a transaction manager is next created
    for that folder, it will open the config file in Exclusive mode so that
    no other FF server may access the same folder.

    The format of the config file is as follows:

    Bytes        Contents
    -----        --------------------------------------------------
    1 - 4        NextLSN for tables in this directory.
    5 - 12       TDateTime of the last LSN rollover.
  }
  TfsSrcTransactionMgr = Class(TFSSpecObject)
  Protected {private}
    {$IFDEF TranLogging}
    FEventLog: TffEventLog;
    {$ENDIF}

    FBufMgr: TfsBufferManager;
    {-The buffer manager handling file access. }
    FByDatabase: TFSSpecThreadList;
    {-Transactions indexed by DatabaseID.  Contains instances of
      TfsIntListItem.  The ExtraData property of each TfsIntListItem
      contains a reference to the transaction. }
    FCommitLSN: TffWord32;
    {-The starting LSN of the oldest uncommitted transaction. }
    FConfigFile: PffFileInfo;
    {-The binary file containing the transaction manager's persistent
      information. }
    FLockMgr: TfsLockManager;
    {-The lock manager coordinating locks for this database. }
    FLSNRolltime: TDateTime;
    {Time of last LSN rollover.}
    FNextLSN: TffWord32;
    {-The next LSN to be assigned to a transaction. }
    FPath: String;
    {-The directory for which this object manages transactions. }
    FPortal: TfsReadWritePortal;
    {-Used to control access to CommitLSN. }
    FReadOnly: boolean;
    {-Used to control whether or not the last LSN is to be preserved
      on disk. }
    FTranList: TFSSpecThreadList;
    {-Holds the list of active transactions sorted by
      transaction ID. }
  Protected

    Function tmGetCommitLSN: TffWord32;
    {-Used to retrieve the CommitLSN. }

    Function tmGetCount: Longint;
    {-Returns the number of active transactions. }

    Function tmGetLSNForTable(Const FullTableName: TffFullFileName) {!!.06}
    : TffWord32; {!!.06}

    Function tmGetLSNFromTables: TffWord32;
    {-Retrieves the NextLSN based on the number stored in the
      header of the tables in the database.}

    Function tmGetTransItem(Find: TfsListFindType; Value: Longint): TfsSrcTransaction;
    {-Find an active transaction by ID or index. }

    Procedure tmHandleLSNRollover;
    {-Used to handle a NextLSN rollover. }

    Procedure tmReadConfig;
    {-Used to retrieve the transaction manager's last LSN from the config
      file. }

    Procedure tmRecalcCommitLSN;
    {-Used to recalculate the CommitLSN after a commit or rollback. }

    Function tmValidConfigFile: Boolean;
    {-Returns True if config file exists and its file time is
      greater than all the tables in the database.}

    Procedure tmWriteConfig(Const CloseFile: Boolean); {!!.13}
    {-Used to store the transaction manager's last LSN in a config file. }

  Public
    Constructor Create(aBufferMgr: TfsBufferManager;
      aLockMgr: TfsLockManager;
      Const aPath: String;
      Const aReadOnly: boolean);
    Destructor Destroy; Override;

    Function Commit(Const aTransID: TffTransID;
      Var wasNested: boolean): TffResult;
    { Commit a transaction. Returns DBIERR_NONE if the commit was
      successful. Output parameter wasNested is set to True if a nested
      transaction was committed. Otherwise it is set to False indicating
      a transaction was fully committed. }

    Procedure Rollback(Const aTransID: TffTransID; Var wasNested: boolean);
    { Rollback a transaction. }

    Function StartTransaction(Const aDatabaseID: TffDatabaseID;
      Const aFailSafe, aImplicit,
      ReadOnly: boolean;
      Const path: TffPath;
      Var aTran: TfsSrcTransaction): TffResult;
    { Starts a new transaction. }

    Property CommitLSN: TffWord32 Read tmGetCommitLSN;
    { Returns the starting LSN of the oldest uncommitted transaction.
      For now, this is really longInt(Self) of the oldest uncommitted
      transaction. }

    Property Count: Longint Read tmGetCount;
    { Returns the number of active transactions. }

    Property IsReadOnly: boolean Read FReadOnly;
    { If False then the transaction manager stores its last LSN in a
      config file when the transaction manager is freed (i.e., when the
      FF server is shutdown). }

    Property NextLSN: TffWord32 Read FNextLSN;
    { The next LSN to be assigned to a transaction. }

    Property Path: String Read FPath;
    { The directory for which this object manages transactions. }

  End;

Implementation

Uses
  Classes,
  fssrbde,
  fsllexcp;

Const
  { Config file }
  fsc_ConfigFile = 'srvtran.trn';
  fsc_ConfigExt: String[fscl_Extension] = 'trn';

  {$I FsCONST.INC}

  {===TfsSrcTransactionMgr===============================================}

Constructor TfsSrcTransactionMgr.Create(aBufferMgr: TfsBufferManager;
  aLockMgr: TfsLockManager;
  Const aPath: String;
  Const aReadOnly: boolean);
Begin
  Inherited Create;
  FBufMgr := aBufferMgr;
  FByDatabase := TFSSpecThreadList.Create;
  FCommitLSN := High(TffWord32);
  FLockMgr := aLockMgr;
  FLSNRollTime := 0;
  FNextLSN := 1;
  FPath := aPath;
  FPortal := TfsReadWritePortal.Create;
  FReadOnly := aReadOnly;
  FTranList := TFSSpecThreadList.Create;

  {$IFDEF TranLogging}
  FEventLog := TffEventLog.Create(Nil);
  FEventLog.FileName := '.\FsTran.LG';
  FEventLog.Enabled := True;
  FEventLog.WriteString(format('Transaction Mgr started for %s',
    [aPath]));
  {$ENDIF}

  If Not FReadOnly Then
    tmReadConfig;
End;
{--------}

Destructor TfsSrcTransactionMgr.Destroy;
Begin
  {$IFDEF TranLogging}
  FEventLog.WriteString('Destroying transaction mgr');
  {$ENDIF}
  If Not FReadOnly Then
    tmWriteConfig(True); {!!.13}
  FByDatabase.Free;
  FPortal.Free;
  FTranList.Free;
  {$IFDEF TranLogging}
  FEventLog.WriteStrings(['',
    format('Transaction Mgr stopped for %s',
      [FPath])]);
  FEventLog.Free;
  {$ENDIF}

  Inherited Destroy;
End;
{--------}

Function TfsSrcTransactionMgr.Commit(Const aTransID: TffTransID;
  Var wasNested: boolean): TffResult;
Var
  aTran: TfsSrcTransaction;
Begin
  Result := DBIERR_NONE;
  aTran := tmGetTransItem(ftFromID, aTransID);
  If assigned(aTran) Then
    Begin

      { Tell the buffer manager to commit the changes. }
      FBufMgr.CommitTransaction(aTran);

      { Have all changes been committed to disk? }
      If aTran.TransLevel.Level = 0 Then
        Begin {!!.10}

          FLockMgr.ReleaseTransactionLocks(aTran, False);

          { Yes. Remove the index entry for the transaction. }
          {$IFDEF TranLogging}
          FEventLog.WriteString('Commit: Delete transaction from FByDatabase.');
          {$ENDIF}
          With FByDatabase.BeginWrite Do
            Try
              Delete(aTran.DatabaseID);
            Finally
              EndWrite;
            End;

          { Remove the transaction from the list. }
          With FTranList.BeginWrite Do
            Try
              Delete(aTran);
              tmRecalcCommitLSN;
            Finally
              EndWrite;
            End;

          wasNested := False;
        End
      Else
        Begin
          { No. We just committed a nested transaction.  Decrement the nesting
            level. }
          aTran.EndNested; {!!.10}
          wasNested := True;
        End;
    End
  Else
    Begin
      Result := DBIERR_INVALIDHNDL;
    End;
End;
{--------}

Procedure TfsSrcTransactionMgr.Rollback(Const aTransID: TffTransID;
  Var wasNested: boolean);
Var
  aTran: TfsSrcTransaction;
Begin
  aTran := tmGetTransItem(ftFromID, aTransID);
  If assigned(aTran) Then
    Begin

      { Tell the buffer manager to rollback the changes. }
      FBufMgr.RollbackTransaction(aTran);

      { Did we rollback a nested transaction? }
      If aTran.TransLevel.Level = 0 Then
        Begin {!!.10}
          { No. Release the locks held by the transaction. }
          FLockMgr.ReleaseTransactionLocks(aTran, False);

          { Remove the index entry for the transaction. }
          {$IFDEF TranLogging}
          FEventLog.WriteString('Rollback: Delete transaction from FByDatabase.');
          {$ENDIF}
          With FByDatabase.BeginWrite Do
            Try
              Delete(aTran.DatabaseID);
            Finally
              EndWrite;
            End;

          { Remove the transaction from the list. }
          With FTranList.BeginWrite Do
            Try
              Delete(aTran);
              tmRecalcCommitLSN;
            Finally
              EndWrite;
            End;

          wasNested := False;

        End
      Else
        Begin
          { Yes.  Decrement the nesting level. }
          aTran.EndNested; {!!.10}
          wasNested := True;
        End;
    End;
End;
{--------}

Function TfsSrcTransactionMgr.StartTransaction(Const aDatabaseID: TffDatabaseID;
  Const aFailSafe, aImplicit,
  ReadOnly: boolean;
  Const path: TffPath;
  Var aTran: TfsSrcTransaction): TffResult;
Var
  anIndex: Longint;
  anItem: TfsIntListItem;
  JnlFileName: TffFullFileName;
  TranName: TffShStr;
Begin
  { Assumption: A client may have multiple databases but one instance of a
    TfsSrcDatabase is unique to a client. }

  Result := DBIERR_NONE;

  { Has a transaction already been started by this particular database? }
  {$IFDEF TranLogging}
  FEventLog.WriteString('StartTran: Obtain read access to FByDatabase.');
  {$ENDIF}
  With FByDatabase.BeginRead Do
    Try
      anIndex := Index(aDatabaseID);
      {Begin move !!.06}
            { Does a transaction already exist on the database? }
      If anIndex > -1 Then
        Begin
          { Yes. Get the transaction. }
          aTran := TfsSrcTransaction(TfsIntListItem(FByDatabase.Items[anIndex]).ExtraData);

          { Increase its nesting level. }
          aTran.StartNested; {!!.10}
        End;
      {End move !!.06}
    Finally
      EndRead;
      {$IFDEF TranLogging}
      FEventLog.WriteString('StartTran: End read access to FByDatabase.');
      {$ENDIF}
    End;

  {!!.06 - Code moved to previous try..finally block. }
  If anIndex = -1 Then
    Begin
      { No. Create a new transaction. }
      aTran := TfsSrcTransaction.Create(aDatabaseID, aImplicit, ReadOnly);

      Try
        { Add the transaction to the active transaction list. }
        With FTranList.BeginWrite Do
          Try
            Insert(aTran);
            aTran.LSN := FNextLSN;
            If FNextLSN = high(TffWord32) Then
              tmHandleLSNRollover
            Else
              inc(FNextLSN);
          Finally
            EndWrite;
          End;

        { Add an index entry on the transaction's cursorID. }
        anItem := TfsIntListItem.Create(aDatabaseID);
        anItem.ExtraData := pointer(aTran);
        {$IFDEF TranLogging}
        FEventLog.WriteString('StartTran: Insert transaction into FByDatabase.');
        {$ENDIF}
        With FByDatabase.BeginWrite Do
          Try
            Insert(anItem);
          Finally
            EndWrite;
            {$IFDEF TranLogging}
            FEventLog.WriteString('StartTran: Finished insert transaction ' +
              'into FByDatabase.');
            {$ENDIF}
          End;

        { Determine the name of the journal file. }
        If aFailSafe Then
          Begin
            JnlFileName := path;
            FFShStrAddChar(JnlFileName, '\');
            Str(aTran.TransactionID, TranName);
            FFShStrConcat(JnlFileName, TranName);
            FFShStrAddChar(JnlFileName, '.');
            FFShStrConcat(JnlFileName, fsc_ExtForTrans);
          End
        Else
          JnlFileName := '';

        { Recalculate the CommitLSN. }
        If Not ReadOnly Then
          Begin
            FPortal.BeginWrite;
            Try
              { Update the commitLSN. }
              FCommitLSN := FFMinDW(FCommitLSN, aTran.LSN);
              { Update the buffer manager's commitLSN. }
    //        FBufMgr.CommitLSN := FCommitLSN;                             {Deleted !!.10}
            Finally
              FPortal.EndWrite;
            End;
          End;

        { Tell the buffer manager to start tracking changes for this transaction. }
        FBufMgr.StartTransaction(aTran, aFailSafe, JnlFileName);

      Except
        If assigned(aTran) Then
          aTran.Free;
        Raise;
      End;
    End;
End;
{--------}

Function TfsSrcTransactionMgr.tmGetCommitLSN: TffWord32;
Begin
  FPortal.BeginRead;
  Try
    Result := FCommitLSN;
  Finally
    FPortal.EndRead;
  End;
End;
{--------}

Function TfsSrcTransactionMgr.tmGetCount: Longint;
Begin
  With FTranList.BeginRead Do
    Try
      Result := FTranList.Count;
    Finally
      EndRead;
    End;
End;
{--------}

Function TfsSrcTransactionMgr.tmGetLSNForTable(Const FullTableName: TffFullFileName) {!!.06 - Added}
: TffWord32;
{!!.07 - Rewritten}
Var
  FileHandle: Integer;
Begin
  FileHandle := FileOpen(FullTableName, fmOpenRead);
  Try
    { The LSN is stored in position 12 of block 0. }
    If ((FileSeek(FileHandle, 12, 0) <> 12) Or
      (FileRead(FileHandle, Result, SizeOf(TffWord32)) <> SizeOf(TffWord32))) Then
      Result := 0;
  Finally
    FileClose(FileHandle);
  End;
End; {!!.06 - End added}
{--------}

Function TfsSrcTransactionMgr.tmGetLSNFromTables: TffWord32;
Var
  SearchRec: TSearchRec;
  {CurrFile  : TFileStream;}{!!.06 - Deleted}
  TempLSN: TffWord32;
  Continue: Boolean;
Begin
  Result := 0;
  If FindFirst(FPath + '\*.' + fsc_ExtForData, faAnyFile, SearchRec) = 0 Then
    Begin
      Continue := True;
      While Continue Do
        Begin
          Try
            TempLSN := tmGetLSNForTable(FFMakeFullFileName(FPath, SearchRec.Name)); {!!.06 - Moved functionality this method}
            If (TempLSN > Result) Then
              Result := TempLSN;
            Continue := FindNext(SearchRec) = 0;
          Except
            Continue := FindNext(SearchRec) = 0;
          End;
        End;
      FindClose(SearchRec);
    End;
  {We have no idea when the last LSN rollover was so we just set it
   to 0.}
  FLSNRollTime := 0;
  {Since the tables store the last used LSN we need to increment our
   result to get the NextLSN.}
  Inc(Result);
End;
{--------}

Function TfsSrcTransactionMgr.tmGetTransItem(Find: TfsListFindType;
  Value: Longint): TfsSrcTransaction;
Var
  Inx: Integer;
Begin
  { Assumption: Caller has not read- or write-locked the transaction list. }
  Result := Nil;
  With FTranList.BeginRead Do
    Try
      If (Find = ftFromID) Then
        Begin
          Inx := FTranList.Index(Value);
          If (Inx <> -1) Then
            Result := TfsSrcTransaction(FTranList[Inx]);
        End
      Else {Find = ftFromIndex}  If (0 <= Value) And (Value < FTranList.Count) Then
        Result := TfsSrcTransaction(FTranList[Value]);
    Finally
      EndRead;
    End;
End;
{--------}

Procedure TfsSrcTransactionMgr.tmHandleLSNRollover;
Var
  anIndex: Longint;
  LSNAdjustment: TffWord32;
  NewLSN: TffWord32;
Begin

  { Assumption: Transaction list is already write-locked. }

  { The situation is as follows:

    1. We have reached the max LSN.
    2. A bunch of RAM pages are marked with LSNs < max(LSN).

    We have to rollover the LSN but we also need to adjust the LSNs on
    the RAM pages.

    RAM pages that are not part of a transaction will have their LSNs set
    to 1.

    We will set the CommitLSN to 2 and then adjust each transaction's LSN
    based upon the difference between its current LSN and CommitLSN.

    The NextLSN will then be set to highest adjusted LSN + 1.
  }

  { Write lock the CommitLSN. }
  FPortal.BeginWrite;
  Try
    { Calculate the adjustment. }
    LSNAdjustment := FCommitLSN - 2;

    { Set the new CommitLSN. }
    FCommitLSN := 2;

    { Set the rollover time.}
    FLSNRollTime := Now;

    { Init next LSN. }
    FNextLSN := 0;

    { Obtain a lock on the buffer manager's internal data structures.  This
      ensures no other threads mess with the RAM pages. }
    FBufMgr.BeginWrite;
    Try
      { Adjust the LSN of each transaction. }
      For anIndex := 0 To pred(FTranList.Count) Do
        Begin
          NewLSN := TfsSrcTransaction(FTranList.Items[anIndex]).AdjustLSN(LSNAdjustment);
          FNextLSN := FFMaxDW(FNextLSN, NewLSN);
        End;
      inc(FNextLSN);

      { Set the LSN of the other RAM pages. }
      FBufMgr.HandleLSNrollover;
    Finally
      FBufMgr.EndWrite;
    End;

  Finally
    FPortal.EndWrite;
  End;
End;
{--------}

Procedure TfsSrcTransactionMgr.tmReadConfig;
{ Revised !!.13}
Var
  PFileName: PAnsiChar;
Begin
  {$IFDEF TranLogging}
  FEventLog.WriteStrings(['',
    format('Tran Mgr tmReadConfig: %s',
      [FPath])]);
  {$ENDIF}

  { Allocate an in-memory structure for the config file & see if the config
    file exists. }
  FConfigFile := FFAllocFileInfo(FFMakeFullFileName(FPath, fsc_ConfigFile),
    fsc_ConfigExt, Nil);
  FFGetMem(PFileName, Length(FConfigFile^.fiName^) + 1);
  Try
    StrPCopy(PFileName, FConfigFile^.fiName^);
    { Good config file? }
    If tmValidConfigFile Then
      Begin
        { Yes. Open the config file in Exclusive mode. }
        Try
          FConfigFile^.fiHandle := FFOpenFilePrim(PFileName,
            omReadWrite,
            smShareRead,
            True,
            False);
          { Read the NextLSN from the config file. }
          FFReadFilePrim(FConfigFile, SizeOf(TffWord32), FNextLSN);
          { Read the NextLSN from the config file. }
          FFReadFilePrim(FConfigFile, SizeOf(TDateTime), FLSNRollTime);
        Except
          {if reading from the file fails, we'll get the LSN from the tables.}
          FNextLSN := tmGetLSNFromTables;
        End;
      End
    Else
      Begin
        {No. Get the LSN info from the tables.}
        FNextLSN := tmGetLSNFromTables;
        { Write the LSN to the table. }
        tmWriteConfig(False);
      End;
  Finally
    FFFreeMem(PFileName, StrLen(PFileName) + 1);
  End;
End;
{--------}

Procedure TfsSrcTransactionMgr.tmRecalcCommitLSN;
Var
  Index: Longint;
Begin

  { Assumption: Transaction list is write-locked. }

  FPortal.BeginWrite;
  Try
    FCommitLSN := high(TffWord32);
    If FTranList.Count > 0 Then
      For Index := 0 To pred(FTranList.Count) Do
        FCommitLSN := FFMinDW(FCommitLSN,
          TfsSrcTransaction(FTranList.Items[Index]).LSN);
    { Update the buffer manager's commitLSN. }
//  FBufMgr.CommitLSN := FCommitLSN;                                   {Deleted !!.10}
  Finally
    FPortal.EndWrite;
  End;
End;
{--------}

Function TfsSrcTransactionMgr.tmValidConfigFile: Boolean;
{Revised !!.13}
Var
  SearchRec: TSearchRec;
  FullFileName: TffFullFileName;
  PFullFileName: PAnsiChar;
  CfgTime: Integer;
  Continue: Boolean;
Begin
  { The config file is valid if it exists, it has a length greater than zero,
    & its file time is greater than any of the tables in the database. }
  FullFileName := FFMakeFullFileName(FPath, fsc_ConfigFile);
  Result := (FindFirst(FullFileName, faAnyFile, SearchRec) = 0);
  If Result Then
    Begin
      Result := (SearchRec.Size > 0);
      If Result Then
        Begin
          CfgTime := (SearchRec.Time + 1000);
          FindClose(SearchRec);
          If (FindFirst(FPath + '\*.' + fsc_ExtForData,
            faAnyFile,
            SearchRec) = 0) Then
            Begin
              Continue := True;
              While Continue Do
                Begin
                  If (SearchRec.Time > CfgTime) Then
                    Begin
                      Result := False;
                      Break;
                    End;
                  Continue := FindNext(SearchRec) = 0;
                End;
              FindClose(SearchRec);
            End; { if }
        End; { if }
    End; { if }

  If Not Result Then
    Begin
      { Create the config file since it doesn't exist or has zero size. }
      FFGetMem(PFullFileName, Length(FullFileName) + 1);
      Try
        FConfigFile^.fiHandle := FFOpenFilePrim(StrPCopy(PFullFileName,
          FullFileName),
          omReadWrite,
          smShareRead,
          True,
          True);
        { NOTE: FConfigFile will be closed when the transaction manager
                is destroyed. }
      Finally
        FFFreeMem(PFullFileName, StrLen(PFullFileName) + 1);
      End;
    End; { if }
End;
{--------}

Procedure TfsSrcTransactionMgr.tmWriteConfig(Const CloseFile: Boolean); {!!.13}
Var
  TempPos: TffInt64;
Begin
  {$IFDEF TranLogging}
  FEventLog.WriteStrings(['',
    format('Tran Mgr tmWriteConfig: %s',
      [FPath])]);
  {$ENDIF}
  If assigned(FConfigFile) And {Start !!.01}
  (FConfigFile^.fiHandle <> INVALID_HANDLE_VALUE) Then
    Begin
      If (Not FReadOnly) Then
        Begin
          FFInitI64(TempPos);
          FFPositionFilePrim(FConfigFile, TempPos);
          FFWriteFilePrim(FConfigFile, sizeOf(TffWord32), FNextLSN);
          FFWriteFilePrim(FConfigFile, sizeOf(TDateTime), FLSNRollTime);
        End;
      If CloseFile Then {!!.13}
        FFCloseFilePrim(FConfigFile);
    End;
  If CloseFile Then {!!.13}
    FFFreeFileInfo(FConfigFile); {End !!.01}
End;
{=====================================================================}
End.

