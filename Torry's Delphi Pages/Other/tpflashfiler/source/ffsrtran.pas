{*********************************************************}
{* FlashFiler: Transaction manager for Server            *}
{*********************************************************}

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
 * The Original Code is TurboPower FlashFiler
 *
 * The Initial Developer of the Original Code is
 * TurboPower Software
 *
 * Portions created by the Initial Developer are Copyright (C) 1996-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

{$I ffdefine.inc}

{.$DEFINE TranLogging}

unit FFSRTran;

interface

uses
  {$IFDEF TranLogging}
  fflllog,
  {$ENDIF}
  Windows,
  Messages,
  SysUtils,
  ffllbase,
  fflleng,
  ffsrlock,
  ffsrbase;

type

  { TffSrTransactionMgr tracks active transactions.  Each instance of
    TffSrFolder has its own instance of TffSrTransactionMgr as the scope
    of a transaction is limited to the tables within one database.

    Any number of transactions may be active per TffSrFolder.  However, only
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
  TffSrTransactionMgr = class(TffObject)
    protected {private}
      {$IFDEF TranLogging}
      FEventLog : TffEventLog;
      {$ENDIF}

      FBufMgr      : TffBufferManager;
        {-The buffer manager handling file access. }
      FByDatabase  : TffThreadList;
        {-Transactions indexed by DatabaseID.  Contains instances of
          TffIntListItem.  The ExtraData property of each TffIntListItem
          contains a reference to the transaction. }
      FCommitLSN   : TffWord32;
        {-The starting LSN of the oldest uncommitted transaction. }
      FConfigFile : PffFileInfo;
        {-The binary file containing the transaction manager's persistent
          information. }
      FLockMgr     : TffLockManager;
        {-The lock manager coordinating locks for this database. }
      FLSNRolltime : TDateTime;
        {Time of last LSN rollover.}
      FNextLSN     : TffWord32;
        {-The next LSN to be assigned to a transaction. }
      FPath        : string;
        {-The directory for which this object manages transactions. }
      FPortal      : TffReadWritePortal;
        {-Used to control access to CommitLSN. }
      FReadOnly    : boolean;
        {-Used to control whether or not the last LSN is to be preserved
          on disk. }
      FTranList    : TffThreadList;
        {-Holds the list of active transactions sorted by
          transaction ID. }
    protected

      function tmGetCommitLSN : TffWord32;
        {-Used to retrieve the CommitLSN. }

      function tmGetCount : Longint;
        {-Returns the number of active transactions. }

      function tmGetLSNForTable(const FullTableName : TffFullFileName) {!!.06}
                                                    : TffWord32;       {!!.06}

      function tmGetLSNFromTables : TffWord32;
        {-Retrieves the NextLSN based on the number stored in the
          header of the tables in the database.}

      function tmGetTransItem(Find : TffListFindType; Value : Longint) : TffSrTransaction;
        {-Find an active transaction by ID or index. }

      procedure tmHandleLSNRollover;
        {-Used to handle a NextLSN rollover. }

      procedure tmReadConfig;
        {-Used to retrieve the transaction manager's last LSN from the config
          file. }

      procedure tmRecalcCommitLSN;
        {-Used to recalculate the CommitLSN after a commit or rollback. }

      function  tmValidConfigFile : Boolean;
        {-Returns True if config file exists and its file time is
          greater than all the tables in the database.}

      procedure tmWriteConfig(const CloseFile : Boolean);              {!!.13}
        {-Used to store the transaction manager's last LSN in a config file. }

    public
      constructor Create(aBufferMgr : TffBufferManager;
                         aLockMgr   : TffLockManager;
                   const aPath      : string;
                   const aReadOnly : boolean);
      destructor Destroy; override;

      function Commit(const aTransID : TffTransID;
                        var wasNested : boolean) : TffResult;
        { Commit a transaction. Returns DBIERR_NONE if the commit was
          successful. Output parameter wasNested is set to True if a nested
          transaction was committed. Otherwise it is set to False indicating
          a transaction was fully committed. }

      procedure Rollback(const aTransID : TffTransID; var wasNested : boolean);
        { Rollback a transaction. }

      function StartTransaction(const aDatabaseID : TffDatabaseID;
                                const aFailSafe, aImplicit,
                                      readOnly   : boolean;
                                const path : TffPath;
                                  var aTran : TffSrTransaction) : TffResult;
        { Starts a new transaction. }

      property CommitLSN : TffWord32 read tmGetCommitLSN;
        { Returns the starting LSN of the oldest uncommitted transaction.
          For now, this is really longInt(Self) of the oldest uncommitted
          transaction. }

      property Count : longInt read tmGetCount;
        { Returns the number of active transactions. }

      property IsReadOnly : boolean read FReadOnly;
        { If False then the transaction manager stores its last LSN in a
          config file when the transaction manager is freed (i.e., when the
          FF server is shutdown). }

      property NextLSN : TffWord32 read FNextLSN;
        { The next LSN to be assigned to a transaction. }

      property Path : string read FPath;
        { The directory for which this object manages transactions. }

  end;

implementation

uses
  Classes,
  ffsrbde,
  ffllexcp;

const
  { Config file }
  ffc_ConfigFile = 'FFSTRAN.CFG';
  ffc_ConfigExt : string[ffcl_Extension] = 'CFG';

{$I FFCONST.INC}


{===TffSrTransactionMgr===============================================}
constructor TffSrTransactionMgr.Create(aBufferMgr : TffBufferManager;
                                       aLockMgr   : TffLockManager;
                                 const aPath      : string;
                                 const aReadOnly  : boolean);
begin
  inherited Create;
  FBufMgr := aBufferMgr;
  FByDatabase := TffThreadList.Create;
  FCommitLSN := High(TffWord32);
  FLockMgr := aLockMgr;
  FLSNRollTime := 0;
  FNextLSN := 1;
  FPath := aPath;
  FPortal := TffReadWritePortal.Create;
  FReadOnly := aReadOnly;
  FTranList := TffThreadList.Create;

  {$IFDEF TranLogging}
  FEventLog := TffEventLog.Create(nil);
  FEventLog.FileName := '.\FFTran.LOG';
  FEventLog.Enabled := True;
  FEventLog.WriteString(format('Transaction Mgr started for %s',
                               [aPath]));
  {$ENDIF}

  if not FReadOnly then
    tmReadConfig;
end;
{--------}
destructor TffSrTransactionMgr.Destroy;
begin
{$IFDEF TranLogging}
  FEventLog.WriteString('Destroying transaction mgr');
{$ENDIF}
  if not FReadOnly then
    tmWriteConfig(true);                                               {!!.13}
  FByDatabase.Free;
  FPortal.Free;
  FTranList.Free;
  {$IFDEF TranLogging}
  FEventLog.WriteStrings(['',
                          format('Transaction Mgr stopped for %s',
                                 [FPath])]);
  FEventLog.Free;
  {$ENDIF}

  inherited Destroy;
end;
{--------}
function TffSrTransactionMgr.Commit(const aTransID : TffTransID;
                                      var wasNested : boolean) : TffResult;
var
  aTran : TffSrTransaction;
begin
  Result := DBIERR_NONE;
  aTran := tmGetTransItem(ftFromID, aTransID);
  if assigned(aTran) then begin

    { Tell the buffer manager to commit the changes. }
    FBufMgr.CommitTransaction(aTran);

    { Have all changes been committed to disk? }
    if aTran.TransLevel.Level = 0 then begin                           {!!.10}

      FLockMgr.ReleaseTransactionLocks(aTran, False);

      { Yes. Remove the index entry for the transaction. }
{$IFDEF TranLogging}
      FEventLog.WriteString('Commit: Delete transaction from FByDatabase.');
{$ENDIF}
      with FByDatabase.BeginWrite do
        try
          Delete(aTran.DatabaseID);
        finally
          EndWrite;
        end;

      { Remove the transaction from the list. }
      with FTranList.BeginWrite do
        try
          Delete(aTran);
          tmRecalcCommitLSN;
        finally
          EndWrite;
        end;

      wasNested := False;
    end
    else begin
      { No. We just committed a nested transaction.  Decrement the nesting
        level. }
      aTran.EndNested;                                                 {!!.10}
      wasNested := True;
    end;
  end else begin
    Result := DBIERR_INVALIDHNDL;
  end;
end;
{--------}
procedure TffSrTransactionMgr.Rollback(const aTransID : TffTransID;
                                         var wasNested : boolean);
var
  aTran : TffSrTransaction;
begin
  aTran := tmGetTransItem(ftFromID, aTransID);
  if assigned(aTran) then begin

    { Tell the buffer manager to rollback the changes. }
    FBufMgr.RollbackTransaction(aTran);

    { Did we rollback a nested transaction? }
    if aTran.TransLevel.Level = 0 then begin                           {!!.10}
      { No. Release the locks held by the transaction. }
      FLockMgr.ReleaseTransactionLocks(aTran, False);

      { Remove the index entry for the transaction. }
{$IFDEF TranLogging}
      FEventLog.WriteString('Rollback: Delete transaction from FByDatabase.');
{$ENDIF}
      with FByDatabase.BeginWrite do
        try
          Delete(aTran.DatabaseID);
        finally
          EndWrite;
        end;

      { Remove the transaction from the list. }
      with FTranList.BeginWrite do
        try
          Delete(aTran);
          tmRecalcCommitLSN;
        finally
          EndWrite;
        end;

      wasNested := false;

    end
    else begin
      { Yes.  Decrement the nesting level. }
      aTran.EndNested;                                                 {!!.10}
      wasNested := true;
    end;
  end;
end;
{--------}
function TffSrTransactionMgr.StartTransaction(const aDatabaseID : TffDatabaseID;
                                              const aFailSafe, aImplicit,
                                                    readOnly   : boolean;
                                              const path : TffPath;
                                                var aTran : TffSrTransaction) : TffResult;
var
  anIndex     : Longint;
  anItem      : TffIntListItem;
  JnlFileName : TffFullFileName;
  TranName    : TffShStr;
begin
  { Assumption: A client may have multiple databases but one instance of a
    TffSrDatabase is unique to a client. }

  Result := DBIERR_NONE;

  { Has a transaction already been started by this particular database? }
{$IFDEF TranLogging}
  FEventLog.WriteString('StartTran: Obtain read access to FByDatabase.');
{$ENDIF}
  with FByDatabase.BeginRead do
    try
      anIndex := Index(aDatabaseID);
{Begin move !!.06}
      { Does a transaction already exist on the database? }
      if anIndex > -1 then begin
        { Yes. Get the transaction. }
        aTran := TffSrTransaction(TffIntListItem(FByDatabase.Items[anIndex]).ExtraData);

        { Increase its nesting level. }
         aTran.StartNested;                                            {!!.10}
      end;
{End move !!.06}
    finally
      EndRead;
{$IFDEF TranLogging}
      FEventLog.WriteString('StartTran: End read access to FByDatabase.');
{$ENDIF}
    end;

{!!.06 - Code moved to previous try..finally block. }
  if anIndex = -1 then begin
    { No. Create a new transaction. }
    aTran := TffSrTransaction.Create(aDatabaseID, aImplicit, readOnly);

    try
      { Add the transaction to the active transaction list. }
      with FTranList.BeginWrite do
        try
          Insert(aTran);
          aTran.LSN := FNextLSN;
          if FNextLSN = high(TffWord32) then
            tmHandleLSNRollover
          else
            inc(FNextLSN);
        finally
          EndWrite;
        end;

      { Add an index entry on the transaction's cursorID. }
      anItem := TffIntListItem.Create(aDatabaseID);
      anItem.ExtraData := pointer(aTran);
{$IFDEF TranLogging}
      FEventLog.WriteString('StartTran: Insert transaction into FByDatabase.');
{$ENDIF}
      with FByDatabase.BeginWrite do
        try
          Insert(anItem);
        finally
          EndWrite;
{$IFDEF TranLogging}
          FEventLog.WriteString('StartTran: Finished insert transaction ' +
                                'into FByDatabase.');
{$ENDIF}
        end;

      { Determine the name of the journal file. }
      if aFailSafe then begin
        JnlFileName := path;
        FFShStrAddChar( JnlFileName, '\' );
        Str(aTran.TransactionID, TranName);
        FFShStrConcat(JnlFileName, TranName );
        FFShStrAddChar(JnlFileName, '.');
        FFShStrConcat(JnlFileName, ffc_ExtForTrans);
      end else
        JnlFileName := '';

      { Recalculate the CommitLSN. }
      if not readOnly then begin
        FPortal.BeginWrite;
        try
          { Update the commitLSN. }
          FCommitLSN := FFMinDW(FCommitLSN, aTran.LSN);
          { Update the buffer manager's commitLSN. }
//        FBufMgr.CommitLSN := FCommitLSN;                             {Deleted !!.10}
        finally
          FPortal.EndWrite;
        end;
      end;

      { Tell the buffer manager to start tracking changes for this transaction. }
      FBufMgr.StartTransaction(aTran, aFailSafe, JnlFileName);

    except
      if assigned(aTran) then
        aTran.Free;
      raise;
    end;
  end;
end;
{--------}
function TffSrTransactionMgr.tmGetCommitLSN : TffWord32;
begin
  FPortal.BeginRead;
  try
    Result := FCommitLSN;
  finally
    FPortal.EndRead;
  end;
end;
{--------}
function TffSrTransactionMgr.tmGetCount : longInt;
begin
  with FTranList.BeginRead do
    try
      Result := FTranList.Count;
    finally
      EndRead;
    end;
end;
{--------}
function TffSrTransactionMgr.tmGetLSNForTable(const FullTableName : TffFullFileName) {!!.06 - Added}
                                                                  : TffWord32;
{!!.07 - Rewritten}
var
  FileHandle : Integer;
begin
  FileHandle := FileOpen(FullTableName, fmOpenRead);
  try
    { The LSN is stored in position 12 of block 0. }
    if ((FileSeek(FileHandle, 12, 0) <> 12) or
        (FileRead(FileHandle, Result, SizeOf(TffWord32)) <> SizeOf(TffWord32))) then
      Result := 0;
  finally
    FileClose(FileHandle);
  end;
end;                                                                   {!!.06 - End added}
{--------}
function TffSrTransactionMgr.tmGetLSNFromTables : TffWord32;
var
  SearchRec : TSearchRec;
  {CurrFile  : TFileStream;}                                           {!!.06 - Deleted}
  TempLSN   : TffWord32;
  Continue  : Boolean;
begin
  Result := 0;
  if FindFirst(FPath + '\*.' + ffc_ExtForData, faAnyFile, SearchRec) = 0 then begin
    Continue := True;
    while Continue do begin
      try
        TempLSN := tmGetLSNForTable(FFMakeFullFileName(FPath, SearchRec.Name)); {!!.06 - Moved functionality this method}
        if (TempLSN > Result) then
          Result := TempLSN;
        Continue := FindNext(SearchRec) = 0;
      except
        Continue := FindNext(SearchRec) = 0;
      end;
    end;
    FindClose(SearchRec);
  end;
  {We have no idea when the last LSN rollover was so we just set it
   to 0.}
  FLSNRollTime := 0;
  {Since the tables store the last used LSN we need to increment our
   result to get the NextLSN.}
  Inc(Result);
end;
{--------}
function TffSrTransactionMgr.tmGetTransItem(Find  : TffListFindType;
                                            Value : Longint) : TffSrTransaction;
var
  Inx : Integer;
begin
  { Assumption: Caller has not read- or write-locked the transaction list. }
  Result := nil;
  with FTranList.BeginRead do
    try
      if (Find = ftFromID) then begin
        Inx := FTranList.Index(Value);
        if (Inx <> -1) then
          Result := TffSrTransaction(FTranList[Inx]);
      end
      else {Find = ftFromIndex}
        if (0 <= Value) and (Value < FTranList.Count) then
          Result := TffSrTransaction(FTranList[Value]);
    finally
      EndRead;
    end;
end;
{--------}
procedure TffSrTransactionMgr.tmHandleLSNRollover;
var
  anIndex       : Longint;
  LSNAdjustment : TffWord32;
  NewLSN        : TffWord32;
begin

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
  try
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
    try
      { Adjust the LSN of each transaction. }
      for anIndex := 0 to pred(FTranList.Count) do begin
        NewLSN := TffSrTransaction(FTranList.Items[anIndex]).AdjustLSN(LSNAdjustment);
        FNextLSN := FFMaxDW(FNextLSN, NewLSN);
      end;
      inc(FNextLSN);

      { Set the LSN of the other RAM pages. }
      FBufMgr.HandleLSNrollover;
    finally
      FBufMgr.EndWrite;
    end;

  finally
    FPortal.EndWrite;
  end;
end;
{--------}
procedure TffSrTransactionMgr.tmReadConfig;
{ Revised !!.13}
var
  PFileName : PAnsiChar;                                            
begin
  {$IFDEF TranLogging}
  FEventLog.WriteStrings(['',
                         format('Tran Mgr tmReadConfig: %s',
                                [FPath])]);
  {$ENDIF}

  { Allocate an in-memory structure for the config file & see if the config
    file exists. }
  FConfigFile := FFAllocFileInfo(FFMakeFullFileName(FPath, ffc_ConfigFile),
                                 ffc_ConfigExt, nil);
  FFGetMem(PFileName, Length(FConfigFile^.fiName^) + 1);
  try
    StrPCopy(PFileName, FConfigFile^.fiName^);
    { Good config file? }
    if tmValidConfigFile then begin
      { Yes. Open the config file in Exclusive mode. }
      try
        FConfigFile^.fiHandle := FFOpenFilePrim(PFileName,
                                                omReadWrite,
                                                smShareRead,
                                                True,
                                                False);
        { Read the NextLSN from the config file. }
        FFReadFilePrim(FConfigFile, SizeOf(TffWord32), FNextLSN);
        { Read the NextLSN from the config file. }
        FFReadFilePrim(FConfigFile, SizeOf(TDateTime), FLSNRollTime);
      except
        {if reading from the file fails, we'll get the LSN from the tables.}
        FNextLSN := tmGetLSNFromTables;
      end;
    end else begin
      {No. Get the LSN info from the tables.}
      FNextLSN := tmGetLSNFromTables;
      { Write the LSN to the table. }
      tmWriteConfig(false);
    end;
  finally
    FFFreeMem(PFileName, StrLen(PFileName) + 1);
  end;
end;
{--------}
procedure TffSrTransactionMgr.tmRecalcCommitLSN;
var
  Index : Longint;
begin

  { Assumption: Transaction list is write-locked. }

  FPortal.BeginWrite;
  try
    FCommitLSN := high(TffWord32);
    if FTranList.Count > 0 then
      for index := 0 to pred(FTranList.Count) do
        FCommitLSN := FFMinDW(FCommitLSN,
                              TffSrTransaction(FTranList.Items[index]).LSN);
    { Update the buffer manager's commitLSN. }
//  FBufMgr.CommitLSN := FCommitLSN;                                   {Deleted !!.10}
  finally
    FPortal.EndWrite;
  end;
end;
{--------}
function  TffSrTransactionMgr.tmValidConfigFile : Boolean;
{Revised !!.13}
var
  SearchRec     : TSearchRec;
  FullFileName  : TffFullFileName;
  PFullFileName : PAnsiChar;
  CfgTime       : Integer;
  Continue      : Boolean;
begin
  { The config file is valid if it exists, it has a length greater than zero,
    & its file time is greater than any of the tables in the database. }
  FullFileName := FFMakeFullFileName(FPath, ffc_ConfigFile);
  Result := (FindFirst(FullFileName, faAnyFile, SearchRec) = 0);
  if Result then begin
    Result := (SearchRec.Size > 0);
    if Result then begin
      CfgTime := (SearchRec.Time + 1000);
      FindClose(SearchRec);
      if (FindFirst(FPath + '\*.' + ffc_ExtForData,
                    faAnyFile,
                    SearchRec) = 0) then begin
        Continue := True;
        while Continue do begin
          if (SearchRec.Time > CfgTime) then begin
            Result := False;
            Break;
          end;
          Continue := FindNext(SearchRec) = 0;
        end;
        FindClose(SearchRec);
      end;  { if }
    end;  { if }
  end;  { if }

  if not Result then begin
    { Create the config file since it doesn't exist or has zero size. }
    FFGetMem(PFullFileName, Length(FullFileName) + 1);
    try
      FConfigFile^.fiHandle := FFOpenFilePrim(StrPCopy(PFullFileName,
                                                       FullFileName),
                                              omReadWrite,
                                              smShareRead,
                                              True,
                                              True);
      { NOTE: FConfigFile will be closed when the transaction manager
              is destroyed. }
    finally
      FFFreeMem(PFullFileName, StrLen(PFullFileName) + 1);
    end;
  end;  { if }
end;
{--------}
procedure TffSrTransactionMgr.tmWriteConfig(const CloseFile : Boolean); {!!.13}
var
  TempPos : TffInt64;
begin
  {$IFDEF TranLogging}
  FEventLog.WriteStrings(['',
                         format('Tran Mgr tmWriteConfig: %s',
                                [FPath])]);
  {$ENDIF}
  if assigned(FConfigFile) and                                         {Start !!.01}
     (FConfigFile^.fiHandle <> INVALID_HANDLE_VALUE) then begin
    if (not FReadOnly) then begin
      FFInitI64(TempPos);
      FFPositionFilePrim(FConfigFile, TempPos);
      FFWriteFilePrim(FConfigFile, sizeOf(TffWord32), FNextLSN);
      FFWriteFilePrim(FConfigFile, sizeOf(TDateTime), FLSNRollTime);
    end;
    if CloseFile then                                                  {!!.13}
      FFCloseFilePrim(FConfigFile);
  end;
  if CloseFile then                                                    {!!.13}
    FFFreeFileInfo(FConfigFile);                                       {End !!.01}
end;
{=====================================================================}
end.
