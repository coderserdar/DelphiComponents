{
   Firebird Library
   Open Source Library No Data Aware to direct access to Firebird
   Relational Database from Borland Delphi / Kylix and Freepascal

   File:FBLTransaction.pas
   Copyright (c) 2002-2004 Alessandro Batisti
   fblib@altervista.org
   http://fblib.altervista.org

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.
}

{$I fbl.inc}

{
@abstract(Managing Firebird Transactions)
@author(Alessandro Batisti <fblib@altervista.org>)
FbLib - Firebird Library @html(<br>)
FBLTransaction.pas unit provides manager Firebird RDBMS Transaction
}
unit FBLTransaction;

interface

uses
  SysUtils,
 {$IFDEF D6P}
  Types,
 {$ENDIF}
  Classes,
  ibase_h, FBLDatabase;

type
  {@EXCLUDE}
  IFBLTranEvent = interface
    ['{ABAEBCBF-722A-4570-A3C9-A0F66045C6BB}']
    procedure DoOnEndTransaction;
    procedure DoOnDestroy;
  end;
  {Transaction Action}
  TTRAction = (TARollback, TACommit, TACommitRetaining);
  {Transaction access mode}
  TTRAccessMode = (amWrite, amRead);
  {Transaction isolation level}
  TTRIsolationLevel = (ilConcurrency, ilConsistency, ilReadCommitted_rec_version,
    ilReadCommitted_no_rec_version);
  {Transaction Lock Resolution}
  TTRLockResolution = (lrWait, lrNoWait);
  {Transaction Reservation Mode}
  TTRTableReservationMode = (rmShared, rmProtected);
  {End transaction event handled}
  TEndTransaction = procedure(Sender: TObject; Action: TTRAction) of object;

  {@abstract(encapsulate properties and method for managing firebird transactions)}
  TFBLTransaction = class(TComponent, IFBLDbEvent)
  private
    FTRHandle: TISC_TR_HANDLE;
    FDatabase: TFBLDatabase;
    FAccessMode: TTRAccessMode;
    FIsolationLevel: TTRIsolationLevel;
    FLockResolution: TTRLockResolution;
    FTableReservationMode: TTRTableReservationMode;
    FDSqls: TList;                        //list of attach TDsqls Objects
    FReservationReadTables: TStrings;
    FReservationWriteTables: TStrings;
    FOnStartTransaction: TNotifyEvent;
    FOnEndTransaction: TEndTransaction;
    function GetTRHandle: PISC_TR_HANDLE;
    function GetInTransaction: boolean;
    function GetDbHandle: PISC_DB_HANDLE;
    function TpbBufferLen: integer;         // Return TPB Lenght or 0 if Tpb Default
    procedure CheckDbConnected;
    procedure SetReservationReadTables(Value: TStrings);
    procedure SetReservationWriteTables(Value: TStrings);
    procedure SetDatabase(const Value: TFBLDatabase);
    procedure EndTransaction(Action: TTRAction);
    {$IFDEF FBL_THREADSAFE}
    procedure Lock;
    procedure Unlock;
    {$ENDIF}
  public
    {Create an instace of TFBLTransaction}
    constructor Create(AOwner: TComponent); override;
    {Free up  all resources associated with this instance}
    destructor Destroy; override;
    {$IFDEF FPC_INFD}
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: integer; stdcall;
    function _Release: integer; stdcall;
    {$ENDIF}
    {@Exclude}
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    {Start transaction, when transaction is active property @link(InTransaction) := True}
    procedure StartTransaction;
    {Commit an active transaction ,
     saves changes permanent in the database,
     close active transaction (@Link(InTransaction):=@False),
     and unprepare any @LINK(TFBLDsql) objects attacched}
    procedure Commit;
    {CommitRetaining an active transaction,
     saves changes permanent in the database,
     doesn't close active transaction (@Link(InTransaction):=@True),
     doens't not close any cursor open of @LINK(TFBLDsql) objects attacched}
    procedure CommitRetaining;
    {RollBack an active transaction ,
     undoes a transaction's changes,returning the database to its previus state,before transaction started
     close active transaction (@Link(InTransaction):=@False),
     and unprepare any @LINK(TFBLDsql) objects attacched }
    procedure RollBack;
    {@exclude}
    procedure DoOnDatabaseDisconnect;
    {@exclude}
    procedure DoOnDestroy;
    {@exclude}
    procedure AddQuery(AQuery: IFBLTranEvent);
    {@exclude}
    procedure RemQuery(AQuery: IFBLTranEvent);
    {the firebird transaction handle , this is used in all calls to firebird api}
    property TRHandle: PISC_TR_HANDLE read GetTRHandle;
    {result @true if Transaction is Active,
    @html(<br>) see also @link(StartTransaction)}
    property InTransaction: boolean read GetInTransaction;
    {the firebird database handle , this is used in all calls to firebird api}
    property DBHandle: PISC_DB_HANDLE read GetDbHandle;
  published
    {TFBLDatabase object where is attached current TFBLTransaction instance }
    property Database: TFBLDatabase read FDatabase write SetDatabase;
    {Occurs after Active transaction is started}
    property OnStartTransaction: TNotifyEvent
      read FOnStartTransaction write FOnStartTransaction;
    {Occurs after Active transaction is ended}
    property OnEndTransaction: TEndTransaction
      read FOnEndTransaction write FOnEndTransaction;
    {The AccessMode Property describes the actions a transaction can perform against a table, default amWrite}
    property AccessMode: TTRAccessMode read FAccessMode write FAccessMode default amWrite;
    {The IsolationLevel Property specifies the view of the database permitted
    a transaction as it relates to actions performed by other
    simultaneously occurring transactions, default ilConcurrency}
    property IsolationLevel: TTRIsolationLevel
      read FIsolationLevel write FIsolationLevel default ilConcurrency;
    {The LockResolution Property describes whats happens if a transaction encouters an access
     conflict during a write operation (update and delete operations on existing rows),default lrWait}
    property LockResolution: TTRLockResolution
      read FLockResolution write FLockResolution default lrWait;
    {The TableReservationMode Property optionally describes an access method and lock resolution for a specified
    table that the transaction access}
    property TableReservationMode: TTRTableReservationMode
      read FTableReservationMode write FTableReservationMode;
    {List of reservation read tables}
    property ReservationReadTables: TStrings
      read FReservationReadTables write SetReservationReadTables;
    {List of reservation write tables}
    property ReservationWriteTables: TStrings
      read FReservationWriteTables write SetReservationWriteTables;
  end;

implementation

uses FBLExcept, FBLmixf, FBLConst;

constructor TFBLTRansaction.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTRHandle := nil;
  FDSqls := TList.Create;
  FReservationReadTables := TStringList.Create;
  FReservationWriteTables := TStringList.Create;
end;

//------------------------------------------------------------------------------

destructor TFBLTransaction.Destroy;
var
  i: integer;
begin
  if inTransaction then EndTransaction(TARollback);
  if Assigned(FDatabase) then FDatabase.RemoveAttachObj(self);
  for i := 0 to FDSqls.Count - 1 do
    IFBLTranEvent(FDSqls[i]).DoOnDestroy;
  FDSqls.Free;
  FReservationReadTables.Free;
  FReservationWriteTables.Free;
  inherited Destroy;
end;

//------------------------------------------------------------------------------

{$IFDEF FBL_THREADSAFE}
procedure TFBLTransaction.Lock;
begin
  if Assigned(FDatabase) then FDatabase.Lock;
end;

procedure TFBLTRansaction.Unlock;
begin
  if Assigned(FDatabase) then FDatabase.UnLock;
end;
{$ENDIF}

//------------------------------------------------------------------------------
{$IFDEF FPC_INFD}
function TFBLTransaction.QueryInterface(const IID: TGUID; out Obj): HResult;
const
  E_NOINTERFACE = HResult($80004002);
begin
  if GetInterface(IID, Obj) then Result := 0 
  else 
    Result := E_NOINTERFACE;
end;

function TFBLTransaction._AddRef: integer;
begin
  Result := -1;
end;

function TFBLTransaction._Release: integer;
begin
  Result := -1;
end;
{$ENDIF}
//------------------------------------------------------------------------------


procedure TFBLTransaction.Notification(AComponent: TComponent;
  Operation: TOperation);//override;
begin
  if Operation = opRemove then
  begin
    if AComponent = FDatabase then FDatabase := nil;
  end;
  inherited Notification(AComponent, Operation);
end;

//------------------------------------------------------------------------------

function TFBLTransaction.GetTRHandle: PISC_TR_HANDLE;
begin
  Result := @FTRhandle;
end;

//------------------------------------------------------------------------------

function TFBLTransaction.GetInTransaction: boolean;
begin
  {$IFDEF FBL_THREADSAFE}
  Lock;
  try
  {$ENDIF}
    Result := (FTRHandle <> nil);
  {$IFDEF FBL_THREADSAFE}
  finally
    UnLock;
  end;
  {$ENDIF}
end;

//------------------------------------------------------------------------------

function TFBLTransaction.GetDbHandle: PISC_DB_HANDLE;
begin
  if not Assigned(FDatabase) then
    FBLError(E_TR_DB_NOT_ASSIGNED);
  Result := FDatabase.DBHandle;
end;

//------------------------------------------------------------------------------

procedure TFBLTransaction.SetReservationReadTables(Value: TStrings);
begin
  FReservationReadTables.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TFBLTransaction.SetReservationWriteTables(Value: TStrings);
begin
  FReservationWriteTables.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TFBLTransaction.CheckDbConnected;
begin
  if not Assigned(FDatabase) then
    FBLError(E_TR_DB_NOT_ASSIGNED);
  if not FDatabase.Connected then
    FBLError(E_DB_NOACTIVE_CON);
end;

//------------------------------------------------------------------------------

procedure TFBLTransaction.SetDatabase(const Value: TFBLDatabase);
begin
  if Assigned(Value) and (Value <> FDatabase) then
    Value.AddAttachObj(self);
  FDatabase := Value;
end;

//------------------------------------------------------------------------------

procedure TFBLTRansaction.AddQuery(AQuery: IFBLTranEvent);
begin
  FDSqls.Add(Pointer(AQuery));
end;

//------------------------------------------------------------------------------

procedure TFBLTransaction.RemQuery(AQuery: IFBLTranEvent);
var
  i: integer;
begin
  i := FDSqls.IndexOf(Pointer(AQuery));
  if (i > -1) then
    FDSqls.Delete(i);
end;

//------------------------------------------------------------------------------

function TFBLTransaction.TpbBufferLen: Integer;
var
  i: integer;
begin
  Result := 0;

  if (FAccessMode = amWrite) and (FIsolationLevel = ilConcurrency) and
    (FLockResolution = lrWait) and (FReservationReadTables.Count = 0) and
    (FReservationWriteTables.Count = 0) then
    Exit; // Transaction Param buffer default

  Result := 4;

  if FReservationReadTables.Count > 0 then
  begin
    for i := 0 to FReservationReadTables.Count - 1 do
    begin
      if Length(FReservationReadTables.Strings[i]) > 31 then
        Inc(Result, 31 + 3)
      else
        Inc(Result, Length(FReservationReadTables.Strings[i]) + 3);
    end;
  end;

  if FReservationWriteTables.Count > 0 then
  begin
    for i := 0 to FReservationWriteTables.Count - 1 do
    begin
      if Length(FReservationWriteTables.Strings[i]) > 31 then
        Inc(Result, 31 + 3)
      else
        Inc(Result, Length(FReservationWriteTables.Strings[i]) + 3);
    end;
  end;


  if (FIsolationLevel = ilReadCommitted_rec_version) or
    (IsolationLevel = ilReadCommitted_no_rec_version) then
    Inc(Result);
end;

//------------------------------------------------------------------------------

procedure TFBLTransaction.StartTransaction;
var
  Status_vector: ISC_STATUS_VECTOR;
  Teb: TISC_TEB;      // Transaction Existence Buffer for 'isc_start_multiple'
  i: integer;
  TpbLen: integer;    // Transaction Paramerer buffer length
  TpbIdx: integer;    // Transaction Paramerer buffer Index
  TabLen: integer;    // Table reservation Name length
  TabName: string;    // Table reservation Name length
  TPB: PChar;         // Transaction Paramerer buffer
begin
  CheckDbConnected;
  {$IFDEF  FBL_THREADSAFE}
  Lock;
  try
  {$ENDIF}
    TpbLen := TpbBufferLen; //  tpb default if TpbLen = 0;
    TPB := nil;
    TpbIdx := 0;
    TabName := '';
    //if inTransaction then FBLError(E_TR_ACTIVE);
    if FTRHandle <> nil then FBLError(E_TR_ACTIVE);
    try
      if TpbLen > 0 then
      begin
        FBLMalloc(TPB, TpbLen);
        TPB[TpbIdx] := char(isc_tpb_version3);
        Inc(TpbIdx);

        if FAccessMode = amWrite then
          TPB[TpbIdx] := char(isc_tpb_write)
        else
          TPB[TpbIdx] := char(isc_tpb_read);
        Inc(TpbIdx);

        if FLockResolution = lrWait then
          TPB[TpbIdx] := char(isc_tpb_wait)
        else if FLockResolution = lrNoWait then
          TPB[TpbIdx] := char(isc_tpb_nowait);
        Inc(TpbIdx);

        if (FIsolationLevel = ilConcurrency) then
          TPB[TpbIdx] := char(isc_tpb_concurrency)
        else if (FIsolationLevel = ilConsistency) then
          TPB[TpbIdx] := char(isc_tpb_consistency)
        else if (FIsolationLevel = ilReadCommitted_rec_version) then
        begin
          TPB[TpbIdx] := char(isc_tpb_read_committed);
          Inc(TpbIdx);
          TPB[TpbIdx] := char(isc_tpb_rec_version);
        end
        else if (FIsolationLevel = ilReadCommitted_no_rec_version) then
        begin
          TPB[TpbIdx] := char(isc_tpb_read_committed);
          Inc(TpbIdx);
          TPB[TpbIdx] := char(isc_tpb_no_rec_version);
        end;
        Inc(TpbIdx);

        if FReservationReadTables.Count > 0 then
        begin
          for i := 0 to FReservationReadTables.Count - 1 do
          begin
            TabLen := Length(FReservationReadTables.Strings[i]);
            TabName := FReservationReadTables.Strings[i];
            if TabLen > 31 then
            begin
              TabLen := 31;
              TabName := Copy(FReservationReadTables.Strings[i], 0,31);
            end;
            if FTableReservationMode = rmShared then
              TPB[Tpbidx] := char(isc_tpb_shared)
            else
              TPB[TpbIdx] := char(isc_tpb_protected);
            Inc(TpbIdx);
            TPB[TpbIdx] := char(isc_tpb_lock_read);
            Inc(TpbIdx);
            TPB[TpbIdx] := char(TabLen);
            Inc(TpbIdx);
            Move(TabName[1], TPB[TpbIdx], TabLen);
            Inc(TpbIdx, TabLen);
          end;
        end;

        if FReservationWriteTables.Count > 0 then
        begin
          for i := 0 to FReservationWriteTables.Count - 1 do
          begin
            TabLen := Length(FReservationWriteTables.Strings[i]);
            TabName := FReservationWriteTables.Strings[i];
            if TabLen > 31 then
            begin
              TabLen := 31;
              TabName := Copy(FReservationWriteTables.Strings[i], 0,31);
            end;
            if FTableReservationMode = rmShared then
              TPB[Tpbidx] := char(isc_tpb_shared)
            else
              TPB[TpbIdx] := char(isc_tpb_protected);
            Inc(TpbIdx);
            TPB[TpbIdx] := char(isc_tpb_lock_write);
            Inc(TpbIdx);
            TPB[TpbIdx] := char(TabLen);
            Inc(TpbIdx);
            Move(TabName[1], TPB[TpbIdx], TabLen);
            Inc(TpbIdx, TabLen);
          end;
        end;
      end;
   {if TpbLen <>  TpbIdx then FBLError('TPB error');}
      Teb.db_handle := FDataBase.DBHandle;
      Teb.tpb_length := TpbLen;
      Teb.tpb_address := TPB;
      if isc_start_multiple(@Status_vector, @FTRHandle, 1, @Teb) <> 0 then
        FBLShowError(@Status_vector);
      if Assigned(FOnStartTransaction) then
        FOnStartTransaction(self);
    finally
      FBLFree(TPB);
    end;
  {$IFDEF FBL_THREADSAFE}
  finally
    UnLock;
  end;
  {$ENDIF}
end;


//-------------------------------------------------------------------------------

procedure TFBLTransaction.EndTransaction(Action: TTRAction);
var
  Status_vector: ISC_STATUS_VECTOR;
  i: integer;
begin
  {$IFDEF FBL_THREADSAFE}
  Lock;
  try
    {$ENDIF}
    if FTRHandle = nil  then FBLError(E_TR_NOACTIVE);
    case Action of
      //TARollback, TACommit, TACommitRetaining
      TARollback:
        begin
          if isc_rollback_transaction(@Status_Vector, @FTRHandle) <> 0 then
            FBLShowError(@Status_Vector);
        end;
      TACommit:
        begin
          if isc_commit_transaction(@Status_vector, @FTRHandle) <> 0 then
            FBLShowError(@Status_Vector);
        end;
      TACommitRetaining:
        begin
          if isc_commit_retaining(@Status_vector, @FTRHandle) <> 0 then
            FBLShowError(@Status_Vector);
        end;
    end;
    for i := 0 to FDSqls.Count - 1 do
      IFBLTranEvent(FDSqls[i]).DoOnEndTransaction;
    if Assigned(FOnEndTransaction) then
      FOnEndTransaction(self, Action);
    {$IFDEF FBL_THREADSAFE}
  finally
    UnLock;
  end;
  {$ENDIF}

end;

//------------------------------------------------------------------------------

procedure TFBLTransaction.Commit;
begin
  EndTransaction(TACommit);
end;

//------------------------------------------------------------------------------

procedure TFBLTransaction.CommitRetaining;
begin
  EndTransaction(TACommitRetaining);
end;

//------------------------------------------------------------------------------

procedure TFBLTransaction.RollBack;
begin
  EndTransaction(TARollback);
end;

//------------------------------------------------------------------------------

procedure TFBLTransaction.DoOnDatabaseDisconnect;
begin
  if InTransaction then
    EndTransaction(TARollback);
end;

//------------------------------------------------------------------------------

procedure TFBLTransaction.DoOnDestroy;
begin
  if FDatabase <> nil then FDatabase := nil;
end;


end.
