unit Main;
{*********************************************************}
{*            NCOCI8 Asta server v 0.3.0                 *}
{*********************************************************}
{*     Copyright (c) 1997-99 Asta Technology Group LLC   *}
{*              Partional rights reserved.               *}
{*                 www.astatech.com                      *}
{*********************************************************}
{*         Copyright (c) 2000-2001 Dmitry Arefiev        *}
{*             Partional rights reserved.                *}
{*                 www.da-soft.com                       *}
{*********************************************************}
{$D+,L+}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ScktComp, AstaServer, StdCtrls, Db, Grids, DBGrids, ComCtrls, AstaDrv2,
  Buttons, ExtCtrls, DBCtrls, AstaDBTools, AstaDBTypes, checklst, astaparamlist,
  AstaCustomSocket, AstaClientSocket, astastringline, AstaSQLGenerate,
  Asta2MetaData, astadatamodule;

type
  TfrmMain = class(TForm)
    AstaServerSocket1: TAstaServerSocket;
    PageControl: TPageControl;
    MemoTabsheet: TTabSheet;
    mRequests: TMemo;
    ConnectedTabSheet: TTabSheet;
    UserDataSet: TAstaDataset;
    DataSource1: TDataSource;
    UserGrid: TDBGrid;
    TabSheet1: TTabSheet;
		GroupBox3: TGroupBox;
    ReceiveMemo: TMemo;
		BitBtn1: TBitBtn;
    TabSheet3: TTabSheet;
    ErrorMemo: TMemo;
		GroupBox1: TGroupBox;
    SendMemo: TMemo;
    TabSheet4: TTabSheet;
    Memo1: TMemo;
    Button3: TButton;
    Button4: TButton;
    TabSheet5: TTabSheet;
    CheckListBox1: TCheckListBox;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    TabSheet6: TTabSheet;
    Memo2: TMemo;
    mLogin: TMemo;
    mSelectSend: TMemo;
    mBroadcastMessage: TMemo;
    mBroadcastPopupMessage: TMemo;
    UserDataSetAddress: TStringField;
    UserDataSetActivity: TStringField;
		UserDataSetDate: TDateTimeField;
    AstaSQLGenerator1: TAstaSQLGenerator;
    AstaMetaData: TAsta2MetaData;
    AstaDataSet1: TAstaDataSet;
		procedure FormCreate(Sender: TObject);
    procedure AstaServerSocket1ClientConnect(Sender: TObject;
			Socket: TCustomWinSocket);
    procedure AstaServerSocket1ClientDisconnect(Sender: TObject;
      Socket: TCustomWinSocket);
    procedure Button5Click(Sender: TObject);
    procedure AstaServerSocket1ClientLogin(Sender: TObject; UserName,
      Password, AppName: string; var Verified: Boolean);
    procedure AstaServerSocket1ExecSQLParamList(Sender: TObject;
      ClientSocket: TCustomWinSocket; AQuery: TComponent; DataBaseStr,
      SQLString: String; var TheResult: Boolean; var Msg: String;
      ParamList: TAstaParamList);
    procedure AstaServerSocket1FetchBlobEvent(Sender: TObject;
      ClientSocket: TCustomWinSocket; AQuery: TComponent; DataBaseStr,
      TableName, FieldName, WhereString: String; var M: TMemoryStream);
    procedure AstaServerSocket1FetchMetaData(Sender: TObject;
      ClientSocket: TCustomWinSocket; MetaRequest: TAstaMetaData;
      DataBaseStr, Arg1, Arg2: String);
    procedure AstaServerSocket1SubmitSQL(Sender: TObject;
      ClientSocket: TCustomWinSocket; AQuery: TDataSet; DataBaseStr,
      SQLString: String; Options: TAstaSelectSQLOptionSet);
    procedure AstaServerSocket1ShowServerMessage(Sender: TObject;
      Msg: String);
    procedure AstaServerSocket1TransactionBegin(Sender: TObject;
      ClientSocket: TCustomWinSocket; Session: TComponent;
      TransActionName: String);
    procedure AstaServerSocket1TransactionEnd(Sender: TObject;
      ClientSocket: TCustomWinSocket; Session: TComponent;
      Success: Boolean);
    procedure AstaServerSocket1ThreadedDBSupplySession(Sender: TObject;
      ClientSocket: TCustomWinSocket; DataBaseString: String;
      var ASession: TComponent);
    procedure AstaServerSocket1ClientDBLogin(Sender: TObject; TheUserName,
      ThePassword, TheAppName: String; var Verified: Boolean;
      var ADataBase: TComponent);
    procedure AstaServerSocket1ThreadedDBSupplyQuery(Sender: TObject;
      ClientSocket: TCustomWinSocket; DBAction: TThreadDbAction;
      DataBaseStr: String; CreateNew: Boolean; var AQuery: TComponent;
      SQLOptions: TAstaSelectSQLOptionSet);
    procedure AstaServerSocket1CodedMessage(Sender: TObject;
      ClientSocket: TCustomWinSocket; MsgID: Integer; Msg: String);
    procedure AstaServerSocket1Decrypt(Sender: TObject; var S: string);
    procedure AstaServerSocket1Encrypt(Sender: TObject; var S: string);
    procedure AstaServerSocket1ChatLine(Sender: TObject; S: String);
    procedure BitBtn1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure AstaServerSocket1StoredProcedure(Sender: TObject;
      ClientSocket: TCustomWinSocket; AQuery: TComponent; DataBaseStr,
      AStoredProcName: String; ClientParams: TAstaParamList;
      NoResultSet: Boolean);
    procedure AstaMetaDataTables(Sender: TObject;
      ClientSocket: TCustomWinSocket; MetaDataRequest: TAstaMetaData;
      MetaDataSet: TAstaDataSet; DatabaseName, TableName: String);
    procedure AstaMetaDataFields(Sender: TObject;
      ClientSocket: TCustomWinSocket; MetaDataRequest: TAstaMetaData;
      MetaDataSet: TAstaDataSet; DatabaseName, TableName: String);
    procedure AstaMetaDataPrimeKeys(Sender: TObject;
      ClientSocket: TCustomWinSocket; MetaDataRequest: TAstaMetaData;
      MetaDataSet: TAstaDataSet; DatabaseName, TableName: String);
    procedure AstaMetaDataIndexes(Sender: TObject;
      ClientSocket: TCustomWinSocket; MetaDataRequest: TAstaMetaData;
      MetaDataSet: TAstaDataSet; DatabaseName, TableName: String);
    procedure AstaMetaDataStoredProcs(Sender: TObject;
      ClientSocket: TCustomWinSocket; MetaDataRequest: TAstaMetaData;
      MetaDataSet: TAstaDataSet; DatabaseName, TableName: String);
    procedure AstaMetaDataStoredProcColumns(Sender: TObject;
      ClientSocket: TCustomWinSocket; MetaDataRequest: TAstaMetaData;
      MetaDataSet: TAstaDataSet; DatabaseName, TableName: String);
    procedure AstaMetaDataDBMSName(Sender: TObject;
      ClientSocket: TCustomWinSocket; MetaDataSet: TAstaDataSet);
    procedure AstaMetaDataViews(Sender: TObject;
      ClientSocket: TCustomWinSocket; MetaDataRequest: TAstaMetaData;
      MetaDataSet: TAstaDataSet; DatabaseName, TableName: String);
    procedure AstaServerSocket1ExecRowsAffected(Sender: TObject;
      ClientSocket: TCustomWinSocket; AQuery: TComponent;
      var RowsAffected: Integer);
	private
		{ Private declarations }
	public
		{ Public declarations }
		Counter: LongInt;
		FUserName, FPassword: string;
    FLogOn: Boolean;
    FDBUser: String;
		procedure LogException(Sender:TObject;ErrorMsg: string; SendToClient: Boolean);
		procedure AstaException(Sender: TObject; E: Exception);
    Procedure SetUpThreads;
    Procedure ReLogonToAllSessions;
		procedure UpdateUserDataSet(Address: string; Connected: Boolean);
    procedure Logit(S: string);
    function GetDataModule(Sender: TObject; ClientSocket: TCustomWinSocket;
      DataBaseName: String): TAstaOracleDataModule;
	end;

var
	frmMain: TfrmMain;

implementation
{$R *.DFM}

uses astaabout, AstaUtil, AstaClientDataset, astathread, NCOci,
     NCOciWrapper, NCOciDB, NCOciParams;

const
	AstaDataBase = 'AstaDataBase';

procedure TfrmMain.AstaException(Sender: TObject; E: Exception);
var
  lSendIt: Boolean;
  sMsg: String;
begin
  AstaServersocket1.VerifyClientconnections;
  lSendIt := True;
  if E is ESocketError then begin
    sMsg := ESocketError(ExceptObject).Message;
    lSendIt := False;
  end
  else if E is EDataBaseError then
    sMsg := EDataBaseError(E).Message
  else
    sMsg := E.Message;
  LogException(Sender, sMsg, lSendIt);
end;

procedure TfrmMain.LogException(Sender:TObject; ErrorMsg: string; SendToClient: Boolean);
begin
  if SendToClient and (Sender is TCustomWinSocket) then
    AstaServerSocket1.SendExceptionToClient(Sender as TCustomWinSocket, ErrorMsg);
  LogIt(ErrorMsg);
end;

procedure TfrmMain.Logit(S: string);
begin
  if FLogOn then
    mRequests.Lines.Add(s);
end;

Procedure TFrmMain.SetUpThreads;
begin
  with dm do
  try
    Caption := 'ASTA NCOCI8 Server www.astatech.com ' + AstaVersion +
      ' listening on port ' + IntToStr(AstaServerSocket1.Port);
    AstaServerSocket1.CreateSessionsForDbThreads(True);
    //changed the above to true to force ASTA to dispose of the DataModule in Pooled Sessions
    Case AstaServerSocket1.ThreadingModel of
      tmSingleSession:
        Caption := Caption + ' using connect string ' + MainSession.UserName +
          '@' + MainSession.ServerName;
      tmPooledSessions:
        Caption := Caption + ' ' + IntToStr(AstaServerSocket1.SessionList.Count) +
          ' Sessions using  ' + DM.MainSession.ServerName;
      tmPersistentSessions:
        Caption := Caption + ' Persistent Sessions using ' + DM.MainSession.ServerName;
    end;
  except
    LogException(nil, 'Resetting thread model error:' +
      Exception(ExceptObject).Message, True);
  end;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
var
  I: Integer;
begin
  Application.OnException := AstaException;
  FLogOn := not ParamBool('NoLog');
  if ParamBool('AstaEncryption') then
    AstaServerSocket1.Encryption := etAstaEncrypt;
  //this is important to get SQL work bench to Function!  Do not remove
  AstaServerSocket1.CommandLinePortcheck;
  AstaServerSocket1.Active := True;
  Caption := Caption + ' ' + AstaVersion + ' listening on port ' + IntToStr(AstaServerSocket1.Port);
  try
    dm := TAstaOracleDataModule.Create(Application);
    I := ParamCheck('User=');
    if i > 0 then begin
      DM.MainSession.LoginPrompt := False;
      DM.MainSession.ConnectString := StringAfterToken(ParamStr(i), '=');
    end;
    DM.MainSession.Open;
  except
    ShowMessage('Unable to Log on to ' + DM.MainSession.ServerName);
    Application.Terminate;
  end;
  AstaServerSocket1.DataBaseName := DM.MainSession.DatabaseName;
  AstaServerSocket1.RegisterDataModule(self);
  AstaServerSocket1.RegisterDataModule(dm);
  Counter := 0;
  SetupThreads;
end;

procedure TfrmMain.UpdateUserDataSet(Address: string; Connected: Boolean);
begin
  with UserDataSet do begin
    Open;
    Append;
    FieldbyName('Address').AsString := Address;
    if Connected then
      FieldbyName('Activity').AsString := 'Connected'
    else
      FieldbyName('Activity').AsString := 'Disconnected';
    FieldByName('Date').AsDATeTime := Now;
    Post;
  end;
end;

procedure TfrmMain.AstaServerSocket1ClientConnect(Sender: TObject;
  Socket: TCustomWinSocket);
begin
  UpdateUserDataSet(Socket.RemoteAddress, True);
end;

procedure TfrmMain.AstaServerSocket1ClientDisconnect(Sender: TObject;
  Socket: TCustomWinSocket);
begin
  UpdateUserDataSet(Socket.RemoteAddress, False);
end;

procedure TfrmMain.Button5Click(Sender: TObject);
begin
  AstaServerSocket1.KillSelectedUsers('Your application is being terminated by ' +
    'the System Administrator.');
end;

procedure TfrmMain.AstaServerSocket1ChatLine(Sender: TObject; S: String);
begin
  ReceiveMemo.Lines.Add(S);
end;

procedure TfrmMain.BitBtn1Click(Sender: TObject);
begin
  AstaServerSocket1.SendBroadCastPopUp(SendMemo.Text, False);
end;

procedure TfrmMain.Button3Click(Sender: TObject);
begin
  AstaServerSocket1.SendBroadcastPopUp(mBroadcastPopupMessage.Text, False);
end;

procedure TfrmMain.Button4Click(Sender: TObject);
begin
  // Broadcasts can be coded with MsgIDs. Using case statements at the client
  // you can choose to treat different broadcasts differently
  AstaServerSocket1.SendBroadcastEvent(mBroadCastMessage.Text, False);
end;

procedure TfrmMain.Button6Click(Sender: TObject);
begin
  AstaServerSocket1.SendSelectPopupMessage(mSelectSend.Text);
end;

procedure TfrmMain.Button7Click(Sender: TObject);
begin
  AstaServerSocket1.SendSelectCodedMessage(500, mSelectSend.Text);
end;

procedure TfrmMain.AstaServerSocket1ClientLogin(Sender: TObject; UserName,
  Password, AppName: string; var Verified: Boolean);
begin
// This is where you would attempt to verify your users. "Verified" should hold
// the result of you attempt.  In this demo we will simply display the fields and
// assume the verification was True.

// Display the information
  mLogin.Lines.Add('UserName: ' + UserName + ' Password: ' + Password + ' Application Name: ' + AppName);
// Return a successful login attempt
  Verified := True;
end;

procedure TfrmMain.AstaServerSocket1Decrypt(Sender: TObject; var S: string);
begin
  S := SimpleDecrypt(S);
end;

procedure TfrmMain.AstaServerSocket1Encrypt(Sender: TObject; var S: string);
begin
  S := SimpleEncrypt(S);
end;

procedure TfrmMain.AstaServerSocket1FetchMetaData(Sender: TObject;
  ClientSocket: TCustomWinSocket; MetaRequest: TAstaMetaData; DataBaseStr,
  Arg1, Arg2: String);
begin
  if (MetaRequest in [mdFields, mdVCLFields, mdIndexes, mdPrimeKeys,
     mdForeignKeys, mdStoredProcColumns]) and (Arg1 = '') then
    Exit;
	try
    if MetaRequest in [mdTables, mdFields, mdVCLFields, mdPrimeKeys, mdIndexes,
                       mdStoredProcColumns, mdStoredProcs, mdDBMSName, mdViews] then
      AstaServerSocket1.MetaDataSet := AstaMetaData.GetMetaData(Sender,
        ClientSocket, MetaRequest, DatabaseStr, Arg1)
    else
      LogIt('*** unsupported meta request ***');
	except
		LogException(ClientSocket,'MetaData Error: ' +
      Exception(ExceptObject).Message, True);
	end;
end;

procedure TfrmMain.AstaServerSocket1FetchBlobEvent(Sender: TObject;
  ClientSocket: TCustomWinSocket; AQuery: TComponent; DataBaseStr,
  TableName, FieldName, WhereString: String; var M: TMemoryStream);
begin
  with TOCIQuery(AQuery) do
  try
    if Active then
      Close;
    SQL.Clear;
    SQL.Add('Select ' + FieldName + ' FROM ' + TableName + #13 + WhereString);
	  LogIt('Blob Fetch :' + SQL.Text);
	  Open;
	  if not Eof then
      case FieldByName(FieldName).DataType of
        ftmemo,ftOraClob: TMemoField(FieldByName(FieldName)).SaveToStream(M);
        ftblob,ftOraBlob: TBlobField(FieldbyName(FieldName)).SaveToStream(M);
     	end;
  except
		LogException(ClientSocket,'Blob Fetch Error: ' +
      Exception(ExceptObject).Message, True);
  end;
end;

procedure TfrmMain.AstaServerSocket1ExecSQLParamList(Sender: TObject;
  ClientSocket: TCustomWinSocket; AQuery: TComponent; DataBaseStr,
  SQLString: String; var TheResult: Boolean; var Msg: String;
  ParamList: TAstaParamList);
var
  I: Integer;
begin
  with TOCIQuery(AQuery) do
  try
    TheResult := False;
    LogIt('Exec ParamList Call: ' + SQLString);
    for I := 0 to ParamList.Count - 1 do
      if not (ParamList[I].datatype in [ftblob, ftmemo]) then
        LogIt(' Param ' + IntToStr(I) + '->' + ParamList[i].Name + ' ' + ParamList[i].AsString)
      else
        LogIt(' Param ' + IntToStr(I) + '->' + ParamList[i].Name + ' Size ' + IntToStr(Length(ParamList[i].AsString)));
    Active := False;
    SQL.Clear;
    SQL.Add(SqlString);
    for I := 0 to ParamList.Count - 1 do begin
      if ParamList[i].IsNull then begin
        Params[i].DataType := ParamList[i].DataType;
        Params[i].Clear;
        Params[i].Bound := True;
      end else
        case ParamList[i].DataType of
          ftstring: Params[i].AsString := ParamList[i].AsString;
          ftinteger, ftsmallint, ftword: Params[i].AsInteger := ParamList[i].AsInteger;
          ftfloat: Params[i].AsFloat := ParamList[i].AsFloat;
          ftboolean: Params[i].AsBoolean := ParamList[i].AsBoolean;
          fttime, ftdate, ftdatetime: Params[i].AsDateTime := ParamList[i].AsDateTime;
          ftblob, ftgraphic: Params[i].AsBlob := ParamList[i].AsString;
          ftmemo: Params[i].AsMemo := ParamList[i].AsString;
        end;
    end;
    ExecSQL;
    TheResult := True;
  except
    LogException(ClientSocket, 'ExecSQL Error:' + Exception(exceptObject).Message, False);
  end;
end;

procedure TfrmMain.AstaServerSocket1ExecRowsAffected(Sender: TObject;
  ClientSocket: TCustomWinSocket; AQuery: TComponent;
  var RowsAffected: Integer);
begin
  try
    RowsAffected := TOCIQuery(AQuery).RowsAffected;
  except
    LogException(ClientSocket, 'RowsAffected Error:' + Exception(exceptObject).Message, False);
  end;
end;

procedure TfrmMain.AstaServerSocket1SubmitSQL(Sender: TObject;
  ClientSocket: TCustomWinSocket; AQuery: TDataSet; DataBaseStr,
  SQLString: String; Options: TAstaSelectSQLOptionSet);
begin
  with TOCIQuery(AQuery) do
  try
    Disconnect;
    LogIt(SQLString);
    SQL.Clear;
    SQL.Add(SQLString);
    Open;
  except
		LogException(ClientSocket,'SubmitSQL Error:' +
      Exception(ExceptObject).Message, True);
  end;
end;

procedure TfrmMain.AstaServerSocket1ShowServerMessage(Sender: TObject;
  Msg: String);
begin
  LogIt(msg);
end;

procedure TfrmMain.AstaServerSocket1TransactionBegin(Sender: TObject;
  ClientSocket: TCustomWinSocket; Session: TComponent;
  TransActionName: String);
begin
  try
    LogIt('Start Transaction :' + TransactionName);
    (Session as TOCIDataBase).StartTransaction;
  except
    LogException(ClientSocket, 'TransactionBegin Error: ' +
      Exception(exceptObject).Message, True);
  end;
end;

procedure TfrmMain.AstaServerSocket1TransactionEnd(Sender: TObject;
  ClientSocket: TCustomWinSocket; Session: TComponent; Success: Boolean);
begin
  try
    if Success then begin
      LogIt('Transacion Commit');
      (Session as TOCIDatabase).Commit;
    end
    else begin
      LogIt('Transaction RollBack');
      (Session as TOCIDatabase).RollBack;
    end;
  except
		LogException(ClientSocket,'TransactionEnd Error: ' +
      Exception(ExceptObject).Message, True);
  end;
end;

procedure TfrmMain.AstaServerSocket1ThreadedDBSupplySession(
  Sender: TObject; ClientSocket: TCustomWinSocket; DataBaseString: String;
  var ASession: TComponent);
begin
  try
    ASession := TAstaOracleDataModule.Create(nil);
    with ASession as TAstaOracleDataModule do begin
      MainSession.ConnectString := DM.MainSession.ConnectString;
      MainSession.SilentMode := True;
      MainSession.Open;
    end;
  except
    LogException(ClientSocket,'Create Session Error: ' +
      Exception(ExceptObject).Message, True);
  end;
end;

Procedure TFrmMain.ReLogonToAllSessions;
var
  i: Integer;
  ADM: TastaOracleDataModule;
begin
  try
    DM.MainSession.Close;
    DM.MainSession.Open;
    LogIt('Main Database reconnected');
  except
    LogIt('Problems reconnecting to Main Login'+' '+EDataBaseError(ExceptObject).Message);
  end;
  try
    if AstaServerSocket1.ThreadingModel = tmSingleSession then
      exit;
    for i:=0 to AstaServerSocket1.SessionList.Count - 1 do begin
      Adm := AstaServerSocket1.SessionList.GetSession(i) as TAstaOracleDataModule;
      Adm.MainSession.Close;
      Adm.MainSession.Open;
      LogIt('Session '+InttoStr(i)+' reconnected');
    end;
  except
    LogIt('Session List Reconnect Problem. '+EDataBaseError(ExceptObject).Message);
    //try again!!!
    AstaServerSocket1.SessionList.Free;
    AstaServerSocket1.SessionList:=nil;
    AstaServerSocket1.CreateSessionsForDbThreads(True);
  end;
end;

procedure TfrmMain.AstaServerSocket1ClientDBLogin(Sender: TObject;
  TheUserName, ThePassword, TheAppName: String; var Verified: Boolean;
  var ADataBase: TComponent);
begin
    ADatabase := TAstaOracleDataModule.Create(nil);
    with ADataBase as TAstaOracleDataModule do begin
      MainSession.UserName := TheUserName;
      MainSession.PassWord := ThePassword;
      MainSession.ServerName := dm.MainSession.ServerName;
      MainSession.SilentMode := True;
      MainSession.Open;
      Verified:=True;
    end;
end;

procedure TfrmMain.AstaServerSocket1ThreadedDBSupplyQuery(Sender: TObject;
  ClientSocket: TCustomWinSocket; DBAction: TThreadDbAction;
  DataBaseStr: String; CreateNew: Boolean; var AQuery: TComponent;
  SQLOptions: TAstaSelectSQLOptionSet);
var
  myDM: TAstaOracleDataModule;
begin
  if Sender is TAstaThread then
    with Sender as TastaThread do begin
      myDM := TAstaOracleDataModule(TAstaThread(Sender).Session);
      if soPackets in SQLOptions then begin
        case DBAction of
          ttstoredproc:
            begin
              AQuery := TOCIStoredProc.Create(nil);
              TOCIStoredProc(AQuery).DatabaseName := myDM.MainSession.DataBaseName;
            end;
          else
            begin
              AQuery := TOCIQuery.Create(nil);
              TOCIQuery(AQuery).DatabaseName := myDM.MainSession.DataBaseName;
            end;
        end
      end
      else begin
        case dbaction of
          tttransaction, tttransactionstart:
            AQuery := myDM.MainSession;
          ttexec:
            AQuery := myDM.ExecQuery;
          else
            AQuery := myDM.SelectQuery;
        end;
      end;
    end
  else
    if soPackets in SQLOptions then begin
      AQuery := TOCIQuery.Create(nil);
      TOCIQuery(AQuery).DatabaseName := DM.MainSession.DataBaseName;
    end
    else
      case DBAction of
        ttstoredproc:
          AQuery := DM.SelectQuery;
        ttexec:
          AQuery := DM.ExecQuery;
        tttransaction, tttransactionstart:
          AQuery := DM.MainSession;
        else
          AQuery := DM.SelectQuery;
      end;
end;

procedure TfrmMain.AstaServerSocket1StoredProcedure(Sender: TObject;
  ClientSocket: TCustomWinSocket; AQuery: TComponent; DataBaseStr,
  AStoredProcName: String; ClientParams: TAstaParamList;
  NoResultSet: Boolean);
var
  I: Integer;
begin
  try
    LogIt('StoredProcedure: ' + AStoredProcName);
    TOCIStoredProc(AQuery).Close;
    if AstaServerSocket1.UseThreads = tmSingleSession then begin
      if (DataBaseStr <> '') then
        DM.ExecProc.DataBaseName := DataBaseStr
      else if (DM.ExecProc.DataBaseName = '') or
              (DM.ExecProc.DataBaseName <> AstaDataBase) then
        DM.ExecProc.DataBaseName := AstaDataBase;
    end;
    TOCIStoredProc(AQuery).StoredProcName := AStoredProcName;
    TOCIStoredProc(AQuery).Prepare;
    if (ClientParams.Count = 0) and (TOCIStoredProc(AQuery).paramCount > 0) then
      raise EDataBAseError.create('No Client Params received for ' + AStoredProcName);
    for i := 0 to Clientparams.Count - 1 do
      TOCIStoredProc(AQuery).ParambyName(ClientParams[i].Name).Value := ClientParams[i].Value;
    TOCIStoredProc(Aquery).ExecProc;
    LogIt(AStoredProcName + ' executed. ');
    ClientParams.Clear;
    for i := 0 to TOCIStoredProc(AQuery).Params.Count - 1 do
      with TOCIStoredProc(AQuery).Params[i] do
        if ParamType in [db.ptOutPut, db.ptInputOutput, db.ptResult] then begin
          if ClientParams.FindParam(Name) = nil then
            ClientParams.CreateParam(DataType, Name, TAstaParamType(ord(ParamType)));
          ClientParams.ParamByName(Name).Value := Value;
        end;
  except
    LogException(ClientSocket, 'StoredProc Error: ' +
      Exception(exceptObject).Message, True);
  end;
end;

procedure TfrmMain.AstaServerSocket1CodedMessage(Sender: TObject;
  ClientSocket: TCustomWinSocket; MsgID: Integer; Msg: String);
begin
  Case Msgid of
  1000: ReLogonToAllSessions;
  end;
end;

function TfrmMain.GetDataModule(Sender: TObject; ClientSocket: TCustomWinSocket;
  DataBaseName: String): TAstaOracleDataModule;
begin
  if AstaServerSocket1.ThreadingModel <> tmPersistentSessions then
    result := TAstaOracleDataModule(AstaServerSocket1.SessionList.
      GetSessionFromSocket(ClientSocket, nil, DataBaseName))
  else
    result := DM;
end;

procedure TfrmMain.AstaMetaDataDBMSName(Sender: TObject;
  ClientSocket: TCustomWinSocket; MetaDataSet: TAstaDataSet);
begin
  with GetDataModule(Sender, ClientSocket, '') do
    MetaDataSet.AppendRecord([AstaServerSocket1.AstaServerName,
      MainSession.ServerName, '', ExtractFileName(Application.EXEName),
      MainSession.UserName, MainSession.Password]);
end;

procedure TfrmMain.AstaMetaDataTables(Sender: TObject;
  ClientSocket: TCustomWinSocket; MetaDataRequest: TAstaMetaData;
  MetaDataSet: TAstaDataSet; DatabaseName, TableName: String);
begin
  with GetDataModule(Sender, ClientSocket, DatabaseName) do begin
    qryTables.Close;
    qryTables.Open;
    while not qryTables.Eof do begin
      MetaDataSet.AppendRecord([qryTables.Fields[0].AsString + '.' +
                                qryTables.Fields[1].AsString,
                                qryTables.Fields[0].AsString]);
      qryTables.Next;
    end;
  end;
end;

procedure TfrmMain.AstaMetaDataFields(Sender: TObject;
  ClientSocket: TCustomWinSocket; MetaDataRequest: TAstaMetaData;
  MetaDataSet: TAstaDataSet; DatabaseName, TableName: String);
var
  sTab, sOwn: String;
  i: Integer;
begin
  with GetDataModule(Sender, ClientSocket, DatabaseName) do begin
    if pos('.', TableName) > 0 then begin
      sTab := StringAfterToken(TableName, '.');
      sOwn := StringBeforeToken(TableName, '.');
    end
    else begin
      sTab := TableName;
      sOwn := MainSession.UserName;
    end;
	  qryFields.Close;
    qryFields.SQL.Text := 'select * from ' + sOwn + '.' + sTab + ' where 0 = 1';
	  qryFields.Open;
    for i := 0 to qryFields.FieldCount - 1 do
      MetaDataSet.AppendRecord([qryFields.Fields[i].FieldName,
                                Integer(qryFields.Fields[i].DataType),
                                qryFields.Fields[i].Size]);
	  qryFields.Close;
  end;
end;

procedure TfrmMain.AstaMetaDataPrimeKeys(Sender: TObject;
  ClientSocket: TCustomWinSocket; MetaDataRequest: TAstaMetaData;
  MetaDataSet: TAstaDataSet; DatabaseName, TableName: String);
var
  sTab, sOwn: String;
begin
  with GetDataModule(Sender,ClientSocket,DatabaseName) do begin
    if pos('.', TableName) > 0 then begin
      sTab := StringAfterToken(TableName, '.');
      sOwn := StringBeforeToken(TableName, '.');
    end
    else begin
      sTab := TableName;
      sOwn := MainSession.UserName;
    end;
    qryPKFields.Close;
	  qryPKFields.ParamByName('Owner').AsString := sOwn;
	  qryPKFields.ParamByName('TableName').AsString := sTab;
    qryPKFields.Open;
    while not qryPKFields.Eof do begin
      MetaDataSet.AppendRecord([qryPKFields.Fields[0].AsString]);
      qryPKFields.Next;
    end;
  end;
end;

procedure TfrmMain.AstaMetaDataIndexes(Sender: TObject;
  ClientSocket: TCustomWinSocket; MetaDataRequest: TAstaMetaData;
  MetaDataSet: TAstaDataSet; DatabaseName, TableName: String);
var
  sTab, sOwn: String;
begin
  with GetDataModule(Sender,ClientSocket,DatabaseName) do begin
    if pos('.', TableName) > 0 then begin
      sTab := StringAfterToken(TableName, '.');
      sOwn := StringBeforeToken(TableName, '.');
    end
    else begin
      sTab := TableName;
      sOwn := MainSession.UserName;
    end;
    qryIndexes.Close;
	  qryIndexes.ParamByName('Owner').AsString := sOwn;
	  qryIndexes.ParamByName('TableName').AsString := sTab;
    qryIndexes.Open;
    while not qryIndexes.Eof do begin
      MetaDataSet.AppendRecord([qryIndexes.Fields[0].AsString]);
      qryIndexes.Next;
    end;
  end;
end;

procedure TfrmMain.AstaMetaDataStoredProcs(Sender: TObject;
  ClientSocket: TCustomWinSocket; MetaDataRequest: TAstaMetaData;
  MetaDataSet: TAstaDataSet; DatabaseName, TableName: String);
begin
  with GetDataModule(Sender, ClientSocket, DatabaseName) do begin
    qrySpName.Close;
   	qrySpName.Open;
    while not qrySpName.Eof do begin
      MetaDataSet.AppendRecord([qrySpName.Fields[0].AsString + '.' +
                                qrySpName.Fields[1].AsString,
                                qrySpName.Fields[0].AsString]);
      qrySpName.Next;
    end;
  end;
end;

procedure TfrmMain.AstaMetaDataStoredProcColumns(Sender: TObject;
  ClientSocket: TCustomWinSocket; MetaDataRequest: TAstaMetaData;
  MetaDataSet: TAstaDataSet; DatabaseName, TableName: String);
var
  i: Integer;
begin
  with GetDataModule(Sender, ClientSocket, DatabaseName) do begin
    spSpColumn.StoredProcName := TableName;
    spSpColumn.Prepare;
    for i := 0 to spSpColumn.Params.Count - 1 do begin
			MetaDataSet.Append;
			MetaDataSet.FieldByName('ColumnName').AsString := spSpColumn.Params[i].Name;
      MetaDataSet.FieldbyName('ColumnType').AsInteger := Integer(spSpColumn.Params[i].DataType);
      MetaDataSet.FieldbyName('ColumnSize').AsInteger := Integer(spSpColumn.Params[i].ODataSize);
      MetaDataSet.FieldbyName('ParamType').AsInteger := Integer(spSpColumn.Params[i].ParamType);
			MetaDataSet.Post;
		end;
  end;
end;

procedure TfrmMain.AstaMetaDataViews(Sender: TObject;
  ClientSocket: TCustomWinSocket; MetaDataRequest: TAstaMetaData;
  MetaDataSet: TAstaDataSet; DatabaseName, TableName: String);
begin
  with GetDataModule(Sender, ClientSocket, DatabaseName) do begin
    qryViews.Close;
    qryViews.Open;
    while not qryViews.Eof do begin
      MetaDataSet.AppendRecord([qryViews.Fields[0].AsString + '.' +
                                qryViews.Fields[1].AsString,
                                qryViews.Fields[0].AsString]);
      qryViews.Next;
    end;
  end;
end;

initialization

//  FOCIDatabaseNameExpandMode := deUseOwner; //deUseThread;

end.


