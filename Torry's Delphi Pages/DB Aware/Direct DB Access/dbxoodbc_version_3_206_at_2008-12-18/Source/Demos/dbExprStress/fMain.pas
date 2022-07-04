{
  DbExpress Stress test tools:
    Testing dbExpress driver in multithreaded working ...

  Part of Kylix / Delphi open source DbExpress driver for ODBC
  Version 1.10, 2003-05-21

  Copyright (c) 2003 by Vadim V.Lopushansky

  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public License
  as published by the Free Software Foundation; either version 2.1
  of the License, or (at your option) any later version.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the GNU Lesser General Public License for more details.
}

unit fMain;

{$B-,J+}

{$DEFINE dbxODBC} // for dbxOpenODBC
//************************************

{$IFDEF VER140} // Delphi 6 only }
  {$DEFINE _D6_}
  {$DEFINE _FIX_D6_THREADCONNECTIONERROR_}
  {---------------------------------------
    SqlExpr.pas
      threadvar
      ^^^^^^^^^ !!! must be threadvar, but not var ...
        GetDriver: function(SVendorLib, SResourceFile: PChar; out Obj): SQLResult; stdcall;
        DllHandle: THandle;
  ---------------------------------------}
{$ELSE}
    {$DEFINE _D7UP_}// Delphi 7 or more ( and Kylix )
    {$IFDEF VER150} // Delphi 7 only
      {$DEFINE _D7_}//
    {$ENDIF}
{$ENDIF}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, DBXpress, DB, SqlExpr, ExtCtrls, ComCtrls,
  {$ifdef dbxODBC} DbxOpenOdbc, DbxOpenOdbcInterface, {$endif}
  SQLConst, DBClient, Provider, FMTBcd, AppEvnts;

const
  WM_STRESSBASE = WM_USER+1000;

  WM_UPDTHREAD  = WM_STRESSBASE+0;
  WM_CLEARLOG   = WM_STRESSBASE+1;
  WM_ADDERROR   = WM_STRESSBASE+2;
  WM_ADDUPDATE  = WM_STRESSBASE+3;

type
  TfrmMain = class(TForm)
    Timer: TTimer;
    SB: TStatusBar;
    pTop: TPanel;
    StaticText1: TStaticText;
    stThreadCount: TStaticText;
    StaticText3: TStaticText;
    stTimeElapsed: TStaticText;
    StaticText7: TStaticText;
    stErrors: TStaticText;
    StaticText8: TStaticText;
    stUpdates: TStaticText;
    btCreateThread: TButton;
    Shape1: TShape;
    pcMain: TPageControl;
    tsCon: TTabSheet;
    tsLog: TTabSheet;
    tsHelp: TTabSheet;
    mLog: TMemo;
    mHelp: TMemo;
    pcCon: TPageControl;
    ts_sql_update: TTabSheet;
    ed_table: TEdit;
    StaticText5: TStaticText;
    StaticText6: TStaticText;
    ts_sql_select: TTabSheet;
    m_sql_select: TMemo;
    tcConStr: TTabSheet;
    StaticText2: TStaticText;
    edDSN: TEdit;
    u_updflds: TMemo;
    StaticText4: TStaticText;
    ed_custStr: TEdit;
    gbTranIsol: TGroupBox;
    cbti_DirtyRead: TCheckBox;
    cbti_ReadCommited: TCheckBox;
    cbti_RepeatableRead: TCheckBox;
    cb_allowopencursorintransaction: TCheckBox;
    pTO: TPanel;
    cb_ReadMetadata: TCheckBox;
    gbFldOp: TGroupBox;
    cboUpdate: TCheckBox;
    cboInsert: TCheckBox;
    cboDelete: TCheckBox;
    btStop: TButton;
    cb_AllowUsagePacketRecords: TCheckBox;
    SQLConTemplate: TSQLConnection;
    Panel1: TPanel;
    Button1: TButton;
    procedure btCreateThreadClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btStopClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
    procedure WM_UPDTHREAD(var Msg:TMessage); message WM_UPDTHREAD;
    procedure WM_CLEARLOG(var Msg:TMessage); message WM_CLEARLOG;
    procedure WM_ADDERROR(var Msg:TMessage); message WM_ADDERROR;
    procedure WM_ADDUPDATE(var Msg:TMessage); message WM_ADDUPDATE;

  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

const
  csLine :String = '****************************************';

{$IFDEF _D6_}
var
 fConFixLock:TRTLCriticalSection;
{$ENDIF}

Type
 TTestThread = class(TThread)
 private

   fMessage:String;
   fCon         :TSQLConnection;
   //Custom Connection String
   fCustConStr     :String;
   // Transaction Issolation
   cbti_DirtyRead: Boolean;
   cbti_ReadCommited: Boolean;
   cbti_RepeatableRead: Boolean;
   fTranIsol:array[0..2]of integer;
   fTranDefIdx:integer;
   //Allow Open Cursor in Transaction
   fAllowOpenCursorInTransaction :Boolean;
   fAllowUsagePacketRecords:Boolean;

   fConString   :String;
   fSQLTemplate :String;
   fSQLTable    :String;
   fReadMetadata :Boolean;
   fUpdateFieldsText :String;
   fUpdateFields :TStringList;

   procedure Task1;
   procedure DoMessage;
   procedure EnableNewThreadBtn;

   procedure SQLConnectionBeforeConnect(Sender: TObject);

   procedure DataSetProviderUpdateError(Sender: TObject;
      DataSet: TCustomClientDataSet; E: EUpdateError;
      UpdateKind: TUpdateKind; var Response: TResolverResponse);

   procedure ClientDataSetReconcileError(
      DataSet: TCustomClientDataSet; E: EReconcileError;
      UpdateKind: TUpdateKind; var Action: TReconcileAction);

 public
   constructor Create( const ConString, CustConStr, SQLTemplate, SQLTable, UpdateField:String;
                       bReadMetadata, bAllowOpenCursorInTransaction, bAllowUsagePacketRecords,
                       bDirtyRead, bReadCommited, bRepeatableRead: Boolean;
                       ConTemplate: TSQLConnection
   );
   destructor Destroy;override;
   procedure Execute;override;
   procedure ShowThreadMessage(const Message:String);
 end;

{ TTestThread }

var
 cThreadCount:Integer;
 cUpdatesCount:Integer;
 cErrors:Integer;
 cMainWnd:HWND;
 cTerminate :Boolean;

constructor TTestThread.Create( const ConString, CustConStr, SQLTemplate, SQLTable, UpdateField: String;
                                bReadMetadata, bAllowOpenCursorInTransaction, bAllowUsagePacketRecords,
                                bDirtyRead, bReadCommited, bRepeatableRead: Boolean;
                                ConTemplate: TSQLConnection);
 var i:integer;
begin
  inherited create(True);
  FreeOnTerminate := True;

  fConString := ConString;
  fCustConStr := CustConStr;
  fSQLTemplate := SQLTemplate;
  fSQLTable := SQLTable;
  fUpdateFieldsText := UpdateField;

  fReadMetadata := bReadMetadata;

  cbti_DirtyRead:=bDirtyRead;
  cbti_ReadCommited:=bReadCommited;
  cbti_RepeatableRead:=bRepeatableRead;

  // Fill transaction type map(randomize array) ...
  if cbti_DirtyRead then
    fTranIsol[0] := 0
  else
    fTranIsol[0] := -1;

  if cbti_ReadCommited then
    fTranIsol[1] := 1
  else
    fTranIsol[1] := -1;

  if cbti_RepeatableRead then
    fTranIsol[2] := 2
  else
    fTranIsol[2] := -1;

  fTranDefIdx := -1;
  for i:=0 to 2 do
    if fTranIsol[i]>0 then
    begin
      fTranDefIdx := i;
      break;
    end;

  if fTranDefIdx>=0 then
    for i:=0 to 2 do
      if fTranIsol[i]<0 then
        fTranIsol[i] := fTranDefIdx;

  fAllowOpenCursorInTransaction := bAllowOpenCursorInTransaction and (fTranDefIdx>=0);

  fAllowUsagePacketRecords := bAllowUsagePacketRecords;

  fCon := TSQLConnection.Create(nil);
  fCon.SQLHourGlass := False;

  fCon.BeforeConnect := SQLConnectionBeforeConnect;
  // Enter you dbExpress Driver Login parameters:
  //**************************************************
  {$ifdef dbxODBC}
    fCon.ConnectionName := 'Open Odbc Simple Dsn';
    fCon.DriverName     := 'OpenOdbc';
    fCon.GetDriverFunc  := 'getSQLDriverODBC';
    fCon.LibraryName    := 'dbxoodbc.dll';
    fCon.VendorLib      := 'odbc32.dll';
  {$else}
    with ConTemplate do begin
      fCon.ConnectionName := ConnectionName;
      fCon.DriverName     := DriverName;
      fCon.GetDriverFunc  := GetDriverFunc;
      fCon.LibraryName    := LibraryName;
      fCon.VendorLib      := VendorLib;
      fCon.Params.Text    := Params.Text;
    end;
  {$endif}
  //**************************************************

  fCon.LoginPrompt    := False;

  if Length(fConString)>0 then
   fCon.Params.Values[DATABASENAME_KEY] := fConString;

  {$ifdef _D7UP_}
  if Length(fCustConStr)>0 then
    fCon.Params.Values[CUSTOM_INFO] := fCustConStr;
  {$endif}

  // Default Connection Transaction:
  if fTranDefIdx>=0 then
    case fTranIsol[random(3)] of
      0: fCon.Params.Values[fCon.DriverName+' TransIsolation'] := 'DirtyRead';
      1: fCon.Params.Values[fCon.DriverName+' TransIsolation'] := 'ReadCommited';
      2: fCon.Params.Values[fCon.DriverName+' TransIsolation'] := 'RepeatableRead';
    end;

  Resume;
end;

destructor TTestThread.Destroy;
begin
  fUpdateFields.Free;
  fCon.Free;
  inherited;
end;

procedure TTestThread.SQLConnectionBeforeConnect(Sender: TObject);
begin
  {$ifdef dbxODBC}
    // dbxODBX build in:
    RegisterDbXpressLib( @DbxOpenOdbc.getSQLDriverODBC );
  {$else}
//    GetDriver:= nil;
//    DllHandle:=0;
  {$endif}
end;

procedure TTestThread.Execute;
 {$ifdef dbxODBC}
 var
   fSqlConIntf: ISqlConnection;
   fSqlConOdbcIntf: ISqlConnectionOdbc;
 {$endif}
begin
  InterlockedIncrement(cThreadCount);
  postMessage(cMainWnd, WM_UPDTHREAD,0,0);
  fUpdateFields := nil;
  try
    fUpdateFields := TStringList.Create;
    fUpdateFields.Text := fUpdateFieldsText;
    // connect:
    try

      {$IFDEF _FIX_D6_THREADCONNECTIONERROR_}
      EnterCriticalSection(fConFixLock);
      try
      {$ENDIF}

        //frmMain.SQLConTemplate.Open;

        fCon.Connected := True;

      {$IFDEF _FIX_D6_THREADCONNECTIONERROR_}
      finally
        LeaveCriticalSection(fConFixLock);
      end;
      {$ENDIF}

    except
      on e:exception do
      begin
        ShowThreadMessage('###Connection Error:'+e.Message);
        fCon.Connected := False;
        exit;
      end;
    end;

    {$ifdef dbxODBC}
    fSqlConIntf := fCon.SqlConnection;
    if fSqlConIntf.QueryInterface(ISqlConnectionOdbc, fSqlConOdbcIntf) = S_OK then
    begin
      case fSqlConOdbcIntf.GetDbmsType of
        eDbmsTypeInformix:
          with TSQLQuery.Create(nil) do
          try
            SQLConnection := fCon;
            try
              begin
                sql.text := 'set lock mode to wait 17';
                execSql;
              end;
            except
              on e:exception do
              begin
                ShowThreadMessage('###LoginSQL:'+e.Message);
                fSqlConOdbcIntf := nil;
                fSqlConIntf := nil;
                fCon.Connected := False;
                exit;
              end;
            end;
          finally
            Free;
          end;
      end;
    end;
    fSqlConOdbcIntf := nil;
    fSqlConIntf := nil;
    {$endif}

    try
      while (not Terminated)and(not cTerminate) do
      begin
        // run task:
        try
         if random(2)=0 then
           sleep(1);
          Task1;
        except
          on EAbort do
            //exit;
            sleep(100);
          on e:Exception do
          begin
            postMessage(cMainWnd, WM_ADDERROR, 1, 1);
            ShowThreadMessage('###Task Error:'+e.Message);
          end;
        end;
      end;
    finally
      fCon.Connected := False;
    end;

  finally
    if InterlockedDecrement(cThreadCount)=0 then
      Synchronize(EnableNewThreadBtn);
    postMessage(cMainWnd, WM_UPDTHREAD,0,0);
  end;
end;

procedure TTestThread.Task1;
 var qry :TSQLQuery;
     cds:TClientDataSet;
     prov:TDataSetProvider;
     Field:TField;
     i, ir1, ir2, Delta :integer;
     List:TList;// cursors
     TD: TTransactionDesc;
     function getStrRandom(MaxLen:Integer; MinLen:Integer = 1):String;
       var i:integer;
     begin
       Result := IntToStr(GetCurrentThreadID)+'-';
       i := Length(Result)+1;
       SetLength(Result, random(MaxLen) );
       if Length(Result)<MinLen then
         SetLength(Result, MinLen);
       if Length(Result)<i then
        i := 1;
       for i:=i to Length(Result) do
       begin
         if random(2)=0 then
           Result[i] := char( ord('a')+random(ord('z')-ord('a')))
         else
           Result[i] := char( ord('A')+random(ord('Z')-ord('A')));
        if ((i div 200)=0) and (Terminated or cTerminate) then
           exit;
       end;
     end;
     procedure initBlobRandom(BF:TBlobField);
      var
       MS: TMemoryStream;
       i:int64;
      type
        TBArr = array[0..0]of byte;
        PBArr = ^TBArr;
     begin
       MS := TMemoryStream.Create;
       try
         if BF.BlobSize>0 then
           MS.Size := random(BF.BlobSize)
         else
           MS.Size := 1 + random(1024*100);
         i:=0;
         while i<MS.Size do
         begin
           {$R-}
           PBArr(MS.Memory)[i] := random(256);
           inc(i);
           if ((i div 200)=0) and (Terminated or cTerminate) then
             exit;
         end;
         BF.LoadFromStream(MS);
       finally
         MS.Free;
       end;
     end;
begin
  if fAllowOpenCursorInTransaction then
  begin
    TD.TransactionID := 1;
    case fTranIsol[random(3)] of
      0: TD.IsolationLevel := xilDIRTYREAD;
      1: TD.IsolationLevel := xilREADCOMMITTED;
      2: TD.IsolationLevel := xilREPEATABLEREAD;
    end;
  end;
  cds:=nil;
  List := TList.Create;
  try
    ir1:= random(33)-1;
    for ir1:=0 to ir1 do
    begin // test reopen query, cloning connection and manu opened cursors:

      if (cds=nil)or(random(14)>5) then
      begin
        qry := TSQLQuery.Create(nil);
        List.Add(qry);
        //read metadata check:
        //if you make updates no ever PK then you can receive error:
        //Record not found or changed by another user
        // Metadata даст возможность определить UpdateMode для ApplyUpdates
        //if not random(33) in [1,13,15,17,29..31] then // <- disable metadata (not recmended)
        if fReadMetadata then
        begin
          qry.NoMetaData := False;
          {$IFDEF _D7UP_}
          qry.GetMetadata := True;
          {$ENDIF}
        end
        else
        begin
          qry.NoMetaData := True;
          {$IFDEF _D7UP_}
          qry.GetMetadata := False;
          {$ENDIF}
        end;

        qry.SQLConnection := fCon;
        qry.SQL.Text := format(fSQLTemplate, [fSQLTable]);
        {try
          qry.Open;
        except
          on e:exception do
          begin
            ShowThreadMessage('###TSQLQuery.Open:'+e.Message);
            Abort;
          end;
        end;}
        cds  := TClientDataSet.Create(qry);
        prov := TDataSetProvider.Create(qry);
        prov.OnUpdateError := DataSetProviderUpdateError;
        prov.Name := 'prov_tmp_'+IntToStr(Integer(@prov));
        cds.ProviderName := prov.Name;
        cds.OnReconcileError := ClientDataSetReconcileError;
        if fAllowUsagePacketRecords and (random(12)>4) then
        begin
          cds.PacketRecords := 5+ random(20);
          cds.FetchOnDemand := True;
        end
        else
          cds.PacketRecords := -1;
        prov.DataSet := qry;
      end;

      if (Terminated or cTerminate) then
        exit;

      // sleep ::= func(cThreadCount, Magic150)
      if random(100)>50 then
        sleep(1+random(150 div cThreadCount));

      if fAllowOpenCursorInTransaction then
      begin
        if fCon.InTransaction then
          try
          fCon.Commit(TD);
          except
            on e:exception do
            begin
              ShowThreadMessage('###Commit:'+e.Message);
              Abort;
            end;
          end;
        if random(2)=0 then
          try
           fCon.StartTransaction(TD);
          except
            on e:exception do
            begin
              ShowThreadMessage('###StartTransaction:'+e.Message);
              Abort;
            end;
          end;
      end;

      cds.Close;
      try
        cds.Open;
      except
        on e:exception do
        begin
          ShowThreadMessage('###CDS.Open:'+e.Message);
          Abort;
        end;
      end;

      // auto detect update mode:
      prov.UpdateMode := upWhereAll;//upWhereChanged;
      if {fReadMetadata and {}(cds.FieldCount>1)
      then
        for i:=0 to cds.FieldCount-1 do begin
           if (pfInKey in cds.Fields[i].ProviderFlags)
           then begin
               prov.UpdateMode := upWhereKeyOnly;
               Break;
           end;
      end;

      // fetch all:
      if random(10) in [2,6,7] then
      begin
        cds.Last;
        if (cds.RecordCount>100) and (random(2)=0) then
          sleep(1);
        cds.First;
      end;

      ir2 := random(7);
      for ir2 := 0 to ir2 do
      begin // allocate CDS changes log

        if (Terminated or cTerminate) then
          exit;

        // move to random position:
        if (cds.RecNo>1)and(random(11)>5) then
          //random prev
          Delta := - random(cds.RecNo+1)
        else
          // random next
          Delta := random(cds.RecordCount-cds.RecNo+2);
        cds.MoveBy(Delta);

        // change record:
        cds.Edit;
          // **********************************************************
          for i:=0 to fUpdateFields.Count-1 do
          begin
            Field := cds.FindField(fUpdateFields[i]);
            if Assigned(Field) and
               (not (pfInKey in Field.ProviderFlags)) //cannot allow change PK fields
            then
            case Field.DataType of
              ftString, ftFixedChar, ftWideString:
                begin
                  Field.AsString := getStrRandom(Field.Size);
                  //ODBC cannot assign zero length string value:
                  {$ifdef dbxODBC}
                  if Length(Field.AsString) = 0 then
                    Field.Clear;
                  {$else}
                  if (Length(Field.AsString) = 0)and(random(2)=0) then
                    Field.Clear;
                  {$endif}
                end;
              ftMemo:
                begin
                  if random(6)=0 then
                    {$ifdef dbxODBC}
                    Field.Clear
                    {$else}
                    if random(2)=0 then
                      Field.Clear
                    else
                      Field.AsString := ''
                    {$endif}
                  else
                  begin
                    if (Field as TMemoField).BlobSize>0 then
                      Field.AsString := getStrRandom(TMemoField(Field).BlobSize,24)
                    else
                      Field.AsString := getStrRandom(1024*100,24);//100Kb ???
                  end;
                end;
              ftBoolean:
                begin
                  if random(6)=0 then
                    Field.Clear
                  else
                  begin
                    if random(2)=0 then
                      Field.AsBoolean := False
                    else
                      Field.AsBoolean := True;
                  end;
                end;
              ftSmallint, ftInteger, ftWord, ftFloat, ftCurrency, ftBCD, ftLargeint, ftFMTBcd:
                begin
                  if random(6)=0 then
                    Field.Clear
                  else
                  begin
                    if Field.DataType in [ftBCD, ftFMTBcd] then
                      Field.AsInteger := random(10) //???
                    else
                      Field.AsCurrency := GetCurrentThreadID;
                  end;
                end;
              ftDate, ftTime, ftDateTime, ftTimeStamp:
                begin
                  if Field.DataType in [ftDateTime, ftTimeStamp] then
                    Field.AsDateTime := Now
                  else
                  if Field.DataType = ftDate then
                    Field.AsDateTime := Date
                  else
                  //if Field.DataType = ftTime then
                    Field.AsDateTime := Time;
                end;
              ftBlob:
                begin
                  if random(6)=0 then
                    Field.Clear
                  else
                  with Field As TBlobField do
                  begin
                    case BlobType of
                      ftBlob:
                        begin
                          initBlobRandom(TBlobField(Field));
                        end;
                      ftMemo:
                        begin
                          if BlobSize>0 then
                            Field.AsString := getStrRandom(BlobSize,1)
                          else
                            Field.AsString := getStrRandom(24+1024*100,1);
                        end;
                    end;
                  end;
                end;
            end;
          end;
          // **********************************************************
        if (Terminated or cTerminate) then
          Abort;
        if random(100)>3 then
          try
            cds.Post     // check log post
          except
            on e:exception do
            begin
             ShowThreadMessage('###CDS.Post:'+e.message);
             postMessage(cMainWnd, WM_ADDERROR,1,1);
             Abort;
            end;
          end
        else
          cds.Cancel;  // check log cancel

      end;//of: for ir2 - packet allocate

      // sleep ::= func(cThreadCount, Magic90)
      if random(100)<50 then
        sleep(1+random(90 div cThreadCount));

      if random(100)<5 then
        cds.MergeChangeLog // check CDS merge log
      else
      if cds.ChangeCount>0 then
      begin
         try
          Delta := cds.ChangeCount;
          try
           cds.ApplyUpdates(0);
          finally
            if cds.ChangeCount>0 then
              Delta := Delta - cds.ChangeCount;
            if (cds.ChangeCount<>Delta) then
            begin
              postMessage(cMainWnd, WM_ADDUPDATE, Delta, Delta);
              sleep(2);
            end
            else
            begin
             Abort;
            end;
          end;
         except
           on e:exception do
           begin
             ShowThreadMessage('###CDS.ApplyUpdates:'+e.Message);
             postMessage(cMainWnd, WM_ADDERROR,1,1);
             Abort;
           end;
         end;
         if cds.ChangeCount>0 then
         begin
           ShowThreadMessage('###CDS.ApplyUpdates:');
           postMessage(cMainWnd, WM_ADDERROR,1,1);
           Abort;
         end;
      end;
    end;//of:for ir1
    if fAllowOpenCursorInTransaction then
    begin
      if fCon.InTransaction then
        try
        fCon.Commit(TD);
        except
          on e:exception do
          begin
            ShowThreadMessage('###Commit:'+e.Message);
            Abort;
          end;
        end;
    end;
  finally
    // sleep ::= func(cThreadCount, Magic25)
    if random(100)<50 then
      sleep(1+random(25 div cThreadCount));
    // clear List
    for i:=List.Count-1 downto 0 do
      TSQLQuery(List[i]).Free;
  end;
end;

procedure TTestThread.DataSetProviderUpdateError(Sender: TObject;
  DataSet: TCustomClientDataSet; E: EUpdateError; UpdateKind: TUpdateKind;
  var Response: TResolverResponse);
begin
  Response := rrSkip;
  ShowThreadMessage('###Provider:'+e.Message);
end;

procedure TTestThread.ClientDataSetReconcileError(
  DataSet: TCustomClientDataSet; E: EReconcileError;
  UpdateKind: TUpdateKind; var Action: TReconcileAction);
begin
  Action := raSkip;
  ShowThreadMessage('###Reconcile:'+e.Message);
end;

procedure TTestThread.ShowThreadMessage(const Message: String);
begin
  fMessage := Message;
  Synchronize(DoMessage);
end;

procedure TTestThread.DoMessage;
begin
  if frmMain = nil then
    exit;
  //frmMain.pcMain.ActivePageIndex := 1;
  frmMain.mLog.Lines.BeginUpdate;
    frmMain.mLog.Lines.Insert(0, csLine);
    frmMain.mLog.Lines.Insert(0, fMessage);
  frmMain.mLog.Lines.EndUpdate;
  if frmMain.mLog.Lines.Count > 5000 then
    PostMessage( frmMain.Handle, WM_CLEARLOG,0,0);
end;

procedure TTestThread.EnableNewThreadBtn;
begin
  if Assigned(frmMain) then
  begin
    frmMain.btCreateThread.Enabled := True;
    frmMain.Timer.Enabled := False;
    cTerminate := False;
  end;
end;

{ TfrmMain }

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  randomize;
  cMainWnd := Handle;
  pcMain.ActivePageIndex := 0;
  pcCon.ActivePageIndex := 0;
  {$ifndef dbxODBC}
    edDSN.Text := SQLConTemplate.Params.Values['Database'];
    GetDriver:= nil;
    DllHandle:=0;
  {$endif}
end;

var
  startDateTime:TDateTime;

procedure TfrmMain.btCreateThreadClick(Sender: TObject);
begin
  if {$ifdef dbxODBC}
     (Trim(edDSN.Text)='')or
     {$endif}
     (Trim(m_sql_select.Text)='')or
     (Trim(ed_table.Text)='')or
     (Trim(u_updflds.Text)='')
  then
  begin
    mLog.Lines.Insert(0, csLine);
    mLog.Lines.Insert(0,'###Thread.Create:bad parameters');
    exit;
  end;

  if not Timer.Enabled then
  begin
    stErrors.Caption := '';

    cThreadCount  := 0;
    cUpdatesCount := 0;
    cErrors       := 0;
    cTerminate    := False;

    startDateTime := Now;
    TimerTimer(nil);
    Timer.Enabled := True;
  end;
  TTestThread.Create(
   Trim(edDSN.Text),       // connection string
   Trim(ed_custStr.Text),  // connection custom string
   (m_sql_select.Text),    // query template text
   Trim(ed_table.Text),    // update table name
   Trim(u_updflds.Text),   // updated fields

   cb_ReadMetadata.Checked, // read table metadata

   cb_allowopencursorintransaction.Checked,
   cb_AllowUsagePacketRecords.Checked,

   cbti_DirtyRead.Checked,
   cbti_ReadCommited.Checked,
   cbti_RepeatableRead.Checked,
   SQLConTemplate
  );

end;

procedure TfrmMain.WM_UPDTHREAD(var Msg:TMessage);
begin
  stThreadCount.Caption := IntToStr(cThreadCount)
end;

procedure TfrmMain.WM_CLEARLOG(var Msg:TMessage);
 var i:integer;
     s:string;
begin
  if mLog.Lines.Count < 5000 then
    exit;
  s:='';
  for i:=0 to 30 do
    s:=s+mLog.Lines[i]+#13#10;
  mLog.Lines.Text := S+csLine;
end;

procedure TfrmMain.WM_ADDERROR(var Msg:TMessage);
begin
  inc(cErrors);
  stErrors.Caption := IntToStr(cErrors);
end;

procedure TfrmMain.WM_ADDUPDATE(var Msg:TMessage);
begin
   inc( cUpdatesCount, Msg.LParam );
   stUpdates.Caption := IntToStr(cUpdatesCount);
end;

function GetLabelDelta(D:TDateTime):String;
  var
    Hour, Min, Sec, MSec: Word;
  function d2s(d:integer):string;
  begin
    if d<10 then
      result := '0'+IntToStr(d)
    else
      result := IntToStr(d);
  end;
begin
    DecodeTime(D, Hour, Min, Sec, MSec);
    Result := format('[ %s:%s:%s ]',[d2s(Hour), d2s(Min), d2s(Sec) ]);
end;

procedure TfrmMain.TimerTimer(Sender: TObject);
begin
  stTimeElapsed.Caption := GetLabelDelta(Now-startDateTime);
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  frmMain := nil;
end;

procedure TfrmMain.btStopClick(Sender: TObject);
begin
  if cThreadCount>0 then
  begin
    cTerminate := True;
    btCreateThread.Enabled := False;
  end;
end;

procedure TfrmMain.Button1Click(Sender: TObject);
begin
  mLog.Clear;
end;

{$IFDEF _FIX_D6_THREADCONNECTIONERROR_}
initialization
  InitializeCriticalSection(fConFixLock);
finalization
  DeleteCriticalSection(fConFixLock);
{$ENDIF}
end.
