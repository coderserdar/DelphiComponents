unit ADOBase;

interface

uses
  Windows, Messages, Classes, SysUtils, ComObj, ActiveX, Registry, DB,
  {$IFDEF MSWINDOWS}
  Variants,
  {$ENDIF}
  ADOApi, DUtils, Forms, ShellApi;

const
  cnsTransaction     = 'Transaction DDL';
  cnsErrorConnection = 'Connection has not been established.';

type
  TCommandOption  = (coUnknown,
                     coText,
                     coTable,
                     coStoredProc,
                     coFile,
                     coTableDirect,
                     coAsyncExecute,
                     coAsyncFetch,
                     coAsyncFetchNonBlocking);
  TCursorType     = (ctOpenForwardOnly,
                     ctOpenKeyset,
                     ctOpenDynamic,
                     ctOpenStatic);
  TLockType       = (ltLockReadOnly,
                     ltLockPessimistic,
                     ltLockOptimistic,
                     ltLockBatchOptimistic);
  TCursorLocation = (clClient, clServer);
  TIsolationLevel = (ilXactChaos,
                     ilXactUnspecified,
                     ilXactBrowse,
                     ilXactReadUncommitted,
                     ilXactCursorStability,
                     ilXactReadCommitted,
                     ilXactRepeatableRead,
                     ilXactIsolated,
                     ilXactSerializable);

  TConnectionMode = (cmModeUnknown,
                     cmModeRead,
                     cmModeWrite,
                     cmModeReadWrite,
                     cmModeShareDenyRead,
                     cmModeShareDenyWrite,
                     cmModeShareExclusive,
                     cmModeShareDenyNone);

  TDConnection = class(TObject)
  private
    { Private declarations }
    FADOConnection  : _Connection;
    FConnect        : String;
    FCommTimeOut    : Integer;
    FConnTimeOut    : Integer;
    FIsolation      : TIsolationLevel;
    FMode           : TConnectionMode;
    FCursor         : TCursorLocation;
    FLocale         : Boolean;
    FDefTable       : String;
    FHasTran        : Boolean;
    FActive         : Boolean;
    FInfo           : TStrings;
    FMachine        : String;

    procedure SetActive(Value: Boolean);
    procedure SetConnect(const Value: String);
    function  GetVersion: String;

    function GetIsolationLevel(Level: TIsolationLevel): IsolationLevelEnum;
    function GetConnectMode(Mode: TConnectionMode): ConnectModeEnum;

  protected
    { Protected declarations }
    procedure InternalOpen; virtual;
    procedure InternalClose; virtual;

  public
    { Public declarations }
    constructor Create(Locale: Boolean);
    destructor Destroy; override;

    procedure Open;
    procedure Close;

    function  ExecSQL(const Command: String): Integer;
    procedure StartTransaction;
    procedure Commit;
    procedure Rollback;

    procedure GetOdbcDriverList(List: TStrings);
    function  GetOdbcDriverExt(const Driver: String): String;
    function  GetOdbcDriverFilter(const Driver: String): String;
    procedure GetOdbcDSNList(List: TStrings);
    procedure GetADOProviderList(List: TStrings); overload;
    procedure GetADOProviderList(Name, Code: TStrings); overload;
    procedure GetADOTableNames(List: TStrings); overload;
    procedure GetADOFieldNames(const TableName: String; List: TStrings); overload;

    // Load connection string from file
    function  LoadUDLFile(const FileName: String; UserName: String = ''; Password: String = ''): String;
    procedure UDLEditor(const FileName: String);

    property ADO               : _Connection     read FADOConnection;
    property Connection        : String          read FConnect     write SetConnect;
    property Active            : Boolean         read FActive      write SetActive;
    property CommandTimeOut    : Integer         read FCommTimeOut write FCommTimeOut;
    property ConnectionTimeOut : Integer         read FConnTimeOut write FConnTimeOut;
    property IsolationLevel    : TIsolationLevel read FIsolation   write FIsolation default ilXactReadCommitted;
    property Mode              : TConnectionMode read FMode        write FMode      default cmModeReadWrite;
    property CursorLocation    : TCursorLocation read FCursor      write FCursor    default clClient;
    property Version           : String          read GetVersion;
    property IsLocale          : Boolean         read FLocale;
    property DefaultTable      : String          read FDefTable    write FDefTable;
    property HaveTransaction   : Boolean         read FHasTran;
    property Information       : TStrings        read FInfo;
    property MachineName       : String          read FMachine     write FMachine;

  published
    { Published declarations }
  end;

{******************************************************************************}

   TRecordStatus  = (rsRecOK,
                     rsRecNew,
                     rsRecModified,
                     rsRecDeleted,
                     rsRecUnmodified,
                     rsRecInvalid,
                     rsRecMultipleChanges,
                     rsRecPendingChanges,
                     rsRecCanceled,
                     rsRecCantRelease,
                     rsRecConcurrencyViolation,
                     rsRecIntegrityViolation,
                     rsRecMaxChangesExceeded,
                     rsRecObjectOpen,
                     rsRecOutOfMemory,
                     rsRecPermissionDenied,
                     rsRecSchemaViolation,
                     rsRecDBDeleted);

  TDRecords = class(TObject)
  private
    { Private declarations }
    FConnection     : TDConnection;
    FADORecords     : _RecordSet;
    FCommandOption  : TCommandOption;
    FCursorType     : TCursorType;
    FLockType       : TLockType;
    FCommand        : String;
    FPosition       : TStringList;
    FBlobName       : TStringList;
    FHasRec         : Boolean;
    FActive         : Boolean;
    FState          : TDataSetState;
    FRecBuf         : Variant;
    FRecSize        : Integer;
    FTransaction    : Boolean;
    FMaxRecords     : Integer;

    // Handles properties
    procedure SetActive(Value: Boolean);
    function  GetSort: String;
    function  GetEof: Boolean;
    function  GetBof: Boolean;
    procedure SetSort(const Value: String);
    function  GetFilter: String;
    procedure SetFilter(const Value: String);
    function  GetRecordCount: Integer;
    function  GetRecNo: Integer;
    function  GetMaxRec: Integer;
    procedure SetMaxRec(Value: Integer);
    function  GetBookmark: Integer;
    procedure SetBookmark(Value: Integer);
    function  GetOkRec: Boolean;
    function  GetNoRec: Boolean;
    procedure SetState(Value: TDataSetState);
    function  GetMachine: String;
    procedure SetMachine(const Machine: String);

    // Utilites
    function  GetOpenOption: CommandTypeEnum;
    function  GetCursorType: CursorTypeEnum;
    function  GetLockType: LockTypeEnum;
    function  GetRecStatus: TRecordStatus;

  protected
    { Protected declarations }
    procedure InternalOpen; virtual;
    procedure InternalClose; virtual;

    procedure StartTransaction();
    procedure EndTransaction(const Commit: Boolean);

  public
    { Public declarations }
    constructor Create(Connection: TDConnection);
    destructor Destroy; override;

    procedure Open;
    procedure Close;

    // Edit methods
    procedure Insert;
    procedure Delete;
    procedure Cancel;
    procedure Update;

    // Navigational methods
    procedure First;
    procedure Last;
    procedure Next;
    procedure Prior;

    // Search
    function Find(const Search: String): Boolean;
    function FilterFind(const Search: String): Integer;

    // Utilites
    procedure LoadRecord(Buffer: PChar; EmptyRec: Boolean = False);
    function  FieldPosition(const FieldName: String): Integer;
    function  FieldOldValue(const FieldName: String): Variant;
    function  CalcRecSize(Fields: TFields): Integer;

  published
    { Published declarations }
    property Active         : Boolean               read FActive         write SetActive;
    property Connection     : TDConnection          read FConnection;
    property Command        : String                read FCommand        write FCommand;
    property ADO            : _RecordSet            read FADORecords;
    property CommandOption  : TCommandOption        read FCommandOption  write FCommandOption  default coText;
    property CursorType     : TCursorType           read FCursorType     write FCursorType     default ctOpenDynamic;
    property LockType       : TLockType             read FLockType       write FLockType       default ltLockOptimistic;
    property EOF            : Boolean               read GetEof;
    property BOF            : Boolean               read GetBof;
    property Sort           : String                read GetSort         write SetSort;
    property Filter         : String                read GetFilter       write SetFilter;
    property RecordCount    : Integer               read GetRecordCount;
    property RecNo          : Integer               read GetRecNo;
    property RecSize        : Integer               read FRecSize;
    property MaxRecords     : Integer               read GetMaxRec       write SetMaxRec;
    property State          : TDataSetState         read FState          write SetState;
    property HaveRecords    : Boolean               read FHasRec;
    property Bookmark       : Integer               read GetBookmark     write SetBookmark;
    property OkRecord       : Boolean               read GetOkRec;
    property AdoEofBof      : Boolean               read GetNoRec;
    property RecordStatus   : TRecordStatus         read GetRecStatus;
    property MachineName    : String                read GetMachine      write SetMachine;
    property Transaction    : Boolean               read FTransaction    write FTransaction default False;

  end;


implementation


{ TDConnection }

constructor TDConnection.Create(Locale: Boolean);
begin
     inherited Create;
     FConnect       := '';
     FCommTimeOut   := 30;
     FConnTimeOut   := 15;
     FIsolation     := ilXactReadCommitted;
     FMode          := cmModeReadWrite;
     FLocale        := Locale;
     FHasTran       := False;
     FActive        := False;
     FInfo          := TStringList.Create;
end;


destructor TDConnection.Destroy;
begin
     if FActive then InternalClose;
     FInfo.Free;
     FInfo := nil;
     inherited Destroy;
end;


procedure TDConnection.InternalOpen;
var
   i     : Integer;
   sName : String;
   sValue: String;
begin
     if FConnect = '' then Exit;
     if not Assigned(FADOConnection) then
     begin
          if FMachine = ''
          then
              FADOConnection := CoConnection.Create
          else
              FADOConnection := CoConnection.CreateRemote(FMachine);
     end;
     if not Assigned(FADOConnection) then Exit;
     if (FADOConnection.State = adStateClosed) then
     begin
          FADOConnection.CommandTimeout    := FCommTimeOut;
          FADOConnection.ConnectionTimeout := FConnTimeOut;
          FADOConnection.IsolationLevel    := GetIsolationLevel(FIsolation);
          FADOConnection.Mode              := GetConnectMode(FMode);
          case FCursor of
               clClient : FADOConnection.CursorLocation := adUseClient;
               clServer : FADOConnection.CursorLocation := adUseServer;
          end;
          FADOConnection.Open(WideString(FConnect), '', '', 0);
          FActive  := (FADOConnection.State <> adStateClosed);
          FHasTran := False;
          FInfo.Clear;
          for i := 0 to FADOConnection.Properties.Count-1 do
          begin
               sName  := FADOConnection.Properties.Item[i].Name;
               sValue := VarToStr(FADOConnection.Properties.Item[i].Value);
               if sName = cnsTransaction then FHasTran := (sValue <> '0');
               FInfo.Add(sName + '=' + sValue);
          end;
     end;
end;


procedure TDConnection.InternalClose;
begin
     if FActive then
     begin
          FADOConnection.Close;
          FADOConnection := nil;
          FActive  := False;
          FHasTran := False;
     end;
end;


procedure TDConnection.SetActive(Value: Boolean);
begin
     if FActive <> Value then
     begin
          if Value then InternalOpen else InternalClose;
     end;
end;


procedure TDConnection.SetConnect(const Value: String);
begin
     if FConnect <> Value then
     begin
          FConnect := Value;
          if FActive then
          begin
               InternalClose;
               InternalOpen;
          end;
     end;
end;


function TDConnection.GetVersion: String;
begin
     Result := 'Unknown';
     if Assigned(FADOConnection) then Result := FADOConnection.Version;
end;


procedure TDConnection.Open;
begin
     if not FActive then InternalOpen;
end;


procedure TDConnection.Close;
begin
     if FActive then InternalClose;
end;


function TDConnection.ExecSQL(const Command: String): Integer;
var
   vRec : OleVariant;
begin
     vRec := Null;
     if Assigned(FADOConnection) then
     begin
          FADOConnection.Execute(WideString(Command), vRec, adCmdText);
          Result := vRec;
     end
     else raise Exception.Create('Connection has not been established.');
end;


procedure TDConnection.StartTransaction;
begin
     if FHasTran then FADOConnection.BeginTrans;
end;


procedure TDConnection.Commit;
begin
     if FHasTran then FADOConnection.CommitTrans;
end;


procedure TDConnection.Rollback;
begin
     if FHasTran then FADOConnection.RollbackTrans;
end;


function TDConnection.GetIsolationLevel(Level: TIsolationLevel): IsolationLevelEnum;
begin
     Result := adXactChaos;
     case Level of
          ilXactChaos           : Result := adXactChaos;
          ilXactUnspecified     : Result := adXactChaos; // adXactUnspecified;
          ilXactBrowse          : Result := adXactBrowse;
          ilXactReadUncommitted : Result := adXactReadUncommitted;
          ilXactCursorStability : Result := adXactCursorStability;
          ilXactReadCommitted   : Result := adXactReadCommitted;
          ilXactRepeatableRead  : Result := adXactRepeatableRead;
          ilXactIsolated        : Result := adXactIsolated;
          ilXactSerializable    : Result := adXactSerializable;
     end;
end;


function TDConnection.GetConnectMode(Mode: TConnectionMode): ConnectModeEnum;
begin
     Result := adModeUnknown;
     case Mode of
          cmModeUnknown        : Result := adModeUnknown;
          cmModeRead           : Result := adModeRead;
          cmModeWrite          : Result := adModeWrite;
          cmModeReadWrite      : Result := adModeReadWrite;
          cmModeShareDenyRead  : Result := adModeShareDenyRead;
          cmModeShareDenyWrite : Result := adModeShareDenyWrite;
          cmModeShareExclusive : Result := adModeShareExclusive;
          cmModeShareDenyNone  : Result := adModeShareDenyNone;
     end;
end;


procedure TDConnection.GetOdbcDriverList(List: TStrings);
var
   oReg : TRegistry;
begin
     oReg := TRegistry.Create;
     try
     begin
          oReg.RootKey := HKEY_LOCAL_MACHINE;
          if oReg.OpenKeyReadOnly('SOFTWARE\ODBC\ODBCINST.INI\ODBC Drivers') then
          begin
               List.Clear;
               oReg.GetValueNames(List);
          end;
     end;
     finally
          oReg.Free;
     end;
end;


function TDConnection.GetOdbcDriverExt(const Driver: String): String;
var
   oReg : TRegistry;
begin
     Result := '';
     oReg := TRegistry.Create;
     try
     begin
          oReg.RootKey := HKEY_LOCAL_MACHINE;
          if oReg.OpenKeyReadOnly('SOFTWARE\ODBC\ODBCINST.INI\' + Driver) then
          begin
               Result := oReg.ReadString('FileExtns');
          end;
     end;
     finally
          oReg.Free;
     end;
end;


function TDConnection.GetOdbcDriverFilter(const Driver: String): String;
var
   oTmp : TStringList;
   i    : Integer;
   sExt : String;
   iPos : Integer;
begin
     Result := '';
     oTmp := TStringList.Create;
     try
     begin
          oTmp.CommaText := GetOdbcDriverExt(Driver);
          for i := 0 to oTmp.Count-1 do
          begin
               iPos := Pos('*.', Driver);
               if iPos > 0 then sExt := Copy(Driver, iPos, 5) else sExt := '';
               Result := Result + StrTran(Driver, sExt, oTmp.Strings[i]) + '|' + oTmp.Strings[i] + '|';
          end;
     end;
     finally
          oTmp.Free;
     end;
     Result := Result + ' All Files (*.*)  |*.*';
end;


procedure TDConnection.GetOdbcDSNList(List: TStrings);
var
   oReg : TRegistry;
   oTmp : TStringList;
begin
     oReg := TRegistry.Create;
     try
     begin
          oTmp := TStringList.Create;
          try
          begin
               List.Clear;
               oReg.RootKey := HKEY_LOCAL_MACHINE;
               if oReg.OpenKeyReadOnly('SOFTWARE\ODBC\ODBC.INI\ODBC Data Sources') then
               begin
                    oReg.GetValueNames(List);
               end;
               oReg.RootKey := HKEY_CURRENT_USER;
               if oReg.OpenKeyReadOnly('SOFTWARE\ODBC\ODBC.INI\ODBC Data Sources') then
               begin
                    oReg.GetValueNames(oTmp);
                    List.AddStrings(oTmp);
               end;
          end;
          finally
               oTmp.Free;
          end;
     end;
     finally
          oReg.Free;
     end;
end;


procedure TDConnection.GetADOProviderList(List: TStrings);
begin
     List.Clear;
     List.Add('MSDASQL=ODBC databases (default)');
     List.Add('SQLOLEDB=Microsoft SQL Server 7. OLE DB');
     List.Add('MSDAORA=Oracle databases');
     List.Add('Microsoft.Jet.OLEDB.4.0=Microsoft Jet databases');
     List.Add('MSIDXS=Microsoft Index Server');
     List.Add('ADSDSOObject=Microsoft Active Directory Service');
end;


procedure TDConnection.GetADOProviderList(Name, Code: TStrings);
var
   oTmp : TStringList;
   i    : Integer;
begin
     oTmp := TStringList.Create;
     try
     begin
          GetADOProviderList(oTmp);
          Name.Clear;
          Code.Clear;
          for i := 0 to oTmp.Count-1 do
          begin
               Name.Add(oTmp.Values[oTmp.Names[i]]);
               Code.Add(oTmp.Names[i]);
          end;
     end;
     finally
          oTmp.Free;
     end;
end;


procedure TDConnection.GetADOTableNames(List: TStrings);
var
   oRec : _RecordSet;
   bAct : Boolean;
begin
     bAct := FActive;
     if not FActive then Open;
     List.Clear;
     oRec := FADOConnection.OpenSchema(adSchemaTables, EmptyParam, EmptyParam);
     while not oRec.EOF do
     begin
          if VarToStr(oRec.Fields['TABLE_TYPE'].Value) = 'TABLE' then List.Add(VarToStr(oRec.Fields['TABLE_NAME'].Value));
          oRec.MoveNext;
     end;
     oRec.Close;
     if not bAct then Close;
end;


procedure TDConnection.GetADOFieldNames(const TableName: String; List: TStrings);
var
   oRec : _RecordSet;
   bAct : Boolean;
   i    : Integer;
begin
     bAct := FActive;
     if not FActive then Open;
     List.Clear;
     oRec := CoRecordset.Create;
     oRec.MaxRecords := 1;
     oRec.Open(TableName, FADOConnection, adOpenForwardOnly, adLockReadOnly, adCmdTable);
     for i := 0 to oRec.Fields.Count-1 do List.Add(oRec.Fields.Item[i].Name);
     oRec.Close();
     oRec := nil;
     if not bAct then Close;
end;


function TDConnection.LoadUDLFile(const FileName: String; UserName: String = ''; Password: String = ''): String;
var
  hFile : THandle;
  aBuff : Array[0..4096] of Char;
  iPos  : Integer;
  iLen  : Integer;
  sTmp  : String;
  i     : Integer;
  slTmp : TStringList;

const
  cnsProvider = 'Provider';
  cnsPassword = 'Password';
  cnsUserID   = 'User ID';
  cnsSecurity = 'Integrated Security';

begin
     hFile := FileOpen(FileName, fmOpenRead or fmShareDenyNone);
     if hFile > 0 then
     begin
          iLen := FileRead(hFile, aBuff, 4096);
          sTmp := '';
          for i := 2 to iLen do
          begin
               if aBuff[i] <> #0 then sTmp := sTmp + aBuff[i];
          end;
          iPos := Pos(cnsProvider, sTmp);
          if iPos > 0 then sTmp := Copy(sTmp, iPos, Length(sTmp));
          iPos := Pos(#13, sTmp);
          if iPos > 0 then sTmp := Copy(sTmp, 1, iPos-1);
          slTmp := TStringList.Create;
          try
          begin
               StrToStrings(sTmp, slTmp, ';');
               if UserName <> '' then
               begin
                    iPos := slTmp.IndexOfName(cnsUserID);
                    if iPos > -1
                    then
                        slTmp.Strings[iPos] := cnsUserID + '=' + UserName
                    else
                        slTmp.Add(cnsUserID + '=' + UserName);

                    iPos := slTmp.IndexOfName(cnsPassword);
                    if iPos > -1
                    then
                        slTmp.Strings[iPos] := cnsPassword + '=' + Password
                    else
                        slTmp.Add(cnsPassword + '=' + Password);
               end;
               if (UserName <> '') and (Password <> '') then
               begin
                    iPos := slTmp.IndexOfName(cnsSecurity);
                    if iPos > -1 then slTmp.Delete(iPos);
               end;
               sTmp := StringsToStr(slTmp, ';');
               if sTmp[1] = ';' then sTmp := Copy(sTmp, 2, Length(sTmp));
          end;
          finally
               slTmp.Free;
          end;
     end;
     FileClose(hFile);
     Result := sTmp;
end;


procedure TDConnection.UDLEditor(const FileName: String);
begin
     if FileExists(FileName) then ShellExecute(Application.Handle, 'open', PChar(FileName), '', '', 1);
end;


{******************************************************************************}


{ TDRecords }

constructor TDRecords.Create(Connection: TDConnection);
begin
     inherited Create;
     FConnection     := Connection;
     FCommandOption  := coText;
     FCursorType     := ctOpenDynamic;
     FLockType       := ltLockOptimistic;
     FHasRec         := False;
     FPosition       := TStringList.Create;
     FBlobName       := TStringList.Create;
     FState          := dsInactive;
     FActive         := False;
     FRecBuf         := NULL;
     FRecSize        := 0;
     FTransaction    := False;
     FMaxRecords     := 0;

     // Create ADO Object
     FADORecords     := CoRecordset.Create;
end;


destructor TDRecords.Destroy;
begin
     if FActive then Close;
     FRecBuf := NULL;
     FADORecords := nil;
     FPosition.Free;
     FPosition := nil;
     FBlobName.Free;
     FBlobName := nil;
     inherited Destroy;
end;


procedure TDRecords.SetActive(Value: Boolean);
begin
     if Active <> Value then
     begin
          if Value then InternalOpen else InternalClose;
     end;
end;


procedure TDRecords.InternalOpen;
var
   i : Integer;
begin
     if Assigned(FConnection) and (FCommand  <> '') and not Active then
     begin
          FADORecords.MaxRecords := FMaxRecords;
          FADORecords.Open(WideString(FCommand),
                           FConnection.ADO,
                           GetCursorType,
                           GetLockType,
                           GetOpenOption);
          FActive := (FADORecords.State <> adStateClosed);
          FHasRec := (RecordCount > 0);
          if FHasRec then FADORecords.MovePrevious; // Move to BOF state
          FPosition.Clear;
          for i := 0 to FADORecords.Fields.Count-1 do
          begin
               FPosition.Add(FADORecords.Fields.Item[i].Name);
          end;
          State := dsBrowse;
     end;
end;


procedure TDRecords.InternalClose;
begin
     if State in [dsEdit, dsInsert] then Cancel;
     FActive := (FADORecords.State <> adStateClosed);
     if FActive then try FADORecords.Close except end;
     FActive := False;
     FRecBuf := NULL;
     State   := dsInactive;
     FHasRec := False;
end;


procedure TDRecords.StartTransaction();
begin
     if FTransaction then Connection.StartTransaction();
end;


procedure TDRecords.EndTransaction(const Commit: Boolean);
begin
     if FTransaction then
     begin
          if Commit
          then
              Connection.Commit()
          else
              Connection.Rollback();
     end;
end;


function TDRecords.GetRecNo: Integer;
begin
     Result := -1;
     if FActive then
     begin
          Result := FADORecords.AbsolutePosition;
          if Result <= 0 then Result := 1;
          if FADORecords.BOF then Result := 1;
          if FADORecords.EOF or (State = dsInsert) then Result := FADORecords.RecordCount + 1;
     end;
end;


function TDRecords.GetRecordCount: Integer;
begin
     Result := -1;
     if FActive then Result := FADORecords.RecordCount;
end;


function TDRecords.CalcRecSize(Fields: TFields): Integer;
var
   i    : Integer;
   oFld : TField;
   iSize: Integer;
begin
     FRecSize := 0;
     if FActive then
     begin
          FBlobName.Clear;
          for i := 0 to FADORecords.Fields.Count-1 do
          begin
               oFld  := Fields.FindField(FADORecords.Fields.Item[i].Name);
               iSize := FADORecords.Fields.Item[i].DefinedSize;
               if FADORecords.Fields.Item[i].Type_ in [adTinyInt,
                                                       adSmallInt,
                                                       adInteger,
                                                       adBigInt,
                                                       adUnsignedTinyInt,
                                                       adUnsignedSmallInt,
                                                       adUnsignedInt,
                                                       adUnsignedBigInt,
                                                       adSingle,
                                                       adDouble,
                                                       adCurrency,
                                                       adDecimal,
                                                       adNumeric] then
               begin
                    iSize := iSize * 8;
               end;

               if FADORecords.Fields.Item[i].Type_ in [adDate, adDBDate] then
               begin
                    iSize := 10;
               end;

               if Assigned(oFld) and (oFld is TBlobField) then
               begin
                    FBlobName.Add(oFld.FieldName);
                    iSize := 10;
               end;
               
               if iSize > 65000 then iSize := 65000;
               Inc(FRecSize, iSize + 1);
          end;
     end;
     Result := FRecSize;
end;


function TDRecords.GetMaxRec: Integer;
begin
     Result := FMaxRecords;
end;


procedure TDRecords.SetMaxRec(Value: Integer);
begin
     if Value < 0 then FMaxRecords := 0 else FMaxRecords := Value;
end;


function TDRecords.GetEof: Boolean;
var
   bBof : Boolean;
begin
     Result := True;
     if FActive then
     begin
          bBof := FADORecords.BOF;
          Result := FADORecords.EOF;
          if not Result then
          begin
               FADORecords.MoveNext();
               Result := FADORecords.EOF;
               if FHasRec then
               begin
                    if bBof then First else FADORecords.MovePrevious();
               end;
          end;
     end;
end;


function TDRecords.GetBof: Boolean;
var
   bEof : Boolean;
begin
     Result := True;
     if FActive then
     begin
          bEof := FADORecords.EOF;
          Result := FADORecords.BOF;
          if not Result then
          begin
               FADORecords.MovePrevious();
               Result := FADORecords.BOF;
               if FHasRec then
               begin
                    if bEof then Last else FADORecords.MoveNext();
               end;
          end;
     end;
end;


procedure TDRecords.Open;
begin
     InternalOpen;
end;


procedure TDRecords.Close;
begin
     InternalClose;
end;


procedure TDRecords.First;
begin
     if FHasRec then
     begin
          FADORecords.MoveFirst;
          FADORecords.MovePrevious;
     end;
end;


procedure TDRecords.Last;
begin
     if FHasRec then
     begin
          FADORecords.MoveLast;
          FADORecords.MoveNext;
     end;
end;


procedure TDRecords.Next;
begin
     if FHasRec then FADORecords.MoveNext;
end;


procedure TDRecords.Prior;
begin
     if FHasRec then FADORecords.MovePrevious;
end;


function TDRecords.Find(const Search: String): Boolean;
var
   iMark : Integer;
begin
     Result := False;
     if FActive then
     begin
          // Searching
          if Search <> '' then
          begin
               if FHasRec then
               begin
                    iMark := Bookmark;
                    FADORecords.MoveFirst;
                    FADORecords.Find(WideString(Search), 0, adSearchForward, EmptyParam);
                    if FADORecords.EOF
                    then
                        Bookmark := iMark
                    else
                        Result := True;
               end;
          end;
     end;
end;


function TDRecords.FilterFind(const Search: String): Integer;
var
   sOld : String;
   sTmp : String;
   vMark: OleVariant;
begin
     Result := 0;
     if FActive then
     begin
          // Searching
          if (Search <> '') and FHasRec then
          begin
               sOld := Filter;
               if sOld <> '' then sTmp := sOld + ' AND ' + Search else sTmp := Search;
               Filter := sTmp;
               if not Fadorecords.BOF and not Fadorecords.EOF then
               begin
                    vMark := FADORecords.Bookmark;
                    Result := Bookmark;
                    Filter := sOld;
                    FADORecords.Bookmark := vMark;
               end
               else Filter := sOld;
          end;
     end;
end;


function TDRecords.GetFilter: String;
begin
     Result := VarToStr(FADORecords.Filter);
     if Result = '0' then Result := '';
end;


procedure TDRecords.SetFilter(const Value: String);
begin
     FADORecords.Filter := WideString(Value);
     FHasRec := (RecordCount > 0);
end;


function TDRecords.GetSort: String;
begin
     Result := FADORecords.Sort;
end;


procedure TDRecords.SetSort(const Value: String);
begin
     FADORecords.Sort := WideString(Value);
end;


function TDRecords.GetMachine: String;
begin
     Result := '';
     if Assigned(FConnection) then Result := FConnection.MachineName;
end;


procedure TDRecords.SetMachine(const Machine: String);
begin
     if Assigned(FConnection) then FConnection.MachineName := Machine;
end;


function TDRecords.GetOpenOption: CommandTypeEnum;
begin
     Result := adCmdUnknown;
     case FCommandOption of
          coUnknown               : Result := adCmdUnknown;
          coText                  : Result := adCmdText;
          coTable                 : Result := adCmdTable;
          coStoredProc            : Result := adCmdStoredProc;
          coFile                  : Result := adCmdFile;
          coTableDirect           : Result := adCmdTableDirect;
          coAsyncExecute          : Result := adAsyncExecute;
          coAsyncFetch            : Result := adAsyncFetch;
          coAsyncFetchNonBlocking : Result := adAsyncFetchNonBlocking;
     end;
end;


function TDRecords.GetCursorType: CursorTypeEnum;
begin
     Result := adOpenStatic;
     case FCursorType of
          ctOpenForwardOnly : Result := adOpenForwardOnly;
          ctOpenKeyset      : Result := adOpenKeySet;
          ctOpenDynamic     : Result := adOpenDynamic;
          ctOpenStatic      : Result := adOpenStatic;
     end;
end;


function TDRecords.GetLockType: LockTypeEnum;
begin
     Result := adLockOptimistic;
     case FLockType of
          ltLockReadOnly        : Result := adLockReadOnly;
          ltLockPessimistic     : Result := adLockPessimistic;
          ltLockOptimistic      : Result := adLockOptimistic;
          ltLockBatchOptimistic : Result := adLockBatchOptimistic;
     end;
end;


function TDRecords.GetRecStatus: TRecordStatus;
begin
     Result := rsRecInvalid;
     if not FActive or AdoEofBof then Exit;
     case FADORecords.Status of
          // The record was successfully updated.
          adRecOK                   : Result := rsRecOk;
          // The record is new.
          adRecNew                  : Result := rsRecNew;
          // The record was modified.
          adRecModified             : Result := rsRecModified;
          // The record was deleted.
          adRecDeleted              : Result := rsRecDeleted;
          // The record was not modified.
          adRecUnmodified           : Result := rsRecUnmodified;
          // The record was not saved because its bookmark is invalid.
          adRecInvalid              : Result := rsRecInvalid;
          // The record was not saved because it would have affected multiple records.
          adRecMultipleChanges      : Result := rsRecMultipleChanges;
          // The record was not saved because it refers to a pending insert.
          adRecPendingChanges       : Result := rsRecPendingChanges;
          // The record was not saved because the operation was canceled.
          adRecCanceled             : Result := rsRecCanceled;
          // The new record was not saved because of existing record locks.
          adRecCantRelease          : Result := rsRecCantRelease;
          // The record was not saved because optimistic concurrency was in use.
          adRecConcurrencyViolation : Result := rsRecConcurrencyViolation;
          // The record was not saved because the user violated integrity constraints.
          adRecIntegrityViolation   : Result := rsRecIntegrityViolation;
          // The record was not saved because there were too many pending changes.
          adRecMaxChangesExceeded   : Result := rsRecMaxChangesExceeded;
          // The record was not saved because of a conflict with an open storage object.
          adRecObjectOpen           : Result := rsRecObjectOpen;
          // The record was not saved because the computer has run out of memory.
          adRecOutOfMemory          : Result := rsRecOutOfMemory;
          // The record was not saved because the user has insufficient permissions.
          adRecPermissionDenied     : Result := rsRecPermissionDenied;
          // The record was not saved because it violates the structure of the underlying database.
          adRecSchemaViolation      : Result := rsRecSchemaViolation;
          //The record has already been deleted from the data source.
          adRecDBDeleted            : Result := rsRecDBDeleted;
     end;
end;


procedure TDRecords.Delete;
begin
     if FActive and OkRecord then
      try
        FADORecords.Delete(adAffectCurrent);
      except
        if fADORecords.Status <> 0 then begin
          FADORecords.CancelBatch(adAffectAll);
          FADORecords.Resync(adAffectall,adResyncAllValues);
        end;
        raise
      end;
     FHasRec := (RecordCount > 0);
end;


procedure TDRecords.Insert;
begin
     if FActive then
     begin
          StartTransaction();
          State := dsInsert;
          FADORecords.AddNew(EmptyParam, EmptyParam);
     end;
end;


procedure TDRecords.Cancel;
begin
     if FActive then
     begin
          if LockType = ltLockBatchOptimistic
          then
              FADORecords.CancelUpdate()
          else
              FADORecords.Cancel();
          State := dsBrowse;
          EndTransaction(False);
     end;
end;


procedure TDRecords.Update;
begin
     if FActive and not AdoEofBof then
     begin
          try
             FADORecords.Update(EmptyParam, EmptyParam);
          except
             FADORecords.CancelUpdate();
             raise;
          end;
          State := dsBrowse;
          EndTransaction(True);
     end;
end;

procedure TDRecords.LoadRecord(Buffer: PChar; EmptyRec: Boolean = False);
var
   i      : Integer;
   oRec   : TStringList;
   sValue : String;
   xStamp : TTimeStamp;
   vValue : OleVariant;
begin
     oRec := TStringList.Create;
     try
     begin
          for i := 0 to FADORecords.Fields.Count-1 do
          begin
               if FBlobName.IndexOf(FADORecords.Fields.Item[i].Name) > -1 then oRec.Add('(Blob)') else
               begin
                    if EmptyRec or not OkRecord
                    then
                        sValue := ''
                    else
                    begin
                         vValue := FADORecords.Fields.Item[i].Value;
                         if VarType(vValue) = varNull then sValue := '' else
                         begin
                              if FADORecords.Fields.Item[i].Type_ in [adDate, adDBDate, adDBTimeStamp] then
                              begin
                                   xStamp := DateTimeToTimeStamp(VarToDateTime(vValue));
                                   if xStamp.Time > 0 then
                                   begin
                                        sValue := IntToStr(FADORecords.Fields.Item[i].DefinedSize);
                                        if sValue='16'
                                        then
                                            sValue := FormatDateTime('yyyy-mm-dd hh:nn:ss', VarToDateTime(vValue))
                                        else
                                            sValue := FormatDateTime('yyyy-mm-dd hh:nn:ss AM/PM', VarToDateTime(vValue));
                                   end
                                   else sValue := FormatDateTime('yyyy-mm-dd', VarToDateTime(vValue));
                              end
                              else sValue := VarToStr(vValue);
                         end;
                    end;
                    oRec.Add(sValue);
               end;
          end;
          StrLCopy(Buffer, PChar(oRec.CommaText), FRecSize);
     end;
     finally
          oRec.Free;
     end;
end;


function TDRecords.GetBookmark: Integer;
begin
     Result := 1;
     if FActive and OkRecord and not AdoEofBof then Result := FADORecords.Bookmark;
end;


procedure TDRecords.SetBookmark(Value: Integer);
var
   dPos : Double;
begin
     dPos := Value;
     if FActive and OkRecord and FHasRec and (dPos > 0) then FADORecords.Bookmark := dPos;
end;


function TDRecords.FieldPosition(const FieldName: String): Integer;
begin
     Result := FPosition.IndexOf(FieldName);
end;

function TDRecords.GetOkRec: Boolean;
begin
     Result := (FADORecords.AbsolutePosition > 0);
end;


function TDRecords.GetNoRec: Boolean;
begin
     Result := (FADORecords.EOF and FADORecords.BOF);
end;


procedure TDRecords.SetState(Value: TDataSetState);
var
   i : Integer;
begin
     if FState <> Value then
     begin
          FState := Value;
          if FActive then
          begin
               case FState of
                    dsEdit :
                    begin
                         FRecBuf := VarArrayCreate([0, FADORecords.Fields.Count], varVariant);
                         for i := 0 to FADORecords.Fields.Count-1 do
                         begin
                              FRecBuf[i] := FADORecords.Fields.Item[i].Value;
                         end;
                    end;
                    dsInsert :
                    begin
                         FRecBuf := VarArrayCreate([0, FADORecords.Fields.Count], varVariant);
                         for i := 0 to FADORecords.Fields.Count-1 do
                         begin
                              FRecBuf[i] := NULL;
                         end;
                    end;
               else FRecBuf := NULL;
               end;
          end;
     end;
end;


function TDRecords.FieldOldValue(const FieldName: String): Variant;
var
   iPos : Integer;
begin
     Result := NULL;
     if FActive then
     begin
          iPos := FieldPosition(FieldName);
          if (iPos > -1) and (VarType(FRecBuf) <> varNull) then Result := FRecBuf[iPos];
     end;
end;


end.
