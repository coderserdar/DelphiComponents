unit DMaster;

interface     

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  DB, DUtils, ADOBase, DEditors;

type
  TSourceType  = (stUnknown,
                  stAccess,
                  stDbase,
                  stExcel,
                  stFoxPro,
                  stParadox,
                  stText,
                  stVisualFoxPro,
                  stOracle,
                  stMsSQL,
                  stSybase,
                  stInformix,
                  stIngres,
                  stDB2);
  TConnectMode = (cmNone, cmSystem, cmFile, cmServer);
  TOdbcDialogFilter = function (const DriverName: String): String of object;

  TDMaster = class(TComponent)
  private
    { Private declarations }
    FConnect    : String;
    FSourceType : TSourceType;
    FDataSets   : TList;
    FConnection : TDConnection;
    FMachine    : String;

    // Handle Properties
    function  GetConnected: Boolean;
    procedure SetConnected(Value: Boolean);
    function  GetSourceType: TSourceType;
    function  GetData: TDConnection;
    function  GetDataSet(Index: Integer): TDataSet;
    function  GetFileDSN: String;
    procedure SetFileDSN(const Value: String);
    function  GetSysDSN: String;
    procedure SetSysDSN(const Value: String);
    function  GetDriver: String;
    procedure SetDriver(const Value: String);
    function  GetServer: String;
    procedure SetServer(const Value: String);
    function  GetDatabase: String;
    procedure SetDatabase(const Value: String);
    function  GetUsername: String;
    procedure SetUsername(const Value: String);
    function  GetPassword: String;
    procedure SetPassword(const Value: String);
    function  GetProvider: String;
    procedure SetProvider(const Value: String);
    function  GetConnectMode: TConnectMode;
    procedure SetConnect(const Value: String);
    function  GetBaseDir: String;
    function  GetSystemDir: String;
    procedure SetMachine(const Value: String);

  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    procedure Assign(Source: TPersistent); override;
    function  Editor(StartPage: Byte = 0): String; virtual;

    // Child DataSets
    procedure InsertDataSet(DataSet: TDataSet);
    procedure RemoveDataSet(DataSet: TDataSet);

    // Load UDL file
    function LoadUDLFile(const FileName: String; UserName: String = ''; Password: String = ''): String;
    procedure UDLEditor(const FileName: String);

    property CentralData       : TDConnection         read GetData;
    property FileName          : String               read GetFileDSN      write SetFileDSN;
    property System            : String               read GetSysDSN       write SetSysDSN;
    property Driver            : String               read GetDriver       write SetDriver;
    property Server            : String               read GetServer       write SetServer;
    property Database          : String               read GetDatabase     write SetDatabase;
    property Password          : String               read GetPassword     write SetPassword;
    property Username          : String               read GetUsername     write SetUsername;
    property Provider          : String               read GetProvider     write SetProvider;
    property Mode              : TConnectMode         read GetConnectMode;
    property SystemDir         : String               read GetSystemDir;
    property BaseDir           : String               read GetBaseDir;
    property SourceType        : TSourceType          read GetSourceType;
    property DataSets[Index: Integer]: TDataSet       read GetDataSet;

  published
    { Published declarations }
    property Connection        : String               read FConnect        write SetConnect;
    property Connected         : Boolean              read GetConnected    write SetConnected;
    property MachineName       : String               read FMachine        write SetMachine;

  end;

implementation

uses DDB;

{$R *.dcr}

{******************************************************************************}
{***                         Central Component                              ***}
{******************************************************************************}


constructor TDMaster.Create(AOwner: TComponent);
begin
     inherited Create(AOwner);
     FDataSets   := TList.Create;
     FSourceType := stUnknown;
     FConnect    := '';
     FConnection := TDConnection.Create(False);
end;


destructor TDMaster.Destroy;
begin
     FDataSets.Free;
     FConnection.Free;
     FConnection := nil;
     inherited Destroy;
end;


procedure TDMaster.Assign(Source: TPersistent);
begin
     if Source is TDMaster then
     begin
          Connection := TDMaster(Source).Connection;
     end
     else inherited Assign(Source);
end;


procedure TDMaster.Notification(AComponent: TComponent; Operation: TOperation);
begin
     inherited Notification(AComponent, Operation);
     if (Operation = opRemove) and (AComponent is TDataSet) then RemoveDataSet(TDataSet(AComponent));
end;


{******************************************************************************}


function TDMaster.Editor(StartPage: Byte = 0): String;
var
   oEditor : TDConnectionEditor;
begin
     oEditor := TDConnectionEditor.Create(Self);
     try
     begin
          if StartPage = 1
          then
              oEditor.Connection := ''
          else
              oEditor.Connection := FConnect;
          if oEditor.Execute then
          begin
               Connection := oEditor.Connection;
               FConnection.DefaultTable := oEditor.DefaultTable;
          end
          else FConnection.DefaultTable := '';
     end;
     finally
          oEditor.Free;
     end;
     FConnection.Connection := FConnect;
     Result := FConnect;
end;


function TDMaster.GetSourceType: TSourceType;
var
   sTmp : String;
begin
     sTmp := AnsiUpperCase(FConnect);
     Result := stUnknown;
     if IsSub('ACCESS', sTmp)        then Result := stAccess;
     if IsSub('JET'   , sTmp)        then Result := stAccess;
     if IsSub('DBASE' , sTmp)        then Result := stDbase;
     if IsSub('EXCEL' , sTmp)        then Result := stExcel;
     if IsSub('FOXPRO', sTmp)        then Result := stFoxPro;
     if IsSub('PARADOX', sTmp)       then Result := stParadox;
     if IsSub('TEXT', sTmp)          then Result := stText;
     if IsSub('VISUAL FOXPRO', sTmp) then Result := stVisualFoxPro;
     if IsSub('ORACLE', sTmp)        then Result := stOracle;
     if IsSub('SQL Server', sTmp)    then Result := stMsSQL;
     if IsSub('SYBASE', sTmp)        then Result := stSybase;
     if IsSub('INFORMIX', sTmp)      then Result := stInformix;
     if IsSub('INGRES', sTmp)        then Result := stIngres;
     if IsSub('DB2', sTmp)           then Result := stDB2;
end;


function TDMaster.GetData: TDConnection;
begin
     if FConnection.Connection <> FConnect then FConnection.Connection := FConnect;
     Result := FConnection;
end;


function TDMaster.GetConnected: Boolean;
begin
     Result := FConnection.Active;
end;


procedure TDMaster.SetConnected(Value: Boolean);
var
   i : Integer;
begin
     if not Value then
     begin
          for i := 0 to FDataSets.Count-1 do
          begin
               try
                  TDataSet(FDataSets.Items[i]).Close;
               except
               end;
          end;
     end;
     if FConnection.Connection <> FConnect then FConnection.Connection := FConnect;
     FConnection.Active := Value;
end;


function TDMaster.GetDataSet(Index: Integer): TDataSet;
begin
     Result := nil;
     if (Index >= 0) and (Index < FDataSets.Count) then
     begin
          Result := TDataSet(FDataSets.Items[Index]);
     end;
end;


function TDMaster.GetFileDSN: String;
begin
     Result := GetADOPart(FConnect, cnsADOFileDSN);
end;


procedure TDMaster.SetFileDSN(const Value: String);
begin
     FConnect := SetADOPart(FConnect, cnsADOFileDSN, Value);
end;


function TDMaster.GetSysDSN: String;
begin
     Result := GetADOPart(FConnect, cnsADOSystemDSN);
end;


procedure TDMaster.SetSysDSN(const Value: String);
begin
     FConnect := SetADOPart(FConnect, cnsADOSystemDSN, Value);
end;


function TDMaster.GetDriver: String;
begin
     Result := GetADOPart(FConnect, cnsADODriver);
end;


procedure TDMaster.SetDriver(const Value: String);
begin
     FConnect := SetADOPart(FConnect, cnsADODriver, Value);
end;


function TDMaster.GetServer: String;
begin
     Result := GetADOPart(FConnect, cnsADOServer);
end;


procedure TDMaster.SetServer(const Value: String);
begin
     FConnect := SetADOPart(FConnect, cnsADOServer, Value);
end;


function TDMaster.GetDatabase: String;
begin
     Result := GetADOPart(FConnect, cnsADOServer);
end;


procedure TDMaster.SetDatabase(const Value: String);
begin
     FConnect := SetADOPart(FConnect, cnsADODatabase, Value);
end;


function TDMaster.GetUsername: String;
begin
     Result := GetADOPart(FConnect, cnsADOUsername);
end;


procedure TDMaster.SetUsername(const Value: String);
begin
     FConnect := SetADOPart(FConnect, cnsADOUsername, Value);
end;


function TDMaster.GetPassword: String;
begin
     Result := GetADOPart(FConnect, cnsADOPassword);
end;


procedure TDMaster.SetPassword(const Value: String);
begin
     FConnect := SetADOPart(FConnect, cnsADOPassword, Value);
end;


function TDMaster.GetProvider: String;
begin
     Result := GetADOPart(FConnect, cnsADOProvider);
end;


procedure TDMaster.SetProvider(const Value: String);
begin
     if Value = ''
     then
         FConnect := SetADOPart(FConnect, cnsADOProvider, cnsDefProvider)
     else
         FConnect := SetADOPart(FConnect, cnsADOProvider, Value);
end;


function TDMaster.GetConnectMode: TConnectMode;
begin
     Result := cmNone;
     if System <> '' then Result := cmSystem;
     if Driver <> '' then Result := cmFile;
     if Server <> '' then Result := cmServer;
end;


procedure TDMaster.SetConnect(const Value: String);
var
   i : Integer;
begin
     if FConnect <> Value then
     begin
          FConnect := Value;
          for i := 0 to FDataSets.Count-1 do
          begin
               if Assigned(FDataSets.Items[i]) then TDADODataSet(FDataSets.Items[i]).Connection := FConnect;
          end;
          FConnection.Close;
          FConnection.Connection := FConnect;
     end;
end;


function TDMaster.GetBaseDir: String;
begin
     Result := IIF((csDesigning in ComponentState), GetWorkDir, GetExeDir);
end;


function TDMaster.GetSystemDir: String;
begin
     Result := BaseDir + 'System\';
end;


procedure TDMaster.InsertDataSet(DataSet: TDataSet);
begin
     if FDataSets.IndexOf(DataSet) = -1 then FDataSets.Add(DataSet);
end;


procedure TDMaster.RemoveDataSet(DataSet: TDataSet);
var
   iPos : Integer;
begin
     iPos := FDataSets.IndexOf(DataSet);
     if iPos > -1 then FDataSets.Remove(DataSet);
end;


function TDMaster.LoadUDLFile(const FileName: String; UserName: String = ''; Password: String = ''): String;
begin
     Result := FConnection.LoadUDLFile(FileName, UserName, Password);
end;


procedure TDMaster.UDLEditor(const FileName: String);
begin
     FConnection.UDLEditor(FileName);
end;


procedure TDMaster.SetMachine(const Value: String);
begin
     if FMachine <> Value then
     begin
          FMachine := Value;
          FConnection.MachineName := FMachine;
     end;
end;


end.
