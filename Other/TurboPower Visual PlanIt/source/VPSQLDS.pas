{*********************************************************}
{*                   VPSQLDS.PAS 1.03                    *}
{*********************************************************}

{* ***** BEGIN LICENSE BLOCK *****                                            *}
{* Version: MPL 1.1                                                           *}
{*                                                                            *}
{* The contents of this file are subject to the Mozilla Public License        *}
{* Version 1.1 (the "License"); you may not use this file except in           *}
{* compliance with the License. You may obtain a copy of the License at       *}
{* http://www.mozilla.org/MPL/                                                *}
{*                                                                            *}
{* Software distributed under the License is distributed on an "AS IS" basis, *}
{* WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License   *}
{* for the specific language governing rights and limitations under the       *}
{* License.                                                                   *}
{*                                                                            *}
{* The Original Code is TurboPower Visual PlanIt                              *}
{*                                                                            *}
{* The Initial Developer of the Original Code is TurboPower Software          *}
{*                                                                            *}
{* Portions created by TurboPower Software Inc. are Copyright (C) 2002        *}
{* TurboPower Software Inc. All Rights Reserved.                              *}
{*                                                                            *}
{* Contributor(s):                                                            *}
{*         Hannes Danzl                                                       *}   
{*                                                                            *}
{* ***** END LICENSE BLOCK *****                                              *}

{This unit was provided by Hannes Danzl and is used here with permission       }


// implements a Visual PlanIt datastore for SQL databases. uses interfaced
// tdataset descendants to create datasets of different dbengines and
// descendants of TBaseSQLDialect for being as independent from the sql
// dialect as possible
unit VPSQLDS;

interface

uses
  classes, VPSQLDialect, db, sysutils, vpDBDs, VPDbIntf, dialogs,
  vpConst, vpBase, vpData, vpBaseDS, vpException;

type

  // the datastore class; for easier extension and flexibilitiy, the datastore
  // handles every VPI table as an internal store; these stores are created
  // in the CreateStores method and linked into the fStores Stringlist. The
  // objects are TBaseSQLDialect descendants. Access from an app to this stores
  // is over the Stores property
  TVPSQLDataStore = class(TVpCustomDBDataStore)
  protected
    // internal list of stores and the according objects;
    // for every "dataset" an internal store of type TBaseSQLDialect is
    // created, it's DBEngine is assigned the correct value
    fStores: TStringlist;
    // see ConnectionParams
    fConnectionParams: TStrings;
    // see Session
    fSession: TComponent;
    // see SQLDialect
    fSQLDialect: String;
    // see DBEngine
    fDBEngine: String;
    // see Stores
    function GetStore(StoreName: String): TVpBaseSQLDialect;
    // see ConnectionParams
    procedure SetConnectionParams(const Value: TStrings); virtual;
    // see Session
    procedure SetSession(const Value: TComponent); virtual;
    // see SQLDialect
    procedure SetSQLDialect(const Value: String); virtual;
    // see DBEngine
    procedure SetDBEngine(const Value: String); virtual;
    // creates one store (internal use)
    function CreateStore(DBEngine: String): TVpBaseSQLDialect; virtual;
    // (should) create all stores
    procedure CreateStores; virtual;
    // frees all stores
    procedure FreeStores; virtual;
    // calls the TVpBaseSQLDialect.CreateTable method for the correct store
    procedure CreateTable(aTableName: String); virtual;
    // sets ConnectionParams and Session for all stores; typically called before
    // Connected is set to true
    procedure SetSessionAndParams; virtual;

    // returns the Dataset of the Resource store
    function GetResourceTable : TDataset; override;
    // returns the Dataset of the Events store
    function GetEventsTable : TDataset; override;
    // returns the Dataset of the Contacts store
    function GetContactsTable : TDataset; override;
    // returns the Dataset of the Tasks store
    function GetTasksTable : TDataset; override;

    // handles AutoConnect and AutoCreate properties
    procedure Loaded; override;
    // connects the datastore to the database
    procedure SetConnected(const Value: boolean);override;
  public
    // constructor
    constructor Create(aOwner:TComponent); override;
    // destructor
    destructor Destroy; override;

    // returns the next id for a store by doing an equivalent of select max(id) from table
    // and increasing the number by one
    function GetNextID(TableName: string): Integer; override;

    // post changes to the store
    procedure PostResources; override;
    // post changes to the store
    procedure PostEvents; override;
    // post changes to the store
    procedure PostContacts; override;
    // post changes to the store
    procedure PostTasks; override;

    // purge the given resource
    procedure PurgeResource(Res: TVpResource); override;
    // purge all items of the store belonging to the given resource
    procedure PurgeContacts(Res: TVpResource); override;
    // purge all items of the store belonging to the given resource
    procedure PurgeEvents(Res: TVpResource); override;
    // purge all items of the store belonging to the given resource
    procedure PurgeTasks(Res: TVpResource); override;

    // returns the named store
    property Stores[StoreName: String]: TVpBaseSQLDialect read GetStore;
  published
    // DBEninge to use; see swhDatabaseIntf.pas for more info
    property DBEngine: String read fDBEngine write SetDBEngine;
    // SQL Dialect to use; see swhSQLDialect.pas for more info
    property SQLDialect: String read fSQLDialect write SetSQLDialect;
    // optional connection parameters for creating the dataset or alternatively
    // use the Session property
    property ConnectionParams: TStrings read fConnectionParams write SetConnectionParams;
    // an untyped session that is passed through to the ISQLDataset; it's the
    // responsisbility of the dataset to handle it
    property Session: TComponent read fSession write SetSession;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Visual PlanIt', [TVPSQLDataStore]);
end;

{ TVPSQLDataStore }

procedure TVPSQLDataStore.CreateTable(aTableName: String);
var
  aDs: TDataset;
  aTable: TVpBaseSQLDialect;
  fDefs: TFieldDefs;
begin
  aDs:=TDataset.Create(nil);
  fDefs:=TFieldDefs.Create(ads);
  try
    CreateFieldDefs(aTableName, fDefs);
    assert(FDefs.Count>0);
    aTable:=Stores[aTableName];
    if aTable<>nil then
      aTable.CreateTable(aTableName, FDefs, nil);
  finally
    fDefs.Free;
    aDs.free;
  end;
end;

function TVPSQLDataStore.GetStore(StoreName: String): TVpBaseSQLDialect;
begin
  result := nil;
  if fStores.IndexOf(StoreName) > -1 then
    result := TVpBaseSQLDialect(fStores.Objects[fStores.IndexOf(StoreName)]);
end;

function TVPSQLDataStore.CreateStore(DBEngine: String): TVpBaseSQLDialect;
begin
  result:=TVpBaseSQLDialect(sSQLDialectFactory.CreateInstance(SQLDialect));
  Result.DBEngine:=DBEngine;
end;

procedure TVPSQLDataStore.SetDBEngine(const Value: String);
begin
  fDBEngine := Value;
end;

procedure TVPSQLDataStore.CreateStores;
var
  aStore: TVpBaseSQLDialect;
begin
  aStore := CreateStore(fDbEngine);
  fStores.AddObject(ResourceTableName, aStore);

  aStore := CreateStore(fDbEngine);
  fStores.AddObject(EventsTableName, aStore);

  aStore := CreateStore(fDbEngine);
  fStores.AddObject(ContactsTableName, aStore);

  aStore := CreateStore(fDbEngine);
  fStores.AddObject(TasksTableName, aStore);

  aStore := CreateStore(fDbEngine);
//  CreateFieldDefs('_Temp', aStore.Fields);
  fStores.AddObject('_Temp', aStore);
end;

constructor TVPSQLDataStore.Create(aOwner: TComponent);
begin
  inherited;
  fStores := TStringlist.Create;
  fConnectionParams := TStringlist.Create;
end;

destructor TVPSQLDataStore.Destroy;
begin
  FreeStores;
  fStores.free;
  fConnectionParams.free;
  inherited;
end;

procedure TVPSQLDataStore.FreeStores;
begin
  while fStores.Count > 0 do    // Iterate
  begin
    if fStores.Objects[0] <> nil then
      fStores.Objects[0].free;
    fStores.Delete(0);
  end;
end;

procedure TVPSQLDataStore.SetConnectionParams(const Value: TStrings);
begin
  fConnectionParams.Assign(Value);
  SetSessionAndParams;
end;

procedure TVPSQLDataStore.SetSessionAndParams;
var
  j: Integer;
begin
  for j:=0 to fStores.Count-1 do
  begin
    TVpBaseSQLDialect(fStores.Objects[j]).Session:=fSession;
    TVpBaseSQLDialect(fStores.Objects[j]).ConnectionParams:=fConnectionParams;
  end;
end;

procedure TVPSQLDataStore.SetSession(const Value: TComponent);
begin
  fSession := Value;
  SetSessionAndParams;
end;

procedure TVPSQLDataStore.SetSQLDialect(const Value: String);
begin
  fSQLDialect := Value;
end;

procedure TVPSQLDataStore.SetConnected(const Value: boolean);
var
  j: Integer;
  aStore: TVpBaseSQLDialect;
begin
  { Don't connect at designtime }
  if csDesigning in ComponentState then Exit;

  { Don't try to connect until we're all loaded up }
  if csLoading in ComponentState then Exit;

  FreeStores;
  CreateStores;
  SetSessionAndParams;
  
  try
    for j := 0 to fStores.Count-1 do    // Iterate
    begin
      if (fStores[j]<>'') and (fStores[j][1]<>'_') then
        try
          aStore:=Stores[fStores[j]];
          aStore.Close;
          aStore.SQL:=StringReplace(aStore.SelectSQL, '%TableName%', fStores[j], [rfIgnoreCase]);
          aStore.Open;
        except
          if AutoCreate then
          begin
            TVpBaseSQLDialect(fStores.Objects[j]).EnsureDatabaseExists;
            CreateTable(fStores[j]);
            aStore.SQL:=StringReplace(aStore.SelectSQL, '%TableName%', fStores[j], [rfIgnoreCase]);
            aStore.Open;
          end;
        end;
    end;    // for
    inherited;
    Load;
  except
    on e: exception do
      showmessage(e.message);
  end;
end;

procedure TVPSQLDataStore.Loaded;
begin
  inherited;
  if not (csDesigning in ComponentState) then
    Connected := AutoConnect;
end;

function TVPSQLDataStore.GetContactsTable: TDataset;
begin
  result:=Stores[ContactsTableName].Dataset;
end;

function TVPSQLDataStore.GetEventsTable: TDataset;
begin
  result:=Stores[EventsTableName].Dataset;
end;

function TVPSQLDataStore.GetResourceTable: TDataset;
begin
  result:=Stores[ResourceTableName].Dataset;
end;

function TVPSQLDataStore.GetTasksTable: TDataset;
begin
  result:=Stores[TasksTableName].Dataset;
end;

function TVPSQLDataStore.GetNextID(TableName: string): Integer;
var
  FldName : string;
begin
  try
    if TableName = ResourceTableName then
      FldName := 'ResourceID'
    else
      FldName := 'RecordID';

    Stores['_Temp'].Close;
    Stores['_Temp'].SQL := 'Select Max(' + FldName + ') as LastID from ' + TableName;

    Stores['_Temp'].Open;
    result := Stores['_Temp'].Dataset.FieldByName('LastID').AsInteger + 1;

    if result < 0 then
      result := 0;

  finally
    Stores['_Temp'].Close;
  end;
end;

{=====}

procedure TVPSQLDataStore.PostResources;
var
  TableName: String;
begin
  TableName:=ResourceTableName;
  Stores[TableName].SQL := StringReplace(
    Stores[TableName].SelectSQL, '%TableName%', TableName, [rfIgnoreCase]);
  Stores[TableName].Open;
  inherited;
end;
{=====}

procedure TVPSQLDataStore.PostEvents;
var
  TableName: String;
begin
  TableName:=EventsTableName;
  Stores[TableName].SQL := StringReplace(
    Stores[TableName].SelectSQL, '%TableName%', TableName, [rfIgnoreCase]);
  Stores[TableName].Open;
  inherited;
end;
{=====}

procedure TVPSQLDataStore.PostContacts;
var
  TableName: String;
begin
  TableName:=ContactsTableName;
  Stores[TableName].SQL := StringReplace(
    Stores[TableName].SelectSQL, '%TableName%', TableName, [rfIgnoreCase]);
  Stores[TableName].Open;
  inherited;
end;
{=====}

procedure TVPSQLDataStore.PostTasks;
var
  TableName: String;
begin
  TableName:=TasksTableName;
  Stores[TableName].SQL := StringReplace(
    Stores[TableName].SelectSQL, '%TableName%', TableName, [rfIgnoreCase]);
  Stores[TableName].Open;
  inherited;
end;
{=====}

procedure TVPSQLDataStore.PurgeResource(Res: TVpResource);
begin
  Res.Deleted := true;                                                
  PostResources;
  Load;
end;
{=====}

procedure TVPSQLDataStore.PurgeEvents(Res: TVpResource);
begin
  Stores[EventsTableName].sql := 'delete from ' + EventsTableName     
    + ' where ResourceID = ' + IntToStr(Res.ResourceID);              
  Stores[EventsTableName].ExecSQL;
  Res.Schedule.ClearEvents;                                           
end;
{=====}

procedure TVPSQLDataStore.PurgeContacts(Res: TVpResource);
begin
  Stores[ContactsTableName].sql := 'delete from ' + ContactsTableName 
    + ' where ResourceID = ' + IntToStr(Res.ResourceID);              
  Stores[ContactsTableName].ExecSQL;
  Res.Contacts.ClearContacts;
end;
{=====}

procedure TVPSQLDataStore.PurgeTasks(Res: TVpResource);
begin
  Stores[TasksTableName].sql := 'delete from ' + TasksTableName       
    + ' where ResourceID = ' + IntToStr(Res.ResourceID);              
  Stores[TasksTableName].ExecSQL;
  Res.Tasks.ClearTasks;                                               
end;
{=====}

end.
