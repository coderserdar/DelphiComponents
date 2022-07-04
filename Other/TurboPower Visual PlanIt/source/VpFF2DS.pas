{*********************************************************}
{*                   VPFF2DS.PAS 1.03                    *}
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
{*                                                                            *}
{* ***** END LICENSE BLOCK *****                                              *}

{$I Vp.INC}

unit VpFF2DS;
  { FlashFiler 2 DataStore component }

{ Supports FlashFiler 2.06 and above only. }

interface

uses
  Windows, Classes, Dialogs, SysUtils, Db, VpBase, VpData,
  VpBaseDS, VpDBDS, VpConst, ffdb;

type
  TVpFF2DataStore = class(TVpCustomDBDataStore)
  protected{private}
    FDatabase        : TffDatabase;
    FAutoCreateAlias : Boolean;
    FResourceTable   : TffTable;
    FEventsTable     : TffTable;
    FContactsTable   : TffTable;
    FTasksTable      : TffTable;
    FAliasName       : string;
    FSession         : TffSession;
    { property getters }
    function GetDatabaseName: string;
    function GetConnected: Boolean;

    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    { ancestor property getters }
    function GetResourceTable : TDataset; override;
    function GetEventsTable : TDataset; override;
    function GetContactsTable : TDataset; override;
    function GetTasksTable : TDataset; override;
    { property setters }
    procedure SetAutoCreateAlias(Value: Boolean);
    procedure SetAliasName(const Value: string);
    procedure SetConnected(const Value: boolean); override;
    procedure SetSession(Value : TffSession);
    procedure Loaded; override;

    procedure SetFilterCriteria(aTable         : TDataset;
                                aUseDateTime   : Boolean;
                                aResourceID    : Integer;
                                aStartDateTime : TDateTime;
                                aEndDateTime   : TDateTime); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function GetNextID(TableName: string): integer; override;

//    procedure PurgeResource(Res: TVpResource); override;            
    procedure PurgeEvents(Res: TVpResource); override;
    procedure PurgeContacts(Res: TVpResource); override;
    procedure PurgeTasks(Res: TVpResource); override;
    procedure Load; override;
    procedure CreateTable(TableName: string);
    property Database: TffDatabase read FDatabase;
  published
    property AutoCreate;
    property AutoConnect;
    { properties }
    property AutoCreateAlias: Boolean
      read FAutoCreateAlias write SetAutoCreateAlias;
    property DayBuffer;
    property ResourceID;
    property AliasName: string read FAliasName write SetAliasName;
    property Session : TffSession read FSession write SetSession;
    property ReadOnly;
    { events }
  end;

implementation

uses
{$IFDEF VERSION6}
  Variants;
{$ELSE}
  FileCtrl;
{$ENDIF}

(*****************************************************************************)
{ TVpFFDataStore }

constructor TVpFF2DataStore.Create(AOwner: TComponent);
begin
  inherited;

  FAliasName := '';
  FConnected := False;
  FResourceID := 0;
end;
{=====}

destructor TVpFF2DataStore.Destroy;
begin
  FResourceTable.Free;
  FEventsTable.Free;
  FContactsTable.Free;
  FTasksTable.Free;

  if Assigned(FDatabase) then
    FDatabase.Close;

  FDatabase.Free;

  inherited;
end;
{=====}

function TVpFF2DataStore.GetNextID(TableName: string): integer;
begin
  { this is not used in the FlashFiler Datastore as the FlashFiler tables use }
  { autoincrement fields }
  result := -1;
end;
{=====}

function TVpFF2DataStore.GetDatabaseName: string;
begin
  result := FDataBase.DatabaseName;
end;
{=====}

function TVpFF2DataStore.GetConnected: Boolean;
begin
  result := FDatabase.Connected;
end;
{=====}

function TVpFF2DataStore.GetResourceTable : TDataset;
begin
  Result := FResourceTable;
end;
{=====}

function TVpFF2DataStore.GetEventsTable : TDataset;
begin
  Result := FEventsTable;
end;
{=====}

function TVpFF2DataStore.GetContactsTable : TDataset;
begin
  Result := FContactsTable;
end;
{=====}

function TVpFF2DataStore.GetTasksTable : TDataset;
begin
  Result := FTasksTable;
end;
{=====}

procedure TVpFF2DataStore.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if Assigned(FSession) and (AComponent = FSession) then begin
    SetConnected(False);
    FSession := nil;
  end;
  inherited;
end;
{=====}

{ Functionality moved to the TVpCustomDBDataStore class }
(*
procedure TVpFF2DataStore.PurgeResource(Res: TVpResource);
begin
  Resource.Deleted := true;
  PostResources;
  Load;
end;
{=====}
*)

procedure TVpFF2DataStore.PurgeEvents(Res: TVpResource);
var
  OldIndex : string;
begin
  with FEventsTable do begin
    OldIndex := IndexName;
    IndexName := VpcIndexNameResID;
    SetRange([Res.ResourceID], [Res.ResourceID]);                     
    DeleteRecords;
    IndexName := OldIndex;
  end;
  inherited;                                                          
//  Resource.Schedule.ClearEvents;                                    
end;
{=====}

procedure TVpFF2DataStore.PurgeContacts(Res: TVpResource);
var
  OldIndex : string;
begin
  with FContactsTable do begin
    OldIndex := IndexName;
    IndexName := VpcIndexNameResID;
    SetRange([Res.ResourceID], [Res.ResourceID]);                     
    DeleteRecords;
    IndexName := OldIndex;
  end;
  inherited;                                                          
//  Resource.Contacts.ClearContacts;                                  
end;
{=====}

procedure TVpFF2DataStore.PurgeTasks(Res: TVpResource);
var
  OldIndex : string;
begin
  with FTasksTable do begin
    OldIndex := IndexName;
    IndexName := VpcIndexNameResID;
    SetRange([Res.ResourceID], [Res.ResourceID]);                     
    DeleteRecords;
    IndexName := OldIndex;
  end;
  inherited;                                                          
//  Resource.Tasks.ClearTasks;                                        
end;
{=====}

procedure TVpFF2DataStore.Load;
begin
  if not FDatabase.Connected then exit;

  inherited;
end;
{=====}

procedure TVpFF2DataStore.CreateTable(TableName: string);
var
  Table: TffTable;
begin
  Table := nil;
  if TableName = ResourceTableName then begin
    { Create Resources Table }
    Table := FResourceTable;
    with Table do begin
      Active := false;
      Name := ResourceTableName;
      DatabaseName := FDatabase.DatabaseName;
      CreateFieldDefs(TableName, FieldDefs);
      { modify field 0 to be an autoinc field }
      FieldDefs.Items[0].DataType := ftAutoInc;
      CreateIndexDefs(TableName, IndexDefs);
    end;
  end

  else if TableName = EventsTableName then begin
    { Create Events Table }
    Table := FEventsTable;
    with Table do begin
      Active := false;
      Name := EventsTableName;
      DatabaseName := FDatabase.DatabaseName;
      CreateFieldDefs(TableName, FieldDefs);
      { modify field 0 to be an autoinc field }
      FieldDefs.Items[0].DataType := ftAutoInc;
      CreateIndexDefs(TableName, IndexDefs);
    end;
  end

  else if TableName = ContactsTableName then begin
    { Create Contacts Table }
    Table := FContactsTable;
    with Table do begin
      Table.Active := false;
      Table.Name := ContactsTableName;
      Table.DatabaseName := FDatabase.DatabaseName;
      CreateFieldDefs(TableName, FieldDefs);
      { modify field 0 to be an autoinc field }
      FieldDefs.Items[0].DataType := ftAutoInc;
      CreateIndexDefs(TableName, IndexDefs);
    end;
  end

  else if TableName = TasksTableName then begin
    { Create Tasks Table }
    Table := FTasksTable;
    with Table do begin
      Table.Active := false;
      Table.Name := TasksTableName;
      Table.DatabaseName := FDatabase.DatabaseName;
      CreateFieldDefs(TableName, FieldDefs);
      { modify field 0 to be an autoinc field }
      FieldDefs.Items[0].DataType := ftAutoInc;
      CreateIndexDefs(TableName, IndexDefs);
    end;
  end;
  if Table <> nil then
    Table.CreateTable;
end;
{=====}

procedure TVpFF2DataStore.SetAliasName(const Value: string);
var
  WasOpen: Boolean;
begin
  if FAliasName <> Value then begin
    WasOpen := Connected;
    SetConnected(False);
    FAliasName := Value;
    SetConnected(WasOpen);
  end;
end;
{=====}

procedure TVpFF2DataStore.SetAutoCreateAlias(Value: Boolean);
begin
  if Value <> FAutoCreateAlias then
    FAutoCreateAlias := Value;
end;
{=====}

procedure TVpFF2DataStore.Loaded;
begin
  inherited;
  if not (csDesigning in ComponentState) then
    Connected := AutoConnect;
end;
{=====}

procedure TVpFF2DataStore.SetConnected(const Value: boolean);
var
  AliasPath: string;
  aSession : TffSession;
begin
  { Don't do anything with live data until run time. }
  if (csDesigning in ComponentState) or
     (csLoading in ComponentState) then
    Exit;

  { Connecting or disconnecting? }
  if Value then begin
    { Connecting. If a session was not explicitly specified then look for the
      default FlashFiler session. }
    if FSession = nil then
      aSession := FFSession
    else
      aSession := FSession;

    aSession.Open;
    if FDatabase = nil then begin
      FDatabase := TffDatabase.Create(nil);
    end;
    FDatabase.ReadOnly := ReadOnly;
    FDatabase.DatabaseName := 'VpDatabase';
    FDatabase.SessionName := aSession.SessionName;
    FDatabase.AliasName := FAliasName;

    if FAutoCreateAlias then begin
      { if there is no defined alias name then create one based on the }
      { application executable file's name and assign it to FDatabase  }
      if FAliasName = '' then begin
        FAliasName := ExtractFileName(ParamStr(0));
        FAliasName := Copy(FAliasName, 1, Pos('.', FAliasName) - 1);
        FDatabase.AliasName := FAliasName;
      end;

      { if the alias doesn't exist, then create it }
      if not aSession.IsAlias(FDatabase.AliasName) then begin
        AliasPath := ExtractFilePath(ParamStr(0)) + 'Data';
        if not DirectoryExists(AliasPath) then
          ForceDirectories(AliasPath);

        aSession.AddAliasEx(FDatabase.AliasName, AliasPath);
      end;
    end else
      if not aSession.IsAlias(FDatabase.AliasName) then Exit;          

    FDataBase.Connected := Value;
    if FDataBase.Connected then begin

      { Set up the tables. }
      if FResourceTable = nil then
        FResourceTable := TffTable.Create(nil);
      FResourceTable.DatabaseName := FDatabase.DatabaseName;
      FResourceTable.SessionName  := aSession.SessionName;
      FResourceTable.TableName    := ResourceTableName;

      if FEventsTable = nil then
        FEventsTable := TffTable.Create(nil);
      FEventsTable.DatabaseName   := FDatabase.DatabaseName;
      FEventsTable.SessionName    := aSession.SessionName;
      FEventsTable.TableName      := EventsTableName;

      if FContactsTable = nil then
        FContactsTable := TffTable.Create(nil);
      FContactsTable.DatabaseName := FDatabase.DatabaseName;
      FContactsTable.SessionName  := aSession.SessionName;
      FContactsTable.TableName    := ContactsTableName;

      if FTasksTable = nil then
        FTasksTable := TffTable.Create(nil);
      FTasksTable.DatabaseName    := FDatabase.DatabaseName;
      FTasksTable.SessionName     := aSession.SessionName;
      FTasksTable.TableName       := TasksTableName;

      if FDatabase.TableExists(ResourceTableName) then
        FResourceTable.Open
      else
        if AutoCreate then begin
          CreateTable(ResourceTableName);
          FResourceTable.Open;
        end;

      if FDatabase.TableExists(EventsTableName) then
        FEventsTable.Open
      else
        if AutoCreate then begin
          CreateTable(EventsTableName);
          FEventsTable.Open;
        end;

      if FDatabase.TableExists(ContactsTableName) then
        FContactsTable.Open
      else
        if AutoCreate then begin
          CreateTable(ContactsTableName);
          FContactsTable.Open;
        end;

      if FDatabase.TableExists(TasksTableName) then
        FTasksTable.Open
      else
        if AutoCreate then begin
          CreateTable(TasksTableName);
          FTasksTable.Open;
        end;

      Load;
    end
  end
  else if Assigned(FDatabase) then
    FDatabase.Close;

  if Assigned(FDatabase) then                                         
    inherited SetConnected(Database.Connected);
end;
{=====}

procedure TVpFF2DataStore.SetSession(Value : TffSession);
var
  WasOpen : Boolean;
begin
  if FSession <> Value then begin
    WasOpen := Connected;
    SetConnected(False);
    FSession := Value;
    if Assigned(FSession) then begin
      FSession.FreeNotification(Self);
      if Assigned(FDatabase) then begin
        FDatabase.SessionName := FSession.SessionName;
        FContactsTable.SessionName := FSession.SessionName;
        FEventsTable.SessionName := FSession.SessionName;
        FResourceTable.SessionName := FSession.SessionName;
        FTasksTable.SessionName := FSession.SessionName;;
      end;
    end;
    SetConnected(WasOpen);
  end;
end;
{=====}

{ Called by the ancestor to properly filter the data for each table, }
{ based on the ResourceID, Date and DayBuffer values.                }
{ Each TVpCustomDBDataStore descendant should define their own       }
{ SetFilterCriteria procedure.                                       }
procedure TVpFF2DataStore.SetFilterCriteria(aTable: TDataset;
  aUseDateTime: Boolean; aResourceID: Integer; aStartDateTime,
  aEndDateTime: TDateTime);
var
  aIndexDef : TIndexDef;
  aFFTable : TffTable;
begin
  if aUseDateTime then begin
    { - Added the ability to use ranges instead of filters for tables     }
    { which have a single index on ResourceID;StartTime                        }
    { tables which were created with versions 1.0 or 1.01 of Visual PlanIt     }
    { will not have the new index, so they will continue to use filters.       }
    aIndexDef := TffTable(aTable).IndexDefs.GetIndexForFields(           
      'ResourceID;StartTime', True);                                     
    if aIndexDef = nil then                                              
      inherited SetFilterCriteria(aTable, aUseDateTime, aResourceID,     
                                aStartDateTime, aEndDateTime)            
    else begin                                                           
      aFFTable := TffTable(aTable);                                      
      aFFTable.IndexName := aIndexDef.Name;                              
      aFFTable.SetRange([aResourceID, aStartDateTime],                   
        [aResourceID, aEndDateTime]);                                    
    end;                                                                 
  end else begin                                                         
    aIndexDef := TffTable(aTable).IndexDefs.GetIndexForFields('ResourceID', True);
    if aIndexDef = nil then
      inherited SetFilterCriteria(aTable, aUseDateTime, aResourceID,
                                  aStartDateTime, aEndDateTime)
    else begin
      aFFTable := TffTable(aTable);
      aFFTable.IndexName := aIndexDef.Name;
      aFFTable.SetRange([aResourceID], [aResourceID]);
    end;
  end;

end;

end.
