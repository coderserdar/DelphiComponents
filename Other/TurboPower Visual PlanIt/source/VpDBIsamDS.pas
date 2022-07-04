{*********************************************************}
{*                    VpDBIsamDS.PAS 1.03                *}
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
{*        Steve Forbes                                                        *}  
{*                                                                            *}
{* ***** END LICENSE BLOCK *****                                              *}

{ This unit was provided by Steve Forbes and is used with permission           }


{$I Vp.INC}
unit VpDBIsamDS;
{ DBISAM DataStore Component }

interface

uses
  Windows, Classes, Dialogs, SysUtils, Db, DBISAMTb, DBISAMLb,
  VpBase, VpData, VpSR, VpBaseDS, VpDBDS, VpException;

type
  TVpDBISAMDataStore = class(TVpCustomDBDataStore)
  protected
    FDatabase        : TDBISAMDatabase;
    FResourceTable   : TDBISAMQuery;
    FEventsTable     : TDBISAMQuery;
    FContactsTable   : TDBISAMQuery;
    FTasksTable      : TDBISAMQuery;
    FRecordIDTable   : TDBISAMQuery;
    FSessionName     : string;

    { property getters }
    function GetDatabaseName: string;
    function GetConnected: Boolean;
    function GetDirectory: String;

    { ancestor property getters }
    function GetResourceTable : TDataset; override;
    function GetEventsTable : TDataset; override;
    function GetContactsTable : TDataset; override;
    function GetTasksTable : TDataset; override;

    { property setters }
    procedure SetConnected(const Value: boolean); override;
    procedure SetDirectory(const AValue: String);

    procedure Loaded; override;
    procedure SetFilterCriteria(aTable: TDataset; aUseDateTime: Boolean;
      aResourceID: Integer; aStartDateTime : TDateTime;
      aEndDateTime: TDateTime); override;
    procedure InitializeRecordIDTable;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function GetNextID(TableName: string): Integer; override;
    procedure Load; override;
    procedure CreateTable(TableName: string);
    procedure CreateIndexDefs(const TableName : string;
                                    IndexDefs : TIndexDefs); override;
    procedure CreateFieldDefs(const TableName : string; FieldDefs : TFieldDefs);
        override;

    procedure PostResources; override;
    procedure PostEvents; override;
    procedure PostContacts; override;
    procedure PostTasks; override;

    procedure PurgeResource(Res: TVpResource); override;
    procedure PurgeEvents(Res: TVpResource); override;
    procedure PurgeContacts(Res: TVpResource); override;
    procedure PurgeTasks(Res: TVpResource); override;

    property Database: TDBISAMDatabase read FDatabase;

  published
    { properties }
    property DayBuffer;
    property ResourceID;
    property ReadOnly;
    property Directory: String read GetDirectory write SetDirectory;
    { events }
  end;

implementation

uses
{$IFDEF VERSION6}
  Variants,
{$ELSE}
  FileCtrl,
{$ENDIF}
  VpConst;

(*****************************************************************************)
{ TVpDBISAMDataStore }

constructor TVpDBISAMDataStore.Create(AOwner: TComponent);
begin
  FConnected := false;
  FSessionName := 'Default';
  FResourceID := 0;

  FDatabase := TDBISAMDatabase.Create(self);
  FDatabase.DatabaseName := '';
  FDatabase.Connected := false;
  FDatabase.SessionName := FSessionName;

  FResourceTable := TDBISAMQuery.Create(self);

  {$IFDEF VERSION5}
    FResourceTable.ReadOnly := FReadOnly;
  {$ENDIF}

  FResourceTable.RequestLive := True;
  FResourceTable.SQL.Text := 'SELECT * FROM Resources';
//  FResourceTable.DatabaseName := 'pig';   How funny!                   
//  ShowMessage(FResourceTable.Database.DatabaseName);

  FEventsTable := TDBISAMQuery.Create(self);

  {$IFDEF VERSION5}
    FEventsTable.ReadOnly := FReadOnly;
  {$ENDIF}

//  FEventsTable.DatabaseName := FDatabase.Name;
  FEventsTable.RequestLive := True;
  FEventsTable.SQL.Text := 'SELECT * FROM Events '
  + 'WHERE (ResourceID = :ResID) '
  + 'AND ((StartTime >= :STime1 AND EndTime <= :ETime) '                 
  + 'OR (RepeatCode > 0 AND :STime2 <= RepeatRangeEnd))';                

  FContactsTable := TDBISAMQuery.Create(self);

  {$IFDEF VERSION5}
    FContactsTable.ReadOnly := FReadOnly;
  {$ENDIF}

//  FContactsTable.DatabaseName := FDatabase.Name;
  FContactsTable.RequestLive := True;
  FContactsTable.SQL.Text := 'SELECT * FROM Contacts '
  + 'WHERE ResourceID = :ResID';

  FTasksTable := TDBISAMQuery.Create(self);

  {$IFDEF VERSION5}
    FTasksTable.ReadOnly := FReadOnly;
  {$ENDIF}

//  FTasksTable.DatabaseName := FDatabase.Name;
  FTasksTable.RequestLive := True;
  FTasksTable.SQL.Text := 'SELECT * FROM Tasks '
  + 'WHERE ResourceID = :ResID';

  FRecordIDTable := TDBISAMQuery.Create(self);

  {$IFDEF VERSION5}
    FRecordIDTable.ReadOnly := FReadOnly;
  {$ENDIF}

  FRecordIDTable.DatabaseName := FDatabase.Name;
  FRecordIDTable.RequestLive := True;

  inherited;
end;
{=====}

destructor TVpDBISAMDataStore.Destroy;
begin
  { free tables }
  FDatabase.Close;
  FDatabase.Free;
  FResourceTable.Free;
  FEventsTable.Free;
  FContactsTable.Free;
  FTasksTable.Free;
  FRecordIDTable.Free;

  inherited;
end;
{=====}

function TVpDBISAMDataStore.GetDatabaseName: string;
begin
  result := FDataBase.DatabaseName;
end;
{=====}

function TVpDBISAMDataStore.GetConnected: Boolean;
begin
  result := FDatabase.Connected;
end;
{=====}

function TVpDBISAMDataStore.GetResourceTable : TDataset;
begin
  Result := FResourceTable;
end;
{=====}

function TVpDBISAMDataStore.GetEventsTable : TDataset;
begin
  Result := FEventsTable;
end;
{=====}

function TVpDBISAMDataStore.GetContactsTable : TDataset;
begin
  Result := FContactsTable;
end;
{=====}

function TVpDBISAMDataStore.GetTasksTable : TDataset;
begin
  Result := FTasksTable;
end;
{=====}

procedure TVpDBISAMDataStore.Load;
begin
  if not FDatabase.Connected then exit;

  FResourceTable.Close;
  FEventsTable.Close;
  FContactsTable.Close;
  FTasksTable.Close;

  inherited Load;
end;
{=====}

function TVpDBISAMDataStore.GetNextID(TableName: string): Integer;
var
  Query: TDBISAMQuery;
  GotIt: Boolean;
  ID   : Integer;
  FldName : string;
begin
  { The DBISAMDataStore uses a support table called RecordIDS, or whatever   }
  { is defined in the RecordIDTableName constant.  It has one record, and is }
  { used to keep track of the last ID used for each table.                   }

  { In a multi-user environment, This prevents collisions between two users  }
  { who happen to enter the same type of new record at the same time.        }

  { New record ID's are created here and then the Record ID table is         }
  { immediately updated to reflect the new value.  If the table is           }
  { unsuccessfully updated, then it is assumed that another user has claimed }
  { that ID, so the ID is incremented and another attempt is made, until we  }
  { are successful.                                                          }

  Query := TDBISAMQuery.Create(self);
  ID := 0;
  try
    Query.SessionName := FSessionName;
    Query.DatabaseName := FDatabase.DatabaseName;

    Query.Sql.Text := 'Select * from ' + RecordIDTableName;
    Query.Open;

    if TableName = ResourceTableName then begin
      FldName := 'ResourceID';
      ID := Query.FieldByName('ResourceID').AsInteger;

    end else if TableName = TasksTableName then begin
      FldName := 'TaskID';
      ID := Query.FieldByName('TaskID').AsInteger;

    end else if TableName = EventsTableName then begin
      FldName := 'EventID';
      ID := Query.FieldByName('EventID').AsInteger;

    end else if TableName = ContactsTableName then begin
      FldName := 'ContactID';
      ID := Query.FieldByName('ContactID').AsInteger;

    end else begin
      raise EInvalidTable.Create;
      exit;
    end;

    Query.Close;
    Query.SQL.Text := 'Update ' + RecordIDTableName + ' Set ' + FldName
      + ' = :NewID Where (' + FldName + ' = :OldID)';

    GotIt := false;
    while not GotIt do begin
      Inc(ID);
      Query.ParamByName('NewID').AsInteger := ID;
      Query.ParamByName('OldID').AsInteger := ID - 1;
      Query.ExecSQL;

      GotIt := (Query.RowsAffected = 1);
    end;
  finally
    Query.Close;
    Query.Free;
  end;

  result := ID;
end;

procedure TVpDBISAMDataStore.CreateTable(TableName: string);
var
  Table: TDBISAMTable;
begin
  Table := TDBISAMTable.Create(self);
  try
    Table.DatabaseName := FDatabase.DatabaseName;

    if TableName = ResourceTableName then begin
      { Create Resources Table }
      Table.Active := false;
      Table.TableName := ResourceTableName;
      Table.DatabaseName := FDatabase.DatabaseName;
    end

    else if TableName = EventsTableName then begin
      { Create Events Table }
      Table.Active := false;
      Table.TableName := EventsTableName;
      Table.DatabaseName := FDatabase.DatabaseName;
    end

    else if TableName = ContactsTableName then begin
      { Create Contacts Table }
      Table.Active := false;
      Table.TableName := ContactsTableName;
      Table.DatabaseName := FDatabase.DatabaseName;
    end

    else if TableName = TasksTableName then begin
      { Create Tasks Table }
      Table.Active := false;
      Table.TableName := TasksTableName;
      Table.DatabaseName := FDatabase.DatabaseName;
    end

    else if TableName = RecordIDTableName then begin
      { Create Tasks Table }
      Table.Active := false;
      Table.TableName := RecordIDTableName;
      Table.DatabaseName := FDatabase.DatabaseName;
    end;

    Table.DatabaseName := FDatabase.DatabaseName;
    CreateFieldDefs(TableName, Table.FieldDefs);
    CreateIndexDefs(TableName, Table.IndexDefs);

        if Table <> nil then
      Table.CreateTable;

    if TableName = RecordIDTableName then
      InitializeRecordIDTable;
  finally
    Table.Free;
  end;
end;
{=====}

procedure TVpDBISAMDataStore.InitializeRecordIDTable;
var
  Qry: TDBISAMQuery;
  ID: Integer;
begin
  Qry := TDBISAMQuery.Create(self);
  try
    Qry.DatabaseName := FDatabase.DatabaseName;

    Qry.SQL.Text := 'Select * from ' + RecordIDTableName;
    Qry.Open;
    if Qry.RowsAffected = 0 then begin
      { create one record in the table }
      Qry.SQL.Clear;
      Qry.SQL.Text := 'INSERT INTO ' + RecordIDTableName
      + '(ResourceID, EventID, TaskID, ContactID) '
      + 'VALUES(0, 0, 0, 0)';
      Qry.ExecSQL;
    end;
    Qry.Close;

    { Initialize Resource ID }
    Qry.SQL.Text := 'Select Max(ResourceID) as MaxRes from '
      + ResourceTableName;
    Qry.Open;
    ID := Qry.Fields[0].AsInteger;
    Qry.Close;

    Qry.SQL.Text := 'Update ' + RecordIDTableName + ' Set ResourceID = :ResID';
    Qry.ParamByName('ResID').AsInteger := ID;
    Qry.ExecSQL;

    { Initialize Event RecordID }
    Qry.SQL.Text := 'Select Max(RecordID) as MaxEvent from '
      + EventsTableName;
    Qry.Open;
    ID := Qry.Fields[0].AsInteger;
    Qry.Close;

    Qry.SQL.Text := 'Update ' + RecordIDTableName + ' Set EventID = :EvID';
    Qry.ParamByName('EvID').AsInteger := ID;
    Qry.ExecSQL;

    { Initialize Contact RecordID }
    Qry.SQL.Text := 'Select Max(RecordID) as MaxContact from '
      + ContactsTableName;
    Qry.Open;
    ID := Qry.Fields[0].AsInteger;
    Qry.Close;

    Qry.SQL.Text := 'Update ' + RecordIDTableName + ' Set ContactID = :CoID';
    Qry.ParamByName('CoID').AsInteger := ID;
    Qry.ExecSQL;

    { Initialize Task RecordID }
    Qry.SQL.Text := 'Select Max(RecordID) as MaxTask from ' + TasksTableName;
    Qry.Open;
    ID := Qry.Fields[0].AsInteger;
    Qry.Close;

    Qry.SQL.Text := 'Update ' + RecordIDTableName + ' Set TaskID = :TsID';
    Qry.ParamByName('TsID').AsInteger := ID;
    Qry.ExecSQL;

  finally
    Qry.Free;
  end;
end;
{=====}

procedure TVpDBISAMDataStore.Loaded;
begin
  inherited;
  if not (csDesigning in ComponentState) then
    Connected := AutoConnect;
end;
{=====}

procedure TVpDBISAMDataStore.PostResources;
var
  I: Integer;
  Resource: TVpResource;
  Qry: TDBISAMQuery;
begin
  if (Resources.Count > 0) then begin
    Qry := TDBISAMQuery.Create(self);
    Qry.SessionName := FSessionName;
    Qry.DatabaseName := FDatabase.DatabaseName;
    Qry.RequestLive := true;
    try
      for I := pred(Resources.Count) downto 0 do begin
        Resource := Resources.Items[I];
        if Resource = nil then begin
          Continue;
        end;

        if Resource.Deleted then begin
          PurgeEvents(Resource);
          PurgeContacts(Resource);
          PurgeTasks(Resource);
          Qry.SQL.Text := 'DELETE FROM Resources '
          + 'WHERE ResourceID = :ResID';
          Qry.ParamByName('ResID').AsInteger := Resource.ResourceID;
          Qry.ExecSQL;
          Resource.Free;
          Continue;
        end

        else if Resource.Changed then begin
          Qry.SQL.Text := 'SELECT * FROM Resources '
            + 'WHERE ResourceID = :ResID';
          Qry.ParamByName('ResID').AsInteger := Resource.ResourceID;
          Qry.Open;

          if Qry.Locate('ResourceID', Resource.ResourceID, [])
          then begin
            { existing record found }
            Qry.Edit;
            try
              Qry.FieldByName('ResourceID').AsInteger := Resource.ResourceID;
              Qry.FieldByName('Description').AsString := Resource.Description;
              Qry.FieldByName('Notes').AsString := Resource.Notes;
              Qry.FieldByName('ResourceActive').AsBoolean := Resource.Active;
              Qry.FieldByName('UserField0').AsString := Resource.UserField0;
              Qry.FieldByName('UserField1').AsString := Resource.UserField1;
              Qry.FieldByName('UserField2').AsString := Resource.UserField2;
              Qry.FieldByName('UserField3').AsString := Resource.UserField3;
              Qry.FieldByName('UserField4').AsString := Resource.UserField4;
              Qry.FieldByName('UserField5').AsString := Resource.UserField5;
              Qry.FieldByName('UserField6').AsString := Resource.UserField6;
              Qry.FieldByName('UserField7').AsString := Resource.UserField7;
              Qry.FieldByName('UserField8').AsString := Resource.UserField8;
              Qry.FieldByName('UserField9').AsString := Resource.UserField9;
              Qry.Post;
            except
              Qry.Cancel;
              raise EDBPostError.Create;
            end;
          end else begin
            Qry.SQL.Clear;
            Qry.SQL.Text := 'INSERT INTO Resources '
            + '(ResourceID, Description, Notes, ResourceActive, UserField0, '
            + 'UserField1, UserField2, UserField3, UserField4, UserField5, '
            + 'UserField6, UserField7, UserField8, UserField9) '
            + 'VALUES(:ResID, :Descr, :Notes, :ResActive, :UserField0, '
            + ':UserField1, :UserField2, :UserField3, :UserField4, '
            + ':UserField5, :UserField6, :UserField7, :UserField8, '
            + ':UserField9)';

            Qry.ParamByName('ResID').AsInteger :=  Resource.ResourceID;
            Qry.ParamByName('Descr').Asstring := Resource.Description;
            Qry.ParamByName('Notes').AsString := Resource.Notes;
            Qry.ParamByName('ResActive').AsBoolean := Resource.Active;
            Qry.ParamByName('UserField0').AsString := Resource.UserField0;
            Qry.ParamByName('UserField1').AsString := Resource.UserField1;
            Qry.ParamByName('UserField2').AsString := Resource.UserField2;
            Qry.ParamByName('UserField3').AsString := Resource.UserField3;
            Qry.ParamByName('UserField4').AsString := Resource.UserField4;
            Qry.ParamByName('UserField5').AsString := Resource.UserField5;
            Qry.ParamByName('UserField6').AsString := Resource.UserField6;
            Qry.ParamByName('UserField7').AsString := Resource.UserField7;
            Qry.ParamByName('UserField8').AsString := Resource.UserField8;
            Qry.ParamByName('UserField9').AsString := Resource.UserField9;

            Qry.ExecSQL;
          end;
          Resource.Changed := false;
        end;
        { if this is the active resource, then update all of its stuff }
        if Resource.ResourceID = ResourceID then begin
          PostEvents;
          PostContacts;
          PostTasks;
        end;
      end;
      Resources.Sort;

      if not Loading then                                                
        NotifyDependents;                                                

      if Assigned(AfterPostResources) then                               
        FAfterPostResources(self);                                       
    finally
      Qry.Close;
      Qry.Free;
    end;
  end;
end;

procedure TVpDBISAMDataStore.PostEvents;
var
  I: Integer;
  Event: TVpEvent;
  Qry: TDBISAMQuery;
begin
  if (Resource <> nil) and Resource.EventsDirty then begin
    Qry := TDBISAMQuery.Create(self);
    try
      Qry.SessionName := FSessionName;
      Qry.DatabaseName := FDatabase.DatabaseName;
      Qry.RequestLive := true;

      for I := pred(Resource.Schedule.EventCount) downto 0 do begin
        Event := Resource.Schedule.GetEvent(I);
        if Event.Deleted then begin
          Qry.SQL.Text := 'DELETE FROM Events '
          + 'WHERE RecordID = :ID';
          Qry.ParamByName('ID').AsInteger := Event.RecordID;
          Qry.ExecSQL;
          Event.Free;
          Continue;
        end

        else if Event.Changed then begin
          Qry.SQL.Text := 'SELECT * FROM Events '
            + 'WHERE RecordID = :ID';
          Qry.ParamByName('ID').AsInteger := Event.RecordID;
          Qry.Open;

          if Qry.Locate('RecordID', Event.RecordID, [])
          then begin
            { existing record found }
            Qry.Edit;
            try
              Qry.FieldByName('RecordID').AsInteger := Event.RecordID;
              Qry.FieldByName('StartTime').AsDateTime := Event.StartTime;
              Qry.FieldByName('EndTime').AsDateTime := Event.EndTime;
              Qry.FieldByName('ResourceID').AsInteger := Resource.ResourceID;
              Qry.FieldByName('Description').AsString := Event.Description;
              Qry.FieldByName('Notes').AsString := Event.Note;
              Qry.FieldByName('Category').AsInteger := Event.Category;
              Qry.FieldByName('DingPath').AsString := Event.AlarmWavPath;
              Qry.FieldByName('AllDayEvent').AsBoolean := Event.AllDayEvent;
              Qry.FieldByName('AlarmSet').AsBoolean := Event.AlarmSet;
              Qry.FieldByName('AlarmAdvance').AsInteger := Event.AlarmAdv;
              Qry.FieldByName('AlarmAdvanceType').AsInteger := Ord(Event.AlarmAdvType);
              Qry.FieldByName('SnoozeTime').AsDateTime := Event.SnoozeTime;
              Qry.FieldByName('RepeatCode').AsInteger := Ord(Event.RepeatCode);
              Qry.FieldByName('RepeatRangeEnd').AsDateTime := Event.RepeatRangeEnd;
              Qry.FieldByName('CustomInterval').AsInteger := Event.CustInterval;
              Qry.FieldByName('UserField0').AsString := Event.UserField0;
              Qry.FieldByName('UserField1').AsString := Event.UserField1;
              Qry.FieldByName('UserField2').AsString := Event.UserField2;
              Qry.FieldByName('UserField3').AsString := Event.UserField3;
              Qry.FieldByName('UserField4').AsString := Event.UserField4;
              Qry.FieldByName('UserField5').AsString := Event.UserField5;
              Qry.FieldByName('UserField6').AsString := Event.UserField6;
              Qry.FieldByName('UserField7').AsString := Event.UserField7;
              Qry.FieldByName('UserField8').AsString := Event.UserField8;
              Qry.FieldByName('UserField9').AsString := Event.UserField9;
              Qry.Post;
            except
              Qry.Cancel;
              raise EDBPostError.Create;
            end;
          end else begin
            Qry.Close;
            Qry.SQL.Text := 'INSERT INTO Events '
            + '(RecordID, StartTime, EndTime, ResourceID, Description, Notes, '
            + 'SnoozeTime, Category, DingPath, AllDayEvent, AlarmSet, '
            + 'AlarmAdvance, AlarmAdvanceType, RepeatCode, '
            + 'RepeatRangeEnd, CustomInterval, '
            + 'UserField0, UserField1, UserField2, UserField3, UserField4, '
            + 'UserField5, UserField6, UserField7, UserField8, UserField9) '
            + 'VALUES(:RecID, :STime, :ETime, :ResID, :Desc, :Notes, :SnTime, '
            + ':Cat, :DPath, :ADEvent, :ASet, :AAdvance, :AAdvanceType, '
            + ':RCode, :RRangeEnd, :CInterval, :UserField0, '
            + ':UserField1, :UserField2, :UserField3, :UserField4, '
            + ':UserField5, :UserField6, :UserField7, :UserField8, '
            + ':UserField9)';

            Qry.ParamByName('RecID').AsInteger        := Event.RecordID;
            Qry.ParamByName('STime').AsDateTime       := Event.StartTime;
            Qry.ParamByName('ETime').AsDateTime       := Event.EndTime;
            Qry.ParamByName('ResID').AsInteger        := Resource.ResourceID;
            Qry.ParamByName('Desc').AsString          := Event.Description;
            Qry.ParamByName('Notes').AsString         := Event.Note;
            Qry.ParamByName('SnTime').AsDateTime      := Event.SnoozeTime;
            Qry.ParamByName('Cat').AsInteger          := Event.Category;
            Qry.ParamByName('DPath').AsString         := Event.AlarmWavPath;
            Qry.ParamByName('ADEvent').AsBoolean      := Event.AllDayEvent;
            Qry.ParamByName('ASet').AsBoolean         := Event.AlarmSet;
            Qry.ParamByName('AAdvance').AsInteger     := Event.AlarmAdv;
            Qry.ParamByName('AAdvanceType').AsInteger := Ord(Event.AlarmAdvType);
            Qry.ParamByName('RCode').AsInteger        := Ord(Event.RepeatCode);
            Qry.ParamByName('RRangeEnd').AsDateTime   := Event.RepeatRangeEnd;
            Qry.ParamByName('CInterval').AsInteger    := Event.CustInterval;
            Qry.ParamByName('UserField0').AsString    := Event.UserField0;
            Qry.ParamByName('UserField1').AsString    := Event.UserField1;
            Qry.ParamByName('UserField2').AsString    := Event.UserField2;
            Qry.ParamByName('UserField3').AsString    := Event.UserField3;
            Qry.ParamByName('UserField4').AsString    := Event.UserField4;
            Qry.ParamByName('UserField5').AsString    := Event.UserField5;
            Qry.ParamByName('UserField6').AsString    := Event.UserField6;
            Qry.ParamByName('UserField7').AsString    := Event.UserField7;
            Qry.ParamByName('UserField8').AsString    := Event.UserField8;
            Qry.ParamByName('UserField9').AsString    := Event.UserField9;

            Qry.ExecSQL;
          end;
          Event.Changed := false;
        end;
      end;
      Resource.Schedule.Sort;
      NotifyDependents;
    finally
      Qry.CLose;
      Qry.Free;
    end;
    Resource.EventsDirty := false;
    Resource.Schedule.Sort;                                              

    if not Loading then                                                  
      NotifyDependents;                                                  

    if Assigned(AfterPostEvents) then                                    
      FAfterPostEvents(self);                                            
  end;
end;

procedure TVpDBISAMDataStore.PostContacts;
var
  I: Integer;
  Contact: TVpContact;
  Qry: TDBISAMQuery;
begin
  if (Resource <> nil) and Resource.ContactsDirty then begin
    { Dump this resource's dirty contacts to the DB }
    Qry := TDBISAMQuery.Create(self);
    try
      Qry.SessionName := FSessionName;
      Qry.DatabaseName := FDatabase.DatabaseName;
      Qry.RequestLive := true;

      for I := pred(Resource.Contacts.Count) downto 0 do begin
        Contact := Resource.Contacts.GetContact(I);

        if Contact.Deleted then begin
          Qry.SQL.Text := 'DELETE FROM Contacts '
          + 'WHERE RecordID = :ID';
          Qry.ParamByName('ID').AsInteger := Contact.RecordID;
          Qry.ExecSQL;
          Contact.Free;
          Continue;
        end

        else if Contact.Changed then begin
          Qry.SQL.Text := 'SELECT * FROM Contacts '
            + 'WHERE RecordID = :ID';
          Qry.ParamByName('ID').AsInteger := Contact.RecordID;
          Qry.Open;

          if Qry.Locate('RecordID', Contact.RecordID, [])
          then begin
            { existing record found }
            Qry.Edit;
            try
              Qry.FieldByName('ResourceID').AsInteger := Resource.ResourceID;
              Qry.FieldByName('RecordID').AsInteger := Contact.RecordID;
              Qry.FieldByName('FirstName').AsString := Contact.FirstName;
              Qry.FieldByName('LastName').AsString := Contact.LastName;
              Qry.FieldByName('Title').AsString := Contact.Title;
              Qry.FieldByName('Company').AsString := Contact.Company;
              Qry.FieldByName('Job_Position').AsString := Contact.Position;
              Qry.FieldByName('EMail').AsString := Contact.EMail;
              Qry.FieldByName('Address').AsString := Contact.Address;
              Qry.FieldByName('City').AsString := Contact.City;
              Qry.FieldByName('State').AsString := Contact.State;
              Qry.FieldByName('Zip').AsString := Contact.Zip;
              Qry.FieldByName('Country').AsString := Contact.Country;
              Qry.FieldByName('Note').AsString := Contact.Note;
              Qry.FieldByName('Phone1').AsString := Contact.Phone1;
              Qry.FieldByName('Phone2').AsString := Contact.Phone2;
              Qry.FieldByName('Phone3').AsString := Contact.Phone3;
              Qry.FieldByName('Phone4').AsString := Contact.Phone4;
              Qry.FieldByName('Phone5').AsString := Contact.Phone5;
              Qry.FieldByName('PhoneType1').AsInteger := Contact.PhoneType1;
              Qry.FieldByName('PhoneType2').AsInteger := Contact.PhoneType2;
              Qry.FieldByName('PhoneType3').AsInteger := Contact.PhoneType3;
              Qry.FieldByName('PhoneType4').AsInteger := Contact.PhoneType4;
              Qry.FieldByName('PhoneType5').AsInteger := Contact.PhoneType5;
              Qry.FieldByName('Category').AsInteger := Contact.Category;
              Qry.FieldByName('Custom1').AsString := Contact.Custom1;
              Qry.FieldByName('Custom2').AsString := Contact.Custom2;
              Qry.FieldByName('Custom3').AsString := Contact.Custom3;
              Qry.FieldByName('Custom4').AsString := Contact.Custom4;
              Qry.FieldByName('UserField0').AsString := Contact.UserField0;
              Qry.FieldByName('UserField1').AsString := Contact.UserField1;
              Qry.FieldByName('UserField2').AsString := Contact.UserField2;
              Qry.FieldByName('UserField3').AsString := Contact.UserField3;
              Qry.FieldByName('UserField4').AsString := Contact.UserField4;
              Qry.FieldByName('UserField5').AsString := Contact.UserField5;
              Qry.FieldByName('UserField6').AsString := Contact.UserField6;
              Qry.FieldByName('UserField7').AsString := Contact.UserField7;
              Qry.FieldByName('UserField8').AsString := Contact.UserField8;
              Qry.FieldByName('UserField9').AsString := Contact.UserField9;
              Qry.Post;
            except
              Qry.Cancel;
              raise EDBPostError.Create;
            end;
          end else begin
            Qry.Close;
            Qry.SQL.Text := 'INSERT INTO Contacts '
            + '(ResourceID, RecordID, FirstName, LastName, Title, Company, '
            + 'Job_Position, EMail, Address, City, State, Zip, Country, '
            + 'Note, Phone1, Phone2, Phone3, Phone4, Phone5, PhoneType1, '
            + 'PhoneType2, PhoneType3, PhoneType4, PhoneType5, Category, '
            + 'Custom1, Custom2, Custom3, Custom4, UserField0, UserField1, '
            + 'UserField2, UserField3, UserField4, UserField5, UserField6, '
            + 'UserField7, UserField8, UserField9 ) '

            + 'VALUES(:ResourceID, :RecordID, :FirstName, :LastName, :Title, '
            + ':Company, :Job_Position, :EMail, :Address, :City, :State, :Zip, '
            + ':Country, :Note, :Phone1, :Phone2, :Phone3, :Phone4, :Phone5, '
            + ':PhoneType1, :PhoneType2, :PhoneType3, :PhoneType4, :PhoneType5, '
            + ':Category, :Custom1, :Custom2, :Custom3, :Custom4, :UserField0, '
            + ':UserField1, :UserField2, :UserField3, :UserField4, :UserField5, '
            + ':UserField6, :UserField7, :UserField8, :UserField9)';

            Qry.ParamByName('ResourceID').AsInteger := Resource.ResourceID;
            Qry.ParamByName('RecordID').AsInteger := Contact.RecordID;
            Qry.ParamByName('FirstName').AsString := Contact.FirstName;
            Qry.ParamByName('LastName').AsString := Contact.LastName;
            Qry.ParamByName('Title').AsString := Contact.Title;
            Qry.ParamByName('Company').AsString := Contact.Company;
            Qry.ParamByName('Job_Position').AsString := Contact.Position;
            Qry.ParamByName('EMail').AsString := Contact.EMail;
            Qry.ParamByName('Address').AsString := Contact.Address;
            Qry.ParamByName('City').AsString := Contact.City;
            Qry.ParamByName('State').AsString := Contact.State;
            Qry.ParamByName('Zip').AsString := Contact.Zip;
            Qry.ParamByName('Country').AsString := Contact.Country;
            Qry.ParamByName('Note').AsString := Contact.Note;
            Qry.ParamByName('Phone1').AsString := Contact.Phone1;
            Qry.ParamByName('Phone2').AsString := Contact.Phone2;
            Qry.ParamByName('Phone3').AsString := Contact.Phone3;
            Qry.ParamByName('Phone4').AsString := Contact.Phone4;
            Qry.ParamByName('Phone5').AsString := Contact.Phone5;
            Qry.ParamByName('PhoneType1').AsInteger := Contact.PhoneType1;
            Qry.ParamByName('PhoneType2').AsInteger := Contact.PhoneType2;
            Qry.ParamByName('PhoneType3').AsInteger := Contact.PhoneType3;
            Qry.ParamByName('PhoneType4').AsInteger := Contact.PhoneType4;
            Qry.ParamByName('PhoneType5').AsInteger := Contact.PhoneType5;
            Qry.ParamByName('Category').AsInteger := Contact.Category;
            Qry.ParamByName('Custom1').AsString := Contact.Custom1;
            Qry.ParamByName('Custom2').AsString := Contact.Custom2;
            Qry.ParamByName('Custom3').AsString := Contact.Custom3;
            Qry.ParamByName('Custom4').AsString := Contact.Custom4;
            Qry.ParamByName('UserField0').AsString := Contact.UserField0;
            Qry.ParamByName('UserField1').AsString := Contact.UserField1;
            Qry.ParamByName('UserField2').AsString := Contact.UserField2;
            Qry.ParamByName('UserField3').AsString := Contact.UserField3;
            Qry.ParamByName('UserField4').AsString := Contact.UserField4;
            Qry.ParamByName('UserField5').AsString := Contact.UserField5;
            Qry.ParamByName('UserField6').AsString := Contact.UserField6;
            Qry.ParamByName('UserField7').AsString := Contact.UserField7;
            Qry.ParamByName('UserField8').AsString := Contact.UserField8;
            Qry.ParamByName('UserField9').AsString := Contact.UserField9;

            Qry.ExecSQL;
          end;
          Contact.Changed := false;
        end;
      end;

    finally
      Qry.Free;
    end;
    Resource.ContactsDirty := false;

    if not Loading then                                                  
      NotifyDependents;                                                  

    if Assigned(AfterPostContacts) then                                  
      FAfterPostContacts(self);                                          
  end;
end;


procedure TVpDBISAMDataStore.PostTasks;
var
  I: Integer;
  Task: TVpTask;
  Qry : TDBISAMQuery;
begin
  if (Resource <> nil) and Resource.TasksDirty then begin
    { Dump this resource's dirty contacts to the DB }
    Qry := TDBISAMQuery.Create(self);
    try
      Qry.SessionName := FSessionName;
      Qry.DatabaseName := FDatabase.DatabaseName;
      Qry.RequestLive := true;

      for I := pred(Resource.Tasks.Count) downto 0 do begin
        Task := Resource.Tasks.GetTask(I);
        if Task.Deleted then begin
          Qry.SQL.Text := 'DELETE FROM Tasks '
          + 'WHERE RecordID = :ID';
          Qry.ParamByName('ID').AsInteger := Task.RecordID;
          Qry.ExecSQL;
          Task.Free;
          Continue;
        end

        else if Task.Changed then begin
          Qry.SQL.Text := 'SELECT * FROM Tasks '
            + 'WHERE RecordID = :ID';
          Qry.ParamByName('ID').AsInteger := Task.RecordID;
          Qry.Open;

          if Qry.Locate('RecordID', Task.RecordID, [])
          then begin
            { existing record found }
            Qry.Edit;
            try
              Qry.FieldByName('ResourceID').AsInteger := Resource.ResourceID;
              Qry.FieldByName('Description').AsString := Task.Description;
              Qry.FieldByName('Details').AsString := Task.Details;
              Qry.FieldByName('Complete').AsBoolean := Task.Complete;
              Qry.FieldByName('DueDate').AsDateTime := Task.DueDate;
              Qry.FieldByName('CreatedOn').AsDateTime := Task.CreatedOn;
              Qry.FieldByName('CompletedOn').AsDateTime := Task.CompletedOn;
              Qry.FieldByName('Priority').AsInteger := Task.Priority;
              Qry.FieldByName('Category').AsInteger := Task.Category;
              Qry.FieldByName('UserField0').AsString := Task.UserField0;
              Qry.FieldByName('UserField1').AsString := Task.UserField1;
              Qry.FieldByName('UserField2').AsString := Task.UserField2;
              Qry.FieldByName('UserField3').AsString := Task.UserField3;
              Qry.FieldByName('UserField4').AsString := Task.UserField4;
              Qry.FieldByName('UserField5').AsString := Task.UserField5;
              Qry.FieldByName('UserField6').AsString := Task.UserField6;
              Qry.FieldByName('UserField7').AsString := Task.UserField7;
              Qry.FieldByName('UserField8').AsString := Task.UserField8;
              Qry.FieldByName('UserField9').AsString := Task.UserField9;
              Qry.Post;
            except
              Qry.Cancel;
              raise EDBPostError.Create;
            end;
          end else begin
            Qry.Close;
            Qry.SQL.Text := 'INSERT INTO Tasks '
            + '(RecordID, ResourceID, Description, Details, '
            + 'Complete, DueDate, CreatedOn, CompletedOn, Priority, Category, '
            + 'UserField0, UserField1, UserField2, UserField3, UserField4, '
            + 'UserField5, UserField6, UserField7, UserField8, UserField9) '

            + 'VALUES(:RecordID, :ResourceID, :Description, :Details, '
            + ':Complete, :DueDate, :CreatedOn, :CompletedOn, :Priority, '
            + ':Category, :UserField0, :UserField1, :UserField2, :UserField3, '
            + ':UserField4, :UserField5, :UserField6, :UserField7, '
            + ':UserField8, :UserField9)';

            Qry.ParamByName('RecordID').AsInteger     := Task.RecordID;
            Qry.ParamByName('ResourceID').AsInteger   := Resource.ResourceID;
            Qry.ParamByName('Description').AsString   := Task.Description;
            Qry.ParamByName('Details').AsString       := Task.Details;
            Qry.ParamByName('Complete').AsBoolean     := Task.Complete;
            Qry.ParamByName('DueDate').AsDateTime     := Task.DueDate;
            Qry.ParamByName('CreatedOn').AsDateTime   := Task.CreatedOn;
            Qry.ParamByName('CompletedOn').AsDateTime := Task.CompletedOn;
            Qry.ParamByName('Priority').AsInteger := Task.Priority;
            Qry.ParamByName('Category').AsInteger := Task.Category;
            Qry.ParamByName('UserField0').AsString := Task.UserField0;
            Qry.ParamByName('UserField1').AsString := Task.UserField1;
            Qry.ParamByName('UserField2').AsString := Task.UserField2;
            Qry.ParamByName('UserField3').AsString := Task.UserField3;
            Qry.ParamByName('UserField4').AsString := Task.UserField4;
            Qry.ParamByName('UserField5').AsString := Task.UserField5;
            Qry.ParamByName('UserField6').AsString := Task.UserField6;
            Qry.ParamByName('UserField7').AsString := Task.UserField7;
            Qry.ParamByName('UserField8').AsString := Task.UserField8;
            Qry.ParamByName('UserField9').AsString := Task.UserField9;
            Qry.ExecSQL;
          end;
          Task.Changed := false;
        end
      end;

    finally
      Qry.Free;
    end;

    Resource.TasksDirty := false;

    if not Loading then                                                  
      NotifyDependents;                                                  

    if Assigned(AfterPostTasks) then                                     
      FAfterPostTasks(self);                                             
  end;
end;

procedure TVpDBISAMDataStore.PurgeResource(Res: TVpResource);
begin
  Res.Deleted := true;                                                   
  PostResources;
  Load;
end;

procedure TVpDBISAMDataStore.PurgeEvents(Res: TVpResource);
var
  Qry: TDBISAMQuery;
begin
  Qry := TDBISAMQuery.Create(self);
  try
    Qry.SessionName := FSessionName;
    Qry.DatabaseName := FDataBase.DatabaseName;

    Qry.SQL.Text := 'delete from Events where ResourceID = :ResID';
    Qry.ParamByName('ResID').AsInteger := Res.ResourceID;                
    Qry.ExecSQL;
  finally
    Qry.Free;
  end;
  Res.Schedule.ClearEvents;                                              
end;

procedure TVpDBISAMDataStore.PurgeContacts(Res: TVpResource);
var
  Qry: TDBISAMQuery;
begin
  Qry := TDBISAMQuery.Create(self);
  try
    Qry.SessionName := FSessionName;
    Qry.DatabaseName := FDataBase.DatabaseName;

    Qry.SQL.Text := 'delete from Contacts where ResourceID = :ResID';
    Qry.ParamByName('ResID').AsInteger := Res.ResourceID;                
    Qry.ExecSQL;
  finally
    Qry.Free;
  end;
  Res.Contacts.ClearContacts;                                            
end;

procedure TVpDBISAMDataStore.PurgeTasks(Res: TVpResource);
var
  Qry: TDBISAMQuery;
begin
  Qry := TDBISAMQuery.Create(self);
  try
    Qry.SessionName := FSessionName;
    Qry.DatabaseName := FDataBase.DatabaseName;

    Qry.SQL.Text := 'delete from Tasks where ResourceID = :ResID';
    Qry.ParamByName('ResID').AsInteger := Res.ResourceID;                
    Qry.ExecSQL;
  finally
    Qry.Free;
  end;
  Res.Tasks.ClearTasks;                                                  
end;
{=====}

procedure TVpDBISAMDataStore.SetConnected(const Value: boolean);
var
  DBPath : string;
  Str    : string;
begin
  { disconnect if destroying }
  if csDestroying in ComponentState then begin
    FDataBase.Connected := false;
    exit;
  end;

  { Don't connect at designtime }
  if csDesigning in ComponentState then exit;

  { Don't try to connect until we're all loaded up }
  if csLoading in ComponentState then exit;

  if FDatabase.DatabaseName = '' then
    FDatabase.DatabaseName := 'VpDatabase' + Name[Length(Name)];

  FDataBase.Connected := Value;
  if FDataBase.Connected then begin
    if FDatabase.Directory <> '' then
        DBPath := FDatabase.Directory + '\'
    else
        DBPath := '';

    { Create / Open Resources Table}
    Str := FDatabase.DatabaseName;
    FResourceTable.DatabaseName := Str;
    if (not FileExists(DBPath + ResourceTableName + '.*'))
    then CreateTable(ResourceTableName);
    try
      FResourceTable.Open;
    except
      if AutoCreate then begin
        CreateTable(ResourceTableName);
        FResourceTable.Open;
      end;
    end;

    { Create / Open Events Table }
    FEventsTable.DatabaseName := FDatabase.DatabaseName;
    if (not FileExists(DBPath + EventsTableName + '.*'))
    then CreateTable(EventsTableName);
    SetFilterCriteria(FEventsTable,
                      True,
                      ResourceTable.FieldByName('ResourceID').AsInteger,
                      TimeRange.StartTime,
                      TimeRange.EndTime);
    try
      FEventsTable.Open;
    except
      if AutoCreate then begin
        CreateTable(EventsTableName);
        FEventsTable.Open;
      end;
    end;

    { Create / Open Contacts Table }
    FContactsTable.DatabaseName := FDatabase.DatabaseName;
    if (not FileExists(DBPath + ContactsTableName + '.*'))
    then CreateTable(ContactsTableName);
    SetFilterCriteria(FContactsTable, False,
                      ResourceTable.FieldByName('ResourceID').AsInteger,
                      0, 0);
    try
      FContactsTable.Open;
    except
      if AutoCreate then begin
        CreateTable(ContactsTableName);
        FContactsTable.Open;
      end;
    end;

    { Create / Open Tasks Table }
    FTasksTable.DatabaseName := FDatabase.DatabaseName;
    if (not FileExists(DBPath + TasksTableName + '.*'))
    then CreateTable(TasksTableName);
    SetFilterCriteria(FTasksTable, False,
                      ResourceTable.FieldByName('ResourceID').AsInteger,
                      0, 0);
    try
      FTasksTable.Open;
    except
      if AutoCreate then begin
        CreateTable(TasksTableName);
        FTasksTable.Open;
      end;
    end;

    { Create / Open RecordID Table }
    FRecordIDTable.DatabaseName := FDatabase.DatabaseName;
    if (not FileExists(DBPath + RecordIDTableName + '.*'))
    then CreateTable(RecordIDTableName);

    Load;
  end
  else begin
    FTasksTable.Close;
    FContactsTable.Close;
    FResourceTable.Close;
    FEventsTable.Close;
  end;

  inherited SetConnected(Database.Connected);
end;
{=====}

{ Called by the ancestor to properly filter the data for each table, }
{ based on the ResourceID, Date and DayBuffer values.                }
{ Each TVpCustomDBDataStore descendant should define their own       }
{ SetFilterCriteria procedure.                                       }
procedure TVpDBISAMDataStore.SetFilterCriteria(aTable :TDataset;
  aUseDateTime: Boolean; aResourceID: Longint; aStartDateTime: TDateTime;
  aEndDateTime: TDateTime);
var
  Qry: TDBISAMQuery;
begin
  Qry := (aTable as TDBISAMQuery);

  Qry.Close;

  Qry.ParamByName('ResID').AsInteger := aResourceID;

  if Qry = EventsTable then begin
    Qry.ParamByName('STime1').AsDateTime := aStartDateTime;
    Qry.ParamByName('ETime').AsDateTime := aEndDateTime;
    Qry.ParamByName('STime2').AsDateTime := aStartDateTime;
  end;

  Qry.Open;
end;
{=====}

procedure TVpDBISAMDataStore.CreateIndexDefs(const TableName: string;
  IndexDefs: TIndexDefs);
begin
  if TableName = ResourceTableName then begin
    with IndexDefs do begin
      Clear;
      { DBISAM primary keys have no name }
      with AddIndexDef do begin
        Name := '';
        Fields := 'ResourceID';
        Options := [ixPrimary,ixUnique];
      end;
      with AddIndexDef do begin
        Name := 'Descr_ndx';
        Fields := 'Description';
        Options := [ixCaseInsensitive];
      end;
    end;
  end else if TableName = EventsTableName then begin
    with IndexDefs do begin
      Clear;
      { DBISAM primary keys have no name }
      with AddIndexDef do begin
        Name := '';
        Fields := 'RecordID';
        Options := [ixPrimary,ixUnique];
      end;
      with AddIndexDef do begin                                          
        Name := 'rid_st_ndx';                                            
        Fields := 'ResourceID;StartTime';                                
        Options := [ixCaseInsensitive];                                  
      end;                                                               
      with AddIndexDef do begin
        Name := 'st_ndx';
        Fields := 'StartTime';
        Options := [];
      end;
      with AddIndexDef do begin
        Name := 'et_ndx';
        Fields := 'EndTime';
        Options := [];
      end;
      with AddIndexDef do begin
        Name := VpcIndexNameResID;
        Fields := 'ResourceID';
        Options := [];
      end;
    end;
  end else if TableName = ContactsTableName then begin
    with IndexDefs do begin
      Clear;
      { DBISAM primary keys have no name }
      with AddIndexDef do begin
        Name := '';
        Fields := 'RecordID';
        Options := [ixPrimary,ixUnique];
      end;
      with AddIndexDef do begin
        Name := VpcIndexNameResID;
        Fields := 'ResourceID';
        Options := [];
      end;
      with AddIndexDef do begin
        Name := 'LName_ndx';
        Fields := 'LastName';
        Options := [ixCaseInsensitive];
      end;
      with AddIndexDef do begin
        Name := 'Company_ndx';
        Fields := 'Company';
        Options := [ixCaseInsensitive];
      end;
    end;
  end else if TableName = TasksTableName then begin
    with IndexDefs do begin
      Clear;
      { DBISAM primary keys have no name }
      with AddIndexDef do begin
        Name := '';
        Fields := 'RecordID';
        Options := [ixPrimary,ixUnique];
      end;
      with AddIndexDef do begin
        Name := VpcIndexNameResID;
        Fields := 'ResourceID';
        Options := [];
      end;
      with AddIndexDef do begin
        Name := 'DueDate';
        Fields := 'DueDate';
        Options := [];
      end;
      with AddIndexDef do begin
        Name := 'CompletedOn';
        Fields := 'CompletedOn';
        Options := [];
      end;
    end;
  end;
end;
{=====}

procedure TVpDBISAMDataStore.CreateFieldDefs(const TableName: string;
    FieldDefs: TFieldDefs);
begin
  if TableName = ResourceTableName then begin
    with FieldDefs do begin
      Clear;
      { Resource ID }
      with AddFieldDef do begin
        Name := 'ResourceID';
        DataType := ftInteger;
        Required := true;
      end;
      { Description }
      with AddFieldDef do begin
        Name := 'Description';
        DataType := ftString;
        Size := 250;
        Required := false;
      end;
      { Description }
      with AddFieldDef do begin
        Name := 'Notes';
        DataType := ftMemo;
        Required := false;
      end;
      { Image Index }
      with AddFieldDef do begin
        Name := 'ImageIndex';
        DataType := ftInteger;
        Required := false;
      end;
      { Active }
      with AddFieldDef do begin
        Name := 'ResourceActive';
        DataType := ftBoolean;
        Required := false;
      end;
      { UserField0 }
      with AddFieldDef do begin
        Name := 'UserField0';
        DataType := ftString;
        Size := 100;
        Required := false;
      end;
      { UserField1 }
      with AddFieldDef do begin
        Name := 'UserField1';
        DataType := ftString;
        Size := 100;
        Required := false;
      end;
      { UserField2 }
      with AddFieldDef do begin
        Name := 'UserField2';
        DataType := ftString;
        Size := 100;
        Required := false;
      end;
      { UserField3 }
      with AddFieldDef do begin
        Name := 'UserField3';
        DataType := ftString;
        Size := 100;
        Required := false;
      end;
      { UserField4 }
      with AddFieldDef do begin
        Name := 'UserField4';
        DataType := ftString;
        Size := 100;
        Required := false;
      end;
      { UserField5 }
      with AddFieldDef do begin
        Name := 'UserField5';
        DataType := ftString;
        Size := 100;
        Required := false;
      end;
      { UserField6 }
      with AddFieldDef do begin
        Name := 'UserField6';
        DataType := ftString;
        Size := 100;
        Required := false;
      end;
      { UserField7 }
      with AddFieldDef do begin
        Name := 'UserField7';
        DataType := ftString;
        Size := 100;
        Required := false;
      end;
      { UserField8 }
      with AddFieldDef do begin
        Name := 'UserField8';
        DataType := ftString;
        Size := 100;
        Required := false;
      end;
      { UserField9 }
      with AddFieldDef do begin
        Name := 'UserField9';
        DataType := ftString;
        Size := 100;
        Required := false;
      end;
    end;
  end else if TableName = EventsTableName then begin
    with FieldDefs do begin
      Clear;
      { Record ID }
      with AddFieldDef do begin
        Name := 'RecordID';
        DataType := ftInteger;
        Required := true;
      end;
      { StartTime }
      with AddFieldDef do begin
        Name := 'StartTime';
        DataType := ftDateTime;
        Required := true;
      end;
      { EndTime }
      with AddFieldDef do begin
        Name := 'EndTime';
        DataType := ftDateTime;
        Required := true;
      end;
      { Resource ID }
      with AddFieldDef do begin
        Name := 'ResourceID';
        DataType := ftInteger;
        Required := true;
      end;
      { Description }
      with AddFieldDef do begin
        Name := 'Description';
        DataType := ftString;
        Size := 250;
        Required := false;
      end;
      { Note }
      with AddFieldDef do begin
        Name := 'Notes';
        DataType := ftMemo;
        Required := false;
      end;
      { Category }
      with AddFieldDef do begin
        Name := 'Category';
        DataType := ftInteger;
        Required := false;
      end;
      { AllDayEvent }
      with AddFieldDef do begin
        Name := 'AllDayEvent';
        DataType := ftBoolean;
        Required := false;
      end;
      { DingPath }
      with AddFieldDef do begin
        Name := 'DingPath';
        DataType := ftString;
        Size := 250;
        Required := false;
      end;
      { AlarmSet }
      with AddFieldDef do begin
        Name := 'AlarmSet';
        DataType := ftBoolean;
        Required := false;
      end;
      { Alarm Advance }
      with AddFieldDef do begin
        Name := 'AlarmAdvance';
        DataType := ftInteger;
        Required := false;
      end;
      { Alarm Advance Type }
      with AddFieldDef do begin
        Name := 'AlarmAdvanceType';
        DataType := ftWord;
        Required := false;
      end;
      { SnoozeTime }
      with AddFieldDef do begin
        Name := 'SnoozeTime';
        DataType := ftDateTime;
        Required := true;
      end;
      { Repeat Code }
      with AddFieldDef do begin
        Name := 'RepeatCode';
        DataType := ftWord;
        Required := false;
      end;
      { Repeat Range End }
      with AddFieldDef do begin
        Name := 'RepeatRangeEnd';
        DataType := ftDateTime;
        Required := false;
      end;
      { Custom Repeat Interval }
      with AddFieldDef do begin
        Name := 'CustomInterval';
        DataType := ftInteger;
        Required := false;
      end;
      { UserField0 }
      with AddFieldDef do begin
        Name := 'UserField0';
        DataType := ftString;
        Size := 100;
        Required := false;
      end;
      { UserField1 }
      with AddFieldDef do begin
        Name := 'UserField1';
        DataType := ftString;
        Size := 100;
        Required := false;
      end;
      { UserField2 }
      with AddFieldDef do begin
        Name := 'UserField2';
        DataType := ftString;
        Size := 100;
        Required := false;
      end;
      { UserField3 }
      with AddFieldDef do begin
        Name := 'UserField3';
        DataType := ftString;
        Size := 100;
        Required := false;
      end;
      { UserField4 }
      with AddFieldDef do begin
        Name := 'UserField4';
        DataType := ftString;
        Size := 100;
        Required := false;
      end;
      { UserField5 }
      with AddFieldDef do begin
        Name := 'UserField5';
        DataType := ftString;
        Size := 100;
        Required := false;
      end;
      { UserField6 }
      with AddFieldDef do begin
        Name := 'UserField6';
        DataType := ftString;
        Size := 100;
        Required := false;
      end;
      { UserField7 }
      with AddFieldDef do begin
        Name := 'UserField7';
        DataType := ftString;
        Size := 100;
        Required := false;
      end;
      { UserField8 }
      with AddFieldDef do begin
        Name := 'UserField8';
        DataType := ftString;
        Size := 100;
        Required := false;
      end;
      { UserField9 }
      with AddFieldDef do begin
        Name := 'UserField9';
        DataType := ftString;
        Size := 100;
        Required := false;
      end;
    end;
  end else if TableName = ContactsTableName then begin
    with FieldDefs do begin
      Clear;
      { Record ID }
      with AddFieldDef do begin
        Name := 'RecordID';
        DataType := ftInteger;
        Required := true;
      end;
      { Resource ID }
      with AddFieldDef do begin
        Name := 'ResourceID';
        DataType := ftInteger;
        Required := true;
      end;
      { FirstName }
      with AddFieldDef do begin
        Name := 'FirstName';
        DataType := ftString;
        Size := 50;
        Required := false;
      end;
      { LastName }
      with AddFieldDef do begin
        Name := 'LastName';
        DataType := ftString;
        Size := 50;
        Required := false;
      end;
      { Birthdate }
      with AddFieldDef do begin
        Name := 'Birthdate';
        DataType := ftDate;
        Required := false;
      end;
      { Anniversary }
      with AddFieldDef do begin
        Name := 'Anniversary';
        DataType := ftDate;
        Required := false;
      end;
      { Title }
      with AddFieldDef do begin
        Name := 'Title';
        DataType := ftString;
        Size := 50;
        Required := false;
      end;
      { Company }
      with AddFieldDef do begin
        Name := 'Company';
        DataType := ftString;
        Size := 50;
        Required := false;
      end;
      { Position }
      with AddFieldDef do begin
        Name := 'Job_Position';
        DataType := ftString;
        Size := 30;
        Required := false;
      end;
      { Address }
      with AddFieldDef do begin
        Name := 'Address';
        DataType := ftString;
        Size := 100;
        Required := false;
      end;
      { City }
      with AddFieldDef do begin
        Name := 'City';
        DataType := ftString;
        Size := 50;
        Required := false;
      end;
      { State }
      with AddFieldDef do begin
        Name := 'State';
        DataType := ftString;
        Size := 25;
        Required := false;
      end;
      { Zip }
      with AddFieldDef do begin
        Name := 'Zip';
        DataType := ftString;
        Size := 10;
        Required := false;
      end;
      { Country }
      with AddFieldDef do begin
        Name := 'Country';
        DataType := ftString;
        Size := 25;
        Required := false;
      end;
      { Note }
      with AddFieldDef do begin
        Name := 'Note';
        DataType := ftMemo;
        Required := false;
      end;
      { Phone1 }
      with AddFieldDef do begin
        Name := 'Phone1';
        DataType := ftString;
        Size := 25;
        Required := false;
      end;
      { Phone2 }
      with AddFieldDef do begin
        Name := 'Phone2';
        DataType := ftString;
        Size := 25;
        Required := false;
      end;
      { Phone3 }
      with AddFieldDef do begin
        Name := 'Phone3';
        DataType := ftString;
        Size := 25;
        Required := false;
      end;
      { Phone4 }
      with AddFieldDef do begin
        Name := 'Phone4';
        DataType := ftString;
        Size := 25;
        Required := false;
      end;
      { Phone5 }
      with AddFieldDef do begin
        Name := 'Phone5';
        DataType := ftString;
        Size := 25;
        Required := false;
      end;
      { Phone1 }
      with AddFieldDef do begin
        Name := 'PhoneType1';
        DataType := ftInteger;
        Required := false;
      end;
      { Phone2 }
      with AddFieldDef do begin
        Name := 'PhoneType2';
        DataType := ftInteger;
        Required := false;
      end;
      { Phone3 }
      with AddFieldDef do begin
        Name := 'PhoneType3';
        DataType := ftInteger;
        Required := false;
      end;
      { Phone4 }
      with AddFieldDef do begin
        Name := 'PhoneType4';
        DataType := ftInteger;
        Required := false;
      end;
      { Phone5 }
      with AddFieldDef do begin
        Name := 'PhoneType5';
        DataType := ftInteger;
        Required := false;
      end;
      { Category }
      with AddFieldDef do begin
        Name := 'Category';
        DataType := ftInteger;
        Required := false;
      end;
      { EMail }
      with AddFieldDef do begin
        Name := 'EMail';
        DataType := ftString;
        Size := 100;
        Required := false;
      end;
      { Custom1 }
      with AddFieldDef do begin
        Name := 'Custom1';
        DataType := ftString;
        Size := 100;
        Required := false;
      end;
      { Custom2 }
      with AddFieldDef do begin
        Name := 'Custom2';
        DataType := ftString;
        Size := 100;
        Required := false;
      end;
      { Custom3 }
      with AddFieldDef do begin
        Name := 'Custom3';
        DataType := ftString;
        Size := 100;
        Required := false;
      end;
      { Custom4 }
      with AddFieldDef do begin
        Name := 'Custom4';
        DataType := ftString;
        Size := 100;
        Required := false;
      end;
      { UserField0 }
      with AddFieldDef do begin
        Name := 'UserField0';
        DataType := ftString;
        Size := 100;
        Required := false;
      end;
      { UserField1 }
      with AddFieldDef do begin
        Name := 'UserField1';
        DataType := ftString;
        Size := 100;
        Required := false;
      end;
      { UserField2 }
      with AddFieldDef do begin
        Name := 'UserField2';
        DataType := ftString;
        Size := 100;
        Required := false;
      end;
      { UserField3 }
      with AddFieldDef do begin
        Name := 'UserField3';
        DataType := ftString;
        Size := 100;
        Required := false;
      end;
      { UserField4 }
      with AddFieldDef do begin
        Name := 'UserField4';
        DataType := ftString;
        Size := 100;
        Required := false;
      end;
      { UserField5 }
      with AddFieldDef do begin
        Name := 'UserField5';
        DataType := ftString;
        Size := 100;
        Required := false;
      end;
      { UserField6 }
      with AddFieldDef do begin
        Name := 'UserField6';
        DataType := ftString;
        Size := 100;
        Required := false;
      end;
      { UserField7 }
      with AddFieldDef do begin
        Name := 'UserField7';
        DataType := ftString;
        Size := 100;
        Required := false;
      end;
      { UserField8 }
      with AddFieldDef do begin
        Name := 'UserField8';
        DataType := ftString;
        Size := 100;
        Required := false;
      end;
      { UserField9 }
      with AddFieldDef do begin
        Name := 'UserField9';
        DataType := ftString;
        Size := 100;
        Required := false;
      end;
    end;
  end else if TableName = TasksTableName then begin
    with FieldDefs do begin
      Clear;
      { Record ID }
      with AddFieldDef do begin
        Name := 'RecordID';
        DataType := ftInteger;
        Required := true;
      end;
      { Resource ID }
      with AddFieldDef do begin
        Name := 'ResourceID';
        DataType := ftInteger;
        Required := true;
      end;
      { Complete }
      with AddFieldDef do begin
        Name := 'Complete';
        DataType := ftBoolean;
        Required := false;
      end;
      { Description }
      with AddFieldDef do begin
        Name := 'Description';
        DataType := ftString;
        Size := 250;
        Required := false;
      end;
      { Details }
      with AddFieldDef do begin
        Name := 'Details';
        DataType := ftMemo;
        Required := false;
      end;
      { Created On Date }
      with AddFieldDef do begin
        Name := 'CreatedOn';
        DataType := ftDateTime;
        Required := false;
      end;
      { Priority }
      with AddFieldDef do begin
        Name := 'Priority';
        DataType := ftInteger;
        Required := false;
      end;
      { Category }
      with AddFieldDef do begin
        Name := 'Category';
        DataType := ftInteger;
        Required := false;
      end;
      { Completed On Date }
      with AddFieldDef do begin
        Name := 'CompletedOn';
        DataType := ftDateTime;
        Required := false;
      end;
      { Due Date }
      with AddFieldDef do begin
        Name := 'DueDate';
        DataType := ftDateTime;
        Required := false;
      end;
      { UserField0 }
      with AddFieldDef do begin
        Name := 'UserField0';
        DataType := ftString;
        Size := 100;
        Required := false;
      end;
      { UserField1 }
      with AddFieldDef do begin
        Name := 'UserField1';
        DataType := ftString;
        Size := 100;
        Required := false;
      end;
      { UserField2 }
      with AddFieldDef do begin
        Name := 'UserField2';
        DataType := ftString;
        Size := 100;
        Required := false;
      end;
      { UserField3 }
      with AddFieldDef do begin
        Name := 'UserField3';
        DataType := ftString;
        Size := 100;
        Required := false;
      end;
      { UserField4 }
      with AddFieldDef do begin
        Name := 'UserField4';
        DataType := ftString;
        Size := 100;
        Required := false;
      end;
      { UserField5 }
      with AddFieldDef do begin
        Name := 'UserField5';
        DataType := ftString;
        Size := 100;
        Required := false;
      end;
      { UserField6 }
      with AddFieldDef do begin
        Name := 'UserField6';
        DataType := ftString;
        Size := 100;
        Required := false;
      end;
      { UserField7 }
      with AddFieldDef do begin
        Name := 'UserField7';
        DataType := ftString;
        Size := 100;
        Required := false;
      end;
      { UserField8 }
      with AddFieldDef do begin
        Name := 'UserField8';
        DataType := ftString;
        Size := 100;
        Required := false;
      end;
      { UserField9 }
      with AddFieldDef do begin
        Name := 'UserField9';
        DataType := ftString;
        Size := 100;
        Required := false;
      end;
    end;
  end else if TableName = RecordIDTableName then begin
    { The RecordID Table has only one record with 4 fields                }
    { each field contains the last record ID for each of the other tables }
    with FieldDefs do begin
      Clear;
      { Resource ID }
      with AddFieldDef do begin
        Name := 'ResourceID';
        DataType := ftInteger;
        Required := true;
      end;
      { Event ID }
      with AddFieldDef do begin
        Name := 'EventID';
        DataType := ftInteger;
        Required := true;
      end;
      { Task ID }
      with AddFieldDef do begin
        Name := 'TaskID';
        DataType := ftInteger;
        Required := true;
      end;
      { Contact ID }
      with AddFieldDef do begin
        Name := 'ContactID';
        DataType := ftInteger;
        Required := true;
      end;
    end; {with FieldDefs do}
  end;
end;
{=====}

function TVpDBISAMDataStore.GetDirectory: String;
begin
  Result := FDatabase.Directory;
end;
{=====}

procedure TVpDBISAMDataStore.SetDirectory(const AValue: String);
begin
  FDatabase.Directory := AValue;
end;

end.
