{*********************************************************}
{*                  VPADVDS.PAS 1.03                     *}
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

unit VpAdvDS;
  { Advantage Database DataStore Component }

interface

uses
  Windows, Classes, Dialogs, SysUtils, Db, DbTables, adstable,
  VpBase, VpData, VpSR, VpBaseDS, VpDBDS, VpException;

type
  TVpAdvDataStore = class(TVpCustomDBDataStore)
  protected{private}
    FDatabaseName    : string;
    FResourceTable   : TAdsQuery;
    FEventsTable     : TAdsQuery;
    FContactsTable   : TAdsQuery;
    FTasksTable      : TAdsQuery;
    FRecordIDTable   : TAdsQuery;
    FParams          : TStrings;
    { property getters }
    function GetConnected: Boolean;
    function GetDatabaseName: string;

    { anscestor property getters }
    function GetResourceTable : TDataset; override;
    function GetEventsTable : TDataset; override;
    function GetContactsTable : TDataset; override;
    function GetTasksTable : TDataset; override;

    { property setters }
    procedure InitializeRecordIDTable;
    procedure SetConnected(const Value: boolean); override;
    procedure SetDatabaseName(const Value: string);
    procedure SetParams(const Value: TStrings);
    procedure SetFilterCriteria(aTable : TDataset; aUseDateTime : Boolean;
      aResourceID : Integer; aStartDateTime : TDateTime;
      aEndDateTime : TDateTime); override;

    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function GetNextID(TableName: string): integer; override;
    procedure Load; override;
    procedure CreateTable(TableName: string);
    procedure CreateIndexDefs(const TableName : string;
                                    IndexDefs : TIndexDefs); override;

    procedure PostResources; override;
    procedure PostEvents; override;
    procedure PostContacts; override;
    procedure PostTasks; override;

    procedure PurgeResource(Res: TVpResource); override;
    procedure PurgeEvents(Res: TVpResource); override;
    procedure PurgeContacts(Res: TVpResource); override;
    procedure PurgeTasks(Res: TVpResource); override;

  published
    property AutoConnect;
    property AutoCreate;
    property DatabaseName : string
      read GetDatabaseName write SetDatabaseName; 

    { properties }
    property DayBuffer;
    property ResourceID;
    property Params : TStrings read FParams write SetParams;
    property ReadOnly;
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
{ TVpAdvDataStore }

constructor TVpAdvDataStore.Create(AOwner: TComponent);
begin
  inherited;

  FParams := TStringList.Create;

  FConnected := false;
  FParams.Clear;
  FResourceID := 0;

  FResourceTable := TAdsQuery.Create(self);
  FResourceTable.RequestLive := true;
  FResourceTable.SQL.Text := 'SELECT * FROM ' + ResourceTableName;

  FEventsTable := TAdsQuery.Create(self);
  FEventsTable.RequestLive := true;
  FEventsTable.SQL.Text := 'SELECT * FROM ' + EventsTableName
  + ' WHERE (ResourceID = :ResID)'
  + ' AND (StartTime >= :STime AND EndTime <= :ETime)'
  + ' OR (RepeatCode > 0 AND :STime <= RepeatRangeEnd)';

  FContactsTable := TAdsQuery.Create(self);
  FContactsTable.RequestLive := true;
  FContactsTable.SQL.Text := 'SELECT * FROM ' + ContactsTableName
  + ' WHERE ResourceID = :ResID';

  FTasksTable := TAdsQuery.Create(self);
  FTasksTable.RequestLive := true;
  FTasksTable.SQL.Text := 'SELECT * FROM ' + TasksTableName
  + ' WHERE ResourceID = :ResID';

  FRecordIDTable := TAdsQuery.Create(self);
  FRecordIDTable.RequestLive := true;
end;
{=====}

destructor TVpAdvDataStore.Destroy;
begin
  FParams.Free;

  { free tables }
  FResourceTable.Close;
  FResourceTable.Free;
  FEventsTable.Close;
  FEventsTable.Free;
  FContactsTable.Close;
  FContactsTable.Free;
  FTasksTable.Close;
  FTasksTable.Free;
  FRecordIDTable.Close;
  FRecordIDTable.Free;

  inherited;
end;
{=====}

function TVpAdvDataStore.GetConnected: Boolean;
begin
  { If the resource table is active, then we can be considered "Connected" }
  result := FResourceTable.Active;
end;
{=====}

function TVpAdvDataStore.GetDatabaseName: string;
begin
  result := FDatabaseName;
end;
{=====}

function TVpAdvDataStore.GetResourceTable : TDataset;
begin
  Result := FResourceTable;
end;
{=====}

function TVpAdvDataStore.GetEventsTable : TDataset;
begin
  Result := FEventsTable;
end;
{=====}

function TVpAdvDataStore.GetContactsTable : TDataset;
begin
  Result := FContactsTable
end;
{=====}

function TVpAdvDataStore.GetTasksTable : TDataset;
begin
  Result := FTasksTable;
end;
{=====}

procedure TVpAdvDataStore.Load;
begin
//  if not Connected then exit;

  FResourceTable.Close;
  FEventsTable.Close;
  FContactsTable.Close;
  FTasksTable.Close;

  inherited;
end;
{=====}

function TVpAdvDataStore.GetNextID(TableName: string): Integer;
var
  Query: TAdsQuery;
  GotIt: Boolean;
  Attempts : Word;
  ID   : Integer;
  FieldName: string;
begin
  { The BDEDataStore uses a support table called RecordIDS, or whatever is   }
  { defined in the RecordIDTableName constant.  It has one record, and is    }
  { used to keep track of the last ID used for each table.                   }

  { In a multi-user environment, This prevents collisions between two users  }
  { who happen to enter the same type of new record at the same time.        }

  { New record ID's are created here and then the Record ID table is         }
  { immediately updated to reflect the new value.  If the table is           }
  { unsuccessfully updated, then it is assumed that another user has claimed }
  { that ID, so the ID is incremented and another attempt is made, until we  }
  { are successful.                                                          }

  Query := TAdsQuery.Create(self);
  Query.RequestLive := true;
  ID := 0;
  Attempts := 0;
  try
    Query.DatabaseName := FDatabaseName;

    Query.Sql.Text := 'Select * from ' + RecordIDTableName;
    Query.Open;

    if TableName = ResourceTableName then begin
      FieldName := 'ResourceID';
      ID := Query.FieldByName('ResourceID').AsInteger;

    end else if TableName = TasksTableName then begin
      FieldName := 'TaskID';
      ID := Query.FieldByName('TaskID').AsInteger;

    end else if TableName = EventsTableName then begin
      FieldName := 'EventID';
      ID := Query.FieldByName('EventID').AsInteger;

    end else if TableName = ContactsTableName then begin
      FieldName := 'ContactID';
      ID := Query.FieldByName('ContactID').AsInteger;

    end else begin
      raise EInvalidTable.Create;
      Exit;
    end;

    Query.Close;
    Query.SQL.Text := 'Update ' + RecordIDTableName + ' Set ' + FieldName
      + ' = :NewID Where (' + FieldName + ' = :OldID)';

    GotIt := false;
    while (not GotIt) and (Attempts < 100) do begin
      Inc(ID);
      Query.ParamByName('NewID').AsInteger := ID;
      Query.ParamByName('OldID').AsInteger := ID - 1;
      Query.ExecSQL;

      GotIt := (Query.RowsAffected = 1);
      Inc(Attempts);
    end;

    if not GotIt then
      raise exception.Create('Error: Unable to update ' + RecordIDTableName);
  finally
    Query.Close;
    Query.Free;
  end;

  result := ID;
end;
{=====}

procedure TVpAdvDataStore.CreateTable(TableName: string);
var
  Table: TAdsTable;
begin
  Table := TAdsTable.Create(self);
  try
    Table.DatabaseName := FDatabaseName;

    if TableName = ResourceTableName then begin
      { Create Resources Table }
      Table.Active := false;
      Table.TableName := ResourceTableName;
    end

    else if TableName = EventsTableName then begin
      { Create Events Table }
      Table.Active := false;
      Table.TableName := EventsTableName;
    end

    else if TableName = ContactsTableName then begin
      { Create Contacts Table }
      Table.Active := false;
      Table.TableName := ContactsTableName;
    end

    else if TableName = TasksTableName then begin
      { Create Tasks Table }
      Table.Active := false;
      Table.TableName := TasksTableName;
    end

    else if TableName = RecordIDTableName then begin
      { Create Tasks Table }
      Table.Active := false;
      Table.TableName := RecordIDTableName;
    end;

    Table.DatabaseName := FDatabaseName;
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

procedure TVpAdvDataStore.InitializeRecordIDTable;
var
  Qry: TAdsQuery;
  ID: Integer;
begin
  Qry := TAdsQuery.Create(self);
  try
    Qry.DatabaseName := FDatabaseName;
    Qry.RequestLive := true;

    Qry.SQL.Text := 'Select * from ' + RecordIDTableName;
    Qry.Open;
    if Qry.RowsAffected < 1 then begin
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

procedure TVpAdvDataStore.Loaded;
begin
  inherited;
  if not (csDesigning in ComponentState) then
    Connected := AutoConnect;
end;
{=====}

procedure TVpAdvDataStore.PostResources;
var
  I: Integer;
  Resource: TVpResource;
  Qry: TAdsQuery;
begin
  if (Resources.Count > 0) then begin
    Qry := TAdsQuery.Create(self);
    Qry.DatabaseName := FDatabaseName;
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
      NotifyDependents;
    finally
      Qry.Close;
      Qry.Free;
    end;
  end;
end;
{=====}

procedure TVpAdvDataStore.PostEvents;
var
  I: Integer;
  Event: TVpEvent;
  Qry: TAdsQuery;
begin
  if (Resource <> nil) and Resource.EventsDirty then begin
    Qry := TAdsQuery.Create(self);
    try
      Qry.DatabaseName := FDatabaseName;
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
      Qry.Close;
      Qry.Free;
    end;
    Resource.EventsDirty := false;
  end;
end;
{=====}

procedure TVpAdvDataStore.PostContacts;
var
  I: Integer;
  Contact: TVpContact;
  Qry: TAdsQuery;
begin
  if (Resource <> nil) and Resource.ContactsDirty then begin
    { Dump this resource's dirty contacts to the DB }
    Qry := TAdsQuery.Create(self);
    try
      Qry.DatabaseName := FDatabaseName;
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
              { - begin}
              Qry.FieldByName('Birthdate').AsDateTime := Contact.BirthDate;
              Qry.FieldByName('Anniversary').AsDateTime := Contact.Anniversary;
              { - end}
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

            { - Modified}
            Qry.SQL.Text := 'INSERT INTO Contacts '
            + '(ResourceID, RecordID, FirstName, LastName, Birthdate, '
            + 'Anniversary, Title, Company, Job_Position, EMail, Address, '
            + 'City, State, Zip, Country, Note, Phone1, Phone2, Phone3, '
            + 'Phone4, Phone5, PhoneType1, PhoneType2, PhoneType3, PhoneType4, '
            + 'PhoneType5, Category, Custom1, Custom2, Custom3, Custom4, '
            + 'UserField0, UserField1, UserField2, UserField3, UserField4, '
            + 'UserField5, UserField6, UserField7, UserField8, UserField9 ) '

            + 'VALUES(:ResourceID, :RecordID, :FirstName, :LastName, '
            + ':Birthdate, :Anniversary, :Title, :Company, :Job_Position, '
            + ':EMail, :Address, :City, :State, :Zip, :Country, :Note, '
            + ':Phone1, :Phone2, :Phone3, :Phone4, :Phone5, :PhoneType1, '
            + ':PhoneType2, :PhoneType3, :PhoneType4, :PhoneType5, :Category, '
            + ':Custom1, :Custom2, :Custom3, :Custom4, :UserField0, '
            + ':UserField1, :UserField2, :UserField3, :UserField4, :UserField5, '
            + ':UserField6, :UserField7, :UserField8, :UserField9)';

            Qry.ParamByName('ResourceID').AsInteger := Resource.ResourceID;
            Qry.ParamByName('RecordID').AsInteger := Contact.RecordID;
            Qry.ParamByName('FirstName').AsString := Contact.FirstName;
            Qry.ParamByName('LastName').AsString := Contact.LastName;
            { - begin}
            Qry.ParamByName('Birthdate').AsDateTime := Contact.Birthdate;
            Qry.ParamByName('Anniversary').AsDateTime := Contact.Anniversary;
            { - end}
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
  end;
end;
{=====}

procedure TVpAdvDataStore.PostTasks;
var
  I: Integer;
  Task: TVpTask;
  Qry : TAdsQuery;
begin
  if (Resource <> nil) and Resource.TasksDirty then begin
    { Dump this resource's dirty contacts to the DB }
    Qry := TAdsQuery.Create(self);
    try
      Qry.DatabaseName := FDatabaseName;
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
  end;
end;
{=====}

procedure TVpAdvDataStore.PurgeResource(Res: TVpResource);
begin
  Resource.Deleted := true;
  PostResources;
  Load;
end;
{=====}

procedure TVpAdvDataStore.PurgeEvents(Res: TVpResource);
var
  Qry: TAdsQuery;
begin
  Qry := TAdsQuery.Create(self);
  try
    Qry.DatabaseName := FDataBaseName;

    Qry.SQL.Text := 'delete from Events where ResourceID = :ResID';
    Qry.ParamByName('ResID').AsInteger := Resource.ResourceID;
    Qry.ExecSQL;
  finally
    Qry.Free;
  end;
  Resource.Schedule.ClearEvents;
end;
{=====}

procedure TVpAdvDataStore.PurgeContacts(Res: TVpResource);
var
  Qry: TAdsQuery;
begin
  Qry := TAdsQuery.Create(self);
  try
    Qry.DatabaseName := FDataBaseName;

    Qry.SQL.Text := 'delete from Contacts where ResourceID = :ResID';
    Qry.ParamByName('ResID').AsInteger := Resource.ResourceID;
    Qry.ExecSQL;
  finally
    Qry.Free;
  end;
  Resource.Contacts.ClearContacts;
end;
{=====}

procedure TVpAdvDataStore.PurgeTasks(Res: TVpResource);
var
  Qry: TAdsQuery;
begin
  Qry := TAdsQuery.Create(self);
  try
    Qry.DatabaseName := FDataBaseName;

    Qry.SQL.Text := 'delete from Tasks where ResourceID = :ResID';
    Qry.ParamByName('ResID').AsInteger := Resource.ResourceID;
    Qry.ExecSQL;
  finally
    Qry.Free;
  end;
  Resource.Tasks.ClearTasks;
end;
{=====}

procedure TVpAdvDataStore.SetConnected(const Value: boolean);
var
  TmpTable: TAdsTable;
begin
  { disconnect if destroying }
  if csDestroying in ComponentState then begin
    Exit;
  end;

  { Don't connect at designtime }
  if csDesigning in ComponentState then Exit;

  { Don't try to connect until we're all loaded up }
  if csLoading in ComponentState then Exit;

  { we are attempting a disconnect }
  if not Value then begin
    FResourceTable.Close;
    FEventsTable.Close;
    FContactsTable.Close;
    FTasksTable.Close;
    FRecordIDTable.Close;
    Exit;
  end;

  { By default, put the data directory in the same place as the application }
  { executable. }
  if FDatabaseName = '' then
    FDatabaseName := ExtractFilePath(ParamStr(0)) + 'Data';

  if not DirectoryExists(FDatabaseName) then
    ForceDirectories(FDatabaseName);

  TmpTable := TAdsTable.Create(self);

  try
    TmpTable.DatabaseName := FDatabaseName;

    { Create / Open Resources Table}
    FResourceTable.DatabaseName := FDatabaseName;

    {See of the Resources table exists}
    TmpTable.TableName := ResourceTableName;
    if not TmpTable.Exists then
      CreateTable(ResourceTableName);

    try
      FResourceTable.Open;
    except
      if AutoCreate then begin
        CreateTable(ResourceTableName);
        FResourceTable.Open;
      end;
    end;

    { Create / Open Events Table }
    FEventsTable.DatabaseName := FDatabaseName;

    {See of the Events table exists}
    TmpTable.TableName := EventsTableName;
    if not TmpTable.Exists then
      CreateTable(EventsTableName);

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
    FContactsTable.DatabaseName := FDatabaseName;

    {See of the Contacts table exists}
    TmpTable.TableName := ContactsTableName;
    if not TmpTable.Exists then
      CreateTable(ContactsTableName);

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
    FTasksTable.DatabaseName := FDatabaseName;

    {See of the Tasks table exists}
    TmpTable.TableName := TasksTableName;
    if not TmpTable.Exists then
      CreateTable(TasksTableName);

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
    FRecordIDTable.DatabaseName := FDatabaseName;
    {See of the Resources table exists}
    TmpTable.TableName := RecordIDTableName;
    if not TmpTable.Exists then
      CreateTable(RecordIDTableName);

  finally
    TmpTable.Free;
  end;

  Load;

  inherited SetConnected(GetConnected);
end;
{=====}

procedure TVpAdvDataStore.SetDatabaseName(const Value: string);
var
  ConStatus: Boolean;
begin
  if FDatabaseName <> Value then begin
    ConStatus := Connected;
    Connected := false;
    FDatabaseName := Value;
    Connected := ConStatus;
  end;
end;
{=====}

procedure TVpAdvDataStore.SetParams(const Value: TStrings);
begin
  FParams.Assign(Value);
end;
{=====}

{ Called by the ancestor to properly filter the data for each table, }
{ based on the ResourceID, Date and DayBuffer values.                }
{ Each TVpCustomDBDataStore descendant should define their own       }
{ SetFilterCriteria procedure.                                       }
procedure TVpAdvDataStore.SetFilterCriteria(aTable : TDataset;
  aUseDateTime : Boolean; aResourceID : Integer; aStartDateTime : TDateTime;
  aEndDateTime : TDateTime);
var
  Qry: TAdsQuery;
begin
  Qry := (aTable as TAdsQuery);

  Qry.Close;

  Qry.ParamByName('ResID').AsInteger := aResourceID;                  

  if Qry = EventsTable then begin
    Qry.ParamByName('STime').AsDateTime := aStartDateTime;
    Qry.ParamByName('ETime').AsDateTime := aEndDateTime;
  end;

  Qry.Open;
end;
{=====}

procedure TVpAdvDataStore.CreateIndexDefs(const TableName: string;
  IndexDefs: TIndexDefs);
begin
  if TableName = ResourceTableName then begin
    with IndexDefs do begin
      Clear;
      { Paradox primary keys have no name }
      with AddIndexDef do begin
        Name := '';
        Fields := 'ResourceID';
        Options := [ixPrimary];
      end;
    end;
  end else if TableName = EventsTableName then begin
    with IndexDefs do begin
      Clear;
      { Paradox primary keys have no name }
      with AddIndexDef do begin
        Name := '';
        Fields := 'RecordID';
        Options := [ixUnique, ixPrimary];
      end;
    end;
  end else if TableName = ContactsTableName then begin
    with IndexDefs do begin
      Clear;
      { Paradox primary keys have no name }
      with AddIndexDef do begin
        Name := '';
        Fields := 'RecordID';
        Options := [ixPrimary];
      end;
    end;
  end else if TableName = TasksTableName then begin
    with IndexDefs do begin
      Clear;
      { Paradox primary keys have no name }
      with AddIndexDef do begin
        Name := '';
        Fields := 'RecordID';
        Options := [ixPrimary];
      end;
    end;
  end;

  inherited CreateIndexDefs(TableName, IndexDefs);
end;
{=====}

end.
