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

unit ExFlexSimplePlannerU1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  VpDlg, VpResEditDlg, StdCtrls, VpTaskList, VpMonthView, VpBase, VpBaseDS,
  VpDayView, VpDBDS, ComCtrls, ExtCtrls, VpContactGrid, VpConst,
  VpTaskEditDlg, VpData, VpCalendar, Db, DBTables, VpFlxDS, VpException;

type
  TFrmSimplePlanner = class(TForm)
    VpControlLink1: TVpControlLink;
    VpResourceEditDialog1: TVpResourceEditDialog;
    Panel1: TPanel;
    VpDayView1: TVpDayView;
    Panel2: TPanel;
    Splitter1: TSplitter;
    VpMonthView1: TVpMonthView;
    Splitter2: TSplitter;
    Panel3: TPanel;
    VpResourceCombo1: TVpResourceCombo;
    Button1: TButton;
    TrackBar1: TTrackBar;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    VpTaskList1: TVpTaskList;
    VpContactGrid1: TVpContactGrid;
    VpTaskEditDialog1: TVpTaskEditDialog;
    Button2: TButton;
    Button3: TButton;
    TabSheet3: TTabSheet;
    VpCalendar1: TVpCalendar;
    GranularityCombo: TComboBox;
    CheckBox1: TCheckBox;
    VpFlexDataStore1: TVpFlexDataStore;
    ResourceTable: TTable;
    EventsTable: TTable;
    ContactsTable: TTable;
    TasksTable: TTable;
    ResourceDS: TDataSource;
    EventsDS: TDataSource;
    ContactsDS: TDataSource;
    TasksDS: TDataSource;
    RecordIDTable: TTable;
    procedure Button1Click(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure GranularityComboChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    function VpFlexDataStore1GetNextID(Sender: TObject;
      TableName: String): Integer;
    procedure VpFlexDataStore1Connect(Sender: TObject);
    procedure VpFlexDataStore1CreateTable(Sender: TObject;
      TableName: String);
  private
    procedure CreateRecordIDTable;
    procedure InitializeRecordIDTable;
    procedure CreateFieldDefs(const TableName: string; FieldDefs: TFieldDefs);
    procedure CreateIndexDefs(const TableName: string; IndexDefs: TIndexDefs);
  public

  end;

var
  FrmSimplePlanner: TFrmSimplePlanner;

implementation

{$R *.DFM}

procedure TFrmSimplePlanner.Button1Click(Sender: TObject);
begin
  VpResourceEditDialog1.AddNewResource;
end;

procedure TFrmSimplePlanner.TrackBar1Change(Sender: TObject);
begin
  VpDayView1.NumDays := TrackBar1.Position;
end;

procedure TFrmSimplePlanner.Button2Click(Sender: TObject);
begin
  VpTaskEditDialog1.Execute(VpFlexDataStore1.Resource.Tasks.GetTask(VpTaskList1.TaskIndex));
end;

procedure TFrmSimplePlanner.Button3Click(Sender: TObject);
var
  Task: TVpTask;

begin
  Task := VpFlexDataStore1.Resource.Tasks.AddTask(VpFlexDataStore1.GetNextID('Tasks'));
  if VpTaskEditDialog1.Execute(Task) then
    VpFlexDataStore1.PostTasks;
end;

procedure TFrmSimplePlanner.GranularityComboChange(Sender: TObject);
begin
  case GranularityCombo.ItemIndex of
  0: VpDayView1.Granularity := GR05Min;
  1: VpDayView1.Granularity := GR06Min;
  2: VpDayView1.Granularity := GR10Min;
  3: VpDayView1.Granularity := GR15Min;
  4: VpDayView1.Granularity := GR20Min;
  5: VpDayView1.Granularity := GR30Min;
  6: VpDayView1.Granularity := GR60Min;
  end;
end;

procedure TFrmSimplePlanner.FormCreate(Sender: TObject);
begin
  GranularityCombo.ItemIndex := 5;
end;

procedure TFrmSimplePlanner.CheckBox1Click(Sender: TObject);
begin
  VpDayView1.IncludeWeekends := CheckBox1.Checked;
end;

function TFrmSimplePlanner.VpFlexDataStore1GetNextID(Sender: TObject;
  TableName: String): Integer;
var
  Query: TQuery;
  GotIt: Boolean;
  ID   : Integer;
  FieldName: string;
begin
  { This demo uses regular old Paradox tables.  I could have used regular  }
  { AutIncrementing fields for the indexes, but I chose to do it the hard  }
  { way as a demonstration of how to safely generate unique record ID's in }
  { a multi0user environment }

  { New record ID's are created here and then the Record ID table is         }
  { immediately updated to reflect the new value.  If the table is           }
  { unsuccessfully updated, then it is assumed that another user has claimed }
  { that ID, so the ID is incremented and another attempt is made, until we  }
  { are successful.                                                          }

  { First, check to see if the record ID table exists, and if not, create it }
  CreateRecordIDTable;

  Query := TQuery.Create(self);
  ID := 0;
  try
    Query.DatabaseName := ResourceTable.DatabaseName;

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

procedure TFrmSimplePlanner.VpFlexDataStore1Connect(Sender: TObject);
begin
  { Check to see if the RecordID table exists and if not, create it. }
  CreateRecordIDTable;
end;

procedure TFrmSimplePlanner.CreateRecordIDTable;
var
  RecTable: TTable;
begin
  { Check to see if the RecordID table exists and if not, create it. }
  RecTable := TTable.Create(self);
  RecTable.DatabaseName := ResourceTable.DatabaseName;
  RecTable.TableName := RecordIDTableName;
  try
    try
      RecTable.Open;
    except
      CreateFieldDefs(RecordIDTableName, RecTable.FieldDefs);
      CreateIndexDefs(RecordIDTableName, RecTable.IndexDefs);
      RecTable.CreateTable;
      InitializeRecordIDTable;
    end;
  finally
    RecTable.Free;
  end;
end;


procedure TFrmSimplePlanner.InitializeRecordIDTable;
var
  Qry: TQuery;
  ID: Integer;
begin
  Qry := TQuery.Create(self);
  try
    Qry.DatabaseName := ResourceTable.DatabaseName;

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

procedure TFrmSimplePlanner.VpFlexDataStore1CreateTable(Sender: TObject;
  TableName: String);
var
  Table: TTable;
begin
  if TableName = ResourceTableName then
    Table := TTable(ResourceTable)
    { I cast it to show you how, in case you }
    { want to use a TQuery with the BDE      }

  else if TableName = EventsTableName then
    Table := TTable(EventsTable)

  else if TableName = ContactsTableName then
    Table := TTable(ContactsTable)

  else if TableName = TasksTableName then
    Table := TTable(TasksTable);

  CreateFieldDefs(TableName, Table.FieldDefs);
  CreateIndexDefs(TableName, Table.IndexDefs);

  if Table <> nil then
    Table.CreateTable;
end;

procedure TFrmSimplePlanner.CreateFieldDefs(const TableName: string;
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
        Size := 255;
        Required := false;
      end;
      { Notes }
      with AddFieldDef do begin
        Name := 'Notes';
        DataType := ftString;
        Size := 1024;
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
        Required := false;
      end;
      { UserField1 }
      with AddFieldDef do begin
        Name := 'UserField1';
        DataType := ftString;
        Required := false;
      end;
      { UserField2 }
      with AddFieldDef do begin
        Name := 'UserField2';
        DataType := ftString;
        Required := false;
      end;
      { UserField3 }
      with AddFieldDef do begin
        Name := 'UserField3';
        DataType := ftString;
        Required := false;
      end;
      { UserField4 }
      with AddFieldDef do begin
        Name := 'UserField4';
        DataType := ftString;
        Required := false;
      end;
      { UserField5 }
      with AddFieldDef do begin
        Name := 'UserField5';
        DataType := ftString;
        Required := false;
      end;
      { UserField6 }
      with AddFieldDef do begin
        Name := 'UserField6';
        DataType := ftString;
        Required := false;
      end;
      { UserField7 }
      with AddFieldDef do begin
        Name := 'UserField7';
        DataType := ftString;
        Required := false;
      end;
      { UserField8 }
      with AddFieldDef do begin
        Name := 'UserField8';
        DataType := ftString;
        Required := false;
      end;
      { UserField9 }
      with AddFieldDef do begin
        Name := 'UserField9';
        DataType := ftString;
        Required := false;
      end;
    end; {with FieldDefs do}
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
      { Repeat Range End }
      with AddFieldDef do begin
        Name := 'RepeatRangeEnd';
        DataType := ftDateTime;
        Required := false;
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
        Size := 255;
        Required := false;
      end;
      { Note }
      with AddFieldDef do begin
        Name := 'Notes';
        DataType := ftString;
        Size := 1024;
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
        Size := 255;
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
        DataType := ftInteger;
        Required := false;
      end;
      { SnoozeTime }
      with AddFieldDef do begin
        Name := 'SnoozeTime';
        DataType := ftDateTime;
        Required := false;
      end;
      { Repeat Code }
      with AddFieldDef do begin
        Name := 'RepeatCode';
        DataType := ftInteger;
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
        Required := false;
      end;
      { UserField1 }
      with AddFieldDef do begin
        Name := 'UserField1';
        DataType := ftString;
        Required := false;
      end;
      { UserField2 }
      with AddFieldDef do begin
        Name := 'UserField2';
        DataType := ftString;
        Required := false;
      end;
      { UserField3 }
      with AddFieldDef do begin
        Name := 'UserField3';
        DataType := ftString;
        Required := false;
      end;
      { UserField4 }
      with AddFieldDef do begin
        Name := 'UserField4';
        DataType := ftString;
        Required := false;
      end;
      { UserField5 }
      with AddFieldDef do begin
        Name := 'UserField5';
        DataType := ftString;
        Required := false;
      end;
      { UserField6 }
      with AddFieldDef do begin
        Name := 'UserField6';
        DataType := ftString;
        Required := false;
      end;
      { UserField7 }
      with AddFieldDef do begin
        Name := 'UserField7';
        DataType := ftString;
        Required := false;
      end;
      { UserField8 }
      with AddFieldDef do begin
        Name := 'UserField8';
        DataType := ftString;
        Required := false;
      end;
      { UserField9 }
      with AddFieldDef do begin
        Name := 'UserField9';
        DataType := ftString;
        Required := false;
      end;
    end; {with FieldDefs do}
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
        DataType := ftString;
        Size := 1024;
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
        Required := false;
      end;
      { UserField1 }
      with AddFieldDef do begin
        Name := 'UserField1';
        DataType := ftString;
        Required := false;
      end;
      { UserField2 }
      with AddFieldDef do begin
        Name := 'UserField2';
        DataType := ftString;
        Required := false;
      end;
      { UserField3 }
      with AddFieldDef do begin
        Name := 'UserField3';
        DataType := ftString;
        Required := false;
      end;
      { UserField4 }
      with AddFieldDef do begin
        Name := 'UserField4';
        DataType := ftString;
        Required := false;
      end;
      { UserField5 }
      with AddFieldDef do begin
        Name := 'UserField5';
        DataType := ftString;
        Required := false;
      end;
      { UserField6 }
      with AddFieldDef do begin
        Name := 'UserField6';
        DataType := ftString;
        Required := false;
      end;
      { UserField7 }
      with AddFieldDef do begin
        Name := 'UserField7';
        DataType := ftString;
        Required := false;
      end;
      { UserField8 }
      with AddFieldDef do begin
        Name := 'UserField8';
        DataType := ftString;
        Required := false;
      end;
      { UserField9 }
      with AddFieldDef do begin
        Name := 'UserField9';
        DataType := ftString;
        Required := false;
      end;
    end; {with FieldDefs do}
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
        Size := 255;
        Required := false;
      end;
      { Details }
      with AddFieldDef do begin
        Name := 'Details';
        DataType := ftString;
        Size := 1024;
        Required := false;
      end;
      { Created On Date }
      with AddFieldDef do begin
        Name := 'CreatedOn';
        DataType := ftDateTime;
        Required := false;
      end;
      { Completed On Date }
      with AddFieldDef do begin
        Name := 'CompletedOn';
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
        Required := false;
      end;
      { UserField1 }
      with AddFieldDef do begin
        Name := 'UserField1';
        DataType := ftString;
        Required := false;
      end;
      { UserField2 }
      with AddFieldDef do begin
        Name := 'UserField2';
        DataType := ftString;
        Required := false;
      end;
      { UserField3 }
      with AddFieldDef do begin
        Name := 'UserField3';
        DataType := ftString;
        Required := false;
      end;
      { UserField4 }
      with AddFieldDef do begin
        Name := 'UserField4';
        DataType := ftString;
        Required := false;
      end;
      { UserField5 }
      with AddFieldDef do begin
        Name := 'UserField5';
        DataType := ftString;
        Required := false;
      end;
      { UserField6 }
      with AddFieldDef do begin
        Name := 'UserField6';
        DataType := ftString;
        Required := false;
      end;
      { UserField7 }
      with AddFieldDef do begin
        Name := 'UserField7';
        DataType := ftString;
        Required := false;
      end;
      { UserField8 }
      with AddFieldDef do begin
        Name := 'UserField8';
        DataType := ftString;
        Required := false;
      end;
      { UserField9 }
      with AddFieldDef do begin
        Name := 'UserField9';
        DataType := ftString;
        Required := false;
      end;
    end; {with FieldDefs do}
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
      { UserField0 }
      with AddFieldDef do begin
        Name := 'UserField0';
        DataType := ftString;
        Required := false;
      end;
      { UserField1 }
      with AddFieldDef do begin
        Name := 'UserField1';
        DataType := ftString;
        Required := false;
      end;
      { UserField2 }
      with AddFieldDef do begin
        Name := 'UserField2';
        DataType := ftString;
        Required := false;
      end;
      { UserField3 }
      with AddFieldDef do begin
        Name := 'UserField3';
        DataType := ftString;
        Required := false;
      end;
      { UserField4 }
      with AddFieldDef do begin
        Name := 'UserField4';
        DataType := ftString;
        Required := false;
      end;
      { UserField5 }
      with AddFieldDef do begin
        Name := 'UserField5';
        DataType := ftString;
        Required := false;
      end;
      { UserField6 }
      with AddFieldDef do begin
        Name := 'UserField6';
        DataType := ftString;
        Required := false;
      end;
      { UserField7 }
      with AddFieldDef do begin
        Name := 'UserField7';
        DataType := ftString;
        Required := false;
      end;
      { UserField8 }
      with AddFieldDef do begin
        Name := 'UserField8';
        DataType := ftString;
        Required := false;
      end;
      { UserField9 }
      with AddFieldDef do begin
        Name := 'UserField9';
        DataType := ftString;
        Required := false;
      end;
    end; {with FieldDefs do}
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

procedure TFrmSimplePlanner.CreateIndexDefs(const TableName: string;
  IndexDefs: TIndexDefs);
begin
  if TableName = ResourceTableName then begin
    with IndexDefs do begin
      with AddIndexDef do begin
        Name := '';
        Fields := 'ResourceID';
        Options := [ixPrimary];
      end;
      with AddIndexDef do begin
        Name := 'Descr_ndx';
        Fields := 'Description';
        Options := [ixCaseInsensitive];
      end;
    end;
  end else if TableName = EventsTableName then begin
    with IndexDefs do begin
      with AddIndexDef do begin
        Name := '';
        Fields := 'RecordID';
        Options := [ixPrimary];
      end;
      with AddIndexDef do begin
        Name := 'ResID_ndx';
        Fields := 'ResourceID';
        Options := [ixCaseInsensitive];
      end;
      with AddIndexDef do begin
        Name := 'st_ndx';
        Fields := 'StartTime';
        Options := [ixCaseInsensitive];
      end;
      with AddIndexDef do begin
        Name := 'et_ndx';
        Fields := 'EndTime';
        Options := [ixCaseInsensitive];
      end;
    end;
  end else if TableName = ContactsTableName then begin
    with IndexDefs do begin
      with AddIndexDef do begin
        Name := '';
        Fields := 'RecordID';
        Options := [ixPrimary];
      end;
      with AddIndexDef do begin
        Name := 'ResID_ndx';
        Fields := 'ResourceID';
        Options := [ixCaseInsensitive];
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
      with AddIndexDef do begin
        Name := '';
        Fields := 'RecordID';
        Options := [ixPrimary];
      end;
      with AddIndexDef do begin
        Name := 'ResID_ndx';
        Fields := 'ResourceID';
        Options := [ixCaseInsensitive];
      end;
      { Rule: A single field case sensitive index on a Paradox table must }
      {       have the same name as the field the index is on.            }
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

end.
