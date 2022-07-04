{*********************************************************}
{*                  VPFLXDS.PAS 1.03                     *}
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

unit VpFlxDS;
  { Flexible DataStore Component }

interface

uses
  Windows, Classes, Dialogs, SysUtils, Db, DbTables,
  VpBase, VpData, VpSR, VpDBDS, VpBaseDS, VpException;

type
  {Forward Declarations}
  TVpFlexDataStore = class;

  TVpGetNextIDEvent = function(Sender: TObject;
    TableName: String): Integer of object;

  TVpTableEvent = procedure(Sender: TObject; TableName: String) of object;

  TVpSetFilterCriteriaEvent = procedure(aTable: TDataset;
  aUseDateTime: Boolean; aResourceID: Integer; aStartDateTime,
  aEndDateTime: TDateTime) of object;

  TVpFieldMapping = class(TCollectionItem)                               
  public                                                                 
    VPField: string;                                                     
    DBField: string;                                                     
  end;                                                                   

  { The TVpDataSources class is simply for clustering the FlexDataStore's }
  { DataSources together in the Object Inspector                           }
  TVpDataSources = class(TPersistent)
  protected
    FOwner : TVpFlexDataStore;
    {setters}
    procedure SetResourceDataSrc(const Value: TDataSource);
    procedure SetContactsDataSrc(const Value: TDataSource);
    procedure SetEventsDataSrc(const Value: TDataSource);
    procedure SetTasksDataSrc(const Value: TDataSource);
    {getters}
    function GetResourceDataSrc: TDataSource;
    function GetContactsDataSrc: TDataSource;
    function GetEventsDataSrc: TDataSource;
    function GetTasksDataSrc: TDataSource;

  public
    constructor Create(Owner: TVpFlexDataStore);
  published
    property ResourceDataSource : TDataSource
     read GetResourceDataSrc write SetResourceDataSrc;
    property EventsDataSource   : TDataSource
      read GetEventsDataSrc write SetEventsDataSrc;
    property ContactsDataSource : TDataSource
      read GetContactsDataSrc write SetContactsDataSrc;
    property TasksDataSource    : TDataSource
      read GetTasksDataSrc write SetTasksDataSrc;
  end;

  TVpFlexDataStore = class(TVpCustomDbDataStore)
  protected{private}
    {Data Sources}
    FDataSources     : TVpDataSources;
    FResourceDataSrc : TDataSource;
    FEventsDataSrc   : TDataSource;
    FContactsDataSrc : TDataSource;
    FTasksDataSrc    : TDataSource;
    FResourceMappings: TCollection;                                      
    FEventMappings   : TCollection;                                      
    FContactMappings : TCollection;                                      
    FTaskMappings    : TCollection;                                      
    FDriverName      : string;
    FLoginPrompt     : boolean;
    FReadOnly        : boolean;
    FSessionName     : string;
    FOnGetNextID     : TVpGetNextIDEvent;
    FOnSetFilterCriteria: TVpSetFilterCriteriaEvent;                     
    FOnCreateTable   : TVpTableEvent;
    { property getters }
    function GetConnected: Boolean;
    function GetResourceTable : TDataset; override;
    function GetEventsTable : TDataset; override;
    function GetContactsTable : TDataset; override;
    function GetTasksTable : TDataset; override;
    { property setters }
    procedure SetConnected(const Value: boolean); override;
    procedure SetResourceDataSrc(Value: TDataSource);
    procedure SetEventsDataSrc(Value: TDataSource);
    procedure SetContactsDataSrc(Value: TDataSource);
    procedure SetTasksDataSrc(Value: TDataSource);
    {streamers}                                                          
    procedure DefineProperties(Filer: TFiler); override;                 
    procedure LoadResMapping(Reader: TReader);                           
    procedure StoreResMapping(Writer: TWriter);                          
    procedure LoadEventMapping(Reader: TReader);                         
    procedure StoreEventMapping(Writer: TWriter);                        
    procedure LoadContactMapping(Reader: TReader);                       
    procedure StoreContactMapping(Writer: TWriter);                      
    procedure LoadTaskMapping(Reader: TReader);                          
    procedure StoreTaskMapping(Writer: TWriter);                         
    { Internal Methods }
    procedure Loaded; override;
    procedure SetFilterCriteria(aTable : TDataset;                       
      aUseDateTime : Boolean;                                            
      aResourceID : Integer;                                             
      aStartDateTime : TDateTime;                                        
      aEndDateTime : TDateTime); override;                               
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Load; override;

    procedure LoadEvents; override;                                      
    procedure LoadContacts; override;                                    
    procedure LoadTasks; override;                                       
    procedure RefreshEvents; override;                                   
    procedure RefreshContacts; override;                                 
    procedure RefreshTasks; override;                                    
    procedure RefreshResource; override;                                 
    procedure PostEvents; override;                                      
    procedure PostContacts; override;                                    
    procedure PostTasks; override;                                       
    procedure PostResources; override;                                   
    procedure PurgeResource(Res: TVpResource); override;                 
    procedure PurgeEvents(Res: TVpResource); override;                   
    procedure PurgeContacts(Res: TVpResource); override;                 
    procedure PurgeTasks(Res: TVpResource); override;                    
    function GetFieldName(Mappings: TCollection;                         
      VPField: string): string;                                          

    function GetNextID(TableName: string): Integer; override;

    { These are published via the TVpDataSources class, which allows them to }
    { be clustered in the Object Inspector Don't publish them individually   }
    property ResourceDataSource : TDataSource
      read FResourceDataSrc write SetResourceDataSrc;
    property EventsDataSource   : TDataSource
      read FEventsDataSrc write SetEventsDataSrc;
    property ContactsDataSource : TDataSource
      read FContactsDataSrc write SetContactsDataSrc;
    property TasksDataSource    : TDataSource
      read FTasksDataSrc write SetTasksDataSrc;

    property ResourceMappings: TCollection                               
      read FResourceMappings;                                            
    property EventMappings : TCollection                                 
      read FEventMappings;                                               
    property ContactMappings : TCollection                               
      read FContactMappings;                                             
    property TaskMappings : TCollection                                  
      read FTaskMappings;                                                

  published
    property AutoConnect;
    { properties }
    property DayBuffer;
    property DataSources: TVpDataSources
      read FDataSources write FDataSources;
    property ResourceID;
    property OnGetNextID: TVpGetNextIDEvent
      read FOnGetNextID write FOnGetNextID;
    property OnCreateTable: TVpTableEvent
     read FOnCreateTable write FOnCreateTable;
    property OnSetFilterCriteria: TVpSetFilterCriteriaEvent
      read FOnSetFilterCriteria write FOnSetFilterCriteria;
  end;

implementation

uses
{$IFDEF VERSION6} Variants, {$ELSE} FileCtrl, {$ENDIF} VpConst;

(*****************************************************************************)
{ TVpFlexDataStore }

constructor TVpFlexDataStore.Create(AOwner: TComponent);
begin
  inherited;
  FResourceMappings := TCollection.Create(TVpFieldMapping);              
  FEventMappings    := TCollection.Create(TVpFieldMapping);              
  FContactMappings  := TCollection.Create(TVpFieldMapping);              
  FTaskMappings     := TCollection.Create(TVpFieldMapping);              
  FDataSources  := TVpDataSources.Create(self);
  FConnected := false;
  FResourceID := 0;
end;
{=====}

destructor TVpFlexDataStore.Destroy;
begin
  FResourceMappings.Free;                                                
  FEventMappings.Free;                                                   
  FContactMappings.Free;                                                 
  FTaskMappings.Free;                                                    
  FDataSources.Free;
  inherited;
end;
{=====}

function TVpFlexDataStore.GetConnected: Boolean;
var
  AllAssigned, AllActive: Boolean;
begin
  AllActive   := false;
  AllAssigned := (FResourceDataSrc.DataSet <> nil)
    and (FEventsDataSrc.DataSet <> nil)
    and (FContactsDataSrc.DataSet <> nil)
    and (FTasksDataSrc.DataSet <> nil);

  if AllAssigned then
    AllActive := FResourceDataSrc.DataSet.Active
      and FEventsDataSrc.DataSet.Active
      and FContactsDataSrc.DataSet.Active
      and FTasksDataSrc.DataSet.Active;

  result := AllAssigned and AllActive;
end;
{=====}

function TVpFlexDataStore.GetResourceTable : TDataset;
begin
  result := nil;
  if (FResourceDataSrc <> nil)
  and (FResourceDataSrc.DataSet <> nil)
  then result := FResourceDataSrc.DataSet;
end;
{=====}

function TVpFlexDataStore.GetEventsTable : TDataset;
begin
  result := nil;
  if (FEventsDataSrc <> nil)
  and (FEventsDataSrc.DataSet <> nil)
  then result := FEventsDataSrc.DataSet;
end;
{=====}

function TVpFlexDataStore.GetContactsTable : TDataset;
begin
  result := nil;
  if (FContactsDataSrc <> nil)
  and (FContactsDataSrc.DataSet <> nil)
  then result := FContactsDataSrc.DataSet;
end;
{=====}

function TVpFlexDataStore.GetTasksTable : TDataset;
begin
  result := nil;
  if (FTasksDataSrc <> nil)
  and (FTasksDataSrc.DataSet <> nil)
  then result := FTasksDataSrc.DataSet;
end;
{=====}

procedure TVpFlexDataStore.SetConnected(const Value: boolean);
begin
  { disconnect if destroying }
  if csDestroying in ComponentState then Exit;
  { Don't connect at designtime }
  if csDesigning in ComponentState then Exit;
  { Don't try to connect until we're all loaded up }
  if csLoading in ComponentState then Exit;

  { Make sure that all DataSources are properly assigned }
  if (FResourceDataSrc = nil)                                            
    and (FEventsDataSrc = nil)                                           
    and (FContactsDataSrc = nil)                                         
    and (FTasksDataSrc = nil)                                            
  then Exit;                                                             

  if (FResourceDataSrc.Dataset = nil)                                    
    and (FEventsDataSrc.Dataset = nil)                                   
    and (FContactsDataSrc.Dataset = nil)                                 
    and (FTasksDataSrc.Dataset = nil)                                    
  then Exit;                                                             

  if Value then begin
    { try to open the tables one at a time.  If they fail, and AutoCreate is }
    { true then we will attempt to create the tables on the fly.             }

    { Open Resources Table}
    if ResourceTable <> nil then begin                                   
      try
        FResourceDataSrc.DataSet.Open;
      except
        if Assigned(OnCreateTable) then begin
          OnCreateTable(Self, ResourceTableName);
        end;
        try
          FResourceDataSrc.DataSet.Open;
        except
          raise Exception.Create(RSUnableToOpen + ResourceTableName);
        end;
      end;
    end;                                                                 

    { Open Events Table }
    if EventsTable <> nil then begin                                     
      try
        FEventsDataSrc.DataSet.Open;
      except
        if Assigned(OnCreateTable) then begin
          OnCreateTable(Self, EventsTableName);
        end;
        try
          FEventsDataSrc.DataSet.Open;
        except
          raise Exception.Create(RSUnableToOpen + EventsTableName);
        end;
      end;
    end;                                                                 

    { Open Contacts Table }
    if ContactsTable <> nil then begin                                   
      try
        FContactsDataSrc.DataSet.Open;
      except
        if Assigned(OnCreateTable) then begin
          OnCreateTable(Self, ContactsTableName);
        end;
        try
          FContactsDataSrc.DataSet.Open;
        except
          raise Exception.Create(RSUnableToOpen + ContactsTableName);
        end;
      end;
    end;                                                                 

    { Open Tasks Table }
    if TasksTable <> nil then begin
      try                                                                
        FTasksDataSrc.DataSet.Open;
      except
        if Assigned(OnCreateTable) then begin
          OnCreateTable(Self, TasksTableName);
        end;
        try
          FTasksDataSrc.DataSet.Open;
        except
          raise Exception.Create(RSUnableToOpen + TasksTableName);
        end;
      end;
    end;                                                                 

    Load;                                                                
  end                                                                    

//  Load;                                                                

  else begin                                                             
     if FResourceDataSrc <> nil then
       FResourceDataSrc.DataSet.Close;
     if FEventsDataSrc <> nil then
       FEventsDataSrc.DataSet.Close;
     if FContactsDataSrc <> nil then
       FContactsDataSrc.DataSet.Close;
     if FTasksDataSrc <> nil then
       FTasksDataSrc.DataSet.Close;
  end;

  inherited;
end;
{=====}

procedure TVpFlexDataStore.Load;
var
  Res        : TVpResource;
  {FieldName}
  FN: string;
begin
  if (csLoading in ComponentState) then
    Exit;

  Loading := true;
  try
    if (ResourceTable <> nil) then
      ResourceTable.Open
    else
      Exit;

    if ResourceTable.Active then begin
      Resource := nil;
      Resources.ClearResources;

      with ResourceTable do begin
        First;
        while not EOF do begin
          { Load this resource into memory }
          Res := Resources.AddResource(-1);
          Res.Loading := true;
          FN := GetFieldName(FResourceMappings, 'ResourceID');
          if FN <> '' then
            Res.ResourceID := ResourceTable.FieldByName(FN).AsInteger;

          FN := GetFieldName(FResourceMappings, 'Description');
          if FN <> '' then
            Res.Description := ResourceTable.FieldByName(FN).AsString;

          FN := GetFieldName(FResourceMappings, 'Notes');
          if FN <> '' then
            Res.Notes := ResourceTable.FieldByName(FN).AsString;

          FN := GetFieldName(FResourceMappings, 'Active');
          if FN <> '' then
            Res.Active := ResourceTable.FieldByName(FN).AsBoolean;

          FN := GetFieldName(FResourceMappings, 'UserField0');
          if FN <> '' then
            Res.UserField0 := ResourceTable.FieldByName(FN).AsString;

          FN := GetFieldName(FResourceMappings, 'UserField1');
          if FN <> '' then
            Res.UserField1 := ResourceTable.FieldByName(FN).AsString;

          FN := GetFieldName(FResourceMappings, 'UserField2');
          if FN <> '' then
            Res.UserField2 := ResourceTable.FieldByName(FN).AsString;

          FN := GetFieldName(FResourceMappings, 'UserField3');
          if FN <> '' then
            Res.UserField3 := ResourceTable.FieldByName(FN).AsString;

          FN := GetFieldName(FResourceMappings, 'UserField4');
          if FN <> '' then
            Res.UserField4 := ResourceTable.FieldByName(FN).AsString;

          FN := GetFieldName(FResourceMappings, 'UserField5');
          if FN <> '' then
            Res.UserField5 := ResourceTable.FieldByName(FN).AsString;

          FN := GetFieldName(FResourceMappings, 'UserField6');
          if FN <> '' then
            Res.UserField6 := ResourceTable.FieldByName(FN).AsString;

          FN := GetFieldName(FResourceMappings, 'UserField7');
          if FN <> '' then
            Res.UserField7 := ResourceTable.FieldByName(FN).AsString;

          FN := GetFieldName(FResourceMappings, 'UserField8');
          if FN <> '' then
            Res.UserField8 := ResourceTable.FieldByName(FN).AsString;

          FN := GetFieldName(FResourceMappings, 'UserField9');
          if FN <> '' then
            Res.UserField9 := ResourceTable.FieldByName(FN).AsString;
          Res.Loading := false;

          { Add events, contacts and tasks for the currently selected resource }
          if (Res.ResourceID = ResourceID) and Res.Active then begin
            Resource := Res;
            LoadEvents;
            LoadContacts;
            LoadTasks;
          end;

          ResourceTable.Next;
        end; {while not EOF do }
      end; {with FResourceDataSrc.Dataset do}
      Resources.Sort;
    end;
  finally
    Loading := false;
  end;
  NotifyDependents;
end;
{=====}

procedure TVpFlexDataStore.LoadEvents;
var
  Event: TVpEvent;
  {Field Name}
  FN1, FN2, FN3  : string;
begin
  if (FEventsDataSrc = nil)
  or (FEventsDataSrc.DataSet = nil) then
    Exit;

  if (FResource <> nil) then begin
    { Load this resource's events into memory }
    SetFilterCriteria(FEventsDataSrc.DataSet,
                      True,
                      FResource.ResourceID,
                      TimeRange.StartTime,
                      TimeRange.EndTime);

    if (FEventsDataSrc = nil)
    or (FEventsDataSrc.DataSet = nil)
    or (not FEventsDataSrc.DataSet.Active) then
      Exit;

    with FEventsDataSrc.Dataset do begin
      First;

      while not EOF do begin
        FN1 := GetFieldName(FEventMappings, 'RecordID');
        FN2 := GetFieldName(FEventMappings, 'StartTime');
        FN3 := GetFieldName(FEventMappings, 'EndTime');
        if (FN1 <> '') and (FN2 <> '') and (FN3 <> '') then begin
          Event := Resource.Schedule.AddEvent(FieldByName(FN1).AsInteger,
            FieldByName(FN2).AsDateTime,
            FieldByName(FN3).AsDateTime);
          if Event <> nil then begin
            Event.Loading := true;

            FN1 := GetFieldName(FEventMappings, 'Description');
            if (FN1 <> '') then
              Event.Description := FieldByName(FN1).AsString;

            FN1 := GetFieldName(FEventMappings, 'Note');                 
            if (FN1 <> '') then
              Event.Note := FieldByName(FN1).AsString;

            FN1 := GetFieldName(FEventMappings, 'Category');
            if (FN1 <> '') then
              Event.Category := FieldByName(FN1).AsInteger;

            FN1 := GetFieldName(FEventMappings, 'AlarmWavPath');
            if (FN1 <> '') then
              Event.AlarmWavPath := FieldByName(FN1).AsString;

            FN1 := GetFieldName(FEventMappings, 'AllDayEvent');
            if (FN1 <> '') then
              Event.AllDayEvent := FieldByName(FN1).AsBoolean;

            FN1 := GetFieldName(FEventMappings, 'AlarmSet');
            if (FN1 <> '') then
              Event.AlarmSet := FieldByName(FN1).AsBoolean;

            FN1 := GetFieldName(FEventMappings, 'AlarmAdv');
            if (FN1 <> '') then
              Event.AlarmAdv := FieldByName(FN1).AsInteger;

            FN1 := GetFieldName(FEventMappings, 'AlarmAdvType');
            if (FN1 <> '') then
              Event.AlarmAdvType := TVpAlarmAdvType(
                FieldByName(FN1).AsInteger);

            FN1 := GetFieldName(FEventMappings, 'SnoozeTime');
            if (FN1 <> '') then
              Event.SnoozeTime := FieldByName(FN1).AsDateTime;

            FN1 := GetFieldName(FEventMappings, 'RepeatCode');
            if (FN1 <> '') then
              Event.RepeatCode := TVpRepeatType(FieldByName(FN1).AsInteger);

            FN1 := GetFieldName(FEventMappings, 'RepeatRangeEnd');
            if (FN1 <> '') then
              Event.RepeatRangeEnd := FieldByName(FN1).AsDateTime;

            FN1 := GetFieldName(FEventMappings, 'CustInterval');
            if (FN1 <> '') then
              Event.CustInterval := FieldByName(FN1).AsInteger;

            FN1 := GetFieldName(FEventMappings, 'UserField0');
            if (FN1 <> '') then
              Event.UserField0 := FieldByName(FN1).AsString;

            FN1 := GetFieldName(FEventMappings, 'UserField1');
            if (FN1 <> '') then
              Event.UserField1 := FieldByName(FN1).AsString;

            FN1 := GetFieldName(FEventMappings, 'UserField2');
            if (FN1 <> '') then
              Event.UserField2 := FieldByName(FN1).AsString;

            FN1 := GetFieldName(FEventMappings, 'UserField3');
            if (FN1 <> '') then
              Event.UserField3 := FieldByName(FN1).AsString;

            FN1 := GetFieldName(FEventMappings, 'UserField4');
            if (FN1 <> '') then
              Event.UserField4 := FieldByName(FN1).AsString;

            FN1 := GetFieldName(FEventMappings, 'UserField5');
            if (FN1 <> '') then
              Event.UserField5 := FieldByName(FN1).AsString;

            FN1 := GetFieldName(FEventMappings, 'UserField6');
            if (FN1 <> '') then
              Event.UserField6 := FieldByName(FN1).AsString;

            FN1 := GetFieldName(FEventMappings, 'UserField7');
            if (FN1 <> '') then
              Event.UserField7 := FieldByName(FN1).AsString;

            FN1 := GetFieldName(FEventMappings, 'UserField8');
            if (FN1 <> '') then
              Event.UserField8 := FieldByName(FN1).AsString;

            FN1 := GetFieldName(FEventMappings, 'UserField9');
            if (FN1 <> '') then
              Event.UserField9 := FieldByName(FN1).AsString;

            Event.Loading := false;
          end; {if Event <> nil}
        end; {if (FN1 <> '') and (FN2 <> '') and (FN3 <> '')}
        Next;
      end; {while}
    end; {with FEventsDataSrc.Dataset}
  end; {if resource <> nil}
end;
{=====}

procedure TVpFlexDataStore.LoadContacts;
var
  Contact: TVpContact;
  {Field Name}
  FN : string;
begin
  if (FResource <> nil) then begin
    {load this resource's contacts into memory}
    if (FContactsDataSrc <> nil)
    and (FContactsDataSrc.DataSet <> nil)
    and (FContactsDataSrc.DataSet.Active) then
    with FContactsDataSrc.DataSet do begin
      SetFilterCriteria(FContactsDataSrc.DataSet, False,
                        FResource.ResourceID, 0, 0);
      First;
      while not EOF do begin
        Contact := Resource.Contacts.AddContact(GetNextID(ContactsTableName));
        if Contact <> nil then begin
          Contact.Loading := true;

          FN := GetFieldName(FContactMappings, 'RecordID');
          if FN <> '' then
            Contact.RecordID := FieldByName(FN).AsInteger;

          FN := GetFieldName(FContactMappings, 'FirstName');
          if FN <> '' then
            Contact.FirstName := FieldByName(FN).AsString;

          FN := GetFieldName(FContactMappings, 'LastName');
          if FN <> '' then
            Contact.LastName := FieldByName(FN).AsString;

          FN := GetFieldName(FContactMappings, 'BirthDate');
          if FN <> '' then
            Contact.Birthdate := FieldByName(FN).AsDateTime;

          FN := GetFieldName(FContactMappings, 'Anniversary');
          if FN <> '' then
            Contact.Anniversary := FieldByName(FN).AsDateTime;

          FN := GetFieldName(FContactMappings, 'Title');
          if FN <> '' then
            Contact.Title := FieldByName(FN).AsString;

          FN := GetFieldName(FContactMappings, 'Company');
          if FN <> '' then
            Contact.Company := FieldByName(FN).AsString;

          FN := GetFieldName(FContactMappings, 'Job_Position');
          if FN <> '' then
            Contact.Position := FieldByName(FN).AsString;

          FN := GetFieldName(FContactMappings, 'EMail');
          if FN <> '' then
            Contact.EMail := FieldByName(FN).AsString;

          FN := GetFieldName(FContactMappings, 'Address');
          if FN <> '' then
            Contact.Address := FieldByName(FN).AsString;

          FN := GetFieldName(FContactMappings, 'City');
          if FN <> '' then
            Contact.City := FieldByName(FN).AsString;

          FN := GetFieldName(FContactMappings, 'State');
          if FN <> '' then
            Contact.State := FieldByName(FN).AsString;

          FN := GetFieldName(FContactMappings, 'Zip');
          if FN <> '' then
            Contact.Zip := FieldByName(FN).AsString;

          FN := GetFieldName(FContactMappings, 'Country');
          if FN <> '' then
            Contact.Country := FieldByName(FN).AsString;

          FN := GetFieldName(FContactMappings, 'Note');
          if FN <> '' then
            Contact.Note := FieldByName(FN).AsString;

          FN := GetFieldName(FContactMappings, 'Phone1');
          if FN <> '' then
            Contact.Phone1 := FieldByName(FN).AsString;

          FN := GetFieldName(FContactMappings, 'Phone2');
          if FN <> '' then
            Contact.Phone2 := FieldByName(FN).AsString;

          FN := GetFieldName(FContactMappings, 'Phone3');
          if FN <> '' then
            Contact.Phone3 := FieldByName(FN).AsString;

          FN := GetFieldName(FContactMappings, 'Phone4');
          if FN <> '' then
            Contact.Phone4 := FieldByName(FN).AsString;

          FN := GetFieldName(FContactMappings, 'Phone5');
          if FN <> '' then
            Contact.Phone5 := FieldByName(FN).AsString;

          FN := GetFieldName(FContactMappings, 'PhoneType1');
          if FN <> '' then
            Contact.PhoneType1 := FieldByName(FN).AsInteger;

          FN := GetFieldName(FContactMappings, 'PhoneType2');
          if FN <> '' then
            Contact.PhoneType2 := FieldByName(FN).AsInteger;

          FN := GetFieldName(FContactMappings, 'PhoneType3');
          if FN <> '' then
            Contact.PhoneType3 := FieldByName(FN).AsInteger;

          FN := GetFieldName(FContactMappings, 'PhoneType4');
          if FN <> '' then
            Contact.PhoneType4 := FieldByName(FN).AsInteger;

          FN := GetFieldName(FContactMappings, 'PhoneType5');
          if FN <> '' then
            Contact.PhoneType5 := FieldByName(FN).AsInteger;

          FN := GetFieldName(FContactMappings, 'Category');
          if FN <> '' then
            Contact.Category := FieldByName(FN).AsInteger;

          FN := GetFieldName(FContactMappings, 'Custom1');
          if FN <> '' then
            Contact.Custom1 := FieldByName(FN).AsString;

          FN := GetFieldName(FContactMappings, 'Custom2');
          if FN <> '' then
            Contact.Custom2 := FieldByName(FN).AsString;

          FN := GetFieldName(FContactMappings, 'Custom3');
          if FN <> '' then
            Contact.Custom3 := FieldByName(FN).AsString;

          FN := GetFieldName(FContactMappings, 'Custom4');
          if FN <> '' then
            Contact.Custom4 := FieldByName(FN).AsString;

          FN := GetFieldName(FContactMappings, 'UserField0');
          if FN <> '' then
            Contact.UserField0 := FieldByName(FN).AsString;

          FN := GetFieldName(FContactMappings, 'UserField1');
          if FN <> '' then
            Contact.UserField1 := FieldByName(FN).AsString;

          FN := GetFieldName(FContactMappings, 'UserField2');
          if FN <> '' then
            Contact.UserField2 := FieldByName(FN).AsString;

          FN := GetFieldName(FContactMappings, 'UserField3');
          if FN <> '' then
            Contact.UserField3 := FieldByName(FN).AsString;

          FN := GetFieldName(FContactMappings, 'UserField4');
          if FN <> '' then
            Contact.UserField4 := FieldByName(FN).AsString;

          FN := GetFieldName(FContactMappings, 'UserField5');
          if FN <> '' then
            Contact.UserField5 := FieldByName(FN).AsString;

          FN := GetFieldName(FContactMappings, 'UserField6');
          if FN <> '' then
            Contact.UserField6 := FieldByName(FN).AsString;

          FN := GetFieldName(FContactMappings, 'UserField7');
          if FN <> '' then
            Contact.UserField7 := FieldByName(FN).AsString;

          FN := GetFieldName(FContactMappings, 'UserField8');
          if FN <> '' then
            Contact.UserField8 := FieldByName(FN).AsString;

          FN := GetFieldName(FContactMappings, 'UserField9');
          if FN <> '' then
            Contact.UserField9 := FieldByName(FN).AsString;

          Contact.Loading := false;
        end;
        Next;
      end; {while}
    end; {with ContactsTable}
  end; {if Resource <> nil}
end;
{=====}

procedure TVpFlexDataStore.LoadTasks;
var
  Task: TVpTask;
  {Field Name}
  FN : string;
begin
  if (FResource <> nil) then begin
    {load this resource's contacts into memory}
    if (FTasksDataSrc <> nil)
    and (FTasksDataSrc.DataSet <> nil)
    and (FTasksDataSrc.DataSet.Active) then begin
      with FTasksDataSrc.DataSet do begin
        SetFilterCriteria(FTasksDataSrc.DataSet, False, FResource.ResourceID,
          0, 0);
        First;
        while not EOF do begin
          Task := Resource.Tasks.AddTask(GetNextID(TasksTableName));
          if Task <> nil then begin
            task.loading := true;

            FN := GetFieldName(FTaskMappings, 'RecordID');
            if FN <> '' then
              Task.RecordID := FieldByName(FN).AsInteger;

            FN := GetFieldName(FTaskMappings, 'Complete');
            if FN <> '' then
              Task.Complete := FieldByName(FN).AsBoolean;

            FN := GetFieldName(FTaskMappings, 'Description');
            if FN <> '' then
              Task.Description := FieldByName(FN).AsString;

            FN := GetFieldName(FTaskMappings, 'Details');
            if FN <> '' then
              Task.Details := FieldByName(FN).AsString;

            FN := GetFieldName(FTaskMappings, 'CreatedOn');
            if FN <> '' then
              Task.CreatedOn := FieldByName(FN).AsDateTime;

            FN := GetFieldName(FTaskMappings, 'CompletedOn');
            if FN <> '' then
              Task.CompletedOn := FieldByName(FN).AsDateTime;

            FN := GetFieldName(FTaskMappings, 'Priority');
            if FN <> '' then
              Task.Priority := FieldByName(FN).AsInteger;

            FN := GetFieldName(FTaskMappings, 'Category');
            if FN <> '' then
              Task.Category := FieldByName(FN).AsInteger;

            FN := GetFieldName(FTaskMappings, 'DueDate');
            if FN <> '' then
              Task.DueDate := FieldByName(FN).AsDateTime;

            FN := GetFieldName(FTaskMappings, 'UserField0');
            if FN <> '' then
              Task.UserField0 := FieldBYName(FN).AsString;

            FN := GetFieldName(FTaskMappings, 'UserField1');
            if FN <> '' then
              Task.UserField1 := FieldBYName(FN).AsString;

            FN := GetFieldName(FTaskMappings, 'UserField2');
            if FN <> '' then
              Task.UserField2 := FieldBYName(FN).AsString;

            FN := GetFieldName(FTaskMappings, 'UserField3');
            if FN <> '' then
              Task.UserField3 := FieldBYName(FN).AsString;

            FN := GetFieldName(FTaskMappings, 'UserField4');
            if FN <> '' then
              Task.UserField4 := FieldBYName(FN).AsString;

            FN := GetFieldName(FTaskMappings, 'UserField5');
            if FN <> '' then
              Task.UserField5 := FieldBYName(FN).AsString;

            FN := GetFieldName(FTaskMappings, 'UserField6');
            if FN <> '' then
              Task.UserField6 := FieldBYName(FN).AsString;

            FN := GetFieldName(FTaskMappings, 'UserField7');
            if FN <> '' then
              Task.UserField7 := FieldBYName(FN).AsString;

            FN := GetFieldName(FTaskMappings, 'UserField8');
            if FN <> '' then
              Task.UserField8 := FieldBYName(FN).AsString;

            FN := GetFieldName(FTaskMappings, 'UserField9');
            if FN <> '' then
              Task.UserField9 := FieldBYName(FN).AsString;

            Task.Loading := false;
          end; {if task <> nil}
          Next;
        end; {While not EOF}
      end; {with FTasksDataSrc.DataSet}
    end;
  end;
end;
{=====}

procedure TVpFlexDataStore.RefreshResource;                              
var
//  Resource: TVpResource;
  {Field Name}
  FN : string;
begin
  if Resource = nil then
    Resource := Resources.GetResource(ResourceID);

  {clear the resource}
  if Resource <> nil then begin
    Resource.Schedule.ClearEvents;
    Resource.Tasks.ClearTasks;
    Resource.Contacts.ClearContacts;
  end;

  if (FResourceDataSrc <> nil)
  and (FResourceDataSrc.DataSet <> nil)
  then FResourceDataSrc.DataSet.Open;

  if FResourceDataSrc.DataSet.Active then begin

    with FResourceDataSrc.DataSet do begin
      { if a resource }
      FN := GetFieldName(FResourceMappings, 'ResourceID');
      if FN <> '' then begin
        if Locate(FN, ResourceID, []) then begin
          Resource.ResourceID := ResourceID;

          FN := GetFieldName(FResourceMappings, 'Description');
          if FN <> '' then
            Resource.Description := FieldByName(FN).AsString;

          FN := GetFieldName(FResourceMappings, 'Notes');
          if FN <> '' then
            Resource.Notes := FieldByName(FN).AsString;

          FN := GetFieldName(FResourceMappings, 'ResourceActive');
          if FN <> '' then
            Resource.Active := FieldByName(FN).AsBoolean;

          FN := GetFieldName(FResourceMappings, 'UserField0');
          if FN <> '' then
            Resource.UserField0 := FieldByName(FN).AsString;

          FN := GetFieldName(FResourceMappings, 'UserField1');
          if FN <> '' then
            Resource.UserField1 := FieldByName(FN).AsString;

          FN := GetFieldName(FResourceMappings, 'UserField2');
          if FN <> '' then
            Resource.UserField2 := FieldByName(FN).AsString;

          FN := GetFieldName(FResourceMappings, 'UserField3');
          if FN <> '' then
            Resource.UserField3 := FieldByName(FN).AsString;

          FN := GetFieldName(FResourceMappings, 'UserField4');
          if FN <> '' then
            Resource.UserField4 := FieldByName(FN).AsString;

          FN := GetFieldName(FResourceMappings, 'UserField5');
          if FN <> '' then
            Resource.UserField5 := FieldByName(FN).AsString;

          FN := GetFieldName(FResourceMappings, 'UserField6');
          if FN <> '' then
            Resource.UserField6 := FieldByName(FN).AsString;

          FN := GetFieldName(FResourceMappings, 'UserField7');
          if FN <> '' then
            Resource.UserField7 := FieldByName(FN).AsString;

          FN := GetFieldName(FResourceMappings, 'UserField8');
          if FN <> '' then
            Resource.UserField8 := FieldByName(FN).AsString;

          FN := GetFieldName(FResourceMappings, 'UserField9');
          if FN <> '' then
            Resource.UserField9 := FieldByName(FN).AsString;

          LoadEvents;
          LoadContacts;
          LoadTasks;
        end; {if Locate(FN, ResourceID, [])}
      end; {if FN <> '' }
    end; {with FResourceDataSrc.DataSet do}
  end; {if FResourceDataSrc.DataSet.Active}
  if not Loading then
    NotifyDependents;
end;
{=====}

procedure TVpFlexDataStore.RefreshEvents;
begin
  if Resource <> nil then begin
    Resource.Schedule.ClearEvents;
    LoadEvents;
  end;
  if not Loading then
    NotifyDependents;
end;
{=====}

procedure TVpFlexDataStore.RefreshContacts;
begin
  if Resource <> nil then begin
    Resource.Contacts.ClearContacts;
    LoadContacts;
  end;
  if not Loading then
    NotifyDependents;
end;
{=====}

procedure TVpFlexDataStore.RefreshTasks;
begin
  if Resource <> nil then begin
    Resource.Tasks.ClearTasks;
    LoadTasks;
  end;
  if not Loading then
    NotifyDependents;
end;
{=====}

procedure TVpFlexDataStore.PostEvents;
var
  J: Integer;
  Event: TVpEvent;
  {FieldName}
  FN: string;
begin
  {if no events dataset has been defined then bail.}
  if (FEventsDataSrc = nil)
  or (FEventsDataSrc.DataSet = nil) then
    Exit;

  if (Resource <> nil) and Resource.EventsDirty then begin
    { Dump this resource's dirty events to the DB }
    if (FResourceDataSrc <> nil)
    and (FResourceDataSrc.DataSet <> nil) then begin

      FResourceDataSrc.DataSet.Open;

      FN := GetFieldName(FEventMappings, 'ResourceID');
      if (FN <> '')
      and FResourceDataSrc.DataSet.Locate(FN, Resource.ResourceID, [])
      then begin
        SetFilterCriteria(FEventsDataSrc.DataSet, False, Resource.ResourceID,
          0, 0);

        for J := pred(Resource.Schedule.EventCount) downto 0 do begin
          Event := Resource.Schedule.GetEvent(J);

          FN := GetFieldName(FEventMappings, 'RecordID');
          if FN <> '' then begin
            { if the delete flag is set then delete it from the database }
            { and free the event instance }
            if Event.Deleted then begin
              if EventsTable.Locate(FN, Event.RecordID, []) then
                EventsTable.Delete;
              Event.Free;
              Continue;
            end;

            if Event.Changed then begin
              if EventsTable.Locate(FN, Event.RecordID, []) then
                { this event already exists in the database so update it }
                EventsTable.Edit
              else begin
                EventsTable.Append;
              end;
              try
                { if a particular descendant datastore uses autoincrementing }
                { RecordID fields, then  don't overwrite them here. }
                if Event.RecordID <> -1 then
                  EventsTable.FieldByName(FN).AsInteger := Event.RecordID;


                FN := GetFieldName(FEventMappings, 'StartTime');
                if FN <> '' then
                  EventsTable.FieldByName(FN).AsDateTime := Event.StartTime;

                FN := GetFieldName(FEventMappings, 'EndTime');
                if FN <> '' then
                  EventsTable.FieldByName(FN).AsDateTime := Event.EndTime;

                FN := GetFieldName(FEventMappings, 'ResourceID');
                if FN <> '' then
                  EventsTable.FieldByName(FN).AsInteger := Resource.ResourceID;

                FN := GetFieldName(FEventMappings, 'Description');
                if FN <> '' then
                  EventsTable.FieldByName(FN).AsString := Event.Description;

                FN := GetFieldName(FEventMappings, 'Note');              
                if FN <> '' then
                  EventsTable.FieldByName(FN).AsString := Event.Note;

                FN := GetFieldName(FEventMappings, 'Category');
                if FN <> '' then
                  EventsTable.FieldByName(FN).AsInteger := Event.Category;

                FN := GetFieldName(FEventMappings, 'AlarmWavPath');      
                if FN <> '' then
                  EventsTable.FieldByName(FN).AsString := Event.AlarmWavPath;

                FN := GetFieldName(FEventMappings, 'AllDayEvent');
                if FN <> '' then
                  EventsTable.FieldByName(FN).AsBoolean := Event.AllDayEvent;

                FN := GetFieldName(FEventMappings, 'AlarmSet');
                if FN <> '' then
                  EventsTable.FieldByName(FN).AsBoolean := Event.AlarmSet;

                FN := GetFieldName(FEventMappings, 'AlarmAdvance');
                if FN <> '' then
                  EventsTable.FieldByName(FN).AsInteger := Event.AlarmAdv;

                FN := GetFieldName(FEventMappings, 'AlarmAdvanceType');
                if FN <> '' then
                  EventsTable.FieldByName(FN).AsInteger := Ord(Event.AlarmAdvType);

                FN := GetFieldName(FEventMappings, 'SnoozeTime');
                if FN <> '' then
                  EventsTable.FieldByName(FN).AsDateTime := Event.SnoozeTime;

                FN := GetFieldName(FEventMappings, 'RepeatCode');
                if FN <> '' then
                  EventsTable.FieldByName(FN).AsInteger := Ord(Event.RepeatCode);

                FN := GetFieldName(FEventMappings, 'RepeatRangeEnd');
                if FN <> '' then
                  EventsTable.FieldByName(FN).AsDateTime := Event.RepeatRangeEnd;

                FN := GetFieldName(FEventMappings, 'CustomInterval');
                if FN <> '' then
                  EventsTable.FieldByName(FN).AsInteger := Event.CustInterval;

                FN := GetFieldName(FEventMappings, 'UserField0');
                if FN <> '' then
                  EventsTable.FieldByName(FN).AsString := Event.UserField0;

                FN := GetFieldName(FEventMappings, 'UserField1');
                if FN <> '' then
                  EventsTable.FieldByName(FN).AsString := Event.UserField1;

                FN := GetFieldName(FEventMappings, 'UserField2');
                if FN <> '' then
                  EventsTable.FieldByName(FN).AsString := Event.UserField2;

                FN := GetFieldName(FEventMappings, 'UserField3');
                if FN <> '' then
                  EventsTable.FieldByName(FN).AsString := Event.UserField3;

                FN := GetFieldName(FEventMappings, 'UserField4');
                if FN <> '' then
                  EventsTable.FieldByName(FN).AsString := Event.UserField4;

                FN := GetFieldName(FEventMappings, 'UserField5');
                if FN <> '' then
                  EventsTable.FieldByName(FN).AsString := Event.UserField5;

                FN := GetFieldName(FEventMappings, 'UserField6');
                if FN <> '' then
                  EventsTable.FieldByName(FN).AsString := Event.UserField6;

                FN := GetFieldName(FEventMappings, 'UserField7');
                if FN <> '' then
                  EventsTable.FieldByName(FN).AsString := Event.UserField7;

                FN := GetFieldName(FEventMappings, 'UserField8');
                if FN <> '' then
                  EventsTable.FieldByName(FN).AsString := Event.UserField8;

                FN := GetFieldName(FEventMappings, 'UserField9');
                if FN <> '' then
                  EventsTable.FieldByName(FN).AsString := Event.UserField9;

                EventsTable.Post;
              except
                EventsTable.Cancel;
                raise EDBPostError.Create;
              end;

              { if a particular descendant datastore uses autoincrementing    }
              { RecordID fields then the RecordID is assigned by the database }
              { and needs to be assigned here...}
              if Event.RecordID = -1 then begin                          
                 FN := GetFieldName(EventMappings, 'RecordID');          
                 if FN <> '' then                                        
                  Event.RecordID                                         
                    := EventsTable.FieldByName(FN).AsInteger;            
              end;                                                       


(* Bad Phillip.
              if Event.RecordID = -1 then
                Event.RecordID := EventsTable.FieldByName('RecordID').AsInteger;
*)
              Event.Changed := false;
            end;
          end;
        end;
      end;
      Resource.EventsDirty := false;
      Resource.Schedule.Sort;                                                
    end;
  end;
  if not Loading then
    NotifyDependents;
end;
{=====}

procedure TVpFlexDataStore.PostContacts;
var
  I: Integer;
  Contact: TVpContact;
  {FieldName}
  FN : string;
begin
  if (Resource <> nil) and Resource.ContactsDirty then begin
    { Dump this resource's dirty contacts to the DB }
    SetFilterCriteria(ContactsTable, False, Resource.ResourceID, 0, 0);

    for I := pred(Resource.Contacts.Count) downto 0 do begin
      Contact := Resource.Contacts.GetContact(I);

      {Get mapped RecordID field name}
      FN := GetFieldName(FContactMappings, 'RecordID');
      {If no RecordID field is mapped then we can't go any further.}
      if (FN = '') then Exit;

      { if the delete flag is set then delete the record }
      { and free the event instance }
      if Contact.Deleted then begin
        if ContactsTable.Locate(FN, Contact.RecordID, [])
        then ContactsTable.Delete;
        Contact.Free;
        Continue;
      end;

      if Contact.Changed then begin
        if ContactsTable.Locate(FN, Contact.RecordID, []) then
          { this event already exists in the database so update it }
          ContactsTable.Edit
        else begin
          { this record doesn't exist in the database, so it's a new event }
          ContactsTable.Append;
        end;
        try
          { DataStore descendants that can use an autoincrement RecordID }
          { field set the RecordID to -1 by default.  If the RecordID is }
          { -1 then this is a new record and we shouldn't overwrite      }
          { RecordID with a bogus value }
          if Contact.RecordID > -1 then
            ContactsTable.FieldByName(FN).AsInteger := Contact.RecordID;

          FN := GetFieldName(FContactMappings, 'ResourceID');
          if FN <> '' then
            ContactsTable.FieldByName(FN).AsInteger := Resource.ResourceID;

          FN := GetFieldName(FContactMappings, 'FirstName');
          if FN <> '' then
            ContactsTable.FieldByName(FN).AsString := Contact.FirstName;

          FN := GetFieldName(FContactMappings, 'LastName');
          if FN <> '' then
            ContactsTable.FieldByName(FN).AsString := Contact.LastName;

          FN := GetFieldName(FContactMappings, 'Birthdate');
          if FN <> '' then
            ContactsTable.FieldByName(FN).AsDateTime := Contact.Birthdate;

          FN := GetFieldName(FContactMappings, 'Anniversary');
          if FN <> '' then
            ContactsTable.FieldByName(FN).AsDateTime := Contact.Anniversary;

          FN := GetFieldName(FContactMappings, 'Title');
          if FN <> '' then
            ContactsTable.FieldByName(FN).AsString := Contact.Title;

          FN := GetFieldName(FContactMappings, 'Company');
          if FN <> '' then
            ContactsTable.FieldByName(FN).AsString := Contact.Company;

          FN := GetFieldName(FContactMappings, 'Position');
          if FN <> '' then
            ContactsTable.FieldByName(FN).AsString := Contact.Position;

          FN := GetFieldName(FContactMappings, 'EMail');
          if FN <> '' then
            ContactsTable.FieldByName(FN).AsString := Contact.EMail;

          FN := GetFieldName(FContactMappings, 'Address');
          if FN <> '' then
            ContactsTable.FieldByName(FN).AsString := Contact.Address;

          FN := GetFieldName(FContactMappings, 'City');
          if FN <> '' then
            ContactsTable.FieldByName(FN).AsString := Contact.City;

          FN := GetFieldName(FContactMappings, 'State');
          if FN <> '' then
            ContactsTable.FieldByName(FN).AsString := Contact.State;

          FN := GetFieldName(FContactMappings, 'Zip');
          if FN <> '' then
            ContactsTable.FieldByName(FN).AsString := Contact.Zip;

          FN := GetFieldName(FContactMappings, 'Country');
          if FN <> '' then
            ContactsTable.FieldByName(FN).AsString := Contact.Country;

          FN := GetFieldName(FContactMappings, 'Note');
          if FN <> '' then
            ContactsTable.FieldByName(FN).AsString := Contact.Note;

          FN := GetFieldName(FContactMappings, 'Phone1');
          if FN <> '' then
            ContactsTable.FieldByName(FN).AsString := Contact.Phone1;

          FN := GetFieldName(FContactMappings, 'Phone2');
          if FN <> '' then
            ContactsTable.FieldByName(FN).AsString := Contact.Phone2;

          FN := GetFieldName(FContactMappings, 'Phone3');
          if FN <> '' then
            ContactsTable.FieldByName(FN).AsString := Contact.Phone3;

          FN := GetFieldName(FContactMappings, 'Phone4');
          if FN <> '' then
            ContactsTable.FieldByName(FN).AsString := Contact.Phone4;

          FN := GetFieldName(FContactMappings, 'Phone5');
          if FN <> '' then
            ContactsTable.FieldByName(FN).AsString := Contact.Phone5;

          FN := GetFieldName(FContactMappings, 'PhoneType1');
          if FN <> '' then
            ContactsTable.FieldByName(FN).AsInteger := Contact.PhoneType1;

          FN := GetFieldName(FContactMappings, 'PhoneType2');
          if FN <> '' then
            ContactsTable.FieldByName(FN).AsInteger := Contact.PhoneType2;

          FN := GetFieldName(FContactMappings, 'PhoneType3');
          if FN <> '' then
            ContactsTable.FieldByName(FN).AsInteger := Contact.PhoneType3;

          FN := GetFieldName(FContactMappings, 'PhoneType4');
          if FN <> '' then
            ContactsTable.FieldByName(FN).AsInteger := Contact.PhoneType4;

          FN := GetFieldName(FContactMappings, 'PhoneType5');
          if FN <> '' then
            ContactsTable.FieldByName(FN).AsInteger := Contact.PhoneType5;

          FN := GetFieldName(FContactMappings, 'Category');
          if FN <> '' then
            ContactsTable.FieldByName(FN).AsInteger := Contact.Category;

          FN := GetFieldName(FContactMappings, 'Custom1');
          if FN <> '' then
            ContactsTable.FieldByName(FN).AsString := Contact.Custom1;

          FN := GetFieldName(FContactMappings, 'Custom2');
          if FN <> '' then
            ContactsTable.FieldByName(FN).AsString := Contact.Custom2;

          FN := GetFieldName(FContactMappings, 'Custom3');
          if FN <> '' then
            ContactsTable.FieldByName(FN).AsString := Contact.Custom3;

          FN := GetFieldName(FContactMappings, 'Custom4');
          if FN <> '' then
            ContactsTable.FieldByName(FN).AsString := Contact.Custom4;

          FN := GetFieldName(FContactMappings, 'UserField0');
          if FN <> '' then
            ContactsTable.FieldByName(FN).AsString := Contact.UserField0;

          FN := GetFieldName(FContactMappings, 'UserField1');
          if FN <> '' then
            ContactsTable.FieldByName(FN).AsString := Contact.UserField1;

          FN := GetFieldName(FContactMappings, 'UserField2');
          if FN <> '' then
            ContactsTable.FieldByName(FN).AsString := Contact.UserField2;

          FN := GetFieldName(FContactMappings, 'UserField3');
          if FN <> '' then
            ContactsTable.FieldByName(FN).AsString := Contact.UserField3;

          FN := GetFieldName(FContactMappings, 'UserField4');
          if FN <> '' then
            ContactsTable.FieldByName(FN).AsString := Contact.UserField4;

          FN := GetFieldName(FContactMappings, 'UserField5');
          if FN <> '' then
            ContactsTable.FieldByName(FN).AsString := Contact.UserField5;

          FN := GetFieldName(FContactMappings, 'UserField6');
          if FN <> '' then
            ContactsTable.FieldByName(FN).AsString := Contact.UserField6;

          FN := GetFieldName(FContactMappings, 'UserField7');
          if FN <> '' then
            ContactsTable.FieldByName(FN).AsString := Contact.UserField7;

          FN := GetFieldName(FContactMappings, 'UserField8');
          if FN <> '' then
            ContactsTable.FieldByName(FN).AsString := Contact.UserField8;

          FN := GetFieldName(FContactMappings, 'UserField9');
          if FN <> '' then
            ContactsTable.FieldByName(FN).AsString := Contact.UserField9;

          ContactsTable.Post;
        except
          ContactsTable.Cancel;
          raise EDBPostError.Create;
        end;
        { DataStore descendants that can use an autoincrement RecordID }
        { field set the RecordID to -1 by default.  If the RecordID is }
        { -1 then this is a new record and we need to assign the real  }
        { record ID value from the dataset. }
        if Contact.RecordID = -1 then begin
          FN := GetFieldName(FContactMappings, 'RecordID');
          if FN <> '' then
          Contact.RecordID := ContactsTable.FieldByName(FN).AsInteger;
        end;

        Contact.Changed := false;
      end;
    end;

    Resource.ContactsDirty := false;
    Resource.Contacts.Sort;
  end;
end;
{=====}

procedure TVpFlexDataStore.PostTasks;
var
  I: Integer;
  Task: TVpTask;
  {FieldName}
  FN: string;
begin
  if (Resource <> nil) and Resource.TasksDirty then begin

    if (FResourceDataSrc <> nil)
    and (FResourceDataSrc.Dataset <> nil) then begin
      FResourceDataSrc.DataSet.Open;
      if FResourceDataSrc.DataSet.Active then begin
        FN := GetFieldName(FTaskMappings, 'ResourceID');
        { Dump this resource's dirty contacts to the DB }
        if not FResourceDataSrc.DataSet.Locate(FN, Resource.ResourceID, [])
        then Exit;
      end; {if FResourceDataSrc.DataSet.Active}
    end; {if (FResourceDataSrc ...}

    {filter tasks}
    SetFilterCriteria(FTasksDataSrc.Dataset, False, Resource.ResourceID, 0, 0);

    for I := pred(Resource.Tasks.Count) downto 0 do begin

      with FTasksDataSrc.DataSet do begin

        Task := Resource.Tasks.GetTask(I);

        FN := GetFieldName(FTaskMappings, 'RecordID');

        { if the delete flag is set then delete the record }
        { and free the event instance }
        if Task.Deleted then begin
          if Locate(FN, Task.RecordID, []) then
            Delete;
          Task.Free;
          Continue;
        end;

        if Task.Changed then begin
          if Locate(FN, Task.RecordID, [])
          then
            { this event already exists in the database so update it }
            Edit
          else
            { this record doesn't exist in the database, so it's a new event }
            Append;
          try

          { DataStore descendants that can use an autoincrement RecordID }
          { field set the RecordID to -1 by default.  If the RecordID is }
          { -1 then this is a new record and we shouldn't overwrite      }
          { RecordID with a bogus value }
            if Task.RecordID > -1 then
              FieldByName(FN).AsInteger := Task.RecordID;

            FN := GetFieldName(FTaskMappings, 'ResourceID');
            if FN <> '' then
              FieldByName(FN).AsInteger := Resource.ResourceID;

            FN := GetFieldName(FTaskMappings, 'Description');
            if FN <> '' then
              FieldByName(FN).AsString := Task.Description;

            FN := GetFieldName(FTaskMappings, 'Details');
            if FN <> '' then
              FieldByName(FN).AsString := Task.Details;

            FN := GetFieldName(FTaskMappings, 'Complete');
            if FN <> '' then
              FieldByName(FN).AsBoolean := Task.Complete;

            FN := GetFieldName(FTaskMappings, 'DueDate');
            if FN <> '' then
              FieldByName(FN).AsDateTime := Task.DueDate;

            FN := GetFieldName(FTaskMappings, 'CreatedOn');
            if FN <> '' then
              FieldByName(FN).AsDateTime := Task.CreatedOn;

            FN := GetFieldName(FTaskMappings, 'CompletedOn');
            if FN <> '' then
              FieldByName(FN).AsDateTime := Task.CompletedOn;

            FN := GetFieldName(FTaskMappings, 'Priority');
            if FN <> '' then
              FieldByName(FN).AsInteger := Task.Priority;

            FN := GetFieldName(FTaskMappings, 'Category');
            if FN <> '' then
              FieldByName(FN).AsInteger := Task.Category;

            FN := GetFieldName(FTaskMappings, 'UserField0');
            if FN <> '' then
              FieldByName(FN).AsString := Task.UserField0;

            FN := GetFieldName(FTaskMappings, 'UserField1');
            if FN <> '' then
              FieldByName(FN).AsString := Task.UserField1;

            FN := GetFieldName(FTaskMappings, 'UserField2');
            if FN <> '' then
              FieldByName(FN).AsString := Task.UserField2;

            FN := GetFieldName(FTaskMappings, 'UserField3');
            if FN <> '' then
              FieldByName(FN).AsString := Task.UserField3;

            FN := GetFieldName(FTaskMappings, 'UserField4');
            if FN <> '' then
              FieldByName(FN).AsString := Task.UserField4;

            FN := GetFieldName(FTaskMappings, 'UserField5');
            if FN <> '' then
              FieldByName(FN).AsString := Task.UserField5;

            FN := GetFieldName(FTaskMappings, 'UserField6');
            if FN <> '' then
              FieldByName(FN).AsString := Task.UserField6;

            FN := GetFieldName(FTaskMappings, 'UserField7');
            if FN <> '' then
              FieldByName(FN).AsString := Task.UserField7;

            FN := GetFieldName(FTaskMappings, 'UserField8');
            if FN <> '' then
              FieldByName(FN).AsString := Task.UserField8;

            FN := GetFieldName(FTaskMappings, 'UserField9');
            if FN <> '' then
              FieldByName(FN).AsString := Task.UserField9;

            Post;
          except
            TasksTable.Cancel;
            raise EDBPostError.Create;
          end;
          { DataStore descendants that can use an autoincrement RecordID }
          { field set the RecordID to -1 by default.  If the RecordID is }
          { -1 then this is a new record and we need to assign the real  }
          { record ID value from the dataset. }
          if Task.RecordID = -1 then begin
            FN := GetFieldName(FTaskMappings, 'RecordID');
            if FN <> '' then
              Task.RecordID := TasksTable.FieldByName(FN).AsInteger;
          end;

          Task.Changed := false;
        end; {with FTasksDataSrc.DataSet do ...}
      end;
    end; {for I := 0 down to ...}
    Resource.TasksDirty := false;
    Resource.Tasks.Sort;
  end;
end;
{=====}

{ - New}
procedure TVpFlexDataStore.PostResources;
var
  I: Integer;
  Res: TVpResource;
  {FieldName}
  FN: string;
begin
  Loading := true;
  try
    if (Resources.Count > 0) then begin
      if (ResourceTable = nil) then
        Exit
      else if (not ResourceTable.Active) then
        ResourceTable.Open;

      if not ResourceTable.Active then
        Exit;

      ResourceTable.First;
      for I := 0 to pred(Resources.Count) do begin
        Res := Resources.Items[I];

        FN := GetFieldName(FResourceMappings, 'ResourceID');
        if (FN <> '') then begin

          if (Res <> nil) and Res.Deleted then begin
            PurgeEvents(Res);
            PurgeContacts(Res);
            PurgeTasks(Res);
            if ResourceTable.Locate(FN, Res.ResourceID, [])
            then ResourceTable.Delete;
            if Resource = Res then
              ResourceID := -1;
            Res.Free;
            Continue;
          end;

          { Dump this resource to the DB }
          if (Res <> nil) and Res.Changed then begin
            with ResourceTable do begin
              if Locate(FN, Res.ResourceID, []) then
                { existing record found }
                Edit
              else
                { this is a new record}
                Append;

              try
                if Res.ResourceID > -1 then
                  FieldByName(FN).AsInteger := Res.ResourceID;

                FN := GetFieldName(FResourceMappings, 'Description');
                if FN <> '' then
                  FieldByName(FN).AsString := Res.Description;

                FN := GetFieldName(FResourceMappings, 'Notes');
                if FN <> '' then
                  FieldByName(FN).AsString := Res.Notes;

                FN := GetFieldName(FResourceMappings, 'ResourceActive');
                if FN <> '' then
                  FieldByName(FN).AsBoolean := Res.Active;

                FN := GetFieldName(FResourceMappings, 'UserField0');
                if FN <> '' then
                  FieldByName(FN).AsString := Res.UserField0;

                FN := GetFieldName(FResourceMappings, 'UserField1');
                if FN <> '' then
                  FieldByName(FN).AsString := Res.UserField1;

                FN := GetFieldName(FResourceMappings, 'UserField2');
                if FN <> '' then
                  FieldByName(FN).AsString := Res.UserField2;

                FN := GetFieldName(FResourceMappings, 'UserField3');
                if FN <> '' then
                  FieldByName(FN).AsString := Res.UserField3;

                FN := GetFieldName(FResourceMappings, 'UserField4');
                if FN <> '' then
                  FieldByName(FN).AsString := Res.UserField4;

                FN := GetFieldName(FResourceMappings, 'UserField5');
                if FN <> '' then
                  FieldByName(FN).AsString := Res.UserField5;

                FN := GetFieldName(FResourceMappings, 'UserField6');
                if FN <> '' then
                  FieldByName(FN).AsString := Res.UserField6;

                FN := GetFieldName(FResourceMappings, 'UserField7');
                if FN <> '' then
                  FieldByName(FN).AsString := Res.UserField7;

                FN := GetFieldName(FResourceMappings, 'UserField8');
                if FN <> '' then
                  FieldByName(FN).AsString := Res.UserField8;

                FN := GetFieldName(FResourceMappings, 'UserField9');
                if FN <> '' then
                  FieldByName(FN).AsString := Res.UserField9;

                Post;
              except
                Cancel;
                raise EDBPostError.Create;
              end;
              if Res.ResourceID = -1 then begin
                FN := GetFieldName(FResourceMappings, 'ResourceID');
                Res.ResourceID := FieldByName(FN).AsInteger;
              end;
            end;
            if Res.ResourceID = ResourceID then begin
              PostEvents;
              PostContacts;
              PostTasks;
            end;
            Res.Changed := false;
          end;
        end;
      end;
      if not Loading then begin
        NotifyDependents;
      end;
    end;
  finally
    Loading := false;
  end;
end;
{=====}

{ - New}
procedure TVpFlexDataStore.PurgeResource(Res: TVpResource);
begin
  Res.Deleted := true;
  PostResources;
  Load;
end;
{=====}

{ - New}
procedure TVpFlexDataStore.PurgeEvents(Res: TVpResource);
var
  I: integer;
begin
  for I := 0 to pred(Res.Schedule.EventCount) do begin
    TVpEvent(Res.Schedule.GetEvent(I)).Deleted := true;
  end;
  PostEvents;
  Res.Schedule.ClearEvents;
end;
{=====}

{ - New}
procedure TVpFlexDataStore.PurgeContacts(Res: TVpResource);
var
  I: integer;
begin
  for I := 0 to pred(Res.Contacts.Count) do begin
    TVpContact(Res.Contacts.GetContact(I)).Deleted := true;
  end;
  PostContacts;
  Res.Contacts.ClearContacts;
end;
{=====}

{ - New}
procedure TVpFlexDataStore.PurgeTasks(Res: TVpResource);
var
  I: integer;
begin
  for I := 0 to pred(Res.Tasks.Count) do begin
    TVpTask(Res.Tasks.GetTask(I)).Deleted := true;
  end;
  PostTasks;
  Res.Tasks.ClearTasks;
end;
{=====}

procedure TVpFlexDataStore.SetResourceDataSrc(Value: TDataSource);
begin
  if FResourceDataSrc <> Value then begin
    FResourceDataSrc := Value;
    if not (csDesigning in ComponentState) then                          
      Load;                                                              
  end;
end;
{=====}

procedure TVpFlexDataStore.SetEventsDataSrc(Value: TDataSource);
begin
  if FEventsDataSrc <> Value then begin
    FEventsDataSrc := Value;
    if not (csDesigning in ComponentState) then                          
      Load;                                                              
  end;
end;
{=====}

procedure TVpFlexDataStore.SetContactsDataSrc(Value: TDataSource);
begin
  if FContactsDataSrc <> Value then begin
    FContactsDataSrc := Value;
    if not (csDesigning in ComponentState) then                          
      Load;                                                              
  end;
end;
{=====}

procedure TVpFlexDataStore.SetTasksDataSrc(Value: TDataSource);
begin
  if FTasksDataSrc <> Value then begin
    FTasksDataSrc := Value;
    if not (csDesigning in ComponentState) then                          
      Load;                                                              
  end;
end;
{=====}

{ - New Field Mapping Streamers}
procedure TVpFlexDataStore.DefineProperties(Filer: TFiler);              
begin                                                                    
  inherited;                                                             
  Filer.DefineProperty('ResourceFieldMappings', LoadResMapping,          
    StoreResMapping, FResourceMappings.Count > 0);                       
  Filer.DefineProperty('EventFieldMappings', LoadEventMapping,           
    StoreEventMapping, FEventMappings.Count > 0);                        
  Filer.DefineProperty('ContactFieldMappings', LoadContactMapping,       
    StoreContactMapping, FContactMappings.Count > 0);                    
  Filer.DefineProperty('TaskFieldMappings', LoadTaskMapping,             
    StoreTaskMapping, FTaskMappings.Count > 0);                          
end;                                                                     
{=====}                                                                  

procedure TVpFlexDataStore.LoadResMapping(Reader: TReader);              
var                                                                      
  FM: TVpFieldMapping;                                                   
begin                                                                    
  FResourceMappings.Clear;                                               
  Reader.ReadListBegin;                                                  
  while not Reader.EndOfList do begin                                    
    FM := TVpFieldMapping(FResourceMappings.Add);                        
    FM.DBField := Reader.ReadString;                                     
    FM.VPField := Reader.ReadString;                                     
  end;                                                                   
  Reader.ReadListEnd;                                                    
end;                                                                     
{=====}                                                                  

procedure TVpFlexDataStore.StoreResMapping(Writer: TWriter);             
var                                                                      
  i: integer;                                                            
  FM: TVpFieldMapping;                                                   
begin                                                                    
  Writer.WriteListBegin;                                                 
    for I := 0 to pred(FResourceMappings.Count) do begin                 
      FM := TVpFieldMapping(FResourceMappings.Items[I]);                 
      Writer.WriteString(FM.DBField);                                    
      Writer.WriteString(FM.VPField);                                    
    end;                                                                 
  Writer.WriteListEnd;                                                   
end;                                                                     
{=====}                                                                  

procedure TVpFlexDataStore.LoadEventMapping(Reader: TReader);            
var                                                                      
  FM: TVpFieldMapping;                                                   
begin                                                                    
  FEventMappings.Clear;                                                  
  Reader.ReadListBegin;                                                  
  while not Reader.EndOfList do begin                                    
    FM := TVpFieldMapping(FEventMappings.Add);                           
    FM.DBField := Reader.ReadString;                                     
    FM.VPField := Reader.ReadString;                                     
  end;                                                                   
  Reader.ReadListEnd;                                                    
end;                                                                     
{=====}                                                                  

procedure TVpFlexDataStore.StoreEventMapping(Writer: TWriter);           
var                                                                      
  i: integer;                                                            
  FM: TVpFieldMapping;                                                   
begin                                                                    
  Writer.WriteListBegin;                                                 
    for I := 0 to pred(FEventMappings.Count) do begin                    
      FM := TVpFieldMapping(FEventMappings.Items[I]);                    
      Writer.WriteString(FM.DBField);                                    
      Writer.WriteString(FM.VPField);                                    
    end;                                                                 
  Writer.WriteListEnd;                                                   
end;                                                                     
{=====}                                                                  

procedure TVpFlexDataStore.LoadContactMapping(Reader: TReader);          
var                                                                      
  FM: TVpFieldMapping;                                                   
begin                                                                    
  FContactMappings.Clear;                                                
  Reader.ReadListBegin;                                                  
  while not Reader.EndOfList do begin                                    
    FM := TVpFieldMapping(FContactMappings.Add);                         
    FM.DBField := Reader.ReadString;                                     
    FM.VPField := Reader.ReadString;                                     
  end;                                                                   
  Reader.ReadListEnd;                                                    
end;                                                                     
{=====}                                                                  

procedure TVpFlexDataStore.StoreContactMapping(Writer: TWriter);         
var                                                                      
  i: integer;                                                            
  FM: TVpFieldMapping;                                                   
begin                                                                    
  Writer.WriteListBegin;                                                 
    for I := 0 to pred(FContactMappings.Count) do begin                  
      FM := TVpFieldMapping(FContactMappings.Items[I]);                  
      Writer.WriteString(FM.DBField);                                    
      Writer.WriteString(FM.VPField);                                    
    end;                                                                 
  Writer.WriteListEnd;                                                   
end;                                                                     
{=====}                                                                  

procedure TVpFlexDataStore.LoadTaskMapping(Reader: TReader);             
var                                                                      
  FM: TVpFieldMapping;                                                   
begin                                                                    
  FTaskMappings.Clear;                                                   
  Reader.ReadListBegin;                                                  
  while not Reader.EndOfList do begin                                    
    FM := TVpFieldMapping(FTaskMappings.Add);                            
    FM.DBField := Reader.ReadString;                                     
    FM.VPField := Reader.ReadString;                                     
  end;                                                                   
  Reader.ReadListEnd;                                                    
end;                                                                     
{=====}                                                                  

procedure TVpFlexDataStore.StoreTaskMapping(Writer: TWriter);            
var                                                                      
  i: integer;                                                            
  FM: TVpFieldMapping;                                                   
begin                                                                    
  Writer.WriteListBegin;                                                 
    for I := 0 to pred(FTaskMappings.Count) do begin                     
      FM := TVpFieldMapping(FTaskMappings.Items[I]);                     
      Writer.WriteString(FM.DBField);                                    
      Writer.WriteString(FM.VPField);                                    
    end;                                                                 
  Writer.WriteListEnd;                                                   
end;                                                                     
{=====}                                                                  

procedure TVpFlexDataStore.Loaded;
begin
  inherited;
  if not (csDesigning in ComponentState) then
    Connected := AutoConnect;
end;
{=====}

function TVpFlexDataStore.GetNextID(TableName: string): Integer;
begin
  { The FlexDataStore has no idea what type of database you are connected to }
  { beyond TDataset compatibility, so it cannot presume to generate record   }
  { ID's for you. }

  { If your database uses AutoIncrementing Record ID fields, then you may    }
  { leave the GetNextID alone, otherwise, you will need to create an         }
  { OnGetNextID event handler and define it to properly return the next ID   }
  { for each table. }

  result := -1;

  if Assigned(OnGetNextID) then
    result := OnGetNextID(Self, TableName);
end;
{=====}

{ returns the name of the dataset field currently mapped to the   }      
{ specified internal Visual PlanIt field. If not field is mapped, }      
{ then it returns an empty string                                 }      
function TVpFlexDataStore.GetFieldName(Mappings: TCollection;            
  VPField: string): string;                                              
var                                                                      
  I: integer;                                                            
  FM: TVpFieldMapping;                                                   
begin                                                                    
  I := 0;                                                                
  result := '';                                                          
  while (I < Mappings.Count)                                             
  and (result = '') do begin                                             
    FM := TVpFieldMapping(Mappings.Items[I]);                            
    if Uppercase(FM.VPField) = Uppercase(VPField) then begin             
      result := FM.DBField;                                              
      I := FResourceMappings.Count;                                      
    end;                                                                 
    Inc(I);                                                              
  end;                                                                   
end;                                                                     
{=====}                                                                  

procedure TVpFlexDataStore.SetFilterCriteria(aTable: TDataset;           
  aUseDateTime: Boolean; aResourceID: Integer; aStartDateTime,           
  aEndDateTime: TDateTime);                                              
begin                                                                    
  if Assigned(OnSetFilterCriteria) then                                  
    OnSetFilterCriteria(aTable, aUseDateTime, aResourceID,               
      aStartDateTime, aEndDateTime)                                      
  else                                                                   
    inherited;                                                           
end;                                                                     
{=====}                                                                  

{ TVpDataSources }

constructor TVpDataSources.Create(Owner: TVpFlexDataStore);
begin
  FOwner := Owner;
end;
{=====}

function TVpDataSources.GetContactsDataSrc: TDataSource;
begin
  result := FOwner.ContactsDataSource;
end;
{=====}

function TVpDataSources.GetEventsDataSrc: TDataSource;
begin
  result := FOwner.EventsDataSource;
end;
{=====}

function TVpDataSources.GetResourceDataSrc: TDataSource;
begin
  result := FOwner.ResourceDataSource;
end;
{=====}

function TVpDataSources.GetTasksDataSrc: TDataSource;
begin
  result := FOwner.TasksDataSource;
end;
{=====}

procedure TVpDataSources.SetContactsDataSrc(const Value: TDataSource);
begin
  FOwner.ContactsDataSource := Value;
end;
{=====}

procedure TVpDataSources.SetEventsDataSrc(const Value: TDataSource);
begin
  FOwner.EventsDataSource := Value;
end;
{=====}

procedure TVpDataSources.SetResourceDataSrc(const Value: TDataSource);
begin
  FOwner.ResourceDataSource := Value;
end;
{=====}

procedure TVpDataSources.SetTasksDataSrc(const Value: TDataSource);
begin
  FOwner.TasksDataSource := Value;
end;
{=====}



end.
