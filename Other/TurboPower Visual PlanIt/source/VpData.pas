{*********************************************************}
{*                   VPDATA.PAS 1.03                     *}
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

unit VpData;
  { Data classes for Visual PlanIt's resources, events, tasks, contacts, etc... }

interface

uses
  SysUtils, Classes,
  {$IFDEF VERSION6} Types, {$ENDIF}
  Windows, VpBase, VpSR, VpConst, Dialogs;

type
  TVpEventRec = packed record
    Rec      : TRect;
    IconRect : TRect;                                                                                                               
    Event    : Pointer;
  end;

type
  TVpEventArray = array of TVpEventRec;
  
  TVpAlarmAdvType = (atMinutes, atHours, atDays);

  TVpRepeatType = (rtNone, rtDaily, rtWeekly, rtMonthlyByDay, rtMonthlyByDate,
    rtYearlyByDay, rtYearlyByDate, rtCustom);

  TVpContactSort = (csLastFirst, csFirstLast);

  { forward declarations }
  TVpResource = class;
  TVpTasks    = class;
  TVpSchedule = class;
  TVpEvent    = class;
  TVpContacts = class;
  TVpContact  = class;
  TVpTask     = class;

  TVpResources = class
  protected{private}
    FOwner: TObject;
    FResourceList: TList;
    function Compare(Descr1, Descr2: string): Integer;
    function GetItem(Index: Integer): TVpResource;
    function GetCount: Integer;
    function NextResourceID: Integer;
  public
    constructor Create(Owner: TObject);
    destructor Destroy; override;
    function AddResource(ResID: Integer): TVpResource;
    function FindResourceByName (AName : string) : TVpResource;
    function GetResource(ID: integer): TVpResource;
    procedure ClearResources;
    procedure RemoveResource(Resource: TVpResource);
    procedure Sort;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TVpResource read GetItem;
    property Owner: TObject read FOwner;
  end;

  TVpResource = class
  protected{private}
    FLoading : Boolean;
    FOwner: TVpResources;
    FActive: Boolean;    { Internal flag used to determine whether to display }
                         { this resource                                      }
    FItemIndex: integer;
    FChanged: Boolean;
    FDeleted: Boolean;
    FEventsDirty: Boolean;
    FContactsDirty: Boolean;
    FTasksDirty: Boolean;
    FSchedule: TVpSchedule;
    FTasks: TVpTasks;
    FNotes: string;
    FDescription: string;
    { reserved for your use }
    FUserField0: string;
    FUserField1: string;
    FUserField2: string;
    FUserField3: string;
    FUserField4: string;
    FUserField5: string;
    FUserField6: string;
    FUserField7: string;
    FUserField8: string;
    FUserField9: string;
    FResourceID: Integer;
    FContacts: TVpContacts;
    function GetSchedule: TVpSchedule;
    procedure SetChanged(Value: Boolean);
    procedure SetDeleted(Value: Boolean);
    procedure SetDescription(const Value: string);
    procedure SetResourceID(const Value: Integer);
    procedure SetSchedule(const Value: TVpSchedule);
    procedure SetTasks(const Value: TVpTasks);
    procedure SetNotes(const Value: string);
    procedure SetContacts(const Value: TVpContacts);
  public
    constructor Create(Owner: TVpResources);
    destructor Destroy; override;
    property Loading: Boolean read FLoading write FLoading;
    property Changed: Boolean read FChanged write SetChanged;
    property Deleted: Boolean read FDeleted write SetDeleted;
    property EventsDirty: Boolean read FEventsDirty write FEventsDirty;
    property ContactsDirty: Boolean read FContactsDirty write FContactsDirty;
    property TasksDirty: Boolean read FTasksDirty write FTasksDirty;
    property Active: Boolean read FActive write FActive;
    property Owner: TVpResources read FOwner;
    property ItemIndex: integer read FItemIndex;
    property Notes: string read FNotes write SetNotes;
    property ResourceID: Integer read FResourceID write SetResourceID;
    property Description: string read FDescription write SetDescription;
    property Schedule: TVpSchedule read GetSchedule write SetSchedule;
    property Tasks: TVpTasks read FTasks write SetTasks;
    property Contacts: TVpContacts read FContacts write SetContacts;
    { Reserved for your use }
    property UserField0: string read FUserField0 write FUserField0;
    property UserField1: string read FUserField1 write FUserField1;
    property UserField2: string read FUserField2 write FUserField2;
    property UserField3: string read FUserField3 write FUserField3;
    property UserField4: string read FUserField4 write FUserField4;
    property UserField5: string read FUserField5 write FUserField5;
    property UserField6: string read FUserField6 write FUserField6;
    property UserField7: string read FUserField7 write FUserField7;
    property UserField8: string read FUserField8 write FUserField8;
    property UserField9: string read FUserField9 write FUserField9;
  end;

  TVpSchedule = class
  protected{private}
    FOwner    : TVpResource;
    FEventList: TList;
    FBatchUpdate: Integer;
    function Compare(Time1, Time2: TDateTime): Integer;
    function FindTimeSlot(StartTime, EndTime: TDateTime): Boolean;
    function GetCount: Integer;
  public
    constructor Create(Owner: TVpResource);
    destructor Destroy; override;
    function AddEvent(RecordID: Integer; StartTime,
      EndTime: TDateTime): TVpEvent;
    procedure DeleteEvent(Event: TVpEvent);
    function GetEvent(Index: Integer): TVpEvent;
    function RepeatsOn(Event: TVpEvent; Day: TDateTime): Boolean;
    procedure Sort;
    procedure ClearEvents;
    procedure BatchUpdate(Value: Boolean);
    function EventCountByDay(Value: TDateTime): Integer;
    procedure EventsByDate(Date: TDateTime; EventList: TList);
    procedure AllDayEventsByDate(Date: TDateTime; EventList: TList);
    property Owner: TVpResource read FOwner;
    property EventCount: Integer read GetCount;
  end;

  TVpEvent = class
  protected{private}
    FOwner: TVpSchedule;
    FItemIndex: Integer;
    FChanged: Boolean;
    FDeleted: Boolean;
    FLoading: Boolean;
    FPrivateEvent: Boolean;
    FAlarmSet: Boolean;
    FDingPath: string;
    FAllDayEvent: Boolean;
    FCategory: Integer;
    FAlarmAdv: Integer;
    FAlertDisplayed: Boolean;
    FAlarmAdvType: TVpAlarmAdvType;
    FRecordID: Integer;
    FNote: string;
    FDescription: string;
    FStartTime: TDateTime;
    FEndTime: TDateTime;
    FSnoozeTime: TDateTime;
    FRepeatCode: TVpRepeatType;
    FRepeatRangeEnd: TDateTime;
    FCustInterval: Integer;
    { reserved for your use }
    FUserField0: string;
    FUserField1: string;
    FUserField2: string;
    FUserField3: string;
    FUserField4: string;
    FUserField5: string;
    FUserField6: string;
    FUserField7: string;
    FUserField8: string;
    FUserField9: string;
    procedure SetAllDayEvent(Value: Boolean);
    procedure SetItemIndex(Value: Integer);
    procedure SetChanged(Value: Boolean);
    procedure SetDeleted(Value: Boolean);
    procedure SetDingPath(Value: string);
    procedure SetAlarmAdv(Value: Integer);
    procedure SetAlarmAdvType(Value: TVpAlarmAdvType);
    procedure SetSnoozeTime(Value: TDateTime);
    procedure SetAlarmSet(Value: Boolean);
    procedure SetCategory(Value: Integer);
    procedure SetDescription(const Value: string);
    procedure SetEndTime(Value: TDateTime);
    procedure SetNote(const Value: string);
    procedure SetRecordID(Value: Integer);
    procedure SetStartTime(Value: TDateTime);
    procedure SetCustInterval(Value: Integer);
    procedure SetRepeatCode(Value: TVpRepeatType);
    procedure SetRepeatRangeEnd(Value: TDateTime);
  public
    constructor Create(Owner: TVpSchedule);
    destructor Destroy; override;
    property AlarmWavPath: string
      read FDingPath write SetDingPath;
    property AlertDisplayed: Boolean
      read FAlertDisplayed write FAlertDisplayed;
    property AllDayEvent: Boolean
      read FAllDayEvent write SetAllDayEvent;
    property Changed: Boolean
      read FChanged write SetChanged;
    property Deleted: Boolean
      read FDeleted write SetDeleted;
    property ItemIndex: Integer
      read FItemIndex;
    property RecordID : Integer
      read FRecordID write SetRecordID;
    property StartTime : TDateTime
      read FStartTime write SetStartTime;
    property EndTime : TDateTime
      read FEndTime write SetEndTime;
    property Description : string
      read FDescription write SetDescription;
    property Note : string
      read FNote write SetNote;
    property Category : Integer
      read FCategory write SetCategory;
    property AlarmSet : Boolean
      read FAlarmSet write SetAlarmSet;
    property AlarmAdv : Integer
      read FAlarmAdv write SetAlarmAdv;
    property Loading : Boolean
      read FLoading write FLoading;
    { 0=Minutes, 1=Hours, 2=Days   }
    property AlarmAdvType : TVpAlarmAdvType
      read FAlarmAdvType write SetAlarmAdvType;
    property SnoozeTime : TDateTime read FSnoozeTime write SetSnoozeTime;
    { rtNone, rtDaily, rtWeekly, rtMonthlyByDay, rtMonthlyByDate, }
    { rtYearlyByDay, rtYearlyByDate, rtCustom                     }
    property RepeatCode   : TVpRepeatType read FRepeatCode write SetRepeatCode;
    property RepeatRangeEnd: TDateTime
      read FRepeatRangeEnd write SetRepeatRangeEnd;
    { Custom Repeat Interval in seconds }
    { is Zero if IntervalCode <> 7      }
    property CustInterval : Integer read FCustInterval write SetCustInterval;
    property Owner: TVpSchedule read FOwner;
    { Reserved for your use }
    property UserField0: string read FUserField0 write FUserField0;
    property UserField1: string read FUserField1 write FUserField1;
    property UserField2: string read FUserField2 write FUserField2;
    property UserField3: string read FUserField3 write FUserField3;
    property UserField4: string read FUserField4 write FUserField4;
    property UserField5: string read FUserField5 write FUserField5;
    property UserField6: string read FUserField6 write FUserField6;
    property UserField7: string read FUserField7 write FUserField7;
    property UserField8: string read FUserField8 write FUserField8;
    property UserField9: string read FUserField9 write FUserField9;
  end;

  TVpTasks = class
  protected{private}
    FOwner: TVpResource;
    FTaskList: TList;
    FBatchUpdate: Integer;
  public
    constructor Create(Owner: TVpResource);
    destructor Destroy; override;
    procedure BatchUpdate(value: Boolean);
    procedure Sort;
    function Compare(Item1, Item2: TVpTask): Integer;
    function AddTask(RecordID: Integer): TVpTask;
    function Count : Integer;
    function CountByDay(Date: TDateTime): Integer;
    function Last: TVpTask;
    function LastByDay(Date: TDateTime): TVpTask;
    function First: TVpTask;
    function FirstByDay(Date: TDateTime): TVpTask;

    procedure DeleteTask(Task: TVpTask);
    function GetTask(Index: Integer): TVpTask;
    procedure ClearTasks;
    property Owner: TVpREsource read FOwner;
  end;

  TVpTask = class
  protected{private}
    FLoading: Boolean;
    FOwner: TVpTasks;
    FChanged: Boolean;
    FDeleted: Boolean;
    FItemIndex: Integer;
    FPriority: Integer;
    FCategory: Integer;
    FComplete: Boolean;
    FDescription: string;
    FDetails: string;
    FCreatedOn: TDateTime;
    FCompletedOn: TDateTIme;
    FRecordID: Integer;
    FDueDate: TDateTime;

    { reserved for your use }
    FUserField0: string;
    FUserField1: string;
    FUserField2: string;
    FUserField3: string;
    FUserField4: string;
    FUserField5: string;
    FUserField6: string;
    FUserField7: string;
    FUserField8: string;
    FUserField9: string;

    procedure SetCategory(const Value: Integer);
    procedure SetChanged(const Value: Boolean);
    procedure SetComplete(const Value: Boolean);
    procedure SetCompletedOn(const Value: TDateTime);
    procedure SetCreatedOn(const Value: TDateTime);
    procedure SetDescription(const Value: string);
    procedure SetDetails(const Value: string);
    procedure SetDueDate(const Value: TDateTime);
    procedure SetPriority(const Value: Integer);
    function IsOverdue: Boolean;
  public
    constructor Create(Owner: TVpTasks);
    destructor Destroy; override;
    property Loading: Boolean read FLoading write FLoading;
    property Changed: Boolean read FChanged write SetChanged;
    property Deleted: Boolean read FDeleted write FDeleted;
    property DueDate: TDateTime read FDueDate write SetDueDate;
    property Description: string read FDescription write SetDescription;
    property ItemIndex: Integer read FItemIndex;
    property Details: string read FDetails write SetDetails;
    property Complete: Boolean read FComplete write SetComplete;
    property RecordID: Integer read FRecordID write FRecordID;
    property CreatedOn: TDateTime read FCreatedOn write SetCreatedOn;
    property CompletedOn: TDateTIme read FCompletedOn write SetCompletedOn;
    property Owner: TVpTasks read FOwner;

    { Not implemented yet }
    property Priority: Integer read FPriority write SetPriority;
    property Category: Integer read FCategory write SetCategory;

    { Reserved for your use }
    property UserField0: string read FUserField0 write FUserField0;
    property UserField1: string read FUserField1 write FUserField1;
    property UserField2: string read FUserField2 write FUserField2;
    property UserField3: string read FUserField3 write FUserField3;
    property UserField4: string read FUserField4 write FUserField4;
    property UserField5: string read FUserField5 write FUserField5;
    property UserField6: string read FUserField6 write FUserField6;
    property UserField7: string read FUserField7 write FUserField7;
    property UserField8: string read FUserField8 write FUserField8;
    property UserField9: string read FUserField9 write FUserField9;
  end;

  TVpContacts  = class
  protected
    FOwner        : TVpResource;
    FContactsList : TList;
    FBatchUpdate  : Integer;
    FContactSort  : TVpContactSort;
    function Compare(Item1, Item2: TVpContact): Integer;
    procedure SetContactSort (const v : TVpContactSort);
  public
    constructor Create(Owner: TVpResource);
    destructor Destroy; override;
    procedure BatchUpdate(Value: Boolean);
    function Count: Integer;
    function Last:TVpContact;
    function First: TVpContact;
    procedure Sort;
    function AddContact(RecordID: Integer): TVpContact;
    procedure DeleteContact(Contact: TVpContact);
    function GetContact(Index: Integer): TVpContact;
    procedure ClearContacts;

    { new functions introduced to support the new buttonbar component }  
    function FindContactByName(const Name: string;                       
      CaseInsensitive: Boolean = True): TVpContact;                      
    function FindContactIndexByName(const Name: string;                  
      CaseInsensitive: Boolean = True): Integer;                         

    property ContactsList: TList read FContactsList;
    property ContactSort : TVpContactSort
             read FContactSort write SetContactSort default csLastFirst;
  end;

  TVpContact = class
  protected{private}
    FLoading      : Boolean;
    FOwner        : TVpContacts;
    FChanged      : Boolean;
    FItemIndex    : Integer;
    FRecordID     : Integer;
    FDeleted      : Boolean;
    FPosition     : string;
    FLastName     : string;
    FFirstName    : string;
    FBirthDate    : TDateTime;
    FAnniversary  : TDateTime;
    FTitle        : string;
    FCompany      : string;
    FEmail        : string;
    FPhone1       : string;
    FPhone2       : string;
    FPhone3       : string;
    FPhone4       : string;
    FPhone5       : string;
    FPhoneType1   : integer;
    FPhoneType2   : integer;
    FPhoneType3   : integer;
    FPhoneType4   : integer;
    FPhoneType5   : integer;
    FAddress      : string;
    FCity         : string;
    FState        : string;
    FZip          : string;
    FCountry      : string;
    FNote         : string;
    FPrivateRec   : boolean;
    FCategory     : integer;
    FCustom1      : string;
    FCustom2      : string;
    FCustom3      : string;
    FCustom4      : string;
    { reserved for your use }
    FUserField0   : string;
    FUserField1   : string;
    FUserField2   : string;
    FUserField3   : string;
    FUserField4   : string;
    FUserField5   : string;
    FUserField6   : string;
    FUserField7   : string;
    FUserField8   : string;
    FUserField9   : string;

    procedure SetAddress(const Value: string);
    procedure SetBirthDate(Value: TDateTime);
    procedure SetAnniversary(Value: TDateTime);
    procedure SetCategory( Value: integer);
    procedure SetChanged(Value: Boolean);
    procedure SetCity(const Value: string);
    procedure SetCompany(const Value: string);
    procedure SetCountry(const Value: string);
    procedure SetCustom1(const Value: string);
    procedure SetCustom2(const Value: string);
    procedure SetCustom3(const Value: string);
    procedure SetCustom4(const Value: string);
    procedure SetDeleted(Value: Boolean);
    procedure SetEMail(const Value: string);
    procedure SetFirstName(const Value: string);
    procedure SetLastName(const Value: string);
    procedure SetNote(const Value: string);
    procedure SetPhone1(const Value: string);
    procedure SetPhone2(const Value: string);
    procedure SetPhone3(const Value: string);
    procedure SetPhone4(const Value: string);
    procedure SetPhone5(const Value: string);
    procedure SetPhoneType1(Value: integer);
    procedure SetPhoneType2(Value: integer);
    procedure SetPhoneType3(Value: integer);
    procedure SetPhoneType4(Value: integer);
    procedure SetPhoneType5(Value: integer);
    procedure SetPosition(const Value: string);
    procedure SetRecordID(Value: Integer);
    procedure SetState(const Value: string);
    procedure SetTitle(const Value: string);
    procedure SetZip(const Value: string);

  public
    constructor Create(Owner: TVpContacts);
    destructor Destroy; override;
    function FullName : string;
    property Loading      : Boolean read FLoading write FLoading;
    property Changed      : Boolean read FChanged write SetChanged;
    property Deleted      : Boolean read FDeleted write SetDeleted;
    property RecordID     : Integer read FRecordID write SetRecordID;
    property Position     : string read FPosition write SetPosition;
    property FirstName    : string read FFirstName write SetFirstName;
    property LastName     : string read FLastName write SetLastName;
    property BirthDate    : TDateTime read FBirthdate write SetBirthdate;
    property Anniversary  : TDateTime read FAnniversary write SetAnniversary;
    property Title        : string read FTitle write SetTitle;
    property Company      : string read FCompany write SetCompany;
    property EMail        : string read FEmail write SetEMail;
    property Phone1       : string read FPhone1 write SetPhone1;
    property Phone2       : string read FPhone2 write SetPhone2;
    property Phone3       : string read FPhone3 write SetPhone3;
    property Phone4       : string read FPhone4 write SetPhone4;
    property Phone5       : string read FPhone5 write SetPhone5;
    property PhoneType1   : integer read FPhoneType1 write SetPhoneType1;
    property PhoneType2   : integer read FPhoneType2 write SetPhoneType2;
    property PhoneType3   : integer read FPhoneType3 write SetPhoneType3;
    property PhoneType4   : integer read FPhoneType4 write SetPhoneType4;
    property PhoneType5   : integer read FPhoneType5 write SetPhoneType5;
    property Address      : string read FAddress write SetAddress;
    property City         : string read FCity write SetCity;
    property State        : string read FState write SetState;
    property Zip          : string read FZip write SetZip;
    property Country      : string read FCountry write SetCountry;
    property Note         : string read FNote write SetNote;
    property Category     : integer read FCategory write SetCategory;
    property Custom1      : string read FCustom1 write SetCustom1;
    property Custom2      : string read FCustom2 write SetCustom2;
    property Custom3      : string read FCustom3 write SetCustom3;
    property Custom4      : string read FCustom4 write SetCustom4;
    property Owner        : TVpContacts read FOwner write FOwner;
    { Reserved for your use }
    property UserField0   : string read FUserField0 write FUserField0;
    property UserField1   : string read FUserField1 write FUserField1;
    property UserField2   : string read FUserField2 write FUserField2;
    property UserField3   : string read FUserField3 write FUserField3;
    property UserField4   : string read FUserField4 write FUserField4;
    property UserField5   : string read FUserField5 write FUserField5;
    property UserField6   : string read FUserField6 write FUserField6;
    property UserField7   : string read FUserField7 write FUserField7;
    property UserField8   : string read FUserField8 write FUserField8;
    property UserField9   : string read FUserField9 write FUserField9;
  end;

implementation

uses
  VpException, VpMisc;

{ TVpResources }
(*****************************************************************************)

constructor TVpResources.Create(Owner: TObject);
begin
  inherited Create;
  FOwner := Owner;
  FResourceList := TList.Create;
end;
{=====}

destructor TVpResources.Destroy;
begin
  ClearResources;
  FResourceList.Free;
  inherited;
end;
{=====}

function TVpResources.GetItem(Index: Integer): TVpResource;
begin
  result := TVpResource(FResourceList.List^[Index]);
end;
{=====}

function TVpResources.GetCount: Integer;
begin
  result := FResourceList.Count;
end;
{=====}

function TVpResources.NextResourceID: Integer;
var
  I, ID: Integer;
  Res: TVpResource;
begin
  ID := 0;
  for I := 0 to pred(FResourceList.Count) do begin
    Res := GetResource(I);
    if (Res <> nil)
    and (ID <= Res.ResourceID) then
      Inc(ID);
  end;
  result := ID;
end;
{=====}

function TVpResources.AddResource(ResID: Integer): TVpResource;
var
  Resource : TVpResource;
begin
  Resource := TVpResource.Create(Self);
  try
    Resource.Loading := true;
    Resource.FItemIndex := FResourceList.Add(Resource);
    Resource.ResourceID := ResID;
    Resource.Active := true;
    Resource.Loading := false;
    result := Resource;
  except
    Resource.Free;
    raise EFailToCreateResource.Create;
  end;
end;
{=====}

function TVpResources.FindResourceByName (AName : string) : TVpResource;
var
  i : Integer;

begin
  Result := nil;
  AName := LowerCase (AName);
  for i := 0 to Count - 1 do
    if LowerCase (Items[i].Description) = AName then begin
      Result := Items[i];
      Break;
    end;
end;
{=====}

function TVpResources.GetResource(ID: integer): TVpResource;
var
  I: Integer;
  Res: TVpResource;
begin
  result := nil;
  for I := 0 to pred(FResourceList.Count) do begin
    res := FResourceList.Items[I];
    if Res.ResourceID = ID then begin
      result := Res;
      Exit;
    end;
  end;
end;
{=====}

procedure TVpResources.ClearResources;
begin
  while FResourceList.Count > 0 do
    TVpResource(FResourceList.Last).Free;
end;
{=====}

procedure TVpResources.RemoveResource(Resource: TVpREsource);
begin
  { The resource removes the list entry in its destructor }
  Resource.Free;
end;
{=====}

procedure TVpResources.Sort;
var
  i, j       : integer;
  IndexOfMin : integer;
  Temp       : pointer;
  CompResult : integer; {Comparison Result}
begin
  for i := 0 to pred(FResourceList.Count) do begin
    IndexOfMin := i;
    for j := i to FResourceList.Count - 1 do begin

      { compare description item[j] and item[i] }
      CompResult := Compare(TVpResource(FResourceList.List^[j]).Description,
        TVpResource(FResourceList.List^[IndexOfMin]).Description);

      { if the description of j is less than the description of i then flip 'em}
      if CompResult < 0 then
        IndexOfMin := j;
    end;

    Temp := FResourceList.List^[i];
    FResourceList.List^[i] := FResourceList.List^[IndexOfMin];
    FResourceList.List^[IndexOfMin] := Temp;
  end;

  { Fix object embedded ItemIndexes }
  for i := 0 to pred(FResourceList.Count) do begin
    TVpResource(FResourceList.List^[i]).FItemIndex := i;
  end;
end;
{=====}

{ Used in the above sort procedure.  Compares the descriptions of the two }
{ passed in events.                                                       }
function TVpResources.Compare(Descr1, Descr2: string): Integer;
begin
  { Compares the value of the Item descriptions }

  if Descr1 < Descr2 then
    result := -1

  else if Descr1 = Descr2 then
    result := 0

  else
    {Descr2 is less than Descr1}
    result := 1;
end;
{=====}


{ TVpResource }
(*****************************************************************************)
constructor TVpResource.Create(Owner: TVpResources);
begin
  inherited Create;
  FOwner := Owner;
  FSchedule := TVpSchedule.Create(Self);
  FTasks := TVpTasks.Create(Self);
  FContacts := TVpContacts.Create(Self);
  FItemIndex := -1;
  FActive := false;
end;
{=====}

destructor TVpResource.Destroy;
begin
  { Clear and free the schedule, tasks and contacts }
  FSchedule.ClearEvents;
  FSchedule.Free;
  FTasks.ClearTasks;
  FTasks.Free;
  FContacts.ClearContacts;
  FContacts.Free;

  { remove self from Resources list }
  if (FItemIndex > -1) and (FOwner <> nil) then
    FOwner.FResourceList.Delete(FItemIndex);

  inherited;
end;
{=====}

procedure TVpResource.SetContacts(const Value: TVpContacts);
begin
  FContacts := Value;
end;
{=====}

procedure TVpResource.SetChanged(Value: Boolean);
begin
  if Loading then Exit;

  if Value <> FChanged then begin
    FChanged := Value;
  end;
end;
{=====}

procedure TVpResource.SetDeleted(Value: Boolean);
begin
  if FDeleted <> Value then begin
    FDeleted := Value;
    Changed := true;
  end;
end;
{=====}

function TVpResource.GetSchedule: TVpSchedule;
begin
  if FSchedule = nil then
    FSchedule := TVpSchedule.Create(self);
  result := FSchedule;
end;
{=====}

procedure TVpResource.SetDescription(const Value: string);
begin
  if Value <> FDescription then begin
    if Assigned (Owner) then begin
      if Owner.FindResourceByName (Value) <> nil then
        raise EDuplicateResource.Create;
    end;

    FDescription := Value;
    FChanged := true;
  end;
end;
{=====}

procedure TVpResource.SetNotes(const Value: string);
begin
  FNotes := Value;
  FChanged := true;
end;

procedure TVpResource.SetResourceID(const Value: Integer);
begin
  FResourceID := Value;
end;
{=====}

procedure TVpResource.SetSchedule(const Value: TVpSchedule);
begin
  FSchedule := Value;
end;
{=====}

procedure TVpResource.SetTasks(const Value: TVpTasks);
begin
  FTasks := Value;
end;
{=====}

{ TVpEvent }
(*****************************************************************************)
constructor TVpEvent.Create(Owner: TVpSchedule);
begin
  inherited Create;
  FAlertDisplayed := false;
  FOwner := Owner;
  FChanged := false;
  FItemIndex := -1;
  FSnoozeTime := 0.0;
end;
{=====}

destructor TVpEvent.Destroy;
begin
  if (FOwner <> nil) and (FItemIndex <> -1) then begin
    FOwner.FEventList.Delete(FItemIndex);
    FOwner.Sort;
  end;
  inherited;
end;
{=====}

procedure TVpEvent.SetAlarmAdv(Value: Integer);
begin
  if Value <> FAlarmAdv then begin
    FAlarmAdv := Value;
    Changed := true;
  end;
end;
{=====}

procedure TVpEvent.SetAlarmAdvType(Value: TVpAlarmAdvType);
begin
  if Value <> FAlarmAdvType then begin
    FAlarmAdvType := Value;
    Changed := true;
  end;
end;
{=====}

procedure TVpEvent.SetSnoozeTime(Value: TDateTime);
begin
  if Value <> FSnoozeTime then begin
    FSnoozeTime := Value;
    Changed := true;
  end;
end;
{=====}

procedure TVpEvent.SetAlarmSet(Value: Boolean);
begin
  if Value <> FAlarmSet then begin
    FAlarmSet := Value;
    Changed := true;
  end;
end;
{=====}

procedure TVpEvent.SetCategory(Value: Integer);
begin
  if Value <> FCategory then begin
    FCategory := Value;
    Changed := true;
  end;
end;
{=====}

procedure TVpEvent.SetDescription(const Value: string);
begin
  if Value <> FDescription then begin
    FDescription := Value;
    Changed := true;
  end;
end;
{=====}

procedure TVpEvent.SetEndTime(Value: TDateTime);
begin
  if Value <> FEndTIme then begin
    FEndTime := Value;
    Changed := true;
  end;
end;
{=====}

procedure TVpEvent.SetAllDayEvent(Value: Boolean);
begin
  if Value <> FAllDayEvent then begin
    FAllDayEvent := Value;
    if FAllDayEvent then begin
      {Set the StartTime to 12:00 AM}
      StartTime := Trunc(StartTime);
      {Set the EndTime to 11:59 PM}
      EndTime := StartTime + 1 - (1 / MinutesInDay);
    end;
    Changed := true;
  end;
end;
{=====}

procedure TVpEvent.SetItemIndex(Value: Integer);
begin
  if Value <> FItemIndex then begin
    FItemIndex := Value;
    Changed := true;
  end;
end;
{=====}

procedure TVpEvent.SetChanged(Value: Boolean);
begin
  if Loading then Exit;

  if Value <> FChanged then begin
    FChanged := Value;
    if FChanged then
      Owner.FOwner.EventsDirty := true;
  end;
end;
{=====}

procedure TVpEvent.SetDeleted(Value: Boolean);
begin
  if Value <> FDeleted then begin
    FDeleted := Value;
    Changed := true;
  end;
end;
{=====}

procedure TVpEvent.SetDingPath(Value: string);
begin
  if Value <> FDingPath then begin
    FDingPath := Value;
    Changed := true;
  end;
end;
{=====}

procedure TVpEvent.SetNote(const Value: string);
begin
  if Value <> FNote then begin
    FNote := Value;
    Changed := true;
  end;
end;
{=====}

procedure TVpEvent.SetRecordID(Value: Integer);
begin
  if Value <> FRecordID then begin
    FRecordID := Value;
    Changed := true;
  end;
end;
{=====}

procedure TVpEvent.SetRepeatCode(Value: TVpRepeatType);
begin
  { rtNone, rtDaily, rtWeekly, rtMonthlyByDay, rtMonthlyByDate, }
  { rtYearlyByDay, rtYearlyByDate, rtCustom                     }
  if Value <> FRepeatCode then begin
    FRepeatCode := Value;
    if value <> rtCustom then
      SetCustInterval(0);
    Changed := true;
  end;
end;
{=====}

procedure TVpEvent.SetRepeatRangeEnd(Value: TDateTime);
begin
  if Value > StartTime then begin
    FRepeatRangeEnd := Value;
    Changed := true;
  end;
end;
{=====}

procedure TVpEvent.SetCustInterval(Value: Integer);
begin
  if Value <> FCustInterval then begin
    if RepeatCode = rtCustom then
      FCustInterval := Value
    else
      FCustInterval := 0;
    Changed := true;
  end;
end;
{=====}

procedure TVpEvent.SetStartTime(Value: TDateTime);
begin
  if Value <> FStartTIme then begin
    FStartTime := Value;
    Changed := true;
  end;
end;
{=====}

{ TVpSchedule }
(*****************************************************************************)
constructor TVpSchedule.Create(Owner: TVpResource);
begin
  inherited Create;
  FOwner := Owner;
  FBatchUpdate := 0;
  FEventList := TList.Create;
end;
{=====}

destructor TVpSchedule.Destroy;
begin
  ClearEvents;
  FEventList.Free;
  inherited;
end;
{=====}

procedure TVpSchedule.Sort;
var
  i, j       : integer;
  IndexOfMin : integer;
  Temp       : pointer;
  CompResult : integer; {Comparison Result}
begin
  { WARNING!!  The DayView component is heavily dependent upon the events }
  { being properly sorted.  If you change the way this procedure works,   }
  { you WILL break the DayView component!!!                               }

  { for greater performance, we don't sort while doing batch updates. }
  if FBatchUpdate > 0 then exit;

  for i := 0 to pred(FEventList.Count) do begin
    IndexOfMin := i;
    for j := i to FEventList.Count - 1 do begin

      { compare start times of item[j] and item[i] }
      CompResult := Compare(TVpEvent(FEventList.List^[j]).StartTime,
        TVpEvent(FEventList.List^[IndexOfMin]).StartTime);

      { if the starttime of j is less than the starttime of i then flip 'em}
      if CompResult < 0 then
        IndexOfMin := j

      { if the start times match then sort by end time }
      else if CompResult = 0 then begin

        { if the endtime of j is less than the end time of i then flip 'em}
        if (Compare(TVpEvent(FEventList.List^[j]).EndTime,
          TVpEvent(FEventList.List^[IndexOfMin]).EndTime) < 0)
        then
          IndexOfMin := j;
      end;
    end;

    Temp := FEventList.List^[i];
    FEventList.List^[i] := FEventList.List^[IndexOfMin];
    FEventList.List^[IndexOfMin] := Temp;
  end;

  { Fix object embedded ItemIndexes }
  for i := 0 to pred(FEventList.Count) do begin
    TVpEvent(FEventList.List^[i]).FItemIndex := i;
  end;
end;
{=====}

{ Used in the above sort procedure.  Compares the start times of the two }
{ passed in events.                                                      }
function TVpSchedule.Compare(Time1, Time2: TDateTime): Integer;
begin
  { Compares the value of the Item start dates }

  if Time1 < Time2 then
    result := -1

  else if Time1 = Time2 then
    result := 0

  else
    {Time2 is earlier than Time1}
    result := 1;
end;
{=====}

{Adds the event to the eventlist and returns a pointer to it, or nil on failure}
function TVpSchedule.AddEvent(RecordID: Integer; StartTime,
  EndTime: TDateTime): TVpEvent;
begin
  result := nil;
  if EndTime > StartTime then begin
    result := TVpEvent.Create(Self);
    try
      result.Loading := true;
      result.FItemIndex := FEventList.Add(result);
      result.RecordID := RecordID;
      result.StartTime := StartTime;
      result.EndTime := EndTime;
      result.Loading := false;
      Sort;
    except
      result.free;
      raise EFailToCreateEvent.Create;
    end;
  end;
end;
{=====}

procedure TVpSchedule.ClearEvents;
begin
  BatchUpdate(true);
  try
    while FEventList.Count > 0 do
      TVpEvent(FEventList.Last).Free;
  finally
    BatchUpdate(false);
  end;
end;
{=====}

procedure TVpSchedule.BatchUpdate(Value: Boolean);
begin
  if Value then
    FBatchUpdate := FBatchUpdate + 1
  else
    FBatchUpdate := FBatchUpdate - 1;

  if FBatchUpdate < 1 then begin
    FBatchUpdate := 0;
    Sort;
  end;
end;
{=====}

{ Frees the specified event, which also removes it from the list. }
procedure TVpSchedule.DeleteEvent(Event: TVpEvent);
begin
  Event.Deleted := true;
  Owner.EventsDirty := true;
end;
{=====}

function TVpSchedule.GetEvent(Index: Integer): TVpEvent;
begin
  { Returns an event on success or nil on failure }
  result := FEventList.Items[Index];
end;
{=====}

function TVpSchedule.RepeatsOn(Event: TVpEvent; Day: TDateTime): Boolean;
var
  EY, EM, ED: Word;
  NY, NM, ND: Word;
  EventWkDay, EventDayCount: Word;
  ThisWkDay, ThisDayCount: Word;
  EventJulian, ThisJulian: Word;
begin
  result := false;

  if (Event.RepeatCode <> rtNone)
  and (trunc(Event.RepeatRangeEnd + 1) > now)
  then begin
    case Event.RepeatCode of
      rtDaily:
        if (Day < trunc(Event.RepeatRangeEnd) + 1)
        and (Day > trunc(Event.StartTime)) then begin
          result := true;
        end;

      rtWeekly:
        if (Day < trunc(Event.RepeatRangeEnd) + 1)
        and (Day > trunc(Event.StartTime)) then begin
          result := (Trunc(Day) - Trunc(Event.StartTime)) mod 7 = 0;
        end;

      rtMonthlyByDay:
        if (Day < trunc(Event.RepeatRangeEnd) + 1)
        and (Day > trunc(Event.StartTime)) then begin
          { get the year, month and day of the first event in the series   }
          DecodeDate(Event.StartTime, EY, EM, ED);
          { get the weekday of the first event in the series               }
          EventWkDay := DayOfWeek(Event.StartTime);
          { Get the occurence of the first event in the series             }
          { (First Monday, Third Monday, etc...)                           }
          EventDayCount := ED div 7 + 1;
          { get the year, month and day of the "Day" parameter             }
          DecodeDate(Day, NY, NM, ND);
          { get the weekday of the "Day" parameter                         }
          ThisWkDay := DayOfWeek(Day);
          { Get the weekday occurence of the "Day" parameter               }
          { (First Monday, Third Monday, etc...)                           }
          ThisDayCount := ND div 7 + 1;
          { if  (ThisWeekDay is equal to EventWkDay)                       }
          { AND (ThisDayCount is equal to EventDayCount)                   }
          { then we have a recurrence on this day                          }
          result := (ThisWkDay = EventWkDay) and (ThisDayCount = EventDayCount);
        end;

      rtMonthlyByDate:
        if (Day < trunc(Event.RepeatRangeEnd) + 1)
        and (Day > trunc(Event.StartTime)) then begin
          { get the year, month and day of the first event in the series   }
          DecodeDate(Event.StartTime, EY, EM, ED);
          { get the year, month and day of the "Day" parameter             }
          DecodeDate(Day, NY, NM, ND);
          { if  the day values are equal then we have a recurrence on this }
          { day                                                            }
          result := ED = ND;
        end;

      rtYearlyByDay:
        if (Day < trunc(Event.RepeatRangeEnd) + 1)
        and (Day > trunc(Event.StartTime)) then begin
          { get the julian date of the first event in the series           }
          EventJulian := GetJulianDate(Event.StartTime);
          { get the julian date of the "Day" parameter                     }
          ThisJulian := GetJulianDate(Day);
          { if  the julian values are equal then we have a recurrence on   }
          { this day                                                       }
          result := EventJulian = ThisJulian;
        end;

      rtYearlyByDate:
        if (Day < trunc(Event.RepeatRangeEnd) + 1)
        and (Day > trunc(Event.StartTime)) then begin
          { get the year, month and day of the first event in the series   }
          DecodeDate(Event.StartTime, EY, EM, ED);
          { get the year, month and day of the "Day" parameter             }
          DecodeDate(Day, NY, NM, ND);
          { if  the day values and month values are equal then we have a   }
          { recurrence on this day                                         }
          result := (ED = ND) and (EM = NM);
        end;

      rtCustom:
        if (Day < trunc(Event.RepeatRangeEnd) + 1)
        and (Day > trunc(Event.StartTime)) then begin
          { if the number of elapsed days between the "Day" parameter and  }
          { the event start time is evenly divisible by the event's custom }
          { interval, then we have a recurrence on this day                }
           result := (Trunc(Day) - Trunc(Event.StartTime))
             mod Event.CustInterval = 0;
        end;
    end;
  end;
end;
{=====}

function TVpSchedule.EventCountByDay(Value: TDateTime): Integer;
var
  I: Integer;
  Evnt: TVpEvent;
begin
  result := 0;
  for I := 0 to pred(EventCount) do begin
    Evnt := GetEvent(I);
    { if this is a repeating event and it falls on today then inc     }
    { result                                                          }
    if (Evnt.RepeatCode > rtNone)
    and (RepeatsOn(Evnt, Value))
    then
      Inc(Result)
    { otherwise if it is an event that naturally falls on today, then }
    { inc result                                                      }
    else if (trunc(Evnt.StartTime) = trunc(Value)) then
      Inc(Result);
  end;
end;
{=====}

procedure TVpSchedule.EventsByDate(Date: TDateTime; EventList: TList);
var
  I: Integer;
  Event: TVpEvent;
begin
  if EventCountByDay(Date) = 0 then
    EventList.Clear

  else begin
    { Add this days events to the Event List. }
    for I := 0 to pred(EventCount) do begin
      Event := GetEvent(I);

      { if this is a repeating event and it falls on "Date" then add it to }
      { the list.                                                          }
      if (Event.RepeatCode > rtNone)
      and (RepeatsOn(Event, Date))
      then
        EventList.Add(Event)
      { otherwise if this event naturally falls on "Date" then add it to   }
      { the list.                                                          }
      else if (trunc(Event.StartTime) = trunc(Date)) then
        EventList.Add(Event);
    end;
  end;
end;
{=====}

procedure TVpSchedule.AllDayEventsByDate(Date: TDateTime; EventList: TList);
var
  I: Integer;
  Event: TVpEvent;
begin
  EventList.Clear;

  if EventCountByDay(Date) = 0 then
    Exit

  else begin
    { Add this days events to the Event List. }
    for I := 0 to pred(EventCount) do begin
      Event := GetEvent(I);
      if (trunc(Event.StartTime) = trunc(Date)) and (Event.AllDayEvent) then
        EventList.Add(Event);
    end;
  end;
end;
{=====}


{ binary search }
function TVpSchedule.FindTimeSlot(StartTime, EndTime: TDateTime): Boolean;
var
  L, R, M: Integer;
  CStart, CEnd, CompStart, CompEnd: integer; { comparison results }

  HitStart, HitEnd, HitStraddle: Boolean;
begin
  HitStart := false;
  HitEnd := false;
  HItStraddle := false;
  { Set left and right indexes }
  L := 0;
  R := Pred(FEventList.Count);
  while (L <= R) do begin
    { Calculate the middle index }
    M := (L + R) div 2;

    { Check to see if the middle item straddles our start time                }
    { Compare the the middle item's starttime against the passed in times     }
    CStart := Compare(TVpEvent(FEventList.List^[M]).StartTime, StartTime);
    CEnd := Compare(TVpEvent(FEventList.List^[M]).EndTime, StartTime);
    { if the middle item's starttime is less than or equal to the given       }
    { starttime AND the middle item's endtime is greater than or equal to the }
    { given starttime then we've hit at the start time                        }
    if ((CStart <= 0) and (CEnd >= 0)) then HitStart := true;

    { Check to see if the middle item straddles our end time                  }
    { Compare the the middle item's Endtime against the passed in times       }
    CStart := Compare(TVpEvent(FEventList.List^[M]).StartTime, EndTime);
    CEnd := Compare(TVpEvent(FEventList.List^[M]).EndTime, EndTime);
    { if the middle item's starttime is less than or equal to the given       }
    { endtime AND the middle item's endtime is greater than or equal to the   }
    { given endtime then we've hit at the end time                            }
    if ((CStart <= 0) and (CEnd >= 0)) then HitEnd := true;

    if (not HitStart) and (not HitEnd) then begin
      { Check to see if our times fall completely within the middle item      }
      CompStart := Compare(TVpEvent(FEventList.List^[M]).StartTime, StartTime);
      CompEnd := Compare(TVpEvent(FEventList.List^[M]).EndTime, EndTime);
      { if the middle item's starttime is less than our starttime AND its     }
      { endtime is greater than our endtime, then teh middle item straddles   }
      { our times                                                             }
      if ((CompStart <= 0) and (CompEnd >= 0)) then HitStraddle := true;

      if not HItStraddle then
      { Check to see if the middle item falls completely inside our times     }
      CompStart := Compare(TVpEvent(FEventList.List^[M]).StartTime, StartTime);
      CompEnd := Compare(TVpEvent(FEventList.List^[M]).EndTime, EndTime);
      { if the middle item's starttime is less than our starttime AND its     }
      { endtime is greater than our endtime, then teh middle item straddles   }
      { our times                                                             }
      if ((CompStart >= 0) and (CompEnd <= 0)) then HitStraddle := true;
    end;

    if (HitStart or HitEnd or HitStraddle) then begin
      result := true;
      exit;
    end
    else begin
      { No hit so keep going }
      CStart := Compare(TVpEvent(FEventList.List^[M]).StartTime, StartTime);
      { if the middle item's starttime is less than or equal to the given       }
      { starttime AND the middle item's endtime is greater than or equal to the }
      { given starttime then we've hit at the start time                        }
      if (CStart < 0) then
        L := Succ(M)
      else
        R := Pred(M);
    end;
  end;
  { if we got here then we didn't hit an existing item }
  result := false;
end;
{=====}

function TVpSchedule.GetCount: Integer;
begin
  result := FEventList.Count;
end;
{=====}



{ TVpContact }
(*****************************************************************************)
constructor TVpContact.Create(Owner: TVpContacts);
begin
  inherited Create;
  FChanged := false;
  FOwner := Owner;
  FItemIndex := -1;
  FPhoneType1 := Ord(ptWork);
  FPhoneType2 := Ord(ptHome);
  FPhoneType3 := Ord(ptWorkFax);
  FPhoneType4 := Ord(ptMobile);
  FPhoneType5 := Ord(ptAssistant);
end;
{=====}

destructor TVpContact.Destroy;
begin
  { Remove self from owners list }
  if (FItemIndex > -1) and (FOwner <> nil) then
    FOwner.FContactsList.Delete(FItemIndex);
  inherited;
end;
{=====}

function TVpContact.FullName : string;
begin
  if (FFirstName = '') and (FLastName = '') then
    Result := ''
  else
    Result := FFirstName + ' ' + FLastName;
end;
{=====}

procedure TVpContact.SetBirthDate(Value: TDateTIme);
begin
  if Value <> FBirthdate then begin
    FBirthdate := Value;
    Changed := true;
  end;
end;
{=====}

procedure TVpContact.SetAnniversary(Value: TDateTIme);
begin
  if Value <> FAnniversary then begin
    FAnniversary := Value;
    Changed := true;
  end;
end;
{=====}

procedure TVpContact.SetAddress(const Value: string);
begin
  if Value <> FAddress then begin
    FAddress := Value;
    Changed := true;
  end;
end;
{=====}

procedure TVpContact.SetCategory(Value: integer);
begin
  if Value <> FCategory then begin
    FCategory := Value;
    Changed := true;
  end;
end;
{=====}

procedure TVpContact.SetChanged(Value: Boolean);
begin
  if Loading then Exit;

  if Value <> FChanged then begin
    FChanged := Value;
    if FChanged then
      FOwner.FOwner.ContactsDirty := true;
  end;
end;
{=====}

procedure TVpContact.SetCity(const Value: string);
begin
  if Value <> FCity then begin
    FCity := Value;
    Changed := true;
  end;
end;
{=====}

procedure TVpContact.SetCompany(const Value: string);
begin
  if Value <> FCompany then begin
    FCompany := Value;
    Changed := true;
  end;
end;
{=====}

procedure TVpContact.SetCountry(const Value: string);
begin
  if Value <> FCountry then begin
    FCountry := Value;
    Changed := true;
  end;
end;
{=====}

procedure TVpContact.SetCustom1(const Value: string);
begin
  if Value <> FCustom1 then begin
    FCustom1 := Value;
    Changed := true;
  end;
end;
{=====}

procedure TVpContact.SetCustom2(const Value: string);
begin
  if Value <> FCustom2 then begin
    FCustom2 := Value;
    Changed := true;
  end;
end;
{=====}

procedure TVpContact.SetCustom3(const Value: string);
begin
  if Value <> FCustom3 then begin
    FCustom3 := Value;
    Changed := true;
  end;
end;
{=====}

procedure TVpContact.SetCustom4(const Value: string);
begin
  if Value <> FCustom4 then begin
    FCustom4 := Value;
    Changed := true;
  end;
end;
{=====}

procedure TVpContact.SetDeleted(Value: Boolean);
begin
  if Value <> FDeleted then begin
    FDeleted := Value;
    Changed := true;
  end;
end;
{=====}

procedure TVpContact.SetEMail(const Value: string);
begin
  if Value <> FEmail then begin
    FEMail := Value;
    Changed := true;
  end;
end;
{=====}

procedure TVpContact.SetFirstName(const Value: string);
begin
  if Value <> FFirstName then begin
    FFirstName := Value;
    Changed := true;
  end;
end;
{=====}

procedure TVpContact.SetLastName(const Value: string);
begin
  if Value <> FLastName then begin
    FLastName := Value;
    Changed := true;
  end;
end;
{=====}

procedure TVpContact.SetNote(const Value: string);
begin
  if Value <> FNote then begin
    FNote := Value;
    Changed := true;
  end;
end;
{=====}

procedure TVpContact.SetPhone1(const Value: string);
begin
  if Value <> FPhone1 then begin
    FPhone1 := Value;
    Changed := true;
  end;
end;
{=====}

procedure TVpContact.SetPhone2(const Value: string);
begin
  if Value <> FPhone2 then begin
    FPhone2 := Value;
    Changed := true;
  end;
end;
{=====}

procedure TVpContact.SetPhone3(const Value: string);
begin
  if Value <> FPhone3 then begin
    FPhone3 := Value;
    Changed := true;
  end;
end;
{=====}

procedure TVpContact.SetPhone4(const Value: string);
begin
  if Value <> FPhone4 then begin
    FPhone4 := Value;
    Changed := true;
  end;
end;
{=====}

procedure TVpContact.SetPhone5(const Value: string);
begin
  if Value <> FPhone5 then begin
    FPhone5 := Value;
    Changed := true;
  end;
end;
{=====}

procedure TVpContact.SetPhoneType1(Value: Integer);
begin
  if Value <> FPhoneType1 then begin
    FPhoneType1 := Value;
    Changed := true;
  end;
end;
{=====}

procedure TVpContact.SetPhoneType2(Value: Integer);
begin
  if Value <> FPhoneType2 then begin
    FPhoneType2 := Value;
    Changed := true;
  end;
end;
{=====}

procedure TVpContact.SetPhoneType3(Value: Integer);
begin
  if Value <> FPhoneType3 then begin
    FPhoneType3 := Value;
    Changed := true;
  end;
end;
{=====}

procedure TVpContact.SetPhoneType4(Value: Integer);
begin
  if Value <> FPhoneType4 then begin
    FPhoneType4 := Value;
    Changed := true;
  end;
end;
{=====}

procedure TVpContact.SetPhoneType5(Value: Integer);
begin
  if Value <> FPhoneType5 then begin
    FPhoneType5 := Value;
    Changed := true;
  end;
end;
{=====}

procedure TVpContact.SetPosition(const Value: string);
begin
  if Value <> FPosition then begin
    FPosition := Value;
    Changed := true;
  end;
end;
{=====}

procedure TVpContact.SetRecordID(Value: Integer);
begin
  if Value <> FRecordID then begin
    FRecordID := Value;
    Changed := true;
  end;
end;
{=====}

procedure TVpContact.SetState(const Value: string);
begin
  if Value <> FState then begin
    FState := Value;
    Changed := true;
  end;
end;
{=====}

procedure TVpContact.SetTitle(const Value: string);
begin
  if Value <> FTitle then begin
    FTitle := Value;
    Changed := true;
  end;
end;
{=====}

procedure TVpContact.SetZip(const Value: string);
begin
  if Value <> FZip then begin
    FZip := Value;
    Changed := true;
  end;
end;
{=====}

{ TVpContacts }
(*****************************************************************************)
constructor TVpContacts.Create(Owner: TVpResource);
begin
  inherited Create;
  FOwner := Owner;
  FContactsList := TList.Create;

  FContactSort := csLastFirst;
end;
{=====}

destructor TVpContacts.Destroy;
begin
  ClearContacts;
  FContactsList.Free;
  inherited;
end;
{=====}

procedure TVpContacts.BatchUpdate(Value: Boolean);
begin
  if Value then
    Inc(FBatchUpdate)
  else
    Dec(FBatchUpdate);

  if FBatchUpdate < 1 then begin
    FBatchUpdate := 0;
    Sort;
  end;
end;
{=====}

procedure TVpContacts.Sort;
var
  i, j       : integer;
  IndexOfMin : integer;
  Temp       : pointer;
begin
  { for greater performance, we don't sort while doing batch updates. }
  if FBatchUpdate > 0 then exit;

  for i := 0 to pred(FContactsList.Count) do begin
    IndexOfMin := i;
    for j := i to FContactsList.Count - 1 do
      if (Compare(FContactsList.List^[j], FContactsList.List^[IndexOfMin]) < 0)
      then IndexOfMin := j;
    Temp := FContactsList.List^[i];
    FContactsList.List^[i] := FContactsList.List^[IndexOfMin];
    FContactsList.List^[IndexOfMin] := Temp;
  end;

  { Fix object embedded ItemIndexes }
  for i := 0 to pred(FContactsList.Count) do begin
    TVpContact(FContactsList.List^[i]).FItemIndex := i;
  end;
end;
{=====}

{ Used by the above sort procedure }
function TVpContacts.Compare(Item1, Item2: TVpContact): Integer;
begin
  if ContactSort = csFirstLast then begin

    { Compares the value of the contact Names }
    if Item1.FirstName < Item2.FirstName then
      result := -1
    else if Item1.FirstName = Item2.FirstName then begin
      { if first names are equal then compare last names }
      if Item1.LastName < Item2.LastName then
        result := -1
      else if Item1.LastName = Item2.LastName then
        result := 0
      else
        result := 1;
    end
    else
      result := 1;

  end else begin
    { Compares the value of the contact Names }
    if Item1.LastName < Item2.LastName then
      result := -1
    else if Item1.LastName = Item2.LastName then begin
      { if last names are equal then compare first names }
      if Item1.FirstName < Item2.FirstName then
        result := -1
      else if Item1.FirstName = Item2.FirstName then
        result := 0
      else
        result := 1;
    end
    else
      result := 1;
  end;
end;
{=====}

function TVpContacts.AddContact(RecordID: Integer): TVpContact;
var
  Contact: TVpContact;
begin
  Contact := TVpContact.Create(Self);
  try
    Contact.Loading := true;
    Contact.FItemIndex := FContactsList.Add(Contact);
    Contact.RecordID := RecordID;
    Contact.Loading := false;
    result := Contact;
  except
    Contact.Free;
    raise EFailToCreateContact.Create;
  end;
end;
{=====}

function TVpContacts.Count: Integer;
begin
  result := FContactsList.Count;
end;
{=====}

function TVpContacts.Last: TVpContact;
begin
  result := FContactsList.Items[FContactsList.Count - 1];
end;
{=====}

function TVpContacts.First: TVpContact;
begin
  result := FContactsList.Items[0];
end;
{=====}

procedure TVpContacts.DeleteContact(Contact: TVpContact);
begin
  {Contacts automatically remove themselves from the list in their destructor }
  Contact.Free;
end;
{=====}

function TVpContacts.GetContact(Index: Integer): TVpContact;
begin
  result := FContactsList.Items[Index];
end;
{=====}

procedure TVpContacts.ClearContacts;
begin
  BatchUpdate(true);
  try
    while FContactsList.Count > 0 do
      TVpContact(FContactsList.Last).Free;
  finally
    BatchUpdate(false);
  end;
end;
{=====}

{ - new}
{ new function introduced to support the new buttonbar component }
function TVpContacts.FindContactByName(const Name: string;               
  CaseInsensitive: Boolean): TVpContact;                                 
var                                                                      
  I: Integer;                                                            
  SearchStr: String;                                                     
  SearchLength: Integer;                                                 
begin                                                                    
  Result := nil;                                                         
                                                                         
  { to enhance performance, uppercase the input name }                   
  { and get its length only once                     }                   
  if CaseInsensitive then                                                
    SearchStr := uppercase(Name)                                         
  else                                                                   
    SearchStr := Name;                                                   
  SearchLength := Length(SearchStr);                                     
                                                                         
  { Iterate the contacts looking for a match }                           
  for I := 0 to FContactsList.Count - 1 do begin                         
    if CaseInsensitive then begin                                        
      { not case sensitive }                                             
      if (Copy(uppercase(TVpContact(FContactsList.List^[I]).LastName), 1,
               SearchLength) = SearchStr)                                
      then begin                                                         
        { we found a match, so return it and bail out }                  
        Result := FContactsList.Items[I];                                
        Exit;                                                            
      end;                                                               
    end else begin                                                       
      { case sensitive }                                                 
      if (Copy(TVpContact(FContactsList.List^[I]).LastName, 1,           
                SearchLength) = SearchStr )                              
      then begin                                                         
        { we found a match, so return it and bail out }                  
        Result := FContactsList.Items[I];                                
        Exit;                                                            
      end;                                                               
    end;                                                                 
  end;                                                                   
end;                                                                     
{=====}                                                                  

{ - new}                                                            
{ new function introduced to support the new buttonbar component }       
function TVpContacts.FindContactIndexByName(const Name: string;          
  CaseInsensitive: Boolean): Integer;                                    
var                                                                      
  Contact: TVpContact;                                                   
begin                                                                    
  result := -1;                                                          
  Contact := FindContactByName(Name, CaseInsensitive);                   
  if Contact <> nil then                                                 
    Result := FContactsList.IndexOf(Contact);                            
end;                                                                     
{=====}                                                                  

procedure TVpContacts.SetContactSort (const v : TVpContactSort);
begin
  if v <> FContactSort then begin
    FContactSort := v;
    Sort;
  end;
end;
{=====}

(*****************************************************************************)
{ TVpTask }

constructor TVpTask.Create(Owner: TVpTasks);
begin
  inherited Create;
  FChanged := false;
  FOwner := Owner;
  SetCreatedOn(Now);
  FDescription := '';
end;
{=====}

destructor TVpTask.Destroy;
begin
  { Remove self from owners list }
  if (FItemIndex > -1) and (FOwner <> nil) then begin
    FOwner.FTaskList.Delete(FItemIndex);
    FOwner.Sort;
  end;
  inherited;
end;
{=====}

function TVpTask.IsOverdue: Boolean;
begin
  result := (Trunc(DueDate) < now + 1);
end;
{=====}

procedure TVpTask.SetCategory(const Value: Integer);
begin
  if Value <> FCategory then begin
    FCategory := Value;
    Changed := true;
  end;
end;
{=====}

procedure TVpTask.SetChanged(const Value: Boolean);
begin
  if Loading then Exit;

  if Value <> FChanged then begin
    FChanged := Value;
    if FChanged then
      Owner.FOwner.TasksDirty := true;                                   
  end;
end;
{=====}

procedure TVpTask.SetComplete(const Value: Boolean);
begin
  if Value <> FComplete then begin
    FComplete := Value;
    if FComplete then
      SetCompletedOn(Now)
    else
      SetCompletedOn(0.0);
  end;
end;
{=====}

procedure TVpTask.SetCompletedOn(const Value: TDateTIme);
begin
  if Value <> FCompletedOn then begin
    FCompletedOn := Value;
    Changed := true;
  end;
end;
{=====}

procedure TVpTask.SetCreatedOn(const Value: TDateTime);
begin
  if Value <> FCreatedOn then begin
    FCreatedOn := Value;
    Changed := true;
  end;
end;
{=====}

procedure TVpTask.SetDescription(const Value: string);
begin
  if Value <> FDescription then begin
    FDescription := Value;
    Changed := true;
  end;
end;
{=====}

procedure TVpTask.SetPriority(const Value: Integer);
begin
  if Value <> FPriority then begin
    FPriority := Value;
    Changed := true;
  end;
end;
{=====}

procedure TVpTask.SetDetails(const Value: string);
begin
  if Value <> FDetails then begin
    FDetails := Value;
    Changed := true;
  end;
end;
{=====}

procedure TVpTask.SetDueDate(const Value: TDateTime);
begin
  { Trunc the time element from the DueDate value so that it reflects }
  { the Date only.                                                    }
  if FDueDate <> Trunc(Value) then begin
    FDueDate := Trunc(Value);
    Changed := true;
  end;
end;
{=====}


(*****************************************************************************)
{ TVpTaskList }

constructor TVpTasks.Create(Owner: TVpResource);
begin
  inherited Create;
  FOwner := Owner;
  FTaskList := TList.Create;
  FTaskList.Clear;                                                    {!!!}
end;
{=====}

destructor TVpTasks.Destroy;
begin
  ClearTasks;
  FTaskList.Free;
  inherited;
end;
{=====}

function TVpTasks.AddTask(RecordID: Integer): TVpTask;
var
  Task: TVpTask;
begin
  Task := TVpTask.Create(Self);
  try
    result := Task;
    Task.Loading := true;
    Task.FItemIndex := FTaskList.Add(result);
    Task.RecordID := RecordID;
    FOwner.TasksDirty := true;
    Task.Loading := false;
    {the data which to sort by has not yet been added to the object}     
//    Sort;                                                              
  except
    Task.Free;
    raise EFailToCreateTask.Create;
  end;
end;
{=====}

function TVpTasks.Count : Integer;
begin
  result := FTaskList.Count;
end;
{=====}

function TVpTasks.Last: TVpTask;
begin
  result := FTaskList.Last;
end;
{=====}

function TVpTasks.First: TVpTask;
begin
  result := FTaskList.First;
end;
{=====}

function TVpTasks.CountByDay(Date: TDateTime): Integer;
var
  i     : Integer;           
  ATask : TVpTask;

begin
  Result := 0;

  for i := 0 to pred (Count) do begin
    ATask := GetTask (i);
    if Trunc (ATask.DueDate) = Trunc (Date) then
      Inc (Result);
  end;
end;
{=====}

function TVpTasks.LastByDay(Date: TDateTime): TVpTask;
var
  i     : Integer;
  ATask : TVpTask;
begin
  result := nil;

  for i := 0 to pred (Count) do begin
    ATask := GetTask (i);
    if Trunc (ATask.CreatedOn) = Trunc (Date) then begin
      Result := ATask;
    end;
  end;
end;
{=====}

function TVpTasks.FirstByDay(Date: TDateTime): TVpTask;
var
  i     : Integer;
  ATask : TVpTask;
begin
  result := nil;

  for i := 0 to pred (Count) do begin
    ATask := GetTask (i);
    if Trunc (ATask.CreatedOn) = Trunc (Date) then begin
      Result := ATask;
      Break;
    end;
  end;
end;
{=====}

procedure TVpTasks.ClearTasks;
begin
  BatchUpdate(true);
  try
    while FTaskList.Count > 0 do
      TVpTask(FTaskList.Last).Free;
  finally
    BatchUpdate(False);
  end;
end;
{=====}

procedure TVpTasks.BatchUpdate(value: Boolean);
begin
  if Value then
    Inc(FBatchUpdate)
  else
    Dec(FBatchUpdate);

  if FBatchUpdate < 1 then begin
    FBatchUpdate := 0;
    Sort;
  end;
end;
{=====}

procedure TVpTasks.Sort;
var
  i, j       : integer;
  IndexOfMin : integer;
  Temp       : pointer;
begin
  { for greater performance, we don't sort while doing batch updates. }
  if FBatchUpdate > 0 then exit;

  for i := 0 to pred(FTaskList.Count) do begin
    IndexOfMin := i;
    for j := i to FTaskList.Count - 1 do
      if (Compare(FTaskList.List^[j], FTaskList.List^[IndexOfMin]) < 0)
      then IndexOfMin := j;
    Temp := FTaskList.List^[i];
    FTaskList.List^[i] := FTaskList.List^[IndexOfMin];
    FTaskList.List^[IndexOfMin] := Temp;
  end;

  { Fix object embedded ItemIndexes }
  for i := 0 to pred(FTaskList.Count) do begin
    TVpTask(FTaskList.List^[i]).FItemIndex := i;
  end;
end;
{=====}

{ Used in the above sort procedure.  Compares the start times of the two }
{ passed in events.                                                      }
function TVpTasks.Compare(Item1, Item2: TVpTask): Integer;
begin
  { Compares the value of the Items DueDates }

  if Item1.DueDate < Item2.DueDate then
    result := -1

  { if the start times are equal then sort by description }
  else if Item1.DueDate = Item2.DueDate then begin
    if Item1.Description < Item2.Description then
      result := -1
    else if Item1.Description = Item2.Description then
      result := 0
    else
      result := 1
  end

  else
    {Item 2 starts earlier than Item 1}
    result := 1;
end;
{=====}

procedure TVpTasks.DeleteTask(Task: TVpTask);
begin
  {Tasks automatically remove themselves from the list in their destructor }
  Task.Free;
end;
{=====}

function TVpTasks.GetTask(Index: Integer): TVpTask;
begin
  result := FTaskList.Items[Index];
end;
{=====}

end.
