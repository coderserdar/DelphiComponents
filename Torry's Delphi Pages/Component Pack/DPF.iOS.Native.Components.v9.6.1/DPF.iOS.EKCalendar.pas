// ------------------------------------------------------------------------------
// DPF.iOS.EKCalendar Component
//
// Dadeh Pardazane Faragir ( DPF ) Co.
//
// Web: http://www.dpfaragir.com
//
// Developed By: Babak Yaghoobi
//
// Email #1: yaghoobi@dpfaragir.com
// Email #2: b_yaghobi@yahoo.com
// Email #3: bayaghoobi@gmail.com
//
//
// Thanks: Massimo Caroccia ( carmas123 ) for hellping
//
// ------------------------------------------------------------------------------
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//
// ------------------------------------------------------------------------------
unit DPF.iOS.EKCalendar;

interface

{$I DPF.iOS.Defs.inc}

uses
  System.Classes,
  System.SysUtils,
  System.TypInfo,
  DPF.iOS.BaseControl,
{$IFDEF IOS}
  Macapi.ObjCRuntime,
  DPF.iOS.EventKit,
  DPF.iOS.Common,

  iOSapi.UIKit,
  Macapi.ObjectiveC,
  iOSapi.CocoaTypes,
  iOSapi.CoreGraphics,
  iOSapi.Foundation,
{$ENDIF}
  FMX.Forms,
  FMX.Dialogs;

type
{$M+}
{$IFDEF IOS}
  // ----------------------------------------------------------------------------

  IEventStoreChangedNotificationHandler = interface( NSObject )
    ['{46643861-E344-45F7-97AE-D89043003B10}']
    procedure storeChanged; cdecl;
  end;

  TEventStoreChangedNotificationHandler = class( TOCLocal )
  public
    procedure storeChanged; cdecl;
    function GetObjectiveCClass: PTypeInfo; override;
  end;
{$ENDIF}

  TOnRequestAccess = procedure( Sender: TObject; const Granted: Boolean ) of object;

  TCalendarEvent = record
    StartDate: TDateTime;
    EndDate: TDateTime;
    Title: string;
    Notes: string;
    Location: string;
    Identifier: string;
  end;

  TArrayOfCalendarEvent = array of TCalendarEvent;

  [ComponentPlatformsAttribute( PidWin32 or pidiOSDevice32 or pidiOSDevice64 or PidiOSSimulator )]
  TDPFEKCalendar = class( TComponent )
  private
    FOnRequestAccess: TOnRequestAccess;
    FGranted        : Boolean;
{$IFDEF IOS}
    FEventStore                          : EKEventStore;
    FDefaultCalendar                     : EKCalendar;
    FEventStoreChangedNotificationHandler: TEventStoreChangedNotificationHandler;
{$ENDIF}
  protected
  public
{$IFDEF IOS}
    function AddOrEditCalendarEvent( const eventIdentifier: string; const EventTitle: string; const AlarmOffset: Integer; const FromDateTime: TDateTime; const ToDateTime: TDateTime; const TimeZone: Integer = MAXINT ): string;
    function RemoveCalendarEvent( eventIdentifier: string ): Boolean;
    function GetAllEvents( const FromDateTime: TDateTime; const ToDateTime: TDateTime; const TimeZone: Integer = MAXINT ): TArrayOfCalendarEvent;
    procedure RequestAccess;
{$IFDEF IOS6}
    procedure RequestAccessCompletionHandler( grant: Pointer; error: Pointer );
{$ENDIF}
{$ENDIF}
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;
  published
    property OnRequestAccess: TOnRequestAccess read FOnRequestAccess write FOnRequestAccess;
    property Granted        : Boolean read FGranted default false;
  end;
  // ----------------------------------------------------------------------------

implementation

// ------------------------------------------------------------------------------
constructor TDPFEKCalendar.Create( AOwner: TComponent );
begin
  inherited Create( AOwner );
  FGranted := false;
{$IFDEF IOS}
  FEventStore                           := TEKEventStore.create;
  FEventStoreChangedNotificationHandler := TEventStoreChangedNotificationHandler.Create;
  TNSNotificationCenter.Wrap( TNSNotificationCenter.OCClass.defaultCenter ).addObserver( FEventStoreChangedNotificationHandler.GetObjectID, sel_getUid( 'storeChanged' ), ( NSSTR( 'EKEventStoreChangedNotification' ) as ILocalObject ).GetObjectID, ( FEventStore as ILocalObject ).GetObjectID );
{$ENDIF}
end;

// ----------------------------------------------------------------------------
destructor TDPFEKCalendar.Destroy;
begin
{$IFDEF IOS}
  FEventStore.release;
  FEventStore := nil;
  FEventStoreChangedNotificationHandler.DisposeOf;
{$ENDIF}
  inherited;
end;

// ----------------------------------------------------------------------------
{$IFDEF IOS}
// ----------------------------------------------------------------------------
{$IFDEF IOS6}

procedure TDPFEKCalendar.RequestAccessCompletionHandler( grant: Pointer; error: Pointer );
var
  authorizationStatus: Integer;
begin
  if Assigned( FOnRequestAccess ) then
  begin
    FGranted := Integer( grant ) = 1;
    if TOSVersion.Major >= 6.0 then
    begin
      authorizationStatus := TEKEventStore.OCClass.authorizationStatusForEntityType( EKEntityTypeEvent );
      FGranted            := authorizationStatus = EKAuthorizationStatusAuthorized;
    end;
    OnRequestAccess( Self, Granted );
  end;
end;
{$ENDIF}

// ----------------------------------------------------------------------------
procedure TDPFEKCalendar.RequestAccess;
var
  authorizationStatus: Integer;
begin
{$IFDEF IOS6}
  if TOSVersion.Major >= 6.0 then
  begin
    authorizationStatus := TEKEventStore.OCClass.authorizationStatusForEntityType( EKEntityTypeEvent );
    if authorizationStatus = EKAuthorizationStatusNotDetermined then
    begin
      FEventStore.requestAccessToEntityType( EKEntityTypeEvent, RequestAccessCompletionHandler );
    end
    else
      RequestAccessCompletionHandler( nil, nil );
  end
  else
    RequestAccessCompletionHandler( nil, nil );
{$ELSE}
  if Assigned( FOnRequestAccess ) then
    OnRequestAccess( Self, True );
{$ENDIF}
end;

// ----------------------------------------------------------------------------
{ TDPFEKCalendar }
function TDPFEKCalendar.AddOrEditCalendarEvent( const eventIdentifier: string; const EventTitle: string; const AlarmOffset: Integer; const FromDateTime: TDateTime; const ToDateTime: TDateTime; const TimeZone: Integer = MAXINT ): string;
var
  I     : Integer;
  FEvent: EKEvent;
  FAlarm: EKAlarm;
  sdate : NSDate;
  edate : NSDate;
  error : NSError;
begin
  Result := '';

  if not FGranted then
    Exit;

  // FEventStore.reset;

  FEvent := nil;
  if eventIdentifier.Length > 0 then
    FEvent := FEventStore.eventWithIdentifier( NSStr( eventIdentifier ) );
  if not Assigned( FEvent ) then
  begin

    FDefaultCalendar := FEventStore.defaultCalendarForNewEvents;

    if not Assigned( FDefaultCalendar ) then
      for I := 0 to FEventStore.calendars.count - 1 do
      begin
        FDefaultCalendar := TEKCalendar.Wrap( FEventStore.calendars.objectAtIndex( I ) );
        // ShowMessage( CSource.title.UTF8String );
        if not Assigned( FDefaultCalendar ) { And ( FDefaultCalendar.allowsContentModifications ) } then
        begin
          // Result := True;
          Break;
        end;
      end;

    if not Assigned( FDefaultCalendar ) then
    begin
      FDefaultCalendar := nil;
      Exit;
    end;
    FEvent := TEKEvent.Wrap( TEKEvent.OCClass.eventWithEventStore( FEventStore ) );
  end;

  if TimeZone = MAXINT then
  begin
    sdate := DateTimeToNSDate( GetGMTDateTime( FromDateTime ) ); // TNSDate.Wrap( TNSDate.OCClass.dateWithTimeIntervalSinceNow( 30 ) );
    edate := DateTimeToNSDate( GetGMTDateTime( ToDateTime ) ); // TNSDate.Wrap( TNSDate.OCClass.dateWithTimeInterval( 1800, sdate ) );
  end
  else
  begin
    sdate := DateTimeToNSDate( GetGMTDateTime( FromDateTime, TimeZone ) ); // TNSDate.Wrap( TNSDate.OCClass.dateWithTimeIntervalSinceNow( 30 ) );
    edate := DateTimeToNSDate( GetGMTDateTime( ToDateTime, TimeZone ) ); // TNSDate.Wrap( TNSDate.OCClass.dateWithTimeInterval( 1800, sdate ) );
  end;

  FEvent.setTitle( NSSTR( EventTitle ) );
  FEvent.setStartDate( sdate );
  FEvent.setEndDate( edate );

  FAlarm := TEKAlarm.Wrap( TEKAlarm.OCClass.alarmWithRelativeOffset( AlarmOffset ) );
  FEvent.addAlarm( FAlarm );

  FEvent.setCalendar( FDefaultCalendar );
  FEventStore.saveEvent( FEvent, EKSpanThisEvent, True, error );
  if not Assigned( error ) then
    Result := NSStrToStr( FEvent.eventIdentifier );
end;

// ----------------------------------------------------------------------------
function TDPFEKCalendar.RemoveCalendarEvent( eventIdentifier: string ): Boolean;
var
  event: EKEvent;
begin
  result := false;
  event  := FEventStore.eventWithIdentifier( NSStr( eventIdentifier ) );
  if Assigned( event ) then
    Result := FEventStore.removeEvent( event, EKSpanThisEvent, nil );

end;

// ----------------------------------------------------------------------------
// !!! Dont Use Long distance between startDate and endDate !!!
// because may be return empty !
// ----------------------------------------------------------------------------
function TDPFEKCalendar.GetAllEvents( const FromDateTime: TDateTime; const ToDateTime: TDateTime; const TimeZone: Integer = MAXINT ): TArrayOfCalendarEvent;
var
  NSP      : NSPredicate;
  AllEvents: NSArray;
  I        : Integer;
  Event    : EKEvent;
begin
  if not FGranted then
    Exit;

  if TimeZone = MAXINT then
    NSP := FEventStore.predicateForEventsWithStartDate( DateTimeToNSDate( GetGMTDateTime( FromDateTime ) ), DateTimeToNSDate( GetGMTDateTime( ToDateTime ) ), nil )
  else
    NSP := FEventStore.predicateForEventsWithStartDate( DateTimeToNSDate( GetGMTDateTime( FromDateTime, TimeZone ) ), DateTimeToNSDate( GetGMTDateTime( ToDateTime, TimeZone ) ), nil );

  // ShowMessage( NSP.predicateFormat.UTF8String );

  AllEvents := FEventStore.eventsMatchingPredicate( NSP );
  if Assigned( AllEvents ) then
  begin
    for I := 0 to AllEvents.count - 1 do
    begin
      SetLength( Result, Length( Result ) + 1 );
      Event               := TEKEvent.Wrap( AllEvents.objectAtIndex( I ) );
      Result[I].StartDate := NSDateToDateTime( Event.StartDate );
      Result[I].EndDate   := NSDateToDateTime( Event.EndDate );
      Result[I].Title     := NSStrToStr( Event.Title );

      if Event.hasNotes then
        Result[I].Notes := NSStrToStr( Event.Notes );
      if Event.Location <> nil then
        Result[I].Location := NSStrToStr( Event.Location );

      Result[I].Identifier := NSStrToStr( Event.eventIdentifier );
    end;
  end
  else
    ShowMessage( 'Empty !' );

end;

// ----------------------------------------------------------------------------
{ TEventStoreChangedNotificationHandler }
function TEventStoreChangedNotificationHandler.GetObjectiveCClass: PTypeInfo;
begin
  Result := TypeInfo( IEventStoreChangedNotificationHandler );
end;

// ----------------------------------------------------------------------------
procedure TEventStoreChangedNotificationHandler.storeChanged;
begin

end;

{$ENDIF}

// ----------------------------------------------------------------------------
end.
