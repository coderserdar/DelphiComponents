// ------------------------------------------------------------------------------
// DPF.iOS.EventKit Wrapped Classes & Interfaces
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
unit DPF.iOS.EventKit;

interface

{$I DPF.iOS.Defs.inc}

uses
  System.Classes,
  System.SysUtils,
  System.TypInfo,
{$IFDEF IOS}
  iOSapi.UIKit,
  Macapi.ObjectiveC,
  iOSapi.CocoaTypes,
  iOSapi.CoreGraphics,
  iOSapi.Foundation,
{$ENDIF}
  FMX.Dialogs;


// ===== External functions =====

{$IFDEF IOS}

const
  libEventKit = '/System/Library/Frameworks/EventKit.framework/EventKit';

  EKEventAvailabilityNotSupported = -1;
  EKEventAvailabilityBusy         = 0;
  EKEventAvailabilityFree         = 1;
  EKEventAvailabilityTentative    = 2;
  EKEventAvailabilityUnavailable  = 3;

  EKSpanThisEvent    = 0;
  EKSpanFutureEvents = 1;

  EKSourceTypeLocal      = 0;
  EKSourceTypeExchange   = 1;
  EKSourceTypeCalDAV     = 2;
  EKSourceTypeMobileMe   = 3;
  EKSourceTypeSubscribed = 4;
  EKSourceTypeBirthdays  = 5;

{$IFDEF IOS6}
  EKEntityTypeEvent    = 0; // iOS 6 and Later
  EKEntityTypeReminder = 1; // iOS 6 and Later
{$ENDIF}
  EKCalendarEventAvailabilityNone        = 0;
  EKCalendarEventAvailabilityBusy        = ( 1 shl 0 );
  EKCalendarEventAvailabilityFree        = ( 1 shl 1 );
  EKCalendarEventAvailabilityTentative   = ( 1 shl 2 );
  EKCalendarEventAvailabilityUnavailable = ( 1 shl 3 );

  EKCalendarTypeLocal        = 0;
  EKCalendarTypeCalDAV       = 1;
  EKCalendarTypeExchange     = 2;
  EKCalendarTypeSubscription = 3;
  EKCalendarTypeBirthday     = 4;

{$IFDEF IOS6}
  EKAuthorizationStatusNotDetermined = 0; // iOS 6 and later
  EKAuthorizationStatusRestricted    = 1; // iOS 6 and later
  EKAuthorizationStatusDenied        = 2; // iOS 6 and later
  EKAuthorizationStatusAuthorized    = 3; // iOS 6 and later
{$ENDIF}

type
{$M+}
  // ===== Forward declarations =====

  EKEvent        = interface;
  EKAlarm        = interface;
  EKEventStore   = interface;
  EKCalendarItem = interface;

  EKRecurrenceFrequency = ( EKRecurrenceFrequencyDaily, EKRecurrenceFrequencyWeekly, EKRecurrenceFrequencyMonthly, EKRecurrenceFrequencyYearly );

  // ===== Interface declarations =====

  // ----------------------------------------------------------------------------
  // EKObject
  // ----------------------------------------------------------------------------
  EKObjectClass = interface( NSObjectClass )
    ['{54BCF304-0634-4921-8252-C886A2DAE466}']
  end;

  EKObject = interface( NSObject )
    ['{60632C61-A310-4E66-9283-D4D07594FD65}']
    function hasChanges: Boolean; cdecl;
    function isNew: Boolean; cdecl;
    procedure refresh; cdecl;
    procedure reset; cdecl;
    procedure rollback; cdecl;
  end;

  // ----------------------------------------------------------------------------
  // EKSource
  // ----------------------------------------------------------------------------
  EKSourceClass = interface( NSObjectClass )
    ['{A61D7ABB-2FD7-4DF3-A216-2286C845CAEE}']
  end;

  EKSource = interface( EKObject )
    ['{8FD0B31D-CFBD-47BE-AD45-213EC03ABEB2}']
    function sourceIdentifier: NSString; cdecl;
    function sourceType: Integer; cdecl;
    function title: NSString; cdecl;
  end;

  TEKSource = class( TOCGenericImport<EKSourceClass, EKSource> )
  end;

  // ----------------------------------------------------------------------------
  // EKReminder
  // ----------------------------------------------------------------------------
  EKReminderClass = interface( NSObjectClass )
    ['{2850A2DD-71CD-40DD-9FBD-B98D40B20FFE}']
  end;

  EKReminder = interface( NSObject )
    ['{B15DCF97-B3E3-4DDD-94F6-456400605423}']
  end;

  TEKReminder = class( TOCGenericImport<EKReminderClass, EKReminder> )
  end;

  // ----------------------------------------------------------------------------
  // EKCalendar
  // ----------------------------------------------------------------------------
  EKCalendarClass = interface( NSObjectClass )
    ['{480CDF7F-08DA-438B-AADD-B7DE20D1E4F0}']
    function calendarForEntityType( entityType: Integer; eventStore: EKEventStore ): Pointer; cdecl;
  end;

  EKCalendar = interface( NSObject )
    ['{C749B4EF-F5FE-491C-ABD8-05E42CE77802}']
    function defaultCalendarForNewEvents: EKCalendar; cdecl;

    function allowsContentModifications: Boolean; cdecl;
    function immutable: Boolean; cdecl;
    function subscribed: Boolean; cdecl;
    function calendarIdentifier: NSString; cdecl;
    function title: NSString; cdecl;
    procedure setTitle( tile: NSString ); cdecl;
    function CGColor: CGColorRef; cdecl;
    function source: EKSource; cdecl;
    procedure setSource( source: EKSource ); cdecl;
    function &type: Integer; cdecl;
    function supportedEventAvailabilities: Integer; cdecl;
    // Function calendarForEntityType( entityType: Integer; eventStore: EKEventStore ): EKCalendar; Cdecl;
  end;

  TEKCalendar = class( TOCGenericImport<EKCalendarClass, EKCalendar> )
  end;


  // ----------------------------------------------------------------------------
  // EKRecurrenceEnd
  // ----------------------------------------------------------------------------

  EKRecurrenceEndClass = interface( NSObjectClass )
    ['{D6B3D6A2-3C09-45E3-ACF4-E62B982EBFEA}']
    function recurrenceEndWithEndDate( endDate: NSDate ): Pointer; cdecl;
    function recurrenceEndWithOccurrenceCount( occurrenceCount: NSUInteger ): Pointer; cdecl;
  end;

  EKRecurrenceEnd = interface( NSObject )
    ['{C893C062-12BB-4584-887F-B9D4429F7A3C}']
    function endDate: NSDate; cdecl;
    function occurrenceCount: NSUInteger; cdecl;
  end;
  // ----------------------------------------------------------------------------
  // EKRecurrenceRule
  // ----------------------------------------------------------------------------

  EKRecurrenceRuleClass = interface( NSObjectClass )
    ['{ED86C814-68FD-4DF0-A3C1-63C5A1F5A3F4}']
  end;

  EKRecurrenceRule = interface( NSObject )
    ['{BC0C4B38-56E7-4764-803E-28B608F8791F}']
    function initRecurrenceWithFrequency( recurrenceFrequencyType: EKRecurrenceFrequency; interval: NSInteger; recurrenceEnd: EKRecurrenceEnd ): Pointer; cdecl; overload;
    function initRecurrenceWithFrequency( recurrenceFrequencyType: EKRecurrenceFrequency; interval: NSInteger; daysOfTheWeek: NSArray; daysOfTheMonth: NSArray; monthsOfTheYear: NSArray; weeksOfTheYear: NSArray; daysOfTheYear: NSArray; setPositions: NSArray; recurrenceEnd: EKRecurrenceEnd ): Pointer; cdecl; overload;
    function calendarIdentifier: NSString; cdecl;
    function recurrenceEnd: EKRecurrenceEnd; cdecl;
    function frequency: EKRecurrenceFrequency; cdecl;
    function interval: NSInteger; cdecl;
    function firstDayOfTheWeek: NSInteger; cdecl;
    function daysOfTheWeek: NSArray; cdecl;
    function daysOfTheMonth: NSArray; cdecl;
    function daysOfTheYear: NSArray; cdecl;
    function weeksOfTheYear: NSArray; cdecl;
    function monthsOfTheYear: NSArray; cdecl;
    function setPositions: NSArray; cdecl;
  end;
  // ----------------------------------------------------------------------------
  // EKCalendarItem
  // ----------------------------------------------------------------------------

  EKCalendarItemClass = interface( NSObjectClass )
    ['{B764C5A2-07F7-4392-8FE9-89569B23AEDD}']
  end;

  EKCalendarItem = interface( NSObject )
    ['{AEE42492-1BD3-41DE-9EC8-08E7B8BD3C1C}']
    function calendar: EKCalendar; cdecl;
    function title: NSString; cdecl;
    function calendarItemIdentifier: NSString; cdecl;
    function calendarItemExternalIdentifier: NSString; cdecl;
    function location: NSString; cdecl;
    function hasNotes: Boolean; cdecl;
    function notes: NSString; cdecl;
    procedure addAlarm( alarm: EKAlarm ); cdecl;
    procedure removeAlarm( alarm: EKAlarm ); cdecl;
    procedure setCalendar( calendar: EKCalendar ); cdecl;
    procedure setTitle( title: NSString ); cdecl;
    procedure setLocation( location: NSString ); cdecl;
    procedure setNotes( notes: NSString ); cdecl;
    function hasRecurrenceRules: Boolean; cdecl;
    function recurrenceRules: NSArray; cdecl;
    procedure addRecurrenceRule( recurrenceRule: EKRecurrenceRule ); cdecl;
  end;

  TEKCalendarItem = class( TOCGenericImport<EKCalendarItemClass, EKCalendarItem> )
  end;

  // ----------------------------------------------------------------------------
  // EKEvent
  // ----------------------------------------------------------------------------
  EKEventClass = interface( NSObjectClass )
    ['{0D24A553-4C9C-4301-9316-8AAB19285C68}']
    function eventWithEventStore( eventStore: EKEventStore ): Pointer; cdecl;
  end;

  EKEvent = interface( EKCalendarItem )
    ['{2B00F832-E1B4-4D6E-9F01-B8CCC7065DD0}']
    function allDay: Boolean; cdecl;
    procedure setAllDay( allDay: Boolean ); cdecl;
    function eventIdentifier: NSString; cdecl;
    function startDate: NSDate; cdecl;
    procedure setStartDate( date: NSDate ); cdecl;
    function endDate: NSDate; cdecl;
    procedure setEndDate( date: NSDate ); cdecl;
    function isDetached: Boolean; cdecl;
    // Function eventWithEventStore( eventStore: EKEventStore ): EKEvent; Cdecl;
  end;

  TEKEvent = class( TOCGenericImport<EKEventClass, EKEvent> )
  end;


  // ----------------------------------------------------------------------------
  // EKEventStore
  // ----------------------------------------------------------------------------

  EKEventStoreClass = interface( NSObjectClass )
    ['{E6889EBA-0DBD-4A7E-8DBB-3057E7FAAAD3}']
{$IFDEF IOS6}
    function authorizationStatusForEntityType( entityType: Integer ): Integer; cdecl; // iOS 6 and later
{$ENDIF}
  end;

  TEKEventStoreRequestAccessCompletionHandler = procedure( grant: Pointer; error: Pointer ) of object;

  EKEventStore = interface( NSObject )
    ['{66A0F263-722E-411B-B764-F19996EF21DB}']
    procedure requestAccessToEntityType( entityType: Integer; completion: TEKEventStoreRequestAccessCompletionHandler ); cdecl;
    function defaultCalendarForNewEvents: EKCalendar; cdecl;
    function saveEvent( event: EKEvent; span: Integer; commit: Boolean; error: NSError ): Boolean; cdecl; overload;
    function saveEvent( event: EKEvent; span: Integer; error: NSError ): Boolean; cdecl; overload;
    function calendarWithIdentifier( identifier: NSString ): EKCalendar; cdecl;
    procedure refreshSourcesIfNecessary; cdecl;
    function sourceWithIdentifier( identifier: NSString ): EKSource; cdecl;
    function eventWithIdentifier( identifier: NSString ): EKEvent; cdecl;
    function removeEvent( event: EKEvent; span: Integer; error: NSError ): Boolean; cdecl;
{$IFDEF IOS6}
    // Function saveReminnder( reminder: EKReminder; commit: Boolean; error: NSError ): Boolean; Cdecl; // iOS 6 and later
{$ENDIF}
    function saveCalendar( calendar: EKCalendar; commit: Boolean; error: NSError ): Boolean; cdecl;
    function commit( error: NSError ): Boolean; cdecl;
    procedure reset; cdecl;
    function sources: NSArray; cdecl;
    function calendars: NSArray; cdecl;
    function predicateForEventsWithStartDate( startDate: NSDate; endDate: NSDate; calendars: NSArray ): NSPredicate; cdecl;
    function eventsMatchingPredicate( predicate: NSPredicate ): NSArray; cdecl;
  end;

  TEKEventStore = class( TOCGenericImport<EKEventStoreClass, EKEventStore> )
  end;

  // ----------------------------------------------------------------------------
  // EKAlarm
  // ----------------------------------------------------------------------------
  EKAlarmClass = interface( NSObjectClass )
    ['{58021B68-ACB6-4BB1-9EC5-CC4C17C0CEED}']
    function alarmWithRelativeOffset( offset: NSTimeInterval ): Pointer; cdecl;
  end;

  EKAlarm = interface( NSObject )
    ['{108DA9A2-9507-4D8C-BD6F-6EDD720FC448}']
    function absoluteDate: NSDate; cdecl;
    procedure setAbsoluteDate( absoluteDate: NSDate ); cdecl;
    function relativeOffset: NSTimeInterval; cdecl;
    procedure setRelativeOffset( relativeOffset: NSTimeInterval ); cdecl;
    // Function alarmWithRelativeOffset( offset: NSTimeInterval ): EKAlarm; Cdecl;
  end;

  TEKAlarm = class( TOCGenericImport<EKAlarmClass, EKAlarm> )
  end;

  // ----------------------------------------------------------------------------
{$ENDIF}

implementation

{$IFDEF IOS}
{$IF Not defined(CPUARM)}

uses Posix.Dlfcn;

var
  iEventKitModule: THandle;
{$ENDIF}

  // ----------------------------------------------------------------------------
function AddCalendarEvent: Boolean;
var
  eventStore: EKEventStore;
  event     : EKEvent;
  alarm     : EKAlarm;
  sdate     : NSDate;
  edate     : NSDate;
  error     : NSError;
  // calendar        : EKCalendar;
  CSource    : EKCalendar;
  DefCalendar: EKCalendar;
  I          : Integer;
begin
  result     := False;
  eventStore := TEKEventStore.Wrap( TEKEventStore.Alloc.init );
  try
    eventStore.reset;
    (*
      If Not assigned( eventStore.sources ) Then
      Begin
      Try
      { eventStore.requestAccessToEntityType( 0, Nil ILocalObject(
      Procedure( granted: Boolean; error: NSError )
      Begin
      End ).GetObjectID  ); }
      eventStore.refreshSourcesIfNecessary;
      Except

      End;
      End;

      If Not assigned( eventStore.sources ) Then
      Begin
      ShowMessage( 'Go to Setting->Privacy->Calendars and switch on this application' );
      Exit;
      End;
    *)

    if not assigned( eventStore.calendars ) or ( eventStore.calendars.count = 0 ) then
    begin
      ShowMessage( 'Go to Setting->Privacy->Calendars and switch on this application' );
      Exit;
    end;

    DefCalendar := nil;
    for I       := 0 to eventStore.calendars.count - 1 do
    begin
      CSource := TEKCalendar.Wrap( eventStore.calendars.objectAtIndex( I ) );

      // ShowMessage( CSource.title.UTF8String );

      if not assigned( DefCalendar ) and CSource.allowsContentModifications then
      begin
        DefCalendar := CSource;
      end;
    end;

    if not assigned( DefCalendar ) then
    begin
      ShowMessage( 'Create a Calander in your device' );
      Exit;
    end;

    sdate := TNSDate.Wrap( TNSDate.OCClass.dateWithTimeIntervalSinceNow( 30 ) );
    edate := TNSDate.Wrap( TNSDate.OCClass.dateWithTimeInterval( 1800, sdate ) );

    event := TEKEvent.Wrap( TEKEvent.OCClass.eventWithEventStore( eventStore ) );

    event.setTitle( NSStr( 'This is a test' ) );
    event.setStartDate( sdate );
    event.setEndDate( edate );

    // alarm := TEKAlarm.Wrap( TEKAlarm.OCClass.alarmWithRelativeOffset( 60.0 * -5.0 ) ); // 5 Minute
    alarm := TEKAlarm.Wrap( TEKAlarm.OCClass.alarmWithRelativeOffset( -10.0 ) ); // 10 sec before event time
    try
      event.addAlarm( alarm );
    finally
      // alarm.release;
    end;

    event.setCalendar( DefCalendar );
    result := eventStore.saveEvent( event, EKSpanThisEvent, error );

  finally
    eventStore.release;
  end;
end;

// ----------------------------------------------------------------------------
{$IFDEF IOS}
{$IF defined(CPUARM)}
procedure EventKitLoader; cdecl; external libEventKit;
{$ELSE}

initialization

iEventKitModule := dlopen( MarshaledAString( libEventKit ), RTLD_LAZY );

finalization

dlclose( iEventKitModule );
{$ENDIF}
{$ENDIF}
{$ENDIF}

end.
