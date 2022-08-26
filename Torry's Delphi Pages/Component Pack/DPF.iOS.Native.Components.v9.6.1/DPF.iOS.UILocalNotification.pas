// ------------------------------------------------------------------------------
// DPF.iOS.UILocalNotification Tools
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

unit DPF.iOS.UILocalNotification;

interface

{$I DPF.iOS.Defs.inc}
{$IFDEF IOS}

uses
  Macapi.CoreFoundation,
  Macapi.ObjCRuntime,
  Macapi.Helpers,
  iOSapi.Foundation,
  DPF.iOS.Common,
  iOSapi.UIKit;

// ------------------------------------------------------------------------------

procedure MakeNotification( MessageBody: string; AlertDateTime: TDateTime; RepeatInterval: NativeUInt = 0 );
procedure CancelAllNotifications;
{$ENDIF}

// ------------------------------------------------------------------------------
implementation

{$IFDEF IOS}

// ------------------------------------------------------------------------------
procedure MakeNotification( MessageBody: string; AlertDateTime: TDateTime; RepeatInterval: NativeUInt = 0 );
var
  app        : UIApplication;
  notifyAlarm: UILocalNotification;
begin

  app := GetSharedApplication;

  notifyAlarm := TUILocalNotification.Wrap( TUILocalNotification.Alloc.init );
  if Assigned( notifyAlarm ) then
  begin
    notifyAlarm.setFireDate( DateTimeToNSDate( GetGMTDateTime( AlertDateTime ) ) );
    notifyAlarm.setTimeZone( TNSTimeZone.Wrap( TNSTimeZone.OCClass.localTimeZone ) );
    notifyAlarm.setSoundName( NSStr( 'Glass.aiff' ) );
    if RepeatInterval > 0 then
      notifyAlarm.setRepeatInterval( RepeatInterval );
    notifyAlarm.setAlertBody( NSStr( MessageBody ) );
    app.scheduleLocalNotification( notifyAlarm );
    notifyAlarm.release;
  end;

end;

// ------------------------------------------------------------------------------
procedure CancelAllNotifications;
var
  oldNotifications: NSArray;
begin
  oldNotifications := GetSharedApplication.scheduledLocalNotifications;
  if assigned( oldNotifications ) and ( oldNotifications.count > 0 ) then
    GetSharedApplication.cancelAllLocalNotifications;
end;

{$ENDIF}

// ------------------------------------------------------------------------------
end.
