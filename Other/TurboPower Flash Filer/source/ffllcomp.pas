{*********************************************************}
{* FlashFiler: Base component classes                    *}
{*********************************************************}

(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is TurboPower FlashFiler
 *
 * The Initial Developer of the Original Code is
 * TurboPower Software
 *
 * Portions created by the Initial Developer are Copyright (C) 1996-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

{$I ffdefine.inc}

unit ffllcomp;

interface

uses
  Classes,
  SysUtils,
  ffllbase,
  fflllog,
  ffsrmgr;

type
  { This type defines the possible states of a TffStateComponent.  Values:
      ffesInactive - The engine and its associated components (i.e., command
        handlers and transports) are inactive.
      ffesInitializing - The engine and its associated components are
        initializing.
      ffesStarting - The engine and its associates are starting.
      ffesStarted - The engine and its associates are operational and
        processing requests.
      ffesShuttingDown - The engine and its associates are in the process of
        shutting down.
      ffesStopping - The engine is in the process of stopping but its
        associated components are still active.
      ffesStopped - The engine is inactive but its associates are still
        active.
      ffesUnsupported - Transport-specific.  The transport is not supported
        on this workstation.  For example, an IPX/SPX transport is unsupported
        if an IPX/SPX driver is not installed on the workstation.
      ffesFailed - A failure occurred and the engine or transport may no
        longer be used.  A transport's state is set to ffesFailed if an error
        occurs during startup.
  }
  TffState = (ffesInactive,
              ffesInitializing,
              ffesStarting,
              ffesStarted,
              ffesShuttingDown,
              ffesStopping,
              ffesStopped,
              ffesUnsupported,
              ffesFailed);

  { This class implements the basic functionality for associating a component
    with a descendant of TffBaseLog. }
  TffLoggableComponent = class(TffComponent)
  protected

    FEventLog     : TffBaseLog;
      { The log to which events may be written. }

    FLogEnabled   : boolean;
      { If True then events may be written to the event log. }

    function lcGetLogEnabled : boolean; virtual;

    procedure lcLog(const aString : string); virtual;
      { Use this to write a string to the event log. }

{Begin !!.06}
    procedure lcLogFmt(const aMsg : string; const args : array of const); virtual;
      { Use this method to write a formatted error string to the event log. }
{End !!.06}

    procedure lcSetEventLog(anEventLog : TffBaseLog); virtual;
      { Sets the event log to be used by this component. }

    procedure lcSetLogEnabled(const aEnabled : boolean); virtual;

  public

    constructor Create(aOwner : TComponent); override;

    destructor Destroy; override;

    procedure FFNotificationEx(const AOp : Byte; AFrom : TffComponent; {!!.11}
                               const AData : TffWord32); override;     {!!.11}
      { When the freeing of the TffBaseLog is detected, this method
        sets FEventLog to nil to avoid using the freed TffBaseLog. }

  published

    property EventLog : TffBaseLog read FEventLog write lcSetEventLog;
      { The event log to which the component may log messages. }

    property EventLogEnabled : boolean
       read lcGetLogEnabled
       write lcSetLogEnabled
       default False;
      { If True then events are logged to EventLog. }

  end;

  { This class implements a basic state engine. }
  TffStateComponent = class(TffLoggableComponent)
  protected

    scOnStateChange : TNotifyEvent;
      { Handler to be called when the component's state changes. }

    scState : TffState;
      { The current state of the component. }

    procedure scCheckInactive; virtual;
      { When setting certain properties or calling certain methods, this
        method is called to ensure the object is inactive.  If the
        object is not inactive then this method raises exception
        ffsce_MustBeInactive. }

    procedure scCheckStarted; virtual;
      { When setting certain properties or calling certain methods, this
        method is called to ensure the object is started.  If the
        object is not started then this method raises exception
        ffsce_MustBeStarted. }

    procedure scInitialize; virtual; abstract;
      { This method is called when the component is to perform
        its initialization. }

    procedure scPrepareForShutdown; virtual; abstract;
      { This method is called when the component is to prepare for
        shutdown. }

    procedure scShutdown; virtual; abstract;
      { This method is called when the component is to finalize its shutdown. }

    procedure scStartup; virtual; abstract;
      { This method is called when the component is to complete the actions
        required for it to do whatever work it is supposed to do. }

    procedure scSetState(const aState : TffState); virtual;
      { Use this method to set the component's state. }

  public

    constructor Create(aOwner : TComponent); override;

    destructor Destroy; override;

    {$IFDEF DCC4OrLater}                                               {!!.03}
    procedure BeforeDestruction; override;                             {!!.03}
    {$ENDIF}                                                           {!!.03}

    procedure Shutdown; virtual;
      { Sets the component's State to ffesInactive. }

    procedure Startup; virtual;
      { Sets the component's State to ffesStarted. }

    procedure Stop; virtual;
      { Sets the component's State to ffesStopped. }

    property State : TffState read scState write scSetState;
      { The current state of the component. }

  published

    property OnStateChange : TNotifyEvent
      read scOnStateChange
      write scOnStateChange;
      { Event handler called when the component's state changes. }

  end;

  { This type of exception is raised by the various server components when
    a component-related error occurs.  For example, if the user or an application
    tries to set the transport's servername property while the transport is
    active. }
  EffServerComponentError = class(Exception)
  protected
    sceErrorCode : longInt;
    function sceGetErrorString : string;
  public
    constructor Create(const aMsg : string);
    constructor CreateViaCode(const aErrorCode : Longint; aDummy : Boolean);
    constructor CreateViaCodeFmt(const aErrorCode : Longint; args : array of const; aDummy : Boolean);
    constructor CreateWithObj(aObj : TComponent; const aMsg : string);

    property ErrorCode : longInt read sceErrorCode;
  end;


{---Helper routines---}
function FFMapStateToString(const aState : TffState) : string;
  { Maps a state value to a string. }
procedure RaiseSCErrorCode(const aErrorCode : longInt);
procedure RaiseSCErrorCodeFmt(const aErrorCode : longInt;
                                 args : array of const);
procedure RaiseSCErrorMsg(const aMsg : string);
procedure RaiseSCErrorObj(aObj : TComponent; const aMsg : string);


var
  ffStrResServerCmp : TffStringResource;
    {-The string resource providing access to the server component error
      strings. }

const
  { The following array implements the server state engine as specified in
    Section 3.4.3.14 of the FlashFiler 2.0 Design Document.  Exceptions are
    as follows:

    1. If the current state is ffesInitializing & the target state is specified
       as ffesInactive, the next state is ffesInactive.

    2. If the current state is ffesStarting & the target state is specified
       as ffesInactive, the next state is ffesInactive.

    3. State ffesUnsupported not shown in diagram.

    4. State ffesFailed not shown in diagram.

    Exceptions 1 and 2 are allowed because we need a way to short-circuit
    transports back to ffesInactive in the event they fail during initialization
    or startup.

    Given the current state of the engine and the target state of the engine,
    this array identifies the state to which the engine should be moved.

    The first dimension (vertical) of the array is the engine's current state.
    The second dimension (horizontal) of the array is the engine's target state.

    To get the next state, index into the array as follows:

    nextState := ffEngineStateDiagram[<current state>, <target state>];
  }
  ffStateDiagram : array [TffState, TffState] of TffState =
      { Horizontal = destination state, Vertical = current state }
      { ffesInactive    - ffesInitializing- ffesStarting    - ffesStarted     - ffesShuttingDown- ffesStopping    - ffesStopped     - ffesUnsupported- ffesFailed }
    ( ( ffesInactive,     ffesInitializing, ffesInitializing, ffesInitializing, ffesInitializing, ffesInitializing, ffesInitializing, ffesUnsupported, ffesFailed),       // ffesInactive
      ( ffesInactive,     ffesStarting,     ffesStarting,     ffesStarting,     ffesStarting,     ffesStarting,     ffesStarting,     ffesInactive,    ffesInactive),     // ffesInitializing
      ( ffesInactive,     ffesStarted,      ffesStarting,     ffesStarted,      ffesStarted,      ffesStarted,      ffesStarted,      ffesInactive,    ffesInactive),     // ffesStarting
      ( ffesShuttingDown, ffesStopping,     ffesStopping,     ffesStarted,      ffesShuttingDown, ffesStopping,     ffesStopping,     ffesInactive,    ffesInactive ),    // ffesStarted
      ( ffesInactive,     ffesInactive,     ffesInactive,     ffesInactive,     ffesShuttingDown, ffesInactive,     ffesInactive,     ffesInactive,    ffesInactive ),    // ffesShuttingDown
      ( ffesStopped,      ffesStopped,      ffesStopped,      ffesStopped,      ffesStopped,      ffesStopping,     ffesStopped,      ffesInactive,    ffesInactive ),    // ffesStopping
      ( ffesInitializing, ffesInitializing, ffesInitializing, ffesInitializing, ffesInitializing, ffesInitializing, ffesStopped,      ffesInactive,    ffesInactive),     // ffesStopped
      ( ffesUnsupported,  ffesUnsupported,  ffesUnsupported,  ffesUnsupported,  ffesUnsupported,  ffesUnsupported,  ffesUnsupported,  ffesUnsupported, ffesUnsupported),  // ffesUnsupported
      ( ffesFailed,       ffesFailed,       ffesFailed,       ffesFailed,       ffesFailed,       ffesFailed,       ffesFailed,       ffesFailed,      ffesFailed)        // ffesFailed
    );


implementation

{$I ffllscst.inc}
{$R ffllscst.res}

resourcestring
  ffcStateInactive      = 'Inactive';
  ffcStateInitializing  = 'Initializing';
  ffcStateStarting      = 'Starting';
  ffcStateStarted       = 'Started';
  ffcStateShuttingDown  = 'Shutting down';
  ffcStateStopping      = 'Stopping';
  ffcStateStopped       = 'Stopped';
  ffcStateUnsupported   = 'Driver not installed';
  ffcStateFailed        = 'Failed';

{===TffLoggableComponent=============================================}
constructor TffLoggableComponent.Create(aOwner : TComponent);
begin
  inherited Create(aOwner);
  FEventLog := nil;
  FLogEnabled := False;
end;
{--------}
destructor TffLoggableComponent.Destroy;
begin
  if assigned(FEventLog) then
    FEventLog.FFRemoveDependent(Self);                                 {!!.11}
  inherited Destroy;
end;
{--------}
function TffLoggableComponent.lcGetLogEnabled : boolean;
begin
  Result := FLogEnabled;
end;
{--------}
procedure TffLoggableComponent.lcLog(const aString : string);
begin
  if FLogEnabled and assigned(FEventLog) then
    FEventLog.WriteString(aString);
end;
{Begin !!.06}
{--------}
procedure TffLoggableComponent.lcLogFmt(const aMsg : string; const args : array of const);
begin
  if FLogEnabled and assigned(FEventLog) then
    FEventLog.WriteStringFmt(aMsg, args);
end;
{End !!.06}
{--------}
procedure TffLoggableComponent.lcSetEventLog(anEventLog : TffBaseLog);
{Rewritten !!.11}
begin
  if FEventLog <> anEventLog then begin
    if assigned(FEventLog) then
      FEventLog.FFRemoveDependent(Self);                                 

    FEventLog := anEventLog;
    if assigned(FEventLog) then
      FEventLog.FFAddDependent(Self);
  end;
end;
{--------}
procedure TffLoggableComponent.lcSetLogEnabled(const aEnabled : boolean);
begin
  FLogEnabled := aEnabled;
end;
{--------}
{Rewritten !!.11}
procedure TffLoggableComponent.FFNotificationEx(const AOp : Byte; AFrom : TffComponent;
                                                const AData : TffWord32);
begin
  inherited;
  if (AFrom = FEventLog) and
     (AOp in [ffn_Destroy, ffn_Remove]) then begin
    FEventLog.FFRemoveDependent(Self);
    FEventLog := nil;
  end;
end;
{====================================================================}

{===TffStateComponent================================================}
constructor TffStateComponent.Create(aOwner : TComponent);
begin
  inherited Create(aOwner);
  scOnStateChange := nil;
  scState := ffesInactive;
end;
{--------}
destructor TffStateComponent.Destroy;
begin
  if scState <> ffesInactive then
    scSetState(ffesInactive);
  inherited Destroy;
end;
{Begin !!.03}
{$IFDEF DCC4OrLater}
{--------}
 procedure TffStateComponent.BeforeDestruction;
begin
  inherited;

  FFNotifyDependents(ffn_Deactivate);                                 {!!.04}

  if scState <> ffesInactive then
    scSetState(ffesInactive);
end;
{$ENDIF}
{End !!.03}
{--------}
procedure TffStateComponent.scCheckInactive;
begin
  if not (scState in [ffesInactive, ffesUnsupported, ffesFailed]) then  {!!.03}
    RaiseSCErrorCode(ffsce_MustBeInactive);
end;
{--------}
procedure TffStateComponent.scCheckStarted;
begin
  if scState <> ffesStarted then
    RaiseSCErrorCode(ffsce_MustBeStarted);
end;
{--------}
procedure TffStateComponent.scSetState(const aState : TffState);
var
  NextState : TffState;
  OldState : TffState;
begin

  if aState = scState then exit;

  OldState := scState;

  try
    while scState <> aState do begin
      NextState := ffStateDiagram[scState, aState];
      { If our next state is exactly our current state then there is no way
        we can get to the destination state.  This happens when the current
        state is ffesUnsupported or ffesFailed. }
      if NextState = scState then exit;

//      if NextState = ffesShuttingDown then                          {!!.04}
//        FFNotifyDependents(ffn_Deactivate);                         {!!.04}

      scState := NextState;
      case NextState of
        ffesInactive :
          scShutdown;
        ffesInitializing :
          scInitialize;
        ffesStarting :
          scStartup;
        ffesShuttingDown :
          scPrepareForShutdown;
      end;  { case }
      if assigned(scOnStateChange) then
        scOnStateChange(Self);
    end;  { while }
  except
    scState := OldState;
    raise;
  end;
end;
{--------}
procedure TffStateComponent.Shutdown;
begin
  State := ffesInactive;
end;
{--------}
procedure TffStateComponent.Startup;
begin
  State := ffesStarted;
end;
{--------}
procedure TffStateComponent.Stop;
begin
  State := ffesStopped;
end;
{====================================================================}

{===Interfaced helper routines=======================================}
function FFMapStateToString(const aState : TffState) : string;
begin
  case aState of
    ffesInactive :      Result := ffcStateInactive;
    ffesInitializing :  Result := ffcStateInitializing;
    ffesStarting :      Result := ffcStateStarting;
    ffesStarted :       Result := ffcStateStarted;
    ffesShuttingDown :  Result := ffcStateShuttingDown;
    ffesStopping :      Result := ffcStateStopping;
    ffesStopped :       Result := ffcStateStopped;
    ffesUnsupported :   Result := ffcStateUnsupported;
    ffesFailed :        Result := ffcStateFailed;
  else
    Result := '';
  end;  { case }
end;
{--------}
procedure RaiseSCErrorCode(const aErrorCode : longInt);
begin
  raise EffServerComponentError.CreateViaCode(aErrorCode, False);
end;
{--------}
procedure RaiseSCErrorCodeFmt(const aErrorCode : longInt;
                                    args : array of const);
begin
  raise EffServerComponentError.CreateViaCode(aErrorCode, False);
end;
{--------}
procedure RaiseSCErrorMsg(const aMsg : string);
begin
  raise EffServerComponentError.Create(aMsg);
end;
{--------}
procedure RaiseSCErrorObj(aObj : TComponent; const aMsg : string);
begin
  raise EffServerComponentError.CreateWithObj(aObj, aMsg);
end;
{====================================================================}

{===EffServerComponentError==========================================}
constructor EffServerComponentError.Create(const aMsg : string);
begin
  sceErrorCode := 0;
  inherited CreateFmt(ffStrResServerCmp[ffsce_NoErrorCode], [aMsg]);
end;
{--------}
constructor EffServerComponentError.CreateViaCode(const aErrorCode : Longint; aDummy : Boolean);
var
  Msg : string;
begin
  sceErrorCode := aErrorCode;
  Msg := sceGetErrorString;
  inherited CreateFmt(ffStrResServerCmp[ffsce_HasErrorCode], [Msg, aErrorCode, aErrorCode]);
end;
{--------}
constructor EffServerComponentError.CreateViaCodeFmt(const aErrorCode : longInt;
                                                           args : array of const;
                                                           aDummy : Boolean);
var
  Msg : string;
begin
  sceErrorCode := aErrorCode;
  Msg := sceGetErrorString;
  inherited CreateFmt(ffStrResServerCmp[ffsce_HasErrorCode],
                      [format(Msg, args), aErrorCode, aErrorCode]);
end;
{--------}
constructor EffServerComponentError.CreateWithObj(aObj : TComponent;
                                     const aMsg : string);
var
  ObjName : string;
begin
  sceErrorCode := 0;
  if (aObj = nil) then
    ObjName := ffStrResServerCmp[ffsce_NilPointer]
  else begin
    ObjName := aObj.Name;
    if (ObjName = '') then
      ObjName := Format(ffStrResServerCmp[ffsce_UnnamedInst], [aObj.ClassName]);
  end;
  inherited CreateFmt(ffStrResServerCmp[ffsce_InstNoCode], [ObjName, aMsg]);
end;
{--------}
function EffServerComponentError.sceGetErrorString : string;
begin
  Result := ffStrResServerCmp[sceErrorCode];
end;
{====================================================================}

procedure FinalizeUnit;
begin
  ffStrResServerCmp.Free;
end;

procedure InitializeUnit;
begin
  ffStrResServerCmp := nil;
  ffStrResServerCmp := TffStringResource.Create(hInstance, 'FF_SERVER_CMP_STRINGS');
end;

initialization
  InitializeUnit;

finalization
  FinalizeUnit;

end.
