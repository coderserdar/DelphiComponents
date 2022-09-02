{*********************************************************}
{* FSSQL: Base component classes                       *}
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

{$I fsdefine.inc}

Unit fsllcomp;

Interface

Uses
  Classes,
  SysUtils,
  fsllbase,
  fslllog,
  fssrmgr;

Type
  { This type defines the possible states of a TfsStateComponent.  Values:
      fsesInactive - The engine and its associated components (i.e., command
        handlers and transports) are inactive.
      fsesInitializing - The engine and its associated components are
        initializing.
      fsesStarting - The engine and its associates are starting.
      fsesStarted - The engine and its associates are operational and
        processing requests.
      fsesShuttingDown - The engine and its associates are in the process of
        shutting down.
      fsesStopping - The engine is in the process of stopping but its
        associated components are still active.
      fsesStopped - The engine is inactive but its associates are still
        active.
      fsesUnsupported - Transport-specific.  The transport is not supported
        on this workstation.  For example, an IPX/SPX transport is unsupported
        if an IPX/SPX driver is not installed on the workstation.
      fsesFailed - A failure occurred and the engine or transport may no
        longer be used.  A transport's state is set to fsesFailed if an error
        occurs during startup.
  }
  TfsState = (fsesInactive,
    fsesInitializing,
    fsesStarting,
    fsesStarted,
    fsesShuttingDown,
    fsesStopping,
    fsesStopped,
    fsesUnsupported,
    fsesFailed);

  { This class implements the basic functionality for associating a component
    with a descendant of TFSBaseLog. }
  TfsLoggableComponent = Class(TFSSpecComp)
  Protected

    FEventLog: TFSBaseLog;
    { The log to which events may be written. }

    FLogEnabled: boolean;
    { If True then events may be written to the event log. }

    Function lcGetLogEnabled: boolean; Virtual;

    Procedure lcLog(Const aString: String); Virtual;
    { Use this to write a string to the event log. }

{Begin !!.06}
    Procedure lcLogFmt(Const aMsg: String; Const args: Array Of Const); Virtual;
    { Use this method to write a formatted error string to the event log. }
{End !!.06}

    Procedure lcSetEventLog(anEventLog: TFSBaseLog); Virtual;
    { Sets the event log to be used by this component. }

    Procedure lcSetLogEnabled(Const aEnabled: boolean); Virtual;

  Public

    Constructor Create(aOwner: TComponent); Override;

    Destructor Destroy; Override;

    Procedure FFNotificationEx(Const AOp: Byte; AFrom: TFSSpecComp; {!!.11}
      Const AData: TffWord32); Override; {!!.11}
    { When the freeing of the TFSBaseLog is detected, this method
      sets FEventLog to nil to avoid using the freed TFSBaseLog. }

  Published

    Property EventLog: TFSBaseLog Read FEventLog Write lcSetEventLog;
    { The event log to which the component may log messages. }

    Property EventLogEnabled: boolean
      Read lcGetLogEnabled
      Write lcSetLogEnabled
      Default False;
    { If True then events are logged to EventLog. }

  End;

  { This class implements a basic state engine. }
  TfsStateComponent = Class(TfsLoggableComponent)
  Protected

    scOnStateChange: TNotifyEvent;
    { Handler to be called when the component's state changes. }

    scState: TfsState;
    { The current state of the component. }

    Procedure scCheckInactive; Virtual;
    { When setting certain properties or calling certain methods, this
      method is called to ensure the object is inactive.  If the
      object is not inactive then this method raises exception
      fssce_MustBeInactive. }

    Procedure scCheckStarted; Virtual;
    { When setting certain properties or calling certain methods, this
      method is called to ensure the object is started.  If the
      object is not started then this method raises exception
      fssce_MustBeStarted. }

    Procedure scInitialize; Virtual; Abstract;
    { This method is called when the component is to perform
      its initialization. }

    Procedure scPrepareForShutdown; Virtual; Abstract;
    { This method is called when the component is to prepare for
      shutdown. }

    Procedure scShutdown; Virtual; Abstract;
    { This method is called when the component is to finalize its shutdown. }

    Procedure scStartup; Virtual; Abstract;
    { This method is called when the component is to complete the actions
      required for it to do whatever work it is supposed to do. }

    Procedure scSetState(Const aState: TfsState); Virtual;
    { Use this method to set the component's state. }

  Public

    Constructor Create(aOwner: TComponent); Override;

    Destructor Destroy; Override;

    {$IFDEF DCC4OrLater} {!!.03}
    Procedure BeforeDestruction; Override; {!!.03}
    {$ENDIF} {!!.03}

    Procedure Shutdown; Virtual;
    { Sets the component's State to fsesInactive. }

    Procedure Startup; Virtual;
    { Sets the component's State to fsesStarted. }

    Procedure Stop; Virtual;
    { Sets the component's State to fsesStopped. }

    Property State: TfsState Read scState Write scSetState;
    { The current state of the component. }

  Published

    Property OnStateChange: TNotifyEvent
      Read scOnStateChange
      Write scOnStateChange;
    { Event handler called when the component's state changes. }

  End;

  { This type of exception is raised by the various server components when
    a component-related error occurs.  For example, if the user or an application
    tries to set the transport's servername property while the transport is
    active. }
  EfsServerComponentError = Class(Exception)
  Protected
    sceErrorCode: Longint;
    Function sceGetErrorString: String;
  Public
    Constructor Create(Const aMsg: String);
    Constructor CreateViaCode(Const aErrorCode: Longint; aDummy: Boolean);
    Constructor CreateViaCodeFmt(Const aErrorCode: Longint; args: Array Of Const; aDummy: Boolean);
    Constructor CreateWithObj(aObj: TComponent; Const aMsg: String);

    Property ErrorCode: Longint Read sceErrorCode;
  End;

  {---Helper routines---}
Function FSMapStateToString(Const aState: TfsState): String;
{ Maps a state value to a string. }
Procedure fsRaiseSCErrorCode(Const aErrorCode: Longint);
Procedure fsRaiseSCErrorCodeFmt(Const aErrorCode: Longint;
  args: Array Of Const);
Procedure fsRaiseSCErrorMsg(Const aMsg: String);
Procedure fsRaiseSCErrorObj(aObj: TComponent; Const aMsg: String);

Var
  fsStrResServerCmp: TfsStringResource;
  {-The string resource providing access to the server component error
    strings. }

Const
  { The following array implements the server state engine as specified in
    Section 3.4.3.14 of the FlashFiler 2.0 Design Document.  Exceptions are
    as follows:

    1. If the current state is fsesInitializing & the target state is specified
       as fsesInactive, the next state is fsesInactive.

    2. If the current state is fsesStarting & the target state is specified
       as fsesInactive, the next state is fsesInactive.

    3. State fsesUnsupported not shown in diagram.

    4. State fsesFailed not shown in diagram.

    Exceptions 1 and 2 are allowed because we need a way to short-circuit
    transports back to fsesInactive in the event they fail during initialization
    or startup.

    Given the current state of the engine and the target state of the engine,
    this array identifies the state to which the engine should be moved.

    The first dimension (vertical) of the array is the engine's current state.
    The second dimension (horizontal) of the array is the engine's target state.

    To get the next state, index into the array as follows:

    nextState := ffEngineStateDiagram[<current state>, <target state>];
  }
  fsStateDiagram: Array[TfsState, TfsState] Of TfsState =
  { Horizontal = destination state, Vertical = current state }
  { fsesInactive    - fsesInitializing- fsesStarting    - fsesStarted     - fsesShuttingDown- fsesStopping    - fsesStopped     - fsesUnsupported- fsesFailed }
  ((fsesInactive, fsesInitializing, fsesInitializing, fsesInitializing, fsesInitializing, fsesInitializing, fsesInitializing, fsesUnsupported,
    fsesFailed), // fsesInactive
    (fsesInactive, fsesStarting, fsesStarting, fsesStarting, fsesStarting, fsesStarting, fsesStarting, fsesInactive, fsesInactive),
      // fsesInitializing
    (fsesInactive, fsesStarted, fsesStarting, fsesStarted, fsesStarted, fsesStarted, fsesStarted, fsesInactive, fsesInactive), // fsesStarting
    (fsesShuttingDown, fsesStopping, fsesStopping, fsesStarted, fsesShuttingDown, fsesStopping, fsesStopping, fsesInactive, fsesInactive),
      // fsesStarted
    (fsesInactive, fsesInactive, fsesInactive, fsesInactive, fsesShuttingDown, fsesInactive, fsesInactive, fsesInactive, fsesInactive),
      // fsesShuttingDown
    (fsesStopped, fsesStopped, fsesStopped, fsesStopped, fsesStopped, fsesStopping, fsesStopped, fsesInactive, fsesInactive), // fsesStopping
    (fsesInitializing, fsesInitializing, fsesInitializing, fsesInitializing, fsesInitializing, fsesInitializing, fsesStopped, fsesInactive,
      fsesInactive), // fsesStopped
    (fsesUnsupported, fsesUnsupported, fsesUnsupported, fsesUnsupported, fsesUnsupported, fsesUnsupported, fsesUnsupported, fsesUnsupported,
      fsesUnsupported), // fsesUnsupported
    (fsesFailed, fsesFailed, fsesFailed, fsesFailed, fsesFailed, fsesFailed, fsesFailed, fsesFailed, fsesFailed) // fsesFailed
    );

Implementation

{$I fsllscst.inc}
{$R fsllscst.res}

Resourcestring
  fscStateInactive = 'Inactive';
  fscStateInitializing = 'Initializing';
  fscStateStarting = 'Starting';
  fscStateStarted = 'Started';
  fscStateShuttingDown = 'Shutting down';
  fscStateStopping = 'Stopping';
  fscStateStopped = 'Stopped';
  fscStateUnsupported = 'Driver not installed';
  fscStateFailed = 'Failed';

  {===TfsLoggableComponent=============================================}

Constructor TfsLoggableComponent.Create(aOwner: TComponent);
Begin
  Inherited Create(aOwner);
  FEventLog := Nil;
  FLogEnabled := False;
End;
{--------}

Destructor TfsLoggableComponent.Destroy;
Begin
  If assigned(FEventLog) Then
    FEventLog.FFRemoveDependent(Self); {!!.11}
  Inherited Destroy;
End;
{--------}

Function TfsLoggableComponent.lcGetLogEnabled: boolean;
Begin
  Result := FLogEnabled;
End;
{--------}

Procedure TfsLoggableComponent.lcLog(Const aString: String);
Begin
  If FLogEnabled And assigned(FEventLog) Then
    FEventLog.WriteString(aString);
End;
{Begin !!.06}
{--------}

Procedure TfsLoggableComponent.lcLogFmt(Const aMsg: String; Const args: Array Of Const);
Begin
  If FLogEnabled And assigned(FEventLog) Then
    FEventLog.WriteStringFmt(aMsg, args);
End;
{End !!.06}
{--------}

Procedure TfsLoggableComponent.lcSetEventLog(anEventLog: TFSBaseLog);
{Rewritten !!.11}
Begin
  If FEventLog <> anEventLog Then
    Begin
      If assigned(FEventLog) Then
        FEventLog.FFRemoveDependent(Self);

      FEventLog := anEventLog;
      If assigned(FEventLog) Then
        FEventLog.FFAddDependent(Self);
    End;
End;
{--------}

Procedure TfsLoggableComponent.lcSetLogEnabled(Const aEnabled: boolean);
Begin
  FLogEnabled := aEnabled;
End;
{--------}
{Rewritten !!.11}

Procedure TfsLoggableComponent.FFNotificationEx(Const AOp: Byte; AFrom: TFSSpecComp;
  Const AData: TffWord32);
Begin
  Inherited;
  If (AFrom = FEventLog) And
    (AOp In [ffn_Destroy, ffn_Remove]) Then
    Begin
      FEventLog.FFRemoveDependent(Self);
      FEventLog := Nil;
    End;
End;
{====================================================================}

{===TfsStateComponent================================================}

Constructor TfsStateComponent.Create(aOwner: TComponent);
Begin
  Inherited Create(aOwner);
  scOnStateChange := Nil;
  scState := fsesInactive;
End;
{--------}

Destructor TfsStateComponent.Destroy;
Begin
  If scState <> fsesInactive Then
    scSetState(fsesInactive);
  Inherited Destroy;
End;
{Begin !!.03}
{$IFDEF DCC4OrLater}
{--------}

Procedure TfsStateComponent.BeforeDestruction;
Begin
  Inherited;

  FFNotifyDependents(ffn_Deactivate); {!!.04}

  If scState <> fsesInactive Then
    scSetState(fsesInactive);
End;
{$ENDIF}
{End !!.03}
{--------}

Procedure TfsStateComponent.scCheckInactive;
Begin
  If Not (scState In [fsesInactive, fsesUnsupported, fsesFailed]) Then {!!.03}
    fsRaiseSCErrorCode(fssce_MustBeInactive);
End;
{--------}

Procedure TfsStateComponent.scCheckStarted;
Begin
  If scState <> fsesStarted Then
    fsRaiseSCErrorCode(fssce_MustBeStarted);
End;
{--------}

Procedure TfsStateComponent.scSetState(Const aState: TfsState);
Var
  NextState: TfsState;
  OldState: TfsState;
Begin

  If aState = scState Then Exit;

  OldState := scState;

  Try
    While scState <> aState Do
      Begin
        NextState := fsStateDiagram[scState, aState];
        { If our next state is exactly our current state then there is no way
          we can get to the destination state.  This happens when the current
          state is fsesUnsupported or fsesFailed. }
        If NextState = scState Then Exit;

        //      if NextState = fsesShuttingDown then                          {!!.04}
        //        FFNotifyDependents(ffn_Deactivate);                         {!!.04}

        scState := NextState;
        Case NextState Of
          fsesInactive:
            scShutdown;
          fsesInitializing:
            scInitialize;
          fsesStarting:
            scStartup;
          fsesShuttingDown:
            scPrepareForShutdown;
        End; { case }
        If assigned(scOnStateChange) Then
          scOnStateChange(Self);
      End; { while }
  Except
    scState := OldState;
    Raise;
  End;
End;
{--------}

Procedure TfsStateComponent.Shutdown;
Begin
  State := fsesInactive;
End;
{--------}

Procedure TfsStateComponent.Startup;
Begin
  State := fsesStarted;
End;
{--------}

Procedure TfsStateComponent.Stop;
Begin
  State := fsesStopped;
End;
{====================================================================}

{===Interfaced helper routines=======================================}

Function FSMapStateToString(Const aState: TfsState): String;
Begin
  Case aState Of
    fsesInactive: Result := fscStateInactive;
    fsesInitializing: Result := fscStateInitializing;
    fsesStarting: Result := fscStateStarting;
    fsesStarted: Result := fscStateStarted;
    fsesShuttingDown: Result := fscStateShuttingDown;
    fsesStopping: Result := fscStateStopping;
    fsesStopped: Result := fscStateStopped;
    fsesUnsupported: Result := fscStateUnsupported;
    fsesFailed: Result := fscStateFailed;
    Else
      Result := '';
  End; { case }
End;
{--------}

Procedure fsRaiseSCErrorCode(Const aErrorCode: Longint);
Begin
  Raise EfsServerComponentError.CreateViaCode(aErrorCode, False);
End;
{--------}

Procedure fsRaiseSCErrorCodeFmt(Const aErrorCode: Longint;
  args: Array Of Const);
Begin
  Raise EfsServerComponentError.CreateViaCode(aErrorCode, False);
End;
{--------}

Procedure fsRaiseSCErrorMsg(Const aMsg: String);
Begin
  Raise EfsServerComponentError.Create(aMsg);
End;
{--------}

Procedure fsRaiseSCErrorObj(aObj: TComponent; Const aMsg: String);
Begin
  Raise EfsServerComponentError.CreateWithObj(aObj, aMsg);
End;
{====================================================================}

{===EfsServerComponentError==========================================}

Constructor EfsServerComponentError.Create(Const aMsg: String);
Begin
  sceErrorCode := 0;
  Inherited CreateFmt(fsStrResServerCmp[fssce_NoErrorCode], [aMsg]);
End;
{--------}

Constructor EfsServerComponentError.CreateViaCode(Const aErrorCode: Longint; aDummy: Boolean);
Var
  Msg: String;
Begin
  sceErrorCode := aErrorCode;
  Msg := sceGetErrorString;
  Inherited CreateFmt(fsStrResServerCmp[fssce_HasErrorCode], [Msg, aErrorCode, aErrorCode]);
End;
{--------}

Constructor EfsServerComponentError.CreateViaCodeFmt(Const aErrorCode: Longint;
  args: Array Of Const;
  aDummy: Boolean);
Var
  Msg: String;
Begin
  sceErrorCode := aErrorCode;
  Msg := sceGetErrorString;
  Inherited CreateFmt(fsStrResServerCmp[fssce_HasErrorCode],
    [format(Msg, args), aErrorCode, aErrorCode]);
End;
{--------}

Constructor EfsServerComponentError.CreateWithObj(aObj: TComponent;
  Const aMsg: String);
Var
  ObjName: String;
Begin
  sceErrorCode := 0;
  If (aObj = Nil) Then
    ObjName := fsStrResServerCmp[fssce_NilPointer]
  Else
    Begin
      ObjName := aObj.Name;
      If (ObjName = '') Then
        ObjName := Format(fsStrResServerCmp[fssce_UnnamedInst], [aObj.ClassName]);
    End;
  Inherited CreateFmt(fsStrResServerCmp[fssce_InstNoCode], [ObjName, aMsg]);
End;
{--------}

Function EfsServerComponentError.sceGetErrorString: String;
Begin
  Result := fsStrResServerCmp[sceErrorCode];
End;
{====================================================================}

Procedure FinalizeUnit;
Begin
  fsStrResServerCmp.Free;
End;

Procedure InitializeUnit;
Begin
  fsStrResServerCmp := Nil;
  fsStrResServerCmp := TfsStringResource.Create(hInstance, 'FS_SERVER_CMP_STRINGS');
End;

Initialization
  InitializeUnit;

Finalization
  FinalizeUnit;

End.

