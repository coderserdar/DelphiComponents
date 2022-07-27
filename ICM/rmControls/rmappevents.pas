{================================================================================
Copyright (C) 1997-2002 Mills Enterprise

Unit     : rmAppEvents
Purpose  : Originally a fix for D5 bug but is now usefull for other versions
           of Delphi.
Date     : 01-15-2000
Author   : Ryan J. Mills
Version  : 1.90
================================================================================}

unit rmAppEvents;

interface

{$I CompilerDefines.INC}

uses Windows, Messages, SysUtils, Classes, Forms, ActnList;

type
  TrmCustomApplicationEvents = class(TComponent)
  private
    FOnActionExecute: TActionEvent;
    FOnActionUpdate: TActionEvent;
    FOnException: TExceptionEvent;
    FOnMessage: TMessageEvent;
    FOnHelp: THelpEvent;
    FOnHint: TNotifyEvent;
    FOnIdle: TIdleEvent;
    FOnDeactivate: TNotifyEvent;
    FOnActivate: TNotifyEvent;
    FOnMinimize: TNotifyEvent;
    FOnRestore: TNotifyEvent;
    FOnShortCut: TShortCutEvent;
    FOnShowHint: TShowHintEvent;
    procedure DoActionExecute(Action: TBasicAction; var Handled: Boolean);
    procedure DoActionUpdate(Action: TBasicAction; var Handled: Boolean);
    procedure DoActivate(Sender: TObject);
    procedure DoDeactivate(Sender: TObject);
    procedure DoException(Sender: TObject; E: Exception);
    procedure DoIdle(Sender: TObject; var Done: Boolean);
    function DoHelp(Command: Word; Data: Longint; var CallHelp: Boolean): Boolean;
    procedure DoHint(Sender: TObject);
    procedure DoMessage(var Msg: TMsg; var Handled: Boolean);
    procedure DoMinimize(Sender: TObject);
    procedure DoRestore(Sender: TObject);
    procedure DoShowHint(var HintStr: string; var CanShow: Boolean;
      var HintInfo: THintInfo);
    procedure DoShortcut(var Msg: TWMKey; var Handled: Boolean);
  protected
    property OnActionExecute: TActionEvent read FOnActionExecute write FOnActionExecute;
    property OnActionUpdate: TActionEvent read FOnActionUpdate write FOnActionUpdate;
    property OnActivate: TNotifyEvent read FOnActivate write FOnActivate;
    property OnDeactivate: TNotifyEvent read FOnDeactivate write FOnDeactivate;
    property OnException: TExceptionEvent read FOnException write FOnException;
    property OnIdle: TIdleEvent read FOnIdle write FOnIdle;
    property OnHelp: THelpEvent read FOnHelp write FOnHelp;
    property OnHint: TNotifyEvent read FOnHint write FOnHint;
    property OnMessage: TMessageEvent read FOnMessage write FOnMessage;
    property OnMinimize: TNotifyEvent read FOnMinimize write FOnMinimize;
    property OnRestore: TNotifyEvent read FOnRestore write FOnRestore;
    property OnShowHint: TShowHintEvent read FOnShowHint write FOnShowHint;
    property OnShortCut: TShortCutEvent read FOnShortCut write FOnShortCut;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TrmApplicationEvents = class(TrmCustomApplicationEvents)
  published
    property OnActionExecute;
    property OnActionUpdate;
    property OnActivate;
    property OnDeactivate;
    property OnException;
    property OnIdle;
    property OnHelp;
    property OnHint;
    property OnMessage;
    property OnMinimize;
    property OnRestore;
    property OnShowHint;
    property OnShortCut;
  end;

implementation

uses Contnrs, Consts, StdActns;

{ TrmCustomApplicationEvents }

constructor TrmCustomApplicationEvents.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  if csdesigning in componentstate then
     exit;

  with Application do
  begin
    OnActionExecute := DoActionExecute;
    OnActionUpdate := DoActionUpdate;
    OnActivate := DoActivate;
    OnDeactivate := DoDeactivate;
    OnException := DoException;
    OnHelp := DoHelp;
    OnHint := DoHint;
    OnIdle := DoIdle;
    OnMessage := DoMessage;
    OnMinimize := DoMinimize;
    OnRestore := DoRestore;
    OnShowHint := DoShowHint;
    OnShortCut := DoShortcut;
  end;

end;

destructor TrmCustomApplicationEvents.Destroy;
begin
  with Application do
  begin
    OnActionExecute := nil;
    OnActionUpdate := nil;
    OnActivate := nil;
    OnDeactivate := nil;
    OnException := nil;
    OnHelp := nil;
    OnHint := nil;
    OnIdle := nil;
    OnMessage := nil;
    OnMinimize := nil;
    OnRestore := nil;
    OnShowHint := nil;
    OnShortCut := nil;
  end;

  inherited;
end;

procedure TrmCustomApplicationEvents.DoActionExecute(Action: TBasicAction;
  var Handled: Boolean);
begin
  if Assigned(FOnActionExecute) then
     FOnActionExecute(Action, Handled);
end;

procedure TrmCustomApplicationEvents.DoActionUpdate(Action: TBasicAction;
  var Handled: Boolean);
begin
  if Assigned(FOnActionUpdate) then
     FOnActionUpdate(Action, Handled);
end;

procedure TrmCustomApplicationEvents.DoActivate(Sender: TObject);
begin
  if Assigned(FOnActivate) then
     FOnActivate(Sender);
end;

procedure TrmCustomApplicationEvents.DoDeactivate(Sender: TObject);
begin
  if Assigned(FOnDeactivate) then FOnDeactivate(Sender);
end;

procedure TrmCustomApplicationEvents.DoException(Sender: TObject;
  E: Exception);
begin
  if Assigned(FOnException) then
     FOnException(Sender, E)
  else
     Application.ShowException(E);
end;

function TrmCustomApplicationEvents.DoHelp(Command: Word; Data: Integer;
  var CallHelp: Boolean): Boolean;
begin
  if Assigned(FOnHelp) then
    Result := FOnHelp(Command, Data, CallHelp)
  else
    Result := False;  
end;

procedure TrmCustomApplicationEvents.DoHint(Sender: TObject);
begin
  if Assigned(FOnHint) then
     FOnHint(Sender)
{$IFDEF D4_OR_HIGHER}
  else
  { Fire THintAction to anyone interested }
  with THintAction.Create(Self) do
  begin
    Hint := Application.hint;
    try
      Execute;
    finally
      Free;
    end;
  end;
{$ENDIF}
end;

procedure TrmCustomApplicationEvents.DoIdle(Sender: TObject; var Done: Boolean);
begin
  if Assigned(FOnIdle) then
     FOnIdle(Sender, Done);
end;

procedure TrmCustomApplicationEvents.DoMessage(var Msg: TMsg; var Handled: Boolean);
begin
  if Assigned(FOnMessage) then
     FOnMessage(Msg, Handled);
end;

procedure TrmCustomApplicationEvents.DoMinimize(Sender: TObject);
begin
  if Assigned(FOnMinimize) then
     FOnMinimize(Sender);
end;

procedure TrmCustomApplicationEvents.DoRestore(Sender: TObject);
begin
  if Assigned(FOnRestore) then
     FOnRestore(Sender);
end;

procedure TrmCustomApplicationEvents.DoShortcut(var Msg: TWMKey;
  var Handled: Boolean);
begin
  if Assigned(FOnShortcut) then
     FOnShortcut(Msg, Handled);
end;

procedure TrmCustomApplicationEvents.DoShowHint(var HintStr: string;
  var CanShow: Boolean; var HintInfo: THintInfo);
begin
  if Assigned(FOnShowHint) then
     FOnShowHint(HintStr, CanShow, HintInfo);
end;

end.
