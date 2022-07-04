{                                                                              }
{                                Threads v3.00                                 }
{                                                                              }
{             This unit is copyright © 2001-2004 by David J Butler             }
{                                                                              }
{                  This unit is part of Delphi Fundamentals.                   }
{                    Its original file name is cThreads.pas                    }
{       The latest version is available from the Fundamentals home page        }
{                     http://fundementals.sourceforge.net/                     }
{                                                                              }
{                I invite you to use this unit, free of charge.                }
{        I invite you to distibute this unit, but it must be for free.         }
{             I also invite you to contribute to its development,              }
{             but do not distribute a modified copy of this file.              }
{                                                                              }
{          A forum is available on SourceForge for general discussion          }
{             http://sourceforge.net/forum/forum.php?forum_id=2117             }
{                                                                              }
{ Description:                                                                 }
{   Thread classes and functions.                                              }
{                                                                              }
{ Revision history:                                                            }
{   03/04/2004  3.00  Revised for Fundamentals 3.                              }
{                                                                              }

{$INCLUDE ..\cDefines.inc}
unit cThreads;

interface

uses
  { Delphi }
  Classes;



{                                                                              }
{ TThreadEx                                                                    }
{   Extended base class for thread implementations.                            }
{                                                                              }
type
  TThreadEx = class(TThread)
  public
    { Make TThread's Synchronize method public }
    procedure Synchronize(Method: TThreadMethod);

    { Make TThread's Terminate method virtual }
    procedure Terminate; virtual;

    { Make Terminated property public }
    property  Terminated;
  end;



{                                                                              }
{ TThreadComponent                                                             }
{   VCL component implementation of a thread.                                  }
{                                                                              }
type
  TThreadComponent = class;
  TThreadComponentEvent = procedure (Sender: TThreadComponent) of object;
  TThreadComponent = class(TComponent)
  protected
    FActive          : Boolean;
    FPriority        : TThreadPriority;
    FLoadActive      : Boolean;
    FOnExecute       : TThreadComponentEvent;
    FOnTerminate     : TThreadComponentEvent;
    FOnError         : TThreadComponentEvent;
    FOnSyncTerminate : TThreadComponentEvent;
    FOnSyncError     : TThreadComponentEvent;
    FThread          : TThreadEx;
    FErrorMessage    : String;

    procedure Loaded; override;
    procedure SetActive(const Active: Boolean);
    procedure SetPriority(const Priority: TThreadPriority);
    function  GetTerminating: Boolean;
    procedure ThreadExecute; virtual;
    procedure NotifyTerminate;
    procedure NotifyError;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property  Active: Boolean read FActive write SetActive default False;
    property  Priority: TThreadPriority read FPriority write SetPriority default tpNormal;
    property  OnExecute: TThreadComponentEvent read FOnExecute write FOnExecute;
    property  OnTerminate: TThreadComponentEvent read FOnTerminate write FOnTerminate;
    property  OnError: TThreadComponentEvent read FOnError write FOnError;
    property  OnSyncTerminate: TThreadComponentEvent read FOnSyncTerminate write FOnSyncTerminate;
    property  OnSyncError: TThreadComponentEvent read FOnSyncError write FOnSyncError;

    property  Terminating: Boolean read GetTerminating;
    procedure Terminate;

    property  Thread: TThreadEx read FThread;
    property  ErrorMessage: String read FErrorMessage;
  end;

  { TfndThread                                                                 }
  {   Published Thread component.                                              }
  TfndThread = class(TThreadComponent)
  published
    property  Active;
    property  Priority;
    property  OnExecute;
    property  OnTerminate;
    property  OnError;
    property  OnSyncTerminate;
    property  OnSyncError;
  end;



implementation

uses
  { Delphi }
  SysUtils;



{                                                                              }
{ TThreadEx                                                                    }
{                                                                              }
procedure TThreadEx.Synchronize(Method: TThreadMethod);
begin
  inherited Synchronize(Method);
end;

procedure TThreadEx.Terminate;
begin
  inherited Terminate;
end;



{                                                                              }
{ TThreadComponentThread                                                       }
{                                                                              }
type
  TThreadComponentThread = class(TThreadEx)
  protected
    FComponent : TThreadComponent;
    procedure Execute; override;
  public
    constructor Create(const Component: TThreadComponent);
  end;

constructor TThreadComponentThread.Create(const Component: TThreadComponent);
begin
  Assert(Assigned(Component));
  FComponent := Component;
  FreeOnTerminate := False;
  inherited Create(False);
end;

procedure TThreadComponentThread.Execute;
begin
  if not Assigned(FComponent) then
    exit;
  Priority := FComponent.Priority;
  FComponent.ThreadExecute;
end;



{                                                                              }
{ TThreadComponent                                                             }
{                                                                              }
constructor TThreadComponent.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPriority := tpNormal;
end;

destructor TThreadComponent.Destroy;
begin
  FreeAndNil(FThread);
  inherited Destroy;
end;

procedure TThreadComponent.Loaded;
begin
  inherited Loaded;
  if FLoadActive then
    SetActive(True);
end;

procedure TThreadComponent.SetActive(const Active: Boolean);
begin
  if Active = FActive then
    exit;
  if csDesigning in ComponentState then
    begin
      FActive := Active;
      exit;
    end;
  if csLoading in ComponentState then
    begin
      FLoadActive := True;
      exit;
    end;
  FActive := Active;
  if Active then
    begin
      FreeAndNil(FThread);
      FThread := TThreadComponentThread.Create(self);
    end
  else
    Terminate;
end;

procedure TThreadComponent.SetPriority(const Priority: TThreadPriority);
begin
  if FPriority = Priority then
    exit;
  FPriority := Priority;
  if [csDesigning, csLoading] * ComponentState <> [] then
    exit;
  if FActive and Assigned(FThread) and not FThread.Terminated then
    FThread.Priority := Priority;
end;

function TThreadComponent.GetTerminating: Boolean;
begin
  Result := Assigned(FThread) and FThread.Terminated;
end;

procedure TThreadComponent.Terminate;
begin
  if Assigned(FThread) then
    try try
      FThread.Terminate;
      FThread.WaitFor;
    except end;
    finally
      FreeAndNil(FThread);
    end;
end;

procedure TThreadComponent.ThreadExecute;
begin
  try try
    FErrorMessage := '';
    if Assigned(FOnExecute) then
      FOnExecute(self);
  except
    on E : Exception do
      begin
        FErrorMessage := E.Message;
        if Assigned(FOnError) then
          FOnError(self);
        if Assigned(FOnSyncError) and Assigned(FThread) then
          FThread.Synchronize(NotifyError);
      end;
  end;
  finally
    FActive := False;
    if Assigned(FOnTerminate) then
      FOnTerminate(self);
    if Assigned(FOnSyncTerminate) and Assigned(FThread) then
      FThread.Synchronize(NotifyTerminate);
  end;
end;

procedure TThreadComponent.NotifyTerminate;
begin
  if Assigned(FOnSyncTerminate) then
    FOnSyncTerminate(self);
end;

procedure TThreadComponent.NotifyError;
begin
  if Assigned(FOnSyncError) then
    FOnSyncError(self);
end;



end.

