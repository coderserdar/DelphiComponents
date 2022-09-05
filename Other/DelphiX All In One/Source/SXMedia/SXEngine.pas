{
 SXMedia  Components - Beta 1
 --------------------------------
 Copyright 1999 Dean Ellis
 http://www.sillex.freeserve.co.uk

 This unit is part of the SXMedia Component Set. This code is
 supplied as is with no guarantees and must be used at your own
 risk.

 No modifications to this code must be made without the express
 permission of the author. Please report any problems to
 support@sillex.freeserve.co.uk

 You may use these components to create any freeware/shareware
 applications that you wish. If the components are to be used in
 a commercail product then credit for developement of these components
 should be given.

 Credits :

 Developer : Dean Ellis
 Testers   : Dominique Louis
             Ivan Blecic
}
unit SXEngine;

interface

uses
  Windows, Messages, SysUtils, Classes, MMSystem;

type
  TFramesPerSecond = 1..100;
  TFreeCyclesEvent = procedure (Sender: TObject; Count:Integer) of object;

  TSXEngine = class(TComponent)
     private
       { Private Data Members }
       FFramesPerSecond:TFramesPerSecond;
       FEnabled:Boolean;
       FThreadPriority:TThreadPriority;
       FFPS:Integer;
       FActivate:TNotifyEvent;
       FDeActivate:TNotifyEvent;
       FRender:TNotifyEvent;
       FFreeCycles:TFreeCyclesEvent;
     protected
       { Property Accessors }
       procedure SetTargetFPS(Value:TFramesPerSecond);
       function GetTargetFPS:TFramesPerSecond;
       procedure SetEnabled(Value:Boolean);
       procedure SetThreadPriority(Value:TThreadPriority);
       { Notification methods }
       procedure DoRender;
       procedure DoActivate;
       procedure DoDeActivate;
       procedure DoFreeCycles(Count: Integer);
       { Class Helper methods }
     public
       { Public methods }
       constructor Create(AOwner:TComponent);override;
       destructor Destroy; override;
       {Public Properties}
       property FramesPerSecond : Integer read FFPS write FFPS;
     published
       { Published properties }
       property TargetFPS : TFramesPerSecond read GetTargetFPS write SetTargetFPS default 30;
       property Enabled : Boolean read FEnabled write SetEnabled default False;
       property ThreadPriority : TThreadPriority read FThreadPriority write SetThreadPriority;
       { Published Events}
       property OnActivate : TNotifyEvent read FActivate write FActivate;
       property OnDeActivate : TNotifyEvent read FDeActivate write FDeActivate;
       property OnRender : TNotifyEvent read FRender write FRender;
       property OnFreeCycles : TFreeCyclesEvent read FFreeCycles write FFreeCycles;
  end;

implementation
{$J+}
const
   FPS : Single = 33.3;
   SECOND : Single = 1000.0;

type
  TGameThread = class(TThread)
     private
       { Private declarations }
       FThreadCallback:TThreadMethod;
     protected
       procedure Execute; override;
       property ThreadCallback: TThreadMethod read FThreadCallback write FThreadCallback;
     public
       constructor Create(CreateSupsended:Boolean;Callback:TThreadMethod);
  end;

var Thread:TGameThread;
{ Important: Methods and properties of objects in VCL can only be used in a
  method called using Synchronize, for example,

      Synchronize(UpdateCaption);

  and UpdateCaption could look like,

    procedure TGameThread.UpdateCaption;
    begin
      Form1.Caption := 'Updated in a thread';
    end; }

{ TGameThread }

procedure TGameThread.Execute;
begin
  { Place thread code here }
  while not Terminated do
  begin
     try
     if Assigned(ThreadCallback) then Synchronize(ThreadCallback);
     except
       Terminate;
     end;
  end;
end;
constructor TGameThread.Create(CreateSupsended:Boolean;Callback:TThreadMethod);
begin
   inherited Create(CreateSupsended);
   ThreadCallback := Callback;
end;

{ TSXEngine }
constructor TSXEngine.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  if not (csDesigning in ComponentState) then
  begin
     if not Assigned(Thread) then
     begin
       Thread := TGameThread.Create(True,DoRender);
       Thread.FreeOnTerminate := False;
       Thread.Priority := ThreadPriority;
       Thread.Suspended := not Enabled;
     end;
     DoActivate;
  end;
  Enabled := False;
  TargetFPS := 30;
end;
destructor TSXEngine.Destroy;
begin
  if not (csDesigning in ComponentState) then
  begin
     if Assigned(Thread) then
     begin
        Thread.Terminate;
        Thread.Free;
     end;
     DoDeActivate;
  end;
  inherited Destroy;
end;
procedure TSXEngine.SetTargetFPS(Value:TFramesPerSecond);
begin
   FFramesPerSecond := Value;
   FPS := SECOND / Value;
end;
function TSXEngine.GetTargetFPS:TFramesPerSecond;
begin
   Result := FFramesPerSecond;
end;
procedure TSXEngine.SetEnabled(Value:Boolean);
begin
  FEnabled := Value;
  if Thread <> nil then
     Thread.Suspended := not Value;
end;
procedure TSXEngine.SetThreadPriority(Value:TThreadPriority);
begin
   FThreadPriority := Value;
   if Thread <> nil then
      Thread.Priority := Value;
end;
procedure TSXEngine.DoRender;
const Start:DWord = 0;
      OldStart:DWord = 0;
      Count:Integer =0;
var FreeCycleCount:integer;
begin
  try
     Start := TimeGetTime;
     FreeCycleCount := 0;
     if Assigned(FRender) then FRender(Self);
     if (Start - OldStart) < SECOND then
        Inc(Count)
     else
     begin
        FramesPerSecond := Count;
        OldStart := Start;
        Count := 0;
     end;
     repeat
        DoFreeCycles(FreeCycleCount);
        inc(FreeCycleCount);
     until (TimeGetTime - Start) >= FPS;
  except
    Enabled := False;
    Thread.Terminate;
  end;
end;
procedure TSXEngine.DoActivate;
begin
  if Assigned(FActivate) then FActivate(Self);
end;
procedure TSXEngine.DoDeActivate;
begin
  if Assigned(FDeActivate) then FDeActivate(Self);
end;
procedure TSXEngine.DoFreeCycles(Count: Integer);
begin
  if Assigned(FFreeCycles) then FFreeCycles(Self,Count);
end;

end.
