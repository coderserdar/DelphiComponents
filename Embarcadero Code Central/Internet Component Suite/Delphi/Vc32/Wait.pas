{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Program:      WAIT.PAS
Object:       Delphi component which enable a component or application to
              wait for some event, optionnaly displaying a progress bar.
Author:       François PIETTE
Creation:     April, 1996
Version:      2.13
EMail:        http://users.swing.be/francois.piette  francois.piette@swing.be
              http://www.rtfm.be/fpiette             francois.piette@rtfm.be
              francois.piette@pophost.eunet.be
Support:      Use the mailing list twsocket@rtfm.be See website for details.
Legal issues: Copyright (C) 1996-2000 by François PIETTE
              Rue de Grady 24, 4053 Embourg, Belgium. Fax: +32-4-365.74.56
              <francois.piette@pophost.eunet.be>

              This software is provided 'as-is', without any express or
              implied warranty.  In no event will the author be held liable
              for any  damages arising from the use of this software.

              Permission is granted to anyone to use this software for any
              purpose, including commercial applications, and to alter it
              and redistribute it freely, subject to the following
              restrictions:

              1. The origin of this software must not be misrepresented,
                 you must not claim that you wrote the original software.
                 If you use this software in a product, an acknowledgment
                 in the product documentation would be appreciated but is
                 not required.

              2. Altered source versions must be plainly marked as such, and
                 must not be misrepresented as being the original software.

              3. This notice may not be removed or altered from any source
                 distribution.

              4. You must register this software by sending a picture postcard
                 to the author. Use a nice stamp and mention your name, street
                 address, EMail address and any comment you like to say.

Updates:
Jul 22, 1997  Adapted to Delphi 3
Oct 22, 1997  V2.00 Added WaitVersion constant and Running property
Nov 11, 1997  V2.10 Made a TCustomWait base component with virtual functions.
              This will easy the making of descendend components.
              Renamed OnWaitEvent    -> OnWait
                      OnTimeOutEvent -> OnTimeout
              Added events:
              OnWaitStart   When the component starts his job.
              OnWaiting     When the component is waiting.
              OnWaitStop    When the component stops his job.
Mar 27, 1998  V2.11 Adapted for C++Builder 3
Apr 20, 1998  V2.12 Removed the second 'r' from 'courrier'
Jul 08, 1998  V2.13 Adadpted for Delphi 4


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit Wait;

{$B-}           { Enable partial boolean evaluation   }
{$T-}           { Untyped pointers                    }
{$IFNDEF VER80} { Not for Delphi 1                    }
    {$J+}       { Allow typed constant to be modified }
{$ENDIF}
{$IFDEF VER110} { C++ Builder V3.0                    }
    {$ObjExportAll On}
{$ENDIF}

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, ExtCtrls;

const
  WaitVersion = 212;

type
  TWaitEvent = procedure(Sender: TObject; Count : integer) of object;

  TCustomWait = class(TCustomControl)
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
  private
    FPen         : TPen;
    FFont        : TFont;
    FBrush       : TBrush;
    FCaption     : String;
    FTimer       : TTimer;
    FOnWait      : TWaitEvent;
    FOnWaiting   : TNotifyEvent;
    FOnWaitStart : TNotifyEvent;
    FOnWaitStop  : TNotifyEvent;
    FOnTimeout   : TNotifyEvent;
    FModalResult : TModalResult;
    FStartVal    : Integer;
  protected
    procedure   Paint; override;
    procedure   TimerEvent(Sender: TObject);
    procedure   AppMessage(var Msg: TMsg; var Handled: Boolean);
    function    GetRunning : Boolean;
    procedure   SetInterval(Value : Word);
    function    GetInterval : Word;
  public
    procedure   Start; virtual;
    procedure   Stop; virtual;
    procedure   StartModal; virtual;
    procedure   Restart; virtual;
  protected
    property Caption     : String       read FCaption      write FCaption;
    property ModalResult : TModalResult read FModalResult  write FModalResult;
    property Interval    : Word         read GetInterval   write SetInterval;
    property Running     : Boolean      read GetRunning;
    property OnWait      : TWaitEvent   read FOnWait       write FOnWait;
    property OnTimeout   : TNotifyEvent read FOnTimeout    write FOnTimeout;
    property OnWaiting   : TNotifyEvent read FOnWaiting    write FOnWaiting;
    property OnWaitStart : TNotifyEvent read FOnWaitStart  write FOnWaitStart;
    property OnWaitStop  : TNotifyEvent read FOnWaitStop   write FOnWaitStop;
  end;

  TWait = class(TCustomWait)
  published
    property Caption;
    property ModalResult;
    property Interval;
    property OnWait;
    property OnWaiting;
    property OnWaitStart;
    property OnWaitStop;
    property OnTimeout;
    property Running;
    property Visible;
  end;

procedure Register;

implementation

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure Register;
begin
    RegisterComponents('FPiette', [TWait]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function atoi(value : string) : Integer;
var
    i : Integer;
begin
    Result := 0;
    i := 1;
    while (i <= Length(Value)) and (Value[i] = ' ') do
        i := i + 1;
    while (i <= Length(Value)) and (Value[i] >= '0') and (Value[i] <= '9')do begin
        Result := Result * 10 + ord(Value[i]) - ord('0');
        i := i + 1;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TCustomWait.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);
    IsControl      := TRUE;
    Width          := 192;
    Height         := 32;
    Caption        := '60';
    FStartVal      := 60;
    FOnWait        := nil;
    FBrush         := TBrush.Create;
    FPen           := TPen.Create;
    FFont          := TFont.Create;
    FFont.Size     := 8;
    FFont.Name     := 'Courier';
    FFont.Pitch    := fpFixed;
    FTimer         := TTimer.Create(Self);
    FTimer.Enabled := FALSE;
    FTimer.OnTimer := TimerEvent;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TCustomWait.Destroy;
begin
    FPen.Free;
    FFont.Free;
    FBrush.Free;
    FTimer.Free;
    inherited Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWait.SetInterval(Value : Word);
begin
    FTimer.Interval := Value;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomWait.GetInterval : Word;
begin
    Result := FTimer.Interval;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWait.Start;
begin
    FStartVal := atoi(Caption);
    if FStartVal = 0 then begin
        FStartVal := 15;
        Caption   := IntToStr(FStartVal);
    end;
    FTimer.Enabled := TRUE;
    if Assigned(FOnWaitStart) then
        FOnWaitStart(Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWait.Restart;
begin
    Caption := IntToStr(FStartVal);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWait.Stop;
begin
    FModalResult   := mrOk;
    FTimer.Enabled := FALSE;
    Caption        := IntToStr(FStartVal);
    if Assigned(FOnWaitStop) then
        FOnWaitStop(Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWait.TimerEvent(Sender: TObject);
var
    Count : Integer;
begin
    Count := atoi(FCaption) - 1;

    if Assigned(FOnWait) then
        FOnWait(Self, Count);

    if Count <= 0 then begin
        FTimer.Enabled := FALSE;
        FCaption       := 'Timeout';
        FModalResult   := mrCancel;
        Caption        := IntToStr(FStartVal);
        if Assigned(FOnTimeout) then
            FOnTimeout(Self);
    end
    else begin
        FCaption := IntToStr(count);
    end;
    Invalidate;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWait.AppMessage(var Msg: TMsg; var Handled: Boolean);
begin
    if (Msg.Message = WM_LBUTTONDOWN)   or
{       (Msg.Message = WM_LBUTTONUP)     or }
       (Msg.Message = WM_RBUTTONDOWN)   or
{       (Msg.Message = WM_RBUTTONUP)     or }
       (Msg.Message = WM_LBUTTONDBLCLK) or
       (Msg.Message = WM_RBUTTONDBLCLK) or
       (Msg.Message = WM_KEYDOWN)       or
{       (Msg.Message = WM_KEYUP)         or }
       (Msg.Message = WM_SYSKEYDOWN)    {or
       (Msg.Message = WM_SYSKEYUP) }
    then begin
        MessageBeep(MB_OK);
        Handled := TRUE;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomWait.GetRunning : Boolean;
begin
    Result := FTimer.Enabled;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWait.StartModal;
var
    OldOnMessage : TMessageEvent;
begin
    OldOnMessage := Application.OnMessage;
    Application.OnMessage := AppMessage;
    FModalResult := mrNone;
    Start;
    while Running do begin
        if Assigned(FOnWaiting) then
            FOnWaiting(Self);
        Application.ProcessMessages;
    end;
    Application.OnMessage := OldOnMessage;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWait.Paint;
var
    Len   : Integer;
begin
    Len := (atoi(Caption) * (Width - 7)) div FStartVal;

    Canvas.Pen   := FPen;
    Canvas.Font  := FFont;
    Canvas.Brush := FBrush;
    Canvas.Brush.Color := clWhite;
    Canvas.Rectangle(0, 0, Width - 1, Height - 1);
    Canvas.Brush.Color := clHighlight;
    Canvas.Rectangle(3, 3, 3 + Len, Height - 4);
    Canvas.TextOut(4, Height div 2 - 8, FCaption);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.



