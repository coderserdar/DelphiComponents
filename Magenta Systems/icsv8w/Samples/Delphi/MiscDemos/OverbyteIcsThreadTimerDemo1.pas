{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Creation:     July 24, 2009
Description:  Demo for TIcsThreadTimer
Version:      1.00
EMail:        francois.piette@overbyte.be   http://www.overbyte.be
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 2002-2010 by François PIETTE
              Rue de Grady 24, 4053 Embourg, Belgium. Fax: +32-4-365.74.56
              <francois.piette@overbyte.be>

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


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsThreadTimerDemo1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  StdCtrls,
  OverbyteIcsWndControl,
  OverbyteIcsWSocket,
  OverbyteIcsThreadTimer;

const
  WM_RESTART   = WM_USER + 1;

type
  TIcsTimerDemoForm = class(TForm)
    StartButton: TButton;
    DisplayMemo: TMemo;
    StopButton: TButton;
    TimerCountEdit: TEdit;
    Label1: TLabel;
    FreeAllButton: TButton;
    SleepButton: TButton;
    GroupBox1: TGroupBox;
    SetGlobalsButton: TButton;
    TimersPerClockEdit: TEdit;
    Label2: TLabel;
    MinResolutionEdit: TEdit;
    Label3: TLabel;
    LoopCheckBox: TCheckBox;
    LoopIntervalEdit: TEdit;
    procedure StartButtonClick(Sender: TObject);
    procedure StopButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FreeAllButtonClick(Sender: TObject);
    procedure SetGlobalsButtonClick(Sender: TObject);
    procedure SleepButtonClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FWSocket      : TWSocket;
    FTimerList    : TList;
    FCount        : Integer;
    FLoopInterval : Integer;
    procedure TimerEvent(Sender : TObject);
  protected
    procedure WMRestart(var Msg: TMessage); message WM_RESTART;
  end;

var
  IcsTimerDemoForm: TIcsTimerDemoForm;

implementation

{$R *.dfm}

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsTimerDemoForm.FormCreate(Sender: TObject);
begin
    { It's possible to fine tune timer behaviour by two global vars as long as }
    { no instance of  TIcsThreadTimer is allocated.                            }
    { GMaxIcsTimerPerThread  // Maximum timers per TIcsClock instance          }
    { GMinIcsTimerResolution // Ticks / Msec interval of TIcsClock             }

    TimersPerClockEdit.Text := IntToStr(GMaxIcsTimerPerThread);
    MinResolutionEdit.Text  := IntToStr(GMinIcsTimerResolution);

    Randomize;
    FTimerList := TList.Create;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsTimerDemoForm.FormDestroy(Sender: TObject);
begin
    FTimerList.Free;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsTimerDemoForm.StartButtonClick(Sender: TObject);
var
    I : Integer;
    ATimer : TIcsThreadTimer;
    TimerCount : Integer;
begin
    FCount := 0;
    DisplayMemo.Clear;
    DisplayMemo.Update;
    FLoopInterval := StrToInt(LoopIntervalEdit.Text);
    // A TIcsThreadTimer /must/ have a TIcsWndControl component as owner because
    // it make use of his hidden window for WM_TIMER message processing.
    // A simple TWSocket will do, as well as any other TIcsWndControl derived.
    if not Assigned(FWSocket) then
        FWSocket := TWSocket.Create(Self);
    // We create the time objects dynamically
    TimerCount := StrToInt(TimerCountEdit.Text);
    while FTimerList.Count > TimerCount do
    begin
        TObject(FTimerList.Last).Free;
        FTimerList.Delete(FTimerList.Count -1);
    end;
    while FTimerList.Count < TimerCount do
    begin
        ATimer := TIcsThreadTimer.Create(FWSocket);
        FTimerList.Add(ATimer);
        ATimer.Interval := Random(2000);
        ATimer.Enabled  := FALSE;
        ATimer.OnTimer  := TimerEvent;
    end;
    for I := 0 to FTimerList.Count -1 do
        if not TIcsThreadTimer(FTimerList[I]).Enabled then
            TIcsThreadTimer(FTimerList[I]).Enabled  := TRUE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsTimerDemoForm.StopButtonClick(Sender: TObject);
var
    I : Integer;
begin
    for I := 0 to FTimerList.Count -1 do
        if TIcsThreadTimer(FTimerList[I]).Enabled then
            TIcsThreadTimer(FTimerList[I]).Enabled := FALSE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsTimerDemoForm.FreeAllButtonClick(Sender: TObject);
begin
    FreeAndNil(FWSocket);
    FTimerList.Clear;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This works only when no TIcsThreadTimer object is instanciated }
procedure TIcsTimerDemoForm.SetGlobalsButtonClick(Sender: TObject);
begin
    GMaxIcsTimerPerThread   := StrToInt(TimersPerClockEdit.Text);
    GMinIcsTimerResolution  := StrToInt(MinResolutionEdit.Text);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsTimerDemoForm.SleepButtonClick(Sender: TObject);
begin
    Sleep(5000);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsTimerDemoForm.TimerEvent(Sender : TObject);
begin
    Inc(FCount);
    if FCount mod FTimerList.Count = 0 then begin
        DisplayMemo.Lines.Add(IntToStr(FCount));
        if LoopCheckBox.Checked and
           (FCount >= (FLoopInterval * FTimerList.Count)) then
            PostMessage(Handle, WM_RESTART, 0, 0);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsTimerDemoForm.WMRestart(var Msg: TMessage);
begin
    FreeAllButtonClick(nil);
    StartButtonClick(nil);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
end.
