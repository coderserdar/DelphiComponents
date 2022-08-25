{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Creation:     July 22, 2007
Description:  Very simple demo for TIcsTimer
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
unit OverbyteIcsTimerDemo1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
{$IFDEF CLR}
  System.ComponentModel,
  Borland.Vcl.StdCtrls,
{$ENDIF}
  OverbyteIcsWndControl,
  OverbyteIcsWSocket, StdCtrls;

type
  TIcsTimerDemoForm = class(TForm)
    StartButton: TButton;
    DisplayMemo: TMemo;
    StopButton: TButton;
    procedure StartButtonClick(Sender: TObject);
    procedure StopButtonClick(Sender: TObject);
  private
    FWSocket : TWSocket;
    FTImer   : TIcsTimer;
    FCount   : Integer;
    procedure TimerEvent(Sender : TObject);
  end;

var
  IcsTimerDemoForm: TIcsTimerDemoForm;

implementation

{$R *.dfm}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsTimerDemoForm.StartButtonClick(Sender: TObject);
begin
    // A TIcsTimer /must/ have a TIcsWndControl component as owner because
    // it make use of his hidden window for WM_TIMER message processing.
    // A simple TWSocket will do, as well as any other TIcsWndControl derived.
    if not Assigned(FWSocket) then
        FWSocket    := TWSocket.Create(Self);
    // We create the time object dynamically
    if not Assigned(FTimer) then begin
        FTimer          := TIcsTimer.Create(FWSocket);
        FTimer.Interval := 1000;
        FTimer.OnTimer  := TimerEvent;
    end;
    // Enable the timer
    FTimer.Enabled  := TRUE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsTimerDemoForm.StopButtonClick(Sender: TObject);
begin
    FTimer.Enabled  := FALSE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsTimerDemoForm.TimerEvent(Sender : TObject);
begin
    Inc(FCount);
    DisplayMemo.Lines.Add(IntToStr(FCount));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.
