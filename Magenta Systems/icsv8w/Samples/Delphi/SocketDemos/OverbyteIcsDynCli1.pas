{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Description:  Sample program to show how to dynamically create a TWSocket.
Creation:     October 02, 1999
Version:      1.01
EMail:        francois.piette@overbyte.be  http://www.overbyte.be
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 1999-2010 by François PIETTE
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

History:


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsDynCli1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, OverbyteIcsWSocket;

const
    DynCliVersion  = 101;
    CopyRight      = ' DynCli (c) 1999-2010 by Francois PIETTE. V1.01';

type
  TDynCliForm = class(TForm)
    DisplayMemo: TMemo;
    Panel1: TPanel;
    Label1: TLabel;
    HostnameEdit: TEdit;
    PortEdit: TEdit;
    Label2: TLabel;
    ConnectButton: TButton;
    DisconnectButton: TButton;
    procedure ConnectButtonClick(Sender: TObject);
    procedure DisconnectButtonClick(Sender: TObject);
  private
    FWSocket : TWSocket;
    FRcvBuf  : String;
    procedure FWSocketSessionConnected(Sender: TObject; Error: Word);
    procedure FWSocketSessionClosed(Sender: TObject; Error: Word);
    procedure FWSocketDataAvailable(Sender: TObject; Error: Word);
  public
    { Déclarations publiques }
  end;

var
  DynCliForm: TDynCliForm;

implementation

{$R *.DFM}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF VER80}
procedure SetLength(var S: String; NewLength: Integer);
begin
    S[0] := Chr(NewLength);
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDynCliForm.ConnectButtonClick(Sender: TObject);
begin
    if Assigned(FWSocket) then
        FWSocket.Release;            { This will close connection as needed }
    { Create a new TWSocket and initialize all needed properties and events }
    FWSocket                    := TWSocket.Create(Self);
    FWSocket.Proto              := 'tcp';
    FWSocket.Port               := PortEdit.Text;
    FWSocket.Addr               := HostnameEdit.Text;
    FWSocket.LineMode           := TRUE;
    FWSocket.LineEnd            := #13#10;
    FWSocket.OnSessionConnected := FWSocketSessionConnected;
    FWSocket.OnSessionClosed    := FWSocketSessionClosed;
    FWSocket.OnDataAvailable    := FWSocketDataAvailable;
    try
        FWSocket.Connect;
    except
        { Connect may fail because of invalid parameters and will trigger }
        { an exception.                                                   }
        on E:Exception do begin
            DisplayMemo.Lines.Add('Unabled to connect: ' +
                                  E.ClassName + ': ' + E.Message);
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDynCliForm.DisconnectButtonClick(Sender: TObject);
begin
     if Assigned(FWSocket) then
         FWSocket.Close;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDynCliForm.FWSocketSessionConnected(Sender: TObject; Error: Word);
begin
    if Error <> 0 then
        DisplayMemo.Lines.Add('Can''t connect. Error #' + IntToStr(Error))
    else
        DisplayMemo.Lines.Add('Session connected.');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDynCliForm.FWSocketSessionClosed(Sender: TObject; Error: Word);
begin
    DisplayMemo.Lines.Add('Session closed.');
    { Destroy the socket. We can't use Destroy here because we are in       }
    { an event handler. We need to use Release which will delay destruction }
    { until we are out of the event handler.                                }
    FWSocket.Release;
    FWSocket := nil;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDynCliForm.FWSocketDataAvailable(Sender: TObject; Error: Word);
begin
    { Remember: we use line mode. We will always receive complete lines     }
    with Sender as TWSocket do
        FRcvBuf := ReceiveStr;
    { Remove trailing CR/LF, if any }
    if (Length(FRcvBuf) > 1) and
       (FRcvBuf[Length(FRcvBuf)] = #10) and
       (FRcvBuf[Length(FRcvBuf) - 1] = #13) then
          SetLength(FRcvBuf, Length(FRcvBuf) - 2);
    { Display received line }
    DisplayMemo.Lines.Add(FRcvBuf);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.
