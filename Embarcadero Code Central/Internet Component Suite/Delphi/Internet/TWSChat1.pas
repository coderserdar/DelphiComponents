{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Description:  TWSChat shows how to use TWSocket to build a chat program
Creation:     November 26, 1997
Version:      1.03
EMail:        francois.piette@pophost.eunet.be   
              francois.piette@rtfm.be             http://www.rtfm.be/fpiette
Support:      Use the mailing list twsocket@rtfm.be See website for details.
Legal issues: Copyright (C) 1997, 1998 by François PIETTE
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

Updates:
Jan 04, 1998  V1.01 Corrected a bug in the CliWSocketDataAvailable which did'nt
              append data to the receive buffer.
Jan 10, 1998  V1.02 Corrected yet another bug in CliWSocketDataAvailable which
              did'nt take into account that variable I is zero based.
Mar 15, 1998  V1.03 Yet another bug in OnDataAvailable event.

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit TWSChat1;

interface

uses
  WinTypes, WinProcs, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, WinSock, WSocket;

const
  TWSChatVersion = 103;
  ChatPort       = '2200';             { Any port would do the job... }

type
  TTWSChatForm = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    DisplayMemo: TMemo;
    SrvWSocket: TWSocket;
    ServerEdit: TEdit;
    Label1: TLabel;
    ConnectButton: TButton;
    CliWSocket: TWSocket;
    DisconnectButton: TButton;
    MessageEdit: TEdit;
    SendButton: TButton;
    TmpWSocket: TWSocket;
    RunningRadioButton: TRadioButton;
    StoppedRadioButton: TRadioButton;
    procedure FormShow(Sender: TObject);
    procedure ConnectButtonClick(Sender: TObject);
    procedure CliWSocketDnsLookupDone(Sender: TObject; Error: Word);
    procedure CliWSocketSessionConnected(Sender: TObject; Error: Word);
    procedure DisconnectButtonClick(Sender: TObject);
    procedure SrvWSocketSessionAvailable(Sender: TObject; Error: Word);
    procedure CliWSocketSessionClosed(Sender: TObject; Error: Word);
    procedure CliWSocketDataAvailable(Sender: TObject; Error: Word);
    procedure SendButtonClick(Sender: TObject);
    procedure StoppedRadioButtonClick(Sender: TObject);
    procedure RunningRadioButtonClick(Sender: TObject);
  private
    { Déclarations privées }
    Initialized : Boolean;
    RcvBuf : array [0..1023] of char;
    RcvLen : integer;
    procedure StartServer;
  public
    { Déclarations publiques }
  end;

var
  TWSChatForm: TTWSChatForm;

implementation

{$R *.DFM}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTWSChatForm.StartServer;
begin
    { Try to be a server }
    SrvWSocket.Port  := ChatPort;
    SrvWSocket.Proto := 'tcp';
    SrvWSocket.Addr  := '0.0.0.0';
    try
        SrvWSocket.Listen;
        RunningRadioButton.Checked := TRUE;
        StoppedRadioButton.Checked := FALSE;
    except
        on E:ESocketException do begin
            { The socket is probably already in use }
            RunningRadioButton.Checked := FALSE;
            StoppedRadioButton.Checked := TRUE;
            if Copy(E.Message, 1, 11) = 'Error 10048' then
                DisplayMemo.Lines.Add('TWSChat already running as server')
            else
                raise;
        end
        else
            raise;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTWSChatForm.FormShow(Sender: TObject);
begin
    if not Initialized then begin
        Initialized := TRUE;
        StartServer;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ The user has clicked on the 'connect' button. We will not connect here,   }
{ but start the DNSLookup. We will receive a event when it is complete.     }
{ The connection will be made at that later time.                           }
procedure TTWSChatForm.ConnectButtonClick(Sender: TObject);
begin
    ConnectButton.Enabled    := FALSE;
    DisconnectButton.Enabled := TRUE;
    CliWSocket.DnsLookup(ServerEdit.Text);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This event handler is triggered when the DNS lookup process is finished   }
{ successfully or not. If DNS lookud failed, display a message.             }
{ If DNS lookup successfull, ask TWSocket to connect the server.            }
procedure TTWSChatForm.CliWSocketDnsLookupDone(Sender: TObject; Error: Word);
begin
    if Error <> 0 then begin
        { DNS Lookup has failed }
        DisplayMemo.Lines.Add('Server name unknown');
        ConnectButton.Enabled    := TRUE;
        DisconnectButton.Enabled := FALSE;
        Exit;
    end;

    { DNS lookup successfull. Try to see if we are the server and we are }
    { trying to connect to ourself. Check loopback address, should also  }
    { check the local IP address (returned by LocalIPList)...            }
    if (SrvWSocket.State = wsListening) and
       (CliWSocket.DnsResult = '127.0.0.1') then begin
        DisplayMemo.Lines.Add('Your are trying to connect to yourself !');
        ConnectButton.Enabled    := TRUE;
        DisconnectButton.Enabled := FALSE;
        Exit;
    end;

    { Transfert the IP address from DNSLookup to the TWSocket for connection }
    { We could use the hostname for the Addr property, TWSocket will do the  }
    { DNS lookup for us, but it will block, maybe for a long time if DNS if  }
    { down.                                                                  }
    CliWSocket.Addr  := CliWSocket.DnsResult;
    CliWSocket.Port  := ChatPort;
    CliWSocket.Proto := 'tcp';

    { The connect method is asynchronous. You get the control back quickly }
    { The OnSessionConnected event will be eventually generated when the   }
    { connection is established.                                           }
    CliWSocket.Connect;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This event handler is triggered when the connection is established with   }
{ the server. Enable the send button and the message edit box.              }
procedure TTWSChatForm.CliWSocketSessionConnected(Sender: TObject; Error: Word);
begin
    if Error = WSAECONNREFUSED then
        DisplayMemo.Lines.Add('No server available')
    else if Error <> 0 then
        DisplayMemo.Lines.Add('Can''t connect, error #' + IntToStr(Error))
    else begin
        DisplayMemo.Lines.Add('Connected');
        SendButton.Enabled  := TRUE;
        MessageEdit.Enabled := TRUE;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This event is triggered when the client connection is closed, either      }
{ by the client himself or by the local user pushing the disconnect button  }
procedure TTWSChatForm.CliWSocketSessionClosed(Sender: TObject; Error: Word);
begin
    DisconnectButton.Enabled := FALSE;
    ConnectButton.Enabled    := TRUE;
    if SendButton.Enabled then begin
        SendButton.Enabled   := FALSE;
        MessageEdit.Enabled  := FALSE;
        DisplayMemo.Lines.Add('Disconnected');
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This event is triggered when data has been received from the client.      }
{ A little bit of work here because the data can comes fragmented or in big }
{ chunks with several client lines. So we assemble the data received in a   }
{ buffer and check the buffer for complete lines (there can be no complete  }
{ line, exactly one complete line, several complete lines and may be an     }
{ incomplete line at the end.                                               }
procedure TTWSChatForm.CliWSocketDataAvailable(Sender: TObject; Error: Word);
var
    Len : Integer;
    I   : Integer;
begin
    { Receive the data that has arrived, put it after the data already here }
    Len := CliWSocket.Receive(@RcvBuf[RcvLen], SizeOf(RcvBuf) - RcvLen - 1);
    if Len <= 0 then
        Exit;
    { Update our conter }
    RcvLen := RcvLen + Len;
    { Place a null byte at the end of the buffer }
    RcvBuf[RcvLen] := #0;

    { Scan the buffer to process each complete line }
    while TRUE do begin
        { find the terminating line feed }
        I := StrScan(@RcvBuf, #10) - RcvBuf;
        if I < 0 then
            break; { not found, incomplete line, break loop }
        { Replace the line feed by a nul char, truncating the line }
        RcvBuf[I] := #0;
        { Display the truncated line }
        DisplayMemo.Lines.Add('Remote> ' + StrPas(RcvBuf));
        { Restore the line feed }
        RcvBuf[I] := #10;
        { Was it the last line in the buffer ? }
        if I >= (RcvLen - 1) then begin
            RcvLen := 0;
            break;
        end;
        { Not the last line, move the next one in front of buffer }
        Move(RcvBuf[I + 1], RcvBuf, RcvLen - I);
        RcvLen := RcvLen - I - 1;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This event is triggered when we - as a server - have received a client    }
{ connection request. We must accept the connection. Two cases: we are      }
{ already busy with another client, or this is the first client connecting. }
procedure TTWSChatForm.SrvWSocketSessionAvailable(Sender: TObject; Error: Word);
begin
    if CliWSocket.State = wsConnected then begin
        { We are already busy with a client. Use the TmpWSocket to send a }
        { busy message to the second client. Display a message to notify  }
        { the user that someone is trying to contact him.                 }
        TmpWSocket.HSocket := SrvWSocket.Accept;
        DisplayMemo.Lines.Add('System> ' + TmpWSocket.GetPeerAddr +
                              ' is trying to call you');
        TmpWSocket.SendStr('Busy ! Try later...' + #13#10);
        TmpWSocket.Close;
        Exit;
    end;

    { This is our first client trying to connect, we accept }
    CliWSocket.HSocket       := SrvWSocket.Accept;
    ConnectButton.Enabled    := FALSE;
    DisconnectButton.Enabled := TRUE;
    SendButton.Enabled       := TRUE;
    MessageEdit.Enabled      := TRUE;
    DisplayMemo.Lines.Add('Connected with ' + CliWSocket.GetPeerAddr);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ The user clicked on the disconnect button.                                }
procedure TTWSChatForm.DisconnectButtonClick(Sender: TObject);
begin
    CliWSocket.Close;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ The user has clicked on the send button. Just send the data in the edit   }
{ box and a CRLF pair to make a complete line.                              }
procedure TTWSChatForm.SendButtonClick(Sender: TObject);
begin
    CliWSocket.SendStr(MessageEdit.Text + #13#10);
    DisplayMemo.Lines.Add(' Local> ' + MessageEdit.Text);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTWSChatForm.StoppedRadioButtonClick(Sender: TObject);
begin
    SrvWSocket.Close;
    RunningRadioButton.Checked := FALSE;
    StoppedRadioButton.Checked := TRUE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTWSChatForm.RunningRadioButtonClick(Sender: TObject);
begin
    if SrvWSocket.State <> wsListening then
        StartServer;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.

