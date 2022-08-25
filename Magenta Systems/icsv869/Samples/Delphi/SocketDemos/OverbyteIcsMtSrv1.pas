{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Description:  ****          MtSrv is an old sample program         ****
              **** It works OK, but you should use ThrdSrv project ****
              This little application shows how to use the TWSocket in a
              multithreaded application. It is a very basic telnet server.
              Each time a client connect to the server, he receive an "hello"
              message. Then every character sent is echoed back to the client.
              There are two units is this application: one for the main
              server code, and one for the client thread.
              Each time a client connect to the server, a new TWSocket is
              created and a new thread is launched to handle the client
              work. When the client disconnect, the TWSocket and the thread
              are destroyed.
              To see this demo working on your computer, start the demo then
              start several copies of the TELNET client program (the one which
              comes with Windows 95 is perfect). Then using each telnet, connect
              to 127.0.0.1. You'll see a new connection in the list box. You
              must receive the "hello" message and see each character as you
              type them. You can use the Disconnect button from the application
              or from the telnet client to see what happends (the connection
              should be closed).
Note:         I made this multithreaded application because so many people
              asked for a multithreaded sample program. Most people think they
              must go multithread in order to simultaneously server several
              clients. This is completely WRONG, specially with TWSocket which
              is a ASYNCHONOUS (non-blocking) component. You can serve as many
              simultaneous clients as you like without using threads ! See how
              TcpSrv or ConSrv demo program does it without using threads.
              Multithreaded programs are actually SLOWER and much more difficult
              to develop than event driven, asynchonous programs such as TnSrv
              and ConSrv demos. You need thread only if your server must do
              some blocking and lengthy tasks, such as querying a database. But
              threads are only needed for processing, not for telecommunication
              using TWSocket. TWSocket does send and receive in the background,
              even with no thread.
Creation:     September 21, 1997
Version:      1.02
EMail:        francois.piette@overbyte.be  http://www.overbyte.be
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 1997-2010 by François PIETTE
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

Updates:
Nov 18, 1997  V1.01 Corrected isxdigit (By Paul Taylor <paul@star.net.au>)
Apr 26, 1998  V1.02 MultiThreaded property correctly setup to TRUE.

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsMtSrv1;

interface

{$IFDEF VER80}
  'This sample program use threads and hence is not compatible with Delphi 1';
{$ENDIF}
{$J+}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, OverbyteIcsMtSrv2, OverbyteIcsWndControl, OverbyteIcsWSocket;

type
  TServerForm = class(TForm)
    ServerWSocket: TWSocket;
    DisconnectButton: TButton;
    QuitButton: TButton;
    ClientListBox: TListBox;
    DisconnectAllButton: TButton;
    Label1: TLabel;
    procedure ServerWSocketSessionAvailable(Sender: TObject; Error: Word);
    procedure FormShow(Sender: TObject);
    procedure DisconnectButtonClick(Sender: TObject);
    procedure QuitButtonClick(Sender: TObject);
    procedure DisconnectAllButtonClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    procedure ClientThreadTerminate(Sender: TObject);
    procedure DisconnectAll;
  end;

var
  ServerForm: TServerForm;

implementation

{$R *.DFM}

uses
    OverbyteIcsUtils;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This event is generated when a client is connecting                       }
procedure TServerForm.ServerWSocketSessionAvailable(Sender: TObject;
  Error: Word);
var
    ClientThread  : TClientThread;
begin
    { Create a new thread to handle client request                          }
    ClientThread             := TClientThread.Create(ServerWSocket.Accept);

    { Assign the thread's OnTerminate event                                 }
    ClientThread.OnTerminate := ClientThreadTerminate;

    { Add the thread to the listbox which is our client list                }
    ClientListBox.Items.Add(IntToHex(Integer(ClientThread), 8));

    { Then start the client thread work                                     }
    { because it was created in the blocked state                           }
{$if RTLVersion >= 21}
    ClientThread.Start;
{$else}
    ClientThread.Resume;
{$ifend}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TServerForm.FormShow(Sender: TObject);
const
    FirstTime : Boolean = TRUE;
begin
    if FirstTime then begin
        FirstTime           := FALSE;
        ServerWSocket.Proto := 'tcp';      { We use a TCP connection        }
        ServerWSocket.Port  := 'telnet';   { We wants to use telnet         }
        ServerWSocket.Addr  := '0.0.0.0';  { We accept any client           }
        ServerWSocket.Listen;              { Start server accepting         }
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This event is generated when the user clicks on the 'Disconnect' button   }
{ when he wants to disconnect the selected client in the listbox.           }
procedure TServerForm.DisconnectButtonClick(Sender: TObject);
var
    ClientThread  : TClientThread;
    Buf           : String;
begin
    { No selected item, nothing to do                                       }
    if ClientListBox.ItemIndex < 0 then
        Exit;

    { Extract the ClientThread pointer from the list box                    }
    Buf := ClientListBox.Items[ClientListBox.ItemIndex];
    ClientThread := TClientThread(htoin(PChar(Buf), Length(Buf)));

    { Call ClientThread.Release which will stop the thread                  }
    { In consequence, the OnTerminate event will be generated               }
    ClientThread.Release;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This event handler is called when one of the client thread terminate      }
{ We will find this thread in our listbox, remove it and destroy the        }
{ TWSocket object use by the corresponding client.                          }
procedure TServerForm.ClientThreadTerminate(Sender: TObject);
var
    ClientThread  : TClientThread;
    Buf           : String;
    Index         : Integer;
begin
    { A thread has been terminated, remove it from our list and destroy     }
    { the ClientWSocket we passed to the thread.                            }
    for Index := 0 to ClientListBox.Items.Count - 1 do begin
        Buf := ClientListBox.Items[Index];
        ClientThread := TClientThread(htoin(PChar(Buf), Length(Buf)));
        if ClientThread = TClientThread(Sender) then begin
            { Remove the client from our listbox                            }
            ClientListBox.Items.Delete(Index);
            Break;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This procedure scan the listbox and halt every ClientThread               }
procedure TServerForm.DisconnectAll;
var
    ClientThread  : TClientThread;
    Buf           : String;
begin
    while ClientListBox.Items.Count > 0 do begin
        Buf          := ClientListBox.Items[0];
        ClientThread := TClientThread(htoin(PChar(Buf), Length(Buf)));
        ClientThread.Release;
        Application.ProcessMessages;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TServerForm.QuitButtonClick(Sender: TObject);
begin
    Close;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TServerForm.DisconnectAllButtonClick(Sender: TObject);
begin
    DisconnectAll;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TServerForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
    DisconnectAll;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.

