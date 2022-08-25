{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Description:  ConCli shows how to use TWSocket in a console mode application
              (not for Delphi 1 which doesn't support console mode)
Creation:     Nov 20, 1997
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
Dec 05, 1998 V1.01 Don't use TWait control anymore
Aug 31, 2003 V1.02 Completely rewritten sample.


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$APPTYPE CONSOLE}
program OverbyteIcsConCli1;

{$I OVERBYTEICSDEFS.INC}
{$IFDEF VER80}
    Bomb('Sorry, Delphi 1 does not support console mode programs');
{$ENDIF}

uses
    SysUtils,
    Forms, 
    OverbyteIcsIniFiles in '..\..\OverbyteIcsIniFiles.pas',
    OverbyteIcsWSocket;

const
    ConCli1Version     = 102;
    CopyRight : String = 'ConCli1 (c) 1997-2010 Francois Piette  V1.02 ';

type
    { TWSocket is event driven. Since all event handlers have to be a       }
    { "procedure of object", we need to define an object to be able to      }
    { definie event handlers. Anyway, it is handy to add our data.          }
    { We define our own object because Delphi runtime doesn't allow to      }
    { derive from or redefine TApplication object.                          }
    TConCliApp = class
    public
        WSocket1 : TWSocket;
        Buffer   : String;
        DoneFlag : Boolean;
        procedure WSocketDataAvailable(Sender : TObject; Error : Word);
        procedure WSocketSessionConnected(Sender : TObject; Error : Word);
        procedure WSocketSessionClosed(Sender : TObject; Error : Word);
        procedure Run;
    end;

var
    ConApp : TConCliApp;



{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ The Run procedure is where the application ocde is.                       }
procedure TConCliApp.Run;
begin
    { TWSocket is event driven. We will have a message loop to trigger all  }
    { event. We have to stop the loop when something happend. The DoneFlag  }
    { is used for that purpose.                                             }
    DoneFlag                 := FALSE;
    { Create TWSocket used by this application.                             }
    WSocket1                 := TWsocket.Create(nil);
    { Initialize paramters for the connection                               }
    WSocket1.Proto           := 'tcp';
    WSocket1.Addr            := 'localhost';
    WSocket1.Port            := 'telnet';
    { Initialize all required event handlers                                }
    WSocket1.OnSessionConnected := WSocketSessionConnected;
    WSocket1.OnSessionClosed    := WSocketSessionClosed;
    WSocket1.OnDataAvailable    := WSocketDataAvailable;
    { Request connection. This is non blocking, event driven.               }
    { Connect will retrun immediately while connection take place in the    }
    { background without blocking the program. When the connection is       }
    { established, we will get the OnSessionConnected event. When it is     }
    { we will get the OnSessionClosed event. When we receive data, the      }
    { OnDataAvailable event is triggered.                                   }
    try
        WriteLn('Connecting to ' + WSocket1.Addr + '/' + WSocket1.Port);
        WSocket1.Connect;
        { Now we enter the message loop. Everything else until the          }
        { connection is closed occur in the event handlers.                 }
{$IFDEF NOFORMS}
        while not DoneFlag do
            WSocket1.ProcessMessages;
{$ELSE}
        while not DoneFlag do begin
            Application.ProcessMessages;
            if Application.Terminated then
                break;
        end;
{$ENDIF}
        { Here the application is almost done. The session is closed.       }
        WriteLn('Done.');
    except
        on E:Exception do
            WriteLn('An error occured. ', E.ClassName + ': ' + E.Message);
    end;

    WSocket1.Destroy;
    Writeln('Hit enter...');
    Readln;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This event handler is called by the TWSocket when some data has been      }
{ received by the lower level.                                              }
procedure TConCliApp.WSocketDataAvailable(Sender : TObject; Error : Word);
begin
    { Set the flag which will end the message loop }
    DoneFlag := TRUE;
    { Receive data from server }
    Buffer   := WSocket1.ReceiveStr;
    Writeln('Server banner is: ' + Buffer);
    { This very simple application receive only one line... }
    { So we just close the connection gracefuly.            }
    WSocket1.Close;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This event handler is called by TWSocket when the connection is ready     }
procedure TConCliApp.WSocketSessionConnected(Sender : TObject; Error : Word);
begin
    if Error = 0 then
        WriteLn('Connected.')
    else begin
        WriteLn('Unable to connect. Error #' + IntToStr(Error));
        DoneFlag := TRUE;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This event handler is called by TWSocket when the connection is closed    }
procedure TConCliApp.WSocketSessionClosed(Sender : TObject; Error : Word);
begin
    { Set the flag which will end the message loop }
    DoneFlag := TRUE;
    if Error = 0 then
        WriteLn('Session closed')
    else
        WriteLn('Session closed with error #' + IntToStr(Error))
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This is the main entry point for the application                          }
begin
    WriteLn(CopyRight);
    ConApp := TConCliApp.Create;  { Create our application object           }
    try                           { Trap any exception                      }
        ConApp.Run;               { Run the application code                }
    finally                       { Be sure to cleanup even with excepions  }
        ConApp.Destroy;           { Destroy the application object          }
    end;
end.
