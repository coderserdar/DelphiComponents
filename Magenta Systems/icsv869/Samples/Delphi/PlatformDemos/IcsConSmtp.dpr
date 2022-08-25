{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
Author:       Franois PIETTE
Object:       This demo shows how to use TSmtpCli component within a console
              mode application. It connect to your ISP server and mail a simple
              message. See the "const" section below to change the SMTP setting
              according to your situation. The mail message is also sent to
              me (CCi).
Creation:     Dec 24, 2001
Version:      1.04
EMail:        http://www.overbyte.be        http://www.rtfm.be/fpiette
              francois.piette@overbyte.be   francois.piette@rtfm.be
                                            francois.piette@pophost.eunet.be
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 2001-2012 by Franois PIETTE
              Rue de Grady 24, 4053 Embourg, Belgium.
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
Nov 11, 2002 V1.01 Changed MsgLine argument of SmtpGetData from PChar to
                   pointer to reflect component change.
Apr 20, 2003 V1.02 Corrected comment about SignOn: It's a domain name.
May 27, 2009 V1.03 A. Garrels Delphi 2009 support.
Oct 24, 2011 V1.04 A. Garrels OS X support.


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
program IcsConSmtp;

{$APPTYPE CONSOLE}

{$IF CompilerVersion < 23}
  {$MESSAGE FATAL 'This project requires Delphi or RAD Studio XE2 or better'};
{$IFEND}
{$IFNDEF NOFORMS}
  {$MESSAGE FATAL 'Please add "NOFORMS" to project option''s defines'};
{$ENDIF}

uses
  System.SysUtils,
  System.Classes,
  OverbyteIcsUtils,
  Ics.Fmx.OverbyteIcsSmtpProt;

const
  ConSmtpVersion = 104;
  CopyRight      = ' ConSmtp (c) 2001-2011 by Francois PIETTE. V1.04';

{ You *MUST* change the settings below so that your ISP mail server is      }
{ addressed. If you don't change those settings, you'll use my ISP SMTP     }
{ server which doesn't allow relaying. So you'll be rejected !              }
const
    { This is your ISP SMPT server hostname }
    YourSmtpServer   = 'localhost';
    { User/account and password }
    YourUsername     = 'test';
    YourPassword     = 'test';
    { This is your sign on. Usually just your domain name. }
    { If empty the host name is used.                      }
    WhoYouAre        = '';
    { And here your own email address }
    YourEmailAddress = 'your.name@domain.com';
    { And finally the destination email address }
    DestinationEmail = 'someone@domain.com';

type
    { We use TConApplication class (actually a component) to encapsulate all }
    { the work to be done. This is easier because TSmtpCli is event driven   }
    { and need methods (that is procedure of object) to handle events.       }
    TConApplication = class(TComponent)
    protected
        FSmtpCli     : TSmtpCli;
        FMessageBody : TStringList;
        procedure SmtpRequestDone(Sender    : TObject;
                                  RqType    : TSmtpRequest;
                                  ErrorCode : Word);
        procedure SmtpGetData(Sender   : TObject;
                              LineNum  : Integer;
                              MsgLine  : Pointer;
                              MaxLen   : Integer;
                              var More : Boolean);
        procedure SmtpResponse(Sender  : TObject;
                               Msg     : String);
    public
        constructor Create(AOwner: TComponent); override;
        destructor  Destroy; override;
        procedure   Execute;
    end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TConApplication.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);
    FSmtpCli     := TSmtpCli.Create(Self);
    FMessageBody := TStringList.Create;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TConApplication.Destroy;
begin
    if Assigned(FSmtpCli) then begin
        FSmtpCli.Destroy;
        FSmtpCli := nil;
    end;
    if Assigned(FMessageBody) then begin
        FMessageBody.Destroy;
        FMessageBody := nil;
    end;
    inherited Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TConApplication.Execute;
begin
    { Prepare connection to SMTP server }
    FSmtpCli.Host           := YourSmtpServer;
    FSmtpCli.Port           := 'smtp';
    FSmtpCli.SignOn         := WhoYouAre;
    { Detect most secure authentication type }
    FSmtpCli.AuthType       := smtpAuthAutoSelect;
    FSmtpCli.Username       := YourUsername;
    FSmtpCli.Password       := YourPassword;
    { Prepare message addressing (two recipients, one is CCi) }
    FSmtpCli.FromName       := YourEmailAddress;
    FSmtpCli.RcptName.Clear;
    FSmtpCli.RcptName.Add(DestinationEmail);
    FSmtpCli.HdrSubject     := 'ICS console mode SMTP demo is working';
    FSmtpCli.HdrTo          := FSmtpCli.RcptName.Strings[0];
    FSmtpCli.HdrFrom        := FSmtpCli.FromName;

    { Construct the message body }
    FMessageBody.Clear;
    FMessageBody.Add('Hello !');
    FMessageBody.Add('');
    FMessageBody.Add('This message has been sent by ConSmtp demo.');
    FMessageBody.Add(Trim(CopyRight));
    FMessageBody.Add('');
    FMessageBody.Add('--');
    FMessageBody.Add(FSmtpCli.SignOn);
    FMessageBody.Add(FSmtpCli.FromName);

    { Prepare component event handlers }
    FSmtpCli.OnRequestDone := SmtpRequestDone;
    FSmtpCli.OnResponse    := SmtpResponse;
    FSmtpCli.OnGetData     := SmtpGetData;

    WriteLn('> CONNECT ' + FSmtpCli.Host + '/' + FSmtpCli.Port);
    FSmtpCli.Connect;

    { We need a message loop in order for windows message processing to work. }
    { There is a message loop built into each TWSocket, so we use the one in  }
    { TSmtpCli control socket.                                                }
    { MessageLoop will exit only when WM_QUIT message is posted. We do that   }
    { form the OnRequestDone event handler when the component has finished.   }
    FSmtpCli.MessageLoop;
    WriteLn('Going back to the OS');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This event handler is called by TSmtpCli to get the next line of message  }
{ body. It is called repeatedly to get all lines, until "More" argument     }
{ is set to false. As we stored the message in a TStringList, we just have  }
{ to read from it. Easy. We could also easily read from a file.             }
procedure TConApplication.SmtpGetData(
    Sender   : TObject;
    LineNum  : Integer;   { The line number, start from 0                   }
    MsgLine  : Pointer;   { Where to store each line (pointer to buffer)    }
    MaxLen   : Integer;   { SizeOf line buffer within smtp component        }
    var More : Boolean);  { Return value to signal end of message body      }
begin
    if LineNum > FMessageBody.count then
        More := FALSE
    else
        IcsStrPLCopy(PAnsiChar(MsgLine),
                  AnsiString(FMessageBody.Strings[LineNum - 1]), MaxLen - 1);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This event handler is called each time the smtp component receive an      }
{ message from smtp server.                                                 }
procedure TConApplication.SmtpResponse(Sender: TObject; Msg: String);
begin
    WriteLn('< ' + Msg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This event handler is called each time smtp component has done a request  }
{ We use it to start the next request because sending a smtp message        }
{ require a few operations: connecting to server, sending helo message,     }
{ sending originator, sending recipients, sending message body and finally  }
{ disconnecting from server. There are of course optional request such as   }
{ authentication.                                                           }
procedure TConApplication.SmtpRequestDone(
    Sender    : TObject;
    RqType    : TSmtpRequest;
    ErrorCode : Word);
var
    EndFlag : Boolean;
begin
    EndFlag := FALSE;
    case RqType of
        smtpConnect:
            begin
                if ErrorCode = 0 then begin
                    WriteLn('> EHLO');
                    FSmtpCli.Ehlo;
                end;
            end;
        smtpEhlo:
            begin
                if ErrorCode = 0 then begin
                    WriteLn('> Auth');
                    FSmtpCli.Auth;
                end;
            end;
        smtpAuth:
            begin
                if ErrorCode = 0 then begin
                    WriteLn('> MAILFROM');
                    FSmtpCli.MailFrom;
                end;
            end;
        smtpMailFrom:
            begin
                if ErrorCode = 0 then begin
                    WriteLn('> RCPTTO');
                    FSmtpCli.RcptTo;
                end;
            end;
        smtpRcptTo:
            begin
                if ErrorCode = 0 then begin
                    WriteLn('> DATA');
                    FSmtpCli.Data;
                end;
            end;
        smtpData:
            begin
                if ErrorCode = 0 then begin
                    WriteLn('> QUIT');
                    FSmtpCli.Quit;
                end;
            end;
        smtpQuit:
            begin
                WriteLn('Quit done');
                EndFlag := TRUE;
            end;
        else
            begin
                WriteLn('Unknown SmtpRequest ' + IntToStr(Ord(RqType)));
                EndFlag := TRUE;
            end;
    end;

    { Check status }
    if ErrorCode <> 0 then begin
        WriteLn('Failed, error #' + IntToStr(ErrorCode));
        EndFlag := TRUE;
    end;

    { If something wrong or end of job, then go back to the OS }
    if EndFlag then begin
        { Prompt the user }
        WriteLn('Hit ENTER key to return quit program...');
        ReadLn;
        { Break message loop we called from the execute method }
        FSmtpCli.PostQuitMessage;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
var
    ConApp : TConApplication;
begin
    WriteLn(CopyRight);
    WriteLn;
    ConApp := TConApplication.Create(nil);
    try
        ConApp.Execute;
    finally
        ConApp.Destroy;
    end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
end.
