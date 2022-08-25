{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *


Author:       François PIETTE
Object:       This demo shows how to use THttpCli component within a console
              mode application. It Connect to www.borland.com and display
              received (default) document on screen.
Creation:     Apr 20, 2002
Version:      6.01
EMail:        http://www.overbyte.be        francois.piette@overbyte.be
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

Updates:
Jul 12, 2008 V6.00 A. Garrels - Bumped version number to 6.00 and slightly 
             modified to work with ICS v6.
Jul 19, 2008 V6.00 F. Piette made small changes for Unicode
Oct 11, 2010 V6.01 A. Garrels fixed a Unicode bug.


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
program OverbyteIcsConHttp;

{$R '..\..\OverbyteIcsCommonVersion.res'  '..\..\OverbyteIcsCommonVersion.rc'}

{$I OVERBYTEICSDEFS.INC}
{$IFDEF VER80}
Bomb('Sorry, Delphi 1 does not support console mode programs');
{$ENDIF}
{$APPTYPE CONSOLE}
{$IFNDEF NOFORMS}
    Bomb('Please add NOFORMS to your project defines');
{$ENDIF}

uses
  Messages,
  Windows,
  SysUtils,
  Classes,
  OverbyteIcsHttpProt;

const
  ConHttpVersion = 601;
  CopyRight      = ' ConHttp (c) 2002-2010 by Francois PIETTE. V6.01';

type
    { We use TConApplication class (actually a component) to encapsulate all }
    { the work to be done. This is easier because THttpCli is event driven   }
    { and need methods (that is procedure of object) to handle events.       }
    TConApplication = class(TComponent)
    protected
        FHttpCli : THttpCli;
        procedure HttpRequestDone(Sender    : TObject;
                                  RqType    : THttpRequest;
                                  ErrorCode : Word);
        procedure HttpDocData(Sender : TObject;
                              Buffer : Pointer;
                              Len    : Integer);
    public
        constructor Create(AOwner: TComponent); override;
        destructor  Destroy; override;
        procedure   Execute;
    end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TConApplication.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);
    FHttpCli := THttpCli.Create(Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TConApplication.Destroy;
begin
    if Assigned(FHttpCli) then begin
        FHttpCli.Destroy;
        FHttpCli := nil;
    end;
    inherited Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TConApplication.Execute;
var
    Url : String;
begin
    Url := 'http://www.borland.com';
    WriteLn('Querying ' + Url);
    FHttpCli.URL           := Url;
    FHttpCli.OnRequestDone := HttpRequestDone;
    FHttpCli.OnDocData     := HttpDocData;
    FHttpCli.GetASync;
    { We need a message loop in order for windows message processing to work. }
    { There is a message loop built into each TWSocket, so we use the one in  }
    { THttpCli control socket.                                                }
    { MessageLoop will exit only when WM_QUIT message is posted. We do that   }
    { form the OnRequestDone event handler when the component has finished.   }
    FHttpCli.MessageLoop;
    WriteLn('Going back to the OS');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This event handler is called each time THttpCli receive document data.    }
procedure TConApplication.HttpDocData(
    Sender : TObject;
    Buffer : Pointer;
    Len    : Integer);
begin
    while Len > 0 do begin                 { While we have bytes...   }
        Write(PAnsiChar(Buffer)^);         { Write to standard output }
        Buffer := PAnsiChar(Buffer) + 1;   { Skip to next byte        }
        Len    := Len - 1;                 { Count down the byte      }
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This event handler is called when THttpCli has finihed his work.          }
procedure TConApplication.HttpRequestDone(
    Sender    : TObject;
    RqType    : THttpRequest;
    ErrorCode : Word);
begin
    { Check status }
    if ErrorCode <> 0 then
        WriteLn('Failed, error #' + IntToStr(ErrorCode))
    else
        WriteLn('Done.');
    { Prompt the user }
    WriteLn('Hit ENTER key to return quit program...');
    ReadLn;
    { Break message loop we called from the execute method }
    FHttpCli.PostQuitMessage;
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
end.
