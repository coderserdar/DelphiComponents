{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Description:  ConCli shows how to use TWSocket in a console mode application
              (for Delphi 2, Delphi 3 or Delphi 4).
EMail:        francois.piette@pophost.eunet.be  http://www.rtfm.be/fpiette
              francois.piette@rtfm.be
Creation:     Nov 20, 1997
Version:      1.01
WebSite:      http://www.rtfm.be/fpiette/indexuk.htm
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
Dec 05, 1998 V1.01 Don't use TWait control anymore

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$APPTYPE CONSOLE}
program ConCli1;

uses
    Forms, WSocket;

var
    WSocket1 : TWSocket;
    Buffer   : String;
begin
    WSocket1          := TWsocket.Create(nil);
    WSocket1.Proto    := 'tcp';
    WSocket1.Addr     := 'localhost';
    WSocket1.Port     := 'telnet';
    WSocket1.Connect;
    { Connect is asynchronous (non-blocking). We will wait while the }
    { session is connecting or application terminated.               }
    while WSocket1.State in [wsConnecting] do begin
        Application.ProcessMessages;
        if Application.Terminated then
            break;
    end;
    if WSocket1.State = wsConnected then begin
        WSocket1.ReadLine(15, Buffer);
        Writeln('Server banner is: ' + Buffer);
        WSocket1.Close;
    end
    else
        Writeln('Connection failed');
    WSocket1.Destroy;

    Writeln('Hit enter...');
    Readln;
end.
