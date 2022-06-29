/*---------------------------------------------------------------------------

Author:       François PIETTE
Description:  ConCli shows how to use TWSocket in a console mode application.
EMail:        francois.piette@pophost.eunet.be  http://www.rtfm.be/fpiette
              francois.piette@rtfm.be
Creation:     Nov 22, 1997
Version:      1.02
WebSite:      http://www.rtfm.be/fpiette/indexuk.htm
Support:      Use the mailing list twsocket@rtfm.be See website for details.
Legal issues: Copyright (C) 1997, 1998 by François PIETTE 
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
Apr 12, 1998 V1.01 Adapted for BCB3
Dec 19, 1998 V1.02 Do not use TWait control anymore.

---------------------------------------------------------------------------*/
#if __BORLANDC__ == 0x520     // BCB1 is BC5.20   BCB3 is BC5.30
    #define _WINSOCKAPI_      // Prevent winsock.h from being included
#endif
#include <vcl\vcl.h>
#include <stdio.h>
#include <conio.h>
#include <condefs.h>
#pragma hdrstop
//---------------------------------------------------------------------------
USEUNIT("..\..\..\delphi\vc32\WSocket.pas");
USEUNIT("..\..\..\delphi\vc32\Wait.pas");
//---------------------------------------------------------------------------
#include <wsocket.hpp>
#include <wait.hpp>
//---------------------------------------------------------------------------
void main(void)
{
    TWSocket   *WSocket1;
    AnsiString Buffer;

    WSocket1           = new TWSocket((void *)NULL);
    WSocket1->Addr     = "localhost";
    WSocket1->Port     = "telnet";
    WSocket1->Proto    = "tcp";
    printf("Connecting to localhost/telnet...\n");
    WSocket1->Connect();
    // Connect is asynchronous (non-blocking). We will wait while the
    // session is connecting or application terminated.
    while (WSocket1->State == wsConnecting) {
        Application->ProcessMessages();
        if (Application->Terminated)
            break;
    }
    if (WSocket1->State == wsConnected) {
        WSocket1->ReadLine(15, Buffer);
        printf("Server banner is: %s\n", Buffer.c_str());
        WSocket1->Close();
    }
    else {
        printf("Connection failed.\n");
    }
    delete WSocket1;

    printf("Hit enter...");
    getch();
}
//---------------------------------------------------------------------------
