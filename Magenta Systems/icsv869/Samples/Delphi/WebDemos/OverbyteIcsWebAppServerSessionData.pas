{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Creation:     April 11, 2009
Description:  This source is part of WebAppServer demo application.
              The purpose is defin the session data used by the application.
Version:      1.02
EMail:        francois.piette@overbyte.be    http://www.overbyte.be
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 2009 by François PIETTE
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
Apr 19, 2010 V1.01 Angus, removed GSessionDataCount which duplicates same
                          variable in OverbyteIcsWebSession
Jun 23, 2010 V1.02 Arno - Added integer field TempVar.

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsWebAppServerSessionData;

interface

uses
    Classes, OverbyteIcsWebSession;

type
    TAppSrvSessionData = class(TWebSessionData)
    protected
       FUserCode       : String;
       FLogonTime      : TDateTime;
       FLastRequest    : TDateTime; // Last request time stamp
       FRequestCount   : Integer;   // Count the requests
       FIP             : String;    // Client IP Adress (beware of proxies)
       FLoginChallenge : String;    // Used for secure login
       FConfigPort     : String;    // Used for configuration process
       FConfigTempDir  : String;    // Used for configuration process
       FConfigHasLogo  : Boolean;   // Used for configuration process
       FTempVar        : Integer;   // Currently used for anti-spam
    public
       constructor Create(AOwner: TComponent); override;
    published
       property UserCode       : String     read  FUserCode
                                            write FUserCode;
       property LogonTime      : TDateTime  read  FLogonTime
                                            write FLogonTime;
       property RequestCount   : Integer    read  FRequestCount
                                            write FRequestCount;
       property LastRequest    : TDateTime  read  FLastRequest
                                            write FLastRequest;
       property IP             : String     read  FIP
                                            write FIP;
       property LoginChallenge : String     read  FLoginChallenge
                                            write FLoginChallenge;
       property ConfigPort     : String     read  FConfigPort
                                            write FConfigPort;
       property ConfigTempDir  : String     read  FConfigTempDir
                                            write FConfigTempDir;
       property ConfigHasLogo  : Boolean    read  FConfigHasLogo
                                            write FConfigHasLogo;
       property TempVar        : Integer    read  FTempVar
                                            write FTempVar;
    end;

//var
//    GSessionDataCount : Integer;

implementation

{ TAppSrvSessionData }

constructor TAppSrvSessionData.Create(AOwner: TComponent);
begin
    inherited;
    FTempVar := -1;
end;

initialization
    RegisterClass(TAppSrvSessionData);

end.
