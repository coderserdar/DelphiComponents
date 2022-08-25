{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Creation:     April 11, 2009
Description:  This source is part of WebAppServer demo application.
              The purpose is to implement a login for the entire application.
              Interestingly, the login mechanism is secure: the password is
              never passed thru the connection. A hash code is use to avoid
              passing the password and offering the highest possible security.
              This implement a "challenge/response" algorithm.
Version:      8.49
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
Jul 5, 2017  V8.49 Log when login fails or OK


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsWebAppServerLogin;

interface

uses
  {$IFDEF MSWINDOWS}
    Windows,
  {$ENDIF}
    Classes, SysUtils,
    OverbyteIcsMD5,
    OverbyteIcsUtils,
    OverbyteIcsHttpSrv,
    OverbyteIcsHttpAppServer,
    OverbyteIcsWebAppServerDataModule,
    OverbyteIcsWebAppServerHttpHandlerBase,
    OverbyteIcsWebAppServerUrlDefs,
    OverbyteIcsWebAppServerSessionData;

type
    TUrlHandlerLoginFormHtml = class(TUrlHandlerBase)
    public
        procedure Execute; override;
    end;

    TUrlHandlerDoLoginSecureHtml = class(TUrlHandlerBase)
    public
        procedure Execute; override;
    end;

    TUrlHandlerJavascriptErrorHtml = class(TUrlHandlerBase)
    public
        procedure Execute; override;
    end;

implementation

{ THttpHandlerLogin }

procedure TUrlHandlerLoginFormHtml.Execute;
var
    MySessionData : TAppSrvSessionData;
    Headers       : String;
begin
    if not ValidateSession then begin
//        Inc(GSessionDataCount);
        MySessionData := TAppSrvSessionData.Create(nil);
//        MySessionData.Name := 'MySessionData' + IntToStr(GSessionDataCount);
        MySessionData.AssignName;  // Angus
        Headers       := NO_CACHE + CreateSession('', 0, MySessionData);
    end
    else begin
        MySessionData := SessionData;
        Headers       := NO_CACHE;
    end;

    MySessionData.LastRequest    := Now;
    MySessionData.RequestCount   := MySessionData.RequestCount + 1;
    MySessionData.LoginChallenge := StrMD5(IntToHex(IcsGetTickCount, 8));
    AnswerPage('',
               Headers,
               'LoginForm.html',
               nil,
               ['Challenge',     MySessionData.LoginChallenge,
                'DoLoginSecure', UrlDoLoginSecure,
                'COUNTER',       UrlCounter]);
    Finish;
end;

procedure TUrlHandlerDoLoginSecureHtml.Execute;
var
    Challenge     : String;
    UserCode      : String;
    PasswordHash  : String;
    Password      : String;
begin
    if NotLogged then
        Exit;

    Challenge    := SessionData.LoginChallenge;
    ExtractURLEncodedValue(Params, 'PasswordHash', PasswordHash);
    ExtractURLEncodedValue(Params, 'UserCode',     UserCode);
    // In this demo we use an hardcode password.
    // In a real world application, you should use a database of
    // usercode/password and associated permissions !
    Password := 'admin';

    if  (UserCode = '') or
        (not SameText(PasswordHash,
                      StrMD5(Challenge + Trim(UpperCase(Password))))) then begin
        WebAppSrvDataModule.CounterIncrement('LoginInvalid');
        DeleteSession;
        NotLogged;
        Display ('Invalid password for ' + UserCode);   { V8.49 }
        Exit;
    end;

    WebAppSrvDataModule.CounterIncrement('LoginOK');
    SessionData.LogonTime  := Now;
    SessionData.UserCode   := UserCode;
    Display ('Login OK for ' + UserCode);   { V8.49 }
    Relocate(UrlHomePage);
    Finish;
end;


procedure TUrlHandlerJavascriptErrorHtml.Execute;
begin
    AnswerPage('',
               '',
               'JavascriptError.html',
               nil,
               ['COUNTER',       UrlCounter]);
    Finish;
end;

end.
