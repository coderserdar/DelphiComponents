{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       Angus Robertson
Creation:     July 3, 2009
Description:  This is an email form demo, designed to send a email to a hard
              coded email address.  It attempts to stop various common
              attacks on email forms and prevents common HTML tags being
              entered in the form.  This demo uses a test email account at
              Magenta Systems, but the sender gets an identical copy of the
              email so you see it worked.
Version:      8.60
EMail:        angus@magsys.co.uk
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 2009 to 2019 by François PIETTE
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

History:
Jul 10, 2009 V1.01 Arno fixed a bug in SmtpClient.OnGetData, we may not send
                   Unicode.
Jul 10, 2009 V1.02 Arno Removed string cast warnings.
Sept 1, 2009 V1.03 Angus - report exceptions creating virtual pages
Nov 16, 2013 V1.04 Removed all references to demo's main form.
May 24, 2016 V1.04 Angus - added OverbyteIcsFormDataDecoder to uses
Jul 5,  2017 V8.49 Changed GetUAgeSizeFile to IcsGetUAgeSizeFile
                   Ensure POST uses same protocol as original page
Feb 20, 2019 V8.60 Now sending email using IcsMailQueue in main unit rather
                     than local SMTP component. 
                   Unit now unique name, not shared with other samples
May 20, 2022 V8.69 Using properties from OverbyteIcsSslMultiWebDataModule so this
                     unit is not dependent upon a single application, and works in
                     DDWebService.


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFNDEF ICS_INCLUDE_MODE}
unit OverbyteIcsSslMultiWebMailer;
{$ENDIF}

interface

uses
  {$IFDEF MSWINDOWS}
    Windows,
  {$ENDIF}
    SysUtils, Classes,
  {$IFDEF FMX}
    FMX.Types,
  {$ELSE}
    ExtCtrls,
  {$ENDIF}
    OverbyteIcsWndControl, OverbyteIcsHttpAppServer, OverbyteIcsHttpSrv,
    OverbyteIcsWebSession, OverbyteIcsUtils, OverbyteIcsWSocket,
    OverbyteIcsFormDataDecoder,
    OverbyteIcsSslMultiWebDataModule;

type

   TUrlHandlerMailer = class(TUrlHandler)
    private
        WSocket: TWSocket;
        AbortTimer: TTimer;
        sMailBody: string ;
        sMailFrom: string ;
        sMailName: string ;
        sMailTo: string ;
        sIPAddr: string ;
        sUserIPHost: string ;
        sPageUrl: string ;
        errorMsg: string ;
        procedure HandleBackgroundExceptions(Sender: TObject; E: Exception; var CanClose : Boolean);
    public
        destructor  Destroy; override;
        procedure Execute; override;
        procedure DoneDnsLookup (Sender: TObject; Error: Word);
        procedure TimerAbortTimer(Sender: TObject);
    end;

{ note the email host is deliberately hardcoded in this form, to prevent it being
  supplied as a form parameter, which is how spammers abuse email forms.
  The account may be passed as a parameter with the form, but sending the form will
  fail unless it's another valid account at ftptest.org }

const
    DefaultEmailDomain = '@ftptest.org' ;
    DefaultEmailAccount = 'testing' ;

implementation

// Uses OverbyteIcsSslMultiWebServ1; // V8.69

function GetTempLastMod (Client: THttpAppSrvConnection; const FName: string): string ;
var
    FileDT: TDateTime ;
    FSize: Int64 ;
    FullName: string ;
const
    DateMmmMask = 'dd mmm yyyy' ;
begin
    FullName := Client.TemplateDir + '/' + Fname ;
    if IcsGetUAgeSizeFile (FullName, FileDT, FSize) then
        DateTimeToString (Result, DateMmmMask, FileDT)
    else
        Result := 'Page not found' ;
end ;

// does a string contain any common HTML tags, used for email body validation to stop spammers using HTML

function IsHtmlTags (const S: string): boolean ;
var
    S2: string ;
begin
    result := false ;
    S2 := Lowercase (S) ;
    if Pos ('<a', S2) > 0 then result := true
    else if Pos ('</a', S2) > 0 then result := true
    else if Pos ('href', S2) > 0 then result := true
    else if Pos ('<img', S2) > 0 then result := true
    else if Pos ('[/url]', S2) > 0 then result := true ;
end;

function IsValidEmail(const Value: String): Boolean;
var
    I : Integer;
    NamePart, ServerPart: String;

    function CheckAllowed(const S: String): Boolean;
    var i: Integer;
    begin
        Result:= false;
        for I := 1 to Length(S) do
          case S[I] of
              'a'..'z', 'A'..'Z', '0'..'9', '_', '-', '.' : {continue};
            else
                Exit;
          end;
        Result:= true;
    end;

begin
    Result := False;
    I := Pos('@', Value);
    if I = 0 then Exit;
    NamePart := Copy(Value, 1, I - 1);
    ServerPart := Copy(Value, I + 1, Length(Value));
    if (Length(NamePart) = 0) or ((Length(ServerPart) < 5)) then
        Exit;
    I := Pos('.', ServerPart);
    if (I = 0) or (I > (Length(ServerPart) - 2)) then
        Exit;
    Result:= CheckAllowed(NamePart) and CheckAllowed(ServerPart);
end;


procedure TUrlHandlerMailer.Execute;
begin
    WSocket := TWSocket.Create (self) ;
    WSocket.OnDnsLookupDone := DoneDnsLookup ;
    WSocket.OnBgException := HandleBackgroundExceptions;
    AbortTimer := TTimer.Create (self) ;
    AbortTimer.OnTimer := TimerAbortTimer ;
    AbortTimer.Interval := 5000 ;     // five second timeout for DNS
    try
    //  get user IP address and lookup host name
        sIPAddr := Client.GetPeerAddr ;
        sUserIPHost := '' ;
        WSocket.ReverseDnsLookup (sIPAddr) ;
        AbortTimer.Enabled := true ;
    except
        Display ('Exception Looking up DNS - ' + IcsGetExceptMess (ExceptObject)) ;
        DoneDnsLookup (Self, 999) ; // continue to use form
    end;
end;

procedure TUrlHandlerMailer.HandleBackgroundExceptions(Sender: TObject;
  E: Exception; var CanClose: Boolean);
begin
  Display('Exception processing page - ' + E.ClassName + ': ' + E.Message);
  CanClose := True;
end;

destructor TUrlHandlerMailer.Destroy;
begin
    if Assigned (AbortTimer) then
    begin
        AbortTimer.Enabled := false ;
        FreeAndNil (AbortTimer) ;
    end ;
    FreeAndNil (WSocket) ;
    inherited Destroy;
end;

procedure TUrlHandlerMailer.TimerAbortTimer(Sender: TObject);
begin
    AbortTimer.Enabled := false ;
    Display ('DNS Lookup Timed Out') ;
    WSocket.CancelDnsLookup ;
end;

procedure TUrlHandlerMailer.DoneDnsLookup (Sender: TObject; Error: Word);
var
    AWSocket: TWSocket ;
    I, Id: integer ;
    sTemp, sTempFrom, sMagEmail: string ;
begin
    AbortTimer.Enabled := false ;
    sTemp := '' ;
    AWSocket:= Sender as TWSocket ;
    if Error = 0 then
    begin
        if AWSocket.DnsResultList.Count <> 0 then
        begin
            for I := 0 to Pred (AWSocket.DnsResultList.Count) do
            begin
                if I <> 0 then sTemp := sTemp + ', ' ;
                sTemp := sTemp + AWSocket.DnsResultList [I] ;
            end
        end
        else
            sTemp := AWSocket.DnsResult ;
        sUserIPHost := sTemp + ' (' + sIPAddr + ')' ;
    end
    else
    begin
        Display ('DNS Lookup Failed - ' + WSocketErrorDesc (Error)) ;
        sUserIPHost := sIPAddr ;
    end;
   { V8.49 ensure POST uses same protocol as original page }
    sPageUrl := Client.RequestProtocol + '://' + Client.RequestHost + Client.Path ;  // used for POST URL
    errorMsg := '' ;

// see if to email account passed as query - no domain
    if Client.Method = 'GET' then
    begin
  //      ExtractURLEncodedValue (Params, 'EmailTo', sMailTo) ;
        sMailTo := Params ;
//  SmtpClient.RcptName.Clear ; // deliberate exception
    end ;

// see if page is being POSTed by itself to send and email
    if Client.Method = 'POST' then
    begin
        ExtractURLEncodedValue (String(Client.PostedData), 'MailZBody', sMailBody) ;
        if IsHtmlTags (sMailBody) then
        begin
            errorMsg := 'Please specify valid content' ; // spammers use HTML tags in the body
            Display ('Email validation error: ' + errorMsg + ' - ' + sMailBody) ;
        end
        else if (Length (sMailBody) < 40) then
        begin
            errorMsg := 'Please specify your full message' ;
            Display ('Email validation error: ' + errorMsg + ' - ' + sMailBody) ;
        end;
        ExtractURLEncodedValue (String(Client.PostedData), 'MailZFrom', sMailFrom) ;
        ExtractURLEncodedValue (String(Client.PostedData), 'MailZName', sMailName) ;
        ExtractURLEncodedValue (String(Client.PostedData), 'MailZTo', sMailTo) ;
        if Length (sMailTo) < 4 then sMailTo := DefaultEmailAccount ;  // no account passed, use default
        if NOT IsValidEmail (sMailFrom) then
        begin
            errorMsg := 'Please specify a valid email address' ;
            Display ('Email validation error: ' + errorMsg + ' - ' + sMailFrom) ;
        end;
     // the IP check is to stop spammers POSTing this page without having GET it first, they have to know the IP and host name
        ExtractURLEncodedValue (String(Client.PostedData), 'MailZIp', sTemp) ;
        if sUserIPHost <> sTemp then
        begin
            errorMsg := 'Please specify your message again, internal error' ;
            Display ('Email validation error: ' + errorMsg + ' - IP Address ' + sTemp) ;
        end;
        if Length (sMailName) < 5 then
            errorMsg := 'Please specify your full name'
        else if NOT IsUsAscii (sMailName) then errorMsg := 'Your full name can not contain punctuation' ;

        if (errorMsg = '') then
        begin
            if NOT SslMultiWebDataModule.IcsMailQueue.Active then begin
                if (SslMultiWebDataModule.IcsMailQueue.MailQuDir <> '') and
                    (SslMultiWebDataModule.IcsMailQueue.MailServers.Count > 0) then begin
                    SslMultiWebDataModule.IcsMailQueue.Active := True;
                end;
            end;
            if NOT SslMultiWebDataModule.IcsMailQueue.Active then
                    errorMsg := 'Sorry, unable to send email at present, please try later' ;
        end ;

    //  build form email and display content
        if (errorMsg = '') then
        begin
          // email it to hardcoded host
            try
                sMagEmail := sMailTo + DefaultEmailDomain ;
                sTempFrom := '"' + sMailName + '" <' + sMailFrom + '>' ;
                with SslMultiWebDataModule.IcsMailQueue.QuHtmlSmtp do begin
                    PlainText.Clear ;
                    PlainText.Text := RemoveHtmlSpecialChars (sMailBody) +  #13#10 +  #13#10 +
                            'User Address: ' + sUserIPHost +  #13#10 +
                            'Email Sent from Web Site Response Form: ' + sPageUrl +  #13#10 ;
                    if Client.AuthUserName <> '' then
                                     PlainText.Add ('User Account: ' + Client.AuthUserName) ;
                    Display ('Sending Email Form to ' + sMagEmail + ' from ' + sTempFrom) ;
                    RcptName.Clear ;
                    RcptName.Add (sMagEmail) ;
                    RcptName.Add (sMailFrom) ;
                    FromName := sMailFrom ;
                    HdrTo := sMagEmail ;
                    HdrFrom := sTempFrom ;
                    HdrReplyTo := sTempFrom ;
                    HdrCc := sTempFrom ;
                    HdrSubject := 'ICS Demo Web Site Email Form - ' + sMailTo ;
                end;
                id := SslMultiWebDataModule.IcsMailQueue.QueueMail ;
                if id > 0 then begin
           // done OK
                    Display('Email Form Queued OK') ;
                    AnswerPage('', NO_CACHE, 'maildone.html', nil, []) ;
                    Finish;
                    exit ;
                end
                else
                begin
                    errorMsg := 'Failed to Queue Email Form: ' +
                                    SslMultiWebDataModule.IcsMailQueue.QuHtmlSmtp.ErrorMessage ;
                    Display (errorMsg) ;
                end ;
            except
                errorMsg := 'Failed to Start Sending Email Form: ' + IcsGetExceptMess (ExceptObject) ;
                Display (errorMsg) ;
            end;
        end ;
    end ;

    AnswerPage('', NO_CACHE, 'mailer.html', nil,
             ['PageLastMod', GetTempLastMod (Client, 'mailer.html'),
              'sPageUrl', sPageUrl, 'sMailTo', sMailTo, 'sMailName', sMailName,
              'sUserIPHost', sUserIPHost, 'sMailFrom', sMailFrom,
              'sMailBody', sMailBody, 'errorMsg', errorMsg
               ]);
    Finish;
end;


end.
