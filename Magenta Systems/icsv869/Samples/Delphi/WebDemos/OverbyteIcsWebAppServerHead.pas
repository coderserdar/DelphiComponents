{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       Arno Garrels
Creation:     June 22, 2010
Description:  This source is part of WebAppServer demo application.
              The purpose is to make a HTTP/HTTPS HEAD request to some
              IPv4/IPv6 server and return the response. Includes a very simple
              'anti-robots' feature.
Version:      1.00
EMail:        francois.piette@overbyte.be    http://www.overbyte.be
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 2012 by François PIETTE
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
Jan 28, 2012 V1.01 Use asynchronous method rather than sync. Handle timeouts
                   with TWSocket's new built-in timeout.
Jun 27, 2017 V8.49 removed BUILTIN_TIMEOUT and USE_SSL checks, standard now

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsWebAppServerHead;

interface

uses
  {$IFDEF MSWINDOWS}
    Windows,
  {$ENDIF}
    Classes, SysUtils, Math,
    OverbyteIcsHttpAppServer,
    OverbyteIcsWebAppServerUrlDefs,
    OverbyteIcsWebAppServerHttpHandlerBase,
    OverbyteIcsHttpSrv,
    OverbyteIcsHttpProt,
    OverbyteIcsWSocket;

type
    THeadOperator = (opAdd, opMinus);
    TUrlHandlerHead = class(TUrlHandlerBase)
    private
        FN1, FN2: Integer;
        FOp: THeadOperator;
        Url, Equals, Response: String;
        Cli : TSslHttpCli;
        AllHdrs: Boolean;
        function GenerateMath: String;
        function VerifyMath(const S: String): Boolean;
        procedure HeadRequestDone(Sender  : TObject;
                                  RqType  : THttpRequest;
                                  ErrCode : Word);
        procedure HeadRequestTimeout(Sender: TObject; Reason: TTimeoutReason);
    public
        procedure Execute; override;
    end;

implementation

const
    sDefaultUrl         = 'http://ipv6.google.com';
    sDoCalc             = 'Please try to calculate the correct value';
    sHeadUrl            = 'HeadUrl';
    sEquals             = 'Equals';
    sResponse           = 'Response';
    sAnswerThisQuestion = 'Please answer this question: <br>';

procedure TUrlHandlerHead.HeadRequestDone(
    Sender  : TObject;
    RqType  : THttpRequest;
    ErrCode : Word);
var
    I : Integer;
begin
    try
        if Cli.RcvdHeader.Count > 0 then begin
            if not AllHdrs then
                Response := Response + Cli.RcvdHeader[0]
            else
                for I := 0 to Cli.RcvdHeader.Count - 1 do
                    Response := Response +
                                Cli.RcvdHeader[I] + '<br>' + CRLF;
        end
        else if ErrCode <> 0 then
            Response := Response + 'error #' + IntToStr(ErrCode)
        else
            Response := Response + 'Unknown error';

        AnswerPage('', NO_CACHE, UrlHeadForm, nil,
                  [sHeadUrl, sDefaultUrl,
                  sEquals, GenerateMath,
                  sResponse, Response]);
    finally
        Finish;
    end;
end;

procedure TUrlHandlerHead.HeadRequestTimeout(Sender: TObject;
  Reason: TTimeoutReason);
begin
    try
        Cli.OnRequestDone := nil;
        Cli.Abort;
        Response := Response + ' Request timeout';
        AnswerPage('', NO_CACHE, UrlHeadForm, nil,
                  [sHeadUrl, sDefaultUrl,
                  sEquals, GenerateMath,
                  sResponse, Response]);
    finally
        Finish;
    end;
end;

procedure TUrlHandlerHead.Execute;
var
    s : String;
    ButtonPressed : Boolean;
    FinishFlag : Boolean;
begin
    if NotLogged then  // Frees this object if not logged in.
        Exit;
    FinishFlag := True;
    try
        Response := '';
        ExtractURLEncodedValue(Params, sHeadUrl, Url);
        ExtractURLEncodedValue(Params, sEquals, Equals);
        ExtractURLEncodedValue(Params, 'Submit', s);
        ButtonPressed := s <> '';
        ExtractURLEncodedValue(Params, 'AllHeaders', s);
        AllHdrs := s <> '';
        if Url = '' then
            Url := sDefaultUrl;
        if (SessionData.TempVar >= 0) and ButtonPressed and
           (Equals <> '') then begin
            if not VerifyMath(Equals) then begin
                AnswerPage('', NO_CACHE, UrlHeadForm, nil,
                          [sHeadUrl, Url,
                          sEquals, GenerateMath,
                          sResponse, sDoCalc]);
            end
            else begin
                try
                    Cli := TSslHttpCli.Create(Self);
                    Cli.SslContext := TSslContext.Create(Cli);
                    try
                        { Not neccessarily required, anyway }
                        if Pos('https', LowerCase(URL)) = 1 then
                            Cli.SslContext.InitContext;
                    except
                        { Make sure the OpenSSL libraries are in the path }
                        Response := 'HTTPS is currently not supported.';
                        AnswerPage('', NO_CACHE, UrlHeadForm, nil,
                                  [sHeadUrl, sDefaultUrl,
                                  sEquals, GenerateMath,
                                  sResponse, Response]);
                        Exit;
                    end;
                    Response := 'Response from "' + Url + '":<br>';
                    { IPv6 and IPv4, prefer IPv4 }
                    Cli.SocketFamily              := sfAnyIPv4;
                    Cli.RequestVer                := '1.1';
                    Cli.URL                       := Url;
                    Cli.CtrlSocket.TimeoutConnect := 5 * 1000;
                    Cli.CtrlSocket.TimeoutIdle    := 10 * 1000;
                    Cli.CtrlSocket.OnTimeout      := HeadRequestTimeout;
                    Cli.OnRequestDone             := HeadRequestDone;
                    Cli.HeadAsync;
                    FinishFlag := False;
                    Exit;
                except
                    Response := Response + ' Internal server error';
                end;
                AnswerPage('', NO_CACHE, UrlHeadForm, nil,
                          [sHeadUrl, sDefaultUrl,
                          sEquals, GenerateMath,
                          sResponse, Response]);
            end;
        end
        else begin
            if ButtonPressed and (Equals = '') then
                Response := sDoCalc;
            AnswerPage('', NO_CACHE, UrlHeadForm, nil,
                      [sHeadUrl, Url,
                      sEquals, GenerateMath,
                      sResponse, Response]);
        end;
    finally
        if FinishFlag then
            Finish; { Make sure this object is freed }
    end;    
end;

function TUrlHandlerHead.GenerateMath: String;
begin
    if not Assigned(SessionData) then
        Result := ''
    else begin
        FN1 := Random(10);
        FN2 := Random(10);
        FOp := THeadOperator(Random(2));
        if FOp = opAdd then begin
            Result := sAnswerThisQuestion +
                      IntToStr(FN1) + ' + ' + IntToStr(FN2) + ' equals ?';
            SessionData.TempVar := FN1 + FN2;
        end
        else begin
            Result := sAnswerThisQuestion +
                      IntToStr(Max(FN1, FN2)) + ' - ' +
                      IntToStr(Min(FN1,FN2)) + ' equals ?';
            SessionData.TempVar := Max(FN1, FN2) - Min(FN1, FN2);
        end;
    end;
end;

function TUrlHandlerHead.VerifyMath(const S: String): Boolean;
begin
    Result := Assigned(SessionData) and (StrToIntDef(S, 0) = SessionData.TempVar);
end;

end.

