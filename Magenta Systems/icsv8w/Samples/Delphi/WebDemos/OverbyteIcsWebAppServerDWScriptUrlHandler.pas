{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Creation:     June 09, 2013
Description:  This source is part of WebAppServer demo application.
              The purpose is to show how to call a DWScript script to build
              a dynamic web page. DWScript (http://code.google.com/p/dwscript/)
              is an object-oriented scripting engine for Delphi based on the
              Delphi language.
              To compile this source, you need to install DWScript on your
              development computer. It is NOT required to have DWScript
              installed on the computer where the code has to run since DWScript
              is linked in your Delphi application.
              See "Usage" below.
Version:      1.00
EMail:        francois.piette@overbyte.be    http://www.overbyte.be
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 2013 by François PIETTE
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

Usage:
   To use this code, you must modify the original OverbyteIcsWebAppServerDemo
   by adding OverbyteIcsWebAppServerDWScriptUrlHandler in the uses clause in
   the main source file OverbyteIcsWebAppServerMain.pas. You must also add
   a line in TWebAppSrvForm.WMAppStartup after all similar lines:
          HttpAppSrv1.AddGetHandler('/DWScripts/*',
                              TUrlHandlerDWScript);
   This line will maps everything in the "DWScript" folder in the root of the
   website to this URL handler. This means that if you have "Hello.pas" located
   in that folder and the browser requests the URL:
       http://localhost:20105/DWScripts/Hello.pas?Value1=123&Value2=457
   the DWScript engine will execute hello.pas.
   The script has two built-in object instances "Request" and "Response" that
   are to be used to get access to the URL parameters and to produce a response.
   See THttpRequest and THttpResponse classes in the code below to see what is
   accessible from the script (all published methods and properties).

History:


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsWebAppServerDWScriptUrlHandler;

interface

uses
    Classes, SysUtils, RTTI, TypInfo,
    OverbyteIcsHttpAppServer,
    OverbyteIcsHttpSrv,
    dwsVCLGUIFunctions,
    dwsMagicExprs,
    dwsRTTIExposer,
    dwsFunctions,
    dwsSymbols,
    dwsExprs,
    dwsComp;


type
    THttpResponse = class(TPersistent)
    private
        FStatus      : String;
        FContentType : String;
    public
        DocStream    : TStream;
    published
        property Status      : String read FStatus      write FStatus;
        property ContentType : String read FContentType write FContentType;
        procedure Write(const S : String);
    end;

    THttpRequest = class(TPersistent)
    public
        Params : String;
        class function ReadTextFile(const FileName : String) : String;
    published
        function  GetParamByName(const ParamName: String): String;
        function  CheckParamByName(const ParamName  : String;
                                   var   ParamValue : String): Boolean;
    end;

    TUrlHandlerDWScript = class(TUrlHandler)
    protected
        FScript                  : IdwsProgram;
        FCompileMsgs             : String;
        FDelphiWebScript         : TDelphiWebScript;
        FUnit                    : TdwsUnit;
        FExec                    : IdwsProgramExecution;
        FHttpRequest             : THttpRequest;
        FHttpResponse            : THttpResponse;
        procedure ExposeInstancesAfterInitTable(Sender: TObject);
    public
        procedure Execute; override;
    end;

implementation


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{ TUrlHandlerDWScript }

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TUrlHandlerDWScript.Execute;
var
    SrcFileName : String;
    Source      : String;
begin
    FDelphiWebScript  := TDelphiWebScript.Create(nil);
    FUnit             := TdwsUnit.Create(nil);
    FHttpResponse     := THttpResponse.Create;
    FHttpRequest      := THttpRequest.Create;
    try
        DocStream.Free;
        DocStream := TMemoryStream.Create;

        FHttpResponse.DocStream    := DocStream;
        FHttpResponse.Status       := '200 OK';
        FHttpResponse.ContentType  := 'text/html';
        FHttpRequest.Params        := Params;

        FUnit.OnAfterInitUnitTable := ExposeInstancesAfterInitTable;
        FUnit.UnitName             := 'WebPage';
        FUnit.Script               := FDelphiWebScript;
        FUnit.ExposeRTTI(TypeInfo(THttpResponse), [eoNoFreeOnCleanup]);
        FUnit.ExposeRTTI(TypeInfo(THttpRequest),  [eoNoFreeOnCleanup]);
        // In this application, we have placed DWScripts source code in
        // a directory at the same level as the "Template" folder.
        SrcFileName := ExcludeTrailingPathDelimiter(
                           ExtractFilePath(Client.TemplateDir)) +
                       StringReplace(Client.Path, '/', '\', [rfReplaceAll]);
        if not FileExists(SrcFileName) then
            FHttpResponse.Write('<html><body>Script not found</body></html>')
        else begin
            Source       := FHttpRequest.ReadTextFile(SrcFileName);
            FScript      := FDelphiWebScript.Compile(Source);
            FCompileMsgs := FScript.Msgs.AsInfo;
            if FScript.Msgs.HasErrors then begin
                FHttpResponse.Write('<html><body>' + FCompileMsgs + '</body></html>');
            end
            else begin
                FExec    := FScript.Execute;
            end;
        end;
        AnswerStream(FHttpResponse.Status, FHttpResponse.ContentType, NO_CACHE);
    finally
        FreeAndNil(FUnit);
        FreeAndNil(FDelphiWebScript);
        FreeAndNil(FHttpResponse);
        FreeAndNil(FHttpRequest);
    end;
    Finish;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TUrlHandlerDWScript.ExposeInstancesAfterInitTable(Sender: TObject);
begin
    FUnit.ExposeInstanceToUnit('Response', 'THttpResponse', FHttpResponse);
    FUnit.ExposeInstanceToUnit('Request',  'THttpRequest',  FHttpRequest);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{ THttpResponse }

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpResponse.Write(const S: String);
var
   Ch : Char;
   B  : Byte;
begin
    for Ch in S do begin
        // We should convert the Unicode string to whatever the document
        // is supposed to be. Here we just convert it, brute force, to ASCII.
        // This won't work eastern character sets.
        B := Ord(AnsiChar(Ch));
        DocStream.Write(B, 1);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{ THttpRequest }

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function THttpRequest.CheckParamByName(
    const ParamName  : String;
    var   ParamValue : String): Boolean;
begin
    Result := ExtractURLEncodedValue(Params, ParamName, ParamValue);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function THttpRequest.GetParamByName(const ParamName: String): String;
begin
    ExtractURLEncodedValue(Params, ParamName, Result);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
class function THttpRequest.ReadTextFile(const FileName : String) : String;
var
    Stream : TFileStream;
    AnsiBuf : AnsiString;
begin
    Stream := TFileStream.Create(FileName, fmOpenRead);
    try
        SetLength(AnsiBuf, Stream.Size);
        Stream.Read(AnsiBuf[1], Stream.Size);
        Result := String(AnsiBuf);
    finally
        FreeAndNil(Stream);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.
