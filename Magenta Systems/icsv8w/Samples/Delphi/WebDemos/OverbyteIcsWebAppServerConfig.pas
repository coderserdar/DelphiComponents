{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Creation:     April 11, 2009
Description:  This source is part of WebAppServer demo application.
              The purpose is to do the server configuration.
              Actually it is mostly a data entry demo showing how to
              handle HTML form with simple text input as well as a
              file upload input.
Version:      1.01
EMail:        francois.piette@overbyte.be    http://www.overbyte.be
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 2009-2010 by François PIETTE
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
Jul 30, 2010 V1.01 F.Piette: Added call to SaveConfig


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsWebAppServerConfig;

interface

uses
  {$IFDEF MSWINDOWS}
    Windows,
  {$ENDIF}
  {$IFDEF POSIX}
    Posix.Unistd,
    Posix.Stdio,
  {$ENDIF}
    Classes, SysUtils,
    OverbyteIcsHttpSrv,
    OverbyteIcsHttpAppServer,
    OverbyteIcsFormDataDecoder,
    OverbyteIcsWebAppServerDataModule,
    OverbyteIcsWebAppServerHttpHandlerBase,
    OverbyteIcsWebAppServerUrlDefs,
    OverbyteIcsWebAppServerSessionData;

type
    TUrlHandlerConfigFormHtml = class(TUrlHandlerBase)
    public
        procedure Execute; override;
    end;

    TUrlHandlerDoConfigHtml = class(TUrlHandlerBase)
    public
        procedure Execute; override;
    end;

    TUrlHandlerConfigLogoPng = class(TUrlHandlerBase)
    public
        procedure Execute; override;
    end;

    TUrlHandlerDoConfigConfirmSaveHtml = class(TUrlHandlerBase)
    public
        procedure Execute; override;
    end;

implementation

procedure TUrlHandlerConfigFormHtml.Execute;
begin
    if NotLogged then
        Exit;
    AnswerPage('', NO_CACHE, 'Config.html', nil,
               ['LOGIN',     UrlLogin,
                'COUNTER',   UrlCounter,
                'USERCODE',  SessionData.UserCode,
                'DOCONFIG',  UrlDoConfigHtml,
                'PORT',      WebAppSrvDataModule.Port,
                'LOGINTIME', DateToStr(SessionData.LogonTime)]);
    Finish;
end;

procedure TUrlHandlerDoConfigHtml.Execute;
var
    Stream   : TMemoryStream;
    Decoder  : TFormDataAnalyser;
    Field    : TFormDataItem;
    FileName : String;
    FileExt  : String;
    ErrMsg   : String;
    TempDir  : String;
begin
    if NotLogged then
        Exit;
    ErrMsg := '';
    SessionData.ConfigPort := '';
    SessionData.ConfigTempDir := '';
    Stream := TMemoryStream.Create;
    try
        Stream.WriteBuffer(Client.PostedData^, Client.PostedDataLen);
        Stream.Seek(0, 0);
        Decoder := TFormDataAnalyser.Create(nil);
        try
            //Decoder.OnDisplay := WebAppSrvDataModule.DisplayHandler;
            Decoder.DecodeStream(Stream);

            if not SameText(Decoder.Part('submit').AsString, 'Save') then
                ErrMsg := 'canceled'
            else begin
                // Extract Port field. Do a minimal verification for validity
                // A port is either a positive 16 bits décimal number, or
                // a well known "service name" such as "http".
                Field := Decoder.Part('port');
                if (Field.DataLength > 0) and (Field.DataLength < 100) then
                    SessionData.ConfigPort := Trim(Field.AsString);

                // Extract logo image file, do a minimal validity check
                Field    := Decoder.Part('logo');
                FileName := ExtractFileName(Field.ContentFileName);
                SessionData.ConfigHasLogo := (FileName <> '');
                if SessionData.ConfigHasLogo then begin
                    FileExt  := ExtractFileExt(FileName);
                    if (not SameText(FileExt, '.png')) or
                       (not (SameText(Field.ContentType, 'image/png') or
                             SameText(Field.ContentType, 'image/x-png')))
                    then
                        ErrMsg := 'Only PNG file accepted for logo'
                    else if Field.DataLength > (50 * 1024) then
                        ErrMsg := 'Logo image file must be < 50KB'
                    else begin
                        // Create a temp dir
                        // The server will delete any tempdir after the datetime
                        // included in the name has expired
                        SessionData.ConfigTempDir := PathDelim +
                                   FormatDateTime('YYYYMMDDHHNNSSZZZ',
                                                  Now + EncodeTime(0, 15, 0, 0));
                        TempDir := SessionData.ConfigTempDir +
                                   PathDelim + SessionData.UserCode;
                        ForceDirectories(WebAppSrvDataModule.DataDir +
                                         TempDir);
                        // Save the logo file in the temp directory
                        // Do not use the original filename !
                        Field.SaveToFile(WebAppSrvDataModule.DataDir +
                                         TempDir + PathDelim + 'Logo.png');
                    end;
                end;
            end;
        finally
            FreeAndNil(Decoder);
        end;
    finally
        FreeAndNil(Stream);
    end;
    if ErrMsg <> '' then
        AnswerString('', '', '',
                     '<html><body><a href="' + UrlConfigForm + '">' +
                     ErrMsg + '</a></body></html>')
    else begin
        AnswerPage('', NO_CACHE, 'ConfigConfirm.html', nil,
                   ['PORT',   SessionData.ConfigPort,
                    'LOGO',   'ConfigLogo.png',
                    'ACTION', UrlDoConfigConfirmSaveHtml]);
    end;
    Finish;
end;

procedure TUrlHandlerConfigLogoPng.Execute;
var
    FileName : String;
begin
    if NotLogged then
        Exit;
    if SessionData.ConfigHasLogo then
        FileName := WebAppSrvDataModule.DataDir +
                    SessionData.ConfigTempDir +
                    PathDelim + SessionData.UserCode +
                    PathDelim + 'Logo.png'
    else
        FileName := WebAppSrvDataModule.ImagesDir +
                    PathDelim + 'Logo.png';

    DocStream.Free;
    DocStream := TFileStream.Create(FileName, fmOpenRead);
    AnswerStream('', 'image/png', NO_CACHE);
    Finish;
end;

procedure TUrlHandlerDoConfigConfirmSaveHtml.Execute;
var
    Submit   : String;
    FileName : String;
begin
    if NotLogged then
        Exit;
    ExtractURLEncodedValue(Params, 'submit', Submit);
    if SameText(Submit, 'OK') then begin
        // We have a new configuration confirmed
        if SessionData.ConfigPort <> '' then begin
            WebAppSrvDataModule.Port := SessionData.ConfigPort;
            WebAppSrvDataModule.SaveConfig;
        end;
        if SessionData.ConfigHasLogo then begin
            FileName := WebAppSrvDataModule.DataDir + SessionData.ConfigTempDir +
                        PathDelim + SessionData.UserCode + PathDelim + 'Logo.png';
            if (SessionData.ConfigTempDir <> '') and (FileExists(FileName)) then begin
                // Replace the existant logo image with the new one
                DeleteFile(WebAppSrvDataModule.ImagesDir + PathDelim + 'Logo.png');
                RenameFile(FileName,
                           WebAppSrvDataModule.ImagesDir + PathDelim + 'Logo.png');
                ForceRemoveDir(WebAppSrvDataModule.DataDir + SessionData.ConfigTempDir);
            end;
        end;
    end;
    Relocate('/');
    Finish;
end;

end.
