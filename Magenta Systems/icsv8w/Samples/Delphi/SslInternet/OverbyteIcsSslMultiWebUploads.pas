{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       Angus Robertson
Creation:     July 6, 2017
Description:  This is an posted data demo, taken from the original web server
              sample that had dedicated code to handle POST requests
Version:      8.69
EMail:        francois.piette@overbyte.be  http://www.overbyte.be
Support:      https://en.delphipraxis.net/forum/37-ics-internet-component-suite/
Legal issues: Copyright (C) 2003-2022 by François PIETTE
              Rue de Grady 24, 4053 Embourg, Belgium.

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
July 6, 2017  V8.49 baseline
May 20, 2022 V8.69 - Recognise more MIME types as download files, more logging.
                   Using properties from OverbyteIcsSslMultiWebDataModule so this
                     unit is not dependent upon a single application, and works in
                     DDWebService.


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsSslMultiWebUploads;

interface

uses
  {$IFDEF MSWINDOWS}
    Windows,
  {$ENDIF}
    Classes, SysUtils, OverbyteIcsUrl, OverbyteIcsHttpSrv,
    OverbyteIcsTicks64, OverbyteIcsUtils, OverbyteIcsFtpSrvT,
    OverbyteIcsHttpAppServer,
    OverbyteIcsFormDataDecoder,
    OverbyteIcsSslMultiWebDataModule;

type
    TUploadDisplayEvent = procedure (Sender : TObject;
                                        const Msg : String) of object;

    TUrlHandlerUploadData = class(TUrlHandler)
    public
        procedure Execute; override;
    end;

    TUrlHandlerUploadFile = class(TUrlHandler)
    public
        procedure Execute; override;
        procedure DecoderDisplay (Sender : TObject; const Msg : String);
    end;


implementation

procedure TUrlHandlerUploadData.Execute;
var
    Stream    : TStream;
    FileName  : String;
    FirstName : String;
    LastName  : String;
    HostName  : String;
    Buf       : String;
    Bom       : array[0..2] of Byte;
    IsUtf8    : Boolean;
    Len       : Integer;
    Utf8Str   : AnsiString;
begin
    if Client.Method = 'POST' then begin

        { Extract fields from posted data. }
        ExtractURLEncodedValue(String(Client.PostedData), 'FirstName', FirstName);
        ExtractURLEncodedValue(String(Client.PostedData), 'LastName',  LastName);
        { Get client IP address. We could to ReverseDnsLookup to get hostname }
        HostName := Client.PeerAddr;
        { Build the record to write to data file }
        Buf      := FormatDateTime('YYYYMMDD HHNNSS ', Now) +
                    FirstName + '.' + LastName + '@' + HostName + #13#10;

        { Save data to a text file }
        FileName := IncludeTrailingPathDelimiter(SslMultiWebDataModule.UploadDir) + 'FormHandler.txt';
        try
            if FileExists(FileName) then begin
                Stream := TFileStream.Create(FileName, fmOpenReadWrite);
                { Check whether the data file is UTF-8 encoded }
                Len := Stream.Read(Bom[0], SizeOf(Bom));
                IsUtf8 := (Len = 3) and (Bom[0] = $EF) and (Bom[1] = $BB) and
                          (Bom[2] = $BF);
                Stream.Seek(0, soFromEnd);
            end
            else begin
                { We use UTF-8 by default for new data files }
                Stream := TFileStream.Create(FileName, fmCreate);
                IsUtf8 := TRUE;
                Bom[0] := $EF; Bom[1] := $BB; Bom[2] := $BF;
                Stream.Write(Bom[0], SizeOf(Bom));
            end;
            if IsUtf8 then begin
                Utf8Str := StringToUtf8(Buf);
                Stream.Write(PAnsiChar(Utf8Str)^, Length(Utf8Str));
            end
            else
                StreamWriteStrA(Stream, Buf);
            Stream.Destroy;
        except
            on E:Exception do
                SslMultiWebDataModule.Display('Exception Saving Posted Data - ' + E.Message) ;
        end;

        { Here is the place to check for valid input data and produce a HTML }
        { answer according to data validation.                               }
        { Here for simplicity, we don't check data and always produce the    }
        { same HTML answer.                                                  }

        AnswerString(
            '',           { Default Status '200 OK'         }
            '',           { Default Content-Type: text/html }
            '',           { Default header                  }
            '<HTML>' +
              '<HEAD>' +
                '<TITLE>ICS WebServer Form Demo</TITLE>' +
              '</HEAD>' + #13#10 +
              '<BODY>' +
                '<H2>Your data has been recorded:</H2>' + #13#10 +
                '<P>' + TextToHtmlText(FirstName) + '.' +
                        TextToHtmlText(LastName)  + '@' +
                        TextToHtmlText(HostName)  +'</P>' +
                '<P>Filename: ' + TextToHtmlText(FileName)  +'</P>' +
                '<A HREF="/form.html">More data entry</A><BR>' +
                '<A HREF="/FormData.html">View data file</A><BR>' +
                '<A HREF="/demo.html">Back to demo menu</A><BR>' +
              '</BODY>' +
            '</HTML>');
        Finish;
    end;
end;

// upload a file to the server

procedure TUrlHandlerUploadFile.Execute;
const
  MAX_UPLOAD_SIZE    = 1024 * 1024 * 60; // Accept max 60MB file
var
    sinfo1, sPageUrl, sContent: string ;
    newfilename, sFileName, sfiletitle: string ;
    Decoder: TFormDataAnalyser;
    Field: TFormDataItem;
    FileStream: TFileStream;
    MemoryStream: TMemoryStream;
    RemoteClient: THttpAppSrvConnection;
    UploadTotTicks: LongWord ;

    procedure Logit (fsize: Int64);
    var
        Speed: LongWord ;
        Duration: string ;
    begin
        try
            sinfo1 :=  'Saved Uploaded File OK' + #13#10 + 'New File Name: ' + newfilename + #13#10 +
                                                                          'File Size: ' + IntToStr (fsize) ;
            if (UploadTotTicks >= 1000) then
                Duration :=  FloatToStrF (UploadTotTicks / 1000, ffFixed, 15, 2) + ' secs'
            else
                Duration := IntToStr (UploadTotTicks) + ' msecs' ;
            speed := 0 ;
            if (UploadTotTicks > 0) and (UploadTotTicks < 60*60*1000) and (fsize > 1000) then begin
                if (UploadTotTicks > 100000) and (fsize > 1000000) then begin
                    UploadTotTicks := UploadTotTicks div 1000 ;  // allow for bizarre divide by zero error
                    speed := fsize div UploadTotTicks
                end
                else
                    speed := (fsize * 1000) div UploadTotTicks ;
            end;
            sinfo1 := sinfo1 + #13#10 + 'Upload Duration: ' + duration + #13#10 +
                                                     'Speed: ' + IntToStr (speed) + ' chars/sec' ;
        except
        end;
    end ;

begin
    RemoteClient := THttpAppSrvConnection(Client) ;
    sinfo1 := '' ;
    sFileName := '' ;
    sfiletitle := '' ;
    UploadTotTicks := IcsElapsedTicks64 (RemoteClient.RequestStartTick) + 1 ;
    sPageUrl := Client.RequestProtocol + '://' + Client.RequestHost + Client.Path ;

    if RemoteClient.Method = 'GET' then  begin
        if Params <> '' then begin   // not really used !!
            ExtractURLEncodedValue (Params, 'FileName', sFileName) ;
            ExtractURLEncodedValue (Params, 'FileTitle', sfiletitle) ;
        end;
    end ;

// see if page is being POSTed by itself to upload a file
    if (RemoteClient.Method = 'POST') or (RemoteClient.Method = 'PUT')  then begin  { V6.69 added PUT }
        sContent := Lowercase(RemoteClient.RequestContentType);  { V8.69 }
        Display ('Received Post Data File, Size ' + IntToStr (RemoteClient.PostedDataLen) + ', Content Type: ' + sContent) ;
        if (RemoteClient.PostedDataLen > MAX_UPLOAD_SIZE) then begin
             sinfo1 := 'Upload File (' + IntToKbyte (RemoteClient.PostedDataLen) + ') Exceeds Maximum Size' ;
        end
        else begin
         // First we must tell the component that we've got all the data
            RemoteClient.PostedDataReceived;
            try
                // now see how the file was uploaded
                if Pos('multipart/form-data', sContent) > 0 then begin
                    MemoryStream := TMemoryStream.Create ;
                    try
                        MemoryStream.WriteBuffer (RemoteClient.PostedData^, RemoteClient.PostedDataLen) ;
                        MemoryStream.Seek(0, 0);
                        Decoder := TFormDataAnalyser.Create(nil);
                        try
                            Decoder.OnDisplay := DecoderDisplay;
                            Decoder.DecodeStream (MemoryStream) ;

                            // Extract file, do a minimal validity check
                            Field := Decoder.Part ('FileName');
                            if not Assigned (Field) then
                                sinfo1 := 'Upload Form Error, Missing FileName Tag'
                            else  begin
                                sFileName := ExtractFileName(Field.ContentFileName);
                                if sFileName = '' then
                                    sinfo1 := 'Upload Form Error, Empty FileName'
                                else if Field.DataLength <= 0 then
                                    sinfo1 := 'Upload Form Error, File Empty'
                                else if ((Pos('/', sFileName) > 0) or
                                           (Pos('\', sFileName) > 0) or
                                           (Pos(':', sFileName) > 0)) then
                                    sinfo1 := 'Illegal Upload File Name: ' + sFileName
                                else begin
                                    try
                                // create a new file name with date and time
                                        newfilename := IncludeTrailingPathDelimiter(SslMultiWebDataModule.UploadDir) +           { V8.69 }
                                                                 FormatDateTime('yyyymmdd"-"hhnnss', Now) + '_' + sFileName;
                                        Display ('Saving MIME Upload File as ' + newfilename);   { V8.69 }
                                        Field.SaveToFile (newfilename);
                                        Logit (Field.DataLength) ;
                                    except
                                        on E:Exception do
                                            sinfo1 := 'Failed to Save MIME Uploaded File as ' + newfilename + ' - ' + E.Message;
                                     end;
                                end;
                            end;
                            Field := Decoder.Part ('FileTitle');
                            if Assigned (Field) then sfiletitle := Field.AsString;
                        finally
                            FreeAndNil(Decoder);
                        end;
                    finally
                        FreeAndNil (MemoryStream) ;
                    end;
                end
                else if (Pos('application', sContent) > 0) or (Pos('audio', sContent) > 0) or (Pos('image', sContent) > 0) then begin { V8.69 }
                    if Params <> '' then begin
                        ExtractURLEncodedValue (Params, 'FileName', sFileName) ;
                        ExtractURLEncodedValue (Params, 'FileTitle', sfiletitle) ;
                    end;
                    if sFileName = '' then
                        sinfo1 := 'Upload Form Error, Empty FileName'
                    else if ((Pos('/', sFileName) > 0) or
                               (Pos('\', sFileName) > 0) or
                               (Pos(':', sFileName) > 0)) then
                        sinfo1 := 'Illegal Upload File Name: ' + sFileName
                    else begin
                        newfilename := IncludeTrailingPathDelimiter(SslMultiWebDataModule.UploadDir) +     { V8.69 }
                                 IncludeTrailingPathDelimiter (newfilename) +
                                            FormatDateTime('yyyymmdd"-"hhnnss', Now) + '_' + sFileName;
                        SslMultiWebDataModule.Display('Saving Simple Upload File as ' + newfilename);   { V8.69 }
                        try
                            FileStream := TFileStream.Create (newfilename, fmCreate) ;
                            try
                                FileStream.WriteBuffer (RemoteClient.PostedData^, RemoteClient.PostedDataLen);
                            finally
                                FreeAndNil (FileStream);
                            end;
                            Logit (RemoteClient.PostedDataLen) ;
                        except
                            on E:Exception do
                             sinfo1 := 'Failed to Save Uploaded File as ' + newfilename + ' - ' + E.Message ;
                        end;
                    end;
                end
                else
           // We don't accept any other request
                   sinfo1 := 'Unknown Post Data Content: ' + RemoteClient.RequestContentType ;
            except
                on E:Exception do
                    Display ('Exception Saving Posted Data - ' + E.Message) ;
            end;
            sinfo1 := sinfo1 + #13#10 +
                    'Upload FileName: ' + sFileName + #13#10 +
                    'FileTitle: ' + sfiletitle + #13#10 +
                    'Post URL: ' + sPageUrl + #13#10 +
                    'From IP Address: ' + RemoteClient.CPeerAddr ;
        end;
    end;
    SslMultiWebDataModule.Display(sinfo1);
    sinfo1 := StringReplace (sinfo1, #13#10, '<br>', [rfReplaceAll]);
    AnswerPage('', '', '\uploadfile.html', nil,
             ['sinfo1', sinfo1, 'sPageUrl', sPageUrl, 'sMaxFileSize', IntToKByte (MAX_UPLOAD_SIZE),
              'sFileName', sFileName, 'sFileTitle', sfiletitle
               ]);
    Finish;
end;

procedure TUrlHandlerUploadFile.DecoderDisplay (Sender : TObject; const Msg : String);
begin
    SslMultiWebDataModule.Display(Msg);
end;

end.

