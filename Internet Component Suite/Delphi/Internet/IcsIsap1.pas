{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       Fran�ois PIETTE
Description:  ISAPI dll showing how to use ICS component to build extension
              to a webserver (note that ICS has a HTTP Server component which
              could be used to build a web application without a webserver).
              This ISAPI DLL is very basic. When invoked, it will open a FTP
              connection to the localhost with user/password root/root and
              retrieve the home directory list and return it as an answer
              to the client browser.
              To use this demo, you have to setup your webserver so that it is
              able to execute this ISAPI dll and so that there is a FTP server
              running on localhost and accepting user root with password root.
              The ICS sample FTP server is OK as well as Microsoft FTP server
              provided you configure it correctly.
              FTP parameters are hardcoded in DoTheJob procedure. In a real
              program, you probably would get them from the requesting URL.
              This sample program has been tested with Delphi 5 and IIS5 running
              on Windows 2000. Should work as well with other configurations...
Creation:     July 22, 2000
Version:      1.00
EMail:        http://users.swing.be/francois.piette  francois.piette@swing.be
              http://www.rtfm.be/fpiette             francois.piette@rtfm.be
              francois.piette@pophost.eunet.be
Support:      Use the mailing list twsocket@elists.org
              See http://www.rtfm.be/fpiette/supportuk.htm for subscription.
Legal issues: Copyright (C) 2000 by Fran�ois PIETTE
              Rue de Grady 24, 4053 Embourg, Belgium. Fax: +32-4-365.74.56
              <francois.piette@pophost.eunet.be><francois.piette@swing.be>

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
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit IcsIsap1;

{$B-}           { Enable partial boolean evaluation   }
{$T-}           { Untyped pointers                    }
{$X+}           { Enable extended syntax              }

interface

uses
  Windows, Messages, SysUtils, Classes, HTTPApp, FtpCli;

const
  IcsIsapi1Version          = 100;
  CopyRight    : String     = ' IcsIsapi (c) 2000 F. Piette V1.00 ';
  WM_START_MESSAGE = WM_USER + 1;

type
  TIcsIsapiWebModule = class(TWebModule)
    procedure WebModule1FtpWebActionItemAction(
                 Sender      : TObject;
                 Request     : TWebRequest;
                 Response    : TWebResponse;
                 var Handled : Boolean);
  end;

var
  IcsIsapiWebModule: TIcsIsapiWebModule;

implementation

{$R *.DFM}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure DoTheJob(FtpCli1 : TFtpClient);
var
    Buf : String;
begin
    try
        // Setup FTP parameters
        FtpCli1.HostName     := 'localhost';
        FtpCli1.Port         := 'ftp';
        FtpCli1.UserName     := 'root';
        FtpCli1.Password     := 'root';
        FtpCli1.HostFileName := '';
        FtpCli1.HostDirName  := '';
        try
            FtpCli1.Directory;   // This is a synchronous function !
            // Add some data to the directory list
            Buf := #13#10 + 'Generated by IcsIsapi' +
                   #13#10 + 'Visit http://www.rtfm.be/fpiette/indexuk.htm';
            FtpCli1.LocalStream.Write(Buf[1], Length(Buf));
        except
            on E:Exception do begin
                Buf := 'FTP Failed: ' + E.ClassName + ' ' + E.Message + #13#10;
                FtpCli1.LocalStream.Position := 0;
                FtpCli1.LocalStream.Write(Buf[1], Length(Buf));
            end;
        end;
    finally
        // Posting WM_QUIT will make GetMessage return FALSE and message pump
        // loop will terminate.
        PostMessage(0, WM_QUIT, 0, 0);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsIsapiWebModule.WebModule1FtpWebActionItemAction(
    Sender      : TObject;
    Request     : TWebRequest;
    Response    : TWebResponse;
    var Handled : Boolean);
var
    FtpCli1 : TFtpClient;
    MsgRec  : TMsg;
begin
    // Dynamically create an ICS FTP client component.
    // We can't use a static component because ISAPI are multithreaded
    FtpCli1 := TFtpClient.Create(nil);
    try
        // We use a stream for data, it is much easier and faster than a file
        FtpCli1.LocalStream := TMemoryStream.Create;
        try
            // This ISAPI is event driven (because ICS uses events)
            // So we have to start processing using a custom message
            // We pass the reference to FtpCli1 so that handler can do the job
            // We can't use a global variable because ISAPI is multithreaded
            Postmessage(0, WM_START_MESSAGE, 0, Integer(FtpCli1));

            // ICS Components need a working message pump because they are event
            // driven whithout message pump, nothing will happend at component
            // level. A message pump is simply a loop calling Windows API
            // GetMessage TranslateMessage and DisptachMessage
            // If GetMessage retrieves the WM_QUIT, the return value is FALSE
            // and the message loop is broken.                                           }
            while GetMessage(MsgRec, 0, 0, 0) do begin
                // Our custom message is a thread message (hwnd is nul)
                if (MsgRec.hwnd = 0) and (MsgRec.message = WM_START_MESSAGE) then
                    DoTheJob(TFtpClient(MsgRec.lParam))
                else begin
                    // Normal processing of any other message
                    TranslateMessage(MsgRec);
                    DispatchMessage(MsgRec)
                end;
            end;
            // We comes here after everything is done with FTP
            // DoTheJob has posted WM_QUIT message
            // We used the stream as an answer for http request
            FtpCli1.LocalStream.Position := 0;
            // Our answer is just plain text data
            Response.ContentType         := 'text/plain';
            Response.ContentStream       := FtpCli1.LocalStream;
            // Send to client browser
            Response.SendResponse;
        finally
            FtpCli1.LocalStream.Free;
        end;
    finally
        FtpCli1.Destroy;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.
