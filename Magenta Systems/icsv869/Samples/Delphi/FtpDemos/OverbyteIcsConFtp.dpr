{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *


Author:       François PIETTE
Object:       This demo shows how to use TFtpCli component within a console
              mode application. It connect to ftp.microsoft.com and download
              readme.txt from /softlib directory. The local file is ConFtp.txt.
Creation:     Dec 24, 2001
Version:      1.00
EMail:        http://www.overbyte.be        francois.piette@overbyte.be
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 2001-2010 by François PIETTE
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

Updates:
Jul 19, 2008 F.Piette made some changes for Unicode


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
program OverbyteIcsConFtp;

{$R '..\..\OverbyteIcsCommonVersion.res'  '..\..\OverbyteIcsCommonVersion.rc'}

{$IFDEF VER80}
Bomb('Sorry, Delphi 1 does not support console mode programs');
{$ENDIF}
{$APPTYPE CONSOLE}
{$IFNDEF NOFORMS}
    Bomb('Please add NOFORMS to your project defines');
{$ENDIF}

uses
  Windows, Messages, SysUtils, Classes,
  OverbyteIcsFtpCli,
  OverbyteIcsWinsock;

const
    ConFtpVersion  = 100;
    CopyRight      = ' ConFtp (c) 2001-2010 by Francois PIETTE. V1.00';

type
    { We use TConApplication class (actually a component) to encapsulate all }
    { the work to be done. This is easier because TFtpCli is event driven    }
    { and need methods (that is procedure of object) to handle events.       }
    TConApplication = class(TComponent)
    protected
        FFtpCli     : TFtpClient;
        FResult     : Integer;
        procedure FtpRequestDone(Sender    : TObject;
                                 RqType    : TFtpRequest;
                                 ErrorCode : Word);
        procedure FtpDisplay(Sender    : TObject;
                             var Msg   : String);
    public
        constructor Create(AOwner: TComponent); override;
        destructor  Destroy; override;
        procedure   Execute;
    end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TConApplication.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);
    FFtpCli := TFtpClient.Create(Self);
    FResult := 0;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TConApplication.Destroy;
begin
    if Assigned(FFtpCli) then begin
        FFtpCli.Destroy;
        FFtpCli := nil;
    end;
    inherited Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TConApplication.Execute;
begin
    { Prepare connection to Ftp server }
    FFtpCli.HostName      := 'ftp.microsoft.com';
    FFtpCli.Port          := 'ftp';
    FFtpCli.HostDirName   := '/softlib';
    FFtpCli.HostFileName  := 'readme.txt';
    FFtpCli.LocalFileName := 'ConFtp.txt';
    FFtpCli.Binary        := TRUE;
    FFtpCli.UserName      := 'anonymous';
    FFtpCli.Password      := 'your.name@your.domain.com';
    FFtpCli.OnDisplay     := FtpDisplay;
    FFtpCli.OnRequestDone := FtpRequestDone;

    { Delete existing file }
    DeleteFile(FFtpCli.LocalFileName);

    { Start FTP transfert by connecting to the server }
    WriteLn('Connecting to ', FFtpCli.HostName, '/', FFtpCli.Port);
    FFtpCli.OpenAsync;

    { We need a message loop in order for windows message processing to work. }
    { There is a message loop built into each TWSocket, so we use the one in  }
    { TFtpCli control socket.                                                 }
    { MessageLoop will exit only when WM_QUIT message is posted. We do that   }
    { form the OnRequestDone event handler when the component has finished.   }
    FFtpCli.ControlSocket.MessageLoop;
    WriteLn('Going back to the OS');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TConApplication.FtpDisplay(Sender: TObject; var Msg: String);
begin
    WriteLn(Msg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TConApplication.FtpRequestDone(
    Sender    : TObject;
    RqType    : TFtpRequest;
    ErrorCode : Word);
var
    EndFlag : Boolean;
begin
    EndFlag := FALSE;

    { Check status }
    if ErrorCode <> 0 then begin
        WriteLn('Failed, error #' + IntToStr(ErrorCode));
        FFtpCli.Abort;
        EndFlag := TRUE;
    end
    else begin
        case RqType of
        ftpOpenAsync :
            begin
                FFtpCli.UserAsync;
            end;
        ftpUserAsync :
            begin
                FFtpCli.PassAsync;
            end;
        ftpPassAsync :
            begin
                FFtpCli.CwdAsync;
            end;
        ftpCwdAsync :
            begin
                FFtpCli.TypeSetAsync;
            end;
        ftpTypeSetAsync:
            begin
                FFtpCli.GetAsync;
            end;
        ftpGetAsync :
            begin
                FResult := FFtpCli.StatusCode;
                FFtpCli.QuitAsync;
            end;
        ftpQuitAsync :
            begin
                EndFlag := TRUE;
            end;
        else
            begin
                WriteLn('Unknown FtpRequest ' + IntToStr(Ord(RqType)));
                EndFlag := TRUE;
            end;
        end;
    end;

    { If something wrong or end of job, then go back to the OS }
    if EndFlag then begin
        if FResult = 226 then
            WriteLn('Transfert succesful. File is ', FFtpCli.LocalFileName)
        else
            WriteLn('Transfert failed !');
        { Prompt the user }
        WriteLn('Hit ENTER key to return quit program...');
        ReadLn;
        { Break message loop we called from the execute method }
        FFtpCli.ControlSocket.PostQuitMessage;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
var
    ConApp : TConApplication;
begin
    WriteLn(CopyRight);
    WriteLn;
    ConApp := TConApplication.Create(nil);
    try
        ConApp.Execute;
    finally
        ConApp.Destroy;
    end;
end.

