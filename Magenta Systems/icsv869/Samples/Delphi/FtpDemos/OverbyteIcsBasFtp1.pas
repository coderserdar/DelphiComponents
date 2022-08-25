{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Creation:     Aug 01, 2004
Description:  Basic FTP client program using ICS and demonstrating how to use
              asynchronous, event-driven programming.
Version:      1.01
EMail:        francois.piette@overbyte.be  http://www.overbyte.be
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 2004-2010 by François PIETTE
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
Aug 08, 2004 V1.01 Make use of IcsUrl unit

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsBasFtp1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  OverbyteIcsIniFiles, StdCtrls, ExtCtrls, OverbyteIcsFtpCli, OverbyteIcsUrl,
  OverbyteIcsWndControl;

type
  TBasicFtpClientForm = class(TForm)
    ToolsPanel: TPanel;
    DisplayMemo: TMemo;
    FtpClient1: TFtpClient;
    GetButton: TButton;
    FtpUrlEdit: TEdit;
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure GetButtonClick(Sender: TObject);
    procedure FtpClient1RequestDone(Sender: TObject; RqType: TFtpRequest;
      ErrCode: Word);
    procedure FtpClient1Display(Sender: TObject; var Msg: String);
  private
    FIniFileName : String;
    FInitialized : Boolean;
    FProto, FUser, FPass, FHost, FPort, FPath, FFileName : String;
    FLocalFileName : String;
    FSuccess       : Boolean;
    FErrorMessage  : String;
  public
    procedure Display(Msg : String);
    property IniFileName : String read FIniFileName write FIniFileName;
  end;

var
  BasicFtpClientForm: TBasicFtpClientForm;

implementation

{$R *.DFM}

const
    SectionWindow      = 'Window';   // Must be unique for each window
    KeyTop             = 'Top';
    KeyLeft            = 'Left';
    KeyWidth           = 'Width';
    KeyHeight          = 'Height';
    SectionData        = 'Data';
    KeyURL             = 'URL';


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TBasicFtpClientForm.FormCreate(Sender: TObject);
begin
    FIniFileName := OverbyteIcsIniFiles.GetIcsIniFileName;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TBasicFtpClientForm.FormShow(Sender: TObject);
var
    IniFile : TIcsIniFile;
begin
    if not FInitialized then begin
        FInitialized := TRUE;
        IniFile      := TIcsIniFile.Create(FIniFileName);
        try
            Width        := IniFile.ReadInteger(SectionWindow, KeyWidth,  Width);
            Height       := IniFile.ReadInteger(SectionWindow, KeyHeight, Height);
            Top          := IniFile.ReadInteger(SectionWindow, KeyTop,
                                            (Screen.Height - Height) div 2);
            Left         := IniFile.ReadInteger(SectionWindow, KeyLeft,
                                            (Screen.Width  - Width)  div 2);
            FtpUrlEdit.Text := IniFile.ReadString(SectionData, KeyURL,
            'ftp://anonymous:guest@unknown@ftp.simtel.net' +
            '/pub/simtelnet/simtel.html');
        finally
            IniFile.Free;
        end;
        DisplayMemo.Clear;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TBasicFtpClientForm.FormClose(Sender: TObject; var Action: TCloseAction);
var
    IniFile : TIcsIniFile;
begin
    IniFile := TIcsIniFile.Create(FIniFileName);
    try
        IniFile.WriteInteger(SectionWindow, KeyTop,         Top);
        IniFile.WriteInteger(SectionWindow, KeyLeft,        Left);
        IniFile.WriteInteger(SectionWindow, KeyWidth,       Width);
        IniFile.WriteInteger(SectionWindow, KeyHeight,      Height);
        IniFile.WriteString(SectionData, KeyURL, FtpUrlEdit.Text);
        IniFile.UpdateFile;
    finally
        IniFile.Free;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TBasicFtpClientForm.Display(Msg : String);
begin
    DisplayMemo.Lines.BeginUpdate;
    try
        if DisplayMemo.Lines.Count > 200 then begin
            while DisplayMemo.Lines.Count > 200 do
                DisplayMemo.Lines.Delete(0);
        end;
        DisplayMemo.Lines.Add(Msg);
    finally
        DisplayMemo.Lines.EndUpdate;
        SendMessage(DisplayMemo.Handle, EM_SCROLLCARET, 0, 0);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TBasicFtpClientForm.FtpClient1Display(
    Sender  : TObject;
    var Msg : String);
begin
    Display(Msg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TBasicFtpClientForm.GetButtonClick(Sender: TObject);
var
    I : Integer;
begin
    DisplayMemo.Clear;
    ParseURL(FtpUrlEdit.Text, FProto, FUser, FPass, FHost, FPort, FPath);
    Display('Proto = "' + FProto + '"');
    Display('User  = "' + FUser  + '"');
    Display('Pass  = "' + FPass  + '"');
    Display('Host  = "' + FHost  + '"');
    Display('Port  = "' + FPort  + '"');
    Display('Path  = "' + FPath  + '"');
    if FProto = '' then
        FProto := 'ftp';
    if FPort = '' then
        FPort := 'ftp';
    if CompareText(FProto, 'ftp') <> 0 then begin
        Display('Only FTP protocol is supported by this demo !');
        Exit;
    end;
    if (FPath = '') or (FPath = '/') then begin
        Display('Please specify a path to get !');
        Exit;
    end;
    { Find the last occurenc of path delimiter }
    I := Posn('/', FPath, -1);
    if I <= 0 then begin
        FFileName := FPath;
        FPath     := '';
    end
    else begin
        FFileName := Copy(FPath, I + 1, Length(FPath));
        FPath     := Copy(FPath, 2, I - 2);
    end;
    Display('FileName = "' + FFileName + '"');
    Display('Path     = "' + FPath     + '"');

    { We use a temporary filename for local file, in current directory }
    FLocalFileName      := 'ftp_tmp_' + FFileName;
    DeleteFile(FLocalFileName);
    { Reset flag to remeber success or failure }
    FSuccess      := FALSE;
    FErrorMessage := '';
    { Setup FTP component }
    FtpClient1.UserName := FUser;
    FtpClient1.Password := FPass;
    FtpClient1.HostName := FHost;
    FtpClient1.Binary   := TRUE;
    FtpClient1.Passive  := TRUE;
    { Start download chain by opening the connection }
    FtpClient1.OpenAsync;
    { OpenAsync is non-blocking, execution continue while the component is  }
    { connecting in the background. You can therefore still resize and move }
    { the application windows, as well as close the application.            }
    { Eventually, the connection is established. Execution will continue    }
    { in the OnRequestDone event handler (see below).                       }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This event is called when the FTP component has done what operation you   }
{ requested. It's where you can check for error and start a new operation   }
{ to be executed. The example below implement some kind of finite state     }
{ machine where the state variable is within FTP component and accessible   }
{ thru the RqType argument which tells you what operation has been          }
{ completed.                                                                }
procedure TBasicFtpClientForm.FtpClient1RequestDone(
    Sender  : TObject;
    RqType  : TFtpRequest;
    ErrCode : Word);
begin
    Display('OnRequestDone. RqType = ' + IntToStr(Ord(RqType)) + '    ' +
            'ErrCode = ' + IntToStr(ErrCode));
    if ErrCode <> 0 then begin
        FErrorMessage := FtpClient1.ErrorMessage;
        Display('*** ERROR ' + IntToStr(ErrCode) + ' ***');
        Display('*** ' + FErrorMessage + ' ***');
        if RqType <> ftpOpenAsync then
            FtpClient1.QuitAsync
        else
            Display('Done.');
        Exit;
    end;
    case RqType of
    ftpOpenAsync: FtpClient1.UserAsync;
    ftpUserAsync: FtpClient1.PassAsync;
    ftpPassAsync: if FPath = '' then
                      FtpClient1.TypeSetAsync
                  else begin
                      FtpClient1.HostDirName := FPath;
                      FtpClient1.CwdAsync;
                  end;
    ftpCwdAsync:  FtpClient1.TypeSetAsync;
    ftpTypeSetAsync:
                  begin
                      FtpClient1.HostFileName  := FFileName;
                      FtpClient1.LocalFileName := FLocalFileName;
                      FtpClient1.GetAsync;
                  end;
    ftpGetAsync:  begin
                      FSuccess := (FtpClient1.StatusCode = 226);
                      FtpClient1.QuitAsync;
                  end;
    ftpQuitAsync: begin
                      Display('');
                      if FSuccess then
                          Display('File downloaded into "' + FLocalFileName + '"')
                      else begin
                          DeleteFile(FLocalFileName);
                          Display(FErrorMessage);
                      end;
                      Display('Done.');
                  end;
    else
        Display('*** Unknown RqType ' + IntToStr(Ord(RqType)) + ' ***');
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.
