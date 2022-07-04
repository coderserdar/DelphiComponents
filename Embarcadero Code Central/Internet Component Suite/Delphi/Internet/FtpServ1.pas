{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Description:  This is a demo program showing how to use the TFtpServer
              component to build a FTP server.
              Waring: As this demo is writtent, full access is given to all
              users to all files accessible by the computer running the demo.
              In production program, you should add code to implement
              security issues.
Creation:     April 21, 1998
Version:      1.02
EMail:        francois.piette@pophost.eunet.be
              francois.piette@rtfm.be             http://www.rtfm.be/fpiette
Support:      Use the mailing list twsocket@rtfm.be See website for details.
Legal issues: Copyright (C) 1996, 1997, 1998 by François PIETTE
              Rue de Grady 24, 4053 Embourg, Belgium. Fax: +32-4-365.74.56
              <francois.piette@pophost.eunet.be>

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

History:
Apr 29, 1998  V0.90 Released for beta testing.
Apr 30, 1998  V0.91 Added an example of virtual file (see the code for
              FtpServer1RetrSessionConnected.
May 01, 1998  V0.92 Adapted for Delphi 1.0
May 03, 1998  V0.93 Adapted for Delphi 2.0 and C++Builder
May 04, 1998  V0.94 Added tools menu.
Jul 09, 1998  V1.00 Adapted for Delphi 4, removed beta status.
Jul 21, 1998  V1.01 Show how to refuse a client in OnClientConnected
Oct 25, 2000  V1.02 Added "List Clients" menu item and coding.


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit FtpServ1;

interface

uses
  WinTypes, WinProcs, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, IniFiles, FtpSrv, FtpSrvC, WSocket, StdCtrls, ExtCtrls, Menus;

const
  FtpServVersion      = 102;
  CopyRight : String  = ' FtpServer (c) 1998-2000 F. Piette V1.02 ';
  WM_APPSTARTUP       = WM_USER + 1;

type
  TLogMsg = class(TComponent)
  public
     procedure Text(Prefix : Char; Msg : String);
  end;

  TFtpServerForm = class(TForm)
    FtpServer1: TFtpServer;
    InfoMemo: TMemo;
    Panel1: TPanel;
    StartMinimizedCheckBox: TCheckBox;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    MnuStartServer: TMenuItem;
    MnuStopServer: TMenuItem;
    MnuQuit: TMenuItem;
    N1: TMenuItem;
    About1: TMenuItem;
    GreenImage: TImage;
    ClientCountLabel: TLabel;
    RedImage: TImage;
    Tools1: TMenuItem;
    Cleardisplay1: TMenuItem;
    MnuListClients: TMenuItem;
    N2: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FtpServer1ClientConnect(Sender: TObject;
      Client: TFtpCtrlSocket; Error: Word);
    procedure FtpServer1ClientDisconnect(Sender: TObject;
      Client: TFtpCtrlSocket; Error: Word);
    procedure FtpServer1Start(Sender: TObject);
    procedure FtpServer1Stop(Sender: TObject);
    procedure FtpServer1ClientCommand(Sender: TObject;
      Client: TFtpCtrlSocket; var Keyword, Params, Answer: TFtpString);
    procedure FtpServer1StorSessionConnected(Sender: TObject;
      Client: TFtpCtrlSocket; Data: TWSocket; Error: Word);
    procedure FtpServer1StorSessionClosed(Sender: TObject;
      Client: TFtpCtrlSocket; Data: TWSocket; Error: Word);
    procedure FtpServer1RetrDataSent(Sender: TObject;
      Client: TFtpCtrlSocket; Data: TWSocket; Error: Word);
    procedure FtpServer1RetrSessionConnected(Sender: TObject;
      Client: TFtpCtrlSocket; Data: TWSocket; Error: Word);
    procedure FtpServer1RetrSessionClosed(Sender: TObject;
      Client: TFtpCtrlSocket; Data: TWSocket; Error: Word);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FtpServer1AnswerToClient(Sender: TObject;
      Client: TFtpCtrlSocket; var Answer: TFtpString);
    procedure FtpServer1Authenticate(Sender: TObject;
      Client: TFtpCtrlSocket; UserName, Password: TFtpString;
      var Authenticated: Boolean);
    procedure FtpServer1ChangeDirectory(Sender: TObject;
      Client: TFtpCtrlSocket; Directory: TFtpString; var Allowed: Boolean);
    procedure MnuQuitClick(Sender: TObject);
    procedure MnuStopServerClick(Sender: TObject);
    procedure MnuStartServerClick(Sender: TObject);
    procedure ImagesDblClick(Sender: TObject);
    procedure FtpServer1BuildDirectory(Sender: TObject;
      Client: TFtpCtrlSocket; var Directory: TFtpString; Detailed: Boolean);
    procedure FtpServer1AlterDirectory(Sender: TObject;
      Client: TFtpCtrlSocket; var Directory: TFtpString; Detailed: Boolean);
    procedure Cleardisplay1Click(Sender: TObject);
    procedure MnuListClientsClick(Sender: TObject);
  private
    FInitialized      : Boolean;
    FIniFileName      : String;
    FPort             : String;
    FXTop             : Integer;
    FXLeft            : Integer;
    FXWidth           : Integer;
    FXHeight          : Integer;
    procedure WMAppStartup(var msg: TMessage); message WM_APPSTARTUP;
    procedure LoadConfig;
    procedure SaveConfig;
    procedure StartServer;
    procedure StopServer;
    procedure UpdateClientCount;
  end;

var
  FtpServerForm: TFtpServerForm;
  Log          : TLogMsg;

implementation

{$R *.DFM}

const
    MainTitle         = 'FTP Server - http://www.rtfm.be/fpiette';

    { Ini file layout }
    SectionData       = 'Data';
    KeyPort           = 'Port';

    SectionWindow     = 'Window';
    KeyTop            = 'Top';
    KeyLeft           = 'Left';
    KeyWidth          = 'Width';
    KeyHeight         = 'Height';
    KeyMinim          = 'RunMinimized';

    STATUS_GREEN      = 0;
    STATUS_YELLOW     = 1;
    STATUS_RED        = 2;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TLogMsg.Text(Prefix : Char; Msg : String);
begin
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServerForm.FormShow(Sender: TObject);
var
    IniFile : TIniFile;
    Minim   : Integer;
begin
    if not FInitialized then begin
        FInitialized        := TRUE;
        Caption             := 'Starting ' + MainTitle;
        Left := -Width;

        IniFile  := TIniFile.Create(FIniFileName);
        FXTop    := IniFile.ReadInteger(SectionWindow, KeyTop,    Top);
        FXLeft   := IniFile.ReadInteger(SectionWindow, KeyLeft,   Left);
        FXWidth  := IniFile.ReadInteger(SectionWindow, KeyWidth,  Width);
        FXHeight := IniFile.ReadInteger(SectionWindow, KeyHeight, Height);
        Minim    := IniFile.ReadInteger(SectionWindow, KeyMinim,  0);

        IniFile.Free;

        LoadConfig;
        SaveConfig;    { Create the inifile keys if they don't exists }

        { Be sure to always have the window visible }
        { with a reasonable width and height        }
        if FXLeft < 0 then
            FXLeft := 0;
        if FXTop < 0 then
            FXTop := 0;
        if FXWidth < 310 then
            FXWidth := 310;
        if FXHeight <= 250 then
            FXHeight := 250;
        if (FXLeft + FXWidth) > Screen.Width then
            FXLeft := Screen.Width - FXWidth;
        if (FXTop + FXHeight) > Screen.Height then
            FXTop := Screen.Height - FXHeight;

        StartMinimizedCheckBox.Checked := (Minim <> 0);

        { We use a custom message to initialize things once the form }
        { is visible                                                 }
        PostMessage(Handle, WM_APPSTARTUP, 0, 0);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServerForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
var
    IniFile : TIniFile;
    Minim   : Integer;
begin
    try
        StopServer;
        Minim   := ord(StartMinimizedCheckBox.Checked);
        IniFile := TIniFile.Create(FIniFileName);
        IniFile.WriteInteger(SectionWindow, KeyTop,    Top);
        IniFile.WriteInteger(SectionWindow, KeyLeft,   Left);
        IniFile.WriteInteger(SectionWindow, KeyWidth,  Width);
        IniFile.WriteInteger(SectionWindow, KeyHeight, Height);
        IniFile.WriteInteger(SectionWindow, KeyMinim,  Minim);
        IniFile.WriteString(SectionData,    KeyPort,   FPort);
        IniFile.Free;
    except
        { Ignore any exception when we are closing }
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServerForm.LoadConfig;
var
    IniFile : TIniFile;
begin
    IniFile := TIniFile.Create(FIniFileName);
    FPort   := IniFile.ReadString(SectionData,    KeyPort,   'ftp');
    IniFile.Free;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServerForm.SaveConfig;
var
    IniFile : TIniFile;
begin
    IniFile := TIniFile.Create(FIniFileName);
    IniFile.WriteString(SectionData, KeyPort, FPort);
    IniFile.Free;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This message handler is triggered by the FormShow event. We comes here    }
{ only when the form is visible on screen.                                  }
procedure TFtpServerForm.WMAppStartup(var msg: TMessage);
var
    PrvWnd  : HWND;
    Buf     : String;
begin
    if StartMinimizedCheckBox.Checked then
        Application.Minimize;
    Top    := FXTop;
    Left   := FXLeft;
    Width  := FXWidth;
    Height := FXHeight;

    { Prevent the server from running twice }
    Buf    := ClassName + #0;
    PrvWnd := FindWindow(@Buf[1], MainTitle);
    if PrvWnd <> 0 then begin
        Log.Text('E', 'Server already running. Shutdown.');
        Close;
        Exit;
    end;
    Caption := MainTitle;
    Update;                { It's nice to have the form completely displayed }

    StartServer;
    UpdateClientCount;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFNDEF VER80 }
{ To debug event driven programs, it is often handy to just use writeln to  }
{ write debug messages to the console. To get a console, just ask the       }
{ linker to build a console mode application. Then you'll get the default   }
{ console. The function below will make it the size you like...             }
procedure BigConsole(nCols, nLines : Integer);
var
    sc : TCoord;
    N  : DWord;
begin
    if not IsConsole then
        Exit;
    sc.x := nCols;
    sc.y := nLines;
    SetConsoleScreenBufferSize(GetStdHandle(STD_OUTPUT_HANDLE), sc);
    SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE),
                            BACKGROUND_BLUE or BACKGROUND_GREEN or
                            BACKGROUND_RED or BACKGROUND_INTENSITY);
    sc.x := 0;
    sc.y := 0;
    FillConsoleOutputAttribute(GetStdHandle(STD_OUTPUT_HANDLE),
                               BACKGROUND_BLUE or BACKGROUND_GREEN or
                               BACKGROUND_RED or BACKGROUND_INTENSITY,
                               nCols * nLines, sc, N);
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServerForm.FormCreate(Sender: TObject);
begin
    { Build Ini file name }
    FIniFileName := LowerCase(ExtractFileName(Application.ExeName));
    FIniFileName := Copy(FIniFileName, 1, Length(FIniFileName) - 3) + 'ini';
    { Create the Log object }
    Log := TLogMsg.Create(Self);

{$IFNDEF VER80}
{    BigConsole(80, 100); }
{$ENDIF}
    InfoMemo.Clear;
    GreenImage.Visible := FALSE;
    RedImage.Visible   := TRUE;
    RedImage.Top       := GreenImage.Top;
    RedImage.Left      := GreenImage.Left;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServerForm.StartServer;
begin
    GreenImage.Visible := FALSE;
    RedImage.Visible   := TRUE;
    Update;

    InfoMemo.Lines.Add(CopyRight);
    FtpServer1.Start;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServerForm.StopServer;
begin
    FtpServer1.Stop;
    FtpServer1.DisconnectAll;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServerForm.MnuQuitClick(Sender: TObject);
begin
    Close;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServerForm.MnuStopServerClick(Sender: TObject);
begin
    StopServer;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServerForm.MnuStartServerClick(Sender: TObject);
begin
    StartServer;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServerForm.ImagesDblClick(Sender: TObject);
begin
    if FtpServer1.Active then
        StopServer
    else
        StartServer;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServerForm.UpdateClientCount;
begin
    if FtpServer1.ClientCount = 0 then
        ClientCountLabel.Caption := 'No user'
    else
        ClientCountLabel.Caption := IntToStr(FtpServer1.ClientCount) + ' users';
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServerForm.FtpServer1ClientConnect(Sender: TObject;
  Client: TFtpCtrlSocket; Error: Word);
begin
    { The next test shows how to refuse a client }
    if Client.GetPeerAddr = '193.121.12.25' then begin
        Client.SendStr('421 Connection not allowed.' + #13#10);
        Client.Close;
        Exit;
    end;
    InfoMemo.Lines.Add('! ' + Client.GetPeerAddr + ' connected');
    UpdateClientCount;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServerForm.FtpServer1ClientDisconnect(Sender: TObject;
  Client: TFtpCtrlSocket; Error: Word);
begin
    InfoMemo.Lines.Add('! ' + Client.GetPeerAddr + ' disconnected');
    UpdateClientCount;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServerForm.FtpServer1Start(Sender: TObject);
begin
    GreenImage.Visible := TRUE;
    RedImage.Visible   := FALSE;
    InfoMemo.Lines.Add('! Server started');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServerForm.FtpServer1Stop(Sender: TObject);
begin
    GreenImage.Visible := FALSE;
    RedImage.Visible   := TRUE;
    InfoMemo.Lines.Add('! Server stopped');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServerForm.FtpServer1StorSessionConnected(Sender: TObject;
  Client: TFtpCtrlSocket; Data: TWSocket; Error: Word);
begin
    if Error <> 0 then
        InfoMemo.Lines.Add('! ' + Client.GetPeerAddr +
                           ' Data session failed to open. Error #' +
                           IntToStr(Error));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServerForm.FtpServer1StorSessionClosed(Sender: TObject;
  Client: TFtpCtrlSocket; Data: TWSocket; Error: Word);
begin
    if Error <> 0 then
        InfoMemo.Lines.Add('! ' + Client.GetPeerAddr +
                           ' Data session closed. Error #' + IntToStr(Error));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServerForm.FtpServer1RetrDataSent(Sender: TObject;
  Client: TFtpCtrlSocket; Data: TWSocket; Error: Word);
begin
    if Error <> 0 then
        InfoMemo.Lines.Add('! ' + Client.GetPeerAddr +
                           ' Data sent. Error #' + IntToStr(Error));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This event handler is called when the data session for a get file has     }
{ been opened. This is a good place build a file or a stream if the data    }
{ requested is not already stored in a file on the file system.             }
{ This feature is very powerfull and enable the FTP protocol to be used to  }
{ retrieve any kind of data. It this sample, we just check for C:\VIRTUAL   }
{ directory. If this directory is curent, then a TMemoryStream is created   }
{ on the fly with some data. If another directory is selected, the FTP      }
{ server works as any other: just send the requested file, if it exist !    }
{ This event handler is also a place where you can abort the file transfer. }
{ Simply trigger an exception and transfer will not take place.             }
{ Note that if you just wants to prohibe access to some directory or file,  }
{ the best place to code that is in the OnValidateGet or OnValidatePut      }
{ event handlers.                                                           }
procedure TFtpServerForm.FtpServer1RetrSessionConnected(Sender: TObject;
    Client : TFtpCtrlSocket;
    Data   : TWSocket;
    Error  : Word);
var
    Buf : String;
begin
    if Error <> 0 then
        InfoMemo.Lines.Add('! ' + Client.GetPeerAddr +
                           ' Data session connected. Error #' + IntToStr(Error))
    else if Copy(UpperCase(Client.FilePath), 1, 19) = 'C:\VIRTUAL\FORBIDEN' then
        raise Exception.Create('Access prohibed !')
    else if Copy(UpperCase(Client.FilePath), 1, 11) = 'C:\VIRTUAL\' then begin
        InfoMemo.Lines.Add('! VIRTUAL FILE');
        Client.UserData   := 1;        { Remember we created a stream }
        if Assigned(Client.DataStream) then
            Client.DataStream.Destroy; { Prevent memory leaks         }
        Client.DataStream := TMemoryStream.Create;
        Buf := 'This is a file created on the fly by the FTP server' + #13#10 +
               'It could result of a query to a database or anything else.' + #13#10 +
               'The request was: ''' + Client.FilePath + '''' + #13#10;
        Client.DataStream.Write(Buf[1], Length(Buf));
        Client.DataStream.Seek(0, 0);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServerForm.FtpServer1RetrSessionClosed(Sender: TObject;
  Client: TFtpCtrlSocket; Data: TWSocket; Error: Word);
begin
    if Error <> 0 then
        InfoMemo.Lines.Add('! ' + Client.GetPeerAddr +
                           ' Data session closed. Error #' + IntToStr(Error));
    if Client.UserData = 1 then begin
        { We created a stream for a virtual file or dir. Delete the TStream }
        if Assigned(Client.DataStream) then begin
            { There is no reason why we should not come here, but who knows ? }
            Client.DataStream.Destroy;
            Client.DataStream := nil;
        end;
        Client.UserData   := 0;     { Reset the flag }
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This event handler is called when the FTP component needs to build a      }
{ directory listing. You can just return without doing anything then the    }
{ component will build the directory for you, based on the actual disk      }
{ content. But you can also build your own directory listing with anything  }
{ you like in it. Just create a stream with the required content. The       }
{ example below construct a virtual directory when the user is on the       }
{ C:\VIRTUAL subdirectory (use elsewhere in this sample program).           }
procedure TFtpServerForm.FtpServer1BuildDirectory(
    Sender        : TObject;
    Client        : TFtpCtrlSocket;
    var Directory : TFtpString;
    Detailed      : Boolean);
var
    Buf : String;
begin
    if UpperCase(Client.Directory) <> 'C:\VIRTUAL\' then
        Exit;
    InfoMemo.Lines.Add('! VIRTUAL DIR');
    Client.UserData   := 1;        { Remember we created a stream }
    if Assigned(Client.DataStream) then
        Client.DataStream.Destroy; { Prevent memory leaks         }
    Client.DataStream := TMemoryStream.Create;
    if Detailed then
        { We need to format directory lines according to the Unix standard }
        Buf :=
      '-rwxrwxrwx   1 ftp      ftp            0 Apr 30 19:00 FORBIDEN' + #13#10 +
      '-rwxrwxrwx   1 ftp      ftp            0 Apr 30 19:00 TEST' + #13#10 +
      'drwxrwxrwx   1 ftp      ftp            0 Apr 30 19:00 SOME DIR' + #13#10
    else
        Buf := 'FORBIDEN' + #13#10 +
               'TEST' + #13#10;
    Client.DataStream.Write(Buf[1], Length(Buf));
    Client.DataStream.Seek(0, 0);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This event handler is called by the FTP component once it has built the   }
{ directory listing. We can use this handler to alter the listing, adding   }
{ or removing some info. This sample add the 'virtual' directory.           }
procedure TFtpServerForm.FtpServer1AlterDirectory(
    Sender        : TObject;
    Client        : TFtpCtrlSocket;
    var Directory : TFtpString;
    Detailed      : Boolean);
var
    Buf : String;
begin
    if UpperCase(Client.Directory) <> 'C:\' then
        Exit;
    { Add our 'virtual' directory to the list }
    if Detailed then begin
        { We need to format directory lines according to the Unix standard }
        Buf :=
        'drwxrwxrwx   1 ftp      ftp            0 Apr 30 19:00 VIRTUAL' + #13#10;
        Client.DataStream.Write(Buf[1], Length(Buf));
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServerForm.FtpServer1ClientCommand(Sender: TObject;
  Client: TFtpCtrlSocket; var Keyword, Params, Answer: TFtpString);
begin
    InfoMemo.Lines.Add('< ' + Client.GetPeerAddr + ' ' +
                       Keyword + ' ' + Params);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServerForm.FtpServer1AnswerToClient(Sender: TObject;
  Client: TFtpCtrlSocket; var Answer: TFtpString);
begin
    InfoMemo.Lines.Add('> ' + Client.GetPeerAddr + ' ' + Answer)
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServerForm.FtpServer1Authenticate(Sender: TObject;
  Client: TFtpCtrlSocket; UserName, Password: TFtpString;
  var Authenticated: Boolean);
begin
    { You should place here the code needed to authenticate the user. }
    { For example a text file with all permitted username/password.   }
    { If the user can't be authenticated, just set Authenticated to   }
    { false before returning.                                         }
    { It is also the right place to setup Client.HomeDir              }
    { If you need to store info about the client for later processing }
    { you can use Client.UserData to store a pointer to an object or  }
    { a record with the needed info.                                  }
    InfoMemo.Lines.Add('! ' + Client.GetPeerAddr +
                       ' User ''' + UserName + ''' is authenticated');
    if Password = 'bad' then
        Authenticated := FALSE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServerForm.FtpServer1ChangeDirectory(Sender: TObject;
  Client: TFtpCtrlSocket; Directory: TFtpString; var Allowed: Boolean);
begin
{$IFDEF NEVER}
    { It the right place to check if a user has access to a given directory }
    { The example below disable C:\ access to non root user.                }
    if (UpperCase(Client.UserName) <> 'ROOT') and
       (UpperCase(Client.Directory) = 'C:\') then
       Allowed := FALSE;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServerForm.Cleardisplay1Click(Sender: TObject);
begin
    InfoMemo.Clear;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServerForm.MnuListClientsClick(Sender: TObject);
var
    I : Integer;
begin
    if FtpServer1.ClientCount <= 0 then begin
        InfoMemo.Lines.Add('No client');
        Exit;
    end;

    for I := 0 to FtpServer1.ClientCount - 1 do begin
        InfoMemo.Lines.Add('Client ' + IntToStr(I + 1) + ': ' +
                           FtpServer1.Client[I].GetPeerAddr + '/' +
                           FtpServer1.Client[I].GetPeerPort);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.


