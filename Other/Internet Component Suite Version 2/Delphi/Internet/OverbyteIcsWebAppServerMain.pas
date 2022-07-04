{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Creation:     April 11, 2009
Description:  WebAppServer is a demo application showing the HTTP application
              server component (THttpAppSrv).
Version:      1.00
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


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsWebAppServerMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  IniFiles, StdCtrls, ExtCtrls,
  OverbyteIcsWndControl,
  OverbyteIcsWebSession,
  OverbyteIcsHttpSrv,
  OverbyteIcsHttpAppServer,
  OverbyteIcsWebAppServerDataModule,
  OverbyteIcsWebAppServerSessionData,
  OverbyteIcsWebAppServerConfig,
  OverbyteIcsWebAppServerUrlDefs,
  OverbyteIcsWebAppServerHomePage,
  OverbyteIcsWebAppServerHelloWorld,
  OverbyteIcsWebAppServerCounter,
  OverbyteIcsWebAppServerLogin,
  OverbyteIcsWebAppServerCounterView;

const
  WM_APPSTARTUP = WM_USER + 1;

type
  TWebAppSrvForm = class(TForm)
    ToolsPanel: TPanel;
    DisplayMemo: TMemo;
    HttpAppSrv1: THttpAppSrv;
    HousekeepingTimer: TTimer;
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure HttpAppSrv1ClientConnect(Sender, Client: TObject; Error: Word);
    procedure HttpAppSrv1GetDocument(Sender, Client: TObject;
                                     var Flags: THttpGetFlag);
    procedure HttpAppSrv1ServerStarted(Sender: TObject);
    procedure HttpAppSrv1ServerStopped(Sender: TObject);
    procedure HttpAppSrv1DeleteSession(Sender: TObject; Session: TWebSession);
    procedure HousekeepingTimerTimer(Sender: TObject);
  private
    FIniFileName : String;
    FInitialized : Boolean;
    FDataDir     : String;
    FSessionFile : String;
    procedure WMAppStartup(var msg: TMessage); message WM_APPSTARTUP;
  public
    procedure Display(const Msg : String);
    procedure DisplayHandler(Sender : TObject; const Msg : String);
    property IniFileName : String read FIniFileName write FIniFileName;
  end;

var
  WebAppSrvForm: TWebAppSrvForm;

implementation

{$R *.DFM}

const
    SectionWindow      = 'Window';   // Must be unique for each window
    KeyTop             = 'Top';
    KeyLeft            = 'Left';
    KeyWidth           = 'Width';
    KeyHeight          = 'Height';


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWebAppSrvForm.FormCreate(Sender: TObject);
begin
    FIniFileName := ChangeFileExt(Application.ExeName, '.ini');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWebAppSrvForm.FormShow(Sender: TObject);
var
    IniFile : TIniFile;
begin
    if not FInitialized then begin
        FInitialized := TRUE;

        IniFile      := TIniFile.Create(FIniFileName);
        Width        := IniFile.ReadInteger(SectionWindow, KeyWidth,  Width);
        Height       := IniFile.ReadInteger(SectionWindow, KeyHeight, Height);
        Top          := IniFile.ReadInteger(SectionWindow, KeyTop,
                                            (Screen.Height - Height) div 2);
        Left         := IniFile.ReadInteger(SectionWindow, KeyLeft,
                                            (Screen.Width  - Width)  div 2);
        IniFile.Destroy;
        DisplayMemo.Clear;
        PostMessage(Handle, WM_APPSTARTUP, 0, 0);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWebAppSrvForm.HousekeepingTimerTimer(Sender: TObject);
begin
    CleanupTimeStampedDir(WebAppSrvDataModule.DataDir);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWebAppSrvForm.HttpAppSrv1ClientConnect(
    Sender, Client: TObject;
    Error: Word);
var
    ClientCnx : THttpAppSrvConnection;
begin
    ClientCnx                := Client as THttpAppSrvConnection;
    ClientCnx.WSessionCookie := 'OverbyteIcsWebAppServer' + HttpAppSrv1.Port;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWebAppSrvForm.HttpAppSrv1DeleteSession(
    Sender: TObject;
    Session: TWebSession);
var
    MySessionData : TAppSrvSessionData;
begin
    MySessionData := Session.SessionData as TAppSrvSessionData;
    Display('Session for user "' + MySessionData.UserCode + '" timed out');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWebAppSrvForm.HttpAppSrv1GetDocument(
    Sender, Client: TObject;
    var Flags: THttpGetFlag);
var
    ClientCnx : THttpAppSrvConnection;
begin
    ClientCnx := Client as THttpAppSrvConnection;
    if ClientCnx.Params = '' then
        Display(ClientCnx.PeerAddr + ' ' + ClientCnx.Path)
    else
        Display(ClientCnx.PeerAddr + ' ' + ClientCnx.Path + '?' + ClientCnx.Params);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWebAppSrvForm.HttpAppSrv1ServerStarted(Sender: TObject);
begin
    Display('Waiting for client on port ' + HttpAppSrv1.Port);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWebAppSrvForm.HttpAppSrv1ServerStopped(Sender: TObject);
begin
    HttpAppSrv1.WSessions.SaveToFile(FSessionFile);
    CleanupTimeStampedDir(WebAppSrvDataModule.DataDir);
    Display('Server is now stopped');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWebAppSrvForm.FormClose(Sender: TObject; var Action: TCloseAction);
var
    IniFile : TIniFile;
begin
    HttpAppSrv1.Stop;
    IniFile := TIniFile.Create(FIniFileName);
    try
        IniFile.WriteInteger(SectionWindow, KeyTop,         Top);
        IniFile.WriteInteger(SectionWindow, KeyLeft,        Left);
        IniFile.WriteInteger(SectionWindow, KeyWidth,       Width);
        IniFile.WriteInteger(SectionWindow, KeyHeight,      Height);
    finally
        IniFile.Destroy;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWebAppSrvForm.Display(const Msg : String);
begin
    if not Assigned(DisplayMemo) then
        Exit;
    DisplayMemo.Lines.BeginUpdate;
    try
        if DisplayMemo.Lines.Count > 500 then begin
            while DisplayMemo.Lines.Count > 500 do
                DisplayMemo.Lines.Delete(0);
        end;
        DisplayMemo.Lines.Add(Msg);
    finally
        DisplayMemo.Lines.EndUpdate;
        SendMessage(DisplayMemo.Handle, EM_SCROLLCARET, 0, 0);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWebAppSrvForm.DisplayHandler(Sender : TObject; const Msg : String);
begin
    Display(Msg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWebAppSrvForm.WMAppStartup(var msg: TMessage);
var
    BaseDir : String;
begin
    BaseDir                    := IncludeTrailingPathDelimiter(
                                      ExtractFilePath(Application.ExeName));
    FDataDir                   := BaseDir + 'WebAppServerData\Data';
    FSessionFile               := FDataDir + '\Sessions.dat';
    HttpAppSrv1.DocDir         := BaseDir + 'WebAppServerData\wwwRoot';
    HttpAppSrv1.TemplateDir    := BaseDir + 'WebAppServerData\Templates';
    HttpAppSrv1.DefaultDoc     := 'index.html';

    // Force directory creation
    ForceDirectories(FDataDir);
    ForceDirectories(HttpAppSrv1.TemplateDir);
    ForceDirectories(HttpAppSrv1.DocDir);
    ForceDirectories(HttpAppSrv1.DocDir + '\Js');
    ForceDirectories(HttpAppSrv1.DocDir + '\Styles');
    ForceDirectories(HttpAppSrv1.DocDir + '\Images');

    WebAppSrvDataModule.IniFileName := FIniFileName;
    WebAppSrvDataModule.OnDisplay   := DisplayHandler;
    WebAppSrvDataModule.DataDir     := FDataDir;
    WebAppSrvDataModule.ImagesDir   := HttpAppSrv1.DocDir + '\Images';
    WebAppSrvDataModule.LoadConfig;

    // Allow direct access to all folders where static documents are.
    HttpAppSrv1.AddGetAllowedPath('/js/',     afBeginBy);
    HttpAppSrv1.AddGetAllowedPath('/styles/', afBeginBy);
    HttpAppSrv1.AddGetAllowedPath('/images/', afBeginBy);

    // Add all dynamic webpage handlers
    HttpAppSrv1.AddGetHandler('/',
                              TUrlHandlerDefaultDoc);
    HttpAppSrv1.AddGetHandler('/' + HttpAppSrv1.DefaultDoc,
                              TUrlHandlerDefaultDoc);
    HttpAppSrv1.AddGetHandler(UrlLogin,
                              TUrlHandlerLoginFormHtml);
    HttpAppSrv1.AddGetHandler(UrlDoLoginSecure,
                              TUrlHandlerDoLoginSecureHtml);
    HttpAppSrv1.AddGetHandler(UrlCounter,
                              TUrlHandlerCounterJpg);
    HttpAppSrv1.AddGetHandler(UrlHomePage,
                              TUrlHandlerHomePageHtml);
    HttpAppSrv1.AddGetHandler(UrlConfigForm,
                              TUrlHandlerConfigFormHtml);
    HttpAppSrv1.AddGetHandler(UrlConfigLogoPng,
                              TUrlHandlerConfigLogoPng);
    HttpAppSrv1.AddGetHandler(UrlDoConfigConfirmSaveHtml,
                              TUrlHandlerDoConfigConfirmSaveHtml);
    HttpAppSrv1.AddPostHandler(UrlDoConfigHtml,
                               TUrlHandlerDoConfigHtml);
    HttpAppSrv1.AddGetHandler(UrlCounterViewHtml,
                              TUrlHandlerCounterViewHtml);
    HttpAppSrv1.AddGetHandler(UrlAjaxFetchCounter,
                              TUrlHandlerAjaxFetchCounter);
    HttpAppSrv1.AddGetHandler(UrlJavascriptErrorHtml,
                              TUrlHandlerJavascriptErrorHtml);

    // Just for demoing the simplest handler, let's add an "Helloworld" one
    HttpAppSrv1.AddGetHandler('/HelloWorld.html',
                              TUrlHandlerHelloWorld);

    if FileExists(FSessionFile) then begin
        try
            HttpAppSrv1.WSessions.LoadFromFile(FSessionFile);
            Display(IntToStr(HttpAppSrv1.SessionsCount) + ' sessions loaded');
        except
            // Ignore any exception, but clear anything partially loaded
            HttpAppSrv1.WSessions.Clear;
            // and delete existing (corrupted) file
            DeleteFile(FSessionFile);
            Display('Unable to load existing sessions');
        end;
    end;

    // Cleanup temporary files left from a previous run
    CleanupTimeStampedDir(WebAppSrvDataModule.DataDir);

    // Start the timer to do some housekeeping while running
    HousekeepingTimer.Interval := 5 * 60 * 1000;
    HousekeepingTimer.Enabled  := TRUE;

    // Start the HTTP server
    HttpAppSrv1.SessionTimeout := 5 * 60;  // Seconds, must be done after loading
    HttpAppSrv1.Port           := WebAppSrvDataModule.Port;
    HttpAppSrv1.Start;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}


end.
