{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
Author:       Arno Garrels
Creation:     November 2012
Description:  Shows how to use ICS components in a worker thread in both Windows
              and OS X (here TSmtpCli). Also shows how to use ICS' window
              messaging in OS X.
Version:      8.65
EMail:        francois.piette@overbyte.be         http://www.overbyte.be
Support:      https://en.delphipraxis.net/forum/37-ics-internet-component-suite/
Legal issues: Copyright (C) 1997-2020 by François PIETTE
              Rue de Grady 24, 4053 Embourg, Belgium.

              This software is provided 'as-is', without any express or
              implied warranty.  In no event will the author be held liable
              for any damages arising from the use of this software.

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
                 to François. Use a nice stamp and mention your name, street
                 address, EMail address and any comment you like to say.
Changelog:
Dec 09, 2020 V8.65 Renamed Ics.Posix.Messages.pas to Ics.Posix.PXMessages.pas.


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit IcsThread1;

{$IF CompilerVersion < 23}
  {$MESSAGE FATAL 'This project requires Delphi or RAD Studio XE2+'};
{$IFEND}

interface

{ Comment next define in order to use ICS' built-in message loop.       }
{ Use of ICS' built-in message loop is very simple and there's no       }
{ difference between Windows and OS X, however if you ever want to      }
{ post custom messages to the thread a custom message loop is required. }
{$DEFINE CUSTOM_MESSAGELOOP}

uses
{$IFDEF MSWINDOWS}
  Winapi.Windows,
  Winapi.Messages,
{$ENDIF}
{$IFDEF POSIX}
  Ics.Posix.PXMessages,
  Ics.Posix.WinTypes,
  Posix.Pthread,  // Removes H2443
  Posix.SysTypes, // Removes H2443
  Posix.Unistd,   // Removes H2443
{$ENDIF}
  System.SysUtils, System.Types, System.UITypes, System.Rtti, System.Classes,
  System.Variants, System.SysConst, FMX.Types, FMX.Controls, FMX.Forms,
  FMX.Dialogs, FMX.Layouts, FMX.Memo, FMX.Edit,
  FMX.StdCtrls,
  Ics.Fmx.DemoUtils,
  OverbyteIcsIniFiles,
  OverbyteIcsUtils,
  Ics.Fmx.OverbyteIcsSmtpProt;

type
  TMailThread = class; //forward
  TfrmMain = class(TForm)
    btnSendMail: TButton;
    mmLog: TMemo;
    edtHost: TEdit;
    edtFromAddr: TEdit;
    edtToAddr: TEdit;
    edtUser: TEdit;
    edtPwd: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    mmMailMessage: TMemo;
    Label6: TLabel;
    Label7: TLabel;
    pnlTop: TPanel;
    Splitter1: TSplitter;
    pnlBottom: TPanel;
    edtSubject: TEdit;
    Label8: TLabel;
    edtFileName: TEdit;
    btnFileSelect: TButton;
    Label9: TLabel;
    OpenDialog1: TOpenDialog;
    edtBytesPerSec: TEdit;
    Label10: TLabel;
    Label11: TLabel;
    procedure btnSendMailClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnFileSelectClick(Sender: TObject);
  private
  {$IFDEF POSIX}
    FMessagePump: TIcsMessagePump;
  {$ENDIF}
    FIniFile: TIcsIniFile;
    FMailThread: TMailThread;
    FNotifyWindow: HWND;
    procedure WndProc(var MsgRec: TMessage);
  end;

  TMailThread = class(TThread)
  private
    FSmtpCli              : TSmtpCli;
    FGUINotifyWindow      : HWND;
    FServer               : string;
    FMailFrom             : string;
    FMailTo               : string;
    FUser                 : string;
    FPassword             : string;
    FMailMessage          : string;
    FSubject              : string;
    FAttachment           : string;
    FBytesPerSec          : Integer;
{$IFDEF CUSTOM_MESSAGELOOP}
  {$IFDEF POSIX}
    FMessagePump: TIcsMessagePump;
    function PosixMessageHandler(ahWnd: HWND; auMsg: UINT; awParam: WPARAM;
      alParam: LPARAM; var Handled: Boolean): LRESULT;
  {$ENDIF}
    procedure CustomMessageLoop;
{$ENDIF}
    procedure Log(const AMsg: string);
    procedure SmtpRequestDone(Sender: TObject; RqType: TSmtpRequest;
      ErrCode: Word);
    procedure SmtpResponse(Sender: TObject; Msg: string);
    procedure InitSmtpAndConnect;
  protected
    procedure Execute; override;
  public
    constructor Create(AGUINotifyWindow: HWND); reintroduce;
    property Server: string read FServer write FServer;
    property MailFrom: string read FMailFrom write FMailFrom;
    property MailTo: string read FMailTo write FMailTo;
    property User: string read FUser write FUser;
    property Password: string read FPassword write FPassword;
    property MailMessage: string read FMailMessage write FMailMessage;
    property Subject: string read FSubject write FSubject;
    property Attachment: string read FAttachment write FAttachment;
    property BytesPerSec: Integer read FBytesPerSec write FBytesPerSec;
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.fmx}

const
  WM_BEEP                   = WM_USER + 1;
  WM_LOG                    = WM_USER + 2;
  WM_THREAD_WILL_TERMINATE  = WM_USER + 3;
  WM_THREAD_IS_READY        = WM_USER + 4;

{ IniFile layout for persistent data }
  SectionWindow             = 'WindowMain';
  KeyTop                    = 'Top';
  KeyLeft                   = 'Left';
  KeyWidth                  = 'Width';
  KeyHeight                 = 'Height';
  SectionSettings           = 'Settings';
  SectionMessage            = 'MessageBody';
  KeyMailServer             = 'MailServer';
  KeyMailFrom               = 'MailFromAddress';
  KeyMailTo                 = 'MailToAddress';
  KeyUser                   = 'UserName';
  KeyPassword               = 'Password';
  KeySubject                = 'Subject';
  KeyMailMessage            = 'MailMessage';
  KeyAttach                 = 'Attachment';
  KeyBytesPerSec            = 'MaxBytesPerSecond';

var
  GCritSect: TIcsCriticalSection;
  GLogStrings: TStringList;

{ TMailThread }

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF CUSTOM_MESSAGELOOP}
{$IFDEF POSIX}
function TMailThread.PosixMessageHandler(ahWnd: HWND; auMsg: UINT;
  awParam: WPARAM; alParam: LPARAM; var Handled: Boolean): LRESULT;
begin
  Result := 0;
  { Every message posted in/to this thread context passes this handler }
  if ahWnd = 0 then // Messages posted with PostThreadMessage()
  begin
    if auMsg = WM_BEEP then
    begin
      { Handle our custom message }
      Handled := True;
      Beep;
    end;
  end;
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMailThread.CustomMessageLoop;
{$IFDEF MSWINDOWS}
var
  LMsg: TMsg;
begin
  { If GetMessage retrieves the WM_QUIT, the return value is FALSE and    }
  { the message loop is broken.                                           }
  while GetMessage(LMsg, 0, 0, 0) do
  begin
    if LMsg.hwnd = 0 then // Messages posted with PostThreadMessage()
    begin
      if LMsg.message = WM_BEEP then
      begin
        { Handle our custom message }
        Beep;
      end
      else begin
        TranslateMessage(LMsg);
        DispatchMessage(LMsg);
      end;
    end
    else begin
      TranslateMessage(LMsg);
      DispatchMessage(LMsg);
    end;
  end;
  FSmtpCli.Terminated := True;
{$ENDIF MSWINDOWS}
{$IFDEF POSIX}
begin
  { If TIcsMessagePump retrieves the WM_QUIT, the message loop is broken. }
  FMessagePump.HandleMessages;
  FSmtpCli.Terminated := True;
{$ENDIF POSIX}
end;
{$ENDIF CUSTOM_MESSAGELOOP}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TMailThread.Create(AGUINotifyWindow: HWND);
begin
  inherited Create(True);
  FGUINotifyWindow := AGUINotifyWindow;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMailThread.InitSmtpAndConnect;
begin
  { Prepare connection to SMTP server }
  FSmtpCli.Host           := FServer;
  FSmtpCli.Port           := 'smtp';
  { Detect most secure authentication type }
  FSmtpCli.AuthType       := smtpAuthAutoSelect;
  FSmtpCli.Username       := FUser;
  FSmtpCli.Password       := FPassword;
  { Prepare message addressing }
  FSmtpCli.FromName       := FMailFrom;
  FSmtpCli.RcptName.Clear;
  FSmtpCli.RcptName.Add(FMailTo);
  FSmtpCli.HdrSubject     := FSubject;
  FSmtpCli.HdrTo          := FSmtpCli.RcptName.Strings[0];
  FSmtpCli.HdrFrom        := FSmtpCli.FromName;

  FSmtpCli.MailMessage.Text := MailMessage;
  FSmtpCli.MailMessage.Add('');
  FSmtpCli.MailMessage.Add('-----------------------------------------');
{$IFDEF CUSTOM_MESSAGELOOP}
  FSmtpCli.MailMessage.Add('This test used a custom message loop');
{$ELSE}
  FSmtpCli.MailMessage.Add('This test used ICS'' built-in message loop');
{$ENDIF}
  FSmtpCli.MailMessage.Add('');
  FSmtpCli.MailMessage.Add('Internet Component Suite');
  { Chinese and Japanese look so nice that I added (Google) translations here. }
  { Anyway this is not important for the purpose of this demo.                 }
  FSmtpCli.MailMessage.Add('Japanese: ' + #$30a4  + #$30f3 + #$30bf + #$30fc +
    #$30cd + #$30c3 + #$30c8 + #$30b3 + #$30f3 + #$30dd + #$30fc + #$30cd +
    #$30f3 + #$30c8 + #$30b9 + #$30a4 + #$30fc + #$30c8);
  FSmtpCli.MailMessage.Add('Chinese: ' + #$7f51 + #$7edc + #$7ec4 + #$4ef6 +
    #$5957 + #$4ef6);

  { Using property MailMessage above and the following settings guarantees }
  { correctly formatted and encoded mails.                                 }
  FSmtpCli.WrapMessageText  := True;
  FSmtpCli.FoldHeaders      := True;
  FSmtpCli.Allow8bitChars   := False;
  FSmtpCli.CharSet          := 'utf-8';

  FSmtpCli.EmailFiles.Clear;
  if FAttachment <> '' then
    FSmtpCli.EmailFiles.Add(FAttachment);

  { Assign component event handlers }
  FSmtpCli.OnRequestDone    := SmtpRequestDone;
  FSmtpCli.OnResponse       := SmtpResponse;

  if FBytesPerSec > 0 then
  begin
    FSmtpCli.CtrlSocket.BandwidthSampling := 1000;
    FSmtpCli.CtrlSocket.BandwidthLimit    := FBytesPerSec;
  end;

  Log('> CONNECT ' + FSmtpCli.Host + '/' + FSmtpCli.Port);
  FSmtpCli.Connect;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMailThread.Execute;
{$IFDEF MSWINDOWS}
var
  LMsg: TMsg;
begin
  { Initialize the message queue in Windows }
  PeekMessage(LMsg, 0, 0, 0, PM_NOREMOVE);
{$ELSE}
begin
{$ENDIF}
  try
    { We must create and free ICS components in thread-context or use method }
    { ThreadAttach which is not subject of this demo, so let's create it.    }
    FSmtpCli := TSmtpCli.Create(nil);
    try
      FSmtpCli.MultiThreaded := True;
    {$IFDEF CUSTOM_MESSAGELOOP}
    {$IFDEF POSIX}
      { Get the thread-singleton instance of TIcsMessagePump that TSmtpCli   }
      { created, don't free it, it's freed when FSmtpCli is freed.           }
      { We may call the constructor of TIcsMessagePump to achive the same    }
      { but then we had to call the destructor later on as well. We are lazy }
      { and call class method Instance.                                      }
      FMessagePump := TIcsMessagePump.Instance;
      { Assign an event handler that is triggered for every message posted   }
      { in/to this thread context.                                           }
      FMessagePump.OnMessage := PosixMessageHandler;
    {$ENDIF}
    {$ENDIF}
      { Tell the main thread that this thread is ready to process messages   }
      PostMessage(FGUINotifyWindow, WM_THREAD_IS_READY, WPARAM(ThreadID), 0);
      InitSmtpAndConnect;
    {$IFDEF CUSTOM_MESSAGELOOP}
      CustomMessageLoop;
    {$ELSE}
      FSmtpCli.MessageLoop;
    {$ENDIF}
      Log('!Leaving TMailThread.Execute');
    finally
      FSmtpCli.Free;
    end;
  finally
    PostMessage(FGUINotifyWindow, WM_THREAD_WILL_TERMINATE, 0, 0);
  end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMailThread.Log(const AMsg: string);
var
  LNotify: Boolean;
begin
  GCritSect.Enter;
  try
    LNotify := (GLogStrings.Count = 0);
    GLogStrings.Add(AMsg);
  finally
    GCritSect.Leave;
  end;
  if LNotify then
    PostMessage(FGUINotifyWindow, WM_LOG, 0, 0);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This event handler is called each time smtp component has done a request  }
{ We use it to start the next request because sending a smtp message        }
{ require a few operations: connecting to server, sending helo message,     }
{ sending originator, sending recipients, sending message body and finally  }
{ disconnecting from server. There are of course optional request such as   }
{ authentication.                                                           }
procedure TMailThread.SmtpRequestDone(Sender: TObject; RqType: TSmtpRequest;
  ErrCode: Word);
var
  LEndFlag: Boolean;
begin
  try
    { Check status }
    if (ErrCode <> 0) or ((RqType <> smtpQuit) and (not FSmtpCli.Connected)) then
    begin
      Log('Failed, error #' + IntToStr(ErrCode));
      LEndFlag := TRUE;
    end
    else begin
      LEndFlag := FALSE;
      case RqType of
        smtpConnect:
          begin
            Log('> EHLO');
            FSmtpCli.Ehlo;
          end;
        smtpEhlo:
          begin
            if FUser <> '' then
            begin
              Log('> Auth');
              FSmtpCli.Auth;
            end
            else begin
              Log('> MAILFROM');
              FSmtpCli.MailFrom;
            end;
          end;
        smtpAuth:
          begin
            Log('> MAILFROM');
            FSmtpCli.MailFrom;
          end;
        smtpMailFrom:
          begin
            Log('> RCPTTO');
            FSmtpCli.RcptTo;
          end;
        smtpRcptTo:
          begin
            Log('> DATA');
            FSmtpCli.Data;
          end;
        smtpData:
          begin
            Log('> QUIT');
            FSmtpCli.Quit;
          end;
        smtpQuit:
          begin
            Log('!Quit done');
            LEndFlag := TRUE;
          end;
        else
          begin
            Log('!Unknown SmtpRequest ' + IntToStr(Ord(RqType)));
            LEndFlag := TRUE;
          end;
      end;
    end;
    { If something went wrong or end of job, then break the message loop }
    if LEndFlag then
    begin
      Log('!PostQuitMessage called');
      { Break message loop we called from the execute method }
      FSmtpCli.PostQuitMessage;
    end;
  except
    on E: Exception do
    begin
      Log(E.ClassName + ', ' + E.Message);
      FSmtpCli.PostQuitMessage;
    end;
  end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMailThread.SmtpResponse(Sender: TObject; Msg: String);
begin
  Log('< ' + Msg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ TfrmMain }
{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmMain.FormCreate(Sender: TObject);
begin
{$IFDEF POSIX}
  FMessagePump      := TIcsMessagePump.Create;
{$ENDIF}
  FNotifyWindow     := AllocateHWND(WndProc);
  GCritSect         := TIcsCriticalSection.Create;
  GLogStrings       := TStringList.Create;

  { Restore props }
  FIniFile          := TIcsIniFile.Create(GetIcsIniFileName);
  Width             := FIniFile.ReadInteger(SectionWindow, KeyWidth,  Width);
  Height            := FIniFile.ReadInteger(SectionWindow, KeyHeight, Height);
  Top               := FIniFile.ReadInteger(SectionWindow, KeyTop,
                                      (ScreenHeight - Height) div 2);
  Left              := FIniFile.ReadInteger(SectionWindow, KeyLeft,
                                      (ScreenWidth  - Width)  div 2);
  edtHost.Text      := FIniFile.ReadString(SectionSettings, KeyMailServer,
                                          'localhost');
  edtFromAddr.Text  := FIniFile.ReadString(SectionSettings, KeyMailFrom,
                                          'name@domain.com');
  edtToAddr.Text    := FIniFile.ReadString(SectionSettings, KeyMailTo,
                                          'name@domain.com');
  edtUser.Text      := FIniFile.ReadString(SectionSettings, KeyUser,
                                          'test');
  edtPwd.Text       := FIniFile.ReadString(SectionSettings, KeyPassword,
                                          'test');
  edtSubject.Text   := FIniFile.ReadString(SectionSettings, KeySubject,
                                          'ICS thread SMTP demo is working');
  edtBytesPerSec.Text := FIniFile.ReadString(SectionSettings, KeyBytesPerSec, '0');
  edtFileName.Text  := FIniFile.ReadString(SectionSettings, KeyAttach, '');
  if not FIniFile.ReadStrings(SectionMessage, KeyMailMessage,
                              mmMailMessage.Lines) then
    mmMailMessage.Text := 'Hello!' + #13#10 +
                          'This message has been sent by IcsThrdMailSnd demo.';
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  if FMailThread <> nil then
  begin
    { Thread is still running }
  {$IFDEF CUSTOM_MESSAGELOOP}
    { Break thread's message loop, this call may fail if thread's  }
    { message pump is already destroyed, no problem.               }
    PostThreadMessage(FMailThread.ThreadID, WM_QUIT, 0, 0);
  {$ENDIF}
    { Waits until the thread ends and frees the object }
    FMailThread.Free;
  end;
  DeallocateHWND(FNotifyWindow);
  GCritSect.Free;
  GLogStrings.Free;
{$IFDEF POSIX}
  FMessagePump.Free;
{$ENDIF}

  { Save props }
  if FIniFile <> nil then
  begin
    FIniFile.WriteInteger(SectionWindow,    KeyTop,         Top);
    FIniFile.WriteInteger(SectionWindow,    KeyLeft,        Left);
    FIniFile.WriteInteger(SectionWindow,    KeyWidth,       Width);
    FIniFile.WriteInteger(SectionWindow,    KeyHeight,      Height);

    FIniFile.WriteString(SectionSettings,   KeyMailServer,  edtHost.Text);
    FIniFile.WriteString(SectionSettings,   KeyMailFrom,    edtFromAddr.Text);
    FIniFile.WriteString(SectionSettings,   KeyMailTo,      edtToAddr.Text);
    FIniFile.WriteString(SectionSettings,   KeyUser,        edtUser.Text);
    FIniFile.WriteString(SectionSettings,   KeyPassword,    edtPwd.Text);
    FIniFile.WriteString(SectionSettings,   KeySubject,     edtSubject.Text);
    FIniFile.WriteString(SectionSettings,   KeyAttach,      edtFileName.Text);
    FIniFile.WriteInteger(SectionSettings,  KeyBytesPerSec, StrToIntDef(edtBytesPerSec.Text, 0));
    FIniFile.WriteStrings(SectionMessage,   KeyMailMessage, mmMailMessage.Lines);

    FIniFile.UpdateFile;
    FIniFile.Free;
  end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmMain.WndProc(var MsgRec: TMessage);
begin
  try
    if MsgRec.Msg = WM_LOG then
    begin
      GCritSect.Enter;
      try
        mmLog.Lines.AddStrings(GLogStrings);
        mmLog.GoToTextEnd;
        GLogStrings.Clear;
      finally
        GCritSect.Leave;
      end;
    end
    else if MsgRec.Msg = WM_THREAD_WILL_TERMINATE then
    begin
      FMailThread.Free; // Waits until the thread ends and frees the object
      FMailThread := nil;
    end
    else if MsgRec.Msg = WM_THREAD_IS_READY then
    begin
    {$IFDEF CUSTOM_MESSAGELOOP}
      PostThreadMessage(FMailThread.ThreadID, WM_BEEP, MsgRec.WParam, 0);
    {$ENDIF}
    end
    else
      MsgRec.Result := DefWindowProc(FNotifyWindow, MsgRec.Msg, MsgRec.WParam, MsgRec.LParam);
  except
    Application.HandleException(Self);
  end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmMain.btnFileSelectClick(Sender: TObject);
begin
  if OpenDialog1.Execute then
    edtFileName.Text := OpenDialog1.FileName;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmMain.btnSendMailClick(Sender: TObject);
begin
  if FMailThread <> nil then
  begin
    ShowMessage('A mail is currently being sent. This demo cannot send ' +
      'multiple mails at the same time');
  end
  else begin
    FMailThread             := TMailThread.Create(FNotifyWindow);
    FMailThread.Server      := edtHost.Text;
    FMailThread.Subject     := edtSubject.Text;
    FMailThread.MailMessage := mmMailMessage.Text;
    FMailThread.MailFrom    := edtFromAddr.Text;
    FMailThread.MailTo      := edtToAddr.Text;
    FMailThread.User        := edtUser.Text;
    FMailThread.Password    := edtPwd.Text;
    FMailThread.Attachment  := edtFileName.Text;
    FMailThread.BytesPerSec := StrToIntDef(edtBytesPerSec.Text, 0);
    FMailThread.Start;
  end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
end.
