{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Creation:     Mar 13, 2003
Description:
Version:      1.00
EMail:        francois.piette@overbyte.be    http://www.overbyte.be
Support:      Unsupported code.
Legal issues: Copyright (C) 2003 by François PIETTE
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
unit OverByteIcsWndControlTest1;

{$I Include\OverbyteIcsDefs.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  OverbyteIcsIniFiles, StdCtrls, ExtCtrls, OverbyteIcsWndControl, OverbyteIcsWSocket,
  OverbyteIcsWSocketS, OverbyteIcsHttpProt, OverbyteIcsFtpCli;

type
  TDisplayEvent = procedure (Sender : TObject;
                             const Msg : String) of object;
  TIcsThread = class(TThread)
  private
    FWSocket1    : TWSocket;
    procedure WSocket1SessionConnected(Sender: TObject; ErrCode: WORD);
    procedure Display(const Msg: String);
  public
    OnDisplay : TDisplayEvent;
    procedure Execute; override;
    procedure Terminate;
  end;

  TAppBaseForm = class(TForm)
    ToolsPanel: TPanel;
    DisplayMemo: TMemo;
    Button1: TButton;
    Button2: TButton;
    Connect1Button: TButton;
    Button4: TButton;
    Label1: TLabel;
    Free1Button: TButton;
    Connect2Button: TButton;
    Free2Button: TButton;
    Release1Button: TButton;
    HttpCliButton: TButton;
    WSocketServerButton: TButton;
    FtpCliButton: TButton;
    Thread1Button: TButton;
    StopThread1Button: TButton;
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Connect1ButtonClick(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Free1ButtonClick(Sender: TObject);
    procedure Connect2ButtonClick(Sender: TObject);
    procedure Free2ButtonClick(Sender: TObject);
    procedure Release1ButtonClick(Sender: TObject);
    procedure HttpCliButtonClick(Sender: TObject);
    procedure WSocketServerButtonClick(Sender: TObject);
    procedure FtpCliButtonClick(Sender: TObject);
    procedure Thread1ButtonClick(Sender: TObject);
    procedure StopThread1ButtonClick(Sender: TObject);
  private
    FIniFileName : String;
    FInitialized : Boolean;
    FWndHandler  : TIcsWndHandler;
    FCtrl1       : TIcsWndControl;
    FCtrl2       : TIcsWndControl;
    FCtrl3       : TIcsWndControl;
    FCtrl4       : TIcsWndControl;
    FWSocket1    : TWSocket;
    FWSocket2    : TWSocket;
    FThread1     : TIcsThread;
    procedure WSocket1SessionConnected(Sender: TObject; ErrCode: WORD);
    procedure ThreadDisplay(Sender: TObject; const Msg: String);
  public
    procedure Display(Msg : String);
    property IniFileName : String read FIniFileName write FIniFileName;
  end;

var
  AppBaseForm: TAppBaseForm;
  GCritSect    : TRTLCriticalSection;

implementation

{$R *.DFM}

const
    SectionWindow      = 'Window';   // Must be unique for each window
    KeyTop             = 'Top';
    KeyLeft            = 'Left';
    KeyWidth           = 'Width';
    KeyHeight          = 'Height';


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TAppBaseForm.FormCreate(Sender: TObject);
begin
    FIniFileName := GetIcsIniFileName;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TAppBaseForm.FormShow(Sender: TObject);
var
    IniFile : TIcsIniFile;
begin
    if not FInitialized then begin
        FInitialized := TRUE;

        IniFile      := TIcsIniFile.Create(FIniFileName);
        Width        := IniFile.ReadInteger(SectionWindow, KeyWidth,  Width);
        Height       := IniFile.ReadInteger(SectionWindow, KeyHeight, Height);
        Top          := IniFile.ReadInteger(SectionWindow, KeyTop,
                                            (Screen.Height - Height) div 2);
        Left         := IniFile.ReadInteger(SectionWindow, KeyLeft,
                                            (Screen.Width  - Width)  div 2);
        IniFile.Free;
        DisplayMemo.Clear;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TAppBaseForm.FormClose(Sender: TObject; var Action: TCloseAction);
var
    IniFile : TIcsIniFile;
begin
    IniFile := TIcsIniFile.Create(FIniFileName);
    IniFile.WriteInteger(SectionWindow, KeyTop,         Top);
    IniFile.WriteInteger(SectionWindow, KeyLeft,        Left);
    IniFile.WriteInteger(SectionWindow, KeyWidth,       Width);
    IniFile.WriteInteger(SectionWindow, KeyHeight,      Height);
    IniFile.UpdateFile;
    IniFile.Free;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TAppBaseForm.Display(Msg : String);
begin
    EnterCriticalSection(GCritSect);
    try
        DisplayMemo.Lines.BeginUpdate;
        try
            if DisplayMemo.Lines.Count > 200 then begin
                while DisplayMemo.Lines.Count > 200 do
                    DisplayMemo.Lines.Delete(0);
            end;
            DisplayMemo.Lines.Add(Msg);
        finally
            DisplayMemo.Lines.EndUpdate;
        end;
    finally
        LeaveCriticalSection(GCritSect);
    end;
    SendMessage(DisplayMemo.Handle, EM_SCROLLCARET, 0, 0);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TAppBaseForm.Button1Click(Sender: TObject);
begin
    FWndHandler        := TIcsWndHandler.Create;
    FWndHandler.MsgLow := WM_USER + 1;
    FCtrl1             := TIcsWndControl.Create(Self);
    FCtrl1.WndHandler  := FWndHandler;
    FCtrl1.Name        := 'FCtrl1';
    FCtrl2             := TIcsWndControl.Create(Self);
    FCtrl2.WndHandler  := FWndHandler;
    FCtrl2.Name        := 'FCtrl2';
    FCtrl3             := TIcsWndControl.Create(Self);
    FCtrl3.WndHandler  := FWndHandler;
    FCtrl3.Name        := 'FCtrl3';
    FCtrl4             := TIcsWndControl.Create(Self);
    FCtrl4.WndHandler  := FWndHandler;
    FCtrl4.Name        := 'FCtrl4';
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

procedure TAppBaseForm.Button2Click(Sender: TObject);
begin
    FCtrl1.Release;
end;

procedure TAppBaseForm.Connect1ButtonClick(Sender: TObject);
begin
     FreeAndNil(FWSocket1);
     FWSocket1       := TWSocket.Create(Self);
     FWSocket1.Name  := 'WSocket1';
     FWSocket1.Proto := 'tcp';
     FWSocket1.Port  := 'telnet';
     FWSocket1.Addr  := 'localhost';
     FWSocket1.OnSessionConnected := WSocket1SessionConnected;
     FWSocket1.Connect;
end;

procedure TAppBaseForm.Free1ButtonClick(Sender: TObject);
begin
     FreeAndNil(FWSocket1);
end;

procedure TAppBaseForm.Release1ButtonClick(Sender: TObject);
begin
    if Assigned(FWSocket1) then begin
        FWSocket1.Release;
        FWSocket1 := nil;
    end;
end;

procedure TAppBaseForm.Connect2ButtonClick(Sender: TObject);
begin
     FreeAndNil(FWSocket2);
     FWSocket2       := TWSocket.Create(Self);
     FWSocket2.Name  := 'WSocket2';
     FWSocket2.Proto := 'tcp';
     FWSocket2.Port  := 'telnet';
     FWSocket2.Addr  := 'localhost';
     FWSocket2.OnSessionConnected := WSocket1SessionConnected;
     FWSocket2.Connect;
end;

procedure TAppBaseForm.Free2ButtonClick(Sender: TObject);
begin
     FreeAndNil(FWSocket2);
end;

procedure TAppBaseForm.WSocket1SessionConnected(
    Sender  : TObject;
    ErrCode : WORD);
begin
    if ErrCode <> 0 then
        Display(TWSocket(Sender).Name + ': Unable to connect. Error #' + IntToStr(ErrCode))
    else
        Display(TWSocket(Sender).Name + ': Connected');
end;

procedure TAppBaseForm.Button4Click(Sender: TObject);
var
    WSocketArray : array of TWSocket;
    I            : Integer;
    Temp         : TWSocket;
begin
    Display('Allocating...');
{$IFNDEF DELPHI25_UP}
    Temp := nil;
{$ENDIF}
    I := 0;
    while I < 65536 do begin
        if I >= Length(WSocketArray) then
            SetLength(WSocketArray, Length(WSocketArray) + 20000);
        try
            Temp := TWSocket.Create(Self);
        except
            break;
        end;
        WSocketArray[I] := Temp;
        Inc(I);
        Label1.caption := IntToStr(I);
        Label1.Update;
    end;
    Display('Destroying');
    while I > 0 do begin
        Dec(I);
        FreeAndNil(WSocketArray[I]);
    end;
    Display('Done');
end;

procedure TAppBaseForm.HttpCliButtonClick(Sender: TObject);
var
    HttpCli1 : THttpCli;
begin
    HttpCli1 := THttpCli.Create(Self);
    try
        HttpCli1.Url        := 'http://www.overbyte.be';
        HttpCli1.RcvdStream := TStringStream.Create('');
        try
            HttpCli1.Get;
            if HttpCli1.StatusCode <> 200 then
                Label1.Caption := IntToStr(HttpCli1.StatusCode) + ' ' + HttpCli1.ReasonPhrase
            else begin
                Label1.Caption := 'Transfer réussi';
                Display(TStringStream(HttpCli1.RcvdStream).DataString);
            end;
        finally
             HttpCli1.RcvdStream.Free;   // Ferme le fichier
             HttpCli1.RcvdStream := nil;
        end;
    finally
        HttpCli1.Free;
    end;
end;

procedure TAppBaseForm.WSocketServerButtonClick(Sender: TObject);
var
    WSocketServer1 : TWSocketServer;
begin
    WSocketServer1 := TWSocketServer.Create(Self);
    try
    finally
        WSocketServer1.Free;
    end;
end;

procedure TAppBaseForm.FtpCliButtonClick(Sender: TObject);
var
    FtpCli1 : TFtpClient;
begin
    FtpCli1 := TFtpClient.Create(Self);
    try
    finally
        FtpCli1.Free;
    end;
end;

{ TIcsThread }

procedure TIcsThread.Execute;
begin
    Display('Thread start');
    FWSocket1 := TWSocket.Create(nil);
    try
        FWSocket1.Name  := 'WSocket1';
        FWSocket1.Proto := 'tcp';
        FWSocket1.Port  := 'telnet';
        FWSocket1.Addr  := 'localhost';
        FWSocket1.OnSessionConnected := WSocket1SessionConnected;
        FWSocket1.Connect;
        FWSocket1.MessageLoop;
    finally
        FWSocket1.Free;
    end;
    Display('Thread stopped');
end;

procedure TIcsThread.Display(const Msg : String);
begin
    if Assigned(OnDisplay) then
        OnDisplay(Self, Msg);
end;

procedure TIcsThread.WSocket1SessionConnected(
    Sender  : TObject;
    ErrCode : WORD);
begin
    if ErrCode <> 0 then
        Display(TWSocket(Sender).Name + ': Unable to connect. Error #' + IntToStr(ErrCode))
    else
        Display(TWSocket(Sender).Name + ': Connected');
end;

procedure TAppBaseForm.ThreadDisplay(
    Sender: TObject;
    const Msg : String);
begin
    Display(Msg);
end;

procedure TAppBaseForm.Thread1ButtonClick(Sender: TObject);
begin
    FThread1                 := TIcsThread.Create(TRUE);
    FThread1.OnDisplay       := ThreadDisplay;
    FThread1.FreeOnTerminate := TRUE;
{$IF CompilerVersion >= 21}
    FThread1.Start;
{$ELSE}
    FThread1.Resume;
{$IFEND}
end;

procedure TAppBaseForm.StopThread1ButtonClick(Sender: TObject);
begin
    if Assigned(FThread1) then
        FThread1.Terminate;
end;

procedure TIcsThread.Terminate;
begin
    PostMessage(FWSocket1.Handle, WM_QUIT, 0, 0);
    inherited Terminate;
end;

initialization
    InitializeCriticalSection(GCritSect);

finalization
    DeleteCriticalSection(GCritSect);

end.
