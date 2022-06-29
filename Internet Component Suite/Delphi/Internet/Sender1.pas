{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Description:  Simple client program which just send data to a server and display
              all incomming data.
EMail:        francois.piette@pophost.eunet.be    francois.piette@rtfm.be
              http://www.rtfm.be/fpiette
Creation:     Oct 01, 1998
Version:      1.03
Support:      Use the mailing list twsocket@rtfm.be See website for details.
Legal issues: Copyright (C) 1998 by François PIETTE
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
Oct 28, 1998  V1.02 Trapped Connect exceptions.
                    Added AutoStartButton and associated logic.
                    Added LingerCheckBox and associated logic.
Mar 07, 1999  V1.03 Adapted for Delphi 1


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit Sender1;

interface

uses
  WinTypes, WinProcs, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, IniFiles, WSocket;

const
  WM_AUTO_START      = WM_USER + 1;
  WM_CLOSE_REQUEST   = WM_USER + 2;

type
{$IFDEF VER80}
  LParam = LongInt;
{$ENDIF}
  TSenderForm = class(TForm)
    Panel1: TPanel;
    Label1: TLabel;
    ServerEdit: TEdit;
    Label2: TLabel;
    PortEdit: TEdit;
    Label3: TLabel;
    DataEdit: TEdit;
    Label4: TLabel;
    RepeatEdit: TEdit;
    ContCheckBox: TCheckBox;
    ActionButton: TButton;
    DisplayMemo: TMemo;
    Label5: TLabel;
    LengthEdit: TEdit;
    WSocket1: TWSocket;
    DisplayDataCheckBox: TCheckBox;
    UseDataSentCheckBox: TCheckBox;
    PauseButton: TButton;
    CountLabel: TLabel;
    AutoStartButton: TButton;
    LingerCheckBox: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ContCheckBoxClick(Sender: TObject);
    procedure WSocket1DnsLookupDone(Sender: TObject; Error: Word);
    procedure WSocket1SessionConnected(Sender: TObject; Error: Word);
    procedure ActionButtonClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure WSocket1DataAvailable(Sender: TObject; Error: Word);
    procedure WSocket1SessionClosed(Sender: TObject; Error: Word);
    procedure DisplayDataCheckBoxClick(Sender: TObject);
    procedure UseDataSentCheckBoxClick(Sender: TObject);
    procedure PauseButtonClick(Sender: TObject);
    procedure AutoStartButtonClick(Sender: TObject);
  private
    FIniFileName : String;
    FInitialized : Boolean;
    FDataBuf     : PChar;
    FDataBufSize : Integer;
    FCount       : Integer;
    FFinalCount  : Integer;
    FSending     : Boolean;
    FDisplayData : Boolean;
    FUseDataSent : Boolean;
    FFinished    : Boolean;
    FPaused      : Boolean;
    FAutoStart   : Integer;
    procedure Display(Msg : String);
    procedure DoSend;
    procedure WSocket1DataSent(Sender: TObject; Error: Word);
    procedure WSocket1NoDataSent(Sender: TObject; Error: Word);
    procedure WMAutoStart(var msg: TMessage); message WM_AUTO_START;
    procedure WMCloseRequest(var msg: TMessage); message WM_CLOSE_REQUEST;
  public
    { Déclarations publiques }
  end;

var
  SenderForm: TSenderForm;

implementation

{$R *.DFM}

const
    SectionWindow   = 'RecvForm';
    KeyTop          = 'Top';
    KeyLeft         = 'Left';
    KeyWidth        = 'Width';
    KeyHeight       = 'Height';
    SectionData     = 'Data';
    KeyPort         = 'Port';
    KeyServer       = 'Server';
    KeyData         = 'Data';
    KeyRepeat       = 'RepeatCount';
    KeyContinuous   = 'ContinuousSend';
    KeyLength       = 'DataLength';
    KeyUseDataSent  = 'UseDataSent';
    KeyDisplay      = 'Display';
    KeyLinger       = 'Linger';


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSenderForm.FormCreate(Sender: TObject);
begin
    FIniFileName := LowerCase(ExtractFileName(Application.ExeName));
    FIniFileName := Copy(FIniFileName, 1, Length(FIniFileName) - 3) + 'ini';
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSenderForm.FormShow(Sender: TObject);
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
        PortEdit.Text        := IniFile.ReadString(SectionData, KeyPort, 'telnet');
        ServerEdit.Text      := IniFile.ReadString(SectionData, KeyServer, 'localhost');
        DataEdit.Text        := IniFile.ReadString(SectionData, KeyData,       'The quick brown fox jumps over the lazy dog');
        RepeatEdit.Text      := IniFile.ReadString(SectionData, KeyRepeat,     '');
        LengthEdit.Text      := IniFile.ReadString(SectionData, KeyLength,     '60');
        ContCheckBox.Checked        := Boolean(IniFile.ReadInteger(SectionData, KeyContinuous,  0));
        LingerCheckBox.Checked      := Boolean(IniFile.ReadInteger(SectionData, KeyLinger,      1));
        DisplayDataCheckBox.Checked := Boolean(IniFile.ReadInteger(SectionData, KeyDisplay,     0));
        UseDataSentCheckBox.Checked := Boolean(IniFile.ReadInteger(SectionData, KeyUseDataSent, 1));
        IniFile.Destroy;
        RepeatEdit.Enabled := not ContCheckBox.Checked;
        CountLabel.Caption  := '';
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSenderForm.FormClose(Sender: TObject; var Action: TCloseAction);
var
    IniFile : TIniFile;
begin
    IniFile := TIniFile.Create(FIniFileName);
    IniFile.WriteInteger(SectionWindow, KeyTop,       Top);
    IniFile.WriteInteger(SectionWindow, KeyLeft,      Left);
    IniFile.WriteInteger(SectionWindow, KeyWidth,     Width);
    IniFile.WriteInteger(SectionWindow, KeyHeight,    Height);
    IniFile.WriteString(SectionData, KeyPort,   PortEdit.text);
    IniFile.WriteString(SectionData, KeyServer, ServerEdit.text);
    IniFile.WriteString(SectionData, KeyData,   DataEdit.text);
    IniFile.WriteString(SectionData, KeyRepeat, RepeatEdit.text);
    IniFile.WriteString(SectionData, KeyLength, LengthEdit.text);
    IniFile.WriteInteger(SectionData, KeyContinuous,  Ord(ContCheckBox.Checked));
    IniFile.WriteInteger(SectionData, KeyLinger,      Ord(LingerCheckBox.Checked));
    IniFile.WriteInteger(SectionData, KeyUseDataSent, Ord(UseDataSentCheckBox.Checked));
    IniFile.WriteInteger(SectionData, KeyDisplay,     Ord(DisplayDataCheckBox.Checked));
    IniFile.Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSenderForm.FormDestroy(Sender: TObject);
begin
    if FDataBuf <> nil then begin
        Freemem(FDataBuf, FDataBufSize);
        FDataBuf := nil;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSenderForm.ContCheckBoxClick(Sender: TObject);
begin
    RepeatEdit.Enabled := not ContCheckBox.Checked;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSenderForm.Display(Msg : String);
begin
    if DisplayMemo.lines.Count > 200 then
        DisplayMemo.Clear;
    DisplayMemo.Lines.Add(Msg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF VER80}
function TrimRight(Str : String) : String;
var
    i : Integer;
begin
    i := Length(Str);
    while (i > 0) and (Str[i] = ' ') do
        i := i - 1;
    Result := Copy(Str, 1, i);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TrimLeft(Str : String) : String;
var
    i : Integer;
begin
    if Str[1] <> ' ' then
        Result := Str
    else begin
        i := 1;
        while (i <= Length(Str)) and (Str[i] = ' ') do
            i := i + 1;
        Result := Copy(Str, i, Length(Str) - i + 1);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function Trim(Str : String) : String;
begin
    Result := TrimLeft(TrimRight(Str));
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSenderForm.ActionButtonClick(Sender: TObject);
var
    Len : Integer;
    N   : Integer;
    T   : Integer;
    Buf : String;
begin
    { The ActionButton is used to start or stop data transmission }
    if FSending then begin
        { We are already sending, so user wants to stop }
        { Display updated counter                       }
        CountLabel.Caption := IntToStr(FCount);

        { Check if some data remains in TWSocket's internal buffer }
        if (not WSocket1.AllSent) and
           (Application.MessageBox('Data is still being sent' + #10 +
                                   'Close anyway ?',
                                   'Warning', MB_YESNO) <> IDYES) then
            Exit;

        Display('Stop requested');
        if not WSocket1.AllSent then
            Display('Not all data has been sent');

        FAutoStart := 0;
        { Close the socket. This will delete any data not already sent to }
        { winsock.                                                        }
        PostMessage(Handle, WM_CLOSE_REQUEST, 0, LParam(WSocket1));

        Exit;
    end;

    { The user wants to start data transmission }
    CountLabel.Caption   := '';
    PauseButton.Caption  := '&Pause';
    PauseButton.Visible  := TRUE;
    ActionButton.Caption := '&Stop';
    FPaused              := FALSE;
    FSending             := TRUE;
    FFinished            := FALSE;
    FCount               := 0;

    { Setup final count }
    if ContCheckBox.Checked then
        FFinalCount := 0
    else
        FFinalCount := StrToInt(Trim(RepeatEdit.Text));

    { Check which method use to send more data }
    { Using OnDataSent event will prevent internal TWSocket buffer to be }
    { enlarged without limit.                                            }
    FUseDataSent := UseDataSentCheckBox.Checked;
    if FUseDataSent then
        WSocket1.OnDataSent := WSocket1DataSent
    else
        WSocket1.OnDataSent := WSocket1NoDataSent;

    { Prepare data to be sent }
    Buf := '0000 ' + DataEdit.Text;
    Len := StrToInt(Trim(LengthEdit.Text));
    if Len <= 0 then
        Len := Length(Buf);
    if FDataBuf <> nil then
        Freemem(FDataBuf, FDataBufSize);
    FDataBufSize := Len + 3;
    GetMem(FDataBuf, FDataBufSize);
    if Len > 0 then begin
        if Len < Length(Buf) then
            Move(Buf[1], FDataBuf[0], Len)
        else begin
            T := 0;
            while T < Len do begin
                N := Length(Buf);
                if (T + N) > Len then
                    N := Len - T;
                if N > 0 then
                    Move(Buf[1], FDataBuf[T], N);
                T := T + N;
            end;
        end;
    end;
    FDataBuf[Len]     := #13;
    FDataBuf[Len + 1] := #10;
    FDataBuf[Len + 2] := #0;

    { Launch DNS lookup. When done, we'll try to connect. }
    WSocket1.DnsLookup(Trim(ServerEdit.Text));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ We comes here when DNS lookup is finished, even in case of failure.       }
procedure TSenderForm.WSocket1DnsLookupDone(Sender: TObject; Error: Word);
begin
    { If any error occured, we just display info and prepare to restart. }
    if Error <> 0 then begin
        MessageBeep(MB_OK);
        Display('DNS failure. Error #' + IntToStr(Error));
        ActionButton.Caption := '&Start';
        PauseButton.Visible  := FALSE;
        Exit;
    end;

    { Now we know the IP address. Try to connect. }
    WSocket1.Addr  := WSocket1.DnsResult;
    WSocket1.Port  := Trim(PortEdit.Text);
    WSocket1.Proto := 'tcp';
    try
        WSocket1.Connect;
    except
        on E:Exception do begin
            MessageBeep(MB_OK);
            Display('Connect failed: ' + E.Message);
            ActionButton.Caption := '&Start';
            PauseButton.Visible  := FALSE;
            FAutoStart           := 0;
            Exit;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSenderForm.WSocket1SessionConnected(Sender: TObject;
  Error: Word);
begin
    if Error <> 0 then begin
        MessageBeep(MB_OK);
        Display('Can''t connect. Error #' + IntToStr(Error));
        ActionButton.Caption := '&Start';
        FAutoStart           := 0;
        Exit;
    end;
    Display('Connected');
    if LingerCheckBox.Checked then
        WSocket1.LingerOnOff   := wsLingerOn
    else
        WSocket1.LingerOnOff   := wsLingerOff;
    WSocket1.LingerTimeout := 300;
    WSocket1.SetLingerOption;
    DoSend;
    if FUseDataSent then
        Exit;

    { User requested to not use OnDataSent event. We will simply loop.      }
    { until all data has been sent. This will fill TWSocket internal buffer }
    { very quickly while data is being sent in the background at network    }
    { speed.                                                                }
    while (FFinalCount <= 0) or (FFinalCount > FCount) do begin
        { We must break the loop if user temrinated the application,        }
        { or if connection is broke, or if user stopped.                    }
        if (Application.Terminated) or
           (WSocket1.State <> wsConnected) or
           (not FSending) then
            Exit;
        { Otherwise, we can send data }
        DoSend;
    end;
    CountLabel.Caption  := IntToStr(FCount);
    PauseButton.Visible := FALSE;
    Display('All data is in TWSocket buffer and being sent in the background');
    FFinished := TRUE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSenderForm.DoSend;
var
    Buf : String;
begin
    repeat
        { Calling ProcessMessages let a chance to button and other events }
        { to be handled.                                                  }
        Application.ProcessMessages;
        { We must stop if the user clicked the close button. }
        if Application.Terminated then begin
            Display('Application terminated');
            Exit;
        end;
        { We must stop if the user requested to stop send }
        if not FSending then
            Exit;
        { We must stop if connection is broken }
        if WSocket1.State <> wsConnected then
            Exit;
        {$IFNDEF VER80}
        { We don't wants to use 100% CPU just looping. Sleep a little bit }
        if FPaused then
            Sleep(250);
        {$ENDIF}
    until FPaused = FALSE;

    { We need to check if we are still connected before sending }
    if WSocket1.State <> wsConnected then
        Exit;

    if (FFinalCount <= 0) or (FFinalCount > FCount) then begin
        { Count the message sent }
        Inc(FCount);
        { Put the counter into the message, truvated to 4 digits }
        Buf := IntToStr(FCount mod 10000) + '    ';
        Move(Buf[1], FDataBuf[0], 4);

        { If required, display in memo box (slow down !) }
        if FDisplayData then
            Display('Sending ' + IntToStr(FCount));
        { Display the counter every 100 sends }
        if (FCount mod 100) = 0 then
            CountLabel.Caption := IntToStr(FCount);

        { Try to send data. Send may fail ! }
        try
            WSocket1.Send(FDataBuf, FDataBufSize - 1);
        except
            on E:Exception do begin
                Display('Exception during TWSocket.Send: ' + E.Message);
                FAutoStart := 0;
                PostMessage(Handle, WM_CLOSE_REQUEST, 0, LParam(WSocket1));
            end;
        end;
    end
    else begin
        Display('Required data has been sent. Closing.');
        { We may have not read data send by server. But anyway, close the }
        { session.                                                        }
        PostMessage(Handle, WM_CLOSE_REQUEST, 0, LParam(WSocket1));
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSenderForm.WSocket1DataSent(Sender: TObject; Error: Word);
begin
    DoSend;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSenderForm.WSocket1NoDataSent(Sender: TObject; Error: Word);
begin
    if FFinished then begin
        if not WSocket1.AllSent then
            Display('Not all sent');
        Display('Required data has been sent. Closing.');
        PostMessage(Handle, WM_CLOSE_REQUEST, 0, LParam(WSocket1));
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSenderForm.WSocket1DataAvailable(Sender: TObject; Error: Word);
var
    Buf : PChar;
    Cli : TWSocket;
    Len : Integer;
    Cnt : Integer;
begin
    Cli := Sender as TWSocket;
    Cnt := Cli.RcvdCount;
    if Cnt <= 0 then
        Exit;
{$IFDEF VER80}
    { Delphi 1 has 255 character limit of strings (StrPas below) }
    if Cnt > 254 then
        Cnt := 254;
{$ENDIF}
    GetMem(Buf, Cnt + 1);
    try
        Len := Cli.Receive(Buf, Cnt);
        if Len > 0 then begin
            Buf[Cnt] := #0;
            Display('Received: ' + StrPas(Buf));
        end;
    finally
        FreeMem(Buf, Cnt + 1);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSenderForm.WSocket1SessionClosed(Sender: TObject; Error: Word);
begin
    if Error = 0 then
        Display('Socket closed, no error')
    else begin
        Display('Socket closed, Error #' + IntToStr(Error));
        FAutoStart := 0;
    end;
    FSending             := FALSE;
    ActionButton.Caption := '&Start';
    PauseButton.Visible  := FALSE;
    FPaused              := FALSE;
    if FAutoStart > 0 then begin
        Inc(FAutoStart);
        AutoStartButton.Caption := IntToStr(FAutoStart);
        PostMessage(Handle, WM_AUTO_START, 0, 0);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSenderForm.DisplayDataCheckBoxClick(Sender: TObject);
begin
    FDisplayData := DisplayDataCheckBox.Checked;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSenderForm.UseDataSentCheckBoxClick(Sender: TObject);
begin
    FUseDataSent := UseDataSentCheckBox.Checked;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSenderForm.PauseButtonClick(Sender: TObject);
begin
    CountLabel.Caption := IntToStr(FCount);
    FPaused := not FPaused;
    if FPaused then
        PauseButton.Caption := '&Resume'
    else
        PauseButton.Caption := '&Pause';
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSenderForm.AutoStartButtonClick(Sender: TObject);
begin
    if FAutoStart <> 0 then begin
        FAutoStart := 0;
        Exit;
    end;

    FAutoStart := 1;
    AutoStartButton.Caption := IntToStr(FAutoStart);
    PostMessage(Handle, WM_AUTO_START, 0, 0);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSenderForm.WMCloseRequest(var msg: TMessage);
var
    WSocket : TWSocket;
begin
    WSocket := TWSocket(msg.LParam);
    WSocket.Close;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSenderForm.WMAutoStart(var msg: TMessage);
begin
    ActionButtonClick(Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.
