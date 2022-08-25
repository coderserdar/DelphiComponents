{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Description:  Demonstration for Client program using TWSocket.
Creation:     8 december 1997
Version:      8.00
EMail:        francois.piette@overbyte.be  http://www.overbyte.be
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 1997-2012 by François PIETTE
              Rue de Grady 24, 4053 Embourg, Belgium.
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
Dec 09, 1997 V1.01 Made it compatible with Delphi 1
Jul 09, 1998 V1.02 Adapted for Delphi 4
Dec 05, 1998 V1.03 Don't use TWait component
Dec 15, 2001 V1.04 Use LineMode
Jan 12, 2004 V1.05 Remove wait loop and use pure event driven code
Jul 30, 2006 V1.06 Added checkboxes to allow adding CRLF or not and to allow
                   embedded binary chars. To enter a binary char in the edit
                   box, simply enter a $ followed by thow digit hex ascii code.
                   To enter a $ sign, enter $24.
Dec 20, 2008 V1.07 Replace StrPas by a string cast. Removed an implicit
                   conversion to string by an explicit to avoid a warning.
May 2012 - V8.00 - Arno converted demo for FireMonkey cross platform Mac
                   OS X support, now XE2 and later only uising FMX components


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit IcsCliDemo1;

interface

{$I Include\OverbyteIcsDefs.inc}
{$IF CompilerVersion < 23}
  {$MESSAGE FATAL 'This project requires Delphi or RAD Studio XE2 or better'};
{$IFEND}

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  System.IOUtils, FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs,
  FMX.Layouts, FMX.Memo, FMX.Edit,
  FMX.StdCtrls,
  { Don't forget to add your vc32 directory to Delphi library path }
  OverbyteIcsUtils, OverbyteIcsIniFiles,
  OverbyteIcsWndControl, OverbyteIcsWSocket, FMX.Memo.Types, FMX.Controls.Presentation, FMX.ScrollBox;

const
  CliDemoVersion     = 800;
  CopyRight : String = ' CliDemo (c) 1997-2012 F. Piette V8.00 ';

type
  TClientForm = class(TForm)
    CliSocket: TWSocket;
    DisplayMemo: TMemo;
    Panel1: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    SendEdit: TEdit;
    SendButton: TButton;
    DisconnectButton: TButton;
    PortEdit: TEdit;
    ServerEdit: TEdit;
    Label3: TLabel;
    AllowBinaryCheckBox: TCheckBox;
    AddCRLFCheckBox: TCheckBox;
    procedure DisconnectButtonClick(Sender: TObject);
    procedure SendButtonClick(Sender: TObject);
    procedure CliSocketDataAvailable(Sender: TObject; ErrCode: Word);
    procedure CliSocketSessionConnected(Sender: TObject; ErrCode: Word);
    procedure CliSocketSessionClosed(Sender: TObject; ErrCode: Word);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
    Buffer       : array [0..1023] of AnsiChar;
    IniFileName  : String;
    procedure Display(Msg : String);
    procedure ProcessCommand(Cmd : String);
    procedure SendData;
    procedure CliSocketAddressListChanged(Sender: TObject; ErrCode: Word);
  end;

var
  ClientForm: TClientForm;

implementation

{$R *.FMX}

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TClientForm.DisconnectButtonClick(Sender: TObject);
begin
    CliSocket.Close;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TClientForm.CliSocketAddressListChanged(Sender: TObject; ErrCode: Word);
begin
    Display('AddressListChanged');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TClientForm.SendButtonClick(Sender: TObject);
begin
    if CliSocket.State = wsConnected then begin
        { Already connected, just send data }
        SendData;
    end
    else begin
        { Not connected yet, start connection }
        CliSocket.Proto    := 'tcp';
        CliSocket.Port     := PortEdit.Text;
        CliSocket.Addr     := ServerEdit.Text;
        CliSocket.LineMode := TRUE;
        CliSocket.LineEnd  := #13#10;
        CliSocket.OnAddressListChanged := CliSocketAddressListChanged;
        CliSocket.OnRoutingInterfaceChanged := CliSocketAddressListChanged;
        try
            CliSocket.Connect;
        except
            CliSocket.Abort;
            raise;
        end;
        { Connect is asynchronous (non-blocking). When the session is  }
        { connected (or fails to), we have an OnSessionConnected event }
        { This is where actual sending of data is done.                }
        SendButton.Enabled := FALSE;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TClientForm.SendData;
var
    Buf : String;
    I   : Integer;
begin
    try
        Buf := SendEdit.Text;
        if AddCRLFCheckBox.IsChecked then
            Buf := Buf + #13#10;
        if AllowBinaryCheckBox.IsChecked then begin
            { Allow embedded binary characters in the editbox, encoded as }
            { $ sign followed by two digit hex asccci code ($41 = 'A')    }
            I := 1;
            while I < (Length(Buf) - 2) do begin
                if Buf[I] = '$' then begin
                    Buf := Copy(Buf, 1, I - 1) +
                           Char(StrToInt(Copy(Buf, I, 3))) +
                           Copy(Buf, I + 3, Length(Buf));
                end;
                Inc(I);
            end;
        end;
        CliSocket.SendStr(Buf);
    except
        on E:Exception do Display(E.ClassName + ': ' + E.Message);
    end;
    ActiveControl := SendEdit;
    SendEdit.SelectAll;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TClientForm.ProcessCommand(Cmd : String);
begin
    { Here you should write your command interpreter.                       }
    { For simplicity, we just display received command !                    }
    { First remove EndOfLine marker                                         }
    if (Length(Cmd) >= Length(CliSocket.LineEnd)) and
       (Copy(Cmd, Length(Cmd) - Length(CliSocket.LineEnd) + 1,
             Length(CliSocket.LineEnd)) = String(CliSocket.LineEnd)) then
        Cmd := Copy(Cmd, 1, Length(Cmd) - Length(CliSocket.LineEnd));
    { Then display in memo                                                  }
    Display(Cmd);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TClientForm.CliSocketDataAvailable(Sender: TObject; ErrCode: Word);
var
    Len : Integer;
begin
    { We use line mode, we will receive a complete line }
    Len := CliSocket.Receive(@Buffer, SizeOf(Buffer) - 1);
    if Len <= 0 then
        Exit;

    Buffer[Len]       := #0;              { Nul terminate  }
    ProcessCommand(String(Buffer));       { Pass as string }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TClientForm.CliSocketSessionConnected(
    Sender  : TObject;
    ErrCode : Word);
begin
    SendButton.Enabled := TRUE;
    if ErrCode <> 0 then
        Display('Can''t connect, error #' + IntToStr(ErrCode))
    else begin
        DisconnectButton.Enabled := TRUE;
        SendData;  { Send the data from edit box }
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TClientForm.CliSocketSessionClosed(Sender: TObject; ErrCode: Word);
begin
    DisconnectButton.Enabled := FALSE;
    if ErrCode <> 0 then
        Display('Disconnected, error #' + IntToStr(ErrCode))
    else
        Display('Disconnected');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TClientForm.FormClose(Sender: TObject; var Action: TCloseAction);
var
    IniFile : TIcsIniFile;
begin
    IniFile := TIcsIniFile.Create(IniFileName);
    IniFile.WriteInteger('Window', 'Top',    Top);
    IniFile.WriteInteger('Window', 'Left',   Left);
    IniFile.WriteInteger('Window', 'Width',  Width);
    IniFile.WriteInteger('Window', 'Height', Height);
    IniFile.WriteString('Data', 'Server',  ServerEdit.Text);
    IniFile.WriteString('Data', 'Port',    PortEdit.Text);
    IniFile.WriteString('Data', 'Command', SendEdit.Text);
    IniFile.UpdateFile;
    IniFile.Free;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TClientForm.FormCreate(Sender: TObject);
var
    IniFile : TIcsIniFile;
begin
{$IFDEF MSWINDOWS}
    ReportMemoryLeaksOnShutdown := (DebugHook <> 0);
{$ENDIF}
    IcsNameThreadForDebugging('Main');
    IniFileName     := GetIcsIniFileName;
    IniFile         := TIcsIniFile.Create(IniFileName);

    Top             := IniFile.ReadInteger('Window', 'Top',    Top);
    Left            := IniFile.ReadInteger('Window', 'Left',   Left);
    Width           := IniFile.ReadInteger('Window', 'Width',  Width);
    Height          := IniFile.ReadInteger('Window', 'Height', Height);

    PortEdit.Text   := IniFile.ReadString('Data', 'Port',    '1024');
    ServerEdit.Text := IniFile.ReadString('Data', 'Server',  'localhost');
    SendEdit.Text   := IniFile.ReadString('Data', 'Command', 'LASTNAME CAESAR');

    IniFile.Free;

    DisplayMemo.Lines.Clear;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Display a message in our display memo. Delete lines to be sure to not     }
{ overflow the memo which may have a limited capacity.                      }
procedure TClientForm.Display(Msg : String);
var
    I : Integer;
begin
    DisplayMemo.Lines.BeginUpdate;
    try
        if DisplayMemo.Lines.Count > 200 then begin
            for I := 1 to 50 do
                DisplayMemo.Lines.Delete(0);
        end;
        DisplayMemo.Lines.Add(Msg);
    finally
        DisplayMemo.Lines.EndUpdate;
        DisplayMemo.GoToTextEnd;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
end.

