{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Description:  Demonstration for Client program using TWSocket.
Creation:     8 december 1997
Version:      1.03
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

Updates:
Dec 09, 1997 V1.01 Made it compatible with Delphi 1
Jul 09, 1998 V1.02 Adapted for Delphi 4
Dec 05, 1998 V1.03 Don't use TWait component


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit CliDemo1;

interface

uses
  WinTypes, WinProcs, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, IniFiles, WSocket, ExtCtrls;

const
  IniFileName = 'CliDemo.ini';

type
  TClientForm = class(TForm)
    CliSocket: TWSocket;
    DisplayMemo: TMemo;
    Panel1: TPanel;
    LineLabel: TLabel;
    Label1: TLabel;
    Label2: TLabel;
    SendEdit: TEdit;
    SendButton: TButton;
    DisconnectButton: TButton;
    PortEdit: TEdit;
    ServerEdit: TEdit;
    procedure DisconnectButtonClick(Sender: TObject);
    procedure SendButtonClick(Sender: TObject);
    procedure CliSocketDataAvailable(Sender: TObject; Error: Word);
    procedure CliSocketSessionConnected(Sender: TObject; Error: Word);
    procedure CliSocketSessionClosed(Sender: TObject; Error: Word);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
  private
    { Déclarations privées }
    Buffer       : array [0..1023] of char;
    Count        : Integer;
    ConnectError : Word;
    Initialized  : Boolean;
    procedure ProcessCommand(Cmd : String);
  public
    { Déclarations publiques }
  end;

var
  ClientForm: TClientForm;

implementation

{$R *.DFM}

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TClientForm.DisconnectButtonClick(Sender: TObject);
begin
    CliSocket.Close;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TClientForm.SendButtonClick(Sender: TObject);
begin
    if CliSocket.State <> wsConnected then begin
        CliSocket.Proto := 'tcp';
        CliSocket.Port  := PortEdit.Text;
        CliSocket.Addr  := ServerEdit.Text;
        CliSocket.Connect;
        { Connect is asynchronous (non-blocking). We will wait while the }
        { session is connecting or application terminated.               }
        while CliSocket.State in [wsConnecting] do begin
            Application.ProcessMessages;
            if Application.Terminated then
                Exit;
        end;
    end;
    { Be sure we are connected before sending anything }
    if CliSocket.State = wsConnected then
        CliSocket.SendStr(SendEdit.Text + #13 + #10);
    ActiveControl := SendEdit;
    SendEdit.SelectAll;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TClientForm.ProcessCommand(Cmd : String);
begin
    DisplayMemo.Lines.Add(Cmd);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TClientForm.CliSocketDataAvailable(Sender: TObject; Error: Word);
var
    Len : Integer;
    I   : Integer;
begin
    Len := CliSocket.Receive(@Buffer[Count], SizeOf(Buffer) - Count - 1);
    if Len <= 0 then
        Exit;

    Count             := Count + Len;
    Buffer[Count]     := #0;
    LineLabel.Caption := StrPas(Buffer);

    while TRUE do begin
        I := 0;
        while (I < Count) and (Buffer[I] <> #10) do
            Inc(I);
        if I >= Count then
            Exit;
        ProcessCommand(Copy(StrPas(Buffer), 1, I));
        Count             := 0;
        LineLabel.Caption := '';
        if I >= Integer(StrLen(Buffer)) then
            break;
        Move(Buffer[I + 1], Buffer, Integer(Strlen(Buffer)) - I);
        LineLabel.Caption := StrPas(Buffer);
        Count             := StrLen(Buffer);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TClientForm.CliSocketSessionConnected(Sender: TObject;
  Error: Word);
begin
    ConnectError := Error;
    if Error <> 0 then
        DisplayMemo.Lines.Add('Can''t connect, error #' + IntToStr(Error))
    else
        DisconnectButton.Enabled := TRUE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TClientForm.CliSocketSessionClosed(Sender: TObject; Error: Word);
begin
    DisconnectButton.Enabled := FALSE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TClientForm.FormClose(Sender: TObject; var Action: TCloseAction);
var
    IniFile : TIniFile;
begin
    IniFile := TIniFile.Create(IniFileName);
    IniFile.WriteInteger('Window', 'Top',    Top);
    IniFile.WriteInteger('Window', 'Left',   Left);
    IniFile.WriteInteger('Window', 'Width',  Width);
    IniFile.WriteInteger('Window', 'Height', Height);
    IniFile.WriteString('Data', 'Server',  ServerEdit.Text);
    IniFile.WriteString('Data', 'Port',    PortEdit.Text);
    IniFile.WriteString('Data', 'Command', SendEdit.Text);
    IniFile.Free;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TClientForm.FormShow(Sender: TObject);
var
    IniFile : TIniFile;
begin
    if Initialized then
        Exit;
    Initialized := TRUE;
    IniFile         := TIniFile.Create(IniFileName);
    
    Top             := IniFile.ReadInteger('Window', 'Top',    Top);
    Left            := IniFile.ReadInteger('Window', 'Left',   Left);
    Width           := IniFile.ReadInteger('Window', 'Width',  Width);
    Height          := IniFile.ReadInteger('Window', 'Height', Height);

    PortEdit.Text   := IniFile.ReadString('Data', 'Port',    'telnet');
    ServerEdit.Text := IniFile.ReadString('Data', 'Server',  'localhost');
    SendEdit.Text   := IniFile.ReadString('Data', 'Command', 'LASTNAME CAESAR');

    IniFile.Free;

    DisplayMemo.Clear;
    ActiveControl := SendEdit;
    SendEdit.SelectAll;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.

