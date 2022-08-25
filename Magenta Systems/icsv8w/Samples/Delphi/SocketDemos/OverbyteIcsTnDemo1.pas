{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Description:  How to use TnCnx (Telnet protocol) with a TMemo
Creation:     December 11, 1997
Version:      1.02
EMail:        francois.piette@overbyte.be  http://www.overbyte.be
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 1997-2010 by François PIETTE
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

Updates:
Oct 23, 2002 V1.01 Changed Buffer arg in OnDataAvailable to untyped var instead
                   of PChar. More portable.
Nov 15, 2010 V1.02 Updated TnCnxDataAvailable so that it support nul char
                   in the input stream. Added const in MemoAddLines arg.

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsTnDemo1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, OverbyteIcsTnCnx, OverbyteIcsWSocket, ExtCtrls,
  OverbyteIcsWndControl;

type
  TTnDemoForm = class(TForm)
    DisplayMemo: TMemo;
    Panel1: TPanel;
    TnCnx: TTnCnx;
    HostLabel: TLabel;
    HostEdit: TEdit;
    ConnectButton: TButton;
    InfoLabel: TLabel;
    DisconnectButton: TButton;
    PortLabel: TLabel;
    PortEdit: TEdit;
    Label1: TLabel;
    DataEdit: TEdit;
    SendButton: TButton;
    procedure ConnectButtonClick(Sender: TObject);
    procedure TnCnxDataAvailable(Sender: TTnCnx; Buffer : Pointer;
      Len: Integer);
    procedure TnCnxSessionConnected(Sender: TTnCnx; Error: Word);
    procedure DisplayMemoKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure DisplayMemoKeyPress(Sender: TObject; var Key: Char);
    procedure TnCnxSessionClosed(Sender: TTnCnx; Error: Word);
    procedure DisconnectButtonClick(Sender: TObject);
    procedure SendButtonClick(Sender: TObject);
  end;

var
  TnDemoForm: TTnDemoForm;

implementation

{$R *.DFM}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{* Display a message in the memo field, breaking with CR                   *}
procedure MemoAddLines(Memo : TMemo; const Msg : String);
const
    CR = #13;
    LF = #10;
var
    Start, Stop : Integer;
begin
    if Memo.Lines.Count = 0 then
        Memo.Lines.Add('');

    Start := 1;
    Stop  := Pos(CR, Msg);
    if Stop = 0 then
        Stop := Length(Msg) + 1;
    while Start <= Length(Msg) do begin
        Memo.Lines.Strings[Memo.Lines.Count - 1] :=
            Memo.Lines.Strings[Memo.Lines.Count - 1] +
            Copy(Msg, Start, Stop - Start);
        if Msg[Stop] = CR then begin
            Memo.Lines.Add('');
            SendMessage(Memo.Handle, WM_KEYDOWN, VK_UP, 1);
        end;
        Start := Stop + 1;
        if Start > Length(Msg) then
            Break;
        if Msg[Start] = LF then
           Start := Start + 1;
        Stop := Start;
        while (Msg[Stop] <> CR) and (Stop <= Length(Msg)) do
            Stop := Stop + 1;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTnDemoForm.ConnectButtonClick(Sender: TObject);
begin
    TnCnx.Host      := HostEdit.Text;
    TnCnx.Port      := PortEdit.Text;
    TnCnx.TermType  := 'VT100';
    TnCnx.LocalEcho := FALSE;
    TnCnx.Connect;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTnDemoForm.DisconnectButtonClick(Sender: TObject);
begin
    TnCnx.Close;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTnDemoForm.TnCnxSessionConnected(Sender: TTnCnx; Error: Word);
begin
    if Error <> 0 then begin
        DisplayMemo.Lines.Add('Unable to connect. Error #' + IntToStr(Error));
        Exit;
    end;

    DisplayMemo.Clear;
    InfoLabel.Caption        := 'Connected';
    DisplayMemo.Enabled      := TRUE;
    ConnectButton.Enabled    := FALSE;
    DisconnectButton.Enabled := TRUE;
    ActiveControl            := DisplayMemo;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTnDemoForm.TnCnxSessionClosed(Sender: TTnCnx; Error: Word);
begin
    InfoLabel.Caption        := 'Disconnected';
    DisplayMemo.Enabled      := FALSE;
    ConnectButton.Enabled    := TRUE;
    DisconnectButton.Enabled := FALSE;
    ActiveControl            := ConnectButton;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTnDemoForm.TnCnxDataAvailable(
    Sender: TTnCnx; Buffer : Pointer; Len: Integer);
var
    Data : AnsiString;
begin
    if Len <= 0 then
        Exit;
    SetLength(Data, Len);
    Move(Buffer^, Data[1], Len);
    MemoAddLines(DisplayMemo, String(Data));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTnDemoForm.DisplayMemoKeyDown(Sender: TObject; var Key: Word;
    Shift: TShiftState);
begin
    Key := 0;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTnDemoForm.DisplayMemoKeyPress(Sender: TObject; var Key: Char);
begin
    TnCnx.Send(@Key, 1);
    if Key = #13 then begin
        { Send a LF after CR key }
        Key := #10;
        TnCnx.Send(@Key, 1);
    end;
    Key := #0;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTnDemoForm.SendButtonClick(Sender: TObject);
begin
    if TnCnx.State = wsConnected then
        TnCnx.SendStr(DataEdit.Text + #13#10)
    else
    DisplayMemo.Lines.Add('*** NOT CONNECTED ***');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.

