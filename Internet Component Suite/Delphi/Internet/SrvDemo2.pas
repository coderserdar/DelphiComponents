{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Description:  Demonstration for Server program using TWSocket.
Creation:     8 december 1997
Version:      1.03
EMail:        francois.piette@pophost.eunet.be
              francois.piette@rtfm.be             http://www.rtfm.be/fpiette
Support:      Use the mailing list twsocket@rtfm.be See website for details.
Legal issues: Copyright (C) 1997, 1998 by François PIETTE
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
Dec 28, 1998 V1.02 Use line mode.
Mar 07, 1999 V1.03 Adapted for Delphi 1


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit SrvDemo2;

interface

uses
  WinTypes, WinProcs, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, WSocket, StdCtrls, Db, DBTables, ExtCtrls;

type
  TCliForm = class(TForm)
    CliSocket: TWSocket;
    DisplayMemo: TMemo;
    Panel1: TPanel;
    SendEdit: TEdit;
    SendButton: TButton;
    Panel2: TPanel;
    LineLabel: TLabel;
    DisconnectButton: TButton;
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure CliSocketDataAvailable(Sender: TObject; Error: Word);
    procedure CliSocketSessionClosed(Sender: TObject; Error: Word);
    procedure SendButtonClick(Sender: TObject);
    procedure DisconnectButtonClick(Sender: TObject);
  private
    { Déclarations privées }
    Initialized : Boolean;
    Buffer : array [0..1023] of char;
    procedure ProcessCommand(Cmd : String);
  public
    { Déclarations publiques }
    DataTable : TTable;
  end;

var
  CliForm: TCliForm;

implementation

{$R *.DFM}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF VER80}
procedure SetLength(var S: string; NewLength: Integer);
begin
    S[0] := chr(NewLength);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
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
procedure TCliForm.FormShow(Sender: TObject);
begin
    if not Initialized then begin
        Initialized   := TRUE;
        DisplayMemo.Clear;
        SendEdit.Text := 'Hello world !';
        ActiveControl := SendEdit;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCliForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
    PostMessage(TForm(Owner).Handle, WM_USER, 0, LongInt(Self));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCliForm.ProcessCommand(Cmd : String);
var
    CommandVerb : String;
    CommandTail : String;
    I, J        : Integer;
begin
    DisplayMemo.Lines.Add(Cmd);

    { Skip leading spaces }
    I := 1;
    while (I <= Length(Cmd)) and (Cmd[I] in [' ', #9]) do
        Inc(I);

    { Find separator and separe CommandVerb and CommandTail }
    J := I;
    while TRUE do begin
        if (J >= Length(Cmd)) then begin
            CommandTail := '';
            CommandVerb := Cmd;
            break;
        end;

        if Cmd[J] in [' ', #9, '/'] then begin
            CommandTail := Copy(Cmd, J, Length(Cmd) - J + 1);
            CommandVerb := Copy(Cmd, I, J - I);
            break;
        end;
        Inc(J);
    end;
    CommandVerb := UpperCase(CommandVerb);
    CommandTail := Trim(CommandTail);

    if CommandVerb = 'LASTNAME' then begin
        DataTable.IndexName := 'NOM';
        DataTable.SetKey;
        DataTable.FieldByName('NOM').AsString := CommandTail;
    end
    else if CommandVerb = 'FIRSTNAME' then begin
        DataTable.IndexName := 'PRENOM';
        DataTable.SetKey;
        DataTable.FieldByName('PRENOM').AsString := CommandTail;
    end
    else begin
        CliSocket.SendStr('Syntax error !' + #13 + #10);
        Exit;
    end;

    if DataTable.GotoKey then
        CliSocket.SendStr(
            '"' + DataTable.FieldByName('NOM').AsString + '", ' +
            '"' + DataTable.FieldByName('PRENOM').AsString + '", ' +
            '"' + DataTable.FieldByName('ADRESSE').AsString + '", ' +
            '"' + DataTable.FieldByName('CP').AsString + '", ' +
            '"' + DataTable.FieldByName('LOCALITE').AsString + '"' +
            #13 + #10)
    else
        CliSocket.SendStr('Pas trouvé' + #13 + #10);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCliForm.CliSocketDataAvailable(Sender: TObject; Error: Word);
var
    Len : Integer;
begin
    { We use line mode. So when we call Receive, we always receive a }
    { complete line, include end of line marker or nothing.          }
    Len := CliSocket.Receive(@Buffer[0], SizeOf(Buffer) - 1);
    if Len <= 0 then
        Exit;

    { Remove end of line marker }
    while (Len > 0) and (Buffer[Len - 1] in [#13, #10]) do
        Dec(Len);

    { Nul terminate the string }
    Buffer[Len] := #0;
    { Display command in label }
    LineLabel.Caption := StrPas(Buffer);
    { Process command }
    ProcessCommand(StrPas(Buffer));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCliForm.CliSocketSessionClosed(Sender: TObject; Error: Word);
begin
    Close;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCliForm.SendButtonClick(Sender: TObject);
begin
    CliSocket.SendStr(SendEdit.Text + #13 + #10);
    ActiveControl := SendEdit;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCliForm.DisconnectButtonClick(Sender: TObject);
begin
    Close;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.

