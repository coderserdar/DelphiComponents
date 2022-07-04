{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Object:       Simple client application demonstrating TWSocket object in action.
Creation:     November 28, 1998
Version:      1.01
EMail:        francois.piette@pophost.eunet.be
WebSite:      http://www.rtfm.be/fpiette/indexuk.htm
Support:      Use the mailing list twsocket@rtfm.be See website for details.
Legal issues: Copyright (C) 1998 by François PIETTE <francois.piette@pophost.eunet.be>

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
Mar 07, 1999  V1.01 Adapted for Delphi 1

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit Cli7;

interface

uses
  WinTypes, WinProcs, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, IniFiles, WSocket;

const
  EndOfLine = #13#10;

type
  TCli7Form = class(TForm)
    Panel1: TPanel;
    PortEdit: TEdit;
    Label6: TLabel;
    HostNameEdit: TEdit;
    Label1: TLabel;
    DisplayMemo: TMemo;
    ConnectButton: TButton;
    LineOnButton: TButton;
    LineOffButton: TButton;
    DisconnectButton: TButton;
    ReadLineButton: TButton;
    WSocket1: TWSocket;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure LineOnButtonClick(Sender: TObject);
    procedure LineOffButtonClick(Sender: TObject);
    procedure ConnectButtonClick(Sender: TObject);
    procedure DisconnectButtonClick(Sender: TObject);
    procedure ReadLineButtonClick(Sender: TObject);
    procedure WSocket1SessionConnected(Sender: TObject; Error: Word);
    procedure WSocket1SessionClosed(Sender: TObject; Error: Word);
    procedure WSocket1DataAvailable(Sender: TObject; Error: Word);
  private
    FIniFileName : String;
    FInitialized : Boolean;
    procedure Display(Msg : String);
  end;

var
  Cli7Form: TCli7Form;

implementation

{$R *.DFM}
const
    SectionWindow = 'Window';
    KeyTop        = 'Top';
    KeyLeft       = 'Left';
    KeyWidth      = 'Width';
    KeyHeight     = 'Height';
    SectionData   = 'Data';
    KeyHostName   = 'HostName';
    KeyPort       = 'Port';


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCli7Form.FormCreate(Sender: TObject);
begin
    FIniFileName := LowerCase(ExtractFileName(Application.ExeName));
    FIniFileName := Copy(FIniFileName, 1, Length(FIniFileName) - 3) + 'ini';
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCli7Form.FormShow(Sender: TObject);
var
    IniFile : TIniFile;
begin
    if not FInitialized then begin
        FInitialized := TRUE;
        IniFile := TIniFile.Create(FIniFileName);
        HostNameEdit.Text  := IniFile.ReadString(SectionData, KeyHostName,
                                                 'localhost');
        PortEdit.Text      := IniFile.ReadString(SectionData, KeyPort,
                                                 'telnet');

        Width  := IniFile.ReadInteger(SectionWindow, KeyWidth,  Width);
        Height := IniFile.ReadInteger(SectionWindow, KeyHeight, Height);
        Top    := IniFile.ReadInteger(SectionWindow, KeyTop,    (Screen.Height - Height) div 2);
        Left   := IniFile.ReadInteger(SectionWindow, KeyLeft,   (Screen.Width - Width) div 2);

        IniFile.Free;
        DisplayMemo.Clear;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCli7Form.FormClose(Sender: TObject; var Action: TCloseAction);
var
    IniFile : TIniFile;
begin
    IniFile := TIniFile.Create(FIniFileName);
    IniFile.WriteString(SectionData, KeyHostName,  HostNameEdit.Text);
    IniFile.WriteString(SectionData, KeyPort,      PortEdit.Text);
    IniFile.WriteInteger(SectionWindow, KeyTop,    Top);
    IniFile.WriteInteger(SectionWindow, KeyLeft,   Left);
    IniFile.WriteInteger(SectionWindow, KeyWidth,  Width);
    IniFile.WriteInteger(SectionWindow, KeyHeight, Height);
    IniFile.Free;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCli7Form.Display(Msg : String);
begin
    if DisplayMemo.Lines.Count > 200 then   { Prevent TMemo overflow }
        DisplayMemo.Clear;
    DisplayMemo.Lines.Add(Msg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCli7Form.LineOnButtonClick(Sender: TObject);
begin
    WSocket1.LineMode := TRUE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCli7Form.LineOffButtonClick(Sender: TObject);
begin
    WSocket1.LineMode := FALSE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCli7Form.ConnectButtonClick(Sender: TObject);
begin
    WSocket1.Proto    := 'tcp';
    WSocket1.Port     := PortEdit.Text;
    WSocket1.Addr     := HostnameEdit.Text;
    WSocket1.LineMode := TRUE;
    WSocket1.LineEnd  := EndOfLine;
    WSocket1.Connect;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCli7Form.DisconnectButtonClick(Sender: TObject);
begin
    WSocket1.Close;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCli7Form.WSocket1SessionConnected(Sender: TObject;
  Error: Word);
begin
    if Error <> 0 then
        Display('Connection failed, error #' + IntToStr(Error))
    else
        Display('Session Connected.');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCli7Form.WSocket1SessionClosed(Sender: TObject;
  Error: Word);
begin
    Display('Session Closed.');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function RemoveEndOfLine(const Line : String) : String;
const
    EOL : String = EndOfLine;   { To make Delphi 1 happy }
begin

    if (Length(Line) >= Length(EndOfLine)) and
       (StrLComp(@Line[1 + Length(Line) - Length(EndOfLine)],
                 @EOL[1],
                 Length(EndOfLine)) = 0) then
        Result := Copy(Line, 1, Length(Line) - Length(EndOfLine))
    else
        Result := Line;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCli7Form.WSocket1DataAvailable(Sender: TObject;
  Error: Word);
var
    Buf : array [0..127] of char;
    Len : Integer;
begin
    Len := TCustomLineWSocket(Sender).Receive(@Buf, Sizeof(Buf) - 1);
    if Len <= 0 then
        Exit;
    Buf[Len] := #0;
    if not WSocket1.LineMode then
        { Normal mode, data is just a buffer with all caracters }
        Display('DataAvailable (' + IntToStr(Len) +' bytes): ''' +
                StrPas(@Buf) + '''')
    else begin
        { Line mode, buffer contains exactly one line, terminated by the }
        { LineEnd string, unless our buffer is too small in which case   }
        { the line is truncated. We'll get the end of line on the next   }
        { call to Receive.                                               }
        Display('Line: ''' + RemoveEndOfLine(StrPas(@Buf)) + '''');
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCli7Form.ReadLineButtonClick(Sender: TObject);
var
    Buffer : String;
begin
    Display('Waiting for a line... (Timeout = 10Sec)');
    WSocket1.ReadLine(10000, Buffer);
    Display('Received line: ''' + RemoveEndOfLine(Buffer) + '''');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.
