{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Creation:     Sep 13, 2009
Description:  Demo for SysLog server compoment
Version:      1.00
EMail:        francois.piette@overbyte.be    francois.piette@rtfm.be
              http://www.overbyte.be
Support:      Unsupported code.
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
unit OverbyteIcsSysLogServerDemo1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  OverbyteIcsIniFiles, StdCtrls, ExtCtrls, OverbyteIcsSysLogServer;

type
  TSysLogServerForm = class(TForm)
    ToolsPanel: TPanel;
    DisplayMemo: TMemo;
    StartButton: TButton;
    StopButton: TButton;
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure StartButtonClick(Sender: TObject);
    procedure SysLogServer1DataAvailable(Sender: TObject; const SrcIP, SrcPort,
      RawMessage: AnsiString);
    procedure StopButtonClick(Sender: TObject);
  private
    FIniFileName : String;
    FInitialized : Boolean;
    FSysLogServer: TSysLogServer;
    procedure SetButtons(Listening:Boolean);
  public
    procedure Display(Msg : String);
    property IniFileName : String read FIniFileName write FIniFileName;
  end;

var
  SysLogServerForm: TSysLogServerForm;

implementation

{$R *.DFM}

const
    SectionWindow      = 'Window';   // Must be unique for each window
    KeyTop             = 'Top';
    KeyLeft            = 'Left';
    KeyWidth           = 'Width';
    KeyHeight          = 'Height';


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSysLogServerForm.FormCreate(Sender: TObject);
begin
    FSysLogServer := TSysLogServer.Create(Self);
    FIniFileName := OverbyteIcsIniFiles.GetIcsIniFileName;
    DisplayMemo.Clear;
    FSysLogServer.OnDataAvailable := SysLogServer1DataAvailable;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSysLogServerForm.FormShow(Sender: TObject);
var
    IniFile : TIcsIniFile;
begin
    if not FInitialized then begin
        FInitialized := TRUE;

        IniFile      := TIcsIniFile.Create(FIniFileName);
        try
            Width        := IniFile.ReadInteger(SectionWindow, KeyWidth,  Width);
            Height       := IniFile.ReadInteger(SectionWindow, KeyHeight, Height);
            Top          := IniFile.ReadInteger(SectionWindow, KeyTop,
                                               (Screen.Height - Height) div 2);
            Left         := IniFile.ReadInteger(SectionWindow, KeyLeft,
                                               (Screen.Width  - Width)  div 2);
        finally
            IniFile.Free;
        end;
        DisplayMemo.Clear;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSysLogServerForm.FormClose(Sender: TObject; var Action: TCloseAction);
var
    IniFile : TIcsIniFile;
begin
    IniFile := TIcsIniFile.Create(FIniFileName);
    try
        IniFile.WriteInteger(SectionWindow, KeyTop,         Top);
        IniFile.WriteInteger(SectionWindow, KeyLeft,        Left);
        IniFile.WriteInteger(SectionWindow, KeyWidth,       Width);
        IniFile.WriteInteger(SectionWindow, KeyHeight,      Height);
        IniFile.UpdateFile;
    finally
        IniFile.Free;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSysLogServerForm.Display(Msg : String);
begin
    DisplayMemo.Lines.BeginUpdate;
    try
        if DisplayMemo.Lines.Count > 200 then begin
            while DisplayMemo.Lines.Count > 200 do
                DisplayMemo.Lines.Delete(0);
        end;
        DisplayMemo.Lines.Add(Msg);
    finally
        DisplayMemo.Lines.EndUpdate;
        SendMessage(DisplayMemo.Handle, EM_SCROLLCARET, 0, 0);
    end;
end;

procedure TSysLogServerForm.SetButtons(Listening:Boolean);
begin
  StartButton.Enabled := not Listening;
  StopButton.Enabled  := Listening;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSysLogServerForm.StartButtonClick(Sender: TObject);
begin
    FSysLogServer.Listen;
    SetButtons(TRUE);
end;


procedure TSysLogServerForm.StopButtonClick(Sender: TObject);
begin
    FSysLogServer.Close;
    SetButtons(FALSE);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSysLogServerForm.SysLogServer1DataAvailable(
    Sender: TObject;
    const SrcIP, SrcPort, RawMessage: AnsiString);
var
    Decoded : TSysLogData;
begin
    try
        FSysLogServer.ParseRawMessage(RawMessage, Decoded);
    except
        on E:Exception do begin
            Display(E.Message);
            Exit;
        end;
    end;
    Display('Received from ' + String(SrcIP) + ':' + String(SrcPort));
    Display('   RawMessage       = ' + String(RawMessage));
    Display('   PRI              = ' + IntToStr(Decoded.Pri));
    Display('   TimeStamp        = ' + Decoded.TimeString);
    Display('   DateTime         = ' + Format('%04.4d-%02.2d-%02.2d %02.2d:%02.2d:%02.2d.%03.3d',
                                           [Decoded.Year, Decoded.Month,
                                            Decoded.Day, Decoded.Hour,
                                            Decoded.Minute, Decoded.Second,
                                            Decoded.MilliSec]));
    Display('   Facility         = ' + IntToStr(Ord(Decoded.Facility)));
    Display('   Severity         = ' + IntToStr(Ord(Decoded.Severity)));
    Display('   HostName         = ' + Decoded.Hostname);
    Display('   Process          = ' + Decoded.Process);
    Display('   PID              = ' + IntToStr(Decoded.PID));
    Display('   Text             = ' + String(Decoded.Text));
    Display('   RFC5424          = ' + IntToStr(Ord(Decoded.RFC5424)));
    if Decoded.RFC5424 then begin
        Display('   MsgVer           = ' + IntToStr(Decoded.MsgVer));
        Display('   MsgID            = ' + String(Decoded.MsgID));
        Display('   StructData       = ' + String(Decoded.StructData));
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}


end.
