{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Description:  Simple server program which just listen for clients and display
              all incomming data.
EMail:        francois.piette@pophost.eunet.be    francois.piette@rtfm.be
              http://www.rtfm.be/fpiette
Creation:     Sep 29, 1998
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
Oct 28, 1998  V1.01 Added Linger and Banner checkboxes.
Dec 30, 1998  V1.02 Remove trailing CR/LF on data receive.
Mar 07, 1999  V1.03 Adapted for Delphi 1

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit Recv1;

interface

uses
  WinTypes, WinProcs, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, WSocket, StdCtrls, ExtCtrls, IniFiles;

const
  WM_DESTROY_SOCKET = WM_USER + 1;

type
  TRecvForm = class(TForm)
    Panel1: TPanel;
    DisplayMemo: TMemo;
    Label1: TLabel;
    PortEdit: TEdit;
    ActionButton: TButton;
    WSocket1: TWSocket;
    CloseAllButton: TButton;
    Label2: TLabel;
    LingerCheckBox: TCheckBox;
    BannerCheckBox: TCheckBox;
    LineModeOnButton: TButton;
    LineOffButton: TButton;
    procedure ActionButtonClick(Sender: TObject);
    procedure PortEditChange(Sender: TObject);
    procedure WSocket1SessionAvailable(Sender: TObject; Error: Word);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure CloseAllButtonClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure LineModeOnButtonClick(Sender: TObject);
    procedure LineOffButtonClick(Sender: TObject);
  private
    FIniFileName : String;
    FInitialized : Boolean;
    FClients     : TList;
    procedure ClientDataAvailable(Sender : TObject; Error : Word);
    procedure ClientSessionClosed(Sender: TObject; Error: Word);
    procedure Display(Msg : String);
    procedure WMDestroySocket(var msg: TMessage); message WM_DESTROY_SOCKET;
  end;

var
  RecvForm: TRecvForm;

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
    KeyLinger       = 'Linger';
    KeyBanner       = 'SendBanner';


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TRecvForm.FormCreate(Sender: TObject);
begin
    FIniFileName := LowerCase(ExtractFileName(Application.ExeName));
    FIniFileName := Copy(FIniFileName, 1, Length(FIniFileName) - 3) + 'ini';
    FClients     := TList.Create;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TRecvForm.FormDestroy(Sender: TObject);
begin
    if Assigned(FClients) then begin
        FClients.Destroy;
        FClients := nil;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TRecvForm.FormShow(Sender: TObject);
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
        PortEdit.Text := IniFile.ReadString(SectionData, KeyPort, 'telnet');
        LingerCheckBox.Checked := Boolean(IniFile.ReadInteger(SectionData, KeyLinger, 0));
        BannerCheckBox.Checked := Boolean(IniFile.ReadInteger(SectionData, KeyBanner, 1));
        Label2.Caption := '';
        IniFile.Destroy;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TRecvForm.FormClose(Sender: TObject; var Action: TCloseAction);
var
    IniFile : TIniFile;
begin
    IniFile := TIniFile.Create(FIniFileName);
    IniFile.WriteInteger(SectionWindow, KeyTop,    Top);
    IniFile.WriteInteger(SectionWindow, KeyLeft,   Left);
    IniFile.WriteInteger(SectionWindow, KeyWidth,  Width);
    IniFile.WriteInteger(SectionWindow, KeyHeight, Height);
    IniFile.WriteString(SectionData,    KeyPort,   PortEdit.text);
    IniFile.WriteInteger(SectionData,   KeyLinger, Ord(LingerCheckBox.Checked));
    IniFile.WriteInteger(SectionData,   KeyBanner, Ord(BannerCheckBox.Checked));
    IniFile.Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TRecvForm.Display(Msg : String);
begin
    if DisplayMemo.Lines.Count > 200 then   { Prevent TMemo overflow }
        DisplayMemo.Clear;
    DisplayMemo.Lines.Add(Msg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TRecvForm.ActionButtonClick(Sender: TObject);
begin
    if ActionButton.Caption = '&Start' then begin
        WSocket1.Addr     := '0.0.0.0';
        WSocket1.Port     := PortEdit.Text;
        WSocket1.Proto    := 'tcp';
        WSocket1.Listen;
        ActionButton.Caption := '&Stop';
        Display('Listening for clients');
    end
    else begin
        WSocket1.Close;
        ActionButton.Caption := '&Start';
        Display('Not listening for clients');
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TRecvForm.PortEditChange(Sender: TObject);
begin
    WSocket1.Close;
    ActionButton.Caption := '&Start';
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TRecvForm.WSocket1SessionAvailable(Sender: TObject; Error: Word);
var
    NewClient : TWSocket;
begin
    Display('Client connected');
    Label2.Caption := '';
    NewClient := TWSocket.Create(nil);
    FClients.Add(NewClient);
    NewClient.LineMode            := TRUE;
    NewClient.OnDataAvailable     := ClientDataAvailable;
    NewClient.OnSessionClosed     := ClientSessionClosed;
    NewClient.HSocket             := WSocket1.Accept;
    if LingerCheckBox.Checked then
        NewClient.LingerOnOff     := wsLingerOn
    else
        NewClient.LingerOnOff     := wsLingerOff;
    NewClient.LingerTimeout       := 300;
    NewClient.SetLingerOption;
    if BannerCheckBox.Checked then
        NewClient.SendStr('Hello !' + #13#10);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TRecvForm.ClientDataAvailable(Sender : TObject; Error : Word);
var
    Buf : array [0..127] of char;
    Len : Integer;
begin
    Len := TWSocket(Sender).Receive(@Buf, Sizeof(Buf) - 1);
    if Len <= 0 then
        Exit;
    { Remove any trailing CR/LF}
    while (Len > 0) and (Buf[Len - 1] in [#13, #10]) do
        Dec(Len);
    { Nul terminate the data }
    Buf[Len] := #0;
    Display('DataAvailable: ''' + StrPas(@Buf) + '''');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TRecvForm.ClientSessionClosed(Sender: TObject; Error: Word);
var
    Cli : TWSocket;
    Itm : Integer;
begin
    Cli := Sender as TWSocket;
    Display('Client diconnected');

    Itm := FClients.IndexOf(Cli);
    if Itm >= 0 then
        FClients.Delete(Itm);
    { We can't destroy a TWSocket from a SessionClosed event handler.   }
    { So we post a message to delay destruction until we are out of the }
    { message handler.                                                  }
    PostMessage(Handle, WM_DESTROY_SOCKET, 0, LongInt(Cli));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TRecvForm.WMDestroySocket(var msg: TMessage);
begin
    TWSocket(msg.LParam).Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TRecvForm.CloseAllButtonClick(Sender: TObject);
begin
    Display('Disconnecting clients');
    while FClients.Count > 0 do
        TWSocket(FClients.Items[0]).Close;
    Display('All clients disconnected');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TRecvForm.LineModeOnButtonClick(Sender: TObject);
var
    I : Integer;
begin
    for I := 0 to FClients.Count - 1 do
        TWSocket(FClients.Items[0]).LineMode := TRUE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TRecvForm.LineOffButtonClick(Sender: TObject);
var
    I : Integer;
begin
    for I := 0 to FClients.Count - 1 do
        TWSocket(FClients.Items[0]).LineMode := FALSE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.

