{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Creation:     Jul 18, 2005
Description:  Demo to show how to do several FTP download in parallel do
              get a list of files. No optimization has been done to avoid
              disconnecting if the next file is on the same server: a new
              connection is done for each file regardless of the fact that it
              is on the same server.
Version:      8.67
EMail:        francois.piette@overbyte.be  http://www.overbyte.be
Support:      https://en.delphipraxis.net/forum/37-ics-internet-component-suite/
Legal issues: Copyright (C) 2005-2021 by François PIETTE
              Rue de Grady 24, 4053 Embourg, Belgium. 

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
Apr 6, 2021 - V8.67 - Made Win64 compatible by correcting Integer(Pointer)
                         typecasts to W/LPARAM for PostMessage, thanks to Fr0sT.
                      Beware ftp.simtel.net no longer exists, you need your
                        own download URLs.  

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsFtpMulti1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  OverbyteIcsIniFiles, StdCtrls, ExtCtrls, OverbyteIcsFtpCli, OverbyteIcsUrl;

const
  WM_NEXT_URL = WM_USER + 1;

type
  { We use use derived component to be able to store the data we }
  { need to handle our multiple transfert.                       }
  TMyFtpClient = Class(TFtpClient)
  protected
    FSuccess      : Boolean;
    FSavedMessage : String;
    FURL          : String;
    FBusy         : Boolean;
  end;

  TFtpMultiForm = class(TForm)
    ToolsPanel: TPanel;
    DisplayMemo: TMemo;
    FileListMemo: TMemo;
    Label1: TLabel;
    DownloadButton: TButton;
    Label2: TLabel;
    DirEdit: TEdit;
    Label3: TLabel;
    ComponentCountEdit: TEdit;
    DoneMemo: TMemo;
    Label4: TLabel;
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure DownloadButtonClick(Sender: TObject);
  private
    FIniFileName : String;
    FInitialized : Boolean;
    FFtpList     : TList;
    FUrlList     : TStringList;
    procedure CreateComponents;
    procedure DeleteComponents;
    procedure StartDownload;
    procedure FtpRequestDone(Sender: TObject; RqType: TFtpRequest;
      ErrCode: Word);
    procedure FtpDisplay(Sender: TObject; var Msg: String);
    procedure WMNextUrl(var msg: TMessage); message WM_NEXT_URL;
    function PickUrl(FtpCli: TMyFtpClient): Boolean;
  public
    procedure Display(Msg : String);
    property IniFileName : String read FIniFileName write FIniFileName;
  end;

var
  FtpMultiForm: TFtpMultiForm;

implementation

{$R *.DFM}

const
    SectionWindow      = 'Window';
    KeyTop             = 'Top';
    KeyLeft            = 'Left';
    KeyWidth           = 'Width';
    KeyHeight          = 'Height';


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpMultiForm.FormCreate(Sender: TObject);
begin
    FIniFileName := OverbyteIcsIniFiles.GetIcsIniFileName;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpMultiForm.FormShow(Sender: TObject);
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
procedure TFtpMultiForm.FormClose(Sender: TObject; var Action: TCloseAction);
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
procedure TFtpMultiForm.Display(Msg : String);
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


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpMultiForm.DownloadButtonClick(Sender: TObject);
begin
    DoneMemo.Clear;
    DisplayMemo.Clear;
    DeleteComponents;
    CreateComponents;
    StartDownload;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpMultiForm.CreateComponents;
var
    I : Integer;
    FtpCli : TMyFtpClient;
begin
    if not Assigned(FFtpList) then
        FFtpList := TList.Create;
    for I := 1 to StrToInt(ComponentCountEdit.Text) do begin
        FtpCli               := TMyFtpClient.Create(nil);
        FtpCli.Tag           := I;
        FtpCli.OnRequestDone := FtpRequestDone;
        FtpCli.OnDisplay     := FtpDisplay;
        FFtpList.Add(FtpCli);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure SplitPath(
    const Path     : String;
    var   HostDir  : String;
    var   HostFile : String);
var
    I : Integer;
begin
    I := Length(Path);
    while (I > 0) and (Path[I] <> '/') do
        Dec(I);
    if I = 0 then begin
        HostDir := '';
        HostFile := Path;
    end
    else begin
        HostDir  := Copy(Path, 1, I - 1);
        HostFile := Copy(Path, I + 1, Length(Path));
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TFtpMultiForm.PickUrl(FtpCli : TMyFtpClient) : Boolean;
var
    URL : String;
    Proto, User, Pass, Host, Port, Path : String;
    HostDir, HostFile : String;
begin
    Result := FALSE;
    while TRUE do begin
        if FUrlList.Count <= 0 then
            Exit;
        URL := FUrlList.Strings[0];
        FUrlList.Delete(0);
        Url := Trim(Url);
        if Url = '' then
            Continue;
        ParseURL(URL, Proto, User, Pass, Host, Port, Path);
        if Path = '' then
            Continue;
        SplitPath(Path, HostDir, HostFile);
        if Proto = '' then
            Proto := 'ftp'
        else
            Proto := LowerCase(Proto);
        if Proto <> 'ftp' then begin
            Display('Bad protocol in url ' + URL);
            Continue;
        end;
        if Port = '' then
            Port := 'ftp';
        if User = '' then begin
            User := 'anonymous';
            if Pass = '' then
                Pass := 'guest@unknown.com';
        end;
        break;
    end;

    FtpCli.FURL          := URL;
    FtpCli.UserName      := User;
    FtpCli.Password      := Pass;
    FtpCli.HostName      := Host;
    FtpCli.Port          := Port;
    FtpCli.HostDirName   := HostDir;
    FtpCli.HostFileName  := HostFile;
    FtpCli.LocalFileName := DirEdit.Text + '\' + HostFile;
    FtpCli.Binary        := TRUE;
    FtpCli.Passive       := TRUE;
    FtpCli.FBusy         := TRUE;
    FtpCli.OpenAsync;
    Result := TRUE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpMultiForm.StartDownload;
var
    I   : Integer;
begin
    if not Assigned(FUrlList) then
        FUrlList := TStringList.Create
    else
        FUrlList.Clear;
    FUrlList.Assign(FileListMemo.Lines);
    I := 0;
    while (I < FFtpList.Count) and (FUrlList.Count > 0) do begin
        if PickUrl(TMyFtpClient(FFtpList.Items[I])) then
            Inc(I);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpMultiForm.DeleteComponents;
begin
    if not Assigned(FFtpList) then
        Exit;
    while FFtpList.Count > 0 do begin
        TObject(FFtpList.Items[FFtpList.Count - 1]).Free;
        FFtpList.Delete(FFtpList.Count - 1);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpMultiForm.FtpDisplay(Sender: TObject; var Msg: String);
var
    FtpCli : TMyFtpClient;
begin
    FtpCli := Sender as TMyFtpClient;
    Display('[' + IntToStr(FtpCli.Tag) + '] ' + Msg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This event handler is used by all FTP component. It is used to start the  }
{ next operation for each transfert.                                        }
procedure TFtpMultiForm.FtpRequestDone(
    Sender  : TObject;
    RqType  : TFtpRequest;
    ErrCode : Word);
var
    FtpCli : TMyFtpClient;
begin
    FtpCli := Sender as TMyFtpClient;
{   Display('OnRequestDone. RqType = ' + IntToStr(Ord(RqType)) + '    ' +
            'ErrCode = ' + IntToStr(ErrCode)); }
    if ErrCode <> 0 then begin
        FtpCli.FSavedMessage := FtpCli.ErrorMessage;
        if RqType = ftpQuitAsync then
            PostMessage(Handle, WM_NEXT_URL, 0, LPARAM(FtpCli))       { V8.67 was Integer }
        else begin
            { Here we could extend the program to put the failed transfert }
            { on a retry list to try it later. Or simply put it again on   }
            { the list of files. Of course one should detect infinite loop }
            Display('*** ERROR ' + IntToStr(ErrCode) + ' ***');
            Display('*** ' + FtpCli.FSavedMessage + ' ***');
        end;
        Exit;
    end;
    case RqType of
    ftpOpenAsync: FtpCli.UserAsync;
    ftpUserAsync: FtpCli.PassAsync;
    ftpPassAsync: if FtpCli.HostDirName = '' then
                      FtpCli.TypeSetAsync
                  else
                      FtpCli.CwdAsync;
    ftpCwdAsync:  FtpCli.TypeSetAsync;
    ftpTypeSetAsync: FtpCli.GetAsync;
    ftpGetAsync:  begin
                      FtpCli.FSuccess := (FtpCli.StatusCode = 226);
                      { We could optimize by not closing the connection until }
                      { we have the next file to get. If not on same server,  }
                      { we reuse the same connection. If the next file is on  }
                      { another server, then we would close and open the      }
                      { other server.                                         }
                      FtpCli.QuitAsync;
                  end;
    ftpQuitAsync: begin
                      Display('');
                      if FtpCli.FSuccess then
                          Display('File downloaded into "' + FtpCli.LocalFileName + '"')
                      else begin
                          DeleteFile(FtpCli.LocalFileName);
                          Display(FtpCli.FSavedMessage);
                      end;
                      Display('Done.');
                      PostMessage(Handle, WM_NEXT_URL, 0, LPARAM(FtpCli));   { V8.67 was Integer }
                  end;
    else
        Display('*** Unknown RqType ' + IntToStr(Ord(RqType)) + ' ***');
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpMultiForm.WMNextUrl(var msg: TMessage);
var
    FtpCli : TMyFtpClient;
    I      : Integer;
    Busy   : Boolean;
begin
    FtpCli := TMyFtpClient(Msg.LParam);
    DoneMemo.Lines.Add(FtpCli.FURL);
    FtpCli.FBusy := FALSE;
    PickUrl(FtpCli);

    // Check if all FtpCli are not busy anymore
    Busy := FALSE;
    for I := 0 to FFtpList.Count - 1 do
        Busy := Busy or TMyFtpClient(FFtpList.Items[I]).FBusy;
    if not Busy then begin
        Display('Finished !');
        Beep;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.
