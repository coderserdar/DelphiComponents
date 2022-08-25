{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Creation:     June 17, 2003
Description:  This sample program shows how to use FTP asynchronous functions
              and OnRequestDone event to get a list of files from a server.
Version:      1.00
EMail:        francois.piette@overbyte.be  http://www.overbyte.be
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 2003-2010 by François PIETTE
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
unit OverbyteIcsFtpAsy1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  OverbyteIcsIniFiles, StdCtrls, ExtCtrls, OverbyteIcsFtpCli, OverbyteIcsWndControl;

type
  TFtpAsyncForm = class(TForm)
    ToolsPanel: TPanel;
    Panel1: TPanel;
    DisplayMemo: TMemo;
    Panel2: TPanel;
    HostNameEdit: TEdit;
    Label1: TLabel;
    Label4: TLabel;
    UserNameEdit: TEdit;
    Label5: TLabel;
    PassWordEdit: TEdit;
    Label6: TLabel;
    PortEdit: TEdit;
    cbBinary: TCheckBox;
    Label2: TLabel;
    HostDirEdit: TEdit;
    Label3: TLabel;
    HostFileEdit: TEdit;
    AddFileButton: TButton;
    FtpClient1: TFtpClient;
    ExecButton: TButton;
    Label7: TLabel;
    LocalDirEdit: TEdit;
    Panel3: TPanel;
    FilesListBox: TListBox;
    ResultsListBox: TListBox;
    RemoveButton: TButton;
    ReplaceButton: TButton;
    AbortButton: TButton;
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure AddFileButtonClick(Sender: TObject);
    procedure ExecButtonClick(Sender: TObject);
    procedure FtpClient1Display(Sender: TObject; var Msg: String);
    procedure FtpClient1RequestDone(Sender: TObject; RqType: TFtpRequest;
      ErrCode: Word);
    procedure RemoveButtonClick(Sender: TObject);
    procedure ReplaceButtonClick(Sender: TObject);
    procedure AbortButtonClick(Sender: TObject);
  private
    FIniFileName : String;
    FInitialized : Boolean;
    FCurrentFile : Integer;
    procedure GetNextFile;
    procedure GetFinished;
  public
    procedure Display(Msg : String);
    property IniFileName : String read FIniFileName write FIniFileName;
  end;

var
  FtpAsyncForm: TFtpAsyncForm;

implementation

{$R *.DFM}

const
    SectionWindow      = 'Window';   // Must be unique for each window
    KeyTop             = 'Top';
    KeyLeft            = 'Left';
    KeyWidth           = 'Width';
    KeyHeight          = 'Height';
    SectionData        = 'Data';
    KeyHostName        = 'HostName';
    KeyUserName        = 'UserName';
    KeyPassWord        = 'PassWord';
    KeyHostDir         = 'HostDir';
    KeyLocalDir        = 'LocalDir';
    KeyPort            = 'Port';
    KeyHostFile        = 'HostFile';


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpAsyncForm.FormCreate(Sender: TObject);
begin
    FIniFileName := OverbyteIcsIniFiles.GetIcsIniFileName;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpAsyncForm.FormShow(Sender: TObject);
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
            HostNameEdit.Text  := IniFile.ReadString(SectionData, KeyHostName,
                                                 'ftp.simtel.net');
            PortEdit.Text      := IniFile.ReadString(SectionData, KeyPort,
                                                    'ftp');
            UserNameEdit.Text  := IniFile.ReadString(SectionData, KeyUserName,
                                                    'anonymous');
            PassWordEdit.Text  := IniFile.ReadString(SectionData, KeyPassWord,
                                                    'your.name@your.company.com');
            LocalDirEdit.Text  := IniFile.ReadString(SectionData, KeyLocalDir,
                                                    'c:\temp');
            HostDirEdit.Text   := IniFile.ReadString(SectionData, KeyHostDir,
                                                    '/pub/simtelnet');
            HostFileEdit.Text  := IniFile.ReadString(SectionData, KeyHostFile,
                                                    'simtel.html');
        finally
            IniFile.Free;
        end;
        FilesListBox.Items.Add('simtel40.gif');
        FilesListBox.Items.Add('CDROMS1.TXT');
        FilesListBox.Items.Add(Trim(HostFileEdit.Text));
        DisplayMemo.Clear;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpAsyncForm.FormClose(Sender: TObject; var Action: TCloseAction);
var
    IniFile : TIcsIniFile;
begin
    IniFile := TIcsIniFile.Create(FIniFileName);
    try
        IniFile.WriteInteger(SectionWindow, KeyTop,         Top);
        IniFile.WriteInteger(SectionWindow, KeyLeft,        Left);
        IniFile.WriteInteger(SectionWindow, KeyWidth,       Width);
        IniFile.WriteInteger(SectionWindow, KeyHeight,      Height);
        IniFile.WriteString(SectionData, KeyHostName,  HostNameEdit.Text);
        IniFile.WriteString(SectionData, KeyPort,      PortEdit.Text);
        IniFile.WriteString(SectionData, KeyUserName,  UserNameEdit.Text);
        IniFile.WriteString(SectionData, KeyPassWord,  PassWordEdit.Text);
        IniFile.WriteString(SectionData, KeyLocalDir,  LocalDirEdit.Text);
        IniFile.WriteString(SectionData, KeyHostDir,   HostDirEdit.Text);
        IniFile.WriteString(SectionData, KeyHostFile,  HostFileEdit.Text);
        IniFile.UpdateFile;
    finally
        IniFile.Free;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpAsyncForm.Display(Msg : String);
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
procedure TFtpAsyncForm.AddFileButtonClick(Sender: TObject);
begin
    if Trim(HostFileEdit.Text) <> '' then
        FilesListBox.Items.Add(HostFileEdit.Text);
    ActiveControl := HostFileEdit;
    HostFileEdit.SelectAll;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpAsyncForm.ExecButtonClick(Sender: TObject);
begin
    DisplayMemo.Clear;
    ResultsListBox.Clear;
    FCurrentFile           := -1;
    FtpClient1.HostName    := Trim(HostNameEdit.Text);
    FtpClient1.Port        := Trim(PortEdit.Text);
    FtpClient1.UserName    := Trim(UserNameEdit.Text);
    FtpClient1.PassWord    := Trim(PasswordEdit.Text);
    FtpClient1.HostDirName := Trim(HostDirEdit.Text);
    FtpClient1.Binary      := cbBinary.Checked;
    FtpClient1.OpenAsync;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpAsyncForm.FtpClient1Display(Sender: TObject;
  var Msg: String);
begin
    Display(Msg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpAsyncForm.FtpClient1RequestDone(Sender: TObject;
  RqType: TFtpRequest; ErrCode: Word);
begin
    if RqType = ftpGetAsync then begin
        if ErrCode = 0 then
            ResultsListBox.Items.Add('File received ' +
                                     FilesListBox.Items.Strings[FCurrentFile])
        else
            ResultsListBox.Items.Add('FAILED (Error ' +
                                     IntToStr(ErrCode) + ') ' +
                                     FilesListBox.Items.Strings[FCurrentFile]);
    end
    else begin
        if ErrCode <> 0 then begin
            Display('Error #' + IntToStr(ErrCode));
            Exit;
        end;
    end;

    try
        case RqType of
        ftpOpenAsync:    FtpClient1.User;
        ftpUserAsync:    FtpClient1.Pass;
        ftpPassAsync:    FtpClient1.Cwd;
        ftpCwdAsync:     FtpClient1.TypeSet;
        ftpTypeSetAsync: GetNextFile;
        ftpGetAsync:     GetNextFile;
        ftpQuitAsync:    GetFinished;
        else
            ResultsListBox.Items.Add('Unexpected RqType ' + IntToStr(Ord(RqType)));
        end;
    except
        on E:Exception do begin
            ResultsListBox.Items.Add('*** ' + E.Message + ' ***');
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpAsyncForm.GetNextFile;
begin
    Inc(FCurrentFile);
    if FCurrentFile >= FilesListBox.Items.Count then begin
        Display('End of list reached');
        FtpClient1.Quit;
        FilesListBox.ItemIndex := -1;
        Exit;
    end;
    FilesListBox.ItemIndex   := FCurrentFile;
    FtpClient1.LocalFileName := Trim(LocalDirEdit.Text) + '\' +
                                FilesListBox.Items.Strings[FCurrentFile];
    FtpClient1.HostFileName  := FilesListBox.Items.Strings[FCurrentFile];
    FtpClient1.GetAsync;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpAsyncForm.GetFinished;
begin
    Display('Done !');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpAsyncForm.RemoveButtonClick(Sender: TObject);
var
    Item : Integer;
begin
    Item := FilesListBox.ItemIndex;
    if Item >= 0 then
        FilesListBox.Items.Delete(Item);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpAsyncForm.ReplaceButtonClick(Sender: TObject);
var
    Item : Integer;
begin
    Item := FilesListBox.ItemIndex;
    if Item < 0 then
        Exit;
    FilesListBox.Items.Delete(Item);
    FilesListBox.Items.Insert(Item, HostFileEdit.Text);
    FilesListBox.ItemIndex := Item;
    ActiveControl := HostFileEdit;
    HostFileEdit.SelectAll;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpAsyncForm.AbortButtonClick(Sender: TObject);
begin
    ResultsListBox.Items.Add('*** ABORTING ***');
    FtpClient1.Abort;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.
