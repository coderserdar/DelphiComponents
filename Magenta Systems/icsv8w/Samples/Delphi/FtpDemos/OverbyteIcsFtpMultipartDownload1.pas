{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Creation:     Mar 13, 2003
Description:
Version:      8.00 ALPHA CODE
EMail:        francois.piette@overbyte.be    francois.piette@rtfm.be
              http://www.overbyte.be
Support:      Unsupported code.
Legal issues: Copyright (C) 2003 by François PIETTE
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
Jul 19, 2008 V6.00 F. Piette made small changes for Unicode, bumped version
                   number to 6.00
May 2012 - V8.00 - this is a Windows only demo, IPv4 only


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsFtpMultipartDownload1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  OverbyteIcsIniFiles, StdCtrls, ExtCtrls, OverbyteIcsWndControl,
  OverbyteIcsMultipartFtpDownloader, OverbyteIcsMultiProgressBar,
  OverbyteIcsFtpCli;

type
  TMultipartFtpDownloadForm = class(TForm)
    ToolsPanel: TPanel;
    DisplayMemo: TMemo;
    MPFtp: TMultipartFtpDownloader;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    FtpServerEdit: TEdit;
    FtpPortEdit: TEdit;
    FtpDirEdit: TEdit;
    LocalFilePathEdit: TEdit;
    Label5: TLabel;
    Label6: TLabel;
    FtpUserEdit: TEdit;
    FtpPassEdit: TEdit;
    PassiveCheckBox: TCheckBox;
    BinaryCheckBox: TCheckBox;
    Label7: TLabel;
    FtpFileNameEdit: TEdit;
    BottomPanel: TPanel;
    CountLabel: TLabel;
    MPBar: TMultiProgressBar;
    PartCountLabel: TLabel;
    PartCountEdit: TEdit;
    AbortButton: TButton;
    DownloadButton: TButton;
    ResumeButton: TButton;
    PauseButton: TButton;
    ClearButton: TButton;
    Label8: TLabel;
    AssumedSizeEdit: TEdit;
    TestButton: TButton;
    FtpClient1: TFtpClient;
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure DownloadButtonClick(Sender: TObject);
    procedure MPFtpDisplay(Sender: TObject; const Msg: String);
    procedure MPFtpProgressAddSegment(Sender: TObject; StartOffset, ASpan,
      InitPos: Int64);
    procedure MPFtpProgressSetPosition(Sender: TObject; Index: Integer;
      Position: Int64);
    procedure MPFtpShowStats(Sender: TObject);
    procedure MPFtpRequestDone(Sender: TObject; ErrorCode: Integer;
      const Reason: String);
    procedure ClearButtonClick(Sender: TObject);
    procedure PauseButtonClick(Sender: TObject);
    procedure ResumeButtonClick(Sender: TObject);
    procedure AbortButtonClick(Sender: TObject);
    procedure TestButtonClick(Sender: TObject);
    procedure FtpClient1Display(Sender: TObject; var Msg: string);
  private
    FIniFileName : String;
    FInitialized : Boolean;
  public
    procedure Display(Msg : String);
    property IniFileName : String read FIniFileName write FIniFileName;
  end;

var
  MultipartFtpDownloadForm: TMultipartFtpDownloadForm;

implementation

{$R *.DFM}

const
    SectionWindow      = 'Window';   // Must be unique for each window
    KeyTop             = 'Top';
    KeyLeft            = 'Left';
    KeyWidth           = 'Width';
    KeyHeight          = 'Height';
    SectionData        = 'Data';
    KeyServer          = 'Server';
    KeyPort            = 'Port';
    KeyUserCode        = 'UserCode';
    KeyPassword        = 'Password';
    KeyDir             = 'Dir';
    KeyFile            = 'File';
    KeyLocalPath       = 'LocalPath';
    KeyPassive         = 'Passive';
    KeyBinary          = 'Binary';
    KeyPartCount       = 'PartCount';
    KeyAssumedSize     = 'AssumedSize';


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMultipartFtpDownloadForm.FormCreate(Sender: TObject);
begin
    FIniFileName := GetIcsIniFileName;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMultipartFtpDownloadForm.FormShow(Sender: TObject);
var
    IniFile : TIcsIniFile;
begin
    if not FInitialized then begin
        FInitialized := TRUE;

        IniFile      := TIcsIniFile.Create(FIniFileName);
        Width        := IniFile.ReadInteger(SectionWindow, KeyWidth,  Width);
        Height       := IniFile.ReadInteger(SectionWindow, KeyHeight, Height);
        Top          := IniFile.ReadInteger(SectionWindow, KeyTop,
                                            (Screen.Height - Height) div 2);
        Left         := IniFile.ReadInteger(SectionWindow, KeyLeft,
                                            (Screen.Width  - Width)  div 2);
        FtpServerEdit.Text      := IniFile.ReadString(
                                       SectionData, KeyServer,
                                       'ftp.borland.com');
        FtpPortEdit.Text        := IniFile.ReadString(
                                       SectionData, KeyPort,
                                       'ftp');
        FtpUserEdit.Text        := IniFile.ReadString(
                                       SectionData, KeyUserCode,
                                       'anonymous');
        FtpPassEdit.Text        := IniFile.ReadString(
                                       SectionData, KeyPassword,
                                       'guest@unknown');
        FtpDirEdit.Text         := IniFile.ReadString(
                                       SectionData, KeyDir,
                                       '/pub/delphi/techpubs/delphi6');
        FtpFileNameEdit.Text    := IniFile.ReadString(
                                       SectionData, KeyFile,
                                       'UsingWebSnap.pdf');
        LocalFilePathEdit.Text  := IniFile.ReadString(
                                       SectionData, KeyLocalPath,
                                       'c:\temp\UsingWebSnap.pdf');
        PartCountEdit.Text      := IniFile.ReadString(
                                       SectionData, KeyPartCount,
                                       '10');
        AssumedSizeEdit.Text := IniFile.ReadString(SectionData, KeyAssumedSize, '0');
        PassiveCheckBox.Checked := IniFile.ReadBool(SectionData,
                                                    KeyPassive, TRUE);
        BinaryCheckBox.Checked  := IniFile.ReadBool(SectionData,
                                                    KeyBinary, TRUE);
        IniFile.Free;
        DisplayMemo.Clear;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMultipartFtpDownloadForm.FormClose(Sender: TObject; var Action: TCloseAction);
var
    IniFile : TIcsIniFile;
begin
    IniFile := TIcsIniFile.Create(FIniFileName);
    IniFile.WriteInteger(SectionWindow, KeyTop,         Top);
    IniFile.WriteInteger(SectionWindow, KeyLeft,        Left);
    IniFile.WriteInteger(SectionWindow, KeyWidth,       Width);
    IniFile.WriteInteger(SectionWindow, KeyHeight,      Height);
    IniFile.WriteString(SectionData, KeyServer,      FtpServerEdit.Text);
    IniFile.WriteString(SectionData, KeyPort,        FtpPortEdit.Text);
    IniFile.WriteString(SectionData, KeyUserCode,    FtpUserEdit.Text);
    IniFile.WriteString(SectionData, KeyPassword,    FtpPassEdit.Text);
    IniFile.WriteString(SectionData, KeyDir,         FtpDirEdit.Text);
    IniFile.WriteString(SectionData, KeyFile,        FtpFileNameEdit.Text);
    IniFile.WriteString(SectionData, KeyLocalPath,   LocalFilePathEdit.Text);
    IniFile.WriteString(SectionData, KeyPartCount,   PartCountEdit.Text);
    IniFile.WriteString(SectionData, KeyAssumedSize, AssumedSizeEdit.Text);
    IniFile.WriteBool(SectionData, KeyPassive, PassiveCheckBox.Checked);
    IniFile.WriteBool(SectionData, KeyBinary,  BinaryCheckBox.Checked);
    IniFile.UpdateFile;
    IniFile.Free;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMultipartFtpDownloadForm.Display(Msg : String);
begin
    DisplayMemo.Lines.BeginUpdate;
    try
        if DisplayMemo.Lines.Count > 1000 then begin
            while DisplayMemo.Lines.Count > 1000 do
                DisplayMemo.Lines.Delete(0);
        end;
        DisplayMemo.Lines.Add(Msg);
    finally
        DisplayMemo.Lines.EndUpdate;
        SendMessage(DisplayMemo.Handle, EM_SCROLLCARET, 0, 0);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function StrToIntDef64(
    const S : String;
    DefVal  : Int64) : Int64;
var
    I   : Integer;
    Neg : Boolean;
begin
    Result := 0;
    Neg    := FALSE;
    I      := 0;
    while I < Length(S) do begin
        Inc(I);
        if (I = 1) and ((S[I] = '+') or (S[I] = '-')) then begin
            Neg := (S[I] = '-');
            continue;
        end;
        if (S[I] >= '0') and (S[I] <= '9') then
            Result := Result * 10 + Ord(S[I]) - Ord('0')
        else begin
            Result := DefVal;
            Exit;
        end;
    end;
    if Neg then
        Result := -Result;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMultipartFtpDownloadForm.DownloadButtonClick(Sender: TObject);
begin
    MPBar.Clear;
    MPFtp.Server        := Trim(FtpServerEdit.Text);
    MPFtp.Port          := Trim(FtpPortEdit.Text);
    MPFtp.User          := Trim(FtpUserEdit.Text);
    MPFtp.Pass          := Trim(FtpPassEdit.Text);
    MPFtp.Dir           := Trim(FtpDirEdit.Text);
    MPFtp.FileName      := Trim(FtpFileNameEdit.Text);
    MPFtp.Passive       := PassiveCheckBox.Checked;
    MPFtp.Binary        := BinaryCheckBox.Checked;
    MPFtp.AssumedSize   := StrToIntDef64(AssumedSizeEdit.Text, 0);
    MPFtp.StateFileName := Trim(LocalFilePathEdit.Text) + '.Status';
    MPFtp.PartCount     := StrToIntDef(Trim(PartCountEdit.Text), 0);
    MPFtp.FileStream    := TFileStream.Create(Trim(LocalFilePathEdit.Text), fmCreate);
    try
        MPFtp.Start;
    except
        on E:Exception do begin
            MPFtp.FileStream.Free;
            MPFtp.FileStream := nil;
            Display('Unable to start download. ' +
                    E.ClassName + ': ' + E.Message);
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMultipartFtpDownloadForm.MPFtpDisplay(
    Sender: TObject;
    const Msg: String);
begin
    Display(Msg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMultipartFtpDownloadForm.MPFtpProgressAddSegment(
    Sender                      : TObject;
    StartOffset, ASpan, InitPos : Int64);
begin
    MPBar.AddSegment(StartOffset, ASpan, InitPos, clBlue);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMultipartFtpDownloadForm.MPFtpProgressSetPosition(
    Sender   : TObject;
    Index    : Integer;
    Position : Int64);
begin
    MPBar.SetPosition(Index, Position);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMultipartFtpDownloadForm.MPFtpShowStats(
    Sender: TObject);
begin
    CountLabel.Caption :=
            'Bytes: ' + IntToStr(MPFtp.TotalCount) +
             '   %: ' + Format('%3.0f', [MPFtp.PercentDone]) +
          '   Kbps: ' + Format('%6.2f', [MPFtp.CurSpeed]) +
        '  Elapsed: ' + FormatDateTime('hh:nn:ss', MPFtp.ElapsedTime);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMultipartFtpDownloadForm.MPFtpRequestDone(
    Sender       : TObject;
    ErrorCode    : Integer;
    const Reason : String);
begin
    MPFtp.FileStream.Free;
    MPFtp.FileStream := nil;
    if ErrorCode = 200 then
        Display('Finished')
    else
        Display('Finished, error #' + IntToStr(ErrorCode) + ' ' + Reason);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMultipartFtpDownloadForm.AbortButtonClick(Sender: TObject);
begin
    MPFtp.Abort;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMultipartFtpDownloadForm.ClearButtonClick(Sender: TObject);
begin
    DisplayMemo.Clear;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMultipartFtpDownloadForm.PauseButtonClick(Sender: TObject);
begin
    MPFtp.Pause;
    MPFtp.FileStream.Free;
    MPFtp.FileStream := nil;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMultipartFtpDownloadForm.ResumeButtonClick(Sender: TObject);
begin
    MPBar.Clear;
    MPFtp.Server        := Trim(FtpServerEdit.Text);
    MPFtp.Port          := Trim(FtpPortEdit.Text);
    MPFtp.User          := Trim(FtpUserEdit.Text);
    MPFtp.Pass          := Trim(FtpPassEdit.Text);
    MPFtp.Dir           := Trim(FtpDirEdit.Text);
    MPFtp.FileName      := Trim(FtpFileNameEdit.Text);
    MPFtp.Passive       := PassiveCheckBox.Checked;
    MPFtp.Binary        := BinaryCheckBox.Checked;
    MPFtp.StateFileName := Trim(LocalFilePathEdit.Text) + '.Status';
    MPFtp.FileStream    := TFileStream.Create(Trim(LocalFilePathEdit.Text), fmOpenWrite);
    try
        MPFtp.Resume;
    except
        on E:Exception do begin
            MPFtp.FileStream.Free;
            MPFtp.FileStream := nil;
            Display('Unable to resume download. ' +
                    E.ClassName + ': ' + E.Message);
        end;
    end;
end;


procedure TMultipartFtpDownloadForm.FtpClient1Display(Sender: TObject;
  var Msg: string);
begin
    Display(Msg);
end;

procedure TMultipartFtpDownloadForm.TestButtonClick(Sender: TObject);
begin
    FtpClient1.ResumeAt      := StrToInt(AssumedSizeEdit.Text);
    FtpClient1.HostDirName   := FtpDirEdit.Text;
    FtpClient1.HostFileName  := FtpFileNameEdit.Text;
    FtpClient1.LocalFileName := LocalFilePathEdit.Text;
    FtpClient1.HostName      := FtpServerEdit.Text;
    FtpClient1.Port          := FtpPortEdit.Text;
    FtpClient1.UserName      := FtpUserEdit.Text;
    FtpClient1.PassWord      := FtpPassEdit.Text;
    FtpClient1.Passive       := TRUE;
    FtpClient1.Binary        := TRUE;
    FtpClient1.RestartGet;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.
