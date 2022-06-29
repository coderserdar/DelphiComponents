{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *


Author:       François PIETTE
Object:       RobotMail is a demo program for the SMTP and MBX component from
              the ICS package. It's a kind of spam machine...
Creation:     May 21, 1998
Version:      1.10  (Tested with Delphi 3 and 4)
EMail:        francois.piette@pophost.eunet.be    francois.piette@rtfm.be
              http://www.rtfm.be/fpiette
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

Updates:
Aug 03, 1998 V1.10 Adapted with new TSmtpCli component


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit MailRob1;

interface

uses
  WinTypes, WinProcs, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, IniFiles, MbxFile, StdCtrls, ExtCtrls, SmtpProt, DB, DBTables;

const
  RobotMailVersion = 110;

type
  TMailRobForm = class(TForm)
    MbxHandler: TMbxHandler;
    DisplayMemo: TMemo;
    TopPanel: TPanel;
    GetFromMbxButton: TButton;
    EMailMemo: TMemo;
    MiddlePanel: TPanel;
    Label1: TLabel;
    HostEdit: TEdit;
    PortEdit: TEdit;
    FromEdit: TEdit;
    Label2: TLabel;
    Subject: TLabel;
    SubjectEdit: TEdit;
    SignOnEdit: TEdit;
    Label4: TLabel;
    SmtpClient: TSyncSmtpCli;
    MbxFileEdit: TEdit;
    SendButton: TButton;
    SaveToListButton: TButton;
    LstFileEdit: TEdit;
    LoadFromListButton: TButton;
    InfoLabel: TLabel;
    Label3: TLabel;
    MsgFileEdit: TEdit;
    MsgFileLoadButton: TButton;
    SaveMsgFileButton: TButton;
    OpenDialog1: TOpenDialog;
    procedure GetFromMbxButtonClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SendButtonClick(Sender: TObject);
    procedure SmtpClientGetData(Sender: TObject; LineNum: Integer;
      MsgLine: PChar; MaxLen: Integer; var More: Boolean);
    procedure SaveToListButtonClick(Sender: TObject);
    procedure LoadFromListButtonClick(Sender: TObject);
    procedure SmtpClientCommand(Sender: TObject; Msg: String);
    procedure SmtpClientResponse(Sender: TObject; Msg: String);
    procedure MsgFileLoadButtonClick(Sender: TObject);
    procedure SaveMsgFileButtonClick(Sender: TObject);
    procedure MbxFileEditDblClick(Sender: TObject);
    procedure MsgFileEditDblClick(Sender: TObject);
    procedure LstFileEditDblClick(Sender: TObject);
  private
    FIniFileName         : String;
    FInitialized         : Boolean;
    FTxtFileName         : String;
    FMsgLines            : TStrings;
    FNames               : TList;
    FLogFileName         : String;
    FLog                 : TStream;
    FRunning             : Boolean;
    procedure ProcessMsg;
    function  SearchHeader(Key : String) : Integer;
    procedure ClearNames;
    procedure CommitLog;
    procedure CloseLog;
    procedure Log(const Msg : String);
    procedure LoadEMailMessage(FileName : String);
    procedure SaveEMailMessage(FileName : String);
  public
    { Déclarations publiques }
  end;

var
  MailRobForm: TMailRobForm;

implementation

{$R *.DFM}
const
    SectionWindow   = 'Window';
    KeyTop          = 'Top';
    KeyLeft         = 'Left';
    KeyWidth        = 'Width';
    KeyHeight       = 'Height';
    SectionData     = 'Data';
    KeyServer       = 'MailServer';
    KeyPort         = 'SmtpPort';
    KeyFrom         = 'From';
    KeySignOn       = 'SignOn';
    KeyMbxFile      = 'MbxFile';
    KeyLstFile      = 'LstFile';
    KeyMsgFile      = 'MsgFile';

    CrLf : array [0..1] of char = (#13, #10);

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMailRobForm.FormCreate(Sender: TObject);
begin
    FMsgLines    := TStringList.Create;
    FNames       := TList.Create;
    FIniFileName := LowerCase(Application.ExeName);
    FIniFileName := Copy(FIniFileName, 1, Length(FIniFileName) - 3) + 'ini';
    FTxtFileName := LowerCase(Application.ExeName);
    FTxtFileName := Copy(FTxtFileName, 1, Length(FTxtFileName) - 3) + 'txt';
    FLogFileName := LowerCase(Application.ExeName);
    FLogFileName := Copy(FLogFileName, 1, Length(FLogFileName) - 3) + 'log';
    CommitLog;
    Log('---- Start ----');
    InfoLabel.Caption := '';
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMailRobForm.FormShow(Sender: TObject);
var
    IniFile : TIniFile;
begin
    if not FInitialized then begin
        FInitialized := TRUE;
        DisplayMemo.Clear;

        IniFile          := TIniFile.Create(FIniFileName);
        Top              := IniFile.ReadInteger(SectionWindow, KeyTop,    Top);
        Left             := IniFile.ReadInteger(SectionWindow, KeyLeft,   Left);
        Width            := IniFile.ReadInteger(SectionWindow, KeyWidth,  Width);
        Height           := IniFile.ReadInteger(SectionWindow, KeyHeight, Height);
        HostEdit.Text    := IniFile.ReadString(SectionData, KeyServer,  'mail.server.provider');
        PortEdit.Text    := IniFile.ReadString(SectionData, KeyPort,    'smtp');
        FromEdit.Text    := IniFile.ReadString(SectionData, KeyFrom,    'your.email@your.provider');
        SignOnEdit.Text  := IniFile.ReadString(SectionData, KeySignOn,  'Your full name');
        MbxFileEdit.Text := IniFile.ReadString(SectionData, KeyMbxFile, '');
        LstFileEdit.Text := IniFile.ReadString(SectionData, KeyLstFile, 'c:\temp\emails.txt');
        MsgFileEdit.Text := IniFile.ReadString(SectionData, KeyMsgFile, 'c:\temp\emailmsg.txt');
        IniFile.Free;
        LoadEMailMessage(FTxtFileName);
    end;
    CloseLog;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMailRobForm.FormClose(Sender: TObject; var Action: TCloseAction);
var
    IniFile : TIniFile;
begin
    Log('Stop');
    IniFile := TIniFile.Create(FIniFileName);
    IniFile.WriteInteger(SectionWindow, KeyTop,       Top);
    IniFile.WriteInteger(SectionWindow, KeyLeft,      Left);
    IniFile.WriteInteger(SectionWindow, KeyWidth,     Width);
    IniFile.WriteInteger(SectionWindow, KeyHeight,    Height);
    IniFile.WriteSTring(SectionData, KeyServer,  HostEdit.Text);
    IniFile.WriteSTring(SectionData, KeyPort,    PortEdit.Text);
    IniFile.WriteSTring(SectionData, KeyFrom,    FromEdit.Text);
    IniFile.WriteSTring(SectionData, KeySignOn,  SignOnEdit.Text);
    IniFile.WriteSTring(SectionData, KeyMbxFile, MbxFileEdit.Text);
    IniFile.WriteSTring(SectionData, KeyLstFile, LstFileEdit.Text);
    IniFile.WriteSTring(SectionData, KeyMsgFile, MsgFileEdit.Text);
    IniFile.Free;
    SaveEMailMessage(FTxtFileName);
    MbxHandler.Active := FALSE;
    FMsgLines.Destroy;
    ClearNames;
    FNames.Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMailRobForm.LoadEMailMessage(FileName : String);
begin
    EMailMemo.Clear;
    SubjectEdit.Text := '';
    try
        EMailMemo.Lines.LoadFromFile(FileName);
        if EMailMemo.Lines.Count > 0 then begin
            SubjectEdit.Text := EMailMemo.Lines[0];
            EMailMemo.Lines.Delete(0);
        end;
    except
        on E:Exception do Log('LoadEMailMessage failed: ' + E.Message);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMailRobForm.SaveEMailMessage(FileName : String);
begin
    EMailMemo.Lines.Insert(0, SubjectEdit.Text);
    EMailMemo.Lines.SaveToFile(FileName);
    EMailMemo.Lines.Delete(0);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMailRobForm.CommitLog;
begin
    if Assigned(FLog) then
        FLog.Destroy;
    try
        FLog := TFileStream.Create(FLogFileName, fmOpenReadWrite or fmShareDenyNone);
    except
        on E:EFOpenError do begin
            try
                FLog := TFileStream.Create(FLogFileName, fmCreate);
                FLog.Destroy;
                FLog := TFileStream.Create(FLogFileName, fmOpenReadWrite or fmShareDenyNone);
            except
                DisplayMemo.Lines.Add('Can''t create log file ' + FLogFileName);
                FLog := nil;
            end;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMailRobForm.CloseLog;
begin
    if Assigned(FLog) then begin
       FLog.Destroy;
       FLog := nil;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMailRobForm.Log(const Msg : String);
var
    Buf : String;
begin
    if not Assigned(FLog) then begin
        CommitLog;
        if not Assigned(FLog) then
            Exit;
    end;
    Buf := FormatDateTime('yyyy/mm/dd hh:nn:ss ', Now);
    FLog.Seek(0, soFromEnd);
    FLog.WriteBuffer(Buf[1], Length(Buf));
    FLog.WriteBuffer(Msg[1], Length(Msg));
    FLog.WriteBuffer(CrLf, 2);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMailRobForm.GetFromMbxButtonClick(Sender: TObject);
begin
    if MbxFileEdit.Text = '' then begin
        Application.MessageBox('Please enter the file name in the edit box !',
                               'Warning', MB_OK);        
        Exit;
    end;

    GetFromMbxButton.Enabled := FALSE;
    try
        MbxHandler.FileName := MbxFileEdit.Text;
        MbxHandler.Active   := TRUE;
        ProcessMsg;
    finally
        GetFromMbxButton.Enabled := TRUE;
        MbxHandler.Active := FALSE;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TMailRobForm.SearchHeader(Key : String) : Integer;
var
    Line  : String;
    I, J  : Integer;
begin
    Key    := UpperCase(Key);
    Result := -1;
    I      := 0;
    while I < FMsgLines.Count do begin
        Line := FMsgLines.Strings[I];
        if Length(Line) = 0 then       { End of header }
            Break;
        J := Pos(':', Line);
        if (J > 0) and (UpperCase(Copy(Line, 1, J - 1)) = Key) then begin
            Result := I;
            Break;
        end;
        Inc(I);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMailRobForm.ClearNames;
begin
    while FNames.Count > 0 do begin
        FreeMem(FNames.Items[0], StrLen(PChar(FNames.Items[0])) + 1);
        FNames.Delete(0);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function StringCompare(Item1, Item2: Pointer): Integer;
begin
    Result := StrComp(PChar(Item1), PChar(Item2));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMailRobForm.ProcessMsg;
var
    Line     : String;
    I        : Integer;
    EMail    : String;
    p        : PChar;
    MsgCount : Integer;
    OkCount  : Integer;
begin
    DisplayMemo.Lines.Add('Clear list');
    ClearNames;

    DisplayMemo.Lines.Add('Reading EMail');
    MsgCount := 0;
    OkCount  := 0;
    while not MbxHandler.Eof do begin
        Inc(MsgCount);
        FMsgLines.LoadFromStream(MbxHandler.MsgStream);
        I := SearchHeader('SUBJECT');
        if I < 0 then
            DisplayMemo.Lines.Add('Subject not found')
        else begin
            Line := FMsgLines.Strings[I];

            if Copy(Line, 10, 9) = 'SUBSCRIBE' then begin
                I := Length(Line);
                while (I > 0) and (Line[I] <> ' ') do
                    Dec(I);
                EMail := Copy(Line, I + 1, 255);
                GetMem(p, Length(EMail) + 1);
                Move(EMail[1], p^, Length(EMail));
                FNames.Add(p);
                Inc(OkCount);
            end;
        end;
        MbxHandler.Next;
        InfoLabel.Caption := Format('%d/%d/%d', [MsgCount, OkCount, MbxHandler.MsgCount]);
        Application.ProcessMessages;
    end;

{$IFNDEF VER80}  { Delphi 1 does'nt support sorting TList items }
    DisplayMemo.Lines.Add('Sort list');
    FNames.Sort(StringCompare);
{$ENDIF}

    DisplayMemo.Lines.Add('Remove duplicates');
    Line := '';
    p    := @Line[1];
    I    := 0;
    while I < FNames.Count do begin
        if StringCompare(p, FNames.Items[I]) = 0 then begin
            FreeMem(FNames.Items[I], StrLen(PChar(FNames.Items[I])) + 1);
            FNames.Delete(I);
        end
        else begin
            p := FNames.Items[I];
            Inc(I);
        end;
    end;

    DisplayMemo.Lines.Add('Display list');
    for I := 0 to FNames.Count - 1 do
        DisplayMemo.Lines.Add(StrPas(PChar(FNames.Items[I])));

    DisplayMemo.Lines.Add('Total : ' + IntToStr(FNames.Count));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMailRobForm.SendButtonClick(Sender: TObject);
var
    EMail    : String;
    I        : Integer;
    OkCount  : Integer;
    BadCount : Integer;
    Success  : Boolean;
begin
    if FRunning then begin
        FRunning := FALSE;
        Exit;
    end;
    FRunning := TRUE;
    DisplayMemo.Lines.Add('Sending EMails');
    if FNames.Count <= 0 then begin
        Application.MessageBox('List is empty', 'Warning', MB_OK);
        Exit;
    end;

    OkCount  := 0;
    BadCount := 0;
    try
        SmtpClient.SignOn          := SignOnEdit.Text;
        SmtpClient.Host            := HostEdit.Text;
        SmtpClient.Port            := PortEdit.Text;
        Success := SmtpClient.OpenSync;
        if not Success then
            Exit;
        I := 0;
        while (not Application.Terminated) and (I < FNames.Count) do begin
            if not FRunning then begin
                Log('Canceled');
                CommitLog;
                DisplayMemo.Lines.Add('Canceled');
                Exit;
            end;
            EMail := StrPas(PChar(FNames.Items[I]));
            DisplayMemo.Lines.Add('Sending to ' + EMail);
            Log('Sending to ' + EMail);
            Success := FALSE;
            try
                SmtpClient.RcptName.Clear;
                SmtpClient.RcptName.Add(EMail);
                SmtpClient.HdrFrom         := FromEdit.Text;
                SmtpClient.HdrTo           := EMail;
                SmtpClient.HdrSubject      := SubjectEdit.Text;
                SmtpClient.FromName        := FromEdit.Text;
                SmtpClient.EmailFiles      := nil;
                Success                    := SmtpClient.MailSync;
            except
                on E:Exception do Log(E.Message);
            end;
            if Success then
                Inc(OkCount)
            else begin
                Inc(BadCount);
                Log('Can''t send to ' + EMail);
                DisplayMemo.Lines.Add('Can''t send to ' + EMail);
                { We failed, so disconnect before continuing }
                try
                    SmtpClient.Quit;
                except
                    on E:Exception do Log(E.Message);
                end;
                try
                    SmtpClient.Abort;
                except
                end;
                SmtpClient.SignOn          := SignOnEdit.Text;
                SmtpClient.Host            := HostEdit.Text;
                SmtpClient.Port            := PortEdit.Text;
                Success := SmtpClient.OpenSync;
                if not Success then
                    Exit;
            end;
            CommitLog;
            Inc(I);
            InfoLabel.Caption := Format('%d/%d/%d', [OkCount, BadCount, FNames.Count]);
        end;
    finally
        try
            SmtpClient.Quit;
        except
            on E:Exception do Log(E.Message);
        end;
        DisplayMemo.Lines.Add(IntToStr(OkCount) + ' emails sent succesfully');
        DisplayMemo.Lines.Add(IntToStr(BadCount) + ' failed');
        Log(IntToStr(OkCount) + ' emails sent succesfully');
        Log(IntToStr(BadCount) + ' failed');
        CloseLog;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMailRobForm.SmtpClientGetData(Sender: TObject; LineNum: Integer;
  MsgLine: PChar; MaxLen: Integer; var More: Boolean);
var
    Len : Integer;
begin
    if LineNum > EMailMemo.Lines.count then
        More := FALSE
    else begin
        Len := Length(EMailMemo.Lines[LineNum - 1]);
        { Truncate the line if too long (should wrap to next line) }
        if Len >= MaxLen then
            StrPCopy(MsgLine, Copy(EMailMemo.Lines[LineNum - 1], 1, MaxLen - 1))
        else
            StrPCopy(MsgLine, EMailMemo.Lines[LineNum - 1]);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMailRobForm.SaveToListButtonClick(Sender: TObject);
var
    Stream : TStream;
    I      : Integer;
begin
    DisplayMemo.Lines.Add('Saving to file');
    if FNames.Count <= 0 then begin
        Application.MessageBox('List is empty', 'Warning', MB_OK);
        Exit;
    end;

    Stream := TFileStream.Create(LstFileEdit.Text, fmCreate);
    try
        for I := 0 to FNames.Count - 1 do begin
            Stream.WriteBuffer(PChar(FNames.Items[I])^, StrLen(PChar(FNames.Items[I])));
            Stream.WriteBuffer(CrLf, 2);
        end;
        DisplayMemo.Lines.Add(IntToStr(FNames.Count) + ' EMails saved');
    finally
        Stream.Destroy;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMailRobForm.LoadFromListButtonClick(Sender: TObject);
var
    Stream   : TMemoryStream;
    I, J     : Integer;
    p, q     : PChar;
    Table    : TTable;
    Field    : TField;
begin
    DisplayMemo.Lines.Add('Loading from file');
    ClearNames;
    if UpperCase(ExtractFileExt(LstFileEdit.Text)) = '.DBF' then begin
        Table := TTable.Create(Self);
        try
            Table.DatabaseName := ExtractFilePath(LstFileEdit.Text);
            Table.TableName := ExtractFileName(LstFileEdit.Text);
            Table.Open;
            Field := Table.FieldByName('EMail');
            while not Table.Eof do begin
                GetMem(q, Length(Field.AsString) + 1);
                StrPCopy(q, Field.AsString);
                FNames.Add(q);
                if DisplayMemo.Lines.Count > 200 then
                    DisplayMemo.Clear;
                DisplayMemo.Lines.Add(StrPas(q));
                Table.Next;
            end;
        finally
            Table.Destroy;
        end;
    end
    else begin
        Stream := TMemoryStream.Create;
        Stream.LoadFromFile(LstFileEdit.Text);
        p := Stream.Memory;
        I := 0;
        while I < Stream.Size do begin
            J := I;
            while (I < Stream.Size) and (p[i] <> #13) do
                Inc(I);
            if p[I] = #13 then
                Dec(I);
            GetMem(q, I - J + 2);
            Move(p[J], q^, I - J + 1);
            q[I - J + 1] := #0;
            FNames.Add(q);
            if DisplayMemo.Lines.Count > 200 then
                DisplayMemo.Clear;
            DisplayMemo.Lines.Add(StrPas(q));
            I := I + 3;
        end;
        Stream.Destroy;
    end;
    DisplayMemo.Lines.Add(IntToStr(FNames.Count) + ' EMails loaded');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMailRobForm.SmtpClientCommand(Sender: TObject; Msg: String);
begin
    { Memo boxes are not unlimited...}
    if DisplayMemo.Lines.Count > 200 then
        DisplayMemo.Clear;
    DisplayMemo.Lines.Add('    ' + Msg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMailRobForm.SmtpClientResponse(Sender: TObject; Msg: String);
begin
    { Memo boxes are not unlimited...}
    if DisplayMemo.Lines.Count > 200 then
        DisplayMemo.Clear;
    DisplayMemo.Lines.Add('    ' + Msg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMailRobForm.MsgFileLoadButtonClick(Sender: TObject);
begin
    LoadEMailMessage(MsgFileEdit.Text);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMailRobForm.SaveMsgFileButtonClick(Sender: TObject);
begin
    SaveEMailMessage(MsgFileEdit.Text);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMailRobForm.MbxFileEditDblClick(Sender: TObject);
begin
    OpenDialog1.DefaultExt := '.mbx';
    OpenDialog1.Filter     := 'Mail files (*.mbx)|*.MBX|All files (*.*)|*.*';
    OpenDialog1.Options    := [ofFileMustExist];
    OpenDialog1.Title      := 'MailRob - Open MBX file';
    OpenDialog1.InitialDir := ExtractFilePath(MbxFileEdit.Text);
    if OpenDialog1.Execute then
         MbxFileEdit.Text := OpenDialog1.FileName;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMailRobForm.MsgFileEditDblClick(Sender: TObject);
begin
    OpenDialog1.DefaultExt := '.txt';
    OpenDialog1.Filter     := 'Message files (*.txt)|*.TXT|All files (*.*)|*.*';
    OpenDialog1.Options    := [ofFileMustExist];
    OpenDialog1.Title      := 'MailRob - Open message file';
    OpenDialog1.InitialDir := ExtractFilePath(MsgFileEdit.Text);
    if OpenDialog1.Execute then
         MsgFileEdit.Text := OpenDialog1.FileName;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMailRobForm.LstFileEditDblClick(Sender: TObject);
begin
    OpenDialog1.DefaultExt := '.txt';
    OpenDialog1.Filter     := 'AMail list files (*.txt)|*.TXT|All files (*.*)|*.*';
    OpenDialog1.Options    := [ofFileMustExist];
    OpenDialog1.Title      := 'MailRob - Open email list file';
    OpenDialog1.InitialDir := ExtractFilePath(LstFileEdit.Text);
    if OpenDialog1.Execute then
         LstFileEdit.Text := OpenDialog1.FileName;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.

