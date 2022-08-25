{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *


Author:       Arno Garrels
Description:  This simple example demonstrates how to manage multiple,
              concurrent socket connections *without* the help of multiple
              threads.

              In order to evaluate all the features of the TSmtpCli component
              you should take a look at the MailSnd demo.

              Real parallel processing is working on multi processor Windows
              systems only. Actually Windows is serializing processing by
              frequently switching between all the threads currently active
              on a single CPU. The more threads are running the more CPU time
              and resources are wasted just for thread management.

              ICS is fully event driven and it uses non-blocking sockets, it
              doesn't require mulithreading in order to build lightning fast
              socket applications with multiple concurrent connections.

Creation:     05 September 2003
Version:      1.00
EMail:        francois.piette@overbyte.be  http://www.overbyte.be
Support:      https://en.delphipraxis.net/forum/37-ics-internet-component-suite/
Legal issues: Copyright (C) 2003 by Arno Garrels

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
Apr 6, 2021 - V8.67 - Made Win64 compatible by correcting Integer(Pointer)
                         typecasts to W/LPARAM for PostMessage, thanks to Fr0sT.

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

unit OverbyteIcsMailSndAsync1;

interface

{$B-}                                 { Enable partial boolean evaluation   }
{$T-}                                 { Untyped pointers                    }
{$X+}                                 { Enable extended syntax              }
{$H+}                                 { Use long strings                    }
{$J+}                                 { Allow typed constant to be modified }
{$I Include\OverbyteIcsDefs.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, OverbyteIcsIniFiles, OverbyteIcsSmtpProt, OverbyteIcsWSocket;

  const
    MailSndAsyncVersion = 100;
    MailSndAsyncDate    = 'Sep 05, 2003';
    MailSndAsyncName    = 'MailSndAsync';
    CopyRight : String  = ' MailSndAsync (c) 2003 Arno Garrels V1.00 ';

    SectionData         = 'Data';
    KeyHost             = 'HostName';
    KeyFrom             = 'From';
    KeyTo               = 'To';
    KeySubject          = 'Subject';
    KeyUser             = 'UserName';
    KeyPwd              = 'Password';
    KeyMessage          = 'Message';
    KeyMaxCon           = 'MaximumConnections';
    KeyNumOfMails       = 'NumberOfMails';
    SectionWindow       = 'MainForm';
    KeyTop              = 'Top';
    KeyLeft             = 'Left';
    KeyWidth            = 'Width';
    KeyHeight           = 'Height';
    KeyDisplayLog       = 'DisplayLog';
    KeyDoAuth           = 'DoAuthenticate';
    WM_REMOVEOBJ        = WM_USER + 1;

type
  TMailData = record
    RcptName    : String;
    HdrTo       : String;
    HdrSubject  : String;
    MailMessage : String;
  end;
  PMailData = ^TMailData;

  TCliRec = record
    SmtpClient  : TSmtpCli;
    LogList     : TStringList;
  end;
  PCliRec = ^TCliRec;

  TForm1 = class(TForm)
    DisplayMemo     : TMemo;
    StartSendbutton : TButton;
    FillQueueButton : TButton;
    AbortButton     : TButton;
    Label1          : TLabel;
    Label2          : TLabel;
    Label3          : TLabel;
    Label4          : TLabel;
    Label5          : TLabel;
    Label6          : TLabel;
    Label7          : TLabel;
    Label8          : TLabel;
    Label9          : TLabel;
    Label10         : TLabel;
    NumOfMailsEdit  : TEdit;
    MaxConEdit      : TEdit;
    HostEdit        : TEdit;
    FromEdit        : TEdit;
    ToEdit          : TEdit;
    SubjectEdit     : TEdit;
    MessageEdit     : TEdit;
    UserEdit        : TEdit;
    PasswordEdit    : TEdit;
    CheckBoxAuth    : TCheckBox;
    CheckBoxDisplay : TCheckBox;
    ClearMemoButton : TButton;

    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure SmtpSessionConnected(Sender: TObject; ErrCode: Word);
    procedure SmtpSessionClosed(Sender: TObject; ErrCode: Word);
    procedure SmtpCommand(Sender: TObject; Msg: String);
    procedure SmtpResponse(Sender: TObject; Msg: String);
    procedure SmtpRequestDone(Sender: TObject; RqType: TSmtpRequest;
                              ErrorCode: Word);
    procedure WMRemoveObj(var Msg : TMessage); Message WM_REMOVEOBJ;
    procedure FillQueueButtonClick(Sender: TObject);
    procedure StartSendbuttonClick(Sender: TObject);
    procedure ClearMemoButtonClick(Sender: TObject);
    procedure AbortButtonClick(Sender: TObject);
    procedure WSocket1DnsLookupDone(Sender: TObject; ErrCode: Word);

  private
    FAbort         : Boolean;
    FHostIP        : String;
    FCount         : Integer;
    FInitialized   : Boolean;
    FProgDir       : String;
    FIniFileName   : String;
    MaxConnections : Integer;
    Queue          : TList;
    Pool           : TList;
    WSocket1       : TWSocket; { Used for name resulution }
    function  FindObj(SmtpClient: TSmtpCli) : Integer;
    procedure AddToPool(MailData: PMailData);
    procedure LogLine(Sender: TObject; Msg: String);
    procedure Display(const Msg : String);
    procedure RefreshDisplay;

  public
    { Public-Deklarationen }
  end;

var
  Form1: TForm1;


implementation

{$R *.DFM}
{$IFDEF COMPILER12_UP}
    uses AnsiStrings;
{$ENDIF}

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TForm1.FormCreate(Sender: TObject);
begin
    FProgDir := ExtractFilePath(ParamStr(0));
    if FProgDir[Length(FProgDir)] <> '\' then
        FProgDir := FProgDir + '\';

    FIniFileName             := GetIcsIniFileName;
    FInitialized             := False;
    FCount                   := 0;
    Queue                    := TList.Create;
    Pool                     := TList.Create;
    WSocket1                 := TWsocket.Create(nil);
    WSocket1.OnDnsLookupDone := WSocket1DnsLookupDone;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TForm1.FormDestroy(Sender: TObject);
var
   PCli       : PCliRec;
   I          : Integer;
begin
   { Just to be sure }
   for I := 0 to Pool.Count -1 do begin
       PCli := Pool.Items[I];
       PCli.LogList.Free;
       PCli.SmtpClient.Free;
       Dispose(PCli);
   end;
   Pool.Free;

   { Just to be sure }
   for I := 0 to Queue.Count -1 do begin
       Dispose(PMailData(Queue.Items[0]));
       Queue.Delete(0);
   end;
   Queue.Free;

   WSocket1.Free;
   WSocket1 := nil;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TForm1.FormShow(Sender: TObject);
var
    IniFile: TIcsIniFile;
begin
    if not FInitialized then begin
        FInitialized := TRUE;
        IniFile := TIcsIniFile.Create(FIniFileName);
        try
            HostEdit.Text       := IniFile.ReadString(SectionData, KeyHost,
                                                  'localhost');
            FromEdit.Text       := IniFile.ReadString(SectionData, KeyFrom,
                                                  'first.last@company.com');
            ToEdit.Text         := IniFile.ReadString(SectionData, KeyTo,
                                                  'john.doe@acme');
            SubjectEdit.Text    := IniFile.ReadString(SectionData, KeySubject,
                                                  'This is the message subject');
            UserEdit.Text       :=  IniFile.ReadString(SectionData, KeyUser,
                                                  'account name');
            PasswordEdit.Text   :=  IniFile.ReadString(SectionData, KeyPwd, 'password');
            MessageEdit.Text    :=  IniFile.ReadString(SectionData, KeyMessage,
                                                  'This is the message text, here ' +
                                                  'just a simple string');
            NumOfMailsEdit.Text := IniFile.ReadString(SectionData, KeyNumOfMails, '10');
            MaxConEdit.Text     := IniFile.ReadString(SectionData, KeyMaxCon, '2');
            CheckBoxDisplay.Checked := IniFile.ReadBool(SectionData, KeyDisplayLog, TRUE);
            CheckBoxAuth.Checked    := IniFile.ReadBool(SectionData, KeyDoAuth, FALSE);

            Top    := IniFile.ReadInteger(SectionWindow, KeyTop,    (Screen.Height - Height) div 2);
            Left   := IniFile.ReadInteger(SectionWindow, KeyLeft,   (Screen.Width - Width) div 2);
            Width  := IniFile.ReadInteger(SectionWindow, KeyWidth,  Width);
            Height := IniFile.ReadInteger(SectionWindow, KeyHeight, Height);
        finally
            IniFile.Free;
        end;
        Label10.Caption   := IntToStr(Queue.Count) + ' Jobs in Queue';
        Caption := 'ICS Parallel SMTP Demo';
        DisplayMemo.Clear;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
var
    IniFile: TIcsIniFile;
begin
    IniFile := TIcsIniFile.Create(FIniFileName);
    try
        IniFile.WriteString(SectionData, KeyHost,             HostEdit.Text);
        IniFile.WriteString(SectionData, KeyFrom,             FromEdit.Text);
        IniFile.WriteString(SectionData, KeyTo,               ToEdit.Text);
        IniFile.WriteString(SectionData, KeySubject,          SubjectEdit.Text);
        IniFile.WriteString(SectionData, KeyUser,             UserEdit.Text);
        IniFile.WriteString(SectionData, KeyPwd,              PasswordEdit.Text);
        IniFile.WriteString(SectionData, KeyMessage,          MessageEdit.Text);
        IniFile.WriteString(SectionData, KeyNumOfMails,       NumOfMailsEdit.Text);
        IniFile.WriteBool(SectionData,   KeyDoAuth,           CheckBoxAuth.Checked);
        IniFile.WriteBool(SectionData,   KeyDisplayLog,       CheckBoxDisplay.Checked);

        IniFile.WriteString(SectionData, KeyMaxCon,           MaxConEdit.Text);

        IniFile.WriteInteger(SectionWindow, KeyTop,           Top);
        IniFile.WriteInteger(SectionWindow, KeyLeft,          Left);
        IniFile.WriteInteger(SectionWindow, KeyWidth,         Width);
        IniFile.WriteInteger(SectionWindow, KeyHeight,        Height);
        IniFile.UpdateFile;
    finally
        IniFile.Free;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TForm1.FillQueueButtonClick(Sender: TObject);
var
    I : Integer;
    P : PMailData;
begin
    { Add some mail data to the queue }
    for I := 1 to StrToInt(Trim(NumOfMailsEdit.Text)) do begin
        New(P);
        P.RcptName         := Trim(ToEdit.Text);
        P.HdrTo            := Trim(ToEdit.Text);
        P.HdrSubject       := Trim(SubjectEdit.Text);
        P.MailMessage      := Trim(MessageEdit.Text);
        Queue.Add(P);
    end;
    StartSendButton.Enabled := (Queue.Count > 0) and (Pool.Count = 0);
    Label10.Caption := IntToStr(Queue.Count) + ' Jobs in Queue';
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TForm1.StartSendbuttonClick(Sender: TObject);
begin
    FAbort := FALSE;
    StartSendButton.Enabled := (Queue.Count > 0) and (Pool.Count = 0);
    if Queue.Count < 1 then
        raise Exception.Create('Queue is empty!');
    MaxConnections := StrToInt(Trim(MaxConEdit.Text));

    { Usually an internal DNS lookup is done by the TSmtpCli component.      }
    { But the OS serializes name resolution, this compromize the parallel    }
    { operation. Anyway, a single name resolution is enough for all requests }
    { to the same server. We use TWSocket for the DNS Lookup here.           }
    if not WSocketIsDottedIP(Trim(AnsiString(HostEdit.Text))) then begin
        Display('Resolving host name...');
        WSocket1.DnsLookup(Trim(HostEdit.Text));
    end
    else begin
        FHostIP := Trim(HostEdit.Text);
        { Feed the 'pool' }
        while (Pool.Count < MaxConnections) and (Queue.Count > 0) and (not FAbort) do
            AddToPool(PMailData(Queue.Items[0]));
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TForm1.ClearMemoButtonClick(Sender: TObject);
begin
    DisplayMemo.Clear;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TForm1.AbortButtonClick(Sender: TObject);
var
   I: Integer;
begin
   FAbort := TRUE;
   for I := 0 to Pool.Count -1 do
       PCliRec(Pool.Items[I]).SmtpClient.Abort;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TForm1.WSocket1DnsLookupDone(Sender: TObject; ErrCode: Word);
begin
    if ErrCode <> 0 then
        Display('DNS lookup failed. Error #'+IntToStr(ErrCode) + ' - '
               + WSocketErrorDesc(ErrCode))
    else begin
        FHostIP := WSocket1.DnsResult;
        Display('Host name resolved to: ' + FHostIP);

        { Feed the 'pool' }
        while (Pool.Count < MaxConnections) and (Queue.Count > 0) and (not FAbort) do
            AddToPool(PMailData(Queue.Items[0]));
    end;
end;



{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TForm1.AddToPool(MailData: PMailData);
var
    PCli  : PCliRec;
    I, J : Integer;
begin
    if not Assigned(MailData) then
        Exit;
    New(PCli);
    PCli.SmtpClient                    := TSmtpCli.Create(nil);
    PCli.SmtpClient.OnRequestDone      := SmtpRequestDone;
    PCli.SmtpClient.OnSessionClosed    := SmtpSessionClosed;
    PCli.SmtpClient.OnSessionConnected := SmtpSessionConnected;
    PCli.SmtpClient.OnResponse         := SmtpResponse;
    PCli.SmtpClient.OnCommand          := SmtpCommand;
    PCli.SmtpClient.Host               := FHostIP;
    PCli.SmtpClient.FromName           := Trim(FromEdit.Text);
    PCli.SmtpClient.HdrFrom            := Trim(FromEdit.Text);
    PCli.SmtpClient.Username           := Trim(UserEdit.Text);
    PCli.SmtpClient.Password           := Trim(PasswordEdit.Text);
    PCli.SmtpClient.RcptName.Add(MailData^.RcptName);
    PCli.SmtpClient.HdrTo              := MailData^.HdrTo;
    PCli.SmtpClient.HdrSubject         := MailData^.HdrSubject;
    PCli.SmtpClient.MailMessage.Text   := MailData^.MailMessage;
    if CheckBoxAuth.Checked then
        PCli.SmtpClient.AuthType := smtpAuthAutoSelect;

    PCli.SmtpClient.Allow8bitChars  := FALSE;
    PCli.SmtpClient.DefaultEncoding := smtpEnc7bit;
    PCli.SmtpClient.WrapMessageText := TRUE;
    PCli.SmtpClient.FoldHeaders     := TRUE;
    try
        PCli.SmtpClient.CharSet := 'utf-8';
    except
        // nothing to do since the default system charset was set
    end;
    { This line is ignored in Unicode compilers }
    PCli.SmtpClient.ConvertToCharset := PCli.SmtpClient.CharSet = 'utf-8';

    PCli.LogList := TStringList.Create;
    I := Pool.Add(PCli);
    J := Queue.IndexOf(MailData);
    Dispose(PCliRec(Queue.Items[J]));
    Queue.Delete(J);

    if FCount = MaxInt then
        FCount := 0;
    Inc(FCount);

    Label10.Caption := IntToStr(Queue.Count) + ' Jobs in Queue';
    LogLine(PCliRec(Pool.Items[I]).SmtpClient, '');
    LogLine(PCliRec(Pool.Items[I]).SmtpClient, '+++++++++++++++++++++++++++++++++++++++++++++++++++');
    LogLine(PCliRec(Pool.Items[I]).SmtpClient, '                      '+   IntToStr(FCount));
    LogLine(PCliRec(Pool.Items[I]).SmtpClient, '+++++++++++++++++++++++++++++++++++++++++++++++++++');

    PCliRec(Pool.Items[I]).SmtpClient.Open; //----->
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TForm1.SmtpRequestDone(Sender: TObject; RqType: TSmtpRequest;
    ErrorCode: Word);
var
    ErrMsg     : String;
    SmtpClient : TSmtpCli;
begin
    { All requests from all instances of TSmtpCli return here. }
    { Sender is a pointer to current TSmtpCli instance.        }
    SmtpClient := (Sender as TSmtpCli);
    if ErrorCode = 0 then begin
        case RqType of
            smtpOpen : SmtpClient.Mail;
            smtpMail : SmtpClient.Quit;
            { We cannot free an instance of TSmtpCli in one of its event handlers. }
            { So we post a custom message, the object will be freed delayed in     }
            { proc WMRemoveObj.                                                    }
            smtpQuit : PostMessage(Form1.Handle, WM_REMOVEOBJ, WPARAM(Sender), 0);       { V8.67 was Integer }
        else
            { Should not happen }
            SmtpClient.Abort;
            LogLine(Sender, 'Unknown request type, session aborted');
            PostMessage(Form1.Handle, WM_REMOVEOBJ, WPARAM(Sender), 0);                 { V8.67 was Integer }
        end;
    end
    else begin
        if ErrorCode < 10000 then
            ErrMsg := SmtpClient.ErrorMessage
        else
            ErrMsg := 'RqType:' + IntToStr(Ord(RqType)) + ' ErrorCode:'
                     + IntToStr(ErrorCode);
        if ErrMsg = '' then
            ErrMsg := 'Unknown error';
        LogLine(Sender, ErrMsg);
        SmtpClient.Abort;
        PostMessage(Form1.Handle, WM_REMOVEOBJ, WPARAM(Sender), 0);                { V8.67 was Integer }
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TForm1.WMRemoveObj(var Msg: TMessage);
var
    I          : Integer;
    SmtpClient : TSmtpCli;
    P          : PCliRec;
begin
    { Reusing the object would be more effective. }
    SmtpClient := TSmtpCli(Msg.WParam);
    I := FindObj(SmtpClient);
    if I > -1 then begin
        P := Pool.Items[I];
        if CheckBoxDisplay.Checked then begin
            DisplayMemo.Lines.AddStrings(P.LogList);
            RefreshDisplay;
        end;
        { clean up }
        P.LogList.Free;
        P.SmtpClient.Free;
        Dispose(P);
        Pool.Delete(I);
    end;

    if (Pool.Count < MaxConnections) and (Queue.Count > 0) and (not FAbort)then
        AddToPool(PMailData(Queue.Items[0]));

    StartSendButton.Enabled := (Queue.Count > 0) and (Pool.Count = 0);
    if StartSendButton.Enabled then begin
        Label10.Caption := IntToStr(Queue.Count) + ' Jobs in Queue';
        Beep;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TForm1.SmtpSessionConnected(Sender: TObject; ErrCode: Word);
begin
    LogLine(Sender, '>SMTP session connected, error #' +IntToStr(ErrCode));
end;



{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TForm1.SmtpSessionClosed(Sender: TObject; ErrCode: Word);
begin
    LogLine(Sender, '>SMTP session closed, error #' +IntToStr(ErrCode));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TForm1.SmtpCommand(Sender: TObject; Msg: String);
begin
    LogLine(Sender, '>' + Msg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TForm1.SmtpResponse(Sender: TObject; Msg: String);
begin
    LogLine(Sender, '<' + Msg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TForm1.FindObj(SmtpClient: TSmtpCli) : Integer;
var
    I: Integer;
begin
    for I := 0 to Pool.Count - 1 do begin
        if SmtpClient = PCliRec(Pool.Items[I]).SmtpClient then begin
            Result := I;
            Exit;
        end;
    end;
    Result := -1;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TForm1.LogLine(Sender: TObject; Msg: String);
var
    I : Integer;
    P : PCliRec;
begin
    if not CheckBoxDisplay.Checked then
        Exit;
    I := FindObj(Sender as TSmtpCli);
    if I > -1 then begin
        P  := Pool.Items[I];
        P.LogList.Add('ID #' + IntToStr(Integer(Sender as TSmtpCli)) + ' ' + Msg);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TForm1.Display(const Msg : String);
begin
    if not CheckBoxDisplay.Checked then
        Exit;
    DisplayMemo.Lines.BeginUpdate;
    try
        if DisplayMemo.Lines.Count > 200 then begin
            { We preserve only 200 lines }
            while DisplayMemo.Lines.Count > 200 do
                DisplayMemo.Lines.Delete(0);
        end;
        DisplayMemo.Lines.Add(Msg);
    finally
        DisplayMemo.Lines.EndUpdate;
        { Makes last line visible }
        SendMessage(DisplayMemo.Handle, EM_SCROLLCARET, 0, 0);
    end;
end;



{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TForm1.RefreshDisplay;
begin
    DisplayMemo.Lines.BeginUpdate;
    try
        if DisplayMemo.Lines.Count > 200 then begin
            { We preserve only 200 lines }
            while DisplayMemo.Lines.Count > 200 do
                DisplayMemo.Lines.Delete(0);
        end;
    finally
        DisplayMemo.Lines.EndUpdate;
        { Makes last line visible }
        SendMessage(DisplayMemo.Handle, EM_SCROLLCARET, 0, 0);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}



end.
