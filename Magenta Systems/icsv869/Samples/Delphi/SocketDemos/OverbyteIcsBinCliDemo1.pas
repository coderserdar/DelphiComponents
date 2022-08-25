{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       Arno Garrels <arno.garrels@gmx.de>
Description:  Client program demonstrating receive of binary and delimited
              text data.
              Works with OverbyteIcsTcpSrv and OverbyteIcsTcpSrvIPv6 demos.
Creation:     28 November 2008
Version:      6.00
EMail:        francois.piette@overbyte.be  http://www.overbyte.be
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 2008 by François PIETTE
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

Updates:


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsBinCliDemo1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, OverbyteIcsIniFiles, ExtCtrls,
  { Don't forget to add your vc32 directory to Delphi library path }
  OverbyteIcsWSocket;

const
  BinCliDemoVersion  = 600;
  CopyRight : String = ' CliDemo (c) 2008 F. Piette V6.00 ';

type

  PHdrRec = ^THdrRec;
  THdrRec = record
    case Integer of
      0: (
        ID1     : Byte;
        ID2     : Byte;
        ID3     : Byte;
        ID4     : Byte;
        SizeLo  : Word;
        SizeHi  : Word);
      1: (
        ID    : Longint;
        Size  : Longint);
  end;

  TBinStatus   = (stBinReady, stBinHdrReceived);
  TReceiveMode = (rmLine, rmBinary);
  TBinaryReceivedEvent = procedure(Sender: TObject; const Buf; Size: Integer) of object;
  TBinaryBeginEvent = procedure(Sender: TObject; const Header: THdrRec) of object;
  TLineReceivedEvent = procedure(Sender: TObject; const Line: String) of object;
  { We derive our own class to handle line data delimited by CRLF and binary }
  { data. The server will send binary data prefixed by a custom header see   }
  { THdrRec above. }
  TBinaryWSocket = class(TWSocket)
  private
    FBinStatus        : TBinStatus;
    FDataLen          : Integer;
    FID               : Byte;
    FReadOffs         : Integer;
    FCurrentBlockSize : Integer;
    FReceiveMode      : TReceiveMode;
    FRcvBuf           : array [0..4095] of Byte;
    FWriteOffs        : Integer;
    FRemainingBytes   : Integer;
    FOnBinaryBegin    : TBinaryBeginEvent;
    FOnBinaryEnd      : TNotifyEvent;
    FOnBinaryReceived : TBinaryReceivedEvent;
    FOnLineReceived   : TLineReceivedEvent;
    FTextLineIDs      : TSysCharSet;
    procedure ParseReceiveBuffer;
    procedure TriggerBinaryReceived(const Buf; Size: Integer);
    procedure TriggerBinaryBegin(PHeader: PHdrRec);
    procedure TriggerBinaryEnd;
    procedure TriggerLineReceived(Buf: PAnsiChar);
  protected
    procedure AssignDefaultValue; override;
    function  TriggerDataAvailable(ErrCode : Word) : Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    property CurrentBlockSize: Integer read FCurrentBlockSize;
    property ReceiveMode  : TReceiveMode read FReceiveMode write FReceiveMode;
    property RemainingBytes : Integer read FRemainingBytes; // bytes not yet processed 
    property TextLineIDs: TSysCharSet read FTextLineIDs write FTextLineIDs;
  published
    property OnBinaryBegin : TBinaryBeginEvent read FOnBinaryBegin write FOnBinaryBegin;
    property OnBinaryEnd : TNotifyEvent read FOnBinaryEnd write FOnBinaryEnd;
    property OnBinaryReceived : TBinaryReceivedEvent read FOnBinaryReceived write FOnBinaryReceived;
    property OnLineReceived : TLineReceivedEvent read FOnLineReceived write FOnLineReceived;
  end;

  TBinClientForm = class(TForm)
    DisplayMemo: TMemo;
    Panel1: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    SendEdit: TEdit;
    SendButton: TButton;
    DisconnectButton: TButton;
    PortEdit: TEdit;
    ServerEdit: TEdit;
    Label3: TLabel;
    AddCRLFCheckBox: TCheckBox;
    procedure DisconnectButtonClick(Sender: TObject);
    procedure SendButtonClick(Sender: TObject);
    procedure CliSocketSessionConnected(Sender: TObject; ErrCode: Word);
    procedure CliSocketSessionClosed(Sender: TObject; ErrCode: Word);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    CliSocket   : TBinaryWSocket;
    Initialized : Boolean;
    IniFileName : String;
    procedure CliSocketBinaryReceived(Sender: TObject; const Buf; Size: Integer);
    procedure CliSocketBinaryBegin(Sender: TObject; const Header: THdrRec);
    procedure CliSocketBinaryEnd(Sender: TObject);
    procedure CliSocketLineReceived(Sender: TObject; const Line: String);
    procedure Display(Msg : String);
    procedure SendData;
    procedure ProcessCommand(Sender: TObject; const Str: String);
  end;

var
  BinClientForm: TBinClientForm;
  
implementation

{$R *.DFM}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function FindCrLf(Buf: PAnsiChar; Count: Integer): Integer;
begin
    for Result := 0 to Count - 1 do
    begin
        if (Buf[Result] = #13) and (Result + 1 < Count) and
           (Buf[Result + 1] = #10) then
            Exit;
    end;
    Result := -1;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TBinaryWSocket.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);
    FTextLineIDs := [#32..#127];
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TBinaryWSocket.TriggerBinaryBegin(PHeader: PHdrRec);
begin
    FCurrentBlockSize := 0;
    if Assigned(FOnBinaryBegin) then
        FOnBinaryBegin(Self, PHeader^);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TBinaryWSocket.TriggerBinaryEnd;
begin
    if Assigned(FOnBinaryEnd) then
        FOnBinaryEnd(Self);
    FCurrentBlockSize := 0;    
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TBinaryWSocket.TriggerBinaryReceived(const Buf; Size: Integer);
begin
    if Assigned(FOnBinaryReceived) then
        FOnBinaryReceived(Self, Buf, Size);
    Inc(FCurrentBlockSize, Size);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TBinaryWSocket.TriggerLineReceived(Buf: PAnsiChar);
begin
    if Assigned(FOnLineReceived) then
        FOnLineReceived(Self, String(Buf));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TBinaryWSocket.ParseReceiveBuffer;
var
    I : Integer;
begin
    if FReceiveMode = rmLine then
    begin
        while (FRemainingBytes > 0) and (FReceiveMode = rmLine) do
        begin
            I := FindCrLf(@FRcvBuf[FReadOffs], FRemainingBytes);
            if (I < 0) then
            begin
                if FRemainingBytes >= SizeOf(FRcvBuf) then
                begin
                    FRemainingBytes := 0; FReadOffs := 0; FWriteOffs := 0;
                    Abort;
                    TriggerLineReceived(PAnsiChar('! Buffer overflow attempt detected'));
                end
                else if FReadOffs <> 0 then
                begin
                    Move(FRcvBuf[FReadOffs],
                         FRcvBuf[0], FRemainingBytes);
                    FWriteOffs := FRemainingBytes;
                    FReadOffs  := 0;
                end;
                Exit;
            end;
            FRcvBuf[FWriteOffs - FRemainingBytes + I] := 0;
            TriggerLineReceived(PAnsiChar(@FRcvBuf[FReadOffs]));
            Dec(FRemainingBytes, I + 2);
            if FRemainingBytes <= 0 then
            begin
                FRemainingBytes := 0; FWriteOffs := 0; FReadOffs := 0;
                Exit;
            end
            else
                FReadOffs := FWriteOffs - FRemainingBytes;
        end;
    end;
    
    if FReceiveMode <> rmBinary then
        Exit;

    case FBinStatus of
        stBinReady :
            begin
                FID := PHdrRec(@FRcvBuf[FReadOffs])^.ID1;
                if (AnsiChar(FID) in FTextLineIDs) then
                { None of our IDs, assume text lines? }
                begin
                    FReceiveMode := rmLine;
                    ParseReceiveBuffer;
                end
                else if FRemainingBytes >= SizeOf(THdrRec) then
                begin
                    FDataLen := PHdrRec(@FRcvBuf[FReadOffs])^.Size;
                    TriggerBinaryBegin(PHdrRec(@FRcvBuf[FReadOffs]));
                    if FDatalen <= 0 then
                        TriggerBinaryEnd
                    else
                        FBinStatus := stBinHdrReceived;    
                    Inc(FReadOffs, SizeOf(THdrRec));
                    Dec(FRemainingBytes, SizeOf(THdrRec));
                    if FRemainingBytes > 0 then
                        ParseReceiveBuffer
                    else
                        FWriteOffs := 0;
                end;
            end;
        stBinHdrReceived :
            if FRemainingBytes >= FDataLen then
            begin
                TriggerBinaryReceived(FRcvBuf[FReadOffs], FDataLen);
                Inc(FReadOffs, FDataLen);
                Dec(FRemainingBytes, FDataLen);
                FDataLen := 0;
                FBinStatus := stBinReady;
                TriggerBinaryEnd;
                if FRemainingBytes > 0 then
                    ParseReceiveBuffer
                else
                    FReadOffs := 0; FWriteOffs := 0;
            end
            else begin
                if FRemainingBytes > 0 then
                begin
                    TriggerBinaryReceived(FRcvBuf[FReadOffs], FRemainingBytes);
                    Dec(FDataLen, FRemainingBytes);
                end;    
                FReadOffs := 0; FWriteOffs := 0; FRemainingBytes := 0;
            end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TBinaryWSocket.TriggerDataAvailable(ErrCode: Word): Boolean;
var
    Rcvd: Integer;
begin
    Result := TRUE;
    Rcvd := Receive(@FRcvBuf[FWriteOffs], (SizeOf(FRcvBuf) - FWriteOffs));
    if Rcvd > 0 then
    begin
        Inc(FRemainingBytes, Rcvd);
        Inc(FWriteOffs, Rcvd);
    end;
    if FRemainingBytes > 0 then
        ParseReceiveBuffer;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TBinaryWSocket.AssignDefaultValue;
begin
    FReadOffs       := 0;
    FWriteOffs      := 0;
    FRemainingBytes := 0;
    FDataLen        := 0;
    FBinStatus      := stBinReady;
    FReceiveMode    := rmLine;
    inherited;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{ TBinClientForm }

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TBinClientForm.ProcessCommand(
    Sender    : TObject;
    const Str : String);
var
    Cli : TBinaryWSocket;
begin
    Cli := TBinaryWSocket(Sender);
    if Pos('200 OK Binary', Str) = 1 then
        Cli.ReceiveMode := rmBinary;
    Display('< ' + Str);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TBinClientForm.SendData;
var
    Buf : String;
begin
    try
        Buf := SendEdit.Text;
        if AddCRLFCheckBox.Checked then
            Buf := Buf + #13#10;
        CliSocket.SendStr(Buf);
    except
        on E:Exception do Display(E.ClassName + ': ' + E.Message);
    end;
    ActiveControl := SendEdit;
    SendEdit.SelectAll;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TBinClientForm.CliSocketSessionConnected(
    Sender  : TObject;
    ErrCode : Word);
begin
    SendButton.Enabled := TRUE;
    if ErrCode <> 0 then
        Display('Can''t connect, error #' + IntToStr(ErrCode))
    else begin
        DisconnectButton.Enabled := TRUE;
        SendData;  { Send the data from edit box }
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TBinClientForm.CliSocketSessionClosed(
    Sender  : TObject;
    ErrCode : Word);
begin
    DisconnectButton.Enabled := FALSE;
    if ErrCode <> 0 then
        Display('Disconnected, error #' + IntToStr(ErrCode))
    else
        Display('Disconnected');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TBinClientForm.CliSocketBinaryBegin(
    Sender        : TObject;
    const Header  : THdrRec);
begin
    Display('! Binary Begin - ID=' + IntToStr(Header.ID) +
            ' size='+ IntToStr(Header.Size));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TBinClientForm.CliSocketBinaryReceived(
    Sender  : TObject;
    const Buf;
    Size: Integer);
var
    S : AnsiString;
begin
    SetLength(S, Size);
    Move(Buf, S[1], Size);
    Display(String(S));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TBinClientForm.CliSocketBinaryEnd(Sender: TObject);
begin
    Display('! Binary End - total size=' +
            IntToStr(TBinaryWSocket(Sender).CurrentBlockSize));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TBinClientForm.CliSocketLineReceived(Sender: TObject; const Line: String);
begin
    ProcessCommand(Sender, Line);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TBinClientForm.FormCreate(Sender: TObject);
begin
    IniFileName                   := GetIcsIniFileName;
    CliSocket                     := TBinaryWSocket.Create(Self);
    CliSocket.ComponentOptions    := [wsoNoReceiveLoop];
    CliSocket.LineMode            := FALSE;  // We use our own line mode!
    CliSocket.ReceiveMode         := rmLine; // Set initial receive mode to rmLine
    CliSocket.OnSessionConnected  := CliSocketSessionConnected;
    CliSocket.OnSessionClosed     := CliSocketSessionClosed;
    CliSocket.OnBinaryBegin       := CliSocketBinaryBegin;
    CliSocket.OnBinaryEnd         := CliSocketBinaryEnd;
    CliSocket.OnBinaryReceived    := CliSocketBinaryReceived;
    CliSocket.OnLineReceived      := CliSocketLineReceived;

end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TBinClientForm.FormClose(Sender: TObject; var Action: TCloseAction);
var
    IniFile : TIcsIniFile;
begin
    IniFile := TIcsIniFile.Create(IniFileName);
    IniFile.WriteInteger('Window', 'Top',    Top);
    IniFile.WriteInteger('Window', 'Left',   Left);
    IniFile.WriteInteger('Window', 'Width',  Width);
    IniFile.WriteInteger('Window', 'Height', Height);
    IniFile.WriteString('Data', 'Server',  ServerEdit.Text);
    IniFile.WriteString('Data', 'Port',    PortEdit.Text);
    IniFile.WriteString('Data', 'Command', SendEdit.Text);
    IniFile.UpdateFile;
    IniFile.Free;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TBinClientForm.FormShow(Sender: TObject);
var
    IniFile : TIcsIniFile;
begin
    if Initialized then
        Exit;
    Initialized := TRUE;
    IniFile         := TIcsIniFile.Create(IniFileName);
    
    Top             := IniFile.ReadInteger('Window', 'Top',    Top);
    Left            := IniFile.ReadInteger('Window', 'Left',   Left);
    Width           := IniFile.ReadInteger('Window', 'Width',  Width);
    Height          := IniFile.ReadInteger('Window', 'Height', Height);

    PortEdit.Text   := IniFile.ReadString('Data', 'Port',    'telnet');
    ServerEdit.Text := IniFile.ReadString('Data', 'Server',  'localhost');
    SendEdit.Text   := IniFile.ReadString('Data', 'Command', 'binary 128');

    IniFile.Free;

    DisplayMemo.Clear;
    ActiveControl := SendEdit;
    SendEdit.SelectAll;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Display a message in our display memo. Delete lines to be sure to not     }
{ overflow the memo which may have a limited capacity.                      }
procedure TBinClientForm.Display(Msg : String);
var
    I : Integer;
begin
    DisplayMemo.Lines.BeginUpdate;
    try
        if DisplayMemo.Lines.Count > 200 then begin
            for I := 1 to 50 do
                DisplayMemo.Lines.Delete(0);
        end;
        DisplayMemo.Lines.Add(Msg);
    finally
        DisplayMemo.Lines.EndUpdate;
        SendMessage(DisplayMemo.Handle, EM_SCROLLCARET, 0, 0);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TBinClientForm.Timer1Timer(Sender: TObject);
begin
    if CliSocket.State = wsConnecting then
        Exit;

    if CliSocket.State <> wsConnected then
        SendButtonClick(nil)
    else
        DisconnectButtonClick(nil);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TBinClientForm.DisconnectButtonClick(Sender: TObject);
begin
    CliSocket.Close;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TBinClientForm.SendButtonClick(Sender: TObject);
begin
    if CliSocket.State = wsConnected then begin
        CliSocket.ReceiveMode := rmLine;
        { Already connected, just send data }
        SendData;
    end
    else begin
        { Not connected yet, start connection }
        CliSocket.Proto       := 'tcp';
        CliSocket.Port        := PortEdit.Text;
        CliSocket.Addr        := ServerEdit.Text;
        //CliSocket.LineMode    := FALSE;
        CliSocket.Connect;
        { Connect is asynchronous (non-blocking). When the session is  }
        { connected (or fails to), we have an OnSessionConnected event }
        { This is where actual sending of data is done.                }
        SendButton.Enabled := FALSE;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.

