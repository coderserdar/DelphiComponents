unit ICQDirect2;
{************************************************
    For updates checkout: http://www.cobans.net
      (C) Alex Demchenko(alex@ritlabs.com)
          Gene Reeves(notgiven2k@lycos.com)
*************************************************}

{ Created on 01-2003}

{$HINTS OFF}

interface
//****************************************************************************//
// TicqDCM - Direct Connection Manager                                        //
//           Supplies interface to ICQClient for managing DCs.                //
//                                                                            //
// TicqBaseDC - Base class for direct connection                              //
//            Handles all common tasks in a DC                                //
//                                                                            //
// TicqDCxxxx - Direct Connection Handler classes                             //
//   TicqDCNormal   - Main DC class used for Msg, File Req, Chat Req, etc.    //
//   TicqDCRecvFile - DC class used to receive files.                         //
//   TicqDCSendFile - DC class used to send files                             //
//   TicqDCChat     - DC class used for chat sessions.                        //
//                                                                            //
//****************************************************************************//
// To Add New DC Type:                                                        //
//                                                                            //
//   1. Create new subclass of TicqBaseDC.                                    //
//   2. Implement Start, Stop and HandlePacket routines in new subclass.      //
//   3. Add interface routines to TicqDCM for needed functionality.           //
//                                                                            //
//****************************************************************************//


// {ToDo}
// Need to add proxy support back into DC's
//     must be done in ICQSock.pas.
// Need to finish TicqDCChat class


Uses
  Classes, Sysutils, ICQWorks, ICQSock, WinSock, Windows;

Type
  PicqDirectUser = ^TicqDirectUser;
  TicqDirectUser = record
    UIN, Cookie,
    ExtIP, IntIP: LongWord;
    Port:Word;
    IsConnected:Boolean;
    LastActivity: LongWord; // Last tickcount of activity
    DCMain,
    DCRecvFile,
    DCSendFile,
    DCChat:Integer
  End;
  TDirectUser = TicqDirectUser;
  TicqBaseDC = class;
  TicqDCnormal = Class;


  //TicqEventType = (ET_NORMAL, ET_FILE_RECV, ET_FILE_SEND, ET_CHAT);

  TicqDCEvent = Procedure(Sender: TicqBaseDC) of Object;
  TOnHandle = procedure(Sender: TObject; UIN: LongWord; Pak: PRawPkt; Len: LongWord) of object;
  TOnParseDirectPkt = procedure(Sender: TObject; Buffer: Pointer; BufLen: LongWord; Incoming: Boolean; UIN:Cardinal) of object;

  //Direct Connection Manager
  TicqDCM = Class(TObject)
    Private
      fSrv:TSrvSocket;
      fTmpDC:TicqDCNormal;
      fPort:Word;
      fProxyType: TProxyType;
      fProxyHost: String;
      fProxyPort: Word;
      fProxyAuth: Boolean;
      fProxyPass: String;
      fUserID: String;
      fResolve: Boolean;
      fOnError: TOnError;
      fOnPktDump: TOnParseDirectPkt;
      fOnHandle:TOnHandle;
      fOnFTInit: TOnFTInit;
      fOnFTStart: TOnFTStart;
      fOnFTFileData: TOnFTFileData;
      fOnSendFileStart:TOnSendFileStart;
      fOnSendFileData:TOnSendFileData;
      fOnSendFileFinish:TOnSendFileFinish;
      fpUser:PicqDirectUser;
      fDestroying:Boolean;
      fTmrIdle:TThreadTimer;

      procedure InternalOnErrorProc(Sender: TObject; ErrorType: TErrorType; ErrorMsg: String); virtual;
      procedure InternalOnPktDump(Sender: TObject; Buffer: Pointer; BufLen: LongWord; Incoming: Boolean; UIN: Cardinal);
      procedure InternalOnHandle(Sender: TObject; UIN: LongWord; Pak: PRawPkt; Len: LongWord);
      procedure OnIdleTimeOut(Sender: TObject);
      procedure OnSrvSockConnect(Sender: TObject; Socket: TMySocket);
    Public
      MyUIN:LongWord;
      fDCL:TList;   // List of DC's
      fUL:TList;    // List of User DC Info.
      constructor Create(aMyUIN: LongWord);
      destructor Destroy; override;

      Function ExtIP:LongWord;
      Function IntIP:LongWord;

      procedure AddUser(aUIN, aCookie, aIPExt, aIPInt: LongWord; aPort: Word);
      Function GetUserIndex(aUIN:LongWord; Var Idx:integer):Boolean;
      Procedure HandleDCEvent(Sender: TicqBaseDC);
      Function SendData(aUIN:LongWord; pPkt: PRawPkt):Boolean;
      function SendDataFile(aUIN: LongWord; Pak: PRawPkt): Boolean;
      Procedure DeleteUser(aUIN: LongWord);
      Procedure DeleteDC(Var aIndex:Integer);
      function AddFileUser(aUIN: LongWord; var aPort: Word; FTReqRec:Pointer = nil): Boolean;
      Procedure SetFileRecord(aUIN: LongWord; aFileRec:TSendFileRec);
      Function AddSendFileUser(aUIN:LongWord; Var aPort, aSeq:Word):Boolean;
      function StopFileReceiving(aUIN: LongWord): Boolean;
      Procedure StopFileSending(aUIN: LongWOrd);
      procedure EstabilishConnection(aUIN: LongWord);
      function ConnectionEstabilished(aUIN: LongWord): Boolean;

      property BindPort: Word read FPort;
      property ProxyType: TProxyType read FProxyType write FProxyType;
      property ProxyHost: String read FProxyHost write FProxyHost;
      property ProxyPort: Word read FProxyPort write FProxyPort;
      property ProxyUserID: String read FUserID write FUserID;
      property ProxyAuth: Boolean read FProxyAuth write FProxyAuth;
      property ProxyPass: String read FProxyPass write FProxyPass;
      property UseProxyResolve: Boolean read FResolve write FResolve default False;
      Property pUser:PicqDirectUser read fpUser;

      property OnPktDump: TOnParseDirectPkt read FOnPktDump write FOnPktDump;
      property OnHandle: TOnHandle read FOnHandle write FOnHandle;
      property OnError: TOnError read FOnError write FOnError;

      property OnFTInit: TOnFTInit read FOnFTInit write FOnFTInit;
      property OnFTStart: TOnFTStart read FOnFTStart write FOnFTStart;
      property OnFTFileData: TOnFTFileData read FOnFTFileData write FOnFTFileData;
      Property OnSendFileStart:TonSendFileStart read FOnSendFileStart write FOnSendFileStart;
      Property OnSendFileData:TOnSendFileData read fOnSendFileData write fOnSendFileData;
      Property OnSendFileFinish:TOnSendFileFinish read fOnSendFileFinish write fOnSendFileFinish;
  End;
  TDirectControl = Class(TicqDCM);
  TDirectClient = TDirectControl;
  //Base Direct Connection Type
  TicqBaseDC = Class(TObject)
    Protected
      fManager:TicqDCM;
      fOnDCEvent:TicqDCEvent;
      fRemUIN:LongWord;
      FPort:Word;
      fIncoming:Boolean;
      FProxyType: TProxyType;
      FProxyHost: String;
      FProxyPort: Word;
      FProxyAuth: Boolean;
      FProxyPass: String;
      FUserID: String;
      FResolve: Boolean;
      FOnError: TOnError;
      FOnPktDump: TOnParseDirectPkt;
      fOnHandle: TOnHandle;
      fpUser: PicqDirectUser;
      CSck:TMySocket;
      SSck:TSrvSocket;
      fDPkt:TRawPkt;
      fPktLen:Integer;
      fPktSize:Integer;

      procedure SetCSck(aSock:TMySocket);
      procedure OnSockError(Sender: TObject);
      procedure OnSockConnectError(Sender: TObject);
      procedure OnConnect(Sender: TObject);
      procedure OnReceive(Sender: TObject; Socket: TSocket; Buffer: Pointer; BufLen: LongWord);
      procedure OnIntPktDump(Sender: TObject; Buffer: Pointer; BufLen: LongWord; Incoming: Boolean; UIN: Cardinal);
      procedure OnIntError(Sender: TObject; ErrorType: TErrorType; ErrorMsg: String); virtual;
    Public
      EventType:Integer;
      MyUIN:LongWord;

      constructor Create(aMyUIN: LongWord);
      destructor Destroy; override;

      Function Start:Boolean;Virtual;abstract;  //Used to start connection.
      Procedure Stop;Virtual;abstract;          //Used to stop connection.
      Function SendData(Pkt: PRawPkt):Boolean;Virtual; // Used to Send Data through Connection.
      Function HandlePacket(pPkt: PRawPkt; PktLen:Integer):Boolean;Virtual;

      Property Client:TMySocket Read CSck write SetCSck;

      Property RemoteUIN:LongWord read fRemUIN write fRemUIN;
      property BindPort: Word read FPort;
      property ProxyType: TProxyType read FProxyType write FProxyType;
      property ProxyHost: String read FProxyHost write FProxyHost;
      property ProxyPort: Word read FProxyPort write FProxyPort;
      property ProxyUserID: String read FUserID write FUserID;
      property ProxyAuth: Boolean read FProxyAuth write FProxyAuth;
      property ProxyPass: String read FProxyPass write FProxyPass;
      property UseProxyResolve: Boolean read FResolve write FResolve default False;

      Property OnDCEvent:TicqDCEvent read fOnDCEvent write fOnDCEvent;
      property OnPktDump: TOnParseDirectPkt read FOnPktDump write FOnPktDump;
      property OnHandle: TOnHandle read fOnHandle write fOnHandle;
      property OnError: TOnError read FOnError write FOnError;
  End;

  // DC for Main DC
  TicqDCNormal = Class(TicqBaseDC)
    Private

    Public
      Constructor Create(aMyUIN: LongWord; aClient: TMySocket; Incoming:Boolean);

      Function Start:Boolean;override;
      Procedure Stop;override;
      Function HandlePacket(pPkt: PRawPkt; PktLen:Integer):Boolean; Override;
  End;

  // DC for Recv File
  TicqDCRecvFile = Class(TicqBaseDC)
    private
      fSrvConnected:Boolean;
      Procedure OnRFSrvSockConnect(Sender: TObject; Socket: TMySocket);
    Public
      PDataPacket:PRawPkt;
      DataPacketLen:LongWord;
      IsLastPacket:Boolean;
      TotalBytes:LongWord;
      NickName:String;
      FTStartRec:TFTStartRec;
      FTRequestRec:TFTRequestRec;
      FTPos:LongWord;

      Constructor Create(MyUIN, aUIN: LongWord; aPort:Dword);

      Function Start:Boolean;override;
      Procedure Stop;override;
      Function HandlePacket(pPkt: PRawPkt; PktLen:Integer):Boolean; Override;
  End;

  // DC for Send File
  TicqDCSendFile = Class(TicqBaseDC)
    Private
      tmrSend:TThreadTimer;
      fPeerReady:Boolean;
      fRemPort:DWord;
      fSeq:Word;
      fPkt:TRawPkt;
      fConnectionFinished: Boolean;
      fTransfering:Boolean;
      fAborted:Boolean;

      procedure OnSFConnect(Sender: TObject);
      procedure OnSFDisconnect(Sender: TObject);
      procedure OnSendTimer(Sender: TObject);
      Procedure OnDataSent(Sender: TObject);
    Public
      PDataPacket:PRawPkt;
      DataPacketLen:LongWord;
      IsLastPacket:Boolean;
      SendFileRecord:TSendFileRec;

      Constructor Create(MyUIN, aUIN: LongWord; aFileRec:TSendFileRec);
      Destructor Destroy;override;

      Function Start:Boolean;override;
      Procedure Stop;override;
      Function HandlePacket(pPkt: PRawPkt; PktLen:Integer):Boolean; Override;

      Property Aborted:Boolean read fAborted;
  End;

  // DC for chat
  TicqDCChat = Class(TicqBaseDC)
    Private
    Public
  End;

Const
  // PEER Commands
  PEER_INIT        = $FF;
  PEER_INIT2       = $03;
  PEER_MSG         = $02;
  PEER_FILE_INIT   = $00;
  PEER_FILE_INITACK= $01;
  PEER_FILE_DATA   = $06;
  //Event Types
  DCEVENT_OnFTInit        = $0002;
  DCEVENT_OnFTStart       = $0004;
  DCEVENT_OnFTFileData    = $0008;
  DCEVENT_OnSendFileStart = $0020;
  DCEVENT_OnSendFileData  = $0040;
  DCEVENT_OnSendFileFinish= $0080;
  // Idle Timeout
  MAXTICKS_TIMEOUT = 18*60*60*5; { 5 min in ticks = 18 ticks per sec, 60 sec per min, 5 min}

ResourceString
  RS_ERROR_BUFFEROVERRUN          = 'Buffer Overrun Error';
  RS_ERROR_NO_DC_WRONGVER         = 'Can not estabilish direct connection due to client using an unsupported protocol version.';
  RS_ERROR_NO_DC_SECURITY         = 'Can not estabilish direct connection due to security issues.';
  RS_ERROR_NO_DC_UNSUPPORTEDPROXY = 'Can not estabilish direct connection due to client using an unsupported proxy type.';
  RS_ERROR_NO_DC_PACKETERROR      = 'Can not estabilish direct connection due to Packet Error.';

implementation

Procedure GiveUpCpuTime;
var
  Msg: TMsg;
begin
  While PeekMessage(Msg, 0, 0, 0, PM_REMOVE) Do
      begin
        TranslateMessage(Msg);
        DispatchMessage(Msg);
      end;
End;

procedure FreeAndNil(var Obj);
var
  P: TObject;
begin
  P := TObject(Obj);
  TObject(Obj) := nil;
  P.Free;
end;

Function IPToStr(aIP:LongWord):String;
Var
  HV:String;
Begin
  HV := IntToHex(aIP, 8);
  Result := '';
  Result := IntToStr(HexToInt(Copy(HV, 7, 2))) + '.';
  Result := Result + IntToStr(HexToInt(Copy(HV, 5, 2)))+ '.';
  Result := Result + IntToStr(HexToInt(Copy(HV, 3, 2))) + '.';
  Result := Result + IntToStr(HexToInt(Copy(HV, 1, 2)));
End;

//****************************************************************************//
{ TicqDCM }
constructor TicqDCM.Create(aMyUIN: LongWord);
Begin
  inherited Create;
  fDestroying := False;
  fUL   := TList.Create;
  fDCL  := TList.Create;
  MyUIN := aMyUin;
  fPort := FindBindPort;
  GetMem(fpUser, SizeOf(TicqDirectUser));
  With fpUser^ Do Begin
    UIN := MyUIN;
    Port := fPort;
    ExtIP := LongWord(GetLocalIP);
    IntIP := LongWord(GetLocalIP);
    LastActivity := GetTickCount;
    DCMain := -1;
    DCRecvFile := -1;
    DCSendFile := -1;
    DCChat     := -1;
  End;
  fSrv  := TSrvSocket.Create;
  fOnError := Nil;
  fOnPktDump := Nil;
  fOnHandle := Nil;

  // idle timeout timer (fires once a sec and checks for nonactivity for MAXTICKS_TIMEOUT)
  fTmrIdle := TThreadTimer.Create;
  fTmrIdle.Interval := 1000;
  fTmrIdle.OnTimer := OnIdleTimeOut;
  fTmrIdle.Enabled := True;

  fSrv.OnClientConnected := OnSrvSockConnect;

  fSrv.StartServer(fPort);
End;

destructor TicqDCM.Destroy;
Begin
  fTmrIdle.Enabled := False;
  fTmrIdle.OnTimer := nil;
  fTmrIdle.Free;

  If fDestroying then Exit;
  fDestroying := True;
  fSrv.OnClientConnected := nil;
  fSrv.StopServer;
  fSrv.Free;
  While fUL.Count > 0 do
    DeleteUser(PicqDirectUser(fUL.items[0])^.UIN);
  fUL.Free;
  fDCL.Free;
  FreeMemory(fpUser);
End;

Procedure TicqDCM.OnIdleTimeOut(Sender: TObject);
Var
  i:integer;
  aTC:LongWord;
Begin

  If fUL.Count = 0 then Exit;
  aTC := GetTickCount - MAXTICKS_TIMEOUT;

  For i := 0 to fUL.Count -1 Do
    With PicqDirectUser(fUL[i])^ Do
      If LastActivity < aTC then Begin
        if DCMain <> -1 then
          DeleteDC(DCMain);
    End;
End;

Function TicqDCM.ExtIP:LongWord;
Begin
  Result := fpUser^.ExtIP;
End;

Function TicqDCM.IntIP:LongWord;
Begin
  Result := fpUser^.IntIP;
End;

procedure TicqDCM.OnSrvSockConnect(Sender: TObject; Socket: TMySocket);
Begin
  // Using Temp DC until recv PEER_INIT with user info.
  fTmpDC := TicqDCNormal.Create(0,Socket, True);
  GetMem(fTmpDC.fpUser, SizeOf(TicqDirectUser));
  fTmpDC.MyUIN := MyUin;
  fTmpDC.fManager := Self;
  fTmpDC.fRemUIN := 0;
  With fTmpDC.fpUser^ Do Begin
    UIN        := 0;
    DCMain     := -1;
    DCRecvFile := -1;
    DCSendFile := -1;
    DCChat     := -1;
  End;

  // Proxy Settings
  fTmpDC.FProxyType := ProxyType;
  fTmpDC.FProxyHost := ProxyHost;
  fTmpDC.FProxyPort := ProxyPort;
  fTmpDC.FProxyAuth := ProxyAuth;
  fTmpDC.FProxyPass := ProxyPass;
  fTmpDC.FUserID    := FUserID;
  fTmpDC.FResolve   := FResolve;
  fTmpDC.UseProxyResolve := UseProxyResolve;
  // Events
  fTmpDC.FOnError   := InternalOnErrorProc;
  fTmpDC.FOnPktDump := InternalOnPktDump;
  fTmpDC.fOnHandle  := InternalOnHandle;
  fTmpDC.fOnDCEvent := HandleDCEvent;
  fpUser^.DCMain := fDCL.Add(fTmpDC);  // Add Connection to List
End;

procedure TicqDCM.InternalOnErrorProc(Sender: TObject; ErrorType: TErrorType; ErrorMsg: String);
Begin
  If Assigned(fOnError) then
    fOnError(Sender, ErrorType, ErrorMsg);
End;

procedure TicqDCM.InternalOnPktDump(Sender: TObject; Buffer: Pointer; BufLen: LongWord; Incoming: Boolean; UIN: Cardinal);
Begin
  If assigned(fOnPktDump) then
    fOnPktDump(Sender, Buffer, BufLen, Incoming, UIN);
End;

procedure TicqDCM.InternalOnHandle(Sender: TObject; UIN: LongWord; Pak: PRawPkt; Len: LongWord);
Begin
  If Assigned(fOnHandle) then
    fOnHandle(Sender, UIN, Pak, Len);
End;

Procedure TicqDCM.HandleDCEvent(Sender: TicqBaseDC);
Var
  dcRF:TicqDCRecvFile;
  dcSF:TicqDCSendFile;
Begin

  // Handle Event From one of the DC's
  Case Sender.EventType Of
    DCEVENT_OnFTInit        :
      Begin
        dcRF := TicqDCRecvFile(Sender);
        If Assigned(fOnFTInit) then
          fOnFTInit(Self, dcRF.RemoteUIN, dcRF.FTStartRec.FilesCount, dcRF.TotalBytes, dcRF.FTStartRec.Speed, dcRF.NickName);
      End;
    DCEVENT_OnFTStart       :
      Begin
        dcRF := TicqDCRecvFile(Sender);
        If Assigned(fOnFTStart) Then
          fOnFTStart(Self, dcRF.FTStartRec, dcRF.FTRequestRec.FileName, dcRF.FTRequestRec.FileSize, dcRF.FTStartRec.Speed);
      End;
    DCEVENT_OnFTFileData    :
      Begin
        dcRF := TicqDCRecvFile(Sender);
        If assigned(fOnFTFileData) Then
          fOnFTFileData(Self, dcRF.RemoteUIN, dcRF.PDataPacket, dcRF.DataPacketLen, dcRF.IsLastPacket);
      End;
    DCEVENT_OnSendFileStart :
      Begin
        dcSF := TicqDCSendFile(Sender);
        If Assigned(fOnSendFileStart) Then
          fOnSendFileStart(Self, dcSF.RemoteUIN, dcSF.SendFileRecord);
      End;
    DCEVENT_OnSendFileData  :
      Begin
        dcSF := TicqDCSendFile(Sender);
        If Assigned(fOnSendFileData) Then
          fOnSendFileData(Self, dcSF.RemoteUIN, dcSF.PDataPacket, dcSF.DataPacketLen, dcSF.IsLastPacket);
      End;
    DCEVENT_OnSendFileFinish:
      Begin
        dcSF := TicqDCSendFile(Sender);
        If Assigned(fOnSendFileFinish) Then
          fOnSendFileFinish(Self, dcSF.RemoteUIN, dcSF.SendFileRecord, dcSF.Aborted);
      End;
  End;
End;

Function TicqDCM.SendData(aUIN:LongWord; pPkt: PRawPkt):Boolean;
Var
  i:integer;
Begin
  // Send Pkt through DCMain of User (aUIN)
  Result := False;
  If GetUserIndex(aUIN, i) then
    With PicqDirectUser(fUL[i])^ Do
      If IsConnected and (DCMain <> -1) Then Begin
        TicqDCNormal(fDCL.Items[DCMain]).SendData(pPkt);
        Result := True;
      End;
End;

function TicqDCM.SendDataFile(aUIN: LongWord; Pak: PRawPkt): Boolean;
Var
  i:Integer;
Begin
  // Send Pkt Through DCRecvFile of User (aUIN)
  Result := False;
  If GetUserIndex(aUIN, i) then
    With PicqDirectUser(fUL[i])^ Do
      If DCRecvFile <> -1 then Begin
        TicqDCRecvFile(fDCL[DCRecvFile]).SendData(Pak);
        Result := True;
      End;
End;

function TicqDCM.AddFileUser(aUIN: LongWord; var aPort: Word; FTReqRec:Pointer = nil): Boolean;
Var
  I:integer;
  aDC:TicqDCRecvFile;
  FTRec:TFTRequestRec;
  BPort:Word;
Begin
  Result := False;
  BPort := FindBindPort;
  // Add a DCRecvFile to User (aUIN)
  If GetUserIndex(aUin, i) then Begin
    With PicqDirectUser(fUL[i])^ Do Begin
      If DCRecvFile <> -1 Then
        DeleteDC(DCRecvFile);
      aPort := BPort;
      aDC := TicqDCRecvFile.Create(MyUIN, aUin, aPort);
      aDC.fIncoming := True;
      DCRecvFile := fDCL.Add(aDC);
      aDC.fpUser     := PicqDirectUser(fUL.Items[i]);
      aDC.fManager   := Self;
      aDC.FPort      := BPort;
      // Proxy Settings
      aDC.FProxyType := ProxyType;
      aDC.FProxyHost := ProxyHost;
      aDC.FProxyPort := ProxyPort;
      aDC.FProxyAuth := ProxyAuth;
      aDC.FProxyPass := ProxyPass;
      aDC.FUserID    := FUserID;
      aDC.FResolve   := FResolve;
      aDC.UseProxyResolve := UseProxyResolve;
      // Events
      aDC.FOnError   := InternalOnErrorProc;
      aDC.FOnPktDump := InternalOnPktDump;
      aDC.fOnHandle  := InternalOnHandle;
      aDC.fOnDCEvent := HandleDCEvent;
      If FTReqRec <> nil then
        With aDC.FTRequestRec Do Begin
          FTRec := TFTRequestRec(FTReqRec^);
          ReqType     := FTRec.ReqType;
          ITime       := FTRec.ITime;
          IRandomID   := FTRec.IRandomID;
          UIN         := FTRec.UIN;
          Description := FTRec.Description;
          FileName    := FTRec.FileName;
          FileSize    := FTRec.FileSize;
          Seq         := FTRec.Seq;
          Port        := aPort;
        End;
      // Connect
      aDC.Start;
      Result := True;
    End;
  End;
End;

Procedure TicqDCM.SetFileRecord(aUIN: LongWord; aFileRec:TSendFileRec);
Var
  i:integer;
  aDC:TicqDCSendFile;
Begin
  If GetUserIndex(aUIN, i) Then
    With PicqDirectUser(fUL[i])^ Do Begin
      If DCSendFile <> -1 Then
        DeleteDC(DCSendFile);
      aDC := TicqDCSendFile.Create(MyUin, aUin, aFileRec);
      DCSendFile := fDCL.Add(aDC);
      aDC.MyUIN     := MyUin;
      aDC.fpUser    := PicqDirectUser(fUL[i]);
      aDC.fIncoming := False;
      aDC.fManager  := Self;
      aDC.FOnError   := InternalOnErrorProc;
      aDC.FOnPktDump := InternalOnPktDump;
      aDC.fOnHandle  := InternalOnHandle;
      aDC.OnDCEvent := HandleDCEvent;
    End;
End;

Function TicqDCM.AddSendFileUser(aUIN:LongWord; Var aPort, aSeq:Word):Boolean;
Var
  i:integer;
Begin
  // Add a DCSendFile to User (aUIN);
  Result := False;
  If GetUserIndex(aUin, i) Then
    With PicqDirectUser(fUL[i])^ do Begin
      If DCSendFile <> -1 Then
       With TicqDCSendFile(fDCL[DCSendFile]) Do Begin
         //fpUser^.Port := Port;
         fSeq := aSeq;
         fRemPort := aPort;
         Start;
        End;
    End;
End;

function TicqDCM.StopFileReceiving(aUIN: LongWord): Boolean;
Var
  I:Integer;
Begin
  Result := false;
  // Break DCRecvFile connection for User (aUIN)
  If GetUserIndex(aUIN, i) Then
    With PicqDirectUser(fUL[i])^ Do
      If DCRecvFile <> -1 then
        DeleteDC(DCRecvFile);
  Result := True;
End;

Procedure TicqDCM.StopFileSending(aUIN: LongWord);
Var
  I:Integer;
Begin
  // Stop File Sending;
  If GetUserIndex(aUIN, i) then
    With PicqDirectUser(fUL[i])^ Do
      If DCSendFile <> -1 then Begin
        TicqDCSendFile(fDCL[DCSendFile]).Stop;
        Sleep(100);
        DeleteDC(DCSendFile);
      End;
End;

procedure TicqDCM.EstabilishConnection(aUIN: LongWord);
Var
  i, i2: integer;
  aDC:TicqDCNormal;
Begin
  // Establish DCMain connection for User (aUIN)
  If GetUserIndex(aUIN, i) then
    With PicqDirectUser(fUL.Items[i])^ Do Begin
      If IsConnected then Exit;
      If fpUser^.IntIP + fpUser^.ExtIP = 0 then Exit;
      if fpUser^.Port < 1025 Then Exit;
      If DCMain <> -1 then
        TicqDCNormal(fDCL.Items[DCMain]).Free;
      aDC := TicqDCNormal.Create(aUIN, TMySocket.Create, False);
      DCMain := fDCL.Add(aDC);
      aDC.fpUser     := PicqDirectUser(fUL.Items[i]);
      aDC.fManager   := Self;
      aDC.fRemUIN    := aUIN;
      aDC.MyUIN      := MyUIN;
      aDC.fPort      := FindBindPort;
      // Proxy Settings
      aDC.FProxyType := ProxyType;
      aDC.FProxyHost := ProxyHost;
      aDC.FProxyPort := ProxyPort;
      aDC.FProxyAuth := ProxyAuth;
      aDC.FProxyPass := ProxyPass;
      aDC.FUserID    := FUserID;
      aDC.FResolve   := FResolve;
      aDC.UseProxyResolve := UseProxyResolve;
      // Events
      aDC.FOnError   := InternalOnErrorProc;
      aDC.FOnPktDump := InternalOnPktDump;
      aDC.fOnHandle  := InternalOnHandle;
      aDC.fOnDCEvent := HandleDCEvent;
      // Connect
      aDC.Start;
      Sleep(100);
      If aDC.Client.Connected Then // If Socket is connecting then give up time and wait.
        For i2 := 0 to 300 Do Begin
          Sleep(10);
          GiveUpCpuTime;
          If IsConnected then Break;
        End;
    End;
End;

function TicqDCM.ConnectionEstabilished(aUIN: LongWord): Boolean;
Var
  i:integer;
  p:PicqDirectUser;
Begin
  // Poll for DCMain Connection for User (aUIN), should open a DCMain
  Result := False;
  If GetUserIndex(aUIN, i) then Begin
    P := PicqDirectUser(fUL.Items[i]);
    Result := P^.IsConnected;
    If Result Then
      Exit;
    // Attemp to Establish Connection
    EstabilishConnection(aUIN);  // Implements Connect On Demmand
    Result := P^.IsConnected;
  End;
End;

Procedure TicqDCM.DeleteUser(aUIN: LongWord);
Var
  i:integer;
Begin
 If GetUserIndex(aUIN, i) Then
   With PicqDirectUser(fUL.Items[i])^ Do Begin
     // Disconnect any connections
     If DCChat <> -1 then
       DeleteDC(DCChat);
     If DCSendFile <> -1 then
       DeleteDC(DCSendFile);
     If DCRecvFile <> -1 then
       DeleteDC(DCRecvFile);
     If DCMain <> -1 then
       DeleteDC(DCMain);
     FreeMem(fUL.Items[i], SizeOf(TicqDirectUser));
     fUL.Delete(i);
     Exit;
   End;
End;

Procedure TicqDCM.DeleteDC(Var aIndex:Integer);
Var
  aDC:TicqBaseDC;
Begin
  If (aIndex < 0) or (aIndex > (fDCL.Count -1))Then Begin
    aIndex := -1;
    Exit;
  End;
  aDC := TicqBaseDC(fDCL[aIndex]);

  fDCL.Delete(aIndex);
  aIndex := -1;
  If aDC = nil then Exit;
  With aDC Do
    If CSck <> nil then
      If CSck.Connected Then
        CSck.Disconnect;
  FreeAndNil(aDC);
End;

Function TicqDCM.GetUserIndex(aUIN:LongWord; Var Idx:integer):Boolean;
Var
  i:Integer;
Begin
  Result := False;
  For i := 0 to fUL.Count -1 Do
    If PicqDirectUser(fUL[i])^.UIN = aUIN then Begin
      Idx := i;
      Result := True;
      Exit;
    End;
End;
procedure TicqDCM.AddUser(aUIN, aCookie, aIPExt, aIPInt: LongWord; aPort: Word);
Var
  i:integer;
  p: PicqDirectUser;
Begin
  If GetUserIndex(aUIN, i) then
    With PicqDirectUser(fUL[i])^ Do Begin
    Cookie := aCookie;
    ExtIP  := aIPExt;
    IntIP  := aIPExt;
    Port   := aPort;
    IsConnected := False;
    If DCChat <> -1 Then
      DeleteDC(DCChat);
    If DCSendFile <> -1 Then
      DeleteDC(DCSendFile);
    If DCRecvFile <> -1 Then
      DeleteDC(DCRecvFile);
    If DCMain <> -1 then
      DeleteDC(DCMain);
    Exit;
    End;
  GetMem(p, SizeOf(TicqDirectUser));
  p^.UIN    := aUin;
  p^.Cookie := aCookie;
  p^.ExtIP  := aIPExt;
  p^.IntIP  := aIPInt;
  p^.Port   := aPort;
  p^.IsConnected := False;
  p^.DCMain      := -1;
  p^.DCRecvFile  := -1;
  p^.DCSendFile  := -1;
  p^.DCChat      := -1;
  fUL.Add(p);
End;

//****************************************************************************//
{ TicqBaseDC }
constructor TicqBaseDC.Create(aMyUIN: LongWord);
Begin
  Inherited Create;
  CSck := nil;
  SSck := nil;
  MyUin := aMyUIN;
  OnDCEvent := nil;
  OnError   := nil;
  OnHandle  := nil;
  OnPktDump := nil;
End;

destructor TicqBaseDC.Destroy;
Begin
  Inherited Destroy;
  Try
    //CSck.Disconnect;
    CSck.Free;
  Except
  End;
End;

Procedure TicqBaseDC.SetCSck(aSock:TMySocket);
Begin
  If CSck = aSock then Exit;
  If CSck <> nil then
    CSck.Free;
  CSck := aSock;
End;

Function TicqBaseDC.SendData(Pkt: PRawPkt):Boolean;
var
  buf: array[0..$FFFF - 1] of Byte;
Begin
  // Send Data
  If CSck.Connected Then Begin
    CSck.SendData(Pkt^.Len, 2);
    CSck.SendData(Pkt^, Pkt^.Len);
    Result := True;
  End Else Begin
    OnSockError(Self);
    Result := False;
    Exit;
  End;
  fpUser^.LastActivity := GetTickCount;
  If Assigned(fOnPktDump) Then Begin
    PWord(@buf)^ := Pkt^.Len;
    Move(Pkt^.Data, Ptr(LongWord(@buf) + 2)^, Pkt^.Len);
    fOnPktDump(Self, @Buf, Pkt^.Len + 2, False, fRemUIN);
  End;
End;

procedure TicqBaseDC.OnSockError(Sender: TObject);
Begin
  //fpUser^.IsConnected := False;

  Try
    If Sender is TMySocket then
    With Sender as TMySocket do Begin
      OnConnectError := Nil;
      OnDisconnect   := Nil;
      OnError        := Nil;
      If Connected Then
        Disconnect;
    End;
  Except
    //Ignore any errors
  End;

  If fpUser^.DCMain <> -1 Then
    If Sender = TicqBaseDC(fManager.fDCL[fpUser^.DCMain]).CSck then Begin
      fManager.DeleteDC(fpUser^.DCMain);
      fpUser^.IsConnected := False;
      Exit;
    End;
  If fpUser^.DCRecvFile <> -1 then
    If Sender = TicqBaseDC(fManager.fDCL[fpUser^.DCRecvFile]).CSck then Begin
      fManager.DeleteDC(fpUser^.DCRecvFile);
      Exit;
    End;
  If fpUser^.DCSendFile <> -1 then
    If Sender = TicqBaseDC(fManager.fDCL[fpUser^.DCSendFile]).CSck then Begin
      fManager.DeleteDC(fpUser^.DCSendFile);
      Exit;
    End;
  If fpUser^.DCChat <> -1 then
    If Sender =  TicqBaseDC(fManager.fDCL[fpUser^.DCChat]).CSck then
      fManager.DeleteDC(fpUser^.DCChat);
End;

procedure TicqBaseDC.OnSockConnectError(Sender: TObject);
Begin
  OnSockError(Self);
End;

procedure TicqBaseDC.OnConnect(Sender: TObject);
Var
  Pkt:TRawPkt;
Begin
  If fIncoming then Begin

  End Else
    With fpUser^ DO Begin
        LastActivity := GetTickCount;
        CreatePEER_INIT(@pkt, Cookie, fRemUIN, MyUIN, fManager.fpUser^.Port, fManager.fpUser^.ExtIP, fManager.fpUser^.IntIP, CSck.ProxyType);
        SendData(@pkt);
    End;
End;

{Recieve data and packetize it, then send it to the subclass}
procedure TicqBaseDC.OnReceive(Sender: TObject; Socket: TSocket; Buffer: Pointer; BufLen: LongWord);
Var
  i:integer;
Begin
  fpUser^.LastActivity := GetTickCount;

  If BufLen = 0 then Exit;
  For i := 0 to BufLen -1 Do Begin
    fDPkt.Data[fPktLen] := PByte(LongWord(Buffer) + LongWord(i))^;
    Inc(fPktLen);
    If fPktLen = 2 then Begin
      fPktSize := PWord(@fDPkt)^;
      If fPktSize > SizeOf(fDPkt.Data) Then Begin
        OnIntError(Self, ERR_INTERNAL, RS_ERROR_BUFFEROVERRUN);
        OnSockError(Self);
        Exit;
      End;
    End;
    If fPktLen = (fPktSize + 2) then Begin
      fDPkt.Len := 0;
      fPktSize  := 0;
      If Not HandlePacket(@fDPkt, fPktLen) Then
        fManager.InternalOnHandle(Sender, fpUser^.UIN, @fDPkt, fDPkt.Len);
      fPktLen   := 0;
    End;
  End;
End;

procedure TicqBaseDC.OnIntPktDump(Sender: TObject; Buffer: Pointer; BufLen: LongWord; Incoming: Boolean; UIN: Cardinal);
Begin
  If Assigned(fOnPktDump) Then
    fOnPktDump(Sender, Buffer, BufLen, Incoming, UIN);
End;

procedure TicqBaseDC.OnIntError(Sender: TObject; ErrorType: TErrorType; ErrorMsg: String);
Begin
  If Assigned(fOnError) Then
    fOnError(Sender, ErrorType, ErrorMsg);
End;

Function TicqBaseDC.HandlePacket(pPkt: PRawPkt; PktLen:Integer):Boolean;
Var
  i:Integer;
  PktPos:Integer;
  aPktLen:Integer;
  lPkt:TRawPkt;
  PrtVer, Port, Port2:Word;
  CheckByte, PrxType:Byte;
  UIN, aRemUIN, Cookie, ExtIP, IntIP:LongWord;
Begin
  Result := False;
  // Handle Incoming Packet;
  PktPos := pPkt^.Len;
  Try
    If Assigned(fOnPktDump) Then
      fOnPktDump(Self, pPkt, PktLen, True, fpUser^.UIN);
    // Decode Packet Len and Command
    aPktLen := GetLint(pPkt, 2);
    Case GetInt(pPkt, 1) Of
      PEER_INIT:
        Begin
          Try
            // Get Data From Packet.
            PrtVer    := GetLInt(pPkt, 2);// ProtoVersion (WORD.L)
            CheckByte := GetLInt(pPkt, 2);// $2B  ??  (BYTE)
            UIN       := GetLInt(pPkt, 4);//  UIN (DOWRD.L)
            Inc(pPkt^.Len, 2);// $00 - $00  (WORD)
            Port      := GetLInt(pPkt, 4);// Port (DWORD.L)
            aRemUIN   := GetLInt(pPkt, 4);// Remote UIN  (DWORD.L)
            ExtIP     := GetLInt(pPkt, 4);// IP's (2 x DWORD.L)
            IntIP     := GetLInt(pPkt, 4);
            PrxType   := GetInt(pPkt, 1);// ProxyType (Byte)
            Port2     := GetLInt(pPkt, 4);// Port (DWORD.L)
            Cookie    := GetInt(pPkt, 4);// Cookie (DWORD)

            // Put Data where it Belongs.
            If fIncoming And (fpUser^.UIN = 0)Then Begin
              // Need to Check if RemUIN is in User list, if not then
              // Should Denie connection (not in Contacts) else
              // Should Move the connection over to that user.

              If fManager.GetUserIndex(aRemUIN, i) then Begin
                fRemUIN := aRemUIN;
                With PicqDirectUser(fManager.fUL[i])^ Do Begin
                  If DCMain <> -1 then
                    DeleteDC(DCMain);
                  DCMain := fManager.fDCL.IndexOf(Self);
                  // Ok Connection is moved, free mem.
                  FreeMem(fpUser, SizeOf(TicqDirectUser));
                  fpUser := PicqDirectUser(fManager.fUL[i]);
                End;
              End Else Begin
                // Else Denie connection.
                i := fManager.fDCL.IndexOf(Self);
                fManager.DeleteDC(i);
              End;
            End;
            // Ack it.
            CreatePEER_ACK(@lPkt);
            SendData(@lPkt);
            // Check Data.
            If PrtVer < 7 then Begin
              OnIntError(Self, ERR_Internal, RS_ERROR_NO_DC_WRONGVER);
            End;
            If (Uin <> MyUIN) or (fRemUIN <> aRemUIN) or
               (Port <> Port2) or (fpUser^.Cookie <> Cookie)Then Begin
              OnIntError(Self, ERR_INTERNAL, RS_ERROR_NO_DC_SECURITY);
              OnSockError(Self);
              Exit;
            End;
            If (PrxType <> 01) and (PrxType <> 02) and (PrxType <> 04) Then Begin
              OnIntError(Self, ERR_INTERNAL, RS_ERROR_NO_DC_UNSUPPORTEDPROXY);
              OnSockError(Self);
              Exit;
            End;
            If fIncoming Then Begin
              // And Send a PEER_INIT
              CreatePEER_INIT(@lpkt, fpUser^.Cookie, aRemUIN, MyUIN, fpUser^.Port, fpUser^.ExtIP, fpUser^.IntIP, CSck.ProxyType);
              SendData(@lPkt);

            End Else Begin
              // And Send PEER_INIT2
              CreatePEER_INIT2(@lpkt, FIncoming);
              SendData(@lpkt);
              //fpUser^.IsConnected := True;
            End;
            Result := True;
          Except
            OnIntError(Self, ERR_INTERNAL, RS_ERROR_NO_DC_PACKETERROR);
          End;
        End;
      PEER_INIT2:
        Begin
        Result := True;
        If fIncoming then Begin
          CreatePEER_INIT2(@lpkt, FIncoming);
          SendData(@lpkt);
          Result := True;
        End;
        fpUser^.IsConnected := True;
        //Result := False;
        End;
    Else
      Result := False;
    End;
  Finally
    If Not Result then
      pPkt^.Len := PktPos;
  End;
End;

//****************************************************************************//
{ TicqDCNormal }
constructor TicqDCNormal.Create(aMyUIN: LongWord; aClient: TMySocket; Incoming:Boolean);
Begin
  Inherited Create(aMyUIN);
  fIncoming := Incoming;
    Client := aClient;
    CSck.OnConnectError := OnSockConnectError;
    CSck.OnDisconnect   := OnSockError;
    Csck.OnConnectProc  := OnConnect;
    CSck.OnReceiveProc  := OnReceive;
End;

Function TicqDCNormal.Start:Boolean;
Var
  inaddr: in_addr;
Begin
  Result := False;
  If fIncoming then Begin
    Exit;
  End;
  // Connect
  inaddr.S_addr := fpUser^.ExtIP;
  CSck.Host := inet_ntoa(inaddr);
  CSck.Port := fpUser^.Port;
  CSck.Connect;
  Result := True;
End;

Procedure TicqDCNormal.Stop;
Begin
  // Disconnect
  cSck.Disconnect;
  fpUser^.IsConnected := False;
End;

Function TicqDCNormal.HandlePacket(pPkt: PRawPkt; PktLen:Integer):Boolean;
Var
  aPktLen:Integer;
  CheckByte, PrxType:Byte;
  UIN, aRemUIN, Cookie, ExtIP, IntIP:LongWord;
Begin
  Result := Inherited HandlePacket(pPkt, PktLen);
  If Result then Exit;
  // Handle Incoming Packet;
    aPktLen := GetLint(pPkt, 2);
    Case GetInt(pPkt, 1) Of
       PEER_MSG:
         Begin
           // Pass Pkt back to ICQClient for handling.
           pPkt^.Len := PktLen;  // Set Len to Size of Pkt for Decryption.
           Result := False;
         End;
    Else
      Result := False;
    End;
End;

//****************************************************************************//
{ TicqDCRecvFile }
Constructor TicqDCRecvFile.Create(MyUIN, aUIN: LongWord; aPort:Dword);
Begin
  Inherited Create(MyUIN);
  fRemUIN := aUIN;
  fPort   := aPort;
  DataPacketLen := 0;
  NickName := '';
  IsLastPacket := False;
  TotalBytes := 0;
  FTRequestRec.UIN := aUIN;
  FTRequestRec.Port:= aPort;
  FTStartRec.UIN   := aUIN;
End;

Procedure TicqDCRecvFile.OnRFSrvSockConnect(Sender: TObject; Socket: TMySocket);
Begin
  Client := Socket;

  CSck.OnConnectError := OnSockConnectError;
  CSck.OnDisconnect   := OnSockError;
  Csck.OnConnectProc  := OnConnect;
  CSck.OnReceiveProc  := OnReceive;

  SSck.StopServer;
  fSrvConnected := True
End;

Function TicqDCRecvFile.Start:Boolean;
Begin
  SSck := TSrvSocket.Create;
  SSck.OnClientConnected := OnRFSrvSockConnect;
  SSck.StartServer(FTRequestRec.Port);
  Result := True;
End;

Procedure TicqDCRecvFile.Stop;
Begin
  If fSrvConnected then Begin
    CSck.Disconnect;
  End Else
    SSck.StopServer;
End;

Function TicqDCRecvFile.HandlePacket(pPkt: PRawPkt; PktLen:Integer):Boolean;
Var
  aPktLen:Integer;
  lPkt:TRawPkt;
Begin
  Result := Inherited HandlePacket(pPkt, PktLen);
  If Result then Exit;
  // Handle Incoming Packet;
    aPktLen := GetLint(pPkt, 2);
    Case GetInt(pPkt, 1) Of
      PEER_FILE_INIT:
        Begin
          Inc(pPkt^.Len, 4);  //Skip Unknown (00 00 00 00)
          FTStartRec.FilesCount := GetLInt(pPkt, 4);  // Number of files sent
          TotalBytes := GetLint(pPkt, 4);             // TotalSize of all Files
          FTStartRec.Speed := GetLInt(pPkt, 4);       // Speed: 0 = Pause, 64 = Bo Delay
                                                      // 0 < n < 64 = (64-n) * 0.05s delay.
          NickName := GetLNTS(pPkt);                  // Nick of Sender;

          CreatePEER_FILEINITACK(@lPkt, FTStartRec.Speed, NickName);
          SendData(@lPkt);

          EventType := DCEVENT_OnFTInit;
          If assigned(fOnDCEvent) then  // Call the Event OnFTInit
            fOnDCEvent(Self);
        End;
      PEER_MSG:
        Begin
          pPkt^.Len := 3;
          Inc(pPkt^.Len, 1);
          FTRequestRec.FileName := GetLNTS(pPkt);
          GetLNTS(pPkt);
          FTRequestRec.FileSize := GetLInt(pPkt, 4);
          FTPos := 0;
          EventType := DCEVENT_OnFTStart;
          If Assigned(fOnDCEvent) Then
            fOnDCEvent(Self);
          pPkt^.Len := PktLen;
          Result := False;
        End;
      PEER_FILE_DATA:
      Begin
        DataPacketLen := aPktLen -1;
        Inc(FTPos, DataPacketLen);
        IsLastPacket := Not (FTPos < FTRequestRec.FileSize);
        PDataPacket := Ptr(LongWord(pPkt) + 3);
        EventType := DCEVENT_OnFTFileData;
        If Assigned(fOnDCEvent) then
          fOnDCEvent(Self);
        If IsLastPacket and (FTStartRec.FilesCount = 1) then
          CSck.Disconnect;    //Last File Dissconnect
        Result := True;
      End;
    Else
      Result := False;
    End;

End;

//****************************************************************************//
{ TicqDCSendFile }
Constructor TicqDCSendFile.Create(MyUIN, aUIN: LongWord; aFileRec:TSendFileRec);
Begin
  Inherited Create(MyUIN);
  pDataPacket := @fPkt;

  fRemUIN := aUIN;

  tmrSend := TThreadTimer.Create;
  tmrSend.Interval := 1;
  tmrSend.OnTimer  := OnSendTimer;

  fConnectionFinished := False;
  fTransfering := False;
  fAborted := False;

  CSck := TMySocket.Create;
  CSck.OnConnectError := OnSockConnectError;
  CSck.OnDisconnect   := OnSFDisconnect;
  Csck.OnConnectProc  := OnSFConnect;
  CSck.OnReceiveProc  := OnReceive;
  CSck.OnDataSent     := OnDataSent;

  With SendFileRecord Do Begin
    SendFileRecord.Uin := aFileRec.UIN;
    Nick      := aFileRec.Nick;
    Seq       := aFileRec.Seq;
    FilesCount:= aFileRec.FilesCount;
    FilePath  := aFileRec.FilePath;
    FileName  := aFileRec.FileName;
    FileDescription := aFileRec.FileDescription;
    FileSize  := aFileRec.FileSize;
    TotalSize := aFileRec.TotalSize;
    SendFileRecord.Port := fRemPort;;
    Speed := aFileRec.Speed;
    Files := aFileRec.Files;
  End;
End;

Procedure TIcqDCSendFile.OnSFDisconnect(Sender: TObject);
Begin
  If Not fTransfering then exit;
  fTransfering := False;
  fAborted := True;
  EventType := DCEVENT_OnSendFileFinish;
  If Assigned(fOnDCEvent) then
    fOnDCEvent(Self);

  OnSockError(Self);
End;

Destructor TicqDCSendFile.Destroy;
Begin
  tmrSend.Free;
  inherited Destroy;
End;

Procedure TicqDCSendFile.OnDataSent(Sender: TObject);
Begin
  If fConnectionFinished then
    fTransfering := False;
End;

procedure TicqDCSendFile.OnSendTimer(Sender: TObject);
Var
  lPkt:TRawPkt;
Begin
  // Send Data;
  tmrSend.Enabled := False;

  If Not CSck.Connected then Exit;
  If IsLastPacket then Begin
    fConnectionFinished := True;
    DataPacketLen := 0;
    EventType := DCEVENT_OnSendFileData;
    If Assigned(fOnDCEvent) then Begin
      fOnDCEvent(Self);
    End;
    Inc(SendFileRecord.FilesCurrent);
    EventType := DCEVENT_OnSendFileFinish;
    If Assigned(fOnDCEvent) Then
      fOnDCEvent(Self);
    If SendFileRecord.Files.Count > SendFileRecord.FilesCurrent then Begin
      With SendFileRecord Do Begin
        FileName := ExtractFileName(Files[FilesCurrent]);
        FilePath := ExtractFilePath(Files[FilesCurrent]);
        FileSize := ICQWorks.FileSizeFromStr(Files[FilesCurrent]);
      End;
      IsLastPacket := False;
      EventType := DCEVENT_OnSendFileStart;
      If Assigned(fOnDCEvent) Then
        fOnDCEvent(Self);

      CreatePEER_FILE_START(@lPkt, SendFileRecord.FileName, SendFileRecord.FileSize, SendFileRecord.Speed);
      SendData(@lPkt);
      exit;
    End;
    // No More Files
    CSck.Disconnect;
    Sleep(150);  // Give time for CSck to disconnect.
    fManager.DeleteDC(fpUser^.DCSendFile);
    Exit;
  End;
  EventType := DCEVENT_OnSendFileData;
  If assigned(fOnDCEvent) then Begin
    fOnDCEvent(Self);
    CreatePEER_FILE_DATA(@lPkt, PDataPacket, DataPacketLen);
    SendData(@lPkt);
    tmrSend.Enabled := True;;
  End;
End;

procedure TicqDCSendFile.OnSFConnect(Sender: TObject);
Var
  lPkt:TRawPkt;
Begin
//  fpUser^.IsConnected := True;
  fConnectionFinished := False;
  fTransfering := False;
  fAborted := False;
  fPeerReady := False;
    CreatePEER_INIT(@lPkt, fpUser^.Cookie, fRemUIN, MyUIN, fPort, fManager.ExtIP, fManager.IntIP, fManager.fProxyType);
    SendData(@lPkt);
End;


Function TicqDCSendFile.Start:Boolean;
Var
  inaddr: in_addr;
Begin
  InAddr.S_addr := fpUser^.ExtIP;
  Csck.Host := inet_ntoa(inaddr);
  CSck.Port := fRemPort;;
  SendFileRecord.Port := fpUser^.Port;
  CSck.Connect;
  EventType := DCEVENT_OnSendFileStart;
  If Assigned(fOnDCEvent) Then
    fOnDCEvent(Self);
  Result := True
End;

Procedure TicqDCSendFile.Stop;
Begin
  CSck.Disconnect;
End;

Function TicqDCSendFile.HandlePacket(pPkt: PRawPkt; PktLen:Integer):Boolean;
Var
  aPktLen:Integer;
  lPkt:TRawPkt;
Begin
  Result := True;
  // Handle Incoming Packet;
   If Assigned(fOnPktDump) Then
     fOnPktDump(Self, pPkt, PktLen, True, fpUser^.UIN);

    aPktLen := GetLint(pPkt, 2);
    Case GetInt(pPkt, 1) Of
       PEER_INIT:
         Begin
           CreatePEER_ACK(@lPkt);
           SendData(@lPkt);
           CreatePEER_FILE_INIT(@lPkt, SendFileRecord.FilesCount, SendFileRecord.FileSize, SendFileRecord.Speed, SendFileRecord.Nick);
           SendData(@lPkt);
           fPeerReady := True;
           Result := True;
         End;
       PEER_INIT2:
         Begin
           tmrSend.Enabled := True;
           Result := True;
         End;
       PEER_FILE_INITACK:
         Begin
           If fPeerReady then Begin
             CreatePEER_FILE_START(@lPkt, SendFileRecord.FileName, SendFileRecord.FileSize, SendFileRecord.Speed);
             SendData(@lPkt);
             Result := True;
           End;
         End;
      PEER_MSG:
        Begin
          // Pass Pkt back to ICQClient for handling.
          pPkt^.Len := PktLen;  // Set Len to Size of Pkt for Decryption.
          Result := False;
        End;
    Else
      Result := False;
    End;
End;

//****************************************************************************//
{ TicqDCChat }
end.

