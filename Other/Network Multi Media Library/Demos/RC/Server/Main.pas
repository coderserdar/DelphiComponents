unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs,
  StdCtrls, Spin, ExtCtrls,
  IdBaseComponent, IdComponent, IdTCPServer, NMMRCServer, NMMCustomServer,
  DB, Grids, DBGrids, ActnList, NMMCustomP2PServer, NMMP2PServer, Menus
  ;

type
  TfrmMain = class(TForm)
    cbActive: TCheckBox;
    Server: TNMMRCServer;
    ActionList: TActionList;
    acShowThreadInfo: TAction;
    edIP: TEdit;
    Label1: TLabel;
    cbEnableRC: TCheckBox;
    Label2: TLabel;
    acActive: TAction;
    acEnableRC: TAction;
    MainMenu1: TMainMenu;
    mnuState: TMenuItem;
    Active1: TMenuItem;
    mnuEnableRC: TMenuItem;
    mnuAbout: TMenuItem;
    mnuExit: TMenuItem;
    edPort: TSpinEdit;
    procedure acShowThreadInfoExecute(Sender: TObject);
    procedure ServerCustomCommand(Sender: TObject; Command: String);
    procedure FormCreate(Sender: TObject);
    procedure acActiveExecute(Sender: TObject);
    procedure mnuAboutClick(Sender: TObject);
    procedure mnuExitClick(Sender: TObject);
    procedure acEnableRCExecute(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    FMouseUpFlag: DWord;
    function LocalIP: string;
  end;

var
  frmMain: TfrmMain;

implementation

uses NMMScreenCapturer, UserList, ThreadInfo, NMMRCCommon, IdSocketHandle, WinSock, ShellAPI,
     ClipBrd, About;

{$R *.dfm}

function TfrmMain.LocalIP: string;
type
   TaPInAddr = array [0..10] of PInAddr;
   PaPInAddr = ^TaPInAddr;
   PHostEnt= ^hostent;
var
    phe: PHostEnt;
    pptr: PaPInAddr;
    Buffer: array [0..63] of char;
    i: Integer;
    GInitData: TWSADATA;
begin
    WSAStartup($101, GInitData);
    Result := '';
    GetHostName(Buffer, SizeOf(Buffer));
    phe:= Pointer(GetHostByName(buffer));
    if phe = nil then Exit;
    pptr := PaPInAddr(Phe^.h_addr_list);
    i := 0;
    while pptr^[i] <> nil do
    begin
      result:=StrPas(inet_ntoa(pptr^[i]^));
      Inc(i);
    end;
    WSACleanup;
end;

procedure TfrmMain.acShowThreadInfoExecute(Sender: TObject);
begin
 frmThreadInfo.Show;
end;

//var LText: array [0..4000] of Char;
procedure TfrmMain.ServerCustomCommand(Sender: TObject;
  Command: String);
var X, Y, LComLen, LKey: Integer;
    Shift: TShiftState;
    ACommand: String;
    LHandle: HWND;
    LClb: TClipBoard;
begin
  LComLen:= Length(Command);
  X:= 1;
  Y:= 1;

  if acEnableRC.Checked then
  begin
    if Pos(cmdMouseMove, Command)=1 then
    begin
      GetXYShift(Copy(Command, Length(cmdMouseMove)+1, LComLen-Length(cmdMouseMove)), X, Y, Shift);
      KeysDown(Shift);
      mouse_event(MOUSEEVENTF_MOVE or MOUSEEVENTF_ABSOLUTE,X,Y,0,0);
      KeysUp(Shift);
    end;
     
    if Pos(cmdMouseDown, Command)=1 then
    begin
      GetXYShift(Copy(Command, Length(cmdMouseDown)+1, LComLen-Length(cmdMouseDown)), X, Y, Shift);
      KeysDown(Shift);
      mouse_event(GenMouseDownFlag(Shift) or MOUSEEVENTF_ABSOLUTE,X,Y,0,0);
      FMouseUpFlag:= GenMouseUpFlag(Shift);
      KeysUp(Shift);
    end;

    if Pos(cmdMouseUp, Command)=1 then
    begin
      GetXYShift(Copy(Command, Length(cmdMouseUp)+1, LComLen-Length(cmdMouseUp)), X, Y, Shift);
      KeysDown(Shift);
      mouse_event(FMouseUpFlag or MOUSEEVENTF_ABSOLUTE,X,Y,0,0);
      FMouseUpFlag:= 0;
      KeysUp(Shift);
    end;

    if Pos(cmdKeyDown, Command)=1 then
    begin
      GetKeyAndShift(Copy(Command, Length(cmdKeyDown)+1, LComLen-Length(cmdKeyDown)), LKey, Shift);
      KeysDown(Shift);
      keybd_event(LKey,0,0,0);
      KeysUp(Shift);
    end;

    if Pos(cmdKeyUp, Command)=1 then
    begin
      GetKeyAndShift(Copy(Command, Length(cmdKeyUp)+1, LComLen-Length(cmdKeyUp)), LKey, Shift);
      KeysDown(Shift);
      keybd_event(LKey,0,KEYEVENTF_KEYUP,0);
      KeysUp(Shift);
    end;

    if Pos(cmdShellExec, Command)=1 then
    begin
      GetShellCommand(Copy(Command, Length(cmdShellExec)+1, LComLen-Length(cmdShellExec)), ACommand);
      ShellExecute({Handle}0,nil{PChar('open')},PChar(ACommand),nil,nil, SW_SHOWNORMAL);
    end;

    if Pos(cmdClipBoard, Command)=1 then
    begin
      LClb:= TClipBoard.Create;
      try
        LClb.AsText:= Copy(Command, Length(cmdClipBoard)+1, LComLen-Length(cmdClipBoard));
        keybd_event(VK_CONTROL,0,0,0);
        keybd_event(86{V},0,0,0);
        keybd_event(86{V},0,KEYEVENTF_KEYUP,0);
        keybd_event(VK_CONTROL,0,KEYEVENTF_KEYUP,0);
      finally
        FreeAndNil(LClb);
      end;
     {
      LHandle:= GetActiveWindow;
      StrCopy(LText,PChar());
      if LHandle<>0 then
         SendMessage(LHandle,WM_SETTEXT,Integer(@LText),0);
         }
    end;
  end;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  FMouseUpFlag:= 0;
end;

procedure TfrmMain.acActiveExecute(Sender: TObject);
begin
 acActive.Checked:= not acActive.Checked;
 try
   if (Sender as TAction).Checked = true then
   begin
     edIP.Text:= LocalIP;
     Server.Port:= edPort.Value;
     Server.Active:= true;
   end else
   begin
     Server.Active:= false;
     edIP.Text:= '';
   end;
 except
   (Sender as TAction).Checked:= not (Sender as TAction).Checked;
   raise;
 end;
end;

procedure TfrmMain.mnuAboutClick(Sender: TObject);
begin
 AboutBox.ShowModal;
end;

procedure TfrmMain.mnuExitClick(Sender: TObject);
begin
 Server.Active:= false;
 Close;
end;

procedure TfrmMain.acEnableRCExecute(Sender: TObject);
begin
  acEnableRC.Checked:= not acEnableRC.Checked;
end;

end.
