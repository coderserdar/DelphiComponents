unit API_tools;

//------------------------------------------------------------------------------
// different tools (many here for testing purposes before placing into their
// own component or to add existing better suitable component).
//------------------------------------------------------------------------------
// component free to use and modify, subject to following restinctions:
// 1. do not mispresent the origin
// 2. altered revisions must be clearly marked as modified from the original
// 3. do not remove this notice from the source code
// 4. send email about bugs, features needed and features you would like
// * if you like this very much, feel free to donate and support supporting
// and developing the package at www.paypal.com - ari pikivirta@kolumbus.fi
//------------------------------------------------------------------------------
//
//  * added function BrowseURL(const URL: string): boolean;
//
// r1.09, 07022007, ari pikivirta
//  * added ProcessMemoryUsage function
//  * added BatteryLeft function, return -1 if not running on battery
//
// r1.08, 05102006, ari pikivirta
//  * added EthernetAddresses function
//  * added WakeOnLanMessage function to return UDP message to send to client
//    to wake up that specific PC on lan
//  * added NetSend function
//
// r1.07, 27082006, ari pikivirta
//  * added ExitWindows(type=reboot,logoff,shutdown) function
//  * added registerasservice(true/false) function
//
// r1.06, 16082006, ari pikivirta
//  * added check if key exists before reading cpu speed (it's not possible
//    to read from registry on win9x)
//
// r1.05, 25.7.2006, ari pikivirta
// * removed math functions because they exist in api_math
// * removed runonlyonce function because it's as separate component also
// * added HideProcesses and ShowProcess functions
// * added BalloonToolTip function (also to use via using unit only)
//
// r1.04, ari pikivirta
// * added set and get wallpaper function
// r1.03, ari pikivirta
// * added local_ip function
// * added getinetfile function
// r1.02, ari pikivirta
// * added getbuinfo procedure
// r1.01, ari pikivirta
// r1.00, ari pikivirta

interface

uses
  Windows, Messages, SysUtils, Classes, graphics, Controls;

type
  texittype = ( LogOff, ShutDown, Reboot, PowerOff, Hibernate );
  tostype = ( Win95, Win98, WinMe, WinNT, Win2000, WinXP, Win2003, WinVista, WinUnknown );

  TAPI_tools = class(tcomponent)
  private
    fversion: string;
    fcurrent_user: string;
    fcomputername: string;
    fmemload :dword;
    fmemtotalphysical :dword;
    fmemavailphysical :dword;
    fmemtotalpagefile :dword;
    fmemavailpagefile :dword;
    fmemtotalvirtaul :dword;
    fmemavailvirtual :dword;
    fProcessHidden: boolean;
    foldapplicationtitle: string;
    fostype: tostype;
    procedure dummys(s: string);
    procedure dummydw(dw: dword);

  protected
  public
    procedure updatefolderinfo;
    procedure updatememorystatus;

    constructor Create(aowner:tcomponent); override;
    destructor Destroy; override;

    // crypto
    procedure GetBuildInfo(var V1, V2, V3, V4: Word);
    function cryptmsg(textin,keyin:string):string;

    // files and drive
    procedure setbit (var fbyte, bitnr:byte); overload;
    function bitisset (fbyte, bitnr:byte): boolean; overload;
    procedure resetbit (var fbyte, bitnr:byte); overload;
    procedure setbit (var fint:integer; bitnr:byte); overload;
    function  bitisset (fint:integer; bitnr:byte): boolean; overload;
    procedure resetbit (var fint:integer; bitnr:byte); overload;

    // system
    function  OsInfo: string;
    function  IsWin95: boolean;
    function  IsWin98: boolean;
    function  IsWinMe: boolean;
    function  IsWinNT: boolean;
    function  IsWin2000: boolean;
    function  IsWinXP: boolean;
    function  IsWin2003: boolean;
    function  IsWinVista: boolean;

    function  IsRemControlled: Boolean;
    function  IsDebuggerPresent: Boolean;
    function  GetcomputerName: string;
    function  SetComputerName(AComputerName: string): Boolean;
    procedure InstallScreenSaver(const FileName: string);
    function  GetScreenSaverTimeout: Integer;
    function  ScreenSaverEnabled: Boolean;
    function  IsAdmin: Boolean;
    procedure SetWallpaper(sWallpaperBMPPath : String; bTile : boolean);
    function  GetWallpaper: string;
    procedure BalloonToolTip(Control: TWinControl; Icon: integer; Title: pchar; Text: PWideChar; BackCL, TextCL: TColor);
    procedure HideApplication;
    procedure ShowApplication;
    function  CpuUsage:dword;
    function  SetPrivilege(sPrivilegeName: string; bEnabled: boolean ): boolean;
    function  ExitWindows( style: texittype; force: boolean ): boolean;
    function  RegisterAsService(Active: boolean): boolean;
    function  NumberOfProcessors: integer;
    function  ProcessMemoryUsage: integer;
    function  BatteryLeft: double;  // -1 if not running on batteries

    // cpu
    function getcpuspeed: string;
    function getCPUname: string;

    // internet
    procedure EthernetAddresses(List: tstrings);
    function  IsConnectedToInternet: Boolean;
    Function  MailTo(EmailAddress :string):boolean;
    Function  OpenUrl(WebURL :string):boolean;
    function  GetInetFile(Agent, fileURL, FileName: String): boolean;
    function  FindComputers(Workgroup: string; Computers: TStrings): boolean;
    function  GetProxy(var Host: string; var Port: integer; var ProxyEnabled: boolean): boolean;
    function getIPAddress: string;

    function  WakeOnLanMessage(EthernetAddress: string): string;
      (*
        example of above with indy9 udp client component:
        Buffer:= API_tools1.WakeOnLanMessage('thisisethernetaddress');
        UDPClient:= TIdUDPClient.Create(nil);
        try
          UDPClient.BroadcastEnabled:= true;
          UDPClient.Host:= '255.255.255.255';
          UDPClient.Port:= 2050;
          UDPClient.SendBuffer(Buffer, length(buffer)); <<< or length can be set to 116 directly
          writeln('Trying to wake-up remote host: ' + UDPClient.host);
        finally
          UDPClient.Free;
        end;
      *)

    function NetSend(Dest, Msg: string): Longint; overload;
    function NetSend(Msg: string): Longint; overload;
      (*
        both netsend functions return zero if on succesfull send
      *)

    // vcl
    // procedure SetProperties(ClassName, SomeProperty: string; Value: Integer);

  published
    property Version: string read fversion write dummys stored false;
    property CurrentUser: string read fcurrent_user write dummys stored false;
    property ComputerName: string read fcomputername write dummys stored false;
    //property Cpu: string read getcpuname write dummys stored false;
    //property OS: string read OsInfo write dummys stored false;
    property CpuSpeed: string read getcpuspeed write dummys stored false;
    property MemLoad: dword read fmemload write dummydw stored false;
    property MemTotalPhysical: dword read fmemtotalphysical write dummydw stored false;
    property MemAvailPhysical: dword read fmemavailphysical write dummydw stored false;
    property MemTotalPagefile: dword read fmemtotalpagefile write dummydw stored false;
    property MemAvailPagefile: dword read fmemavailpagefile write dummydw stored false;
    property MemTotalVirtual: dword read fmemtotalvirtaul write dummydw stored false;
    property MemAvailVirtual: dword read fmemavailvirtual write dummydw stored false;
    property IPAddress: string read getipaddress write dummys stored false;

  end;

procedure Register;

// exported functions
// -----------------------------------------------------------------------------
function BrowseURL(const URL: string) : boolean;
procedure ShowBalloonToolTip(Control: TWinControl; Icon: integer;               // show balloon tool tip
            Title: pchar; Text: PWideChar; BackCL, TextCL: TColor);
procedure OsBuildInfo(var v1,v2,v3,v4: word);                                   // get operating system
function ConnectedToInternet: boolean;                                         // connected to internet
function GetIfTable(pIfTable: Pointer; var pdwSize: LongInt; bOrder: LongInt ): LongInt; stdcall;
function NetSend(dest, Source, Msg: string): Longint;
procedure GetNetworkAddresses(var IPAddress: string; var SubnetMask: string; var Gateway: string);
// -----------------------------------------------------------------------------

implementation

{$include '..\API_source\inc\CompilerVersions.INC'}
{$R *.RES}
{$WARN UNSAFE_CODE OFF}

uses
  shellapi, registry, inifiles, mmsystem, wininet, shlobj, winsock, WinProcs,
  Commctrl, dialogs, forms, psAPI;

const
  versioninfo = 'r1.09/ari.pikivirta@kolumbus.fi';

     MAX_INTERFACE_NAME_LEN             = $100;
     ERROR_SUCCESS                      = 0;
     MAXLEN_IFDESCR                     = $100;
     MAXLEN_PHYSADDR                    = 8;

     MIB_IF_OPER_STATUS_NON_OPERATIONAL = 0 ;
     MIB_IF_OPER_STATUS_UNREACHABLE     = 1;
     MIB_IF_OPER_STATUS_DISCONNECTED    = 2;
     MIB_IF_OPER_STATUS_CONNECTING      = 3;
     MIB_IF_OPER_STATUS_CONNECTED       = 4;
     MIB_IF_OPER_STATUS_OPERATIONAL     = 5;

     MIB_IF_TYPE_OTHER                  = 1;
     MIB_IF_TYPE_ETHERNET               = 6;
     MIB_IF_TYPE_TOKENRING              = 9;
     MIB_IF_TYPE_FDDI                   = 15;
     MIB_IF_TYPE_PPP                    = 23;
     MIB_IF_TYPE_LOOPBACK               = 24;
     MIB_IF_TYPE_SLIP                   = 28;

     MIB_IF_ADMIN_STATUS_UP             = 1;
     MIB_IF_ADMIN_STATUS_DOWN           = 2;
     MIB_IF_ADMIN_STATUS_TESTING        = 3;

type
 TReg = function (dwProcessID, dwType: DWord): DWord;

 MIB_IFROW = record
  wszName : Array[0 .. (MAX_INTERFACE_NAME_LEN*2-1)] of char;
  dwIndex              : LongInt;
  dwType               : LongInt;
  dwMtu                : LongInt;
  dwSpeed              : LongInt;
  dwPhysAddrLen        : LongInt;
  bPhysAddr : Array[0 .. (MAXLEN_PHYSADDR-1)] of Byte;
  dwAdminStatus        : LongInt;
  dwOperStatus         : LongInt;
  dwLastChange         : LongInt;
  dwInOctets           : LongInt;
  dwInUcastPkts        : LongInt;
  dwInNUcastPkts       : LongInt;
  dwInDiscards         : LongInt;
  dwInErrors           : LongInt;
  dwInUnknownProtos    : LongInt;
  dwOutOctets          : LongInt;
  dwOutUcastPkts       : LongInt;
  dwOutNUcastPkts      : LongInt;
  dwOutDiscards        : LongInt;
  dwOutErrors          : LongInt;
  dwOutQLen            : LongInt;
  dwDescrLen           : LongInt;
  bDescr     : Array[0 .. (MAXLEN_IFDESCR - 1)] of Char;
 end;

var
  prewUT: dword;
  RegisterServiceProcess: TReg;

Function GetIfTable; stdcall; external 'IPHLPAPI.DLL';
procedure TAPI_tools.dummys(s: string); begin end;
procedure TAPI_tools.dummydw(dw: dword); begin end;

//------------------------------------------------------------------------------
function BrowseURL(const URL: string) : boolean;
var
  Browser: string;
begin
  Result := True;
  Browser := '';
  with TRegistry.Create do
  try
    RootKey := HKEY_CLASSES_ROOT;
    Access := KEY_QUERY_VALUE;
    if OpenKey('\htmlfile\shell\open\command', False) then Browser := ReadString('') ;
    CloseKey;
  finally
    Free;
  end;
  if Browser = '' then
  begin
    Result := False;
    Exit;
  end;
  Browser := Copy(Browser, Pos('"', Browser) + 1, Length(Browser)) ;
  Browser := Copy(Browser, 1, Pos('"', Browser) - 1) ;
  ShellExecute(0, 'open', PChar(Browser), PChar(URL), nil, SW_SHOW) ;
end;

//------------------------------------------------------------------------------
(*
procedure TAPI_Tools.SetProperties(ClassName, SomeProperty: string; Value: Integer);
var
  i: integer;
  PropInfo: PPropInfo;
  Component: TComponent;
begin
  for i := 0 to ComponentCount - 1 do
  begin
    Component := Components[i];
    if (Component is TControl) and ((Component.ClassName = ClassName) or (ClassName = 'ALL')) then
    begin
      PropInfo := GetPropInfo(Component.ClassInfo, SomeProperty);
      if Assigned(PropInfo) then SetOrdProp(Component, PropInfo, Integer(Value));
    end;
  end;
end;

// Examples, Beispiele:

procedure TForm1.Button1Click(Sender: TObject);
begin
  // Make all components readonly
  SetProperties('AllComponents', 'readonly', 1);
  // Make all components of Class TEdit invisible
  SetProperties('TEdit', 'visible', 0);
  // Set the Height Property of all TLabels
  SetProperties('TLabel', 'Height', 30);
  // Set ShowHint to false for all components
  SetProperties('AllComponents', 'ShowHint', 0);
  // Disable all TMemo Components
  SetProperties('TMemo', 'Enabled', 0);
  // Set Autosize to true for all TLabels
  SetProperties('TLabel', 'Autosize', 1);
end;
*)

//------------------------------------------------------------------------------
function TAPI_TOOLS.ProcessMemoryUsage: integer;
var
  pmc: PPROCESS_MEMORY_COUNTERS;
  cb: Integer;
  MemStat: tMemoryStatus;
begin
  MemStat.dwLength:= SizeOf(MemStat);
  cb:= SizeOf(TProcessMemoryCounters);
  GetMem(pmc, cb);
  pmc^.cb:= cb;
  if GetProcessMemoryInfo(GetCurrentProcess(), pmc, cb) then
  begin
    // return process memory info
    result:= Longint(pmc^.WorkingSetSize);
  end else
    // failed to get process memory info
    result:= -1;
  FreeMem(pmc);
end;

//------------------------------------------------------------------------------
function TAPI_tools.BatteryLeft: double;
var
  SysPowerStatus: TSystemPowerStatus;
begin
  GetSystemPowerStatus(SysPowerStatus);
  if not Boolean(SysPowerStatus.ACLineStatus) then
  begin
    result:= SysPowerStatus.BatteryLifePercent;
  end else
    result:= -1;
end;

//------------------------------------------------------------------------------
// internal function that is used by the component, this internal function
// can be used outside the component by just adding the api_tools.pas into
// the uses clause of your application
function NetSend(dest, Source, Msg: string): Longint;
type
  TNetMessageBufferSendFunction = function(servername, msgname, fromname: PWideChar; buf: PWideChar; buflen: Cardinal): Longint; stdcall;
var
  NetMessageBufferSend: TNetMessageBufferSendFunction;
  SourceWideChar: PWideChar;
  DestWideChar: PWideChar;
  MessagetextWideChar: PWideChar;
  Handle: THandle;
begin
  Handle := LoadLibrary('NETAPI32.DLL');
  if Handle = 0 then
  begin
    Result := GetLastError;
    Exit;
  end;
  @NetMessageBufferSend := GetProcAddress(Handle, 'NetMessageBufferSend');
  if @NetMessageBufferSend = nil then
  begin
    Result := GetLastError;
    Exit;
  end;
  MessagetextWideChar := nil;
  try
    GetMem(MessagetextWideChar, Length(Msg) * SizeOf(WideChar) + 1);
    GetMem(DestWideChar, 20 * SizeOf(WideChar) + 1);
    StringToWideChar(Msg, MessagetextWideChar, Length(Msg) * SizeOf(WideChar) + 1);
    StringToWideChar(Dest, DestWideChar, 20 * SizeOf(WideChar) + 1);
    if Source = '' then
      Result := NetMessageBufferSend(nil, DestWideChar, nil, MessagetextWideChar, Length(Msg) * SizeOf(WideChar) + 1)
      else
      begin
        GetMem(SourceWideChar, 20 * SizeOf(WideChar) + 1);
        StringToWideChar(Source, SourceWideChar, 20 * SizeOf(WideChar) + 1);
        Result := NetMessageBufferSend(nil, DestWideChar, SourceWideChar, MessagetextWideChar, Length(Msg) * SizeOf(WideChar) + 1);
        FreeMem(SourceWideChar);
      end;
  finally
    FreeMem(MessagetextWideChar);
    FreeLibrary(Handle);
  end;
end;

function TAPI_tools.NetSend(Dest, Msg: string): Longint;
begin
  Result:= api_tools.NetSend(Dest, '', Msg);
end;

function TAPI_Tools.NetSend(Msg: string): Longint;
begin
  Result:= api_tools.NetSend('', '', Msg);
end;

(*
  Example:

  const
    NERR_BASE = 2100;
    NERR_NameNotFound = NERR_BASE + 173;
    NERR_NetworkError = NERR_BASE + 36;
    NERR_Success = 0;

  var
    Res: Longint;
    sMsg: string;

  begin
    Res := NetSend('LoginName', 'Your Message...');
    case Res of
      ERROR_ACCESS_DENIED: sMsg :='user does not have access to the requested information.';
      ERROR_INVALID_PARAMETER: sMsg := 'The specified parameter is invalid.';
      ERROR_NOT_SUPPORTED: sMsg := 'This network request is not supported.';
      NERR_NameNotFound: sMsg := 'The user name could not be found.';
      NERR_NetworkError: sMsg := 'A general failure occurred in the network hardware.';
      NERR_Success: sMsg := 'Message sent!';
    end;
    ShowMessage(sMsg);
  end;
*)

//------------------------------------------------------------------------------
procedure TAPI_tools.EthernetAddresses(List: tstrings);
const
  _MAX_ROWS_ = 20;
type
  _IfTable = Record
    nRows : LongInt;
    ifRow : Array[1.._MAX_ROWS_] of MIB_IFROW;
  end;
var
  pIfTable: ^_IfTable;
  TableSize: longint;
  tmp: string;
  i,j: integer;
  ErrCode: longint;
begin
  pIfTable:= nil;
  list.clear;
  try
    TableSize:=0;
    GetIfTable(pIfTable, TableSize, 1);
    if (TableSize < SizeOf(MIB_IFROW)+Sizeof(LongInt)) then exit;
    GetMem(pIfTable, TableSize);
    ErrCode:= GetIfTable(pIfTable, TableSize, 1);
    if ErrCode<>ERROR_SUCCESS then exit;
    for i := 1 to pIfTable^.nRows do
    try
      if pIfTable^.ifRow[i].dwType=MIB_IF_TYPE_ETHERNET then
      begin
        tmp:='';
        for j:=0 to pIfTable^.ifRow[i].dwPhysAddrLen-1 do
          tmp:= tmp+format('%.2x',[pIfTable^.ifRow[i].bPhysAddr[j]]);
        if Length(tmp)>0 then list.Add(tmp);
      end;
    except
      exit;
    end;
  finally
    if Assigned(pIfTable) then FreeMem(pIfTable,TableSize);
  end;
end;

//------------------------------------------------------------------------------
procedure GetNetworkAddresses(var IPAddress: string; var SubnetMask: string; var Gateway: string);
const
  nString = 'SOFTWARE\MICROSOFT\WINDOWS NT\CURRENTVERSION\NetworkCards';
  nEthernet = 'Ethernet';
  nEtherjet = 'Etherjet'; {if Hardware IBM PL300 with Chip 10/100}
  nTcpIp1 = 'SYSTEM\CurrentControlSet\Services\';
  nTcpIp2 = '\Parameters\Tcpip';
var
  reg: tregistry;
  stringlist: tstringlist;
  adapter, adapter_key: string;
  buffer1, buffer2, buffer3: array [1..32] of Char;  

  // search adapter from registry
  function search_adapter_key: string;
  var
    astring, description, st: string;
    nPos, i: integer;
    ServiceName: string;
  begin
    reg := TRegistry.Create;
    stringlist  := TStringList.Create;
    reg.RootKey := HKEY_LOCAL_MACHINE;
    reg.OpenKey(nString, False);
    reg.GetKeyNames(stringlist);
    reg.CloseKey;
    for i:= 0 to (stringlist.Count - 1) do
    begin
      st := stringlist[i];
      aString := nString + '\' + st;
      reg := TRegistry.Create;
      reg.RootKey := HKEY_LOCAL_MACHINE;
      reg.OpenKey(aString, False);
      description := reg.ReadString('Description');
      nPos:= AnsiPos(nEthernet, description); {search description for string Ethernet}
      if nPos > 0 then
      begin
        ServiceName := reg.ReadString('ServiceName');
        Adapter_Key:= nTcpIp1 + ServiceName + nTcpIp2;
      end;
      nPos := AnsiPos(nEtherjet, description); {search description for string 'Etherjet if IBM PL300 with MotherboardChip}
      if nPos > 0 then
      begin
        ServiceName := reg.ReadString('ServiceName');
        Adapter_Key := nTcpIp1 + ServiceName + nTcpIp2;
      end;
      reg.CloseKey;
    end;
    Result := Adapter_Key;
  end;

  // get adapter registry items
  procedure search_for_adapter_NT;
  var
    BufSize: Integer;    {Bufsize requested but not used}
  begin
    bufsize:= 0;
    adapter:= search_adapter_key;
    if adapter <> '' then
    begin
      Reg:= TRegistry.Create;
      try
        Reg.RootKey := HKEY_LOCAL_MACHINE;
        if Reg.OpenKey(adapter, False) then
        begin
          Reg.ReadBinaryData('IpAddress', buffer1, BufSize);
          Reg.CloseKey;
        end;
        if Reg.OpenKey(adapter, False) then
        begin
          Reg.ReadBinaryData('SubnetMask', buffer2, BufSize);
          Reg.CloseKey;
        end;
        if Reg.OpenKey(adapter, False) then
        begin
          Reg.ReadBinaryData('DefaultGateway', buffer3, BufSize);
          Reg.CloseKey;
        end;
      finally
        Reg.Free;
      end;
    end;
  end;

// main
begin
  search_for_adapter_NT;
  IPAddress:= buffer1;
  SubnetMask:= buffer2;
  Gateway:= buffer3;
end;

//------------------------------------------------------------------------------
function TAPI_tools.WakeOnLanMessage(EthernetAddress: string): string;

  (*
    following directly from copy-pasted from the internet.. by browsing this
    subject - however - it just might be easier to rebuild this and use
    normal string routines instead :) ;) - i don't care, origral author has
    done this way - and what'a hell - who really cares while it works
  *)
  Function HexToInt(S:String): LongInt;
  const
    HexVals: Array [0..$F] Of Integer = (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, $A, $B, $C, $D, $E, $F);
    {$ifdef DELPHI2009UP}
    DecDigits: TSysCharSet = ['0'..'9'];
    UpCaseHexLetters: TSysCharSet = ['A'..'F'];
    LowCaseHexLetters: TSysCharSet = ['a'..'f'];
    {$else}
    DecDigits: Set Of '0'..'9' = ['0'..'9'];
    UpCaseHexLetters: Set Of 'A'..'F' = ['A'..'F'];
    LowCaseHexLetters: Set Of 'a'..'f' = ['a'..'f'];
    {$endif}
  var
    v: LongInt;
    i: integer;
    LookUpIndex: integer;
  begin
    if length(S) <= 8 then
    begin
      v := 0;
      for i := 1 to length(S) do
      begin
        {$R-}
        v := v Shl 4;
        {$R+}
        {$ifdef DELPHI2009UP}
        if (charinset(S[i], DecDigits)) then
        {$else}
        if S[i] in DecDigits then
        {$endif}
        begin
          LookUpIndex := Ord(S[i]) - Ord('0');
        end else
        begin
          {$ifdef DELPHI2009UP}
          if (charinset(S[i], UpCaseHexLetters)) then
          {$else}
          if S[i] in UpCaseHexLetters then
          {$endif}
          begin
            LookUpIndex := Ord(S[i]) - Ord('A') + $A;
          end else
          begin
            {$ifdef DELPHI2009UP}
            if (charinset(S[i], LowCaseHexLetters)) then
            {$else}
            if S[i] in LowCaseHexLetters then
            {$endif}
            begin
              LookUpIndex := Ord(S[i]) - Ord('a') + $A;
            end else
            begin
              LookUpIndex := 0;
            end;
          end;
        end;
        v := v Or HexVals[LookUpIndex];
      end;
      result := v;
    end else
    begin
      result := 0;
    end;
  end;

var
  i, j: Byte;
  lBuffer: array[1..116] of Byte;
begin
  for i := 1 to 6 do
  begin
    lBuffer[i] := HexToInt(EthernetAddress[(i * 2) - 1] + EthernetAddress[i * 2]);
  end;
  lBuffer[7] := $00;
  lBuffer[8] := $74;
  lBuffer[9] := $FF;
  lBuffer[10] := $FF;
  lBuffer[11] := $FF;
  lBuffer[12] := $FF;
  lBuffer[13] := $FF;
  lBuffer[14] := $FF;
  for j := 1 to 16 do
  begin
    for i := 1 to 6 do
    begin
      lBuffer[15 + (j - 1) * 6 + (i - 1)] := lBuffer[i];
    end;
  end;
  lBuffer[116] := $00;
  lBuffer[115] := $40;
  lBuffer[114] := $90;
  lBuffer[113] := $90;
  lBuffer[112] := $00;
  lBuffer[111] := $40;

  // buffer filled.. now we just throw whole shit
  // into string that can be as well sent out as
  // a buffer using indy for example (udp message).
  result:= '';
  for i:=1 to 116 do
    result:= result + char(lbuffer[i]);
end;

//------------------------------------------------------------------------------
function TAPI_tools.RegisterAsService(Active: boolean): boolean;
  const
    RSP_SIMPLE_SERVICE = 1;
    RSP_UNREGISTER_SERVICE = 0;
  type
    TRegisterServiceProcessFunction =
      function (dwProcessID, dwType: Integer): Integer; stdcall;
  var
    module: HMODULE;
    RegServProc: TRegisterServiceProcessFunction;
  begin
    Result := False;
    module := LoadLibrary('KERNEL32.DLL');
    if module <> 0 then
      try
        RegServProc := GetProcAddress(module, 'RegisterServiceProcess');
        if Assigned(RegServProc) then
          if Active then
            Result := RegServProc(0, RSP_SIMPLE_SERVICE) = 1
          else
            Result := RegServProc(0, RSP_UNREGISTER_SERVICE) = 1;
      finally
        FreeLibrary(module);
      end;
  end;

//------------------------------------------------------------------------------
function TAPI_tools.SetPrivilege(sPrivilegeName: string; bEnabled: boolean ): boolean;
var
  TPPrev, TP: TTokenPrivileges;
  Token: THandle;
  dwRetLen: DWord;
begin
  Result:= False;
  windows.OpenProcessToken(GetCurrentProcess, TOKEN_ADJUST_PRIVILEGES or TOKEN_QUERY, Token);
  try
    TP.PrivilegeCount:= 1;
    if (windows.LookupPrivilegeValue(Nil, PChar(sPrivilegeName), TP.Privileges[0].LUID))then
    begin
      if (bEnabled) then
        TP.Privileges[0].Attributes:= SE_PRIVILEGE_ENABLED
        else TP.Privileges[0].Attributes:= 0;
      dwRetLen:= 0;
      Result:= windows.AdjustTokenPrivileges(Token, False, TP, SizeOf(TPPrev), TPPrev, dwRetLen);
    end;
  finally
    windows.CloseHandle(Token);
  end;
end;

//------------------------------------------------------------------------------
function TAPI_tools.ExitWindows( style: texittype; force: boolean ): boolean;
var
  flags: cardinal;
begin
  Result:= True;

  // do required action
  flags:= 0;
  case style of
    ShutDown:
      begin
        Flags:= flags or EWX_SHUTDOWN;
        flags:= flags or EWX_FORCEIFHUNG;
      end;
    Reboot:
      Flags:= flags or EWX_REBOOT;
    LogOff:
      Flags:= flags or EWX_LOGOFF;
    PowerOff:
      Flags:= Flags or EWX_POWEROFF;
    Hibernate:
      SetSystemPowerState(False,False);
  end;

  if (force) and (flags>0) then
    flags:= flags or EWX_FORCE;

  // set privileges
  if Win32Platform=VER_PLATFORM_WIN32_NT then
    if not (SetPrivilege('SeShutdownPrivilege', True)) then
      exit;

  // try exitwindowsex
  windows.ExitWindowsEx(flags, 0);

  // disable shutdown again
  //SetPrivilege( 'SeShutdownPrivilege', False )
end;

//------------------------------------------------------------------------------
function TAPI_tools.GetProxy(var Host: string; var Port: integer; var ProxyEnabled: boolean): boolean;
var
  s: string;
  p: integer;
  reg: tregistry;
  temp: string;
begin
  reg:= tregistry.Create;
  try
    reg.RootKey:= HKEY_CURRENT_USER;
    ProxyEnabled:= false;
    s:= '';
    reg.OpenKey('\Software\Microsoft\Windows\CurrentVersion\Internet Settings', True);
    if reg.ValueExists('ProxyServer') then s:= reg.ReadString('ProxyServer');
    if s<>'' then
    begin
      p:= pos(':', s);
      if p=0 then p:= length(s)+1;
      Host:= copy(s, 1, p-1);
      try
        Port:= StrToInt(copy (s,p+1,999));
      except
        Port:= 80;
      end;
      ProxyEnabled:= true;
    end;
    if reg.ValueExists('ProxyEnable') then
    begin
        case reg.GetDataType('ProxyEnable') of
          rdString, rdExpandString:
            begin
              temp:= AnsiLowerCase(reg.ReadString('ProxyEnable'));
              ProxyEnabled := true;
              if pos(' '+temp+' ', ' yes true t enabled 1 ')>0 then ProxyEnabled:= true
                else if pos(' '+temp+' ', ' no false f none disabled 0 ')>0 then ProxyEnabled:= false;
            end;
          rdInteger:
            ProxyEnabled:= reg.ReadBool('ProxyEnable');
          rdBinary:
            begin
              ProxyEnabled := true;
              reg.ReadBinaryData('ProxyEnable', ProxyEnabled, 1);
            end;
        end;
    end;
  finally
    reg.Free;
  end;
  Result:= s<>'';
end;

//------------------------------------------------------------------------------
// added 25.7.2006, ari pikivirta
procedure TAPI_tools.HideApplication;
var
  handle: thandle;
begin
  if not fprocesshidden then
  begin
    // hide applications title
    foldapplicationtitle:= application.title;
    application.title:= '';

    // hide whole form
    ShowWindow(Application.Handle, SW_HIDE);
//    SetWindowLong(Application.Handle, GWL_EXSTYLE, GetWindowLong(Application.Handle, GWL_EXSTYLE) or WS_EX_TOOLWINDOW );

    // for window 9x
    // register as service
    handle:= LoadLibrary('KERNEL32.DLL');
    if handle<>0 then
    try
      @RegisterServiceProcess := GetProcAddress(handle, 'RegisterServiceProcess');
      if @RegisterServiceProcess<>nil then
      begin
        RegisterServiceProcess(GetCurrentProcessID, 1);
      end;
    finally
      freelibrary(handle);
    end;

    fprocesshidden:= true;
  end;
end;

procedure TAPI_tools.ShowApplication;
var
  handle: thandle;
begin
  if fprocesshidden then
  begin
    // return application's title
    application.title:= foldapplicationtitle;

    // show form again
    ShowWindow(Application.Handle, SW_SHOW);

    // for window 9x
    // un-register as service
    handle:= LoadLibrary('KERNEL32.DLL');
    if handle<>0 then
    try
      @RegisterServiceProcess := GetProcAddress(handle, 'RegisterServiceProcess');
      if @RegisterServiceProcess<>nil then
      begin
        RegisterServiceProcess(GetCurrentProcessID, 0);
      end;
    finally
      freelibrary(handle);
    end;

    fprocesshidden:= false;
  end;
end;

//------------------------------------------------------------------------------
// added 25.7.2006, ari pikivirta
// usage ShowBalloonToolTip(button1,1,'Note!','This is a tooltip.',clnavy,clwhite);
procedure ShowBalloonToolTip(Control: TWinControl; Icon: integer; Title: pchar; Text: PWideChar; BackCL, TextCL: TColor);
const
  TOOLTIPS_CLASS = 'tooltips_class32';
  TTS_ALWAYSTIP = $01;
  TTS_NOPREFIX = $02;
  TTS_BALLOON = $40;
  TTF_SUBCLASS = $0010;
  TTF_TRANSPARENT = $0100;
  TTF_CENTERTIP = $0002;
  TTM_ADDTOOL = $0400 + 50;
  TTM_SETTITLE = (WM_USER + 32);
  ICC_WIN95_CLASSES = $000000FF;
type
  TOOLINFO = packed record
    cbSize: Integer;
    uFlags: Integer;
    hwnd: THandle;
    uId: Integer;
    rect: TRect;
    hinst: THandle;
    lpszText: PWideChar;
    lParam: Integer;
  end;
var
  hWndTip: THandle;
  ti: TOOLINFO;
  hWnd: THandle;
begin
  hWnd:= control.Handle;
  hWndTip := CreateWindow(TOOLTIPS_CLASS, nil, WS_POPUP or TTS_NOPREFIX or TTS_BALLOON or TTS_ALWAYSTIP, 0, 0, 0, 0, hWnd, 0, HInstance, nil);
  if hWndTip <> 0 then
  begin
    SetWindowPos(hWndTip, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOACTIVATE or SWP_NOMOVE or SWP_NOSIZE);
    ti.cbSize := SizeOf(ti);
    ti.uFlags := TTF_CENTERTIP or TTF_TRANSPARENT or TTF_SUBCLASS;
    ti.hwnd := hWnd;
    ti.lpszText := Text;
    Windows.GetClientRect(hWnd, ti.rect);
    SendMessage(hWndTip, TTM_SETTIPBKCOLOR, BackCL, 0);
    SendMessage(hWndTip, TTM_SETTIPTEXTCOLOR, TextCL, 0);
    SendMessage(hWndTip, TTM_ADDTOOL, 1, Integer(@ti));
    SendMessage(hWndTip, TTM_SETTITLE, Icon mod 4, Integer(Title));
  end;
end;

procedure TAPI_tools.BalloonToolTip(Control: TWinControl; Icon: integer; Title: pchar; Text: PWideChar; BackCL, TextCL: TColor);
begin
  showBalloonToolTip(control, icon, title, text, backcl, textcl);
end;

//------------------------------------------------------------------------------
procedure TAPI_tools.SetWallpaper(sWallpaperBMPPath : String; bTile : boolean);
var
  reg : TRegIniFile;
begin
  reg := TRegIniFile.Create('Control PanelDesktop');
  try
    with reg do
    begin
      WriteString('', 'Wallpaper', sWallpaperBMPPath);
      if( bTile )then
        WriteString('', 'TileWallpaper', '1')
        else  WriteString('', 'TileWallpaper', '0');
    end;
  finally
    reg.Free;
  end;
  SystemParametersInfo(SPI_SETDESKWALLPAPER, 0, Nil, SPIF_SENDWININICHANGE);
end;

//------------------------------------------------------------------------------
function TAPI_tools.GetWallpaper: string;
var
  reg : TRegIniFile;
begin
  reg := TRegIniFile.Create('Control PanelDesktop');
  try
    with reg do
    begin
      ReadString('', 'Wallpaper', result);
    end;
  finally
    reg.Free;
  end;
end;

//------------------------------------------------------------------------------
procedure OsBuildInfo(var v1,v2,v3,v4: word);
var
   VerInfoSize,
   VerValueSize,
   Dummy       : DWORD;
   VerInfo	   : Pointer;
   VerValue	   : PVSFixedFileInfo;
begin
	 VerInfoSize := GetFileVersionInfoSize(PChar(ParamStr(0)), Dummy);
	 GetMem(VerInfo, VerInfoSize);
	 GetFileVersionInfo(PChar(ParamStr(0)), 0, VerInfoSize, VerInfo);
	 VerQueryValue(VerInfo, '\', Pointer(VerValue), VerValueSize);
	 With VerValue^ do
	 begin
	 	V1 := dwFileVersionMS shr 16;
		V2 := dwFileVersionMS and $FFFF;
		V3 := dwFileVersionLS shr 16;
		V4 := dwFileVersionLS and $FFFF;
	end;
	FreeMem(VerInfo, VerInfoSize);
end;

procedure TAPI_tools.GetBuildInfo(var V1, V2, V3, V4: Word);
begin
  OsBuildInfo(v1,v2,v3,v4);
end;

//------------------------------------------------------------------------------
function tAPI_tools.CPUUsage:dword;
var
  CreationT, ExitT, KernelT, UserT: TFileTime;
begin
  GetProcessTimes(GetCurrentProcess, CreationT, ExitT, KernelT, UserT);
  Result := (UserT.dwLowDateTime - PrewUT) div 10000;
  PrewUT := UserT.dwLowDateTime;
end;

//------------------------------------------------------------------------------
function TAPI_tools.getIPAddress : string;
var
  wsaData: TWSAData;
  addr: TSockAddrIn;
  Phe: PHostEnt;
  szHostName: pansichar;
begin
  Result := '';
  if WSAStartup($101, WSAData) <> 0 then Exit;
  try
    SzHostname:= '';
    if GetHostName(szHostName, 128) <> SOCKET_ERROR then
    begin
      Phe := GetHostByName(szHostName);
      if Assigned(Phe) then
      begin
        addr.sin_addr.S_addr := longint(plongint(Phe^.h_addr_list^)^);
        Result := string(inet_ntoa(addr.sin_addr));
      end;
    end;
  finally
    WSACleanup;
  end;
end;

//------------------------------------------------------------------------------
procedure tAPI_tools.updatememorystatus;
var
   MemoryStatus : TMemoryStatus;
begin
   with MemoryStatus do
   begin
      dwLength := SizeOf( TMemoryStatus );
      Windows.GlobalMemoryStatus( MemoryStatus );
      fmemload := dwMemoryLoad;
      fmemtotalphysical := dwTotalPhys     div 1024;
      fmemavailphysical := dwAvailPhys     div 1024;
      fmemtotalpagefile := dwTotalPageFile div 1024;
      fmemavailpagefile := dwAvailPageFile div 1024;
      fmemtotalvirtaul := dwTotalVirtual  div 1024;
      fmemavailvirtual := dwAvailVirtual  div 1024;
   end; // with MemoryStatus
end;

//------------------------------------------------------------------------------
function TAPI_tools.FindComputers(Workgroup: string; Computers: TStrings): boolean;
Var
  EnumHandle: THandle;
  WorkgroupRS: TNetResource;
  Buf: Array[1..500] of TNetResource;
  BufSize: cardinal;
  Entries: cardinal;
  Res: Integer;
begin
  if not assigned(computers) then
  begin
    result:= false;
    exit;
  end;
  Workgroup:= Workgroup + #0;

  FillChar(WorkgroupRS, SizeOf(WorkgroupRS) , 0);
  With WorkgroupRS do
  begin
    dwScope:= 2;
    dwType:= 3;
    dwDisplayType:= 1;
    dwUsage:= 2;
    lpRemoteName:= @Workgroup[1];
  end;

  WNetOpenEnum( RESOURCE_GLOBALNET,
                RESOURCETYPE_ANY,
                0,
                @WorkgroupRS,
                EnumHandle );

  computers.clear;
  Repeat
    Entries:= 1;
    BufSize:= SizeOf(Buf);
    Res:= WNetEnumResource( EnumHandle, Entries, @Buf, BufSize );
    if (Res = NO_ERROR) and (Entries = 1) then
      computers.add(StrPas(Buf[1].lpRemoteName));
  Until (Entries<>1) or (Res<>NO_ERROR);
  WNetCloseEnum( EnumHandle );

  result:= true;
end;

//------------------------------------------------------------------------------
function tAPI_tools.isadmin: Boolean;
const
  SECURITY_NT_AUTHORITY: TSIDIdentifierAuthority = (Value: (0, 0, 0, 0, 0, 5));
  SECURITY_BUILTIN_DOMAIN_RID = $00000020;
  DOMAIN_ALIAS_RID_ADMINS = $00000220;
var
  hAccessToken: THandle;
  ptgGroups: PTokenGroups;
  dwInfoBufferSize: DWORD;
  psidAdministrators: PSID;
  x: Integer;
  bSuccess: BOOL;
begin
  Result := False;
  bSuccess := OpenThreadToken(GetCurrentThread, TOKEN_QUERY, True, hAccessToken);
  if not bSuccess then
    if GetLastError = ERROR_NO_TOKEN then
      bSuccess := OpenProcessToken(GetCurrentProcess, TOKEN_QUERY, hAccessToken);
  if bSuccess then
  begin
    GetMem(ptgGroups, 1024);
    try
      bSuccess := GetTokenInformation(hAccessToken, TokenGroups, ptgGroups, 1024, dwInfoBufferSize);
      CloseHandle(hAccessToken);
      if bSuccess then
      begin
        AllocateAndInitializeSid(SECURITY_NT_AUTHORITY, 2, SECURITY_BUILTIN_DOMAIN_RID, DOMAIN_ALIAS_RID_ADMINS, 0, 0, 0, 0, 0, 0, psidAdministrators);
        try
          for x := 0 to ptgGroups.GroupCount - 1 do
          if EqualSid(psidAdministrators, ptgGroups.Groups[x].Sid) then
          begin
            Result := True;
            Break;
          end;
        finally
          FreeSid(psidAdministrators);
        end;
      end;
    finally
      FreeMem(ptgGroups);
    end;
  end;
end;

//------------------------------------------------------------------------------
function ConnectedToInternet: boolean;
var
  dwConnectionTypes: DWORD;
begin
  dwConnectionTypes := INTERNET_CONNECTION_MODEM + INTERNET_CONNECTION_LAN + INTERNET_CONNECTION_PROXY;
  Result := InternetGetConnectedState(@dwConnectionTypes, 0);
end;

function tAPI_tools.isconnectedtointernet: Boolean;
begin
  Result:= connectedtointernet;
end;

//------------------------------------------------------------------------------
procedure tAPI_tools.InstallScreenSaver(const FileName: string);
begin
  ShellExecute(0, 'open', PChar('rundll32.exe'),
    PChar('desk.cpl,InstallScreenSaver ' + FileName), nil, SW_SHOWNORMAL);
end;

//------------------------------------------------------------------------------
function tAPI_tools.GetScreenSaverTimeout: Integer;
begin
  SystemParametersInfo(SPI_GETSCREENSAVETIMEOUT, 0, @Result, 0);
end;

//------------------------------------------------------------------------------
function tAPI_tools.ScreenSaverEnabled: Boolean;
var
  status: Bool;
begin
  SystemParametersInfo(SPI_GETSCREENSAVEACTIVE, 0, @status, 0);
  Result := status = True;
end;

//------------------------------------------------------------------------------
function tAPI_tools.getCPUname: string;
var
  Reg: TRegistry;
begin
  result := '';
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    if Reg.OpenKey('\Hardware\Description\System\CentralProcessor\0', False) then
      result:= Reg.ReadString('Identifier');
  finally
    Reg.Free;
  end;
end;

//------------------------------------------------------------------------------
function tAPI_tools.getcpuspeed: string;
var
  Reg: TRegistry;
begin
  result:= '';
  Reg := TRegistry.Create;
  try
    Reg.RootKey:= HKEY_LOCAL_MACHINE;
    if reg.KeyExists('Hardware\Description\System\CentralProcessor\0') then
      if Reg.OpenKey('Hardware\Description\System\CentralProcessor\0', False) then
      try
        if reg.ValueExists('~MHz') then
          Result := IntToStr(Reg.ReadInteger('~MHz')) + ' MHz';
      finally
        Reg.CloseKey;
      end;
  finally
    Reg.Free;
  end;
end;

//------------------------------------------------------------------------------
function tAPI_tools.getcomputername: string;
var
  buffer: array[0..MAX_COMPUTERNAME_LENGTH + 1] of Char;
  Size: Cardinal;
begin
  Size := MAX_COMPUTERNAME_LENGTH + 1;
  Windows.GetComputerName(@buffer, Size);
  Result := StrPas(buffer);
end;

//------------------------------------------------------------------------------
function tAPI_tools.setcomputername(AComputerName: string): Boolean;
var
  ComputerName: array[0..MAX_COMPUTERNAME_LENGTH + 1] of Char;
begin
  StrPCopy(ComputerName, AComputerName);
  Result := Windows.SetComputerName(ComputerName);
end;

//------------------------------------------------------------------------------
Function tAPI_tools.mailto(EmailAddress :string):boolean;
var
 sCommand:string;
begin
  result:=false;
  if EmailAddress = '' then exit;
  sCommand := 'mailto:'+ EmailAddress;
  Result := shellexecute(0,nil,pchar(sCommand),nil,nil,sw_restore) >= 33;
end;

//------------------------------------------------------------------------------
Function tAPI_tools.openurl(WebURL :string):boolean;
var
 sCommand:string;
begin
  result:=false;
  if WebURL = '' then exit;
  sCommand := 'http://'+ WebURL;
  Result := shellexecute(0,nil,pchar(sCommand),nil,nil,sw_restore) >= 33;
end;

//------------------------------------------------------------------------------
function TAPI_tools.OsInfo: string;
var
  Platform, OS_Caption: string;
  BuildNumber: Integer;
begin
  fostype:= WinUnknown;
  Platform:= 'Unknown';
  Buildnumber:= 0;

  case Win32Platform of

    VER_PLATFORM_WIN32s:
      begin
        BuildNumber := Win32BuildNumber and $0000FFFF;
        Platform := 'Windows (32bit)';
      end;

    VER_PLATFORM_WIN32_WINDOWS:
      begin
        case Win32MinorVersion of
          0:
            begin
              Platform:= 'Windows 95';
              fostype:= Win95;
            end;
          10:
            begin
              Platform:= 'Windows 98';
              fostype:= Win98;
            end;
          90:
            begin
              Platform:= 'Windows Me';
              fostype:= WinMe;
            end
          else
            Platform := 'Windows 9x';
        end;
        BuildNumber := Win32BuildNumber and $0000FFFF;
      end;

    VER_PLATFORM_WIN32_NT:
      begin
        case Win32MajorVersion of
          3, 4:
            begin
              case Win32MinorVersion of
                0:
                  begin
                    Platform:= 'Windows NT';
                    fostype:= WinNT;
                  end;
              end;
            end;
          5:
            begin
              // Windows 2000 or XP
              case Win32MinorVersion of
                0:
                  begin
                    Platform:= 'Windows 2000';
                    fostype:= Win2000;
                  end;
                1:
                  begin
                    Platform:= 'Windows XP';
                    fostype:= WinXP;
                  end;
                2:
                  begin
                    Platform:= 'Windows 2003';
                    fostype:= Win2003;
                  end;
              end;
            end;
          6:
            begin
              // Windows Vista
              case Win32MinorVersion of
                0:
                  begin
                    Platform:= 'Windows Vista';
                    fostype:= WinVista;
                  end;
              end;
            end;
        end;
      end;

  end;

  if (Win32Platform = VER_PLATFORM_WIN32_WINDOWS) or
    (Win32Platform = VER_PLATFORM_WIN32_NT) then
  begin
    if Win32CSDVersion = '' then
      OS_Caption := Format('%s %d.%d (Build %d)', [Platform, Win32MajorVersion,
        Win32MinorVersion, BuildNumber])
    else
      OS_Caption := Format('%s %d.%d (Build %d: %s)', [Platform, Win32MajorVersion,
        Win32MinorVersion, BuildNumber, Win32CSDVersion]);
  end else
    OS_Caption := Format('%s %d.%d', [Platform, Win32MajorVersion,
      Win32MinorVersion]);

   result:= OS_caption;
end;

//------------------------------------------------------------------------------
function TAPI_tools.IsWin95: boolean;
begin
  result:= fostype = win95;
end;

function TAPI_tools.IsWin98: boolean;
begin
  result:= fostype = win98;
end;

function TAPI_tools.IsWinMe: boolean;
begin
  result:= fostype = winme;
end;

function TAPI_tools.IsWinNT: boolean;
begin
  result:= fostype = winnt;
end;

function TAPI_tools.IsWin2000: boolean;
begin
  result:= fostype = win2000;
end;

function TAPI_tools.IsWinXP: boolean;
begin
  result:= fostype = winxp;
end;

function TAPI_tools.IsWin2003: boolean;
begin
  result:= fostype = win2003;
end;

function TAPI_tools.IsWinVista: boolean;
begin
  result:= fostype = winvista;
end;

//------------------------------------------------------------------------------
function tAPI_tools.isremcontrolled: Boolean;
const
  SM_REMOTECONTROL = $2001; // from WinUser.h
begin
  Result := Boolean(GetSystemMetrics(SM_REMOTECONTROL));
end;

//------------------------------------------------------------------------------
function tAPI_tools.isdebuggerpresent: Boolean;
type
  TDebugProc = function: Boolean; //stdcal;
var
  kernel32 : HMODULE;
  debugproc: TDebugProc;
begin
  result := False;
  kernel32 := getmodulehandle('kernel32');
  if kernel32 <> 0 then
  begin
    @debugproc := getprocaddress(kernel32, 'IsDebuggerPresent');
    if assigned(debugproc) then result := debugproc;
  end;
end;

//------------------------------------------------------------------------------
function SendMCICommand(Cmd: string): string;
var
  RetVal: Integer;
  ErrMsg: array[0..254] of char;
begin
  result:='';
  RetVal := mciSendString(PChar(Cmd), nil, 0, 0);
  if RetVal <> 0 then
  begin
    mciGetErrorString(RetVal, ErrMsg, 255);
    result:=StrPas(ErrMsg);
  end;
end;

//------------------------------------------------------------------------------
procedure tAPI_tools.setbit (var fbyte, bitnr:byte);
begin
  fbyte:=fbyte or (1 shl Bitnr);
end;

//------------------------------------------------------------------------------
function tAPI_tools.bitisset (fbyte, bitnr:byte): boolean;
begin
  result:=(fbyte and (1 shl Bitnr))<>0;
end;

//------------------------------------------------------------------------------
procedure tAPI_tools.resetbit (var fbyte, bitnr:byte);
begin
  fbyte:=fbyte and ($FF xor (1 shl Bitnr));
end;

//------------------------------------------------------------------------------
procedure tAPI_tools.setbit (var fint:integer; bitnr:byte);
begin
  fint:=fint or (1 shl Bitnr);
end;

//------------------------------------------------------------------------------
function tAPI_tools.bitisset (fint:integer; bitnr:byte): boolean;
begin
  result:=(fint and (1 shl Bitnr))<>0;
end;

//------------------------------------------------------------------------------
procedure tAPI_tools.resetbit (var fint:integer; bitnr:byte);
begin
  fint:=fint and ($FFFF xor (1 shl Bitnr));
end;

//------------------------------------------------------------------------------
function tAPI_tools.cryptmsg(textin,keyin:string):string;
var
  I,I2,Val,KeyLen: longint;
  DirRight: boolean;
begin
  result:='';
  DirRight:=true;
  KeyLen := length(KeyIn);
  I2 := 1;
  For I := 1 to length(TextIn) do
  Begin
    if Dirright then
    begin
      Inc(I2,1);
      If (I2>KeyLen) then Dirright:=false;
    End else
    begin
      Dec(I2,1);
      if (I2<1) then Dirright:=true;
    end;
    Val := (ord(TextIn[I]) XOR ord(KeyIn[I2]));
    Result := Result + char(Val);
  End;
End;

//------------------------------------------------------------------------------
function getcurrentusername(var CurrentUserName: string): Boolean;
var
  BufferSize: DWORD;
  pUser: PChar;
begin
  BufferSize := 0;
  GetUserName(nil, BufferSize);
  pUser := StrAlloc(BufferSize);
  try
    Result := GetUserName(pUser, BufferSize);
    CurrentUserName := StrPas(pUser);
  finally
    StrDispose(pUser);
  end;
end;

//------------------------------------------------------------------------------
constructor tAPI_tools.create(aowner:tcomponent);
begin
  inherited create(aowner);
  fversion:= versioninfo;
  fprocesshidden:= false;               // process is shown in task manager as default
  //updatefolderinfo;
  //updatememorystatus;
  //osinfo;                               // just update operating system info
end;

//------------------------------------------------------------------------------
destructor tAPI_tools.destroy;
begin
  if fprocesshidden then ShowApplication;   // show process if hidden
  inherited destroy;
end;

//------------------------------------------------------------------------------
procedure tAPI_tools.updatefolderinfo;
begin
  getcurrentusername(fcurrent_user);
  fcomputername:=getcomputername;
end;

//------------------------------------------------------------------------------
function TAPI_tools.GetInetFile(Agent, fileURL, FileName: String): boolean;
const BufferSize = 1024;
var
  hSession, hURL: HInternet;
  Buffer: array[1..BufferSize] of Byte;
  BufferLen: DWORD;
  f: File;
begin
  hSession := InternetOpen(PChar(Agent), INTERNET_OPEN_TYPE_PRECONFIG, nil, nil, 0);
  try
    hURL := InternetOpenURL(hSession, PChar(fileURL), nil,0,0,0);
    try
      AssignFile(f, FileName);
      Rewrite(f,1);
      repeat
        InternetReadFile(hURL, @Buffer, SizeOf(Buffer), BufferLen);
        BlockWrite(f, Buffer, BufferLen)
      until BufferLen = 0;
      CloseFile(f);
      Result:=True;
    finally
      InternetCloseHandle(hURL)
    end
  finally
    InternetCloseHandle(hSession)
  end
end;

function TAPI_tools.NumberOfProcessors;
var
  SysInfo: TSystemInfo;
begin
  GetSystemInfo(SysInfo);
  result:= SysInfo.dwNumberOfProcessors;
end;

//------------------------------------------------------------------------------
procedure Register;
begin
  RegisterComponents('API Misc', [TAPI_tools]);
end;

end.
