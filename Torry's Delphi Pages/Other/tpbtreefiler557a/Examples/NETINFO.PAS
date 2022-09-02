{*********************************************************}
(*                B-Tree Filer Ver 5.55                  *)
{*     Copyright (c) TurboPower Software Co 1986-99      *}
{*                 All rights reserved.                  *}
(*      Based in part on code written by Ralf Nagel      *)
{*********************************************************}

{$R-,S-,I+,V-,X+}

{
  This program attempts to determine the type of network you are
  running under, and provides information known about that system.

  If Novell NetWare is running, a comprehensive report is shown
  with file server information, workstation information, and
  network printer information.  Also, if Novell's NetBIOS emulator
  is running a message informing you of that is printed.

  No matter what network is running, a redirection report is run
  listing all of the currently redirected devices.  Also, any network
  print devices are listed with their printer setup strings.

  This program has been severely updated compared with older versions
  to reflect the changes to NetWare support in B-Tree Filer.}

{$I BTDEFINE.INC}

program NetInfo;

uses
  {$IFDEF Windows}
  {$IFDEF Ver80}
  SysUtils,
  {$ENDIF}
  WinProcs,
  WinCrt,
  {$IFDEF Ver80}
  BaseSupp,
  DosSupp,
  {$ELSE}
  WinDos,
  {$ENDIF}
  DPMI,
  {$ELSE}
    {$IFNDEF UseOPCRT}
      {$IFDEF UseTPCRT}
        TpCrt,
      {$ELSE}
        Crt,
      {$ENDIF}
    {$ELSE}
      OpCrt,
    {$ENDIF}
  Dos,
  {$ENDIF}
  NWBase,    {the routines for Novell's NetWare}
  NWConn,
  NWFile,
  NWTTS,
  NWMsg,
  NWPrint,
  NetBIOS,   {NetBIOS related routines}
  Share;     {DOS and redirector services}

type
  Str9 = string[9];
  Str40 = string[40];

  {Types of network recognised}
  NetType = (LanNovell,             {Novell NetWare}
             LanNovellWithNetBIOS,  {Novell NetWare & NetBIOS emulator}
             LanNetBIOS,            {NetBIOS}
             LanPCLan,              {IBM PC Lan}
             LanOther);             {Unknown LAN, or none present}

  {$IFDEF Windows}
   {$IFDEF Ver80}
   Registers = GenRegisters;
   {$ELSE}
   Registers = TRegisters;
   {$ENDIF}
  {$ENDIF}

const
  Digits : array[0..$F] of Char = '0123456789ABCDEF';
  DayNames : array [NWDayOfWeek] of Str9 =
          ('Sunday', 'Monday', 'Tuesday', 'Wednesday',
           'Thursday', 'Friday', 'Saturday');

  MonthNames : array[1..12] of Str9 =
            ('January', 'February', 'March', 'April', 'May', 'June',
             'July', 'August', 'September', 'October', 'November', 'December');

  HiLite    = $0F;              {color attribute used for emphasis}
  CtrlAttr  = $70;

var
  PCLanOperation : PCLanOpType;
     {for PC Lan: indicates mode in which PC LAN is operating}


function HexB(B : byte) : string;
  {-Return hex string for byte}
  begin
    HexB[0] := #2;
    HexB[1] := Digits[B shr 4];
    HexB[2] := Digits[B and $F];
  end;

function HexW(W : word) : string;
  {-Return hex string for word}
  begin
    HexW[0] := #4;
    HexW[1] := Digits[hi(W) shr 4];
    HexW[2] := Digits[hi(W) and $F];
    HexW[3] := Digits[lo(W) shr 4];
    HexW[4] := Digits[lo(W) and $F];
  end;

function HexPtr(P : Pointer) : string;
  {-Return hex string for pointer}
  begin
    HexPtr := HexW(Seg(P^)) + ':' + HexW(Ofs(P^));
  end;


procedure PressAnyKey(S : string);
  {-Display a prompt, wait for a keypress, and clear the prompt}
  var
    SaveAttr : word;
  begin
    {$IFDEF Windows}
    write('Press any key ',S);
    if ReadKey = #0 then ;
    writeln;
    {$ELSE}
    writeln;
    SaveAttr := TextAttr;
    TextAttr := $0F;
    write('Press any key ', S);
    if ReadKey = #0 then ;
    GotoXY(1, WhereY);
    ClrEOL;
    TextAttr := SaveAttr;
    {$ENDIF}
  end;


function Pad(S : string; Num : byte) : string;
  {-Pads a string with spaces so it is Num length}
  var
    Len : integer;
    i   : integer;
  begin
    Len := length(S);
    if (Len < Num) then
      for i := Len+1 to Num do
        S[I] := ' ';
    S[0] := char(Num);
    Pad := S;
  end;

function WordToStr(W : word) : string;
  {-Convert a word value to a string}
  var
    S : string[11];
  begin
    Str(W, S);
    WordToStr := S
  end;

function TwoDigitStr(W : word) : string;
  {-Left zero fills a number to two digits (for use in date/time display)}
  begin
    if (W > 99) then
      TwoDigitStr := '**'
    else
      begin
        TwoDigitStr[0] := #2;
        TwoDigitStr[1] := Digits[W div 10];
        TwoDigitStr[2] := Digits[W mod 10];
      end;
  end;


function OptionSpecified(Option : Char) : boolean;
  {-Return true if the character specifed by Option was specified on the
    command line (preceded by a / or a -).}
  var
    i : integer;
    S : string;
  begin
    OptionSpecified := true;
    Option := UpCase(Option);
    for i := 1 to ParamCount do
      begin
        S := ParamStr(i);
        if (length(S) = 2) then
          if S[1] in ['/', '-'] then
            if (Option = UpCase(S[2])) then
              Exit;
      end;
    OptionSpecified := false;
  end;

function FormatTime(Hour, Minute, Second : word) : string;
  {-Formats a time into HH:MM:SS format}
  begin
    FormatTime := TwoDigitStr(Hour) + ':' +
                  TwoDigitStr(Minute) + ':' +
                  TwoDigitStr(Second);
  end;

function FormatDateTime(var DT : TnwDate) : string;
  {-Format a server date and time for display}
  begin
    with DT do
      FormatDateTime := DayNames[WeekDay] + ' ' +
                        MonthNames[Month] + ' ' +
                        WordToStr(Day) + ', ' +
                        WordToStr(Year) + '  ' +
                        FormatTime(Hour, Minute, Second);
  end;

function NetBIOSMachineName : string;
  {-Return the NetBIOS machine name for this station}
  var
    Name     : NBNameStr;
    NameNum  : byte;
  begin
    if (GetMachineName(Name, NameNum) <> 0) then
      Name := '';
    NetBIOSMachineName := Name;
  end;

procedure WriteCtrlStr(S : string; CtrlAtt : byte);
  {-Write string out with embedded control characters in highlight
    attribute and in ^X format.}
  var
    OrigAttr : word;
    i : integer;
    c : char;
  begin
    {$IFDEF Windows}
    write(S);
    {$ELSE}
    OrigAttr := TextAttr;
    for i := 1 to length(S) do
      begin
        C := S[i];
        {if this char is between #0 and #32 excl, then translate it}
        if (#0 < C) and (C < #32) then
          begin
            TextAttr := CtrlAtt;
            write('^', char(ord(C) + ord('A') - 1));
            TextAttr := OrigAttr;
          end
        else
          write(C);
      end;
    {$ENDIF}
  end;

procedure PrinterSetupStr;
  {-Show the printer setup string for each network print device}
  var
    Parm,Inx           : word;
    Name,NetName,SetUp
                     : string;
    Dev              : DeviceType;
    Valid            : boolean;
    Sav              : byte;
  begin
    Inx := 0;
    writeln('Looking in redirection list for Network printers...');
    while (GetRedirectionEntry(Inx, Name, NetName, Parm, Dev) = 0) do
      begin
        {if this is a valid Printer device...}
        if (Dev = DevPrinter) then
          {show its setup string}
          if (GetPrinterSetup(SetUp, Inx) = 0) then
            begin
              write('Setup string for ',Name,' = ');
              {$IFNDEF Windows}
              Sav := TextAttr;
              TextAttr := HiLite;
              {$ENDIF}
              if SetUp = '' then
                writeln('<none>')
              else
                begin
                  WriteCtrlStr(SetUp,CtrlAttr);
                  writeln;
                end;
              {$IFNDEF Windows}
              TextAttr := Sav;
              {$ENDIF}
            end;
        inc(Inx);
      end;
    writeln;
  end;

function ShowServer(Name : TnwServerName;
                    Server : word;
                    var ExtraData) : boolean; far;
  const
    SFTLevelStr : array [1..3] of string[3] = ('I','II','III');
  const
    BModeStr : array [TnwBroadcastMode] of string[26] =
               ('Display server & user msgs',
                'Display server msgs',
                'Store server msgs',
                'Store server & user msgs');
  var
    Status : word;
    Handle : TnwServer absolute ExtraData;
    Date   : TnwDate;
    SI     : TnwServerInfo;
    BMode  : TnwBroadcastMode;
  begin
    ShowServer := true;
    write('===Server ', Name, '===[handle $', HexW(Server), ']===');
    if (Handle = Server) then
         writeln('  **default**')
    else writeln;
    {get and display server info}
    Status := nwGetServerInfo(Server, SI);
    if (Status <> 0) then
      writeln('!! Could not get info for ', Name)
    else
      with SI do
        begin
          write('  running NetWare version ',
                NetWareVer, '.', TwoDigitStr(NetWareSub),
                ' revision ', Revision);
         if (SFTLevel > 0) then
           writeln('  SFT Level ', SFTLevelStr[SFTLevel]);
         writeln('  using ', UsedConns,' of the ',
                             MaxConns, ' possible connections');
        end;
    {get the time and date maintained by the file server}
    Status := nwGetServerTime(Server, Date);
    if (Status <> 0) then
      writeln('!! Could not get date/time for ', Name)
    else
      writeln('  date & time: ', FormatDateTime(Date));
    {determine whether this server supports TTS}
    write('  transaction tracking (TTS) is ');
    if not nwTTSAvailable(Server) then
      write('not ');
    writeln('available.');
    {report our connection number}
    writeln('  our connection number is ', nwGetConnNo(Server));
    {report the current broadcast mode}
    Status := nwGetBroadcastMode(Server, BMode);
    if (Status <> 0) then
      writeln('!! Could not get current broadcast mode')
    else
      writeln('  our broadcast mode is [', BModeStr[BMode], ']');
    writeln;
  end;

procedure NovellSystem(NetBIOSEmulator : boolean);
  {-This is a Novell NetWare system.  Show assorted NetWare related info}
  var
    NumOfPrinters : byte;
    AtLeast1Capture : boolean;
  {------}
  procedure ServerInfo;
    {-Show info. relating to the file server and server services}
    var
      Server : TnwServer;
    begin
      {$IFNDEF Windows}
      ClrScr;
      {$ENDIF}
      writeln('SERVER INFORMATION');
      writeln('==================');
      writeln;
      Server := nwDefaultServer;
      nwEnumServers(ShowServer, Server);
    end;
  {------}
  procedure DriveInfo;
    {-Show info. related to accessible drives}
    var
      DriveLetter  : char;
      Status       : word;
      LineNum      : word;
      PathToParse  : string[3];
      Server       : TnwServer;
      SName        : TnwServerName;
      VName        : TnwVolumeName;
      Path         : string;
    begin
      {$IFNDEF Windows}
      ClrScr;
      {$ENDIF}
      writeln('DRIVE INFORMATION');
      writeln('=================');
      writeln;

      LineNum := 4;
      for DriveLetter := 'A' to 'Z' do
        if IsDriveLocal(ord(DriveLetter) - ord('A') + 1) then
          begin
            if (LineNum > 22) then
              begin
                PressAnyKey('for next page');
                LineNum := 0;
              end;
            writeln(DriveLetter, ': is a local drive');
            inc(LineNum);
          end
        else
          begin
            PathToParse := DriveLetter + ':';
            Status := nwParseFileName(PathToParse, Server, SName, VName, Path);
            if (Status <> 0) or (Server = 0) then
              begin
                if (LineNum > 22) then
                  begin
                    PressAnyKey('for next page');
                    LineNum := 0;
                  end;
                writeln(DriveLetter, ': is a remote non-NetWare drive');
                inc(LineNum);
              end
            else
              begin
                if (LineNum > 21) then
                  begin
                    PressAnyKey('for next page');
                    LineNum := 0;
                  end;
                writeln(DriveLetter, ': is a remote NetWare drive, mapped to');
                writeln('   ', SName, '\', VName, Path);
                inc(LineNum, 2);
              end;
          end;
    end;
  {------}
  procedure WSInfo;
    {-Show info. related to the workstation this program is run from}
    var
      NWDateTime : TnwDate;
      Y, Mo, D, DOW, H, Mi, S, S100 : word;
      SV : word;
    begin
      {$IFNDEF Windows}
      ClrScr;
      {$ENDIF}
      writeln('WORKSTATION INFORMATION');
      writeln('=======================');
      writeln;
      write('This workstation is on a Novell NetWare network');
      {report whether the NetBIOS emulator is loaded}
      if NetBIOSEmulator then
        writeln(' with the NetBIOS emulator loaded')
      else
        writeln;
      writeln;

      SV := nwShellVersion;
      case nwShellType of
        nsNETX : write('  using a NETX shell');
        nsVLM : write('  using the VLM requester');
      end;
      writeln(', version: ', Hi(SV), '.', TwoDigitStr(Lo(SV)));

      {get the local time and date from the workstation}
      GetDate(Y, Mo, D, DOW);
      GetTime(H, Mi, S, S100);
      with NWDateTime do
        begin
          Year := Y;
          Month := Mo;
          Day := D;
          WeekDay := NWDayOfWeek(DOW);
          Hour := H;
          Minute := Mi;
          Second := S;
        end;
      writeln('  date & time: ', FormatDateTime(NWDateTime));
    end;
  {------}
  procedure PrinterInfo;
    {-Show information about the local printers.}
    var
      BannerUser : TnwBannerName;
      LPT        : TnwPrinter;
      UpperLPT   : TnwPrinter;
    begin
      {$IFNDEF Windows}
      ClrScr;
      {$ENDIF}
      writeln('PRINTER INFORMATION');
      writeln('===================');
      writeln;
      {get the user name that's printed on the banner page}
      if (nwGetBannerName(BannerUser) = 0) then
        writeln('  the banner user name is: ', BannerUser);

      AtLeast1Capture := false;
      {get the number of recognised printers}
      NumOfPrinters := nwGetNumPrinters;
      if (NumOfPrinters = 0) then
        writeln('  no printer ports are configured for this workstation')
      else
        begin
          UpperLPT := TnwPrinter(pred(NumOfPrinters));
          for LPT := nwLPT1 to UpperLPT do
            begin
              write('  LPT', succ(ord(LPT)), ' is ');
              if not nwIsCaptured(LPT) then
                write('not ')
              else
                AtLeast1Capture := true;
              writeln('captured by NetWare');
            end;
        end;
    end;
  {------}
  procedure CaptureFlagDump;
    {-Show the capture flags for all captured printers.}
    var
      LPT      : TnwPrinter;
      UpperLPT : TnwPrinter;
      Status   : word;
      CF       : TnwCaptureFlags;
    begin
      UpperLPT := TnwPrinter(pred(NumOfPrinters));
      for LPT := nwLPT1 to UpperLPT do
        if nwIsCaptured(LPT) then
          begin
            {$IFNDEF Windows}
            ClrScr;
            {$ENDIF}
            writeln('CAPTURE INFORMATION FOR LPT', succ(ord(LPT)));
            writeln('============================');
            writeln;
            Status := nwGetCaptureFlags(LPT, CF);
            if (Status <> 0) then
              writeln('!! Error reading the capture flags')
            else
              with CF do
                begin
                  writeln('..flags:               $', HexB(PrintFlags));
                  writeln('..tabsize:             ', TabSize);
                  writeln('..copies:              ', NumCopies);
                  writeln('..formtype:            ', FormType);
                  writeln('..job name:            [', BannerJobName, ']');
                  writeln('..timeout:             ', FlushTimeout);
                  writeln('..flush:               ', FlushOnClose);
                  writeln('..lines:               ', MaxLines);
                  writeln('..cols:                ', MaxCols);
                  writeln('..formname:            [', FormName, ']');
                  writeln('..is captured:         ', IsCaptured);
                  writeln('..is capturing:        ', IsCapturingData);
                  writeln('..is timing out:       ', IsDoingTimeOut);
                  writeln('..is captured to file: ', IsCapturedToFile);
                end;
            PressAnyKey('to continue');
          end;
    end;
  {------}
  begin
    {start with workstation info, and proceed...}
    WSInfo;
    PressAnyKey('for the server information');
    ServerInfo;
    PressAnyKey('for the drive information');
    DriveInfo;
    PressAnyKey('for the printer information');
    PrinterInfo;
    if (NumOfPrinters > 0) and AtLeast1Capture and OptionSpecified('P') then
      begin
        PressAnyKey('for the dump of the capture flags for each printer');
        CaptureFlagDump;
      end;
  end;


procedure DoNetBIOSInfo;
begin
  writeln;
  writeln('The network name for this machine is: ', NetBIOSMachineName);

  PrinterSetupStr;
end;


procedure NetBIOSSystem;
  {-This is a NetBIOS compatible system.}
begin
  {Tell them this is NetBIOS}
  writeln('This system is running a NetBIOS network');
  DoNetBIOSInfo;
end;


procedure OtherSystem;
  {-We can not identify the LAN running on this system (it may be that
    no LAN is is running).}
begin
  writeln('This system is running some unknown LAN');
  writeln('It is possible that no LAN software is installed');
  writeln;
  writeln('Attempting to continue with DOS/SHARE information');
end;


procedure PCLanSystem;
  {-IBM PC Lan or close compatible is running on this system}
begin
  writeln('This system is running under PC-Lan (or close compatible)');
  DoNetBIOSInfo;
end;

function DetermineNetwork : NetType;
  {-Determine the network type to the best of our ability}
var
  NetBiosIsPresent : boolean;
  Logged           : boolean;
begin
  {check for NetBIOS first.}
  NetBiosIsPresent := NetBiosInstalled;

  {now check for Novell NetWare}
  if (nwShellType <> nsNone) then
    if NetBiosIsPresent then
      DetermineNetwork := LanNovellWithNetBIOS
    else
      DetermineNetwork := LanNovell
  {now check for PC Lan}
  else if IBMPCLanLoaded(PCLanOperation) then
    DetermineNetwork := LanPCLan
  else if NetBiosIsPresent then
    DetermineNetwork := LanNetBIOS
  else
    DetermineNetwork := LanOther;
end;

function DeviceTypeSt(Dev : DeviceType) : string;
  {-Return a device type string for RedirectionReport}
  begin
    case Dev of
      DevPrinter : DeviceTypeSt := 'printer    ';
      DevDrive   : DeviceTypeSt := 'disk drive ';
    else
      DeviceTypeSt := '??unknown  ';
    end;
  end;

procedure RedirectionReport;
  {-Show all the redirections currently in use by the LAN.}
const
  LINESPERSCREEN   = 20;
var
  Parm    : word;
  Inx     : word;
  LineNum : word;
  Name    : string[19];
  NetName : string;
  Dev     : DeviceType;
begin
  Inx := 0;
  LineNum := 5;
  {display each item in the redirection list.}
  while (GetRedirectionEntry(Inx, Name, NetName, Parm, Dev) = 0) do
    begin
      {display the redirection info for this entry}
      if (Dev <> DevInvalid) then
        begin
          inc(LineNum);
          if (LineNum = LINESPERSCREEN) then
            begin
              PressAnyKey(' for next page');
              LineNum := 1;
            end;
          writeln(DeviceTypeSt(Dev), Pad(Name,5),
                  ' has been redirected to [', NetName, ']');
          inc(Inx);
        end;
    end;
  if (Inx = 0) then
    writeln('!! No redirected devices found');
end;

procedure ShareInfo;
  {-Show information obtained through DOS 3.x+ calls}
begin
  {$IFNDEF Windows}
  ClrScr;
  {$ENDIF}
  writeln('DOS/SHARE INFORMATION');
  writeln('=====================');
  writeln;
  writeln('DOS version ', DosMajor, '.', TwoDigitStr(DosMinor));

  if DosMajor >= 3 then
    begin
      write('SHARE is ');
      if ShareInstalled then
        writeln('loaded')
      else
        writeln('not loaded');
      {report on redirected network devices}
      RedirectionReport;
    end
  else
    writeln('  DOS 3.x+ services required for further information');
end;

var
  Network : NetType;

begin
  ClrScr;
  writeln('NetINFO - by TurboPower Software - Version 5.55');
  writeln;

  {try to figure out what LAN we're running on}
  Network := DetermineNetwork;
  {display info about that network}
  case Network of
    LanNovell       : NovellSystem(false);  {NetWare without NetBIOS}
    LanNovellWithNetBIOS
                    : NovellSystem(true);   {NetWare with NetBIOS}
    LanNetBIOS      : NetBIOSSystem;        {NetBIOS}
    LanPCLan        : PCLanSystem;          {PC Lan}
    LanOther        : OtherSystem;          {Unkown or no network}
  end;
  {display the DOS/SHARE info}
  PressAnyKey('to show DOS/SHARE information');
  ShareInfo;
end.

