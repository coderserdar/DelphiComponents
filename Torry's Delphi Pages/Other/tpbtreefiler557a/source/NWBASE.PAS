(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is TurboPower B-Tree Filer
 *
 * The Initial Developer of the Original Code is
 * TurboPower Software
 *
 * Portions created by the Initial Developer are Copyright (C) 1996-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Based in part on code written by Ralf Nagel
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

{--Conditional defines and compiler options}
  {$I BTDefine.Inc}
  {$B+,F-,I-,R-,S-,V-}
  {$IFDEF CanAllowOverlays}
    {$F+,O+,A-}
  {$ENDIF}
  {$IFDEF CanSetOvrflowCheck}
    {$Q-}
    {$IFDEF Ver70}
    {$P-}
    {$ENDIF}
    {$IFDEF Ver80}
    {$P-}
    {$ENDIF}
  {$ENDIF}
  {all other compiler options are 'don't care'}

{$IFDEF Win32}
  !! Error - this unit cannot be compiled for 32-bit
{$ENDIF}

unit NWBase;
  {-Unit to provide base NetWare call functionality.}

{!!.51 Notes:
  - Many routines in the interface were rearranged to allow
    for the unit to be initialized automatically but not in
    the unit initialization block. These code movements have
    NOT been flagged.
  - It is a programmer error to call a NETX or VLM specific
    call without first checking for the presence of NETX or
    VLM. Generally it will crash the system if you attempt
    to call a shell that's not present. Call nwShellType
    first.}

{-Activate the CreateRealModeStack compiler define if you wish to use
  a user allocated 1K stack for simulated real mode calls instead of
  the small one supplied by the DPMI server. A debug tool.}
{$IFDEF DPMIorWnd}                                              {!!.51}
{.$DEFINE CreateRealModeStack}                                  {!!.51}
{$ENDIF}                                                        {!!.51}

interface

{$IFDEF DPMIorWnd}
uses
  {$IFDEF Windows}
  WinProcs, WinTypes,
  {$IFDEF VER80}                                                {!!.51}
  DosSupp,                                                      {!!.51}
  {$ELSE}                                                       {!!.51}
  WinDos,
  {$ENDIF}                                                      {!!.51}
  {$ELSE}
  WinAPI,
  {$ENDIF}
  DPMI;
{$ENDIF}

const
  {NWXxxx NetWare units error codes}
  nwErrDPMI    = $7F01; {DPMI error: either out of selectors, or DOS memory}
  nwErrWrongVer= $7F02; {Server NetWare version does not support call}
  nwErrShell   = $7F03; {Shell error: eg no shell, wrong shell for call}
  nwErrMemory  = $7F04; {Out of heap memory}
  nwErrIntr    = $7F05; {Error on an interrupt, generally DPMI based}
  nwErrBadData = $7F06; {Bad data passed to routine}
  nwErrTooManyConns = $7F07; {Too many connections}
  nwErrNoMoreConns  = $7F08; {No more connections found}

  nwErrBaseNETX  = $8100; {Base error code for NETX errors: $8100-$81FF}
  nwErrBaseVLM   = $8800; {Base error code for VLM errors: $8800-$88FF}
  nwErrBaseServer= $8900; {Base error code for server errors: $8900-$89FF}

const
  {VLM Destination ID values}
  vlmVLM       = $01;  {VLM.EXE manager}           {**}
  vlmConn      = $10;  {CONN.VLM}                  {**}
  vlmTransport = $20;  {TRAN.VLM}                  {**}
  vlmIPX       = $21;  {IPX.VLM}
  vlmTCP       = $22;  {TCP.VLM}
  vlmNWP       = $30;  {NWP.VLM}                   {**}
  vlmBindery   = $31;  {BIND.VLM}
  vlmNDS       = $32;  {NDS.VLM}
  vlmPNW       = $33;  {PNW.VLM}
  vlmRSA       = $34;  {RSA.VLM}
  vlmRedir     = $40;  {REDIR.VLM}                 {**}
  vlmFIO       = $41;  {FIO.VLM}
  vlmPrint     = $42;  {PRINT.VLM (optional)}      {**}
  vlmGeneral   = $43;  {GENERAL.VLM}               {**}
  vlmNETX      = $50;  {NETX.VLM}
  vlmAuto      = $60;  {AUTO.VLM}
  vlmNMR       = $100; {NMR.VLM}
  {Those constants marked ** are used by these NetWare units}

const
  {Versions where 1000-user capabilities appear}
  ServerVersion311 = $030B;  {version number for NetWare 3.11}
  NETXVersion330 = $031E;    {version number for NETX 3.30}

type
  {Prepare for 32-bit Pascal}
  nwInt = integer;           {16-bit signed integer}
  nwLong= longint;           {32-bit signed integer}

  {Registers variable for NetWare units}
  TnwRegisters = record
    case byte of
      0 : (EDI, ESI, EBP, ERes,
           EBX, EDX, ECX, EAX : nwLong;
           Flags : Word;
           ES, DS, FS, GS, IP, CS, SP, SS : Word);
      1 : (DI, EDIH,
           SI, ESIH,
           BP, EBPH : word;
           Fill1    : nwLong;
           BX, EBXH,
           DX, EDXH,
           CX, ECXH,
           AX, EAXH : word);
      2 : (Fill2 : array [0..3] of nwLong;
           BL, BH : byte; Fill3 : word;
           DL, DH : byte; Fill4 : word;
           CL, CH : byte; Fill5 : word;
           AL, AH : byte; Fill6 : word);
    end;

  {NetWare Shell types}
  TnwShellType = (nsNone,      {..none detected}
                  nsNETX,      {..NETX}
                  nsVLM);      {..VLM}

  {A server handle}
  TnwServer = word;

  {A NetWare error code}
  TnwErrorCode = word;

  {A bindery object name}
  TnwObjectStr = string[47];
  {A bindery property name}
  TnwPropStr = string[15];

  {The date/time types used by NetWare units}
  nwDayOfWeek = (nwSun, nwMon, nwTue, nwWed, nwThu, nwFri, nwSat);
  TnwDate = record
    Year    : word;
    Month   : byte;
    Day     : byte;
    Hour    : byte;
    Minute  : byte;
    Second  : byte;
    WeekDay : nwDayOfWeek;
  end;

  {A 6 byte type for NetWare node addresses}
  PhysicalNodeAddress = array[1..6] of Byte;

  {Type that defines a NetWare 3-part internet address}
  IPXAddress = record
    Network : nwLong; {high-low}
    Node    : PhysicalNodeAddress;
    Socket  : Word;
  end;

  {A procedural type to uppercase a string}
  TnwUpperStr   = procedure (var S : string);

  {The server name table under NETX. Internal Use Only.}
  PNETXServerNameTable = ^TNETXServerNameTable;
  TNETXServerNameTable = array [1..8] of array [0..47] of char;

type
  {Buffer type for data transfer}
  PRealBuffer = ^TRealBuffer;
  TRealBuffer = array [0..1023] of byte;

const
  {Global buffer variables for NWXxxx units}
  {Note: In pmode, the buffer is in realmode memory; nwGlobalBuf is
         a pmode pointer to it and nwGlobalBufRealPtr is the realmode
         pointer.
         In realmode, both nwGlobalBuf and nwGlobalBufRealPtr are
         the same value.
         The default size is 640 bytes.
         The buffer is expanded with nwGrowGlobalBuf.
         The VLM calls make use of nwGlobalBufVLM, in real and Windows
         modes this equals nwGlobalBuf, in pmode nwGlobalBufPtr.}
  nwGlobalBuf    : PRealBuffer = nil;  {Pointer to global buffer}
  nwGlobalBufSize: word = 0;           {Current size of nwGlobalBuf}
  nwGlobalBufRealPtr : PRealBuffer = nil;  {Realmode pointer to nwGlobalBuf}
  nwGlobalBufVLM : PRealBuffer = nil;  {Pointer to global buffer for VLM calls} {!!.51}

var
  {Routine that will be used to uppercase a string. The default one
   will map 'a'..'z' onto 'A'..'Z'; change it if you want other
   characters to be remapped as well.}
  nwUpperStr : TnwUpperStr;

function nwShellType : TnwShellType;
  {-Return the type of NetWare shell present on the workstation.}

function nwShellVersion : word;
  {-Return the version number (hibyte=major, lobyte=minor) of the shell.}

function nwServerCall(Server : TnwServer;
                      Func : byte;
                      ReqLen : word; var Request;
                      RpyLen : word; var Reply) : TnwErrorCode;
  {-Call NetWare server via NCP with pre-initialised request and reply
    packets. Function result is error code.}

function nwIsValidServer(Server : TnwServer) : boolean;
  {-Return true if Server is a recognized server handle.}

function nwSwapLong(L : nwLong) : nwLong;
  {-Swap a NetWare longint into Intel format (and vice-versa).}
  inline($5A/         {pop dx}
         $86/$D6/     {xchg dh, dl}
         $58/         {pop ax}
         $86/$C4);    {xchg ah, al}

function nwIPXAddressStr(var Address : IPXAddress) : string;
  {-Converts an IPX address into a displayable string of the form
    wwwwwwww:nnnnnnnnnnnn:ssss with w..w being the hex network
    number (8 chars), n..n being the hex node address (12 chars),
    and s the hex socket number (4 chars), making 26 chars altogether.}

function vlmCall(DestID   : word;
                 DestFunc : word;
                 var Regs : TnwRegisters) : TnwErrorCode;
  {-Internal routine to call VLM directly. Function result is error code.
    It is undefined as to what would happen if the VLM manager is not
    loaded. Check this first with a call to nwShellType.}

function vlmVersion(DestID : word) : word;
  {-Routine to get version number of any VLM module. Pass one of the VLM
    destination ID constants, returns major version in high byte, minor
    version in low byte. Returns zero if VLM module not loaded.}

function nwGetMem(var P; Size : word) : boolean;
  {-Internal routine to allocate heap memory safely, returns true for success}

procedure nwNETXPushServer(Server : TnwServer);
  {-Saves the current preferred server, sets it to Server. NETX ONLY.}

procedure nwNETXPopServer;
  {-Restores the change made by nwNETXPushServer. NETX ONLY.}

function nwIntr(Intr : byte; var Regs : TnwRegisters) : TnwErrorCode;
  {-Issue an Intr interrupt using register values in Regs.}

function nwGrowGlobalBuf(NewSize : word) : boolean;
  {-Grow the realmode buffer pointed to by nwGlobalBuf, return true if
    successful.}

procedure nwCvtStrToAsciiz(var Buffer; MaxStrLen : byte);
  {-Convert Pascal string to an ASCIIZ string in situ.
    Note that the buffer is assumed to be at least MaxStrLen+1 bytes long}

procedure nwCvtAsciizToStr(var Buffer; MaxStrLen : byte);
  {-Convert an ASCIIZ string to a Pascal string in situ.
    Note that the buffer is assumed to be at least MaxStrLen+1 bytes long}

function nwMinI(X, Y : nwInt) : nwInt;
  inline($58/$5A/   {pop ax & dx}
         $39/$D0/   {cmp ax, dx}
         $7C/$01/   {jl @@exit}
         $92        {xchg ax, dx}
         );         {@@exit:}
function nwMaxI(X, Y : nwInt) : nwInt;
  inline($58/$5A/   {pop ax & dx}
         $39/$D0/   {cmp ax, dx}
         $7F/$01/   {jg @@exit}
         $92        {xchg ax, dx}
         );         {@@exit:}
  {-Routines to find min and max of nwInts}

function nwNETXGetDriveTable(Func : word; var Table) : TnwErrorCode;
  {-Internal routine. NETX ONLY.}

function nwNETXGetServerNameTable : PNETXServerNameTable;
  {-Internal routine. NETX ONLY.}

procedure nwInitRegs(var Regs : TnwRegisters);
  {-Internal routine. Sets a TnwRegisters variable to zero.}
  inline($5F/$07/        {pop di, pop es}
         $FC/            {cld}
         $31/$C0/        {xor ax, ax}
         $B9/$19/$00/    {mov cx, sizeof(TnwRegisters) div 2}
         $F3/$AB);       {rep stosw}

implementation

var
  ExitSave : pointer;                {Exit procedure chain}
  InitCalled : boolean;              {True if unit's initialisation called}
  InternalShellType : TnwShellType;  {Saved shell type}
  InternalShellVer  : word;          {Saved shell version}
  vlmCallAddress : pointer;          {VLM realmode entry point}
  PushedServer      : TnwServer;     {Old server handle: see nwNETXPushServer}
  NETXServerNameTable : PNETXServerNameTable;
  {$IFDEF Windows}                                              {!!.52}
  NWCALLSLoaded : boolean;  {True if NWCALLS explicitly loaded} {!!.52}
  {$ENDIF}                                                      {!!.52}

{$IFDEF CreateRealModeStack}                                    {!!.51 start}
const
  RealModeStackSize = 1024; {do not reduce below 1K}
type
  PRealModeStack = ^TRealModeStack;
  TRealModeStack = array [0..pred(RealModeStackSize)] of byte;
var
  RealModeStack : pointer;
  RealModeStackPtr : PRealModeStack;
{$ENDIF}                                                        {!!.51 end}

type
  OS = record O, S : word; end; {to split pointer into sel/seg & ofs}
  LH = record L, H : word; end; {to split nwLong into hi/lo words}

  {A VLM fragment and fragment list. Element [0] is the request buffer,
   element [1] the reply buffer.}
  TFragment = record
    Address : pointer;
    Length  : word;
  end;
  TFragmentList = array [0..1] of TFragment;

{$IFDEF Windows}                                                {!!.51 start}
type
  TNWCALLSRegPack = record {Register struc for nwVLMRequest}
    nwSI, nsDS, nwDI, nwES, nwAX, nwBX, nwCX, nwDX : word;
  end;

  TnwCallsInit = function (Pin, POut : pointer) : TnwErrorCode;
  TnwVLMRequest = function (SourceID, DestID, DestFunc : word;
                            var Regs : TNWCALLSRegPack;
                            SegFlags : word) : TnwErrorCode;
var
  nwCallsInit : TnwCallsInit;            {entry point in NWCALLS.DLL}
  nwVLMRequest : TnwVLMRequest;          {entry point in NWCALLS.DLL}
{$ENDIF}                                                        {!!.51 end}

function nwHeapError(Size : word) : integer; far;
  begin
    nwHeapError := 1;
  end;

function nwGetMem(var P; Size : word) : boolean;
  var
    Pt : pointer absolute P;
    SaveHeapError : pointer;
  begin
    SaveHeapError := HeapError;
    HeapError := @nwHeapError;
    GetMem(Pt, Size);
    nwGetMem := Pt <> nil;
    HeapError := SaveHeapError;
  end;

{$IFDEF DPMIorWnd}
function DOSGetMem(var RealPtr, ProtPtr; Size : word) : boolean; near;
  var
    RealMode : pointer absolute RealPtr;
    ProtMode : pointer absolute ProtPtr;
    AllocResult : longint;
  begin
    AllocResult := GlobalDOSAlloc(Size);
    if (AllocResult <> 0) then
      begin
        RealMode := Ptr(LH(AllocResult).H, 0);
        ProtMode := Ptr(LH(AllocResult).L, 0);
        DOSGetMem := true;
      end
    else DOSGetMem := false;
  end;

function DOSFreeMem(ProtPtr : pointer) : boolean; near;
  begin
    DOSFreeMem := GlobalDOSFree(OS(ProtPtr).S) = 0;
  end;
{$ENDIF}

procedure nwNETXPushServer(Server : TnwServer); assembler;
  asm
    {API: Get/Set Preferred Connection ID}
    {get current preferred server}
    mov ax, $F001
    int $21
    mov PushedServer, ax
    {set preferred server to the one we want}
    mov ax, $F000
    mov dl, Server.Byte[0]
    int $21
  end;

procedure nwNETXPopServer; assembler;
  asm
    {API: Set Preferred Connection ID}
    {set preferred server back again}
    mov ax, $F000
    mov dl, PushedServer.Byte[0]
    int $21
  end;

procedure GetServerNameTable;
  var
    i      : word;
    Status : word;
    Regs   : TnwRegisters;
  begin
    {API: Get File Server Name}
    NETXServerNameTable := nil;
    nwInitRegs(Regs);
    with Regs do
      begin
        AX := $EF04;
        if (nwIntr($21, Regs) <> 0) then
          Exit;
        {$IFDEF DPMIOrWnd}
        if (GetSelectorForRealMem(Ptr(ES, 0),
              SI+sizeof(TNETXServerNameTable), ES) <> 0) then
          Exit;
        {$ENDIF}
        NETXServerNameTable := Ptr(ES, SI);
      end;
  end;

function nwNETXGetServerNameTable : PNETXServerNameTable;
  begin
    nwNETXGetServerNameTable := NETXServerNameTable;
  end;

function GetNETXVersion : word;
  {-Return the NETX shell version number.}
  var
    Regs : TnwRegisters;
  begin
    {API: Get Shell Version And Type}
    nwInitRegs(Regs);
    with Regs do
      begin
        AX := $EA01;
        ES := OS(nwGlobalBufRealPtr).S;
        DI := OS(nwGlobalBufRealPtr).O;
        if (nwIntr($21, Regs) = 0) then
             GetNETXVersion := BX
        else GetNETXVersion := 0;
      end;
  end;

function NETXLoaded : boolean;
  {-Return true if NETX is loaded}
  begin
    InternalShellVer := GetNETXVersion;
    NETXLoaded := (InternalShellVer <> 0);
  end;

procedure vlmGetCallAddress;
  {-Gets the VLM entry point}
  var
    Regs : TnwRegisters;
  begin
    vlmCallAddress := nil;
    nwInitRegs(Regs);
    Regs.ax := $7A20;
    if (nwIntr($2F, Regs) = 0) then
      if (Regs.al = 0) then
        vlmCallAddress := Ptr(Regs.es, Regs.bx);
  end;

function NETXValidServer(Server : TnwServer) : boolean; near;
  begin
    if (1 <= Server) and (Server <= 8) then
         NETXValidServer := (NETXServerNameTable^[Server][0] <> #0)
    else NETXValidServer := false;
  end;

function vlmValidServer(Server : TnwServer) : boolean; near;
  var
    Regs : TnwRegisters;
  begin
    nwInitRegs(Regs);
    Regs.cx := Server;
    vlmValidServer := (vlmCall(vlmConn, $05, Regs) = 0);
  end;

function nwIsValidServer(Server : TnwServer) : boolean;
  begin
    case nwShellType of                                         {!!.51}
      nsNETX : nwIsValidServer := NETXValidServer(Server);
      nsVLM  : nwIsValidServer := vlmValidServer(Server);
    else
      nwIsValidServer := false;
    end;{case}
  end;

function nwIPXAddressStr(var Address : IPXAddress) : string;
  function GetHexChar(B : byte; Upper : boolean) : char; near; assembler;
    asm
      mov al, B
      cmp Upper, 0
      jne @@ExtractUpperNibble
      and al, $0F
      jmp @@ConvertToASCII
    @@ExtractUpperNibble:
      {$IFDEF G+}
      shr al, 4
      {$ELSE}
      shr al, 1
      shr al, 1
      shr al, 1
      shr al, 1
      {$ENDIF}
    @@ConvertToASCII:
      add al, '0'
      cmp al, '9'
      jbe @@Exit
      add al, 'a' - '0' - 10
    @@Exit:
    end;
  var
    A : array [1..12] of byte absolute Address;
    i, j : integer;
    S : string[26];
  begin
    S[0] := #26;
    j := 0;
    for i := 1 to 4 do
      begin
        inc(j); S[j] := GetHexChar(A[i], true);
        inc(j); S[j] := GetHexChar(A[i], false);
      end;
    S[9] := ':';
    j := 9;
    for i := 5 to 10 do
      begin
        inc(j); S[j] := GetHexChar(A[i], true);
        inc(j); S[j] := GetHexChar(A[i], false);
      end;
    S[22] := ':';
    j := 22;
    for i := 11 to 12 do
      begin
        inc(j); S[j] := GetHexChar(A[i], true);
        inc(j); S[j] := GetHexChar(A[i], false);
      end;
    nwIPXAddressStr := S;
  end;

procedure ExitNWBase; far;
  {-Exit procedure}
  {$IFDEF Windows}                                              {!!.52}
  var                                                           {!!.52}
    H : THandle;                                                {!!.52}
  {$ENDIF}                                                      {!!.52}
  begin
    ExitProc := ExitSave;
    if not InitCalled then                                      {!!.51}
      Exit;                                                     {!!.51}
    {$IFDEF DPMIOrWnd}
    if (nwGlobalBufSize <> 0) then
      if not DOSFreeMem(nwGlobalBuf) then
        {nothing};
    if (InternalShellType = nsNETX) then
      if (FreeLDTDescriptor(OS(NETXServerNameTable).S) <> 0) then
        {nothing};
    {$ELSE}
    if (nwGlobalBufSize <> 0) then
      FreeMem(nwGlobalBuf, nwGlobalBufSize);
    {$ENDIF}
    {$IFDEF CreateRealModeStack}                                {!!.51}
    if (RealModeStackPtr <> nil) then                           {!!.51}
      if not DOSFreeMem(RealModeStackPtr) then                  {!!.51}
        {nothing};                                              {!!.51}
    {$ENDIF}                                                    {!!.51}
    {$IFDEF Windows}                                            {!!.52}
    if NWCALLSLoaded then                                       {!!.52}
      begin                                                     {!!.52}
        H := GetModuleHandle('NWCALLS');                        {!!.52}
        if (H <> 0) then                                        {!!.52}
          FreeLibrary(H);                                       {!!.52}
      end;                                                      {!!.52}
    {$ENDIF}                                                    {!!.52}
  end;

{$IFDEF CreateRealModeStack}                                    {!!.51 start}
function GetRealModeStack : boolean;
  begin
    GetRealModeStack := false;
    if not DOSGetMem(RealModeStack, RealModeStackPtr, RealModeStackSize) then
      Exit;
    {for debugging purposes, fill the stack with $CC}
    FillChar(RealModeStackPtr^, RealModeStackSize, $CC);
    GetRealModeStack := true;
  end;
{$ENDIF}                                                        {!!.51 end}

{$IFDEF Windows}                                                {!!.51 start}
function InitNWCALLS : boolean;
  var
    H : THandle;
  begin
    InitNWCALLS := false;
    H := GetModuleHandle('NETWARE');                            {!!.52}
    if (H = 0) then                                             {!!.52}
      Exit;                                                     {!!.52}

    H := LoadLibrary('NWCALLS.DLL');                            {!!.52}
    if H < 32 then                                              {!!.52}
      Exit;                                                     {!!.52}
    NWCALLSLoaded := true;                                      {!!.52}

    @nwCallsInit := GetProcAddress(H, 'NWCALLSINIT');
    if (@nwCallsInit = nil) then
      Exit;
    if (nwCallsInit(nil, nil) <> 0) then
      Exit;

    @nwVLMRequest := GetProcAddress(H, 'NWVLMREQUEST');
    if (@nwVLMRequest = nil) then
      Exit;

    InitNWCALLS := true;
  end;
{$ENDIF}                                                        {!!.51 end}

procedure InitNWBase;
  {-Unit initialization routine.}
  begin
    InitCalled := true;                                         {!!.51}

    InternalShellType := nsNone;
    InternalShellVer := 0;

    if not nwGrowGlobalBuf(640) then
      Exit;

    {$IFDEF CreateRealModeStack}                                {!!.51 start}
    if not GetRealModeStack then
      begin
        if not DOSFreeMem(nwGlobalBuf) then
          {nothing};
        Exit;
      end;
    {$ENDIF}                                                    {!!.51 end}

    {Get shell info}
    {..first look for the VLM}
    vlmGetCallAddress;
    if (vlmCallAddress <> nil) then
      begin
        InternalShellType := nsVLM;
        {$IFDEF Windows}                                        {!!.51}
        {in Windows: only call vlmVersion if NWCALLS is present}{!!.51}
        if InitNWCALLS then                                     {!!.51}
        {$ENDIF}                                                {!!.51}
        InternalShellVer := vlmVersion(vlmVLM);
      end
    {..if VLM is not loaded then look for NETX}
    else if NETXLoaded then
      begin
        InternalShellType := nsNETX;
        {InternalShellVer is set within NETXLoaded}
        GetServerNameTable;
        if (NETXServerNameTable = nil) then
          InternalShellVer := 0;
      end;

    if (InternalShellVer = 0) then
      InternalShellType := nsNone;
  end;

{$IFDEF DPMI} {!!.51 various versions of vlmCall moved}
function vlmCall(DestID   : word;
                 DestFunc : word;
                 var Regs : TnwRegisters) : TnwErrorCode; assembler;
  {-Calls VLM module DestID using subfunction DestFunc}
  asm
    cmp InternalShellType, nsVLM
    je @@VLMisThere
    mov ax, nwErrShell
    jmp @@Exit
  @@VLMisThere:
    xor bx, bx
    push bx
    push DestID
    push DestFunc
    mov ax, 0301h
    mov cx, 6
    les di, Regs
    {$IFDEF CreateRealModeStack}                                {!!.51 start}
    mov dx, RealModeStack.Word[2]
    mov es:[di].TnwRegisters.&ss, dx
    mov dx, RealModeStack.Word[0]
    add dx, RealModeStackSize
    mov es:[di].TnwRegisters.&sp, dx
    {$ENDIF}                                                    {!!.51 end}
    mov dx, vlmCallAddress.Word[2]
    mov es:[di].TnwRegisters.&cs, dx
    mov dx, vlmCallAddress.Word[0]
    mov es:[di].TnwRegisters.&ip, dx
    int 31h
    jc @@DPMIError
    mov ax, es:[di].TnwRegisters.&ax.Word[0]
    jmp @@Exit
  @@DPMIError:
    mov ax, nwErrDPMI
  @@Exit:
    mov sp, bp            {Deallocate pushed variables}
  end;
{$ELSE}{$IFDEF MSDOS}
function vlmCall(DestID   : word;
                 DestFunc : word;
                 var Regs : TnwRegisters) : TnwErrorCode; assembler;
  {-Calls VLM module DestID using subfunction DestFunc}
  var
    vlmEntryPoint : pointer;
  asm
    cmp InternalShellType, nsVLM
    je @@VLMisThere
    mov ax, nwErrShell
    jmp @@Exit
  @@VLMisThere:
    {Notes: the Regs.BP field and BP register are not set/restored as
            the VLM shell internally uses it as a scratch register.}
    mov ax, vlmCallAddress.Word[2]
    mov vlmEntryPoint.Word[2], ax
    mov ax, vlmCallAddress.Word[0]
    mov vlmEntryPoint.Word[0], ax
    push ds               {save our data segment}
    push bp               {...and our stack frame}
    xor ax, ax            {push the parameters for the VLM}
    push ax
    push DestID
    push DestFunc

    lds si, Regs                    {Set up the registers...}
    mov ax, [si].TnwRegisters.&SI
    push ax
    mov ax, [si].TnwRegisters.&DS
    push ax
    mov di, [si].TnwRegisters.&DI
    mov bx, [si].TnwRegisters.&BX
    mov dx, [si].TnwRegisters.&DX
    mov cx, [si].TnwRegisters.&CX
    mov ax, [si].TnwRegisters.&AX
    mov es, [si].TnwRegisters.&ES
    pop ds
    pop si

    call vlmEntryPoint    {call the VLM}

    pop bp                {discard returned BP & restore our stack frame}
    push ax               {To return the function result}
    push ds               {Set up the Regs variable}
    pushf
    push si
    lds si, Regs
    mov [si].TnwRegisters.&DI, di
    mov [si].TnwRegisters.&BX, bx
    mov [si].TnwRegisters.&DX, dx
    mov [si].TnwRegisters.&CX, cx
    mov [si].TnwRegisters.&AX, ax
    mov [si].TnwRegisters.&ES, es
    pop ax
    mov [si].TnwRegisters.&SI, ax
    pop ax
    mov [si].TnwRegisters.&Flags, ax
    pop ax
    mov [si].TnwRegisters.&DS, ax

    pop ax                {get VLM call result code}
    pop ds                {restore Pascal's DS}
  @@Exit:
  end;
{$ELSE} {Windows, new for !!.51}
function vlmCall(DestID   : word;
                 DestFunc : word;
                 var Regs : TnwRegisters) : TnwErrorCode; assembler;
  {-Calls VLM module DestID using subfunction DestFunc}
  var
    NWRP : TNWCALLSRegPack;
  asm
    cmp InternalShellType, nsVLM
    je @@VLMisThere
    mov ax, nwErrShell
    jmp @@Exit
  @@VLMisThere:
    mov dx, ds              {save our data segment}
    xor bx, bx              {zero the SegFlags value}

    lds si, Regs            {get the external registers structure}
    mov di, ss              {get the NWCALLS registers structure}
    mov es, di
    lea di, NWRP
    cld                     {forwards}

    mov ax, [si].TnwRegisters.&SI;   stosw     {transfer the register values}
    mov ax, [si].TnwRegisters.&DS;   stosw     {  to the NWCALLS structure}
    or ax, ax
    jz @@DoneDS
    or bl, 1
  @@DoneDS:
    mov ax, [si].TnwRegisters.&DI;   stosw
    mov ax, [si].TnwRegisters.&ES;   stosw
    or ax, ax
    jz @@DoneES
    or bl, 2
  @@DoneES:
    mov ax, [si].TnwRegisters.&AX;   stosw
    mov ax, [si].TnwRegisters.&BX;   stosw
    mov ax, [si].TnwRegisters.&CX;   stosw
    mov ax, [si].TnwRegisters.&DX;   stosw

    mov ds, dx              {get back our data segment}

    xor ax, ax              {call the NWCALLS DLL entry point}
    push ax
    push DestID
    push DestFunc
    push ss
    lea ax, NWRP
    push ax
    push bx
    call nwVLMRequest

    mov dx, ds              {save our data segment}
    xchg ax, bx             {save the VLM return value in BX}

    mov si, ss              {get the NWCALLS registers structure}
    mov ds, si
    lea si, NWRP
    les di, Regs            {get the external registers structure}
    cld

    lodsw;   mov es:[di].TnwRegisters.&SI, ax   {transfer the register values}
    lodsw;   mov es:[di].TnwRegisters.&DS, ax   {  from the NWCALLS structure}
    lodsw;   mov es:[di].TnwRegisters.&DI, ax
    lodsw;   mov es:[di].TnwRegisters.&ES, ax
    lodsw;   mov es:[di].TnwRegisters.&AX, ax
    lodsw;   mov es:[di].TnwRegisters.&BX, ax
    lodsw;   mov es:[di].TnwRegisters.&CX, ax
    lodsw;   mov es:[di].TnwRegisters.&DX, ax

    xchg ax, bx             {get back the return value}
    mov ds, dx              {get back our data segment}
  @@Exit:
  end;
{$ENDIF}{$ENDIF}


{$IFDEF DPMIorWnd}
function nwGrowGlobalBuf(NewSize : word) : boolean;
  {-Grow the realmode buffer, if required}
  begin
    if (NewSize <= nwGlobalBufSize) then
      nwGrowGlobalBuf := true
    else
      begin
        if (nwGlobalBufSize <> 0) then
          if not DOSFreeMem(nwGlobalBuf) then
            {nothing};
        {round up new size to nearest 128 bytes}
        nwGlobalBufSize := (NewSize + 127) and $FF80;
        if DOSGetMem(nwGlobalBufRealPtr, nwGlobalBuf, nwGlobalBufSize) then
          begin                                                 {!!.51}
            nwGrowGlobalBuf := true;                            {!!.51}
            {$IFDEF Windows}                                    {!!.51}
            nwGlobalBufVLM := nwGlobalBuf;                      {!!.51}
            {$ELSE}                                             {!!.51}
            nwGlobalBufVLM := nwGlobalBufRealPtr;               {!!.51}
            {$ENDIF}                                            {!!.51}
          end                                                   {!!.51}
        else
          begin
            nwGrowGlobalBuf := false;
            nwGlobalBufRealPtr := nil;
            nwGlobalBuf := nil;
            nwGlobalBufSize := 0;
            nwGlobalBufVLM := nil;                              {!!.51}
          end;
      end;
  end;

function nwServerCall(Server : TnwServer;
                      Func : byte;
                      ReqLen : word; var Request;
                      RpyLen : word; var Reply) : TnwErrorCode;
  const
    VLMLeeway = $20; {ample leeway for the extra VLM memory requirements}
  var
    TotalSize : word;
    Status    : word;
    Fragment  : TFragmentList;
    Regs      : TnwRegisters;
    SavedConn : byte;
  begin
    if (nwShellType = nsNone) then                              {!!.51}
      begin
        nwServerCall := nwErrShell;
        Exit;
      end;
    {call the NetWare server either via NETX or VLM, using NCP}
    if (InternalShellType = nsNETX) then
      begin
        if not NETXValidServer(Server) then
          begin
            nwServerCall := nwErrBaseNETX + $01;
            Exit;
          end;
        {calculate the size of the request and reply packets}   {!!.51}
        TotalSize := ReqLen + RpyLen;                           {!!.51}
        {if this is greater than our realmode buffer, grow it}  {!!.51}
        if not nwGrowGlobalBuf(TotalSize) then                  {!!.51}
          begin                                                 {!!.51}
            nwServerCall := nwErrDPMI;                          {!!.51}
            Exit;                                               {!!.51}
          end;                                                  {!!.51}
        {move the request buffer over to real mode}             {!!.51}
        Move(Request, nwGlobalBuf^[0], ReqLen);                 {!!.51}
        {push current server handle}                            {!!.51}
        nwNETXPushServer(Server);
        {Set up the registers and call the server via NETX}
        nwInitRegs(Regs);
        with Regs do
          begin
            ax := $F200 + Func;   {NCP call}
            cx := ReqLen;
            dx := RpyLen;
            ds := OS(nwGlobalBufRealPtr).S;  {point ds:si at the request buffer}
            si := OS(nwGlobalBufRealPtr).O;
            es := ds;                        {point es:di at the reply buffer}
            di := si + ReqLen;
            Status := nwIntr($21, Regs);
            if (Status = 0) then
              if (al <> 0) then
                Status := nwErrBaseServer + al;
            nwServerCall := Status;
          end;
        nwNETXPopServer;
        {move the reply buffer back to the user's buffer}       {!!.51}
        if (RpyLen <> 0) then                                   {!!.51}
          Move(nwGlobalBuf^[ReqLen], Reply, RpyLen);            {!!.51}
      end
    else {we're using VLM}
      begin
        {Note: for a pmode target we must fiddle around with realmode pointers,
               copy the request buffer to realmode before making the call, and
               the reply buffer from realmode afterwards. For a Windows target
               the NetWare DLLs do that for us, so we can just pass the
               protected mode pointers.}
        if not vlmValidServer(Server) then
          begin
            nwServerCall := nwErrBaseVLM + $01;
            Exit;
          end;
        {API: _TRAN Request Reply}
        {$IFDEF Windows}                                        {!!.51}
        {Set up the fragments in pmode, we use 1 fragment/buffer} {!!.51}
        Fragment[0].Address := @Request;                        {!!.51}
        Fragment[0].Length := ReqLen;                           {!!.51}
        Fragment[1].Address := @Reply;                          {!!.51}
        Fragment[1].Length := RpyLen;                           {!!.51}
        {Set up the registers and call the server via the VLM}  {!!.51}
        nwInitRegs(Regs);                                       {!!.51}
        with Regs do                                            {!!.51}
          begin                                                 {!!.51}
            al := Func;                                         {!!.51}
            bl := 1; {one fragment for the request buffer}      {!!.51}
            if (RpyLen <> 0) then                               {!!.51}
              dl := 1; {...one fragment for the reply buffer}   {!!.51}
            cx := Server;                                       {!!.51}
            ds := Seg(Fragment);      {point ds:si at the request fragment}
            si := Ofs(Fragment[0]);                             {!!.51}
            es := ds;                 {point es:di at the reply fragment}
            di := Ofs(Fragment[1]);                             {!!.51}
            nwServerCall := vlmCall(vlmTransport, $06, Regs);   {!!.51}
          end;                                                  {!!.51}
        {$ELSE} {DPMI}                                          {!!.51}
        {calculate the size of the request and reply packets}   {!!.51}
        TotalSize := ReqLen + RpyLen + VLMLeeway;               {!!.51}
        {if this is greater than our realmode buffer, grow it}  {!!.51}
        if not nwGrowGlobalBuf(TotalSize) then                  {!!.51}
          begin                                                 {!!.51}
            nwServerCall := nwErrDPMI;                          {!!.51}
            Exit;                                               {!!.51}
          end;                                                  {!!.51}
        {move the request buffer over to real mode}             {!!.51}
        Move(Request, nwGlobalBuf^[0], ReqLen);                 {!!.51}
        {Set up the fragments in realmode, we use 1 fragment/buffer}
        Fragment[0].Address := nwGlobalBufRealPtr;
        Fragment[0].Length := ReqLen;
        Fragment[1].Address := Ptr(OS(nwGlobalBufRealPtr).S,
                                   OS(nwGlobalBufRealPtr).O + ReqLen);
        Fragment[1].Length := RpyLen;
        Move(Fragment, nwGlobalBuf^[ReqLen+RpyLen], sizeof(Fragment));
        {Set up the registers and call the server via the VLM}
        nwInitRegs(Regs);
        with Regs do
          begin
            al := Func;
            bl := 1; {one fragment for the request buffer}
            if (RpyLen <> 0) then
              dl := 1; {...one fragment for the reply buffer}
            cx := Server;
            ds := OS(nwGlobalBufRealPtr).S;  {point ds:si at the request fragment}
            si := OS(nwGlobalBufRealPtr).O + ReqLen+RpyLen;
            es := ds;                        {point es:di at the reply fragment}
            di := si + sizeof(Fragment[0]);
            nwServerCall := vlmCall(vlmTransport, $06, Regs);
          end;
        {move the reply buffer back to the user's buffer}       {!!.51}
        if (RpyLen <> 0) then                                   {!!.51}
          Move(nwGlobalBuf^[ReqLen], Reply, RpyLen);            {!!.51}
        {$ENDIF}                                                {!!.51}
      end;
  end;
{$ELSE} {it's a realmode target}
function nwGrowGlobalBuf(NewSize : word) : boolean;
  {-Grow the realmode buffer, if required}
  begin
    nwGrowGlobalBuf := true;
    if (NewSize > nwGlobalBufSize) then
      begin
        if (nwGlobalBufSize <> 0) then
          FreeMem(nwGlobalBuf, nwGlobalBufSize);
        {round up new size to nearest 128 bytes}
        nwGlobalBufSize := (NewSize + 127) and $FF80;
        if not nwGetMem(nwGlobalBuf, nwGlobalBufSize) then
          begin
            nwGrowGlobalBuf := false;
            nwGlobalBufSize := 0;
          end;
        nwGlobalBufRealPtr := nwGlobalBuf;
        nwGlobalBufVLM := nwGlobalBuf;                          {!!.51}
      end;
  end;

function nwServerCall(Server : TnwServer;
                      Func : byte;
                      ReqLen : word; var Request;
                      RpyLen : word; var Reply) : TnwErrorCode;
  var
    Status   : word;
    Regs     : TnwRegisters;
    Fragment : TFragmentList;
  begin
    case nwShellType of                                         {!!.51}
      nsNone :
         nwServerCall := nwErrShell;
      nsNETX :
         begin
           if not NETXValidServer(Server) then
             begin
               nwServerCall := nwErrBaseNETX + $01;
               Exit;
             end;
           nwNETXPushServer(Server);
           nwInitRegs(Regs);
           with Regs do
             begin
               ax := $F200 + Func; {NCP call}
               cx := ReqLen;
               dx := RpyLen;
               ds := seg(Request);
               si := ofs(Request);
               es := seg(Reply);
               di := ofs(Reply);
               Status := nwIntr($21, Regs);
               if (al = 0) then
                    nwServerCall := 0
               else nwServerCall := nwErrBaseServer + al;
             end;
           nwNETXPopServer;
         end;
      nsVLM :
         begin
           if not vlmValidServer(Server) then
             begin
               nwServerCall := nwErrBaseVLM + $01;
               Exit;
             end;
           {API: _TRAN Request Reply}
           {Set up the fragments in realmode, we use 1 fragment/buffer}
           Fragment[0].Address := @Request;
           Fragment[0].Length := ReqLen;
           Fragment[1].Address := @Reply;
           Fragment[1].Length := RpyLen;
           {Set up the registers and call the VLM}
           nwInitRegs(Regs);
           with Regs do
             begin
               al := Func;
               bl := 1; {one fragment for the request buffer}
               if (RpyLen <> 0) then
                 dl := 1; {...one fragment for the reply buffer}
               cx := Server;
               ds := Seg(Fragment[0]);  {point ds:si at the request packet}
               si := Ofs(Fragment[0]);
               es := Seg(Fragment[1]);  {point es:di at the reply fragment}
               di := Ofs(Fragment[1]);
               nwServerCall := vlmCall(vlmTransport, $06, Regs);
             end;
         end;
      end;{case}
  end;
{$ENDIF}

procedure nwCvtStrToAsciiz(var Buffer; MaxStrLen : byte);
assembler;
  asm
    mov dx, ds              {save Pascal's DS}
    xor ax, ax              {zero ax}
    mov cx, ax              {zero cx, the counter}
    mov cl, MaxStrLen       {get maximum string length}
    lds si, Buffer          {get the buffer}
    mov al, [si]            {get the string length byte}
    cmp ax, cx              {is it less than the max string length?}
    jae @@LengthIsOk        {no, so number of chars to move is OK}
    xchg ax, cx             {set num of chars to move to the string length}
  @@LengthIsOk:
    jcxz @@Exit             {anything to do? No? Exit if so}
    mov di, si              {set source and dest offsets for move}
    inc si
    mov bx, ds              {set up for move string}
    mov es, bx
    cld                     {forwards!}
    rep movsb               {move characters}
    mov es:[di], cl         {set string null terminator}
  @@Exit:
    mov ds, dx              {restore Pascal's DS}
  end;

procedure nwCvtAsciizToStr(var Buffer; MaxStrLen : byte);
assembler;
  asm
    mov dx, ds              {save Pascal's DS}
    xor ax, ax              {zero ax, ah is string length, al is null byte}
    mov cx, ax              {zero cx, the counter}
    mov cl, MaxStrLen       {get maximum string length}
    add ah, cl              {save max string length}
    jz @@Exit               {nothing to do if it's zero}
    les di, Buffer          {get the buffer}
    cld                     {forwards!}
    repne scasb             {scan for terminating zero byte}
    jne @@NoNull            {none found}
    inc cx                  {calc length of string}
    sub ah, cl
    jz @@Exit               {if zero, nothing to do}
    dec di                  {dest: point to zero byte}
  @@NoNull:
    mov si, di              {source: point to last char of string}
    dec si
    mov bx, es              {set up for move string}
    mov ds, bx
    std                     {backwards!}
    mov cl, ah              {set number of chars to move}
    rep movsb               {move characters}
    mov al, ah              {set string length byte}
    stosb
    cld                     {be kind to others}
  @@Exit:
    mov ds, dx              {restore Pascal's DS}
  end;

{$IFDEF DPMIorWnd}
function nwIntr(Intr : byte; var Regs : TnwRegisters) : TnwErrorCode;
assembler;
  asm
    cmp InitCalled, 1                                           {!!.51}
    je @@InitWasCalled                                          {!!.51}
    call InitNWBase                                             {!!.51}
  @@InitWasCalled:                                              {!!.51}
    mov ax, $0300               {set up for DPMI simulate realmode}
    xor cx, cx                  {  interrupt}
    mov bx, cx
    mov bl, Intr
    les di, Regs
    {$IFDEF CreateRealModeStack}                                {!!.51 start}
    mov dx, RealModeStack.Word[2]
    mov es:[di].TnwRegisters.&ss, dx
    mov dx, RealModeStack.Word[0]
    add dx, RealModeStackSize
    mov es:[di].TnwRegisters.&sp, dx
    {$ENDIF}                                                    {!!.51 end}
    int $31
    jc @@Error                  {was there a DPMI error?}
    xor ax, ax                  {No, so return zero error}
    jmp @@Exit
  @@Error:
    mov ax, nwErrIntr           {Yes, return DPMI error code}
  @@Exit:
  end;
{$ELSE}
function nwIntr(Intr : byte; var Regs : TnwRegisters) : TnwErrorCode;
assembler;
  asm
    cmp InitCalled, 1                                           {!!.51}
    je @@InitWasCalled                                          {!!.51}
    call InitNWBase                                             {!!.51}
  @@InitWasCalled:                                              {!!.51}
    push ds                        {save Pascal's DS}
    lea di, @@IntrCall             {patch the interrupt number} {!!.51}
    mov al, Intr
    mov cs:[di+1], al                                           {!!.51}

    lds si, Regs                   {set up the registers from the}
    mov ax, [si].TnwRegisters.&SI  {  TnwRegisters structure}
    push ax
    mov ax, [si].TnwRegisters.&DS
    push ax
    mov di, [si].TnwRegisters.&DI
    mov bp, [si].TnwRegisters.&BP
    mov bx, [si].TnwRegisters.&BX
    mov dx, [si].TnwRegisters.&DX
    mov cx, [si].TnwRegisters.&CX
    mov ax, [si].TnwRegisters.&AX
    mov es, [si].TnwRegisters.&ES
    pop ds
    pop si

  @@IntrCall:                                                   {!!.51}
    int 0                          {do the interrupt}           {!!.51}

    push ds                        {get ready for setting the}
    pushf                          {  TnwRegisters structure}
    push bp
    push si
    mov bp, sp                     {restore our stack frame}
    add bp, 10
    lds si, Regs                   {set up the TnwRegisters structure}
    mov [si].TnwRegisters.&DI, di
    mov [si].TnwRegisters.&BX, bx
    mov [si].TnwRegisters.&DX, dx
    mov [si].TnwRegisters.&CX, cx
    mov [si].TnwRegisters.&AX, ax
    mov [si].TnwRegisters.&ES, es
    pop ax
    mov [si].TnwRegisters.&SI, ax
    pop ax
    mov [si].TnwRegisters.&BP, ax
    pop ax
    mov [si].TnwRegisters.&Flags, ax
    pop ax
    mov [si].TnwRegisters.&DS, ax

    pop ds                      {restore Pascal's DS}
    xor ax, ax                  {return zero error}
  end;
{$ENDIF}

function nwNETXGetDriveTable(Func : word; var Table) : TnwErrorCode;
  {-Return one of the 3 drive tables. NETX ONLY.
    Func = $EF00 - Get Drive Handle Table
           $EF01 - Get Drive Flag Table
           $EF02 - Get Drive Connection ID}
  var
    Status : word;
    Regs   : TnwRegisters;
    P      : pointer;
  begin
    {Note: the drive tables are all 32 bytes long}
    nwInitRegs(Regs);
    Regs.ax := Func;
    Status := nwIntr($21, Regs);
    if (Status = 0) then
      with Regs do
        begin
          {$IFDEF DPMIorWnd}
          if (GetSelectorForRealMem(Ptr(ES, 0), SI+32, ES) <> 0) then
            begin
              nwNETXGetDriveTable := nwErrDPMI;
              Exit;
            end;
          {$ENDIF}
          P := Ptr(ES, SI);
          Move(P^, Table, 32);
          {$IFDEF DPMIorWnd}
          if (FreeLDTDescriptor(ES) <> 0) then
            {nothing};
          {$ENDIF}
        end;
    nwNETXGetDriveTable := Status;
  end;

procedure DefUpperStr(var S : string); far; assembler;
  {-Default routine to convert a string to uppercase in situ.}
  asm
    mov dx, ds                  {save Pascal's DS}
    lds si, S                   {get string}
    xor cx, cx                  {calc number of chars to uppercase}
    mov cl, [si]
    inc cx                      {for first time through}
  @@NextChar:
    dec cx                      {any more characters left?}
    jz @@Exit                   {no - exit}
    inc si                      {get next char into AL}
    mov al, [si]
    cmp al, 'a'                 {is next char less than 'a'?}
    jb @@NextChar               {yes - go get next char}
    cmp al, 'z'                 {is next char greater than 'z'?}
    ja @@NextChar               {yes - go get next char}
    sub al, 'a' - 'A'           {convert char to uppercase}
    mov [si], al                {store in string}
    jmp @@NextChar              {go get next char}
  @@Exit:
    mov ds, dx                  {restore Pascal's DS}
  end;

function vlmVersion(DestID   : word) : word;
  {-Return the version of VLM module DestID.}
  var
    Regs : TnwRegisters;
  begin
    vlmVersion := 0;                                            {!!.51}
    if (nwShellType = nsVLM) then                               {!!.51}
      begin                                                     {!!.51}
        {API: _Get VLM Version}
        nwInitRegs(Regs);
        if (vlmCall(DestID, $01, Regs) = 0) then
          vlmVersion := (Regs.BX * $100) + Regs.CX;
      end;                                                      {!!.51}
  end;

function nwShellType : TnwShellType;
  begin
    if not InitCalled then                                      {!!.51}
      InitNWBase;                                               {!!.51}
    nwShellType := InternalShellType;
  end;

function nwShellVersion : word;
  begin
    if not InitCalled then                                      {!!.51}
      InitNWBase;                                               {!!.51}
    nwShellVersion := InternalShellVer;
  end;

begin
  nwUpperStr := DefUpperStr;                                    {!!.51}
  InitCalled := false;                                          {!!.51}
  {$IFDEF CreateRealModeStack}                                  {!!.51}
  RealModeStackPtr := nil;                                      {!!.51}
  {$ENDIF}                                                      {!!.51}
  ExitSave := ExitProc;                                         {!!.51}
  ExitProc := @ExitNWBase;                                      {!!.51}
  {$IFDEF Windows}                                              {!!.52}
  NWCALLSLoaded := false;                                       {!!.52}
  {$ENDIF}                                                      {!!.52}
end.
