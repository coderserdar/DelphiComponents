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
  {$F-,V-,B-,S-,I-,R-}
  {$IFDEF CanAllowOverlays}
    {$F+,O+,A-}
  {$ENDIF}
  {$IFDEF CanSetOvrflowCheck}
    {$Q-}
  {$ENDIF}

  {$IFDEF DPMIorWnd}
    {$C FIXED PERMANENT DEMANDLOAD}
  {$ENDIF}

  {$UNDEF SmartCallbacks}
  {$IFDEF Windows}
  {$IFNDEF VER15}
   {$IFOPT K+}
    {$IFDEF Ver70}
     {$DEFINE SmartCallbacks}
    {$ENDIF}
    {$IFDEF Ver80}
     {$DEFINE SmartCallbacks}
    {$ENDIF}
   {$ENDIF}
  {$ENDIF}
  {$ENDIF}


{
  This unit implements support for NetBios name, Datagram and session
  functions.  These facilities can be used to implement peer to peer (station
  to station) communications.

  Supports NoWait operation with Post Routines.

  This unit was completely rewritten for B-Tree Filer version 5.50 to
  clean up the interface, especially between real mode and other targets,
  and also to follow the naming conventions of B-Tree Filer for C.

  Note that under Windows, the VNETBIOS driver in KERNEL takes care
  of calling the real mode NetBIOS driver, and so we don't need to.
}

{$IFDEF Win32}
  !! Error - this unit cannot be compiled for 32-bit
{$ENDIF}

unit NetBios;

interface

uses
  {$IFDEF Windows}
    WinProcs,
    WinTypes,
   {$IFDEF Ver80}
    DosSupp;
   {$ELSE}
    WinDos;
   {$ENDIF}
  {$ELSE}
    {$IFDEF DPMI}
      WinAPI,
      Dos,
      DPMI;
    {$ELSE}
      Dos;
    {$ENDIF}
  {$ENDIF}

const
  NetBiosIntr      = $5C;  {NetBios is accessed through int 5Ch}
  NetBiosInt21FC   = $2A;  {or DOS function 2Ah}
  MaxSessionCount  = 254;  {the max number of sessions allowed}

  NoWait           = $80;  {allows function calls to return immediately}

  NBNameMax        = 16;   {max number of chars in NetBIOS name string}

{===The NetBIOS function codes===}
const
  NBInvalidCommand   = $7F;  {an invalid NetBIOS call for testing}

  NBAddName          = $30;  {subfn to add a name}
  NBAddGroupName     = $36;  {subfn to add a group name}
  NBDeleteName       = $31;  {subfn to delete a name}
  NBFindName         = $78;  {subfn to find a name}

  NBResetWaitOnly    = $32;  {subfn to reset adpater}
  NBCancelWaitOnly   = $35;  {subfn to cancel pending NetBIOS call}

  NBCall             = $10;  {subfn to "call" another station}
  NBListen           = $11;  {subfn to listen for "call" from another station}
  NBHangUp           = $12;  {subfn to hangup a session}
  NBSend             = $14;  {subfn to send a packet}
  NBSendNoAck        = $71;
  NBChainSend        = $17;
  NBChainSendNoAck   = $72;
  NBReceive          = $15;  {subfn to receive a packet}
  NBReceiveAny       = $16;

  NBSendDatagram     = $20;  {subfn to send a datagram}
  NBReceiveDatagram  = $21;  {subfn to receive a datagram}
  NBSendBDatagram    = $22;  {subfn to send a broadcast datagram}
  NBReceiveBDatagram = $23;  {subfn to receive a datagram}

  NBAdapterStatus    = $33;

{===The NetBIOS Error Codes===}
  NBESuccess              = $00;        {successful operation}
  NBEInvalidBufferLength  = $01;
  NBEInvalidCommand       = $03;
  NBETimedOut             = $05;
  NBEIncomplete           = $06;
  NBELocalNoAckFailed     = $07;
  NBEInvalidLSN           = $08;
  NBENoResourceAvail      = $09;
  NBESessionClosed        = $0A;
  NBECommandCancelled     = $0B;
  NBEDuplicateName        = $0D;
  NBENameTableFull        = $0E;
  NBENameHasActive        = $0F;
  NBELocalSessionTableFull= $11;
  NBESessionNoListen      = $12;
  NBEIllegalNameNumber    = $13;
  NBECannotFindName       = $14;
  NBENoAnswer             = $14;
  NBEInvalidName          = $15;
  NBENameInUseOnRemote    = $16;
  NBENameDeleted          = $17;
  NBESessionAbnormal      = $18;
  NBENameConflict         = $19;
  NBEIncompatibleDevice   = $1A;
  NBEInterfaceBusy        = $21;
  NBETooManyCommands      = $22;
  NBEInvalidLanA          = $23;
  NBECompletedWhileCancel = $24;
  NBEReservedName         = $25;
  NBENotValidCancel       = $26;

  NBESystemError          = $40;
  NBEHotCarrierFromRemote = $41;
  NBEHotCarrier           = $42;
  NBENoCarrier            = $43;

  {50h-F6h INDICATE ADAPTER MALFUNCTION}

  NBECommandPending       = $FF;
  NBEDPMIError            = $FE; {general DPMI error, subclassed below}
  NBEUnexpectedAdaptClose = $FD;

  {Extra error codes for DPMI problems - for NetBiosDPMIErrorCode}
  NBEOutOfMemory = $01;      {No DOS addressable memory available}
  NBEBadPacket   = $02;      {Data packet is not real mode addressable}
  NBEBadNCB      = $03;      {NCB is not real mode addressable}

var
  NetBiosDPMIErrorCode : integer;
    {if a NETBIOS routine returns NBEDPMIError, then this variable
     will contain NBEOutOfMemory, NBEBadPacket, or NBEBadNCB}

{$IFDEF DPMI}
const
  NetBiosReenterError : boolean = false;
  {In protected mode the NetBIOS post event routines use a DPMI
   callback. Generally these are not reentrant. The NETBIOS unit
   traps any attempts at reentrancy and sets this flag before
   immediately returning. It is up to the programmer to reset it.}
{$ENDIF}

const
  DefaultAdapterNum : Byte = 0;         {the default adapter number}

type
  NBNameStr = String[NBNameMax];  {type for NetBIOS names}
  CallNameType = Array [1..NBNameMax] of Char;
                                  {used by NetBIOS internally for names}

  {Pointer to NetBIOS Control Block}
  PNCB = ^TNCB;

  {The NetBIOS Post Routine procedure type definition}
  NetBiosPostRoutine = procedure(LastError : Byte; N : PNCB);

  {The NetBIOS Control Block}
  TNCB = record
    Command     : Byte;          {the NetBIOS command}
    RetCode     : Byte;          {the return code}
    LSN         : Byte;          {the local session num}
    NameNum     : Byte;          {the NetBIOS name table num}
    Buffer      : Pointer;       {point to data buffer}
    BufLen      : Word;          {length of the data buffer}
    RemName     : CallNameType;  {the remote name}
    LocName     : CallNameType;  {the local name}
    RTO         : Byte;          {Receive Time Out}
    STO         : Byte;          {Send Time Out}
    PostRoutine : Pointer;       {the post event routine}
    LanANum     : Byte;          {the LAN adapter number}
    CmdComplete : Byte;          {the command complete flag}
    Reserved    : Array[1..14] of Byte; {filler to make 64 bytes in all}
    {$IFDEF Windows}
    PostEvProc  : NetBiosPostRoutine;
    {$IFDEF SmartCallbacks}
    OurDS       : word;
    {$ENDIF}
    {$ENDIF}
  end;

  {The NetBIOS post routine handler definition}
  PPostHandler = ^TPostHandler;
  {$IFDEF DPMI}
  RealModeCallbackProc = pointer;
  TPostHandler = record
    Regs     : DPMIRegisters;
    TempRegs : DPMIRegisters;
    Post     : NetBiosPostRoutine;
    CallBack : RealModeCallbackProc;
    Sele     : Word;                                          {!!.51}
    OurDS    : Word;
    InUse    : Boolean;
  end;
  {$ELSE}
  TPostHandler = record
    OurDS       : Word;
    Post        : NetBiosPostRoutine;
    {$IFDEF MSDOS}
    IntrHandler : array [0..21] of Byte;
    {$ENDIF}
    CallBack    : Pointer;
  end;
  {$ENDIF}

  {The NetBIOS data packet or datagram}
  PnbPacket = pointer;

  {NetBIOS status information - returned by NetBiosInfo}
  NetBiosName = record
    Name   : CallNameType;
    Number : Byte;
    Status : Byte;
  end;
  PNetBiosStatus = ^TNetBiosStatus;
  TNetBiosStatus = record
    PermanentNodeName  : array [1..6] of char;  {hardware node name}
    ExtJumpers         : byte;
    SelfTest           : byte;
    ProtocolMajor      : byte;          {version number - major}
    ProtocolMinor      : byte;          {..and minor}
    ReportingPeriod    : word;          {dynamic status of driver}
    CRCCount           : word;
    AlignmentErrors    : word;
    Collisions         : word;
    TransmitAborts     : word;
    Transmits          : longint;
    Receives           : longint;
    Retransmits        : word;
    ResourceDepletion  : word;
    ReservedArea1      : array [1..8] of byte;
    FreeCommandBlocks  : word;          {available pending commands}
    CurrentMaxNCBs     : word;          {max NCBs driver configured for}
    HardwareMaxNCBs    : word;          {max NCBs driver can support}
    ReservedArea2      : array [1..4] of byte;
    Sessions           : word;          {current number of active sessions}
    CurrentMaxSessions : word;          {max sessions driver configured for}
    HardwareMaxSessions: word;          {max sessions driver can support}
    MaxPacketSize      : word;          {largest packet in bytes}
    NameCount          : word;          {number of names in adapter table}
    NetBiosNames       : array [1..16] of NetBiosName;
                                        {start of variable length array}
  end;


{===NetBIOS memory and routine allocation services===}
function NetBiosAllocNCB : PNCB;
  {-Allocate memory for an NCB}

function NetBiosAllocPacket(Size : word) : PnbPacket;
  {-Allocate memory for a packet}

function NetBiosAllocPost(Handler : NetBiosPostRoutine) : PPostHandler;
  {-Allocates and returns a post-event handler}

procedure NetBiosClearNCB(N : PNCB);
  {-Initializes an NCB to zero. Required before using an NCB.}

procedure NetBiosFreeNCB(N : PNCB);
  {-Frees an NCB allocated with NetBiosAllocNCB}

procedure NetBiosFreePacket(P : PnbPacket);
  {-Frees a packet P allocated with NetBiosAllocPacket}

procedure NetBiosFreePost(P : PPostHandler);
  {-Frees a post-event handler allocated with NetBiosAllocPost}


{===NetBIOS name services===}
function NetBiosAddGroupName(NameToAdd : NBNameStr;
                             var NameNumber : Byte) : Byte;
  {-Adds a group name to the local NetBIOS name table}

function NetBiosAddName(NameToAdd : NBNameStr;
                        var NameNumber : Byte) : Byte;
  {-Adds a name to the local NetBIOS name table}

function NetBiosDeleteName(NameToDelete : NBNameStr) : Byte;
  {-Deletes a name or group name from the local NetBIOS name table}


{===NetBIOS session initialisation and closedown===}
function NetBiosHangUp(SessionNumber : Byte) : Byte;
  {-Closes an existing session}

function NetBiosListen(RemoteName, LocalName : NBNameStr;
                       SendTimeOut, RecTimeOut : Byte;
                       var SessionNumber : Byte) : Byte;
procedure NetBiosListenNoWait(N : PNCB;
                              PostEvent : PPostHandler;
                              RemoteName, LocalName : NBNameStr;
                              SendTimeOut, RecTimeOut : Byte);
  {-Listens for a "calls" from another station to initiate a session, the
    first routine waits for the connection and the second does not.}

function NetBiosOpen(RemoteName, LocalName : NBNameStr;
                     SendTimeOut, RecTimeOut : Byte;
                     var SessionNumber : Byte) : Byte;
procedure NetBiosOpenNoWait(N : PNCB;
                            PostEvent : PPostHandler;
                            RemoteName, LocalName : NBNameStr;
                            SendTimeOut, RecTimeOut : Byte);
  {-"Calls" another station to initiate a session, the first routine waits
    for the connection and the second does not.}


{===NetBIOS session send/receive services===}
function NetBiosCancelRequest(N : PNCB) : Byte;
  {-Cancels a pending NetBIOS request}

function NetBiosReceive(SessionNumber : Byte;
                        PacketSize    : Word;
                        Packet        : PnbPacket) : Byte;
procedure NetBiosReceiveNoWait(N         : PNCB;
                               PostEvent : PPostHandler;
                               SessionNumber : Byte;
                               PacketSize    : Word;
                               Packet        : PnbPacket);
  {-Receives a packet via NetBIOS session, the first routine waits until
    event complete, the second does not}

function NetBiosSend(SessionNumber : Byte;
                     PacketSize    : Word;
                     Packet        : PnbPacket) : Byte;
procedure NetBiosSendNoWait(N         : PNCB;
                            PostEvent : PPostHandler;
                            SessionNumber : Byte;
                            PacketSize    : Word;
                            Packet        : PnbPacket);
  {-Sends a packet via NetBIOS session, the first routine waits until the
    event is complete, the second does not}


{===NetBIOS datagram services===}
function NetBiosReceiveBDG(ReceiverNameNum : Byte;
                           DatagramSize    : Word;
                           Datagram        : PnbPacket) : Byte;
procedure NetBiosReceiveBDGNoWait(N               : PNCB;
                                  PostEvent       : PPostHandler;
                                  ReceiverNameNum : Byte;
                                  DatagramSize    : Word;
                                  Datagram        : PnbPacket);
  {-Receive a broadcast datagram, with or without a wait for the event
    to complete}

function NetBiosReceiveDG(ReceiverNameNum : Byte;
                          DatagramSize    : Word;
                          Datagram        : PnbPacket) : Byte;
procedure NetBiosReceiveDGNoWait(N               : PNCB;
                                 PostEvent       : PPostHandler;
                                 ReceiverNameNum : Byte;
                                 DatagramSize    : Word;
                                 Datagram        : PnbPacket);
  {-Receive a datagram, with or without a wait for the event to complete}

function NetBiosSendBDG(SenderNameNum : Byte;
                        DatagramSize  : Word;
                        Datagram      : PnbPacket) : Byte;
procedure NetBiosSendBDGNoWait(N             : PNCB;
                               PostEvent     : PPostHandler;
                               SenderNameNum : Byte;
                               DatagramSize  : Word;
                               Datagram      : PnbPacket);
  {-Send a broadcast datagram, with or without a wait for the event to
    complete}

function NetBiosSendDG(SenderNameNum : Byte;
                       ReceiverName  : NBNameStr;
                       DatagramSize  : Word;
                       Datagram      : PnbPacket) : Byte;
procedure NetBiosSendDGNoWait(N             : PNCB;
                              PostEvent     : PPostHandler;
                              SenderNameNum : Byte;
                              ReceiverName  : NBNameStr;
                              DatagramSize  : Word;
                              Datagram      : PnbPacket);
  {-Send a datagram, with or without a wait for the event to complete}


{===NetBIOS miscellaneous===}
function NetBiosCmdCompleted(N : PNCB; var FinalRetCode : Byte) : Boolean;
  {-Returns True if NetBIOS call made in NoWait state has been completed.
    If the event is complete, the final return code for the call is
    returned.}

function NetBiosInfo(NS : PNetBiosStatus;
                     Name : NBNameStr;
                     MaxNames : byte) : byte;
  {-Returns information about a NetBIOS driver or adapter}

function NetBiosInstalled : Boolean;
  {-Returns TRUE if NetBIOS is installed}

procedure NetBiosRequest(N : PNCB);
  {-Calls the NetBIOS driver with the given NCB}
  {$IFNDEF DPMI}
    inline($5B/                   {pop bx}
           $07/                   {pop es}
           $1E/                   {push ds}
           $CD/NetBiosIntr/       {int $5C}
           $1F);                  {pop ds}
  {$ENDIF}

function NetBiosResetAdapter(SessionCount : Byte;
                             CommandCount : Byte) : Byte;
  {-Reset the NETBIOS adapter given by DefaultAdapterNum.}


implementation

type
  LH = record L, H : word; end; {split a longint into lo/hi words}
  OS = record O, S : word; end; {split a pointer into Sel/Seg and Ofs}

{Note: to isolate the different usage of packets in protected mode
programming and in real mode programming (basically we *must* have a
DOS memory block in protected mode, and it's the real mode pointer
to the block that is passed to NetBIOS), NetBiosAllocPacket will
return the address of a block of memory that follows directly on from
an 8 byte block of the form TPacketInfo. By this 'trick' we can ensure
packets (or datagrams) passed to the relevant NetBios routines in
protected mode have been properly allocated, and also the code for
calling these routines is the same in real mode and protected mode.
Windows programmers are spoilt because the KERNEL DLL will provide the
pmode to real mode mappings for us, so they can just 'pretend' to be
real mode programmers.}
type
  PPacketInfo = ^TPacketInfo;
  TPacketInfo = record         {THIS MUST BE EIGHT (8) BYTES}
    Sig     : word;            {Always 'PI' back-words}
    RealPtr : pointer;         {DPMI real-mode pointer to data}
    DSize   : word;            {size of data block}
  end;
const
  PacketSig = $4950;           {'PI' back-words}

{---utility routines---}
function nbHeapError(Size : word) : integer; far;
  {-Simple heap error function for nbGetMem}
  begin
    nbHeapError := 1;
  end;

function nbGetMem(var P; Size : word) : boolean; near;
  {-Safe memory allocation, return true is successful}
  var
    Pt : pointer absolute P;
    SaveHeapError : pointer;
  begin
    SaveHeapError := HeapError;
    HeapError := @nbHeapError;
    GetMem(Pt, Size);
    nbGetMem := Pt <> nil;
    HeapError := SaveHeapError;
  end;

{$IFDEF DPMI}
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

{$IFDEF DPMI}
procedure NetBiosCallBackShell; far; assembler;
  { ES:DI points to the DPMI register structure }
  {       (this will be the one at the top of TPostHandler)}
  { DS:SI points to the real mode's SS:SP }
  const
    SizeOfDPMIRegs = sizeof(DPMIRegisters);
  asm
    {Entering a critical section}
    mov ax, 1681h
    int 2Fh
    {Are we reentering?}
    cmp es:[di].TPostHandler.InUse, 1
    je @@TryingToReenter
    {Make sure we can't be reentered}
    mov es:[di].TPostHandler.InUse, 1
    {Fix the DPMI registers for our return}
    cld
    lodsw; mov es:[di].TPostHandler.Regs.&IP, ax
    lodsw; mov es:[di].TPostHandler.Regs.&CS, ax
    lodsw; mov es:[di].TPostHandler.Regs.&Flags, ax
    add es:[di].TPostHandler.Regs.&SP, 6
    {Copy the DPMI registers Regs to TempRegs}
    mov bx, di
    mov si, di
    mov cx, SizeOfDPMIRegs
    add di, cx
    mov ax, es; mov ds, ax
    push es; push di; push bx
    shr cx, 1
    rep movsw
    {Leaving critical section, turn on interrupts}
    mov ax, 1682h
    int 2Fh
    mov ax, 0901h
    int 31h
    {Set up our DS}                                        {!!.51 start}
    mov ds, es:[bx].TPostHandler.OurDS
    {Remap our selector onto the NCB}
    push bx
    mov dx, es:[bx].TPostHandler.TempRegs.&ES
    xor ax, ax
    mov al, dh
    mov cl, 4
    shr ax, cl
    shl dx, cl
    xchg ax, cx
    mov bx, es:[bx].TPostHandler.Sele
    mov ax, $7
    int $31
    xchg ax, bx
    pop bx
    {Push the parameters for the post routine and call it}
    push word ptr es:[bx].TPostHandler.TempRegs.&AX
    push ax
    push word ptr es:[bx].TPostHandler.TempRegs.&BX        {!!.51 end}
    call dword ptr es:[bx].TPostHandler.Post
    {Get the TPostHandler back, turn off interrupts, reset InUse flag}
    pop bx; pop di; pop es
    mov ax, 0900h
    int 31h
    mov es:[bx].TPostHandler.InUse, 0
    iret

  @@TryingToReenter:
    {The callback is already active, so get out NOW}
    cld
    lodsw; mov es:[di].TPostHandler.Regs.&IP, ax
    lodsw; mov es:[di].TPostHandler.Regs.&CS, ax
    lodsw; mov es:[di].TPostHandler.Regs.&Flags, ax
    add es:[di].TPostHandler.Regs.&SP, 6
    mov ds, es:[di].TPostHandler.OurDS
    mov NetBiosReenterError, 1
    mov ax,1682h
    int 2Fh
    iret
  end;
{$ENDIF}

{$IFDEF MSDOS}
const
  {This interrupt routine gets copied into every post-event routine's
   control block by NetBiosAllocPost. It provides a callback to allow
   a normal Pascal routine to be called by NetBIOS.}
  NetBiosIntrCode : array [0..21] of byte
    = ($E8, $00, $00,    {     call @@1         } {Get this code's offset}
       $5F,              {@@1: pop di           } {..into DI}
       $50, $06, $53,    {     push ax, es, bx  } {push handler's parameters}
       $83, $EF, $09,    {     sub di, 9        } {point to struc's DataSeg}
       $2E, $8E, $1D,    {     mov ds, cs:[di]  } {set up DS}
       $83, $C7, $02,    {     add di, 2        } {point to handler}
       $2E, $FF, $1D,    {     call far cs:[di] } {call handler}
       $CF,              {     iret             } {return to NetBIOS}
       $90, $90);        {..filler nop bytes    }
{$ENDIF}

{$IFDEF Windows}
var
  WindowsPostEventHandler : pointer;

procedure WinPost; export; assembler;
  asm
    {Note: smart callbacks don't work with the VNETBIOS driver that's in
           KERNEL (I'm not sure why) because the callback gets called with
           an SS value that's not the application's SS. As SS gets copied
           into DS for our data segment, we need to patch our own DS value.}
    {$IFDEF SmartCallbacks}
    mov ax, es:[bx].TNCB.OurDS
    mov ds, ax
    {$ENDIF}
    {Note: the instance thunk will trash AX. As this is where NetBIOS
           puts the return code, we must get it again from the value
           in the NCB for the user's postevent handler.}
    xor ax, ax
    mov al, es:[bx].TNCB.RetCode
    push ax
    push es
    push bx
    call es:[bx].TNCB.PostEvProc
    {Note: the VNETBIOS driver allows us to do a far return rather than
           the more usual IRET. Handy: we don't have to duplicate the
           normal routine exit code.}
  end;
{$ENDIF}

function GetPtrForDriver(Packet : PnbPacket) : pointer;
  {-Returns a pointer for Packet that the NetBIOS driver can use.}
  {$IFDEF DPMI}
  assembler;
  asm
    les bx, Packet
    sub bx, 6
    mov ax, es:[bx]
    mov dx, es:[bx+2]
  end;
  {$ELSE}
  inline($58/$5A); {pop ax; pop dx}
  {$ENDIF}

function IsAnotherPacketType(Packet : PnbPacket) : boolean;
  {-Returns true if Packet was *not* allocated by NetBiosAllocPacket}
  {$IFDEF DPMI}
  assembler;
  asm
    mov ax, 1                           {assume true, ie it's not ours}
    mov bx, Packet.Word[0]              {get offset}
    sub bx, 8                           {subtract sizeof(TPacketInfo)}
    jl @@Exit                           {if negative not ours, so exit}
    mov es, Packet.Word[2]              {get segment/selector}
    cmp word ptr es:[bx], PacketSig     {1st word should be signature}
    jne @@Exit                          {no, so not ours}
    xor ax, ax                          {it's our type, return false}
  @@Exit:
  end;
  {$ELSE}
  begin
    IsAnotherPacketType := true;
    if (OS(Packet).O >= sizeof(TPacketInfo)) then
      begin
        dec(OS(Packet).O, sizeof(TPacketInfo));
        if (PPacketInfo(Packet)^.Sig = PacketSig) then
          IsAnotherPacketType := false;
      end;
  end;
  {$ENDIF}

function NetBiosAllocPacket(Size : word) : PnbPacket;
  var
    P : PPacketInfo;
    R : pointer;
  begin
    {$IFDEF DPMI}
    if DOSGetMem(R, P, Size+sizeof(TPacketInfo)) then
      begin
        with P^ do
          begin
            Sig := PacketSig;
            RealPtr := R;
            inc(OS(RealPtr).O, sizeof(TPacketInfo));
            DSize := Size;
          end;
        inc(OS(P).O, sizeof(TPacketInfo));
      end
    else P := nil;
    {$ELSE}
    if nbGetMem(P, Size+sizeof(TPacketInfo)) then
      begin
        with P^ do
          begin
            Sig := PacketSig;
            RealPtr := P;
            inc(OS(RealPtr).O, sizeof(TPacketInfo));
            DSize := Size;
          end;
        P := P^.RealPtr;
      end;
    {$ENDIF}
    NetBiosAllocPacket := PnbPacket(P);
  end;

procedure NetBiosFreePacket(P : PnbPacket);
  var
    Size  : word;
    TempP : PPacketInfo;
  begin
    if not IsAnotherPacketType(P) then
      begin
        TempP := P;
        dec(OS(TempP).O, sizeof(TPacketInfo));
        Size := TempP^.DSize+sizeof(TPacketInfo);
        {trash the memory block to ensure that it cannot inadvertently
         be used again without reallocation}
        FillChar(TempP^, Size, $CC);
        {$IFDEF DPMI}
        if not DOSFreeMem(TempP) then
          {nothing};
        {$ELSE}
        FreeMem(TempP, Size);
        {$ENDIF}
      end;
  end;

procedure NetBiosClearNCB(N : PNCB);
  begin
    if (N <> nil) then
      begin
        FillChar(N^, SizeOf(TNCB), 0);     {fill the NCB with 0}
        N^.LanANum := DefaultAdapterNum;
      end;
  end;

function NetBiosAllocNCB : PNCB;
  begin
    NetBiosAllocNCB := PNCB(NetBiosAllocPacket(sizeof(TNCB)));
  end;

procedure NetBiosFreeNCB(N : PNCB);
  begin
    NetBiosFreePacket(PnbPacket(N));
  end;

function NetBiosAllocPost(Handler : NetBiosPostRoutine) : PPostHandler;
  var
    P : PPostHandler;
    Dummy : word;
  begin
    if nbGetMem(P, sizeof(TPostHandler)) then
      with P^ do
        begin
          Post := Handler;
          OurDS := DSeg;
          {$IFDEF DPMI}
          InUse := false;
          FillChar(Regs, sizeof(Regs), 0);
          if (AllocRealModeCallBackAddr(@NetBiosCallBackShell, Regs, Callback) <> 0) then
            begin
              {no callbacks left - dispose of the memory block}
              Dispose(P);
              P := nil;
            end;
          if (AllocLDTDescriptors(1, Sele) <> 0) then          {!!.51 start}
            begin
              {out of selectors-yuk}
              Dummy := FreeRealModeCallbackAddr(Callback);
              Dispose(P);
              P := nil;
            end
          else
            begin
              if (SetSegmentLimit(Sele, sizeof(TNCB) + sizeof(TPacketInfo)) <> 0) then
                begin
                  {Error resizing the selector-unusual}
                  Dummy := FreeLDTDescriptor(Sele);
                  Dummy := FreeRealModeCallbackAddr(Callback);
                  Dispose(P);
                  P := nil;
                end;
            end;                                               {!!.51 end}
          {$ELSE}
          {$IFDEF MSDOS}
          Move(NetBiosIntrCode, IntrHandler, sizeof(IntrHandler));
          CallBack := @IntrHandler;
          {$ELSE} {ie Windows}
          CallBack := WindowsPostEventHandler;
          {$ENDIF}
          {$ENDIF}
        end;
    NetBiosAllocPost := P;
  end;

procedure NetBiosFreePost(P : PPostHandler);
  var
    Dummy : word;
  begin
    if (P <> nil) then
      begin
        {$IFDEF DPMI}
        Dummy := FreeRealModeCallbackAddr(P^.Callback);
        Dummy := FreeLDTDescriptor(P^.Sele);                   {!!.51}
        {$ENDIF}
        Dispose(P);
      end;
  end;

function NetBiosCancelRequest(N : PNCB) : Byte;
  var
    TempNCB : PNCB;
    NCBBuffer : TNCB;
  begin
    {$IFDEF DPMI}
    if IsAnotherPacketType(PnbPacket(N)) then
      begin
        NetBiosDPMIErrorCode := NBEBadPacket;
        NetBiosCancelRequest := NBEDPMIError;
        Exit;
      end;
    TempNCB := NetBiosAllocNCB;
    if (TempNCB = nil) then
      begin
        NetBiosDPMIErrorCode := NBEOutOfMemory;
        NetBiosCancelRequest := NBEDPMIError;
        Exit;
      end;
    {$ELSE}
    TempNCB := @NCBBuffer;
    {$ENDIF}
    NetBiosClearNCB(TempNCB);
    {Build the NCB for the cancel request. Note the buffer is pointed
    at the NCB to cancel}
    with TempNCB^ do
      begin
        Command := NBCancelWaitOnly;    {this function always waits}
        Buffer := GetPtrForDriver(N);
      end;
    {call the NetBIOS}
    NetBiosRequest(TempNCB);
    {the return value is the NetBIOS RetCode}
    with TempNCB^ do
      NetBiosCancelRequest := RetCode;
    {$IFDEF DPMI}
    NetBiosFreeNCB(TempNCB);
    {$ENDIF}
  end;

function NetBiosAddName(NameToAdd : NBNameStr;
                        var NameNumber : Byte) : Byte;
  var
    TempNCB : PNCB;
    NCBBuffer : TNCB;
  begin
    {$IFDEF DPMI}
    TempNCB := NetBiosAllocNCB;
    if (TempNCB = nil) then
      begin
        NetBiosDPMIErrorCode := NBEOutOfMemory;
        NetBiosAddName := NBEDPMIError;
        Exit;
      end;
    {$ELSE}
    TempNCB := @NCBBuffer;
    {$ENDIF}
    NetBiosClearNCB(TempNCB);
    {build the NCB}
    with TempNCB^ do
      begin
        Command := NBAddName;
        {set the name: note that it will automatically be padded with
         nulls because of the NetBiosClearNCB.}
        Move(NameToAdd[1], LocName, Length(NameToAdd));
      end;
    {call the NetBIOS}
    NetBiosRequest(TempNCB);
    {return result code, and number of name in table}
    with TempNCB^ do
      begin
        NetBiosAddName := RetCode;
        NameNumber := NameNum;
      end;
    {$IFDEF DPMI}
    NetBiosFreeNCB(TempNCB);
    {$ENDIF}
  end;

function NetBiosAddGroupName(NameToAdd : NBNameStr;
                             var NameNumber : Byte) : Byte;
  var
    TempNCB : PNCB;
    NCBBuffer : TNCB;
  begin
    {$IFDEF DPMI}
    TempNCB := NetBiosAllocNCB;
    if (TempNCB = nil) then
      begin
        NetBiosDPMIErrorCode := NBEOutOfMemory;
        NetBiosAddGroupName := NBEDPMIError;
        Exit;
      end;
    {$ELSE}
    TempNCB := @NCBBuffer;
    {$ENDIF}
    NetBiosClearNCB(TempNCB);
    {build the NCB}
    with TempNCB^ do
      begin
        Command := NBAddGroupName;
        {set the name: note that it will automatically be padded with
         nulls because of the NetBiosClearNCB.}
        Move(NameToAdd[1], LocName, Length(NameToAdd));
      end;
    {call the NetBIOS}
    NetBiosRequest(TempNCB);
    {return result code, and number of name in table}
    with TempNCB^ do
      begin
        NetBiosAddGroupName := RetCode;
        NameNumber := NameNum;
      end;
    {$IFDEF DPMI}
    NetBiosFreeNCB(TempNCB);
    {$ENDIF}
  end;

function NetBiosDeleteName(NameToDelete : NBNameStr) : Byte;
  var
    TempNCB : PNCB;
    NCBBuffer : TNCB;
  begin
    {$IFDEF DPMI}
    TempNCB := NetBiosAllocNCB;
    if (TempNCB = nil) then
      begin
        NetBiosDPMIErrorCode := NBEOutOfMemory;
        NetBiosDeleteName := NBEDPMIError;
        Exit;
      end;
    {$ELSE}
    TempNCB := @NCBBuffer;
    {$ENDIF}
    NetBiosClearNCB(TempNCB);
    {build the NCB}
    with TempNCB^ do
      begin
        Command := NBDeleteName;
        {set the name: note that it will automatically be padded with
         nulls because of the NetBiosClearNCB.}
        Move(NameToDelete[1], LocName, Length(NameToDelete));
      end;
    {call the NetBIOS}
    NetBiosRequest(TempNCB);
    {return result code}
    with TempNCB^ do
      NetBiosDeleteName := RetCode;
    {$IFDEF DPMI}
    NetBiosFreeNCB(TempNCB);
    {$ENDIF}
  end;

function NetBiosResetAdapter(SessionCount : Byte;
                             CommandCount : Byte) : Byte;
  var
    TempNCB : PNCB;
    NCBBuffer : TNCB;
  begin
    {$IFDEF DPMI}
    TempNCB := NetBiosAllocNCB;
    if (TempNCB = nil) then
      begin
        NetBiosDPMIErrorCode := NBEOutOfMemory;
        NetBiosResetAdapter := NBEDPMIError;
        Exit;
      end;
    {$ELSE}
    TempNCB := @NCBBuffer;
    {$ENDIF}
    NetBiosClearNCB(TempNCB);
    {build the NCB}
    with TempNCB^ do
      begin
        Command   := NBResetWaitOnly;
        LSN       := SessionCount;      {the max sessions}
        NameNum   := CommandCount;      {the max pending commands}
      end;
    {call the NetBIOS}
    NetBiosRequest(TempNCB);
    {return result code}
    with TempNCB^ do
      NetBiosResetAdapter := RetCode;
    {$IFDEF DPMI}
    NetBiosFreeNCB(TempNCB);
    {$ENDIF}
  end;

procedure ReportBadNCB(N : PNCB);
  begin
    NetBiosDPMIErrorCode := NBEBadNCB;
    if (N <> nil) then
      begin
        NetBiosClearNCB(N);
        N^.RetCode := NBEDPMIError;
      end;
  end;

function GetPostRoutine(P : PPostHandler; N : PNCB) : pointer;
  begin
    if (P = nil) then
      GetPostRoutine := nil
    else
      begin
        GetPostRoutine := P^.CallBack;
        {$IFDEF Windows}
        N^.PostEvProc := P^.Post;
        {$IFDEF SmartCallbacks}
        N^.OurDS := P^.OurDS;
        {$ENDIF}
        {$ENDIF}
      end;
  end;

function NetBiosOpen(RemoteName, LocalName : NBNameStr;
                     SendTimeOut, RecTimeOut : Byte;
                     var SessionNumber : Byte) : Byte;
  var
    TempNCB : PNCB;
    NCBBuffer : TNCB;
  begin
    {$IFDEF DPMI}
    TempNCB := NetBiosAllocNCB;
    if (TempNCB = nil) then
      begin
        NetBiosDPMIErrorCode := NBEOutOfMemory;
        NetBiosOpen := NBEDPMIError;
        Exit;
      end;
    {$ELSE}
    TempNCB := @NCBBuffer;
    {$ENDIF}
    NetBiosClearNCB(TempNCB);
    {build the NCB}
    with TempNCB^ do
      begin
        Command := NBCall;
        Move(LocalName[1], LocName, Length(LocalName));
        Move(RemoteName[1], RemName, Length(RemoteName));
        RTO := RecTimeOut;
        STO := SendTimeOut;
      end;
    {call the NetBIOS}
    NetBiosRequest(TempNCB);
    {return result code}
    with TempNCB^ do
      begin
        NetBiosOpen := RetCode;
        SessionNumber := LSN;
      end;
    {$IFDEF DPMI}
    NetBiosFreeNCB(TempNCB);
    {$ENDIF}
  end;

procedure NetBiosOpenNoWait(N : PNCB;
                            PostEvent : PPostHandler;
                            RemoteName, LocalName : NBNameStr;
                            SendTimeOut, RecTimeOut : Byte);
  begin
    {$IFDEF DPMI}
    if IsAnotherPacketType(N) then
      begin
        ReportBadNCB(N);
        Exit;
      end;
    {$ENDIF}
    NetBiosClearNCB(N);
    {build the NCB}
    with N^ do
      begin
        Command := NBCall + NoWait;
        Move(LocalName[1], LocName, Length(LocalName));
        Move(RemoteName[1], RemName, Length(RemoteName));
        RTO := RecTimeOut;
        STO := SendTimeOut;
        PostRoutine := GetPostRoutine(PostEvent, N);
      end;
    {call the NetBIOS}
    NetBiosRequest(N);
  end;


function NetBiosListen(RemoteName, LocalName : NBNameStr;
                       SendTimeOut, RecTimeOut : Byte;
                       var SessionNumber : Byte) : Byte;
  var
    TempNCB : PNCB;
    NCBBuffer : TNCB;
  begin
    {$IFDEF DPMI}
    TempNCB := NetBiosAllocNCB;
    if (TempNCB = nil) then
      begin
        NetBiosDPMIErrorCode := NBEOutOfMemory;
        NetBiosListen := NBEDPMIError;
        Exit;
      end;
    {$ELSE}
    TempNCB := @NCBBuffer;
    {$ENDIF}
    NetBiosClearNCB(TempNCB);
    {build the NCB}
    with TempNCB^ do
      begin
        Command := NBListen;
        Move(LocalName[1], LocName, Length(LocalName));
        Move(RemoteName[1], RemName, Length(RemoteName));
        RTO := RecTimeOut;
        STO := SendTimeOut;
      end;
    {call the NetBIOS}
    NetBiosRequest(TempNCB);
    {return result code}
    with TempNCB^ do
      begin
        NetBiosListen := RetCode;
        SessionNumber := LSN;
      end;
    {$IFDEF DPMI}
    NetBiosFreeNCB(TempNCB);
    {$ENDIF}
  end;

procedure NetBiosListenNoWait(N : PNCB;
                              PostEvent : PPostHandler;
                              RemoteName, LocalName : NBNameStr;
                              SendTimeOut, RecTimeOut : Byte);
  begin
    {$IFDEF DPMI}
    if IsAnotherPacketType(N) then
      begin
        ReportBadNCB(N);
        Exit;
      end;
    {$ENDIF}
    NetBiosClearNCB(N);
    {build the NCB}
    with N^ do
      begin
        Command := NBListen + NoWait;
        Move(LocalName[1], LocName, Length(LocalName));
        Move(RemoteName[1], RemName, Length(RemoteName));
        RTO := RecTimeOut;
        STO := SendTimeOut;
        PostRoutine := GetPostRoutine(PostEvent, N);
      end;
    {call the NetBIOS}
    NetBiosRequest(N);
  end;

function NetBiosHangUp(SessionNumber : Byte) : Byte;
  var
    TempNCB : PNCB;
    NCBBuffer : TNCB;
  begin
    {$IFDEF DPMI}
    TempNCB := NetBiosAllocNCB;
    if (TempNCB = nil) then
      begin
        NetBiosDPMIErrorCode := NBEOutOfMemory;
        NetBiosHangUp := NBEDPMIError;
        Exit;
      end;
    {$ELSE}
    TempNCB := @NCBBuffer;
    {$ENDIF}
    NetBiosClearNCB(TempNCB);
    {build the NCB}
    with TempNCB^ do
      begin
        Command := NBHangUp;
        LSN := SessionNumber;   {The local session number is all that's needed}
      end;
    {call the NetBIOS}
    NetBiosRequest(TempNCB);
    {return result code}
    with TempNCB^ do
      NetBiosHangUp := RetCode;
    {$IFDEF DPMI}
    NetBiosFreeNCB(TempNCB);
    {$ENDIF}
  end;

function NetBiosReceive(SessionNumber : Byte;
                        PacketSize    : Word;
                        Packet        : PnbPacket) : Byte;
  var
    TempNCB : PNCB;
    NCBBuffer : TNCB;
  begin
    {$IFDEF DPMI}
    if IsAnotherPacketType(Packet) then
      begin
        NetBiosDPMIErrorCode := NBEBadPacket;
        NetBiosReceive := NBEDPMIError;
        Exit;
      end;
    TempNCB := NetBiosAllocNCB;
    if (TempNCB = nil) then
      begin
        NetBiosDPMIErrorCode := NBEOutOfMemory;
        NetBiosReceive := NBEDPMIError;
        Exit;
      end;
    {$ELSE}
    TempNCB := @NCBBuffer;
    {$ENDIF}
    NetBiosClearNCB(TempNCB);
    {build the NCB}
    with TempNCB^ do
      begin
        Command := NBReceive;
        LSN := SessionNumber;
        Buffer := GetPtrForDriver(Packet);
        BufLen := PacketSize;
      end;
    {call the NetBIOS}
    NetBiosRequest(TempNCB);
    {return result code}
    with TempNCB^ do
      NetBiosReceive := RetCode;
    {$IFDEF DPMI}
    NetBiosFreeNCB(TempNCB);
    {$ENDIF}
  end;

procedure NetBiosReceiveNoWait(N         : PNCB;
                               PostEvent : PPostHandler;
                               SessionNumber : Byte;
                               PacketSize    : Word;
                               Packet        : PnbPacket);
  begin
    {$IFDEF DPMI}
    if IsAnotherPacketType(N) then
      begin
        ReportBadNCB(N);
        Exit;
      end;
    {$ENDIF}
    NetBiosClearNCB(N);
    {$IFDEF DPMI}
    if IsAnotherPacketType(Packet) then
      begin
        NetBiosDPMIErrorCode := NBEBadPacket;
        N^.RetCode := NBEDPMIError;
        Exit;
      end;
    {$ENDIF}
    {build the NCB}
    with N^ do
      begin
        Command := NBReceive + NoWait;
        LSN := SessionNumber;
        Buffer := GetPtrForDriver(Packet);
        BufLen := PacketSize;
        PostRoutine := GetPostRoutine(PostEvent, N);
      end;
    {call the NetBIOS}
    NetBiosRequest(N);
  end;

function NetBiosSend(SessionNumber : Byte;
                     PacketSize    : Word;
                     Packet        : PnbPacket) : Byte;
  var
    TempNCB : PNCB;
    NCBBuffer : TNCB;
  begin
    {$IFDEF DPMI}
    if IsAnotherPacketType(Packet) then
      begin
        NetBiosDPMIErrorCode := NBEBadPacket;
        NetBiosSend := NBEDPMIError;
        Exit;
      end;
    TempNCB := NetBiosAllocNCB;
    if (TempNCB = nil) then
      begin
        NetBiosDPMIErrorCode := NBEOutOfMemory;
        NetBiosSend := NBEDPMIError;
        Exit;
      end;
    {$ELSE}
    TempNCB := @NCBBuffer;
    {$ENDIF}
    NetBiosClearNCB(TempNCB);
    {build the NCB}
    with TempNCB^ do
      begin
        Command := NBSend;
        LSN := SessionNumber;
        Buffer := GetPtrForDriver(Packet);
        BufLen := PacketSize;
      end;
    {call the NetBIOS}
    NetBiosRequest(TempNCB);
    {return result code}
    with TempNCB^ do
      NetBiosSend := RetCode;
    {$IFDEF DPMI}
    NetBiosFreeNCB(TempNCB);
    {$ENDIF}
  end;

procedure NetBiosSendNoWait(N         : PNCB;
                            PostEvent : PPostHandler;
                            SessionNumber : Byte;
                            PacketSize    : Word;
                            Packet        : PnbPacket);
  begin
    {$IFDEF DPMI}
    if IsAnotherPacketType(N) then
      begin
        ReportBadNCB(N);
        Exit;
      end;
    {$ENDIF}
    NetBiosClearNCB(N);
    {$IFDEF DPMI}
    if IsAnotherPacketType(Packet) then
      begin
        NetBiosDPMIErrorCode := NBEBadPacket;
        N^.RetCode := NBEDPMIError;
        Exit;
      end;
    {$ENDIF}
    {build the NCB}
    with N^ do
      begin
        Command := NBSend + NoWait;
        LSN := SessionNumber;
        Buffer := GetPtrForDriver(Packet);
        BufLen := PacketSize;
        PostRoutine := GetPostRoutine(PostEvent, N);
      end;
    {call the NetBIOS}
    NetBiosRequest(N);
  end;

function NetBiosCmdCompleted(N : PNCB; var FinalRetCode : Byte) : Boolean;
  begin
    with N^ do
      if (CmdComplete = $FF) then
        NetBiosCmdCompleted := False
      else
        begin
          FinalRetCode := RetCode;
          NetBiosCmdCompleted := True;
        end;
  end;

function NetBiosSendDG(SenderNameNum : Byte;
                       ReceiverName  : NBNameStr;
                       DatagramSize  : Word;
                       Datagram      : PnbPacket) : Byte;
  var
    TempNCB : PNCB;
    NCBBuffer : TNCB;
  begin
    {$IFDEF DPMI}
    if IsAnotherPacketType(Datagram) then
      begin
        NetBiosDPMIErrorCode := NBEBadPacket;
        NetBiosSendDG := NBEDPMIError;
        Exit;
      end;
    TempNCB := NetBiosAllocNCB;
    if (TempNCB = nil) then
      begin
        NetBiosDPMIErrorCode := NBEOutOfMemory;
        NetBiosSendDG := NBEDPMIError;
        Exit;
      end;
    {$ELSE}
    TempNCB := @NCBBuffer;
    {$ENDIF}
    NetBiosClearNCB(TempNCB);
    {build the NCB}
    with TempNCB^ do
      begin
        Command := NBSendDatagram;
        Buffer := GetPtrForDriver(Datagram);
        BufLen := DatagramSize;
        NameNum := SenderNameNum;
        Move(ReceiverName[1], RemName, Length(ReceiverName));
      end;
    {call the NetBIOS}
    NetBiosRequest(TempNCB);
    {return result code}
    with TempNCB^ do
      NetBiosSendDG := RetCode;
    {$IFDEF DPMI}
    NetBiosFreeNCB(TempNCB);
    {$ENDIF}
  end;

procedure NetBiosSendDGNoWait(N             : PNCB;
                              PostEvent     : PPostHandler;
                              SenderNameNum : Byte;
                              ReceiverName  : NBNameStr;
                              DatagramSize  : Word;
                              Datagram      : PnbPacket);
  begin
    {$IFDEF DPMI}
    if IsAnotherPacketType(N) then
      begin
        ReportBadNCB(N);
        Exit;
      end;
    {$ENDIF}
    NetBiosClearNCB(N);
    {$IFDEF DPMI}
    if IsAnotherPacketType(Datagram) then
      begin
        NetBiosDPMIErrorCode := NBEBadPacket;
        N^.RetCode := NBEDPMIError;
        Exit;
      end;
    {$ENDIF}
    {build the NCB}
    with N^ do
      begin
        Command := NBSendDatagram + NoWait;
        Buffer := GetPtrForDriver(Datagram);
        BufLen := DatagramSize;
        NameNum := SenderNameNum;
        Move(ReceiverName[1], RemName, Length(ReceiverName));
        PostRoutine := GetPostRoutine(PostEvent, N);
      end;
    {call the NetBIOS}
    NetBiosRequest(N);
  end;

function NetBiosSendBDG(SenderNameNum : Byte;
                        DatagramSize  : Word;
                        Datagram      : PnbPacket) : Byte;
  var
    TempNCB : PNCB;
    NCBBuffer : TNCB;
  begin
    {$IFDEF DPMI}
    if IsAnotherPacketType(Datagram) then
      begin
        NetBiosDPMIErrorCode := NBEBadPacket;
        NetBiosSendBDG := NBEDPMIError;
        Exit;
      end;
    TempNCB := NetBiosAllocNCB;
    if (TempNCB = nil) then
      begin
        NetBiosDPMIErrorCode := NBEOutOfMemory;
        NetBiosSendBDG := NBEDPMIError;
        Exit;
      end;
    {$ELSE}
    TempNCB := @NCBBuffer;
    {$ENDIF}
    NetBiosClearNCB(TempNCB);
    {build the NCB}
    with TempNCB^ do
      begin
        Command := NBSendBDatagram;
        Buffer := GetPtrForDriver(Datagram);
        BufLen := DatagramSize;
        NameNum := SenderNameNum;
      end;
    {call the NetBIOS}
    NetBiosRequest(TempNCB);
    {return result code}
    with TempNCB^ do
      NetBiosSendBDG := RetCode;
    {$IFDEF DPMI}
    NetBiosFreeNCB(TempNCB);
    {$ENDIF}
  end;

procedure NetBiosSendBDGNoWait(N             : PNCB;
                               PostEvent     : PPostHandler;
                               SenderNameNum : Byte;
                               DatagramSize  : Word;
                               Datagram      : PnbPacket);
  begin
    {$IFDEF DPMI}
    if IsAnotherPacketType(N) then
      begin
        ReportBadNCB(N);
        Exit;
      end;
    {$ENDIF}
    NetBiosClearNCB(N);
    {$IFDEF DPMI}
    if IsAnotherPacketType(Datagram) then
      begin
        NetBiosDPMIErrorCode := NBEBadPacket;
        N^.RetCode := NBEDPMIError;
        Exit;
      end;
    {$ENDIF}
    {build the NCB}
    with N^ do
      begin
        Command := NBSendBDatagram + NoWait;
        Buffer := GetPtrForDriver(Datagram);
        BufLen := DatagramSize;
        NameNum := SenderNameNum;
        PostRoutine := GetPostRoutine(PostEvent, N);
      end;
    {call the NetBIOS}
    NetBiosRequest(N);
  end;

function NetBiosReceiveDG(ReceiverNameNum : Byte;
                          DatagramSize    : Word;
                          Datagram        : PnbPacket) : Byte;
  var
    TempNCB : PNCB;
    NCBBuffer : TNCB;
  begin
    {$IFDEF DPMI}
    if IsAnotherPacketType(Datagram) then
      begin
        NetBiosDPMIErrorCode := NBEBadPacket;
        NetBiosReceiveDG := NBEDPMIError;
        Exit;
      end;
    TempNCB := NetBiosAllocNCB;
    if (TempNCB = nil) then
      begin
        NetBiosDPMIErrorCode := NBEOutOfMemory;
        NetBiosReceiveDG := NBEDPMIError;
        Exit;
      end;
    {$ELSE}
    TempNCB := @NCBBuffer;
    {$ENDIF}
    NetBiosClearNCB(TempNCB);
    {build the NCB}
    with TempNCB^ do
      begin
        Command := NBReceiveDatagram;
        Buffer := GetPtrForDriver(Datagram);
        BufLen := DatagramSize;
        NameNum := ReceiverNameNum;
      end;
    {call the NetBIOS}
    NetBiosRequest(TempNCB);
    {return result code}
    with TempNCB^ do
      NetBiosReceiveDG := RetCode;
    {$IFDEF DPMI}
    NetBiosFreeNCB(TempNCB);
    {$ENDIF}
  end;

procedure NetBiosReceiveDGNoWait(N               : PNCB;
                                 PostEvent       : PPostHandler;
                                 ReceiverNameNum : Byte;
                                 DatagramSize    : Word;
                                 Datagram        : PnbPacket);
  begin
    {$IFDEF DPMI}
    if IsAnotherPacketType(N) then
      begin
        ReportBadNCB(N);
        Exit;
      end;
    {$ENDIF}
    NetBiosClearNCB(N);
    {$IFDEF DPMI}
    if IsAnotherPacketType(Datagram) then
      begin
        NetBiosDPMIErrorCode := NBEBadPacket;
        N^.RetCode := NBEDPMIError;
        Exit;
      end;
    {$ENDIF}
    {build the NCB}
    with N^ do
      begin
        Command := NBReceiveDatagram + NoWait;
        Buffer := GetPtrForDriver(Datagram);
        BufLen := DatagramSize;
        NameNum := ReceiverNameNum;
        PostRoutine := GetPostRoutine(PostEvent, N);
      end;
    {call the NetBIOS}
    NetBiosRequest(N);
  end;

function NetBiosReceiveBDG(ReceiverNameNum : Byte;
                           DatagramSize    : Word;
                           Datagram        : PnbPacket) : Byte;
  var
    TempNCB : PNCB;
    NCBBuffer : TNCB;
  begin
    {$IFDEF DPMI}
    if IsAnotherPacketType(Datagram) then
      begin
        NetBiosDPMIErrorCode := NBEBadPacket;
        NetBiosReceiveBDG := NBEDPMIError;
        Exit;
      end;
    TempNCB := NetBiosAllocNCB;
    if (TempNCB = nil) then
      begin
        NetBiosDPMIErrorCode := NBEOutOfMemory;
        NetBiosReceiveBDG := NBEDPMIError;
        Exit;
      end;
    {$ELSE}
    TempNCB := @NCBBuffer;
    {$ENDIF}
    NetBiosClearNCB(TempNCB);
    {build the NCB}
    with TempNCB^ do
      begin
        Command := NBReceiveBDatagram;
        Buffer := GetPtrForDriver(Datagram);
        BufLen := DatagramSize;
        NameNum := ReceiverNameNum;
      end;
    {call the NetBIOS}
    NetBiosRequest(TempNCB);
    {return result code}
    with TempNCB^ do
      NetBiosReceiveBDG := RetCode;
    {$IFDEF DPMI}
    NetBiosFreeNCB(TempNCB);
    {$ENDIF}
  end;

procedure NetBiosReceiveBDGNoWait(N               : PNCB;
                                  PostEvent       : PPostHandler;
                                  ReceiverNameNum : Byte;
                                  DatagramSize    : Word;
                                  Datagram        : PnbPacket);
  begin
    {$IFDEF DPMI}
    if IsAnotherPacketType(N) then
      begin
        ReportBadNCB(N);
        Exit;
      end;
    {$ENDIF}
    NetBiosClearNCB(N);
    {$IFDEF DPMI}
    if IsAnotherPacketType(Datagram) then
      begin
        NetBiosDPMIErrorCode := NBEBadPacket;
        N^.RetCode := NBEDPMIError;
        Exit;
      end;
    {$ENDIF}
    {build the NCB}
    with N^ do
      begin
        Command := NBReceiveBDatagram + NoWait;
        Buffer := GetPtrForDriver(Datagram);
        BufLen := DatagramSize;
        NameNum := ReceiverNameNum;
        PostRoutine := GetPostRoutine(PostEvent, N);
      end;
    {call the NetBIOS}
    NetBiosRequest(N);
  end;

function NetBIOSIntVecInstalled : Boolean;
  {-Determine whether the NetBIOS interrupt vector is installed}
  var
    Vec : Pointer;
    S   : Word;
  begin
    {get the NetBIOS interrupt vector, isolate the segment}
    {$IFDEF DPMI}
    GetRealModeIntVector(NetBiosIntr, Vec);
    {$ELSE}
    GetIntVec(NetBiosIntr, Vec);
    {$ENDIF}
    S := OS(Vec).S;
    {the vector is 'installed' if it's non-nil and its segment does not
     point into the BIOS (ie segment $F000)}
    NetBiosIntVecInstalled := (Vec <> nil) and (S <> $F000);
  end;

function BogusCallWasDetected : Boolean;
  {-Issues an invalid NetBIOS call. If such a call is made, and the
    adapter/driver returns the NetBIOS invalid command error code,
    then NetBIOS or a NetBIOS emulator is installed.}
  var
    TempNCB : PNCB;
    NCBBuffer : TNCB;
    OrigAdapterNumber : byte;                                   {!!.53}
  begin
    {$IFDEF DPMI}
    TempNCB := NetBiosAllocNCB;
    if (TempNCB = nil) then
      begin
        {if we cannot allocate any DOS memory, it's moot whether the
         NetBIOS is there or not: we won't be able to use it anyway}
        BogusCallWasDetected := false;
        Exit;
      end;
    {$ELSE}
    TempNCB := @NCBBuffer;
    {$ENDIF}
    NetBiosClearNCB(TempNCB);
    {build the NCB}
    with TempNCB^ do
      begin
        Command := NBInvalidCommand;
      end;
    {call the NetBIOS}
    NetBiosRequest(TempNCB);
    {if we got an "invalid adapter number" error, try and find} {!!.53}
    {the valid one for this machine}                            {!!.53}
    if (TempNCB^.RetCode = NBEInvalidLanA) then                 {!!.53}
      begin                                                     {!!.53}
        OrigAdapterNumber := DefaultAdapterNum;                 {!!.53}
        DefaultAdapterNum := 0;                                 {!!.53}
        while (DefaultAdapterNum < 255) and                     {!!.53}
              (TempNCB^.RetCode = NBEInvalidLanA) do            {!!.53}
          begin                                                 {!!.53}
            NetBiosClearNCB(TempNCB);                           {!!.53}
            TempNCB^.Command := NBInvalidCommand;               {!!.53}
            NetBiosRequest(TempNCB);                            {!!.53}
            inc(DefaultAdapterNum);                             {!!.53}
          end;                                                  {!!.53}
        if (TempNCB^.RetCode = NBEInvalidCommand) then          {!!.53}
          dec(DefaultAdapterNum)                                {!!.53}
        else                                                    {!!.53}
          DefaultAdapterNum := OrigAdapterNumber;               {!!.53}
      end;                                                      {!!.53}
    {check result code}
    with TempNCB^ do
      BogusCallWasDetected := RetCode = NBEInvalidCommand;
    {$IFDEF DPMI}
    NetBiosFreeNCB(TempNCB);
    {$ENDIF}
  end;

function NetBiosInstalled : Boolean;
  begin
    NetBiosDPMIErrorCode := NBESuccess;
    {a NetBIOS adapter or driver is there if (1) its vector is present
     and (2) NetBIOS recognises an invalid function request}
    if NetBIOSIntVecInstalled then
      NetBIOSInstalled := BogusCallWasDetected
    else {no valid vector}
      NetBIOSInstalled := false;
  end;

{$IFDEF DPMI}
procedure NetBiosRequest(N : PNCB);
  var
    Dummy : word;
    RealP: pointer;
    Regs : DPMIRegisters;
  begin
    RealP := GetPtrForDriver(N);
    FillChar(Regs, SizeOf(Regs), 0);
    with Regs do
      begin
        ES := OS(RealP).S;
        BX := OS(RealP).O;
      end;
    Dummy := SimulateRealModeInt(NetBiosIntr, Regs);
  end;
{$ENDIF}

function NetBiosInfo(NS : PNetBiosStatus;
                     Name : NBNameStr;
                     MaxNames : byte) : byte;
  var
    TempNCB : PNCB;
    NCBBuffer : TNCB;
  begin
    {$IFDEF DPMI}
    if IsAnotherPacketType(NS) then
      begin
        NetBiosDPMIErrorCode := NBEBadPacket;
        NetBiosInfo := NBEDPMIError;
        Exit;
      end;
    TempNCB := NetBiosAllocNCB;
    if (TempNCB = nil) then
      begin
        NetBiosDPMIErrorCode := NBEOutOfMemory;
        NetBiosInfo := NBEDPMIError;
        Exit;
      end;
    {$ELSE}
    TempNCB := @NCBBuffer;
    {$ENDIF}
    NetBiosClearNCB(TempNCB);
    {build the NCB}
    with TempNCB^ do
      begin
        Command := NBAdapterStatus;
        Move(Name[1], RemName, Length(Name));
        Buffer := GetPtrForDriver(NS);
        {set the buffer length: note NetBiosStatus includes space for
         16 NetBiosNames already}
        BufLen := sizeof(TNetBiosStatus) +
                  ((word(MaxNames) - 16) * sizeof(NetBiosName));
      end;
    {call the NetBIOS}
    NetBiosRequest(TempNCB);
    {return result code, and number of name in table}
    with TempNCB^ do
      begin
        NetBiosInfo := RetCode;
      end;
    {$IFDEF DPMI}
    NetBiosFreeNCB(TempNCB);
    {$ENDIF}
  end;

{$IFDEF Windows}
begin
  {$IFNDEF SmartCallbacks}
  WindowsPostEventHandler := MakeProcInstance(@WinPost, HInstance);
  {$ELSE}
  WindowsPostEventHandler := @WinPost;
  {$ENDIF}
{$ELSE}
{$IFDEF InitAllUnits}
begin
{$ENDIF}
{$ENDIF}
end.
