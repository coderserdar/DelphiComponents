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
  {$F+,V-,B-,S-,I-,R-}
  {$IFDEF CanSetOvrflowCheck}
    {$Q-,P-}
  {$ENDIF}
  {$IFDEF MSDOS}
  {$O-}
  {$ENDIF}
  {$IFDEF Windows}
  {$W-}
  {$ENDIF}

{$IFDEF Win32}
  !! Error - this unit cannot be compiled for 32-bit
{$ENDIF}

unit NWIPXSPX;

interface

uses
  NWBase,
  {$IFDEF Windows}
  WinProcs,
  WinTypes,
   {$IFDEF Ver80}
   DosSupp,
   {$ELSE}
   WinDos,
   {$ENDIF}
  DPMI;
  {$ELSE}
    {$IFDEF DPMI}
    WinAPI,
    Dos, DPMI;
    {$ELSE}
    Dos;
    {$ENDIF}
  {$ENDIF}


const
{---IPX/SPX Error Codes---}
  {NWIPXSPX unit specific error codes}
  IPXErrOutOfMemory   = 1;  {Either heap or DOS memory is exhausted}
  IPXErrDataTooLarge  = 2;  {user specified data is larger than maximum}
  IPXErrBadPacket     = 3;  {data packet was not allocated with IPXAllocPacket}
  IPXErrBadEventRec   = 4;  {IPX event record was not allocated with IPXAllocEventRec}
  AESErrBadECB        = 5;  {AES ECB was not allocated with AESAllocECB}
  SPXErrNoConnection  = 6;  {the SPXEvent record has no connection}
  SPXErrBadEventRec   = 7;  {SPX event record was not allocated with SPXAllocEventRec}
  SPXErrRemoteHasGone = 8;  {The remote partner called SPXTerminateConn}
  IPXErrDPMIError     = 9;  {Some DPMI error}
  SPXErrSameSockets   =10;  {Local and remote sockets equal}

  {Novell defined error codes}
  IPXErrSocketAlreadyOpen  = $FF;  {attempted to open already open socket}
  IPXErrSocketTableFull    = $FE;  {this connection may not have any more open sockets}
  IPXErrCannotCancel       = $F9;  {the event was in a critical phase: cancel failed}
  IPXErrNotActive          = $FF;  {the event was complete: cancel failed}
  IPXErrCancelled          = $FC;  {the event was cancelled}
  IPXErrMalformedPacket    = $FD;  {packet not proper size and/or format}
  IPXErrPacketUndeliverable= $FE;  {cannot deliver the packet}
  IPXErrPhysicalError      = $FF;  {hardware error}
  IPXErrPacketOverflow     = $FD;  {more data received than buffer size specified}
  IPXErrSocketNotOpen      = $FF;  {the socket for communications is not open}
  SPXErrRemoteEndedConn    = $EC;  {remote ended connection without receiving last packet}
  SPXErrFailedConn         = $ED;  {SPX watchdog has noticed the connection has failed}
  SPXErrInvalidConn        = $EE;  {non-existent connection number}
  SPXErrConnTableFull      = $EF;  {Local SPX connection table is full}

const
{---IPX constants---}
  IPXMaxDataSize = 546;  {max size of the data part of an IPX packet}

{---SPX constants---}
  SPXEndOfConn = $FE;    {Data type received by SPXReceivedPacket that
                          means remote station called SPXTerminateConn}
  SPXMaxPoolCount = 16;  {max number of ECBs in an SPX listen pool}
  SPXMaxDataSize = 534;  {max size of the data part of an SPX packet}

type
{---IPX / SPX Types---}
  {a pointer to an IPX/SPX packet}
  PPacket = pointer;

  {the header structure for IPX calls}
  PipxHeader = ^TipxHeader;
  TipxHeader = record
    CheckSum        : word;
    Len             : word;
    TransportControl: byte;
    PacketType      : byte;
    Destination     : IPXAddress;
    Source          : IPXAddress;
  end;

  {the record type used to represent a buffer fragment descriptor for IPX/SPX}
  TipxFragment = record
    Data : Pointer;  {the data}
    Size : word;     {the size of the data}
  end;

  {The IPX/SPX Event Control Block}
  PipxECB = ^TipxECB;
  TipxECB = record
    Link            : Pointer;              {used internally}
    ESRAddress      : Pointer;              {the ESR dispatcher}
    InUse           : byte;                 {in use semaphore}
    CompletionCode  : byte;                 {completion error code}
    SocketNumber    : word;                 {the session socket (hi-lo)}
    IPXWorkSpace    : nwLong;               {reserved for internal use}
    DriverWorkSpace : Array[1..12] of byte; {reserved}
    ImmediateAddress: PhysicalNodeAddress;  {the internet address}
    FragmentCount   : word;                 {the number of buffers}
    FD1             : TipxFragment;         {buffer 1}
    FD2             : TipxFragment;         {buffer 2}
  end;

  {The IPX Event record}
  PipxEventRec = ^TipxEventRec;
  {..prototype for an IPX event service routine}
  IPXEventServiceRoutine = procedure (FromAES : boolean; IPXEvent : PipxEventRec);
  TipxEventRec = record
    ECB    : TipxECB;                   {the event record's ECB (MUST be first)}
    Header : TipxHeader;                {the header for the ECB}
    Next   : PipxEventRec;              {next TipxEventRec in the linked list}
    UserESR: IPXEventServiceRoutine;    {the user's ESR}
  end;

  {The Asynchronous Event Scheduler's Event Control Block}
  PAESECB = ^TAESECB;
  {..prototype for an AES special event service routine}
  AESEventServiceRoutine = procedure (ECB : PAESECB);
  TAESECB = record
    Link         : Pointer;                {used internally}
    ESRAddress   : Pointer;                {the ESR dispatcher}
    InUse        : byte;                   {in use semaphore}
    AESWorkSpace : Array[1..5] of byte;    {AES work area - reserved}
    Next         : PAESECB;                {next AES ECB in linked list}
    UserESR      : AESEventServiceRoutine; {the user's ESR}
    UserData     : pointer;                {pointer to any user data}
  end;

  {The SPXConnStatus function sets a variable of this type}
  TspxStatus = record
    State                   : byte;
    Flag                    : byte;
    SourceConn              : word;
    DestConn                : word;
    SequenceNum             : word;
    AckNum                  : word;
    AllocNum                : word;
    RemoteAckNum            : word;
    RemoteAllocNum          : word;
    ConnSocket              : word;
    ImmediateAdd            : PhysicalNodeAddress;
    Destination             : IPXAddress;
    RetransmitCount         : word;
    EstimatedRoundTripDelay : word;
    RetransmittedPackets    : word;
    SuppressedPackets       : word;
  end;

  PspxHeader = ^TspxHeader;
  TspxHeader = record
    IPXHeader       : TipxHeader;
    ConnectControl  : byte;
    DataType        : byte;
    SourceConnID    : word;
    DestConnID      : word;
    SequenceNo      : word;
    AcknowledgeNo   : word;
    AllocationNo    : word;
  end;

  PspxPoolECB = ^TspxPoolECB;
  TspxPoolECB = record
    PoolECB    : TipxECB;      {MUST be first}
    PoolHeader : TspxHeader;   {MUST be second}
    PoolData   : array [1..SPXMaxDataSize] of byte;
  end;

  PspxEventRec = ^TspxEventRec;
  TspxEventRec = record
    ECB        : TipxECB;      {ECB for connecting and sending (MUST be first)}
    Header     : TspxHeader;   {header for connecting and sending}
    AmConnected: boolean;      {Is set true if connected}
    ConnID     : word;         {SPX connection id}
    Next       : PspxEventRec; {The next SPX record in the list}
    PacketSize : word;         {max packet size to transfer}
    PoolCount  : byte;         {actual number of buffers in pool}
    QueueCount : byte;         {number of received buffers}
    Pool       : array [1..SPXMaxPoolCount] of PspxPoolECB;
                               {array of pointers to listen buffers}
    Queue      : array [1..SPXMaxPoolCount] of byte;
                               {array of queued buffer indexes}
  end;

const
  SPXWatchDog   : boolean = false; {set true to use SPX watchdog services}
  SPXRetryCount : byte = 0;     {indicates to use the default retry count}

  IPXAllNodes : PhysicalNodeAddress = ($FF,$FF,$FF,$FF,$FF,$FF);

var
  IPXDoNothingESR : IPXEventServiceRoutine; {a do-nothing ESR for IPX}
  AESDoNothingESR : AESEventServiceRoutine; {a do-nothing ESR for AES}


{****************************************************
 IPX Services (Internetwork Packet Exchange Protocol)
 ****************************************************}

function IPXAllocEventRec(ESR : IPXEventServiceRoutine) : PipxEventRec;
  {-Allocate an IPX event record for passing onto other IPX routines.
    Notes: IPX event records passed to the IPX routines in this unit
    MUST be allocated with this function, as it performs some
    initializations after allocating the event record. This applies
    to real-mode programming as well. ESR can be nil, or can be the
    address of an event service routine that will gain control after
    IPX has serviced the event that the event record will handle.
    Returns either a pointer to the TipxEventRec, or nil if out of memory.}

function IPXAllocPacket(Size : word) : PPacket;
  {-Return a pointer to a memory block of Size bytes.
    Notes: Under real mode this function is equivalent to GetMem, but
    under protected mode or Windows, the memory block is in DOS
    addressable memory. All data packets used for IPX and SPX MUST be
    allocated with this function.
    Returns either a pointer to the packet, or nil if out of memory.}

function IPXCancelEvent(IPXEvent : PipxEventRec) : byte;
  {-Cancel the pending IPX event.
    Notes: This function cancels any pending event. If the ECB was in
    use, the InUse flag gets set to zero, and the completion code is
    set to IPXCancelled. However, if the IPX driver was in the process
    of transferring data to/from the ECB, the cancel request will fail.}

procedure IPXCloseSocket(Socket : word);
  {-Close the specified socket.
    Notes: Closing a socket will automatically cancel any pending
    events associated with the socket. Attempting to close a socket
    that is not open has no effect.
    Important: Transient applications (non TSRs) MUST close all their
    sockets before terminating, even if the socket was opened with the
    Forever flag set false. There exists a small window of time between
    the program ending and IPX automatically closing the socket when a
    incoming packet could arrive, causing a system crash. Also do not
    call IPXCloseSocket from within an Event Service Routine.}

function IPXEventComplete(IPXEvent : PipxEventRec;
                          var CompletionCode : byte) : boolean;
  {-Return the status of a pending IPX event.
    Notes: If this function returns true, the event's completion code
    is returned in CompletionCode.}

procedure IPXFreeEventRec(IPXEvent : PipxEventRec);
  {-Free an IPX event record that was allocated by IPXAllocEventRec.}

procedure IPXFreePacket(P : PPacket);
  {-Free a data packet that was allocated by IPXAllocPacket}

procedure IPXInternetAddress(var Address : IPXAddress);
  {-Return the internetwork address of the calling workstation.
    Notes: Only the network number and physical node address subfields
    are returned by this function. The socket field is not returned.}

function IPXListen(IPXEvent : PipxEventRec;
                   Socket   : word;
                   WaitForCompletion : boolean;
                   MaxPacketSize : word;
                   DataPacket    : PPacket) : byte;
  {-Initialize the IPX event record to listen for a data packet, and
    submit it to IPX.
    Notes: IPXEvent must be allocated with IPXAllocEventRec. IPXListen
    initializes this record and opens the socket (if not already open).
    DataPacket must be allocated with IPXAllocPacket. The listen event
    cannot be assumed to be complete when this function returns
    (unless WaitForCompletion is true), hence the IPXEvent pointer
    must remain in scope until the event does complete. Use
    IPXEventComplete to check for completion of the listen event, and
    the completion code.}

function IPXOpenSocket(Socket : word; Forever : boolean) : byte;
  {-Open a socket for use by IPX or SPX.
    Notes: If Forever is true the socket will remain open until
    explictly closed with IPXCloseSocket. If false NetWare closes the
    socket when the program ends (in DOS mode only).}

function IPXOpenUniqueSocket(var Socket : word;
                             Forever : boolean) : word;
  {-Open and return a unique socket number.
    Notes: Novell calls these dynamic sockets, their value depends on
    the network loading at the time.}

procedure IPXRelinquish;
  {-Yield processing to IPX.
    Notes: This procedure serves one of two purposes, depending whether
    it is invoked from a non-dedicated file server or a workstation. On
    a server, this function temporarily suspends the calling process so
    that the server program gets CPU resources immediately. Similarly,
    on a workstation, the NetWare shell gets termporary control during
    which it may check for incoming and outgoing messages.
    IPXRelinquish should be called frequently while an app waits for an
    IPX or SPX event to complete. IPXEventComplete and SPXEventComplete
    both call IPXRelinquish automatically.}

function IPXSend(IPXEvent : PipxEventRec;
                 Receiver : IPXAddress;
                 Socket   : word;
                 WaitForCompletion : boolean;
                 DataPacketSize : word;
                 DataPacket     : PPacket) : byte;
  {-Initialize the IPX event record to submit a data packet, and submit
    it to IPX.
    Notes: IPXEvent must be allocated with IPXAllocEventRec. IPXSend
    initializes this record and opens the socket (if not already open).
    The caller must initialize Receiver with a valid IPX address (except
    for the socket number); to do an IPX broadcast use IPXAllNodes as
    the physical node address. DataPacket must be allocated with
    IPXAllocPacket. The send event cannot be assumed to be complete
    when this function returns (unless WaitForCompletion is true),
    hence the IPXEvent pointer must remain in scope until the event
    does complete. Use IPXEventComplete to check for completion of the
    send event, and the completion code.}

function IPXServicesAvail : boolean;
  {-Return true if IPX services are available}


{*******************************************
 AES Services (Asynchronous Event Scheduler)
 *******************************************}

function AESAllocECB(ESR : AESEventServiceRoutine) : PAESECB;
  {-Allocate an ECB for use with AES}

function AESCancelEvent(ECB : PAESECB) : byte;
  {-Cancel the previously scheduled AES event}

function AESEventComplete(ECB : PAESECB) : boolean;
  {-Return true if the AES scheduled event is complete}

procedure AESFreeECB(ECB : PAESECB);
  {-Free an ECB previously allocated with AESAllocECB}

function AESScheduleEvent(DelayTicks : word; ECB : PAESECB) : byte;
  {-Schedule an AES event to occur in DelayTicks time}


{*************************************************
 SPX Services (Sequenced Packet Exchange Protocol)
 *************************************************}

procedure SPXAbortConn(SPXEvent : PspxEventRec);
  {-Unilaterally abort an SPX event.
    Notes: The connected partner is not informed of the break, so this
    procedure should only be used in emergency. SPXTerminateConn
    should normally be used.}

function SPXAllocEventRec(NumECBs : byte; MaxPacketSize : word) : PspxEventRec;
  {-Allocate an SPX event record and initialize it ready for SPX.
    Notes: SPX event records passed to the SPX routines in this unit
    MUST be allocated with this function, as it performs some
    initialisations after allocating the event record. NumECBs should
    be at least 2 and at most SPXMaxPoolCount, and will be used to
    set the size of the pool of buffers actively listening for packets
    to be received via this event record's connection (SPX itself
    requires at most two of these buffers). MaxPacketSize is the
    maximum size of the user-defined packets that are expected to be
    received and must be less than SPXMaxDataSize. If the function
    returns nil either heap or DOS memory is exhausted.}

procedure SPXCancelListenForConn(SPXEvent : PspxEventRec);
  {-Cancels a call to SPXListenForConn made with WaitForCompletion
    set to false that hasn't completed yet.}

function SPXECBsListening(SPXEvent : PspxEventRec) : byte;
  {-Return the number of buffers listening within this event record.}

function SPXEstablishConn(SPXEvent     : PspxEventRec;
                          Receiver     : IPXAddress;
                          LocalSocket  : word;
                          RemoteSocket : word;
                          WaitForCompletion : boolean) : byte;
  {-Attempt to establish an SPX connection with Receiver.
    Notes: You must initialize Receiver with an exact IPX address, IPX
    broadcasts are not allowed. LocalSocket defines the socket this
    process will use to talk to SPX, RemoteSocket defines the socket
    the potential partner will use; they cannot be the same.
    SPXEstablishConn will open the local socket, post all the pool
    ECBs to listen on that socket and then send a message to Receiver
    requesting connection. If the remote partner is listening with
    SPXListenForConn on the RemoteSocket he will acknowledge, and the
    connection is set up. The connection number will be stored in the
    internal field SPXEvent^.ConnID. Unless the WaitForCompletion flag
    is true, the SPX event cannot be assumed to be complete when this
    routine terminates, hence the SPXEvent must remain in scope. Use
    SPXEventComplete to check for the completion of the event. If
    WaitForCompletion is False, and you wish to cancel the event, use
    SPXAbortConn *not* SPXTerminateConn.}

function SPXEventComplete(SPXEvent : PspxEventRec;
                          var FinalResult : byte) : boolean;
  {-Return the status of a pending SPX event.
    Notes: If this function returns true, the event's completion code
    is returned in CompletionCode.}

procedure SPXFreeEventRec(SPXEvent : PspxEventRec);
  {-Free an SPX event record allocated by SPXAllocEventRec
    Notes: The connection associated with this event record must have
    been terminated first, otherwise a system crash will occur.}

function SPXGetConnStatus(SPXEvent   : PspxEventRec;
                      var ConnStatus : TSPXStatus) : byte;
  {-Return the status of the connection associated with SPXEvent}

function SPXListenForConn(SPXEvent    : PspxEventRec;
                          LocalSocket : word;
                          WaitForCompletion : boolean) : byte;
  {-Listen for an SPX connection request from a remote partner
    Notes: This function is the counterpart to SPXEstablishConn. For
    a connection to be established between two partners, one must
    be passively listening with SPXListenForConn, and the other
    actively asking for connection with SPXEstablishConn. LocalSocket
    is the socket that this process will use to talk to SPX (it MUST
    equal RemoteSocket in SPXEstablishConn). This routine will post
    all the listen buffers in the event record to listen for the
    establish connection message. Unless the WaitForCompletion flag is
    true, the SPX event cannot be assumed to be complete when this
    routine terminates, hence the SPXEvent must remain in scope. Use
    SPXEventComplete to check for the completion of the event. If
    WaitForCompletion is false and you wish to cancel the event use
    SPXCancelListenForConn.}

function SPXPacketReceived(SPXEvent           : PspxEventRec;
                           var Index          : byte;
                           var DataType       : byte;
                           var CompletionCode : byte;
                           var DataPtr        : pointer) : boolean;
  {-Return true if an SPX packet has been received.
    Notes: Index identifies the listen buffer that received the packet
    and its value should be used to repost the buffer to the pool with
    SPXReactivateECB. DataType identifies the type of data received:
    zero for user-defined packets or SPXEndOfConn if the remote
    partner has terminated the connection (note that this routine will
    mark the SPXEvent accordingly). CompletionCode will be one of
    0, IPXCancelled, IPXMalformedPacket, IPXPacketOverflow,
    IPXPhysicalError or SPXFailedConn. DataPtr is set to point to the
    received data in the listen buffer. Ideally if this routine returns
    true the application should copy the data to a local buffer, and
    resubmit the listen buffer to the pool as soon as possible.}

procedure SPXReactivateECB(SPXEvent : PspxEventRec;
                           Index    : byte);
  {-Resubmit an buffer to the listen pool}

function SPXSend(SPXEvent       : PspxEventRec;
                 WaitForCompletion : boolean;
                 DataPacketSize : word;
                 DataPacket     : PPacket) : byte;
  {-Send a data packet using SPX services
    Notes: The connection must be completely initialized prior to
    calling this routine (ie the establish or listen for connection
    event should have completed successfully). DataPacket must be
    allocated with IPXAllocPacket. This function will then set up
    the event record to send the packet, and submit the request to
    SPX. Unless WaitForCompletion is true, the event cannot be assumed
    complete when this routine terminates, use SPXEventComplete to
    check for completion of the event.}

function SPXServicesAvail(var Version : word;
                          var MaxSPXConn : word;
                          var AvailSPXConn : word) : boolean;
  {-Return true if SPX services are available}

procedure SPXTerminateConn(SPXEvent : PspxEventRec);
  {-Terminate an existing SPX connection
    Notes: When this routine returns the connection has been broken
    and the SPXEvent record can be reused to establish another
    connection or freed.}

implementation

type
  LH = record L, H : word; end; {decompose a nwLong into 2 words}
  OS = record O, S : word; end; {decompose a pointer into seg/sel and ofs}

type
  TargetType = record                   {used by IPXGetLocalTarget}
    IPXAdd : IPXAddress;
    TargetNode : PhysicalNodeAddress;
  end;

const
  IPXServicesAvailable : boolean = false;

var
  IPXLocation : pointer;                {IPX realmode entry point address}

{$IFDEF DPMIorWnd}
{---The DPMI IPX event service routine variables---}
var
  IPXEventHandlerRegs : DPMIRegisters;
  IPXCallBack : pointer;
const
  IPXRecList : PipxEventRec = nil;         {the linked list of IPXRecs}

{---The DPMI AES event service routine variables---}
var
  AESEventHandlerRegs : DPMIRegisters;
  AESCallBack : pointer;
const
  AESECBList : PAESECB = nil;         {the linked list of AESECBs}

{---The DPMI SPX event service routine variables---}
var
  SPXEventHandlerRegs : DPMIRegisters;
  SPXCallBack : pointer;
{$ENDIF}

const
  SPXRecList : PspxEventRec = nil;         {the linked list of SPXRecs}


{Note: to isolate the different usage of packets in protected mode
programming and in real-mode programming (basically we *must* have a
DOS memory block in protected mode, and it's the real-mode pointer
to the block that is passed to IPX/SPX), IPXAllocPacket will
return the address of a block of memory that follows directly on from
an 8 byte block of the form TPacketInfo. By this 'trick' we can ensure
packets (or datagrams) passed to the relevant IPX/SPX routines in
protected mode have been properly allocated, that we can get the real
mode pointer easily for a pmode block, and also that the code for
calling these routines is the same in real-mode and protected mode.}
type
  PPacketInfo = ^TPacketInfo;
  TPacketInfo = record         {THIS MUST BE EIGHT (8) BYTES}
    Sig     : word;            {Always 'PI' back-words}
    RealPtr : pointer;         {DPMI real-mode pointer to data}
    DSize   : word;            {size of data block}
  end;
const
  PacketSig = $4950;           {'PI' back-words}

{$IFDEF DPMIorWnd}
function DOSGetMem(var RealPtr, ProtPtr; Size : word) : boolean; near;
  {Get a block of DOS memory and return real and protected pointers}
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

procedure DOSFreeMem(ProtPtr : pointer); near;
  {Free a block of DOS memory}
  var
    Dummy : word;
  begin
    Dummy := GlobalDOSFree(OS(ProtPtr).S);
  end;
{$ENDIF}

function GetRealModePtr(Packet : PPacket) : pointer;
  {-Returns the real mode pointer for Packet. In pmode this code
    assumes that IsAnotherPacketType has been called and it
    returned false.}
  {$IFDEF DPMIorWnd}
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

function IsAnotherPacketType(Packet : PPacket) : boolean;
  {-Returns true if Packet was *not* allocated by IPXAllocPacket}
  assembler;
  asm
    mov ax, 1                           {assume true, ie it's not ours}
    mov bx, Packet.word[0]              {get offset}
    sub bx, 8                           {subtract sizeof(TPacketInfo)}
    jl @@Exit                           {if negative not ours, so exit}
    mov es, Packet.word[2]              {get segment/selector}
    cmp word ptr es:[bx], PacketSig     {1st word should be signature}
    jne @@Exit                          {no, so not ours}
    xor ax, ax                          {it's our type, return false}
  @@Exit:
  end;

procedure DisableInterrupts;
  {-Disable hardware interrupts}
  {$IFDEF DPMIorWnd}
  inline($B8/$00/$09/   {mov ax, $0900}
         $CD/$31/       {int $31}
         $B8/$81/$16/   {mov ax, $1681}
         $CD/$2F);      {int $2F}
  {$ELSE}
  inline($FA);
  {$ENDIF}

procedure EnableInterrupts;
  {-Reenable hardware interrupts}
  {$IFDEF DPMIorWnd}
  inline($B8/$01/$09/   {mov ax, $0901}
         $CD/$31/       {int $31}
         $B8/$82/$16/   {mov ax, $1682}
         $CD/$2F);      {int $2F}
  {$ELSE}
  inline($FB);
  {$ENDIF}


{****************************************************
 IPX Services (Internetwork Packet Exchange Protocol)
 ****************************************************}

{$IFDEF DPMIorWnd}
procedure IPXEventHandler(FromAES : boolean; RealModePtr : nwLong);
  {-The DPMI IPX event handler. Works out which ECB was returned for
    processing by IPX and calls its event handler.}
  var
    IPXEvent : PipxEventRec;
    StillLooking : boolean;
    R : pointer;
  begin
    IPXEvent := IPXRecList;
    StillLooking := true;
    while StillLooking and (IPXEvent <> nil) do
      if (nwLong(GetRealModePtr(IPXEvent)) <> RealModePtr) then
        IPXEvent := IPXEvent^.Next
      else {We've found the relevant IPX event record}
        begin
          StillLooking := false;
          {call the user's ESR}
          IPXEvent^.UserESR(FromAES, IPXEvent);
        end;
  end;

procedure IPXCallbackShell; far; assembler;
  asm
    {Note: this uses the same processing as SPXCallBackShell. See that
           routine for comments.}
    mov ax, 1681h
    int 2Fh
    push es; push di
    cld
    lodsw; mov es:[di].DPMIRegisters.&IP, ax
    lodsw; mov es:[di].DPMIRegisters.&CS, ax
    add es:[di].DPMIRegisters.&SP, 4
    push word ptr es:[di].DPMIRegisters.&AX.word[0]
    push word ptr es:[di].DPMIRegisters.&ES
    push word ptr es:[di].DPMIRegisters.&SI.word[0]
    mov ax, es
    mov ds, ax
    call IPXEventHandler
    pop di; pop es
    mov ax,1682h
    int 2Fh
    iret
  end;

function IPXAllocateDPMIESR : boolean;
  begin
    IPXAllocateDPMIESR :=
       AllocRealModeCallBackAddr(@IPXCallBackShell,
                                 IPXEventHandlerRegs,
                                 IPXCallBack) = 0;
  end;

procedure IPXFreeDPMIESR;
  var
    Dummy : word;
  begin
    Dummy := FreeRealModeCallBackAddr(IPXCallBack);
  end;
{$ELSE}
procedure IPXESRDispatch; far; assembler;
  asm
    inc ax               {al=$FF => IPX, al=0 => AES}
    push ax
    mov ax, SEG @Data
    mov ds, ax
    push es
    push si
    call dword ptr es:[si].TipxEventRec.UserESR
  end;
{$ENDIF}

{===Packet allocation and deallocation===}
function IPXAllocPacket(Size : word) : PPacket;
  var
    P : PPacketInfo;
    R : pointer;
  begin
    {$IFDEF DPMIorWnd}
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
    if nwGetMem(P, Size+sizeof(TPacketInfo)) then
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
    IPXAllocPacket := PPacket(P);
  end;
{--------}
procedure IPXFreePacket(P : PPacket);
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
         be used again without reallocation, $CC is INT 3}
        FillChar(TempP^, Size, $CC);
        {$IFDEF DPMIorWnd}
        DOSFreeMem(TempP);
        {$ELSE}
        FreeMem(TempP, Size);
        {$ENDIF}
      end;
  end;


{===IPX Event record allocation and deallocation===}
function IPXAllocEventRec(ESR : IPXEventServiceRoutine) : PipxEventRec;
  var
    P : PipxEventRec;
  begin
    P := PipxEventRec(IPXAllocPacket(sizeof(TipxEventRec)));
    if (P <> nil) then
      with P^ do
        begin
          {zeroize the entire record (incl ESRAddress), and set the
           user's ESR field}
          FillChar(P^, sizeof(TipxEventRec), 0);
          UserESR := ESR;
          {$IFDEF DPMIorWnd}
          if IPXRecList = nil then
            if not IPXAllocateDPMIESR then
              begin
                IPXFreePacket(PPacket(P));
                IPXAllocEventRec := nil;
                Exit;
              end;
          Next := IPXRecList;
          IPXRecList := P;
          {If there is an actual ESR, set the ESRAddress field so that
           IPX can call us via the call back}
          if (@ESR <> nil) then
            ECB.ESRAddress := IPXCallBack;
          {$ELSE}
          {If there is an actual ESR, set the ESRAddress field so that
           IPX can call us via the dispatcher}
          if (@ESR <> nil) then
            ECB.ESRAddress := @IPXESRDispatch;
          {$ENDIF}
        end;
    IPXAllocEventRec := P;
  end;
{--------}
procedure IPXFreeEventRec(IPXEvent : PipxEventRec);
  var
    Temp, Dad : PipxEventRec;
  begin
    {$IFDEF DPMIorWnd}
    Temp := IPXRecList;
    Dad := nil;
    while (Temp <> nil) and (Temp <> IPXEvent) do
      begin
        Dad := Temp;
        Temp := Temp^.Next;
      end;
    if (Temp <> nil) then
      if (Dad = nil) then
        begin
          IPXRecList := Temp^.Next;
          if (IPXRecList = nil) then
            IPXFreeDPMIESR;
        end
      else
        Dad^.Next := Temp^.Next;
    {$ENDIF}
    {free the IPXEvent as a packet}
    IPXFreePacket(PPacket(IPXEvent));
  end;


{===Primitive routines to call IPX===}
function IPXCall(var Regs : TnwRegisters) : byte; near;
  {-Calls the real mode IPX driver at IPXLocation}
{$IFDEF DPMIorWnd}
  var
    DPMIRegs : DPMIRegisters absolute Regs;
    Dummy : word;
  begin
    with Regs do begin
      CS := OS(IPXLocation).S;
      IP := OS(IPXLocation).O;
    end;
    if (CallFarRealModeProc(0, nil, DPMIRegs) <> 0) then
      IPXCall := IPXErrDPMIError
    else
      IPXCall := Regs.AL;
  end;
{$ELSE}
  assembler;
  var
    ipxEntryPoint : pointer;
  asm
    {Notes: the Regs.BP field and BP register are not set/restored as
            IPX can internally use it as a scratch register.}
    mov ax, IPXLocation.word[2]
    mov ipxEntryPoint.word[2], ax
    mov ax, IPXLocation.word[0]
    mov ipxEntryPoint.word[0], ax
    push ds               {save our data segment}
    push bp               {...and our stack frame}

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

    call ipxEntryPoint    {call IPX}
    pop bp                {discard returned BP & restore our stack frame}
    push ax               {push the call result}
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

    pop ax                {get the call result}
    pop ds                {restore Pascal's DS}
  @@Exit:
  end;
{$ENDIF}
{--------}
function IPXCallWithECB(Func : word; ECB : PipxECB) : byte; near;
  {-Primitive to call IPX with an ECB.
    Notes: in pmode ECB is a realmode pointer}
  var
    Regs : TnwRegisters;
  begin
    nwInitRegs(Regs);
    with Regs do
      begin
        BX := Func;
        ES := OS(ECB).S;
        SI := OS(ECB).O;
      end;
    IPXCallWithECB := IPXCall(Regs);
  end;

{===Utiltity routines for IPX===}
function IPXInstalled : boolean; near;
  {-Checks for presence of IPX and sets the driver entry address IPXLocation}
  var
    IntrResult : word;
    Regs : TnwRegisters;
  begin
    {Assume IPX not found}
    IPXInstalled := false;
    IPXLocation := nil;
    nwInitRegs(Regs);
    Regs.AX := $7A00;
    IntrResult := nwIntr($2F, Regs);
    if (IntrResult = 0) and (Regs.AL = $FF) then
      begin
        IPXInstalled := true;
        IPXLocation := Ptr(Regs.ES, Regs.DI); {pointer to IPX entry point in ES:DI}
      end;
  end;
{--------}
procedure InitializeIPX;
  {-Initializes the IPX/SPX variables - called by unit initialisation}
  begin
    if IPXInstalled then {this call to IPXInstalled also sets IPXlocation}
      IPXServicesAvailable := true;
  end;
{--------}
function IPXServicesAvail : boolean;
  begin
    IPXServicesAvail := IPXServicesAvailable;
  end;
{--------}
procedure IPXInternetAddress(var Address : IPXAddress);
  var
    Regs : TnwRegisters;
    Dummy : byte;
  begin
    FillChar(nwGlobalBuf^, SizeOf(IPXAddress), 0);
    nwInitRegs(Regs);
    with Regs do
      begin
        BX := $09;
        ES := OS(nwGlobalBufRealPtr).S;
        SI := OS(nwGlobalBufRealPtr).O;
      end;
    Dummy := IPXCall(Regs);
    Move(nwGlobalBuf^, Address, SizeOf(IPXAddress));
  end;
{--------}
function IPXGetLocalTarget(var LocalTarget : TargetType;
                           var TransportTime : word) : byte;
  var
    Regs : TnwRegisters;
    CallResult : byte;
  begin
    Move(LocalTarget, nwGlobalBuf^, SizeOf(TargetType));
    nwInitRegs(Regs);
    with Regs do
      begin
        BX := $02;
        ES := OS(nwGlobalBufRealPtr).S;
        SI := OS(nwGlobalBufRealPtr).O;
        DI := SI + sizeof(IPXAddress);
      end;
    CallResult := IPXCall(Regs);
    if (CallResult = 0) then
      begin
        TransportTime := Regs.CX;
        Move(nwGlobalBuf^, LocalTarget, SizeOf(TargetType));
      end;
    IPXGetLocalTarget := CallResult;
  end;


{===Socket access===}
function IPXOpenSocket(Socket : word; Forever : boolean) : byte;
  {-Open a socket}
  var
    Regs : TnwRegisters;
  begin
    nwInitRegs(Regs);
    if Forever then
      Regs.AX := $FF;
    Regs.DX := Swap(Socket);
    IPXOpenSocket := IPXCall(Regs);
  end;
{--------}
function IPXOpenUniqueSocket(var Socket : word; Forever : boolean) : word;
  {-Open a dynamic socket and return it}
  var
    Regs : TnwRegisters;
    CallResult : byte;
  begin
    nwInitRegs(Regs);
    if Forever then
      Regs.AX := $FF;
    CallResult := IPXCall(Regs);
    if (CallResult = 0) then
      Socket := Swap(Regs.DX);
    IPXOpenUniqueSocket := CallResult;
  end;
{--------}
procedure IPXCloseSocket(Socket : word);
  {-Close a socket}
  var
    Regs : TnwRegisters;
    Dummy : byte;
  begin
    nwInitRegs(Regs);
    Regs.BX := 1;
    Regs.DX := Swap(Socket);
    Dummy := IPXCall(Regs);
  end;

{===IPX Communication routines===}
function IPXEventComplete(IPXEvent : PipxEventRec;
                          var CompletionCode : byte) : boolean;
  begin
    with IPXEvent^ do
      begin
        IPXRelinquish;
        if (ECB.InUse = 0) then
          begin
            IPXEventComplete := true;
            CompletionCode := ECB.CompletionCode;
          end
        else
          IPXEventComplete := false;
      end;
  end;
{--------}
procedure IPXRelinquish;
  var
    Regs : TnwRegisters;
    Dummy : byte;
  begin
    nwInitRegs(Regs);
    Regs.BX := $0A;
    Dummy := IPXCall(Regs);
  end;
{--------}
function IPXSend(IPXEvent : PipxEventRec;
                 Receiver : IPXAddress;
                 Socket   : word;
                 WaitForCompletion : boolean;
                 DataPacketSize : word;
                 DataPacket     : PPacket) : byte;
  var
    TransportTime : word;
    ActualTarget  : TargetType;
    CallResult    : byte;
  begin
    CallResult := 0;
    {check that the DataPacket is valid}
    if IsAnotherPacketType(DataPacket) then
      CallResult := IPXErrBadPacket
    {check the size of the data passed to make sure it is not too big}
    else if (DataPacketSize > IPXMaxDataSize) then
      CallResult := IPXErrDataTooLarge
    {check that the event record is one of ours}
    else if IsAnotherPacketType(IPXEvent) then
      CallResult := IPXErrBadEventRec;
    {open the socket}
    if (CallResult = 0) then
      begin
        CallResult := IPXOpenSocket(Socket, false);
        if (CallResult = IPXErrSocketAlreadyOpen) then
          CallResult := 0;
      end;
    {exit if there's a problem so far}
    if (CallResult <> 0) then
      begin
        IPXSend := CallResult;
        Exit;
      end;
    {do the work}
    with IPXEvent^, ECB, Header do
      begin
        {Set the fields in the ECB}
        InUse := 0;
        SocketNumber := Swap(Socket);
        {the high level IPX routines always use two buffer fragments. FD1
         points to the IPX header, and FD2 points to the user data.}
        FragmentCount  := 2;
        with FD1 do
          begin               {point FD1 to the TipxHeader}
            Data := GetRealModePtr(IPXEvent);
            inc(OS(Data).O, sizeof(TipxECB));
            Size := SizeOf(TipxHeader);
          end;
        with FD2 do
          begin               {point FD2 to the user data}
            Data := GetRealModePtr(DataPacket);
            Size := DataPacketSize;
          end;
        {init the ImmediateAddress}
        Move(Receiver, ActualTarget, SizeOf(IPXAddress));
        if IPXGetLocalTarget(ActualTarget, TransportTime) <> 0 then
          Move(Receiver.Node, ActualTarget.TargetNode,
               SizeOf(ActualTarget.TargetNode));
        Move(ActualTarget.TargetNode,ImmediateAddress,
             SizeOf(PhysicalNodeAddress));
        {init the IPX Header}
        PacketType := 4; {4 = Packet Exchange Packet}
        {init the Destination record with Network, node and socket info}
        Destination.Network := Receiver.Network;
        Destination.Node := Receiver.Node;
        Destination.Socket := Swap(Socket);
        {Get IPX to do the sending}
        CallResult := IPXCallWithECB(3, GetRealModePtr(IPXEvent));
        if (CallResult = 0) and WaitForCompletion then
          repeat until IPXEventComplete(IPXEvent, CallResult);
        IPXSend := CallResult;
      end;
  end;

function IPXListen(IPXEvent : PipxEventRec;
                   Socket   : word;
                   WaitForCompletion : boolean;
                   MaxPacketSize : word;
                   DataPacket    : PPacket) : byte;
  var
    CallResult : byte;
  begin
    CallResult := 0;
    {check that the DataPacket is valid}
    if IsAnotherPacketType(DataPacket) then
      CallResult := IPXErrBadPacket
    {check the size of the data passed to make sure it is not too big}
    else if (MaxPacketSize > IPXMaxDataSize) then
      CallResult := IPXErrDataTooLarge
    {check that the event record is one of ours}
    else if IsAnotherPacketType(IPXEvent) then
      CallResult := IPXErrBadEventRec;
    {open the socket}
    if (CallResult = 0) then
      begin
        CallResult := IPXOpenSocket(Socket, false);
        if (CallResult = IPXErrSocketAlreadyOpen) then
          CallResult := 0;
      end;
    {exit if there's a problem so far}
    if (CallResult <> 0) then
      begin
        IPXListen := CallResult;
        Exit;
      end;
    {do the work}
    with IPXEvent^, ECB do
      begin
        {Set the fields in the ECB}
        InUse := 0;
        SocketNumber := Swap(Socket);
        {the high level IPX routines always use two buffer fragments. FD1
         points to the IPX header, and FD2 points to the user data.}
        FragmentCount := 2;
        with FD1 do
          begin               {point FD1 to the TipxHeader}
            Data := GetRealModePtr(IPXEvent);
            inc(OS(Data).O, sizeof(TipxECB));
            Size := SizeOf(TipxHeader);
          end;
        with FD2 do
          begin               {point FD2 to the user data}
            Data := GetRealModePtr(DataPacket);
            Size := MaxPacketSize;
          end;
        {Get IPX to listen in}
        CallResult := IPXCallWithECB(4, GetRealModePtr(IPXEvent));
        if (CallResult = 0) and WaitForCompletion then
          repeat until IPXEventComplete(IPXEvent, CallResult);
        IPXListen := CallResult;
      end;
  end;
{--------}
function IPXCancelEvent(IPXEvent : PipxEventRec) : byte;
  var
    CancelResult : byte;
  begin
    if IsAnotherPacketType(IPXEvent) then
      IPXCancelEvent := IPXErrBadEventRec
    else
      begin
        CancelResult := IPXCallWithECB(6, GetRealModePtr(IPXEvent));
        {zero out the ECB: recommendation by Steve Meyer (Avanti)}
        if (CancelResult = 0) then
          FillChar(IPXEvent^.ECB, sizeof(TipxECB), 0);
        IPXCancelEvent := CancelResult;
      end;
  end;


{*******************************************
 AES Services (Asynchronous Event Scheduler)
 *******************************************}

{$IFDEF DPMIorWnd}
procedure AESEventHandler(RealModePtr : nwLong);
  {-The DPMI AES event handler. Works out which ECB was returned for
    processing by AES and calls its event handler.}
  var
    ECB : PAESECB;
    StillLooking : boolean;
    R : pointer;
  begin
    ECB := AESECBList;
    StillLooking := true;
    while StillLooking and (ECB <> nil) do
      if (nwLong(GetRealModePtr(ECB)) <> RealModePtr) then
        ECB := ECB^.Next
      else {We've found the relevant AES ECB}
        begin
          StillLooking := false;
          {call the user's ESR}
          ECB^.UserESR(ECB);
        end;
  end;

procedure AESCallbackShell; far; assembler;
  asm
    {Note: this uses the same processing as SPXCallBackShell. See that
           routine for comments.}
    mov ax, 1681h
    int 2Fh
    push es; push di
    cld
    lodsw; mov es:[di].DPMIRegisters.&IP, ax
    lodsw; mov es:[di].DPMIRegisters.&CS, ax
    add es:[di].DPMIRegisters.&SP, 4
    push word ptr es:[di].DPMIRegisters.&ES
    push word ptr es:[di].DPMIRegisters.&SI.word[0]
    mov ax, es
    mov ds, ax
    call AESEventHandler
    pop di; pop es
    mov ax,1682h
    int 2Fh
    iret
  end;

function AESAllocateDPMIESR : boolean;
  begin
    AESAllocateDPMIESR :=
       AllocRealModeCallBackAddr(@AESCallBackShell,
                                 AESEventHandlerRegs,
                                 AESCallBack) = 0;
  end;

procedure AESFreeDPMIESR;
  var
    Dummy : word;
  begin
    Dummy := FreeRealModeCallBackAddr(AESCallBack);
  end;
{$ELSE}
procedure AESESRDispatch; far; assembler;
  asm
    mov ax, SEG @Data
    mov ds, ax
    push es
    push si
    call dword ptr es:[si].TAESECB.UserESR
  end;
{$ENDIF}

function AESAllocECB(ESR : AESEventServiceRoutine) : PAESECB;
  var
    P : PAESECB;
  begin
    P := PAESECB(IPXAllocPacket(sizeof(TAESECB)));
    if (P <> nil) then
      with P^ do
        begin
          {zeroize the entire record (incl ESRAddress), and set the
           user's ESR field}
          FillChar(P^, sizeof(TAESECB), 0);
          UserESR := ESR;
          {$IFDEF DPMIorWnd}
          if AESECBList = nil then
            if not AESAllocateDPMIESR then
              begin
                IPXFreePacket(PPacket(P));
                AESAllocECB := nil;
                Exit;
              end;
          Next := AESECBList;
          AESECBList := P;
          {If there is an actual ESR, set the ESRAddress field so that
           AES can call us via the call back}
          if (@ESR <> nil) then
            ESRAddress := AESCallBack;
          {$ELSE}
          {If there is an actual ESR, set the ESRAddress field so that
           AES can call us via the AES dispatcher}
          if (@ESR <> nil) then
            ESRAddress := @AESESRDispatch;
          {$ENDIF}
        end;
    AESAllocECB := P;
  end;

function AESCancelEvent(ECB : PAESECB) : byte;
  begin
    if IsAnotherPacketType(ECB) then
      AESCancelEvent := AESErrBadECB
    else
      AESCancelEvent := IPXCallWithECB(6, GetRealModePtr(ECB));
  end;

function AESEventComplete(ECB : PAESECB) : boolean;
  begin
    IPXRelinquish;
    AESEventComplete := (ECB^.InUse = 0);
  end;

procedure AESFreeECB(ECB : PAESECB);
  var
    Temp, Dad : PAESECB;
  begin
    {$IFDEF DPMIorWnd}
    Temp := AESECBList;
    Dad := nil;
    while (Temp <> nil) and (Temp <> ECB) do
      begin
        Dad := Temp;
        Temp := Temp^.Next;
      end;
    if (Temp <> nil) then
      if (Dad = nil) then
        begin
          AESECBList := Temp^.Next;
          if (AESECBList = nil) then
            AESFreeDPMIESR;
        end
      else
        Dad^.Next := Temp^.Next;
    {$ENDIF}
    {free the ECB as a packet}
    IPXFreePacket(PPacket(ECB));
  end;

function AESScheduleEvent(DelayTicks : word; ECB : PAESECB) : byte;
  var
    Regs : TnwRegisters;
    RealECB : pointer;
  begin
    if IsAnotherPacketType(ECB) then
      AESScheduleEvent := AESErrBadECB
    else
      begin
        RealECB := GetRealModePtr(ECB);
        nwInitRegs(Regs);
        with Regs do
          begin
            BX := $07;
            AX := DelayTicks;
            ES := OS(RealECB).S;
            SI := OS(RealECB).O;
          end;
        AESScheduleEvent := IPXCall(Regs);
      end;
  end;


{*************************************************
 SPX Services (Sequenced Packet Exchange Protocol)
 *************************************************}

{===Handy routines===}
procedure SPXEventHandler(RealModeSeg, RealModeOfs : word); far;
  {-The listen pool event handler. Works out which ECB was returned for
    processing by SPX and places it in the pool queue to be processed.
    WARNING: this routine's code assumes that the segment/selector value
    will be different for each PspxEventRec. In pmode this means that
    GlobalDOSAlloc must provide different selector values for each
    allocation (it does as the offset is assumed 0) and in real mode that
    GetMem gives you a different segment value for each allocation (it does
    for TP6 and BP7: the pointers are always normalised with offsets 0 or
    8).}
  var
    SPXEvent : PspxEventRec;
    StillLooking : boolean;
    R : pointer;
    Index : integer;
  begin
    SPXEvent := SPXRecList;
    StillLooking := true;
    while StillLooking and (SPXEvent <> nil) do
      begin
        R := GetRealModePtr(SPXEvent);
        if (OS(R).S <> RealModeSeg) then
          SPXEvent := SPXEvent^.Next
        else {We've found the relevant SPX event record}
          begin
            Index := 1;
            with SPXEvent^ do
              while StillLooking do
                if (OS(Pool[Index]).O <> RealModeOfs) then
                  begin
                    inc(Index);
                    if (Index > PoolCount) then
                      StillLooking := false;
                  end
                else
                  begin
                    StillLooking := false;
                    {Note: interrupts are disabled so we can alter
                     the queue with impunity}
                    inc(QueueCount);
                    Queue[QueueCount] := Index;
                  end;
          end;
      end;
  end;
{--------}
{$IFDEF DPMIorWnd}
procedure SPXCallbackShell; far; assembler;
  asm
    {WARNING - this assumes that the DPMI registers structure is a
               global variable. SPXEventHandlerRegs certainly is.}

    {Entering a critical section}
    mov ax, 1681h
    int 2Fh
    { ES:DI points to the DPMI register structure }
    { DS:SI points to the real mode's SS:SP }
    {Save the DPMI register structure for returning to real-mode}
    push es; push di
    {Set up the return registers}
    cld
    lodsw; mov es:[di].DPMIRegisters.&IP, ax
    lodsw; mov es:[di].DPMIRegisters.&CS, ax
    add es:[di].DPMIRegisters.&SP, 4
    {Set things up for SPXEventHandler}
    push word ptr es:[di].DPMIRegisters.&ES
    push word ptr es:[di].DPMIRegisters.&SI.word[0]
    mov ax, es
    mov ds, ax
    call SPXEventHandler
    {Get the return DPMI register structure}
    pop di; pop es
    {Leaving critical section}
    mov ax,1682h
    int 2Fh
    iret
  end;
{--------}
function SPXAllocateDPMIESR : boolean;
  begin
    SPXAllocateDPMIESR :=
       AllocRealModeCallBackAddr(@SPXCallBackShell,
                                 SPXEventHandlerRegs,
                                 SPXCallBack) = 0;
  end;
{--------}
procedure SPXFreeDPMIESR;
  var
    Dummy : word;
  begin
    Dummy := FreeRealModeCallBackAddr(SPXCallBack);
  end;
{$ELSE}
procedure SPXCallBack; far; assembler;
  {-A stub for SPX to call SPXEventHandler}
  asm
    mov ax, SEG @Data
    mov ds, ax
    push es
    push si
    call SPXEventHandler
  end;
{$ENDIF}
{--------}
function GetRealModePoolECBPtr(SPXEvent : PspxEventRec; Index : byte) : pointer;
  {-Returns the real mode pointer to the Index'th listen pool ECB in
    SPXEvent - equivalent to a real mode pointer to the pool record
    itself.}
  {$IFDEF DPMIorWnd}
  var
    R : pointer;
    ECB : pointer;
  begin
    R := GetRealModePtr(SPXEvent);
    ECB := SPXEvent^.Pool[Index]; {the ECB is the first field}
    GetRealModePoolECBPtr := Ptr(OS(R).S, OS(ECB).O);
  end;
  {$ELSE}
  begin
    GetRealModePoolECBPtr := SPXEvent^.Pool[Index]; {the ECB is the first field}
  end;
  {$ENDIF}
{--------}
procedure SPXClearOnDisconnect(SPXEvent : PspxEventRec);
  {-Clears the Header and ECB, the pool records and closes the socket
    when clearing a connection}
  var
    PoolIndex : word;
    Socket : word;
  begin
    with SPXEvent^ do
      begin
        Socket := Swap(ECB.SocketNumber);
        IPXCloseSocket(Socket);
        FillChar(ECB, sizeof(TipxECB), 0);
        FillChar(Header, sizeof(TspxHeader), 0);
        AmConnected := false;
        ConnID := 0;
        QueueCount := 0;
        for PoolIndex := 1 to PoolCount do
          with Pool[PoolIndex]^ do
            begin
              with PoolECB do
                FillChar(InUse,
                         sizeof(InUse)+sizeof(CompletionCode)+
                            sizeof(SocketNumber)+sizeof(IPXWorkSpace)+
                            sizeof(DriverWorkSpace)+sizeof(ImmediateAddress),
                         0);
              FillChar(PoolHeader, sizeof(TspxHeader), 0);
            end;
      end;
  end;


{===Primary routines===}
function SPXEstablishConnPrim(RetryCount : byte; WatchDog : boolean;
                              ECB : PipxECB;
                              var AssignedConn : word) : byte;
  {Note: in pmode ECB is a real-mode pointer}
  var
    Regs : TnwRegisters;
    CallResult : byte;
  begin
    nwInitRegs(Regs);
    with Regs do
      begin
        BX := $11;
        AX := (word(WatchDog) shl 8) + RetryCount;
        ES := OS(ECB).S;
        SI := OS(ECB).O;
      end;
    CallResult := IPXCall(Regs);
    if (CallResult = 0) then
      AssignedConn := Regs.DX;
    SPXEstablishConnPrim := CallResult;
  end;
{--------}
procedure SPXListenForConnPrim(Retry : byte; WatchDog : boolean;
                               ECB : PipxECB);
  {Note: in pmode ECB is a real-mode pointer}
  var
    Regs : TnwRegisters;
    Dummy : byte;
  begin
    nwInitRegs(Regs);
    with Regs do
      begin
        BX := $12;
        AX := (word(WatchDog) shl 8) + Retry;
        ES := OS(ECB).S;
        SI := OS(ECB).O;
      end;
    Dummy := IPXCall(Regs);
  end;
{--------}
procedure SPXTerminateConnPrim(ConnID : word;
                               ECB : PipxECB);
  {Note: in pmode ECB is a real-mode pointer}
  var
    Regs : TnwRegisters;
    Dummy : byte;
  begin
    nwInitRegs(Regs);
    with Regs do
      begin
        BX := $13;
        DX := ConnID;
        ES := OS(ECB).S;
        SI := OS(ECB).O;
      end;
    Dummy := IPXCall(Regs);
  end;
{--------}
procedure SPXAbortConnPrim(ConnID : word);
  var
    Regs : TnwRegisters;
    Dummy : byte;
  begin
    nwInitRegs(Regs);
    with Regs do
      begin
        BX := $14;
        DX := ConnID;
    end;
    Dummy := IPXCall(Regs);
  end;
{--------}
procedure SPXSendPrim(ConnID : word; ECB : PipxECB);
  var
    Regs : TnwRegisters;
    Dummy : byte;
  begin
    nwInitRegs(Regs);
    with Regs do
      begin
        BX := $16;
        DX := ConnID;
        ES := OS(ECB).S;
        SI := OS(ECB).O;
      end;
    Dummy := IPXCall(Regs);
  end;
{--------}
procedure SPXListenPrim(ECB : PipxECB);
  var
    Regs : TnwRegisters;
    Dummy : byte;
  begin
    nwInitRegs(Regs);
    with Regs do
      begin
        BX := $17;
        ES := OS(ECB).S;
        SI := OS(ECB).O;
      end;
    Dummy := IPXCall(Regs);
  end;


{===Utiltity SPX routines===}
function SPXServicesAvail(var Version : word;
                          var MaxSPXConn : word;
                          var AvailSPXConn : word) : boolean;
  var
    Regs : TnwRegisters;
    CallResult : byte;
  begin
    SPXServicesAvail := false;
    if IPXServicesAvailable then
      begin
        nwInitRegs(Regs);
        with Regs do
          begin
            BX := $10;
            CallResult := IPXCall(Regs);
            if (CallResult = $FF) then
              begin
                Version := BX;
                MaxSPXConn := CX;
                AvailSPXConn := DX;
                SPXServicesAvail := true;
              end;
          end;
      end;
  end;
{--------}
function SPXEventComplete(SPXEvent : PspxEventRec;
                          var FinalResult : byte) : boolean;
  {-Note: should be used only for SPXSend, SPXEstablishConn and
          SPXListenForConn type events.}
  begin
    if IsAnotherPacketType(SPXEvent) then
      begin
        SPXEventComplete := true;
        FinalResult := SPXErrBadEventRec;
      end
    else
      with SPXEvent^, ECB do
        begin
          IPXRelinquish;
          if (InUse = 0) then
            begin
              SPXEventComplete := true;
              FinalResult := CompletionCode;
              if (not AmConnected) and (FinalResult = 0) then
                begin
                  ConnID := word(ECB.IPXWorkSpace);
                  AmConnected := true;
                end;
              if (FinalResult = SPXErrFailedConn) or
                 (FinalResult = SPXErrRemoteEndedConn) then
                begin
                  SPXClearOnDisconnect(SPXEvent);
                end;
            end
          else
            SPXEventComplete := false;
        end;
  end;
{--------}
function SPXGetConnStatus(SPXEvent   : PspxEventRec;
                      var ConnStatus : TSPXStatus) : byte;
  var
    Regs : TnwRegisters;
    CallResult : byte;
  begin
    if IsAnotherPacketType(SPXEvent) then
      SPXGetConnStatus := SPXErrBadEventRec
    else
      begin
        FillChar(nwGlobalBuf^, sizeof(TSPXStatus), 0);
        nwInitRegs(Regs);
        with Regs do
          begin
            BX := $15;
            DX := SPXEvent^.ConnID;
            ES := OS(nwGlobalBufRealPtr).S;
            SI := OS(nwGlobalBufRealPtr).O;
          end;
        CallResult := IPXCall(Regs);
        if (CallResult = 0) then
          begin
            Move(nwGlobalBuf^, ConnStatus, sizeof(TSPXStatus));
            with ConnStatus do
              begin
                SourceConn := Swap(SourceConn);
                DestConn := Swap(DestConn);
                SequenceNum := Swap(SequenceNum);
                AckNum := Swap(AckNum);
                AllocNum := Swap(AllocNum);
                RemoteAckNum := Swap(RemoteAckNum);
                RemoteAllocNum := Swap(RemoteAllocNum);
                ConnSocket := Swap(ConnSocket);
                RetransmitCount := Swap(RetransmitCount);
                EstimatedRoundTripDelay := Swap(EstimatedRoundTripDelay);
                RetransmittedPackets := Swap(RetransmittedPackets);
                SuppressedPackets := Swap(SuppressedPackets);
              end;
          end;
        SPXGetConnStatus := CallResult;
      end;
  end;


{===SPX Event records allocation and deallocation===}
function SPXAllocEventRec(NumECBs : byte; MaxPacketSize : word) : PspxEventRec;
  var
    PoolRecSize : word;
    Size        : word;
    Count       : word;
    R : pointer;
    P : pointer;
    SPXEvent : PspxEventRec;
  begin
    {This routine attempts to allocate the Event record as one block
     and then splits it up into ECBs and data packets. The splitting
     process imposes the following data layout for the block:
        SPXRec
        Pool[1]
        Pool[2]
        ...
        Pool[NumECBs]
     Each Pool[x] has exactly MaxPacketSize bytes in its data block.
     The size of the block allocated will be
       sizeof(SPXRec) +
       NumECBs * (sizeof(TipxECB) + sizeof(TspxHeader) + MaxPacketSize)
     which will be a maximum of 10,364 bytes.}

    {get simple checks out of the way}
    SPXAllocEventRec := nil;
    if (NumECBs < 2) or (NumECBs > SPXMaxPoolCount) or
       (MaxPacketSize = 0) or (MaxPacketSize > SPXMaxDataSize) then
      Exit;
    {calculate the block size, and allocate it in one go}
    PoolRecSize := sizeof(TipxECB) + sizeof(TspxHeader) + MaxPacketSize;
    Size := sizeof(TspxEventRec) + (NumECBs * PoolRecSize);
    SPXEvent := IPXAllocPacket(Size);
    if (SPXEvent = nil) then Exit;
    {clean it up}
    FillChar(SPXEvent^, Size, 0);
    {set the values for the SPXRec fields}
    with SPXEvent^ do
      begin
        {$IFDEF DPMIorWnd}
        if (SPXRecList = nil) then
          if not SPXAllocateDPMIESR then
            begin
              IPXFreePacket(PPacket(SPXEvent));
              SPXAllocEventRec := nil;
              exit;
            end;
        {$ENDIF}
        Next := SPXRecList;
        SPXRecList := SPXEvent;
        PacketSize := MaxPacketSize;
        PoolCount := NumECBs;
        {set up the pool pointers (in pmode these'll be pmode pointers)}
        P := Ptr(OS(SPXEvent).S, OS(SPXEvent).O + sizeof(TspxEventRec));
        Pool[1] := P;
        for Count := 2 to NumECBs do
          begin
            inc(OS(P).O, PoolRecSize);
            Pool[Count] := P;
          end;
        {now initialize the pool ECBs' fragment descriptors: each ECB
         has two descriptors, the first to the relevant SPX header, the
         second to the data buffer. Real-mode pointers of course.}
        for Count := 1 to NumECBs do
          with Pool[Count]^.PoolECB do
            begin
              FragmentCount := 2;
              {$IFDEF DPMIorWnd}
              ESRAddress := SPXCallBack;
              {$ELSE}
              ESRAddress := @SPXCallBack;
              {$ENDIF}
              R := GetRealModePoolECBPtr(SPXEvent, Count);
              FD1.Data := Ptr(OS(R).S, OS(R).O + sizeof(TipxECB));
              FD1.Size := sizeof(TspxHeader);
              FD2.Data := Ptr(OS(R).S, OS(R).O + sizeof(TipxECB)+sizeof(TspxHeader));
              FD2.Size := PacketSize;
          end;
      end;
    SPXAllocEventRec := SPXEvent;
  end;
{--------}
procedure SPXFreeEventRec(SPXEvent : PspxEventRec);
  var
    Temp, Dad : PspxEventRec;
  begin
    if IsAnotherPacketType(SPXEvent) then Exit;
    {unlink this SPXEvent from the linked list of Event records}
    Temp := SPXRecList;
    Dad := nil;
    while (Temp <> nil) and (Temp <> SPXEvent) do
      begin
        Dad := Temp;
        Temp := Temp^.Next;
      end;
    if (Temp <> nil) then
      if (Dad = nil) then
        begin
          SPXRecList := Temp^.Next;
          {$IFDEF DPMIorWnd}
          if (SPXRecList = nil) then
            SPXFreeDPMIESR;
          {$ENDIF}
        end
      else
        Dad^.Next := Temp^.Next;

    {free the SPXEvent as a packet}
    IPXFreePacket(SPXEvent);
  end;


{===SPX communications===}
function SPXSend(SPXEvent          : PspxEventRec;
                 WaitForCompletion : boolean;
                 DataPacketSize    : word;
                 DataPacket        : PPacket) : byte;
  var
    FinalResult : byte;
  begin
    if IsAnotherPacketType(SPXEvent) then
      begin
        SPXSend := SPXErrBadEventRec;
        Exit;
      end;
    if IsAnotherPacketType(DataPacket) then
      begin
        SPXSend := IPXErrBadPacket;
        Exit;
      end;
    with SPXEvent^, ECB do
      begin
        if not AmConnected then
          begin
            SPXSend := SPXErrNoConnection;
            Exit;
          end;
        {init the required fields of the send ECB}
        {the high level SPX routines always use two buffer fragments. FD1 }
        {points to the SPX header, and FD2 points to the user data.}
        FragmentCount := 2;
        with FD1 do
          begin                         {point FD1 at the SPX header}
            Data := GetRealModePtr(SPXEvent);
            inc(OS(Data).O, sizeof(TipxECB));
            Size := SizeOf(TspxHeader);
          end;
        with FD2 do
          begin                         {point FD2 at the user data}
            Data := GetRealModePtr(DataPacket);
            Size := DataPacketSize;
          end;
        Header.IPXHeader.PacketType := 5; {SPX packet}
        {submit the send request to SPX}
        SPXSendPrim(ConnID, GetRealModePtr(SPXEvent));
        if WaitForCompletion then
          begin
            repeat until SPXEventComplete(SPXEvent, FinalResult);
            SPXSend := FinalResult;
          end
        else
          SPXSend := 0;
      end;
  end;
{--------}
function SPXPacketReceived(SPXEvent           : PspxEventRec;
                           var Index          : byte;
                           var DataType       : byte;
                           var CompletionCode : byte;
                           var DataPtr        : pointer) : boolean;
  var
    RemoteHasGone : boolean;
  begin
    if IsAnotherPacketType(SPXEvent) then
      begin
        {if it's not a valid record, fake a received packet}
        Index := 0;
        DataType := 0;
        CompletionCode := SPXErrBadEventRec;
        DataPtr := nil;
        SPXPacketReceived := true;
      end
    else
      begin
        IPXRelinquish;

        with SPXEvent^ do
          if (QueueCount = 0) then
            SPXPacketReceived := false
          else
            begin
              RemoteHasGone := false;
              SPXPacketReceived := true;
              Index := Queue[1];
              with Pool[Queue[1]]^ do
                begin
                  DataPtr := @PoolData;
                  DataType := PoolHeader.DataType;
                  {Check for the remote terminating the connection}
                  if (DataType = SPXEndOfConn) then
                    begin
                      RemoteHasGone := true;
                      CompletionCode := SPXErrRemoteHasGone;
                    end
                  else
                    begin
                      CompletionCode := PoolECB.CompletionCode;
                      {Check for the SPX watchdog noticing the connection
                      has been broken and informing us}
                      if (CompletionCode = SPXErrFailedConn) then
                        RemoteHasGone := true;
                    end;
                end;
              if RemoteHasGone then
                SPXClearOnDisconnect(SPXEvent)
              else
                begin
                  DisableInterrupts;
                  dec(QueueCount);
                  if (QueueCount > 0) then
                    Move(Queue[2], Queue[1], QueueCount);
                  EnableInterrupts;
                end;
            end;
      end;
  end;
{--------}
procedure SPXReactivateECB(SPXEvent : PspxEventRec;
                           Index    : byte);
  begin
    if not IsAnotherPacketType(SPXEvent) then
      with SPXEvent^ do
        if AmConnected and
           (1 <= Index) and (Index <= PoolCount) and
           (Pool[Index]^.PoolECB.InUse = 0) then
          SPXListenPrim(GetRealModePoolECBPtr(SPXEvent, Index));
  end;

function SPXECBsListening(SPXEvent : PspxEventRec) : byte;
  var
    Num   : byte;
    Index : word;
  begin
    Num := 0;
    if not IsAnotherPacketType(SPXEvent) then
      with SPXEvent^ do
        for Index := 1 to PoolCount do
          if (Pool[Index]^.PoolECB.InUse <> 0) then
            inc(Num);
    SPXECBsListening := Num;
  end;


{===Opening an SPX Connection===}
function SPXEstablishConn(SPXEvent     : PspxEventRec;
                          Receiver     : IPXAddress;
                          LocalSocket  : word;
                          RemoteSocket : word;
                          WaitForCompletion : boolean) : byte;
  var
    PoolIndex   : word;
    FinalResult : byte;
    PrimResult  : byte;
  begin
    PrimResult := 0;
    if IsAnotherPacketType(SPXEvent) then
      PrimResult := SPXErrBadEventRec
    else if (LocalSocket = RemoteSocket) then
      PrimResult := SPXErrSameSockets
    else
      begin
        PrimResult := IPXOpenSocket(LocalSocket, false);
        if (PrimResult = IPXErrSocketAlreadyOpen) then
          PrimResult := 0;
      end;
    {if we have an error so far, exit}
    if (PrimResult <> 0) then
      begin
        SPXEstablishConn := PrimResult;
        Exit;
      end;
    {do the work}
    with SPXEvent^ do
      begin
        {zeroize the connection ECB and SPX header, etc}
        FillChar(ECB, sizeof(TipxECB), 0);
        FillChar(Header, sizeof(TspxHeader), 0);
        ConnID := 0;
        QueueCount := 0;
        {init the connection Event Control Block}
        with ECB do
          begin
            SocketNumber := Swap(LocalSocket);
            {we only use one buffer fragment to initiate a connection,
             it should point to the the SPX header}
            FragmentCount  := 1;
            with FD1 do
              begin
                Data := GetRealModePtr(SPXEvent);
                inc(OS(Data).O, sizeof(TipxECB));
                Size := sizeof(TspxHeader);
              end;
          end;
        {init the connection SPX header}
        Receiver.Socket := Swap(RemoteSocket);
        Header.IPXHeader.PacketType := 5;
        Header.IPXHeader.Destination := Receiver;

        {post all the pooled listen ECBs - we've got to receive the
         connection established message, etc}
        for PoolIndex := 1 to PoolCount do
          begin
            with Pool[PoolIndex]^.PoolECB do
              SocketNumber := Swap(LocalSocket);
            SPXListenPrim(GetRealModePoolECBPtr(SPXEvent, PoolIndex));
          end;

        {call the low level establish connection routine}
        PrimResult := SPXEstablishConnPrim(SPXRetryCount,
                                           SPXWatchDog,
                                           GetRealModePtr(SPXEvent),
                                           ConnID);
        SPXEstablishConn := PrimResult;
        if (PrimResult = 0) then
          begin
            AmConnected := true;
            if WaitForCompletion then
              begin
                repeat until SPXEventComplete(SPXEvent, FinalResult);
                SPXEstablishConn := FinalResult;
              end;
          end;
      end;
  end;
{--------}
function SPXListenForConn(SPXEvent    : PspxEventRec;
                          LocalSocket : word;
                          WaitForCompletion : boolean) : byte;
  var
    PoolIndex   : word;
    FinalResult : byte;
    PrimResult  : byte;
  begin
    PrimResult := 0;
    if IsAnotherPacketType(SPXEvent) then
      PrimResult := SPXErrBadEventRec
    else
      begin
        PrimResult := IPXOpenSocket(LocalSocket, false);
        if (PrimResult = IPXErrSocketAlreadyOpen) then
          PrimResult := 0;
      end;
    {if we have an error so far, exit}
    if (PrimResult <> 0) then
      begin
        SPXListenForConn := PrimResult;
        Exit;
      end;
    {do the work}
    with SPXEvent^ do
      begin
        {zeroize the connection ECB and SPX header, etc}
        FillChar(ECB, sizeof(TipxECB), 0);
        FillChar(Header, sizeof(TspxHeader), 0);
        ConnID := 0;
        QueueCount := 0;
        {init the connection Event Control Block}
        with ECB do
          begin
            SocketNumber := Swap(LocalSocket);
            {we only use one buffer fragment to listen for a connection,
             it should point to the the SPX header}
            FragmentCount  := 1;
            with FD1 do
              begin
                Data := GetRealModePtr(SPXEvent);
                inc(OS(Data).O, sizeof(TipxECB));
                Size := sizeof(TspxHeader);
              end;
          end;
        {post all the pooled listen ECBs - we've got to receive the
         Establish connection message, etc}
        for PoolIndex := 1 to PoolCount do
          begin
            with Pool[PoolIndex]^.PoolECB do
              SocketNumber := Swap(LocalSocket);
            SPXListenPrim(GetRealModePoolECBPtr(SPXEvent, PoolIndex));
          end;
        {call the low level establish connection routine}
        SPXListenForConnPrim(SPXRetryCount,
                             SPXWatchDog,
                             GetRealModePtr(SPXEvent));
        if WaitForCompletion then
          begin
            repeat until SPXEventComplete(SPXEvent, FinalResult);
            SPXListenForConn := FinalResult;
          end
        else
          SPXListenForConn := 0;
      end;
  end;


{===Cancelling SPX Connections===}
procedure SPXTerminateConn(SPXEvent : PspxEventRec);
  begin
    if IsAnotherPacketType(SPXEvent) then Exit;
    with SPXEvent^ do
      begin
        if not AmConnected then Exit;
        SPXTerminateConnPrim(ConnID, GetRealModePtr(SPXEvent));
        while (ECB.InUse <> 0) do
          IPXRelinquish;
      end;
    SPXClearOnDisconnect(SPXEvent);
  end;
{--------}
procedure SPXCancelListenForConn(SPXEvent : PspxEventRec);
  var
    CancelResult : byte;
  begin
    if IsAnotherPacketType(SPXEvent) then Exit;
    {An SPX Listen For Connection is cancelled with an IPX Cancel Event}
    if (SPXEvent^.ECB.InUse <> 0) then
      CancelResult := IPXCallWithECB(6, GetRealModePtr(SPXEvent));
    SPXClearOnDisconnect(SPXEvent);
  end;
{--------}
procedure SPXAbortConn(SPXEvent : PspxEventRec);
  begin
    if IsAnotherPacketType(SPXEvent) then Exit;
    with SPXEvent^ do
      begin
        if not AmConnected then Exit;
        SPXAbortConnPrim(ConnID);
      end;
    SPXClearOnDisconnect(SPXEvent);
  end;

begin
  {Automatically initialize IPX}
  InitializeIPX;
  {Set the 'do nothing' ESR addresses}
  @IPXDoNothingESR := nil;
  @AESDoNothingESR := nil;
end.

