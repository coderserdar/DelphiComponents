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
    {$Q-,P-}
  {$ENDIF}
  {all other compiler options are 'don't care'}

{$IFDEF Win32}
  !! Error - this unit cannot be compiled for 32-bit
{$ENDIF}

unit NWPrint;
  {-Unit to provide NetWare printing functionality.}

interface

uses
  {$IFDEF Windows}
   {$IFDEF Ver80}
   SysUtils,
   DosSupp,
   {$ELSE}
   WinDOS,
   {$ENDIF}
  {$ELSE}
  DOS,
  {$ENDIF}
  NWBase,
  NWConn,
  NWBind,
  NWFile;

const
  {NWPRINT extra error codes}
  nwpErrBadPrinter = $7F51;  {Bad LPTx parameter, not enough printers.}
  nwqErrNoSuchJob  = $89D5;  {No such job, it has been deleted or processed}

const
  {TnwCaptureFlags/TnwPrintJob: constants for PrintFlags field}
  nwpfSuppressFF   = $08;    {do not issue form feed at end}
  nwpfTabExpand    = $40;    {enable tab expansion}
  nwpfPrintBanner  = $80;    {print banner page}

const
  {TnwPrintJob: constants for JobControlFlags field}
  nwjcAutoStart    = $08;    {autostart even if server connection broken}
  nwjcRestart      = $10;    {remains in queue after job cancelled}
  nwjcEntryOpen    = $20;    {job file is being created}
  nwjcUserHold     = $40;    {user has job on hold}
  nwjcOperatorHold = $80;    {operator has job on hold}

const
  {Constant to put in TnwPrintJob.TargetExecTime to print the job
   as soon as possible}
  nwPrintASAP : TnwDate =
     (Year   : $FFFF; Month  : $FF; Day    : $FF;
      Hour   : $FF;   Minute : $FF; Second : $FF;
      WeekDay: nwSun);

type
  {Enumeration function type for print queues.}
  TnwEnumQueueFunc = function (Name : TnwObjectStr; ID : nwLong;
                               var ExtraData) : boolean;
  {Enumeration function type for print jobs.}
  TnwEnumPrintJobFunc = function (JobNumber : nwLong;
                                  var ExtraData) : boolean;

  {Possible printers (LPT4-LPT9 only available with VLMs.}
  TnwPrinter = (nwLPT1, nwLPT2, nwLPT3,
                nwLPT4, nwLPT5, nwLPT6,
                nwLPT7, nwLPT8, nwLPT9);

  {Banner strings: user name and job name}
  TnwBannerName = string[12];
  TnwBannerJob  = string[12];

  {A form name string}
  TnwFormName = string[12];

  {Data used/returned by workstation capture commands}
  TnwCaptureFlags = record
    {read/write values, can be set with nwSetCaptureFlags}
    PrintFlags        : byte;         {print flags}
    TabSize           : byte;         {tab size (1..18)}
    NumCopies         : byte;         {number of copies (0..255)}
    FormType          : byte;         {form type (0..255)}
    MaxLines          : word;         {maximum lines per page}
    MaxCols           : word;         {maximum columns per line}
    FlushTimeout      : word;         {ticks before automatic flush}
    BannerJobName     : TnwBannerJob; {print job name for banner page}
    FlushOnClose      : boolean;      {true when autoflush enabled}
    FormName          : TnwFormName;  {form name corresponding to FormType}
    {read only values - returned by nwGetCaptureFlags}
    Printer           : TnwPrinter;   {the local printer}
    IsCaptured        : boolean;      {true if printer is captured}
    IsCapturingData   : boolean;      {true if print data is being captured}
    IsDoingTimeOut    : boolean;      {true if capture is timing out}
    IsCapturedToFile  : boolean;      {true if captured to a file}
    Server            : TnwServer;    {server processing the capture}
    QueueID           : nwLong;       {print queue bindery ID if IsCapturedToFile is false}
  end;

  {Print Queue Job data structure}
  TnwPrintJob = record
    VerifyFlag     : word;          {** verify flag}
    Server         : word;          {** server handle}
    QueueID        : nwLong;        {** print queue bindery ID}
    ServerVersion  : word;          {** effective server version}
    ClientStation  : nwLong;        {** client who started job: conn. number}
    ClientTaskNum  : nwLong;        {** ...task number}
    ClientID       : nwLong;        {** ...bindery object ID}
    TargetServerID : nwLong;        {   print server ID, -1 = any}
    TargetExecTime : TnwDate;       {   time to print job, $FF = ASAP}
    JobEntryTime   : TnwDate;       {** time job entered the queue}
    JobNumber      : nwLong;        {** job number}
    JobType        : word;          {   type of job (usually 0)}
    JobPosition    : word;          {** position in queue, 1 = at top}
    JobControlFlags: word;          {   job control flags}
    JobFileName    : string[13];    {** filename of queue job}
    JobFileHandle  : TnwFileHandle; {** NetWare handle of JobFileName}
    ServerStation  : nwLong;        {** print server: conn.number}
    ServerTaskNum  : nwLong;        {** ...task number}
    ServerID       : nwLong;        {** ...bindery object ID}
    JobDesc        : string[49];    {   description of job}
    PrintFlags     : byte;          {   print flags}
    TabSize        : byte;          {   default tab size}
    FormName       : TnwFormName;   {   name of form to print on}
    NumCopies      : byte;          {   number of copies}
    MaxLines       : word;          {   max lines per page}
    MaxCols        : word;          {   max columns per page}
    BannerUserName : TnwBannerName; {   user name for banner page}
    BannerJobName  : TnwBannerJob;  {   print job name for banner page}
    JobFileSize    : nwLong;        {   size of file JobFileName}
    {** These fields are read only and are set by QMS, the others can
        be altered by nwChangePrintJob
     The TnwDate fields do not use or return the WeekDay field.}
  end;

{---Banner name routines---}
function nwGetBannerName(var Name : TnwBannerName) : TnwErrorCode;
  {-Return the default banner user name.}

function nwSetBannerName(Name : TnwBannerName) : TnwErrorCode;
  {-Set the default banner user name.}


{---Capture routines---}
function nwCancelCapture(Printer : TnwPrinter) : TnwErrorCode;
  {-Cancel capturing of Printer.}

function nwEndCapture(Printer : TnwPrinter) : TnwErrorCode;
  {-End capturing of Printer.}

function nwFlushCapture(Printer : TnwPrinter) : TnwErrorCode;
  {-Flush Printer's capture buffer.
    Note: if caturing to a file, this will close the file and end
          capturing under both VLMs and NETX.}

function nwGetCaptureFlags(Printer : TnwPrinter;
                           var CapFlags : TnwCaptureFlags) : TnwErrorCode;
  {-Get Printer's capture flags.}

function nwGetNumPrinters : byte;
  {-Get the number of printers the workstation can support.}

function nwIsCaptured(Printer : TnwPrinter) : boolean;
  {-Return true if Printer is being captured.}

function nwSetCaptureFlags(Printer : TnwPrinter;
                           var CapFlags : TnwCaptureFlags) : TnwErrorCode;
  {-Set Printer's capture flags.}

function nwStartCaptureToFile(Printer: TnwPrinter;
                              FileName : string) : TnwErrorCode;
  {-Start capturing Printer to a network file.}

function nwStartCaptureToQueue(Printer   : TnwPrinter;
                               Server    : TnwServer;
                               QueueName : TnwObjectStr) : TnwErrorCode;
  {-Start capturing Printer to a print queue.}


{---Queue routines---}
procedure nwEnumQueues(Server : TnwServer;
                       EnumFunc : TnwEnumQueueFunc; var ExtraData);
  {-Enumerate all print queues on a server.}

procedure nwqEnumPrintJobs(Server : TnwServer; QueueName : TnwObjectStr;
                           EnumFunc : TnwEnumPrintJobFunc;
                           var ExtraData);
  {-Enumerate all print jobs on a print queue.}

function nwqCreatePrintJobFile(Server : TnwServer; QueueName : TnwObjectStr;
                               var PrintJobData : TnwPrintJob;
                               var F : file) : TnwErrorCode;
  {-Create a new print job on a queue and return a Pascal file variable
    that you write to to add print data to the job.}

function nwqClosePrintJobFile(var PrintJobData : TnwPrintJob;
                              var F : file) : TnwErrorCode;
  {-Close a print job created by nwCreatePrintJobFile and release the
    print job ready for printing. Closes the file variable as well.}

function nwqAbortPrintJobFile(var PrintJobData : TnwPrintJob;
                              var F : file) : TnwErrorCode;
  {-Close a print job created by nwCreatePrintJobFile and delete it.
    Closes the file variable as well.}

function nwqChangePrintJob(var PrintJobData : TnwPrintJob) : TnwErrorCode;
  {-Change details of an existing print job.}

function nwqChangePrintJobPos(var PrintJobData : TnwPrintJob;
                              NewPosition : word) : TnwErrorCode;
  {-Change position of an existing print job. The user must be a
    queue operator.}

function nwqGetPrintJob(Server : TnwServer; QueueName : TnwObjectStr;
                        JobNumber : nwLong;
                        var PrintJobData : TnwPrintJob) : TnwErrorCode;
  {-Given the number of a job and the queue it is on, return the
    print job data structure.}

function nwqRemovePrintJob(var PrintJobData : TnwPrintJob) : TnwErrorCode;
  {-Delete an existing print job from the queue.}

function nwqRefreshPrintJob(var PrintJobData : TnwPrintJob) : TnwErrorCode;
  {-Refresh the data for an existing print job.}

implementation

type
  OS = record O, S : word; end; {to split pointer into sel/seg & ofs}
  LH = record L, H : word; end; {to split nwLong into hi/lo words}

type
  PString = ^string;

type
  TNewJobStruc = record {the raw new-style QueueJob structure}
    rs0 : array [0..9] of byte;
    cst, ctn,
    coi, toi : nwLong;
    tet, jet : array [0..5] of byte;
    jnm : nwLong;
    jtp : word;
    jps, jcf : word;
    jfn : string[13];
    jfh : array [0..3] of byte;
    sst, stn,
    soi : nwLong;
    tjd : array [0..49] of char;
    rs1, tbs, dm1, ncp, dm2, pfg : byte;
    mxl, mxc : word;
    fnm : array [0..15] of char; {Note: Bullets (Oct94) says [0..12]}
    rs2 : array [0..5] of byte;  {Note: Bullets (Oct94) says [0..8]}
    bun, bjn : array [0..12] of char;
    rs3 : array [1..94] of char;
  end;

  TOldJobStruc = record {the raw old-style QueueJob structure}
    cst, ctn : byte;
    coi, toi : nwLong;
    tet, jet : array [0..5] of byte;
    jnm, jtp : word;
    jps, jcf : byte;
    jfn : array [0..13] of char;
    jfh : array [0..5] of byte;
    sst, stn : byte;
    soi : nwLong;
    tjd : array [0..49] of char;
    rs1, tbs, dm1, ncp, dm2, pfg : byte;
    mxl, mxc : word;
    fnm : array [0..15] of char; {Note: Bullets (Oct94) says [0..12]}
    rs2 : array [0..5] of byte;  {Note: Bullets (Oct94) says [0..8]}
    bun, bjn : array [0..12] of char;
    rs3 : array [1..94] of char;
  end;

type
  TCFBuffer = record {the raw capture flags structure}
    st, pf, ts, pr, nc, ft, r1 : byte;
    bt : array [0..13] of char;
    ld : byte;
    tc : word;
    fc : byte;
    ml, mc : word;
    fn : array [0..12] of char;
    cf, ff, tf : byte;
    a1, a2 : nwLong;
    sr, cp, pq, pj : byte;
    oi : nwLong;
    jn : word;
  end;

const
  VerifyValue = $CAFE;

{---Banner name routines---}

function vlmGetBannerName(var Name : TnwBannerName) : TnwErrorCode;
  {-Return banner name. VLM ONLY.}
  var
    Status: word;
    Regs  : TnwRegisters;
  begin
    {API: _PRINT Get Banner Name}
    nwInitRegs(Regs);
    with Regs do
      begin
        BX := 1;
        {ES := OS(nwGlobalBufRealPtr).S;}        {!!.51}
        {DI := OS(nwGlobalBufRealPtr).O;}        {!!.51}
        ES := OS(nwGlobalBufVLM).S;              {!!.51}
        DI := OS(nwGlobalBufVLM).O;              {!!.51}
      end;
    Status := vlmCall(vlmPrint, $0C, Regs);
    if (Status <> 0) then
      FillChar(Name, sizeof(TnwBannerName), 0)
    else
      begin
        nwCvtAsciizToStr(nwGlobalBuf^, 12);
        Name := PString(nwGlobalBuf)^;
      end;
    vlmGetBannerName := Status;
  end;

function NETXGetBannerName(var Name : TnwBannerName) : TnwErrorCode;
  {-Return banner name. NETX ONLY.}
  var
    Status : word;
    Regs   : TnwRegisters;
  begin
    {API: Get Banner User Name}
    FillChar(Name, sizeof(TnwBannerName), 0);
    with Regs do
      begin
        nwInitRegs(Regs);
        AX := $B808;
        ES := OS(nwGlobalBufRealPtr).S;
        BX := OS(nwGlobalBufRealPtr).O;
        Status := nwIntr($21, Regs);
        if (Status = 0) then
          if (AL <> 0) then
            Status := AL + NWerrBaseNETX;
      end;
    if (Status = 0) then
      begin
        nwCvtAsciizToStr(nwGlobalBuf^, 12);
        Name := PString(nwGlobalBuf)^;
      end;
    NETXGetBannerName := Status;
  end;

function nwGetBannerName(var Name : TnwBannerName) : TnwErrorCode;
  begin
    case nwShellType of
      nsNETX : nwGetBannerName := NETXGetBannerName(Name);
      nsVLM  : nwGetBannerName := vlmGetBannerName(Name);
    else
      nwGetBannerName := nwErrShell;
    end;{case}
  end;

function vlmSetBannerName(Name : TnwBannerName) : TnwErrorCode;
  {-Set the banner name. VLM ONLY.}
  var
    CallResult : word;
    Regs : TnwRegisters;
  begin
    {API: _PRINT Set Banner Name}
    nwInitRegs(Regs);
    with Regs do
      begin
        FillChar(nwGlobalBuf^[0], sizeof(TnwBannerName), 0);
        Move(Name[1], nwGlobalBuf^[0], length(Name));
        {DS := OS(nwGlobalBufRealPtr).S;}                {!!.51}
        {SI := OS(nwGlobalBufRealPtr).O;}                {!!.51}
        DS := OS(nwGlobalBufVLM).S;                      {!!.51}
        SI := OS(nwGlobalBufVLM).O;                      {!!.51}
      end;
    vlmSetBannerName := vlmCall(vlmPrint, $0C, Regs);
  end;

function NETXSetBannerName(Name : TnwBannerName) : TnwErrorCode;
  {-Set the banner name. NETX ONLY.}
  var
    Status : word;
    Regs : TnwRegisters;
  begin
    {API: Set Banner User Name}
    nwInitRegs(Regs);
    with Regs do
      begin
        AX := $B809;
        FillChar(nwGlobalBuf^[0], sizeof(TnwBannerName), 0);
        Move(Name[1], nwGlobalBuf^[0], length(Name));
        ES := OS(nwGlobalBufRealPtr).S;
        BX := OS(nwGlobalBufRealPtr).O;
        Status := nwIntr($21, Regs);
        if (Status = 0) then
          if (AL <> 0) then
            Status := AL + NWerrBaseNETX;
      end;
    NETXSetBannerName := Status;
  end;

function nwSetBannerName(Name : TnwBannerName) : TnwErrorCode;
  begin
    case nwShellType of
      nsNETX : nwSetBannerName := NETXSetBannerName(Name);
      nsVLM  : nwSetBannerName := vlmSetBannerName(Name);
    else
      nwSetBannerName := nwErrShell;
    end;{case}
  end;


{---Capture routines---}
function nwGetNumPrinters : byte;
  var
    Regs : TnwRegisters;
  begin
    {Note: under NETX this always returns 3, under VLM the user is
           allowed to set it anywhere from 0 to 9 in NET.CFG.}
    if (nwShellType = nsVLM) then
      begin
        {API: _PRINT Get Num Of Printers}
        nwInitRegs(Regs);
        Regs.BX := 1;
        if (vlmCall(vlmPrint, $07, Regs) = 0) then
          nwGetNumPrinters := Regs.BL
        else {the PRINT.VLM probably hasn't been loaded}
          nwGetNumPrinters := 0
      end
    else
      nwGetNumPrinters := 3;
  end;

procedure CvtCapFlagsToPascal(var D; var CF : TnwCaptureFlags;
                              NewBufType : boolean);
  {-Convert the buffer from the server to a Pascal capture flags record.}
  var
    CFBuf : TCFBuffer absolute D;
  begin
    FillChar(CF, sizeof(TnwCaptureFlags), 0);
    with CF, CFBuf do
      begin
        PrintFlags := pf;
        TabSize := ts;
        NumCopies := nc;
        FormType := ft;
        {Note: bt is 14 bytes, BannerJobName is 13}
        Move(bt, BannerJobName, sizeof(BannerJobName));
        nwCvtAsciizToStr(BannerJobName, pred(sizeof(BannerJobName)));
        Printer := TnwPrinter(ld);
        FlushTimeout := tc;
        FlushOnClose := (fc = 0);
        MaxLines := Swap(ml);
        MaxCols := Swap(mc);
        Move(fn, FormName, sizeof(fn));
        nwCvtAsciizToStr(FormName, pred(sizeof(FormName)));
        IsCaptured := cf = $FF;
        IsCapturingData := cp = $FF;
        IsDoingTimeOut := tf = $FF;
        IsCapturedToFile := ff = $FF;
        if IsCaptured then
          begin
            if not NewBufType then
              Server := sr;
            if not IsCapturedToFile then
              QueueID := oi;
          end;
      end;
  end;

function vlmGetCapFlags(Printer: TnwPrinter;
                        var CapFlags : TnwCaptureFlags) : TnwErrorCode;
  {-Get the capture flags for Printer. VLM ONLY.}
  var
    Status : word;
    Regs   : TnwRegisters;
  begin
    {API: _PRINT Get Data, standard and extended control flags}
    {Note: the job number field is returned properly by the PRINT VLM,
           but since NETX doesn't, we ignore it.}
    nwInitRegs(Regs);
    with Regs do
      begin
        BX := 1;
        CX := 63;
        DX := ord(Printer);
        {ES := OS(nwGlobalBufRealPtr).S;}                {!!.51}
        {DI := OS(nwGlobalBufRealPtr).O;}                {!!.51}
        ES := OS(nwGlobalBufVLM).S;                      {!!.51}
        DI := OS(nwGlobalBufVLM).O;                      {!!.51}
      end;
    Status := vlmCall(vlmPrint, $04, Regs);
    if (Status = 0) then
      begin
        CvtCapFlagsToPascal(nwGlobalBuf^[0], CapFlags, true);
        if CapFlags.IsCaptured then
          begin
            {Note: the extended print control structure has changed from
                   that described in the Client API docs, but the server
                   number is still at offset 4, and that's all we want.}
            nwInitRegs(Regs);
            with Regs do
              begin
                BX := 3;
                CX := 96;
                DX := ord(Printer);
                {ES := OS(nwGlobalBufRealPtr).S;}             {!!.51}
                {DI := OS(nwGlobalBufRealPtr).O;}             {!!.51}
                ES := OS(nwGlobalBufVLM).S;                   {!!.51}
                DI := OS(nwGlobalBufVLM).O;                   {!!.51}
              end;
            Status := vlmCall(vlmPrint, $04, Regs);
            if (Status = 0) then
              Move(nwGlobalBuf^[4], CapFlags.Server, 2);
          end;
      end;
    if (Status <> 0) then
      FillChar(CapFlags, sizeof(CapFlags), 0);
    vlmGetCapFlags := Status;
  end;

function NETXGetCapFlags(Printer: TnwPrinter;
                         var CapFlags : TnwCaptureFlags) : TnwErrorCode;
  {-Get the capture flags for Printer. NETX ONLY.}
  var
    Status : word;
    Regs   : TnwRegisters;
  begin
    {API: Get Capture Flags}
    {Note: as far as 4 byte job numbers are concerned, this API call is
           broken. Under NETX 3.22 and 3.32 the JobNumber field is the
           low word of the 4 byte job number, under NETX 3.26 it's the
           high word. The GetJobNumber API call (AX=$B80B, DH=printer,
           int $21) is also broken: it's not supported by NETX 3.22, for
           NETX 3.26 it gives the high word in AX, the low word in DX,
           for NETX 3.32 it gives the low word in both AX and DX. Thus
           the job number is not returned by this routine.}
    nwInitRegs(Regs);
    with Regs do
      begin
        AX := $B802;
        CX := 63;
        ES := OS(nwGlobalBufRealPtr).S;
        BX := OS(nwGlobalBufRealPtr).O;
        DH := ord(Printer);
        Status := nwIntr($21, Regs);
        if (Status = 0) then
          if (AL <> 0) then
            Status := AL + NWerrBaseNETX;
      end;
    if (Status = 0) then
      CvtCapFlagsToPascal(nwGlobalBuf^[0], CapFlags, false)
    else
      FillChar(CapFlags, sizeof(CapFlags), 0);
    NETXGetCapFlags := Status;
  end;

function nwGetCaptureFlags(Printer: TnwPrinter;
                           var CapFlags : TnwCaptureFlags) : TnwErrorCode;
  begin
    if (ord(Printer) >= nwGetNumPrinters) then
      begin
        nwGetCaptureFlags := nwpErrBadPrinter;
        Exit;
      end;
    case nwShellType of
      nsNETX : nwGetCaptureFlags := NETXGetCapFlags(Printer, CapFlags);
      nsVLM  : nwGetCaptureFlags := vlmGetCapFlags(Printer, CapFlags);
    else
      nwGetCaptureFlags := nwErrShell;
    end;{case}
  end;

procedure CvtPascalToCapFlags(var D; var CF : TnwCaptureFlags);
  {-Convert the Pascal capture flags record to a buffer for the server}
  var
    CFBuf : TCFBuffer absolute D;
  begin
    FillChar(CFBuf, 42, 0);  {there are 42 bytes of read/write fields}
    with CF, CFBuf do
      begin
        pf := PrintFlags;
        ts := TabSize;
        nc := NumCopies;
        ft := FormType;
        {Note: BannerJobName is 13 bytes, bt is 14}
        Move(BannerJobName, bt, sizeof(BannerJobName));
        nwCvtStrToAsciiz(bt, pred(sizeof(bt)));
        ld := ord(Printer);
        tc := FlushTimeout;
        if FlushOnClose then
             fc := 0
        else fc := $FF;
        ml := Swap(MaxLines);
        mc := Swap(MaxCols);
        Move(FormName, fn, sizeof(fn));
        nwCvtStrToAsciiz(fn, pred(sizeof(fn)));
      end;
  end;

function vlmSetCapFlags(Printer: TnwPrinter;
                        var CapFlags : TnwCaptureFlags) : TnwErrorCode;
  {-Set the capture flags for Printer. VLM ONLY.}
  var
    Regs   : TnwRegisters;
  begin
    {API: _PRINT Set Data}
    CvtPascalToCapFlags(nwGlobalBuf^, CapFlags);
    nwInitRegs(Regs);
    with Regs do
      begin
        CX := 42;
        DX := ord(Printer);
        {DS := OS(nwGlobalBufRealPtr).S;}           {!!.51}
        {SI := OS(nwGlobalBufRealPtr).O;}           {!!.51}
        DS := OS(nwGlobalBufVLM).S;                 {!!.51}
        SI := OS(nwGlobalBufVLM).O;                 {!!.51}
      end;
    vlmSetCapFlags := vlmCall(vlmPrint, $04, Regs);
  end;

function NETXSetCapFlags(Printer: TnwPrinter; Default : boolean;
                         var CapFlags : TnwCaptureFlags) : TnwErrorCode;
  {-Set the capture flags for Printer. NETX ONLY.}
  var
    Status : word;
    Regs   : TnwRegisters;
  begin
    {API: Set (Default) Capture Flags}
    CvtPascalToCapFlags(nwGlobalBuf^, CapFlags);
    nwInitRegs(Regs);
    with Regs do
      begin
        if Default then
          AX := $B801
        else
          begin
            AX := $B803;
            DH := ord(Printer);
          end;
        CX := 42;
        ES := OS(nwGlobalBufRealPtr).S;
        BX := OS(nwGlobalBufRealPtr).O;
        Status := nwIntr($21, Regs);
        if (Status = 0) then
          if (AL <> 0) then
            Status := AL + NWerrBaseNETX;
      end;
    NETXSetCapFlags := Status;
  end;

function nwSetCaptureFlags(Printer: TnwPrinter;
                           var CapFlags : TnwCaptureFlags) : TnwErrorCode;
  begin
    if (ord(Printer) >= nwGetNumPrinters) then
      begin
        nwSetCaptureFlags := nwpErrBadPrinter;
        Exit;
      end;
    if (CapFlags.NumCopies = 0) then {NetWare takes 0 as being 256!}
      CapFlags.NumCopies := 1;
    case nwShellType of
      nsNETX : nwSetCaptureFlags := NETXSetCapFlags(Printer, false, CapFlags);
      nsVLM  : nwSetCaptureFlags := vlmSetCapFlags(Printer, CapFlags);
    else
      nwSetCaptureFlags := nwErrShell;
    end;{case}
  end;

function vlmIsCaptured(Printer : TnwPrinter) : boolean;
  {-Return true if Printer is captured. VLM ONLY.}
  var
    Status : word;
    Regs : TnwRegisters;
  begin
    {API: _PRINT Redirection}
    vlmIsCaptured := false;
    nwInitRegs(Regs);
    with Regs do
      begin
        BX := 1;
        DX := ord(Printer);
        {ES := OS(nwGlobalBufRealPtr).S;}         {!!.51}
        {DI := OS(nwGlobalBufRealPtr).O;}         {!!.51}
        ES := OS(nwGlobalBufVLM).S;               {!!.51}
        DI := OS(nwGlobalBufVLM).O;               {!!.51}
      end;
    Status := vlmCall(vlmPrint, $08, Regs);
    if (Status = 0) then
      vlmIsCaptured := true;
  end;

function NETXIsCaptured(Printer : TnwPrinter) : boolean;
  {-Return true if Printer is captured. NETX ONLY.}
  var
    Status : word;
    CF     : TnwCaptureFlags;
  begin
    {Note: The Get LPT Capture Status API (INT $21, AX=$F003)
           returns true for all printers under NETX 3.32 whether
           they are captured or not. This may also be true for
           earlier versions of NETX. However, the capture flags
           for the required printer has the correct setting.}
    NETXIsCaptured := false;
    Status := NETXGetCapFlags(Printer, CF);
    if (Status = 0) then
      NETXIsCaptured := CF.IsCaptured;
  end;

function nwIsCaptured(Printer : TnwPrinter) : boolean;
  begin
    if (ord(Printer) >= nwGetNumPrinters) then
      begin
        nwIsCaptured := false;
        Exit;
      end;
    case nwShellType of
      nsNETX : nwIsCaptured := NETXIsCaptured(Printer);
      nsVLM  : nwIsCaptured := vlmIsCaptured(Printer);
    else
      nwIsCaptured := false;
    end;{case}
  end;

function vlmCancelCapture(Printer: TnwPrinter) : TnwErrorCode;
  {-Cancel capture of Printer. VLM ONLY.}
  var
    Regs : TnwRegisters;
  begin
    {API: _PRINT Redirection}
    nwInitRegs(Regs);
    Regs.BX := 2;
    Regs.DX := ord(Printer);
    vlmCancelCapture := vlmCall(vlmPrint, $08, Regs);
  end;

function NETXCancelCapture(Printer: TnwPrinter) : TnwErrorCode;
  {-Cancel capture of Printer. NETX ONLY.}
  var
    Status : word;
    Regs   : TnwRegisters;
  begin
    {API: Cancel LPT Capture}
    nwInitRegs(Regs);
    with Regs do
      begin
        AX := $DF00;
        DL := $06;
        DH := ord(Printer);
        Status := nwIntr($21, Regs);
        if (Status = 0) then
          if (AL <> 0) then
            Status := AL + NWerrBaseNETX;
      end;
    NETXCancelCapture := Status;
  end;

function nwCancelCapture(Printer: TnwPrinter) : TnwErrorCode;
  begin
    if (ord(Printer) >= nwGetNumPrinters) then
      begin
        nwCancelCapture := nwpErrBadPrinter;
        Exit;
      end;
    case nwShellType of
      nsNETX : nwCancelCapture := NETXCancelCapture(Printer);
      nsVLM  : nwCancelCapture := vlmCancelCapture(Printer);
    else
      nwCancelCapture := nwErrShell;
    end;{case}
  end;

function vlmFlushCapture(Printer: TnwPrinter) : TnwErrorCode;
  {-Flush capture of Printer. VLM ONLY.
    Note: if the printer is being captured to file, this call
          closes the file, and cancels the capture.}
  var
    Status : word;
    Regs   : TnwRegisters;
  begin
    {API: _PRINT Flush & Close Job}
    nwInitRegs(Regs);
    Regs.DX := ord(Printer);
    vlmFlushCapture := vlmCall(vlmPrint, $09, Regs);
  end;

function vlmEndCapture(Printer: TnwPrinter) : TnwErrorCode;
  {-End capture of Printer. VLM ONLY.}
  var
    Status : word;
  begin
    {Note: There is no 'end capture' API call for VLM, so we do a
           flush followed by a cancel.
           vlmFlushCapture will leave the printer captured *unless*
           the printer was captured to file. So we must check that
           the printer is still captured before calling
           vlmCancelCapture.}
    Status := vlmFlushCapture(Printer);
    if (Status = 0) and vlmIsCaptured(Printer) then
      Status := vlmCancelCapture(Printer);
    vlmEndCapture := Status;
  end;

function NETXEndCapture(Printer: TnwPrinter) : TnwErrorCode;
  {-End capture of Printer. NETX ONLY.}
  var
    Status : word;
    Regs   : TnwRegisters;
  begin
    {API: End LPT Capture}
    nwInitRegs(Regs);
    with Regs do
      begin
        AX := $DF00;
        DL := $05;
        DH := ord(Printer);
        Status := nwIntr($21, Regs);
        if (Status = 0) then
          if (AL <> 0) then
            Status := AL + NWerrBaseNETX;
      end;
    NETXEndCapture := Status;
  end;

function NETXFlushCapture(Printer: TnwPrinter) : TnwErrorCode;
  {-Flush capture of Printer. NETX ONLY.}
  var
    Status : word;
    Regs   : TnwRegisters;
    CF     : TnwCaptureFlags;
  begin
    {API: Flush LPT Capture}
    {Note: Because a VLM flush call will end a capture if the
           printer was captured to file and and NETX doesn't (it
           leaves the printer captured to some 'default' queue),
           we shall end the capture to file for NETX in order to
           be consistent.}
    Status := NETXGetCapFlags(Printer, CF);
    if (Status = 0) then
      begin
        nwInitRegs(Regs);
        with Regs do
          begin
            AX := $DF00;
            DL := $07;
            DH := ord(Printer);
            Status := nwIntr($21, Regs);
            if (Status = 0) then
              if (AL <> 0) then
                Status := AL + NWerrBaseNETX;
          end;
        if (Status = 0) and CF.IsCapturedToFile then
          Status := NETXEndCapture(Printer);
      end;
    NETXFlushCapture := Status;
  end;

function nwFlushCapture(Printer: TnwPrinter) : TnwErrorCode;
  begin
    if (ord(Printer) >= nwGetNumPrinters) then
      begin
        nwFlushCapture := nwpErrBadPrinter;
        Exit;
      end;
    case nwShellType of
      nsNETX : nwFlushCapture := NETXFlushCapture(Printer);
      nsVLM  : nwFlushCapture := vlmFlushCapture(Printer);
    else
      nwFlushCapture := nwErrShell;
    end;{case}
  end;

function nwEndCapture(Printer: TnwPrinter) : TnwErrorCode;
  begin
    if (ord(Printer) >= nwGetNumPrinters) then
      begin
        nwEndCapture := nwpErrBadPrinter;
        Exit;
      end;
    case nwShellType of
      nsNETX : nwEndCapture := NETXEndCapture(Printer);
      nsVLM  : nwEndCapture := vlmEndCapture(Printer);
    else
      nwEndCapture := nwErrShell;
    end;{case}
  end;

function NETXStartCapture(Printer : TnwPrinter) : TnwErrorCode;
  {-Start the capture for Printer. NETX ONLY.}
  var
    Status : word;
    Regs   : TnwRegisters;
  begin
    {API: Start LPT Capture}
    nwInitRegs(Regs);
    with Regs do
      begin
        AX := $DF00;
        DL := $04;
        DH := ord(Printer);
        Status := nwIntr($21, Regs);
        if (Status = 0) then
          if (AL <> 0) then
            Status := AL + NWerrBaseNETX;
      end;
    NETXStartCapture := Status;
  end;

function vlmStartCaptureToQueue(Printer: TnwPrinter;
                                Server : TnwServer;
                                QID    : nwLong;
                                QueueName : TnwObjectStr) : TnwErrorCode;
  {-Start the capture for Printer to a queue. VLM ONLY.}
  var
    Regs   : TnwRegisters;
  begin
    {API: _PRINT Redirection}
    nwUpperStr(QueueName);
    nwInitRegs(Regs);
    with Regs do
      begin
        AX := length(QueueName);
        CX := Server;
        DX := ord(Printer);
        Move(QID, nwGlobalBuf^[0], sizeof(QID));
        Move(QueueName[1], nwGlobalBuf^[sizeof(QID)], length(QueueName));
        {ES := OS(nwGlobalBufRealPtr).S;}             {!!.51}
        {DI := OS(nwGlobalBufRealPtr).O;}             {!!.51}
        ES := OS(nwGlobalBufVLM).S;                   {!!.51}
        DI := OS(nwGlobalBufVLM).O;                   {!!.51}
        DS := ES;
        SI := DI + sizeof(QID);
      end;
    vlmStartCaptureToQueue := vlmCall(vlmPrint, $08, Regs);
  end;

function NETXStartCaptureToQueue(Printer: TnwPrinter;
                                 Server : TnwServer;
                                 QID    : nwLong) : TnwErrorCode;
  {-Start the capture for Printer to a queue. NETX ONLY.}
  var
    Status : word;
    Regs   : TnwRegisters;
  begin
    {API: Set Capture Print Queue}
    nwNETXPushServer(Server);
    nwInitRegs(Regs);
    with Regs do
      begin
        AX := $B806;
        DH := ord(Printer);
        DL := Server;
        BX := LH(QID).L;
        CX := LH(QID).H;
        Status := nwIntr($21, Regs);
        if (Status = 0) then
          if (AL <> 0) then
            Status := AL + NWerrBaseNETX;
      end;
    if (Status = 0) then
      Status := NETXStartCapture(Printer);
    nwNETXPopServer;
    NETXStartCaptureToQueue := Status;
  end;

function nwStartCaptureToQueue(Printer: TnwPrinter;
                               Server : TnwServer;
                               QueueName : TnwObjectStr) : TnwErrorCode;
  var
    Status : word;
    QID    : nwLong;
  begin
    if (ord(Printer) >= nwGetNumPrinters) then
      begin
        nwStartCaptureToQueue := nwpErrBadPrinter;
        Exit;
      end;
    Status := nwbGetObjectID(Server, NWboPrintQueue, QueueName, QID);
    if (Status = 0) then
      case nwShellType of
        nsNETX : Status := NETXStartCaptureToQueue(Printer, Server, QID);
        nsVLM  : Status := vlmStartCaptureToQueue(Printer, Server, QID, QueueName);
      else
        nwStartCaptureToQueue := nwErrShell;
      end;{case}
    nwStartCaptureToQueue := Status;
  end;

function vlmStartCaptureToFile(Printer: TnwPrinter;
                               var FileName : string) : TnwErrorCode;
  {-Start the capture for Printer to a file. VLM ONLY.}
  var
    Status : word;
    Regs   : TnwRegisters;
    Server : TnwServer;
    SName  : TnwServerName;
    VName  : TnwVolumeName;
    Path   : string;
  begin
    {API: _PRINT Redirection}
    Status := nwParseFileName(FileName, Server, SName, VName, Path);
    if (Status = 0) and (Server = 0) then
      Status := nwfErrNotOnServer;
    if (Status = 0) then
      begin
        nwInitRegs(Regs);
        with Regs do
          begin
            BX := 3;
            CX := Server;
            DX := ord(Printer);
            Move(VName[1], nwGlobalBuf^[0], length(VName));
            Move(Path[1], nwGlobalBuf^[length(VName)], length(Path));
            nwGlobalBuf^[length(VName)+length(Path)] := 0;
            {ES := OS(nwGlobalBufRealPtr).S;}               {!!.51}
            {DI := OS(nwGlobalBufRealPtr).O;}               {!!.51}
            ES := OS(nwGlobalBufVLM).S;                     {!!.51}
            DI := OS(nwGlobalBufVLM).O;                     {!!.51}
          end;
        Status := vlmCall(vlmPrint, $08, Regs);
      end;
    vlmStartCaptureToFile := Status;
  end;

function NETXStartCaptureToFile(Printer: TnwPrinter;
                                var FileName : string) : TnwErrorCode;
  {-Start the capture for Printer to a file. NETX ONLY.}
  var
    ReqLen : word;
    Status : word;
    Regs   : TnwRegisters;
    Server : TnwServer;
    SName  : TnwServerName;
    VName  : TnwVolumeName;
    Path   : string;
    CF     : TnwCaptureFlags;
  begin
    {API: Specify Capture File}
    {Notes: this is somewhat fiddly as the NETX Specify Capture File
            call only works on the default printer, not on specific
            printers. Hence we set the default printer to the required
            printer and start the capture.}
    Status := nwParseFileName(FileName, Server, SName, VName, Path);
    if (Status = 0) and (Server = 0) then
      Status := nwfErrNotOnServer;
    if (Status = 0) then
      begin
        Status := NETXGetCapFlags(Printer, CF);
        if (Status = 0) then
          begin
            CF.Printer := Printer;
            Status := NETXSetCapFlags(Printer, true, CF);
          end;
        if (Status = 0) then
          begin
            nwNETXPushServer(Server);
            FillChar(nwGlobalBuf^, 262, 0);
            ReqLen := 3 + length(VName) + length(Path);
            nwGlobalBuf^[2] := Lo(ReqLen);
            nwGlobalBuf^[3] := Hi(ReqLen);
            nwGlobalBuf^[4] := $09;
            nwGlobalBuf^[6] := length(VName) + length(Path);
            Move(VName[1], nwGlobalBuf^[7], length(VName));
            Move(Path[1], nwGlobalBuf^[length(VName)+7], length(Path));
            nwInitRegs(Regs);
            with Regs do
              begin
                AX := $E009;
                ES := OS(nwGlobalBufRealPtr).S;
                DI := OS(nwGlobalBufRealPtr).O;
                DS := ES;
                SI := DI + 2;
                Status := nwIntr($21, Regs);
                if (Status = 0) then
                  if (AL <> 0) then
                    Status := AL + NWerrBaseNETX;
              end;
            if (Status = 0) then
              Status := NETXStartCapture(Printer);
            nwNETXPopServer;
          end;
      end;
    NETXStartCaptureToFile := Status;
  end;

function nwStartCaptureToFile(Printer: TnwPrinter;
                              FileName : string) : TnwErrorCode;
  begin
    if (ord(Printer) >= nwGetNumPrinters) then
      begin
        nwStartCaptureToFile := nwpErrBadPrinter;
        Exit;
      end;
    case nwShellType of
      nsNETX : nwStartCaptureToFile :=
                  NETXStartCaptureToFile(Printer, FileName);
      nsVLM  : nwStartCaptureToFile :=
                  vlmStartCaptureToFile(Printer, FileName);
    else
      nwStartCaptureToFile := nwErrShell;
    end;{case}
  end;


{---QUEUE ROUTINES---}

procedure nwEnumQueues(Server : TnwServer;
                       EnumFunc : TnwEnumQueueFunc; var ExtraData);
  var
    Dyn   : boolean;
    Props : boolean;
    Sec   : byte;
    StillEnumerating : boolean;
    ObjType  : word;
    Status   : word;
    LastID   : nwLong;
    LastName : TnwObjectStr;
  begin
    StillEnumerating := true;
    LastID := -1;
    ObjType := nwboPrintQueue;
    while StillEnumerating do
      begin
        LastName := '*';
        Status := nwbScanObject(Server,
                                ObjType,
                                LastName, LastID,
                                Dyn, Sec, Props);
        if (Status = 0) then
             StillEnumerating := EnumFunc(LastName, LastID, ExtraData)
        else StillEnumerating := false;
      end;
  end;

function CvtByteYear(yb : byte) : word; near;
  begin
    if (yb = $FF) then
      CvtByteYear := $FFFF
    else if (yb < 80) then
      CvtByteYear := 2000 + yb
    else
      CvtByteYear := 1900 + yb;
  end;

function CvtWordYear(yw : word) : byte; near;
  begin
    if (yw = $FFFF) then
      CvtWordYear := $FF
    else if (2000 <= yw) and (yw <= 2079) then
      CvtWordYear := yw - 2000
    else if (1980 <= yw) and (yw <= 1999) then
      CvtWordYear := yw - 1900
    else
      CvtWordYear := 80;
  end;

procedure CvtOldPJDToPascal(var D : TOldJobStruc; var PJD : TnwPrintJob); near;
  begin
    with PJD, D do
      begin
        ClientStation := cst;
        ClientTaskNum := ctn;
        ClientID := coi;
        TargetServerID := toi;
        TargetExecTime.Year := CvtByteYear(tet[0]);
        Move(tet[1], TargetExecTime.Month, 5);
        JobEntryTime.Year := CvtByteYear(jet[0]);
        Move(jet[1], JobEntryTime.Month, 5);
        JobNumber := jnm;
        JobType := jtp;
        JobPosition := jps;
        JobControlFlags := jcf;
        Move(jfn, JobFileName, sizeof(JobFileName));
        nwCvtAsciizToStr(JobFileName, pred(sizeof(JobFileName)));
        Move(jfh, JobFileHandle, sizeof(JobFileHandle));
        ServerStation := sst;
        ServerTaskNum := stn;
        ServerID := soi;
        Move(tjd, JobDesc, sizeof(JobDesc));
        nwCvtAsciizToStr(JobDesc, pred(sizeof(JobDesc)));
        TabSize := tbs;
        NumCopies := ncp;
        PrintFlags := pfg;
        MaxLines := Swap(mxl);
        MaxCols := Swap(mxc);
        Move(fnm, FormName, sizeof(FormName));
        nwCvtAsciizToStr(FormName, pred(sizeof(FormName)));
        Move(bun, BannerUserName, sizeof(BannerUserName));
        nwCvtAsciizToStr(BannerUserName, pred(sizeof(BannerUserName)));
        Move(bjn, BannerJobName, sizeof(BannerJobName));
        nwCvtAsciizToStr(BannerJobName, pred(sizeof(BannerJobName)));
      end;
  end;

procedure CvtNewPJDToPascal(var D : TNewJobStruc; var PJD : TnwPrintJob);
  begin
    with PJD, D do
      begin
        ClientStation := cst;
        ClientTaskNum := ctn;
        ClientID := coi;
        TargetServerID := toi;
        TargetExecTime.Year := CvtByteYear(tet[0]);
        Move(tet[1], TargetExecTime.Month, 5);
        JobEntryTime.Year := CvtByteYear(jet[0]);
        Move(jet[1], JobEntryTime.Month, 5);
        JobNumber := jnm;
        JobType := jtp;
        JobPosition := jps;
        JobControlFlags := jcf;
        JobFileName := jfn;
        {the 4-byte jfh goes into the last 4 bytes of JobFileHandle}
        Move(jfh, JobFileHandle[1], sizeof(jfh));
        ServerStation := sst;
        ServerTaskNum := stn;
        ServerID := soi;
        Move(tjd, JobDesc, sizeof(JobDesc));
        nwCvtAsciizToStr(JobDesc, pred(sizeof(JobDesc)));
        TabSize := tbs;
        NumCopies := ncp;
        PrintFlags := pfg;
        MaxLines := Swap(mxl);
        MaxCols := Swap(mxc);
        Move(fnm, FormName, sizeof(FormName));
        nwCvtAsciizToStr(FormName, pred(sizeof(FormName)));
        Move(bun, BannerUserName, sizeof(BannerUserName));
        nwCvtAsciizToStr(BannerUserName, pred(sizeof(BannerUserName)));
        Move(bjn, BannerJobName, sizeof(BannerJobName));
        nwCvtAsciizToStr(BannerJobName, pred(sizeof(BannerJobName)));
      end;
  end;

procedure CvtPascalToOldPJD(var D : TOldJobStruc; var PJD : TnwPrintJob);
  begin
    {Note: only convert the fields that can be changed by the user,
           plus the job number so that QMS can recognize it}
    FillChar(D, sizeof(D), 0);
    with PJD, D do
      begin
        toi := TargetServerID;
        tet[0] := CvtWordYear(TargetExecTime.Year);
        Move(TargetExecTime.Month, tet[1], 5);
        jnm := JobNumber;
        jtp := JobType;
        jcf := JobControlFlags;
        Move(JobDesc[1], tjd[0], length(JobDesc));
        tbs := TabSize;
        ncp := NumCopies;
        pfg := PrintFlags;
        mxl := Swap(MaxLines);
        mxc := Swap(MaxCols);
        Move(FormName[1], fnm[0], length(FormName));
        Move(BannerUserName[1], bun[0], length(BannerUserName));
        Move(BannerJobName[1], bjn[0], length(BannerJobName));
      end;
  end;

procedure CvtPascalToNewPJD(var D : TNewJobStruc; var PJD : TnwPrintJob);
  begin
    {Note: only convert the fields that can be changed by the user,
           plus the job number so that QMS can recognize it}
    FillChar(D, sizeof(D), 0);
    with PJD, D do
      begin
        toi := TargetServerID;
        tet[0] := CvtWordYear(TargetExecTime.Year);
        Move(TargetExecTime.Month, tet[1], 5);
        jnm := JobNumber;
        jtp := JobType;
        jcf := JobControlFlags;
        Move(JobDesc[1], tjd[0], length(JobDesc));
        tbs := TabSize;
        ncp := NumCopies;
        pfg := PrintFlags;
        mxl := Swap(MaxLines);
        mxc := Swap(MaxCols);
        Move(FormName[1], fnm[0], length(FormName));
        Move(BannerUserName[1], bun[0], length(BannerUserName));
        Move(BannerJobName[1], bjn[0], length(BannerJobName));
      end;
  end;

function ReadPrintJobEntryOld(var PrintJobData : TnwPrintJob) : TnwErrorCode;
  var
    Status : word;
    Request : record
      Len  : word;
      Func : byte;
      QID  : nwLong;
      JobN : word;
    end;
    Reply : TOldJobStruc;
    FReply: record
      QID : nwLong;
      JobN: word;
      FSiz: nwLong;
    end;
  begin
    {API: Read Queue Job Entry (old)
          Get Queue Job File Size (old)}
    with Request do
      begin
        Len := sizeof(Request) - 2;
        Func := $6C;
        QID := PrintJobData.QueueID;
        JobN := PrintJobData.JobNumber;
      end;
    Status := nwServerCall(PrintJobData.Server, $17, sizeof(Request), Request,
                                                     sizeof(Reply), Reply);
    if (Status = 0) then
      begin
        CvtOldPJDToPascal(Reply, PrintJobData);
        with Request do
          begin
            Len := sizeof(Request) - 2;
            Func := $78;
            QID := PrintJobData.QueueID;
            JobN := PrintJobData.JobNumber;
          end;
        Status := nwServerCall(PrintJobData.Server, $17, sizeof(Request), Request,
                                                         sizeof(FReply), FReply);
        if (Status = 0) then
          PrintJobData.JobFileSize := nwSwapLong(FReply.FSiz);
      end;
    ReadPrintJobEntryOld := Status;
  end;

function ReadPrintJobEntryNew(var PrintJobData : TnwPrintJob) : TnwErrorCode;
  var
    Status : word;
    Request : record
      Len  : word;
      Func : byte;
      QID  : nwLong;
      JobN : nwLong;
    end;
    Reply : TNewJobStruc;
    FReply: record
      QID : nwLong;
      JobN: nwLong;
      FSiz: nwLong;
    end;
  begin
    {API: Read Queue Job Entry
          Get Queue Job File Size}
    with Request do
      begin
        Len := sizeof(Request) - 2;
        Func := $7A;
        QID := PrintJobData.QueueID;
        JobN := PrintJobData.JobNumber;
      end;
    Status := nwServerCall(PrintJobData.Server, $17, sizeof(Request), Request,
                                                     sizeof(Reply), Reply);
    if (Status = 0) then
      begin
        CvtNewPJDToPascal(Reply, PrintJobData);
        with Request do
          begin
            Len := sizeof(Request) - 2;
            Func := $87;
            QID := PrintJobData.QueueID;
            JobN := PrintJobData.JobNumber;
          end;
        Status := nwServerCall(PrintJobData.Server, $17, sizeof(Request), Request,
                                                         sizeof(FReply), FReply);
        if (Status = 0) then
          PrintJobData.JobFileSize := nwSwapLong(FReply.FSiz);
      end;
    ReadPrintJobEntryNew := Status;
  end;

function nwqRefreshPrintJob(var PrintJobData : TnwPrintJob) : TnwErrorCode;
  begin
    if (PrintJobData.VerifyFlag <> VerifyValue) then
      nwqRefreshPrintJob := NWerrBadData
    else
      if (PrintJobData.ServerVersion < ServerVersion311) then
        nwqRefreshPrintJob := ReadPrintJobEntryOld(PrintJobData)
      else
        nwqRefreshPrintJob := ReadPrintJobEntryNew(PrintJobData)
  end;

function ChangePrintJobEntryOld(var PrintJobData : TnwPrintJob) : TnwErrorCode;
  var
    Status : word;
    Request : record
      Len  : word;
      Func : byte;
      QID  : nwLong;
      JobD : TOldJobStruc;
    end;
    Dummy : word;
  begin
    {API: Change Queue Job Entry (old)}
    with Request do
      begin
        Len := sizeof(Request) - 2;
        Func := $6D;
        QID := PrintJobData.QueueID;
        CvtPascalToOldPJD(JobD, PrintJobData);
      end;
    Status := nwServerCall(PrintJobData.Server, $17, sizeof(Request), Request,
                                                     0, Dummy);
    if (Status = 0) then
      Status := ReadPrintJobEntryOld(PrintJobData);
    ChangePrintJobEntryOld := Status;
  end;

function ChangePrintJobEntryNew(var PrintJobData : TnwPrintJob) : TnwErrorCode;
  var
    Status : word;
    Request : record
      Len  : word;
      Func : byte;
      QID  : nwLong;
      JobD : TNewJobStruc;
    end;
    Dummy : word;
  begin
    {API: Change Queue Job Entry}
    with Request do
      begin
        Len := sizeof(Request) - 2;
        Func := $7B;
        QID := PrintJobData.QueueID;
        CvtPascalToNewPJD(JobD, PrintJobData);
      end;
    Status := nwServerCall(PrintJobData.Server, $17, sizeof(Request), Request,
                                                     0, Dummy);
    if (Status = 0) then
      Status := ReadPrintJobEntryNew(PrintJobData);
    ChangePrintJobEntryNew := Status;
  end;

function nwqChangePrintJob(var PrintJobData : TnwPrintJob) : TnwErrorCode;
  begin
    if (PrintJobData.VerifyFlag <> VerifyValue) then
      nwqChangePrintJob := NWerrBadData
    else
      begin
        if (PrintJobData.NumCopies = 0) then {NetWare takes 0 as being 256!}
          PrintJobData.NumCopies := 1;
        if (PrintJobData.ServerVersion < ServerVersion311) then
          nwqChangePrintJob := ChangePrintJobEntryOld(PrintJobData)
        else
          nwqChangePrintJob := ChangePrintJobEntryNew(PrintJobData);
      end;
  end;

function ChangePrintJobPosition(var PrintJobData : TnwPrintJob;
                                    NewPosn : word) : TnwErrorCode;
  var
    Status : word;
    Request : record
      Len  : word;
      Func : byte;
      QID  : nwLong;
      JobN : word;
      Posn : byte;
    end;
    Dummy : word;
  begin
    {API: Change Queue Job Position}
    with Request do
      begin
        Len := sizeof(Request) - 2;
        Func := $6E;
        QID := PrintJobData.QueueID;
        JobN := PrintJobData.JobNumber;
        Posn := NewPosn;
      end;
    Status := nwServerCall(PrintJobData.Server, $17, sizeof(Request), Request,
                                                     0, Dummy);
    if (Status = 0) then
      Status := ReadPrintJobEntryOld(PrintJobData);
    ChangePrintJobPosition := Status;
  end;

function ChangeJobPriority(var PrintJobData : TnwPrintJob;
                           NewPosn : word) : TnwErrorCode;
  var
    Status : word;
    Request : record
      Len  : word;
      Func : byte;
      QID  : nwLong;
      JobN : nwLong;
      Posn : nwLong;
    end;
    Dummy : word;
  begin
    {API: Change Job Priority}
    with Request do
      begin
        Len := sizeof(Request) - 2;
        Func := $82;
        QID := PrintJobData.QueueID;
        JobN := PrintJobData.JobNumber;
        Posn := NewPosn;
      end;
    Status := nwServerCall(PrintJobData.Server, $17, sizeof(Request), Request,
                                                     0, Dummy);
    if (Status = 0) then
      Status := ReadPrintJobEntryNew(PrintJobData);
    ChangeJobPriority := Status;
  end;

function nwqChangePrintJobPos(var PrintJobData : TnwPrintJob;
                              NewPosition : word) : TnwErrorCode;
  begin
    if (PrintJobData.VerifyFlag <> VerifyValue) then
      nwqChangePrintJobPos := NWerrBadData
    else
      if (PrintJobData.ServerVersion < ServerVersion311) then
        nwqChangePrintJobPos := ChangePrintJobPosition(PrintJobData, NewPosition)
      else
        nwqChangePrintJobPos := ChangeJobPriority(PrintJobData, NewPosition)
  end;

function RemovePrintJobEntryOld(var PrintJobData : TnwPrintJob) : TnwErrorCode;
  var
    Request : record
      Len  : word;
      Func : byte;
      QID  : nwLong;
      JobN : word;
    end;
    Dummy : word;
  begin
    {API: Remove Job From Queue (old)}
    with Request do
      begin
        Len := sizeof(Request) - 2;
        Func := $6A;
        QID := PrintJobData.QueueID;
        JobN := PrintJobData.JobNumber;
      end;
    RemovePrintJobEntryOld := nwServerCall(PrintJobData.Server, $17,
                                           sizeof(Request), Request,
                                           0, Dummy);
  end;

function RemovePrintJobEntryNew(var PrintJobData : TnwPrintJob) : TnwErrorCode;
  var
    Request : record
      Len  : word;
      Func : byte;
      QID  : nwLong;
      JobN : nwLong;
    end;
    Dummy : word;
  begin
    {API: Remove Job From Queue}
    with Request do
      begin
        Len := sizeof(Request) - 2;
        Func := $80;
        QID := PrintJobData.QueueID;
        JobN := PrintJobData.JobNumber;
      end;
    RemovePrintJobEntryNew := nwServerCall(PrintJobData.Server, $17,
                                           sizeof(Request), Request,
                                           0, Dummy);
  end;

function nwqRemovePrintJob(var PrintJobData : TnwPrintJob) : TnwErrorCode;
  begin
    if (PrintJobData.VerifyFlag <> VerifyValue) then
      nwqRemovePrintJob := NWerrBadData
    else
      if (PrintJobData.ServerVersion < ServerVersion311) then
        nwqRemovePrintJob := RemovePrintJobEntryOld(PrintJobData)
      else
        nwqRemovePrintJob := RemovePrintJobEntryNew(PrintJobData)
  end;

function CreatePrintJobAndFileOld(QueueID : nwLong;
                                  var PrintJobData : TnwPrintJob) : TnwErrorCode;
  var
    Status : word;
    Request : record
      Len  : word;
      Func : byte;
      QID  : nwLong;
      JobD : TOldJobStruc;
    end;
    Reply : record
      Len  : word;
      JobD : array [0..53] of byte;
    end;
    Regs : TnwRegisters;
  begin
    {API: Create Queue Job And File (old)}
    with Request do
      begin
        Len := sizeof(Request) - 2;
        Func := $68;
        QID := QueueID;
        CvtPascalToOldPJD(JobD, PrintJobData);
      end;
    {Note: under NETX we can use the NETQ device *only* if we make a
           call through the proper NETX calling sequence; NETX does
           not filter the NCP calls for this function and so does not
           notice and provide the NETQ device.}
    if (nwShellType = nsNETX) then
      begin
        nwNETXPushServer(PrintJobData.Server);
        Reply.Len := sizeof(Reply.JobD);
        Move(Request, nwGlobalBuf^[0], sizeof(Request));
        Move(Reply, nwGlobalBuf^[sizeof(Request)], sizeof(Reply.Len));
        nwInitRegs(Regs);
        with Regs do
          begin
            AH := $E3;
            DS := OS(nwGlobalBufRealPtr).S;
            SI := OS(nwGlobalBufRealPtr).O;
            ES := DS;
            DI := SI + sizeof(Request);
            Status := nwIntr($21, Regs);
            if (Status = 0) then
              if (AL <> 0) then
                Status := NWerrBaseNETX + AL;
          end;
        nwNETXPopServer;
        if (Status = 0) then
          Move(nwGlobalBuf^[sizeof(Request)], Reply, sizeof(Reply));
      end
    else {it's a VLM}
      Status := nwServerCall(PrintJobData.Server, $17,
                             sizeof(Request), Request,
                             sizeof(Reply.JobD), Reply.JobD);
    if (Status = 0) then
      begin
        Move(Reply.JobD, Request.JobD, sizeof(Reply.JobD));
        CvtOldPJDToPascal(Request.JobD, PrintJobData);
      end;
    CreatePrintJobAndFileOld := Status;
  end;

function CreatePrintJobAndFileNew(QueueID : nwLong;
                                  var PrintJobData : TnwPrintJob) : TnwErrorCode;
  var
    Status : word;
    Request : record
      Len  : word;
      Func : byte;
      QID  : nwLong;
      JobD : TNewJobStruc;
    end;
    Reply : record
      Len : word;
      JobD : array [0..77] of byte;
    end;
    Regs : TnwRegisters;
  begin
    {API: Create Queue Job And File}
    with Request do
      begin
        Len := sizeof(Request) - 2;
        Func := $79;
        QID := QueueID;
        CvtPascalToNewPJD(JobD, PrintJobData);
      end;
    {Note: under NETX we can use the NETQ device *only* if we make a
           call through the proper NETX calling sequence; NETX does
           not filter the NCP calls for this function and so does not
           notice and provide the NETQ device.}
    if (nwShellType = nsNETX) then
      begin
        nwNETXPushServer(PrintJobData.Server);
        Reply.Len := sizeof(Reply.JobD);
        Move(Request, nwGlobalBuf^[0], sizeof(Request));
        Move(Reply, nwGlobalBuf^[sizeof(Request)], sizeof(Reply.Len));
        nwInitRegs(Regs);
        with Regs do
          begin
            AH := $E3;
            DS := OS(nwGlobalBufRealPtr).S;
            SI := OS(nwGlobalBufRealPtr).O;
            ES := DS;
            DI := SI + sizeof(Request);
            Status := nwIntr($21, Regs);
            if (Status = 0) then
              if (AL <> 0) then
                Status := NWerrBaseNETX + AL;
          end;
        nwNETXPopServer;
        if (Status = 0) then
          Move(nwGlobalBuf^[sizeof(Request)], Reply, sizeof(Reply));
      end
    else {it's a VLM}
      Status := nwServerCall(PrintJobData.Server, $17,
                             sizeof(Request), Request,
                             sizeof(Reply.JobD), Reply.JobD);
    if (Status = 0) then
      begin
        Move(Reply.JobD, Request.JobD, sizeof(Reply.JobD));
        CvtNewPJDToPascal(Request.JobD, PrintJobData);
      end;
    CreatePrintJobAndFileNew := Status;
  end;

function vlmMapNetWareHandle(var F : file;
                             var PJD : TnwPrintJob) : TnwErrorCode;
  const
    VLMfilename : array [0..7] of char = 'VLM_NETQ';
  type
    TnwCSFT = record
      Handle : TnwFileHandle;
      Res1   : word;
      NameZ  : array [0..13] of char;
      Attr   : byte;
      Res2   : byte;
      Len    : nwLong;
      CreateDate : word;
      AccessDate : word;
      UpdateDate : word;
      UpdateTime : word;
    end;
  var
    Status : word;
    {$IFDEF Windows}
    FR : TFileRec absolute F;
    {$ELSE}
    FR : FileRec absolute F;
    {$ENDIF}
    CSFT : ^TnwCSFT;
    Regs : TnwRegisters;
  begin
    {API: _REDIR Build SFT}
    FillChar(FR, sizeof(FR), 0);
    FR.RecSize := 1;
    CSFT := pointer(nwGlobalBuf);
    FillChar(CSFT^, sizeof(CSFT^), 0);
    with CSFT^ do
      begin
        Handle := PJD.JobFileHandle;
        Handle[0] := Handle[1] + 1;
        Attr := 2;
      end;
    nwInitRegs(Regs);
    with Regs do
      begin
        CX := PJD.Server;
        {ES := OS(nwGlobalBufRealPtr).S;}              {!!.51}
        {DI := OS(nwGlobalBufRealPtr).O;}              {!!.51}
        ES := OS(nwGlobalBufVLM).S;                    {!!.51}
        DI := OS(nwGlobalBufVLM).O;                    {!!.51}
        Status := vlmCall(vlmRedir, $04, Regs);
        if (Status = 0) then
          begin
            FR.Mode := fmInOut;
            FR.Handle := BX;
            Move(VLMfilename, FR.Name, sizeof(VLMfilename)); {for fun}
          end
        else
          FR.Mode := fmClosed;
      end;
    vlmMapNetWareHandle := Status;
  end;


function nwqCreatePrintJobFile(Server : TnwServer; QueueName : TnwObjectStr;
                               var PrintJobData : TnwPrintJob;
                               var F : file) : TnwErrorCode;
  const
    UnknownStr = '(unknown)';
  var
    Status : word;
    QID    : nwLong;
    SaveFileMode : integer;
  begin
    if (nwShellType = nsNone) then
      begin
        nwqCreatePrintJobFile := nwErrShell;
        Exit;
      end;
    {Notes: This routine is greatly complicated by the different
            combinations that can occur. Essentially there are two
            different types of servers: ones that support only 255
            workstations and the others that support more. Also there
            are essentially three different types of shell: the old NETX
            that support up to 255 connections, the newer NETX that
            support over 255 connections, and the VLMs. If an old NETX
            is spotted accessing a server that supports over 255
            connections, then this routine 'pretends' that the server
            is version 2.20 (ie that it doesn't support over 255 conns).
            Also under NETX, the NETQ device only becomes visible *if* you
            use the proper NETX call, and does not if you use the NCP
            call. The Novell manuals are decidedly ambiguous about this.}
    Status := nwbGetObjectID(Server, NWboPrintQueue, QueueName, QID);
    if (Status = 0) then
      begin
        FillChar(PrintJobData, sizeof(PrintJobData), 0);
        PrintJobData.Server := Server;
        with PrintJobData do
          begin
            VerifyFlag := VerifyValue;
            QueueID := QID;
            ServerVersion := nwServerVersion(Server);
            if (ServerVersion >= ServerVersion311) and
               (nwShellType = nsNETX) and (nwShellVersion < NETXVersion330) then
              ServerVersion := $0214;
            TargetServerID := -1;
            FillChar(TargetExecTime, pred(sizeof(TargetExecTime)), $FF);
            JobDesc := UnknownStr;
            NumCopies := 1;
            if (nwGetBannerName(BannerUserName) <> 0) then
              BannerUserName := UnknownStr;
            BannerJobName := UnknownStr;
          end;
        if (PrintJobData.ServerVersion < ServerVersion311) then
          Status := CreatePrintJobAndFileOld(QID, PrintJobData)
        else
          Status := CreatePrintJobAndFileNew(QID, PrintJobData);
        if (Status = 0) then
          case nwShellType of
            nsNETX :
              begin
                nwNETXPushServer(PrintJobData.Server);
                Assign(F, 'NETQ');
                SaveFileMode := FileMode;
                FileMode := $42; {deny-none, read/write}
                Reset(F, 1);     {MUST use DOS open call, not create}
                FileMode := SaveFileMode;
                Status := IOResult;
                nwNETXPopServer;
              end;
            nsVLM  :
              Status := vlmMapNetWareHandle(F, PrintJobData);
          end;{case}
      end;
    nwqCreatePrintJobFile := Status;
  end;

function CloseFileAndStartPrintJobOld(var PrintJobData : TnwPrintJob) : TnwErrorCode;
  var
    Request : record
      Len  : word;
      Func : byte;
      QID  : nwLong;
      JobN : word;
    end;
    Dummy : word;
  begin
    {API: Close File And Start Queue Job (old)}
    with Request do
      begin
        Len := sizeof(Request) - 2;
        Func := $69;
        QID := PrintJobData.QueueID;
        JobN := PrintJobData.JobNumber;
      end;
    CloseFileAndStartPrintJobOld := nwServerCall(PrintJobData.Server, $17,
                                                 sizeof(Request), Request,
                                                 0, Dummy);
  end;

function CloseFileAndStartPrintJobNew(var PrintJobData : TnwPrintJob) : TnwErrorCode;
  var
    Request : record
      Len  : word;
      Func : byte;
      QID  : nwLong;
      JobN : nwLong;
    end;
    Dummy : word;
  begin
    {API: Close File And Start Queue Job}
    with Request do
      begin
        Len := sizeof(Request) - 2;
        Func := $7F;
        QID := PrintJobData.QueueID;
        JobN := PrintJobData.JobNumber;
      end;
    CloseFileAndStartPrintJobNew := nwServerCall(PrintJobData.Server, $17,
                                                 sizeof(Request), Request,
                                                 0, Dummy);
  end;

function nwqClosePrintJobFile(var PrintJobData : TnwPrintJob;
                              var F : file) : TnwErrorCode;
  begin
    if (PrintJobData.VerifyFlag <> VerifyValue) then
      nwqClosePrintJobFile := NWerrBadData
    else
      begin
        Close(F);
        if (IOresult <> 0) then
          {nothing};
        if (PrintJobData.ServerVersion < ServerVersion311) then
          nwqClosePrintJobFile := CloseFileAndStartPrintJobOld(PrintJobData)
        else
          nwqClosePrintJobFile := CloseFileAndStartPrintJobNew(PrintJobData);
      end;
  end;

function nwqAbortPrintJobFile(var PrintJobData : TnwPrintJob;
                              var F : file) : TnwErrorCode;
  begin
    if (PrintJobData.VerifyFlag <> VerifyValue) then
      nwqAbortPrintJobFile := NWerrBadData
    else
      begin
        Close(F);
        if (IOresult <> 0) then
          {nothing};
        nwqAbortPrintJobFile := nwqRemovePrintJob(PrintJobData);
      end;
  end;

function nwqGetPrintJob(Server : TnwServer; QueueName : TnwObjectStr;
                        JobNumber : nwLong;
                        var PrintJobData : TnwPrintJob) : TnwErrorCode;
  var
    Status : word;
    QID    : nwLong;
  begin
    if (nwShellType = nsNone) then
      Status := nwErrShell
    else
      begin
        Status := nwbGetObjectID(Server, NWboPrintQueue, QueueName, QID);
        if (Status = 0) then
          begin
            FillChar(PrintJobData, sizeof(PrintJobData), 0);
            PrintJobData.Server := Server;
            PrintJobData.JobNumber := JobNumber;
            with PrintJobData do
              begin
                VerifyFlag := VerifyValue;
                QueueID := QID;
                ServerVersion := nwServerVersion(Server);
              end;
            Status := nwqRefreshPrintJob(PrintJobData);
          end;
      end;
    nwqGetPrintJob := Status
  end;

procedure OldEnumPrintJobs(Server : TnwServer; QID : nwLong;
                           EnumFunc : TnwEnumPrintJobFunc;
                           var ExtraData);
  var
    Status : word;
    index  : word;
    Request : record
      Len : word;
      Func: byte;
      Q   : nwLong;
    end;
    StillEnumerating : boolean;
    Reply : record
      NumJobs : word;
      JobList : array [1..250] of word;
    end;
  begin
    with Request do
      begin
        Len := sizeof(Request) - 2;
        Func := $6B;
        Q := QID;
      end;
    Reply.NumJobs := 0;
    Status := nwServerCall(Server, $17,
                           sizeof(Request), Request, sizeof(Reply), Reply);
    if (Status = 0) and (Reply.NumJobs <> 0) then
      begin
        index := 0;
        StillEnumerating := true;
        while StillEnumerating and (index < Reply.NumJobs) do
          begin
            inc(index);
            StillEnumerating := EnumFunc(Reply.JobList[index], ExtraData);
          end;
      end;
  end;

procedure NewEnumPrintJobs(Server : TnwServer; QID : nwLong;
                           EnumFunc : TnwEnumPrintJobFunc;
                           var ExtraData);
  var
    Status : word;
    index  : word;
    LastPos: nwLong;
    Request : record
      Len : word;
      Func: byte;
      Q   : nwLong;
      SPos: nwLong;
    end;
    StillEnumerating : boolean;
    Reply : record
      TotJobs : nwLong;
      NumJobs : nwLong;
      JobList : array [1..125] of nwLong;
    end;
  begin
    LastPos := 1;
    StillEnumerating := true;
    while StillEnumerating and (LastPos <> -1) do
      begin
        with Request do
          begin
            Len := sizeof(Request) - 2;
            Func := $81;
            Q := QID;
            SPos := LastPos;
          end;
        Reply.NumJobs := 0;
        Status := nwServerCall(Server, $17,
                               sizeof(Request), Request,
                               sizeof(Reply), Reply);
        if (Status <> 0) then
          StillEnumerating := false
        else
          if (Reply.NumJobs <> 125) then
            LastPos := -1
          else
            inc(LastPos, 125);
          index := 0;
          while StillEnumerating and (index < Reply.NumJobs) do
            begin
              inc(index);
              StillEnumerating := EnumFunc(Reply.JobList[index], ExtraData);
            end;
      end;
  end;

procedure nwqEnumPrintJobs(Server : TnwServer; QueueName : TnwObjectStr;
                           EnumFunc : TnwEnumPrintJobFunc;
                           var ExtraData);
  var
    QID : nwLong;
  begin
    if (nwShellType <> nsNone) then
      if (nwbGetObjectID(Server, NWboPrintQueue, QueueName, QID) = 0) then
        if (nwServerVersion(Server) < ServerVersion311) then
          OldEnumPrintJobs(Server, QID, EnumFunc, ExtraData)
        else
          NewEnumPrintJobs(Server, QID, EnumFunc, ExtraData);
  end;

end.
