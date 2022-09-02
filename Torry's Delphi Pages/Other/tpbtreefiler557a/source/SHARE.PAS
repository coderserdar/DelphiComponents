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
  {$F-,V-,B-,S-,I-,R-,X+}                                       {!!.51}
  {$IFDEF CanAllowOverlays}
    {$F+,O+,A-}
  {$ENDIF}
  {$IFDEF CanSetOvrflowCheck}
    {$Q-,P-}
  {$ENDIF}

{$IFDEF Win32}
  !! Error - this unit cannot be compiled for 32-bit
{$ENDIF}

unit Share;
  {-DOS 3.x+ network related routines}

interface

uses
  {$IFDEF Windows}
  WinProcs,
  WinTypes,
   {$IFDEF Ver80}
   SysUtils,
   DosSupp,
   {$ELSE}
   WinDos,
   {$ENDIF}
  DPMI;
  {$ELSE}
  {$IFDEF DPMI}
  WinAPI,
  DPMI,
  {$ENDIF}
  Dos;
  {$ENDIF}

const
  shErrShareNotLoaded = $FFFF;  {SHARE.EXE or equivalent not loaded}
  shErrFileNotOpen    = $FFFE;  {File variable is not open}
  shErrBadDosVersion  = $FFFD;  {Call not supported under version of DOS}
  shErrNoDosMemory    = $FFFC;  {No DOS memory available for call}

type
  DeviceType = (DevInvalid,     {enum type for redirectable devices}
                DevPrinter,
                DevDrive);
  PrnSetupStr = string[64];     {type for printer setup strings}
  PCLanOpType = (LanUnknown,    {enum type for PC LAN operating modes}
                 LanRedirector,
                 LanReceiver,
                 LanMessenger,
                 LanServer);

  LocalStr   = string[15];  {string type for local (NetBIOS) names}
  NetworkStr = string[127]; {string type for network names}

var
  DosMajor, DosMinor : byte;    {the workstation's DOS version}

function CancelRedirection(LocalName : LocalStr) : word;
  {-Cancel a redirection previously set with RedirectDevice}

function DosLockRec(var F; FilePosition, FileLength : longint) : word;
  {-Lock region of file
    Notes: this function uses the DOS function to lock a region of a
    file. The function result is 0 if successful, the DOS error code,
    or one of the shErrXxxx error codes.}

function GetExtendedError(var EClass, Action, Locus : byte) : word;
  {-Return extended information about the last DOS error
    Notes: this function must be called *immediately* a DOS error
    occurs and before another DOS function is called.}

function GetMachineName(var MachineName : LocalStr;
                        var MachineNum  : byte) : word;
  {-Return the workstation's machine name and NetBIOS name index}

function GetPrinterSetup(var SetupStr : PrnSetupStr; RDLIndex : word) : word;
  {-Return the printer setup string for the specified device in the
    redirection table}

function GetRedirectionEntry(RDLIndex : Word;
                             var LocalName  : LocalStr;
                             var NetworkName: NetworkStr;
                             var Parameter  : word;
                             var Dev        : DeviceType) : word;
  {-Return information about the specified redirection entry}

function GetTempFileName(PathName : string;
                         var TempFileName : string) : word;
  {-Return a file name guaranteed to be unique in the specified
    directory.
    Notes: the file will be created and closed by this function. You
    must use the returned name to open the file. You are responsible
    for deleting the file if required - it is not automatically
    erased.}

function IBMPCLanLoaded(var Lan : PCLanOpType) : boolean;
  {-Return true and the LAN type if the IBM PC LAN program is loaded.
    Notes: other network programs may also pass this test, eg NetWare
    will if the INT2F TSR has been loaded.}

function IsDriveLocal(Drive : byte) : boolean;
  {-Return true if the specified drive number is local to the current
    workstation.
    Notes: the drive number id one of: 0 = default, 1 = A:, 2 = B:,
    and so on. If any errors occur then the function returns true.}

function IsFileLocal(var F) : Boolean;
  {-Return true if the specified file is local to the current
    workstation.
    Notes: If any errors occur then the function returns true.}

function RedirectDevice(TypeOfDev  : DeviceType;
                        LocalName  : LocalStr;
                        NetworkName: NetworkStr;
                        Password   : NetworkStr;
                        Parameter  : word) : word;
  {-Associate a local name with a network printer or disk.
    Notes: LocalName is the name of a local device (eg LPT1, LPT2,
    etc) or drive (eg 'F:', 'G:', etc). NetworkName specifies the name
    of the network resource LocalName will refer to (the syntax for
    specifying directories may vary from network to network). Password
    may be required by a network to gain access to a network resource.
    Parameter is a user-specified word value that will be returned by
    GetRedirectionEntry. The function result is 0 is successful, or
    the DOS error code if not.}

function SetPrinterSetup(SetupStr : PrnSetupStr;
                         RDLIndex : word) : word;
  {-Define a printer setup string for the specified device in the
    redirection table}

function ShareInstalled : boolean;
  {-Return true if the SHARE.EXE file-sharing engine is installed.
    Notes: this function always returns true under Windows 3.1, so
    to really check you must try and lock a region of a file.}

function UnlockDosRec(var F; FilePosition, FileLength : longint) : word;
  {-Unlock region of file
    Notes: this function uses the DOS function to unlock a region of a
    file. The positiona dnlength parameters must match exactly a
    previous call to DosLockRec. You must unlock all file regions
    explicitly at the end of the program. The function result is 0 if
    successful, the DOS error code, or one of the shErrXxxx error
    codes.}

function UpdateFile(var F) : word;
  {-Flushes an open file to disk.
    Notes: the function result is 0 if successful, or the DOS error
    code, or one of the shErrXxxx error codes.}

implementation

type
  LH = record L, H : word; end;
  OS = record O, S : word; end;

{$IFDEF Windows}
type
 {$IFDEF Ver80}
  Registers = DOSRegisters;                                    {!!.04}
 {$ELSE}
  Registers = TRegisters;
 {$ENDIF}
  FileRec   = TFileRec;
{$ENDIF}

{=== Helper routines ===}

function MinI(X, Y : integer) : integer;
  {-Return minimum of X and Y}
  inline($58/$5A/   {pop ax & dx}
         $39/$D0/   {cmp ax, dx}
         $7C/$01/   {jl @@exit}
         $92        {xchg ax, dx}
         );         {@@exit:}

procedure CvtAsciizToStr(var Buffer; MaxStrLen : byte);
  {-Convert an ASCIIZ string to a Pascal string in situ}
  var
    AZ : array [0..255] of char absolute Buffer;
    S  : string absolute Buffer;
    i  : integer;
  begin
    i := 0;
    while (i < MaxStrLen) and (AZ[i] <> #0) do
      inc(i);
    Move(AZ[0], S[1], i);
    S[0] := char(i);
  end;

procedure CvtStrToAsciiz(var Buffer; MaxStrLen : byte);
  {-Convert Pascal string to an ASCIIZ string in situ}
  var
    AZ : array [0..255] of char absolute Buffer;
    S  : string absolute Buffer;
    i  : integer;
  begin
    i := MinI(MaxStrLen, length(S));
    if (i <> 0) then
      Move(S[1], AZ[0], i);
    AZ[i] := #0;
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

{=== Interfaced routines ===}

function DosLockRec(var F; FilePosition, FileLength : longint) : word;
  var
    Regs : Registers;
  begin
    if (DOSMajor < 3) then
      DosLockRec:= shErrBadDosVersion
    else if (FileRec(F).Mode = fmClosed) then
      DosLockRec := shErrFileNotOpen
    else
      begin
        FillChar(Regs, sizeof(Regs), 0);
        with Regs do
          begin
            ax := $5C00;
            bx := FileRec(F).Handle;
            cx := LH(FilePosition).H;
            dx := LH(FilePosition).L;
            si := LH(FileLength).H;
            di := LH(FileLength).L;
            Intr($21, Regs);
            if not Odd(Flags) then
              ax := 0;
            DosLockRec := ax
          end;
      end;
  end;

function UnlockDosRec(var F; FilePosition, FileLength : longint) : word;
  var
    Regs : Registers;
  begin
    if (DOSMajor < 3) then
      UnlockDosRec := shErrBadDosVersion
    else if (FileRec(F).Mode = fmClosed) then
      UnlockDosRec := shErrFileNotOpen
    else
      begin
        FillChar(Regs, sizeof(Regs), 0);
        with Regs do
          begin
            ax := $5C01;
            bx := FileRec(F).Handle;
            cx := LH(FilePosition).H;
            dx := LH(FilePosition).L;
            si := LH(FileLength).H;
            di := LH(FileLength).L;
            Intr($21, Regs);
            if not Odd(Flags) then
              ax := 0;
            UnlockDosRec := ax
          end;
      end;
  end;

function UpdateFile(var F) : word;
  var
    Regs : Registers;
  begin
    if (FileRec(F).Mode = fmClosed) then
      UpdateFile := shErrFileNotOpen
    else if (DosMajor > 3) or ((DosMajor = 3) and (DosMinor >= 30)) then
      begin
        FillChar(Regs, sizeof(Regs), 0);
        with Regs do
          begin
            ah := $68; {commit file}
            bx := FileRec(F).Handle;
            Intr($21, Regs);
            if not Odd(Flags) then
              ax := 0;
            UpdateFile := ax
          end;
      end
    else
      begin
        FillChar(Regs, sizeof(Regs), 0);
        with Regs do
          begin
            ah := $45; {duplicate handle}
            bx := FileRec(F).Handle;
            Intr($21, Regs);
            if Odd(Flags) then
              UpdateFile := ax
            else
              begin
                bx := ax;
                ah := $3E; {close file handle}
                Intr($21, Regs);
                if not Odd(Flags) then
                  ax := 0;
                UpdateFile := ax
              end;
          end;
      end;
  end;

function GetExtendedError(var EClass, Action, Locus : byte) : word;
  var
    Regs : Registers;
  begin
    if (DosMajor < 3) then
      GetExtendedError := shErrBadDosVersion
    else
      begin
        FillChar(Regs, sizeof(Regs), 0);
        with Regs do
          begin
            ah := $59;
            Intr($21, Regs);
            GetExtendedError := AX;
            EClass := BH;
            Action := BL;
            Locus := CH;
          end;
      end;
  end;


function IBMPCLanLoaded(var Lan : PCLanOpType) : boolean;
  const
    REDIRECTORFLAG  = $0008;
    RECEIVERFLAG    = $0080;
    MESSENGERFLAG   = $0004;
    SERVERFLAG      = $0040;
  var
    {$IFDEF DPMIOrWnd}                                         {!!.51}
    Regs : DPMIRegisters;                                      {!!.51}
    {$ELSE}                                                    {!!.51}
    Regs : Registers;
    {$ENDIF}                                                   {!!.51}
  begin
    IBMPCLanLoaded := false;
    Lan := LanUnknown;
    if (DosMajor < 3) then
      Exit;
    with Regs do
      begin
        FillChar(Regs, sizeof(Regs), 0);
        ax := $B800;
        {$IFDEF DPMIorWnd}
        SimulateRealModeInt($2F, Regs);                        {!!.51}
        {$ELSE}
        Intr($2F, Regs);
        {$ENDIF}
        if (al = 0) then
          Exit;
        IBMPCLanLoaded := true;
        if      ((bl and SERVERFLAG) <> 0) then
          Lan := LanServer
        else if ((bl and MESSENGERFLAG) <> 0) then
          Lan := LanMessenger
        else if ((bl and RECEIVERFLAG) <> 0) then
          Lan := LanReceiver
        else if ((bl and REDIRECTORFLAG) <> 0) then
          Lan := LanRedirector;
      end;
  end;

function IsDriveLocal(Drive : byte) : boolean;
  var
    Regs : Registers;
  begin
    IsDriveLocal := true;
    if (DosMajor >= 3) then
      begin
        FillChar(Regs, sizeof(Regs), 0);
        with Regs do
          begin
            ax := $4409;
            bl := Drive;
            Intr($21, Regs);
            if not Odd(Flags) then
              IsDriveLocal := (dx and $1000) = 0;
          end;
      end;
  end;

function IsFileLocal(var F) : boolean;
  var
    Regs : Registers;
  begin
    IsFileLocal := true;
    if (DosMajor >= 3) then
      begin
        FillChar(Regs, sizeof(Regs), 0);
        with Regs do
          begin
            ax := $440A;
            bx := FileRec(F).Handle;
            Intr($21, Regs);
            if not Odd(Flags) then
              IsFileLocal := (dx and $8000) = 0;
          end;
      end;
  end;

function ShareInstalled : boolean;
  var
    {$IFDEF DPMI}                                              {!!.51}
    Regs : DPMIRegisters;                                      {!!.51}
    {$ELSE}                                                    {!!.51}
    Regs : Registers;
    {$ENDIF}                                                   {!!.51}
  begin
    {Under Windows:
       1. VSHARE.386 can be present instead of SHARE.EXE, & DOS
          boxes will even 'see' SHARE as being present.
       2. The pmode INT $2F call always returns true.
       3. The real mode INT $2F call will miss the presence of
          VSHARE.386.
       4. Microsoft recommend that SHARE/VSHARE is loaded, and
          many Windows programs won't work without it.
     Hence this routine always returns true}
    {$IFDEF Windows}                                            {!!.51}
    ShareInstalled := true;                                     {!!.51}
    {$ELSE}                                                     {!!.51}
    ShareInstalled := false;
    if (DosMajor >= 3) then
      begin
        FillChar(Regs, sizeof(Regs), 0);
        with Regs do
          begin
            AX := $1000;
            {$IFDEF DPMI}                                       {!!.51}
            SimulateRealModeInt($2F, Regs);                     {!!.51}
            {$ELSE}
            Intr($2F, Regs);
            {$ENDIF}
            ShareInstalled := (AL = $FF);
          end;
      end;
    {$ENDIF}                                                    {!!.51}
  end;

function GetPrinterSetup(var SetupStr : PrnSetupStr; RDLIndex : word) : word;
  var
    RealS: pointer;
    St   : ^string;
    {$IFDEF DPMIorWnd}
    Regs : DPMIRegisters;
    {$ELSE}
    Regs : Registers;
    TempS: string;
    {$ENDIF}
  begin
    if (DosMajor < 3) or ((DosMajor = 3) and (DosMinor < 10)) then
      GetPrinterSetup := shErrBadDosVersion
    else
      begin
        {$IFDEF DPMIorWnd}
        if not DOSGetMem(RealS, St, SizeOf(PrnSetUpStr)) then
          begin
            GetPrinterSetup := shErrNoDosMemory;
            Exit;
          end;
        {$ELSE}
        St := @TempS;
        RealS := St;
        {$ENDIF}
        FillChar(Regs, sizeof(Regs), 0);
        with Regs do
          begin
            ax := $5E03;
            bx := RDLIndex;
            es := OS(RealS).S;
            di := succ(OS(RealS).O);
            {$IFDEF DPMIorWnd}
            SimulateRealModeInt($21, Regs);
            {$ELSE}
            Intr($21, Regs);
            {$ENDIF}
            if Odd(Flags) then
              GetPrinterSetUp := ax
            else
              begin
                GetPrinterSetup := 0;
                St^[0] := char(cx);
                SetupStr := St^;
              end;
          end;
        {$IFDEF DPMIorWnd}
        if not DOSFreeMem(St) then
          {nothing};
        {$ENDIF}
      end;
  end;

function SetPrinterSetup(SetupStr : PrnSetupStr; RDLIndex : word) : word;
  var
    RealS: pointer;
    {$IFDEF DPMIorWnd}
    St   : ^string;
    Regs : DPMIRegisters;
    {$ELSE}
    Regs : Registers;
    {$ENDIF}
  begin
    if (DosMajor < 3) or ((DosMajor = 3) and (DosMinor < 10)) then
      SetPrinterSetup := shErrBadDosVersion
    else
      begin
        {$IFDEF DPMIorWnd}
        if not DOSGetMem(RealS, St, SizeOf(PrnSetupStr)) then
          begin
            SetPrinterSetup := shErrNoDosMemory;
            Exit;
          end;
        St^ := SetupStr;
        {$ELSE}
        RealS := @SetupStr;
        {$ENDIF}
        FillChar(Regs, sizeof(Regs), 0);
        with Regs do
          begin
            ax := $5E02;
            bx := RDLIndex;
            cx := length(SetupStr);
            ds := OS(RealS).S;
            si := succ(OS(RealS).O);
            {$IFDEF DPMIorWnd}
            SimulateRealModeInt($21, Regs);
            {$ELSE}
            Intr($21, Regs);
            {$ENDIF}
            if not Odd(Flags) then
              ax := 0;
            SetPrinterSetUp := ax
          end;
        {$IFDEF DPMIorWnd}
        if not DOSFreeMem(St) then
          {nothing};
        {$ENDIF}
      end;
  end;

function GetMachineName(var MachineName : LocalStr;
                        var MachineNum  : byte) : word;
  var
    RealS: pointer;
    St   : ^string;
    {$IFDEF DPMIorWnd}
    Regs : DPMIRegisters;
    {$ELSE}
    Regs : Registers;
    TempS: LocalStr;
    {$ENDIF}
  begin
    MachineName := '';
    MachineNum := 0;
    if (DosMajor < 3) or ((DosMajor = 3) and (DosMinor < 10)) then
      GetMachineName := shErrBadDosVersion
    else
      begin
        {$IFDEF DPMIorWnd}
        if not DOSGetMem(RealS, St, SizeOf(LocalStr)) then      {!!.51}
          begin
            GetMachineName := shErrNoDosMemory;
            Exit;
          end;
        {$ELSE}
        St := @TempS;
        RealS := St;
        {$ENDIF}
        FillChar(Regs, sizeof(Regs), 0);
        with Regs do
          begin
            ax := $5E00;
            ds := OS(RealS).S;
            dx := OS(RealS).O;                                  {!!.51}
            {$IFDEF DPMIorWnd}
            SimulateRealModeInt($21, Regs);
            {$ELSE}
            Intr($21, Regs);
            {$ENDIF}
            if Odd(Flags) then
              GetMachineName := ax
            else
              begin
                GetMachineName := 0;
                {$IFDEF DPMIorWnd}
                if (Hi(LH(cx).L) <> 0) then
                {$ELSE}
                if (ch <> 0) then
                {$ENDIF}
                  begin {(ch <> 0) => machine name is defined}
                    CvtAsciizToStr(St^, pred(sizeof(LocalStr)));
                    MachineName := St^;
                    MachineNum := cx and $FF;
                  end;
              end;
          end;
        {$IFDEF DPMIorWnd}
        if not DOSFreeMem(St) then
          {nothing};
        {$ENDIF}
      end;
  end;

function GetTempFileName(PathName : string;
                         var TempFileName : string) : word;
  var
    RealS: pointer;
    St   : ^string;
    {$IFDEF DPMIorWnd}
    Regs : DPMIRegisters;
    {$ELSE}
    Regs : Registers;
    TempS: string;
    {$ENDIF}
  begin
    TempFileName := '';
    if (DosMajor < 3) then
      GetTempFileName := shErrBadDosVersion
    else
      begin
        {$IFDEF DPMIorWnd}
        if not DOSGetMem(RealS, St, SizeOf(string)) then
          begin
            GetTempFileName := shErrNoDosMemory;
            Exit;
          end;
        {$ELSE}
        St := @TempS;
        RealS := St;
        {$ENDIF}
        St^ := PathName;
        CvtStrToAsciiz(St^, pred(sizeof(string)));
        FillChar(Regs, sizeof(Regs), 0);
        with Regs do
          begin
            ax := $5A00;
           {cx := 0;}
            ds := OS(RealS).S;
            dx := OS(RealS).O;
            {$IFDEF DPMIorWnd}
            SimulateRealModeInt($21, Regs);
            {$ELSE}
            Intr($21, Regs);
            {$ENDIF}
            if Odd(Flags) then
              GetTempFileName := ax
            else
              begin
                bx := ax;
                ax := $3E00; {close file handle}
                {$IFDEF DPMIorWnd}
                SimulateRealModeInt($21, Regs);
                {$ELSE}
                Intr($21, Regs);
                {$ENDIF}
                if not Odd(Flags) then
                  ax := 0;
                GetTempFileName := ax;
                CvtAsciizToStr(St^, pred(sizeof(string)));
                TempFileName := St^;
              end;
          end;
        {$IFDEF DPMIorWnd}
        if not DOSFreeMem(St) then
          {nothing};
        {$ENDIF}
      end;
  end;

function CancelRedirection(LocalName : LocalStr) : word;
  var
    RealS: pointer;
    St   : ^string;
    {$IFDEF DPMIorWnd}
    Regs : DPMIRegisters;
    {$ELSE}
    Regs : Registers;
    {$ENDIF}
  begin
    if (DosMajor < 3) or ((DosMajor = 3) and (DosMinor < 10)) then
      CancelRedirection := shErrBadDosVersion
    else
      begin
        {$IFDEF DPMIorWnd}
        if not DOSGetMem(RealS, St, SizeOf(LocalStr)) then
          begin
            CancelRedirection := shErrNoDosMemory;
            Exit;
          end;
        St^ := LocalName;
        {$ELSE}
        RealS := @LocalName;
        St := RealS;
        {$ENDIF}
        CvtStrToAsciiz(St^, pred(sizeof(LocalStr)));
        FillChar(Regs, sizeof(Regs), 0);
        with Regs do
          begin
            ax := $5F04;
            ds := OS(RealS).S;
            si := OS(RealS).O;
            {$IFDEF DPMIorWnd}
            SimulateRealModeInt($21, Regs);
            {$ELSE}
            Intr($21, Regs);
            {$ENDIF}
            if not Odd(Flags) then
              ax := 0;
            CancelRedirection := ax;
          end;
        {$IFDEF DPMIorWnd}
        if not DOSFreeMem(St) then
          {nothing};
        {$ENDIF}
      end;
  end;


function GetRedirectionEntry(RDLIndex : Word;
                             var LocalName  : LocalStr;
                             var NetworkName: NetworkStr;
                             var Parameter  : word;
                             var Dev        : DeviceType) : word;
  type
    PRedir = ^TRedir;
    TRedir = record
      Local   : LocalStr;
      Network : NetworkStr;
    end;
  var
    RealData : pointer;
    RedirData: PRedir;
    {$IFDEF DPMIorWnd}
    Regs : DPMIRegisters;
    {$ELSE}
    Regs : Registers;
    TempData : TRedir;
    {$ENDIF}
  begin
    if (DosMajor < 3) or ((DosMajor = 3) and (DosMinor < 10)) then
      GetRedirectionEntry := shErrBadDosVersion
    else
      begin
        {$IFDEF DPMIorWnd}
        if not DOSGetMem(RealData, RedirData, SizeOf(TRedir)) then
          begin
            GetRedirectionEntry := shErrNoDosMemory;
            Exit;
          end;
        {$ELSE}
        RedirData := @TempData;
        RealData := RedirData;
        {$ENDIF}
        FillChar(RedirData^, sizeof(TRedir), 0);
        FillChar(Regs, sizeof(Regs), 0);
        with Regs do
          begin
            ax := $5F02;
            bx := RDLIndex;
            ds := OS(RealData).S;
            si := OS(RealData).O;
            es := OS(RealData).S;
            di := OS(RealData).O + sizeof(LocalStr);
            {$IFDEF DPMIorWnd}
            SimulateRealModeInt($21, Regs);
            {$ELSE}
            Intr($21, Regs);
            {$ENDIF}
            if Odd(Flags) then
              GetRedirectionEntry := ax
            else
              begin
                GetRedirectionEntry := 0;
                if ((bx and $10) = 0) then
                  if ((bx and $0F) = 3) then
                       Dev := DevPrinter
                  else Dev := DevDrive
                else   Dev := DevInvalid;
                Parameter := cx;
                with RedirData^ do
                  begin
                    CvtAsciizToStr(Local, pred(sizeof(LocalStr)));
                    LocalName := Local;
                    CvtAsciizToStr(Network, pred(sizeof(NetworkStr)));
                    NetworkName := Network;
                  end;
              end;
          end;
        {$IFDEF DPMIorWnd}
        if not DOSFreeMem(RedirData) then
          {nothing};
        {$ENDIF}
      end;
  end;

function RedirectDevice(TypeOfDev  : DeviceType;
                        LocalName  : LocalStr;
                        NetworkName: NetworkStr;
                        Password   : NetworkStr;
                        Parameter  : word) : word;
  type
    PRedir = ^TRedir;
    TRedir = record
      Local   : LocalStr;
      Network : string;
    end;
  var
    RealData : pointer;
    RedirData: PRedir;
    {$IFDEF DPMIorWnd}
    Regs : DPMIRegisters;
    {$ELSE}
    Regs : Registers;
    TempData : TRedir;
    {$ENDIF}
  begin
    if (DosMajor < 3) or ((DosMajor = 3) and (DosMinor < 10)) then
      RedirectDevice := shErrBadDosVersion
    else
      begin
        {$IFDEF DPMIorWnd}
        if not DOSGetMem(RealData, RedirData, SizeOf(TRedir)) then
          begin
            RedirectDevice := shErrNoDosMemory;
            Exit;
          end;
        {$ELSE}
        RedirData := @TempData;
        RealData := RedirData;
        {$ENDIF}
        FillChar(RedirData^, sizeof(TRedir), 0);
        with RedirData^ do
          begin
            Local := LocalName;
            CvtStrToAsciiz(Local, pred(sizeof(LocalStr)));      {!!.51}
            Network := NetworkName + #0 + Password;
            CvtStrToAsciiz(Network, pred(sizeof(string)));      {!!.51}
          end;
        FillChar(Regs, sizeof(Regs), 0);
        with Regs do
          begin
            ax := $5F03;
            if (TypeOfDev = DevPrinter) then
                 bx := 3
            else bx := 4;
            cx := Parameter;
            ds := OS(RealData).S;
            si := OS(RealData).O;
            es := OS(RealData).S;
            di := OS(RealData).O + sizeof(LocalStr);
            {$IFDEF DPMIorWnd}
            SimulateRealModeInt($21, Regs);
            {$ELSE}
            Intr($21, Regs);
            {$ENDIF}
            if not Odd(Flags) then
              ax := 0;
            RedirectDevice := ax
          end;
        {$IFDEF DPMIorWnd}
        if not DOSFreeMem(RedirData) then
          {nothing};
        {$ENDIF}
      end;
  end;

var
  DosVer : word;
begin
  DosVer := DosVersion;
  DosMajor := Lo(DosVer);
  DosMinor := Hi(DosVer);
end.

