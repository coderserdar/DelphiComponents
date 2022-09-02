{********************************************************************}
{* ISNETSUP.INC - network support routines                          *}
{********************************************************************}

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

{$IFDEF MSDOS}
type
  IsamINT24HandlerDef = record
    PushAXBX,
    PushCXDX,
    PushBPSI,
    PushDIDS,
    PushESF        : Word;
    AndDIFF1       : LongInt;
    CmpDI02        : LongInt;
    Jz1DoOldINT    : Word;                               {!!.41 mod}
    CmpDI09        : LongInt;                            {!!.41}
    Jz2DoOldINT    : Word;                               {!!.41}
    MovAHExtErr    : Word;
    NopMovBX       : Word;
    Level          : Word;
    Int21          : Word;
    JcDoOldINT     : Word;
    CmpBH, LockNop : Word;
    JzMyTurn       : Word;
    Cmp1BH         : Word;
    InterLockNop   : Word;
    Jz1MyTurn      : Word;
{ DoOldINT : }
    PopFES1,
    PopDSDI1,
    PopSIBP1,
    PopDXCX1,
    PopBXAX1       : Word;
    NopJmpFar      : Word;
    OldINT24Addr   : Pointer;
{ MyTurn : }
    NopMovAX       : Word;
    SegLockError   : Word;
    MovDSAX        : Word;
    MovByte        : Word;
    OfsLockError   : Word;
    ConstTrueNop   : Word;
    PopFES2,
    PopDSDI2,
    PopSIBP2,
    PopDXCX2,
    PopBXAX2       : Word;
    MovALFail      : Word;
    IRetNop        : Word;
  end;

const
  IsamINT24Handler : IsamINT24HandlerDef = (
    PushAXBX : $5350;
    PushCXDX : $5251;
    PushBPSI : $5655;
    PushDIDS : $1E57;
    PushESF : $9C06;
    AndDIFF1 : $00FFE781;
    CmpDI02 : $0002FF81;
    Jz1DoOldINT : $1674;                                 {!!.41 mod}
    CmpDI09 : $0009FF81;                                 {!!.41}
    Jz2DoOldINT : $1074;                                 {!!.41}
    MovAHExtErr: $59B4;
    NopMovBX : $BB90;
    Level : $0000;
    Int21 : $21CD;
    JcDoOldINT : $0672;
    CmpBH : $FF80;
    LockNop : $9002;
    JzMyTurn : $1674;
    Cmp1BH : $FF80;
    InterLockNop : $900A;
    Jz1MyTurn : $1074;

    PopFES1 : $079D;
    PopDSDI1 : $5F1F;
    PopSIBP1 : $5D5E;
    PopDXCX1 : $595A;
    PopBXAX1 : $585B;
    NopJmpFar : $EA90;
    OldINT24Addr : Nil;

    NopMovAX : $B890;
    SegLockError : $FFFF;
    MovDSAX : $D88E;
    MovByte : $06C6;
    OfsLockError : $FFFF;
    ConstTrueNop : $9000;
    PopFES2 : $079D;
    PopDSDI2 : $5F1F;
    PopSIBP2 : $5D5E;
    PopDXCX2 : $595A;
    PopBXAX2 : $585B;
    MovALFail : $03B0;
    IRetNop : $90CF);


procedure IsamInstallInt24Handler;
begin
  with IsamINT24Handler do begin
    CallGetIntVec($24, OldINT24Addr);
    SegLockError := Seg (IsamLockError);
    OfsLockError := Ofs (IsamLockError);
    ConstTrueNop := ConstTrueNop + Ord (True);
  end;
  CallSetIntVec($24, @ IsamINT24Handler);
end;


procedure IsamRemoveInt24Handler;
begin
  CallSetIntVec($24, IsamINT24Handler.OldINT24Addr);
end;
{$ENDIF}


{===NoNet network definition=========================================}
function NoNetLockRecord(Start,
                         Len       : LongInt;
                         Handle    : IsamHandle;
                         TimeOut,
                         DelayTime : Word) : Boolean; far;
begin
  NoNetLockRecord := True;
end;
{--------}
function NoNetUnLockRecord(Start,
                           Len    : LongInt;
                           Handle : IsamHandle) : Boolean; far;
begin
  NoNetUnLockRecord := True;
end;
{--------}
function NoNetExitNet : Boolean; far;
begin
  {$IFDEF MSDOS}
  IsamRemoveInt24Handler;
  {$ENDIF}
  NoNetExitNet := True;
end;
{--------}
function NoNetInitNet : Boolean;
begin
  IsamDefNrOfWS := 0;
  {$IFDEF MSDOS}
  IsamInstallInt24Handler;
  {$ENDIF}
  IsamLockRecord := NoNetLockRecord;
  IsamUnLockRecord := NoNetUnLockRecord;
  IsamExitNet := NoNetExitNet;
  NoNetInitNet := True;
end;
{====================================================================}


{===Novell network definition========================================}
{$IFDEF Novell}
{$IFDEF SupportVLM} {!!.TP}
function NovellLockRecord(Start,
                          Len       : LongInt;
                          Handle    : IsamHandle;
                          TimeOut,
                          DelayTime : Word) : Boolean; far;
var
  Status : word;
  TimeCount : word;
begin
  Status := nwLockRecord(Handle, Start, Len, TimeOut div 55); {!!.51}
  if ((Lo(Status) = $FF) and (TimeOut <> 0)) then
    begin
      {-Call has failed without waiting for timeout; this occurs only
        if a region is attempted to be locked which is already locked
        by another task of the same machine; so give preemptive
        multitasking a chance by reapeating the call with delays.}
      TimeCount := DelayTime;
      while (Status <> 0) and (TimeCount < TimeOut) do
        begin
          Status := nwLockRecord(Handle, Start, Len, 0);
          inc(TimeCount, DelayTime);
          if (Status <> 0) and (TimeCount < TimeOut) then
            IsamDelay(DelayTime);
        end;
    end;
  if (Status <> 0) and (IsamDOSError = 0) then
    begin
      if (nwShellType = nsVLM) then
           IsamDOSFunc := $1A01
      else IsamDOSFunc := $BC01;
      IsamDOSError := Status;
    end;
  NovellLockRecord := (Status = 0);
end;
{--------}
function NovellUnlockRecord(Start,
                          Len    : LongInt;
                          Handle : IsamHandle) : Boolean; far;
var
  Status : word;
begin
  Status := nwUnlockRecord(Handle, Start, Len);
  if (Status <> 0) then
    begin
      if (IsamDOSError = 0) then
        begin
          if (nwShellType = nsVLM) then
               IsamDOSFunc := $1E00
          else IsamDOSFunc := $BE00;
          IsamDOSError := Status;
        end;
      NovellUnlockRecord := False;
    end
  else
    NovellUnlockRecord := True;
end;
{--------}
function NovellExitNet : Boolean; far;
begin
  {$IFDEF MSDOS}
  IsamRemoveInt24Handler;
  {$ENDIF}
  NovellExitNet := True;
end;
{$ELSE}
type
  ILI = record Lo, Hi : word; end;
function NovellLockRecord(Start,
                          Len       : LongInt;
                          Handle    : IsamHandle;
                          TimeOut,
                          DelayTime : Word) : Boolean; far;
var
  IRR       : GenRegisters;
  TimeCount : Word;
begin
  DefaultRegisters(IRR);                              {!!.41}
  with IRR do begin
    BX := Handle;
    CX := ILI(Start).Hi;
    DX := ILI(Start).Lo;
    SI := ILI(Len).Hi;
    DI := ILI(Len).Lo;
    BP := TimeOut Div 55;
    AH := $BC;
    AL := $01;
    if IsamDOSError = 0 then
      IsamDOSFunc := AX;
    {$IFDEF DPMI}
    CallDPMIRealModeIntr($21, IRR);
    {$else}
    CallMsDos(IRR);
    {$ENDIF}
    if IsamDOSError = 0 then
      IsamDOSError := AL;
    if (AL = $FF) and (TimeOut <> 0) then begin
      {-Call is failed without waiting for timeout; this occurs only, if a
        region is attempted to be locked, which is already locked by
        another task of the same machine; so give preemptive multitasking
        a chance by reapeating the call with delays}
      TimeCount := DelayTime;
      while (AL <> 0) and (TimeCount < TimeOut) do begin
        DefaultRegisters(IRR);                        {!!.41}
        BX := Handle;
        CX := ILI(Start).Hi;
        DX := ILI(Start).Lo;
        SI := ILI(Len).Hi;
        DI := ILI(Len).Lo;
        BP := 0; {No timeout; it doesn't work anyway}
        AH := $BC;
        AL := $01;
        if IsamDOSError = 0 then
          IsamDOSFunc := AX;
        {$IFDEF DPMI}
        CallDPMIRealModeIntr($21, IRR);
        {$else}
        CallMsDos(IRR);
        {$ENDIF}
        if IsamDOSError = 0 then
          IsamDOSError := AX;
        Inc(TimeCount, DelayTime);
        if (AL <> 0) and (TimeCount < TimeOut) then begin
          IsamDelay(DelayTime);
        end;
      end;
    end;
    NovellLockRecord := AL = 0;
  end;
end;
{--------}
function NovellUnLockRecord(Start,
                            Len    : LongInt;
                            Handle : IsamHandle) : Boolean; far;
var
  IRR : GenRegisters;
begin
  DefaultRegisters(IRR);                                {!!.41}
  with IRR do begin
    BX := Handle;
    CX := ILI (Start).Hi;
    DX := ILI (Start).Lo;
    SI := ILI (Len).Hi;                                    {!!.41}
    DI := ILI (Len).Lo;                                    {!!.41}
    AX := $BE00;
    if IsamDOSError = 0 then
      IsamDOSFunc := AX;
    {$IFDEF DPMI}
    CallDPMIRealModeIntr($21, IRR);
    {$else}
    CallMsDos(IRR);
    {$ENDIF}
    if IsamDOSError = 0 then
      IsamDOSError := AL;
    NovellUnLockRecord := AL = 0;
  end;
end;
{--------}
function NovellExitNet : Boolean; far;
begin
  {$IFDEF MSDOS}
  IsamRemoveInt24Handler;
  {$ENDIF}
  NovellExitNet := True;
end;
{$ENDIF}
{--------}
function NovellInitNet : Boolean;
  {------}
  function NovellInstalled : Boolean;                        {!!.41}
  {$IFDEF SupportVLM} {!!.TP}
  begin
    NovellInstalled := (nwShellType <> nsNone);
  end;
  {$else}
    {----}
    function ServerInfoAvailable : Boolean;
    type
      TRequestBuf = Record
        BufLen  : Word;
        SubFunc : Byte;
      end;
      TServerInfo = Record
        BufLen              : Word;
        ServerNameStartChar : Char;
        NotUsedSpace        : Array [1..127] Of Byte;
      end;
    var
      RequestBufPtr : ^TRequestBuf;
      ServerInfoPtr : ^TServerInfo;
      IRR           : GenRegisters;
      RealModePtr,
      ProtModePtr   : Pointer;
    begin
      ServerInfoAvailable := False;

      if not CallAllocRealModeMem(SizeOf (TRequestBuf)
          + SizeOf (TServerInfo), RealModePtr, ProtModePtr) then Exit;

      RequestBufPtr := ProtModePtr;
      ServerInfoPtr := ProtModePtr;
      Inc (Word (ServerInfoPtr), SizeOf (TRequestBuf));
      FillChar(ServerInfoPtr^, SizeOf (TServerInfo), 0);
      with RequestBufPtr^ do begin
        BufLen := 1;
        SubFunc := $11;
      end;

      ServerInfoPtr^.BufLen := SizeOf (TServerInfo) - 2;

      DefaultRegisters(IRR);
      with IRR do begin
        AX := $E300;
        DS := LongInt (RealModePtr) Shr 16;
        SI := LongInt (RealModePtr) and $FFFF;
        ES := DS;
        DI := SI + SizeOf (TRequestBuf);
      end;
      CallDPMIRealModeIntr($21, IRR);

      ServerInfoAvailable := ServerInfoPtr^.ServerNameStartChar <> #0;

      CallFreeRealModeMem(SizeOf (TRequestBuf)
          + SizeOf (TServerInfo), ProtModePtr);
    end;
    {----}
  begin
    NovellInstalled := ServerInfoAvailable;
  end;
  {$ENDIF}
  {------}
begin
  NovellInitNet := False;                                    {!!.41}
  if TestNetExistance then begin                             {!!.41}
    if not NovellInstalled then Exit;                        {!!.41}
  end;                                                       {!!.41}
  IsamDefNrOfWS := MaxNrOfWorkStations;
  {$IFDEF MSDOS}
  IsamInstallInt24Handler;
  {$ENDIF}
  IsamLockRecord := NovellLockRecord;
  IsamUnLockRecord := NovellUnLockRecord;
  IsamExitNet := NovellExitNet;
  NovellInitNet := True;                                     {!!.41}
end;
{$ENDIF}
{====================================================================}


{===MsNet network definition=========================================}
{$IFDEF MsNet}
function MsNetLockRecord(Start,
                         Len       : LongInt;
                         Handle    : IsamHandle;
                         TimeOut,
                         DelayTime : Word) : Boolean; far;
var
  TimeCount : longint;
  LockedIt  : boolean;
begin
  TimeCount := 0;
  repeat
    inc(TimeCount, DelayTime);
    if not btfLockFile(Handle, Start, Len) then begin
      SetIsamDOSError;
      if (TimeCount < TimeOut) then
        IsamDelay(DelayTime);
      LockedIt := false;
    end
    else
      LockedIt := true;
  until LockedIt or (TimeCount >= TimeOut);
  MsNetLockRecord := LockedIt;
end;
{--------}
function MsNetUnLockRecord(Start,
                           Len    : LongInt;
                           Handle : IsamHandle) : Boolean; far;
begin
  if not btfUnlockFile(Handle, Start, Len) then begin
    SetIsamDOSError;
    MsNetUnLockRecord := false;
  end
  else
    MsNetUnLockRecord := true;
end;
{--------}
function MsNetExitNet : Boolean; far;
begin
  {$IFDEF MSDOS}
  IsamRemoveInt24Handler;
  {$ENDIF}
  MsNetExitNet := true;
end;
{--------}
function MsNetInitNet : Boolean;
  {------}
  function MSNetInstalled : boolean;
  begin
    MSNetInstalled := true; {check removed !!.53}
  end;
  {------}
begin
  MsNetInitNet := False;
  if TestNetExistance then
    if not MSNetInstalled then
      Exit;
  IsamDefNrOfWS := MaxNrOfWorkStations;
  IsamLockRecord := MsNetLockRecord;
  IsamUnLockRecord := MsNetUnLockRecord;
  IsamExitNet := MsNetExitNet;
  MsNetInitNet := true;
end;
{$ENDIF}
{====================================================================}


{--Additional net interfaces insert here}


{====================================================================}
function IsamInitNet(ExpectedNet : NetSupportType) : Boolean;
begin
  case ExpectedNet Of
    NoNet  : IsamInitNet := NoNetInitNet;
    {$IFDEF Novell}
    Novell : IsamInitNet := NovellInitNet;
    {$ENDIF}
    {$IFDEF MsNet}
    MsNet  : IsamInitNet := MsNetInitNet;
    {$ENDIF}
  else
    IsamInitNet := False;
  end; {Case}
end;

function IsamDoneNet : Boolean;
begin
  IsamDoneNet := IsamExitNet;
end;
{====================================================================}

