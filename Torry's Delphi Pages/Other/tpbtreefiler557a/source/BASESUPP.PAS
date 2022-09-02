{********************************************************************}
{* BASESUPP.PAS - Basic interrupt and DOS support routines          *}
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

{--Conditional defines and compiler options}
{$I BTDefine.Inc}
{$IFDEF CanAllowOverlays}
  {$O+,F+}
{$ENDIF}

{$IFDEF Win32}
  !! Error - this unit cannot be compiled for 32-bit
{$ENDIF}

unit BaseSupp;

interface

uses
{$IFDEF Windows}
  WinProcs                                                  {!!.41}
 {$IFNDEF VER80}                                            {!!.51}
  , WinDos                                                  {!!.51}
 {$ENDIF};                                                  {!!.51}
{$ELSE}
 {$IFDEF DPMI}
  WinAPI,                                                   {!!.41}
 {$ENDIF}
  Dos;
{$ENDIF}

type
  GenRegisters = record
    case Integer of
      0: (AX, BX, CX, DX, BP, SI, DI, DS, ES, Flags: Word);
      1: (AL, AH, BL, BH, CL, CH, DL, DH: Byte);
  end;

var
  AdrMsDos,
  AdrIntr,
  AdrDPMIRealModeIntr,
  AdrDosVersion,
  AdrSetIntVec,
  AdrGetIntVec,
  AdrGetTime,
  AdrGetDate    : Pointer;


procedure CallMsDos(var Regs : GenRegisters); inline
   ($FF / $1E / AdrMsDos); {call far dword ptr [AdrMsDos]}
  {-Call MsDos via AdrMsDos and allows to use type GenRegisters}


procedure CallIntr(IntNo : Byte; var Regs : GenRegisters); inline
   ($FF / $1E / AdrIntr); {call far dword ptr [AdrIntr]}
  {-Call Intr via AdrMsDos and allows to use type GenRegisters}


procedure CallDPMIRealModeIntr(IntNo : Byte;
                           var Regs  : GenRegisters); inline
   ($FF / $1E / AdrDPMIRealModeIntr);
      {-call far dword ptr [AdrDPMIRealModeIntr]}
  {-Call Intr via AdrDPMIRealModeIntr and allows to use type GenRegisters}


function CallDosVersion : Word; inline
   ($FF / $1E / AdrDosVersion); {call far dword ptr [AdrDosVersion]}
  {-Call DosVersion via AdrDosVersion}


procedure CallSetIntVec(IntNo : Byte; Vector : Pointer); inline
   ($FF / $1E / AdrSetIntVec); {call far dword ptr [AdrSetIntVec]}
  {-Call SetIntVec via AdrSetIntVec}


procedure CallGetIntVec(IntNo : Byte; var Vector : Pointer); inline
   ($FF / $1E / AdrGetIntVec); {call far dword ptr [AdrGetIntVec]}
  {-Call GetIntVec via AdrGetIntVec}


procedure CallGetTime(var Hour, Minute, Second, Sec100 : Word); inline
   ($FF / $1E / AdrGetTime); {call far dword ptr [AdrGetTime]}
  {-Call GetTime via AdrGetTime}


procedure CallGetDate(var Year, Month, Day, DayOfWeek : Word); inline
   ($FF / $1E / AdrGetDate); {call far dword ptr [AdrGetDate]}
  {-Call GetDate via AdrGetDate}


procedure DefaultRegisters(var Regs : GenRegisters);

{DefaultZeroedRegisters deleted}                          {!!.41}

function CallAllocRealModeMem(Size         : Word;  {!!.41}
                          var RealModePtr,
                              ProtModePtr  : Pointer) : Boolean;

procedure CallFreeRealModeMem(Size        : Word;       {!!.41}
                              ProtModePtr : Pointer);


implementation

{$IFDEF VER80}                                                  {!!.51}
uses DosSupp;                                                   {!!.51}
{$ENDIF}                                                        {!!.51}

procedure DefaultRegisters(var Regs : GenRegisters);
begin
  FillChar(Regs, SizeOf(Regs), 0);
  Regs.DS := DSeg;
  Regs.ES := DSeg;
end;


{$IFDEF DPMIOrWnd}                                             {!!.41}
procedure DPMIRealModeIntr(IntNr : Byte; var Regs : GenRegisters); far;
var
  DPMIRegs : record
    DI : LongInt;
    SI : LongInt;
    BP : LongInt;
    Reserved : LongInt;
    BX : LongInt;
    DX : LongInt;
    CX : LongInt;
    AX : LongInt;
    Flags : Word;
    ES : Word;
    DS : Word;
    FS : Word;
    GS : Word;
    IP : Word;
    CS : Word;
    SP : Word;
    SS : Word;
  end;
  DPMIError : Word;
begin
  DPMIError := 0;
  FillChar(DPMIRegs, SizeOf(DPMIRegs), 0);
  with DPMIRegs do begin
    AX := Regs.AX;
    BX := Regs.BX;
    CX := Regs.CX;
    DX := Regs.DX;
    BP := Regs.BP;
    SI := Regs.SI;
    DI := Regs.DI;
    DS := Regs.DS;
    ES := Regs.ES;
    Flags := Regs.Flags;
  end;
  asm
    MOV     AX, 0300H
    MOV     BL, IntNr
    MOV     BH, 0
    MOV     CX, 0
    PUSH    SS
    POP     ES
    LEA     DI, [BP+OFFSET DPMIRegs]
    INT     31H
    JNC     @@EndPoint
    MOV     DPMIError, AX
  @@EndPoint:
  end;
  with DPMIRegs do begin
    Regs.AX := AX;
    Regs.BX := BX;
    Regs.CX := CX;
    Regs.DX := DX;
    Regs.BP := BP;
    Regs.SI := SI;
    Regs.DI := DI;
    Regs.DS := DS;
    Regs.ES := ES;
    Regs.Flags := Flags;
  end;
end;
{$ENDIF}


function CallAllocRealModeMem(Size        : Word;              {!!.41}
                          var RealModePtr,
                              ProtModePtr : Pointer) : Boolean;
var
  L : LongInt;
begin
  CallAllocRealModeMem := False;
  {$IFDEF DPMIOrWnd}
  L := GlobalDosAlloc(Size);
  if L <> 0 then begin                                         {!!.51}
    RealModePtr := Ptr(L Shr 16, 0);
    ProtModePtr := Ptr(L and $FFFF, 0);
  end
  else begin
    Exit;
  end;
  {$ELSE}
  if MaxAvail < Size then Exit;
  GetMem(RealModePtr, Size);
  ProtModePtr := RealModePtr;
  {$ENDIF}
  CallAllocRealModeMem := True;
end;


procedure CallFreeRealModeMem(Size        : Word;              {!!.41}
                              ProtModePtr : Pointer);
begin
  {$IFDEF DPMIOrWnd}
  if GlobalDosFree(LongInt(ProtModePtr) Shr 16) = 0 then
    {do nothing};
  {$ELSE}
  FreeMem(ProtModePtr, Size);
  {$ENDIF}
end;


begin
  AdrMsDos := @MsDos;
  AdrIntr := @Intr;
  {$IFDEF DPMIOrWnd}                                           {!!.41}
  AdrDPMIRealModeIntr := @DPMIRealModeIntr;
  {$ELSE}
  AdrDPMIRealModeIntr := @Intr;
  {$ENDIF}
  AdrDosVersion := @DosVersion;
  AdrSetIntVec := @SetIntVec;
  AdrGetIntVec := @GetIntVec;
  AdrGetTime := @GetTime;
  AdrGetDate := @GetDate;
end.
