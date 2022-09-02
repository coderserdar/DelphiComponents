{********************************************************************}
{* DOSSUPP.PAS - DOS support routines for Delphi 1                  *}
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

{$IFNDEF VER80}
  !! Error - this unit can only be compiled by Delphi 1
{$ENDIF}

unit DosSupp;

interface

uses
  SysUtils,
  WinProcs,
  WinTypes;

type
  DOSRegisters = record
    case Integer of
      0: (AX, BX, CX, DX, BP, SI, DI, DS, ES, Flags: Word);
      1: (AL, AH, BL, BH, CL, CH, DL, DH: Byte);
  end;

procedure Intr(IntNo : Byte; var Regs: DOSRegisters);
procedure MsDos(var Regs: DOSRegisters);
function  DosVersion: Word;
procedure GetIntVec(IntNo: Byte; var Vector: Pointer);
procedure SetIntVec(IntNo: Byte; Vector: Pointer);
procedure GetDate(var Year, Month, Day, DayOfWeek: Word);
procedure GetTime(var Hour, Minute, Second, Sec100: Word);

implementation

procedure Intr(IntNo : Byte; var Regs: DOSRegisters); assembler;
asm
  push ds

  push cs                                                       {!!.52}
  call AllocCStoDSAlias                                         {!!.52}
  mov ds, ax
  lea di, @@IntrCall
  mov al, IntNo
  mov [di+1], al
  push ds                                                       {!!.52}

  lds si, Regs                     {set up the registers from the}
  mov ax, [si].DOSRegisters.&SI    {  DOSRegisters structure }
  push ax
  mov ax, [si].DOSRegisters.&DS
  or ax, ax
  jnz @@SetDS
  mov ax, ds

  @@SetDS:
  push ax
  mov di, [si].DOSRegisters.&DI
  mov bp, [si].DOSRegisters.&BP
  mov bx, [si].DOSRegisters.&BX
  mov dx, [si].DOSRegisters.&DX
  mov cx, [si].DOSRegisters.&CX
  mov ax, [si].DOSRegisters.&AX
  mov si, [si].DOSRegisters.&ES
  or si, si
  jnz @@SetES
  mov si, ds                                                    {!!.52}

  @@SetES:
  mov es, si
  pop ds
  pop si

  @@IntrCall:
  int 0                          {do the interrupt}

  push ds                        {get ready for setting the}
  pushf                          {  DOSRegisters structure}
  push bp
  push si
  mov bp, sp                     {restore our stack frame}
  {$IFOPT W+}                                                   {!!.52}
  add bp, 14                                                    {!!.52}
  {$ELSE}                                                       {!!.52}
  add bp, 12                                                    {!!.52}
  {$ENDIF}                                                      {!!.52}
  lds si, Regs                   {set up the DOSRegisters structure}
  mov [si].DOSRegisters.&DI, di
  mov [si].DOSRegisters.&BX, bx
  mov [si].DOSRegisters.&DX, dx
  mov [si].DOSRegisters.&CX, cx
  mov [si].DOSRegisters.&AX, ax
  mov [si].DOSRegisters.&ES, es
  pop ax
  mov [si].DOSRegisters.&SI, ax
  pop ax
  mov [si].DOSRegisters.&BP, ax
  pop ax
  mov [si].DOSRegisters.&Flags, ax
  pop ax
  mov [si].DOSRegisters.&DS, ax

  pop ds
  lea di, @@IntrCall
  xor al, al
  mov [di+1], al                                                {!!.52}

  pop ax                                                        {!!.52}
  push ds                                                       {!!.52}
  mov ds, ax                                                    {!!.52}
  call FreeSelector                                             {!!.52}
end;

procedure MsDos(var Regs: DOSRegisters);
begin
  Intr($21, Regs);
end;

function DosVersion: Word; assembler;
asm
  mov ax, $3000
  call Dos3Call
end;

procedure GetIntVec(IntNo: Byte; var Vector: Pointer); assembler;
asm
  mov ah, $35
  mov al, IntNo
  call Dos3Call
  mov ax, es
  les di, Vector
  mov es:[di], bx
  mov es:[di+2], ax
end;

procedure SetIntVec(IntNo: Byte; Vector: Pointer); assembler;
asm
  push ds
  mov ah, $25
  mov al, IntNo
  lds dx, Vector
  call Dos3Call
  pop ds
end;

procedure GetDate(var Year, Month, Day, DayOfWeek: Word);
begin
  DecodeDate(Date, Year, Month, Day);
  DayOfWeek := SysUtils.DayOfWeek(Date);
end;

procedure GetTime(var Hour, Minute, Second, Sec100: Word);
begin
  DecodeTime(Time, Hour, Minute, Second, Sec100);
end;

end.
