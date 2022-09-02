{$S-,R-,V-,I-,B-,F+}

{$IFNDEF Ver40}
  {$R-,A-}
{$ENDIF}

{$IFDEF DPMI}
  {$DEFINE PMODE}
{$ENDIF}
{$IFDEF Windows}
  {$DEFINE PMODE}
{$ENDIF}

{$IFDEF PMODE}
  {$G+}
{$ELSE}
  {$O-}
{$ENDIF}

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

unit Dpmi;       {primitive routines for DPMI management}

interface

{-The following consts are used throughout Object Professional.  Your code
  is free to reference them, but they must *not* be changed.}
const
  DpmiInUse : Boolean = False;        {True if running in protected mode}
  ColorSele : Word = $B800;           {selector/segment for color video}
  MonoSele  : Word = $B000;           {selector/segment for mono video}
  BiosDataSele : Word = $0040;        {selector/segment for bios data area}
  BiosSele : Word = $F000;            {selector/segment for bios memory}


{$IFDEF PMODE}
type
  {.Z+}
  DoubleWord = record
                 LoWord  : Word;
                 HiWord  : Word;
               end;

  DPMIRegisters =                        {!!.31, added AL, AH, ..., DH}
    record
      DI : LongInt;
      SI : LongInt;
      BP : LongInt;
      Reserved : LongInt;
      case integer of
        1 : (BX : LongInt;
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
             SS : Word);
        2 : (BL, BH : Byte; EBXH : Word;
             DL, DH : Byte; EDXH : Word;
             CL, CH : Byte; ECXH : Word;
             AL, AH : Byte; EAXH : Word);
    end;

  MemInfoRec =
    record
      LargestFreeBlock   : LongInt;
      MaxUnlockedPages   : LongInt;
      MaxLockedPages     : LongInt;
      LinearAddrPages    : LongInt;
      TotalUnlockedPages : LongInt;
      TotalFreePages     : LongInt;
      TotalPhysicalPages : LongInt;
      FreeLinearPages    : LongInt;
      PageSize           : LongInt;
      Reserved           : Array[1..$C] of Byte;
    end;

  DPMIInfoRec =
    record {Information returned by GetDPMIInfo routine}
      MinorVersion     : Byte;
      MajorVersion     : Byte;
      Flags            : Word;
      SlavePICInt      : Byte;
      MasterPICInt     : Byte;
      Processor        : Byte;
    end;

type
  DescriptorTableEntry =
    record
      LimitL : Word;
      BaseL  : Word;
      Words : Array[0..1] of Word;
    end;
  {.Z-}

function Linear(P : Pointer) : LongInt;
  {-Converts a pointer to a linear address to allow differences in addresses
    to be calculated. The pointer must be in the range $0:$0 to $FFFF:$000F.}

function UnLinear(L : LongInt) : Pointer;
  {-Converts a linear address to a pointer to allow selector base addresses to
    be converted to pointers. The longInt must be in the range $0 to $000FFFFF.}

function ValidPointer(P : Pointer) : Boolean; {!!.22 new}
  {-verify a pmode pointer is valid, by verifying the access rights and limit
    on the selector and verifying the selector is write-able.  This only checks
    the specific pointer address; if you will be accessing a range from the
    pointer you should validate the maximum possible offset for the pointer
    as well.}

function AllocLDTDescriptors(NumOfDesc : Word; var BaseSelector : Word) : Word;
  {-Allocates one or more descriptors in the task's Local Descriptor Table
    (LDT). The descriptor is not initialized; this must be done with calls to
    SetSegmentBaseAddr and SetSegmentLimit. The allocated descriptor will be
    set to "data" with a priviledge level equal to the application's code
    segment priviledge level. If requesting more than one descriptor, the
    BaseSelector will be set to the first of a contiguous array of
    descriptors. The Selector values for subsequent descriptors in the array
    must be calculated by adding the value returned by GetSelectorIncrement.}

function GetSelectorIncrement : Word;
  {-gets the selector increment value}

function SetSegmentBaseAddr(Selector : Word; BaseAddress : LongInt) : Word;
  {-Sets the base (starting) address for Selector}

function SetSegmentLimit(Selector : Word; Limit : LongInt) : Word;
  {-Sets the limit (length) for Selector}

function GetSegmentBaseAddr(Selector : Word; var BaseAddress: LongInt) : Word;
  {-Gets the base (starting) address for Selector}

function GetSegmentLimit(Selector : Word; var Limit : LongInt) : Word;
  {-Gets the limit (length) for Selector}

function FreeLDTDescriptor(Selector : Word) : Word;
  {-Deallocates Selector}

function GetSelectorForRealMem(RealPtr : Pointer; Limit : LongInt; var Selector : Word) : Word;
  {-Allocates Selector of Size bytes in Real memory, starting at RealPtr}

function GetDescriptor(Selector : Word;
                       var Descriptor : DescriptorTableEntry) : Word;
  {-Gets the Descriptor Table information on Selector, returns 0 if successful}

function CallFarRealModeProc(StackWords : Word; StackData : Pointer;
                             var Regs : DPMIRegisters) : Word;
  {-Simulates a FAR CALL to a real mode procedure.}

function SimulateRealModeInt(IntNo : Byte;
                             var Regs : DPMIRegisters) : Word;
  {-Simulates an interrupt in real mode. Control is transferred to the
    address specified by the real mode interrupt vector.}

procedure GetRealModeIntVector(IntNo : Byte; var Vector : Pointer);
  {-Returns the contents of the current virtual machine's real mode interrupt
    vector number for IntNo. Note, the returned address is a real mode
    segment:offset.}

procedure SetRealModeIntVector(IntNo : Byte; Vector : Pointer);
  {-Set the current virtual machine's real mode interrupt vector for
    vector IntNo. Vector must be a real mode segment:offset.}

function AllocRealModeCallbackAddr(CallbackProc : Pointer;
                                   var Regs : DPMIRegisters;
                                   var Callback : Pointer) : Word;
  {-Allocates a unique real mode segment:offset that will transfer control
    from real mode to a protected mode procedure.}

function FreeRealModeCallbackAddr(Callback : Pointer) : Word;
  {-Frees a real mode callback previously allocated with
    AllocateRealModeCallbackAddr.}

procedure GetProtectedModeInt(IntNo : Byte; var Handler : Pointer);
  {-Returns the address of the current protected mode interrupt handler for
    IntNo.}

function SetProtectedModeInt(IntNo : Byte; Handler : Pointer) : Word;
  {-Sets the address of the protected mode handler for IntNo.}

procedure GetDPMIMemInfo(var MemInfo : MemInfoRec);
  {-Returns information about the amount of available physical memory, linear
    address space, and disk space for page swapping. See the MemInfoRec
    declared above for information on the returned values. Only the first
    field of the MemInfoRec is guantanteed to be valid. All invalid fields
    will be set to -1.}

{$ENDIF}

implementation


{$IFDEF PMODE}
type
  OS =
    record
      O, S : Word;
    end;

var
  DpmiPrimExitPtr : Pointer;

  function Linear(P : Pointer) : LongInt;
    {-Converts a pointer to a linear address to allow differences in addresses
      to be calculated. The pointer must be in the range $0:$0 to $FFFF:$000F.}
  begin
    with OS(P) do
      Linear := (LongInt(S) shl 4)+LongInt(O);
  end;

  function UnLinear(L : LongInt) : Pointer;
    {-Converts a linear address to a pointer allow selector base addresses to
      be converted to pointers. The longInt must be in the range $0 to $000FFFFF.}
  begin
    UnLinear := Ptr(Word(L shr 4), Word(L and $000F));  {!!.21}
  end;

  function ValidPointer(P : Pointer) : Boolean; Assembler;  {!!.22 new}
    {-verify a pmode pointer is valid}
  asm
    push   dx;               {preserve dx}
    push   bx;               {preserve bx}
    xor    ax,ax;            {assume failure}
    mov    dx,[bp+8];        {get selector portion of pointer}
    lar    bx,dx;            {get Access Rights byte}
    jnz    @@Out;            {bad selector, get out}
    lsl    bx,dx;            {get Selector Limit}
    jnz    @@Out;            {bad selector, get out}
    cmp    bx,[bp+6];        {compare selector limit to offset of pointer}
    jb     @@Out;            {limit is less, get out}
    verw   dx;               {is selector writable?}
    jnz    @@Out;            {nope, get out}
    mov    al,1;             {return True}
@@Out:
    pop    bx;               {restore bx}
    pop    dx;               {restore dx}
  end;

  function CallFarRealModeProc(StackWords : Word; StackData : Pointer;
                               var Regs : DPMIRegisters) : Word; Assembler;
  asm
    push    ds;
    mov     cx,StackWords;
    jcxz    @@NoParams;
    lds     si,StackData;
    mov     ax,cx;
    dec     ax;
    shl     ax,1;
    add     si,ax;
    std;
  @@ParamLoop:
    lodsw;
    push    ax;
    loop    @@ParamLoop;
  @@NoParams:
    cld;
    xor     bx,bx;
    mov     cx,StackWords;
    les     di,Regs;
    mov     ax,0301h;
    int     31h;
    jc      @@ExitPoint;
    xor     ax,ax;
  @@ExitPoint:
    mov     bx,StackWords;
    shl     bx,1;
    add     sp,bx;
    pop     ds;
  end;

  function SimulateRealModeInt(IntNo : Byte;
                               var Regs : DPMIRegisters) : Word; Assembler;
  asm
    xor     bx,bx;
    mov     bl,IntNo;
    xor     cx,cx;       {StackWords = 0}
    les     di,Regs;
    mov     ax,0300h;
    int     31h;
    jc      @@ExitPoint;
    xor     ax,ax;
  @@ExitPoint:
  end;

  procedure GetRealModeIntVector(IntNo : Byte; var Vector : Pointer); Assembler;
  asm
    mov     ax,0200h;
    mov     bl,IntNo;
    int     31h;
    les     di,Vector;
    mov     word ptr es:[di],dx;
    mov     word ptr es:[di+2],cx;
  end;

  procedure SetRealModeIntVector(IntNo : Byte; Vector : Pointer); assembler;
  asm
    mov     ax,$0201;
    mov     bl,IntNo;
    mov     dx,word ptr Vector;
    mov     cx,word ptr Vector+2;
    int     $31;
  end;

  function GetCPUFlags : Byte; Assembler;
  asm
    lahf;
    mov     al,ah;
  end;

  {Doesn't work under Windows 3.1. Don't use in Windows!}
  function AllocDosMem(SizeInParas : Word;
                       var RealModeSeg : Word;
                       var ProtModeSel : Word) : Word; Assembler;
  asm
    mov     bx,SizeInParas;
    mov     ax,0100h;
    int     31h;
    jc      @@ExitPoint;
    les     di,RealModeSeg;
    mov     es:[di],ax;
    les     di,ProtModeSel;
    mov     es:[di],dx;
    xor     ax,ax;
  @@ExitPoint:
  end;

  {Doesn't work under Windows 3.1. Don't use in Windows!}
  function FreeDosMem(ProtModeSel : Word) : Word; Assembler;
  asm
    mov     ax,0101h;
    mov     dx,ProtModeSel;
    int     31h;
    jc      @@ExitPoint;
    xor     ax,ax;
  @@ExitPoint:
  end;

  function AllocLDTDescriptors(NumOfDesc : Word; var BaseSelector : Word) : Word; Assembler;
  asm
    mov     cx,NumOfDesc;
    xor     ax,ax;
    int     31h;
    jc      @@ErrorExitPoint;        {!!.31}
    les     di,BaseSelector;
    mov     es:[di],ax;
    xor     ax,ax;
    jmp     @@ExitPoint              {!!.31}
  @@ErrorExitPoint:                  {!!.31}
    mov     ax,1                     {!!.31}
  @@ExitPoint:
  end;

  function SetSegmentBaseAddr(Selector : Word; BaseAddress : LongInt) : Word; Assembler;
  asm
    mov     bx,Selector;
    mov     dx,word ptr BaseAddress;
    mov     cx,word ptr BaseAddress+2;
    mov     ax,0007h;
    int     31h;
    jc      @@ExitPoint;
    xor     ax,ax;
  @@ExitPoint:
  end;

  function GetSegmentAccessRights(Selector : Word; var Rights : Word) : Word;
  var
    Status : Word;
    Descriptor : DescriptorTableEntry;
  begin
    Status := GetDescriptor(Selector, Descriptor);
    if Status = 0 then
      with Descriptor do
        Rights := (Words[0] shr 8) or ((Words[1] and $00F0) shl 8);
    GetSegmentAccessRights := Status;
  end;

  function SetRightsPrim(Selector : Word; Rights : Word) : Word; Assembler;
    {-Primitive rights change}
  asm
    mov     bx,Selector;
    mov     cx,Rights;
    mov     ax,0009h;
    int     31h;
    jc      @@ExitPoint;
    xor     ax,ax;
  @@ExitPoint:
  end;

  function SetSegmentAccessRights(Selector : Word;
                                  ReadWrite : WordBool; Code : WordBool) : Word;
  var
    Rights : Word;
    Status : Word;
  begin
    Status := GetSegmentAccessRights(Selector, Rights);
    if Status <> 0 then begin
      SetSegmentAccessRights := Status;
      Exit;
    end;

    {Modify the Rights mask according to parameters}
    if Code then begin
      ReadWrite := True; {For code, means segment can be read as well as executed}
      Rights := Rights and not $0004; {Code is always expand-up}
      Rights := Rights or $0008;      {Set Code bit}
    end else
      Rights := Rights and not $0008; {Clear Code bit}
    if ReadWrite then
      Rights := Rights or $0002       {Set ReadWrite bit}
    else
      Rights := Rights and not $0002; {Clear ReadWrite bit}

    {Change the rights}
    SetSegmentAccessRights := SetRightsPrim(Selector, Rights);
  end;

  function GetSegmentLimit(Selector : Word; var Limit : LongInt) : Word;
  var
    Status : Word;
    Descriptor : DescriptorTableEntry;
  begin
    Status := GetDescriptor(Selector, Descriptor);
    if Status = 0 then
      with Descriptor do begin
        Limit := LongInt(LimitL) or (LongInt(Words[1] and $0F) shl 16);
        {Account for granularity}
        if Words[1] and $80 <> 0 then
          Limit := Limit*4096;
      end;
    GetSegmentLimit := Status;
  end;

  function GetSegmentBaseAddr(Selector : Word; var BaseAddress : LongInt) : Word; Assembler;
  asm
    mov     bx,Selector;
    mov     ax,0006h;
    int     31h;
    jc      @@ExitPoint;
    xor     ax,ax;
    les     di,BaseAddress;
    mov     es:[di],dx;
    mov     es:[di+2],cx;
  @@ExitPoint:
  end;

  function SetLimitPrim(Selector : Word; Limit : LongInt) : Word; Assembler;
    {-Primitive limit change}
  asm
    mov     bx,Selector;
    mov     dx,word ptr Limit;
    mov     cx,word ptr Limit+2;
    mov     ax,0008h;
    int     31h;
    jc      @@ExitPoint;
    xor     ax,ax;
  @@ExitPoint:
  end;

  function SetSegmentLimit(Selector : Word; Limit : LongInt) : Word;
  var
    Rights : Word;
    Status : Word;
  begin
    {Handle limit granularity}
    Status := GetSegmentAccessRights(Selector, Rights);
    if Status <> 0 then begin
      SetSegmentLimit := Status;
      Exit;
    end;
    if Limit > $FFFFF then begin
      {Segment larger than 1MB}
      if Limit and $FFF <> $FFF then begin
        {Not page aligned}
        SetSegmentLimit := $8021;
        Exit;
      end;
      Rights := Rights or $8000;       {Page-granular}
    end else
      Rights := Rights and not $8000;  {Byte-granular}

    {Assure no overflow when granularity changed}
    Status := SetLimitPrim(Selector, 0);
    if Status = 0 then
      Status := SetRightsPrim(Selector, Rights);
    if Status = 0 then
      SetSegmentLimit := SetLimitPrim(Selector, Limit);
    SetSegmentLimit := Status;
  end;

  function FreeLDTDescriptor(Selector : Word) : Word; Assembler;
  asm
    mov     bx,Selector;
    mov     ax,0001h;
    int     31h;
    jc      @@ExitPoint;
    xor     ax,ax;
  @@ExitPoint:
  end;

  function GetSelectorIncrement : Word; Assembler;
  asm
    mov     ax,0003h;
    int     31h;
  end;

  function GetSelectorForRealMem(RealPtr : Pointer; Limit : LongInt; var Selector : Word) : Word;

    procedure FreeSele;
    begin
      FreeLDTDescriptor(Selector);
    end;

  var
    ErrorCode : Word;

  begin
    ErrorCode := AllocLDTDescriptors(1, Selector);
    if ErrorCode = 0 then begin
      ErrorCode := SetSegmentBaseAddr(Selector, Linear(RealPtr));
      if ErrorCode = 0 then begin
        ErrorCode := SetSegmentLimit(Selector, Limit);
        if ErrorCode <> 0 then
          FreeSele;
      end
      else
        FreeSele;
    end;
    GetSelectorForRealMem := ErrorCode;
  end;

  function AllocRealModeCallbackAddr(CallbackProc : Pointer;
                                     var Regs : DPMIRegisters;
                                     var Callback : Pointer) : Word; Assembler;
  asm
    push    ds;
    lds     si,CallbackProc;
    les     di,Regs;
    mov     ax,0303h;
    int     31h;
    jnc     @@Exitpoint;
    xor     cx,cx;
    xor     dx,dx;
    jmp     @@ExitPoint2;
  @@ExitPoint:
    xor     ax,ax;
  @@ExitPoint2:
    les     di,Callback;
    mov     word ptr es:[di],dx;
    mov     word ptr es:[di+2],cx;
    pop     ds;
  end;

  function FreeRealModeCallbackAddr(Callback : Pointer) : Word; Assembler;
  asm
    mov     cx,word ptr Callback+2;
    mov     dx,word ptr Callback;
    mov     ax,0304h;
    int     31h;
    jc      @@ExitPoint;
    xor     ax,ax;
  @@ExitPoint:
  end;

  procedure GetProtectedModeInt(IntNo : Byte; var Handler : Pointer); Assembler;
  asm
    mov     ax,0204h;
    mov     bl,IntNo;
    int     31h;
    les     di,Handler;
    mov     word ptr es:[di],dx;
    mov     word ptr es:[di+2],cx;
  end;

  function SetProtectedModeInt(IntNo : Byte; Handler : Pointer) : Word; Assembler;
  asm
    mov     bl,IntNo;
    mov     dx,word ptr Handler;
    mov     cx,word ptr Handler+2;
    mov     ax,0205h;
    int     31h;
    jc      @@ExitPoint;
    xor     ax,ax;
  @@ExitPoint:
  end;

  function GetExceptionHandler(ExceptionNum : Byte;
                               var Handler : Pointer) : Word; Assembler;
  asm
    mov     bl,ExceptionNum;
    mov     ax,0202h;
    int     31h;
    jc      @@ExitPoint;
    xor     ax,ax;
    les     di,Handler;
    mov     word ptr es:[di],dx;
    mov     word ptr es:[di+2],cx;
  @@ExitPoint:
  end;

  function SetExceptionHandler(ExceptionNum : Byte;
                               Handler : Pointer) : Word; Assembler;
  asm
    mov     bl,ExceptionNum;
    mov     ax,0203h;
    mov     dx,word ptr Handler;
    mov     cx,word ptr Handler+2;
    int     31h;
    jc      @@ExitPoint;
    xor     ax,ax;
  @@ExitPoint:
  end;


  procedure GetDPMIMemInfo(var MemInfo : MemInfoRec); Assembler;
  const
    SizeOfMemInfoRec = SizeOf(MemInfoRec);
  asm
    les     di,MemInfo;
    mov     si,di;
    mov     cx,SizeOfMemInfoRec;
    mov     al,0FFh;
    cld;
    rep     stosb;          {set record to -1 in case DPMI doesn't}
    mov     di,si;
    mov     ax,0500h;       {get free memory info}
    int     31h;            {this function doesn't fail}
    mov     ax,0604h;       {get page size}
    int     31h;
    jc      @@ExitPoint;    {not supported by 16-bit hosts}
    lea     di,MemInfoRec(es:[si]).PageSize;
    cld;
    mov     ax,cx;
    stosw;
    mov     ax,bx;
    stosw;
  @@ExitPoint:
  end;

  procedure GetDPMIInfo(var DPMIInfo : DPMIInfoRec); Assembler;
  asm
    mov     ax,0400h;
    int     31h;            {this function doesn't fail}
    les     di,DPMIInfo;
    cld;
    stosw;                  {store minor and major version numbers}
    mov     ax,bx;
    stosw;                  {store Flags}
    mov     ax,dx;
    stosw;                  {store PIC base interrupt numbers}
    mov     al,cl;
    stosb;                  {store processor type}
  end;

  function GetPageSize(var PageSize : LongInt) : Word; Assembler;
  asm
    mov     ax,0604h;
    int     31h;
    jc      @@ExitPoint;
    les     di,PageSize;
    mov     es:[di],cx;
    mov     es:[di+2],bx;
    xor     ax,ax;
  @@ExitPoint:
  end;

  function GetDescriptor(Selector : Word;
                         var Descriptor : DescriptorTableEntry) : Word; Assembler;
  asm
    mov     ax,000Bh;
    mov     bx,Selector;
    les     di,Descriptor;
    int     31h;
    jc      @@ExitPoint;
    xor     ax,ax;
  @@ExitPoint:
  end;

  procedure DpmiPrimExitProc;
    {-Our exit handler for this unit}
  begin
    ExitProc := DpmiPrimExitPtr;

    {free our BiosSele selector}
    if (BiosSele <> 0) then
      FreeLDTDescriptor(BiosSele);

    {$IFDEF Windows}
    {free our BiosDataSele selector}
    if (BiosDataSele <> 0) then
      FreeLDTDescriptor(BiosDataSele);
    {$ENDIF}
  end;

var
  W : Word;

begin
  BiosSele := 0;
  {$IFDEF DPMI}
  ColorSele    := SegB800;
  MonoSele     := SegB000;
  BiosDataSele := Seg0040;
  {$ELSE}
  ColorSele    := 0;
  MonoSele     := 0;
  BiosDataSele := 0;
  {$ENDIF}
  DpmiInUse    := True;

  {set up an exit handler to release our selectors}
  DpmiPrimExitPtr := ExitProc;
  ExitProc := @DpmiPrimExitProc;

  {since the RTL doesn't provide an important predefined selector, we get one}
  W := GetSelectorForRealMem(Ptr($F000, 0), $FFFF, BiosSele);
  if W <> 0 then
    {failed; generate Runtime Error 203 (out of heap)}
    RunError(203);

  {$IFDEF Windows}
  {since the Windows RTL doesn't provide another important predefined
   selector, we get one}
  W := GetSelectorForRealMem(Ptr($0040, 0), $FFFF, BiosDataSele);
  if W <> 0 then
    {failed; generate Runtime Error 203 (out of heap)}
    RunError(203);
  {$ENDIF}

{$ENDIF}
end.

