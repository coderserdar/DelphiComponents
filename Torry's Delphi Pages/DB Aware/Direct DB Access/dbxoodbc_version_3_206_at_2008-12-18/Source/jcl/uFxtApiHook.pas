unit uFxtApiHook;

// http://wasm.ru/article.php?article=apihook_2
// http://wasm.ru/pub/21/files/advapihook.rar

{$I FxtVer.inc}
{$B-,O-}

interface

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF MSWINDOWS}
  SysUtils;

function SizeOfCode(Code: Pointer): DWord;
function SizeOfProc(Proc: Pointer): DWord;
function SaveOldFunction(Proc: Pointer; Old: Pointer): DWord;
function HookCode(TargetProc, NewProc: Pointer; var OldProc: Pointer): Boolean;
function UnhookCode(OldProc: Pointer): Boolean;
function HookProc(lpModuleName, lpProcName: PChar; NewProc: Pointer; var OldProc: Pointer): Boolean;

{+}

var
  IsMemoryCheck: Boolean;

function FinalizeHooks(): Boolean;

// @@@ dbxoodbc @@@ : http://open-dbexpress.sourceforge.net/
{--------------------------------------------------------------------------------------------------------}
// {begin cut : dbxoodbc: SqlExprFix.pas}
// *********************** RUNTIME PATCH ***********************

type
  PJumpInstruction = ^TJumpInstruction;
  TJumpInstruction = packed record
    case Byte of
      1: (
        Code: Byte; // jump instruction ($FF)
        Offset: DWord; // jump offset
        UnUsed: Byte;
        );
      2: (
        DW1: DWord;
        DW2: DWord;
        );
  end;

  PPointer = ^Pointer;
  PPPointer = ^PPointer;
  PWORD = ^Word;
  PDWORD = ^DWord;

function GetNativeAddr(Ptr: Pointer): Pointer;
function JumpFromCode(MSrc, MDest: Pointer; SaveJump: PJumpInstruction): Boolean;
function RestoreSavedCode(MSrc: Pointer; SaveJump: PJumpInstruction): Boolean;
function IsJumpFromCode(MSrc, MDest: Pointer): Boolean;

// {end cut : dbxoodbc: SqlExprFix.pas}
{--------------------------------------------------------------------------------------------------------}

function FxtGetStubAddress(AStub: Pointer; OffsetE8: Integer = 0): Pointer;

{ VMT low-level utilities }

{ Returns the address of virtual method of AClass with index AIndex }
function GetVirtualMethodAddress(AClass: TClass; AIndex: Integer): Pointer;
{ Updates VMT of AClass and sets the new method address of method with index AIndex }
function SetVirtualMethodAddress(AClass: TClass; AIndex: Integer;
  NewAddress: Pointer): Pointer;
{ Iterates through VMT of AClass and seeks for method MethodAddr }
function FindVirtualMethodIndex(AClass: TClass; MethodAddr: Pointer): Integer;
{ calculate virtualmethod offset }
function GetVirtualMethodOffset(AClass: TClass; MethodAddr: Pointer): Integer;

{+.}

implementation

function FinalizeHooks(): Boolean;
begin
  Result := IsLibrary or IsMemoryCheck;
end;

const
  Opcodes1: array[0..255] of Word =
  (
    $4211, $42E4, $2011, $20E4, $8401, $8C42, $0000, $0000, $4211, $42E4,
    $2011, $20E4, $8401, $8C42, $0000, $0000, $4211, $42E4, $2011, $20E4,

    $8401, $8C42, $0000, $0000, $4211, $42E4, $2011, $20E4, $8401, $8C42,

    $0000, $0000, $4211, $42E4, $2011, $20E4, $8401, $8C42, $0000, $8000,

    $4211, $42E4, $2011, $20E4, $8401, $8C42, $0000, $8000, $4211, $42E4,

    $2011, $20E4, $8401, $8C42, $0000, $8000, $0211, $02E4, $0011, $00E4,

    $0401, $0C42, $0000, $8000, $6045, $6045, $6045, $6045, $6045, $6045,

    $6045, $6045, $6045, $6045, $6045, $6045, $6045, $6045, $6045, $6045,

    $0045, $0045, $0045, $0045, $0045, $0045, $0045, $0045, $6045, $6045,

    $6045, $6045, $6045, $6045, $6045, $6045, $0000, $8000, $00E4, $421A,

    $0000, $0000, $0000, $0000, $0C00, $2CE4, $0400, $24E4, $0000, $0000,

    $0000, $0000, $1400, $1400, $1400, $1400, $1400, $1400, $1400, $1400,

    $1400, $1400, $1400, $1400, $1400, $1400, $1400, $1400, $0510, $0DA0,

    $0510, $05A0, $0211, $02E4, $A211, $A2E4, $4211, $42E4, $2011, $20E4,

    $42E3, $20E4, $00E3, $01A0, $0000, $E046, $E046, $E046, $E046, $E046,

    $E046, $E046, $8000, $0000, $0000, $0000, $0000, $0000, $0000, $8000,

    $8101, $8142, $0301, $0342, $0000, $0000, $0000, $0000, $0401, $0C42,

    $0000, $0000, $8000, $8000, $0000, $0000, $6404, $6404, $6404, $6404,

    $6404, $6404, $6404, $6404, $6C45, $6C45, $6C45, $6C45, $6C45, $6C45,

    $6C45, $6C45, $4510, $45A0, $0800, $0000, $20E4, $20E4, $4510, $4DA0,

    $0000, $0000, $0800, $0000, $0000, $0400, $0000, $0000, $4110, $41A0,

    $4110, $41A0, $8400, $8400, $0000, $8000, $0008, $0008, $0008, $0008,

    $0008, $0008, $0008, $0008, $1400, $1400, $1400, $1400, $8401, $8442,

    $0601, $0642, $1C00, $1C00, $0000, $1400, $8007, $8047, $0207, $0247,

    $0000, $0000, $0000, $0000, $0000, $0000, $0008, $0008, $0000, $0000,

    $0000, $0000, $0000, $0000, $4110, $01A0
    );

  Opcodes2: array[0..255] of Word =
  (
    $0118, $0120, $20E4, $20E4, $FFFF, $0000, $0000, $0000, $0000, $0000,
    $FFFF, $FFFF, $FFFF, $0110, $0000, $052D, $003F, $023F, $003F, $023F,

    $003F, $003F, $003F, $023F, $0110, $FFFF, $FFFF, $FFFF, $FFFF, $FFFF,

    $FFFF, $FFFF, $4023, $4023, $0223, $0223, $FFFF, $FFFF, $FFFF, $FFFF,

    $003F, $023F, $002F, $023F, $003D, $003D, $003F, $003F, $0000, $8000,

    $8000, $8000, $0000, $0000, $FFFF, $FFFF, $FFFF, $FFFF, $FFFF, $FFFF,

    $FFFF, $FFFF, $FFFF, $FFFF, $20E4, $20E4, $20E4, $20E4, $20E4, $20E4,

    $20E4, $20E4, $20E4, $20E4, $20E4, $20E4, $20E4, $20E4, $20E4, $20E4,

    $4227, $003F, $003F, $003F, $003F, $003F, $003F, $003F, $003F, $003F,

    $003F, $003F, $003F, $003F, $003F, $003F, $00ED, $00ED, $00ED, $00ED,

    $00ED, $00ED, $00ED, $00ED, $00ED, $00ED, $00ED, $00ED, $00ED, $00ED,

    $0065, $00ED, $04ED, $04A8, $04A8, $04A8, $00ED, $00ED, $00ED, $0000,

    $FFFF, $FFFF, $FFFF, $FFFF, $FFFF, $FFFF, $0265, $02ED, $1C00, $1C00,

    $1C00, $1C00, $1C00, $1C00, $1C00, $1C00, $1C00, $1C00, $1C00, $1C00,

    $1C00, $1C00, $1C00, $1C00, $4110, $4110, $4110, $4110, $4110, $4110,

    $4110, $4110, $4110, $4110, $4110, $4110, $4110, $4110, $4110, $4110,

    $0000, $0000, $8000, $02E4, $47E4, $43E4, $C211, $C2E4, $0000, $0000,

    $0000, $42E4, $47E4, $43E4, $0020, $20E4, $C211, $C2E4, $20E4, $42E4,

    $20E4, $22E4, $2154, $211C, $FFFF, $FFFF, $05A0, $42E4, $20E4, $20E4,

    $2154, $211C, $A211, $A2E4, $043F, $0224, $0465, $24AC, $043F, $8128,

    $6005, $6005, $6005, $6005, $6005, $6005, $6005, $6005, $FFFF, $00ED,

    $00ED, $00ED, $00ED, $00ED, $02ED, $20AC, $00ED, $00ED, $00ED, $00ED,

    $00ED, $00ED, $00ED, $00ED, $00ED, $00ED, $00ED, $00ED, $00ED, $00ED,

    $003F, $02ED, $00ED, $00ED, $00ED, $00ED, $00ED, $00ED, $00ED, $00ED,

    $FFFF, $00ED, $00ED, $00ED, $00ED, $00ED, $00ED, $00ED, $00ED, $00ED,

    $00ED, $00ED, $00ED, $00ED, $00ED, $0000
    );

  Opcodes3: array[0..9] of array[0..15] of Word =
  (
    ($0510, $FFFF, $4110, $4110, $8110, $8110, $8110, $8110, $0510, $FFFF,
    $4110, $4110, $8110, $8110, $8110, $8110),
    ($0DA0, $FFFF, $41A0, $41A0, $81A0, $81A0, $81A0, $81A0, $0DA0, $FFFF,
    $41A0, $41A0, $81A0, $81A0, $81A0, $81A0),
    ($0120, $0120, $0120, $0120, $0120, $0120, $0120, $0120, $0036, $0036,
    $0030, $0030, $0036, $0036, $0036, $0036),
    ($0120, $FFFF, $0120, $0120, $0110, $0118, $0110, $0118, $0030, $0030,
    $0000, $0030, $0000, $0000, $0000, $0000),
    ($0120, $0120, $0120, $0120, $0120, $0120, $0120, $0120, $0036, $0036,
    $0036, $0036, $FFFF, $0000, $FFFF, $FFFF),
    ($0120, $FFFF, $0120, $0120, $FFFF, $0130, $FFFF, $0130, $0036, $0036,
    $0036, $0036, $0000, $0036, $0036, $0000),
    ($0128, $0128, $0128, $0128, $0128, $0128, $0128, $0128, $0236, $0236,
    $0030, $0030, $0236, $0236, $0236, $0236),
    ($0128, $FFFF, $0128, $0128, $0110, $FFFF, $0110, $0118, $0030, $0030,
    $0030, $0030, $0030, $0030, $FFFF, $FFFF),
    ($0118, $0118, $0118, $0118, $0118, $0118, $0118, $0118, $0236, $0236,
    $0030, $0236, $0236, $0236, $0236, $0236),
    ($0118, $FFFF, $0118, $0118, $0130, $0128, $0130, $0128, $0030, $0030,
    $0030, $0030, $0000, $0036, $0036, $FFFF)
    );

function SizeOfCode(Code: Pointer): DWord;
var
  Opcode: Word;
  Modrm: Byte;
  Fixed, AddressOveride: Boolean;
  Last, OperandOveride, Flags, Rm, Size, Extend: DWord;
begin
  try
    Last := DWord(Code);
    if Code <> nil then
    begin
      AddressOveride := False;
      Fixed := False;
      OperandOveride := 4;
      Extend := 0;
      repeat
        Opcode := Byte(Code^);
        Code := Pointer(DWord(Code) + 1);
        if Opcode = $66 then
          OperandOveride := 2
        else if Opcode = $67 then
          AddressOveride := True
        else if not ((Opcode and $E7) = $26) then
          if not (Opcode in [$64..$65]) then
            Fixed := True;
      until Fixed;
      if Opcode = $0F then
      begin
        Opcode := Byte(Code^);
        Flags := Opcodes2[Opcode];
        Opcode := Opcode + $0F00;
        Code := Pointer(DWord(Code) + 1);
      end
      else
        Flags := Opcodes1[Opcode];

      if ((Flags and $0038) <> 0) then
      begin
        Modrm := Byte(Code^);
        Rm := Modrm and $7;
        Code := Pointer(DWord(Code) + 1);

        case (Modrm and $C0) of
          $40: Size := 1;
          $80: if AddressOveride then
              Size := 2
            else
              Size := 4;
        else
          Size := 0;
        end;

        if not (((Modrm and $C0) <> $C0) and AddressOveride) then
        begin
          if (Rm = 4) and ((Modrm and $C0) <> $C0) then
            Rm := Byte(Code^) and $7;
          if ((Modrm and $C0 = 0) and (Rm = 5)) then
            Size := 4;
          Code := Pointer(DWord(Code) + Size);
        end;

        if ((Flags and $0038) = $0008) then
        begin
          case Opcode of
            $F6: Extend := 0;
            $F7: Extend := 1;
            $D8: Extend := 2;
            $D9: Extend := 3;
            $DA: Extend := 4;
            $DB: Extend := 5;
            $DC: Extend := 6;
            $DD: Extend := 7;
            $DE: Extend := 8;
            $DF: Extend := 9;
          end;
          if ((Modrm and $C0) <> $C0) then
            Flags := Opcodes3[Extend][(Modrm shr 3) and $7]
          else
            Flags := Opcodes3[Extend][((Modrm shr 3) and $7) + 8];
        end;

      end;
      case (Flags and $0C00) of
        $0400: Code := Pointer(DWord(Code) + 1);
        $0800: Code := Pointer(DWord(Code) + 2);
        $0C00: Code := Pointer(DWord(Code) + OperandOveride);
      else
        begin
          case Opcode of
            $9A, $EA: Code := Pointer(DWord(Code) + OperandOveride + 2);
            $C8: Code := Pointer(DWord(Code) + 3);
            $A0..$A3:
              begin
                if AddressOveride then
                  Code := Pointer(DWord(Code) + 2)
                else
                  Code := Pointer(DWord(Code) + 4);
              end;
          end;
        end;
      end;
    end;
    Result := DWord(Code) - Last;
  except
    Result := 0;
  end;
end;

function SizeOfProc(Proc: Pointer): DWord;
var
  Length: DWord;
begin
  Result := 0;
  repeat
    Length := SizeOfCode(Proc);
    Inc(Result, Length);
    if ((Length = 1) and (Byte(Proc^) = $C3)) then
      Break;
    Proc := Pointer(DWord(Proc) + Length);
  until Length = 0;
end;

function SaveOldFunction(Proc: Pointer; Old: Pointer): DWord;
var
  SaveSize, Size: DWord;
  Next: Pointer;
begin
  SaveSize := 0;
  Next := Proc;
  while SaveSize < 5 do
  begin
    Size := SizeOfCode(Next);
    Next := Pointer(DWord(Next) + Size);
    Inc(SaveSize, Size);
  end;
  CopyMemory(Old, Proc, SaveSize);
  Byte(Pointer(DWord(Old) + SaveSize)^) := $E9;
  DWord(Pointer(DWord(Old) + SaveSize + 1)^) := DWord(Next) - DWord(Old) - SaveSize - 5;
  Result := SaveSize;
end;

function HookCode(TargetProc, NewProc: Pointer; var OldProc: Pointer): Boolean;
var
  Address: DWord;
  OldProtect: DWord;
  OldFunction: Pointer;
  Proc: Pointer;
begin
  Result := False;
  try
    Proc := TargetProc;
    Address := DWord(NewProc) - DWord(Proc) - 5;
    {+}
    GetMem(OldFunction, 255);
    VirtualProtect(OldFunction, 255, PAGE_EXECUTE_READWRITE, OldProtect);
    {+.}
    VirtualProtect(Proc, 255, PAGE_EXECUTE_READWRITE, OldProtect);
    DWord(OldFunction^) := DWord(Proc);
    Byte(Pointer(DWord(OldFunction) + 4)^) := SaveOldFunction(Proc, Pointer(DWord(OldFunction) + 5));
    Byte(Proc^) := $E9;
    DWord(Pointer(DWord(Proc) + 1)^) := Address;
    VirtualProtect(Proc, 255, OldProtect, OldProtect);
    {+}
    FlushInstructionCache(GetCurrentProcess, Proc, 255);
    FlushInstructionCache(GetCurrentProcess, OldFunction, 255);
    {+.}
    OldProc := Pointer(DWord(OldFunction) + 5);
  except
    Exit;
  end;
  Result := True;
end;

function HookProc(lpModuleName, lpProcName: PChar; NewProc: Pointer; var OldProc: Pointer): Boolean;
var
  hModule: DWord;
  fnAdr: Pointer;
begin
  Result := False;
  hModule := GetModuleHandle(lpModuleName);
  if hModule = 0 then
    hModule := LoadLibrary(lpModuleName);
  if hModule = 0 then
    Exit;
  fnAdr := GetProcAddress(hModule, lpProcName);
  if fnAdr = nil then
    Exit;
  Result := HookCode(fnAdr, NewProc, OldProc);
end;

function UnhookCode(OldProc: Pointer): Boolean;
var
  OldProtect: DWord;
  Proc: Pointer;
  SaveSize, iProtect: DWord;
begin
  Result := True;
  if OldProc <> nil then
  try
    Proc := Pointer(DWord(Pointer(DWord(OldProc) - 5)^));
    SaveSize := Byte(Pointer(DWord(OldProc) - 1)^);
    {+}
    iProtect := SaveSize;
    if (iProtect < 5) then
      iProtect := 5
    else if (iProtect > 255) then
      iProtect := 255;
    {+.}
    VirtualProtect(Proc, {5}iProtect, PAGE_EXECUTE_READWRITE, OldProtect);
    CopyMemory(Proc, OldProc, SaveSize);
    VirtualProtect(Proc, {5}iProtect, OldProtect, OldProtect);
    {+}
    FlushInstructionCache(GetCurrentProcess, Proc, iProtect);
    {+.}
    FreeMem(Pointer(DWord(OldProc) - 5));
  except
    Result := False;
  end;
end;

{+}

// @@@ dbxoodbc @@@ : http://open-dbexpress.sourceforge.net/
{--------------------------------------------------------------------------------------------------------}
// {begin cut : dbxoodbc: SqlExprFix.pas}
// *********************** RUNTIME PATCH ***********************

const
  PageRWFlag = {$IFDEF LINUX}PROT_READ or PROT_WRITE{$ELSE}PAGE_READWRITE{$ENDIF};

function GetNativeAddr(Ptr: Pointer): Pointer;
begin
  Result := Ptr;
  if (PJumpInstruction(Result)^.Code = $FF) and // long jmp to package_address_5b
    (PJumpInstruction(Integer(Result) + 6)^.Code = $8B) and // mov
    (PJumpInstruction(Integer(Result) + 7)^.Code = $C0) {// eax,eax} then
    Result := PPPointer(Integer(Result) + 2)^^;
end;

function JumpFromCode(MSrc, MDest: Pointer; SaveJump: PJumpInstruction): Boolean;
var
  SaveFlagSrc: DWord;
begin
  Result := False;
  if Assigned(SaveJump) then
    FillChar(SaveJump^, SizeOf(TJumpInstruction), 0);
  if MSrc = nil then
    Exit;
  if MSrc = MDest then
    Exit;
  MSrc := GetNativeAddr(MSrc);
  if MSrc = MDest then
    Exit;
  if VirtualProtect(MSrc, SizeOf(TJumpInstruction), PageRWFlag, @SaveFlagSrc) then
  try
    if Assigned(SaveJump) then
      SaveJump^ := PJumpInstruction(MSrc)^;
    with PJumpInstruction(MSrc)^ do
    begin
      Code := $E9; // JMP SIZE = 5b
      Offset := Integer(MDest) - Integer(MSrc) - 5;
    end;
    Result := True;
  finally
    VirtualProtect(MSrc, SizeOf(TJumpInstruction), SaveFlagSrc, @SaveFlagSrc);
{$IFDEF LINUX}
{$ELSE}
    FlushInstructionCache(GetCurrentProcess, MSrc, SizeOf(TJumpInstruction));
{$ENDIF}
  end;
end;

function RestoreSavedCode(MSrc: Pointer; SaveJump: PJumpInstruction): Boolean;
var
  SaveFlagSrc: DWord;
begin
  Result := False;
  if (SaveJump = nil) or (SaveJump.Code = 0) or (MSrc = nil) then
    Exit;
  MSrc := GetNativeAddr(MSrc);
  try
    if VirtualProtect(MSrc, SizeOf(TJumpInstruction), PageRWFlag, @SaveFlagSrc) then
    try
      PJumpInstruction(MSrc)^ := SaveJump^; // Restore Code
       // Clear Buffer
      FillChar(SaveJump, SizeOf(TJumpInstruction), 0);
    finally
      VirtualProtect(MSrc, SizeOf(TJumpInstruction), SaveFlagSrc, @SaveFlagSrc);
{$IFDEF LINUX}
{$ELSE}
      FlushInstructionCache(GetCurrentProcess, MSrc, SizeOf(TJumpInstruction));
{$ENDIF}
    end;
    Result := True;
  except
  end;
end;

function IsJumpFromCode(MSrc, MDest: Pointer): Boolean;
begin
  if (MSrc = nil) or (MDest = nil) then
    Result := False
  else
    with PJumpInstruction(MSrc)^ do
      Result := (Code = $E9) and (Integer(Offset) = (Integer(MDest) - Integer(MSrc) - 5));
end; { IsJumpFromCode }

function GetVirtualMethodAddress(AClass: TClass; AIndex: Integer): Pointer;
var
  Table: PPointer;
begin
  Table := PPointer(AClass);
  Inc(Table, AIndex - 1);
  Result := Table^;
end;

function SetVirtualMethodAddress(AClass: TClass; AIndex: Integer;
  NewAddress: Pointer): Pointer;
const
  PageSize = SizeOf(Pointer);
var
  Table: PPointer;
  SaveFlag: DWord;
begin
  Table := PPointer(AClass);
  Inc(Table, AIndex - 1);
  Result := Table^;
  if VirtualProtect(Table, PageSize, PAGE_EXECUTE_READWRITE, @SaveFlag) then
  try
    Table^ := NewAddress;
  finally
    VirtualProtect(Table, PageSize, SaveFlag, @SaveFlag);
  end;
end;

function FindVirtualMethodIndex(AClass: TClass; MethodAddr: Pointer): Integer;
begin
  //{
  try
    for Result := 1 to 1000000 do
    begin
      if GetVirtualMethodAddress(AClass, Result) = MethodAddr then
        Exit;
    end;
    Result := -1;
  except
    Result := -1;
  end;
  {}
  {
  Result := 0;
  repeat
    Inc(Result);
  until (GetVirtualMethodAddress(AClass, Result) = MethodAddr);
  {}
end;

function GetVirtualMethodOffset(AClass: TClass; MethodAddr: Pointer): Integer;
begin
  Result := (FindVirtualMethodIndex(AClass, MethodAddr) - 1) * SizeOf(Pointer);
end;

function FxtGetStubAddress(AStub: Pointer; OffsetE8: Integer = 0): Pointer;
begin
  Inc(Integer(AStub), OffsetE8);
  if PByte(AStub)^ = $E8 then
  begin
    Inc(Integer(AStub));
    Result := Pointer(Integer(AStub) + SizeOf(Pointer) + PInteger(AStub)^);
  end
  else
    Result := nil;
end;

{+.}

end.
