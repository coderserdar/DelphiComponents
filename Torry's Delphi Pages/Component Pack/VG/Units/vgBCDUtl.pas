{*******************************************************}
{                                                       }
{         Vladimir Gaitanoff Delphi VCL Library         }
{         BCD utility routines                          }
{                                                       }
{         A number of functions is written by            }
{         Vadim Lopushansky, NotaBene@dak.uame.com.ua   }
{                                                       }
{         Copyright (c) 1997, 2000                      }
{                                                       }
{*******************************************************}

{$I VG.INC }
{$D-,L-}

unit vgBCDUtl;

interface
uses BDE, SysUtils;

type
{ TMantissa }
  TMantissa = array [0..63] of Byte;
  TMantissa2 = array [0..127] of Byte;

  PMantissa = ^TMantissa;

function FMTBCDToCurr(const BCD: FMTBcd; var Curr: Currency): Boolean;
{ Converts BCD to Currency }

function CurrToFMTBCD(Curr: Currency; var BCD: FMTBcd; Precision,
  Decimals: Integer): Boolean;
{ Converts Currency to BCD }

procedure UnpackFMTBCD(const BCD: FMTBCD; var Mantissa: TMantissa; var Negative: Boolean);
{ Unpacks BCD }

procedure PackFMTBCD(const Mantissa: TMantissa; Negative: Boolean; var BCD: FMTBCD);
{ Packs BCD }

function CompareFMTBCD(const Value1, Value2: FMTBCD): Integer;
{ Returns <0 if Value1 < Value2, 0 of Value1 = Value2, >0 if Value1 < Value2 }

procedure SetZeroBcd(var BCD: FMTBCD);
{ Empties Value }

function IsNegativeFMTBCD(const BCD: FMTBCD): Boolean;
{ Returns True if negative }

function IsPositiveFMTBCD(const BCD: FMTBCD): Boolean;
{ Returns True if positive }

function NegFMTBCD(const BCD: FMTBCD): FMTBCD;
{ Calculates negative }

function AbsFMTBCD(const BCD: FMTBCD): FMTBCD;
{ Calculates absolute }

function AddFMTBCD(const Value1, Value2: FMTBCD): FMTBCD;
{ Calculates addition }

function SubFMTBCD(const Value1, Value2: FMTBCD): FMTBCD;
{ Calculates subtruction }

function MulFMTBCD(const Value1, Value2: FMTBCD): FMTBCD;
{ Calculates multiplication }

function DivFMTBCD(const Value1, Value2: FMTBCD): FMTBCD;
{ Calculates division }

{ BCD exceptions  }

type
  EBcdZeroDivide = class(EZeroDivide);
  EBcdOverflow   = class(EOverflow);
  EBcdUnderflow  = class(EUnderflow);

procedure BcdZeroDivide;
procedure BcdOverflow;
procedure BcdUnderflow;

implementation
uses Windows, {$IFDEF _D4_}SysConst,{$ENDIF} vgUtils;

const
  SignBit             = $80;                {mask to get just the sign}
  NoSignBit           = $7F;                {mask to get just the exponent}
  MantissaSize        = SizeOf(TMantissa);
  MantissaSize2       = MantissaSize * 2;

procedure BcdZeroDivide;
begin
  raise EBcdOverflow.Create(ResStr(SZeroDivide));
end;

procedure BcdOverflow;
begin
  raise EBcdOverflow.Create(ResStr(SOverflow));
end;

procedure BcdUnderflow;
begin
  raise EBcdOverflow.Create(ResStr(SUnderflow));
end;

function FMTBCDToCurr(const BCD: FMTBcd; var Curr: Currency): Boolean;
const
  FConst10: Single = 10;
  CWNear: Word = $133F;
var
  CtrlWord: Word;
  Temp: Integer;
  Mantissa: TMantissa;
asm
        PUSH    EBX
        PUSH    ESI
        MOV     EBX,EAX
        MOV     ESI,EDX
        MOV     AL,0
        MOVZX   EDX,[EBX].FMTBcd.iPrecision
        OR      EDX,EDX
        JE      @@8
        LEA     ECX,[EDX+1]
        SHR     ECX,1
@@1:    MOV     AL,[EBX].FMTBcd.iFraction.Byte[ECX-1]
        MOV     AH,AL
        SHR     AL,4
        AND     AH,0FH
        MOV     Mantissa.Word[ECX*2-2],AX
        DEC     ECX
        JNE     @@1
        XOR     EAX,EAX
@@2:    MOV     AL,Mantissa.Byte[ECX]
        OR      AL,AL
        JNE     @@3
        INC     ECX
        CMP     ECX,EDX
        JNE     @@2
        FLDZ
        JMP     @@7
@@3:    MOV     Temp,EAX
        FILD    Temp
@@4:    INC     ECX
        CMP     ECX,EDX
        JE      @@5
        FMUL    FConst10
        MOV     AL,Mantissa.Byte[ECX]
        MOV     Temp,EAX
        FIADD   Temp
        JMP     @@4
@@5:    MOV     AL,[EBX].FMTBcd.iSignSpecialPlaces
        OR      AL,AL
        JNS     @@6
        FCHS
@@6:    AND     EAX,3FH
        SUB     EAX,4
        NEG     EAX
        CALL    FPower10
@@7:    FSTCW   CtrlWord
        FLDCW   CWNear
        FISTP   [ESI].Currency
        FSTSW   AX
        NOT     AL
        AND     AL,1
        FCLEX
        FLDCW   CtrlWord
        FWAIT
@@8:    POP     ESI
        POP     EBX
end;

function CurrToFMTBCD(Curr: Currency; var BCD: FMTBcd; Precision,
  Decimals: Integer): Boolean;
const
  Power10: array[0..3] of Single = (10000, 1000, 100, 10);
var
  Mantissa: TMantissa;
asm
        PUSH    EBX
        PUSH    ESI
        PUSH    EDI
        MOV     ESI,EAX
        XCHG    ECX,EDX
        MOV     [ESI].FMTBcd.iPrecision,CL
        MOV     [ESI].FMTBcd.iSignSpecialPlaces,DL
@@1:    SUB     EDX,4
        JE      @@3
        JA      @@2
        FILD    Curr
        FDIV    Power10.Single[EDX*4+16]
        FISTP   Curr
        JMP     @@3
@@2:    DEC     ECX
        MOV     Mantissa.Byte[ECX],0
        DEC     EDX
        JNE     @@2
@@3:    MOV     EAX,Curr.Integer[0]
        MOV     EBX,Curr.Integer[4]
        OR      EBX,EBX
        JNS     @@4
        NEG     EBX
        NEG     EAX
        SBB     EBX,0
        OR      [ESI].FMTBcd.iSignSpecialPlaces,80H
@@4:    MOV     EDI,10
@@5:    MOV     EDX,EAX
        OR      EDX,EBX
        JE      @@7
        XOR     EDX,EDX
        OR      EBX,EBX
        JE      @@6
        XCHG    EAX,EBX
        DIV     EDI
        XCHG    EAX,EBX
@@6:    DIV     EDI
@@7:    MOV     Mantissa.Byte[ECX-1],DL
        DEC     ECX
        JNE     @@5
        OR      EAX,EBX
        MOV     AL,0
        JNE     @@9
        MOV     CL,[ESI].FMTBcd.iPrecision
        INC     ECX
        SHR     ECX,1
@@8:    MOV     AX,Mantissa.Word[ECX*2-2]
        SHL     AL,4
        OR      AL,AH
        MOV     [ESI].FMTBcd.iFraction.Byte[ECX-1],AL
        DEC     ECX
        JNE     @@8
        MOV     AL,1
@@9:    POP     EDI
        POP     ESI
        POP     EBX
end;

procedure UnpackFMTBCD(const BCD: FMTBCD; var Mantissa: TMantissa; var Negative: Boolean);
asm
        PUSH    EBX
        PUSH    ESI
        MOV     EBX,EAX
        MOV     ESI,EDX
        MOVZX   EDX,[EBX].FMTBcd.iPrecision
        OR      EDX,EDX
        JE      @@3
        MOV     AL,[EBX].FMTBcd.iSignSpecialPlaces
        MOV     AH,AL
        AND     AL,SignBit
        SHR     AL, 7
        MOV     BYTE PTR [ECX], AL
        MOVZX   ECX,AH
        AND     CL,00111111b
        ADD     ESI, ECX
        ADD     ESI, 32
        SUB     ESI, EDX
        LEA     ECX,[EDX+1]
        SHR     ECX,1
@@1:    MOV     AL,[EBX].FMTBcd.iFraction.Byte[ECX-1]
        MOV     AH,AL
        SHR     AL,4
        AND     AH,0FH
        MOV     ESI.Word[ECX*2-2],AX
        DEC     ECX
        JNE     @@1
@@3:    POP     ESI
        POP     EBX
end;

procedure PackFMTBCD(const Mantissa: TMantissa; Negative: Boolean; var BCD: FMTBCD);
asm
        PUSH    EDI
        PUSH    ESI
        MOV     EDI,EAX       // Mantissa
        MOV     ESI,ECX       // BCD
        XOR     EAX,EAX
        OR      DL,DL         // Sign
        JZ      @@1
        MOV     AL, SignBit
@@1:    MOV     [ESI].FmtBcd.iSignSpecialPlaces, AL
        MOV     AL,AH
        MOV     ECX, MantissaSize
        MOV     EDX,EDI
        ADD     EDX,MantissaSize / 2
        REPE    SCASB
        DEC     EDI           // Seeking for first non-zero
        CMP     CL,32
        JB      @@2
        MOV     CL,32
@@2:    MOV     [ESI].FMTBcd.iPrecision,CL
        SUB     EDX,EDI
        JB      @@3
        OR      [ESI].FmtBcd.iSignSpecialPlaces, DL
@@3:    INC     ECX
        SHR     ECX,1
        NEG     EDX
        ADD     EDX,32
        SHR     EDX,1
        ADD     ESI, EDX
@@4:    MOV     AX, WORD PTR [EDI]
        SHL     AL,4
        OR      AL,AH
        MOV     BYTE PTR [ESI], AL
        INC     ESI
        INC     EDI
        INC     EDI
        LOOP    @@4
        POP     ESI
        POP     EDI
end;

function CompareFMTBCD(const Value1, Value2: FMTBCD): Integer;
var
  Sign1, Sign2, Tmp: Boolean;
  M: array [0..1] of TMantissa;
begin
  Sign1 := (Value1.iSignSpecialPlaces and SignBit) = 0;
  Sign2 := (Value2.iSignSpecialPlaces and SignBit) = 0;
  Result := 0;
  if Sign1 and not Sign2 then
    Inc(Result)
  else if not Sign1 and Sign2 then
    Dec(Result)
  else begin
    ZeroMem(@M, SizeOf(M));
    UnpackFMTBCD(Value1, M[0], Tmp);
    UnpackFMTBCD(Value2, M[1], Tmp);
    Result := CompareChars(M[0], M[1], MantissaSize);
    if not (Sign1 or Sign2) then
      Result := - Result;
  end;
end;

procedure SetZeroBcd(var BCD: FMTBCD);
asm
        MOV     DWORD PTR [EAX],1
end;

function IsNegativeFMTBCD(const BCD: FMTBCD): Boolean;
asm
        XOR     ECX,ECX
        TEST    BYTE PTR [EAX].FMTBcd.iSignSpecialPlaces, SignBit
        MOV     EAX,ECX
        JNE     @@1
        INC     EAX
@@1:
end;

function IsPositiveFMTBCD(const BCD: FMTBCD): Boolean;
asm
        XOR     ECX,ECX
        TEST    BYTE PTR [EAX].FMTBcd.iSignSpecialPlaces, SignBit
        MOV     EAX,ECX
        JE      @@1
        INC     EAX
@@1:
end;

function NegFMTBCD(const BCD: FMTBCD): FMTBCD;
begin
  Result := BCD;
  Result.iSignSpecialPlaces := Result.iSignSpecialPlaces xor SignBit;
end;

{ Calculates negative }

function AbsFMTBCD(const BCD: FMTBCD): FMTBCD;
begin
  Result := BCD;
  Result.iSignSpecialPlaces := Result.iSignSpecialPlaces and NoSignBit;
end;

procedure AddMantissas(const Mantissa1: TMantissa; var Mantissa2: TMantissa);
asm
        PUSH    ESI
        XCHG    EDI, EDX     // Initialize and save EDI
        MOV     ESI, EAX
        MOV     ECX, MantissaSize - 1
        ADD     ESI, ECX
        ADD     EDI, ECX
        INC     ECX
        CLC
        STD
@@1:    LODSB
        ADC     AL,BYTE PTR [EDI]
        AAA
        STOSB
        DEC     ECX
        JNZ     @@1
        JNC     @@2
        CALL    BcdOverflow
@@2:    CLD
        MOV     EDI, EDX     // Restore EDI
        POP     ESI
end;

procedure AddMantissas2(const Mantissa1: TMantissa2; var Mantissa2: TMantissa2);
asm
       PUSH    ESI
       XCHG    EDI, EDX     // Initialize and save EDI
       MOV     ESI, EAX
       MOV     ECX, MantissaSize2 - 1
       ADD     ESI, ECX
       ADD     EDI, ECX
       INC     ECX
       CLC
       STD
@@1:   LODSB
       ADC     AL,BYTE PTR [EDI]
       AAA
       STOSB
       DEC     ECX
       JNZ     @@1
       JNC     @@2
       CALL    BcdOverflow
@@2:   CLD
       MOV     EDI, EDX     // Restore EDI
       POP     ESI
end;

procedure SubMantissas(const Mantissa1: TMantissa; var Mantissa2: TMantissa);
asm
        PUSH    ESI
        MOV     ESI, EAX
        XCHG    EDI, EDX     // Initialize and save EDI
        MOV     ECX, MantissaSize - 1
        ADD     ESI, ECX
        ADD     EDI, ECX
        INC     ECX
        CLC
        STD
@@1:    MOV     AL, BYTE PTR [EDI]
        SBB     AL, BYTE PTR [ESI]
        AAS
        STOSB
        DEC     ESI
        LOOP    @@1
        JNC     @@2
        CALL    BcdUnderflow
@@2:    CLD
        MOV     EDI, EDX     // Restore EDI
        POP     ESI
end;

{
procedure RoundMantissa(var Mantissa: TMantissa; Last: Integer);
asm
        PUSH    EDI
        MOV     EDI, EAX
        MOV     ECX, MantissaSize
        ADD     EDI, ECX
        DEC     EDI
        XOR     AL,AL
        STD
        REPE    SCASB
        JE      @@2
        INC     EDI          // Last non-zero digit
        SUB     ECX, EDX     // Number of digits to empty
        JLE     @@2
        XOR     AL, AL       // Set to zero emptied digits
        REP     STOSB
        MOV     ECX, EDX     // Number of digits to be adjusted
        XCHG    AL, BYTE PTR [EDI]
        DEC     EDI
        ADD     AL, 5
        AAA                  // CF initialized
@@1:    MOV     AL, BYTE PTR [EDI]
        ADC     AL, 0
        AAA
        STOSB
        JNC     @@2
        LOOP    @@1
@@2:    CLD
        POP     EDI
end;
}

procedure NegMantissa(var Mantissa: TMantissa);
asm
        PUSH    EDI
        MOV     EDI, EAX
        MOV     ECX, MantissaSize - 1
        ADD     EDI, ECX
        INC     ECX
        XOR     DH, DH
        CLC
        STD
@@1:    MOV     AL, DH
        SBB     AL, BYTE PTR [EDI]
        AAS
        STOSB
        LOOP    @@1
        CLD
        POP     EDI
end;

function IsZeroMantissa(const Mantissa: TMantissa) : Boolean;
asm
        MOV     EDX, EDI
        MOV     EDI, EAX
        XOR     EAX, EAX
        MOV     ECX, MantissaSize
        REPE    SCASB
        JNE     @@1
        INC     EAX
@@1:    MOV     EDI, EDX
end;

function AddFMTBCD(const Value1, Value2: FMTBCD): FMTBCD;
var
  S1, S2: Boolean;
  M: array [0..1] of TMantissa;
  Cmp: Integer;
begin
  if Value1.iPrecision = 0 then
    Result := Value2
  else if Value2.iPrecision = 0 then
    Result := Value1
  else begin
    ZeroMem(@M, SizeOf(M));
    ZeroMem(@Result, SizeOf(FMTBCD));

    UnpackFMTBCD(Value1, M[0], S1);
    UnpackFMTBCD(Value2, M[1], S2);

    if S1 = S2 then
      AddMantissas(M[0], M[1])
    else begin
      Cmp := CompareChars(M[0], M[1], MantissaSize);
      if Cmp <> 0 then
      begin
        SubMantissas(M[0], M[1]);
        if Cmp > 0 then
        begin
          NegMantissa(M[1]);
          S2 := S1;
        end;
      end else
        Exit;
    end;
    PackFMTBCD(M[1], S2, Result);
  end;
end;

function SubFMTBCD(const Value1, Value2: FMTBCD): FMTBCD;
begin
  Result := AddFMTBCD(Value1, NegFMTBCD(Value2));
end;

procedure MulMantissas(const Mantissa1: TMantissa; var Mantissa2: TMantissa);
var
  I, J, K: Integer;
  ACM: Byte;
  T: array [0..1] of TMantissa2;
begin
  ZeroMem(@T, MantissaSize2);
  ACM := 0;
  for I := MantissaSize - 1 downto 0 do
    if Mantissa2[I] <> 0 then
    begin
      ZeroMem(@T[1], MantissaSize2);
      for J := MantissaSize - 1 downto 0 do
      begin
        K := MantissaSize + J - (MantissaSize - I - 1);
        T[1][K] := Mantissa2[I] * Mantissa1[J] + ACM;
        ACM := 0;
        if T[1][K] >= 10 then
        begin
          ACM := T[1][K] div 10 ;
          Dec(T[1][K], ACM * 10);
        end;
      end;
      AddMantissas2(T[1], T[0]);
    end;
  if ACM > 0 then BCDOverflow;

  I := MantissaSize + MantissaSize div 2;
  if T[0][I + 1] > 4 then
  begin
    T[0][I + 1] := 0;
    repeat
      Inc(T[0][I]);
      if T[0][I] >= 10 then Dec(T[0][I], 10);
      Dec(I);
      if I < 0 then BCDOverflow;
    until T[0][I] < 10;
  end;

  Mantissa2 := PMantissa(@T[0][MantissaSize div 2])^;
end;

function MulFMTBCD(const Value1, Value2: FMTBCD): FMTBCD;
var
  S1, S2: Boolean;
  M: array [0..1] of TMantissa;
begin
  ZeroMem(@M, SizeOf(M));
  UnpackFMTBCD(Value1, M[0], S1);
  UnpackFMTBCD(Value2, M[1], S2);
  MulMantissas(M[0], M[1]);
  PackFMTBCD(M[1], S1 xor S2, Result);
end;

procedure SubMantissas2(var Mantissa1: TMantissa; const Mantissa2: TMantissa);
asm
        PUSH    ESI
        MOV     EDI, EAX
        XCHG    ESI, EDX     // Initialize and save EDI
        MOV     ECX, MantissaSize - 1
        ADD     ESI, ECX
        ADD     EDI, ECX
        INC     ECX
        CLC
        STD
@@1:    MOV     AL, BYTE PTR [EDI]
        SBB     AL, BYTE PTR [ESI]
        AAS
        STOSB
        DEC     ESI
        LOOP    @@1
@@2:    CLD
        MOV     EDI, EDX     // Restore EDI
        POP     ESI
end;

function DivMantissas(var Mantissa1: TMantissa; Mantissa2: TMantissa; var Remainder: Boolean): TMantissa;
var
  I: Integer;
  F: Boolean;
  D: TMantissa;
begin
  ZeroMem(@D, MantissaSize);
  ZeroMem(@Result, MantissaSize);
  D[MantissaSize div 2 - 1] := 1; // D := 1
  // Выравнивание
  I := 0;
  F := True;
  Remainder := False;
  while (F) and (I < MantissaSize div 2 - 1) do // D*10 Mantissa2*10
  begin
    Inc(I);
    D[MantissaSize div 2 - I] := 0;
    D[MantissaSize div 2 - I - 1 ] := 1;
    if Mantissa2[0] <> 0 then BCDOverflow;
    Move(Mantissa2[1], Mantissa2[0], MantissaSize - 1);
    Mantissa2[MantissaSize - 1] := 0;
    F := CompareChars(Mantissa2, Mantissa1, MantissaSize) < 0;
  end;

  // Prepare for division
  if (I > 0) and (I < MantissaSize div 2) then
  begin
    Move(Mantissa2[0], Mantissa2[1], MantissaSize - 1);
    Mantissa2[0] := 0;
    if I > 0 then
    begin
      D[MantissaSize div 2 -I - 1 ] := 0;
      Dec(I);
      D[MantissaSize div 2 - 1 - I] := 1;
    end;
  end;

  // Integer division
  while I >= 0 do
  begin
    SubMantissas2(Mantissa1, Mantissa2);
    AddMantissas(D, Result);
    if IsZeroMantissa(Mantissa1) then Exit;
    if CompareChars(Mantissa1, Mantissa2, MantissaSize) < 0 then
    begin
       if I > 0 then
       begin
         D[MantissaSize div 2 - I - 1] := 0;
         Dec(I);
         D[MantissaSize div 2 - I - 1] := 1;
         Move(Mantissa2[0], Mantissa2[1], MantissaSize - 1);
         Mantissa2[0] := 0;
         if IsZeroMantissa(Mantissa2) then Exit;
         if CompareChars(Mantissa1, Mantissa2, MantissaSize) < 0 then Break;
      end else
        Break;
    end;
  end;
  Remainder := True;
end;

function DivFMTBCD(const Value1, Value2: FMTBCD): FMTBCD;
var
  I: Integer;
  S1, S2, F: Boolean;
  A, B, R, R1: TMantissa;
begin
  ZeroMem(@A,  MantissaSize);
  ZeroMem(@B,  MantissaSize);
  ZeroMem(@R,  MantissaSize);
  ZeroMem(@R1, MantissaSize);

  UnpackFMTBCD(Value2, B, S2);
  if IsZeroMantissa(B) then BcdZeroDivide;

  UnpackFMTBCD(Value1, A, S1);
  if IsZeroMantissa(A) then
  begin
    SetZeroBCD(Result);
    Exit;
  end;

  I := CompareChars(A, B, MantissaSize);
  if I = 0 then
  begin
    PDWORD(@Result)^ := $00100001;
    Exit;
  end;

  F := I > 0;

  if F then
    R := DivMantissas(A, B, F) // Integer division
  else
    F := True;

  if F then                    // Reminder division
  begin
    F := False;
    I := 0;
    while not F do
    begin
      F := CompareChars(A, B, MantissaSize) < 0;
      while F do
      begin
        if A[0] = 0 then
        begin
          Move(A[1], A[0], MantissaSize - 1);
          Inc(I);
          F := CompareChars(A, B, MantissaSize) < 0;
        end else begin
          F := True;
          Break;
        end;
      end;
      if F then Break;

      // I - number of shifts
      R1 := DivMantissas(A, B, F);
      Move(R1[0], R1[I], MantissaSize - I);
      ZeroMem(@R1[0], I);
      AddMantissas(R1, R);
      F := (not F) or (I > 31);
    end;
  end;
  PackFMTBCD(R, S1 xor S2, Result);
end;

end.
