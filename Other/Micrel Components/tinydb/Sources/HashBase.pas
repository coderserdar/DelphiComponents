unit HashBase;

{$I TinyDB.INC}
{$WRITEABLECONST ON}
//for 32 x 64 bit compatibility by JB.
{$IFDEF As64BitCompatibility}
{$DEFINE PUREPASCAL}
{$ENDIF}
interface

uses Classes, Windows {$IFDEF DELPHI_18_UP}, AnsiStrings{$ENDIF};

type
  PIntArray = ^TIntArray;
  TIntArray = array[0..1023] of LongWord;

  {the Base-Class of all Hashs}
  THashClass = class of THash;

  THash = class(TPersistent)
  private
    function GetDigestStr(Index: Integer): AnsiString;
  protected
    class function TestVector: Pointer; virtual; {must override}
  public
    destructor Destroy; override;
    procedure Init; virtual;
    procedure Calc(const Data; DataSize: Integer); virtual; {must override}
    procedure Done; virtual;
    function DigestKey: Pointer; virtual; {must override}

    class function DigestKeySize: Integer; virtual; {must override}
    class function CalcBuffer(Digest: Pointer; const Buffer; BufferSize: Integer): AnsiString;
    class function CalcStream(Digest: Pointer; const Stream: TStream; StreamSize: Integer): AnsiString;
    class function CalcString(Digest: Pointer; const Data: AnsiString): AnsiString;
    class function CalcFile(Digest: Pointer; const FileName: string): AnsiString;
    {test the correct working}
    class function SelfTest: Boolean;

    {give back the Digest binary in a AnsiString}
    property DigestKeyStr: AnsiString index -1 read GetDigestStr;
    {give back the Default AnsiString Format from the Digest}
    property DigestString: AnsiString index  0 read GetDigestStr;
    {give back a HEX-AnsiString form the Digest}
    property DigestBase16: AnsiString index 16 read GetDigestStr;
    {give back a Base64-MIME AnsiString}
    property DigestBase64: AnsiString index 64 read GetDigestStr;
  end;

  {calculate CRC32 Checksum, CRC is default $FFFFFFFF, after calc you must inverse Result with NOT}
  function CRC32(CRC: LongWord; const Data: Pointer; DataSize: {$IFDEF WIN64}NativeUInt{$ELSE}LongWord{$ENDIF}): LongWord;
  function GetTestVector: PAnsiChar; register;
  {AnsiString convert.}
  function StrToBase64(Value: PAnsiChar; Len: Integer): AnsiString;
  function Base64ToStr(Value: PAnsiChar; Len: Integer): AnsiString;
  function StrToBase16(Value: PAnsiChar; Len: Integer): Ansistring;
  function Base16ToStr(Value: PAnsiChar; Len: Integer): Ansistring;
  {Utility funcs}
  function ROL(Value: LongWord; Shift: Integer): LongWord;
  function ROLADD(Value, Add: LongWord; Shift: Integer): LongWord;
  function ROLSUB(Value, Sub: LongWord; Shift: Integer): LongWord;
  function ROR(Value: LongWord; Shift: Integer): LongWord;
  function RORADD(Value, Add: LongWord; Shift: Integer): LongWord;
  function RORSUB(Value, Sub: LongWord; Shift: Integer): LongWord;
  function SwapBits(Value: LongWord): LongWord;
  function CPUType: Integer; {3 = 386, 4 = 486, 5 = Pentium, 6 > Pentium}

const
  {change this to 16 - HEX, 64 - Base64 MIME or -1 - binary}
  DefaultDigestStringFormat : Integer = 64;
  InitTestIsOk              : Boolean = False;

  {this is set to SwapInt for <= 386 and BSwapInt >= 486 CPU, don't modify}
  SwapInteger: function(Value: LongWord): LongWord  = nil;
  {Count of Integers Buffer}
  SwapIntegerBuffer: procedure(Source, Dest: Pointer; Count: Integer) = nil;
  FCPUType: Integer = 0;

implementation

uses SysUtils;

const
  HashMaxBufSize = 1024 * 4;  {Buffersize for File, Stream-Access}

function CPUType: Integer;
begin
  Result := FCPUType;
end;

{I am missing the INLINE Statement :-( }
function ROL(Value: LongWord; Shift: Integer): LongWord; {$IFNDEF PUREPASCAL}assembler; {$ENDIF}
{$IFDEF PUREPASCAL}
begin
  Result := (Value shl Shift) or (Value shr (32 - Shift));
end;
{$ELSE}
asm
  {$IFDEF  WIN64}
  MOV  EAX, ECX
  MOV  CL, DL
  ROL  EAX, CL
  {$ELSE}
  MOV   CL,DL
  ROL   EAX,CL
  {$ENDIF}
end;
{$ENDIF}

function ROLADD(Value, Add: LongWord; Shift: Integer): LongWord; {$IFNDEF  WIN64}assembler;
asm
  ROL   EAX,CL
  ADD   EAX,EDX
end;
{$ELSE}
begin
  Result := ROL(Value, Shift) + Add;
end;
{$ENDIF}

function ROLSUB(Value, Sub: LongWord; Shift: Integer): LongWord; {$IFNDEF  WIN64}assembler;
asm
  ROL   EAX,CL
  SUB   EAX,EDX
end;
{$ELSE}
begin
  Result := ROL(Value, Shift) - Sub;
end;
{$ENDIF}

function ROR(Value: LongWord; Shift: Integer): LongWord; {$IFNDEF PUREPASCAL}assembler; {$ENDIF}
{$IFDEF PUREPASCAL}
begin
  Result := (Value shr Shift) or (Value shl (32-Shift));
end;
{$ELSE}
asm
  {$IFDEF  WIN64}
  MOV  EAX, ECX
  MOV  CL, DL
  ROR  EAX, CL
  {$ELSE}
  MOV   CL,DL
  ROR   EAX,CL
  {$ENDIF}
end;
{$ENDIF}

function RORADD(Value, Add: LongWord; Shift: Integer): LongWord; {$IFNDEF  WIN64}assembler;
asm
  ROR  EAX,CL
  ADD  EAX,EDX
end;
{$ELSE}
begin
  Result := ROR(Value, Shift) + Add;
end;
{$ENDIF}

function RORSUB(Value, Sub: LongWord; Shift: Integer): LongWord; {$IFNDEF  WIN64}assembler;
asm
  ROR  EAX,CL
  SUB  EAX,EDX
end;
{$ELSE}
begin
  Result := ROR(Value, Shift) - Sub;
end;
{$ENDIF}

{swap 4 Bytes Intel}
{$IFDEF PUREPASCAL}
function SwapInt(Value: LongWord): LongWord;
begin
  Result := (Value shr 24) or ((Value shr 8) and $FF00) or ((Value shl 8) and $FF0000) or (Value shl 24);
end;
{$ELSE}
function SwapInt(Value: LongWord): LongWord; assembler; register;
asm
       XCHG  AH,AL
       ROL   EAX,16
       XCHG  AH,AL
end;
{$ENDIF}

{$IFDEF PUREPASCAL}
function BSwapInt(Value: LongWord): LongWord; inline;
var
  A, B: array [0..3] of Byte;
begin
  Move(Value, A, SizeOf(A));
  B[0] := A[3];
  B[1] := A[2];
  B[2] := A[1];
  B[3] := A[0];
  Move(B, Result, SizeOf(Result));
end;
{$ELSE}
function BSwapInt(Value: LongWord): LongWord; assembler; register;
asm
  BSWAP  EAX
end;
{$ENDIF}

{$IFDEF WIN64}
type
   PLongWordArr = ^TLongWordArr;
   TLongWordArr = array[0..0] of LongWord;

procedure SwapIntBuf(Source, Dest: Pointer; Count: Integer);
begin
  Dec(Count);
  while Count >= 0 do
  begin
    PLongWordArr(Dest)[Count] := SwapInt(PLongWordArr(Source)[Count]);
    Dec(Count);
  end;
end;

procedure BSwapIntBuf(Source, Dest: Pointer; Count: Integer);
var
  P: LongWord;
begin
  Dec(Count);
  while Count >= 0 do
  begin
    P := PLongWordArr(Source)[Count];
    P := BSwapInt(P);
    PLongWordArr(Dest)[Count] := P;

    Dec(Count);
  end;
end;

{$ELSE}

procedure SwapIntBuf(Source,Dest: Pointer; Count: Integer); assembler; register;
asm
       JCXZ   @Exit
       PUSH   EBX
       SUB    EAX,4
       SUB    EDX,4
@@1:   MOV    EBX,[EAX + ECX * 4]
       XCHG   BL,BH
       ROL    EBX,16
       XCHG   BL,BH
       MOV    [EDX + ECX * 4],EBX
       DEC    ECX
       JNZ    @@1
       POP    EBX
@Exit:
end;

procedure BSwapIntBuf(Source, Dest: Pointer; Count: Integer); assembler; register;
asm
       JCXZ   @Exit
       PUSH   EBX
       SUB    EAX,4
       SUB    EDX,4
@@1:   MOV    EBX,[EAX + ECX * 4]
       BSWAP  EBX
       MOV    [EDX + ECX * 4],EBX
       DEC    ECX
       JNZ    @@1
       POP    EBX
@Exit:
end;
{$ENDIF}

{reverse the bit order from a integer}
function SwapBits(Value: LongWord): LongWord;
{$IFDEF  WIN64}
  function ReverseBits(b: Byte): Byte;
  var
    i: Integer;
  begin
    Result := 0;
    for i := 1 to 8 do
    begin
      Result := (Result shl 1) or (b and 1);
      b := b shr 1;
    end;
  end;
type
  tb = array [0..3] of Byte;
var
  b: tb;
begin
  Move(Value, b, SizeOf(b));
  b[0] := ReverseBits(b[0]);
  b[1] := ReverseBits(b[1]);
  b[2] := ReverseBits(b[2]);
  b[3] := ReverseBits(b[3]);
  Move(b, Result, SizeOf(b));
  Result := SwapInt(Result);
end;
{$ELSE}
asm
       CMP    FCPUType,3
       JLE    @@1
       BSWAP  EAX
       JMP    @@2
@@1:   XCHG   AH,AL
       ROL    EAX,16
       XCHG   AH,AL
@@2:   MOV    EDX,EAX
       AND    EAX,0AAAAAAAAh
       SHR    EAX,1
       AND    EDX,055555555h
       SHL    EDX,1
       OR     EAX,EDX
       MOV    EDX,EAX
       AND    EAX,0CCCCCCCCh
       SHR    EAX,2
       AND    EDX,033333333h
       SHL    EDX,2
       OR     EAX,EDX
       MOV    EDX,EAX
       AND    EAX,0F0F0F0F0h
       SHR    EAX,4
       AND    EDX,00F0F0F0Fh
       SHL    EDX,4
       OR     EAX,EDX
end;
{$ENDIF}

function StrToBase64(Value: PAnsiChar; Len: Integer): AnsiString;
const
  Table: PAnsiChar = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';
var
  B,J,L: Integer;
  S: PByte;
  D: PAnsiChar;
begin
  Result := '';
  if Value = nil then Exit;
  if Len < 0 then Len := {$IFDEF DELPHI_18_UP}AnsiStrings.{$ENDIF}StrLen(Value);
  L := Len;
  SetLength(Result, (L+2) div 3 * 4);
  D := PAnsiChar(Result);
  B := 0;
  S := PByte(Value);
  while L > 0 do
  begin
    for J := 1 to 3 do
    begin
      if L > 0 then
      begin
        B := B or S^;
        Inc(S);
        Dec(L);
      end;
      B := B shl 8;
    end;
    while B <> 0 do
    begin
      B := ROL(B, 6);
      D^ := Table[B and $3F];
      Inc(D);
      B := B and not $3F;
    end;
  end;
  SetLength(Result, D - PAnsiChar(Result));
  if Len mod 3 = 1 then Result := Result + '==' else
    if Len mod 3 = 2 then Result := Result + '=';
end;

function Base64ToStr(Value: PAnsiChar; Len: Integer): AnsiString;
var
  B,J: Integer;
  D: PAnsiChar;
  S: PByte;
begin
  Result := '';
  if Value = nil then Exit;
  if Len < 0 then Len := {$IFDEF DELPHI_18_UP}AnsiStrings.{$ENDIF}StrLen(Value);
  SetLength(Result, Len);
  if Len = 0 then Exit;
  Move(PAnsiChar(Value)^, PAnsiChar(Result)^, Len);
  while Len and 3 <> 0 do
  begin
    Result := Result + '=';
    Inc(Len);
  end;
  D := PAnsiChar(Result);
  S := PByte(Result);
  Len := Len div 4 * 3;
  while Len > 0 do
  begin
    B := 0;
    for J := 1 to 4 do
    begin
      if (S^ >= 97) and (S^ <= 122) then Inc(B, S^ - 71) else
        if (S^ >= 65) and (S^ <= 90) then Inc(B, S^ - 65) else
          if (S^ >= 48) and (S^ <= 57) then Inc(B, S^ + 4) else
            if S^ = 43 then Inc(B, 62) else
              if S^ <> 61 then Inc(B, 63) else Dec(Len);
      B := B shl 6;
      Inc(S);
    end;
    B := ROL(B, 2);
    for J := 1 to 3 do
    begin
      if Len <= 0 then Break;
      B := ROL(B, 8);
      D^ := AnsiChar(B);
      Inc(D);
      Dec(Len);
    end;
  end;
  SetLength(Result, D - PAnsiChar(Result));
end;

function StrToBase16(Value: PAnsiChar; Len: Integer): Ansistring;
const
  H: array[0..15] of AnsiChar = '0123456789ABCDEF';
var
  S: PByte;
  D: PAnsiChar;
begin
  Result := '';
  if Value = nil then Exit;
  if Len < 0 then Len := {$IFDEF DELPHI_18_UP}AnsiStrings.{$ENDIF}StrLen(Value);
  SetLength(Result, Len * 2);
  if Len = 0 then Exit;
  D := PAnsiChar(Result);
  S := PByte(Value);
  while Len > 0 do
  begin
    D^ := H[S^ shr  4]; Inc(D);
    D^ := H[S^ and $F]; Inc(D);
    Inc(S);
    Dec(Len);
  end;
end;

function Base16ToStr(Value: PAnsiChar; Len: Integer): Ansistring;
var
  D: PByte;
  V: Byte;
  S: PAnsiChar;
begin
  Result := '';
  if Value = nil then Exit;
  if Len < 0 then Len := {$IFDEF DELPHI_18_UP}AnsiStrings.{$ENDIF}StrLen(Value);
  SetLength(Result, (Len +1) div 2);
  D := PByte(Result);
  S := PAnsiChar(Value);
  while Len > 0 do
  begin
    V := Byte(UpCase(S^));
    Inc(S);
    if V > Byte('9') then D^ := V - Byte('A') + 10
      else D^ := V - Byte('0');
    V := Byte(UpCase(S^));
    Inc(S);
    D^ := D^ shl 4;
    if V > Byte('9') then D^ := D^ or (V - Byte('A') + 10)
      else D^ := D^ or (V - Byte('0'));
    Dec(Len, 2);
    Inc(D);
  end;
  SetLength(Result, PAnsiChar(D) - PAnsiChar(Result));
end;

{$IFDEF PUREPASCAL}
const
  CRCTable: array[0..267] of LongWord = (
         $00000000, $77073096, $EE0E612C, $990951BA,
         $076DC419, $706AF48F, $E963A535, $9E6495A3,
         $0EDB8832, $79DCB8A4, $E0D5E91E, $97D2D988,
         $09B64C2B, $7EB17CBD, $E7B82D07, $90BF1D91,
         $1DB71064, $6AB020F2, $F3B97148, $84BE41DE,
         $1ADAD47D, $6DDDE4EB, $F4D4B551, $83D385C7,
         $136C9856, $646BA8C0, $FD62F97A, $8A65C9EC,
         $14015C4F, $63066CD9, $FA0F3D63, $8D080DF5,
         $3B6E20C8, $4C69105E, $D56041E4, $A2677172,
         $3C03E4D1, $4B04D447, $D20D85FD, $A50AB56B,
         $35B5A8FA, $42B2986C, $DBBBC9D6, $ACBCF940,
         $32D86CE3, $45DF5C75, $DCD60DCF, $ABD13D59,
         $26D930AC, $51DE003A, $C8D75180, $BFD06116,
         $21B4F4B5, $56B3C423, $CFBA9599, $B8BDA50F,
         $2802B89E, $5F058808, $C60CD9B2, $B10BE924,
         $2F6F7C87, $58684C11, $C1611DAB, $B6662D3D,
         $76DC4190, $01DB7106, $98D220BC, $EFD5102A,
         $71B18589, $06B6B51F, $9FBFE4A5, $E8B8D433,
         $7807C9A2, $0F00F934, $9609A88E, $E10E9818,
         $7F6A0DBB, $086D3D2D, $91646C97, $E6635C01,
         $6B6B51F4, $1C6C6162, $856530D8, $F262004E,
         $6C0695ED, $1B01A57B, $8208F4C1, $F50FC457,
         $65B0D9C6, $12B7E950, $8BBEB8EA, $FCB9887C,
         $62DD1DDF, $15DA2D49, $8CD37CF3, $FBD44C65,
         $4DB26158, $3AB551CE, $A3BC0074, $D4BB30E2,
         $4ADFA541, $3DD895D7, $A4D1C46D, $D3D6F4FB,
         $4369E96A, $346ED9FC, $AD678846, $DA60B8D0,
         $44042D73, $33031DE5, $AA0A4C5F, $DD0D7CC9,
         $5005713C, $270241AA, $BE0B1010, $C90C2086,
         $5768B525, $206F85B3, $B966D409, $CE61E49F,
         $5EDEF90E, $29D9C998, $B0D09822, $C7D7A8B4,
         $59B33D17, $2EB40D81, $B7BD5C3B, $C0BA6CAD,
         $EDB88320, $9ABFB3B6, $03B6E20C, $74B1D29A,
         $EAD54739, $9DD277AF, $04DB2615, $73DC1683,
         $E3630B12, $94643B84, $0D6D6A3E, $7A6A5AA8,
         $E40ECF0B, $9309FF9D, $0A00AE27, $7D079EB1,
         $F00F9344, $8708A3D2, $1E01F268, $6906C2FE,
         $F762575D, $806567CB, $196C3671, $6E6B06E7,
         $FED41B76, $89D32BE0, $10DA7A5A, $67DD4ACC,
         $F9B9DF6F, $8EBEEFF9, $17B7BE43, $60B08ED5,
         $D6D6A3E8, $A1D1937E, $38D8C2C4, $4FDFF252,
         $D1BB67F1, $A6BC5767, $3FB506DD, $48B2364B,
         $D80D2BDA, $AF0A1B4C, $36034AF6, $41047A60,
         $DF60EFC3, $A867DF55, $316E8EEF, $4669BE79,
         $CB61B38C, $BC66831A, $256FD2A0, $5268E236,
         $CC0C7795, $BB0B4703, $220216B9, $5505262F,
         $C5BA3BBE, $B2BD0B28, $2BB45A92, $5CB36A04,
         $C2D7FFA7, $B5D0CF31, $2CD99E8B, $5BDEAE1D,
         $9B64C2B0, $EC63F226, $756AA39C, $026D930A,
         $9C0906A9, $EB0E363F, $72076785, $05005713,
         $95BF4A82, $E2B87A14, $7BB12BAE, $0CB61B38,
         $92D28E9B, $E5D5BE0D, $7CDCEFB7, $0BDBDF21,
         $86D3D2D4, $F1D4E242, $68DDB3F8, $1FDA836E,
         $81BE16CD, $F6B9265B, $6FB077E1, $18B74777,
         $88085AE6, $FF0F6A70, $66063BCA, $11010B5C,
         $8F659EFF, $F862AE69, $616BFFD3, $166CCF45,
         $A00AE278, $D70DD2EE, $4E048354, $3903B3C2,
         $A7672661, $D06016F7, $4969474D, $3E6E77DB,
         $AED16A4A, $D9D65ADC, $40DF0B66, $37D83BF0,
         $A9BCAE53, $DEBB9EC5, $47B2CF7F, $30B5FFE9,
         $BDBDF21C, $CABAC28A, $53B39330, $24B4A3A6,
         $BAD03605, $CDD70693, $54DE5729, $23D967BF,
         $B3667A2E, $C4614AB8, $5D681B02, $2A6F2B94,
         $B40BBE37, $C30C8EA1, $5A05DF1B, $2D02EF8D,
         $74726F50, $736E6F69, $706F4320, $67697279,
         $28207468, $31202963, $20393939, $48207962,
         $6E656761, $64655220, $6E616D64, $6FBBA36E);

function CRC32(CRC: LongWord; const Data: Pointer; DataSize: {$IFDEF WIN64}NativeUInt{$ELSE}LongWord{$ENDIF}): LongWord;
var
  B: PByte;//^Byte;
  I: LongWord;
  BB: Byte;
begin
  Result := CRC;
  if Data = nil then Exit;
  if DataSize = 0 then Exit;
  B := Data;
  for I := 0 to DataSize - 1 do
  begin
    BB := B^;
    Result := (Result shr 8) xor CRCTable[BB xor (Result and $000000FF)];
    Inc(B);
  end;
end;

//function CalcCRC32(p: PByte; length: NativeUInt): dword;
//var
//  i: Integer;
//begin
//  Result := $FFFFFFFF;
//  for i := 0 to length-1 do
//  begin
//    Result := (Result shr 8) xor CRCTable[ pbyte(p)^ xor (Result and $000000FF) ];
//    Inc(p);
//  end;
//  Result := not Result;
//end;

function GetTestVector: PAnsiChar; register;
const
  Vect: array [0..39] of Byte = (
    $30,$44,$ED,$6E,$45,$A4,$96,$F5,
    $F6,$35,$A2,$EB,$3D,$1A,$5D,$D6,
    $CB,$1D,$09,$82,$2D,$BD,$F5,$60,
    $C2,$B8,$58,$A1,$91,$F9,$81,$B1,
    $00,$00,$00,$00,$00,$00,$00,$00);
begin
  Result := @Vect[0];
end;
{$ELSE}
function CRC32(CRC: LongWord; const Data: Pointer; DataSize: LongWord): LongWord; assembler;
asm
         AND    EDX,EDX
         JZ     @Exit
         JCXZ   @Exit
         PUSH   EBX
@Start:
         MOVZX  EBX,AL
         XOR    BL,[EDX]
         SHR    EAX,8
         XOR    EAX,CS:[EBX * 4 + OFFSET @CRC32]
         INC    EDX
         DEC    ECX
         JNZ    @Start
         POP    EBX
@Exit:   RET

@CRC32:  DD 000000000h, 077073096h, 0EE0E612Ch, 0990951BAh
         DD 0076DC419h, 0706AF48Fh, 0E963A535h, 09E6495A3h
         DD 00EDB8832h, 079DCB8A4h, 0E0D5E91Eh, 097D2D988h
         DD 009B64C2Bh, 07EB17CBDh, 0E7B82D07h, 090BF1D91h
         DD 01DB71064h, 06AB020F2h, 0F3B97148h, 084BE41DEh
         DD 01ADAD47Dh, 06DDDE4EBh, 0F4D4B551h, 083D385C7h
         DD 0136C9856h, 0646BA8C0h, 0FD62F97Ah, 08A65C9ECh
         DD 014015C4Fh, 063066CD9h, 0FA0F3D63h, 08D080DF5h
         DD 03B6E20C8h, 04C69105Eh, 0D56041E4h, 0A2677172h
         DD 03C03E4D1h, 04B04D447h, 0D20D85FDh, 0A50AB56Bh
         DD 035B5A8FAh, 042B2986Ch, 0DBBBC9D6h, 0ACBCF940h
         DD 032D86CE3h, 045DF5C75h, 0DCD60DCFh, 0ABD13D59h
         DD 026D930ACh, 051DE003Ah, 0C8D75180h, 0BFD06116h
         DD 021B4F4B5h, 056B3C423h, 0CFBA9599h, 0B8BDA50Fh
         DD 02802B89Eh, 05F058808h, 0C60CD9B2h, 0B10BE924h
         DD 02F6F7C87h, 058684C11h, 0C1611DABh, 0B6662D3Dh
         DD 076DC4190h, 001DB7106h, 098D220BCh, 0EFD5102Ah
         DD 071B18589h, 006B6B51Fh, 09FBFE4A5h, 0E8B8D433h
         DD 07807C9A2h, 00F00F934h, 09609A88Eh, 0E10E9818h
         DD 07F6A0DBBh, 0086D3D2Dh, 091646C97h, 0E6635C01h
         DD 06B6B51F4h, 01C6C6162h, 0856530D8h, 0F262004Eh
         DD 06C0695EDh, 01B01A57Bh, 08208F4C1h, 0F50FC457h
         DD 065B0D9C6h, 012B7E950h, 08BBEB8EAh, 0FCB9887Ch
         DD 062DD1DDFh, 015DA2D49h, 08CD37CF3h, 0FBD44C65h
         DD 04DB26158h, 03AB551CEh, 0A3BC0074h, 0D4BB30E2h
         DD 04ADFA541h, 03DD895D7h, 0A4D1C46Dh, 0D3D6F4FBh
         DD 04369E96Ah, 0346ED9FCh, 0AD678846h, 0DA60B8D0h
         DD 044042D73h, 033031DE5h, 0AA0A4C5Fh, 0DD0D7CC9h
         DD 05005713Ch, 0270241AAh, 0BE0B1010h, 0C90C2086h
         DD 05768B525h, 0206F85B3h, 0B966D409h, 0CE61E49Fh
         DD 05EDEF90Eh, 029D9C998h, 0B0D09822h, 0C7D7A8B4h
         DD 059B33D17h, 02EB40D81h, 0B7BD5C3Bh, 0C0BA6CADh
         DD 0EDB88320h, 09ABFB3B6h, 003B6E20Ch, 074B1D29Ah
         DD 0EAD54739h, 09DD277AFh, 004DB2615h, 073DC1683h
         DD 0E3630B12h, 094643B84h, 00D6D6A3Eh, 07A6A5AA8h
         DD 0E40ECF0Bh, 09309FF9Dh, 00A00AE27h, 07D079EB1h
         DD 0F00F9344h, 08708A3D2h, 01E01F268h, 06906C2FEh
         DD 0F762575Dh, 0806567CBh, 0196C3671h, 06E6B06E7h
         DD 0FED41B76h, 089D32BE0h, 010DA7A5Ah, 067DD4ACCh
         DD 0F9B9DF6Fh, 08EBEEFF9h, 017B7BE43h, 060B08ED5h
         DD 0D6D6A3E8h, 0A1D1937Eh, 038D8C2C4h, 04FDFF252h
         DD 0D1BB67F1h, 0A6BC5767h, 03FB506DDh, 048B2364Bh
         DD 0D80D2BDAh, 0AF0A1B4Ch, 036034AF6h, 041047A60h
         DD 0DF60EFC3h, 0A867DF55h, 0316E8EEFh, 04669BE79h
         DD 0CB61B38Ch, 0BC66831Ah, 0256FD2A0h, 05268E236h
         DD 0CC0C7795h, 0BB0B4703h, 0220216B9h, 05505262Fh
         DD 0C5BA3BBEh, 0B2BD0B28h, 02BB45A92h, 05CB36A04h
         DD 0C2D7FFA7h, 0B5D0CF31h, 02CD99E8Bh, 05BDEAE1Dh
         DD 09B64C2B0h, 0EC63F226h, 0756AA39Ch, 0026D930Ah
         DD 09C0906A9h, 0EB0E363Fh, 072076785h, 005005713h
         DD 095BF4A82h, 0E2B87A14h, 07BB12BAEh, 00CB61B38h
         DD 092D28E9Bh, 0E5D5BE0Dh, 07CDCEFB7h, 00BDBDF21h
         DD 086D3D2D4h, 0F1D4E242h, 068DDB3F8h, 01FDA836Eh
         DD 081BE16CDh, 0F6B9265Bh, 06FB077E1h, 018B74777h
         DD 088085AE6h, 0FF0F6A70h, 066063BCAh, 011010B5Ch
         DD 08F659EFFh, 0F862AE69h, 0616BFFD3h, 0166CCF45h
         DD 0A00AE278h, 0D70DD2EEh, 04E048354h, 03903B3C2h
         DD 0A7672661h, 0D06016F7h, 04969474Dh, 03E6E77DBh
         DD 0AED16A4Ah, 0D9D65ADCh, 040DF0B66h, 037D83BF0h
         DD 0A9BCAE53h, 0DEBB9EC5h, 047B2CF7Fh, 030B5FFE9h
         DD 0BDBDF21Ch, 0CABAC28Ah, 053B39330h, 024B4A3A6h
         DD 0BAD03605h, 0CDD70693h, 054DE5729h, 023D967BFh
         DD 0B3667A2Eh, 0C4614AB8h, 05D681B02h, 02A6F2B94h
         DD 0B40BBE37h, 0C30C8EA1h, 05A05DF1Bh, 02D02EF8Dh
         DD 074726F50h, 0736E6F69h, 0706F4320h, 067697279h
         DD 028207468h, 031202963h, 020393939h, 048207962h
         DD 06E656761h, 064655220h, 06E616D64h, 06FBBA36Eh
end;

{a Random generated Testvector 256bit - 32 Bytes, it's used for Self Test}
function GetTestVector: PAnsiChar; assembler; register;
asm
         MOV   EAX,OFFSET @Vector
         RET
@Vector: DB    030h,044h,0EDh,06Eh,045h,0A4h,096h,0F5h
         DB    0F6h,035h,0A2h,0EBh,03Dh,01Ah,05Dh,0D6h
         DB    0CBh,01Dh,009h,082h,02Dh,0BDh,0F5h,060h
         DB    0C2h,0B8h,058h,0A1h,091h,0F9h,081h,0B1h
         DB    000h,000h,000h,000h,000h,000h,000h,000h
end;
{$ENDIF}

destructor THash.Destroy;
begin
  FillChar(DigestKey^, DigestKeySize, 0);
  inherited Destroy;
end;

procedure THash.Init;
begin
end;

procedure THash.Calc(const Data; DataSize: Integer);
begin
end;

procedure THash.Done;
begin
end;

function THash.DigestKey: Pointer;
begin
  Result := GetTestVector;
end;

class function THash.DigestKeySize: Integer;
begin
  Result := 0;
end;

function THash.GetDigestStr(Index: Integer): AnsiString;
begin
  if Index = 0 then Index := DefaultDigestStringFormat;
  case Index of
    16: Result := StrToBase16(PAnsiChar(DigestKey), DigestKeySize);
    64: Result := StrToBase64(PAnsiChar(DigestKey), DigestKeySize);
  else
    begin
      SetLength(Result, DigestKeySize);
      Move(DigestKey^, PAnsiChar(Result)^, DigestKeySize);
    end;
  end;
end;

class function THash.TestVector: Pointer;
begin
  Result := GetTestVector;
end;

class function THash.CalcStream(Digest: Pointer; const Stream: TStream; StreamSize: Integer): AnsiString;
var
  Buf: Pointer;
  BufSize: Integer;
  //Size: Integer;
  H: THash;
begin
  H := Create;
  with H do
  try
    Buf := AllocMem(HashMaxBufSize);
    Init;
    if StreamSize < 0 then
 {if Size < 0 then reset the Position, otherwise, calc with the specific
  Size and from the aktual Position in the Stream}
    begin
      Stream.Position := 0;
      StreamSize := Stream.Size;
    end;
    //Size := StreamSize;
    //DoProgress(H, 0, Size);
    repeat
      BufSize := StreamSize;
      if BufSize > HashMaxBufSize then BufSize := HashMaxBufSize;
      BufSize := Stream.Read(Buf^, BufSize);
      if BufSize <= 0 then Break;
      Calc(Buf^, BufSize);
      Dec(StreamSize, BufSize);
      //DoProgress(H, Size - StreamSize, Size);
    until BufSize <= 0;
    Done;
    if Digest <> nil then Move(DigestKey^, Digest^, DigestKeySize);
    Result := DigestString;
  finally
    //DoProgress(H, 0, 0);
    Free;
    ReallocMem(Buf, 0);
  end;
end;

class function THash.CalcString(Digest: Pointer; const Data: AnsiString): AnsiString;
begin
  with Self.Create do
  try
    Init;
    Calc(PAnsiChar(Data)^, Length(Data));
    Done;
    Result := DigestString;
    if Digest <> nil then Move(DigestKey^, Digest^, DigestKeySize);
  finally
    Free;
  end;
end;

class function THash.CalcFile(Digest: Pointer; const FileName: string): AnsiString;
var
  S: TFileStream;
begin
  S := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  try
    Result := CalcStream(Digest, S, -1);
  finally
    S.Free;
  end;
end;

class function THash.CalcBuffer(Digest: Pointer; const Buffer; BufferSize: Integer): AnsiString;
begin
  with Create do {create an object from my Classtype}
  try
    Init;
    Calc(Buffer, BufferSize);
    Done;
    if Digest <> nil then Move(DigestKey^, Digest^, DigestKeySize);
    Result := DigestString;
  finally
    Free; {destroy it}
  end;
end;

class function THash.SelfTest: Boolean;
var
  Test: AnsiString;
begin
  SetLength(Test, DigestKeySize);
  CalcBuffer(PAnsiChar(Test), GetTestVector^, 32);
  Result := InitTestIsOk and CompareMem(PAnsiChar(Test), TestVector, DigestKeySize);
end;

{get the CPU Type from your system}
function GetCPUType: Integer; assembler;
asm
  {$IFDEF WIN64}
        PUSH  RBX
        PUSH  RDI
        MOV   RDI, RCX
        MOV   EAX, 1
        CPUID
        MOV    AL,AH
        AND    EAX,0Fh
        POP   RDI
        POP   RBX
  {$ELSE}
         PUSH   EBX
         PUSH   ECX
         PUSH   EDX
         MOV    EBX,ESP
         AND    ESP,0FFFFFFFCh
         PUSHFD
         PUSHFD
         POP    EAX
         MOV    ECX,EAX
         XOR    EAX,40000h
         PUSH   EAX
         POPFD
         PUSHFD
         POP    EAX
         XOR    EAX,ECX
         MOV    EAX,3
         JE     @Exit
         PUSHFD
         POP    EAX
         MOV    ECX,EAX
         XOR    EAX,200000h
         PUSH   EAX
         POPFD
         PUSHFD
         POP    EAX
         XOR    EAX,ECX
         MOV    EAX,4
         JE     @Exit
         PUSH   EBX
         MOV    EAX,1
         {$IFDEF CONDITIONALEXPRESSIONS}
         CPUID
         {$ELSE}
         DB     0Fh,0A2h      //CPUID
         {$ENDIF}
         MOV    AL,AH
         AND    EAX,0Fh
         POP    EBX
@Exit:   POPFD
         MOV    ESP,EBX
         POP    EDX
         POP    ECX
         POP    EBX
  {$ENDIF}
end;

initialization
  FCPUType := GetCPUType;
  if FCPUType > 3 then
  begin
    SwapInteger := BSwapInt;
    SwapIntegerBuffer := BSwapIntBuf;
  end else
  begin
    SwapInteger := SwapInt;
    SwapIntegerBuffer := SwapIntBuf;
  end;
  {this calculate a Checksum (CRC32) over the function CRC32 and the TestVector,
   if InitTestIsOk = False any modification from Testvector or CRC32() detected, :-) }
  {$IFDEF PUREPASCAL}
  InitTestIsOk := CRC32(CRC32($29524828, @CRCTable, 1072), GetTestVector, 32) = $8D0C4372; {new magic number}
  {$ELSE}
  //this magic number is wrong, all saved CRC have to recalculates and save again
  //please use purepascal conditional to correct it
  InitTestIsOk := CRC32(CRC32($29524828, PAnsiChar(@CRC32) + 28, 1076), GetTestVector, 32) = $9D470592;
  {$ENDIF}

{$WRITEABLECONST OFF}
end.