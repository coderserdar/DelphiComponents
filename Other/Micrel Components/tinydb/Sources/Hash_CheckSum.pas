unit Hash_CheckSum;

{$I TinyDB.INC}

interface

uses Classes, HashBase;

type
  THash_XOR16 = class(THash)
  private
    FCRC: Word;
  protected
    class function TestVector: Pointer; override;
  public
    class function DigestKeySize: Integer; override;
    procedure Init; override;
    procedure Calc(const Data; DataSize: Integer); override;
    function DigestKey: Pointer; override;
  end;

  THash_XOR32 = class(THash)
  private
    FCRC: LongWord;
  protected
    class function TestVector: Pointer; override;
  public
    class function DigestKeySize: Integer; override;
    procedure Init; override;
    procedure Calc(const Data; DataSize: Integer); override;
    function DigestKey: Pointer; override;
  end;

  THash_CRC32 = class(THash_XOR32)
  private
  protected
    class function TestVector: Pointer; override;
  public
    procedure Init; override;
    procedure Calc(const Data; DataSize: Integer); override;
    procedure Done; override;
  end;

  THash_CRC16_CCITT = class(THash_XOR16)
  private
  protected
    class function TestVector: Pointer; override;
  public
    procedure Init; override;
    procedure Calc(const Data; DataSize: Integer); override;
  end;

  THash_CRC16_Standard = class(THash_XOR16)
  private
  protected
    class function TestVector: Pointer; override;
  public
    procedure Calc(const Data; DataSize: Integer); override;
  end;

implementation

uses SysUtils;

class function THash_XOR16.TestVector: Pointer;
{$IFDEF WIN64}
const
  Vect: array [0..1] of Byte = ($79,$E8);
begin
  Result := @Vect[0];
end;
{$ELSE}
asm
         MOV   EAX,OFFSET @Vector
         RET
@Vector: DB    079h,0E8h
end;
{$ENDIF}

class function THash_XOR16.DigestKeySize: Integer;
begin
  Result := 2;
end;

procedure THash_XOR16.Init;
begin
  FCRC := 0;
end;

{$IFDEF WIN64}
function ROR32(value: LongWord ; n: integer): LongWord;
begin
  Result := (value shr n) or (value shl (32-n))
end;
function ROL32(value: LongWord ; n: integer): LongWord;
begin
  Result := (value shl n) or (value shr (32-n))
end;
function ROR16(value: Word ; n: integer): LongWord;
begin
  Result := (value shr n) or (value shl (16-n))
end;
function ROL16(value: Word ; n: integer): LongWord;
begin
  Result := (value shl n) or (value shr (16-n))
end;
{$ENDIF}

procedure THash_XOR16.Calc(const Data; DataSize: Integer); assembler; register;
{$IFDEF WIN64}
var
  i: Integer;
  p: ^Byte;
  w: Word;
  b: Byte;
begin
  if DataSize = 0 then Exit;
  w := FCRC;
  p := Pointer(Data);
  for i := 0 to DataSize - 1 do
  begin
    ROL16(w, 5);
    b := Lo(w);
    b := b or p^;
    inc(p);
    w := 256*Hi(w)+b;
  end;
  FCRC := w;
end;
{$ELSE}
//EAX -> first parametr (can be self in object)
//EDX -> second parameter
//ECX -> third parameter
asm
         JECXZ   @Exit
         PUSH    EAX //self
         MOV     AX,[EAX].THash_XOR16.FCRC
@@1:     ROL     AX,5
         XOR     AL,[EDX]
         INC     EDX //point to data
         DEC     ECX //point to counter
         JNZ     @@1
         POP     EDX
         MOV     [EDX].THash_XOR16.FCRC,AX
@Exit:
end;
{$ENDIF}

function THash_XOR16.DigestKey: Pointer;
begin
  Result := @FCRC;
end;

class function THash_XOR32.TestVector: Pointer;
{$IFDEF WIN64}
const
  Vect: array [0..3] of Byte = ($8D, $AD, $89, $7F);
begin
  Result := @Vect[0];
end;
{$ELSE}
asm
         MOV   EAX,OFFSET @Vector
         RET
@Vector: DB    08Dh,0ADh,089h,07Fh
end;
{$ENDIF}

class function THash_XOR32.DigestKeySize: Integer;
begin
  Result := 4;
end;

procedure THash_XOR32.Init;
begin
  FCRC := 0;
end;

procedure THash_XOR32.Calc(const Data; DataSize: Integer); assembler; register;
{$IFDEF WIN64}
var
  i: Integer;
  p: ^Byte;
  w: LongWord;
  b: Byte;
begin
  if DataSize = 0 then Exit;
  w := FCRC;
  p := Pointer(Data);

  for i := 0 to DataSize - 1 do
  begin
    ROL32(w, 5);
    b := Lo(w);
    b := b or p^;
    inc(p);
    w := 256*Hi(w)+b;
  end;
  FCRC := w;
end;
{$ELSE}
//EAX -> first parametr (can be self in object)
//EDX -> second parameter
//ECX -> third parameter
asm
         JECXZ   @Exit
         PUSH    EAX
         MOV     EAX,[EAX].THash_XOR32.FCRC
         TEST    ECX,1
         JE      @@1
         XOR     AX,[EDX]
         INC     EDX
@@1:     SHR     ECX,1
         JECXZ   @@3
@@2:     ROL     EAX,5
         XOR     AX,[EDX]
         ADD     EDX,2
         DEC     ECX
         JNZ     @@2
@@3:     POP     EDX
         MOV     [EDX].THash_XOR32.FCRC,EAX
@Exit:
end;
{$ENDIF}

function THash_XOR32.DigestKey: Pointer;
begin
  Result := @FCRC;
end;

class function THash_CRC32.TestVector: Pointer;
{$IFDEF WIN64}
const
  Vect: array [0..3] of Byte = ($58, $EE, $1F, $31);
begin
  Result := @Vect[0];
end;
{$ELSE}
asm
         MOV   EAX,OFFSET @Vector
         RET
@Vector: DB    058h,0EEh,01Fh,031h
end;
{$ENDIF}

procedure THash_CRC32.Init;
begin
  FCRC := $FFFFFFFF;
end;

procedure THash_CRC32.Calc(const Data; DataSize: Integer); {$IFNDEF WIN64}assembler; register;{$ENDIF}
{$IFDEF WIN64}
begin
  HashBase.CRC32(FCRC, Pointer(Data), DataSize);
end;
{$ELSE}
asm
         PUSH   EAX
         MOV    EAX,[EAX].THash_CRC32.FCRC
         CALL   CRC32
         POP    EDX
         MOV    [EDX].THash_CRC32.FCRC,EAX
end;
{$ENDIF}

procedure THash_CRC32.Done;
begin
  FCRC := not FCRC;
end;

class function THash_CRC16_CCITT.TestVector: Pointer;
{$IFDEF WIN64}
const
  Vect: array [0..1] of Byte = ($B0,$D1);
begin
  Result := @Vect[0];
end;
{$ELSE}
asm
         MOV   EAX,OFFSET @Vector
         RET
@Vector: DB    0B0h,0D1h
end;
{$ENDIF}

procedure THash_CRC16_CCITT.Init;
begin
  FCRC := $FFFF;
end;


{$IFDEF WIN64}

{
function CRC16CCITT(bytes: TBytes): Word;
const
  polynomial = $1021;   // 0001 0000 0010 0001  (0, 5, 12)
var
  crc: Word;
  I, J: Integer;
  b: Byte;
  bit, c15: Boolean;
begin
  crc := $FFFF; // initial value
  for I := 0 to High(bytes) do
  begin
    b := bytes[I];
    for J := 0 to 7 do
    begin
      bit := (((b shr (7-J)) and 1) = 1);
      c15 := (((crc shr 15) and 1) = 1);
      crc := crc shl 1;
      if ((c15 xor bit) <> 0) then crc := crc xor polynomial;
    end;
  end;
  Result := crc and $FFFF;
end; }
function CRC16CCITT(Buffer:TBytes;Polynom,Initial:Cardinal):Cardinal;
var
  i,j: Integer;
begin
  Result:=Initial;
  for i:=Low(Buffer) to High(Buffer) do
  begin
    Result:=Result xor (ord(buffer[i]) shl 8);
    for j:=0 to 7 do
    begin
      if (Result and $8000)<>0 then Result:=(Result shl 1) xor Polynom
      else Result:=Result shl 1;
    end;
  end;
  Result:=Result and $ffff;
end;

procedure THash_CRC16_CCITT.Calc(const Data; DataSize: Integer);
var
  b: TBytes;
  a: ^TBytes;
  i: Integer;
begin
  a := Pointer(data);
  SetLength(b, DataSize);
  for i := 0 to DataSize - 1 do
  begin
    b[i] := Byte(a^[i]);
    inc(a);
  end;
  FCRC := CRC16CCITT(b, $1021, FCRC);
end;
{$ELSE}
procedure THash_CRC16_CCITT.Calc(const Data; DataSize: Integer);
asm
         OR     EDX,EDX
         JE     @Exit
         JCXZ   @Exit
         PUSH   EAX
         MOV    AX,[EAX].THash_CRC16_CCITT.FCRC
         PUSH   EBX
         XOR    EBX,EBX
@Start:
         MOV    BL,AH
         SHL    AX,8
         MOV    AL,[EDX]
         XOR    AX,CS:[EBX * 2 + OFFSET @CRC16]
         INC    EDX
         DEC    ECX
         JNZ    @Start

         POP    EBX
         POP    EDX
         MOV    [EDX].THash_CRC16_CCITT.FCRC,AX
@Exit:   RET

@CRC16:  DW     00000h, 01021h, 02042h, 03063h, 04084h, 050A5h, 060C6h, 070E7h
         DW     08108h, 09129h, 0A14Ah, 0B16Bh, 0C18Ch, 0D1ADh, 0E1CEh, 0F1EFh
         DW     01231h, 00210h, 03273h, 02252h, 052B5h, 04294h, 072F7h, 062D6h
         DW     09339h, 08318h, 0B37Bh, 0A35Ah, 0D3BDh, 0C39Ch, 0F3FFh, 0E3DEh
         DW     02462h, 03443h, 00420h, 01401h, 064E6h, 074C7h, 044A4h, 05485h
         DW     0A56Ah, 0B54Bh, 08528h, 09509h, 0E5EEh, 0F5CFh, 0C5ACh, 0D58Dh
         DW     03653h, 02672h, 01611h, 00630h, 076D7h, 066F6h, 05695h, 046B4h
         DW     0B75Bh, 0A77Ah, 09719h, 08738h, 0F7DFh, 0E7FEh, 0D79Dh, 0C7BCh
         DW     048C4h, 058E5h, 06886h, 078A7h, 00840h, 01861h, 02802h, 03823h
         DW     0C9CCh, 0D9EDh, 0E98Eh, 0F9AFh, 08948h, 09969h, 0A90Ah, 0B92Bh
         DW     05AF5h, 04AD4h, 07AB7h, 06A96h, 01A71h, 00A50h, 03A33h, 02A12h
         DW     0DBFDh, 0CBDCh, 0FBBFh, 0EB9Eh, 09B79h, 08B58h, 0BB3Bh, 0AB1Ah
         DW     06CA6h, 07C87h, 04CE4h, 05CC5h, 02C22h, 03C03h, 00C60h, 01C41h
         DW     0EDAEh, 0FD8Fh, 0CDECh, 0DDCDh, 0AD2Ah, 0BD0Bh, 08D68h, 09D49h
         DW     07E97h, 06EB6h, 05ED5h, 04EF4h, 03E13h, 02E32h, 01E51h, 00E70h
         DW     0FF9Fh, 0EFBEh, 0DFDDh, 0CFFCh, 0BF1Bh, 0AF3Ah, 09F59h, 08F78h
         DW     09188h, 081A9h, 0B1CAh, 0A1EBh, 0D10Ch, 0C12Dh, 0F14Eh, 0E16Fh
         DW     01080h, 000A1h, 030C2h, 020E3h, 05004h, 04025h, 07046h, 06067h
         DW     083B9h, 09398h, 0A3FBh, 0B3DAh, 0C33Dh, 0D31Ch, 0E37Fh, 0F35Eh
         DW     002B1h, 01290h, 022F3h, 032D2h, 04235h, 05214h, 06277h, 07256h
         DW     0B5EAh, 0A5CBh, 095A8h, 08589h, 0F56Eh, 0E54Fh, 0D52Ch, 0C50Dh
         DW     034E2h, 024C3h, 014A0h, 00481h, 07466h, 06447h, 05424h, 04405h
         DW     0A7DBh, 0B7FAh, 08799h, 097B8h, 0E75Fh, 0F77Eh, 0C71Dh, 0D73Ch
         DW     026D3h, 036F2h, 00691h, 016B0h, 06657h, 07676h, 04615h, 05634h
         DW     0D94Ch, 0C96Dh, 0F90Eh, 0E92Fh, 099C8h, 089E9h, 0B98Ah, 0A9ABh
         DW     05844h, 04865h, 07806h, 06827h, 018C0h, 008E1h, 03882h, 028A3h
         DW     0CB7Dh, 0DB5Ch, 0EB3Fh, 0FB1Eh, 08BF9h, 09BD8h, 0ABBBh, 0BB9Ah
         DW     04A75h, 05A54h, 06A37h, 07A16h, 00AF1h, 01AD0h, 02AB3h, 03A92h
         DW     0FD2Eh, 0ED0Fh, 0DD6Ch, 0CD4Dh, 0BDAAh, 0AD8Bh, 09DE8h, 08DC9h
         DW     07C26h, 06C07h, 05C64h, 04C45h, 03CA2h, 02C83h, 01CE0h, 00CC1h
         DW     0EF1Fh, 0FF3Eh, 0CF5Dh, 0DF7Ch, 0AF9Bh, 0BFBAh, 08FD9h, 09FF8h
         DW     06E17h, 07E36h, 04E55h, 05E74h, 02E93h, 03EB2h, 00ED1h, 01EF0h
end;
{$ENDIF}

class function THash_CRC16_Standard.TestVector: Pointer;
{$IFDEF WIN64}
const
  Vect: array [0..1] of Byte = ($ED,$75);
begin
  Result := @Vect[0];
end;
{$ELSE}
asm
         MOV   EAX,OFFSET @Vector
         RET
@Vector: DB    0EDh,075h
end;
{$ENDIF}

{$IFDEF WIN64}
const
    table_std_crc16:  ARRAY[0..255] OF WORD =
     ($0000,$C0C1,$C181,$0140,$C301,$03C0,$0280,$C241,$C601,$06C0,$0780,
      $C741,$0500,$C5C1,$C481,$0440,$CC01,$0CC0,$0D80,$CD41,$0F00,$CFC1,
      $CE81,$0E40,$0A00,$CAC1,$CB81,$0B40,$C901,$09C0,$0880,$C841,$D801,
      $18C0,$1980,$D941,$1B00,$DBC1,$DA81,$1A40,$1E00,$DEC1,$DF81,$1F40,
      $DD01,$1DC0,$1C80,$DC41,$1400,$D4C1,$D581,$1540,$D701,$17C0,$1680,
      $D641,$D201,$12C0,$1380,$D341,$1100,$D1C1,$D081,$1040,$F001,$30C0,
      $3180,$F141,$3300,$F3C1,$F281,$3240,$3600,$F6C1,$F781,$3740,$F501,
      $35C0,$3480,$F441,$3C00,$FCC1,$FD81,$3D40,$FF01,$3FC0,$3E80,$FE41,
      $FA01,$3AC0,$3B80,$FB41,$3900,$F9C1,$F881,$3840,$2800,$E8C1,$E981,
      $2940,$EB01,$2BC0,$2A80,$EA41,$EE01,$2EC0,$2F80,$EF41,$2D00,$EDC1,
      $EC81,$2C40,$E401,$24C0,$2580,$E541,$2700,$E7C1,$E681,$2640,$2200,
      $E2C1,$E381,$2340,$E101,$21C0,$2080,$E041,$A001,$60C0,$6180,$A141,
      $6300,$A3C1,$A281,$6240,$6600,$A6C1,$A781,$6740,$A501,$65C0,$6480,
      $A441,$6C00,$ACC1,$AD81,$6D40,$AF01,$6FC0,$6E80,$AE41,$AA01,$6AC0,
      $6B80,$AB41,$6900,$A9C1,$A881,$6840,$7800,$B8C1,$B981,$7940,$BB01,
      $7BC0,$7A80,$BA41,$BE01,$7EC0,$7F80,$BF41,$7D00,$BDC1,$BC81,$7C40,
      $B401,$74C0,$7580,$B541,$7700,$B7C1,$B681,$7640,$7200,$B2C1,$B381,
      $7340,$B101,$71C0,$7080,$B041,$5000,$90C1,$9181,$5140,$9301,$53C0,
      $5280,$9241,$9601,$56C0,$5780,$9741,$5500,$95C1,$9481,$5440,$9C01,
      $5CC0,$5D80,$9D41,$5F00,$9FC1,$9E81,$5E40,$5A00,$9AC1,$9B81,$5B40,
      $9901,$59C0,$5880,$9841,$8801,$48C0,$4980,$8941,$4B00,$8BC1,$8A81,
      $4A40,$4E00,$8EC1,$8F81,$4F40,$8D01,$4DC0,$4C80,$8C41,$4400,$84C1,
      $8581,$4540,$8701,$47C0,$4680,$8641,$8201,$42C0,$4380,$8341,$4100,
      $81C1,$8081,$4040);

type
  pbuffer = ^buffer;
  buffer = array of Byte;


procedure THash_CRC16_Standard.Calc(const Data; DataSize: Integer);
    VAR
      i:  WORD;
      q:  pbuffer;

   {The following is a little cryptic (but executes very quickly).
    The algorithm is as follows:
      1.  exclusive-or the input byte with the low-order byte of
          the CRC register to get an INDEX
      2.  shift the CRC register eight bits to the right
      3.  exclusive-or the CRC register with the contents of
          Table[INDEX]
      4.  repeat steps 1 through 3 for all bytes}
begin
    q := @data;
    FOR   i := 0 TO DataSize - 1 DO
      FCRC := Hi(FCRC)  XOR  table_std_crc16[ q^[i] XOR Lo(FCRC) ]
end;
{$ELSE}
procedure THash_CRC16_Standard.Calc(const Data; DataSize: Integer);
asm
         OR     EDX,EDX
         JE     @Exit
         JCXZ   @Exit
         PUSH   EAX
         MOV    AX,[EAX].THash_CRC16_Standard.FCRC
         PUSH   EBX
         XOR    EBX,EBX
@Start:
         MOV    BL,[EDX]
         XOR    BL,AL
         SHR    AX,8
         XOR    AX,CS:[EBX * 2 + OFFSET @CRC16]
         INC    EDX
         DEC    ECX
         JNZ    @Start

         POP    EBX
         POP    EDX
         MOV    [EDX].THash_CRC16_Standard.FCRC,AX
@Exit:   RET

@CRC16:  DW     00000h, 0C0C1h, 0C181h, 00140h, 0C301h, 003C0h, 00280h, 0C241h
         DW     0C601h, 006C0h, 00780h, 0C741h, 00500h, 0C5C1h, 0C481h, 00440h
         DW     0CC01h, 00CC0h, 00D80h, 0CD41h, 00F00h, 0CFC1h, 0CE81h, 00E40h
         DW     00A00h, 0CAC1h, 0CB81h, 00B40h, 0C901h, 009C0h, 00880h, 0C841h
         DW     0D801h, 018C0h, 01980h, 0D941h, 01B00h, 0DBC1h, 0DA81h, 01A40h
         DW     01E00h, 0DEC1h, 0DF81h, 01F40h, 0DD01h, 01DC0h, 01C80h, 0DC41h
         DW     01400h, 0D4C1h, 0D581h, 01540h, 0D701h, 017C0h, 01680h, 0D641h
         DW     0D201h, 012C0h, 01380h, 0D341h, 01100h, 0D1C1h, 0D081h, 01040h
         DW     0F001h, 030C0h, 03180h, 0F141h, 03300h, 0F3C1h, 0F281h, 03240h
         DW     03600h, 0F6C1h, 0F781h, 03740h, 0F501h, 035C0h, 03480h, 0F441h
         DW     03C00h, 0FCC1h, 0FD81h, 03D40h, 0FF01h, 03FC0h, 03E80h, 0FE41h
         DW     0FA01h, 03AC0h, 03B80h, 0FB41h, 03900h, 0F9C1h, 0F881h, 03840h
         DW     02800h, 0E8C1h, 0E981h, 02940h, 0EB01h, 02BC0h, 02A80h, 0EA41h
         DW     0EE01h, 02EC0h, 02F80h, 0EF41h, 02D00h, 0EDC1h, 0EC81h, 02C40h
         DW     0E401h, 024C0h, 02580h, 0E541h, 02700h, 0E7C1h, 0E681h, 02640h
         DW     02200h, 0E2C1h, 0E381h, 02340h, 0E101h, 021C0h, 02080h, 0E041h
         DW     0A001h, 060C0h, 06180h, 0A141h, 06300h, 0A3C1h, 0A281h, 06240h
         DW     06600h, 0A6C1h, 0A781h, 06740h, 0A501h, 065C0h, 06480h, 0A441h
         DW     06C00h, 0ACC1h, 0AD81h, 06D40h, 0AF01h, 06FC0h, 06E80h, 0AE41h
         DW     0AA01h, 06AC0h, 06B80h, 0AB41h, 06900h, 0A9C1h, 0A881h, 06840h
         DW     07800h, 0B8C1h, 0B981h, 07940h, 0BB01h, 07BC0h, 07A80h, 0BA41h
         DW     0BE01h, 07EC0h, 07F80h, 0BF41h, 07D00h, 0BDC1h, 0BC81h, 07C40h
         DW     0B401h, 074C0h, 07580h, 0B541h, 07700h, 0B7C1h, 0B681h, 07640h
         DW     07200h, 0B2C1h, 0B381h, 07340h, 0B101h, 071C0h, 07080h, 0B041h
         DW     05000h, 090C1h, 09181h, 05140h, 09301h, 053C0h, 05280h, 09241h
         DW     09601h, 056C0h, 05780h, 09741h, 05500h, 095C1h, 09481h, 05440h
         DW     09C01h, 05CC0h, 05D80h, 09D41h, 05F00h, 09FC1h, 09E81h, 05E40h
         DW     05A00h, 09AC1h, 09B81h, 05B40h, 09901h, 059C0h, 05880h, 09841h
         DW     08801h, 048C0h, 04980h, 08941h, 04B00h, 08BC1h, 08A81h, 04A40h
         DW     04E00h, 08EC1h, 08F81h, 04F40h, 08D01h, 04DC0h, 04C80h, 08C41h
         DW     04400h, 084C1h, 08581h, 04540h, 08701h, 047C0h, 04680h, 08641h
         DW     08201h, 042C0h, 04380h, 08341h, 04100h, 081C1h, 08081h, 04040h
end;
{$ENDIF}

end.

