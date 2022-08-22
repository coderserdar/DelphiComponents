unit Enc_Twofish;

{$I TinyDB.INC}

interface

uses Classes, Windows, SysUtils, TinyDB, EncryptBase;

type
  TEnc_Twofish = class(TEncrypt)
  protected
    class procedure GetContext(var ABufSize, AKeySize, AUserSize: Integer); override;
    class function TestVector: Pointer; override;
    procedure Encode(Data: Pointer); override;
    procedure Decode(Data: Pointer); override;
  public
    procedure Init(const Key; Size: Integer; IVector: Pointer); override;
  end;

{ TEncAlgo_Twofish }

  TEncAlgo_Twofish = class(TEncAlgo_Base)
  protected
    function GetEncryptObjectClass: TEncryptClass; override;
  end;

implementation

uses HashBase, Hash_SHA;

{$I Enc_Twofish.inc}

class procedure TEnc_Twofish.GetContext(var ABufSize, AKeySize, AUserSize: Integer);
begin
  ABufSize := 16;
  AKeySize := 32;
  AUserSize := 4256;
end;

class function TEnc_Twofish.TestVector: Pointer;
{$IFDEF WIN64}
const
  Vect: array [0..31] of Byte = (
  $4A, $07, $91, $E6, $BC, $2B, $4B, $AC,
  $B6, $55, $AE, $A1, $7F, $7D, $19, $AA,
  $CD, $88, $9D, $92, $45, $8A, $40, $93,
  $9F, $34, $32, $C0, $72, $E1, $8A, $E9
);
begin
  Result := @Vect[0];
end;
{$ELSE}
asm
         MOV   EAX,OFFSET @Vector
         RET
@Vector: DB    04Ah,007h,091h,0E6h,0BCh,02Bh,04Bh,0ACh
         DB    0B6h,055h,0AEh,0A1h,07Fh,07Dh,019h,0AAh
         DB    0CDh,088h,09Dh,092h,045h,08Ah,040h,093h
         DB    09Fh,034h,032h,0C0h,072h,0E1h,08Ah,0E9h
end;
{$ENDIF}

procedure TEnc_Twofish.Encode(Data: Pointer);
var
  I,X,Y: LongWord;
  A,B,C,D: LongWord;
  S: PLongword;
  Box: PIntArray;
begin
  S := User;
  Box := @PIntArray(User)[40];
  A := PIntArray(Data)[0] xor S^; Inc(S);
  B := PIntArray(Data)[1] xor S^; Inc(S);
  C := PIntArray(Data)[2] xor S^; Inc(S);
  D := PIntArray(Data)[3] xor S^;
  S := @PIntArray(User)[8];
  for I := 0 to 14 do
  begin
    X := Box[A shl  1 and $1FE] xor
         Box[A shr  7 and $1FE + 1] xor
         Box[A shr 15 and $1FE + 512] xor
         Box[A shr 23 and $1FE + 513];
    Y := Box[B shr 23 and $1FE] xor
         Box[B shl  1 and $1FE + 1] xor
         Box[B shr  7 and $1FE + 512] xor
         Box[B shr 15 and $1FE + 513];
    D := D shl 1 or D shr 31;
    C := C xor (X + Y       + S^); Inc(S);
    D := D xor (X + Y shl 1 + S^); Inc(S);
    C := C shr 1 or C shl 31;

    X := Box[C shl  1 and $1FE] xor
         Box[C shr  7 and $1FE + 1] xor
         Box[C shr 15 and $1FE + 512] xor
         Box[C shr 23 and $1FE + 513];
    Y := Box[D shr 23 and $1FE] xor
         Box[D shl  1 and $1FE + 1] xor
         Box[D shr  7 and $1FE + 512] xor
         Box[D shr 15 and $1FE + 513];
    B := B shl 1 or B shr 31;
    A := A xor (X + Y       + S^); Inc(S);
    B := B xor (X + Y shl 1 + S^); Dec(S);
    A := A shr 1 or A shl 31;
  end;
  S := @PIntArray(User)[4];
  PIntArray(Data)[0] := C xor S^; Inc(S);
  PIntArray(Data)[1] := D xor S^; Inc(S);
  PIntArray(Data)[2] := A xor S^; Inc(S);
  PIntArray(Data)[3] := B xor S^;
end;

procedure TEnc_Twofish.Decode(Data: Pointer);
var
  I,T0,T1: LongWord;
  A,B,C,D: LongWord;
  S: PLongword;
  Box: PIntArray;
begin
  S := @PIntArray(User)[4];
  Box := @PIntArray(User)[40];
  C := PIntArray(Data)[0] xor S^; Inc(S);
  D := PIntArray(Data)[1] xor S^; Inc(S);
  A := PIntArray(Data)[2] xor S^; Inc(S);
  B := PIntArray(Data)[3] xor S^;
  S := @PIntArray(User)[39];
  for I := 0 to 14 do
  begin
    T0 := Box[C shl  1 and $1FE] xor
          Box[C shr  7 and $1FE + 1] xor
          Box[C shr 15 and $1FE + 512] xor
          Box[C shr 23 and $1FE + 513];
    T1 := Box[D shr 23 and $1FE] xor
          Box[D shl  1 and $1FE + 1] xor
          Box[D shr  7 and $1FE + 512] xor
          Box[D shr 15 and $1FE + 513];
    A  := A shl 1 or A shr 31;
    B  := B xor (T0 + T1 shl 1 + S^); Dec(S);
    A  := A xor (T0 + T1       + S^); Dec(S);
    B  := B shr 1 or B shl 31;

    T0 := Box[A shl  1 and $1FE] xor
          Box[A shr  7 and $1FE + 1] xor
          Box[A shr 15 and $1FE + 512] xor
          Box[A shr 23 and $1FE + 513];
    T1 := Box[B shr 23 and $1FE] xor
          Box[B shl  1 and $1FE + 1] xor
          Box[B shr  7 and $1FE + 512] xor
          Box[B shr 15 and $1FE + 513];
    C  := C shl 1 or C shr 31;
    D  := D xor (T0 + T1 shl 1 + S^); Dec(S);
    C  := C xor (T0 + T1       + S^); Inc(S);
    D  := D shr 1 or D shl 31;
  end;
  S := User;
  PIntArray(Data)[0] := A xor S^; Inc(S);
  PIntArray(Data)[1] := B xor S^; Inc(S);
  PIntArray(Data)[2] := C xor S^; Inc(S);
  PIntArray(Data)[3] := D xor S^;
end;

procedure TEnc_Twofish.Init(const Key; Size: Integer; IVector: Pointer);
var
  BoxKey: array[0..3] of Integer;
  SubKey: PIntArray;
  Box: PIntArray;

  procedure SetupKey;

    function Encode(K0, K1: Integer): Integer;
    var
      R, I, J, G2, G3: Integer;
      B: byte;
    begin
      R := 0;
      for I := 0 to 1 do
      begin
        if I <> 0 then R := R xor K0 else R := R xor K1;
        for J := 0 to 3 do
        begin
          B := R shr 24;
          if B and $80 <> 0 then G2 := (B shl 1 xor $014D) and $FF
            else G2 := B shl 1 and $FF;
          if B and 1 <> 0 then G3 := (B shr 1 and $7F) xor $014D shr 1 xor G2
            else G3 := (B shr 1 and $7F) xor G2;
          R := R shl 8 xor G3 shl 24 xor G2 shl 16 xor G3 shl 8 xor B;
        end;
      end;
      Result := R;
    end;

    function F32(X: Integer; K: array of Integer): Integer;
    var
      A, B, C, D: Integer;
    begin
      A := X and $FF;
      B := X shr  8 and $FF;
      C := X shr 16 and $FF;
      D := X shr 24;
      if Size = 32 then
      begin
        A := Twofish_8x8[1, A] xor K[3] and $FF;
        B := Twofish_8x8[0, B] xor K[3] shr  8 and $FF;
        C := Twofish_8x8[0, C] xor K[3] shr 16 and $FF;
        D := Twofish_8x8[1, D] xor K[3] shr 24;
      end;
      if Size >= 24 then
      begin
        A := Twofish_8x8[1, A] xor K[2] and $FF;
        B := Twofish_8x8[1, B] xor K[2] shr  8 and $FF;
        C := Twofish_8x8[0, C] xor K[2] shr 16 and $FF;
        D := Twofish_8x8[0, D] xor K[2] shr 24;
      end;
      A := Twofish_8x8[0, A] xor K[1] and $FF;
      B := Twofish_8x8[1, B] xor K[1] shr  8 and $FF;
      C := Twofish_8x8[0, C] xor K[1] shr 16 and $FF;
      D := Twofish_8x8[1, D] xor K[1] shr 24;

      A := Twofish_8x8[0, A] xor K[0] and $FF;
      B := Twofish_8x8[0, B] xor K[0] shr  8 and $FF;
      C := Twofish_8x8[1, C] xor K[0] shr 16 and $FF;
      D := Twofish_8x8[1, D] xor K[0] shr 24;

      Result := Twofish_Data[0, A] xor Twofish_Data[1, B] xor
                Twofish_Data[2, C] xor Twofish_Data[3, D];
    end;

  var
    I,J,A,B: Integer;
    E,O: array[0..3] of Integer;
    K: array[0..7] of Integer;
  begin
    FillChar(K, SizeOf(K), 0);
    Move(Key, K, Size);
    if Size <= 16 then Size := 16 else
      if Size <= 24 then Size := 24
        else Size := 32;
    J := Size shr 3 - 1;
    for I := 0 to J do
    begin
      E[I] := K[I shl 1];
      O[I] := K[I shl 1 + 1];
      BoxKey[J] := Encode(E[I], O[I]);
      Dec(J);
    end;
    J := 0;
    for I := 0 to 19 do
    begin
      A := F32(J, E);
      B := ROL(F32(J + $01010101, O), 8);
      SubKey[I shl 1] := A + B;
      B := A + B shr 1;
      SubKey[I shl 1 + 1] := ROL(B, 9);
      Inc(J, $02020202);
    end;
  end;

  procedure DoXOR(D, S: PIntArray; Value: LongWord);
  var
    I: LongWord;
  begin
    Value := Value and $FF;
    for I := 0 to 63 do D[I] := S[I] xor (Value * $01010101);
  end;

  procedure SetupBox128;
  var
    L: array[0..255] of Byte;
    A,I: Integer;
  begin
    DoXOR(@L, @Twofish_8x8[0], BoxKey[1]);
    A := BoxKey[0] and $FF;
    for I := 0 to 255 do
      Box[I shl 1] := Twofish_Data[0, Twofish_8x8[0, L[I]] xor A];
    DoXOR(@L, @Twofish_8x8[1], BoxKey[1] shr 8);
    A := BoxKey[0] shr  8 and $FF;
    for I := 0 to 255 do
      Box[I shl 1 + 1] := Twofish_Data[1, Twofish_8x8[0, L[I]] xor A];
    DoXOR(@L, @Twofish_8x8[0], BoxKey[1] shr 16);
    A := BoxKey[0] shr 16 and $FF;
    for I := 0 to 255 do
      Box[I shl 1 + 512] := Twofish_Data[2, Twofish_8x8[1, L[I]] xor A];
    DoXOR(@L, @Twofish_8x8[1], BoxKey[1] shr 24);
    A := BoxKey[0] shr 24;
    for I := 0 to 255 do
      Box[I shl 1 + 513] := Twofish_Data[3, Twofish_8x8[1, L[I]] xor A];
  end;

  procedure SetupBox192;
  var
    L: array[0..255] of Byte;
    A,B,I: Integer;
  begin
    DoXOR(@L, @Twofish_8x8[1], BoxKey[2]);
    A := BoxKey[0] and $FF;
    B := BoxKey[1] and $FF;
    for I := 0 to 255 do
      Box[I shl 1] := Twofish_Data[0, Twofish_8x8[0, Twofish_8x8[0, L[I]] xor B] xor A];
    DoXOR(@L, @Twofish_8x8[1], BoxKey[2] shr 8);
    A := BoxKey[0] shr  8 and $FF;
    B := BoxKey[1] shr  8 and $FF;
    for I := 0 to 255 do
      Box[I shl 1 + 1] := Twofish_Data[1, Twofish_8x8[0, Twofish_8x8[1, L[I]] xor B] xor A];
    DoXOR(@L, @Twofish_8x8[0], BoxKey[2] shr 16);
    A := BoxKey[0] shr 16 and $FF;
    B := BoxKey[1] shr 16 and $FF;
    for I := 0 to 255 do
      Box[I shl 1 + 512] := Twofish_Data[2, Twofish_8x8[1, Twofish_8x8[0, L[I]] xor B] xor A];
    DoXOR(@L ,@Twofish_8x8[0], BoxKey[2] shr 24);
    A := BoxKey[0] shr 24;
    B := BoxKey[1] shr 24;
    for I := 0 to 255 do
      Box[I shl 1 + 513] := Twofish_Data[3, Twofish_8x8[1, Twofish_8x8[1, L[I]] xor B] xor A];
  end;

  procedure SetupBox256;
  var
    L: array[0..255] of Byte;
    K: array[0..255] of Byte;
    A,B,I: Integer;
  begin
    DoXOR(@K, @Twofish_8x8[1], BoxKey[3]);
    for I := 0 to 255 do L[I] := Twofish_8x8[1, K[I]];
    DoXOR(@L, @L, BoxKey[2]);
    A := BoxKey[0] and $FF;
    B := BoxKey[1] and $FF;
    for I := 0 to 255 do
      Box[I shl 1] := Twofish_Data[0, Twofish_8x8[0, Twofish_8x8[0, L[I]] xor B] xor A];
    DoXOR(@K, @Twofish_8x8[0], BoxKey[3] shr 8);
    for I := 0 to 255 do L[I] := Twofish_8x8[1, K[I]];
    DoXOR(@L, @L, BoxKey[2] shr 8);
    A := BoxKey[0] shr  8 and $FF;
    B := BoxKey[1] shr  8 and $FF;
    for I := 0 to 255 do
      Box[I shl 1 + 1] := Twofish_Data[1, Twofish_8x8[0, Twofish_8x8[1, L[I]] xor B] xor A];
    DoXOR(@K, @Twofish_8x8[0],BoxKey[3] shr 16);
    for I := 0 to 255 do L[I] := Twofish_8x8[0, K[I]];
    DoXOR(@L, @L, BoxKey[2] shr 16);
    A := BoxKey[0] shr 16 and $FF;
    B := BoxKey[1] shr 16 and $FF;
    for I := 0 to 255 do
      Box[I shl 1 + 512] := Twofish_Data[2, Twofish_8x8[1, Twofish_8x8[0, L[I]] xor B] xor A];
    DoXOR(@K, @Twofish_8x8[1], BoxKey[3] shr 24);
    for I := 0 to 255 do L[I] := Twofish_8x8[0, K[I]];
    DoXOR(@L, @L, BoxKey[2] shr 24);
    A := BoxKey[0] shr 24;
    B := BoxKey[1] shr 24;
    for I := 0 to 255 do
      Box[I shl 1 + 513] := Twofish_Data[3, Twofish_8x8[1, Twofish_8x8[1, L[I]] xor B] xor A];
  end;

begin
  InitBegin(Size);
  SubKey := User;
  Box    := @SubKey[40];
  SetupKey;
  if Size = 16 then SetupBox128 else
    if Size = 24 then SetupBox192
      else SetupBox256;
  InitEnd(IVector);
end;

{ TEncAlgo_Twofish }

function TEncAlgo_Twofish.GetEncryptObjectClass: TEncryptClass;
begin
  Result := TEnc_Twofish;
end;

initialization
  RegisterEncryptClass(TEncAlgo_Twofish, 'Twofish');
  
end.
