unit Hash_RipeMD;

{$I TinyDB.INC}

interface

uses Classes, HashBase, Hash_MD;

type
  THash_RipeMD128 = class(THash_MD4) {RACE Integrity Primitives Evaluation Message Digest}
  protected
    class function TestVector: Pointer; override;
    procedure Transform(Buffer: PIntArray); override;
  end;

  THash_RipeMD160 = class(THash_MD4)
  protected
    class function TestVector: Pointer; override;
    procedure Transform(Buffer: PIntArray); override;
  public
    {DigestKey-Size 160 bit}
    class function DigestKeySize: Integer; override;
  end;

  THash_RipeMD256 = class(THash_MD4)
  protected
    class function TestVector: Pointer; override;
    procedure Transform(Buffer: PIntArray); override;
  public
    {DigestKey-Size 256 bit}
    class function DigestKeySize: Integer; override;
    procedure Init; override;
  end;

  THash_RipeMD320 = class(THash_MD4)
  protected
    class function TestVector: Pointer; override;
    procedure Transform(Buffer: PIntArray); override;
  public
    {DigestKey-Size 320 bit}
    class function DigestKeySize: Integer; override;
  end;

implementation

uses SysUtils;

class function THash_RipeMD128.TestVector: Pointer;
{$IFDEF WIN64}
const
  Vect: array [0..15] of Byte = (
    $CF,$A0,$32,$CF,$D0,$8F,$87,$3A,
    $78,$DF,$13,$E7,$EB,$CD,$98,$0F
);
begin
  Result := @Vect[0];
end;
{$ELSE}
asm
         MOV   EAX,OFFSET @Vector
         RET
@Vector: DB    0CFh,0A0h,032h,0CFh,0D0h,08Fh,087h,03Ah
         DB    078h,0DFh,013h,0E7h,0EBh,0CDh,098h,00Fh
end;
{$ENDIF}

procedure THash_RipeMD128.Transform(Buffer: PIntArray);
var
  A1, B1, C1, D1: LongWord;
  A2, B2, C2, D2: LongWord;
begin
  A1 := FDigest[0];
  B1 := FDigest[1];
  C1 := FDigest[2];
  D1 := FDigest[3];
  A2 := A1;
  B2 := B1;
  C2 := C1;
  D2 := D1;

  Inc(A1, B1 xor C1 xor D1 + Buffer[ 0]); A1 := A1 shl 11 or A1 shr 21;
  Inc(D1, A1 xor B1 xor C1 + Buffer[ 1]); D1 := D1 shl 14 or D1 shr 18;
  Inc(C1, D1 xor A1 xor B1 + Buffer[ 2]); C1 := C1 shl 15 or C1 shr 17;
  Inc(B1, C1 xor D1 xor A1 + Buffer[ 3]); B1 := B1 shl 12 or B1 shr 20;
  Inc(A1, B1 xor C1 xor D1 + Buffer[ 4]); A1 := A1 shl  5 or A1 shr 27;
  Inc(D1, A1 xor B1 xor C1 + Buffer[ 5]); D1 := D1 shl  8 or D1 shr 24;
  Inc(C1, D1 xor A1 xor B1 + Buffer[ 6]); C1 := C1 shl  7 or C1 shr 25;
  Inc(B1, C1 xor D1 xor A1 + Buffer[ 7]); B1 := B1 shl  9 or B1 shr 23;
  Inc(A1, B1 xor C1 xor D1 + Buffer[ 8]); A1 := A1 shl 11 or A1 shr 21;
  Inc(D1, A1 xor B1 xor C1 + Buffer[ 9]); D1 := D1 shl 13 or D1 shr 19;
  Inc(C1, D1 xor A1 xor B1 + Buffer[10]); C1 := C1 shl 14 or C1 shr 18;
  Inc(B1, C1 xor D1 xor A1 + Buffer[11]); B1 := B1 shl 15 or B1 shr 17;
  Inc(A1, B1 xor C1 xor D1 + Buffer[12]); A1 := A1 shl  6 or A1 shr 26;
  Inc(D1, A1 xor B1 xor C1 + Buffer[13]); D1 := D1 shl  7 or D1 shr 25;
  Inc(C1, D1 xor A1 xor B1 + Buffer[14]); C1 := C1 shl  9 or C1 shr 23;
  Inc(B1, C1 xor D1 xor A1 + Buffer[15]); B1 := B1 shl  8 or B1 shr 24;

  Inc(A1, B1 and C1 or not B1 and D1 + Buffer[ 7] + $5A827999); A1 := A1 shl  7 or A1 shr 25;
  Inc(D1, A1 and B1 or not A1 and C1 + Buffer[ 4] + $5A827999); D1 := D1 shl  6 or D1 shr 26;
  Inc(C1, D1 and A1 or not D1 and B1 + Buffer[13] + $5A827999); C1 := C1 shl  8 or C1 shr 24;
  Inc(B1, C1 and D1 or not C1 and A1 + Buffer[ 1] + $5A827999); B1 := B1 shl 13 or B1 shr 19;
  Inc(A1, B1 and C1 or not B1 and D1 + Buffer[10] + $5A827999); A1 := A1 shl 11 or A1 shr 21;
  Inc(D1, A1 and B1 or not A1 and C1 + Buffer[ 6] + $5A827999); D1 := D1 shl  9 or D1 shr 23;
  Inc(C1, D1 and A1 or not D1 and B1 + Buffer[15] + $5A827999); C1 := C1 shl  7 or C1 shr 25;
  Inc(B1, C1 and D1 or not C1 and A1 + Buffer[ 3] + $5A827999); B1 := B1 shl 15 or B1 shr 17;
  Inc(A1, B1 and C1 or not B1 and D1 + Buffer[12] + $5A827999); A1 := A1 shl  7 or A1 shr 25;
  Inc(D1, A1 and B1 or not A1 and C1 + Buffer[ 0] + $5A827999); D1 := D1 shl 12 or D1 shr 20;
  Inc(C1, D1 and A1 or not D1 and B1 + Buffer[ 9] + $5A827999); C1 := C1 shl 15 or C1 shr 17;
  Inc(B1, C1 and D1 or not C1 and A1 + Buffer[ 5] + $5A827999); B1 := B1 shl  9 or B1 shr 23;
  Inc(A1, B1 and C1 or not B1 and D1 + Buffer[ 2] + $5A827999); A1 := A1 shl 11 or A1 shr 21;
  Inc(D1, A1 and B1 or not A1 and C1 + Buffer[14] + $5A827999); D1 := D1 shl  7 or D1 shr 25;
  Inc(C1, D1 and A1 or not D1 and B1 + Buffer[11] + $5A827999); C1 := C1 shl 13 or C1 shr 19;
  Inc(B1, C1 and D1 or not C1 and A1 + Buffer[ 8] + $5A827999); B1 := B1 shl 12 or B1 shr 20;

  Inc(A1, (B1 or not C1) xor D1 + Buffer[ 3] + $6ED9EBA1); A1 := A1 shl 11 or A1 shr 21;
  Inc(D1, (A1 or not B1) xor C1 + Buffer[10] + $6ED9EBA1); D1 := D1 shl 13 or D1 shr 19;
  Inc(C1, (D1 or not A1) xor B1 + Buffer[14] + $6ED9EBA1); C1 := C1 shl  6 or C1 shr 26;
  Inc(B1, (C1 or not D1) xor A1 + Buffer[ 4] + $6ED9EBA1); B1 := B1 shl  7 or B1 shr 25;
  Inc(A1, (B1 or not C1) xor D1 + Buffer[ 9] + $6ED9EBA1); A1 := A1 shl 14 or A1 shr 18;
  Inc(D1, (A1 or not B1) xor C1 + Buffer[15] + $6ED9EBA1); D1 := D1 shl  9 or D1 shr 23;
  Inc(C1, (D1 or not A1) xor B1 + Buffer[ 8] + $6ED9EBA1); C1 := C1 shl 13 or C1 shr 19;
  Inc(B1, (C1 or not D1) xor A1 + Buffer[ 1] + $6ED9EBA1); B1 := B1 shl 15 or B1 shr 17;
  Inc(A1, (B1 or not C1) xor D1 + Buffer[ 2] + $6ED9EBA1); A1 := A1 shl 14 or A1 shr 18;
  Inc(D1, (A1 or not B1) xor C1 + Buffer[ 7] + $6ED9EBA1); D1 := D1 shl  8 or D1 shr 24;
  Inc(C1, (D1 or not A1) xor B1 + Buffer[ 0] + $6ED9EBA1); C1 := C1 shl 13 or C1 shr 19;
  Inc(B1, (C1 or not D1) xor A1 + Buffer[ 6] + $6ED9EBA1); B1 := B1 shl  6 or B1 shr 26;
  Inc(A1, (B1 or not C1) xor D1 + Buffer[13] + $6ED9EBA1); A1 := A1 shl  5 or A1 shr 27;
  Inc(D1, (A1 or not B1) xor C1 + Buffer[11] + $6ED9EBA1); D1 := D1 shl 12 or D1 shr 20;
  Inc(C1, (D1 or not A1) xor B1 + Buffer[ 5] + $6ED9EBA1); C1 := C1 shl  7 or C1 shr 25;
  Inc(B1, (C1 or not D1) xor A1 + Buffer[12] + $6ED9EBA1); B1 := B1 shl  5 or B1 shr 27;

  Inc(A1, B1 and D1 or C1 and not D1 + Buffer[ 1] + $8F1BBCDC); A1 := A1 shl 11 or A1 shr 21;
  Inc(D1, A1 and C1 or B1 and not C1 + Buffer[ 9] + $8F1BBCDC); D1 := D1 shl 12 or D1 shr 20;
  Inc(C1, D1 and B1 or A1 and not B1 + Buffer[11] + $8F1BBCDC); C1 := C1 shl 14 or C1 shr 18;
  Inc(B1, C1 and A1 or D1 and not A1 + Buffer[10] + $8F1BBCDC); B1 := B1 shl 15 or B1 shr 17;
  Inc(A1, B1 and D1 or C1 and not D1 + Buffer[ 0] + $8F1BBCDC); A1 := A1 shl 14 or A1 shr 18;
  Inc(D1, A1 and C1 or B1 and not C1 + Buffer[ 8] + $8F1BBCDC); D1 := D1 shl 15 or D1 shr 17;
  Inc(C1, D1 and B1 or A1 and not B1 + Buffer[12] + $8F1BBCDC); C1 := C1 shl  9 or C1 shr 23;
  Inc(B1, C1 and A1 or D1 and not A1 + Buffer[ 4] + $8F1BBCDC); B1 := B1 shl  8 or B1 shr 24;
  Inc(A1, B1 and D1 or C1 and not D1 + Buffer[13] + $8F1BBCDC); A1 := A1 shl  9 or A1 shr 23;
  Inc(D1, A1 and C1 or B1 and not C1 + Buffer[ 3] + $8F1BBCDC); D1 := D1 shl 14 or D1 shr 18;
  Inc(C1, D1 and B1 or A1 and not B1 + Buffer[ 7] + $8F1BBCDC); C1 := C1 shl  5 or C1 shr 27;
  Inc(B1, C1 and A1 or D1 and not A1 + Buffer[15] + $8F1BBCDC); B1 := B1 shl  6 or B1 shr 26;
  Inc(A1, B1 and D1 or C1 and not D1 + Buffer[14] + $8F1BBCDC); A1 := A1 shl  8 or A1 shr 24;
  Inc(D1, A1 and C1 or B1 and not C1 + Buffer[ 5] + $8F1BBCDC); D1 := D1 shl  6 or D1 shr 26;
  Inc(C1, D1 and B1 or A1 and not B1 + Buffer[ 6] + $8F1BBCDC); C1 := C1 shl  5 or C1 shr 27;
  Inc(B1, C1 and A1 or D1 and not A1 + Buffer[ 2] + $8F1BBCDC); B1 := B1 shl 12 or B1 shr 20;

  Inc(A2, B2 and D2 or C2 and not D2 + Buffer[ 5] + $50A28BE6); A2 := A2 shl  8 or A2 shr 24;
  Inc(D2, A2 and C2 or B2 and not C2 + Buffer[14] + $50A28BE6); D2 := D2 shl  9 or D2 shr 23;
  Inc(C2, D2 and B2 or A2 and not B2 + Buffer[ 7] + $50A28BE6); C2 := C2 shl  9 or C2 shr 23;
  Inc(B2, C2 and A2 or D2 and not A2 + Buffer[ 0] + $50A28BE6); B2 := B2 shl 11 or B2 shr 21;
  Inc(A2, B2 and D2 or C2 and not D2 + Buffer[ 9] + $50A28BE6); A2 := A2 shl 13 or A2 shr 19;
  Inc(D2, A2 and C2 or B2 and not C2 + Buffer[ 2] + $50A28BE6); D2 := D2 shl 15 or D2 shr 17;
  Inc(C2, D2 and B2 or A2 and not B2 + Buffer[11] + $50A28BE6); C2 := C2 shl 15 or C2 shr 17;
  Inc(B2, C2 and A2 or D2 and not A2 + Buffer[ 4] + $50A28BE6); B2 := B2 shl  5 or B2 shr 27;
  Inc(A2, B2 and D2 or C2 and not D2 + Buffer[13] + $50A28BE6); A2 := A2 shl  7 or A2 shr 25;
  Inc(D2, A2 and C2 or B2 and not C2 + Buffer[ 6] + $50A28BE6); D2 := D2 shl  7 or D2 shr 25;
  Inc(C2, D2 and B2 or A2 and not B2 + Buffer[15] + $50A28BE6); C2 := C2 shl  8 or C2 shr 24;
  Inc(B2, C2 and A2 or D2 and not A2 + Buffer[ 8] + $50A28BE6); B2 := B2 shl 11 or B2 shr 21;
  Inc(A2, B2 and D2 or C2 and not D2 + Buffer[ 1] + $50A28BE6); A2 := A2 shl 14 or A2 shr 18;
  Inc(D2, A2 and C2 or B2 and not C2 + Buffer[10] + $50A28BE6); D2 := D2 shl 14 or D2 shr 18;
  Inc(C2, D2 and B2 or A2 and not B2 + Buffer[ 3] + $50A28BE6); C2 := C2 shl 12 or C2 shr 20;
  Inc(B2, C2 and A2 or D2 and not A2 + Buffer[12] + $50A28BE6); B2 := B2 shl  6 or B2 shr 26;

  Inc(A2, (B2 or not C2) xor D2 + Buffer[ 6] + $5C4DD124); A2 := A2 shl  9 or A2 shr 23;
  Inc(D2, (A2 or not B2) xor C2 + Buffer[11] + $5C4DD124); D2 := D2 shl 13 or D2 shr 19;
  Inc(C2, (D2 or not A2) xor B2 + Buffer[ 3] + $5C4DD124); C2 := C2 shl 15 or C2 shr 17;
  Inc(B2, (C2 or not D2) xor A2 + Buffer[ 7] + $5C4DD124); B2 := B2 shl  7 or B2 shr 25;
  Inc(A2, (B2 or not C2) xor D2 + Buffer[ 0] + $5C4DD124); A2 := A2 shl 12 or A2 shr 20;
  Inc(D2, (A2 or not B2) xor C2 + Buffer[13] + $5C4DD124); D2 := D2 shl  8 or D2 shr 24;
  Inc(C2, (D2 or not A2) xor B2 + Buffer[ 5] + $5C4DD124); C2 := C2 shl  9 or C2 shr 23;
  Inc(B2, (C2 or not D2) xor A2 + Buffer[10] + $5C4DD124); B2 := B2 shl 11 or B2 shr 21;
  Inc(A2, (B2 or not C2) xor D2 + Buffer[14] + $5C4DD124); A2 := A2 shl  7 or A2 shr 25;
  Inc(D2, (A2 or not B2) xor C2 + Buffer[15] + $5C4DD124); D2 := D2 shl  7 or D2 shr 25;
  Inc(C2, (D2 or not A2) xor B2 + Buffer[ 8] + $5C4DD124); C2 := C2 shl 12 or C2 shr 20;
  Inc(B2, (C2 or not D2) xor A2 + Buffer[12] + $5C4DD124); B2 := B2 shl  7 or B2 shr 25;
  Inc(A2, (B2 or not C2) xor D2 + Buffer[ 4] + $5C4DD124); A2 := A2 shl  6 or A2 shr 26;
  Inc(D2, (A2 or not B2) xor C2 + Buffer[ 9] + $5C4DD124); D2 := D2 shl 15 or D2 shr 17;
  Inc(C2, (D2 or not A2) xor B2 + Buffer[ 1] + $5C4DD124); C2 := C2 shl 13 or C2 shr 19;
  Inc(B2, (C2 or not D2) xor A2 + Buffer[ 2] + $5C4DD124); B2 := B2 shl 11 or B2 shr 21;

  Inc(A2, B2 and C2 or not B2 and D2 + Buffer[15] + $6D703EF3); A2 := A2 shl  9 or A2 shr 23;
  Inc(D2, A2 and B2 or not A2 and C2 + Buffer[ 5] + $6D703EF3); D2 := D2 shl  7 or D2 shr 25;
  Inc(C2, D2 and A2 or not D2 and B2 + Buffer[ 1] + $6D703EF3); C2 := C2 shl 15 or C2 shr 17;
  Inc(B2, C2 and D2 or not C2 and A2 + Buffer[ 3] + $6D703EF3); B2 := B2 shl 11 or B2 shr 21;
  Inc(A2, B2 and C2 or not B2 and D2 + Buffer[ 7] + $6D703EF3); A2 := A2 shl  8 or A2 shr 24;
  Inc(D2, A2 and B2 or not A2 and C2 + Buffer[14] + $6D703EF3); D2 := D2 shl  6 or D2 shr 26;
  Inc(C2, D2 and A2 or not D2 and B2 + Buffer[ 6] + $6D703EF3); C2 := C2 shl  6 or C2 shr 26;
  Inc(B2, C2 and D2 or not C2 and A2 + Buffer[ 9] + $6D703EF3); B2 := B2 shl 14 or B2 shr 18;
  Inc(A2, B2 and C2 or not B2 and D2 + Buffer[11] + $6D703EF3); A2 := A2 shl 12 or A2 shr 20;
  Inc(D2, A2 and B2 or not A2 and C2 + Buffer[ 8] + $6D703EF3); D2 := D2 shl 13 or D2 shr 19;
  Inc(C2, D2 and A2 or not D2 and B2 + Buffer[12] + $6D703EF3); C2 := C2 shl  5 or C2 shr 27;
  Inc(B2, C2 and D2 or not C2 and A2 + Buffer[ 2] + $6D703EF3); B2 := B2 shl 14 or B2 shr 18;
  Inc(A2, B2 and C2 or not B2 and D2 + Buffer[10] + $6D703EF3); A2 := A2 shl 13 or A2 shr 19;
  Inc(D2, A2 and B2 or not A2 and C2 + Buffer[ 0] + $6D703EF3); D2 := D2 shl 13 or D2 shr 19;
  Inc(C2, D2 and A2 or not D2 and B2 + Buffer[ 4] + $6D703EF3); C2 := C2 shl  7 or C2 shr 25;
  Inc(B2, C2 and D2 or not C2 and A2 + Buffer[13] + $6D703EF3); B2 := B2 shl  5 or B2 shr 27;

  Inc(A2, B2 xor C2 xor D2 + Buffer[ 8]); A2 := A2 shl 15 or A2 shr 17;
  Inc(D2, A2 xor B2 xor C2 + Buffer[ 6]); D2 := D2 shl  5 or D2 shr 27;
  Inc(C2, D2 xor A2 xor B2 + Buffer[ 4]); C2 := C2 shl  8 or C2 shr 24;
  Inc(B2, C2 xor D2 xor A2 + Buffer[ 1]); B2 := B2 shl 11 or B2 shr 21;
  Inc(A2, B2 xor C2 xor D2 + Buffer[ 3]); A2 := A2 shl 14 or A2 shr 18;
  Inc(D2, A2 xor B2 xor C2 + Buffer[11]); D2 := D2 shl 14 or D2 shr 18;
  Inc(C2, D2 xor A2 xor B2 + Buffer[15]); C2 := C2 shl  6 or C2 shr 26;
  Inc(B2, C2 xor D2 xor A2 + Buffer[ 0]); B2 := B2 shl 14 or B2 shr 18;
  Inc(A2, B2 xor C2 xor D2 + Buffer[ 5]); A2 := A2 shl  6 or A2 shr 26;
  Inc(D2, A2 xor B2 xor C2 + Buffer[12]); D2 := D2 shl  9 or D2 shr 23;
  Inc(C2, D2 xor A2 xor B2 + Buffer[ 2]); C2 := C2 shl 12 or C2 shr 20;
  Inc(B2, C2 xor D2 xor A2 + Buffer[13]); B2 := B2 shl  9 or B2 shr 23;
  Inc(A2, B2 xor C2 xor D2 + Buffer[ 9]); A2 := A2 shl 12 or A2 shr 20;
  Inc(D2, A2 xor B2 xor C2 + Buffer[ 7]); D2 := D2 shl  5 or D2 shr 27;
  Inc(C2, D2 xor A2 xor B2 + Buffer[10]); C2 := C2 shl 15 or C2 shr 17;
  Inc(B2, C2 xor D2 xor A2 + Buffer[14]); B2 := B2 shl  8 or B2 shr 24;

  Inc(D2, C1 + FDigest[1]);
  FDigest[1] := FDigest[2] + D1 + A2;
  FDigest[2] := FDigest[3] + A1 + B2;
  FDigest[3] := FDIgest[0] + B1 + C2;
  FDigest[0] := D2;
end;

class function THash_RipeMD160.TestVector: Pointer;
{$IFDEF WIN64}
const
  Vect: array [0..19] of Byte = (
    $19,$54,$DE,$BC,$1B,$55,$35,$30,
    $08,$1D,$9B,$80,$70,$A0,$F2,$4A,
    $9D,$F7,$34,$04
);
begin
  Result := @Vect[0];
end;
{$ELSE}
asm
         MOV   EAX,OFFSET @Vector
         RET
@Vector: DB    019h,054h,0DEh,0BCh,01Bh,055h,035h,030h
         DB    008h,01Dh,09Bh,080h,070h,0A0h,0F2h,04Ah
         DB    09Dh,0F7h,034h,004h
end;
{$ENDIF}

procedure THash_RipeMD160.Transform(Buffer: PIntArray);
var
  A1, B1, C1, D1, E1: LongWord;
  A, B, C, D, E: LongWord;
begin
  A := FDigest[0];
  B := FDigest[1];
  C := FDigest[2];
  D := FDigest[3];
  E := FDigest[4];

  Inc(A, Buffer[ 0] + (B xor C xor D)); A := A shl 11 or A shr 21 + E; C := C shl 10 or C shr 22;
  Inc(E, Buffer[ 1] + (A xor B xor C)); E := E shl 14 or E shr 18 + D; B := B shl 10 or B shr 22;
  Inc(D, Buffer[ 2] + (E xor A xor B)); D := D shl 15 or D shr 17 + C; A := A shl 10 or A shr 22;
  Inc(C, Buffer[ 3] + (D xor E xor A)); C := C shl 12 or C shr 20 + B; E := E shl 10 or E shr 22;
  Inc(B, Buffer[ 4] + (C xor D xor E)); B := B shl  5 or B shr 27 + A; D := D shl 10 or D shr 22;
  Inc(A, Buffer[ 5] + (B xor C xor D)); A := A shl  8 or A shr 24 + E; C := C shl 10 or C shr 22;
  Inc(E, Buffer[ 6] + (A xor B xor C)); E := E shl  7 or E shr 25 + D; B := B shl 10 or B shr 22;
  Inc(D, Buffer[ 7] + (E xor A xor B)); D := D shl  9 or D shr 23 + C; A := A shl 10 or A shr 22;
  Inc(C, Buffer[ 8] + (D xor E xor A)); C := C shl 11 or C shr 21 + B; E := E shl 10 or E shr 22;
  Inc(B, Buffer[ 9] + (C xor D xor E)); B := B shl 13 or B shr 19 + A; D := D shl 10 or D shr 22;
  Inc(A, Buffer[10] + (B xor C xor D)); A := A shl 14 or A shr 18 + E; C := C shl 10 or C shr 22;
  Inc(E, Buffer[11] + (A xor B xor C)); E := E shl 15 or E shr 17 + D; B := B shl 10 or B shr 22;
  Inc(D, Buffer[12] + (E xor A xor B)); D := D shl  6 or D shr 26 + C; A := A shl 10 or A shr 22;
  Inc(C, Buffer[13] + (D xor E xor A)); C := C shl  7 or C shr 25 + B; E := E shl 10 or E shr 22;
  Inc(B, Buffer[14] + (C xor D xor E)); B := B shl  9 or B shr 23 + A; D := D shl 10 or D shr 22;
  Inc(A, Buffer[15] + (B xor C xor D)); A := A shl  8 or A shr 24 + E; C := C shl 10 or C shr 22;

  Inc(E, Buffer[ 7] + $5A827999 + ((A and B) or (not A and C))); E := E shl  7 or E shr 25 + D; B := B shl 10 or B shr 22;
  Inc(D, Buffer[ 4] + $5A827999 + ((E and A) or (not E and B))); D := D shl  6 or D shr 26 + C; A := A shl 10 or A shr 22;
  Inc(C, Buffer[13] + $5A827999 + ((D and E) or (not D and A))); C := C shl  8 or C shr 24 + B; E := E shl 10 or E shr 22;
  Inc(B, Buffer[ 1] + $5A827999 + ((C and D) or (not C and E))); B := B shl 13 or B shr 19 + A; D := D shl 10 or D shr 22;
  Inc(A, Buffer[10] + $5A827999 + ((B and C) or (not B and D))); A := A shl 11 or A shr 21 + E; C := C shl 10 or C shr 22;
  Inc(E, Buffer[ 6] + $5A827999 + ((A and B) or (not A and C))); E := E shl  9 or E shr 23 + D; B := B shl 10 or B shr 22;
  Inc(D, Buffer[15] + $5A827999 + ((E and A) or (not E and B))); D := D shl  7 or D shr 25 + C; A := A shl 10 or A shr 22;
  Inc(C, Buffer[ 3] + $5A827999 + ((D and E) or (not D and A))); C := C shl 15 or C shr 17 + B; E := E shl 10 or E shr 22;
  Inc(B, Buffer[12] + $5A827999 + ((C and D) or (not C and E))); B := B shl  7 or B shr 25 + A; D := D shl 10 or D shr 22;
  Inc(A, Buffer[ 0] + $5A827999 + ((B and C) or (not B and D))); A := A shl 12 or A shr 20 + E; C := C shl 10 or C shr 22;
  Inc(E, Buffer[ 9] + $5A827999 + ((A and B) or (not A and C))); E := E shl 15 or E shr 17 + D; B := B shl 10 or B shr 22;
  Inc(D, Buffer[ 5] + $5A827999 + ((E and A) or (not E and B))); D := D shl  9 or D shr 23 + C; A := A shl 10 or A shr 22;
  Inc(C, Buffer[ 2] + $5A827999 + ((D and E) or (not D and A))); C := C shl 11 or C shr 21 + B; E := E shl 10 or E shr 22;
  Inc(B, Buffer[14] + $5A827999 + ((C and D) or (not C and E))); B := B shl  7 or B shr 25 + A; D := D shl 10 or D shr 22;
  Inc(A, Buffer[11] + $5A827999 + ((B and C) or (not B and D))); A := A shl 13 or A shr 19 + E; C := C shl 10 or C shr 22;
  Inc(E, Buffer[ 8] + $5A827999 + ((A and B) or (not A and C))); E := E shl 12 or E shr 20 + D; B := B shl 10 or B shr 22;

  Inc(D, Buffer[ 3] + $6ED9EBA1 + ((E or not A) xor B)); D := D shl 11 or D shr 21 + C; A := A shl 10 or A shr 22;
  Inc(C, Buffer[10] + $6ED9EBA1 + ((D or not E) xor A)); C := C shl 13 or C shr 19 + B; E := E shl 10 or E shr 22;
  Inc(B, Buffer[14] + $6ED9EBA1 + ((C or not D) xor E)); B := B shl  6 or B shr 26 + A; D := D shl 10 or D shr 22;
  Inc(A, Buffer[ 4] + $6ED9EBA1 + ((B or not C) xor D)); A := A shl  7 or A shr 25 + E; C := C shl 10 or C shr 22;
  Inc(E, Buffer[ 9] + $6ED9EBA1 + ((A or not B) xor C)); E := E shl 14 or E shr 18 + D; B := B shl 10 or B shr 22;
  Inc(D, Buffer[15] + $6ED9EBA1 + ((E or not A) xor B)); D := D shl  9 or D shr 23 + C; A := A shl 10 or A shr 22;
  Inc(C, Buffer[ 8] + $6ED9EBA1 + ((D or not E) xor A)); C := C shl 13 or C shr 19 + B; E := E shl 10 or E shr 22;
  Inc(B, Buffer[ 1] + $6ED9EBA1 + ((C or not D) xor E)); B := B shl 15 or B shr 17 + A; D := D shl 10 or D shr 22;
  Inc(A, Buffer[ 2] + $6ED9EBA1 + ((B or not C) xor D)); A := A shl 14 or A shr 18 + E; C := C shl 10 or C shr 22;
  Inc(E, Buffer[ 7] + $6ED9EBA1 + ((A or not B) xor C)); E := E shl  8 or E shr 24 + D; B := B shl 10 or B shr 22;
  Inc(D, Buffer[ 0] + $6ED9EBA1 + ((E or not A) xor B)); D := D shl 13 or D shr 19 + C; A := A shl 10 or A shr 22;
  Inc(C, Buffer[ 6] + $6ED9EBA1 + ((D or not E) xor A)); C := C shl  6 or C shr 26 + B; E := E shl 10 or E shr 22;
  Inc(B, Buffer[13] + $6ED9EBA1 + ((C or not D) xor E)); B := B shl  5 or B shr 27 + A; D := D shl 10 or D shr 22;
  Inc(A, Buffer[11] + $6ED9EBA1 + ((B or not C) xor D)); A := A shl 12 or A shr 20 + E; C := C shl 10 or C shr 22;
  Inc(E, Buffer[ 5] + $6ED9EBA1 + ((A or not B) xor C)); E := E shl  7 or E shr 25 + D; B := B shl 10 or B shr 22;
  Inc(D, Buffer[12] + $6ED9EBA1 + ((E or not A) xor B)); D := D shl  5 or D shr 27 + C; A := A shl 10 or A shr 22;

  Inc(C, Buffer[ 1] + $8F1BBCDC + ((D and A) or (E and not A))); C := C shl 11 or C shr 21 + B; E := E shl 10 or E shr 22;
  Inc(B, Buffer[ 9] + $8F1BBCDC + ((C and E) or (D and not E))); B := B shl 12 or B shr 20 + A; D := D shl 10 or D shr 22;
  Inc(A, Buffer[11] + $8F1BBCDC + ((B and D) or (C and not D))); A := A shl 14 or A shr 18 + E; C := C shl 10 or C shr 22;
  Inc(E, Buffer[10] + $8F1BBCDC + ((A and C) or (B and not C))); E := E shl 15 or E shr 17 + D; B := B shl 10 or B shr 22;
  Inc(D, Buffer[ 0] + $8F1BBCDC + ((E and B) or (A and not B))); D := D shl 14 or D shr 18 + C; A := A shl 10 or A shr 22;
  Inc(C, Buffer[ 8] + $8F1BBCDC + ((D and A) or (E and not A))); C := C shl 15 or C shr 17 + B; E := E shl 10 or E shr 22;
  Inc(B, Buffer[12] + $8F1BBCDC + ((C and E) or (D and not E))); B := B shl  9 or B shr 23 + A; D := D shl 10 or D shr 22;
  Inc(A, Buffer[ 4] + $8F1BBCDC + ((B and D) or (C and not D))); A := A shl  8 or A shr 24 + E; C := C shl 10 or C shr 22;
  Inc(E, Buffer[13] + $8F1BBCDC + ((A and C) or (B and not C))); E := E shl  9 or E shr 23 + D; B := B shl 10 or B shr 22;
  Inc(D, Buffer[ 3] + $8F1BBCDC + ((E and B) or (A and not B))); D := D shl 14 or D shr 18 + C; A := A shl 10 or A shr 22;
  Inc(C, Buffer[ 7] + $8F1BBCDC + ((D and A) or (E and not A))); C := C shl  5 or C shr 27 + B; E := E shl 10 or E shr 22;
  Inc(B, Buffer[15] + $8F1BBCDC + ((C and E) or (D and not E))); B := B shl  6 or B shr 26 + A; D := D shl 10 or D shr 22;
  Inc(A, Buffer[14] + $8F1BBCDC + ((B and D) or (C and not D))); A := A shl  8 or A shr 24 + E; C := C shl 10 or C shr 22;
  Inc(E, Buffer[ 5] + $8F1BBCDC + ((A and C) or (B and not C))); E := E shl  6 or E shr 26 + D; B := B shl 10 or B shr 22;
  Inc(D, Buffer[ 6] + $8F1BBCDC + ((E and B) or (A and not B))); D := D shl  5 or D shr 27 + C; A := A shl 10 or A shr 22;
  Inc(C, Buffer[ 2] + $8F1BBCDC + ((D and A) or (E and not A))); C := C shl 12 or C shr 20 + B; E := E shl 10 or E shr 22;

  Inc(B, Buffer[ 4] + $A953FD4E + (C xor (D or not E))); B := B shl  9 or B shr 23 + A; D := D shl 10 or D shr 22;
  Inc(A, Buffer[ 0] + $A953FD4E + (B xor (C or not D))); A := A shl 15 or A shr 17 + E; C := C shl 10 or C shr 22;
  Inc(E, Buffer[ 5] + $A953FD4E + (A xor (B or not C))); E := E shl  5 or E shr 27 + D; B := B shl 10 or B shr 22;
  Inc(D, Buffer[ 9] + $A953FD4E + (E xor (A or not B))); D := D shl 11 or D shr 21 + C; A := A shl 10 or A shr 22;
  Inc(C, Buffer[ 7] + $A953FD4E + (D xor (E or not A))); C := C shl  6 or C shr 26 + B; E := E shl 10 or E shr 22;
  Inc(B, Buffer[12] + $A953FD4E + (C xor (D or not E))); B := B shl  8 or B shr 24 + A; D := D shl 10 or D shr 22;
  Inc(A, Buffer[ 2] + $A953FD4E + (B xor (C or not D))); A := A shl 13 or A shr 19 + E; C := C shl 10 or C shr 22;
  Inc(E, Buffer[10] + $A953FD4E + (A xor (B or not C))); E := E shl 12 or E shr 20 + D; B := B shl 10 or B shr 22;
  Inc(D, Buffer[14] + $A953FD4E + (E xor (A or not B))); D := D shl  5 or D shr 27 + C; A := A shl 10 or A shr 22;
  Inc(C, Buffer[ 1] + $A953FD4E + (D xor (E or not A))); C := C shl 12 or C shr 20 + B; E := E shl 10 or E shr 22;
  Inc(B, Buffer[ 3] + $A953FD4E + (C xor (D or not E))); B := B shl 13 or B shr 19 + A; D := D shl 10 or D shr 22;
  Inc(A, Buffer[ 8] + $A953FD4E + (B xor (C or not D))); A := A shl 14 or A shr 18 + E; C := C shl 10 or C shr 22;
  Inc(E, Buffer[11] + $A953FD4E + (A xor (B or not C))); E := E shl 11 or E shr 21 + D; B := B shl 10 or B shr 22;
  Inc(D, Buffer[ 6] + $A953FD4E + (E xor (A or not B))); D := D shl  8 or D shr 24 + C; A := A shl 10 or A shr 22;
  Inc(C, Buffer[15] + $A953FD4E + (D xor (E or not A))); C := C shl  5 or C shr 27 + B; E := E shl 10 or E shr 22;
  Inc(B, Buffer[13] + $A953FD4E + (C xor (D or not E))); B := B shl  6 or B shr 26 + A; D := D shl 10 or D shr 22;

  A1 := A;
  B1 := B;
  C1 := C;
  D1 := D;
  E1 := E;

  A := FDigest[0];
  B := FDigest[1];
  C := FDigest[2];
  D := FDigest[3];
  E := FDigest[4];

  Inc(A, Buffer[ 5] + $50A28BE6 + (B xor (C or not D))); A := A shl  8 or A shr 24 + E; C := C shl 10 or C shr 22;
  Inc(E, Buffer[14] + $50A28BE6 + (A xor (B or not C))); E := E shl  9 or E shr 23 + D; B := B shl 10 or B shr 22;
  Inc(D, Buffer[ 7] + $50A28BE6 + (E xor (A or not B))); D := D shl  9 or D shr 23 + C; A := A shl 10 or A shr 22;
  Inc(C, Buffer[ 0] + $50A28BE6 + (D xor (E or not A))); C := C shl 11 or C shr 21 + B; E := E shl 10 or E shr 22;
  Inc(B, Buffer[ 9] + $50A28BE6 + (C xor (D or not E))); B := B shl 13 or B shr 19 + A; D := D shl 10 or D shr 22;
  Inc(A, Buffer[ 2] + $50A28BE6 + (B xor (C or not D))); A := A shl 15 or A shr 17 + E; C := C shl 10 or C shr 22;
  Inc(E, Buffer[11] + $50A28BE6 + (A xor (B or not C))); E := E shl 15 or E shr 17 + D; B := B shl 10 or B shr 22;
  Inc(D, Buffer[ 4] + $50A28BE6 + (E xor (A or not B))); D := D shl  5 or D shr 27 + C; A := A shl 10 or A shr 22;
  Inc(C, Buffer[13] + $50A28BE6 + (D xor (E or not A))); C := C shl  7 or C shr 25 + B; E := E shl 10 or E shr 22;
  Inc(B, Buffer[ 6] + $50A28BE6 + (C xor (D or not E))); B := B shl  7 or B shr 25 + A; D := D shl 10 or D shr 22;
  Inc(A, Buffer[15] + $50A28BE6 + (B xor (C or not D))); A := A shl  8 or A shr 24 + E; C := C shl 10 or C shr 22;
  Inc(E, Buffer[ 8] + $50A28BE6 + (A xor (B or not C))); E := E shl 11 or E shr 21 + D; B := B shl 10 or B shr 22;
  Inc(D, Buffer[ 1] + $50A28BE6 + (E xor (A or not B))); D := D shl 14 or D shr 18 + C; A := A shl 10 or A shr 22;
  Inc(C, Buffer[10] + $50A28BE6 + (D xor (E or not A))); C := C shl 14 or C shr 18 + B; E := E shl 10 or E shr 22;
  Inc(B, Buffer[ 3] + $50A28BE6 + (C xor (D or not E))); B := B shl 12 or B shr 20 + A; D := D shl 10 or D shr 22;
  Inc(A, Buffer[12] + $50A28BE6 + (B xor (C or not D))); A := A shl  6 or A shr 26 + E; C := C shl 10 or C shr 22;

  Inc(E, Buffer[ 6] + $5C4DD124 + ((A and C) or (B and not C))); E := E shl  9 or E shr 23 + D; B := B shl 10 or B shr 22;
  Inc(D, Buffer[11] + $5C4DD124 + ((E and B) or (A and not B))); D := D shl 13 or D shr 19 + C; A := A shl 10 or A shr 22;
  Inc(C, Buffer[ 3] + $5C4DD124 + ((D and A) or (E and not A))); C := C shl 15 or C shr 17 + B; E := E shl 10 or E shr 22;
  Inc(B, Buffer[ 7] + $5C4DD124 + ((C and E) or (D and not E))); B := B shl  7 or B shr 25 + A; D := D shl 10 or D shr 22;
  Inc(A, Buffer[ 0] + $5C4DD124 + ((B and D) or (C and not D))); A := A shl 12 or A shr 20 + E; C := C shl 10 or C shr 22;
  Inc(E, Buffer[13] + $5C4DD124 + ((A and C) or (B and not C))); E := E shl  8 or E shr 24 + D; B := B shl 10 or B shr 22;
  Inc(D, Buffer[ 5] + $5C4DD124 + ((E and B) or (A and not B))); D := D shl  9 or D shr 23 + C; A := A shl 10 or A shr 22;
  Inc(C, Buffer[10] + $5C4DD124 + ((D and A) or (E and not A))); C := C shl 11 or C shr 21 + B; E := E shl 10 or E shr 22;
  Inc(B, Buffer[14] + $5C4DD124 + ((C and E) or (D and not E))); B := B shl  7 or B shr 25 + A; D := D shl 10 or D shr 22;
  Inc(A, Buffer[15] + $5C4DD124 + ((B and D) or (C and not D))); A := A shl  7 or A shr 25 + E; C := C shl 10 or C shr 22;
  Inc(E, Buffer[ 8] + $5C4DD124 + ((A and C) or (B and not C))); E := E shl 12 or E shr 20 + D; B := B shl 10 or B shr 22;
  Inc(D, Buffer[12] + $5C4DD124 + ((E and B) or (A and not B))); D := D shl  7 or D shr 25 + C; A := A shl 10 or A shr 22;
  Inc(C, Buffer[ 4] + $5C4DD124 + ((D and A) or (E and not A))); C := C shl  6 or C shr 26 + B; E := E shl 10 or E shr 22;
  Inc(B, Buffer[ 9] + $5C4DD124 + ((C and E) or (D and not E))); B := B shl 15 or B shr 17 + A; D := D shl 10 or D shr 22;
  Inc(A, Buffer[ 1] + $5C4DD124 + ((B and D) or (C and not D))); A := A shl 13 or A shr 19 + E; C := C shl 10 or C shr 22;
  Inc(E, Buffer[ 2] + $5C4DD124 + ((A and C) or (B and not C))); E := E shl 11 or E shr 21 + D; B := B shl 10 or B shr 22;

  Inc(D, Buffer[15] + $6D703EF3 + ((E or not A) xor B)); D := D shl  9 or D shr 23 + C; A := A shl 10 or A shr 22;
  Inc(C, Buffer[ 5] + $6D703EF3 + ((D or not E) xor A)); C := C shl  7 or C shr 25 + B; E := E shl 10 or E shr 22;
  Inc(B, Buffer[ 1] + $6D703EF3 + ((C or not D) xor E)); B := B shl 15 or B shr 17 + A; D := D shl 10 or D shr 22;
  Inc(A, Buffer[ 3] + $6D703EF3 + ((B or not C) xor D)); A := A shl 11 or A shr 21 + E; C := C shl 10 or C shr 22;
  Inc(E, Buffer[ 7] + $6D703EF3 + ((A or not B) xor C)); E := E shl  8 or E shr 24 + D; B := B shl 10 or B shr 22;
  Inc(D, Buffer[14] + $6D703EF3 + ((E or not A) xor B)); D := D shl  6 or D shr 26 + C; A := A shl 10 or A shr 22;
  Inc(C, Buffer[ 6] + $6D703EF3 + ((D or not E) xor A)); C := C shl  6 or C shr 26 + B; E := E shl 10 or E shr 22;
  Inc(B, Buffer[ 9] + $6D703EF3 + ((C or not D) xor E)); B := B shl 14 or B shr 18 + A; D := D shl 10 or D shr 22;
  Inc(A, Buffer[11] + $6D703EF3 + ((B or not C) xor D)); A := A shl 12 or A shr 20 + E; C := C shl 10 or C shr 22;
  Inc(E, Buffer[ 8] + $6D703EF3 + ((A or not B) xor C)); E := E shl 13 or E shr 19 + D; B := B shl 10 or B shr 22;
  Inc(D, Buffer[12] + $6D703EF3 + ((E or not A) xor B)); D := D shl  5 or D shr 27 + C; A := A shl 10 or A shr 22;
  Inc(C, Buffer[ 2] + $6D703EF3 + ((D or not E) xor A)); C := C shl 14 or C shr 18 + B; E := E shl 10 or E shr 22;
  Inc(B, Buffer[10] + $6D703EF3 + ((C or not D) xor E)); B := B shl 13 or B shr 19 + A; D := D shl 10 or D shr 22;
  Inc(A, Buffer[ 0] + $6D703EF3 + ((B or not C) xor D)); A := A shl 13 or A shr 19 + E; C := C shl 10 or C shr 22;
  Inc(E, Buffer[ 4] + $6D703EF3 + ((A or not B) xor C)); E := E shl  7 or E shr 25 + D; B := B shl 10 or B shr 22;
  Inc(D, Buffer[13] + $6D703EF3 + ((E or not A) xor B)); D := D shl  5 or D shr 27 + C; A := A shl 10 or A shr 22;

  Inc(C, Buffer[ 8] + $7A6D76E9 + ((D and E) or (not D and A))); C := C shl 15 or C shr 17 + B; E := E shl 10 or E shr 22;
  Inc(B, Buffer[ 6] + $7A6D76E9 + ((C and D) or (not C and E))); B := B shl  5 or B shr 27 + A; D := D shl 10 or D shr 22;
  Inc(A, Buffer[ 4] + $7A6D76E9 + ((B and C) or (not B and D))); A := A shl  8 or A shr 24 + E; C := C shl 10 or C shr 22;
  Inc(E, Buffer[ 1] + $7A6D76E9 + ((A and B) or (not A and C))); E := E shl 11 or E shr 21 + D; B := B shl 10 or B shr 22;
  Inc(D, Buffer[ 3] + $7A6D76E9 + ((E and A) or (not E and B))); D := D shl 14 or D shr 18 + C; A := A shl 10 or A shr 22;
  Inc(C, Buffer[11] + $7A6D76E9 + ((D and E) or (not D and A))); C := C shl 14 or C shr 18 + B; E := E shl 10 or E shr 22;
  Inc(B, Buffer[15] + $7A6D76E9 + ((C and D) or (not C and E))); B := B shl  6 or B shr 26 + A; D := D shl 10 or D shr 22;
  Inc(A, Buffer[ 0] + $7A6D76E9 + ((B and C) or (not B and D))); A := A shl 14 or A shr 18 + E; C := C shl 10 or C shr 22;
  Inc(E, Buffer[ 5] + $7A6D76E9 + ((A and B) or (not A and C))); E := E shl  6 or E shr 26 + D; B := B shl 10 or B shr 22;
  Inc(D, Buffer[12] + $7A6D76E9 + ((E and A) or (not E and B))); D := D shl  9 or D shr 23 + C; A := A shl 10 or A shr 22;
  Inc(C, Buffer[ 2] + $7A6D76E9 + ((D and E) or (not D and A))); C := C shl 12 or C shr 20 + B; E := E shl 10 or E shr 22;
  Inc(B, Buffer[13] + $7A6D76E9 + ((C and D) or (not C and E))); B := B shl  9 or B shr 23 + A; D := D shl 10 or D shr 22;
  Inc(A, Buffer[ 9] + $7A6D76E9 + ((B and C) or (not B and D))); A := A shl 12 or A shr 20 + E; C := C shl 10 or C shr 22;
  Inc(E, Buffer[ 7] + $7A6D76E9 + ((A and B) or (not A and C))); E := E shl  5 or E shr 27 + D; B := B shl 10 or B shr 22;
  Inc(D, Buffer[10] + $7A6D76E9 + ((E and A) or (not E and B))); D := D shl 15 or D shr 17 + C; A := A shl 10 or A shr 22;
  Inc(C, Buffer[14] + $7A6D76E9 + ((D and E) or (not D and A))); C := C shl  8 or C shr 24 + B; E := E shl 10 or E shr 22;

  Inc(B, Buffer[12] + (C xor D xor E)); B := B shl  8 or B shr 24 + A; D := D shl 10 or D shr 22;
  Inc(A, Buffer[15] + (B xor C xor D)); A := A shl  5 or A shr 27 + E; C := C shl 10 or C shr 22;
  Inc(E, Buffer[10] + (A xor B xor C)); E := E shl 12 or E shr 20 + D; B := B shl 10 or B shr 22;
  Inc(D, Buffer[ 4] + (E xor A xor B)); D := D shl  9 or D shr 23 + C; A := A shl 10 or A shr 22;
  Inc(C, Buffer[ 1] + (D xor E xor A)); C := C shl 12 or C shr 20 + B; E := E shl 10 or E shr 22;
  Inc(B, Buffer[ 5] + (C xor D xor E)); B := B shl  5 or B shr 27 + A; D := D shl 10 or D shr 22;
  Inc(A, Buffer[ 8] + (B xor C xor D)); A := A shl 14 or A shr 18 + E; C := C shl 10 or C shr 22;
  Inc(E, Buffer[ 7] + (A xor B xor C)); E := E shl  6 or E shr 26 + D; B := B shl 10 or B shr 22;
  Inc(D, Buffer[ 6] + (E xor A xor B)); D := D shl  8 or D shr 24 + C; A := A shl 10 or A shr 22;
  Inc(C, Buffer[ 2] + (D xor E xor A)); C := C shl 13 or C shr 19 + B; E := E shl 10 or E shr 22;
  Inc(B, Buffer[13] + (C xor D xor E)); B := B shl  6 or B shr 26 + A; D := D shl 10 or D shr 22;
  Inc(A, Buffer[14] + (B xor C xor D)); A := A shl  5 or A shr 27 + E; C := C shl 10 or C shr 22;
  Inc(E, Buffer[ 0] + (A xor B xor C)); E := E shl 15 or E shr 17 + D; B := B shl 10 or B shr 22;
  Inc(D, Buffer[ 3] + (E xor A xor B)); D := D shl 13 or D shr 19 + C; A := A shl 10 or A shr 22;
  Inc(C, Buffer[ 9] + (D xor E xor A)); C := C shl 11 or C shr 21 + B; E := E shl 10 or E shr 22;
  Inc(B, Buffer[11] + (C xor D xor E)); B := B shl 11 or B shr 21 + A; D := D shl 10 or D shr 22;

  Inc(D, C1 + FDigest[1]);
  FDigest[1] := FDigest[2] + D1 + E;
  FDigest[2] := FDigest[3] + E1 + A;
  FDigest[3] := FDigest[4] + A1 + B;
  FDigest[4] := FDigest[0] + B1 + C;
  FDigest[0] := D;
end;

class function THash_RipeMD160.DigestKeySize: Integer;
begin
  Result := 20;
end;

class function THash_RipeMD256.TestVector: Pointer;
{$IFDEF WIN64}
const
  Vect: array [0..31] of Byte = (
    $C3,$B1,$D7,$AC,$A8,$9A,$47,$7A,
    $38,$D3,$6D,$39,$EF,$00,$FB,$45,
    $FC,$4E,$C3,$1A,$71,$21,$DB,$9E,
    $1C,$76,$C5,$DE,$99,$88,$18,$C2
);
begin
  Result := @Vect[0];
end;
{$ELSE}
asm
         MOV   EAX,OFFSET @Vector
         RET
@Vector: DB    0C3h,0B1h,0D7h,0ACh,0A8h,09Ah,047h,07Ah
         DB    038h,0D3h,06Dh,039h,0EFh,000h,0FBh,045h
         DB    0FCh,04Eh,0C3h,01Ah,071h,021h,0DBh,09Eh
         DB    01Ch,076h,0C5h,0DEh,099h,088h,018h,0C2h
end;
{$ENDIF}

procedure THash_RipeMD256.Transform(Buffer: PIntArray);
var
  A1, B1, C1, D1: LongWord;
  A2, B2, C2, D2: LongWord;
  T: LongWord;
begin
  A1 := FDigest[0];
  B1 := FDigest[1];
  C1 := FDigest[2];
  D1 := FDigest[3];
  A2 := FDigest[4];
  B2 := FDigest[5];
  C2 := FDigest[6];
  D2 := FDigest[7];

  Inc(A1, B1 xor C1 xor D1 + Buffer[ 0]); A1 := A1 shl 11 or A1 shr 21;
  Inc(D1, A1 xor B1 xor C1 + Buffer[ 1]); D1 := D1 shl 14 or D1 shr 18;
  Inc(C1, D1 xor A1 xor B1 + Buffer[ 2]); C1 := C1 shl 15 or C1 shr 17;
  Inc(B1, C1 xor D1 xor A1 + Buffer[ 3]); B1 := B1 shl 12 or B1 shr 20;
  Inc(A1, B1 xor C1 xor D1 + Buffer[ 4]); A1 := A1 shl  5 or A1 shr 27;
  Inc(D1, A1 xor B1 xor C1 + Buffer[ 5]); D1 := D1 shl  8 or D1 shr 24;
  Inc(C1, D1 xor A1 xor B1 + Buffer[ 6]); C1 := C1 shl  7 or C1 shr 25;
  Inc(B1, C1 xor D1 xor A1 + Buffer[ 7]); B1 := B1 shl  9 or B1 shr 23;
  Inc(A1, B1 xor C1 xor D1 + Buffer[ 8]); A1 := A1 shl 11 or A1 shr 21;
  Inc(D1, A1 xor B1 xor C1 + Buffer[ 9]); D1 := D1 shl 13 or D1 shr 19;
  Inc(C1, D1 xor A1 xor B1 + Buffer[10]); C1 := C1 shl 14 or C1 shr 18;
  Inc(B1, C1 xor D1 xor A1 + Buffer[11]); B1 := B1 shl 15 or B1 shr 17;
  Inc(A1, B1 xor C1 xor D1 + Buffer[12]); A1 := A1 shl  6 or A1 shr 26;
  Inc(D1, A1 xor B1 xor C1 + Buffer[13]); D1 := D1 shl  7 or D1 shr 25;
  Inc(C1, D1 xor A1 xor B1 + Buffer[14]); C1 := C1 shl  9 or C1 shr 23;
  Inc(B1, C1 xor D1 xor A1 + Buffer[15]); B1 := B1 shl  8 or B1 shr 24;

  Inc(A2, B2 and D2 or C2 and not D2 + Buffer[ 5] + $50A28BE6); A2 := A2 shl  8 or A2 shr 24;
  Inc(D2, A2 and C2 or B2 and not C2 + Buffer[14] + $50A28BE6); D2 := D2 shl  9 or D2 shr 23;
  Inc(C2, D2 and B2 or A2 and not B2 + Buffer[ 7] + $50A28BE6); C2 := C2 shl  9 or C2 shr 23;
  Inc(B2, C2 and A2 or D2 and not A2 + Buffer[ 0] + $50A28BE6); B2 := B2 shl 11 or B2 shr 21;
  Inc(A2, B2 and D2 or C2 and not D2 + Buffer[ 9] + $50A28BE6); A2 := A2 shl 13 or A2 shr 19;
  Inc(D2, A2 and C2 or B2 and not C2 + Buffer[ 2] + $50A28BE6); D2 := D2 shl 15 or D2 shr 17;
  Inc(C2, D2 and B2 or A2 and not B2 + Buffer[11] + $50A28BE6); C2 := C2 shl 15 or C2 shr 17;
  Inc(B2, C2 and A2 or D2 and not A2 + Buffer[ 4] + $50A28BE6); B2 := B2 shl  5 or B2 shr 27;
  Inc(A2, B2 and D2 or C2 and not D2 + Buffer[13] + $50A28BE6); A2 := A2 shl  7 or A2 shr 25;
  Inc(D2, A2 and C2 or B2 and not C2 + Buffer[ 6] + $50A28BE6); D2 := D2 shl  7 or D2 shr 25;
  Inc(C2, D2 and B2 or A2 and not B2 + Buffer[15] + $50A28BE6); C2 := C2 shl  8 or C2 shr 24;
  Inc(B2, C2 and A2 or D2 and not A2 + Buffer[ 8] + $50A28BE6); B2 := B2 shl 11 or B2 shr 21;
  Inc(A2, B2 and D2 or C2 and not D2 + Buffer[ 1] + $50A28BE6); A2 := A2 shl 14 or A2 shr 18;
  Inc(D2, A2 and C2 or B2 and not C2 + Buffer[10] + $50A28BE6); D2 := D2 shl 14 or D2 shr 18;
  Inc(C2, D2 and B2 or A2 and not B2 + Buffer[ 3] + $50A28BE6); C2 := C2 shl 12 or C2 shr 20;
  Inc(B2, C2 and A2 or D2 and not A2 + Buffer[12] + $50A28BE6); B2 := B2 shl  6 or B2 shr 26;

  T := A1; A1 := A2; A2 := T;

  Inc(A1, B1 and C1 or not B1 and D1 + Buffer[ 7] + $5A827999); A1 := A1 shl  7 or A1 shr 25;
  Inc(D1, A1 and B1 or not A1 and C1 + Buffer[ 4] + $5A827999); D1 := D1 shl  6 or D1 shr 26;
  Inc(C1, D1 and A1 or not D1 and B1 + Buffer[13] + $5A827999); C1 := C1 shl  8 or C1 shr 24;
  Inc(B1, C1 and D1 or not C1 and A1 + Buffer[ 1] + $5A827999); B1 := B1 shl 13 or B1 shr 19;
  Inc(A1, B1 and C1 or not B1 and D1 + Buffer[10] + $5A827999); A1 := A1 shl 11 or A1 shr 21;
  Inc(D1, A1 and B1 or not A1 and C1 + Buffer[ 6] + $5A827999); D1 := D1 shl  9 or D1 shr 23;
  Inc(C1, D1 and A1 or not D1 and B1 + Buffer[15] + $5A827999); C1 := C1 shl  7 or C1 shr 25;
  Inc(B1, C1 and D1 or not C1 and A1 + Buffer[ 3] + $5A827999); B1 := B1 shl 15 or B1 shr 17;
  Inc(A1, B1 and C1 or not B1 and D1 + Buffer[12] + $5A827999); A1 := A1 shl  7 or A1 shr 25;
  Inc(D1, A1 and B1 or not A1 and C1 + Buffer[ 0] + $5A827999); D1 := D1 shl 12 or D1 shr 20;
  Inc(C1, D1 and A1 or not D1 and B1 + Buffer[ 9] + $5A827999); C1 := C1 shl 15 or C1 shr 17;
  Inc(B1, C1 and D1 or not C1 and A1 + Buffer[ 5] + $5A827999); B1 := B1 shl  9 or B1 shr 23;
  Inc(A1, B1 and C1 or not B1 and D1 + Buffer[ 2] + $5A827999); A1 := A1 shl 11 or A1 shr 21;
  Inc(D1, A1 and B1 or not A1 and C1 + Buffer[14] + $5A827999); D1 := D1 shl  7 or D1 shr 25;
  Inc(C1, D1 and A1 or not D1 and B1 + Buffer[11] + $5A827999); C1 := C1 shl 13 or C1 shr 19;
  Inc(B1, C1 and D1 or not C1 and A1 + Buffer[ 8] + $5A827999); B1 := B1 shl 12 or B1 shr 20;

  Inc(A2, (B2 or not C2) xor D2 + Buffer[ 6] + $5C4DD124); A2 := A2 shl  9 or A2 shr 23;
  Inc(D2, (A2 or not B2) xor C2 + Buffer[11] + $5C4DD124); D2 := D2 shl 13 or D2 shr 19;
  Inc(C2, (D2 or not A2) xor B2 + Buffer[ 3] + $5C4DD124); C2 := C2 shl 15 or C2 shr 17;
  Inc(B2, (C2 or not D2) xor A2 + Buffer[ 7] + $5C4DD124); B2 := B2 shl  7 or B2 shr 25;
  Inc(A2, (B2 or not C2) xor D2 + Buffer[ 0] + $5C4DD124); A2 := A2 shl 12 or A2 shr 20;
  Inc(D2, (A2 or not B2) xor C2 + Buffer[13] + $5C4DD124); D2 := D2 shl  8 or D2 shr 24;
  Inc(C2, (D2 or not A2) xor B2 + Buffer[ 5] + $5C4DD124); C2 := C2 shl  9 or C2 shr 23;
  Inc(B2, (C2 or not D2) xor A2 + Buffer[10] + $5C4DD124); B2 := B2 shl 11 or B2 shr 21;
  Inc(A2, (B2 or not C2) xor D2 + Buffer[14] + $5C4DD124); A2 := A2 shl  7 or A2 shr 25;
  Inc(D2, (A2 or not B2) xor C2 + Buffer[15] + $5C4DD124); D2 := D2 shl  7 or D2 shr 25;
  Inc(C2, (D2 or not A2) xor B2 + Buffer[ 8] + $5C4DD124); C2 := C2 shl 12 or C2 shr 20;
  Inc(B2, (C2 or not D2) xor A2 + Buffer[12] + $5C4DD124); B2 := B2 shl  7 or B2 shr 25;
  Inc(A2, (B2 or not C2) xor D2 + Buffer[ 4] + $5C4DD124); A2 := A2 shl  6 or A2 shr 26;
  Inc(D2, (A2 or not B2) xor C2 + Buffer[ 9] + $5C4DD124); D2 := D2 shl 15 or D2 shr 17;
  Inc(C2, (D2 or not A2) xor B2 + Buffer[ 1] + $5C4DD124); C2 := C2 shl 13 or C2 shr 19;
  Inc(B2, (C2 or not D2) xor A2 + Buffer[ 2] + $5C4DD124); B2 := B2 shl 11 or B2 shr 21;

  T := B1; B1 := B2; B2 := T;

  Inc(A1, (B1 or not C1) xor D1 + Buffer[ 3] + $6ED9EBA1); A1 := A1 shl 11 or A1 shr 21;
  Inc(D1, (A1 or not B1) xor C1 + Buffer[10] + $6ED9EBA1); D1 := D1 shl 13 or D1 shr 19;
  Inc(C1, (D1 or not A1) xor B1 + Buffer[14] + $6ED9EBA1); C1 := C1 shl  6 or C1 shr 26;
  Inc(B1, (C1 or not D1) xor A1 + Buffer[ 4] + $6ED9EBA1); B1 := B1 shl  7 or B1 shr 25;
  Inc(A1, (B1 or not C1) xor D1 + Buffer[ 9] + $6ED9EBA1); A1 := A1 shl 14 or A1 shr 18;
  Inc(D1, (A1 or not B1) xor C1 + Buffer[15] + $6ED9EBA1); D1 := D1 shl  9 or D1 shr 23;
  Inc(C1, (D1 or not A1) xor B1 + Buffer[ 8] + $6ED9EBA1); C1 := C1 shl 13 or C1 shr 19;
  Inc(B1, (C1 or not D1) xor A1 + Buffer[ 1] + $6ED9EBA1); B1 := B1 shl 15 or B1 shr 17;
  Inc(A1, (B1 or not C1) xor D1 + Buffer[ 2] + $6ED9EBA1); A1 := A1 shl 14 or A1 shr 18;
  Inc(D1, (A1 or not B1) xor C1 + Buffer[ 7] + $6ED9EBA1); D1 := D1 shl  8 or D1 shr 24;
  Inc(C1, (D1 or not A1) xor B1 + Buffer[ 0] + $6ED9EBA1); C1 := C1 shl 13 or C1 shr 19;
  Inc(B1, (C1 or not D1) xor A1 + Buffer[ 6] + $6ED9EBA1); B1 := B1 shl  6 or B1 shr 26;
  Inc(A1, (B1 or not C1) xor D1 + Buffer[13] + $6ED9EBA1); A1 := A1 shl  5 or A1 shr 27;
  Inc(D1, (A1 or not B1) xor C1 + Buffer[11] + $6ED9EBA1); D1 := D1 shl 12 or D1 shr 20;
  Inc(C1, (D1 or not A1) xor B1 + Buffer[ 5] + $6ED9EBA1); C1 := C1 shl  7 or C1 shr 25;
  Inc(B1, (C1 or not D1) xor A1 + Buffer[12] + $6ED9EBA1); B1 := B1 shl  5 or B1 shr 27;

  Inc(A2, B2 and C2 or not B2 and D2 + Buffer[15] + $6D703EF3); A2 := A2 shl  9 or A2 shr 23;
  Inc(D2, A2 and B2 or not A2 and C2 + Buffer[ 5] + $6D703EF3); D2 := D2 shl  7 or D2 shr 25;
  Inc(C2, D2 and A2 or not D2 and B2 + Buffer[ 1] + $6D703EF3); C2 := C2 shl 15 or C2 shr 17;
  Inc(B2, C2 and D2 or not C2 and A2 + Buffer[ 3] + $6D703EF3); B2 := B2 shl 11 or B2 shr 21;
  Inc(A2, B2 and C2 or not B2 and D2 + Buffer[ 7] + $6D703EF3); A2 := A2 shl  8 or A2 shr 24;
  Inc(D2, A2 and B2 or not A2 and C2 + Buffer[14] + $6D703EF3); D2 := D2 shl  6 or D2 shr 26;
  Inc(C2, D2 and A2 or not D2 and B2 + Buffer[ 6] + $6D703EF3); C2 := C2 shl  6 or C2 shr 26;
  Inc(B2, C2 and D2 or not C2 and A2 + Buffer[ 9] + $6D703EF3); B2 := B2 shl 14 or B2 shr 18;
  Inc(A2, B2 and C2 or not B2 and D2 + Buffer[11] + $6D703EF3); A2 := A2 shl 12 or A2 shr 20;
  Inc(D2, A2 and B2 or not A2 and C2 + Buffer[ 8] + $6D703EF3); D2 := D2 shl 13 or D2 shr 19;
  Inc(C2, D2 and A2 or not D2 and B2 + Buffer[12] + $6D703EF3); C2 := C2 shl  5 or C2 shr 27;
  Inc(B2, C2 and D2 or not C2 and A2 + Buffer[ 2] + $6D703EF3); B2 := B2 shl 14 or B2 shr 18;
  Inc(A2, B2 and C2 or not B2 and D2 + Buffer[10] + $6D703EF3); A2 := A2 shl 13 or A2 shr 19;
  Inc(D2, A2 and B2 or not A2 and C2 + Buffer[ 0] + $6D703EF3); D2 := D2 shl 13 or D2 shr 19;
  Inc(C2, D2 and A2 or not D2 and B2 + Buffer[ 4] + $6D703EF3); C2 := C2 shl  7 or C2 shr 25;
  Inc(B2, C2 and D2 or not C2 and A2 + Buffer[13] + $6D703EF3); B2 := B2 shl  5 or B2 shr 27;

  T := C1; C1 := C2; C2 := T;

  Inc(A1, B1 and D1 or C1 and not D1 + Buffer[ 1] + $8F1BBCDC); A1 := A1 shl 11 or A1 shr 21;
  Inc(D1, A1 and C1 or B1 and not C1 + Buffer[ 9] + $8F1BBCDC); D1 := D1 shl 12 or D1 shr 20;
  Inc(C1, D1 and B1 or A1 and not B1 + Buffer[11] + $8F1BBCDC); C1 := C1 shl 14 or C1 shr 18;
  Inc(B1, C1 and A1 or D1 and not A1 + Buffer[10] + $8F1BBCDC); B1 := B1 shl 15 or B1 shr 17;
  Inc(A1, B1 and D1 or C1 and not D1 + Buffer[ 0] + $8F1BBCDC); A1 := A1 shl 14 or A1 shr 18;
  Inc(D1, A1 and C1 or B1 and not C1 + Buffer[ 8] + $8F1BBCDC); D1 := D1 shl 15 or D1 shr 17;
  Inc(C1, D1 and B1 or A1 and not B1 + Buffer[12] + $8F1BBCDC); C1 := C1 shl  9 or C1 shr 23;
  Inc(B1, C1 and A1 or D1 and not A1 + Buffer[ 4] + $8F1BBCDC); B1 := B1 shl  8 or B1 shr 24;
  Inc(A1, B1 and D1 or C1 and not D1 + Buffer[13] + $8F1BBCDC); A1 := A1 shl  9 or A1 shr 23;
  Inc(D1, A1 and C1 or B1 and not C1 + Buffer[ 3] + $8F1BBCDC); D1 := D1 shl 14 or D1 shr 18;
  Inc(C1, D1 and B1 or A1 and not B1 + Buffer[ 7] + $8F1BBCDC); C1 := C1 shl  5 or C1 shr 27;
  Inc(B1, C1 and A1 or D1 and not A1 + Buffer[15] + $8F1BBCDC); B1 := B1 shl  6 or B1 shr 26;
  Inc(A1, B1 and D1 or C1 and not D1 + Buffer[14] + $8F1BBCDC); A1 := A1 shl  8 or A1 shr 24;
  Inc(D1, A1 and C1 or B1 and not C1 + Buffer[ 5] + $8F1BBCDC); D1 := D1 shl  6 or D1 shr 26;
  Inc(C1, D1 and B1 or A1 and not B1 + Buffer[ 6] + $8F1BBCDC); C1 := C1 shl  5 or C1 shr 27;
  Inc(B1, C1 and A1 or D1 and not A1 + Buffer[ 2] + $8F1BBCDC); B1 := B1 shl 12 or B1 shr 20;

  Inc(A2, B2 xor C2 xor D2 + Buffer[ 8]); A2 := A2 shl 15 or A2 shr 17;
  Inc(D2, A2 xor B2 xor C2 + Buffer[ 6]); D2 := D2 shl  5 or D2 shr 27;
  Inc(C2, D2 xor A2 xor B2 + Buffer[ 4]); C2 := C2 shl  8 or C2 shr 24;
  Inc(B2, C2 xor D2 xor A2 + Buffer[ 1]); B2 := B2 shl 11 or B2 shr 21;
  Inc(A2, B2 xor C2 xor D2 + Buffer[ 3]); A2 := A2 shl 14 or A2 shr 18;
  Inc(D2, A2 xor B2 xor C2 + Buffer[11]); D2 := D2 shl 14 or D2 shr 18;
  Inc(C2, D2 xor A2 xor B2 + Buffer[15]); C2 := C2 shl  6 or C2 shr 26;
  Inc(B2, C2 xor D2 xor A2 + Buffer[ 0]); B2 := B2 shl 14 or B2 shr 18;
  Inc(A2, B2 xor C2 xor D2 + Buffer[ 5]); A2 := A2 shl  6 or A2 shr 26;
  Inc(D2, A2 xor B2 xor C2 + Buffer[12]); D2 := D2 shl  9 or D2 shr 23;
  Inc(C2, D2 xor A2 xor B2 + Buffer[ 2]); C2 := C2 shl 12 or C2 shr 20;
  Inc(B2, C2 xor D2 xor A2 + Buffer[13]); B2 := B2 shl  9 or B2 shr 23;
  Inc(A2, B2 xor C2 xor D2 + Buffer[ 9]); A2 := A2 shl 12 or A2 shr 20;
  Inc(D2, A2 xor B2 xor C2 + Buffer[ 7]); D2 := D2 shl  5 or D2 shr 27;
  Inc(C2, D2 xor A2 xor B2 + Buffer[10]); C2 := C2 shl 15 or C2 shr 17;
  Inc(B2, C2 xor D2 xor A2 + Buffer[14]); B2 := B2 shl  8 or B2 shr 24;

  T := D1; D1 := D2; D2 := T;

  Inc(FDigest[0], A1);
  Inc(FDigest[1], B1);
  Inc(FDigest[2], C1);
  Inc(FDigest[3], D1);
  Inc(FDigest[4], A2);
  Inc(FDigest[5], B2);
  Inc(FDigest[6], C2);
  Inc(FDigest[7], D2);
end;

class function THash_RipeMD256.DigestKeySize: Integer;
begin
  Result := 32;
end;

procedure THash_RipeMD256.Init;
begin
  inherited Init;
  FDigest[4] := $76543210;
  FDigest[5] := $FEDCBA98;
  FDigest[6] := $89ABCDEF;
  FDigest[7] := $01234567;
end;

class function THash_RipeMD320.TestVector: Pointer;
{$IFDEF WIN64}
const
  Vect: array [0..39] of Byte = (
    $B7,$BD,$2C,$75,$B7,$13,$50,$91,
    $E4,$67,$09,$46,$F1,$41,$5A,$48,
    $45,$DF,$8E,$07,$BA,$67,$4E,$A9,
    $FD,$66,$ED,$01,$D9,$6F,$23,$20,
    $B5,$11,$12,$C5,$A7,$41,$A6,$5C
);
begin
  Result := @Vect[0];
end;
{$ELSE}
asm
         MOV   EAX,OFFSET @Vector
         RET
@Vector: DB    0B7h,0BDh,02Ch,075h,0B7h,013h,050h,091h
         DB    0E4h,067h,009h,046h,0F1h,041h,05Ah,048h
         DB    045h,0DFh,08Eh,007h,0BAh,067h,04Eh,0A9h
         DB    0FDh,066h,0EDh,001h,0D9h,06Fh,023h,020h
         DB    0B5h,011h,012h,0C5h,0A7h,041h,0A6h,05Ch
end;
{$ENDIF}

procedure THash_RipeMD320.Transform(Buffer: PIntArray);
var
  A1, B1, C1, D1, E1: LongWord;
  A2, B2, C2, D2, E2: LongWord;
  T: LongWord;
begin
  A1 := FDigest[0];
  B1 := FDigest[1];
  C1 := FDigest[2];
  D1 := FDigest[3];
  E1 := FDigest[4];
  A2 := FDigest[5];
  B2 := FDigest[6];
  C2 := FDigest[7];
  D2 := FDigest[8];
  E2 := FDigest[9];

  Inc(A1, Buffer[ 0] + (B1 xor C1 xor D1)); A1 := A1 shl 11 or A1 shr 21 + E1; C1 := C1 shl 10 or C1 shr 22;
  Inc(E1, Buffer[ 1] + (A1 xor B1 xor C1)); E1 := E1 shl 14 or E1 shr 18 + D1; B1 := B1 shl 10 or B1 shr 22;
  Inc(D1, Buffer[ 2] + (E1 xor A1 xor B1)); D1 := D1 shl 15 or D1 shr 17 + C1; A1 := A1 shl 10 or A1 shr 22;
  Inc(C1, Buffer[ 3] + (D1 xor E1 xor A1)); C1 := C1 shl 12 or C1 shr 20 + B1; E1 := E1 shl 10 or E1 shr 22;
  Inc(B1, Buffer[ 4] + (C1 xor D1 xor E1)); B1 := B1 shl  5 or B1 shr 27 + A1; D1 := D1 shl 10 or D1 shr 22;
  Inc(A1, Buffer[ 5] + (B1 xor C1 xor D1)); A1 := A1 shl  8 or A1 shr 24 + E1; C1 := C1 shl 10 or C1 shr 22;
  Inc(E1, Buffer[ 6] + (A1 xor B1 xor C1)); E1 := E1 shl  7 or E1 shr 25 + D1; B1 := B1 shl 10 or B1 shr 22;
  Inc(D1, Buffer[ 7] + (E1 xor A1 xor B1)); D1 := D1 shl  9 or D1 shr 23 + C1; A1 := A1 shl 10 or A1 shr 22;
  Inc(C1, Buffer[ 8] + (D1 xor E1 xor A1)); C1 := C1 shl 11 or C1 shr 21 + B1; E1 := E1 shl 10 or E1 shr 22;
  Inc(B1, Buffer[ 9] + (C1 xor D1 xor E1)); B1 := B1 shl 13 or B1 shr 19 + A1; D1 := D1 shl 10 or D1 shr 22;
  Inc(A1, Buffer[10] + (B1 xor C1 xor D1)); A1 := A1 shl 14 or A1 shr 18 + E1; C1 := C1 shl 10 or C1 shr 22;
  Inc(E1, Buffer[11] + (A1 xor B1 xor C1)); E1 := E1 shl 15 or E1 shr 17 + D1; B1 := B1 shl 10 or B1 shr 22;
  Inc(D1, Buffer[12] + (E1 xor A1 xor B1)); D1 := D1 shl  6 or D1 shr 26 + C1; A1 := A1 shl 10 or A1 shr 22;
  Inc(C1, Buffer[13] + (D1 xor E1 xor A1)); C1 := C1 shl  7 or C1 shr 25 + B1; E1 := E1 shl 10 or E1 shr 22;
  Inc(B1, Buffer[14] + (C1 xor D1 xor E1)); B1 := B1 shl  9 or B1 shr 23 + A1; D1 := D1 shl 10 or D1 shr 22;
  Inc(A1, Buffer[15] + (B1 xor C1 xor D1)); A1 := A1 shl  8 or A1 shr 24 + E1; C1 := C1 shl 10 or C1 shr 22;

  Inc(A2, Buffer[ 5] + $50A28BE6 + (B2 xor (C2 or not D2))); A2 := A2 shl  8 or A2 shr 24 + E2; C2 := C2 shl 10 or C2 shr 22;
  Inc(E2, Buffer[14] + $50A28BE6 + (A2 xor (B2 or not C2))); E2 := E2 shl  9 or E2 shr 23 + D2; B2 := B2 shl 10 or B2 shr 22;
  Inc(D2, Buffer[ 7] + $50A28BE6 + (E2 xor (A2 or not B2))); D2 := D2 shl  9 or D2 shr 23 + C2; A2 := A2 shl 10 or A2 shr 22;
  Inc(C2, Buffer[ 0] + $50A28BE6 + (D2 xor (E2 or not A2))); C2 := C2 shl 11 or C2 shr 21 + B2; E2 := E2 shl 10 or E2 shr 22;
  Inc(B2, Buffer[ 9] + $50A28BE6 + (C2 xor (D2 or not E2))); B2 := B2 shl 13 or B2 shr 19 + A2; D2 := D2 shl 10 or D2 shr 22;
  Inc(A2, Buffer[ 2] + $50A28BE6 + (B2 xor (C2 or not D2))); A2 := A2 shl 15 or A2 shr 17 + E2; C2 := C2 shl 10 or C2 shr 22;
  Inc(E2, Buffer[11] + $50A28BE6 + (A2 xor (B2 or not C2))); E2 := E2 shl 15 or E2 shr 17 + D2; B2 := B2 shl 10 or B2 shr 22;
  Inc(D2, Buffer[ 4] + $50A28BE6 + (E2 xor (A2 or not B2))); D2 := D2 shl  5 or D2 shr 27 + C2; A2 := A2 shl 10 or A2 shr 22;
  Inc(C2, Buffer[13] + $50A28BE6 + (D2 xor (E2 or not A2))); C2 := C2 shl  7 or C2 shr 25 + B2; E2 := E2 shl 10 or E2 shr 22;
  Inc(B2, Buffer[ 6] + $50A28BE6 + (C2 xor (D2 or not E2))); B2 := B2 shl  7 or B2 shr 25 + A2; D2 := D2 shl 10 or D2 shr 22;
  Inc(A2, Buffer[15] + $50A28BE6 + (B2 xor (C2 or not D2))); A2 := A2 shl  8 or A2 shr 24 + E2; C2 := C2 shl 10 or C2 shr 22;
  Inc(E2, Buffer[ 8] + $50A28BE6 + (A2 xor (B2 or not C2))); E2 := E2 shl 11 or E2 shr 21 + D2; B2 := B2 shl 10 or B2 shr 22;
  Inc(D2, Buffer[ 1] + $50A28BE6 + (E2 xor (A2 or not B2))); D2 := D2 shl 14 or D2 shr 18 + C2; A2 := A2 shl 10 or A2 shr 22;
  Inc(C2, Buffer[10] + $50A28BE6 + (D2 xor (E2 or not A2))); C2 := C2 shl 14 or C2 shr 18 + B2; E2 := E2 shl 10 or E2 shr 22;
  Inc(B2, Buffer[ 3] + $50A28BE6 + (C2 xor (D2 or not E2))); B2 := B2 shl 12 or B2 shr 20 + A2; D2 := D2 shl 10 or D2 shr 22;
  Inc(A2, Buffer[12] + $50A28BE6 + (B2 xor (C2 or not D2))); A2 := A2 shl  6 or A2 shr 26 + E2; C2 := C2 shl 10 or C2 shr 22;

  T := A1; A1 := A2; A2 := T;

  Inc(E1, Buffer[ 7] + $5A827999 + ((A1 and B1) or (not A1 and C1))); E1 := E1 shl  7 or E1 shr 25 + D1; B1 := B1 shl 10 or B1 shr 22;
  Inc(D1, Buffer[ 4] + $5A827999 + ((E1 and A1) or (not E1 and B1))); D1 := D1 shl  6 or D1 shr 26 + C1; A1 := A1 shl 10 or A1 shr 22;
  Inc(C1, Buffer[13] + $5A827999 + ((D1 and E1) or (not D1 and A1))); C1 := C1 shl  8 or C1 shr 24 + B1; E1 := E1 shl 10 or E1 shr 22;
  Inc(B1, Buffer[ 1] + $5A827999 + ((C1 and D1) or (not C1 and E1))); B1 := B1 shl 13 or B1 shr 19 + A1; D1 := D1 shl 10 or D1 shr 22;
  Inc(A1, Buffer[10] + $5A827999 + ((B1 and C1) or (not B1 and D1))); A1 := A1 shl 11 or A1 shr 21 + E1; C1 := C1 shl 10 or C1 shr 22;
  Inc(E1, Buffer[ 6] + $5A827999 + ((A1 and B1) or (not A1 and C1))); E1 := E1 shl  9 or E1 shr 23 + D1; B1 := B1 shl 10 or B1 shr 22;
  Inc(D1, Buffer[15] + $5A827999 + ((E1 and A1) or (not E1 and B1))); D1 := D1 shl  7 or D1 shr 25 + C1; A1 := A1 shl 10 or A1 shr 22;
  Inc(C1, Buffer[ 3] + $5A827999 + ((D1 and E1) or (not D1 and A1))); C1 := C1 shl 15 or C1 shr 17 + B1; E1 := E1 shl 10 or E1 shr 22;
  Inc(B1, Buffer[12] + $5A827999 + ((C1 and D1) or (not C1 and E1))); B1 := B1 shl  7 or B1 shr 25 + A1; D1 := D1 shl 10 or D1 shr 22;
  Inc(A1, Buffer[ 0] + $5A827999 + ((B1 and C1) or (not B1 and D1))); A1 := A1 shl 12 or A1 shr 20 + E1; C1 := C1 shl 10 or C1 shr 22;
  Inc(E1, Buffer[ 9] + $5A827999 + ((A1 and B1) or (not A1 and C1))); E1 := E1 shl 15 or E1 shr 17 + D1; B1 := B1 shl 10 or B1 shr 22;
  Inc(D1, Buffer[ 5] + $5A827999 + ((E1 and A1) or (not E1 and B1))); D1 := D1 shl  9 or D1 shr 23 + C1; A1 := A1 shl 10 or A1 shr 22;
  Inc(C1, Buffer[ 2] + $5A827999 + ((D1 and E1) or (not D1 and A1))); C1 := C1 shl 11 or C1 shr 21 + B1; E1 := E1 shl 10 or E1 shr 22;
  Inc(B1, Buffer[14] + $5A827999 + ((C1 and D1) or (not C1 and E1))); B1 := B1 shl  7 or B1 shr 25 + A1; D1 := D1 shl 10 or D1 shr 22;
  Inc(A1, Buffer[11] + $5A827999 + ((B1 and C1) or (not B1 and D1))); A1 := A1 shl 13 or A1 shr 19 + E1; C1 := C1 shl 10 or C1 shr 22;
  Inc(E1, Buffer[ 8] + $5A827999 + ((A1 and B1) or (not A1 and C1))); E1 := E1 shl 12 or E1 shr 20 + D1; B1 := B1 shl 10 or B1 shr 22;

  Inc(E2, Buffer[ 6] + $5C4DD124 + ((A2 and C2) or (B2 and not C2))); E2 := E2 shl  9 or E2 shr 23 + D2; B2 := B2 shl 10 or B2 shr 22;
  Inc(D2, Buffer[11] + $5C4DD124 + ((E2 and B2) or (A2 and not B2))); D2 := D2 shl 13 or D2 shr 19 + C2; A2 := A2 shl 10 or A2 shr 22;
  Inc(C2, Buffer[ 3] + $5C4DD124 + ((D2 and A2) or (E2 and not A2))); C2 := C2 shl 15 or C2 shr 17 + B2; E2 := E2 shl 10 or E2 shr 22;
  Inc(B2, Buffer[ 7] + $5C4DD124 + ((C2 and E2) or (D2 and not E2))); B2 := B2 shl  7 or B2 shr 25 + A2; D2 := D2 shl 10 or D2 shr 22;
  Inc(A2, Buffer[ 0] + $5C4DD124 + ((B2 and D2) or (C2 and not D2))); A2 := A2 shl 12 or A2 shr 20 + E2; C2 := C2 shl 10 or C2 shr 22;
  Inc(E2, Buffer[13] + $5C4DD124 + ((A2 and C2) or (B2 and not C2))); E2 := E2 shl  8 or E2 shr 24 + D2; B2 := B2 shl 10 or B2 shr 22;
  Inc(D2, Buffer[ 5] + $5C4DD124 + ((E2 and B2) or (A2 and not B2))); D2 := D2 shl  9 or D2 shr 23 + C2; A2 := A2 shl 10 or A2 shr 22;
  Inc(C2, Buffer[10] + $5C4DD124 + ((D2 and A2) or (E2 and not A2))); C2 := C2 shl 11 or C2 shr 21 + B2; E2 := E2 shl 10 or E2 shr 22;
  Inc(B2, Buffer[14] + $5C4DD124 + ((C2 and E2) or (D2 and not E2))); B2 := B2 shl  7 or B2 shr 25 + A2; D2 := D2 shl 10 or D2 shr 22;
  Inc(A2, Buffer[15] + $5C4DD124 + ((B2 and D2) or (C2 and not D2))); A2 := A2 shl  7 or A2 shr 25 + E2; C2 := C2 shl 10 or C2 shr 22;
  Inc(E2, Buffer[ 8] + $5C4DD124 + ((A2 and C2) or (B2 and not C2))); E2 := E2 shl 12 or E2 shr 20 + D2; B2 := B2 shl 10 or B2 shr 22;
  Inc(D2, Buffer[12] + $5C4DD124 + ((E2 and B2) or (A2 and not B2))); D2 := D2 shl  7 or D2 shr 25 + C2; A2 := A2 shl 10 or A2 shr 22;
  Inc(C2, Buffer[ 4] + $5C4DD124 + ((D2 and A2) or (E2 and not A2))); C2 := C2 shl  6 or C2 shr 26 + B2; E2 := E2 shl 10 or E2 shr 22;
  Inc(B2, Buffer[ 9] + $5C4DD124 + ((C2 and E2) or (D2 and not E2))); B2 := B2 shl 15 or B2 shr 17 + A2; D2 := D2 shl 10 or D2 shr 22;
  Inc(A2, Buffer[ 1] + $5C4DD124 + ((B2 and D2) or (C2 and not D2))); A2 := A2 shl 13 or A2 shr 19 + E2; C2 := C2 shl 10 or C2 shr 22;
  Inc(E2, Buffer[ 2] + $5C4DD124 + ((A2 and C2) or (B2 and not C2))); E2 := E2 shl 11 or E2 shr 21 + D2; B2 := B2 shl 10 or B2 shr 22;

  T := B1; B1 := B2; B2 := T;

  Inc(D1, Buffer[ 3] + $6ED9EBA1 + ((E1 or not A1) xor B1)); D1 := D1 shl 11 or D1 shr 21 + C1; A1 := A1 shl 10 or A1 shr 22;
  Inc(C1, Buffer[10] + $6ED9EBA1 + ((D1 or not E1) xor A1)); C1 := C1 shl 13 or C1 shr 19 + B1; E1 := E1 shl 10 or E1 shr 22;
  Inc(B1, Buffer[14] + $6ED9EBA1 + ((C1 or not D1) xor E1)); B1 := B1 shl  6 or B1 shr 26 + A1; D1 := D1 shl 10 or D1 shr 22;
  Inc(A1, Buffer[ 4] + $6ED9EBA1 + ((B1 or not C1) xor D1)); A1 := A1 shl  7 or A1 shr 25 + E1; C1 := C1 shl 10 or C1 shr 22;
  Inc(E1, Buffer[ 9] + $6ED9EBA1 + ((A1 or not B1) xor C1)); E1 := E1 shl 14 or E1 shr 18 + D1; B1 := B1 shl 10 or B1 shr 22;
  Inc(D1, Buffer[15] + $6ED9EBA1 + ((E1 or not A1) xor B1)); D1 := D1 shl  9 or D1 shr 23 + C1; A1 := A1 shl 10 or A1 shr 22;
  Inc(C1, Buffer[ 8] + $6ED9EBA1 + ((D1 or not E1) xor A1)); C1 := C1 shl 13 or C1 shr 19 + B1; E1 := E1 shl 10 or E1 shr 22;
  Inc(B1, Buffer[ 1] + $6ED9EBA1 + ((C1 or not D1) xor E1)); B1 := B1 shl 15 or B1 shr 17 + A1; D1 := D1 shl 10 or D1 shr 22;
  Inc(A1, Buffer[ 2] + $6ED9EBA1 + ((B1 or not C1) xor D1)); A1 := A1 shl 14 or A1 shr 18 + E1; C1 := C1 shl 10 or C1 shr 22;
  Inc(E1, Buffer[ 7] + $6ED9EBA1 + ((A1 or not B1) xor C1)); E1 := E1 shl  8 or E1 shr 24 + D1; B1 := B1 shl 10 or B1 shr 22;
  Inc(D1, Buffer[ 0] + $6ED9EBA1 + ((E1 or not A1) xor B1)); D1 := D1 shl 13 or D1 shr 19 + C1; A1 := A1 shl 10 or A1 shr 22;
  Inc(C1, Buffer[ 6] + $6ED9EBA1 + ((D1 or not E1) xor A1)); C1 := C1 shl  6 or C1 shr 26 + B1; E1 := E1 shl 10 or E1 shr 22;
  Inc(B1, Buffer[13] + $6ED9EBA1 + ((C1 or not D1) xor E1)); B1 := B1 shl  5 or B1 shr 27 + A1; D1 := D1 shl 10 or D1 shr 22;
  Inc(A1, Buffer[11] + $6ED9EBA1 + ((B1 or not C1) xor D1)); A1 := A1 shl 12 or A1 shr 20 + E1; C1 := C1 shl 10 or C1 shr 22;
  Inc(E1, Buffer[ 5] + $6ED9EBA1 + ((A1 or not B1) xor C1)); E1 := E1 shl  7 or E1 shr 25 + D1; B1 := B1 shl 10 or B1 shr 22;
  Inc(D1, Buffer[12] + $6ED9EBA1 + ((E1 or not A1) xor B1)); D1 := D1 shl  5 or D1 shr 27 + C1; A1 := A1 shl 10 or A1 shr 22;

  Inc(D2, Buffer[15] + $6D703EF3 + ((E2 or not A2) xor B2)); D2 := D2 shl  9 or D2 shr 23 + C2; A2 := A2 shl 10 or A2 shr 22;
  Inc(C2, Buffer[ 5] + $6D703EF3 + ((D2 or not E2) xor A2)); C2 := C2 shl  7 or C2 shr 25 + B2; E2 := E2 shl 10 or E2 shr 22;
  Inc(B2, Buffer[ 1] + $6D703EF3 + ((C2 or not D2) xor E2)); B2 := B2 shl 15 or B2 shr 17 + A2; D2 := D2 shl 10 or D2 shr 22;
  Inc(A2, Buffer[ 3] + $6D703EF3 + ((B2 or not C2) xor D2)); A2 := A2 shl 11 or A2 shr 21 + E2; C2 := C2 shl 10 or C2 shr 22;
  Inc(E2, Buffer[ 7] + $6D703EF3 + ((A2 or not B2) xor C2)); E2 := E2 shl  8 or E2 shr 24 + D2; B2 := B2 shl 10 or B2 shr 22;
  Inc(D2, Buffer[14] + $6D703EF3 + ((E2 or not A2) xor B2)); D2 := D2 shl  6 or D2 shr 26 + C2; A2 := A2 shl 10 or A2 shr 22;
  Inc(C2, Buffer[ 6] + $6D703EF3 + ((D2 or not E2) xor A2)); C2 := C2 shl  6 or C2 shr 26 + B2; E2 := E2 shl 10 or E2 shr 22;
  Inc(B2, Buffer[ 9] + $6D703EF3 + ((C2 or not D2) xor E2)); B2 := B2 shl 14 or B2 shr 18 + A2; D2 := D2 shl 10 or D2 shr 22;
  Inc(A2, Buffer[11] + $6D703EF3 + ((B2 or not C2) xor D2)); A2 := A2 shl 12 or A2 shr 20 + E2; C2 := C2 shl 10 or C2 shr 22;
  Inc(E2, Buffer[ 8] + $6D703EF3 + ((A2 or not B2) xor C2)); E2 := E2 shl 13 or E2 shr 19 + D2; B2 := B2 shl 10 or B2 shr 22;
  Inc(D2, Buffer[12] + $6D703EF3 + ((E2 or not A2) xor B2)); D2 := D2 shl  5 or D2 shr 27 + C2; A2 := A2 shl 10 or A2 shr 22;
  Inc(C2, Buffer[ 2] + $6D703EF3 + ((D2 or not E2) xor A2)); C2 := C2 shl 14 or C2 shr 18 + B2; E2 := E2 shl 10 or E2 shr 22;
  Inc(B2, Buffer[10] + $6D703EF3 + ((C2 or not D2) xor E2)); B2 := B2 shl 13 or B2 shr 19 + A2; D2 := D2 shl 10 or D2 shr 22;
  Inc(A2, Buffer[ 0] + $6D703EF3 + ((B2 or not C2) xor D2)); A2 := A2 shl 13 or A2 shr 19 + E2; C2 := C2 shl 10 or C2 shr 22;
  Inc(E2, Buffer[ 4] + $6D703EF3 + ((A2 or not B2) xor C2)); E2 := E2 shl  7 or E2 shr 25 + D2; B2 := B2 shl 10 or B2 shr 22;
  Inc(D2, Buffer[13] + $6D703EF3 + ((E2 or not A2) xor B2)); D2 := D2 shl  5 or D2 shr 27 + C2; A2 := A2 shl 10 or A2 shr 22;

  T := C1; C1 := C2; C2 := T;

  Inc(C1, Buffer[ 1] + $8F1BBCDC + ((D1 and A1) or (E1 and not A1))); C1 := C1 shl 11 or C1 shr 21 + B1; E1 := E1 shl 10 or E1 shr 22;
  Inc(B1, Buffer[ 9] + $8F1BBCDC + ((C1 and E1) or (D1 and not E1))); B1 := B1 shl 12 or B1 shr 20 + A1; D1 := D1 shl 10 or D1 shr 22;
  Inc(A1, Buffer[11] + $8F1BBCDC + ((B1 and D1) or (C1 and not D1))); A1 := A1 shl 14 or A1 shr 18 + E1; C1 := C1 shl 10 or C1 shr 22;
  Inc(E1, Buffer[10] + $8F1BBCDC + ((A1 and C1) or (B1 and not C1))); E1 := E1 shl 15 or E1 shr 17 + D1; B1 := B1 shl 10 or B1 shr 22;
  Inc(D1, Buffer[ 0] + $8F1BBCDC + ((E1 and B1) or (A1 and not B1))); D1 := D1 shl 14 or D1 shr 18 + C1; A1 := A1 shl 10 or A1 shr 22;
  Inc(C1, Buffer[ 8] + $8F1BBCDC + ((D1 and A1) or (E1 and not A1))); C1 := C1 shl 15 or C1 shr 17 + B1; E1 := E1 shl 10 or E1 shr 22;
  Inc(B1, Buffer[12] + $8F1BBCDC + ((C1 and E1) or (D1 and not E1))); B1 := B1 shl  9 or B1 shr 23 + A1; D1 := D1 shl 10 or D1 shr 22;
  Inc(A1, Buffer[ 4] + $8F1BBCDC + ((B1 and D1) or (C1 and not D1))); A1 := A1 shl  8 or A1 shr 24 + E1; C1 := C1 shl 10 or C1 shr 22;
  Inc(E1, Buffer[13] + $8F1BBCDC + ((A1 and C1) or (B1 and not C1))); E1 := E1 shl  9 or E1 shr 23 + D1; B1 := B1 shl 10 or B1 shr 22;
  Inc(D1, Buffer[ 3] + $8F1BBCDC + ((E1 and B1) or (A1 and not B1))); D1 := D1 shl 14 or D1 shr 18 + C1; A1 := A1 shl 10 or A1 shr 22;
  Inc(C1, Buffer[ 7] + $8F1BBCDC + ((D1 and A1) or (E1 and not A1))); C1 := C1 shl  5 or C1 shr 27 + B1; E1 := E1 shl 10 or E1 shr 22;
  Inc(B1, Buffer[15] + $8F1BBCDC + ((C1 and E1) or (D1 and not E1))); B1 := B1 shl  6 or B1 shr 26 + A1; D1 := D1 shl 10 or D1 shr 22;
  Inc(A1, Buffer[14] + $8F1BBCDC + ((B1 and D1) or (C1 and not D1))); A1 := A1 shl  8 or A1 shr 24 + E1; C1 := C1 shl 10 or C1 shr 22;
  Inc(E1, Buffer[ 5] + $8F1BBCDC + ((A1 and C1) or (B1 and not C1))); E1 := E1 shl  6 or E1 shr 26 + D1; B1 := B1 shl 10 or B1 shr 22;
  Inc(D1, Buffer[ 6] + $8F1BBCDC + ((E1 and B1) or (A1 and not B1))); D1 := D1 shl  5 or D1 shr 27 + C1; A1 := A1 shl 10 or A1 shr 22;
  Inc(C1, Buffer[ 2] + $8F1BBCDC + ((D1 and A1) or (E1 and not A1))); C1 := C1 shl 12 or C1 shr 20 + B1; E1 := E1 shl 10 or E1 shr 22;

  Inc(C2, Buffer[ 8] + $7A6D76E9 + ((D2 and E2) or (not D2 and A2))); C2 := C2 shl 15 or C2 shr 17 + B2; E2 := E2 shl 10 or E2 shr 22;
  Inc(B2, Buffer[ 6] + $7A6D76E9 + ((C2 and D2) or (not C2 and E2))); B2 := B2 shl  5 or B2 shr 27 + A2; D2 := D2 shl 10 or D2 shr 22;
  Inc(A2, Buffer[ 4] + $7A6D76E9 + ((B2 and C2) or (not B2 and D2))); A2 := A2 shl  8 or A2 shr 24 + E2; C2 := C2 shl 10 or C2 shr 22;
  Inc(E2, Buffer[ 1] + $7A6D76E9 + ((A2 and B2) or (not A2 and C2))); E2 := E2 shl 11 or E2 shr 21 + D2; B2 := B2 shl 10 or B2 shr 22;
  Inc(D2, Buffer[ 3] + $7A6D76E9 + ((E2 and A2) or (not E2 and B2))); D2 := D2 shl 14 or D2 shr 18 + C2; A2 := A2 shl 10 or A2 shr 22;
  Inc(C2, Buffer[11] + $7A6D76E9 + ((D2 and E2) or (not D2 and A2))); C2 := C2 shl 14 or C2 shr 18 + B2; E2 := E2 shl 10 or E2 shr 22;
  Inc(B2, Buffer[15] + $7A6D76E9 + ((C2 and D2) or (not C2 and E2))); B2 := B2 shl  6 or B2 shr 26 + A2; D2 := D2 shl 10 or D2 shr 22;
  Inc(A2, Buffer[ 0] + $7A6D76E9 + ((B2 and C2) or (not B2 and D2))); A2 := A2 shl 14 or A2 shr 18 + E2; C2 := C2 shl 10 or C2 shr 22;
  Inc(E2, Buffer[ 5] + $7A6D76E9 + ((A2 and B2) or (not A2 and C2))); E2 := E2 shl  6 or E2 shr 26 + D2; B2 := B2 shl 10 or B2 shr 22;
  Inc(D2, Buffer[12] + $7A6D76E9 + ((E2 and A2) or (not E2 and B2))); D2 := D2 shl  9 or D2 shr 23 + C2; A2 := A2 shl 10 or A2 shr 22;
  Inc(C2, Buffer[ 2] + $7A6D76E9 + ((D2 and E2) or (not D2 and A2))); C2 := C2 shl 12 or C2 shr 20 + B2; E2 := E2 shl 10 or E2 shr 22;
  Inc(B2, Buffer[13] + $7A6D76E9 + ((C2 and D2) or (not C2 and E2))); B2 := B2 shl  9 or B2 shr 23 + A2; D2 := D2 shl 10 or D2 shr 22;
  Inc(A2, Buffer[ 9] + $7A6D76E9 + ((B2 and C2) or (not B2 and D2))); A2 := A2 shl 12 or A2 shr 20 + E2; C2 := C2 shl 10 or C2 shr 22;
  Inc(E2, Buffer[ 7] + $7A6D76E9 + ((A2 and B2) or (not A2 and C2))); E2 := E2 shl  5 or E2 shr 27 + D2; B2 := B2 shl 10 or B2 shr 22;
  Inc(D2, Buffer[10] + $7A6D76E9 + ((E2 and A2) or (not E2 and B2))); D2 := D2 shl 15 or D2 shr 17 + C2; A2 := A2 shl 10 or A2 shr 22;
  Inc(C2, Buffer[14] + $7A6D76E9 + ((D2 and E2) or (not D2 and A2))); C2 := C2 shl  8 or C2 shr 24 + B2; E2 := E2 shl 10 or E2 shr 22;

  T := D1; D1 := D2; D2 := T;

  Inc(B1, Buffer[ 4] + $A953FD4E + (C1 xor (D1 or not E1))); B1 := B1 shl  9 or B1 shr 23 + A1; D1 := D1 shl 10 or D1 shr 22;
  Inc(A1, Buffer[ 0] + $A953FD4E + (B1 xor (C1 or not D1))); A1 := A1 shl 15 or A1 shr 17 + E1; C1 := C1 shl 10 or C1 shr 22;
  Inc(E1, Buffer[ 5] + $A953FD4E + (A1 xor (B1 or not C1))); E1 := E1 shl  5 or E1 shr 27 + D1; B1 := B1 shl 10 or B1 shr 22;
  Inc(D1, Buffer[ 9] + $A953FD4E + (E1 xor (A1 or not B1))); D1 := D1 shl 11 or D1 shr 21 + C1; A1 := A1 shl 10 or A1 shr 22;
  Inc(C1, Buffer[ 7] + $A953FD4E + (D1 xor (E1 or not A1))); C1 := C1 shl  6 or C1 shr 26 + B1; E1 := E1 shl 10 or E1 shr 22;
  Inc(B1, Buffer[12] + $A953FD4E + (C1 xor (D1 or not E1))); B1 := B1 shl  8 or B1 shr 24 + A1; D1 := D1 shl 10 or D1 shr 22;
  Inc(A1, Buffer[ 2] + $A953FD4E + (B1 xor (C1 or not D1))); A1 := A1 shl 13 or A1 shr 19 + E1; C1 := C1 shl 10 or C1 shr 22;
  Inc(E1, Buffer[10] + $A953FD4E + (A1 xor (B1 or not C1))); E1 := E1 shl 12 or E1 shr 20 + D1; B1 := B1 shl 10 or B1 shr 22;
  Inc(D1, Buffer[14] + $A953FD4E + (E1 xor (A1 or not B1))); D1 := D1 shl  5 or D1 shr 27 + C1; A1 := A1 shl 10 or A1 shr 22;
  Inc(C1, Buffer[ 1] + $A953FD4E + (D1 xor (E1 or not A1))); C1 := C1 shl 12 or C1 shr 20 + B1; E1 := E1 shl 10 or E1 shr 22;
  Inc(B1, Buffer[ 3] + $A953FD4E + (C1 xor (D1 or not E1))); B1 := B1 shl 13 or B1 shr 19 + A1; D1 := D1 shl 10 or D1 shr 22;
  Inc(A1, Buffer[ 8] + $A953FD4E + (B1 xor (C1 or not D1))); A1 := A1 shl 14 or A1 shr 18 + E1; C1 := C1 shl 10 or C1 shr 22;
  Inc(E1, Buffer[11] + $A953FD4E + (A1 xor (B1 or not C1))); E1 := E1 shl 11 or E1 shr 21 + D1; B1 := B1 shl 10 or B1 shr 22;
  Inc(D1, Buffer[ 6] + $A953FD4E + (E1 xor (A1 or not B1))); D1 := D1 shl  8 or D1 shr 24 + C1; A1 := A1 shl 10 or A1 shr 22;
  Inc(C1, Buffer[15] + $A953FD4E + (D1 xor (E1 or not A1))); C1 := C1 shl  5 or C1 shr 27 + B1; E1 := E1 shl 10 or E1 shr 22;
  Inc(B1, Buffer[13] + $A953FD4E + (C1 xor (D1 or not E1))); B1 := B1 shl  6 or B1 shr 26 + A1; D1 := D1 shl 10 or D1 shr 22;

  Inc(B2, Buffer[12] + (C2 xor D2 xor E2)); B2 := B2 shl  8 or B2 shr 24 + A2; D2 := D2 shl 10 or D2 shr 22;
  Inc(A2, Buffer[15] + (B2 xor C2 xor D2)); A2 := A2 shl  5 or A2 shr 27 + E2; C2 := C2 shl 10 or C2 shr 22;
  Inc(E2, Buffer[10] + (A2 xor B2 xor C2)); E2 := E2 shl 12 or E2 shr 20 + D2; B2 := B2 shl 10 or B2 shr 22;
  Inc(D2, Buffer[ 4] + (E2 xor A2 xor B2)); D2 := D2 shl  9 or D2 shr 23 + C2; A2 := A2 shl 10 or A2 shr 22;
  Inc(C2, Buffer[ 1] + (D2 xor E2 xor A2)); C2 := C2 shl 12 or C2 shr 20 + B2; E2 := E2 shl 10 or E2 shr 22;
  Inc(B2, Buffer[ 5] + (C2 xor D2 xor E2)); B2 := B2 shl  5 or B2 shr 27 + A2; D2 := D2 shl 10 or D2 shr 22;
  Inc(A2, Buffer[ 8] + (B2 xor C2 xor D2)); A2 := A2 shl 14 or A2 shr 18 + E2; C2 := C2 shl 10 or C2 shr 22;
  Inc(E2, Buffer[ 7] + (A2 xor B2 xor C2)); E2 := E2 shl  6 or E2 shr 26 + D2; B2 := B2 shl 10 or B2 shr 22;
  Inc(D2, Buffer[ 6] + (E2 xor A2 xor B2)); D2 := D2 shl  8 or D2 shr 24 + C2; A2 := A2 shl 10 or A2 shr 22;
  Inc(C2, Buffer[ 2] + (D2 xor E2 xor A2)); C2 := C2 shl 13 or C2 shr 19 + B2; E2 := E2 shl 10 or E2 shr 22;
  Inc(B2, Buffer[13] + (C2 xor D2 xor E2)); B2 := B2 shl  6 or B2 shr 26 + A2; D2 := D2 shl 10 or D2 shr 22;
  Inc(A2, Buffer[14] + (B2 xor C2 xor D2)); A2 := A2 shl  5 or A2 shr 27 + E2; C2 := C2 shl 10 or C2 shr 22;
  Inc(E2, Buffer[ 0] + (A2 xor B2 xor C2)); E2 := E2 shl 15 or E2 shr 17 + D2; B2 := B2 shl 10 or B2 shr 22;
  Inc(D2, Buffer[ 3] + (E2 xor A2 xor B2)); D2 := D2 shl 13 or D2 shr 19 + C2; A2 := A2 shl 10 or A2 shr 22;
  Inc(C2, Buffer[ 9] + (D2 xor E2 xor A2)); C2 := C2 shl 11 or C2 shr 21 + B2; E2 := E2 shl 10 or E2 shr 22;
  Inc(B2, Buffer[11] + (C2 xor D2 xor E2)); B2 := B2 shl 11 or B2 shr 21 + A2; D2 := D2 shl 10 or D2 shr 22;

  T := E1; E1 := E2; E2 := T;

  Inc(FDigest[0], A1);
  Inc(FDigest[1], B1);
  Inc(FDigest[2], C1);
  Inc(FDigest[3], D1);
  Inc(FDigest[4], E1);
  Inc(FDigest[5], A2);
  Inc(FDigest[6], B2);
  Inc(FDigest[7], C2);
  Inc(FDigest[8], D2);
  Inc(FDigest[9], E2);
end;

class function THash_RipeMD320.DigestKeySize: Integer;
begin
  Result := 40;
end;

end.

