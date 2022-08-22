unit Enc_Blowfish;

{$I TinyDB.INC}

interface

uses Classes, Windows, SysUtils, TinyDB, EncryptBase;

type
  TEnc_Blowfish = class(TEncrypt)
  protected
    class procedure GetContext(var ABufSize, AKeySize, AUserSize: Integer); override;
    class function TestVector: Pointer; override;
    procedure Encode(Data: Pointer); override;
    procedure Decode(Data: Pointer); override;
  public
    procedure Init(const Key; Size: Integer; IVector: Pointer); override;
  end;

{ TEncAlgo_Blowfish }

  TEncAlgo_Blowfish = class(TEncAlgo_Base)
  protected
    function GetEncryptObjectClass: TEncryptClass; override;
  end;

implementation

uses HashBase, Hash_SHA;

{$I Enc_Blowfish.inc}

class procedure TEnc_Blowfish.GetContext(var ABufSize, AKeySize, AUserSize: Integer);
begin
  ABufSize := 8;
  AKeySize := 56;
  AUserSize := SizeOf(Blowfish_Data) + SizeOf(Blowfish_Key);
end;

class function TEnc_Blowfish.TestVector: Pointer;
{$IFDEF WIN64}
const
  Vect: array [0..31] of Byte = (
$19, $71, $CA, $CD, $2B, $9C, $85, $29,
$DA, $81, $47, $B7, $EB, $CE, $16, $C6,
$91, $0E, $1D, $C8, $40, $12, $3E, $35,
$70, $ED, $BC, $96, $4C, $13, $D0, $B8);
begin
  Result := @Vect[0];
end;
{$ELSE}
asm
         MOV   EAX,OFFSET @Vector
         RET
@Vector: DB    019h,071h,0CAh,0CDh,02Bh,09Ch,085h,029h
         DB    0DAh,081h,047h,0B7h,0EBh,0CEh,016h,0C6h
         DB    091h,00Eh,01Dh,0C8h,040h,012h,03Eh,035h
         DB    070h,0EDh,0BCh,096h,04Ch,013h,0D0h,0B8h
end;
{$ENDIF}

type
  PBlowfish = ^TBlowfish;
  TBlowfish = array[0..3, 0..255] of LongWord;

procedure TEnc_Blowfish.Encode(Data: Pointer);
var
  I,A,B: LongWord;
  P: PLongWord;
  D: PBlowfish;
begin
  D := User;
  P := Pointer(PAnsiChar(User) + SizeOf(Blowfish_Data));
  A := SwapInteger(PEncryptRec(Data).A) xor P^; Inc(P);
  B := SwapInteger(PEncryptRec(Data).B);
  for I := 0 to 7 do
  begin
    B := B xor P^ xor (D[0, A shr 24] +
                       D[1, A shr 16 and $FF] xor
                       D[2, A shr  8 and $FF] +
                       D[3, A and $FF]);
    Inc(P);
    A := A xor P^ xor (D[0, B shr 24] +
                       D[1, B shr 16 and $FF] xor
                       D[2, B shr  8 and $FF] +
                       D[3, B and $FF]);
    Inc(P);
  end;
  PEncryptRec(Data).A := SwapInteger(B xor P^);
  PEncryptRec(Data).B := SwapInteger(A);
end;

procedure TEnc_Blowfish.Decode(Data: Pointer);
var
  I,A,B: LongWord;
  P: PLongWord;
  D: PBlowfish;
begin
  D := User;
  P := Pointer(PAnsiChar(User) + SizeOf(Blowfish_Data) + SizeOf(Blowfish_Key) - SizeOf(Integer));
  A := SwapInteger(PEncryptRec(Data).A) xor P^; Dec(P);
  B := SwapInteger(PEncryptRec(Data).B);
  for I := 0 to 7 do
  begin
    B := B xor P^ xor (D[0, A shr 24] +
                       D[1, A shr 16 and $FF] xor
                       D[2, A shr  8 and $FF] +
                       D[3, A and $FF]);
    Dec(P);
    A := A xor P^ xor (D[0, B shr 24] +
                       D[1, B shr 16 and $FF] xor
                       D[2, B shr  8 and $FF] +
                       D[3, B and $FF]);
    Dec(P);
  end;
  PEncryptRec(Data).A := SwapInteger(B xor P^);
  PEncryptRec(Data).B := SwapInteger(A);
end;

procedure TEnc_Blowfish.Init(const Key; Size: Integer; IVector: Pointer);
var
  I,J: Integer;
  B: array[0..7] of Byte;
  K: PByteArray;
  P: PIntArray;
  S: PBlowfish;
begin
  InitBegin(Size);
  K := @Key;
  S := User;
  P := Pointer(PAnsiChar(User) + SizeOf(Blowfish_Data));
  Move(Blowfish_Data, S^, SizeOf(Blowfish_Data));
  Move(Blowfish_Key, P^, Sizeof(Blowfish_Key));
  J := 0;
  for I := 0 to 17 do
  begin
    P[I] := P[I] xor (K[(J + 3) mod Size] +
                      K[(J + 2) mod Size] shl 8 +
                      K[(J + 1) mod Size] shl 16 +
                      K[J] shl 24);
    J := (J + 4) mod Size;
  end;
  FillChar(B, SizeOf(B), 0);
  for I := 0 to 8 do
  begin
    Encode(@B);
    P[I * 2]     := SwapInteger(PEncryptRec(@B).A);
    P[I * 2 + 1] := SwapInteger(PEncryptRec(@B).B);
  end;
  for I := 0 to 3 do
    for J := 0 to 127 do
    begin
      Encode(@B);
      S[I, J * 2]    := SwapInteger(PEncryptRec(@B).A);
      S[I, J * 2 +1] := SwapInteger(PEncryptRec(@B).B);
    end;
  FillChar(B, SizeOf(B), 0);
  InitEnd(IVector);
end;

{ TEncAlgo_Blowfish }

function TEncAlgo_Blowfish.GetEncryptObjectClass: TEncryptClass;
begin
  Result := TEnc_Blowfish;
end;

initialization
  RegisterEncryptClass(TEncAlgo_Blowfish, 'Blowfish');

end.
