unit Enc_Gost;

{$I TinyDB.INC}

interface

uses Classes, Windows, SysUtils, TinyDB, EncryptBase;

type
  TEnc_Gost = class(TEncrypt) {russian Encrypt}
  protected
    class procedure GetContext(var ABufSize, AKeySize, AUserSize: Integer); override;
    class function TestVector: Pointer; override;
    procedure Encode(Data: Pointer); override;
    procedure Decode(Data: Pointer); override;
  public
    procedure Init(const Key; Size: Integer; IVector: Pointer); override;
  end;

{ TEncAlgo_Gost }

  TEncAlgo_Gost = class(TEncAlgo_Base)
  protected
    function GetEncryptObjectClass: TEncryptClass; override;
  end;

implementation

uses HashBase, Hash_SHA;

{$I Enc_Gost.inc}

class procedure TEnc_Gost.GetContext(var ABufSize, AKeySize, AUserSize: Integer);
begin
  ABufSize := 8;
  AKeySize := 32;
  AUserSize := 32;
end;

class function TEnc_Gost.TestVector: Pointer;
{$IFDEF WIN64}
const
  Vect: array [0..31] of Byte = (
   $B3, $03, $A0, $3F, $B5, $7B, $91, $4D,
   $97, $51, $24, $40, $BD, $CF, $25, $15,
   $34, $05, $9C, $F8, $AB, $10, $86, $9F,
   $F2, $80, $47, $84, $47, $9B, $1A, $D1
);
begin
  Result := @Vect[0];
end;
{$ELSE}
asm
         MOV   EAX,OFFSET @Vector
         RET
@Vector: DB    0B3h,003h,0A0h,03Fh,0B5h,07Bh,091h,04Dh
         DB    097h,051h,024h,040h,0BDh,0CFh,025h,015h
         DB    034h,005h,09Ch,0F8h,0ABh,010h,086h,09Fh
         DB    0F2h,080h,047h,084h,047h,09Bh,01Ah,0D1h
end;
{$ENDIF}

type
  PEncryptRec = ^TEncryptRec;
  TEncryptRec = packed record
                  case Integer of
                    0: (X: array[0..7] of Byte);
                    1: (A, B: LongWord);
                end;

procedure TEnc_Gost.Encode(Data: Pointer);
var
  I,A,B: LongWord;
  K: PLongword;
begin
  K := User;
  A := PEncryptRec(Data).A;
  B := PEncryptRec(Data).B;
  for I := 0 to 11 do
  begin
    if I and 3 = 0 then K := User;
    B := B xor Gost_Data[0, (A + K^) and $FF] xor
               Gost_Data[1, (A + K^) shr  8 and $FF] xor
               Gost_Data[2, (A + K^) shr 16 and $FF] xor
               Gost_Data[3, (A + K^) shr 24];
    Inc(K);
    A := A xor Gost_Data[0, (B + K^) and $FF] xor
               Gost_Data[1, (B + K^) shr  8 and $FF] xor
               Gost_Data[2, (B + K^) shr 16 and $FF] xor
               Gost_Data[3, (B + K^) shr 24];
    Inc(K);
  end;
  K := @PIntArray(User)[7];
  for I := 0 to 3 do
  begin
    B := B xor Gost_Data[0, (A + K^) and $FF] xor
               Gost_Data[1, (A + K^) shr  8 and $FF] xor
               Gost_Data[2, (A + K^) shr 16 and $FF] xor
               Gost_Data[3, (A + K^) shr 24];
    Dec(K);
    A := A xor Gost_Data[0, (B + K^) and $FF] xor
               Gost_Data[1, (B + K^) shr  8 and $FF] xor
               Gost_Data[2, (B + K^) shr 16 and $FF] xor
               Gost_Data[3, (B + K^) shr 24];
    Dec(K);
  end;
  PEncryptRec(Data).A := B;
  PEncryptRec(Data).B := A;
end;

procedure TEnc_Gost.Decode(Data: Pointer);
var
  I,A,B: LongWord;
  K: PLongword;
begin
  A := PEncryptRec(Data).A;
  B := PEncryptRec(Data).B;
  K := User;
  for I := 0 to 3 do
  begin
    B := B xor Gost_Data[0, (A + K^) and $FF] xor
               Gost_Data[1, (A + K^) shr  8 and $FF] xor
               Gost_Data[2, (A + K^) shr 16 and $FF] xor
               Gost_Data[3, (A + K^) shr 24];
    Inc(K);
    A := A xor Gost_Data[0, (B + K^) and $FF] xor
               Gost_Data[1, (B + K^) shr  8 and $FF] xor
               Gost_Data[2, (B + K^) shr 16 and $FF] xor
               Gost_Data[3, (B + K^) shr 24];
    Inc(K);
  end;
  for I := 0 to 11 do
  begin
    if I and 3 = 0 then K := @PIntArray(User)[7];
    B := B xor Gost_Data[0, (A + K^) and $FF] xor
               Gost_Data[1, (A + K^) shr  8 and $FF] xor
               Gost_Data[2, (A + K^) shr 16 and $FF] xor
               Gost_Data[3, (A + K^) shr 24];
    Dec(K);
    A := A xor Gost_Data[0, (B + K^) and $FF] xor
               Gost_Data[1, (B + K^) shr  8 and $FF] xor
               Gost_Data[2, (B + K^) shr 16 and $FF] xor
               Gost_Data[3, (B + K^) shr 24];
    Dec(K);
  end;
  PEncryptRec(Data).A := B;
  PEncryptRec(Data).B := A;
end;

procedure TEnc_Gost.Init(const Key; Size: Integer; IVector: Pointer);
begin
  InitBegin(Size);
  Move(Key, User^, Size);
  InitEnd(IVector);
end;

{ TEncAlgo_Gost }

function TEncAlgo_Gost.GetEncryptObjectClass: TEncryptClass;
begin
  Result := TEnc_Gost;
end;

initialization
  RegisterEncryptClass(TEncAlgo_Gost, 'Gost');
  
end.
