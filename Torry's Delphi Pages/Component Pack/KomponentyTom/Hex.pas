unit Hex;


interface
uses Windows, SysUtils, Math;

type

  TBit = 0..1;

  TByteRek = record
    B0, B1, B2, B3, B4, B5, B6, B7: TBit;
  end;

  TWordRek = record
    B0, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15: TBit;
  end;

  TDWordRek = record
    B0, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15,
    B16, B17, B18, B19, B20, B21, B22, B23, B24, B25, B26, B27, B28, B29,
    B30, B31: TBit;
  end;

function ByteToDec(B: Byte): string;

function ByteToHex(B: Byte; Przerwa: string): string;

function WordToHex(W: Word; Przerwa: string): string;

function DWordToHex(DW: DWord; Przerwa: string): string;

function ByteToBinRek(B: Byte): TByteRek;

function ByteToBin(B: Byte; Przerwa: string): string;

function WordToBinRek(W: Word): TWordRek;

function WordToBin(W: Word; Przerwa: string): string;

function DWordToBinRek(DW: DWord): TDWordRek;

function DWordToBin(DW: DWord; Przerwa: string): string;

function BinRekToByte(ByteRek: TByteRek): Byte;

function BinRekToWord(WordRek: TWordRek): Word;

function BinRekToDWord(WordRek: TDWordRek): DWord;

function BinStrToDWord(Bin: string): DWord;


implementation
uses Classes;


function ByteToDec(B: Byte): string;
var S: string;
const
    A: array[1..3] of string = ('00', '0', '');
begin
  S:= Format('%d', [B]);
  Result:= A[Length(S)] + S;
end;

function ByteToHex(B: Byte; Przerwa: string): string;
begin
  Result:= Format('%x' + Przerwa + '%x', [(B and $F0) shr 4, B and $0F]);
end;

function WordToHex(W: Word; Przerwa: string): string;
begin
  Result:= Format('%s' + Przerwa + '%s', [ByteToHex((W and $FF00) shr 8, ''),
    ByteToHex(W and $00FF, '')]);
end;

function DWordToHex(DW: DWord; Przerwa: string): string;
begin
  Result:= Format('%s' + Przerwa + '%s',
    [WordToHex((DW and $FFFF0000) shr 16, Przerwa),
     WordToHex(DW and $0000FFFF, Przerwa)]);
end;

function ByteToBinRek(B: Byte): TByteRek;
begin
  Result.B0:= B and $01;
  Result.B1:= (B and $02) shr 1;
  Result.B2:= (B and $04) shr 2;
  Result.B3:= (B and $08) shr 3;
  Result.B4:= (B and $10) shr 4;
  Result.B5:= (B and $20) shr 5;
  Result.B6:= (B and $40) shr 6;
  Result.B7:= (B and $80) shr 7;
end;


function ByteToBin(B: Byte; Przerwa: string): string;
var BR: TByteRek;
begin
  BR:= ByteToBinRek(B);
  with BR do
    Result:= Format('%d%d%d%d' + Przerwa + '%d%d%d%d',[B7, B6, B5, B4, B3, B2, B1, B0]);
end;

function WordToBinRek(W: Word): TWordRek;
var BR : TByteRek;
begin
  BR:= ByteToBinRek(W and $00FF);
  with BR do
  begin
    Result.B0:= BR.B0;
    Result.B1:= BR.B1;
    Result.B2:= BR.B2;
    Result.B3:= BR.B3;
    Result.B4:= BR.B4;
    Result.B5:= BR.B5;
    Result.B6:= BR.B6;
    Result.B7:= BR.B7;
  end;
  BR:= ByteToBinRek((W and $FF00) shr 8);
  with BR do
  begin
    Result.B8:= BR.B0;
    Result.B9:= BR.B1;
    Result.B10:= BR.B2;
    Result.B11:= BR.B3;
    Result.B12:= BR.B4;
    Result.B13:= BR.B5;
    Result.B14:= BR.B6;
    Result.B15:= BR.B7;
  end;
end;

function WordToBin(W: Word; Przerwa: string): string;
var WR: TWordRek;
begin
  WR:= WordToBinRek(W);
  with WR do
    Result:= Format('%d%d%d%d' + Przerwa + '%d%d%d%d' + Przerwa +
      '%d%d%d%d' + Przerwa + '%d%d%d%d',
      [B15, B14, B13, B12, B11, B10, B9, B8, B7, B6, B5, B4, B3, B2, B1, B0]);
end;

function DWordToBinRek(DW: DWord): TDWordRek;
var BR : TByteRek;
begin
  BR:= ByteToBinRek(DW and $000000FF);
  with BR do
  begin
    Result.B0:= BR.B0;
    Result.B1:= BR.B1;
    Result.B2:= BR.B2;
    Result.B3:= BR.B3;
    Result.B4:= BR.B4;
    Result.B5:= BR.B5;
    Result.B6:= BR.B6;
    Result.B7:= BR.B7;
  end;
  BR:= ByteToBinRek((DW and $0000FF00) shr 8);
  with BR do
  begin
    Result.B8:= BR.B0;
    Result.B9:= BR.B1;
    Result.B10:= BR.B2;
    Result.B11:= BR.B3;
    Result.B12:= BR.B4;
    Result.B13:= BR.B5;
    Result.B14:= BR.B6;
    Result.B15:= BR.B7;
  end;
  BR:= ByteToBinRek((DW and $00FF0000) shr 16);
  with BR do
  begin
    Result.B16:= BR.B0;
    Result.B17:= BR.B1;
    Result.B18:= BR.B2;
    Result.B19:= BR.B3;
    Result.B20:= BR.B4;
    Result.B21:= BR.B5;
    Result.B22:= BR.B6;
    Result.B23:= BR.B7;
  end;
  BR:= ByteToBinRek((DW and $FF000000) shr 24);
  with BR do
  begin
    Result.B24:= BR.B0;
    Result.B25:= BR.B1;
    Result.B26:= BR.B2;
    Result.B27:= BR.B3;
    Result.B28:= BR.B4;
    Result.B29:= BR.B5;
    Result.B30:= BR.B6;
    Result.B31:= BR.B7;
  end;
end;

function DWordToBin(DW: DWord; Przerwa: string): string;
begin
  Result:= Format('%s' + Przerwa + '%s',
    [WordToBin((DW and $FFFF0000) shr 16, Przerwa),
     WordToBin(DW and $0000FFFF, Przerwa)]);
end;

function BinRekToByte(ByteRek: TByteRek): Byte;
var E: Extended;
begin
  E:= ByteRek.B7 * Power(2, 7) +
      ByteRek.B6 * Power(2, 6) +
      ByteRek.B5 * Power(2, 5) +
      ByteRek.B4 * Power(2, 4) +
      ByteRek.B3 * Power(2, 3) +
      ByteRek.B2 * Power(2, 2) +
      ByteRek.B1 * Power(2, 1) +
      ByteRek.B0 * Power(2, 0);
  Result:= Trunc(E);
end;

function BinRekToWord(WordRek: TWordRek): Word;
var E: Extended;
begin
  E:=
      WordRek.B15 * Power(2, 15) +
      WordRek.B14 * Power(2, 14) +
      WordRek.B13 * Power(2, 13) +
      WordRek.B12 * Power(2, 12) +
      WordRek.B11 * Power(2, 11) +
      WordRek.B10 * Power(2, 10) +
      WordRek.B9 * Power(2, 9) +
      WordRek.B8 * Power(2, 8) +
      WordRek.B7 * Power(2, 7) +
      WordRek.B6 * Power(2, 6) +
      WordRek.B5 * Power(2, 5) +
      WordRek.B4 * Power(2, 4) +
      WordRek.B3 * Power(2, 3) +
      WordRek.B2 * Power(2, 2) +
      WordRek.B1 * Power(2, 1) +
      WordRek.B0 * Power(2, 0);
  Result:= Trunc(E);
end;

function BinRekToDWord(WordRek: TDWordRek): DWord;
var E: Extended;
begin
  E:=
      WordRek.B31 * Power(2, 31) +
      WordRek.B30 * Power(2, 30) +
      WordRek.B29 * Power(2, 29) +
      WordRek.B28 * Power(2, 28) +
      WordRek.B27 * Power(2, 27) +
      WordRek.B26 * Power(2, 26) +
      WordRek.B25 * Power(2, 25) +
      WordRek.B24 * Power(2, 24) +
      WordRek.B23 * Power(2, 23) +
      WordRek.B22 * Power(2, 22) +
      WordRek.B21 * Power(2, 21) +
      WordRek.B20 * Power(2, 20) +
      WordRek.B19 * Power(2, 19) +
      WordRek.B18 * Power(2, 18) +
      WordRek.B17 * Power(2, 17) +
      WordRek.B16 * Power(2, 16) +
      WordRek.B15 * Power(2, 15) +
      WordRek.B14 * Power(2, 14) +
      WordRek.B13 * Power(2, 13) +
      WordRek.B12 * Power(2, 12) +
      WordRek.B11 * Power(2, 11) +
      WordRek.B10 * Power(2, 10) +
      WordRek.B9 * Power(2, 9) +
      WordRek.B8 * Power(2, 8) +
      WordRek.B7 * Power(2, 7) +
      WordRek.B6 * Power(2, 6) +
      WordRek.B5 * Power(2, 5) +
      WordRek.B4 * Power(2, 4) +
      WordRek.B3 * Power(2, 3) +
      WordRek.B2 * Power(2, 2) +
      WordRek.B1 * Power(2, 1) +
      WordRek.B0 * Power(2, 0);
  Result:= Trunc(E);
end;

function BinStrToDWord(Bin: string): DWord;
var i, l: Byte;

    dwr: TDWordRek;
    buf: PByte;

    function char_na_byte(Znak: Char): Byte;
    begin
      if Znak = '1' then
        Result:= 1
      else
        Result:= 0;
    end;

begin
  buf:= PByte(@dwr);
  l:= Length(Bin);
  for i := 0 to 31 do
    if i <= l - 1 then
      PByte(Integer(buf) + i)^:= char_na_byte(Bin[l - i])
    else
      PByte(Integer(buf) + i)^:= 0;

  Result:= BinRekToDWord(dwr);
end;

end.
