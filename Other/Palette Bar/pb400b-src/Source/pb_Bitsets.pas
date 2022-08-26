unit pb_Bitsets;

interface

uses SysUtils, Classes;

TYPE TBit = (Bit0,   Bit1,   Bit2,   Bit3,   Bit4,   Bit5,   Bit6,   Bit7,
             Bit8,   Bit9,   Bit10,  Bit11,  Bit12,  Bit13,  Bit14,  Bit15,
             Bit16,  Bit17,  Bit18,  Bit19,  Bit20,  Bit21,  Bit22,  Bit23,
             Bit24,  Bit25,  Bit26,  Bit27,  Bit28,  Bit29,  Bit30,  Bit31,
             Bit32,  Bit33,  Bit34,  Bit35,  Bit36,  Bit37,  Bit38,  Bit39,
             Bit40,  Bit41,  Bit42,  Bit43,  Bit44,  Bit45,  Bit46,  Bit47,
             Bit48,  Bit49,  Bit50,  Bit51,  Bit52,  Bit53,  Bit54,  Bit55,
             Bit56,  Bit57,  Bit58,  Bit59,  Bit60,  Bit61,  Bit62,  Bit63,
             Bit64,  Bit65,  Bit66,  Bit67,  Bit68,  Bit69,  Bit70,  Bit71,
             Bit72,  Bit73,  Bit74,  Bit75,  Bit76,  Bit77,  Bit78,  Bit79,
             Bit80,  Bit81,  Bit82,  Bit83,  Bit84,  Bit85,  Bit86,  Bit87,
             Bit88,  Bit89,  Bit90,  Bit91,  Bit92,  Bit93,  Bit94,  Bit95,
             Bit96,  Bit97,  Bit98,  Bit99,  Bit100, Bit101, Bit102, Bit103,
             Bit104, Bit105, Bit106, Bit107, Bit108, Bit109, Bit110, Bit111,
             Bit112, Bit113, Bit114, Bit115, Bit116, Bit117, Bit118, Bit119,
             Bit120, Bit121, Bit122, Bit123, Bit124, Bit125, Bit126, Bit127,
             Bit128, Bit129, Bit130, Bit131, Bit132, Bit133, Bit134, Bit135,
             Bit136, Bit137, Bit138, Bit139, Bit140, Bit141, Bit142, Bit143,
             Bit144, Bit145, Bit146, Bit147, Bit148, Bit149, Bit150, Bit151,
             Bit152, Bit153, Bit154, Bit155, Bit156, Bit157, Bit158, Bit159,
             Bit160, Bit161, Bit162, Bit163, Bit164, Bit165, Bit166, Bit167,
             Bit168, Bit169, Bit170, Bit171, Bit172, Bit173, Bit174, Bit175,
             Bit176, Bit177, Bit178, Bit179, Bit180, Bit181, Bit182, Bit183,
             Bit184, Bit185, Bit186, Bit187, Bit188, Bit189, Bit190, Bit191,
             Bit192, Bit193, Bit194, Bit195, Bit196, Bit197, Bit198, Bit199,
             Bit200, Bit201, Bit202, Bit203, Bit204, Bit205, Bit206, Bit207,
             Bit208, Bit209, Bit210, Bit211, Bit212, Bit213, Bit214, Bit215,
             Bit216, Bit217, Bit218, Bit219, Bit220, Bit221, Bit222, Bit223,
             Bit224, Bit225, Bit226, Bit227, Bit228, Bit229, Bit230, Bit231,
             Bit232, Bit233, Bit234, Bit235, Bit236, Bit237, Bit238, Bit239,
             Bit240, Bit241, Bit242, Bit243, Bit244, Bit245, Bit246, Bit247,
             Bit248, Bit249, Bit250, Bit251, Bit252, Bit253, Bit254, Bit255);

type  T8Bits =   set of Bit0..Bit7;    // Byte    1
      T16Bits =  set of Bit0..Bit15;   // Word    2
      T32Bits =  set of Bit0..Bit31;   // Long    4
      T64Bits =  set of Bit0..Bit63;   // Int64   8
      T128Bits = set of Bit0..Bit127;  // Int128  16
      T256Bits = set of Bit0..Bit255;  // Int256  32

type  TInt128 =  array [0..1] of Int64;
      TInt256 =  array [0..3] of Int64;

//------------------------------------------------------------------------------
// Set-Wandlungen
//------------------------------------------------------------------------------

function ValTo8Bits   (const i8):  t8Bits;
function ValTo16Bits  (const i16): t16Bits;
function ValTo32Bits  (const i32): t32Bits;
function ValTo64Bits  (const i64): t64Bits;
function ValTo128Bits (const i128): t128Bits;
function ValTo256Bits (const i256): t256Bits;
// Wert in Bitset 

function BitsToByte   (const t8:  t8Bits): Byte;
function BitsToWord   (const t16: t16Bits): Word;
function BitsToInt    (const t32: t32Bits): Integer;
function BitsToInt64  (const t64: t64Bits): Int64;
function BitsToInt128 (const t128: t128Bits): tInt128;
function BitsToInt256 (const t256: t256Bits): tInt256;
// Bitset in Wert

function BitsetToHex  (const setvar; size: integer): string;
// Bitset in Hex-String

function BitSetToBin  (const setvar; size: Integer): string;
// Bitset in Binär-String

procedure HexToBitset (const h: string; var setvar; Size: Integer);
// Hex-String in Bitset

procedure BinToBitset (const h: string; var setvar; Size: Integer);
// Binärstring in Bitset

function CountBits (const setvar; size: Integer): Integer;
// Zählt die Anzahl der gesetzten Bits


implementation

uses Basics;

function ValTo8Bits  (CONST i8): t8Bits;
var b: t8Bits absolute i8;
begin
 Result:= b;
end;

function ValTo16Bits (CONST i16): t16Bits;
var b: t16Bits absolute i16;
begin
 Result:= b;
end;

function ValTo32Bits (CONST i32): t32Bits;
var b: t32Bits absolute i32;
begin
 Result:= b;
end;

function ValTo64Bits (CONST i64): t64Bits;
var b: t64Bits absolute i64;
begin
 Result:= b;
end;

function ValTo128Bits (CONST i128): t128Bits;
var b: t128Bits absolute i128;
begin
 Result:= b;
end;

function ValTo256Bits (CONST i256): t256Bits;
var b: t256Bits absolute i256;
begin
 Result:= b;
end;

function BitsToByte (CONST t8: t8Bits): Byte;
var b: Byte absolute t8;
begin
 Result:= b;
end;

function BitsToWord (CONST t16: t16Bits): Word;
var b: Word absolute t16;
begin
 Result:= b;
end;

function BitsToInt (CONST t32: t32Bits): Integer;
var b: Integer absolute t32;
begin
 Result:= b;
end;

function BitsToInt64 (CONST t64: t64Bits): Int64;
var b: int64 absolute t64;
begin
 Result:= b;
end;

function BitsToInt128 (CONST t128: t128Bits): tInt128;
var b: tInt128 absolute t128;
begin
 Result:= b;
end;

function BitsToInt256 (CONST t256: t256Bits): tInt256;
var b: tInt256 absolute t256;
begin
 Result:= b;
end;

type tBitarray =  array [0..31] of T8Bits;
     tBytearray = array [0..31] of Byte;

function BitsetToHex (const setvar; Size: integer): string;
var bits: tByteArray absolute setvar;
    i: Integer;
begin
 Result:= '';
 for i:= 0 to size - 1 do Insert (IntToHex (bits[i], 2), Result, 1);
end;

function BitSetToBin (const setvar; size: Integer): string;
var bits: tBitArray absolute setvar;
    i: Integer;
    b: TBit;
begin
 Result:= '';
 if Size = 0 then Exit;
 for i:= size - 1 downto 0 do begin
  for b:= Bit7 downto Bit0 do begin
   if b in bits[i] then Result:= Result + '1'
                   else Result:= Result + '0';
  end;
 end;
end;

procedure StrToBitset (const h: string; var setvar; Size, num, base: Integer);
var bits: tBitArray absolute setvar;
    i: Integer;
    b: Byte;
    s: string;
begin
 FillChar (bits, size, #0);
 s:= h;
 for i:= size - 1 downto 0 do begin
  b:= StrToNum (Copy (s, 1, num), base, False, MaxInt);
  Delete (s, 1, num);
  bits[i]:= ValTo8Bits (b);
 end;
end;

procedure HexToBitset (const h: string; var setvar; Size: Integer);
begin
 StrToBitset (h, setvar, Size, 2, 16);
end;

procedure BinToBitset (const h: string; var setvar; Size: Integer);
begin
 StrToBitset (h, setvar, Size, 8, 2);
end;

function CountBits (const setvar; size: Integer): Integer;
var bits: tBitArray absolute setvar;
    i: Integer;
    b: TBit;
begin
 Result:= 0;
 for i:= 0 to size - 1 do begin
  for b:= Bit0 to Bit7 do begin
   if b in bits[i] then Inc (Result);
  end;
 end;
end (*CountBits*);

end (*Bitsets*).

