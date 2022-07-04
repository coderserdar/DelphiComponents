Unit Crc;

{
  crc32.c -- compute the CRC-32 of a data stream
  Copyright (C) 1995-1998 Mark Adler

  Pascal tranlastion
  Copyright (C) 1998 by Jacques Nomssi Nzali
  For conditions of distribution and use, see copyright notice in readme.txt
}

interface

{$I zconf.inc}

uses
  zutil, bzlib;

function crc32(crc : uLong; buf : pBytef; len : uInt) : uLong;

{  Update a running crc with the bytes buf[0..len-1] and return the updated
   crc. If buf is NULL, this function returns the required initial value
   for the crc. Pre- and post-conditioning (one's complement) is performed
   within this function so it shouldn't be done by the application.
   Usage example:

    var
      crc : uLong;
    begin
      crc := crc32(0, Z_NULL, 0);

      while (read_buffer(buffer, length) <> EOF) do
        crc := crc32(crc, buffer, length);

      if (crc <> original_crc) then error();
    end;

}

function get_crc_table : puLong;  { can be used by asm versions of crc32() }


implementation

{$IFDEF DYNAMIC_CRC_TABLE}

{local}
const
  crc_table_empty : boolean = TRUE;
{local}
var
  crc_table : array[0..256-1] of uLongf;


{
  Generate a table for a byte-wise 32-bit CRC calculation on the polynomial:
  x^32+x^26+x^23+x^22+x^16+x^12+x^11+x^10+x^8+x^7+x^5+x^4+x^2+x+1.

  Polynomials over GF(2) are represented in binary, one bit per coefficient,
  with the lowest powers in the most significant bit.  Then adding polynomials
  is just exclusive-or, and multiplying a polynomial by x is a right shift by
  one.  If we call the above polynomial p, and represent a byte as the
  polynomial q, also with the lowest power in the most significant bit (so the
  byte 0xb1 is the polynomial x^7+x^3+x+1), then the CRC is (q*x^32) mod p,
  where a mod b means the remainder after dividing a by b.

  This calculation is done using the shift-register method of multiplying and
  taking the remainder.  The register is initialized to zero, and for each
  incoming bit, x^32 is added mod p to the register if the bit is a one (where
  x^32 mod p is p+x^32 = x^26+...+1), and the register is multiplied mod p by
  x (which is shifting right by one and adding x^32 mod p if the bit shifted
  out is a one).  We start with the highest power (least significant bit) of
  q and repeat for all eight bits of q.

  The table is simply the CRC of all possible eight bit values.  This is all
  the information needed to generate CRC's on data a byte at a time for all
  combinations of CRC register values and incoming bytes.
}
{local}
procedure make_crc_table;
var
 c    : uLong;
 n,k  : int;
 poly : uLong; { polynomial exclusive-or pattern }

const
 { terms of polynomial defining this crc (except x^32): }
 p: array [0..13] of Byte = (0,1,2,4,5,7,8,10,11,12,16,22,23,26);

begin
  { make exclusive-or pattern from polynomial ($EDB88320) }
  poly := Long(0);
  for n := 0 to (sizeof(p) div sizeof(Byte))-1 do
    poly := poly or (Long(1) shl (31 - p[n]));

  for n := 0 to 255 do
  begin
    c := uLong(n);
    for k := 0 to 7 do
    begin
      if (c and 1) <> 0 then
        c := poly xor (c shr 1)
      else
        c := (c shr 1);
    end;
    crc_table[n] := c;
  end;
  crc_table_empty := FALSE;
end;

{$ELSE}

{ ========================================================================
  Table of CRC-32's of all single-byte values (made by make_crc_table) }

{local}
const
  crc_table : array[0..256-1] of uLongf = (
  uLongf($00000000), uLongf($77073096), uLongf($ee0e612c), uLongf($990951ba), uLongf($076dc419),
  uLongf($706af48f), uLongf($e963a535), uLongf($9e6495a3), uLongf($0edb8832), uLongf($79dcb8a4),
  uLongf($e0d5e91e), uLongf($97d2d988), uLongf($09b64c2b), uLongf($7eb17cbd), uLongf($e7b82d07),
  uLongf($90bf1d91), uLongf($1db71064), uLongf($6ab020f2), uLongf($f3b97148), uLongf($84be41de),
  uLongf($1adad47d), uLongf($6ddde4eb), uLongf($f4d4b551), uLongf($83d385c7), uLongf($136c9856),
  uLongf($646ba8c0), uLongf($fd62f97a), uLongf($8a65c9ec), uLongf($14015c4f), uLongf($63066cd9),
  uLongf($fa0f3d63), uLongf($8d080df5), uLongf($3b6e20c8), uLongf($4c69105e), uLongf($d56041e4),
  uLongf($a2677172), uLongf($3c03e4d1), uLongf($4b04d447), uLongf($d20d85fd), uLongf($a50ab56b),
  uLongf($35b5a8fa), uLongf($42b2986c), uLongf($dbbbc9d6), uLongf($acbcf940), uLongf($32d86ce3),
  uLongf($45df5c75), uLongf($dcd60dcf), uLongf($abd13d59), uLongf($26d930ac), uLongf($51de003a),
  uLongf($c8d75180), uLongf($bfd06116), uLongf($21b4f4b5), uLongf($56b3c423), uLongf($cfba9599),
  uLongf($b8bda50f), uLongf($2802b89e), uLongf($5f058808), uLongf($c60cd9b2), uLongf($b10be924),
  uLongf($2f6f7c87), uLongf($58684c11), uLongf($c1611dab), uLongf($b6662d3d), uLongf($76dc4190),
  uLongf($01db7106), uLongf($98d220bc), uLongf($efd5102a), uLongf($71b18589), uLongf($06b6b51f),
  uLongf($9fbfe4a5), uLongf($e8b8d433), uLongf($7807c9a2), uLongf($0f00f934), uLongf($9609a88e),
  uLongf($e10e9818), uLongf($7f6a0dbb), uLongf($086d3d2d), uLongf($91646c97), uLongf($e6635c01),
  uLongf($6b6b51f4), uLongf($1c6c6162), uLongf($856530d8), uLongf($f262004e), uLongf($6c0695ed),
  uLongf($1b01a57b), uLongf($8208f4c1), uLongf($f50fc457), uLongf($65b0d9c6), uLongf($12b7e950),
  uLongf($8bbeb8ea), uLongf($fcb9887c), uLongf($62dd1ddf), uLongf($15da2d49), uLongf($8cd37cf3),
  uLongf($fbd44c65), uLongf($4db26158), uLongf($3ab551ce), uLongf($a3bc0074), uLongf($d4bb30e2),
  uLongf($4adfa541), uLongf($3dd895d7), uLongf($a4d1c46d), uLongf($d3d6f4fb), uLongf($4369e96a),
  uLongf($346ed9fc), uLongf($ad678846), uLongf($da60b8d0), uLongf($44042d73), uLongf($33031de5),
  uLongf($aa0a4c5f), uLongf($dd0d7cc9), uLongf($5005713c), uLongf($270241aa), uLongf($be0b1010),
  uLongf($c90c2086), uLongf($5768b525), uLongf($206f85b3), uLongf($b966d409), uLongf($ce61e49f),
  uLongf($5edef90e), uLongf($29d9c998), uLongf($b0d09822), uLongf($c7d7a8b4), uLongf($59b33d17),
  uLongf($2eb40d81), uLongf($b7bd5c3b), uLongf($c0ba6cad), uLongf($edb88320), uLongf($9abfb3b6),
  uLongf($03b6e20c), uLongf($74b1d29a), uLongf($ead54739), uLongf($9dd277af), uLongf($04db2615),
  uLongf($73dc1683), uLongf($e3630b12), uLongf($94643b84), uLongf($0d6d6a3e), uLongf($7a6a5aa8),
  uLongf($e40ecf0b), uLongf($9309ff9d), uLongf($0a00ae27), uLongf($7d079eb1), uLongf($f00f9344),
  uLongf($8708a3d2), uLongf($1e01f268), uLongf($6906c2fe), uLongf($f762575d), uLongf($806567cb),
  uLongf($196c3671), uLongf($6e6b06e7), uLongf($fed41b76), uLongf($89d32be0), uLongf($10da7a5a),
  uLongf($67dd4acc), uLongf($f9b9df6f), uLongf($8ebeeff9), uLongf($17b7be43), uLongf($60b08ed5),
  uLongf($d6d6a3e8), uLongf($a1d1937e), uLongf($38d8c2c4), uLongf($4fdff252), uLongf($d1bb67f1),
  uLongf($a6bc5767), uLongf($3fb506dd), uLongf($48b2364b), uLongf($d80d2bda), uLongf($af0a1b4c),
  uLongf($36034af6), uLongf($41047a60), uLongf($df60efc3), uLongf($a867df55), uLongf($316e8eef),
  uLongf($4669be79), uLongf($cb61b38c), uLongf($bc66831a), uLongf($256fd2a0), uLongf($5268e236),
  uLongf($cc0c7795), uLongf($bb0b4703), uLongf($220216b9), uLongf($5505262f), uLongf($c5ba3bbe),
  uLongf($b2bd0b28), uLongf($2bb45a92), uLongf($5cb36a04), uLongf($c2d7ffa7), uLongf($b5d0cf31),
  uLongf($2cd99e8b), uLongf($5bdeae1d), uLongf($9b64c2b0), uLongf($ec63f226), uLongf($756aa39c),
  uLongf($026d930a), uLongf($9c0906a9), uLongf($eb0e363f), uLongf($72076785), uLongf($05005713),
  uLongf($95bf4a82), uLongf($e2b87a14), uLongf($7bb12bae), uLongf($0cb61b38), uLongf($92d28e9b),
  uLongf($e5d5be0d), uLongf($7cdcefb7), uLongf($0bdbdf21), uLongf($86d3d2d4), uLongf($f1d4e242),
  uLongf($68ddb3f8), uLongf($1fda836e), uLongf($81be16cd), uLongf($f6b9265b), uLongf($6fb077e1),
  uLongf($18b74777), uLongf($88085ae6), uLongf($ff0f6a70), uLongf($66063bca), uLongf($11010b5c),
  uLongf($8f659eff), uLongf($f862ae69), uLongf($616bffd3), uLongf($166ccf45), uLongf($a00ae278),
  uLongf($d70dd2ee), uLongf($4e048354), uLongf($3903b3c2), uLongf($a7672661), uLongf($d06016f7),
  uLongf($4969474d), uLongf($3e6e77db), uLongf($aed16a4a), uLongf($d9d65adc), uLongf($40df0b66),
  uLongf($37d83bf0), uLongf($a9bcae53), uLongf($debb9ec5), uLongf($47b2cf7f), uLongf($30b5ffe9),
  uLongf($bdbdf21c), uLongf($cabac28a), uLongf($53b39330), uLongf($24b4a3a6), uLongf($bad03605),
  uLongf($cdd70693), uLongf($54de5729), uLongf($23d967bf), uLongf($b3667a2e), uLongf($c4614ab8),
  uLongf($5d681b02), uLongf($2a6f2b94), uLongf($b40bbe37), uLongf($c30c8ea1), uLongf($5a05df1b),
  uLongf($2d02ef8d));

{$ENDIF}

{ =========================================================================
  This function can be used by asm versions of crc32() }

function get_crc_table : {const} puLong;
begin
{$ifdef DYNAMIC_CRC_TABLE}
  if (crc_table_empty) then
    make_crc_table;
{$endif}
  get_crc_table :=  {const} puLong(@crc_table);
end;

{ ========================================================================= }

function crc32 (crc : uLong; buf : pBytef; len : uInt): uLong;
begin
  if (buf = Z_NULL) then
    crc32 := Long(0)
  else
  begin

{$IFDEF DYNAMIC_CRC_TABLE}
    if crc_table_empty then
      make_crc_table;
{$ENDIF}

    crc := crc xor Long($ffffffff);
    while (len >= 8) do
    begin
      {DO8(buf)}
      crc := crc_table[(int(crc) xor buf^) and $ff] xor (crc shr 8);
      inc(buf);
      crc := crc_table[(int(crc) xor buf^) and $ff] xor (crc shr 8);
      inc(buf);
      crc := crc_table[(int(crc) xor buf^) and $ff] xor (crc shr 8);
      inc(buf);
      crc := crc_table[(int(crc) xor buf^) and $ff] xor (crc shr 8);
      inc(buf);
      crc := crc_table[(int(crc) xor buf^) and $ff] xor (crc shr 8);
      inc(buf);
      crc := crc_table[(int(crc) xor buf^) and $ff] xor (crc shr 8);
      inc(buf);
      crc := crc_table[(int(crc) xor buf^) and $ff] xor (crc shr 8);
      inc(buf);
      crc := crc_table[(int(crc) xor buf^) and $ff] xor (crc shr 8);
      inc(buf);

      Dec(len, 8);
    end;
    if (len <> 0) then
    repeat
      {DO1(buf)}
      crc := crc_table[(int(crc) xor buf^) and $ff] xor (crc shr 8);
      inc(buf);

      Dec(len);
    until (len = 0);
    crc32 := crc xor Long($ffffffff);
  end;
end;


end.