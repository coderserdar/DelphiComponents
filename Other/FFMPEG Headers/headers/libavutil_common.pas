(*
 * copyright (c) 2006 Michael Niedermayer <michaelni@gmx.at>
 *
 * This file is part of FFmpeg.
 *
 * FFmpeg is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * FFmpeg is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with FFmpeg; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 *)

(**
 * @file
 * common internal and external API header
 *)

(*
 * FFVCL - Delphi FFmpeg VCL Components
 * http://www.DelphiFFmpeg.com
 *
 * Original file: libavutil/common.h
 * Ported by CodeCoolie@CNSW 2008/04/02 -> $Date:: 2021-04-25 #$
 *)

(*
FFmpeg Delphi/Pascal Headers and Examples License Agreement

A modified part of FFVCL - Delphi FFmpeg VCL Components.
Copyright (c) 2008-2022 DelphiFFmpeg.com
All rights reserved.
http://www.DelphiFFmpeg.com

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions
are met:
1. Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.
2. Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the distribution.

This source code is provided "as is" by DelphiFFmpeg.com without
warranty of any kind, either expressed or implied, including but not
limited to the implied warranties of merchantability and/or fitness
for a particular purpose.

Please also notice the License agreement of FFmpeg libraries.
*)

unit libavutil_common;

interface

{$I CompilerDefines.inc}

uses
{$IFDEF VCL_XE2_OR_ABOVE}
  System.SysUtils;
{$ELSE}
  SysUtils;
{$ENDIF}

{$I libversion.inc}

//#if AV_HAVE_BIGENDIAN
//#   define AV_NE(be, le) (be)
//#else
//#   define AV_NE(be, le) (le)
//#endif

//rounded division & shift
//#define RSHIFT(a,b) ((a) > 0 ? ((a) + ((1<<(b))>>1))>>(b) : ((a) + ((1<<(b))>>1)-1)>>(b))
(* assume b>0 *)
//#define ROUNDED_DIV(a,b) (((a)>=0 ? (a) + ((b)>>1) : (a) - ((b)>>1))/(b))
(* Fast a/(1<<b) rounded toward +inf. Assume a>=0 and b>=0 *)
//#define AV_CEIL_RSHIFT(a,b) (!av_builtin_constant_p(b) ? -((-(a)) >> (b)) \
//                                                       : ((a) + (1<<(b)) - 1) >> (b))
function AV_CEIL_RSHIFT(a, b: Integer): Integer; {$IFDEF USE_INLINE}inline;{$ENDIF}
(* Backwards compat. *)
//#define FF_CEIL_RSHIFT AV_CEIL_RSHIFT

//#define FFUDIV(a,b) (((a)>0 ?(a):(a)-(b)+1) / (b))
//#define FFUMOD(a,b) ((a)-(b)*FFUDIV(a,b))

(**
 * Absolute value, Note, INT_MIN / INT64_MIN result in undefined behavior as they
 * are not representable as absolute values of their type. This is the same
 * as with *abs()
 * @see FFNABS()
 *)
//#define FFABS(a) ((a) >= 0 ? (a) : (-(a)))
//#define FFSIGN(a) ((a) > 0 ? 1 : -1)
function FFSIGN(a: Double): Integer; {$IFDEF USE_INLINE}inline;{$ENDIF}

(**
 * Negative Absolute value.
 * this works for all integers of all types.
 * As with many macros, this evaluates its argument twice, it thus must not have
 * a sideeffect, that is FFNABS(x++) has undefined behavior.
 *)
//#define FFNABS(a) ((a) <= 0 ? (a) : (-(a)))

(**
 * Unsigned Absolute value.
 * This takes the absolute value of a signed int and returns it as a unsigned.
 * This also works with INT_MIN which would otherwise not be representable
 * As with many macros, this evaluates its argument twice.
 *)
//#define FFABSU(a) ((a) <= 0 ? -(unsigned)(a) : (unsigned)(a))
//#define FFABS64U(a) ((a) <= 0 ? -(uint64_t)(a) : (uint64_t)(a))

(**
 * Comparator.
 * For two numerical expressions x and y, gives 1 if x > y, -1 if x < y, and 0
 * if x == y. This is useful for instance in a qsort comparator callback.
 * Furthermore, compilers are able to optimize this to branchless code, and
 * there is no risk of overflow with signed types.
 * As with many macros, this evaluates its argument multiple times, it thus
 * must not have a side-effect.
 *)
//#define FFDIFFSIGN(x,y) (((x)>(y)) - ((x)<(y)))

//#define FFMAX(a,b) ((a) > (b) ? (a) : (b))
//#define FFMAX3(a,b,c) FFMAX(FFMAX(a,b),c)
//#define FFMIN(a,b) ((a) > (b) ? (b) : (a))
//#define FFMIN3(a,b,c) FFMIN(FFMIN(a,b),c)

//#define FFSWAP(type,a,b) do{type SWAP_tmp= b; b= a; a= SWAP_tmp;}while(0)
//#define FF_ARRAY_ELEMS(a) (sizeof(a) / sizeof((a)[0]))

(* misc math functions *)

const
  TAG_YUY2 = Ord('Y') or (Ord('U') shl 8) or (Ord('Y') shl 16) or (Ord('2') shl 24);
  TAG_I420 = Ord('I') or (Ord('4') shl 8) or (Ord('2') shl 16) or (Ord('0') shl 24);
  TAG_dvsd = Ord('d') or (Ord('v') shl 8) or (Ord('s') shl 16) or (Ord('d') shl 24);

function my_sar(AValue: Integer; AShift: Byte): Integer; {$IFDEF USE_INLINE}inline;{$ENDIF}

function av_log2(v: Cardinal): Integer;

function av_ceil_log2(x: Integer): Integer; {$IFDEF USE_INLINE}inline;{$ENDIF}
function av_clip(a, amin, amax: Integer): Integer; {$IFDEF USE_INLINE}inline;{$ENDIF}
function av_clip64(a, amin, amax: Int64): Int64; {$IFDEF USE_INLINE}inline;{$ENDIF}
function av_clip_uint8(a: Integer): Byte; {$IFDEF USE_INLINE}inline;{$ENDIF}
function av_clip_int8(a: Integer): Byte; {$IFDEF USE_INLINE}inline;{$ENDIF}
function av_clip_uint16(a: Integer): SmallInt; {$IFDEF USE_INLINE}inline;{$ENDIF}
function av_clip_int16(a: Integer): SmallInt; {$IFDEF USE_INLINE}inline;{$ENDIF}
function av_clipl_int32(a: Int64): Integer; {$IFDEF USE_INLINE}inline;{$ENDIF}
function av_clip_intp2(a, p: Integer): Integer; {$IFDEF USE_INLINE}inline;{$ENDIF}
function av_clip_uintp2(a, p: Integer): Cardinal; {$IFDEF USE_INLINE}inline;{$ENDIF}
function av_mod_uintp2(a, p: Cardinal): Cardinal; {$IFDEF USE_INLINE}inline;{$ENDIF}
function av_sat_add32(a, b: Integer): Integer; {$IFDEF USE_INLINE}inline;{$ENDIF}
function av_sat_dadd32(a, b: Integer): Integer; {$IFDEF USE_INLINE}inline;{$ENDIF}
function av_sat_sub32(a, b: Integer): Integer; {$IFDEF USE_INLINE}inline;{$ENDIF}
function av_sat_dsub32(a, b: Integer): Integer; {$IFDEF USE_INLINE}inline;{$ENDIF}
function av_clipf(a, amin, amax: Single): Single; {$IFDEF USE_INLINE}inline;{$ENDIF}
function av_clipd(a, amin, amax: Double): Double; {$IFDEF USE_INLINE}inline;{$ENDIF}
function av_popcount(x: Cardinal): Integer; {$IFDEF USE_INLINE}inline;{$ENDIF}
function av_popcount64(x: Int64): Integer; {$IFDEF USE_INLINE}inline;{$ENDIF}

function MKTAG(a,b,c,d: AnsiChar): Cardinal; {$IFDEF USE_INLINE}inline;{$ENDIF}
function MKBETAG(a,b,c,d: AnsiChar): Cardinal; {$IFDEF USE_INLINE}inline;{$ENDIF}

implementation

const
  ff_log2_tab: array[0..255] of Byte = (  // libavutil/intmath.h
        0,0,1,1,2,2,2,2,3,3,3,3,3,3,3,3,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,
        5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,
        6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,
        6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,
        7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
        7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
        7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
        7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7
  );

// Shift Arithmetic Right
function my_sar(AValue: Integer; AShift: Byte): Integer;
begin
//  if AValue < 0 then
//    Assert(-(-AValue shr AShift) = AValue div (1 shl AShift));
  if AValue < 0 then
    Result := AValue div (1 shl AShift)
  else
    Result := AValue shr AShift;
end;

function av_log2(v: Cardinal): Integer; // libavutil/intmath.h
begin
  Result := 0;
  if (v and $ffff0000) <> 0 then
  begin
    v := my_sar(v, 16); // v shr 16;
    Inc(Result, 16);
  end;
  if (v and $ff00) <> 0 then
  begin
    v := my_sar(v, 8); // v shr 8;
    Inc(Result, 8);
  end;
  Inc(Result, ff_log2_tab[v]);
end;

function AV_CEIL_RSHIFT(a, b: Integer): Integer;
begin
//  if av_builtin_constant_p(b) = 0 then
    Result := -my_sar(-a, b);
//  else
//    Result := my_sar(a + (1 shl b) - 1, b);
//Assert(-my_sar(-a, b) = my_sar(a + (1 shl b) - 1, b));
end;

function FFSIGN(a: Double): Integer;
begin
  if a > 0 then
    Result := 1
  else
    Result := -1;
end;

(** Compute ceil(log2(x)).
 * @param x value used to compute ceil(log2(x))
 * @return computed ceiling of log2(x)
 *)
function av_ceil_log2(x: Integer): Integer;
begin
  Result := av_log2((x - 1) shl 1);
end;

(**
 * Clip a signed integer value into the amin-amax range.
 * @param a value to clip
 * @param amin minimum value of the clip range
 * @param amax maximum value of the clip range
 * @return clipped value
 *)
function av_clip(a, amin, amax: Integer): Integer;
begin
//#if defined(HAVE_AV_CONFIG_H) && defined(ASSERT_LEVEL) && ASSERT_LEVEL >= 2
//  if (amin > amax) abort();
//#endif
  if a < amin then
    Result := amin
  else if a > amax then
    Result := amax
  else
    Result := a;
end;

(**
 * Clip a signed 64bit integer value into the amin-amax range.
 * @param a value to clip
 * @param amin minimum value of the clip range
 * @param amax maximum value of the clip range
 * @return clipped value
 *)
function av_clip64(a, amin, amax: Int64): Int64;
begin
//#if defined(HAVE_AV_CONFIG_H) && defined(ASSERT_LEVEL) && ASSERT_LEVEL >= 2
//  if (amin > amax) abort();
//#endif
  if a < amin then
    Result := amin
  else if a > amax then
    Result := amax
  else
    Result := a;
end;

(**
 * Clip a signed integer value into the 0-255 range.
 * @param a value to clip
 * @return clipped value
 *)
function av_clip_uint8(a: Integer): Byte;
begin
//  if (a&(~0xFF)) return (~a)>>31;
//  else           return a;
  if a < 0 then
    Result := 0
  else if a > 255 then
    Result := 255
  else
    Result := a;
end;

(**
 * Clip a signed integer value into the -128,127 range.
 * @param a value to clip
 * @return clipped value
 *)
function av_clip_int8(a: Integer): Byte;
begin
  if ((a + $80) and (not $FF)) <> 0 then
    Result := my_sar(a, 31) xor $7F
  else
    Result := a;
end;

(**
 * Clip a signed integer value into the 0-65535 range.
 * @param a value to clip
 * @return clipped value
 *)
function av_clip_uint16(a: Integer): SmallInt;
begin
//  if (a&(~0xFFFF)) return (~a)>>31;
//  else             return a;
  if (a and not $FFFF) <> 0 then
    Result := my_sar(-a, 31)
  else
    Result := a;
end;

(**
 * Clip a signed integer value into the -32768,32767 range.
 * @param a value to clip
 * @return clipped value
 *)
function av_clip_int16(a: Integer): SmallInt;
begin
// TODO: check
//  if ((a+0x8000) & ~0xFFFF) return (a>>31) ^ 0x7FFF;
//  else                      return a;
  if a < -32768 then
    Result := -32768
  else if a > 32767 then
    Result := 32767
  else
    Result := a;
end;

(**
 * Clip a signed 64-bit integer value into the -2147483648,2147483647 range.
 * @param a value to clip
 * @return clipped value
 *)
function av_clipl_int32(a: Int64): Integer;
begin
// TODO: check
//  if ((a+0x80000000u) & ~UINT64_C(0xFFFFFFFF)) return (int32_t)((a>>63) ^ 0x7FFFFFFF);
//  else                                         return a;
  if a < Low(Integer) then
    Result := Low(Integer)
  else if a > High(Integer) then
    Result := High(Integer)
  else
    Result := Integer(a);
end;

(**
 * Clip a signed integer into the -(2^p),(2^p-1) range.
 * @param  a value to clip
 * @param  p bit position to clip at
 * @return clipped value
 *)
function av_clip_intp2(a, p: Integer): Integer;
begin
  if ((Cardinal(a) + (1 shl p)) and not ((2 shl p) - 1)) <> 0 then
    Result := (a shr 31) xor ((1 shl p) - 1)  // TODO: my_sar(a, 31)
  else
    Result := a;
end;

(**
 * Clip a signed integer to an unsigned power of two range.
 * @param  a value to clip
 * @param  p bit position to clip at
 * @return clipped value
 *)
function av_clip_uintp2(a, p: Integer): Cardinal;
begin
  if (a <> 0) and (not ((1 shl p) - 1) <> 0) then
  begin
    if a > 0 then
      Result := $FFFFFFFF and ((1 shl p) - 1)
    else
      Result := 0
  end
  else
    Result := a;
end;

(**
 * Clear high bits from an unsigned integer starting with specific bit position
 * @param  a value to clip
 * @param  p bit position to clip at
 * @return clipped value
 *)
function av_mod_uintp2(a, p: Cardinal): Cardinal;
begin
  Result := a and ((1 shl p) - 1);
end;

(**
 * Add two signed 32-bit values with saturation.
 *
 * @param  a one value
 * @param  b another value
 * @return sum with signed saturation
 *)
function av_sat_add32(a, b: Integer): Integer;
begin
  Result := av_clipl_int32(Int64(a) + Int64(b));
end;

(**
 * Add a doubled value to another value with saturation at both stages.
 *
 * @param  a first value
 * @param  b value doubled and added to a
 * @return sum sat(a + sat(2*b)) with signed saturation
 *)
function av_sat_dadd32(a, b: Integer): Integer;
begin
  Result := av_sat_add32(a, av_sat_add32(b, b));
end;

(**
 * Subtract two signed 32-bit values with saturation.
 *
 * @param  a one value
 * @param  b another value
 * @return difference with signed saturation
 *)
function av_sat_sub32(a, b: Integer): Integer;
begin
  Result := av_clipl_int32(Int64(a) - Int64(b));
end;

(**
 * Subtract a doubled value from another value with saturation at both stages.
 *
 * @param  a first value
 * @param  b value doubled and subtracted from a
 * @return difference sat(a - sat(2*b)) with signed saturation
 *)
function av_sat_dsub32(a, b: Integer): Integer;
begin
  Result := av_sat_sub32(a, av_sat_add32(b, b));
end;

(**
 * Add two signed 64-bit values with saturation.
 *
 * @param  a one value
 * @param  b another value
 * @return sum with signed saturation
 *)
(*
static av_always_inline int64_t av_sat_add64_c(int64_t a, int64_t b) {
#if (!defined(__INTEL_COMPILER) && AV_GCC_VERSION_AT_LEAST(5,1)) || AV_HAS_BUILTIN(__builtin_add_overflow)
    int64_t tmp;
    return !__builtin_add_overflow(a, b, &tmp) ? tmp : (tmp < 0 ? INT64_MAX : INT64_MIN);
#else
    int64_t s = a+(uint64_t)b;
    if ((int64_t)(a^b | ~s^b) >= 0)
        return INT64_MAX ^ (b >> 63);
    return s;
#endif
}*)

(**
 * Subtract two signed 64-bit values with saturation.
 *
 * @param  a one value
 * @param  b another value
 * @return difference with signed saturation
 *)
(*static av_always_inline int64_t av_sat_sub64_c(int64_t a, int64_t b) {
#if (!defined(__INTEL_COMPILER) && AV_GCC_VERSION_AT_LEAST(5,1)) || AV_HAS_BUILTIN(__builtin_sub_overflow)
    int64_t tmp;
    return !__builtin_sub_overflow(a, b, &tmp) ? tmp : (tmp < 0 ? INT64_MAX : INT64_MIN);
#else
    if (b <= 0 && a >= INT64_MAX + b)
        return INT64_MAX;
    if (b >= 0 && a <= INT64_MIN + b)
        return INT64_MIN;
    return a - b;
#endif
}*)

(**
 * Clip a float value into the amin-amax range.
 * @param a value to clip
 * @param amin minimum value of the clip range
 * @param amax maximum value of the clip range
 * @return clipped value
 *)
function av_clipf(a, amin, amax: Single): Single;
begin
//#if defined(HAVE_AV_CONFIG_H) && defined(ASSERT_LEVEL) && ASSERT_LEVEL >= 2
//  if (amin > amax) abort();
//#endif
  if a < amin then
    Result := amin
  else if a > amax then
    Result := amax
  else
    Result := a;
end;

(**
 * Clip a double value into the amin-amax range.
 * @param a value to clip
 * @param amin minimum value of the clip range
 * @param amax maximum value of the clip range
 * @return clipped value
 *)
function av_clipd(a, amin, amax: Double): Double;
begin
  Assert(amin <= amax);
  if a < amin then
    Result := amin
  else if a > amax then
    Result := amax
  else
    Result := a;
end;

(**
 * Count number of bits set to one in x
 * @param x value to count bits of
 * @return the number of bits set to one in x
 *)
function av_popcount(x: Cardinal): Integer;
begin
  x := x - ((x shr 1) and $55555555);
  x := (x and $33333333) + ((x shr 2) and $33333333);
  x := (x + (x shr 4)) and $0F0F0F0F;
  x := x + x shr 8;
  Result := (x + (x shr 16)) and $3F;
end;

(**
 * Count number of bits set to one in x
 * @param x value to count bits of
 * @return the number of bits set to one in x
 *)
function av_popcount64(x: Int64): Integer;
var
  r: Int64Rec;
begin
  Move(x, r, SizeOf(Int64));
  Result := av_popcount(r.Lo) + av_popcount(r.Hi);
end;

//static av_always_inline av_const int av_parity_c(uint32_t v)
{
    return av_popcount(v) & 1;
}

function MKTAG(a,b,c,d: AnsiChar): Cardinal;
begin
//#define MKTAG(a,b,c,d) ((a) | ((b) << 8) | ((c) << 16) | ((unsigned)(d) << 24))
  Result := Ord(a) or (Ord(b) shl 8) or (Ord(c) shl 16) or (Ord(d) shl 24)
end;

function MKBETAG(a,b,c,d: AnsiChar): Cardinal;
begin
//#define MKBETAG(a,b,c,d) ((d) | ((c) << 8) | ((b) << 16) | ((unsigned)(a) << 24))
  Result := Ord(d) or (Ord(c) shl 8) or (Ord(b) shl 16) or (Ord(a) shl 24)
end;

(*!
 * Convert a UTF-8 character (up to 4 bytes) to its 32-bit UCS-4 encoded form.
 *
 * @param val      Output value, must be an lvalue of type uint32_t.
 * @param GET_BYTE Expression reading one byte from the input.
 *                 Evaluated up to 7 times (4 for the currently
 *                 assigned Unicode range).  With a memory buffer
 *                 input, this could be *ptr++, or if you want to make sure
 *                 that *ptr stops at the end of a NULL terminated string then
 *                 *ptr ? *ptr++ : 0
 * @param ERROR    Expression to be evaluated on invalid input,
 *                 typically a goto statement.
 *
 * @warning ERROR should not contain a loop control statement which
 * could interact with the internal while loop, and should force an
 * exit from the macro code (e.g. through a goto or a return) in order
 * to prevent undefined results.
 *)
(*
#define GET_UTF8(val, GET_BYTE, ERROR)\
    val= (GET_BYTE);\
    {\
        uint32_t top = (val & 128) >> 1;\
        if ((val & 0xc0) == 0x80 || val >= 0xFE)\
            {ERROR}\
        while (val & top) {\
            unsigned int tmp = (GET_BYTE) - 128;\
            if(tmp>>6)\
                {ERROR}\
            val= (val<<6) + tmp;\
            top <<= 5;\
        }\
        val &= (top << 1) - 1;\
    }
*)

(*!
 * Convert a UTF-16 character (2 or 4 bytes) to its 32-bit UCS-4 encoded form.
 *
 * @param val       Output value, must be an lvalue of type uint32_t.
 * @param GET_16BIT Expression returning two bytes of UTF-16 data converted
 *                  to native byte order.  Evaluated one or two times.
 * @param ERROR     Expression to be evaluated on invalid input,
 *                  typically a goto statement.
 *)
(*
#define GET_UTF16(val, GET_16BIT, ERROR)\
    val = (GET_16BIT);\
    {\
        unsigned int hi = val - 0xD800;\
        if (hi < 0x800) {\
            val = (GET_16BIT) - 0xDC00;\
            if (val > 0x3FFU || hi > 0x3FFU)\
                {ERROR}\
            val += (hi<<10) + 0x10000;\
        }\
    }\
*)

(**
 * @def PUT_UTF8(val, tmp, PUT_BYTE)
 * Convert a 32-bit Unicode character to its UTF-8 encoded form (up to 4 bytes long).
 * @param val is an input-only argument and should be of type uint32_t. It holds
 * a UCS-4 encoded Unicode character that is to be converted to UTF-8. If
 * val is given as a function it is executed only once.
 * @param tmp is a temporary variable and should be of type uint8_t. It
 * represents an intermediate value during conversion that is to be
 * output by PUT_BYTE.
 * @param PUT_BYTE writes the converted UTF-8 bytes to any proper destination.
 * It could be a function or a statement, and uses tmp as the input byte.
 * For example, PUT_BYTE could be "*output++ = tmp;" PUT_BYTE will be
 * executed up to 4 times for values in the valid UTF-8 range and up to
 * 7 times in the general case, depending on the length of the converted
 * Unicode character.
 *)
(*
#define PUT_UTF8(val, tmp, PUT_BYTE)\
    {\
        int bytes, shift;\
        uint32_t in = val;\
        if (in < 0x80) {\
            tmp = in;\
            PUT_BYTE\
        } else {\
            bytes = (av_log2(in) + 4) / 5;\
            shift = (bytes - 1) * 6;\
            tmp = (256 - (256 >> bytes)) | (in >> shift);\
            PUT_BYTE\
            while (shift >= 6) {\
                shift -= 6;\
                tmp = 0x80 | ((in >> shift) & 0x3f);\
                PUT_BYTE\
            }\
        }\
    }

/**
 * @def PUT_UTF16(val, tmp, PUT_16BIT)
 * Convert a 32-bit Unicode character to its UTF-16 encoded form (2 or 4 bytes).
 * @param val is an input-only argument and should be of type uint32_t. It holds
 * a UCS-4 encoded Unicode character that is to be converted to UTF-16. If
 * val is given as a function it is executed only once.
 * @param tmp is a temporary variable and should be of type uint16_t. It
 * represents an intermediate value during conversion that is to be
 * output by PUT_16BIT.
 * @param PUT_16BIT writes the converted UTF-16 data to any proper destination
 * in desired endianness. It could be a function or a statement, and uses tmp
 * as the input byte.  For example, PUT_BYTE could be "*output++ = tmp;"
 * PUT_BYTE will be executed 1 or 2 times depending on input character.
 */
#define PUT_UTF16(val, tmp, PUT_16BIT)\
    {\
        uint32_t in = val;\
        if (in < 0x10000) {\
            tmp = in;\
            PUT_16BIT\
        } else {\
            tmp = 0xD800 | ((in - 0x10000) >> 10);\
            PUT_16BIT\
            tmp = 0xDC00 | ((in - 0x10000) & 0x3FF);\
            PUT_16BIT\
        }\
    }\
*)

end.
