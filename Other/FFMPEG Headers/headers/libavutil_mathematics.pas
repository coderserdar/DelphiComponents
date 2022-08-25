(*
 * copyright (c) 2005-2012 Michael Niedermayer <michaelni@gmx.at>
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
 * @addtogroup lavu_math
 * Mathematical utilities for working with timestamp and time base.
 *)

(*
 * FFVCL - Delphi FFmpeg VCL Components
 * http://www.DelphiFFmpeg.com
 *
 * Original file: libavutil/mathematics.h
 * Ported by CodeCoolie@CNSW 2008/03/29 -> $Date:: 2017-02-09 #$
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

unit libavutil_mathematics;

interface

{$I CompilerDefines.inc}

uses
{$IFDEF BCB}
  FFTypes,
{$ENDIF}
  libavutil_rational;

{$I libversion.inc}

const
  M_E            = 2.7182818284590452354;   (* e *)
  {$EXTERNALSYM M_E}
  M_LN2          = 0.69314718055994530942;  (* log_e 2 *)
  {$EXTERNALSYM M_LN2}
  M_LN10         = 2.30258509299404568402;  (* log_e 10 *)
  {$EXTERNALSYM M_LN10}
  M_LOG2_10      = 3.32192809488736234787;  (* log_2 10 *)
  {$EXTERNALSYM M_PHI}
  M_PHI          = 1.61803398874989484820;  (* phi / golden ratio *)
  {$EXTERNALSYM M_LOG2_10}
  M_PI           = 3.14159265358979323846;  (* pi *)
  {$EXTERNALSYM M_PI}
  M_PI_2         = 1.57079632679489661923;  (* pi/2 *)
  {$EXTERNALSYM M_PI_2}
  M_SQRT1_2      = 0.70710678118654752440;  (* 1/sqrt(2) *)
  {$EXTERNALSYM M_SQRT1_2}
  M_SQRT2        = 1.41421356237309504880;  (* sqrt(2) *)
  {$EXTERNALSYM M_SQRT2}
//#ifndef NAN
//#define NAN            av_int2float(0x7fc00000)
//#endif
//#ifndef INFINITY
//#define INFINITY       av_int2float(0x7f800000)
//#endif

(**
 * @addtogroup lavu_math
 *
 * @{
 *)

(**
 * Rounding methods.
 *)
type
  TAVRounding = (
    AV_ROUND_ZERO     = 0, ///< Round toward zero
    AV_ROUND_INF      = 1, ///< Round away from zero
    AV_ROUND_DOWN     = 2, ///< Round toward -infinity
    AV_ROUND_UP       = 3, ///< Round toward +infinity
    AV_ROUND_NEAR_INF = 5, ///< Round to nearest and halfway cases away from zero
    (**
     * Flag telling rescaling functions to pass `INT64_MIN`/`MAX` through
     * unchanged, avoiding special cases for #AV_NOPTS_VALUE.
     *
     * Unlike other values of the enumeration AVRounding, this value is a
     * bitmask that must be used in conjunction with another value of the
     * enumeration through a bitwise OR, in order to set behavior for normal
     * cases.
     *
     * @code{.c}
     * av_rescale_rnd(3, 1, 2, AV_ROUND_UP | AV_ROUND_PASS_MINMAX);
     * // Rescaling 3:
     * //     Calculating 3 * 1 / 2
     * //     3 / 2 is rounded up to 2
     * //     => 2
     *
     * av_rescale_rnd(AV_NOPTS_VALUE, 1, 2, AV_ROUND_UP | AV_ROUND_PASS_MINMAX);
     * // Rescaling AV_NOPTS_VALUE:
     * //     AV_NOPTS_VALUE == INT64_MIN
     * //     AV_NOPTS_VALUE is passed through
     * //     => AV_NOPTS_VALUE
     * @endcode
     *)
    AV_ROUND_PASS_MINMAX = 8192
  );

(**
 * Compute the greatest common divisor of two integer operands.
 *
 * @param a,b Operands
 * @return GCD of a and b up to sign; if a >= 0 and b >= 0, return value is >= 0;
 * if a == 0 and b == 0, returns 0.
 *)
function av_gcd(a, b: Int64): Int64; cdecl; external AVUTIL_LIBNAME name _PU + 'av_gcd';

(**
 * Rescale a 64-bit integer with rounding to nearest.
 *
 * The operation is mathematically equivalent to `a * b / c`, but writing that
 * directly can overflow.
 *
 * This function is equivalent to av_rescale_rnd() with #AV_ROUND_NEAR_INF.
 *
 * @see av_rescale_rnd(), av_rescale_q(), av_rescale_q_rnd()
 *)
function av_rescale(a, b, c: Int64): Int64; cdecl; external AVUTIL_LIBNAME name _PU + 'av_rescale';

(**
 * Rescale a 64-bit integer with specified rounding.
 *
 * The operation is mathematically equivalent to `a * b / c`, but writing that
 * directly can overflow, and does not support different rounding methods.
 *
 * @see av_rescale(), av_rescale_q(), av_rescale_q_rnd()
 *)
function av_rescale_rnd(a, b, c: Int64; rnd: TAVRounding): Int64; cdecl; external AVUTIL_LIBNAME name _PU + 'av_rescale_rnd';

(**
 * Rescale a 64-bit integer by 2 rational numbers.
 *
 * The operation is mathematically equivalent to `a * bq / cq`.
 *
 * This function is equivalent to av_rescale_q_rnd() with #AV_ROUND_NEAR_INF.
 *
 * @see av_rescale(), av_rescale_rnd(), av_rescale_q_rnd()
 *)
function av_rescale_q(a: Int64; bq, cq: TAVRational): Int64; cdecl; external AVUTIL_LIBNAME name _PU + 'av_rescale_q';

(**
 * Rescale a 64-bit integer by 2 rational numbers with specified rounding.
 *
 * The operation is mathematically equivalent to `a * bq / cq`.
 *
 * @see av_rescale(), av_rescale_rnd(), av_rescale_q()
 *)
function av_rescale_q_rnd(a: Int64; bq, cq: TAVRational;
                            rnd: Integer{TAVRounding}): Int64; cdecl; external AVUTIL_LIBNAME name _PU + 'av_rescale_q_rnd';

(**
 * Compare two timestamps each in its own time base.
 *
 * @return One of the following values:
 *         - -1 if `ts_a` is before `ts_b`
 *         - 1 if `ts_a` is after `ts_b`
 *         - 0 if they represent the same position
 *
 * @warning
 * The result of the function is undefined if one of the timestamps is outside
 * the `int64_t` range when represented in the other's timebase.
 *)
function av_compare_ts(ts_a: Int64; tb_a: TAVRational; ts_b: Int64; tb_b: TAVRational): Integer; cdecl; external AVUTIL_LIBNAME name _PU + 'av_compare_ts';

(**
 * Compare the remainders of two integer operands divided by a common divisor.
 *
 * In other words, compare the least significant `log2(mod)` bits of integers
 * `a` and `b`.
 *
 * @code{.c}
 * av_compare_mod(0x11, 0x02, 0x10) < 0 // since 0x11 % 0x10  (0x1) < 0x02 % 0x10  (0x2)
 * av_compare_mod(0x11, 0x02, 0x20) > 0 // since 0x11 % 0x20 (0x11) > 0x02 % 0x20 (0x02)
 * @endcode
 *
 * @param a,b Operands
 * @param mod Divisor; must be a power of 2
 * @return
 *         - a negative value if `a % mod < b % mod`
 *         - a positive value if `a % mod > b % mod`
 *         - zero             if `a % mod == b % mod`
 *)
function av_compare_mod(a, b, mod_: Int64): Int64; cdecl; external AVUTIL_LIBNAME name _PU + 'av_compare_mod';

(**
 * Rescale a timestamp while preserving known durations.
 *
 * This function is designed to be called per audio packet to scale the input
 * timestamp to a different time base. Compared to a simple av_rescale_q()
 * call, this function is robust against possible inconsistent frame durations.
 *
 * The `last` parameter is a state variable that must be preserved for all
 * subsequent calls for the same stream. For the first call, `*last` should be
 * initialized to #AV_NOPTS_VALUE.
 *
 * @param[in]     in_tb    Input time base
 * @param[in]     in_ts    Input timestamp
 * @param[in]     fs_tb    Duration time base; typically this is finer-grained
 *                         (greater) than `in_tb` and `out_tb`
 * @param[in]     duration Duration till the next call to this function (i.e.
 *                         duration of the current packet/frame)
 * @param[in,out] last     Pointer to a timestamp expressed in terms of
 *                         `fs_tb`, acting as a state variable
 * @param[in]     out_tb   Output timebase
 * @return        Timestamp expressed in terms of `out_tb`
 *
 * @note In the context of this function, "duration" is in term of samples, not
 *       seconds.
 *)
function av_rescale_delta(in_tb: TAVRational; in_ts: Int64; fs_tb: TAVRational; duration: Integer; last: PInt64; out_tb: TAVRational): Int64; cdecl; external AVUTIL_LIBNAME name _PU + 'av_rescale_delta';

(**
 * Add a value to a timestamp.
 *
 * This function guarantees that when the same value is repeatly added that
 * no accumulation of rounding errors occurs.
 *
 * @param[in] ts     Input timestamp
 * @param[in] ts_tb  Input timestamp time base
 * @param[in] inc    Value to be added
 * @param[in] inc_tb Time base of `inc`
 *)
function av_add_stable(ts_tb: TAVRational; ts: Int64; inc_tb: TAVRational; incr: TAVRational): Int64; cdecl; external AVUTIL_LIBNAME name _PU + 'av_add_stable';


(**
 * @}
 *)

implementation

end.
