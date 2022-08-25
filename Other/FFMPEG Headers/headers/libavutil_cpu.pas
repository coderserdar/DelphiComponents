(*
 * Copyright (c) 2000, 2001, 2002 Fabrice Bellard
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

(*
 * FFVCL - Delphi FFmpeg VCL Components
 * http://www.DelphiFFmpeg.com
 *
 * Original file: libavutil/cpu.h
 * Ported by CodeCoolie@CNSW 2010/09/17 -> $Date:: 2021-04-25 #$
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

unit libavutil_cpu;

interface

{$I CompilerDefines.inc}

uses
  FFTypes;

{$I libversion.inc}

const
  AV_CPU_FLAG_FORCE   = $80000000; (* force usage of selected flags (OR) *)

  (* lower 16 bits - CPU features *)
  AV_CPU_FLAG_MMX      =     $0001; ///< standard MMX
  AV_CPU_FLAG_MMXEXT   =     $0002; ///< SSE integer functions or AMD MMX ext
  AV_CPU_FLAG_MMX2     =     $0002; ///< SSE integer functions or AMD MMX ext
  AV_CPU_FLAG_3DNOW    =     $0004; ///< AMD 3DNOW
  AV_CPU_FLAG_SSE      =     $0008; ///< SSE functions
  AV_CPU_FLAG_SSE2     =     $0010; ///< PIV SSE2 functions
  AV_CPU_FLAG_SSE2SLOW = $40000000; ///< SSE2 supported, but usually not faster
                                    ///< than regular MMX/SSE (e.g. Core1)
  AV_CPU_FLAG_3DNOWEXT =     $0020; ///< AMD 3DNowExt
  AV_CPU_FLAG_SSE3     =     $0040; ///< Prescott SSE3 functions
  AV_CPU_FLAG_SSE3SLOW = $20000000; ///< SSE3 supported, but usually not faster
                                    ///< than regular MMX/SSE (e.g. Core1)
  AV_CPU_FLAG_SSSE3    =     $0080; ///< Conroe SSSE3 functions
  AV_CPU_FLAG_SSSE3SLOW=  $4000000; ///< SSSE3 supported, but usually not faster
  AV_CPU_FLAG_ATOM     = $10000000; ///< Atom processor, some SSSE3 instructions are slower
  AV_CPU_FLAG_SSE4     =     $0100; ///< Penryn SSE4.1 functions
  AV_CPU_FLAG_SSE42    =     $0200; ///< Nehalem SSE4.2 functions
  AV_CPU_FLAG_AESNI    =    $80000; ///< Advanced Encryption Standard functions
  AV_CPU_FLAG_AVX      =     $4000; ///< AVX functions: requires OS support even if YMM registers aren't used
  AV_CPU_FLAG_AVXSLOW  =  $8000000; ///< AVX supported, but slow when using YMM registers (e.g. Bulldozer)
  AV_CPU_FLAG_XOP      =     $0400; ///< Bulldozer XOP functions
  AV_CPU_FLAG_FMA4     =     $0800; ///< Bulldozer FMA4 functions
  AV_CPU_FLAG_CMOV     =     $1000; ///< supports cmov instruction
  AV_CPU_FLAG_AVX2     =     $8000; ///< AVX2 functions: requires OS support even if YMM registers aren't used
  AV_CPU_FLAG_FMA3     =    $10000; ///< Haswell FMA3 functions
  AV_CPU_FLAG_BMI1     =    $20000; ///< Bit Manipulation Instruction Set 1
  AV_CPU_FLAG_BMI2     =    $40000; ///< Bit Manipulation Instruction Set 2
  AV_CPU_FLAG_AVX512   =   $100000; ///< AVX-512 functions: requires OS support even if YMM/ZMM registers aren't used

  AV_CPU_FLAG_ALTIVEC  =     $0001; ///< standard
  AV_CPU_FLAG_VSX      =     $0002; ///< ISA 2.06
  AV_CPU_FLAG_POWER8   =     $0004; ///< ISA 2.07

  AV_CPU_FLAG_ARMV5TE  =    (1 shl 0);
  AV_CPU_FLAG_ARMV6    =    (1 shl 1);
  AV_CPU_FLAG_ARMV6T2  =    (1 shl 2);
  AV_CPU_FLAG_VFP      =    (1 shl 3);
  AV_CPU_FLAG_VFPV3    =    (1 shl 4);
  AV_CPU_FLAG_NEON     =    (1 shl 5);
  AV_CPU_FLAG_ARMV8    =    (1 shl 6);
  AV_CPU_FLAG_VFP_VM   =    (1 shl 7); ///< VFPv2 vector mode, deprecated in ARMv7-A and unavailable in various CPUs implementations
  AV_CPU_FLAG_SETEND   =    (1 shl 16);

  AV_CPU_FLAG_MMI      =    (1 shl 0);
  AV_CPU_FLAG_MSA      =    (1 shl 1);

(**
 * Return the flags which specify extensions supported by the CPU.
 * The returned value is affected by av_force_cpu_flags() if that was used
 * before. So av_get_cpu_flags() can easily be used in an application to
 * detect the enabled cpu flags.
 *)
function av_get_cpu_flags: Integer; cdecl; external AVUTIL_LIBNAME name _PU + 'av_get_cpu_flags';

(**
 * Disables cpu detection and forces the specified flags.
 * -1 is a special case that disables forcing of specific flags.
 *)
procedure av_force_cpu_flags(flags: Integer); cdecl; external AVUTIL_LIBNAME name _PU + 'av_force_cpu_flags';

(**
 * Set a mask on flags returned by av_get_cpu_flags().
 * This function is mainly useful for testing.
 * Please use av_force_cpu_flags() and av_get_cpu_flags() instead which are more flexible
 *)
procedure av_set_cpu_flags_mask(mask: Integer); cdecl; external AVUTIL_LIBNAME name _PU + 'av_set_cpu_flags_mask';

(**
 * Parse CPU flags from a string.
 *
 * The returned flags contain the specified flags as well as related unspecified flags.
 *
 * This function exists only for compatibility with libav.
 * Please use av_parse_cpu_caps() when possible.
 * @return a combination of AV_CPU_* flags, negative on error.
 *)
function av_parse_cpu_flags(const s: PAnsiChar): Integer; cdecl; external AVUTIL_LIBNAME name _PU + 'av_parse_cpu_flags';

(**
 * Parse CPU caps from a string and update the given AV_CPU_* flags based on that.
 *
 * @return negative on error.
 *)
function av_parse_cpu_caps(flags: PCardinal; const s: PAnsiChar): Integer; cdecl; external AVUTIL_LIBNAME name _PU + 'av_parse_cpu_caps';

(**
 * @return the number of logical CPU cores present.
 *)
function av_cpu_count(): Integer; cdecl; external AVUTIL_LIBNAME name _PU + 'av_cpu_count';

(**
 * Get the maximum data alignment that may be required by FFmpeg.
 *
 * Note that this is affected by the build configuration and the CPU flags mask,
 * so e.g. if the CPU supports AVX, but libavutil has been built with
 * --disable-avx or the AV_CPU_FLAG_AVX flag has been disabled through
 *  av_set_cpu_flags_mask(), then this function will behave as if AVX is not
 *  present.
 *)
function av_cpu_max_align(): Size_t; cdecl; external AVUTIL_LIBNAME name _PU + 'av_cpu_max_align';

implementation

end.
