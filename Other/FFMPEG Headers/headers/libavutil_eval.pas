(*
 * Copyright (c) 2002 Michael Niedermayer <michaelni@gmx.at>
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
 * simple arithmetic expression evaluator
 *)

(*
 * FFVCL - Delphi FFmpeg VCL Components
 * http://www.DelphiFFmpeg.com
 *
 * Original file: libavutil/eval.h
 * Ported by CodeCoolie@CNSW 2014/07/21 -> $Date:: 2020-11-15 #$
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

unit libavutil_eval;

interface

{$I CompilerDefines.inc}

{$I libversion.inc}

type
  PPAVExpr = ^PAVExpr;
  PAVExpr = ^TAVExpr;
  TAVExpr = record
    // need {$ALIGN 8}
    // defined in libavutil/eval.c
  end;

(**
 * Parse and evaluate an expression.
 * Note, this is significantly slower than av_expr_eval().
 *
 * @param res a pointer to a double where is put the result value of
 * the expression, or NAN in case of error
 * @param s expression as a zero terminated string, for example "1+2^3+5*5+sin(2/3)"
 * @param const_names NULL terminated array of zero terminated strings of constant identifiers, for example {"PI", "E", 0}
 * @param const_values a zero terminated array of values for the identifiers from const_names
 * @param func1_names NULL terminated array of zero terminated strings of funcs1 identifiers
 * @param funcs1 NULL terminated array of function pointers for functions which take 1 argument
 * @param func2_names NULL terminated array of zero terminated strings of funcs2 identifiers
 * @param funcs2 NULL terminated array of function pointers for functions which take 2 arguments
 * @param opaque a pointer which will be passed to all functions from funcs1 and funcs2
 * @param log_ctx parent logging context
 * @return >= 0 in case of success, a negative value corresponding to an
 * AVERROR code otherwise
 *)
  Tfuncs1Call = function(p: Pointer; v: double): PDouble; cdecl;
  Tfuncs2Call = function(p: Pointer; v1, v2: Double): PDouble; cdecl;
function av_expr_parse_and_eval(res: Double; const s: PAnsiChar;
                           const const_names: PPAnsiChar; const const_values: PDouble;
                           const func1_names: PPAnsiChar; funcs1: Tfuncs1Call;
                           const func2_names: PPAnsiChar; funcs2: Tfuncs2Call;
                           opaque: Pointer; log_offset: Integer; log_ctx: Pointer): Integer; cdecl; external AVUTIL_LIBNAME name _PU + 'av_expr_parse_and_eval';

(**
 * Parse an expression.
 *
 * @param expr a pointer where is put an AVExpr containing the parsed
 * value in case of successful parsing, or NULL otherwise.
 * The pointed to AVExpr must be freed with av_expr_free() by the user
 * when it is not needed anymore.
 * @param s expression as a zero terminated string, for example "1+2^3+5*5+sin(2/3)"
 * @param const_names NULL terminated array of zero terminated strings of constant identifiers, for example {"PI", "E", 0}
 * @param func1_names NULL terminated array of zero terminated strings of funcs1 identifiers
 * @param funcs1 NULL terminated array of function pointers for functions which take 1 argument
 * @param func2_names NULL terminated array of zero terminated strings of funcs2 identifiers
 * @param funcs2 NULL terminated array of function pointers for functions which take 2 arguments
 * @param log_ctx parent logging context
 * @return >= 0 in case of success, a negative value corresponding to an
 * AVERROR code otherwise
 *)
function av_expr_parse(expr: PPAVExpr; const s: PAnsiChar;
                  const const_names: PPAnsiChar;
                  const func1_names: PPAnsiChar; funcs1: Tfuncs1Call;
                  const func2_names: PPAnsiChar; funcs2: Tfuncs2Call;
                  log_offset: Integer; log_ctx: Pointer): Integer; cdecl; external AVUTIL_LIBNAME name _PU + 'av_expr_parse';

(**
 * Evaluate a previously parsed expression.
 *
 * @param const_values a zero terminated array of values for the identifiers from av_expr_parse() const_names
 * @param opaque a pointer which will be passed to all functions from funcs1 and funcs2
 * @return the value of the expression
 *)
function av_expr_eval(e: PAVExpr; const const_values: PDouble; opaque: Pointer): Double; cdecl; external AVUTIL_LIBNAME name _PU + 'av_expr_eval';

(**
 * Track the presence of variables and their number of occurrences in a parsed expression
 *
 * @param counter a zero-initialized array where the count of each variable will be stored
 * @param size size of array
 * @return 0 on success, a negative value indicates that no expression or array was passed
 * or size was zero
 *)
function av_expr_count_vars(e: PAVExpr; counter: PCardinal; size: Integer): Integer; cdecl; external AVUTIL_LIBNAME name _PU + 'av_expr_count_vars';

(**
 * Track the presence of user provided functions and their number of occurrences
 * in a parsed expression.
 *
 * @param counter a zero-initialized array where the count of each function will be stored
 *                if you passed 5 functions with 2 arguments to av_expr_parse()
 *                then for arg=2 this will use upto 5 entries.
 * @param size size of array
 * @param arg number of arguments the counted functions have
 * @return 0 on success, a negative value indicates that no expression or array was passed
 * or size was zero
 *)
function av_expr_count_func(e: PAVExpr; counter: PCardinal; size, arg: Integer): Integer; cdecl; external AVUTIL_LIBNAME name _PU + 'av_expr_count_func';

(**
 * Free a parsed expression previously created with av_expr_parse().
 *)
procedure av_expr_free(e: PAVExpr); cdecl; external AVUTIL_LIBNAME name _PU + 'av_expr_free';

(**
 * Parse the string in numstr and return its value as a double. If
 * the string is empty, contains only whitespaces, or does not contain
 * an initial substring that has the expected syntax for a
 * floating-point number, no conversion is performed. In this case,
 * returns a value of zero and the value returned in tail is the value
 * of numstr.
 *
 * @param numstr a string representing a number, may contain one of
 * the International System number postfixes, for example 'K', 'M',
 * 'G'. If 'i' is appended after the postfix, powers of 2 are used
 * instead of powers of 10. The 'B' postfix multiplies the value by
 * 8, and can be appended after another postfix or used alone. This
 * allows using for example 'KB', 'MiB', 'G' and 'B' as postfix.
 * @param tail if non-NULL puts here the pointer to the char next
 * after the last parsed character
 *)
function av_strtod(const numstr: PAnsiChar; tail: PPAnsiChar): Double; cdecl; external AVUTIL_LIBNAME name _PU + 'av_strtod';

implementation

end.
