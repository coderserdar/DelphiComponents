(*
 * FFVCL - Delphi FFmpeg VCL Components
 * http://www.DelphiFFmpeg.com
 *
 * This file is a unit of some utils on ffmpeg.
 * Created by CodeCoolie@CNSW 2014/08/23 -> $Date:: 2018-01-15 #$
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

unit FFUtils;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

uses
{$IFDEF FPC}
  {$IFDEF MSWINDOWS}
    Windows,
  {$ENDIF}
{$ELSE}
  {$IF CompilerVersion >= 23.0}
    {$IFDEF MSWINDOWS}
      Winapi.Windows,
    {$ENDIF}
  {$ELSE}
    {$IFDEF MSWINDOWS}
      Windows,
    {$ENDIF}
  {$IFEND}
{$ENDIF}
  libavformat,
  FFTypes;

function snprintf(buf: PAnsiChar; size: Cardinal; const fmt: PAnsiChar): Integer; cdecl varargs; external 'msvcrt' name '_snprintf';

function PPtrIdx(P: PPAnsiChar; I: Integer): PAnsiChar; overload;
function PPtrIdx(P: PPByte; I: Integer): PByte; overload;
function PPtrIdx(P: PInteger; I: Integer): Integer; overload;
function PPtrIdx(P: PInt64; I: Integer): Int64; overload;

function PtrIdx(P: PSmallInt; I: Integer): PSmallInt; overload;
function PtrIdx(P: PSingle; I: Integer): PSingle; overload;
function PtrIdx(P: PDouble; I: Integer): PDouble; overload;
function PtrIdx(P: PInteger; I: Integer): PInteger; overload;

function PPtrIdx(P: PPAVStream; I: Integer): PAVStream; overload;

type
  av_intfloat32 = record
    case Integer of
      0: (i: Cardinal);
      1: (f: Single);
  end;

(**
 * Reinterpret a 32-bit integer as a float.
 *)
function av_int2float(i: Cardinal): Single;

implementation

function PPtrIdx(P: PPAnsiChar; I: Integer): PAnsiChar;
begin
  Inc(P, I);
  Result := P^;
end;

function PPtrIdx(P: PPByte; I: Integer): PByte;
begin
  Inc(P, I);
  Result := P^;
end;

function PPtrIdx(P: PInteger; I: Integer): Integer;
begin
  Inc(P, I);
  Result := P^;
end;

function PPtrIdx(P: PInt64; I: Integer): Int64;
begin
  Inc(P, I);
  Result := P^;
end;

function PtrIdx(P: PSmallInt; I: Integer): PSmallInt;
begin
  Result := P;
  Inc(Result, I);
end;

function PtrIdx(P: PSingle; I: Integer): PSingle;
begin
  Inc(P, I);
  Result := P;
end;

function PtrIdx(P: PDouble; I: Integer): PDouble;
begin
  Inc(P, I);
  Result := P;
end;

function PtrIdx(P: PInteger; I: Integer): PInteger;
begin
  Inc(P, I);
  Result := P;
end;

function PPtrIdx(P: PPAVStream; I: Integer): PAVStream;
begin
  Inc(P, I);
  Result := P^;
end;

function av_int2float(i: Cardinal): Single;
var
  v: av_intfloat32;
begin
  v.i := i;
  Result := v.f;
end;

end.
