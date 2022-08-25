(*
 * Copyright (c) 2011 Reinhard Tartler
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 *)

(**
 * @file
 * Shows how the metadata API can be used in application programs.
 * @example metadata.c
 *)

(*
 * FFVCL - Delphi FFmpeg VCL Components
 * http://www.DelphiFFmpeg.com
 *
 * Original file: doc/examples/metadata.c
 * Ported by CodeCoolie@CNSW 2014/08/29 -> $Date:: 2021-02-22 #$
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

program metadata;

{$APPTYPE CONSOLE}

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

uses
{$IFDEF FPC}
  {$IFDEF MSWINDOWS}
    Windows,
  {$ENDIF}
  SysUtils,
{$ELSE}
  {$IF CompilerVersion >= 23.0}
    {$IFDEF MSWINDOWS}
      Winapi.Windows,
    {$ENDIF}
    System.SysUtils,
  {$ELSE}
    {$IFDEF MSWINDOWS}
      Windows,
    {$ENDIF}
    SysUtils,
  {$IFEND}
{$ENDIF}

{$I headers.inc}
  FFUtils;

function main(): Integer;
var
  fmt_ctx: PAVFormatContext;
  tag: PAVDictionaryEntry;
  ret: Integer;
begin
  fmt_ctx := nil;
  tag := nil;

  if ParamCount <> 1 then
  begin
    Writeln(ErrOutput, Format('usage: %s <input_file>' + sLineBreak +
           'example program to demonstrate the use of the libavformat metadata API.',
           [ExtractFileName(ParamStr(0))]));
    Result := 1;
    Exit;
  end;

  ret := avformat_open_input(@fmt_ctx, PAnsiChar(AnsiString(ParamStr(1))), nil, nil);
  if ret < 0 then
  begin
    Writeln(ErrOutput, Format('Could not open %s', [ParamStr(1)]));
    Result := ret;
    Exit;
  end;

  ret := avformat_find_stream_info(fmt_ctx, nil);
  if ret < 0 then
  begin
    Writeln(ErrOutput, 'Cannot find stream information');
    Result := ret;
    Exit;
  end;

  while True do
  begin
    tag := av_dict_get(fmt_ctx.metadata, '', tag, AV_DICT_IGNORE_SUFFIX);
    if Assigned(tag) then
      Writeln(Format('%s=%s', [string(tag.key), string(tag.value)]))
    else
      Break;
  end;

  avformat_close_input(@fmt_ctx);
  Result := 0;
end;

begin
  try
    ExitCode := main();
  except
    on E: Exception do
      Writeln(ErrOutput, E.ClassName, ': ', E.Message);
  end;
end.
