(*
 * Copyright (c) 2014 Lukasz Marek
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

(*
 * FFVCL - Delphi FFmpeg VCL Components
 * http://www.DelphiFFmpeg.com
 *
 * Original file: doc/examples/avio_list_dir.c
 * Ported by CodeCoolie@CNSW 2015/11/17 -> $Date:: 2021-04-18 #$
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

program avio_dir_cmd;

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

function print_error(const filename: string; err: Integer): string;
var
  I: Integer;
  S: string;
begin
  S := '';
  for I := 0 to High(CErrorList) do
    if CErrorList[I].err = err then
    begin
      S := CErrorList[I].msg;
      Break;
    end;
  if S = '' then
    S := Format('Error number %d occurred', [err]);
  if filename <> '' then
    Result := filename + ': ' + S
  else
    Result := S;
end;

function type_string(type_: Integer): PAnsiChar;
begin
  case TAVIODirEntryType(type_) of
    AVIO_ENTRY_DIRECTORY: Result := '<DIR>';
    AVIO_ENTRY_FILE: Result := '<FILE>';
    AVIO_ENTRY_BLOCK_DEVICE: Result := '<BLOCK DEVICE>';
    AVIO_ENTRY_CHARACTER_DEVICE: Result := '<CHARACTER DEVICE>';
    AVIO_ENTRY_NAMED_PIPE: Result := '<PIPE>';
    AVIO_ENTRY_SYMBOLIC_LINK: Result := '<LINK>';
    AVIO_ENTRY_SOCKET: Result := '<SOCKET>';
    AVIO_ENTRY_SERVER: Result := '<SERVER>';
    AVIO_ENTRY_SHARE: Result := '<SHARE>';
    AVIO_ENTRY_WORKGROUP: Result := '<WORKGROUP>';
    AVIO_ENTRY_UNKNOWN: Result := '<UNKNOWN>';
  else
    Result := '<UNKNOWN>';
  end;
end;

function list_op(const input_dir: string): Integer;
var
  entry: PAVIODirEntry;
  ctx: PAVIODirContext;
  cnt, ret: Integer;
  filemode: string;
  uid_and_gid: string;
  s1, s2, s3, s4: string;
label
  fail;
begin
  entry := nil;
  ctx := nil;

  ret := avio_open_dir(@ctx, PAnsiChar(AnsiString(input_dir)), nil);
  if ret < 0 then
  begin
    //av_log(nil, AV_LOG_ERROR, 'Cannot open directory: %s.'#10, av_err2str(ret));
    av_log(nil, AV_LOG_ERROR, 'Cannot open directory: %s.'#10, PAnsiChar(AnsiString(print_error('', ret))));
    goto fail;
  end;

  cnt := 0;
  while True do
  begin
    ret := avio_read_dir(ctx, @entry);
    if ret < 0 then
    begin
      //av_log(nil, AV_LOG_ERROR, 'Cannot list directory: %s.'#10, av_err2str(ret));
      av_log(nil, AV_LOG_ERROR, 'Cannot list directory: %s.'#10, PAnsiChar(AnsiString(print_error('', ret))));
      goto fail;
    end;
    if not Assigned(entry) then
      Break;
    if entry.filemode = -1 then
      filemode := '???'
    else
      filemode := StringReplace(Format('%3d', [entry.filemode]), ' ', '0', [rfReplaceAll]);
    uid_and_gid := Format('%d(%d)', [entry.user_id, entry.group_id]);
    s1 := StringReplace(Format('%12d', [entry.size]), ' ', '0', [rfReplaceAll]);
    s2 := StringReplace(Format('%16d', [entry.modification_timestamp]), ' ', '0', [rfReplaceAll]);
    s3 := StringReplace(Format('%16d', [entry.access_timestamp]), ' ', '0', [rfReplaceAll]);
    s4 := StringReplace(Format('%16d', [entry.status_change_timestamp]), ' ', '0', [rfReplaceAll]);
    if cnt = 0 then
      av_log(nil, AV_LOG_INFO, '%-9s %12s %30s %10s %s %16s %16s %16s'#10,
             'TYPE', 'SIZE', 'NAME', 'UID(GID)', 'UGO', 'MODIFIED',
             'ACCESSED', 'STATUS_CHANGED');
    av_log(nil, AV_LOG_INFO, '%-9s %s %30s %10s %s %16"PRId64" %16"PRId64" %16"PRId64"'#10,
           type_string(entry.type_),
           PAnsiChar(AnsiString(s1)),
           entry.name,
           PAnsiChar(AnsiString(uid_and_gid)),
           PAnsiChar(AnsiString(filemode)),
           PAnsiChar(AnsiString(s2)),
           PAnsiChar(AnsiString(s3)),
           PAnsiChar(AnsiString(s4)));
    avio_free_directory_entry(@entry);
    Inc(cnt);
  end;

fail:
  avio_close_dir(@ctx);
  Result := ret;
end;

procedure usage(const program_name: string);
begin
  Writeln(ErrOutput, Format('usage: %s input_dir' + sLineBreak +
          'API example program to show how to list files in directory ' +
          'accessed through AVIOContext.', [program_name]));
end;

function main(): Integer;
var
  ret: Integer;
begin
  av_log_set_level(AV_LOG_DEBUG);

  if ParamCount < 1 then
  begin
    usage(ExtractFileName(ParamStr(0)));
    Result := 1;
    Exit;
  end;

  avformat_network_init();

  ret := list_op(ParamStr(1));

  avformat_network_deinit();

  if ret < 0 then
    Result := 1
  else
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
