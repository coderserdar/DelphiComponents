(*
 * Copyright (c) 2012 Stefano Sabatini
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
 * @example resampling_audio.c
 * libswresample API use example.
 *)

(*
 * FFVCL - Delphi FFmpeg VCL Components
 * http://www.DelphiFFmpeg.com
 *
 * Original file: doc/examples/resampling.c
 * Ported by CodeCoolie@CNSW 2014/08/30 -> $Date:: 2019-08-24 #$
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

program resampling_audio;

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

function get_format_from_sample_fmt(const fmt: PPAnsiChar;
  sample_fmt: TAVSampleFormat): Integer;
type
  Psample_fmt_entry = ^Tsample_fmt_entry;
  Tsample_fmt_entry = record
    sample_fmt: TAVSampleFormat;
    fmt_be, fmt_le: PAnsiChar;
  end;
const
  sample_fmt_entries: array[0..4] of Tsample_fmt_entry = (
      (sample_fmt: AV_SAMPLE_FMT_U8;  fmt_be: 'u8';    fmt_le: 'u8'    ),
      (sample_fmt: AV_SAMPLE_FMT_S16; fmt_be: 's16be'; fmt_le: 's16le' ),
      (sample_fmt: AV_SAMPLE_FMT_S32; fmt_be: 's32be'; fmt_le: 's32le' ),
      (sample_fmt: AV_SAMPLE_FMT_FLT; fmt_be: 'f32be'; fmt_le: 'f32le' ),
      (sample_fmt: AV_SAMPLE_FMT_DBL; fmt_be: 'f64be'; fmt_le: 'f64le' )
    );
var
  i: Integer;
  entry: Psample_fmt_entry;
begin
  fmt^ := nil;

  for i := 0 to High(sample_fmt_entries) do
  begin
    entry := @sample_fmt_entries[i];
    if sample_fmt = entry.sample_fmt then
    begin
      fmt^ := entry.fmt_le; //AV_NE(entry.fmt_be, entry.fmt_le);
      Result := 0;
      Exit;
    end;
  end;

  Writeln(ErrOutput,
          Format('Sample format %s not supported as output format',
          [av_get_sample_fmt_name(sample_fmt)]));
  Result := AVERROR_EINVAL;
end;

(**
 * Fill dst buffer with nb_samples, generated starting from t.
 *)
procedure fill_samples(dst: PDouble; nb_samples, nb_channels, sample_rate: Integer; t: PDouble);
var
  i, j: Integer;
  tincr: Double;
  dstp: PDouble;
  c: Double;
begin
  tincr := 1.0 / sample_rate;
  dstp := dst;
  c := 2 * M_PI * 440.0;

  (* generate sin tone with 440Hz frequency and duplicated channels *)
  for i := 0 to nb_samples - 1 do
  begin
    dstp^ := sin(c * t^);
    for j := 1 to nb_channels - 1 do
      PtrIdx(dstp, j)^ := dstp^;
    Inc(dstp, nb_channels);
    t^ := t^ + tincr;
  end;
end;

function main(): Integer;
var
  src_ch_layout, dst_ch_layout: Int64;
  src_rate, dst_rate: Integer;
  src_data, dst_data: PPByte;
  src_nb_channels, dst_nb_channels: Integer;
  src_linesize, dst_linesize: Integer;
  src_nb_samples, dst_nb_samples, max_dst_nb_samples: Integer;
  src_sample_fmt, dst_sample_fmt: TAVSampleFormat;
  dst_filename: string;
  dst_file: THandle;
  dst_bufsize: Integer;
  fmt: PAnsiChar;
  swr_ctx: PSwrContext;
  t: Double;
  ret: Integer;
label
  the_end;
begin
  src_ch_layout := AV_CH_LAYOUT_STEREO;
  dst_ch_layout := AV_CH_LAYOUT_SURROUND;
  src_rate := 48000;
  dst_rate := 44100;
  src_data := nil;
  dst_data := nil;
  src_nb_samples := 1024;
  src_sample_fmt := AV_SAMPLE_FMT_DBL;
  dst_sample_fmt := AV_SAMPLE_FMT_S16;

  if ParamCount <> 1 then
  begin
    Writeln(ErrOutput, Format('Usage: %s output_file' + sLineBreak +
            'API example program to show how to resample an audio stream with libswresample.' + sLineBreak +
            'This program generates a series of audio frames, resamples them to a specified ' +
            'output format and rate and saves them to an output file named output_file.',
        [ExtractFileName(ParamStr(0))]));
    Result := 1;
    Exit;
  end;
  dst_filename := ParamStr(1);

  dst_file := FileCreate(dst_filename);
  if dst_file = INVALID_HANDLE_VALUE then
  begin
    Writeln(ErrOutput, Format('Could not open destination file %s', [dst_filename]));
    Result := 1;
    Exit;
  end;

  (* create resampler context *)
  swr_ctx := swr_alloc();
  if not Assigned(swr_ctx) then
  begin
    Writeln(ErrOutput, 'Could not allocate resampler context');
    ret := AVERROR_ENOMEM;
    goto the_end;
  end;

  (* set options *)
  av_opt_set_int(swr_ctx, 'in_channel_layout',    src_ch_layout, 0);
  av_opt_set_int(swr_ctx, 'in_sample_rate',       src_rate, 0);
  av_opt_set_sample_fmt(swr_ctx, 'in_sample_fmt', src_sample_fmt, 0);

  av_opt_set_int(swr_ctx, 'out_channel_layout',    dst_ch_layout, 0);
  av_opt_set_int(swr_ctx, 'out_sample_rate',       dst_rate, 0);
  av_opt_set_sample_fmt(swr_ctx, 'out_sample_fmt', dst_sample_fmt, 0);

  (* initialize the resampling context *)
  ret := swr_init(swr_ctx);
  if ret < 0 then
  begin
    Writeln(ErrOutput, 'Failed to initialize the resampling context');
    goto the_end;
  end;

  (* allocate source and destination samples buffers *)

  src_nb_channels := av_get_channel_layout_nb_channels(src_ch_layout);
  ret := av_samples_alloc_array_and_samples(@src_data, @src_linesize, src_nb_channels,
                                           src_nb_samples, src_sample_fmt, 0);
  if ret < 0 then
  begin
    Writeln(ErrOutput, 'Could not allocate source samples');
    goto the_end;
  end;

  (* compute the number of converted samples: buffering is avoided
   * ensuring that the output buffer will contain at least all the
   * converted input samples *)
  dst_nb_samples := av_rescale_rnd(src_nb_samples, dst_rate, src_rate, AV_ROUND_UP);
  max_dst_nb_samples := dst_nb_samples;

  (* buffer is going to be directly written to a rawaudio file, no alignment *)
  dst_nb_channels := av_get_channel_layout_nb_channels(dst_ch_layout);
  ret := av_samples_alloc_array_and_samples(@dst_data, @dst_linesize, dst_nb_channels,
                                           dst_nb_samples, dst_sample_fmt, 0);
  if ret < 0 then
  begin
    Writeln(ErrOutput, 'Could not allocate destination samples');
    goto the_end;
  end;

  t := 0;
  repeat
    (* generate synthetic audio *)
    fill_samples(PDouble(src_data^), src_nb_samples, src_nb_channels, src_rate, @t);

    (* compute destination number of samples *)
    dst_nb_samples := av_rescale_rnd(swr_get_delay(swr_ctx, src_rate) +
                                    src_nb_samples, dst_rate, src_rate, AV_ROUND_UP);
    if dst_nb_samples > max_dst_nb_samples then
    begin
      av_freep(@dst_data^);
      ret := av_samples_alloc(dst_data, @dst_linesize, dst_nb_channels,
                             dst_nb_samples, dst_sample_fmt, 1);
      if ret < 0 then
        Break;
      max_dst_nb_samples := dst_nb_samples;
    end;

    (* convert to destination format *)
    ret := swr_convert(swr_ctx, dst_data, dst_nb_samples, src_data, src_nb_samples);
    if ret < 0 then
    begin
      Writeln(ErrOutput, 'Error while converting');
      goto the_end;
    end;
    dst_bufsize := av_samples_get_buffer_size(@dst_linesize, dst_nb_channels,
                                             ret, dst_sample_fmt, 1);
    if dst_bufsize < 0 then
    begin
      Writeln(ErrOutput, 'Could not get sample buffer size');
      goto the_end;
    end;
    Writeln(Format('t:%f in:%d out:%d', [t, src_nb_samples, ret]));
    FileWrite(dst_file, dst_data^^, dst_bufsize);
  until t >= 10;

  ret := get_format_from_sample_fmt(@fmt, dst_sample_fmt);
  if ret < 0 then
    goto the_end;
  Writeln(ErrOutput, Format('Resampling succeeded. Play the output file with the command:' + sLineBreak +
          'ffplay -f %s -channel_layout %d -channels %d -ar %d %s',
          [fmt, dst_ch_layout, dst_nb_channels, dst_rate, dst_filename]));

the_end:
  FileClose(dst_file);

  if Assigned(src_data) then
    av_freep(@src_data^);
  av_freep(@src_data);

  if Assigned(dst_data) then
    av_freep(@dst_data^);
  av_freep(@dst_data);

  swr_free(@swr_ctx);
  Result := Ord(ret < 0);
end;

begin
  try
    ExitCode := main();
  except
    on E: Exception do
      Writeln(ErrOutput, E.ClassName, ': ', E.Message);
  end;
end.
