(*
 * copyright (c) 2013 Andrew Kelley
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
 * libavfilter API usage example.
 *
 * @example filter_audio.c
 * This example will generate a sine wave audio,
 * pass it through a simple filter chain, and then compute the MD5 checksum of
 * the output data.
 *
 * The filter chain it uses is:
 * (input) -> abuffer -> volume -> aformat -> abuffersink -> (output)
 *
 * abuffer: This provides the endpoint where you can feed the decoded samples.
 * volume: In this example we hardcode it to 0.90.
 * aformat: This converts the samples to the samplefreq, channel layout,
 *          and sample format required by the audio device.
 * abuffersink: This provides the endpoint where you can read the samples after
 *              they have passed through the filter chain.
 *)

(*
 * FFVCL - Delphi FFmpeg VCL Components
 * http://www.DelphiFFmpeg.com
 *
 * Original file: doc/examples/filter_audio.c
 * Ported by CodeCoolie@CNSW 2014/08/27 -> $Date:: 2019-08-24 #$
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

program filter_audio;

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

const
  INPUT_SAMPLERATE     = 48000;
  INPUT_FORMAT         = AV_SAMPLE_FMT_FLTP;
  INPUT_CHANNEL_LAYOUT = AV_CH_LAYOUT_5POINT0;

  VOLUME_VAL = '0.90';

function init_filter_graph(graph: PPAVFilterGraph; src: PPAVFilterContext;
  sink: PPAVFilterContext): Integer;
var
  filter_graph: PAVFilterGraph;
  abuffer_ctx: PAVFilterContext;
  abuffer: PAVFilter;
  volume_ctx: PAVFilterContext;
  volume: PAVFilter;
  aformat_ctx: PAVFilterContext;
  aformat: PAVFilter;
  abuffersink_ctx: PAVFilterContext;
  abuffersink: PAVFilter;

  options_dict: PAVDictionary;
  options_str: array[0..1024-1] of AnsiChar;
  ch_layout: array[0..64-1] of AnsiChar;

  err: Integer;

  I64: Int64;
{$IF Defined(VER140) Or Defined(VER150)}
  Lo, Hi: Integer;
{$IFEND}
begin
  options_dict := nil;

  (* Create a new filtergraph, which will contain all the filters. *)
  filter_graph := avfilter_graph_alloc();
  if not Assigned(filter_graph) then
  begin
    Writeln(ErrOutput, 'Unable to create filter graph.');
    Result := AVERROR_ENOMEM;
    Exit;
  end;

  (* Create the abuffer filter;
   * it will be used for feeding the data into the graph. *)
  abuffer := avfilter_get_by_name('abuffer');
  if not Assigned(abuffer) then
  begin
    Writeln(ErrOutput, 'Could not find the abuffer filter.');
    Result := AVERROR_FILTER_NOT_FOUND;
    Exit;
  end;

  abuffer_ctx := avfilter_graph_alloc_filter(filter_graph, abuffer, 'src');
  if not Assigned(abuffer_ctx) then
  begin
    Writeln(ErrOutput, 'Could not allocate the abuffer instance.');
    Result := AVERROR_ENOMEM;
    Exit;
  end;

  (* Set the filter options through the AVOptions API. *)
  av_get_channel_layout_string(@ch_layout[0], SizeOf(ch_layout), 0, INPUT_CHANNEL_LAYOUT);
  av_opt_set    (abuffer_ctx, 'channel_layout', @ch_layout[0],                        AV_OPT_SEARCH_CHILDREN);
  av_opt_set    (abuffer_ctx, 'sample_fmt',     av_get_sample_fmt_name(INPUT_FORMAT), AV_OPT_SEARCH_CHILDREN);
  av_opt_set_q  (abuffer_ctx, 'time_base',      av_make_q(1, INPUT_SAMPLERATE),       AV_OPT_SEARCH_CHILDREN);
  av_opt_set_int(abuffer_ctx, 'sample_rate',    INPUT_SAMPLERATE,                     AV_OPT_SEARCH_CHILDREN);

  (* Now initialize the filter; we pass NULL options, since we have already
   * set all the options above. *)
  err := avfilter_init_str(abuffer_ctx, nil);
  if err < 0 then
  begin
    Writeln(ErrOutput, 'Could not initialize the abuffer filter.');
    Result := err;
    Exit;
  end;

  (* Create volume filter. *)
  volume := avfilter_get_by_name('volume');
  if not Assigned(volume) then
  begin
    Writeln(ErrOutput, 'Could not find the volume filter.');
    Result := AVERROR_FILTER_NOT_FOUND;
    Exit;
  end;

  volume_ctx := avfilter_graph_alloc_filter(filter_graph, volume, 'volume');
  if not Assigned(volume_ctx) then
  begin
    Writeln(ErrOutput, 'Could not allocate the volume instance.');
    Result := AVERROR_ENOMEM;
    Exit;
  end;

  (* A different way of passing the options is as key/value pairs in a
   * dictionary. *)
  av_dict_set(@options_dict, 'volume', VOLUME_VAL, 0);
  err := avfilter_init_dict(volume_ctx, @options_dict);
  av_dict_free(@options_dict);
  if err < 0 then
  begin
    Writeln(ErrOutput, 'Could not initialize the volume filter.');
    Result := err;
    Exit;
  end;

  (* Create the aformat filter;
   * it ensures that the output is of the format we want. *)
  aformat := avfilter_get_by_name('aformat');
  if not Assigned(aformat) then
  begin
    Writeln(ErrOutput, 'Could not find the aformat filter.');
    Result := AVERROR_FILTER_NOT_FOUND;
    Exit;
  end;

  aformat_ctx := avfilter_graph_alloc_filter(filter_graph, aformat, 'aformat');
  if not Assigned(aformat_ctx) then
  begin
    Writeln(ErrOutput, 'Could not allocate the aformat instance.');
    Result := AVERROR_ENOMEM;
    Exit;
  end;

  (* A third way of passing the options is in a string of the form
   * key1=value1:key2=value2.... *)
  I64 := AV_CH_LAYOUT_STEREO;
{$IF Defined(VER140) Or Defined(VER150)}
  // Int64Rec on non-local variables will cause Internal error(URW699) in Delphi 6
  Lo := Int64Rec(I64).Lo;
  Hi := Int64Rec(I64).Hi;
{$IFEND}
  snprintf(@options_str[0], SizeOf(options_str),
{$IFDEF MSWINDOWS}
           // '%lld/%llx' works on Vista or above, '%I64d/%I64x' works always
           'sample_fmts=%s:sample_rates=%d:channel_layouts=0x%I64x',
{$ELSE}
           'sample_fmts=%s:sample_rates=%d:channel_layouts=0x%llx',
{$ENDIF}
           av_get_sample_fmt_name(AV_SAMPLE_FMT_S16), 44100,
{$IF Defined(VER140) Or Defined(VER150)}
           // http://qc.embarcadero.com/wc/qcmain.aspx?d=6338
           // Int64 and Single are incorrectly passed to cdecl/varargs functions
           Lo, Hi);
{$ELSE}
           I64);
{$IFEND}
  err := avfilter_init_str(aformat_ctx, @options_str[0]);
  if err < 0 then
  begin
    Writeln(ErrOutput, 'Could not initialize the aformat filter.');
    Result := err;
    Exit;
  end;

  (* Finally create the abuffersink filter;
   * it will be used to get the filtered data out of the graph. *)
  abuffersink := avfilter_get_by_name('abuffersink');
  if not Assigned(abuffersink) then
  begin
    Writeln(ErrOutput, 'Could not find the abuffersink filter.');
    Result := AVERROR_FILTER_NOT_FOUND;
    Exit;
  end;

  abuffersink_ctx := avfilter_graph_alloc_filter(filter_graph, abuffersink, 'sink');
  if not Assigned(abuffersink_ctx) then
  begin
    Writeln(ErrOutput, 'Could not allocate the abuffersink instance.');
    Result := AVERROR_ENOMEM;
    Exit;
  end;

  (* This filter takes no options. *)
  err := avfilter_init_str(abuffersink_ctx, nil);
  if err < 0 then
  begin
    Writeln(ErrOutput, 'Could not initialize the abuffersink instance.');
    Result := err;
    Exit;
  end;

  (* Connect the filters;
   * in this simple case the filters just form a linear chain. *)
  err := avfilter_link(abuffer_ctx, 0, volume_ctx, 0);
  if err >= 0 then
    err := avfilter_link(volume_ctx, 0, aformat_ctx, 0);
  if err >= 0 then
    err := avfilter_link(aformat_ctx, 0, abuffersink_ctx, 0);
  if err < 0 then
  begin
    Writeln(ErrOutput, 'Error connecting filters');
    Result := err;
    Exit;
  end;

  (* Configure the graph. *)
  err := avfilter_graph_config(filter_graph, nil);
  if err < 0 then
  begin
    Writeln(ErrOutput, 'Error configuring the filter graph');
    Result := err;
    Exit;
  end;

  graph^ := filter_graph;
  src^   := abuffer_ctx;
  sink^  := abuffersink_ctx;

  Result := 0;
end;

(* Do something useful with the filtered data: this simple
 * example just prints the MD5 checksum of each plane to stdout. *)
function process_output(md5: PAVMD5; frame: PAVFrame): Integer;
var
  planar: Integer;
  channels: Integer;
  planes: Integer;
  bps: Integer;
  plane_size: Integer;
  i, j: Integer;
  checksum: array[0..15] of Byte;
  s: string;
begin
  planar     := av_sample_fmt_is_planar(TAVSampleFormat(frame.format));
  channels   := av_get_channel_layout_nb_channels(frame.channel_layout);
  if planar <> 0 then
    planes   := channels
  else
    planes   := 1;
  bps        := av_get_bytes_per_sample(TAVSampleFormat(frame.format));
  if planar <> 0 then
    plane_size := bps * frame.nb_samples * 1
  else
    plane_size := bps * frame.nb_samples * channels;

  for i := 0 to planes - 1 do
  begin
    av_md5_init(md5);
    av_md5_sum(@checksum[0], PPtrIdx(frame.extended_data, i), plane_size);

    s := Format('plane %d: 0x', [i]);
    for j := 0 to SizeOf(checksum) - 1 do
      s := s + Format('%.2X', [checksum[j]]);
    Writeln(s);
  end;

  Result := 0;
end;

const
  FRAME_SIZE = 1024;

(* Construct a frame of audio data to be filtered;
 * this simple example just synthesizes a sine wave. *)
function get_input(frame: PAVFrame; frame_num: Integer): Integer;
var
  err, i, j: Integer;
  data: PSingle;
begin
  (* Set up the frame properties and allocate the buffer for the data. *)
  frame.sample_rate    := INPUT_SAMPLERATE;
  frame.format         := Ord(INPUT_FORMAT);
  frame.channel_layout := INPUT_CHANNEL_LAYOUT;
  frame.nb_samples     := FRAME_SIZE;
  frame.pts            := frame_num * FRAME_SIZE;

  err := av_frame_get_buffer(frame, 0);
  if err < 0 then
  begin
    Result := err;
    Exit;
  end;

  (* Fill the data for each channel. *)
  for i := 0 to 4 do
  begin
    data := PSingle(PPtrIdx(frame.extended_data, i));

    for j := 0 to frame.nb_samples - 1 do
      PtrIdx(data, j)^ := Sin(2 * M_PI * (frame_num + j) * (i + 1) / FRAME_SIZE);
  end;

  Result := 0;
end;

function main(): Integer;
var
  md5: PAVMD5;
  graph: PAVFilterGraph;
  src, sink: PAVFilterContext;
  frame: PAVFrame;
  errstr: array[0..1024-1] of AnsiChar;
  duration: Single;
  err, nb_frames, i: Integer;
label
  fail;
begin
  if ParamCount < 1 then
  begin
    Writeln(ErrOutput, Format('Usage: %s <duration>', [ExtractFileName(ParamStr(0))]));
    Result := 1;
    Exit;
  end;

  duration  := StrToFloat(ParamStr(1));
  nb_frames := Round(duration * INPUT_SAMPLERATE / FRAME_SIZE);
  if nb_frames <= 0 then
  begin
    Writeln(ErrOutput, Format('Invalid duration: %s', [ParamStr(1)]));
    Result := 1;
    Exit;
  end;

  (* Allocate the frame we will be using to store the data. *)
  frame := av_frame_alloc();
  if not Assigned(frame) then
  begin
    Writeln(ErrOutput, 'Error allocating the frame');
    Result := 1;
    Exit;
  end;

  md5 := av_md5_alloc();
  if not Assigned(md5) then
  begin
    Writeln(ErrOutput, 'Error allocating the MD5 context');
    Result := 1;
    Exit;
  end;

  (* Set up the filtergraph. *)
  err := init_filter_graph(@graph, @src, @sink);
  if err < 0 then
  begin
    Writeln(ErrOutput, 'Unable to init filter graph:');
    goto fail;
  end;

  (* the main filtering loop *)
  for i := 0 to nb_frames - 1 do
  begin
    (* get an input frame to be filtered *)
    err := get_input(frame, i);
    if err < 0 then
    begin
      Writeln(ErrOutput, 'Error generating input frame:');
      goto fail;
    end;

    (* Send the frame to the input of the filtergraph. *)
    err := av_buffersrc_add_frame(src, frame);
    if err < 0 then
    begin
      av_frame_unref(frame);
      Writeln(ErrOutput, 'Error submitting the frame to the filtergraph:');
      goto fail;
    end;

    (* Get all the filtered output that is available. *)
    err := av_buffersink_get_frame(sink, frame);
    while err >= 0 do
    begin
      (* now do something with our filtered frame *)
      err := process_output(md5, frame);
      if err < 0 then
      begin
        Writeln(ErrOutput, 'Error processing the filtered frame:');
        goto fail;
      end;
      av_frame_unref(frame);
      err := av_buffersink_get_frame(sink, frame);
    end;

    if err = AVERROR_EAGAIN then
      (* Need to feed more frames in. *)
      Continue
    else if err = AVERROR_EOF then
      (* Nothing more to do, finish. *)
      Break
    else if err < 0 then
    begin
      (* An error occurred. *)
      Writeln(ErrOutput, 'Error filtering the data:');
      goto fail;
    end;
  end;

  avfilter_graph_free(@graph);
  av_frame_free(@frame);
  av_freep(@md5);

  Result := 0;
  Exit;

fail:
  av_strerror(err, @errstr[0], SizeOf(errstr));
  Writeln(ErrOutput, string(errstr));
  Result := 1;
end;

begin
  try
    ExitCode := main();
  except
    on E: Exception do
      Writeln(ErrOutput, E.ClassName, ': ', E.Message);
  end;
end.
