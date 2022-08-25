(*
 * Copyright (c) 2010 Nicolas George
 * Copyright (c) 2011 Stefano Sabatini
 * Copyright (c) 2012 Cl¨¦ment B?sch
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
 * API example for audio decoding and filtering
 * @example filtering_audio.c
 *)

(*
 * FFVCL - Delphi FFmpeg VCL Components
 * http://www.DelphiFFmpeg.com
 *
 * Original file: doc/examples/filtering_audio.c
 * Ported by CodeCoolie@CNSW 2014/08/28 -> $Date:: 2019-08-24 #$
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

program filtering_audio;

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
  filter_descr = 'aresample=8000,aformat=sample_fmts=s16:channel_layouts=mono';
  player       = 'ffplay -f s16le -ar 8000 -ac 1 -';

var
  fmt_ctx: PAVFormatContext;
  dec_ctx: PAVCodecContext;
  buffersink_ctx: PAVFilterContext;
  buffersrc_ctx: PAVFilterContext;
  filter_graph: PAVFilterGraph;
  audio_stream_index: Integer = -1;

function open_input_file(const filename: string): Integer;
var
  ret: Integer;
  avdec: PAVCodec;
begin
  ret := avformat_open_input(@fmt_ctx, PAnsiChar(AnsiString(filename)), nil, nil);
  if ret < 0 then
  begin
    av_log(nil, AV_LOG_ERROR, 'Cannot open input file'#10);
    Result := ret;
    Exit;
  end;

  ret := avformat_find_stream_info(fmt_ctx, nil);
  if ret < 0 then
  begin
    av_log(nil, AV_LOG_ERROR, 'Cannot find stream information'#10);
    Result := ret;
    Exit;
  end;

  (* select the audio stream *)
  ret := av_find_best_stream(fmt_ctx, AVMEDIA_TYPE_AUDIO, -1, -1, @avdec, 0);
  if ret < 0 then
  begin
    av_log(nil, AV_LOG_ERROR, 'Cannot find an audio stream in the input file'#10);
    Result := ret;
    Exit;
  end;
  audio_stream_index := ret;

  (* create decoding context *)
  dec_ctx := avcodec_alloc_context3(avdec);
  if not Assigned(dec_ctx) then
  begin
    Result := AVERROR_ENOMEM;
    Exit;
  end;
  avcodec_parameters_to_context(dec_ctx, PPtrIdx(fmt_ctx.streams, audio_stream_index).codecpar);

  (* init the audio decoder *)
  ret := avcodec_open2(dec_ctx, avdec, nil);
  if ret < 0 then
  begin
    av_log(nil, AV_LOG_ERROR, 'Cannot open audio decoder'#10);
    Result := ret;
    Exit;
  end;

  Result := 0;
end;

function init_filters(const filters_descr: PAnsiChar): Integer;
const
  out_sample_fmts: array[0..1] of TAVSampleFormat = ( AV_SAMPLE_FMT_S16, AV_SAMPLE_FMT_NONE{-1} );
  out_channel_layouts: array[0..1] of Int64 = ( AV_CH_LAYOUT_MONO, -1 );
  out_sample_rates: array[0..1] of Integer = ( 8000, -1 );
var
  args: array[0..512-1] of AnsiChar;
  ret: Integer;
  abuffersrc: PAVFilter;
  abuffersink: PAVFilter;
  outputs: PAVFilterInOut;
  inputs: PAVFilterInOut;
  outlink: PAVFilterLink;
  time_base: TAVRational;
{$IF Defined(VER140) Or Defined(VER150)}
  I64: Int64;
  Lo, Hi: Integer;
{$IFEND}
label
  the_end;
begin
  abuffersrc  := avfilter_get_by_name('abuffer');
  abuffersink := avfilter_get_by_name('abuffersink');
  outputs := avfilter_inout_alloc();
  inputs  := avfilter_inout_alloc();
  time_base := PPtrIdx(fmt_ctx.streams, audio_stream_index).time_base;

  filter_graph := avfilter_graph_alloc();
  if not Assigned(outputs) or not Assigned(inputs) or not Assigned(filter_graph) then
  begin
    ret := AVERROR_ENOMEM;
    goto the_end;
  end;

  (* buffer audio source: the decoded frames from the decoder will be inserted here. *)
  if dec_ctx.channel_layout = 0 then
    dec_ctx.channel_layout := av_get_default_channel_layout(dec_ctx.channels);
{$IF Defined(VER140) Or Defined(VER150)}
  I64 := dec_ctx.channel_layout;
  // Int64Rec on non-local variables will cause Internal error(URW699) in Delphi 6
  Lo := Int64Rec(I64).Lo;
  Hi := Int64Rec(I64).Hi;
{$IFEND}
  snprintf(@args[0], SizeOf(args),
{$IFDEF MSWINDOWS}
          // '%lld/%llx' works on Vista or above, '%I64d/%I64x' works always
          'time_base=%d/%d:sample_rate=%d:sample_fmt=%s:channel_layout=0x%I64x',
{$ELSE}
          'time_base=%d/%d:sample_rate=%d:sample_fmt=%s:channel_layout=0x%llx',
{$ENDIF}
           time_base.num, time_base.den, dec_ctx.sample_rate,
           av_get_sample_fmt_name(dec_ctx.sample_fmt),
{$IF Defined(VER140) Or Defined(VER150)}
           // http://qc.embarcadero.com/wc/qcmain.aspx?d=6338
           // Int64 and Single are incorrectly passed to cdecl/varargs functions
           Lo, Hi);
{$ELSE}
           dec_ctx.channel_layout);
{$IFEND}
  ret := avfilter_graph_create_filter(@buffersrc_ctx, abuffersrc, 'in',
                                      @args[0], nil, filter_graph);
  if ret < 0 then
  begin
    av_log(nil, AV_LOG_ERROR, 'Cannot create audio buffer source'#10);
    goto the_end;
  end;

  (* buffer audio sink: to terminate the filter chain. *)
  ret := avfilter_graph_create_filter(@buffersink_ctx, abuffersink, 'out',
                                     nil, nil, filter_graph);
  if ret < 0 then
  begin
    av_log(nil, AV_LOG_ERROR, 'Cannot create audio buffer sink'#10);
    goto the_end;
  end;

  ret := av_opt_set_int_list(buffersink_ctx, 'sample_fmts', @out_sample_fmts[0], SizeOf(TAVSampleFormat), -1,
                            AV_OPT_SEARCH_CHILDREN);
  if ret < 0 then
  begin
    av_log(nil, AV_LOG_ERROR, 'Cannot set output sample format'#10);
    goto the_end;
  end;

  ret := av_opt_set_int_list(buffersink_ctx, 'channel_layouts', @out_channel_layouts[0], SizeOf(Int64), -1,
                            AV_OPT_SEARCH_CHILDREN);
  if ret < 0 then
  begin
    av_log(nil, AV_LOG_ERROR, 'Cannot set output channel layout'#10);
    goto the_end;
  end;

  ret := av_opt_set_int_list(buffersink_ctx, 'sample_rates', @out_sample_rates, SizeOf(Integer), -1,
                            AV_OPT_SEARCH_CHILDREN);
  if ret < 0 then
  begin
    av_log(nil, AV_LOG_ERROR, 'Cannot set output sample rate'#10);
    goto the_end;
  end;

  (*
   * Set the endpoints for the filter graph. The filter_graph will
   * be linked to the graph described by filters_descr.
   *)

  (*
   * The buffer source output must be connected to the input pad of
   * the first filter described by filters_descr; since the first
   * filter input label is not specified, it is set to "in" by
   * default.
   *)
  outputs.name       := av_strdup('in');
  outputs.filter_ctx := buffersrc_ctx;
  outputs.pad_idx    := 0;
  outputs.next       := nil;

  (*
   * The buffer sink input must be connected to the output pad of
   * the last filter described by filters_descr; since the last
   * filter output label is not specified, it is set to "out" by
   * default.
   *)
  inputs.name       := av_strdup('out');
  inputs.filter_ctx := buffersink_ctx;
  inputs.pad_idx    := 0;
  inputs.next       := nil;

  ret := avfilter_graph_parse_ptr(filter_graph, filters_descr,
                                      @inputs, @outputs, nil);
  if ret < 0 then
    goto the_end;

  ret := avfilter_graph_config(filter_graph, nil);
  if ret < 0 then
    goto the_end;

  (* Print summary of the sink buffer
   * Note: args buffer is reused to store channel layout string *)
  outlink := buffersink_ctx.inputs^;
  av_get_channel_layout_string(@args[0], SizeOf(args), -1, outlink.channel_layout);
  av_log(nil, AV_LOG_INFO, 'Output: srate:%dHz fmt:%s chlayout:%s'#10,
         outlink.sample_rate,
         av_x_if_null(av_get_sample_fmt_name(TAVSampleFormat(outlink.format)), '?'),
         @args[0]);

the_end:
  avfilter_inout_free(@inputs);
  avfilter_inout_free(@outputs);

  Result := ret;
end;

procedure print_frame(const frame: PAVFrame);
var
  n: Integer;
  p: PSmallInt;
  p_end: PSmallInt;
begin
  n := frame.nb_samples * av_get_channel_layout_nb_channels(frame.channel_layout);
  p := PSmallInt(frame.data[0]);
  p_end := p;
  Inc(p_end, n);

  while Integer(p) < Integer(p_end) do
  begin
    Write(AnsiChar(p^ and $ff));
    Write(AnsiChar((p^ shr 8) and $ff));
    Inc(p);
  end;
end;

function main(): Integer;
var
  ret: Integer;
  packet: TAVPacket;
  frame: PAVFrame;
  filt_frame: PAVFrame;
label
  the_end;
begin
  frame := av_frame_alloc();
  filt_frame := av_frame_alloc();
  if not Assigned(frame) or not Assigned(filt_frame) then
  begin
    Writeln(ErrOutput, 'Could not allocate frame');
    Result := 1;
    Exit;
  end;
  if ParamCount <> 1 then
  begin
    Writeln(ErrOutput, Format('Usage: %s file | %s', [ExtractFileName(ParamStr(0)), player]));
    Result := 1;
    Exit;
  end;

  ret := open_input_file(ParamStr(1));
  if ret < 0 then
    goto the_end;
  ret := init_filters(filter_descr);
  if ret < 0 then
    goto the_end;

  (* read all packets *)
  while True do
  begin
    ret := av_read_frame(fmt_ctx, @packet);
    if ret < 0 then
      Break;

    if packet.stream_index = audio_stream_index then
    begin
      ret := avcodec_send_packet(dec_ctx, @packet);
      if ret < 0 then
      begin
        av_log(nil, AV_LOG_ERROR, 'Error while sending a packet to the decoder'#10);
        Break;
      end;

      while ret >= 0 do
      begin
        ret := avcodec_receive_frame(dec_ctx, frame);
        if (ret = AVERROR_EAGAIN) or (ret = AVERROR_EOF) then
          Break
        else if ret < 0 then
        begin
          av_log(nil, AV_LOG_ERROR, 'Error while receiving a frame from the decoder'#10);
          goto the_end;
        end;

        if ret >= 0 then
        begin
          (* push the audio data from decoded frame into the filtergraph *)
          if av_buffersrc_add_frame_flags(buffersrc_ctx, frame, AV_BUFFERSRC_FLAG_KEEP_REF) < 0 then
          begin
            av_log(nil, AV_LOG_ERROR, 'Error while feeding the audio filtergraph'#10);
            Break;
          end;

          (* pull filtered audio from the filtergraph *)
          while True do
          begin
            ret := av_buffersink_get_frame(buffersink_ctx, filt_frame);
            if (ret = AVERROR_EAGAIN) or (ret = AVERROR_EOF) then
              Break;
            if ret < 0 then
              goto the_end;
            print_frame(filt_frame);
            av_frame_unref(filt_frame);
          end;
          av_frame_unref(frame);
        end;
      end;
    end;
    av_packet_unref(@packet);
  end;
the_end:
  avfilter_graph_free(@filter_graph);
  avcodec_free_context(@dec_ctx);
  avformat_close_input(@fmt_ctx);
  av_frame_free(@frame);
  av_frame_free(@filt_frame);

  if (ret < 0) and (ret <> AVERROR_EOF) then
  begin
    Writeln(ErrOutput, Format('Error occurred: %s', [string(av_err2str(ret))]));
    Result := 1;
    Exit;
  end;

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
