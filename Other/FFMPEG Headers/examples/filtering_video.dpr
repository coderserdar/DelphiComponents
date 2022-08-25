(*
 * Copyright (c) 2010 Nicolas George
 * Copyright (c) 2011 Stefano Sabatini
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
 * API example for decoding and filtering
 * @example filtering_video.c
 *)

(*
 * FFVCL - Delphi FFmpeg VCL Components
 * http://www.DelphiFFmpeg.com
 *
 * Original file: doc/examples/filtering_video.c
 * Ported by CodeCoolie@CNSW 2014/08/29 -> $Date:: 2019-08-24 #$
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

program filtering_video;

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
  filter_descr: PAnsiChar = 'scale=78:24,transpose=cclock';
(* other way:
   scale=78:24 [scl]; [scl] transpose=cclock // assumes "[in]" and "[out]" to be input output pads respectively
 *)

var
  fmt_ctx: PAVFormatContext;
  dec_ctx: PAVCodecContext;
  buffersink_ctx: PAVFilterContext;
  buffersrc_ctx: PAVFilterContext;
  filter_graph: PAVFilterGraph;
  video_stream_index: Integer = -1;
  last_pts: Int64; // = AV_NOPTS_VALUE;

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

  (* select the video stream *)
  ret := av_find_best_stream(fmt_ctx, AVMEDIA_TYPE_VIDEO, -1, -1, @avdec, 0);
  if ret < 0 then
  begin
    av_log(nil, AV_LOG_ERROR, 'Cannot find a video stream in the input file'#10);
    Result := ret;
    Exit;
  end;
  video_stream_index := ret;

  (* create decoding context *)
  dec_ctx := avcodec_alloc_context3(avdec);
  if not Assigned(dec_ctx) then
  begin
    Result := AVERROR_ENOMEM;
    Exit;
  end;
  avcodec_parameters_to_context(dec_ctx, PPtrIdx(fmt_ctx.streams, video_stream_index).codecpar);

  (* init the video decoder *)
  ret := avcodec_open2(dec_ctx, avdec, nil);
  if ret < 0 then
  begin
    av_log(nil, AV_LOG_ERROR, 'Cannot open video decoder'#10);
    Result := ret;
    Exit;
  end;

  Result := 0;
end;

function init_filters(const filters_descr: PAnsiChar): Integer;
const
  pix_fmts: array[0..1] of TAVPixelFormat = ( AV_PIX_FMT_GRAY8, AV_PIX_FMT_NONE );
var
  args: array[0..512-1] of AnsiChar;
  ret: Integer;
  buffersrc: PAVFilter;
  buffersink: PAVFilter;
  outputs: PAVFilterInOut;
  inputs: PAVFilterInOut;
  time_base: TAVRational;
label
  the_end;
begin
  buffersrc  := avfilter_get_by_name('buffer');
  buffersink := avfilter_get_by_name('buffersink');
  outputs := avfilter_inout_alloc();
  inputs  := avfilter_inout_alloc();
  time_base := PPtrIdx(fmt_ctx.streams, video_stream_index).time_base;

  filter_graph := avfilter_graph_alloc();
  if not Assigned(outputs) or not Assigned(inputs) or not Assigned(filter_graph) then
  begin
    ret := AVERROR_ENOMEM;
    goto the_end;
  end;

  (* buffer video source: the decoded frames from the decoder will be inserted here. *)
  snprintf(@args[0], SizeOf(args),
          'video_size=%dx%d:pix_fmt=%d:time_base=%d/%d:pixel_aspect=%d/%d',
          dec_ctx.width, dec_ctx.height, dec_ctx.pix_fmt,
          time_base.num, time_base.den,
          dec_ctx.sample_aspect_ratio.num, dec_ctx.sample_aspect_ratio.den);

  ret := avfilter_graph_create_filter(@buffersrc_ctx, buffersrc, 'in',
                                      @args[0], nil, filter_graph);
  if ret < 0 then
  begin
    av_log(nil, AV_LOG_ERROR, 'Cannot create buffer source'#10);
    goto the_end;
  end;

  (* buffer video sink: to terminate the filter chain. *)
  ret := avfilter_graph_create_filter(@buffersink_ctx, buffersink, 'out',
                                     nil, nil, filter_graph);
  if ret < 0 then
  begin
    av_log(nil, AV_LOG_ERROR, 'Cannot create buffer sink'#10);
    goto the_end;
  end;

  ret := av_opt_set_int_list(buffersink_ctx, 'pix_fmts', @pix_fmts[0], SizeOf(TAVPixelFormat),
                            Int64(AV_PIX_FMT_NONE), AV_OPT_SEARCH_CHILDREN);
  if ret < 0 then
  begin
    av_log(nil, AV_LOG_ERROR, 'Cannot set output pixel format'#10);
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

the_end:
  avfilter_inout_free(@inputs);
  avfilter_inout_free(@outputs);

  Result := ret;
end;

procedure init_screen();
var
  con: THandle;
  R: TSmallRect;
  x, y: Integer;
begin
  con := GetStdHandle(STD_ERROR_HANDLE);
  R.Left := 0;
  R.Top := 0;
  R.Right := 80 - 1;
  R.Bottom := 25 - 1;
  SetConsoleWindowInfo(con, True, R);
  for y := 0 to 24 do
  begin
    for x := 0 to 79 do
      Write(' ');
    Write(#10);
  end;
  SetConsoleWindowInfo(con, True, R);
end;

procedure reset_position();
var
  con: THandle;
  coo: TCoord;
begin
  con := GetStdHandle(STD_ERROR_HANDLE);
  coo.X := 0;
  coo.Y := 0;
  SetConsoleCursorPosition(con, coo);
end;

procedure display_frame(const frame: PAVFrame; time_base: TAVRational);
var
  x, y: Integer;
  p0, p: PByte;
  delay: Int64;
  s: AnsiString;
begin
  if frame.pts <> AV_NOPTS_VALUE then
  begin
    if last_pts <> AV_NOPTS_VALUE then
    begin
      (* sleep roughly the right amount of time;
       * usleep is in microseconds, just like AV_TIME_BASE. *)
      delay := av_rescale_q(frame.pts - last_pts,
                           time_base, AV_TIME_BASE_Q);
      if (delay > 0) and (delay < 1000000) then
        Sleep(delay div 1000); //usleep(delay);
    end;
    last_pts := frame.pts;
  end;

  (* Trivial ASCII grayscale display. *)
  p0 := frame.data[0];
  //Writeln(#27'c');
  reset_position;
  s := ' .-+#';
  for y := 0 to frame.height - 1 do
  begin
    p := p0;
    for x := 0 to frame.width - 1 do
    begin
      Write(s[1 + p^ div 52]);
      Inc(p);
    end;
    Write(#10);
    Inc(p0, frame.linesize[0]);
  end;
  Writeln(IntToStr(frame.pts));
  //fflush(stdout);
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
  if ParamCount <> 1 then
  begin
    Writeln(ErrOutput, Format('Usage: %s file', [ExtractFileName(ParamStr(0))]));
    Result := 1;
    Exit;
  end;

  frame := av_frame_alloc();
  filt_frame := av_frame_alloc();

  if not Assigned(frame) or not Assigned(filt_frame) then
  begin
    Writeln(ErrOutput, 'Could not allocate frame');
    Result := 1;
    Exit;
  end;

  last_pts := AV_NOPTS_VALUE;

  ret := open_input_file(ParamStr(1));
  if ret < 0 then
    goto the_end;
  ret := init_filters(filter_descr);
  if ret < 0 then
    goto the_end;

  init_screen;

  (* read all packets *)
  while True do
  begin
    ret := av_read_frame(fmt_ctx, @packet);
    if ret < 0 then
      Break;

    if packet.stream_index = video_stream_index then
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

        frame.pts := frame.best_effort_timestamp;

        (* push the decoded frame into the filtergraph *)
        if av_buffersrc_add_frame_flags(buffersrc_ctx, frame, AV_BUFFERSRC_FLAG_KEEP_REF) < 0 then
        begin
          av_log(nil, AV_LOG_ERROR, 'Error while feeding the filtergraph'#10);
          Break;
        end;

        (* pull filtered frames from the filtergraph *)
        while True do
        begin
          ret := av_buffersink_get_frame(buffersink_ctx, filt_frame);
          if (ret = AVERROR_EAGAIN) or (ret = AVERROR_EOF) then
            Break;
          if ret < 0 then
            goto the_end;
          display_frame(filt_frame, buffersink_ctx.inputs^.time_base);
          av_frame_unref(filt_frame);
        end;
        av_frame_unref(frame);
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
    Writeln(ErrOutput, Format('Error occurred: %s', [av_err2str(ret)]));
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
