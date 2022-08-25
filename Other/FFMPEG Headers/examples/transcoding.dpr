(*
 * Copyright (c) 2010 Nicolas George
 * Copyright (c) 2011 Stefano Sabatini
 * Copyright (c) 2014 Andrey Utkin
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
 * API example for demuxing, decoding, filtering, encoding and muxing
 * @example transcoding.c
 *)

(*
 * FFVCL - Delphi FFmpeg VCL Components
 * http://www.DelphiFFmpeg.com
 *
 * Original file: doc/examples/transcoding.c
 * Ported by CodeCoolie@CNSW 2014/09/22 -> $Date:: 2022-02-20 #$
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

program transcoding;

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

var
  ifmt_ctx: PAVFormatContext;
  ofmt_ctx: PAVFormatContext;

type
  PFilteringContext = ^TFilteringContext;
  TFilteringContext = record
    buffersink_ctx: PAVFilterContext;
    buffersrc_ctx: PAVFilterContext;
    filter_graph: PAVFilterGraph;

    enc_pkt: PAVPacket;
    filtered_frame: PAVFrame;
  end;

var
  filter_ctx: PFilteringContext;

type
  PStreamContext = ^TStreamContext;
  TStreamContext = record
    dec_ctx: PAVCodecContext;
    enc_ctx: PAVCodecContext;

    dec_frame: PAVFrame;
  end;

var
  stream_ctx: PStreamContext;

function PtrIdx(P: PStreamContext; I: Integer): PStreamContext; overload;
begin
  Inc(P, I);
  Result := P;
end;

function open_input_file(const filename: string): Integer;
var
  ret: Integer;
  i: Cardinal;
  stream: PAVStream;
  avdec: PAVCodec;
  codec_ctx: PAVCodecContext;
begin
  ifmt_ctx := nil;
  ret := avformat_open_input(@ifmt_ctx, PAnsiChar(AnsiString(filename)), nil, nil);
  if ret < 0 then
  begin
    av_log(nil, AV_LOG_ERROR, 'Cannot open input file'#10);
    Result := ret;
    Exit;
  end;

  ret := avformat_find_stream_info(ifmt_ctx, nil);
  if ret < 0 then
  begin
    av_log(nil, AV_LOG_ERROR, 'Cannot find stream information'#10);
    Result := ret;
    Exit;
  end;

  stream_ctx := av_mallocz_array(ifmt_ctx.nb_streams, SizeOf(stream_ctx^));
  if not Assigned(stream_ctx) then
  begin
    Result := AVERROR_ENOMEM;
    Exit;
  end;

  for i := 0 to ifmt_ctx.nb_streams - 1 do
  begin
    stream := PPtrIdx(ifmt_ctx.streams, i);
    avdec := avcodec_find_decoder(stream.codecpar.codec_id);
    if not Assigned(avdec) then
    begin
      av_log(nil, AV_LOG_ERROR, 'Failed to find decoder for stream #%u'#10, i);
      Result := AVERROR_DECODER_NOT_FOUND;
      Exit;
    end;
    codec_ctx := avcodec_alloc_context3(avdec);
    if not Assigned(codec_ctx) then
    begin
      av_log(nil, AV_LOG_ERROR, 'Failed to allocate the decoder context for stream #%u'#10, i);
      Result := AVERROR_ENOMEM;
      Exit;
    end;
    ret := avcodec_parameters_to_context(codec_ctx, stream.codecpar);
    if ret < 0 then
    begin
      av_log(nil, AV_LOG_ERROR, 'Failed to copy decoder parameters to input decoder context for stream #%u'#10, i);
      Result := ret;
      Exit;
    end;
    (* Reencode video & audio and remux subtitles etc. *)
    if (codec_ctx.codec_type = AVMEDIA_TYPE_VIDEO) or
       (codec_ctx.codec_type = AVMEDIA_TYPE_AUDIO) then
    begin
      if codec_ctx.codec_type = AVMEDIA_TYPE_VIDEO then
        codec_ctx.framerate := av_guess_frame_rate(ifmt_ctx, stream, nil);
      (* Open decoder *)
      ret := avcodec_open2(codec_ctx, avdec, nil);
      if ret < 0 then
      begin
        av_log(nil, AV_LOG_ERROR, 'Failed to open decoder for stream #%u'#10, i);
        Result := ret;
        Exit;
      end;
    end;
    PtrIdx(stream_ctx, i).dec_ctx := codec_ctx;

    PtrIdx(stream_ctx, i).dec_frame := av_frame_alloc();
    if not Assigned(PtrIdx(stream_ctx, i).dec_frame) then
    begin
      Result := AVERROR_ENOMEM;
      Exit;
    end;
  end;

  av_dump_format(ifmt_ctx, 0, PAnsiChar(AnsiString(filename)), 0);
  Result := 0;
end;

function open_output_file(const filename: string): Integer;
var
  out_stream: PAVStream;
  in_stream: PAVStream;
  dec_ctx, enc_ctx: PAVCodecContext;
  encoder: PAVCodec;
  ret: Integer;
  i: Cardinal;
begin
  ofmt_ctx := nil;
  avformat_alloc_output_context2(@ofmt_ctx, nil, nil, PAnsiChar(AnsiString(filename)));
  if not Assigned(ofmt_ctx) then
  begin
    av_log(nil, AV_LOG_ERROR, 'Could not create output context'#10);
    Result := AVERROR_UNKNOWN;
    Exit;
  end;


  for i := 0 to ifmt_ctx.nb_streams - 1 do
  begin
    out_stream := avformat_new_stream(ofmt_ctx, nil);
    if not Assigned(out_stream) then
    begin
      av_log(nil, AV_LOG_ERROR, 'Failed allocating output stream'#10);
      Result := AVERROR_UNKNOWN;
      Exit;
    end;

    in_stream := PPtrIdx(ifmt_ctx.streams, i);
    dec_ctx := PtrIdx(stream_ctx, i).dec_ctx;

    if (dec_ctx.codec_type = AVMEDIA_TYPE_VIDEO) or
       (dec_ctx.codec_type = AVMEDIA_TYPE_AUDIO) then
    begin
      (* in this example, we choose transcoding to same codec *)
      encoder := avcodec_find_encoder(dec_ctx.codec_id);
      if not Assigned(encoder) then
      begin
        av_log(nil, AV_LOG_FATAL, 'Necessary encoder not found'#10);
        Result := AVERROR_INVALIDDATA;
        Exit;
      end;
      enc_ctx := avcodec_alloc_context3(encoder);
      if not Assigned(enc_ctx) then
      begin
        av_log(nil, AV_LOG_FATAL, 'Failed to allocate the encoder context'#10);
        Result := AVERROR_ENOMEM;
        Exit;
      end;

      (* In this example, we transcode to same properties (picture size,
       * sample rate etc.). These properties can be changed for output
       * streams easily using filters *)
      if dec_ctx.codec_type = AVMEDIA_TYPE_VIDEO then
      begin
        enc_ctx.height := dec_ctx.height;
        enc_ctx.width := dec_ctx.width;
        enc_ctx.sample_aspect_ratio := dec_ctx.sample_aspect_ratio;
        (* take first format from list of supported formats *)
        if Assigned(encoder.pix_fmts) then
          enc_ctx.pix_fmt := encoder.pix_fmts^
        else
          enc_ctx.pix_fmt := dec_ctx.pix_fmt;
        (* video time_base can be set to whatever is handy and supported by encoder *)
        enc_ctx.time_base := av_inv_q(dec_ctx.framerate);
        // hack for libx264
        if dec_ctx.codec_id = AV_CODEC_ID_H264 then
        begin
          enc_ctx.me_range := 16;
          enc_ctx.max_qdiff := 4;
          enc_ctx.qmin := 10;
          enc_ctx.qmax := 51;
          //enc_ctx.qcompress := 0.6;
        end;
      end
      else
      begin
        enc_ctx.sample_rate := dec_ctx.sample_rate;
        enc_ctx.channel_layout := dec_ctx.channel_layout;
        enc_ctx.channels := av_get_channel_layout_nb_channels(enc_ctx.channel_layout);
        (* take first format from list of supported formats *)
        enc_ctx.sample_fmt := encoder.sample_fmts^;
        enc_ctx.time_base := av_make_q(1, enc_ctx.sample_rate);
      end;

      if (ofmt_ctx.oformat.flags and AVFMT_GLOBALHEADER) <> 0 then
        enc_ctx.flags := enc_ctx.flags or AV_CODEC_FLAG_GLOBAL_HEADER;

      (* Third parameter can be used to pass settings to encoder *)
      ret := avcodec_open2(enc_ctx, encoder, nil);
      if ret < 0 then
      begin
        av_log(nil, AV_LOG_ERROR, 'Cannot open video encoder for stream #%u'#10, i);
        Result := ret;
        Exit;
      end;
      ret := avcodec_parameters_from_context(out_stream.codecpar, enc_ctx);
      if ret < 0 then
      begin
        av_log(nil, AV_LOG_ERROR, 'Failed to copy encoder parameters to output stream #%u'#10, i);
        Result := ret;
        Exit;
      end;

      out_stream.time_base := enc_ctx.time_base;
      PtrIdx(stream_ctx, i).enc_ctx := enc_ctx;
    end
    else if dec_ctx.codec_type = AVMEDIA_TYPE_UNKNOWN then
    begin
      av_log(nil, AV_LOG_FATAL, 'Elementary stream #%d is of unknown type, cannot proceed'#10, i);
      Result := AVERROR_INVALIDDATA;
      Exit;
    end
    else
    begin
      (* if this stream must be remuxed *)
      ret := avcodec_parameters_copy(out_stream.codecpar, in_stream.codecpar);
      if ret < 0 then
      begin
        av_log(nil, AV_LOG_ERROR, 'Copying parameters for stream #%u failed'#10, i);
        Result := ret;
        Exit;
      end;
      out_stream.time_base := in_stream.time_base;
    end;

  end;
  av_dump_format(ofmt_ctx, 0, PAnsiChar(AnsiString(filename)), 1);

  if (ofmt_ctx.oformat.flags and AVFMT_NOFILE) = 0 then
  begin
    ret := avio_open(@ofmt_ctx.pb, PAnsiChar(AnsiString(filename)), AVIO_FLAG_WRITE);
    if ret < 0 then
    begin
      av_log(nil, AV_LOG_ERROR, 'Could not open output file ''%s'''#10, PAnsiChar(AnsiString(filename)));
      Result := ret;
      Exit;
    end;
  end;

  (* init muxer, write output file header *)
  ret := avformat_write_header(ofmt_ctx, nil);
  if ret < 0 then
  begin
    av_log(nil, AV_LOG_ERROR, 'Error occurred when opening output file'#10);
    Result := ret;
    Exit;
  end;

  Result := 0;
end;

function init_filter(fctx: PFilteringContext; dec_ctx: PAVCodecContext;
  enc_ctx: PAVCodecContext; const filter_spec: PAnsiChar): Integer;
var
  args: array[0..512-1] of AnsiChar;
  ret: Integer;
  buffersrc: PAVFilter;
  buffersink: PAVFilter;
  buffersrc_ctx: PAVFilterContext;
  buffersink_ctx: PAVFilterContext;
  outputs: PAVFilterInOut;
  inputs: PAVFilterInOut;
  filter_graph: PAVFilterGraph;
{$IF Defined(VER140) Or Defined(VER150)}
  I64: Int64;
  Lo, Hi: Integer;
{$IFEND}
label
  the_end;
begin
  buffersrc_ctx := nil;
  buffersink_ctx := nil;
  outputs := avfilter_inout_alloc();
  inputs  := avfilter_inout_alloc();
  filter_graph := avfilter_graph_alloc();

  if not Assigned(outputs) or not Assigned(inputs) or not Assigned(filter_graph) then
  begin
    ret := AVERROR_ENOMEM;
    goto the_end;
  end;

  if dec_ctx.codec_type = AVMEDIA_TYPE_VIDEO then
  begin
    buffersrc := avfilter_get_by_name('buffer');
    buffersink := avfilter_get_by_name('buffersink');
    if not Assigned(buffersrc) or not Assigned(buffersink) then
    begin
      av_log(nil, AV_LOG_ERROR, 'filtering source or sink element not found'#10);
      ret := AVERROR_UNKNOWN;
      goto the_end;
    end;

    snprintf(@args[0], SizeOf(args),
            'video_size=%dx%d:pix_fmt=%d:time_base=%d/%d:pixel_aspect=%d/%d',
            dec_ctx.width, dec_ctx.height, dec_ctx.pix_fmt,
            dec_ctx.time_base.num, dec_ctx.time_base.den,
            dec_ctx.sample_aspect_ratio.num,
            dec_ctx.sample_aspect_ratio.den);

    ret := avfilter_graph_create_filter(@buffersrc_ctx, buffersrc, 'in',
                                        @args[0], nil, filter_graph);
    if ret < 0 then
    begin
      av_log(nil, AV_LOG_ERROR, 'Cannot create buffer source'#10);
      goto the_end;
    end;

    ret := avfilter_graph_create_filter(@buffersink_ctx, buffersink, 'out',
            nil, nil, filter_graph);
    if ret < 0 then
    begin
      av_log(nil, AV_LOG_ERROR, 'Cannot create buffer sink'#10);
      goto the_end;
    end;

    ret := av_opt_set_bin(buffersink_ctx, 'pix_fmts',
            @enc_ctx.pix_fmt, SizeOf(enc_ctx.pix_fmt),
            AV_OPT_SEARCH_CHILDREN);
    if ret < 0 then
    begin
      av_log(nil, AV_LOG_ERROR, 'Cannot set output pixel format'#10);
      goto the_end;
    end;
  end
  else if dec_ctx.codec_type = AVMEDIA_TYPE_AUDIO then
  begin
    buffersrc := avfilter_get_by_name('abuffer');
    buffersink := avfilter_get_by_name('abuffersink');
    if not Assigned(buffersrc) or not Assigned(buffersink) then
    begin
      av_log(nil, AV_LOG_ERROR, 'filtering source or sink element not found'#10);
      ret := AVERROR_UNKNOWN;
      goto the_end;
    end;

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
            dec_ctx.time_base.num, dec_ctx.time_base.den, dec_ctx.sample_rate,
            av_get_sample_fmt_name(dec_ctx.sample_fmt),
{$IF Defined(VER140) Or Defined(VER150)}
            // http://qc.embarcadero.com/wc/qcmain.aspx?d=6338
            // Int64 and Single are incorrectly passed to cdecl/varargs functions
            Lo, Hi);
{$ELSE}
            dec_ctx.channel_layout);
{$IFEND}
    ret := avfilter_graph_create_filter(@buffersrc_ctx, buffersrc, 'in',
                                        @args[0], nil, filter_graph);
    if ret < 0 then
    begin
      av_log(nil, AV_LOG_ERROR, 'Cannot create audio buffer source'#10);
      goto the_end;
    end;

    ret := avfilter_graph_create_filter(@buffersink_ctx, buffersink, 'out',
            nil, nil, filter_graph);
    if ret < 0 then
    begin
      av_log(nil, AV_LOG_ERROR, 'Cannot create audio buffer sink'#10);
      goto the_end;
    end;

    ret := av_opt_set_bin(buffersink_ctx, 'sample_fmts',
            @enc_ctx.sample_fmt, SizeOf(enc_ctx.sample_fmt),
            AV_OPT_SEARCH_CHILDREN);
    if ret < 0 then
    begin
      av_log(nil, AV_LOG_ERROR, 'Cannot set output sample format'#10);
      goto the_end;
    end;

    ret := av_opt_set_bin(buffersink_ctx, 'channel_layouts',
            @enc_ctx.channel_layout,
            SizeOf(enc_ctx.channel_layout), AV_OPT_SEARCH_CHILDREN);
    if ret < 0 then
    begin
      av_log(nil, AV_LOG_ERROR, 'Cannot set output channel layout'#10);
      goto the_end;
    end;

    ret := av_opt_set_bin(buffersink_ctx, 'sample_rates',
            @enc_ctx.sample_rate, SizeOf(enc_ctx.sample_rate),
            AV_OPT_SEARCH_CHILDREN);
    if ret < 0 then
    begin
      av_log(nil, AV_LOG_ERROR, 'Cannot set output sample rate'#10);
      goto the_end;
    end;
  end
  else
  begin
    ret := AVERROR_UNKNOWN;
    goto the_end;
  end;

  (* Endpoints for the filter graph. *)
  outputs.name       := av_strdup('in');
  outputs.filter_ctx := buffersrc_ctx;
  outputs.pad_idx    := 0;
  outputs.next       := nil;

  inputs.name       := av_strdup('out');
  inputs.filter_ctx := buffersink_ctx;
  inputs.pad_idx    := 0;
  inputs.next       := nil;

  if not Assigned(outputs.name) or not Assigned(inputs.name) then
  begin
    ret := AVERROR_ENOMEM;
    goto the_end;
  end;

  ret := avfilter_graph_parse_ptr(filter_graph, filter_spec,
                  @inputs, @outputs, nil);
  if ret < 0 then
    goto the_end;

  ret := avfilter_graph_config(filter_graph, nil);
  if ret < 0 then
    goto the_end;

  (* Fill FilteringContext *)
  fctx.buffersrc_ctx := buffersrc_ctx;
  fctx.buffersink_ctx := buffersink_ctx;
  fctx.filter_graph := filter_graph;

the_end:
  avfilter_inout_free(@inputs);
  avfilter_inout_free(@outputs);

  Result := ret;
end;

function init_filters(): Integer;
var
  filter_spec: PAnsiChar;
  i: Cardinal;
  ret: Integer;
  ctx: PFilteringContext;
begin
  filter_ctx := av_malloc_array(ifmt_ctx.nb_streams, SizeOf(filter_ctx^));
  if not Assigned(filter_ctx) then
  begin
    Result := AVERROR_ENOMEM;
    EXit;
  end;

  ctx := filter_ctx;
  for i := 0 to ifmt_ctx.nb_streams - 1 do
  begin
    ctx.buffersrc_ctx  := nil;
    ctx.buffersink_ctx := nil;
    ctx.filter_graph   := nil;
    if (PPtrIdx(ifmt_ctx.streams, i).codecpar.codec_type <> AVMEDIA_TYPE_AUDIO) and
       (PPtrIdx(ifmt_ctx.streams, i).codecpar.codec_type <> AVMEDIA_TYPE_VIDEO) then
      Continue;

    if PPtrIdx(ifmt_ctx.streams, i).codecpar.codec_type = AVMEDIA_TYPE_VIDEO then
      filter_spec := 'null' (* passthrough (dummy) filter for video *)
    else
      filter_spec := 'anull'; (* passthrough (dummy) filter for audio *)
    ret := init_filter(ctx, PtrIdx(stream_ctx, i).dec_ctx,
            PtrIdx(stream_ctx, i).enc_ctx, filter_spec);
    if ret <> 0 then
    begin
      Result := ret;
      Exit;
    end;

    ctx.enc_pkt := av_packet_alloc();
    if not Assigned(ctx.enc_pkt) then
    begin
      Result := AVERROR_ENOMEM;
      Exit;
    end;

    ctx.filtered_frame := av_frame_alloc();
    if not Assigned(ctx.filtered_frame) then
    begin
      Result := AVERROR_ENOMEM;
      Exit;
    end;

    Inc(ctx);
  end;
  Result := 0;
end;

function encode_write_frame(stream_index: Cardinal; flush: Integer): Integer;
var
  stream: PStreamContext;
  filter: PFilteringContext;
  filt_frame: PAVFrame;
  enc_pkt: PAVPacket;
  ret: Integer;
begin
  stream := stream_ctx;
  Inc(stream, stream_index);
  filter := filter_ctx;
  Inc(filter, stream_index);
  if flush <> 0 then
    filt_frame := nil
  else
    filt_frame := filter.filtered_frame;
  enc_pkt := filter.enc_pkt;

  av_log(nil, AV_LOG_INFO, 'Encoding frame'#10);
  (* encode filtered frame *)
  av_packet_unref(enc_pkt);

  ret := avcodec_send_frame(stream.enc_ctx, filt_frame);

  if ret < 0 then
  begin
    Result := ret;
    Exit;
  end;

  while ret >= 0 do
  begin
    ret := avcodec_receive_packet(stream.enc_ctx, enc_pkt);
    if (ret = AVERROR_EAGAIN) or (ret = AVERROR_EOF) then
    begin
      Result := 0;
      Exit;
    end;

    (* prepare packet for muxing *)
    enc_pkt.stream_index := stream_index;
    av_packet_rescale_ts(enc_pkt,
                        stream.enc_ctx.time_base,
                        PPtrIdx(ofmt_ctx.streams, stream_index).time_base);

    av_log(nil, AV_LOG_DEBUG, 'Muxing frame'#10);
    (* mux encoded frame *)
    ret := av_interleaved_write_frame(ofmt_ctx, enc_pkt);
  end;

  Result := ret;
end;

function filter_encode_write_frame(frame: PAVFrame; stream_index: Cardinal): Integer;
var
  filter: PFilteringContext;
  ret: Integer;
begin
  filter := filter_ctx;
  Inc(filter, stream_index);
  av_log(nil, AV_LOG_INFO, 'Pushing decoded frame to filters'#10);
  (* push the decoded frame into the filtergraph *)
  ret := av_buffersrc_add_frame_flags(filter.buffersrc_ctx, frame, 0);
  if ret < 0 then
  begin
    av_log(nil, AV_LOG_ERROR, 'Error while feeding the filtergraph'#10);
    Result := ret;
    Exit;
  end;

  (* pull filtered frames from the filtergraph *)
  while True do
  begin
    av_log(nil, AV_LOG_INFO, 'Pulling filtered frame from filters'#10);
    ret := av_buffersink_get_frame(filter.buffersink_ctx, filter.filtered_frame);
    if ret < 0 then
    begin
      (* if no more frames for output - returns AVERROR(EAGAIN)
       * if flushed and no more frames for output - returns AVERROR_EOF
       * rewrite retcode to 0 to show it as normal procedure completion
       *)
      if (ret = AVERROR_EAGAIN) or (ret = AVERROR_EOF) then
        ret := 0;
      Break;
    end;

    filter.filtered_frame.pict_type := AV_PICTURE_TYPE_NONE;
    ret := encode_write_frame(stream_index, 0);
    av_frame_unref(filter.filtered_frame);
    if ret < 0 then
      Break;
  end;

  Result := ret;
end;

function flush_encoder(stream_index: Cardinal): Integer;
begin
  if (PtrIdx(stream_ctx, stream_index).enc_ctx.codec.capabilities and AV_CODEC_CAP_DELAY) = 0 then
  begin
    Result := 0;
    Exit;
  end;

  av_log(nil, AV_LOG_INFO, 'Flushing stream #%u encoder'#10, stream_index);
  Result := encode_write_frame(stream_index, 1);
end;

function main(): Integer;
var
  ret: Integer;
  packet: PAVPacket;
  stream_index: Cardinal;
  i: Cardinal;
  stream: PStreamContext;
  ctx: PFilteringContext;
label
  the_end;
begin
  packet := nil;

  if ParamCount <> 2 then
  begin
    av_log(nil, AV_LOG_ERROR, 'Usage: %s <input file> <output file>'#10, PAnsiChar(AnsiString(ExtractFileName(ParamStr(0)))));
    Result := 1;
    Exit;
  end;

  ret := open_input_file(ParamStr(1));
  if ret < 0 then
    goto the_end;
  ret := open_output_file(ParamStr(2));
  if ret < 0 then
    goto the_end;
  ret := init_filters();
  if ret < 0 then
    goto the_end;
  packet := av_packet_alloc();
  if not Assigned(packet) then
  begin
    ret := AVERROR_ENOMEM;
    goto the_end;
  end;

  (* read all packets *)
  while True do
  begin
    ret := av_read_frame(ifmt_ctx, packet);
    if ret < 0 then
      Break;
    stream_index := packet.stream_index;
    av_log(nil, AV_LOG_DEBUG, 'Demuxer gave frame of stream_index %u'#10, stream_index);

    ctx := filter_ctx;
    Inc(ctx, stream_index);
    if Assigned(ctx.filter_graph) then
    begin
      stream := stream_ctx;
      Inc(stream, stream_index);

      av_log(nil, AV_LOG_DEBUG, 'Going to reencode and filter the frame'#10);

      av_packet_rescale_ts(packet,
              PPtrIdx(ifmt_ctx.streams, stream_index).time_base,
              stream.dec_ctx.time_base);
      ret := avcodec_send_packet(stream.dec_ctx, packet);
      if ret < 0 then
      begin
        av_log(nil, AV_LOG_ERROR, 'Decoding failed'#10);
        Break;
      end;

      while ret >= 0 do
      begin
        ret := avcodec_receive_frame(stream.dec_ctx, stream.dec_frame);
        if (ret = AVERROR_EOF) or (ret = AVERROR_EAGAIN) then
          Break
        else if ret < 0 then
          goto the_end;

        stream.dec_frame.pts := stream.dec_frame.best_effort_timestamp;
        ret := filter_encode_write_frame(stream.dec_frame, stream_index);
        if ret < 0 then
          goto the_end;
      end;
    end
    else
    begin
      (* remux this frame without reencoding *)
      av_packet_rescale_ts(packet,
              PPtrIdx(ifmt_ctx.streams, stream_index).time_base,
              PPtrIdx(ofmt_ctx.streams, stream_index).time_base);

      ret := av_interleaved_write_frame(ofmt_ctx, packet);
      if ret < 0 then
        goto the_end;
    end;
    av_packet_unref(packet);
  end;

  (* flush filters and encoders *)
  for i := 0 to ifmt_ctx.nb_streams - 1 do
  begin
    ctx := filter_ctx;
    Inc(ctx, i);
    (* flush filter *)
    if not Assigned(ctx.filter_graph) then
      Continue;
      ret := filter_encode_write_frame(nil, i);
    if ret < 0 then
    begin
      av_log(nil, AV_LOG_ERROR, 'Flushing filter failed'#10);
      goto the_end;
    end;

    (* flush encoder *)
    ret := flush_encoder(i);
    if ret < 0 then
    begin
      av_log(nil, AV_LOG_ERROR, 'Flushing encoder failed'#10);
      goto the_end;
    end;
  end;

  av_write_trailer(ofmt_ctx);
the_end:
  av_packet_free(@packet);
  if Assigned(ifmt_ctx) then
    for i := 0 to ifmt_ctx.nb_streams - 1 do
    begin
      ctx := filter_ctx;
      Inc(ctx, i);
      avcodec_close(PPtrIdx(ifmt_ctx.streams, i).codec);
      avcodec_free_context(@PAVCodecContext(PtrIdx(stream_ctx, i).dec_ctx));
      if Assigned(ofmt_ctx) and (ofmt_ctx.nb_streams > i) and
        Assigned(PPtrIdx(ofmt_ctx.streams, i)) and Assigned(PtrIdx(stream_ctx, i).enc_ctx) then
        avcodec_free_context(@PAVCodecContext(PtrIdx(stream_ctx, i).enc_ctx));
      if Assigned(filter_ctx) and Assigned(ctx.filter_graph) then
      begin
        avfilter_graph_free(@ctx.filter_graph);
        av_packet_free(@ctx.enc_pkt);
        av_frame_free(@ctx.filtered_frame);
      end;
      av_frame_free(@PAVFrame(PtrIdx(stream_ctx, i).dec_frame));
    end;
  av_free(filter_ctx);
  av_free(stream_ctx);
  avformat_close_input(@ifmt_ctx);
  if Assigned(ofmt_ctx) and ((ofmt_ctx.oformat.flags and AVFMT_NOFILE) = 0) then
    avio_closep(@ofmt_ctx.pb);
  avformat_free_context(ofmt_ctx);

  if ret < 0 then
    av_log(nil, AV_LOG_ERROR, 'Error occurred: %s'#10, av_err2str(ret));

  if ret <> 0 then
    Result := 1
  else
    Result := 0;
end;

begin
  //Set8087CW($133f); // Disable FPU Exceptions
  try
    ExitCode := main();
  except
    on E: Exception do
      Writeln(ErrOutput, E.ClassName, ': ', E.Message);
  end;
end.
