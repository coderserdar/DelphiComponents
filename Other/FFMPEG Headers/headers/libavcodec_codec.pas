(*
 * AVCodec public API
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
 * Original file: libavcodec/codec.h
 * Ported by CodeCoolie@CNSW 2020/11/11 -> $Date:: 2021-04-25 #$
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

unit libavcodec_codec;

interface

{$I CompilerDefines.inc}

uses
  libavcodec_codec_id,
  libavutil,
  libavutil_hwcontext,
  libavutil_log,
  libavutil_pixfmt,
  libavutil_rational,
  libavutil_samplefmt;

{$I libversion.inc}

(**
 * @addtogroup lavc_core
 * @{
 *)

const
(**
 * Decoder can use draw_horiz_band callback.
 *)
  AV_CODEC_CAP_DRAW_HORIZ_BAND     = (1 shl  0);
(**
 * Codec uses get_buffer() or get_encode_buffer() for allocating buffers and
 * supports custom allocators.
 * If not set, it might not use get_buffer() or get_encode_buffer() at all, or
 * use operations that assume the buffer was allocated by
 * avcodec_default_get_buffer2 or avcodec_default_get_encode_buffer.
 *)
  AV_CODEC_CAP_DR1                 = (1 shl  1);
  AV_CODEC_CAP_TRUNCATED           = (1 shl  3);
(**
 * Encoder or decoder requires flushing with NULL input at the end in order to
 * give the complete and correct output.
 *
 * NOTE: If this flag is not set, the codec is guaranteed to never be fed with
 *       with NULL data. The user can still send NULL data to the public encode
 *       or decode function, but libavcodec will not pass it along to the codec
 *       unless this flag is set.
 *
 * Decoders:
 * The decoder has a non-zero delay and needs to be fed with avpkt->data=NULL,
 * avpkt->size=0 at the end to get the delayed data until the decoder no longer
 * returns frames.
 *
 * Encoders:
 * The encoder needs to be fed with NULL data at the end of encoding until the
 * encoder no longer returns data.
 *
 * NOTE: For encoders implementing the AVCodec.encode2() function, setting this
 *       flag also means that the encoder must set the pts and duration for
 *       each output packet. If this flag is not set, the pts and duration will
 *       be determined by libavcodec from the input frame.
 *)
  AV_CODEC_CAP_DELAY               = (1 shl  5);
(**
 * Codec can be fed a final frame with a smaller size.
 * This can be used to prevent truncation of the last audio samples.
 *)
  AV_CODEC_CAP_SMALL_LAST_FRAME    = (1 shl  6);

(**
 * Codec can output multiple frames per AVPacket
 * Normally demuxers return one frame at a time, demuxers which do not do
 * are connected to a parser to split what they return into proper frames.
 * This flag is reserved to the very rare category of codecs which have a
 * bitstream that cannot be split into frames without timeconsuming
 * operations like full decoding. Demuxers carrying such bitstreams thus
 * may return multiple frames in a packet. This has many disadvantages like
 * prohibiting stream copy in many cases thus it should only be considered
 * as a last resort.
 *)
  AV_CODEC_CAP_SUBFRAMES           = (1 shl  8);
(**
 * Codec is experimental and is thus avoided in favor of non experimental
 * encoders
 *)
  AV_CODEC_CAP_EXPERIMENTAL        = (1 shl  9);
(**
 * Codec should fill in channel configuration and samplerate instead of container
 *)
  AV_CODEC_CAP_CHANNEL_CONF        = (1 shl 10);
(**
 * Codec supports frame-level multithreading.
 *)
  AV_CODEC_CAP_FRAME_THREADS       = (1 shl 12);
(**
 * Codec supports slice-based (or partition-based) multithreading.
 *)
  AV_CODEC_CAP_SLICE_THREADS       = (1 shl 13);
(**
 * Codec supports changed parameters at any point.
 *)
  AV_CODEC_CAP_PARAM_CHANGE        = (1 shl 14);
(**
 * Codec supports multithreading through a method other than slice- or
 * frame-level multithreading. Typically this marks wrappers around
 * multithreading-capable external libraries.
 *)
  AV_CODEC_CAP_OTHER_THREADS       = (1 shl 15);
{$IFDEF FF_API_AUTO_THREADS}
  AV_CODEC_CAP_AUTO_THREADS        = AV_CODEC_CAP_OTHER_THREADS;
{$ENDIF}
(**
 * Audio encoder supports receiving a different number of samples in each call.
 *)
  AV_CODEC_CAP_VARIABLE_FRAME_SIZE = (1 shl 16);
(**
 * Decoder is not a preferred choice for probing.
 * This indicates that the decoder is not a good choice for probing.
 * It could for example be an expensive to spin up hardware decoder,
 * or it could simply not provide a lot of useful information about
 * the stream.
 * A decoder marked with this flag should only be used as last resort
 * choice for probing.
 *)
  AV_CODEC_CAP_AVOID_PROBING       = (1 shl 17);

{$IFDEF FF_API_UNUSED_CODEC_CAPS}
(**
 * Deprecated and unused. Use AVCodecDescriptor.props instead
 *)
  AV_CODEC_CAP_INTRA_ONLY          = $40000000;
(**
 * Deprecated and unused. Use AVCodecDescriptor.props instead
 *)
  AV_CODEC_CAP_LOSSLESS            = $80000000;
{$ENDIF}

(**
 * Codec is backed by a hardware implementation. Typically used to
 * identify a non-hwaccel hardware decoder. For information about hwaccels, use
 * avcodec_get_hw_config() instead.
 *)
  AV_CODEC_CAP_HARDWARE            = (1 shl 18);

(**
 * Codec is potentially backed by a hardware implementation, but not
 * necessarily. This is used instead of AV_CODEC_CAP_HARDWARE, if the
 * implementation provides some sort of internal fallback.
 *)
  AV_CODEC_CAP_HYBRID              = (1 shl 19);

(**
 * This codec takes the reordered_opaque field from input AVFrames
 * and returns it in the corresponding field in AVCodecContext after
 * encoding.
 *)
  AV_CODEC_CAP_ENCODER_REORDERED_OPAQUE = (1 shl 20);

(**
 * This encoder can be flushed using avcodec_flush_buffers(). If this flag is
 * not set, the encoder must be closed and reopened to ensure that no frames
 * remain pending.
 *)
  AV_CODEC_CAP_ENCODER_FLUSH       = (1 shl 21);

type
(**
 * AVProfile.
 *)
  PAVProfile = ^TAVProfile;
  TAVProfile = record
    profile: Integer;
    name: PAnsiChar; ///< short name for the profile
  end;

  PPAVCodecHWConfigInternal = ^PAVCodecHWConfigInternal;
  PAVCodecHWConfigInternal = ^TAVCodecHWConfigInternal;
  TAVCodecHWConfigInternal = record
    // need {$ALIGN 8}
    // defined in libavcodec/hwaccel.h
  end;

  PAVCodecDefault = ^TAVCodecDefault;
  TAVCodecDefault = record
    // need {$ALIGN 8}
    // defined in libavcodec/internal.h
  end;

  PAVCodecContext = ^TAVCodecContext;
  TAVCodecContext = record
    // need {$ALIGN 8}
    // defined in libavcodec/avcodec.h
  end;
  PAVSubtitle = ^TAVSubtitle;
  TAVSubtitle = record
    // need {$ALIGN 8}
    // defined in libavcodec/avcodec.h
  end;
  PAVPacket = ^TAVPacket;
  TAVPacket = record
    // need {$ALIGN 8}
    // defined in libavcodec/packet.h
  end;
  PAVFrame = ^TAVFrame;
  TAVFrame = record
    // need {$ALIGN 8}
    // defined in libavutil/frame.h
  end;

(**
 * AVCodec.
 *)
  PPAVCodec = ^PAVCodec;
  PAVCodec = ^TAVCodec;
  TAVCodec = record
    (**
     * Name of the codec implementation.
     * The name is globally unique among encoders and among decoders (but an
     * encoder and a decoder can share the same name).
     * This is the primary way to find a codec from the user perspective.
     *)
    name: PAnsiChar;
    (**
     * Descriptive name for the codec, meant to be more human readable than name.
     * You should use the NULL_IF_CONFIG_SMALL() macro to define it.
     *)
    long_name: PAnsiChar;
    ttype: TAVMediaType;
    id: TAVCodecID;
    (**
     * Codec capabilities.
     * see AV_CODEC_CAP_*
     *)
    capabilities: Integer;
    supported_framerates: PAVRational;  ///< array of supported framerates, or NULL if any, array is terminated by {0,0}
    pix_fmts: PAVPixelFormat;           ///< array of supported pixel formats, or NULL if unknown, array is terminated by -1
    supported_samplerates: PInteger;    ///< array of supported audio samplerates, or NULL if unknown, array is terminated by 0
    sample_fmts: PAVSampleFormat;       ///< array of supported sample formats, or NULL if unknown, array is terminated by -1
    channel_layouts: PInt64;            ///< array of support channel layouts, or NULL if unknown. array is terminated by 0
    max_lowres: Byte;                   ///< maximum value for lowres supported by the decoder
    priv_class: PAVClass;               ///< AVClass for the private context
    profiles: PAVProfile;               ///< array of recognized profiles, or NULL if unknown, array is terminated by {FF_PROFILE_UNKNOWN}

    (**
     * Group name of the codec implementation.
     * This is a short symbolic name of the wrapper backing this codec. A
     * wrapper uses some kind of external implementation for the codec, such
     * as an external library, or a codec implementation provided by the OS or
     * the hardware.
     * If this field is NULL, this is a builtin, libavcodec native codec.
     * If non-NULL, this will be the suffix in AVCodec.name in most cases
     * (usually AVCodec.name will be of the form "<codec_name>_<wrapper_name>").
     *)
    wrapper_name: PAnsiChar;

    (*****************************************************************
     * No fields below this line are part of the public API. They
     * may not be used outside of libavcodec and can be changed and
     * removed at will.
     * New public fields should be added right above.
     *****************************************************************
     *)
    priv_data_size: Integer;
{$IFDEF FF_API_NEXT}
    next: PAVCodec;
{$ENDIF}
    (**
     * @name Frame-level threading support functions
     * @{
     *)
    (**
     * Copy necessary context variables from a previous thread context to the current one.
     * If not defined, the next thread will start automatically; otherwise, the codec
     * must call ff_thread_finish_setup().
     *
     * dst and src will (rarely) point to the same context, in which case memcpy should be skipped.
     *)
    update_thread_context: function(dst, src: PAVCodecContext): Integer; cdecl;
    (** @} *)

    (**
     * Private codec-specific defaults.
     *)
    defaults: PAVCodecDefault;

    (**
     * Initialize codec static data, called from av_codec_iterate().
     *
     * This is not intended for time consuming operations as it is
     * run for every codec regardless of that codec being used.
     *)
    init_static_data: function(codec: PAVCodec): Pointer; cdecl;

    init: function(avctx: PAVCodecContext): Integer; cdecl;
    encode_sub: function(avctx: PAVCodecContext; buf: PByte; buf_size: Integer;
                      const sub: PAVSubtitle): Integer; cdecl;
    (**
     * Encode data to an AVPacket.
     *
     * @param      avctx          codec context
     * @param      avpkt          output AVPacket
     * @param[in]  frame          AVFrame containing the raw data to be encoded
     * @param[out] got_packet_ptr encoder sets to 0 or 1 to indicate that a
     *                            non-empty packet was returned in avpkt.
     * @return 0 on success, negative error code on failure
     *)
    encode2: function(avctx: PAVCodecContext; avpkt: PAVPacket;
                      const frame: PAVFrame; got_packet_ptr: PInteger): Integer; cdecl;
    (**
     * Decode picture or subtitle data.
     *
     * @param      avctx          codec context
     * @param      outdata        codec type dependent output struct
     * @param[out] got_frame_ptr  decoder sets to 0 or 1 to indicate that a
     *                            non-empty frame or subtitle was returned in
     *                            outdata.
     * @param[in]  avpkt          AVPacket containing the data to be decoded
     * @return amount of bytes read from the packet on success, negative error
     *         code on failure
     *)
    decode: function(avctx: PAVCodecContext; outdata: Pointer;
                      got_frame_ptr: PInteger; avpkt: PAVPacket): Integer; cdecl;
    close: function(avcctx: PAVCodecContext): Integer; cdecl;
    (**
     * Encode API with decoupled frame/packet dataflow. This function is called
     * to get one output packet. It should call ff_encode_get_frame() to obtain
     * input data.
     *)
    receive_packet: function(avctx: PAVCodecContext; avpkt: PAVPacket): Integer; cdecl;

    (**
     * Decode API with decoupled packet/frame dataflow. This function is called
     * to get one output frame. It should call ff_decode_get_packet() to obtain
     * input data.
     *)
    receive_frame: function(avctx: PAVCodecContext; frame: PAVFrame): Integer; cdecl;
    (**
     * Flush buffers.
     * Will be called when seeking
     *)
    flush: procedure(avcctx: PAVCodecContext); cdecl;
    (**
     * Internal codec capabilities.
     * See FF_CODEC_CAP_* in internal.h
     *)
    caps_internal: Integer;

    (**
     * Decoding only, a comma-separated list of bitstream filters to apply to
     * packets before decoding.
     *)
    bsfs: PAnsiChar;

    (**
     * Array of pointers to hardware configurations supported by the codec,
     * or NULL if no hardware supported.  The array is terminated by a NULL
     * pointer.
     *
     * The user can only access this field via avcodec_get_hw_config().
     *)
    hw_configs: PPAVCodecHWConfigInternal;

    (**
     * List of supported codec_tags, terminated by FF_CODEC_TAGS_END.
     *)
    codec_tags: PCardinal;
  end;

(**
 * Iterate over all registered codecs.
 *
 * @param opaque a pointer where libavcodec will store the iteration state. Must
 *               point to NULL to start the iteration.
 *
 * @return the next registered codec or NULL when the iteration is
 *         finished
 *)
function av_codec_iterate(opaque: PPointer): PAVCodec; cdecl; external AVCODEC_LIBNAME name _PU + 'av_codec_iterate';

(**
 * Find a registered decoder with a matching codec ID.
 *
 * @param id AVCodecID of the requested decoder
 * @return A decoder if one was found, NULL otherwise.
 *)
function avcodec_find_decoder(id: TAVCodecID): PAVCodec; cdecl; external AVCODEC_LIBNAME name _PU + 'avcodec_find_decoder';

(**
 * Find a registered decoder with the specified name.
 *
 * @param name name of the requested decoder
 * @return A decoder if one was found, NULL otherwise.
 *)
function avcodec_find_decoder_by_name(const name: PAnsiChar): PAVCodec; cdecl; external AVCODEC_LIBNAME name _PU + 'avcodec_find_decoder_by_name';

(**
 * Find a registered encoder with a matching codec ID.
 *
 * @param id AVCodecID of the requested encoder
 * @return An encoder if one was found, NULL otherwise.
 *)
function avcodec_find_encoder(id: TAVCodecID): PAVCodec; cdecl; external AVCODEC_LIBNAME name _PU + 'avcodec_find_encoder';

(**
 * Find a registered encoder with the specified name.
 *
 * @param name name of the requested encoder
 * @return An encoder if one was found, NULL otherwise.
 *)
function avcodec_find_encoder_by_name(const name: PAnsiChar): PAVCodec; cdecl; external AVCODEC_LIBNAME name _PU + 'avcodec_find_encoder_by_name';

(**
 * @return a non-zero number if codec is an encoder, zero otherwise
 *)
function av_codec_is_encoder(const codec: PAVCodec): Integer; cdecl; external AVCODEC_LIBNAME name _PU + 'av_codec_is_encoder';

(**
 * @return a non-zero number if codec is a decoder, zero otherwise
 *)
function av_codec_is_decoder(const codec: PAVCodec): Integer; cdecl; external AVCODEC_LIBNAME name _PU + 'av_codec_is_decoder';

const
  (**
   * The codec supports this format via the hw_device_ctx interface.
   *
   * When selecting this format, AVCodecContext.hw_device_ctx should
   * have been set to a device of the specified type before calling
   * avcodec_open2().
   *)
  AV_CODEC_HW_CONFIG_METHOD_HW_DEVICE_CTX = $01;
  (**
   * The codec supports this format via the hw_frames_ctx interface.
   *
   * When selecting this format for a decoder,
   * AVCodecContext.hw_frames_ctx should be set to a suitable frames
   * context inside the get_format() callback.  The frames context
   * must have been created on a device of the specified type.
   *
   * When selecting this format for an encoder,
   * AVCodecContext.hw_frames_ctx should be set to the context which
   * will be used for the input frames before calling avcodec_open2().
   *)
  AV_CODEC_HW_CONFIG_METHOD_HW_FRAMES_CTX = $02;
  (**
   * The codec supports this format by some internal method.
   *
   * This format can be selected without any additional configuration -
   * no device or frames context is required.
   *)
  AV_CODEC_HW_CONFIG_METHOD_INTERNAL      = $04;
  (**
   * The codec supports this format by some ad-hoc method.
   *
   * Additional settings and/or function calls are required.  See the
   * codec-specific documentation for details.  (Methods requiring
   * this sort of configuration are deprecated and others should be
   * used in preference.)
   *)
  AV_CODEC_HW_CONFIG_METHOD_AD_HOC        = $08;

type
  PAVCodecHWConfig = ^TAVCodecHWConfig;
  TAVCodecHWConfig = record
    (**
     * For decoders, a hardware pixel format which that decoder may be
     * able to decode to if suitable hardware is available.
     *
     * For encoders, a pixel format which the encoder may be able to
     * accept.  If set to AV_PIX_FMT_NONE, this applies to all pixel
     * formats supported by the codec.
     *)
    pix_fmt: TAVPixelFormat;
    (**
     * Bit set of AV_CODEC_HW_CONFIG_METHOD_* flags, describing the possible
     * setup methods which can be used with this configuration.
     *)
    methods: Integer;
    (**
     * The device type associated with the configuration.
     *
     * Must be set for AV_CODEC_HW_CONFIG_METHOD_HW_DEVICE_CTX and
     * AV_CODEC_HW_CONFIG_METHOD_HW_FRAMES_CTX, otherwise unused.
     *)
    device_type: TAVHWDeviceType;
  end;

(**
 * Retrieve supported hardware configurations for a codec.
 *
 * Values of index from zero to some maximum return the indexed configuration
 * descriptor; all other values return NULL.  If the codec does not support
 * any hardware configurations then it will always return NULL.
 *)
function avcodec_get_hw_config(const codec: PAVCodec; index: Integer): PAVCodecHWConfig; cdecl; external AVCODEC_LIBNAME name _PU + 'avcodec_get_hw_config';

(**
 * @}
 *)

implementation

end.
