(*
 * Bitstream filters public API
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
 * Original file: libavcodec/bsf.h
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

unit libavcodec_bsf;

interface

{$I CompilerDefines.inc}

uses
  libavcodec_codec_id,
  libavcodec_codec_par,
  libavcodec_packet,
  libavutil_dict,
  libavutil_log,
  libavutil_rational;

{$I libversion.inc}

(**
 * @addtogroup lavc_core
 * @{
 *)

type
  PAVBSFInternal = ^TAVBSFInternal;
  TAVBSFInternal = record
    // need {$ALIGN 8}
    // defined in libavcodec/bsf.c
  end;

(**
 * The bitstream filter state.
 *
 * This struct must be allocated with av_bsf_alloc() and freed with
 * av_bsf_free().
 *
 * The fields in the struct will only be changed (by the caller or by the
 * filter) as described in their documentation, and are to be considered
 * immutable otherwise.
 *)
  PAVBitStreamFilter = ^TAVBitStreamFilter;
  PPAVBSFContext = ^PAVBSFContext;
  PAVBSFContext = ^TAVBSFContext;
  TAVBSFContext = record
    (**
     * A class for logging and AVOptions
     *)
    av_class: PAVClass;

    (**
     * The bitstream filter this context is an instance of.
     *)
    filter: PAVBitStreamFilter;

    (**
     * Opaque libavcodec internal data. Must not be touched by the caller in any
     * way.
     *)
    internal: PAVBSFInternal;

    (**
     * Opaque filter-specific private data. If filter->priv_class is non-NULL,
     * this is an AVOptions-enabled struct.
     *)
    priv_data: Pointer;

    (**
     * Parameters of the input stream. This field is allocated in
     * av_bsf_alloc(), it needs to be filled by the caller before
     * av_bsf_init().
     *)
    par_in: PAVCodecParameters;

    (**
     * Parameters of the output stream. This field is allocated in
     * av_bsf_alloc(), it is set by the filter in av_bsf_init().
     *)
    par_out: PAVCodecParameters;

    (**
     * The timebase used for the timestamps of the input packets. Set by the
     * caller before av_bsf_init().
     *)
    time_base_in: TAVRational;

    (**
     * The timebase used for the timestamps of the output packets. Set by the
     * filter in av_bsf_init().
     *)
    time_base_out: TAVRational;
  end;

  TAVBitStreamFilter = record
    name: PAnsiChar;

    (**
     * A list of codec ids supported by the filter, terminated by
     * AV_CODEC_ID_NONE.
     * May be NULL, in that case the bitstream filter works with any codec id.
     *)
    codec_ids: PAVCodecID;

    (**
     * A class for the private data, used to declare bitstream filter private
     * AVOptions. This field is NULL for bitstream filters that do not declare
     * any options.
     *
     * If this field is non-NULL, the first member of the filter private data
     * must be a pointer to AVClass, which will be set by libavcodec generic
     * code to this class.
     *)
    priv_class: PAVClass;

    (*****************************************************************
     * No fields below this line are part of the public API. They
     * may not be used outside of libavcodec and can be changed and
     * removed at will.
     * New public fields should be added right above.
     *****************************************************************
     *)

    priv_data_size: Integer;
    init: function(ctx: PAVBSFContext): Integer; cdecl;
    filter: function(ctx: PAVBSFContext; pkt: PAVPacket): Integer; cdecl;
    close: procedure(ctx: PAVBSFContext); cdecl;
    flush: procedure(ctx: PAVBSFContext); cdecl;
  end;

(**
 * @return a bitstream filter with the specified name or NULL if no such
 *         bitstream filter exists.
 *)
function av_bsf_get_by_name(const name: PAnsiChar): PAVBitStreamFilter; cdecl; external AVCODEC_LIBNAME name _PU + 'av_bsf_get_by_name';

(**
 * Iterate over all registered bitstream filters.
 *
 * @param opaque a pointer where libavcodec will store the iteration state. Must
 *               point to NULL to start the iteration.
 *
 * @return the next registered bitstream filter or NULL when the iteration is
 *         finished
 *)
function av_bsf_iterate(opaque: PPointer): PAVBitStreamFilter; cdecl; external AVCODEC_LIBNAME name _PU + 'av_bsf_iterate';

(**
 * Allocate a context for a given bitstream filter. The caller must fill in the
 * context parameters as described in the documentation and then call
 * av_bsf_init() before sending any data to the filter.
 *
 * @param filter the filter for which to allocate an instance.
 * @param ctx a pointer into which the pointer to the newly-allocated context
 *            will be written. It must be freed with av_bsf_free() after the
 *            filtering is done.
 *
 * @return 0 on success, a negative AVERROR code on failure
 *)
function av_bsf_alloc(const filter: PAVBitStreamFilter; ctx: PPAVBSFContext): Integer; cdecl; external AVCODEC_LIBNAME name _PU + 'av_bsf_alloc';

(**
 * Prepare the filter for use, after all the parameters and options have been
 * set.
 *)
function av_bsf_init(ctx: PAVBSFContext): Integer; cdecl; external AVCODEC_LIBNAME name _PU + 'av_bsf_init';

(**
 * Submit a packet for filtering.
 *
 * After sending each packet, the filter must be completely drained by calling
 * av_bsf_receive_packet() repeatedly until it returns AVERROR(EAGAIN) or
 * AVERROR_EOF.
 *
 * @param pkt the packet to filter. The bitstream filter will take ownership of
 * the packet and reset the contents of pkt. pkt is not touched if an error occurs.
 * If pkt is empty (i.e. NULL, or pkt->data is NULL and pkt->side_data_elems zero),
 * it signals the end of the stream (i.e. no more non-empty packets will be sent;
 * sending more empty packets does nothing) and will cause the filter to output
 * any packets it may have buffered internally.
 *
 * @return 0 on success. AVERROR(EAGAIN) if packets need to be retrieved from the
 * filter (using av_bsf_receive_packet()) before new input can be consumed. Another
 * negative AVERROR value if an error occurs.
 *)
function av_bsf_send_packet(ctx: PAVBSFContext; pkt: PAVPacket): Integer; cdecl; external AVCODEC_LIBNAME name _PU + 'av_bsf_send_packet';

(**
 * Retrieve a filtered packet.
 *
 * @param[out] pkt this struct will be filled with the contents of the filtered
 *                 packet. It is owned by the caller and must be freed using
 *                 av_packet_unref() when it is no longer needed.
 *                 This parameter should be "clean" (i.e. freshly allocated
 *                 with av_packet_alloc() or unreffed with av_packet_unref())
 *                 when this function is called. If this function returns
 *                 successfully, the contents of pkt will be completely
 *                 overwritten by the returned data. On failure, pkt is not
 *                 touched.
 *
 * @return 0 on success. AVERROR(EAGAIN) if more packets need to be sent to the
 * filter (using av_bsf_send_packet()) to get more output. AVERROR_EOF if there
 * will be no further output from the filter. Another negative AVERROR value if
 * an error occurs.
 *
 * @note one input packet may result in several output packets, so after sending
 * a packet with av_bsf_send_packet(), this function needs to be called
 * repeatedly until it stops returning 0. It is also possible for a filter to
 * output fewer packets than were sent to it, so this function may return
 * AVERROR(EAGAIN) immediately after a successful av_bsf_send_packet() call.
 *)
function av_bsf_receive_packet(ctx: PAVBSFContext; pkt: PAVPacket): Integer; cdecl; external AVCODEC_LIBNAME name _PU + 'av_bsf_receive_packet';

(**
 * Reset the internal bitstream filter state. Should be called e.g. when seeking.
 *)
procedure av_bsf_flush(ctx: PAVBSFContext); cdecl; external AVCODEC_LIBNAME name _PU + 'av_bsf_flush';

(**
 * Free a bitstream filter context and everything associated with it; write NULL
 * into the supplied pointer.
 *)
procedure av_bsf_free(ctx: PPAVBSFContext); cdecl; external AVCODEC_LIBNAME name _PU + 'av_bsf_free';

(**
 * Get the AVClass for AVBSFContext. It can be used in combination with
 * AV_OPT_SEARCH_FAKE_OBJ for examining options.
 *
 * @see av_opt_find().
 *)
function av_bsf_get_class: PAVClass; cdecl; external AVCODEC_LIBNAME name _PU + 'av_bsf_get_class';

type
(**
 * Structure for chain/list of bitstream filters.
 * Empty list can be allocated by av_bsf_list_alloc().
 *)
  PPAVBSFList = ^PAVBSFList;
  PAVBSFList = ^TAVBSFList;
  TAVBSFList = record
    // need {$ALIGN 8}
    // defined in libavcodec/bsf.c
  end;

(**
 * Allocate empty list of bitstream filters.
 * The list must be later freed by av_bsf_list_free()
 * or finalized by av_bsf_list_finalize().
 *
 * @return Pointer to @ref AVBSFList on success, NULL in case of failure
 *)
function av_bsf_list_alloc: PAVBSFList; cdecl; external AVCODEC_LIBNAME name _PU + 'av_bsf_list_alloc';

(**
 * Free list of bitstream filters.
 *
 * @param lst Pointer to pointer returned by av_bsf_list_alloc()
 *)
procedure av_bsf_list_free(lst: PPAVBSFList); cdecl; external AVCODEC_LIBNAME name _PU + 'av_bsf_list_free';

(**
 * Append bitstream filter to the list of bitstream filters.
 *
 * @param lst List to append to
 * @param bsf Filter context to be appended
 *
 * @return >=0 on success, negative AVERROR in case of failure
 *)
function av_bsf_list_append(lst: PAVBSFList; bsf: PAVBSFContext): Integer; cdecl; external AVCODEC_LIBNAME name _PU + 'av_bsf_list_append';

(**
 * Construct new bitstream filter context given it's name and options
 * and append it to the list of bitstream filters.
 *
 * @param lst      List to append to
 * @param bsf_name Name of the bitstream filter
 * @param options  Options for the bitstream filter, can be set to NULL
 *
 * @return >=0 on success, negative AVERROR in case of failure
 *)
function av_bsf_list_append2(lst: PAVBSFList; const bsf_name: PAnsiChar; options: PPAVDictionary): Integer; cdecl; external AVCODEC_LIBNAME name _PU + 'av_bsf_list_append2';

(**
 * Finalize list of bitstream filters.
 *
 * This function will transform @ref AVBSFList to single @ref AVBSFContext,
 * so the whole chain of bitstream filters can be treated as single filter
 * freshly allocated by av_bsf_alloc().
 * If the call is successful, @ref AVBSFList structure is freed and lst
 * will be set to NULL. In case of failure, caller is responsible for
 * freeing the structure by av_bsf_list_free()
 *
 * @param      lst Filter list structure to be transformed
 * @param[out] bsf Pointer to be set to newly created @ref AVBSFContext structure
 *                 representing the chain of bitstream filters
 *
 * @return >=0 on success, negative AVERROR in case of failure
 *)
function av_bsf_list_finalize(lst: PPAVBSFList; bsf: PPAVBSFContext): Integer; cdecl; external AVCODEC_LIBNAME name _PU + 'av_bsf_list_finalize';

(**
 * Parse string describing list of bitstream filters and create single
 * @ref AVBSFContext describing the whole chain of bitstream filters.
 * Resulting @ref AVBSFContext can be treated as any other @ref AVBSFContext freshly
 * allocated by av_bsf_alloc().
 *
 * @param      str String describing chain of bitstream filters in format
 *                 `bsf1[=opt1=val1:opt2=val2][,bsf2]`
 * @param[out] bsf Pointer to be set to newly created @ref AVBSFContext structure
 *                 representing the chain of bitstream filters
 *
 * @return >=0 on success, negative AVERROR in case of failure
 *)
function av_bsf_list_parse_str(const str: PAnsiChar; bsf: PPAVBSFContext): Integer; cdecl; external AVCODEC_LIBNAME name _PU + 'av_bsf_list_parse_str';

(**
 * Get null/pass-through bitstream filter.
 *
 * @param[out] bsf Pointer to be set to new instance of pass-through bitstream filter
 *
 * @return
 *)
function av_bsf_get_null_filter(bsf: PPAVBSFContext): Integer; cdecl; external AVCODEC_LIBNAME name _PU + 'av_bsf_get_null_filter';

(**
 * @}
 *)

implementation

end.
