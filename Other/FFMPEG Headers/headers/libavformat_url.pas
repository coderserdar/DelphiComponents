(*
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
 * unbuffered private I/O API
 *)

(*
 * FFVCL - Delphi FFmpeg VCL Components
 * http://www.DelphiFFmpeg.com
 *
 * Original file: libavformat/url.h
 * Ported by CodeCoolie@CNSW 2012/10/30 -> $Date:: 2021-04-25 #$
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

unit libavformat_url;

interface

{$I CompilerDefines.inc}

uses
  libavformat_avio,
  libavutil_dict,
  libavutil_log;

{$I libversion.inc}

const
  URL_PROTOCOL_FLAG_NESTED_SCHEME = 1; (*< The protocol name can be the first part of a nested protocol scheme *)
  URL_PROTOCOL_FLAG_NETWORK       = 2; (*< The protocol uses network *)

//extern const AVClass ffurl_context_class;

type
  PPURLProtocol = ^PURLProtocol;
  PURLProtocol = ^TURLProtocol;
  PPURLContext = ^PURLContext;
  PURLContext = ^TURLContext;
  TURLContext = record
    av_class: PAVClass;         (**< information for av_log(). Set by url_open(). *)
    prot: PURLProtocol;
    priv_data: Pointer;
    filename: PAnsiChar;        (**< specified URL *)
    flags: Integer;
    max_packet_size: Integer;   (**< if non zero, the stream is packetized with this max packet size *)
    is_streamed: Integer;       (**< true if streamed (no seek possible), default = false *)
    is_connected: Integer;
    interrupt_callback: TAVIOInterruptCB;
    rw_timeout: Int64;          (**< maximum time to wait for (network) read/write operation completion, in mcs *)
    protocol_whitelist: PAnsiChar;
    protocol_blacklist: PAnsiChar;
    min_packet_size: Integer;   (**< if non zero, the stream is packetized with this min packet size *)
  end;

  PPInteger = ^PInteger;
  TURLProtocol = record
    name: PAnsiChar;
    url_open: function(h: PURLContext; const url: PAnsiChar; flags: Integer): Integer; cdecl;
    (**
     * This callback is to be used by protocols which open further nested
     * protocols. options are then to be passed to ffurl_open_whitelist()
     * or ffurl_connect() for those nested protocols.
     * for those nested protocols.
     *)
    url_open2: function(h: PURLContext; const url: PAnsiChar; flags: Integer; options: PPAVDictionary): Integer; cdecl;
    url_accept: function(s: PURLContext; c: PPURLContext): Integer; cdecl;
    url_handshake: function(c: PURLContext): Integer; cdecl;

    (**
     * Read data from the protocol.
     * If data is immediately available (even less than size), EOF is
     * reached or an error occurs (including EINTR), return immediately.
     * Otherwise:
     * In non-blocking mode, return AVERROR(EAGAIN) immediately.
     * In blocking mode, wait for data/EOF/error with a short timeout (0.1s),
     * and return AVERROR(EAGAIN) on timeout.
     * Checking interrupt_callback, looping on EINTR and EAGAIN and until
     * enough data has been read is left to the calling function; see
     * retry_transfer_wrapper in avio.c.
     *)
    url_read: function(h: PURLContext; buf: PAnsiChar; size: Integer): Integer; cdecl;
    url_write: function(h: PURLContext; const buf: PAnsiChar; size: Integer): Integer; cdecl;
    url_seek: function(h: PURLContext; pos: Int64; whence: Integer): Int64; cdecl;
    url_close: function(h: PURLContext): Integer; cdecl;
    url_read_pause: function(h: PURLContext; pause: Integer): Integer; cdecl;
    url_read_seek: function(h: PURLContext; stream_index: Integer;
                             timestamp: Int64; flags: Integer): Int64; cdecl;
    url_get_file_handle: function(h: PURLContext): Integer; cdecl;
    url_get_multi_file_handle: function(h: PURLContext; handles: PPInteger;
                                     numhandles: PInteger): Integer; cdecl;
    url_get_short_seek: function(h: PURLContext): Integer; cdecl;
    url_shutdown: function(h: PURLContext; flags: Integer): Integer; cdecl;
    priv_data_class: PAVClass;
    priv_data_size: Integer;
    flags: Integer;
    url_check: function(h: PURLContext; mask: Integer): Integer; cdecl;
    url_open_dir: function(h: PURLContext): Integer; cdecl;
    url_read_dir: function(h: PURLContext; next: PPAVIODirEntry): Integer; cdecl;
    url_close_dir: function(h: PURLContext): Integer; cdecl;
    url_delete: function(h: PURLContext): Integer; cdecl;
    url_move: function(h_src, h_dst: PURLContext): Integer; cdecl;
    default_whitelist: PAnsiChar;
  end;

(**
 * Create a URLContext for accessing to the resource indicated by
 * url, but do not initiate the connection yet.
 *
 * @param puc pointer to the location where, in case of success, the
 * function puts the pointer to the created URLContext
 * @param flags flags which control how the resource indicated by url
 * is to be opened
 * @param int_cb interrupt callback to use for the URLContext, may be
 * NULL
 * @return >= 0 in case of success, a negative value corresponding to an
 * AVERROR code in case of failure
 *)
function ffurl_alloc(puc: PPURLContext; const filename: PAnsiChar; flags: Integer;
                const int_cb: PAVIOInterruptCB): Integer; cdecl; external AVFORMAT_LIBNAME name _PU + 'ffurl_alloc';

(**
 * Connect an URLContext that has been allocated by ffurl_alloc
 *
 * @param options  A dictionary filled with options for nested protocols,
 * i.e. it will be passed to url_open2() for protocols implementing it.
 * This parameter will be destroyed and replaced with a dict containing options
 * that were not found. May be NULL.
 *)
function ffurl_connect(uc: PURLContext; options: PPAVDictionary): Integer; cdecl; external AVFORMAT_LIBNAME name _PU + 'ffurl_connect';

(**
 * Create an URLContext for accessing to the resource indicated by
 * url, and open it.
 *
 * @param puc pointer to the location where, in case of success, the
 * function puts the pointer to the created URLContext
 * @param flags flags which control how the resource indicated by url
 * is to be opened
 * @param int_cb interrupt callback to use for the URLContext, may be
 * NULL
 * @param options  A dictionary filled with protocol-private options. On return
 * this parameter will be destroyed and replaced with a dict containing options
 * that were not found. May be NULL.
 * @param parent An enclosing URLContext, whose generic options should
 *               be applied to this URLContext as well.
 * @return >= 0 in case of success, a negative value corresponding to an
 * AVERROR code in case of failure
 *)
function ffurl_open_whitelist(puc: PPURLContext; const filename: PAnsiChar; flags: Integer;
               const int_cb: PAVIOInterruptCB; options: PPAVDictionary;
               const whitelist: PAnsiChar; const blacklist: PAnsiChar;
               parent: PURLContext): Integer; cdecl; external AVFORMAT_LIBNAME name _PU + 'ffurl_open_whitelist';

(**
 * Accept an URLContext c on an URLContext s
 *
 * @param  s server context
 * @param  c client context, must be unallocated.
 * @return >= 0 on success, ff_neterrno() on failure.
 *)
function ffurl_accept(s: PURLContext; c: PPURLContext): Integer; cdecl; external AVFORMAT_LIBNAME name _PU + 'ffurl_accept';

(**
 * Perform one step of the protocol handshake to accept a new client.
 * See avio_handshake() for details.
 * Implementations should try to return decreasing values.
 * If the protocol uses an underlying protocol, the underlying handshake is
 * usually the first step, and the return value can be:
 * (largest value for this protocol) + (return value from other protocol)
 *
 * @param  c the client context
 * @return >= 0 on success or a negative value corresponding
 *         to an AVERROR code on failure
 *)
function ffurl_handshake(c: PURLContext): Integer; cdecl; external AVFORMAT_LIBNAME name _PU + 'ffurl_handshake';

(**
 * Read up to size bytes from the resource accessed by h, and store
 * the read bytes in buf.
 *
 * @return The number of bytes actually read, or a negative value
 * corresponding to an AVERROR code in case of error. A value of zero
 * indicates that it is not possible to read more from the accessed
 * resource (except if the value of the size argument is also zero).
 *)
function ffurl_read(h: PURLContext; buf: PByte; size: Integer): Integer; cdecl; external AVFORMAT_LIBNAME name _PU + 'ffurl_read';

(**
 * Read as many bytes as possible (up to size), calling the
 * read function multiple times if necessary.
 * This makes special short-read handling in applications
 * unnecessary, if the return value is < size then it is
 * certain there was either an error or the end of file was reached.
 *)
function ffurl_read_complete(h: PURLContext; buf: PByte; size: Integer): Integer; cdecl; external AVFORMAT_LIBNAME name _PU + 'ffurl_read_complete';

(**
 * Write size bytes from buf to the resource accessed by h.
 *
 * @return the number of bytes actually written, or a negative value
 * corresponding to an AVERROR code in case of failure
 *)
function ffurl_write(h: PURLContext; const buf: PByte; size: Integer): Integer; cdecl; external AVFORMAT_LIBNAME name _PU + 'ffurl_write';

(**
 * Change the position that will be used by the next read/write
 * operation on the resource accessed by h.
 *
 * @param pos specifies the new position to set
 * @param whence specifies how pos should be interpreted, it must be
 * one of SEEK_SET (seek from the beginning), SEEK_CUR (seek from the
 * current position), SEEK_END (seek from the end), or AVSEEK_SIZE
 * (return the filesize of the requested resource, pos is ignored).
 * @return a negative value corresponding to an AVERROR code in case
 * of failure, or the resulting file position, measured in bytes from
 * the beginning of the file. You can use this feature together with
 * SEEK_CUR to read the current file position.
 *)
function ffurl_seek(h: PURLContext; pos: Int64; whence: Integer): Int64; cdecl; external AVFORMAT_LIBNAME name _PU + 'ffurl_seek';

(**
 * Close the resource accessed by the URLContext h, and free the
 * memory used by it. Also set the URLContext pointer to NULL.
 *
 * @return a negative value if an error condition occurred, 0
 * otherwise
 *)
function ffurl_closep(h: PPURLContext): Integer; cdecl; external AVFORMAT_LIBNAME name _PU + 'ffurl_closep';
function ffurl_close(h: PURLContext): Integer; cdecl; external AVFORMAT_LIBNAME name _PU + 'ffurl_close';

(**
 * Return the filesize of the resource accessed by h, AVERROR(ENOSYS)
 * if the operation is not supported by h, or another negative value
 * corresponding to an AVERROR error code in case of failure.
 *)
function ffurl_size(h: PURLContext): Int64; cdecl; external AVFORMAT_LIBNAME name _PU + 'ffurl_size';

(**
 * Return the file descriptor associated with this URL. For RTP, this
 * will return only the RTP file descriptor, not the RTCP file descriptor.
 *
 * @return the file descriptor associated with this URL, or <0 on error.
 *)
function ffurl_get_file_handle(h: PURLContext): Integer; cdecl; external AVFORMAT_LIBNAME name _PU + 'ffurl_get_file_handle';

(**
 * Return the file descriptors associated with this URL.
 *
 * @return 0 on success or <0 on error.
 *)
function ffurl_get_multi_file_handle(h: PURLContext; handles: PPInteger; numhandles: Integer): Integer; cdecl; external AVFORMAT_LIBNAME name _PU + 'ffurl_get_multi_file_handle';

(**
 * Return the current short seek threshold value for this URL.
 *
 * @return threshold (>0) on success or <=0 on error.
 *)
function ffurl_get_short_seek(h: PURLContext): Integer; cdecl; external AVFORMAT_LIBNAME name _PU + 'ffurl_get_short_seek';

(**
 * Signal the URLContext that we are done reading or writing the stream.
 *
 * @param h pointer to the resource
 * @param flags flags which control how the resource indicated by url
 * is to be shutdown
 *
 * @return a negative value if an error condition occurred, 0
 * otherwise
 *)
function ffurl_shutdown(h: PURLContext; flags: Integer): Integer; cdecl; external AVFORMAT_LIBNAME name _PU + 'ffurl_shutdown';

(**
 * Check if the user has requested to interrup a blocking function
 * associated with cb.
 *)
function ff_check_interrupt(cb: PAVIOInterruptCB): Integer; cdecl; external AVFORMAT_LIBNAME name _PU + 'ff_check_interrupt';

(* udp.c *)
function ff_udp_set_remote_url(h: PURLContext; const uri: PAnsiChar): Integer; cdecl; external AVFORMAT_LIBNAME name _PU + 'ff_udp_set_remote_url';
function ff_udp_get_local_port(h: PURLContext): Integer; cdecl; external AVFORMAT_LIBNAME name _PU + 'ff_udp_get_local_port';

(**
 * Assemble a URL string from components. This is the reverse operation
 * of av_url_split.
 *
 * Note, this requires networking to be initialized, so the caller must
 * ensure ff_network_init has been called.
 *
 * @see av_url_split
 *
 * @param str the buffer to fill with the url
 * @param size the size of the str buffer
 * @param proto the protocol identifier, if null, the separator
 *              after the identifier is left out, too
 * @param authorization an optional authorization string, may be null.
 *                      An empty string is treated the same as a null string.
 * @param hostname the host name string
 * @param port the port number, left out from the string if negative
 * @param fmt a generic format string for everything to add after the
 *            host/port, may be null
 * @return the number of characters written to the destination buffer
 *)
function ff_url_join(str: PAnsiChar; size: Integer; const proto,
                authorization, hostname: PAnsiChar;
                port: Integer; const fmt: PAnsiChar): Integer; cdecl varargs; external AVFORMAT_LIBNAME name _PU + 'ff_url_join';

(**
 * Convert a relative url into an absolute url, given a base url.
 *
 * @param buf the buffer where output absolute url is written
 * @param size the size of buf
 * @param base the base url, may be equal to buf.
 * @param handle_dos_paths handle DOS paths for file or unspecified protocol
 *)
function ff_make_absolute_url2(buf: PAnsiChar; size: Integer; const base: PAnsiChar;
                         const rel: PAnsiChar; handle_dos_paths: Integer): Integer; cdecl; external AVFORMAT_LIBNAME name _PU + 'ff_make_absolute_url2';

(**
 * Convert a relative url into an absolute url, given a base url.
 *
 * Same as ff_make_absolute_url2 with handle_dos_paths being equal to
 * HAVE_DOS_PATHS config variable.
 *)
procedure ff_make_absolute_url(buf: PAnsiChar; size: Integer; const base, rel: PAnsiChar); cdecl; external AVFORMAT_LIBNAME name _PU + 'ff_make_absolute_url';

(**
 * Allocate directory entry with default values.
 *
 * @return entry or NULL on error
 *)
function ff_alloc_dir_entry: PAVIODirEntry; cdecl; external AVFORMAT_LIBNAME name _PU + 'ff_alloc_dir_entry';

{$IFDEF FF_API_CHILD_CLASS_NEXT}
function ff_urlcontext_child_class_next(const prev: PAVClass): PAVClass; cdecl; external AVFORMAT_LIBNAME name _PU + 'ff_urlcontext_child_class_next';
{$ENDIF}

function ff_urlcontext_child_class_iterate(iter: PPointer): PAVClass; cdecl; external AVFORMAT_LIBNAME name _PU + 'ff_urlcontext_child_class_iterate';

(**
 * Construct a list of protocols matching a given whitelist and/or blacklist.
 *
 * @param whitelist a comma-separated list of allowed protocol names or NULL. If
 *                  this is a non-empty string, only protocols in this list will
 *                  be included.
 * @param blacklist a comma-separated list of forbidden protocol names or NULL.
 *                  If this is a non-empty string, all protocols in this list
 *                  will be excluded.
 *
 * @return a NULL-terminated array of matching protocols. The array must be
 * freed by the caller.
 *)
function ffurl_get_protocols(const whitelist: PAnsiChar;
                                      const blacklist: PAnsiChar): PPURLProtocol; cdecl; external AVFORMAT_LIBNAME name _PU + 'ffurl_get_protocols';

implementation

end.
