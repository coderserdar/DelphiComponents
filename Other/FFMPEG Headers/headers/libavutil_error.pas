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
 * error code definitions
 *)

(*
 * FFVCL - Delphi FFmpeg VCL Components
 * http://www.DelphiFFmpeg.com
 *
 * Original file: libavutil/error.h
 * Ported by CodeCoolie@CNSW 2010/03/20 -> $Date:: 2019-11-02 #$
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

unit libavutil_error;

interface

{$I CompilerDefines.inc}

uses
  FFTypes;

{$I libversion.inc}

const

(**
 * @addtogroup lavu_error
 *
 * @{
 *)


(* error handling *)
//#if EDOM > 0
//#define AVERROR(e) (-(e))   ///< Returns a negative error code from a POSIX error code, to return from library functions.
//#define AVUNERROR(e) (-(e)) ///< Returns a POSIX error code from a library function error return value.
//#else
(* Some platforms have E* and errno already negated. *)
//#define AVERROR(e) (e)
//#define AVUNERROR(e) (e)
//#endif

//#define FFERRTAG(a, b, c, d) (-(int)MKTAG(a, b, c, d))

//#define AVERROR_BSF_NOT_FOUND      FFERRTAG(0xF8,'B','S','F') ///< Bitstream filter not found
  AVERROR_BSF_NOT_FOUND       = -(    $F8  or (Ord('B') shl 8) or (Ord('S') shl 16) or (Ord('F') shl 24));
//#define AVERROR_BUG                FFERRTAG( 'B','U','G','!') ///< Internal bug, also see AVERROR_BUG2
  AVERROR_BUG                 = -(Ord('B') or (Ord('U') shl 8) or (Ord('G') shl 16) or (Ord('!') shl 24));
//#define AVERROR_BUFFER_TOO_SMALL   FFERRTAG( 'B','U','F','S') ///< Buffer too small
  AVERROR_BUFFER_TOO_SMALL    = -(Ord('B') or (Ord('U') shl 8) or (Ord('F') shl 16) or (Ord('S') shl 24));
//#define AVERROR_DECODER_NOT_FOUND  FFERRTAG(0xF8,'D','E','C') ///< Decoder not found
  AVERROR_DECODER_NOT_FOUND   = -(    $F8  or (Ord('D') shl 8) or (Ord('E') shl 16) or (Ord('C') shl 24));
//#define AVERROR_DEMUXER_NOT_FOUND  FFERRTAG(0xF8,'D','E','M') ///< Demuxer not found
  AVERROR_DEMUXER_NOT_FOUND   = -(    $F8  or (Ord('D') shl 8) or (Ord('E') shl 16) or (Ord('M') shl 24));
//#define AVERROR_ENCODER_NOT_FOUND  FFERRTAG(0xF8,'E','N','C') ///< Encoder not found
  AVERROR_ENCODER_NOT_FOUND   = -(    $F8  or (Ord('E') shl 8) or (Ord('N') shl 16) or (Ord('C') shl 24));
//#define AVERROR_EOF                FFERRTAG( 'E','O','F',' ') ///< End of file
  AVERROR_EOF                 = -(Ord('E') or (Ord('O') shl 8) or (Ord('F') shl 16) or (Ord(' ') shl 24));
//#define AVERROR_EXIT               FFERRTAG( 'E','X','I','T') ///< Immediate exit was requested; the called function should not be restarted
  AVERROR_EXIT                = -(Ord('E') or (Ord('X') shl 8) or (Ord('I') shl 16) or (Ord('T') shl 24));
//#define AVERROR_EXTERNAL           FFERRTAG( 'E','X','T',' ') ///< Generic error in an external library
  AVERROR_EXTERNAL            = -(Ord('E') or (Ord('X') shl 8) or (Ord('T') shl 16) or (Ord(' ') shl 24));
//#define AVERROR_FILTER_NOT_FOUND   FFERRTAG(0xF8,'F','I','L') ///< Filter not found
  AVERROR_FILTER_NOT_FOUND    = -(    $F8  or (Ord('F') shl 8) or (Ord('I') shl 16) or (Ord('L') shl 24));
//#define AVERROR_INVALIDDATA        FFERRTAG( 'I','N','D','A') ///< Invalid data found when processing input
  AVERROR_INVALIDDATA         = -(Ord('I') or (Ord('N') shl 8) or (Ord('D') shl 16) or (Ord('A') shl 24));
//#define AVERROR_MUXER_NOT_FOUND    FFERRTAG(0xF8,'M','U','X') ///< Muxer not found
  AVERROR_MUXER_NOT_FOUND     = -(    $F8  or (Ord('M') shl 8) or (Ord('U') shl 16) or (Ord('X') shl 24));
//#define AVERROR_OPTION_NOT_FOUND   FFERRTAG(0xF8,'O','P','T') ///< Option not found
  AVERROR_OPTION_NOT_FOUND    = -(    $F8  or (Ord('O') shl 8) or (Ord('P') shl 16) or (Ord('T') shl 24));
//#define AVERROR_PATCHWELCOME       FFERRTAG( 'P','A','W','E') ///< Not yet implemented in FFmpeg, patches welcome
  AVERROR_PATCHWELCOME        = -(Ord('P') or (Ord('A') shl 8) or (Ord('W') shl 16) or (Ord('E') shl 24));
//#define AVERROR_PROTOCOL_NOT_FOUND FFERRTAG(0xF8,'P','R','O') ///< Protocol not found
  AVERROR_PROTOCOL_NOT_FOUND  = -(    $F8  or (Ord('P') shl 8) or (Ord('R') shl 16) or (Ord('O') shl 24));
//#define AVERROR_STREAM_NOT_FOUND   FFERRTAG(0xF8,'S','T','R') ///< Stream not found
  AVERROR_STREAM_NOT_FOUND    = -(    $F8  or (Ord('S') shl 8) or (Ord('T') shl 16) or (Ord('R') shl 24));

(**
 * This is semantically identical to AVERROR_BUG
 * it has been introduced in Libav after our AVERROR_BUG and with a modified value.
 *)
//#define AVERROR_BUG2               FFERRTAG( 'B','U','G',' ')
  AVERROR_BUG2                = -(Ord('B') or (Ord('U') shl 8) or (Ord('G') shl 16) or (Ord(' ') shl 24));
//#define AVERROR_UNKNOWN            FFERRTAG( 'U','N','K','N') ///< Unknown error, typically from an external library
  AVERROR_UNKNOWN             = -(Ord('U') or (Ord('N') shl 8) or (Ord('K') shl 16) or (Ord('N') shl 24));
//#define AVERROR_EXPERIMENTAL       (-0x2bb2afa8) ///< Requested feature is flagged experimental. Set strict_std_compliance if you really want to use it.
  AVERROR_EXPERIMENTAL       = -$2bb2afa8;
//#define AVERROR_INPUT_CHANGED      (-0x636e6701) ///< Input changed between calls. Reconfiguration is required. (can be OR-ed with AVERROR_OUTPUT_CHANGED)
  AVERROR_INPUT_CHANGED      = -$636e6701;
//#define AVERROR_OUTPUT_CHANGED     (-0x636e6702) ///< Output changed between calls. Reconfiguration is required. (can be OR-ed with AVERROR_INPUT_CHANGED)
  AVERROR_OUTPUT_CHANGED     = -$636e6702;
///* HTTP & RTSP errors */
//#define AVERROR_HTTP_BAD_REQUEST   FFERRTAG(0xF8,'4','0','0')
  AVERROR_HTTP_BAD_REQUEST   = -($F8 or (Ord('4') shl 8) or (Ord('0') shl 16) or (Ord('0') shl 24));
//#define AVERROR_HTTP_UNAUTHORIZED  FFERRTAG(0xF8,'4','0','1')
  AVERROR_HTTP_UNAUTHORIZED  = -($F8 or (Ord('4') shl 8) or (Ord('0') shl 16) or (Ord('1') shl 24));
//#define AVERROR_HTTP_FORBIDDEN     FFERRTAG(0xF8,'4','0','3')
  AVERROR_HTTP_FORBIDDEN     = -($F8 or (Ord('4') shl 8) or (Ord('0') shl 16) or (Ord('3') shl 24));
//#define AVERROR_HTTP_NOT_FOUND     FFERRTAG(0xF8,'4','0','4')
  AVERROR_HTTP_NOT_FOUND     = -($F8 or (Ord('4') shl 8) or (Ord('0') shl 16) or (Ord('4') shl 24));
//#define AVERROR_HTTP_OTHER_4XX     FFERRTAG(0xF8,'4','X','X')
  AVERROR_HTTP_OTHER_4XX     = -($F8 or (Ord('4') shl 8) or (Ord('X') shl 16) or (Ord('X') shl 24));
//#define AVERROR_HTTP_SERVER_ERROR  FFERRTAG(0xF8,'5','X','X')
  AVERROR_HTTP_SERVER_ERROR  = -($F8 or (Ord('5') shl 8) or (Ord('X') shl 16) or (Ord('X') shl 24));

  AV_ERROR_MAX_STRING_SIZE    = 64;

  // TODO: check http://www.ioplex.com/~miallen/errcmp.html
  // strerror_r()
  AVERROR_EPERM           =   -1; ///< Operation not permitted
  AVERROR_ENOENT          =   -2; ///< No such file or directory
  AVERROR_ESRCH           =   -3; ///< No such process
  AVERROR_EINTR           =   -4; ///< Interrupted function call
  AVERROR_EIO             =   -5; ///< I/O error
  AVERROR_ENXIO           =   -6; ///< No such device or address
  AVERROR_E2BIG           =   -7; ///< Argument list too long
  AVERROR_ENOEXEC         =   -8; ///< Exec format error
  AVERROR_EBADF           =   -9; ///< Bad file number
  AVERROR_ECHILD          =  -10; ///< No child processes
{$IFDEF MSWINDOWS}
  AVERROR_EAGAIN          =  -11; ///< Resource temporarily unavailable / Try again
{$ENDIF}
{$IFDEF MACOS}
  AVERROR_EAGAIN          =  -35; ///< Resource temporarily unavailable / Try again
{$ENDIF}
  AVERROR_ENOMEM          =  -12; ///< Not enough space / Out of memory
  AVERROR_EACCES          =  -13; ///< Permission denied
  AVERROR_EFAULT          =  -14; ///< Bad address
  AVERROR_ENOTBLK         =  -15; ///< Block device required (WIN: Unknown error)
  AVERROR_EBUSY           =  -16; ///< Device or resource busy
  AVERROR_EEXIST          =  -17; ///< File exists
  AVERROR_EXDEV           =  -18; ///< Cross-device link
  AVERROR_ENODEV          =  -19; ///< No such device
  AVERROR_ENOTDIR         =  -20; ///< Not a directory
  AVERROR_EISDIR          =  -21; ///< Is a directory
  AVERROR_EINVAL          =  -22; ///< Invalid argument
  AVERROR_ENFILE          =  -23; ///< Too many open files in system / File table overflow
  AVERROR_EMFILE          =  -24; ///< Too many open files
  AVERROR_ENOTTY          =  -25; ///< Inappropriate I/O control operation / Not a typewriter
  AVERROR_ETXTBSY         =  -26; ///< Text file busy (WIN: Unknown error)
  AVERROR_EFBIG           =  -27; ///< File too large
  AVERROR_ENOSPC          =  -28; ///< No space left on device
  AVERROR_ESPIPE          =  -29; ///< Illegal seek
  AVERROR_EROFS           =  -30; ///< Read-only file system
  AVERROR_EMLINK          =  -31; ///< Too many links
  AVERROR_EPIPE           =  -32; ///< Broken pipe
  AVERROR_EDOM            =  -33; ///< Math argument out of domain of func
  AVERROR_ERANGE          =  -34; ///< Math result not representable
{$IFDEF MSWINDOWS}
  AVERROR_EDEADLK         =  -36; ///< Resource deadlock avoided
{$ENDIF}
{$IFDEF MACOS}
  AVERROR_EDEADLK         =  -11; ///< Resource deadlock avoided
{$ENDIF}
{$IFDEF MSWINDOWS}
  AVERROR_ENAMETOOLONG    =  -38; ///< File name too long
  AVERROR_ENOLCK          =  -39; ///< No locks available
  AVERROR_ENOSYS          =  -40; ///< Function not implemented
  AVERROR_ENOTEMPTY       =  -41; ///< Directory not empty
  AVERROR_ELOOP           = -114; ///< Too many symbolic links encountered
{$ENDIF}
{$IFDEF MACOS}
  AVERROR_ENAMETOOLONG    =  -63; ///< File name too long
  AVERROR_ENOLCK          =  -77; ///< No locks available
  AVERROR_ENOSYS          =  -78; ///< Function not implemented
  AVERROR_ENOTEMPTY       =  -66; ///< Directory not empty
  AVERROR_ELOOP           =  -62; ///< Too many symbolic links encountered
{$ENDIF}
  AVERROR_ENOMSG          =  -91; ///< No message of desired type (WIN: Unknown error)
  AVERROR_EIDRM           =  -90; ///< Identifier removed (WIN: Unknown error)
  AVERROR_ENOSTR          =  -99; ///< Device not a stream
  AVERROR_ENODATA         =  -96; ///< No data available
  AVERROR_ETIME           = -101; ///< Timer expired
  AVERROR_ENOSR           =  -98; ///< Out of streams resources
  AVERROR_EREMOTE         =  -71; ///< Too many levels of remote in path
  AVERROR_ENOLINK         =  -97; ///< Link has been severed
  AVERROR_EMULTIHOP       =  -95; ///< Multihop attempted
{
  AVERROR_EDOTDOT         =  -73; ///< RFS specific error
}
  AVERROR_EBADMSG         =  -94; ///< Not a data message
{$IFDEF MSWINDOWS}
  AVERROR_EPROTO          = -134; ///< Protocol error
  AVERROR_EOVERFLOW       = -132; ///< Value too large for defined data type
  AVERROR_EILSEQ          =  -42; ///< Illegal byte sequence
  AVERROR_EUSERS          =  -68; ///< Too many users
  AVERROR_ENOTSOCK        = -128; ///< Socket operation on non-socket
  AVERROR_EDESTADDRREQ    = -109; ///< Destination address required
  AVERROR_EMSGSIZE        = -115; ///< Message too long
  AVERROR_EPROTOTYPE      = -136; ///< Protocol wrong type for socket
  AVERROR_ENOPROTOOPT     = -123; ///< Protocol not available
  AVERROR_EPROTONOSUPPORT = -135; ///< Protocol not supported
  AVERROR_ESOCKTNOSUPPORT =  -44; ///< Socket type not supported
  AVERROR_EOPNOTSUPP      = -130; ///< Operation not supported on transport endpoint
  AVERROR_EPFNOSUPPORT    =  -46; ///< Protocol family not supported
{$ENDIF}
{$IFDEF MACOS}
  AVERROR_EPROTO          = -100; ///< Protocol error
  AVERROR_EOVERFLOW       =  -84; ///< Value too large for defined data type
  AVERROR_EILSEQ          =  -92; ///< Illegal byte sequence
  AVERROR_EUSERS          =  -68; ///< Too many users
  AVERROR_ENOTSOCK        =  -38; ///< Socket operation on non-socket
  AVERROR_EDESTADDRREQ    =  -39; ///< Destination address required
  AVERROR_EMSGSIZE        =  -40; ///< Message too long
  AVERROR_EPROTOTYPE      =  -41; ///< Protocol wrong type for socket
  AVERROR_ENOPROTOOPT     =  -42; ///< Protocol not available
  AVERROR_EPROTONOSUPPORT =  -43; ///< Protocol not supported
  AVERROR_ESOCKTNOSUPPORT =  -44; ///< Socket type not supported
  AVERROR_EOPNOTSUPP      =  -45; ///< Operation not supported on transport endpoint
  AVERROR_EPFNOSUPPORT    =  -46; ///< Protocol family not supported
{$ENDIF}
{$IFDEF MSWINDOWS}
  AVERROR_EAFNOSUPPORT    = -102; ///< Address family not supported by protocol
  AVERROR_EADDRINUSE      = -100; ///< Address already in use
  AVERROR_EADDRNOTAVAIL   = -101; ///< Cannot assign requested address
  AVERROR_ENETDOWN        = -116; ///< Network is down
  AVERROR_ENETUNREACH     = -118; ///< Network is unreachable
  AVERROR_ENETRESET       = -117; ///< Network dropped connection because of reset
  AVERROR_ECONNABORTED    = -106; ///< Software caused connection abort
  AVERROR_ECONNRESET      = -108; ///< Connection reset by peer
  AVERROR_ENOBUFS         = -119; ///< No buffer space available
  AVERROR_EISCONN         = -113; ///< Transport endpoint is already connected
  AVERROR_ENOTCONN        = -126; ///< Transport endpoint is not connected
  AVERROR_ESHUTDOWN       =  -58; ///< Cannot send after transport endpoint shutdown
  AVERROR_ETOOMANYREFS    =  -59; ///< Too many references: cannot splice
  AVERROR_ETIMEDOUT       = -138; ///< Connection timed out
  AVERROR_ECONNREFUSED    = -107; ///< Connection refused
  AVERROR_EHOSTDOWN       =  -64; ///< Host is down
  AVERROR_EHOSTUNREACH    = -110; ///< No route to host
  AVERROR_EALREADY        = -103; ///< Operation already in progress
  AVERROR_EINPROGRESS     = -112; ///< Operation now in progress
  AVERROR_ESTALE          =  -70; ///< Stale NFS file handle
  AVERROR_ECANCELED       = -105; ///< Operation Canceled
  AVERROR_EOWNERDEAD      = -133; ///< Owner died
  AVERROR_ENOTRECOVERABLE =  -44; ///< State not recoverable
{$ENDIF}
{$IFDEF MACOS}
  AVERROR_EAFNOSUPPORT    =  -47; ///< Address family not supported by protocol
  AVERROR_EADDRINUSE      =  -48; ///< Address already in use
  AVERROR_EADDRNOTAVAIL   =  -49; ///< Cannot assign requested address
  AVERROR_ENETDOWN        =  -50; ///< Network is down
  AVERROR_ENETUNREACH     =  -51; ///< Network is unreachable
  AVERROR_ENETRESET       =  -52; ///< Network dropped connection because of reset
  AVERROR_ECONNABORTED    =  -53; ///< Software caused connection abort
  AVERROR_ECONNRESET      =  -54; ///< Connection reset by peer
  AVERROR_ENOBUFS         =  -55; ///< No buffer space available
  AVERROR_EISCONN         =  -56; ///< Transport endpoint is already connected
  AVERROR_ENOTCONN        =  -57; ///< Transport endpoint is not connected
  AVERROR_ESHUTDOWN       =  -58; ///< Cannot send after transport endpoint shutdown
  AVERROR_ETOOMANYREFS    =  -59; ///< Too many references: cannot splice
  AVERROR_ETIMEDOUT       =  -60; ///< Connection timed out
  AVERROR_ECONNREFUSED    =  -61; ///< Connection refused
  AVERROR_EHOSTDOWN       =  -64; ///< Host is down
  AVERROR_EHOSTUNREACH    =  -65; ///< No route to host
  AVERROR_EALREADY        =  -37; ///< Operation already in progress
  AVERROR_EINPROGRESS     =  -36; ///< Operation now in progress
  AVERROR_ESTALE          =  -70; ///< Stale NFS file handle
  AVERROR_ECANCELED       =  -89; ///< Operation Canceled
  AVERROR_EOWNERDEAD      = -105; ///< Owner died
  AVERROR_ENOTRECOVERABLE = -104; ///< State not recoverable
{$ENDIF}
{
  AVERROR_ECHRNG          =  -37; ///< Channel number out of range
  AVERROR_EL2NSYNC        =  -38; ///< Level 2 not synchronized
  AVERROR_EL3HLT          =  -39; ///< Level 3 halted
  AVERROR_EL3RST          =  -40; ///< Level 3 reset
  AVERROR_ELNRNG          =  -41; ///< Link number out of range
  AVERROR_EUNATCH         =  -42; ///< Protocol driver not attached
  AVERROR_ENOCSI          =  -43; ///< No CSI structure available
  AVERROR_EL2HLT          =  -44; ///< Level 2 halted
  AVERROR_EBADE           =  -50; ///< Invalid exchange
  AVERROR_EBADR           =  -51; ///< Invalid request descriptor
  AVERROR_EXFULL          =  -52; ///< Exchange full
  AVERROR_ENOANO          =  -53; ///< No anode
  AVERROR_EBADRQC         =  -54; ///< Invalid request code
  AVERROR_EBADSLT         =  -55; ///< Invalid slot
  AVERROR_EDEADLOCK       =  -56; ///< File locking deadlock error
  AVERROR_EBFONT          =  -57; ///< Bad font file format
  AVERROR_ENONET          =  -64; ///< Machine is not on the network
  AVERROR_ENOPKG          =  -65; ///< Package not installed
  AVERROR_EADV            =  -68; ///< Advertise error
  AVERROR_ESRMNT          =  -69; ///< Srmount error
  AVERROR_ECOMM           =  -70; ///< Communication error on send
  AVERROR_ENOTUNIQ        =  -76; ///< Name not unique on network
  AVERROR_EBADFD          =  -77; ///< File descriptor in bad state
  AVERROR_EREMCHG         =  -78; ///< Remote address changed
  AVERROR_ELIBACC         =  -79; ///< Can not access a needed shared library
  AVERROR_ELIBBAD         =  -80; ///< Accessing a corrupted shared library
  AVERROR_ELIBSCN         =  -81; ///< .lib section in a.out corrupted
  AVERROR_ELIBMAX         =  -82; ///< Attempting to link in too many shared libraries
  AVERROR_ELIBEXEC        =  -83; ///< Cannot exec a shared library directly
  AVERROR_ERESTART        =  -85; ///< Interrupted system call should be restarted
  AVERROR_ESTRPIPE        =  -86; ///< Streams pipe error
  AVERROR_EUCLEAN         = -117; ///< Structure needs cleaning
  AVERROR_ENOTNAM         = -118; ///< Not a XENIX named type file
  AVERROR_ENAVAIL         = -119; ///< No XENIX semaphores available
  AVERROR_EISNAM          = -120; ///< Is a named type file
  AVERROR_EREMOTEIO       = -121; ///< Remote I/O error
  AVERROR_EDQUOT          = -122; ///< Quota exceeded
  AVERROR_ENOMEDIUM       = -123; ///< No medium found
  AVERROR_EMEDIUMTYPE     = -124; ///< Wrong medium type
  AVERROR_ENOKEY          = -126; ///< Required key not available
  AVERROR_EKEYEXPIRED     = -127; ///< Key has expired
  AVERROR_EKEYREVOKED     = -128; ///< Key has been revoked
  AVERROR_EKEYREJECTED    = -129; ///< Key was rejected by service
}

{$IFDEF MSWINDOWS}
  WSABASEERR                 = -10000;
  {$EXTERNALSYM WSABASEERR}
  WSAEINTR                   = WSABASEERR - 4;
  {$EXTERNALSYM WSAEINTR}
  WSAEBADF                   = WSABASEERR - 9;
  {$EXTERNALSYM WSAEBADF}
  WSAEACCES                  = WSABASEERR - 13;
  {$EXTERNALSYM WSAEACCES}
  WSAEFAULT                  = WSABASEERR - 14;
  {$EXTERNALSYM WSAEFAULT}
  WSAEINVAL                  = WSABASEERR - 22;
  {$EXTERNALSYM WSAEINVAL}
  WSAEMFILE                  = WSABASEERR - 24;
  {$EXTERNALSYM WSAEMFILE}
  WSAEWOULDBLOCK             = WSABASEERR - 35;
  {$EXTERNALSYM WSAEWOULDBLOCK}
  WSAEINPROGRESS             = WSABASEERR - 36; (* deprecated on WinSock2 *)
  {$EXTERNALSYM WSAEINPROGRESS}
  WSAEALREADY                = WSABASEERR - 37;
  {$EXTERNALSYM WSAEALREADY}
  WSAENOTSOCK                = WSABASEERR - 38;
  {$EXTERNALSYM WSAENOTSOCK}
  WSAEDESTADDRREQ            = WSABASEERR - 39;
  {$EXTERNALSYM WSAEDESTADDRREQ}
  WSAEMSGSIZE                = WSABASEERR - 40;
  {$EXTERNALSYM WSAEMSGSIZE}
  WSAEPROTOTYPE              = WSABASEERR - 41;
  {$EXTERNALSYM WSAEPROTOTYPE}
  WSAENOPROTOOPT             = WSABASEERR - 42;
  {$EXTERNALSYM WSAENOPROTOOPT}
  WSAEPROTONOSUPPORT         = WSABASEERR - 43;
  {$EXTERNALSYM WSAEPROTONOSUPPORT}
  WSAESOCKTNOSUPPORT         = WSABASEERR - 44;
  {$EXTERNALSYM WSAESOCKTNOSUPPORT}
  WSAEOPNOTSUPP              = WSABASEERR - 45;
  {$EXTERNALSYM WSAEOPNOTSUPP}
  WSAEPFNOSUPPORT            = WSABASEERR - 46;
  {$EXTERNALSYM WSAEPFNOSUPPORT}
  WSAEAFNOSUPPORT            = WSABASEERR - 47;
  {$EXTERNALSYM WSAEAFNOSUPPORT}
  WSAEADDRINUSE              = WSABASEERR - 48;
  {$EXTERNALSYM WSAEADDRINUSE}
  WSAEADDRNOTAVAIL           = WSABASEERR - 49;
  {$EXTERNALSYM WSAEADDRNOTAVAIL}
  WSAENETDOWN                = WSABASEERR - 50;
  {$EXTERNALSYM WSAENETDOWN}
  WSAENETUNREACH             = WSABASEERR - 51;
  {$EXTERNALSYM WSAENETUNREACH}
  WSAENETRESET               = WSABASEERR - 52;
  {$EXTERNALSYM WSAENETRESET}
  WSAECONNABORTED            = WSABASEERR - 53;
  {$EXTERNALSYM WSAECONNABORTED}
  WSAECONNRESET              = WSABASEERR - 54;
  {$EXTERNALSYM WSAECONNRESET}
  WSAENOBUFS                 = WSABASEERR - 55;
  {$EXTERNALSYM WSAENOBUFS}
  WSAEISCONN                 = WSABASEERR - 56;
  {$EXTERNALSYM WSAEISCONN}
  WSAENOTCONN                = WSABASEERR - 57;
  {$EXTERNALSYM WSAENOTCONN}
  WSAESHUTDOWN               = WSABASEERR - 58;
  {$EXTERNALSYM WSAESHUTDOWN}
  WSAETOOMANYREFS            = WSABASEERR - 59;
  {$EXTERNALSYM WSAETOOMANYREFS}
  WSAETIMEDOUT               = WSABASEERR - 60;
  {$EXTERNALSYM WSAETIMEDOUT}
  WSAECONNREFUSED            = WSABASEERR - 61;
  {$EXTERNALSYM WSAECONNREFUSED}
  WSAELOOP                   = WSABASEERR - 62;
  {$EXTERNALSYM WSAELOOP}
  WSAENAMETOOLONG            = WSABASEERR - 63;
  {$EXTERNALSYM WSAENAMETOOLONG}
  WSAEHOSTDOWN               = WSABASEERR - 64;
  {$EXTERNALSYM WSAEHOSTDOWN}
  WSAEHOSTUNREACH            = WSABASEERR - 65;
  {$EXTERNALSYM WSAEHOSTUNREACH}
  WSAENOTEMPTY               = WSABASEERR - 66;
  {$EXTERNALSYM WSAENOTEMPTY}
  WSAEPROCLIM                = WSABASEERR - 67;
  {$EXTERNALSYM WSAEPROCLIM}
  WSAEUSERS                  = WSABASEERR - 68;
  {$EXTERNALSYM WSAEUSERS}
  WSAEDQUOT                  = WSABASEERR - 69;
  {$EXTERNALSYM WSAEDQUOT}
  WSAESTALE                  = WSABASEERR - 70;
  {$EXTERNALSYM WSAESTALE}
  WSAEREMOTE                 = WSABASEERR - 71;
  {$EXTERNALSYM WSAEREMOTE}
  WSAEDISCON                 = WSABASEERR - 101;
  {$EXTERNALSYM WSAEDISCON}
  WSASYSNOTREADY             = WSABASEERR - 91;
  {$EXTERNALSYM WSASYSNOTREADY}
  WSAVERNOTSUPPORTED         = WSABASEERR - 92;
  {$EXTERNALSYM WSAVERNOTSUPPORTED}
  WSANOTINITIALISED          = WSABASEERR - 93;
  {$EXTERNALSYM WSANOTINITIALISED}
  WSAHOST_NOT_FOUND          = WSABASEERR - 1001;
  {$EXTERNALSYM WSAHOST_NOT_FOUND}
  WSATRY_AGAIN               = WSABASEERR - 1002;
  {$EXTERNALSYM WSATRY_AGAIN}
  WSANO_RECOVERY             = WSABASEERR - 1003;
  {$EXTERNALSYM WSANO_RECOVERY}
  WSANO_DATA                 = WSABASEERR - 1004;
  {$EXTERNALSYM WSANO_DATA}
  (* WinSock2 specific error codes *)
  WSAENOMORE                 = WSABASEERR - 102;
  {$EXTERNALSYM WSAENOMORE}
  WSAECANCELLED              = WSABASEERR - 103;
  {$EXTERNALSYM WSAECANCELLED}
  WSAEINVALIDPROCTABLE       = WSABASEERR - 104;
  {$EXTERNALSYM WSAEINVALIDPROCTABLE}
  WSAEINVALIDPROVIDER        = WSABASEERR - 105;
  {$EXTERNALSYM WSAEINVALIDPROVIDER}
  WSAEPROVIDERFAILEDINIT     = WSABASEERR - 106;
  {$EXTERNALSYM WSAEPROVIDERFAILEDINIT}
  WSASYSCALLFAILURE          = WSABASEERR - 107;
  {$EXTERNALSYM WSASYSCALLFAILURE}
  WSASERVICE_NOT_FOUND       = WSABASEERR - 108;
  {$EXTERNALSYM WSASERVICE_NOT_FOUND}
  WSATYPE_NOT_FOUND          = WSABASEERR - 109;
  {$EXTERNALSYM WSATYPE_NOT_FOUND}
  WSA_E_NO_MORE              = WSABASEERR - 110;
  {$EXTERNALSYM WSA_E_NO_MORE}
  WSA_E_CANCELLED            = WSABASEERR - 111;
  {$EXTERNALSYM WSA_E_CANCELLED}
  WSAEREFUSED                = WSABASEERR - 112;
  {$EXTERNALSYM WSAEREFUSED}
  (* WS QualityofService errors *)
  WSA_QOS_RECEIVERS          = WSABASEERR - 1005;
  {$EXTERNALSYM WSA_QOS_RECEIVERS}
  WSA_QOS_SENDERS            = WSABASEERR - 1006;
  {$EXTERNALSYM WSA_QOS_SENDERS}
  WSA_QOS_NO_SENDERS         = WSABASEERR - 1007;
  {$EXTERNALSYM WSA_QOS_NO_SENDERS}
  WSA_QOS_NO_RECEIVERS       = WSABASEERR - 1008;
  {$EXTERNALSYM WSA_QOS_NO_RECEIVERS}
  WSA_QOS_REQUEST_CONFIRMED  = WSABASEERR - 1009;
  {$EXTERNALSYM WSA_QOS_REQUEST_CONFIRMED}
  WSA_QOS_ADMISSION_FAILURE  = WSABASEERR - 1010;
  {$EXTERNALSYM WSA_QOS_ADMISSION_FAILURE}
  WSA_QOS_POLICY_FAILURE     = WSABASEERR - 1011;
  {$EXTERNALSYM WSA_QOS_POLICY_FAILURE}
  WSA_QOS_BAD_STYLE          = WSABASEERR - 1012;
  {$EXTERNALSYM WSA_QOS_BAD_STYLE}
  WSA_QOS_BAD_OBJECT         = WSABASEERR - 1013;
  {$EXTERNALSYM WSA_QOS_BAD_OBJECT}
  WSA_QOS_TRAFFIC_CTRL_ERROR = WSABASEERR - 1014;
  {$EXTERNALSYM WSA_QOS_TRAFFIC_CTRL_ERROR}
  WSA_QOS_GENERIC_ERROR      = WSABASEERR - 1015;
  {$EXTERNALSYM WSA_QOS_GENERIC_ERROR}
  WSA_QOS_ESERVICETYPE       = WSABASEERR - 1016;
  {$EXTERNALSYM WSA_QOS_ESERVICETYPE}
  WSA_QOS_EFLOWSPEC          = WSABASEERR - 1017;
  {$EXTERNALSYM WSA_QOS_EFLOWSPEC}
  WSA_QOS_EPROVSPECBUF       = WSABASEERR - 1018;
  {$EXTERNALSYM WSA_QOS_EPROVSPECBUF}
  WSA_QOS_EFILTERSTYLE       = WSABASEERR - 1019;
  {$EXTERNALSYM WSA_QOS_EFILTERSTYLE}
  WSA_QOS_EFILTERTYPE        = WSABASEERR - 1020;
  {$EXTERNALSYM WSA_QOS_EFILTERTYPE}
  WSA_QOS_EFILTERCOUNT       = WSABASEERR - 1021;
  {$EXTERNALSYM WSA_QOS_EFILTERCOUNT}
  WSA_QOS_EOBJLENGTH         = WSABASEERR - 1022;
  {$EXTERNALSYM WSA_QOS_EOBJLENGTH}
  WSA_QOS_EFLOWCOUNT         = WSABASEERR - 1023;
  {$EXTERNALSYM WSA_QOS_EFLOWCOUNT}
  WSA_QOS_EUNKOWNPSOBJ       = WSABASEERR - 1024;
  {$EXTERNALSYM WSA_QOS_EUNKOWNPSOBJ}
  WSA_QOS_EPOLICYOBJ         = WSABASEERR - 1025;
  {$EXTERNALSYM WSA_QOS_EPOLICYOBJ}
  WSA_QOS_EFLOWDESC          = WSABASEERR - 1026;
  {$EXTERNALSYM WSA_QOS_EFLOWDESC}
  WSA_QOS_EPSFLOWSPEC        = WSABASEERR - 1027;
  {$EXTERNALSYM WSA_QOS_EPSFLOWSPEC}
  WSA_QOS_EPSFILTERSPEC      = WSABASEERR - 1028;
  {$EXTERNALSYM WSA_QOS_EPSFILTERSPEC}
  WSA_QOS_ESDMODEOBJ         = WSABASEERR - 1029;
  {$EXTERNALSYM WSA_QOS_ESDMODEOBJ}
  WSA_QOS_ESHAPERATEOBJ      = WSABASEERR - 1030;
  {$EXTERNALSYM WSA_QOS_ESHAPERATEOBJ}
  WSA_QOS_RESERVED_PETYPE    = WSABASEERR - 1031;
  {$EXTERNALSYM WSA_QOS_RESERVED_PETYPE}
{$ENDIF}

(**
 * Put a description of the AVERROR code errnum in errbuf.
 * In case of failure the global variable errno is set to indicate the
 * error. Even in case of failure av_strerror() will print a generic
 * error message indicating the errnum provided to errbuf.
 *
 * @param errnum      error code to describe
 * @param errbuf      buffer to which description is written
 * @param errbuf_size the size in bytes of errbuf
 * @return 0 on success, a negative value if a description for errnum
 * cannot be found
 *)
function av_strerror(errnum: Integer; errbuf: PAnsiChar; errbuf_size: Size_t): Integer; cdecl; external AVUTIL_LIBNAME name _PU + 'av_strerror';

(**
 * Fill the provided buffer with a string containing an error string
 * corresponding to the AVERROR code errnum.
 *
 * @param errbuf         a buffer
 * @param errbuf_size    size in bytes of errbuf
 * @param errnum         error code to describe
 * @return the buffer in input, filled with the error description
 * @see av_strerror()
 *)
function av_make_error_string(errbuf: PAnsiChar; errbuf_size: Size_t; errnum: Integer): PAnsiChar; {$IFDEF USE_INLINE}inline;{$ENDIF}
{
    av_strerror(errnum, errbuf, errbuf_size);
    return errbuf;
}

(**
 * Convenience macro, the return value should be used only directly in
 * function arguments but never stand-alone.
 *)
//#define av_err2str(errnum) \
//    av_make_error_string((char[AV_ERROR_MAX_STRING_SIZE]){0}, AV_ERROR_MAX_STRING_SIZE, errnum)
function av_err2str(errnum: Integer): PAnsiChar;

(**
 * @}
 *)

type
  TErrorItem = record
    err: Integer;
    msg: string;
  end;
const
  CErrorList: array[0..111{$IFDEF MSWINDOWS}+62{$ENDIF}] of TErrorItem = (
{$IFDEF MSWINDOWS}
    (err: WSAEINTR;                   msg: 'Interrupted function call'),
    (err: WSAEBADF;                   msg: 'Bad file number'),
    (err: WSAEACCES;                  msg: 'Permission denied'),
    (err: WSAEFAULT;                  msg: 'Bad address'),
    (err: WSAEINVAL;                  msg: 'Invalid argument / Invalid data found when processing input'),
    (err: WSAEMFILE;                  msg: 'Too many open files'),
    (err: WSAENAMETOOLONG;            msg: 'File name too long'),
    (err: WSAENOTEMPTY;               msg: 'Directory not empty'),
    (err: WSAELOOP;                   msg: 'Too many symbolic links encountered'),
    (err: WSAEREMOTE;                 msg: 'Too many levels of remote in path'),
    (err: WSAEUSERS;                  msg: 'Too many users'),
    (err: WSAENOTSOCK;                msg: 'Socket operation on non-socket'),
    (err: WSAEDESTADDRREQ;            msg: 'Destination address required'),
    (err: WSAEMSGSIZE;                msg: 'Message too long'),
    (err: WSAEPROTOTYPE;              msg: 'Protocol wrong type for socket'),
    (err: WSAENOPROTOOPT;             msg: 'Protocol not available'),
    (err: WSAEPROTONOSUPPORT;         msg: 'Protocol not supported'),
    (err: WSAESOCKTNOSUPPORT;         msg: 'Socket type not supported'),
    (err: WSAEOPNOTSUPP;              msg: 'Operation not supported on transport endpoint'),
    (err: WSAEPFNOSUPPORT;            msg: 'Protocol family not supported'),
    (err: WSAEAFNOSUPPORT;            msg: 'Address family not supported by protocol'),
    (err: WSAEADDRINUSE;              msg: 'Address already in use'),
    (err: WSAEADDRNOTAVAIL;           msg: 'Cannot assign requested address'),
    (err: WSAENETDOWN;                msg: 'Network is down'),
    (err: WSAENETUNREACH;             msg: 'Network is unreachable'),
    (err: WSAENETRESET;               msg: 'Network dropped connection because of reset'),
    (err: WSAECONNABORTED;            msg: 'Software caused connection abort'),
    (err: WSAECONNRESET;              msg: 'Connection reset by peer'),
    (err: WSAENOBUFS;                 msg: 'No buffer space available'),
    (err: WSAEISCONN;                 msg: 'Transport endpoint is already connected'),
    (err: WSAENOTCONN;                msg: 'Transport endpoint is not connected'),
    (err: WSAESHUTDOWN;               msg: 'Cannot send after transport endpoint shutdown'),
    (err: WSAETOOMANYREFS;            msg: 'Too many references: cannot splice'),
    (err: WSAETIMEDOUT;               msg: 'Connection timed out'),
    (err: WSAECONNREFUSED;            msg: 'Connection refused'),
    (err: WSAEHOSTDOWN;               msg: 'Host is down'),
    (err: WSAEHOSTUNREACH;            msg: 'No route to host'),
    (err: WSAEALREADY;                msg: 'Operation already in progress'),
    (err: WSAEINPROGRESS;             msg: 'Operation now in progress'),
    (err: WSAESTALE;                  msg: 'Stale NFS file handle'),
    (err: WSAEDQUOT;                  msg: 'Quota exceeded'),
    (err: WSAEWOULDBLOCK;             msg: 'WSAEWOULDBLOCK'),
    (err: WSAEPROCLIM;                msg: 'WSAEPROCLIM'),
    (err: WSAEDISCON;                 msg: 'WSAEDISCON'),
    (err: WSASYSNOTREADY;             msg: 'WSASYSNOTREADY'),
    (err: WSAVERNOTSUPPORTED;         msg: 'WSAVERNOTSUPPORTED'),
    (err: WSANOTINITIALISED;          msg: 'WSANOTINITIALISED'),
    (err: WSAHOST_NOT_FOUND;          msg: 'WSAHOST_NOT_FOUND'),
    (err: WSATRY_AGAIN;               msg: 'WSATRY_AGAIN'),
    (err: WSANO_RECOVERY;             msg: 'WSANO_RECOVERY'),
    (err: WSANO_DATA;                 msg: 'WSANO_DATA'),
    (err: WSAENOMORE;                 msg: 'WSAENOMORE'),
    (err: WSAECANCELLED;              msg: 'WSAECANCELLED'),
    (err: WSAEINVALIDPROCTABLE;       msg: 'WSAEINVALIDPROCTABLE'),
    (err: WSAEINVALIDPROVIDER;        msg: 'WSAEINVALIDPROVIDER'),
    (err: WSAEPROVIDERFAILEDINIT;     msg: 'WSAEPROVIDERFAILEDINIT'),
    (err: WSASYSCALLFAILURE;          msg: 'WSASYSCALLFAILURE'),
    (err: WSASERVICE_NOT_FOUND;       msg: 'WSASERVICE_NOT_FOUND'),
    (err: WSATYPE_NOT_FOUND;          msg: 'WSATYPE_NOT_FOUND'),
    (err: WSA_E_NO_MORE;              msg: 'WSA_E_NO_MORE'),
    (err: WSA_E_CANCELLED;            msg: 'WSA_E_CANCELLED'),
    (err: WSAEREFUSED;                msg: 'WSAEREFUSED'),
{$ENDIF}
    // av_strerror()
    (err: AVERROR_BSF_NOT_FOUND;      msg: 'Bitstream filter not found'),
    (err: AVERROR_BUG;                msg: 'Internal bug, should not have happened'),
    (err: AVERROR_BUG2;               msg: 'Internal bug, should not have happened'),
    (err: AVERROR_BUFFER_TOO_SMALL;   msg: 'Buffer too small'),
    (err: AVERROR_DECODER_NOT_FOUND;  msg: 'Decoder not found'),
    (err: AVERROR_DEMUXER_NOT_FOUND;  msg: 'Demuxer not found'),
    (err: AVERROR_ENCODER_NOT_FOUND;  msg: 'Encoder not found'),
    (err: AVERROR_EOF;                msg: 'End of file'),
    (err: AVERROR_EXIT;               msg: 'Immediate exit requested'),
    (err: AVERROR_EXTERNAL;           msg: 'Generic error in an external library'),
    (err: AVERROR_FILTER_NOT_FOUND;   msg: 'Filter not found'),
    (err: AVERROR_INVALIDDATA;        msg: 'Invalid data found when processing input'),
    (err: AVERROR_MUXER_NOT_FOUND;    msg: 'Muxer not found'),
    (err: AVERROR_OPTION_NOT_FOUND;   msg: 'Option not found'),
    (err: AVERROR_PATCHWELCOME;       msg: 'Not yet implemented in FFmpeg, patches welcome'),
    (err: AVERROR_PROTOCOL_NOT_FOUND; msg: 'Protocol not found'),
    (err: AVERROR_STREAM_NOT_FOUND;   msg: 'Stream not found'),
    (err: AVERROR_UNKNOWN;            msg: 'Unknown error occurred'),
    (err: AVERROR_EXPERIMENTAL;       msg: 'Requested feature is flagged experimental. Set strict_std_compliance if you really want to use it.'),
    (err: AVERROR_INPUT_CHANGED;      msg: 'Input changed between calls. Reconfiguration is required. (can be OR-ed with AVERROR_OUTPUT_CHANGED)'),
    (err: AVERROR_OUTPUT_CHANGED;     msg: 'Output changed between calls. Reconfiguration is required. (can be OR-ed with AVERROR_INPUT_CHANGED)'),
    (err: AVERROR_HTTP_BAD_REQUEST;   msg: 'HTTP or RTSP error: bad request(400)'),
    (err: AVERROR_HTTP_UNAUTHORIZED;  msg: 'HTTP or RTSP error: unauthorized(401)'),
    (err: AVERROR_HTTP_FORBIDDEN;     msg: 'HTTP or RTSP error: forbidden(403)'),
    (err: AVERROR_HTTP_NOT_FOUND;     msg: 'HTTP or RTSP error: not found(404)'),
    (err: AVERROR_HTTP_OTHER_4XX;     msg: 'HTTP or RTSP error: other error(4xx)'),
    (err: AVERROR_HTTP_SERVER_ERROR;  msg: 'HTTP or RTSP error: server error(5xx)'),

    //(err: AVERROR_EPERM;              msg: 'Operation not permitted'),
    (err: AVERROR_ENOENT;             msg: 'No such file or directory'),
    (err: AVERROR_ESRCH;              msg: 'No such process'),
    (err: AVERROR_EINTR;              msg: 'Interrupted function call'),
    (err: AVERROR_EIO;                msg: 'I/O error'),
    (err: AVERROR_ENXIO;              msg: 'No such device or address'),
    (err: AVERROR_E2BIG;              msg: 'Argument list too long'),
    (err: AVERROR_ENOEXEC;            msg: 'Exec format error'),
    (err: AVERROR_EBADF;              msg: 'Bad file number'),
    (err: AVERROR_ECHILD;             msg: 'No child processes'),
    (err: AVERROR_EAGAIN;             msg: 'Resource temporarily unavailable / Try again'),
    (err: AVERROR_ENOMEM;             msg: 'Not enough space / Out of memory'),
    (err: AVERROR_EACCES;             msg: 'Permission denied'),
    (err: AVERROR_EFAULT;             msg: 'Bad address'),
{$IFDEF MSWINDOWS}
    (err: AVERROR_ENOTBLK;            msg: 'Unknown error'),
{$ELSE}
    (err: AVERROR_ENOTBLK;            msg: 'Block device required'),
{$ENDIF}
    (err: AVERROR_EBUSY;              msg: 'Device or resource busy'),
    (err: AVERROR_EEXIST;             msg: 'File exists'),
    (err: AVERROR_EXDEV;              msg: 'Cross-device link'),
    (err: AVERROR_ENODEV;             msg: 'No such device'),
    (err: AVERROR_ENOTDIR;            msg: 'Not a directory'),
    (err: AVERROR_EISDIR;             msg: 'Is a directory'),
    (err: AVERROR_EINVAL;             msg: 'Invalid argument / Invalid data found when processing input'),
    (err: AVERROR_ENFILE;             msg: 'Too many open files in system / File table overflow'),
    (err: AVERROR_EMFILE;             msg: 'Too many open files'),
    (err: AVERROR_ENOTTY;             msg: 'Inappropriate I/O control operation / Not a typewriter'),
{$IFDEF MSWINDOWS}
    (err: AVERROR_ETXTBSY;            msg: 'Unknown error'),
{$ELSE}
    (err: AVERROR_ETXTBSY;            msg: 'Text file busy'),
{$ENDIF}
    (err: AVERROR_EFBIG;              msg: 'File too large'),
    (err: AVERROR_ENOSPC;             msg: 'No space left on device'),
    (err: AVERROR_ESPIPE;             msg: 'Illegal seek'),
    (err: AVERROR_EROFS;              msg: 'Read-only file system'),
    (err: AVERROR_EMLINK;             msg: 'Too many links'),
    (err: AVERROR_EPIPE;              msg: 'Broken pipe'),
    (err: AVERROR_EDOM;               msg: 'Math argument out of domain of func'),
    (err: AVERROR_ERANGE;             msg: 'Math result not representable'),
    (err: AVERROR_EDEADLK;            msg: 'Resource deadlock avoided'),
    (err: AVERROR_ENAMETOOLONG;       msg: 'File name too long'),
    (err: AVERROR_ENOLCK;             msg: 'No locks available'),
    (err: AVERROR_ENOSYS;             msg: 'Function not implemented'),
    (err: AVERROR_ENOTEMPTY;          msg: 'Directory not empty'),
    (err: AVERROR_ELOOP;              msg: 'Too many symbolic links encountered'),
{$IFDEF MSWINDOWS}
    (err: AVERROR_ENOMSG;             msg: 'Unknown error'),
    (err: AVERROR_EIDRM;              msg: 'Unknown error'),
    (err: AVERROR_ENOSTR;             msg: 'Unknown error'),
    (err: AVERROR_ENODATA;            msg: 'Unknown error'),
    (err: AVERROR_ETIME;              msg: 'Unknown error'),
    (err: AVERROR_ENOSR;              msg: 'Unknown error'),
{$ELSE}
    (err: AVERROR_ENOMSG;             msg: 'No message of desired type'),
    (err: AVERROR_EIDRM;              msg: 'Identifier removed'),
    (err: AVERROR_ENOSTR;             msg: 'Device not a stream'),
    (err: AVERROR_ENODATA;            msg: 'No data available'),
    (err: AVERROR_ETIME;              msg: 'Timer expired'),
    (err: AVERROR_ENOSR;              msg: 'Out of streams resources'),
{$ENDIF}
{$IFDEF MSWINDOWS}
    (err: AVERROR_EREMOTE;            msg: 'Unknown error'),
    (err: AVERROR_ENOLINK;            msg: 'Unknown error'),
{$ELSE}
    (err: AVERROR_EREMOTE;            msg: 'Too many levels of remote in path'),
    (err: AVERROR_ENOLINK;            msg: 'Link has been severed'),
{$ENDIF}
    (err: AVERROR_EPROTO;             msg: 'Protocol error'),
{$IFDEF MSWINDOWS}
    (err: AVERROR_EMULTIHOP;          msg: 'Unknown error'),
    (err: AVERROR_EBADMSG;            msg: 'Unknown error'),
{$ELSE}
    (err: AVERROR_EMULTIHOP;          msg: 'Multihop attempted'),
    (err: AVERROR_EBADMSG;            msg: 'Not a data message'),
{$ENDIF}
    (err: AVERROR_EOVERFLOW;          msg: 'Value too large for defined data type'),
    (err: AVERROR_EILSEQ;             msg: 'Illegal byte sequence'),
{$IFDEF MSWINDOWS}
    (err: AVERROR_EUSERS;             msg: 'Unknown error'),
{$ELSE}
    (err: AVERROR_EUSERS;             msg: 'Too many users'),
{$ENDIF}
    (err: AVERROR_ENOTSOCK;           msg: 'Socket operation on non-socket'),
    (err: AVERROR_EDESTADDRREQ;       msg: 'Destination address required'),
    (err: AVERROR_EMSGSIZE;           msg: 'Message too long'),
    (err: AVERROR_EPROTOTYPE;         msg: 'Protocol wrong type for socket'),
    (err: AVERROR_ENOPROTOOPT;        msg: 'Protocol not available'),
    (err: AVERROR_EPROTONOSUPPORT;    msg: 'Protocol not supported'),
{$IFDEF MSWINDOWS}
    (err: AVERROR_ESOCKTNOSUPPORT;    msg: 'Unknown error'),
{$ELSE}
    (err: AVERROR_ESOCKTNOSUPPORT;    msg: 'Socket type not supported'),
{$ENDIF}
    (err: AVERROR_EOPNOTSUPP;         msg: 'Operation not supported on transport endpoint'),
{$IFDEF MSWINDOWS}
    (err: AVERROR_EPFNOSUPPORT;       msg: 'Unknown error'),
{$ELSE}
    (err: AVERROR_EPFNOSUPPORT;       msg: 'Protocol family not supported'),
{$ENDIF}
    (err: AVERROR_EAFNOSUPPORT;       msg: 'Address family not supported by protocol'),
    (err: AVERROR_EADDRINUSE;         msg: 'Address already in use'),
    (err: AVERROR_EADDRNOTAVAIL;      msg: 'Cannot assign requested address'),
    (err: AVERROR_ENETDOWN;           msg: 'Network is down'),
    (err: AVERROR_ENETUNREACH;        msg: 'Network is unreachable'),
    (err: AVERROR_ENETRESET;          msg: 'Network dropped connection because of reset'),
    (err: AVERROR_ECONNABORTED;       msg: 'Software caused connection abort'),
    (err: AVERROR_ECONNRESET;         msg: 'Connection reset by peer'),
    (err: AVERROR_ENOBUFS;            msg: 'No buffer space available'),
    (err: AVERROR_EISCONN;            msg: 'Transport endpoint is already connected'),
    (err: AVERROR_ENOTCONN;           msg: 'Transport endpoint is not connected'),
{$IFDEF MSWINDOWS}
    (err: AVERROR_ESHUTDOWN;          msg: 'Unknown error'),
    (err: AVERROR_ETOOMANYREFS;       msg: 'Unknown error'),
{$ELSE}
    (err: AVERROR_ESHUTDOWN;          msg: 'Cannot send after transport endpoint shutdown'),
    (err: AVERROR_ETOOMANYREFS;       msg: 'Too many references: cannot splice'),
{$ENDIF}
    (err: AVERROR_ETIMEDOUT;          msg: 'Connection timed out'),
    (err: AVERROR_ECONNREFUSED;       msg: 'Connection refused'),
{$IFDEF MSWINDOWS}
    (err: AVERROR_EHOSTDOWN;          msg: 'Unknown error'),
{$ELSE}
    (err: AVERROR_EHOSTDOWN;          msg: 'Host is down'),
{$ENDIF}
    (err: AVERROR_EHOSTUNREACH;       msg: 'No route to host'),
    (err: AVERROR_EALREADY;           msg: 'Operation already in progress'),
    (err: AVERROR_EINPROGRESS;        msg: 'Operation now in progress'),
{$IFDEF MSWINDOWS}
    (err: AVERROR_ESTALE;             msg: 'Unknown error'),
{$ELSE}
    (err: AVERROR_ESTALE;             msg: 'Stale NFS file handle'),
{$ENDIF}
    (err: AVERROR_ECANCELED;          msg: 'Operation Canceled'),
    (err: AVERROR_EOWNERDEAD;         msg: 'Owner died'),
    (err: AVERROR_ENOTRECOVERABLE;    msg: 'State not recoverable')
{
    (err: AVERROR_ECHRNG;             msg: 'Channel number out of range'),
    (err: AVERROR_EL2NSYNC;           msg: 'Level 2 not synchronized'),
    (err: AVERROR_EL3HLT;             msg: 'Level 3 halted'),
    (err: AVERROR_EL3RST;             msg: 'Level 3 reset'),
    (err: AVERROR_ELNRNG;             msg: 'Link number out of range'),
    (err: AVERROR_EUNATCH;            msg: 'Protocol driver not attached'),
    (err: AVERROR_ENOCSI;             msg: 'No CSI structure available'),
    (err: AVERROR_EL2HLT;             msg: 'Level 2 halted'),
    (err: AVERROR_EBADE;              msg: 'Invalid exchange'),
    (err: AVERROR_EBADR;              msg: 'Invalid request descriptor'),
    (err: AVERROR_EXFULL;             msg: 'Exchange full'),
    (err: AVERROR_ENOANO;             msg: 'No anode'),
    (err: AVERROR_EBADRQC;            msg: 'Invalid request code'),
    (err: AVERROR_EBADSLT;            msg: 'Invalid slot'),
    (err: AVERROR_EBFONT;             msg: 'Bad font file format'),
    (err: AVERROR_ENONET;             msg: 'Machine is not on the network'),
    (err: AVERROR_ENOPKG;             msg: 'Package not installed'),
    (err: AVERROR_EADV;               msg: 'Advertise error'),
    (err: AVERROR_ESRMNT;             msg: 'Srmount error'),
    (err: AVERROR_ECOMM;              msg: 'Communication error on send'),
    (err: AVERROR_EDOTDOT;            msg: 'RFS specific error'),
    (err: AVERROR_ENOTUNIQ;           msg: 'Name not unique on network'),
    (err: AVERROR_EBADFD;             msg: 'File descriptor in bad state'),
    (err: AVERROR_EREMCHG;            msg: 'Remote address changed'),
    (err: AVERROR_ELIBACC;            msg: 'Can not access a needed shared library'),
    (err: AVERROR_ELIBBAD;            msg: 'Accessing a corrupted shared library'),
    (err: AVERROR_ELIBSCN;            msg: '.lib section in a.out corrupted'),
    (err: AVERROR_ELIBMAX;            msg: 'Attempting to link in too many shared libraries'),
    (err: AVERROR_ELIBEXEC;           msg: 'Cannot exec a shared library directly'),
    (err: AVERROR_ERESTART;           msg: 'Interrupted system call should be restarted'),
    (err: AVERROR_ESTRPIPE;           msg: 'Streams pipe error'),
    (err: AVERROR_EUCLEAN;            msg: 'Structure needs cleaning'),
    (err: AVERROR_ENOTNAM;            msg: 'Not a XENIX named type file'),
    (err: AVERROR_ENAVAIL;            msg: 'No XENIX semaphores available'),
    (err: AVERROR_EISNAM;             msg: 'Is a named type file'),
    (err: AVERROR_EREMOTEIO;          msg: 'Remote I/O error'),
    (err: AVERROR_EDQUOT;             msg: 'Quota exceeded'),
    (err: AVERROR_ENOMEDIUM;          msg: 'No medium found'),
    (err: AVERROR_EMEDIUMTYPE;        msg: 'Wrong medium type'),
    (err: AVERROR_ENOKEY;             msg: 'Required key not available'),
    (err: AVERROR_EKEYEXPIRED;        msg: 'Key has expired'),
    (err: AVERROR_EKEYREVOKED;        msg: 'Key has been revoked'),
    (err: AVERROR_EKEYREJECTED;       msg: 'Key was rejected by service'),
}
  );

implementation


function av_make_error_string(errbuf: PAnsiChar; errbuf_size: Size_t; errnum: Integer): PAnsiChar;
begin
  av_strerror(errnum, errbuf, errbuf_size);
  Result := errbuf;
end;

var
 errbuf: array[0..AV_ERROR_MAX_STRING_SIZE-1] of AnsiChar;

function av_err2str(errnum: Integer): PAnsiChar;
begin
  FillChar(errbuf[0], SizeOf(errbuf), 0);
  Result := av_make_error_string(PAnsiChar(@errbuf[0]), AV_ERROR_MAX_STRING_SIZE, errnum);
end;

end.
