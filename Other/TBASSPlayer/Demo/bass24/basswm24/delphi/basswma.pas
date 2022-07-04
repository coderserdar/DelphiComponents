{
  BASSWMA 2.4 Delphi unit
  Copyright (c) 2002-2010 Un4seen Developments Ltd.

  See the BASSWMA.CHM file for more detailed documentation
}

unit BassWMA;

interface

uses Windows, Bass;

const
  // Additional error codes returned by BASS_ErrorGetCode
  BASS_ERROR_WMA_LICENSE     = 1000; // the file is protected
  BASS_ERROR_WMA             = 1001; // Windows Media (9 or above) is not installed
  BASS_ERROR_WMA_WM9         = BASS_ERROR_WMA;
  BASS_ERROR_WMA_DENIED      = 1002; // access denied (user/pass is invalid)
  BASS_ERROR_WMA_INDIVIDUAL  = 1004; // individualization is needed
  BASS_ERROR_WMA_PUBINIT     = 1005; // publishing point initialization problem

  // Additional BASS_SetConfig options
  BASS_CONFIG_WMA_PRECHECK   = $10100;
  BASS_CONFIG_WMA_PREBUF     = $10101;
  BASS_CONFIG_WMA_BASSFILE   = $10103;
  BASS_CONFIG_WMA_NETSEEK    = $10104;
  BASS_CONFIG_WMA_VIDEO      = $10105;

  // additional WMA sync types
  BASS_SYNC_WMA_CHANGE       = $10100;
  BASS_SYNC_WMA_META         = $10101;

  // additional BASS_StreamGetFilePosition WMA mode
  BASS_FILEPOS_WMA_BUFFER    = 1000; // internet buffering progress (0-100%)

  // Additional flags for use with BASS_WMA_EncodeOpen/File/Network/Publish
  BASS_WMA_ENCODE_STANDARD   = $2000;  // standard WMA
  BASS_WMA_ENCODE_PRO        = $4000;  // WMA Pro
  BASS_WMA_ENCODE_24BIT      = $8000;  // 24-bit
  BASS_WMA_ENCODE_PCM        = $10000; // uncompressed PCM
  BASS_WMA_ENCODE_SCRIPT     = $20000; // set script (mid-stream tags) in the WMA encoding

  // Additional flag for use with BASS_WMA_EncodeGetRates
  BASS_WMA_ENCODE_RATES_VBR  = $10000; // get available VBR quality settings

  // WMENCODEPROC "type" values
  BASS_WMA_ENCODE_HEAD       = 0;
  BASS_WMA_ENCODE_DATA       = 1;
  BASS_WMA_ENCODE_DONE       = 2;

  // BASS_WMA_EncodeSetTag "form" values
  BASS_WMA_TAG_ANSI          = 0;
  BASS_WMA_TAG_UNICODE       = 1;
  BASS_WMA_TAG_UTF8          = 2;
  BASS_WMA_TAG_BINARY        = $100; // FLAG: binary tag (HIWORD=length)

  // BASS_CHANNELINFO type
  BASS_CTYPE_STREAM_WMA      = $10300;
  BASS_CTYPE_STREAM_WMA_MP3  = $10301;

  // Additional BASS_ChannelGetTags type
  BASS_TAG_WMA               = 8; // WMA header tags : series of null-terminated UTF-8 strings
  BASS_TAG_WMA_META          = 11; // WMA mid-stream tag : UTF-8 string
  BASS_TAG_WMA_CODEC         = 12; // WMA codec


type
  HWMENCODE = DWORD;		// WMA encoding handle

  CLIENTCONNECTPROC = procedure(handle:HWMENCODE; connect:BOOL; ip:PAnsiChar; user:Pointer); stdcall;
  {
    Client connection notification callback function.
    handle : The encoder
    connect: TRUE=client is connecting, FALSE=disconnecting
    ip     : The client's IP (xxx.xxx.xxx.xxx:port)
    user   : The 'user' parameter value given when calling BASS_WMA_EncodeSetNotify
  }

  WMENCODEPROC = procedure(handle:HWMENCODE; dtype:DWORD; buffer:Pointer; length:DWORD; user:Pointer); stdcall;
  {
    Encoder callback function.
    handle : The encoder handle
    dtype  : The type of data, one of BASS_WMA_ENCODE_xxx values
    buffer : The encoded data
    length : Length of the data
    user   : The 'user' parameter value given when calling BASS_WMA_EncodeOpen
  }


const
  basswmadll = 'basswma.dll';

function BASS_WMA_StreamCreateFile(mem:BOOL; fl:pointer; offset,length:QWORD; flags:DWORD): HSTREAM; stdcall; external basswmadll;
function BASS_WMA_StreamCreateFileAuth(mem:BOOL; fl:pointer; offset,length:QWORD; flags:DWORD; user,pass:PChar): HSTREAM; stdcall; external basswmadll;
function BASS_WMA_StreamCreateFileUser(system,flags:DWORD; var procs:BASS_FILEPROCS; user:Pointer): HSTREAM; stdcall; external basswmadll;

function BASS_WMA_GetTags(fname:PChar; flags:DWORD): PAnsiChar; stdcall; external basswmadll;

function BASS_WMA_EncodeGetRates(freq,chans,flags:DWORD): PDWORD; stdcall; external basswmadll;
function BASS_WMA_EncodeOpen(freq,chans,flags,bitrate:DWORD; proc:WMENCODEPROC; user:Pointer): HWMENCODE; stdcall; external basswmadll;
function BASS_WMA_EncodeOpenFile(freq,chans,flags,bitrate:DWORD; fname:PChar): HWMENCODE; stdcall; external basswmadll;
function BASS_WMA_EncodeOpenNetwork(freq,chans,flags,bitrate,port,clients:DWORD): HWMENCODE; stdcall; external basswmadll;
function BASS_WMA_EncodeOpenNetworkMulti(freq,chans,flags:DWORD; bitrates:PDWORD; port,clients:DWORD): HWMENCODE; stdcall; external basswmadll;
function BASS_WMA_EncodeOpenPublish(freq,chans,flags,bitrate:DWORD; url,user,pass:PChar): HWMENCODE; stdcall; external basswmadll;
function BASS_WMA_EncodeOpenPublishMulti(freq,chans,flags:DWORD; bitrates:PDWORD; url,user,pass:PChar): HWMENCODE; stdcall; external basswmadll;
function BASS_WMA_EncodeGetPort(handle:HWMENCODE): DWORD; stdcall; external basswmadll;
function BASS_WMA_EncodeSetNotify(handle:HWMENCODE; proc:CLIENTCONNECTPROC; user:Pointer): BOOL; stdcall; external basswmadll;
function BASS_WMA_EncodeGetClients(handle:HWMENCODE): DWORD; stdcall; external basswmadll;
function BASS_WMA_EncodeSetTag(handle:HWMENCODE; tag,text:PChar; form:DWORD): BOOL; stdcall; external basswmadll;
function BASS_WMA_EncodeWrite(handle:HWMENCODE; buffer:Pointer; length:DWORD): BOOL; stdcall; external basswmadll;
function BASS_WMA_EncodeClose(handle:HWMENCODE): BOOL; stdcall; external basswmadll;

function BASS_WMA_GetWMObject(handle:DWORD): Pointer; stdcall; external basswmadll;

implementation

end.
