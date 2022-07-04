unit wa_ipc;

{
  Winamp IPC
  Release #3 (February 23th 2005)
  Translated to Pascal for use with Borland Delphi by Saivert
  Homepage -> http://saivertweb.no-ip.com

  What's new:
  Updated to support Winamp 5.03 API's
  More documentation for the functions at the end of this unit.

  Notes:
  This is the original wa_ipc.h file from the new Winamp v2.9x API.
  I (Saivert) have translated/adapted it for use with the Borland Delphi
  development environment (which uses the Object Pascal language).
  All comments added by me are marked with "[Saivert]". }

{ I converted this after downloading the new Winamp 2.9x API.
  Smokin' hot new features like the ones you only got in Wasabi
  (support for extending the preferences dialog and access to
  video interface + many common dialogs like Open URL).

  Please read the comments scattered around in this file.
  All comments added by me are marked with "[Saivert]".
  All other comments are from the C source.

  I have also written a few wrapper functions for some of the API calls.
  This includes:
    * IPC_OPENURL - function WAOpenURLBox
    * IPC_GETEQDATA/IPC_SETEQDATA - function WASetEqData
    * IPC_GETHTTPGETTER - function WADownloadFile
    * IPC_GET_EXTENDED_FILE_INFO - function WAGetExtendedFileInfo
    * IPC_GETPLAYLISTFILE, IPC_GETPLAYLISTTITLE, IPC_GETLISTPOS and
      IPC_GETLISTLENGTH - function WAGetPlaylistItem

  Look near the end of this unit for more information about these functions.

  My homepage URL is:
    http://saivertweb.no-ip.com
  My e-mail address is (I got spam protection, so don't bother...):
    saivert@gmail.com

  Following is the original Nullsoft license: }

(*
** Copyright (C) 2003 Nullsoft, Inc.
**
** This software is provided 'as-is', without any express or implied warranty. In no event will the authors be held
** liable for any damages arising from the use of this software.
**
** Permission is granted to anyone to use this software for any purpose, including commercial applications, and to
** alter it and redistribute it freely, subject to the following restrictions:
**
**   1. The origin of this software must not be misrepresented; you must not claim that you wrote the original software.
**      If you use this software in a product, an acknowledgment in the product documentation would be appreciated but is not required.
**
**   2. Altered source versions must be plainly marked as such, and must not be misrepresented as being the original software.
**
**   3. This notice may not be removed or altered from any source distribution.
**
*)


// Followings is the comment by Silhwan Hyun  at 2008-07-24
// Added 2 IPC message constants which appreared at Winamp SDK v5.31, which are used by
//  Milkdrop 2.0e
//    IPC_GETPLAYLISTFILEW,  IPC_GETPLAYLISTTITLEW
// Added 2 IPC message constants which probably can be used by any plug-ins.
//    IPC_GET_PLAYING_FILENAME, IPC_GET_PLAYING_TITLE

interface

{ This unit does not depend on SysUtils so you can use this
  in a non-VCL setup. (To generate a very small DLL file). }

uses Windows, Messages;

const

(*
** This is the modern replacement for the classic 'frontend.h'. Most of these
** updates are designed for in-process use, i.e. from a plugin.
**
*)

{* message used to sent many messages to winamp's main window.
** most all of the IPC_* messages involve sending the message in the form of:
**   result = SendMessage(hwnd_winamp,WM_WA_IPC,(parameter),IPC_*);
*}
  WM_WA_IPC = WM_USER;
(* but some of them use WM_COPYDATA. be afraid.
*)

  IPC_GETVERSION = 0;
(* int version = SendMessage(hwnd_winamp,WM_WA_IPC,0,IPC_GETVERSION);
**
** Version will be 0x20yx for winamp 2.yx. versions previous to Winamp 2.0
** typically (but not always) use 0x1zyx for 1.zx versions. Weird, I know.
*)

  IPC_GETREGISTEREDVERSION = 770;
  { [Saivert] This message opens the Preferences dialog and displays the
    Winamp Pro registration page. }


type
  TenqueueFileWithMetaStruct = record
    filename: PChar;
    title: PChar;
    length: Integer;
  end;
{ send this to a IPC_PLAYFILE in a non WM_COPYDATA,
  and you get the nice desired result. if title is nil, it is treated as a "thing",
  otherwise it's assumed to be a file (for speed) }

{ [Saivert] What the hell do Nullsoft mean with this: it is treated as a "thing"?! }

const
  IPC_PLAYFILE = 100;  {dont be fooled, this is really the same as enqueufile}
  IPC_ENQUEUEFILE = 100;
(* sent as a WM_COPYDATA, with IPC_PLAYFILE as the dwData, and the string to play
** as the lpData. Just enqueues, does not clear the playlist or change the playback
** state.
*)

  IPC_DELETE = 101;
  IPC_DELETE_INT = 1101; {don't use this, it's used internally by winamp when
                          dealing with some lame explorer issues.}
(* SendMessage(hwnd_winamp,WM_WA_IPC,0,IPC_DELETE);
** Use IPC_DELETE to clear Winamp's internal playlist.
*)


  IPC_STARTPLAY = 102; {starts playback. almost like hitting play in Winamp.}
  IPC_STARTPLAY_INT = 1102; {used internally, don't bother using it (won't be any fun)}


  IPC_CHDIR = 103;
(* sent as a WM_COPYDATA, with IPC_CHDIR as the dwData, and the directory to change to
** as the lpData.
*)


  IPC_ISPLAYING = 104;
(* res := SendMessage(hwnd_winamp,WM_WA_IPC,0,IPC_ISPLAYING);
** If it returns 1, it is playing. if it returns 3, it is paused,
** if it returns 0, it is not playing.
*)


  IPC_GETOUTPUTTIME = 105;
(* res := SendMessage(hwnd_winamp,WM_WA_IPC,mode,IPC_GETOUTPUTTIME);
** returns the position in milliseconds of the current track (mode = 0),
** or the track length, in seconds (mode = 1). Returns -1 if not playing or error.
*)

  IPC_JUMPTOTIME = 106;
(* (requires Winamp 1.60+)
** SendMessage(hwnd_winamp,WM_WA_IPC,ms,IPC_JUMPTOTIME);
** IPC_JUMPTOTIME sets the position in milliseconds of the
** current song (approximately).
** Returns -1 if not playing, 1 on eof, or 0 if successful
*)

  IPC_GETMODULENAME = 109;
  IPC_EX_ISRIGHTEXE = 666;
(* usually shouldnt bother using these, but here goes:
** send a WM_COPYDATA with IPC_GETMODULENAME, and an internal
** flag gets set, which if you send a normal WM_WA_IPC message with
** IPC_EX_ISRIGHTEXE, it returns whether or not that filename
** matches. lame, I know.
*)

  IPC_WRITEPLAYLIST = 120;
(* (requires Winamp 1.666+)
** SendMessage(hwnd_winamp,WM_WA_IPC,0,IPC_WRITEPLAYLIST);
**
** IPC_WRITEPLAYLIST writes the current playlist to <winampdir>\\Winamp.m3u,
** and returns the current playlist position.
** Kinda obsoleted by some of the 2.x new stuff, but still good for when
** using a front-end (instead of a plug-in)
*)


  IPC_SETPLAYLISTPOS = 121;
(* (requires Winamp 2.0+)
** SendMessage(hwnd_winamp,WM_WA_IPC,position,IPC_SETPLAYLISTPOS)
** IPC_SETPLAYLISTPOS sets the playlist position to 'position'. It
** does not change playback or anything, it just sets position, and
** updates the view if necessary
*)


  IPC_SETVOLUME = 122;
(* (requires Winamp 2.0+)
** SendMessage(hwnd_winamp,WM_WA_IPC,volume,IPC_SETVOLUME);
** IPC_SETVOLUME sets the volume of Winamp (from 0-255).
*)
{ Tip from Saivert: use -666 as volume to get the current volume level,
  like: cur_vol := SendMessage(hwnd_winamp, WM_WA_IPC, -666, IPC_SETVOLUME);
  Justin likes that special number (associated with the devil). }


  IPC_SETPANNING = 123;
(* (requires Winamp 2.0+)
** SendMessage(hwnd_winamp,WM_WA_IPC,panning,IPC_SETPANNING);
** IPC_SETPANNING sets the panning of Winamp (from 0 (left) to 255 (right)).
*)
{ Tip from Saivert: use -666 as panning to get the current balance setting,
  like: cur_pan := SendMessage(hwnd_winamp, WM_WA_IPC, -666, IPC_SETPANNING); }


  IPC_GETLISTLENGTH = 124;
(* (requires Winamp 2.0+)
** length := SendMessage(hwnd_winamp,WM_WA_IPC,0,IPC_GETLISTLENGTH);
** IPC_GETLISTLENGTH returns the length of the current playlist, in
** tracks.
*)


  IPC_GETLISTPOS = 125;
(* (requires Winamp 2.05+)
** pos := SendMessage(hwnd_winamp,WM_WA_IPC,0,IPC_GETLISTPOS);
** IPC_GETLISTPOS returns the playlist position. A lot like IPC_WRITEPLAYLIST
** only faster since it doesn't have to write out the list. Heh, silly me.
*)


  IPC_GETINFO = 126;
(* (requires Winamp 2.05+)
** inf := SendMessage(hwnd_winamp,WM_WA_IPC,mode,IPC_GETINFO);
** IPC_GETINFO returns info about the current playing song. The value
** it returns depends on the value of 'mode'.
** Mode      Meaning
** ------------------
** 0         Samplerate (i.e. 44100)
** 1         Bitrate  (i.e. 128)
** 2         Channels (i.e. 2)
** 3 (5+)    Video LOWORD=w HIWORD=h
** 4 (5+)    > 65536, string (video description)
*)


  IPC_GETEQDATA = 127;
(* (requires Winamp 2.05+)
** data := SendMessage(hwnd_winamp,WM_WA_IPC,pos,IPC_GETEQDATA);
** IPC_GETEQDATA queries the status of the EQ.
** The value returned depends on what 'pos' is set to:
** Value      Meaning
** ------------------
** 0-9        The 10 bands of EQ data. 0-63 (+20db - -20db)
** 10         The preamp value. 0-63 (+20db - -20db)
** 11         Enabled. zero if disabled, nonzero if enabled.
** 12         Autoload. zero if disabled, nonzero if enabled.
*)


  IPC_SETEQDATA = 128;
(* (requires Winamp 2.05+)
** SendMessage(hwnd_winamp,WM_WA_IPC,pos,IPC_GETEQDATA);
** SendMessage(hwnd_winamp,WM_WA_IPC,value,IPC_SETEQDATA);
** IPC_SETEQDATA sets the value of the last position retrieved
** by IPC_GETEQDATA. This is pretty lame, and we should provide
** an extended version that lets you do a MAKELPARAM(pos,value).
** someday...

  new (2.92+): 
    if the high byte is set to 0xDB, then the third byte specifies
    which band, and the bottom word specifies the value.

  Saivert (an example):
    SendMessage(hwnd_winamp, WM_WA_IPC,
      MAKEWPARAM(value, MAKEWORD(band, 0xDB)), IPC_SETEQDATA);
  My utility function WASetEQData uses this method if the version of
  Winamp is >= 0x2902
*)


  IPC_ADDBOOKMARK = 129;
(* (requires Winamp 2.4+)
** Sent as a WM_COPYDATA, using IPC_ADDBOOKMARK, adds the specified
** file/url to the Winamp bookmark list.

In winamp 5+, we use this as a normal WM_WA_IPC and the string:

  "filename\0title\0"

  to notify the library/bookmark editor that a bookmark
was added. Note that using this message in this context does not
actually add the bookmark.
do not use :)

*)


  IPC_INSTALLPLUGIN = 130;
(* not implemented, but if it was you could do a WM_COPYDATA with
** a path to a .wpz, and it would install it.
*)


  IPC_RESTARTWINAMP = 135;
(* (requires Winamp 2.2+)
** SendMessage(hwnd_winamp,WM_WA_IPC,0,IPC_RESTARTWINAMP);
** IPC_RESTARTWINAMP will restart Winamp (isn't that obvious ? :)
*)


  IPC_ISFULLSTOP = 400;
(* (requires winamp 2.7+ I think)
** ret := SendMessage(hwnd_winamp,WM_WA_IPC,0,IPC_ISFULLSTOP);
** useful for when you're an output plugin, and you want to see
** if the stop/close is a full stop, or just between tracks.
** returns nonzero if it's full, zero if it's just a new track.
*)


  IPC_INETAVAILABLE = 242;
(* (requires Winamp 2.05+)
** val := SendMessage(hwnd_winamp,WM_WA_IPC,0,IPC_INETAVAILABLE);
** IPC_INETAVAILABLE will return 1 if the Internet connection is available for Winamp.
*)


  IPC_UPDTITLE = 243;
(* (requires Winamp 2.2+)
** SendMessage(hwnd_winamp,WM_WA_IPC,0,IPC_UPDTITLE);
** IPC_UPDTITLE will ask Winamp to update the informations about the current title.
*)


  IPC_REFRESHPLCACHE = 247;
(* (requires Winamp 2.2+)
** SendMessage(hwnd_winamp,WM_WA_IPC,0,IPC_REFRESHPLCACHE);
** IPC_REFRESHPLCACHE will flush the playlist cache buffer.
** (send this if you want it to go refetch titles for tracks)
*)


  IPC_GET_SHUFFLE = 250;
(* (requires Winamp 2.4+)
** val := SendMessage(hwnd_winamp,WM_WA_IPC,0,IPC_GET_SHUFFLE);
**
** IPC_GET_SHUFFLE returns the status of the Shuffle option (1 if set)
*)


  IPC_GET_REPEAT = 251;
(* (requires Winamp 2.4+)
** val := SendMessage(hwnd_winamp,WM_WA_IPC,0,IPC_GET_REPEAT);
**
** IPC_GET_REPEAT returns the status of the Repeat option (1 if set)
*)


  IPC_SET_SHUFFLE = 252;
(* (requires Winamp 2.4+)
** SendMessage(hwnd_winamp,WM_WA_IPC,value,IPC_SET_SHUFFLE);
**
** IPC_SET_SHUFFLE sets the status of the Shuffle option (1 to turn it on)
*)


  IPC_SET_REPEAT = 253;
(* (requires Winamp 2.4+)
** SendMessage(hwnd_winamp,WM_WA_IPC,value,IPC_SET_REPEAT);
**
** IPC_SET_REPEAT sets the status of the Repeat option (1 to turn it on)
*)


  IPC_ENABLEDISABLE_ALL_WINDOWS = 259; { 0xdeadbeef to disable }
(* (requires Winamp 2.9+)
** if enable then
**   SendMessage(hwnd_winamp,WM_WA_IPC,0,IPC_ENABLEDISABLE_ALL_WINDOWS)
** else SendMessage(hwnd_winamp,WM_WA_IPC,$DEADBEEF,IPC_ENABLEDISABLE_ALL_WINDOWS);
** sending with $DEADBEEF as the param disables all winamp windows,
** any other values will enable all winamp windows.
*)


  IPC_GETWND = 260;
(* (requires Winamp 2.9+)
** _hwnd := SendMessage(hwnd_winamp,WM_WA_IPC,IPC_GETWND_xxx,IPC_GETWND);
** returns the HWND of the window specified.
*)
    IPC_GETWND_EQ = 0; {use one of these for the param}
    IPC_GETWND_PE = 1;
    IPC_GETWND_MB = 2;
    IPC_GETWND_VIDEO = 3;


(************************************************************************
***************** in-process only (WE LOVE PLUGINS)
************************************************************************)


  IPC_SETSKIN = 200;
(* (requires Winamp 2.04+, only usable from plug-ins (not external apps))
** SendMessage(hwnd_winamp,WM_WA_IPC,WPARAM(PChar('skinname')),IPC_SETSKIN);
** IPC_SETSKIN sets the current skin to "skinname". Note that skinname
** can be the name of a skin, a skin .zip file, with or without path.
** If path isn't specified, the default search path is the winamp skins
** directory.
*)


  IPC_GETSKIN = 201;
(* (requires Winamp 2.04+, only usable from plug-ins (not external apps))
** SendMessage(hwnd_winamp,WM_WA_IPC,WPARAM(skinname_buffer),IPC_GETSKIN);
** IPC_GETSKIN puts the directory where skin bitmaps can be found
** into  skinname_buffer.
** skinname_buffer must be MAX_PATH characters in length.
** When using a .zip'd skin file, it'll return a temporary directory
** where the ZIP was decompressed.
*)


  IPC_EXECPLUG = 202;
(* (requires Winamp 2.04+, only usable from plug-ins (not external apps))
** SendMessage(hwnd_winamp,WM_WA_IPC,WPARAM(PChar('vis_file.dll')),IPC_EXECPLUG);
** IPC_EXECPLUG executes a visualization plug-in pointed to by WPARAM.
** the format of this string can be:
** "vis_whatever.dll"
** "vis_whatever.dll,0" // (first mod, file in winamp plug-in dir)
** "C:\dir\vis_whatever.dll,1"
*)


  IPC_GETPLAYLISTFILE = 211;
  IPC_GETPLAYLISTFILEW = 214;  // * Added at 2008-07-24
(* (requires Winamp 2.04+, only usable from plug-ins (not external apps))
** name := PChar(SendMessage(hwnd_winamp,WM_WA_IPC,index,IPC_GETPLAYLISTFILE));
** IPC_GETPLAYLISTFILE gets the filename of the playlist entry [index].
** returns a pointer to it. returns NULL on error.
*)


  IPC_GETPLAYLISTTITLE = 212;
  IPC_GETPLAYLISTTITLEW = 213; // * Added at 2008-07-24
(* (requires Winamp 2.04+, only usable from plug-ins (not external apps))
** name = PChar(SendMessage(hwnd_winamp,WM_WA_IPC,index,IPC_GETPLAYLISTTITLE));
**
** IPC_GETPLAYLISTTITLE gets the title of the playlist entry [index].
** returns a pointer to it. returns NULL on error.
*)


  IPC_GETHTTPGETTER = 240;
(* retrieves a function pointer to a HTTP retrieval function.
** if this is unsupported, returns 1 or 0.
** the function should be:
** int (*httpRetrieveFile)(HWND hwnd, char *url, char *file, char *dlgtitle);
** if you call this function, with a parent window, a URL, an output file, and a dialog title,
** it will return 0 on successful download, 1 on error.
*)


// Followings 4 items are undocumented Winamp IPC messages  (Contributed by Saivert saivert@gmail.com)
  IPC_PLAYLIST_GET_NEXT_SELECTED = 3029;

// Example usage:
//   while((sel = SendMessage(plugin.hwndParent,WM_WA_IPC,sel,IPC_PLAYLIST_GET_NEXT_SELECTED))!=-1)
//   {           // For each iteration, sel is a selected playlist item index.
//   }

  IPC_GET_RANDFUNC = 3015;
// returns a function to get a random number
// int (*randfunc)(void) = SendMessage(hMainWindow, WM_WA_IPC, 0, IPC_GET_RANDFUNC);

  IPC_REGISTER_LOWORD_COMMAND = 3019;
// assigns you a unique ID to make your own commands (for menus, etc)
// WORD id = SendMessage(hMainWindow, WM_WA_IPC, 0, IPC_REGISTER_LOWORD_COMMAND);

  IPC_GET_PROXY_STRING = 3023;

   

{ [Saivert] I have made a pascal prototype, here it is: }
type
  TFNhttpRetrieveFile = function(parentwnd: HWND; url, _file, dlgtitle: PChar): Integer; cdecl;

{ [Saivert] Get the function by calling this:
  var
    httpgetfn: TFNhttpRetrieveFile;
  begin
    @httpgetfn := Pointer(SendMessage(hwnd_winamp,WM_WA_IPC,0,IPC_GETHTTPGETTER));
  end;
}

const
  IPC_MBOPEN = 241;
(* (requires Winamp 2.05+)
** SendMessage(hwnd_winamp,WM_WA_IPC,0,IPC_MBOPEN);
** SendMessage(hwnd_winamp,WM_WA_IPC,WPARAM(PChar(url)),IPC_MBOPEN);
** IPC_MBOPEN will open a new URL in the minibrowser. if url is nil, it will open the Minibrowser window.
*)



  IPC_CHANGECURRENTFILE = 245;
(* (requires Winamp 2.05+)
** SendMessage(hwnd_winamp,WM_WA_IPC,WPARAM(PChar(file)),IPC_CHANGECURRENTFILE);
** IPC_CHANGECURRENTFILE will set the current playlist item.
*)


  IPC_GETMBURL = 246;
(* (requires Winamp 2.2+)
** var buffer: array[0..4095] of Char; // Urls can be VERY long
** SendMessage(hwnd_winamp,WM_WA_IPC,WPARAM(Pointer(buffer)),IPC_GETMBURL);
** IPC_GETMBURL will retrieve the current Minibrowser URL into buffer.
** buffer must be at least 4096 bytes long.
*)


  IPC_MBBLOCK = 248;
(* (requires Winamp 2.4+)
** SendMessage(hwnd_winamp,WM_WA_IPC,value,IPC_MBBLOCK);
**
** IPC_MBBLOCK will block the Minibrowser from updates if value is set to 1
*)

  IPC_MBOPENREAL = 249;
(* (requires Winamp 2.4+)
** SendMessage(hwnd_winamp,WM_WA_IPC,WPARAM(PChar(url)),IPC_MBOPENREAL);
**
** IPC_MBOPENREAL works the same as IPC_MBOPEN except that it will works even if
** IPC_MBBLOCK has been set to 1
*)

  IPC_ADJUST_OPTIONSMENUPOS = 280;
(* (requires Winamp 2.9+)
** newpos := SendMessage(hwnd_winamp,WM_WA_IPC,WPARAM(adjust_offset),IPC_ADJUST_OPTIONSMENUPOS);
** moves where winamp expects the Options menu in the main menu.
** Useful if you wish to insert a menu item above the options/skins/vis menus.
*)

  IPC_GET_HMENU = 281;
(* (requires Winamp 2.9+)
** _hMenu := SendMessage(hwnd_winamp,WM_WA_IPC,WPARAM(0),IPC_GET_HMENU);
** values for data:
** 0 : main popup menu 
** 1 : main menubar file menu
** 2 : main menubar options menu
** 3 : main menubar windows menu
** 4 : main menubar help menu
** other values will return NULL.
*)

  IPC_GET_EXTENDED_FILE_INFO = 290; {pass a pointer to the following struct in wParam}
(* (requires Winamp 2.9+)
** to use, create an extendedFileInfoStruct, point the values filename and metadata to the
** filename and metadata field you wish to query, and ret to a buffer, with retlen to the
** length of that buffer, and then SendMessage(hwnd_winamp,WM_WA_IPC,Integer(@struct),IPC_GET_EXTENDED_FILE_INFO);
** the results should be in the buffer pointed to by ret.
** returns 1 if the decoder supports a getExtendedFileInfo method
*)
type
  TextendedFileInfoStruct = record
    filename: PChar;
    metadata: PChar;
    ret: PChar;
    retlen: Integer;
  end;

const
  IPC_GET_BASIC_FILE_INFO = 291; {pass a pointer to the following struct in wParam}

type
  TbasicFileInfoStruct = record
    filename: PChar;
    quickCheck: Integer;

    {filled in by winamp}
    length: Integer;
    title: PChar;
    titlelen: Integer;
  end;

const
  IPC_GET_EXTLIST = 292; {returns doublenull delimited.
  GlobalFree() it when done. if data is 0, returns raw extlist,
  if 1, returns something suitable for getopenfilename}

  IPC_INFOBOX = 293;

type
  PinfoBoxParam = ^TinfoBoxParam;
  TinfoBoxParam = record
    parent: HWND;
    filename: PChar;
  end;

{ [Saivert] Nullsoft didn't specify this, but I guess you call it like:
  SendMessage(hwnd_winamp,WM_WA_IPC,Integer(@AnInfoBoxParam),IPC_INFOBOX);
}

const
  IPC_SET_EXTENDED_FILE_INFO = 294; {pass a pointer to the a extendedFileInfoStruct in wParam}
(* (requires Winamp 2.9+)
** to use, create an extendedFileInfoStruct, point the values filename and metadata to the
** filename and metadata field you wish to write in ret. (retlen is not used). and then
** SendMessage(hwnd_winamp,WM_WA_IPC,Integer(@struct),IPC_SET_EXTENDED_FILE_INFO);
** returns 1 if the metadata is supported
** Call IPC_WRITE_EXTENDED_FILE_INFO once you're done setting all the metadata you want to update
*)

  IPC_WRITE_EXTENDED_FILE_INFO = 295;
(* (requires Winamp 2.9+)
** writes all the metadata set thru IPC_SET_EXTENDED_FILE_INFO to the file
** returns 1 if the file has been successfully updated, 0 if error
*)

  IPC_FORMAT_TITLE = 297;
type
  PwaFormatTitle = ^TwaFormatTitle;
  TwaFormatTitle = record
    spec: PChar; // NULL=default winamp spec
    p: Pointer;

    _out: PChar;
    _out_len: Integer;

    //return 0 if not found
    TAGFUNC: function(tag: PChar; p: Pointer): PChar; cdecl;
    { [Saivert's note] This will be called by Winamp to free the
      GlobalAlloc'ed string you returned in TAGFUNC.}
    TAGFREEFUNC: procedure(tag: PChar; p: Pointer); cdecl;
  end;

const
  IPC_GETUNCOMPRESSINTERFACE = 331;
(* returns a function pointer to uncompress().
** int (*uncompress)(unsigned char *dest, unsigned long *destLen, const unsigned char *source, unsigned long sourceLen);
** right out of zlib, useful for decompressing zlibbed data.
** if you pass the parm of 0x10100000, it will return a wa_inflate_struct * to an inflate API.
*)

type
  Pwa_inflate_struct = ^Twa_inflate_struct;
  Twa_inflate_struct = record
    inflateReset: function(strm: Pointer): Integer; cdecl;
    inflateInit_: function(strm: Pointer; version: PChar; stream_size: Integer): Integer; cdecl;
    inflate:      function(strm: Pointer; flush: Integer): Integer; cdecl;
    inflateEnd:   function(strm: Pointer): Integer; cdecl;
    crc32:        function(crc: Cardinal; buf: PWideChar; len: Cardinal): Cardinal; cdecl;
  end;


{ [Saivert] Here I have a function prototype suitable for use in Delphi: }
type
  TFNuncompress = function(dest: PChar; var destLen: Integer; source: PChar; sourceLen: Longint): Integer; cdecl;

{ [Saivert] Please read my comments about IPC_ADD_PREFS_DLG below }
const
  IPC_ADD_PREFS_DLG = 332;
  IPC_REMOVE_PREFS_DLG = 333;
(* (requires Winamp 2.9+)
** to use, allocate a prefsDlgRec structure (either on the heap or some global
** data, but NOT on the stack), initialize the members:
** hInst to the DLL instance where the resource is located
** dlgID to the ID of the dialog,
** proc to the window procedure for the dialog
** name to the name of the prefs page in the prefs.
** where to 0 (eventually we may add more options)
** then, SendMessage(hwnd_winamp,WM_WA_IPC,Integer(@prefsRec),IPC_ADD_PREFS_DLG);
**
** you can also IPC_REMOVE_PREFS_DLG with the address of the same prefsRec,
** but you shouldn't really ever have to.
**
*)

  IPC_OPENPREFSTOPAGE = 380; // pass an id of a builtin page, or a &prefsDlgRec of prefs page to open
  { [Saivert (new in rel. #3)] As the text vaguely indicates you need to pass
    the address of the prefsDlgRec to open. Not the value of the _id member
    which would be more logical. For built in pages like the "File types" page
    you must pass the id of that page. }

(* [Saivert] original c statement kept for reference
typedef struct _prefsDlgRec {
  HINSTANCE hInst;
  int dlgID;
  void *proc;
  char *name;
  int where; // 0 for options, tho we need to define more
  int _id;
  struct _prefsDlgRec *next;
} prefsDlgRec;
*)

type
  PprefsDlgRec = ^TprefsDlgRec;
  TprefsDlgRec = record
    _hInst: HINST;
    dlgID: Integer;
    proc: Pointer;

    name: PChar;
    where: Integer; {0 for options, though we they need to define more}

    _id: Integer;
    _prefsDlgRec: PprefsDlgRec;
  end;

const
  IPC_GETINIFILE = 334;
 // returns a pointer to the full file path of winamp.ini
 // char *ini=(char*)SendMessage(hwnd_winamp,WM_WA_IPC,0,IPC_GETINIFILE);

  IPC_GETINIDIRECTORY = 335;
 // returns a pointer to the directory where you put config files (if you don't want to use winamp.ini)
 // char *dir=(char*)SendMessage(hwnd_winamp,WM_WA_IPC,0,IPC_GETINIDIRECTORY);

  IPC_GETPLUGINDIRECTORY = 336;
 // returns a pointer to the directory where Winamp has its plugins stored
 // char *plugdir=(char*)SendMessage(hwnd_winamp,WM_WA_IPC,0,IPC_GETPLUGINDIRECTORY);

  IPC_GETM3UDIRECTORY = 337;
 // returns a pointer to the directory where winamp.m3u (and winamp.m3u8 if supported) is stored
 // char *m3udir=(char*)SendMessage(hwnd_winamp,WM_WA_IPC,0,IPC_GETM3UDIRECTORY);

  IPC_GETM3UDIRECTORYW = 338;
 // returns a pointer to the directory where winamp.m3u (and winamp.m3u8 if supported) is stored
 // wchar_t *m3udirW=(wchar_t*)SendMessage(hwnd_winamp,WM_WA_IPC,0,IPC_GETM3UDIRECTORYW);

  IPC_SPAWNBUTTONPOPUP = 361; // param =
// 0 = eject
// 1 = previous
// 2 = next
// 3 = pause
// 4 = play
// 5 = stop

  IPC_OPENURLBOX = 360; {pass a HWND to a parent,
  returns a HGLOBAL that needs to be freed with GlobalFree(), if successful}
 // HGLOBAL hglobal = (HGLOBAL)SendMessage(hwnd_winamp,WM_WA_IPC,(WPARAM)(HWND)parent,IPC_OPENURLBOX);

  IPC_OPENFILEBOX = 362; // pass a HWND to a parent
  IPC_OPENDIRBOX = 363; // pass a HWND to a parent

// pass an HWND to a parent. call this if you take over the whole UI so that the dialogs are not appearing on the
// bottom right of the screen since the main winamp window is at 3000x3000, call again with NULL to reset
  IPC_SETDIALOGBOXPARENT = 364;
  IPC_GETDIALOGBOXPARENT = 365;
  IPC_UPDATEDIALOGBOXPARENT = 366;

{ pass 0 for a copy of the skin HBITMAP
  pass 1 for name of font to use for playlist editor likeness
  pass 2 for font charset
  pass 3 for font size }
  IPC_GET_GENSKINBITMAP = 503;


  IPC_GET_EMBEDIF = 505; {pass an embedWindowState}
{ returns an HWND embedWindow(embedWindowState *); if the data is NULL,
 otherwise returns the HWND directly }

type
  PembedWindowState = ^TembedWindowState;
  TembedWindowState = record
    me: HWND;             // hwnd of the window
    flags: Integer;       // see EMBED_FLAGS_...
    r: TRect;
    user_ptr: Pointer;    // for application use
    extra_data: array[0..63] of Integer;  // for internal winamp use
  end;

{ Here is a function prototype for embedWindow }
type
  TembedWindow = function(ews: PembedWindowState): HWND; cdecl;

const
  EMBED_FLAGS_NORESIZE = 1; { set this bit to keep window from being resizable}
  EMBED_FLAGS_NOTRANSPARENCY = 2; { set this bit to make gen_ff turn transparency
                                    off for this wnd }
  EMBED_FLAGS_NOWINDOWMENU = 4;  { set this bit to prevent gen_ff from automatically
                                   adding your window to the right-click menu }
  EMBED_FLAGS_GUID = 8;   { call SET_EMBED_GUID(yourEmbedWindowStateStruct, GUID)
                            to define a GUID for this window }
 // SET_EMBED_GUID(windowState, windowGUID)
 // { windowState->flags |= EMBED_FLAGS_GUID; *((GUID *)&windowState->extra_data[4])=windowGUID; }
 // GET_EMBED_GUID(windowState) (*((GUID *)&windowState->extra_data[4]))

(* [Saivert] To embed a window write this:
    var
      ews: TembedWindowState;
    .
    .
    .
    var
      embedWnd: TembedWindow;
      wnd: HWND;
    begin
      embedWnd := TembedWindow(
        SendMessage(hwnd_winamp, WM_WA_IPC, 0, IPC_GET_EMBEDIF)
      );

      ews.r.Top := 0;
      ews.r.Left := 0;
      ews.r.Right := 300;
      ews.r.Bottom := 200;

      wnd := embedWindow(@ews);
      { ews.me also refers to the newly created window }
      SetParent(my_plugin_window, wnd);
      // or: MyPluginForm.ParentWindow := wnd;
      ShowWindow(wnd, SW_SHOW);
    end;

  PS! Remember that the TembedWindowState variable must be global, the
  pointer must remain valid after calling "embedWindow". An easy way
  of ensuring this is to declare it outside of a function.
*)

  IPC_EMBED_ENUM = 532;
type
  PembedEnumStruct = ^TembedEnumStruct;
  TembedEnumStruct = record
    enumProc: function(embedWindowState: PembedWindowState;
      embedEnumStruct: PembedEnumStruct): Integer; cdecl; // return 1 to abort or more :)
    user_data: Integer;
  end;
  // pass
const
  IPC_EMBED_ISVALID = 533;




  IPC_CONVERTFILE = 506;
(* (requires Winamp 2.92+)
** Converts a given file to a different format (PCM, MP3, etc...)
** To use, pass a pointer to a waFileConvertStruct struct
** This struct can be either on the heap or some global
** data, but NOT on the stack. At least, until the conversion is done.
**
** eg: SendMessage(hwnd_winamp,WM_WA_IPC,&myConvertStruct,IPC_CONVERTFILE);
**
** Return value:
** 0: Can't start the conversion. Look at myConvertStruct->error for details.
** 1: Conversion started. Status messages will be sent to the specified callbackhwnd.
**    Be sure to call IPC_CONVERTFILE_END when your callback window receives the
**    IPC_CB_CONVERT_DONE message.
*)
type
  PconvertFileStruct = ^TconvertFileStruct;
  TconvertFileStruct = record
    sourcefile: PChar;// "c:\\source.mp3"
    destfile: PChar;  // "c:\\dest.pcm"
    destformat: array[0..8-1] of Integer; // like 'PCM ',srate,nch,bps
    callbackhwnd: HWND; // window that will receive the IPC_CB_CONVERT
                        //notification messages
    //filled in by winamp.exe
    error: PChar; //if IPC_CONVERTFILE returns 0, the reason will be here

    bytes_done: Integer; //you can look at both of these values for speed statistics
    bytes_total: Integer;
    bytes_out: Integer;

    killswitch: Integer;// don't set it manually, use IPC_CONVERTFILE_END
    extra_data: array[0..64-1] of Integer;// for internal winamp use
  end;

const
  IPC_CONVERTFILE_END = 507;
(* (requires Winamp 2.92+)
** Stop/ends a convert process started from IPC_CONVERTFILE
** You need to call this when you receive the IPC_CB_CONVERTDONE message or when you
** want to abort a conversion process
**
** eg: SendMessage(hwnd_winamp,WM_WA_IPC,&myConvertStruct,IPC_CONVERTFILE_END);
**
** No return value
*)

type
  PconvertConfigStruct = ^TconvertConfigStruct;
  TconvertConfigStruct = record
    hwndParent: HWND;
    format: Integer;

  //filled in by winamp.exe
    hwndConfig: HWND;
    extra_data: array[0..8-1] of Integer;
  end;

const
  IPC_CONVERT_CONFIG = 508;
  IPC_CONVERT_CONFIG_END = 509;

type
  PconverterEnumFmtStruct = ^TconverterEnumFmtStruct;
  TconverterEnumFmtStruct = record
    enumProc: procedure(user_data: Integer; desc: PChar; fourcc: Integer); cdecl;
    user_data: Integer;
  end;

const
  IPC_CONVERT_CONFIG_ENUMFMTS = 510;
(* (requires Winamp 2.92+) *)

type
  PburnCDStruct = ^TburnCDStruct;
  TburnCDStruct = record
    cdletter: Char;
    playlist_file: PChar;
    callback_hwnd: HWND;

  //filled in by winamp.exe
    error: PChar;
end;

const
  IPC_BURN_CD = 511;
(* (requires Winamp 5.0+) *)

type
  PconvertSetPriority = ^TconvertSetPriority;
  TconvertSetPriority = record
    cfs: PconvertFileStruct;
    priority: Integer;
  end;

const
  IPC_CONVERT_SET_PRIORITY = 512;

type
  PwaHookTitleStruct = ^TwaHookTitleStruct;
  TwaHookTitleStruct = record
    filename: PChar;
    title: PChar; // 2048 bytes
    length: Integer;
    force_useformatting: Integer; // can set this to 1 if you want to force a
                                  //url to use title formatting shit
  end;

const
// return TRUE if you hook this
  IPC_HOOK_TITLES = 850;

  IPC_GETSADATAFUNC = 800;
// 0: returns a char *export_sa_get() that returns 150 bytes of data
// 1: returns a export_sa_setreq(int want);

  IPC_ISMAINWNDVISIBLE = 900;

  IPC_SETPLEDITCOLORS = 920;

type
  PwaSetPlColorsStruct = ^TwaSetPlColorsStruct;
  TwaSetPlColorsStruct = record
    numElems: Integer;
    elems: PInteger;
    bm: HBITMAP; // set if you want to override
  end;

const
// the following IPC use waSpawnMenuParms as parameter
  IPC_SPAWNEQPRESETMENU = 933;
  IPC_SPAWNFILEMENU = 934; //menubar
  IPC_SPAWNOPTIONSMENU = 935; //menubar
  IPC_SPAWNWINDOWSMENU = 936; //menubar
  IPC_SPAWNHELPMENU = 937; //menubar
  IPC_SPAWNPLAYMENU = 938; //menubar
  IPC_SPAWNPEFILEMENU = 939; //menubar
  IPC_SPAWNPEPLAYLISTMENU = 940; //menubar
  IPC_SPAWNPESORTMENU = 941; //menubar
  IPC_SPAWNPEHELPMENU = 942; //menubar
  IPC_SPAWNMLFILEMENU = 943; //menubar
  IPC_SPAWNMLVIEWMENU = 944; //menubar
  IPC_SPAWNMLHELPMENU = 945; //menubar
  IPC_SPAWNPELISTOFPLAYLISTS = 946;

type
  PwaSpawnMenuParms = ^TwaSpawnMenuParms;
  TwaSpawnMenuParms = record
    wnd: HWND;
    xpos: Integer;// in screen coordinates
    ypos: Integer;
  end;

// waSpawnMenuParms2 is used by the menubar submenus
  PwaSpawnMenuParms2 = ^TwaSpawnMenuParms2;
  TwaSpawnMenuParms2 = record
    wnd: HWND;
    xpos: Integer;// in screen coordinates
    ypos: Integer;
    width: Integer;
    height: Integer;
  end;

const

{  system tray sends this (you might want to simulate it) }
  WM_WA_SYSTRAY = WM_USER+1;

{ input plugins send this when they are done playing back }
  WM_WA_MPEG_EOF = WM_USER+2;


{ V I D E O   I N T E R F A C E }

{ video stuff }
const
  IPC_GET_IVIDEOOUTPUT = 500; { see below for IVideoOutput interface }
  IPC_IS_PLAYING_VIDEO = 501; // returns >1 if playing, 0 if not, 1 if old version (so who knows):)

{ VIDEO_MAKETYPE(A,B,C,D) ((A) | ((B)<<8) | ((C)<<16) | ((D)<<24)) }
{ There is no MACRO equivalent in Pascal,
  so here is a function which does the same: }
function Video_MakeType(A, B, C, D: Integer): Integer;

const
  VIDUSER_SET_INFOSTRING = $1000;
  VIDUSER_GET_VIDEOHWND  = $1001;
  VIDUSER_SET_VFLIP      = $1002;


{$IFNDEF NO_IVIDEO_DECLARE}
type
  SubsItem = Pointer; // SubsItem is a class (currently undefined)

  TYV12_PLANE = record
    baseAddr: PChar;
    rowBytes: Longint;
  end;

  TYV12_PLANES = record
    y: TYV12_PLANE;
    u: TYV12_PLANE;
    v: TYV12_PLANE;
  end;

  Tmsgcallback = function(token: Pointer; hwindow: HWND; uMsg: UINT;
    wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;

  IVideoOutput = interface

    function open(w, h, vflip: Integer; aspectratio: DOUBLE;
      fmt: Cardinal): Integer; cdecl;
    procedure setcallback(msgcallback: Tmsgcallback;
      token: Pointer);  cdecl;
    procedure close; cdecl;
    procedure draw(frame: Pointer); cdecl;
    procedure drawSubtitle(item: SubsItem); cdecl;
    procedure showStatusMsg(text: PChar); cdecl;
    function get_latency: Integer;  cdecl;
    procedure notifyBufferState(bufferstate: Integer); cdecl; { bufferstate = 0-255 }
    function extended(param1, param2, param3: Integer): Integer; cdecl;
  end;

  ITrackSelector = interface
    function getNumAudioTracks(): Integer; cdecl;
    procedure enumAudioTrackName(n: Integer; buf: PChar; size: Integer); cdecl;
    function getCurAudioTrack(): Integer; cdecl;
    function getNumVideoTracks(): Integer; cdecl;
    procedure enumVideoTrackName(n: Integer; buf: PChar; size: Integer); cdecl;
    function getCurVideoTrack(): Integer; cdecl;

    procedure setAudioTrack(n: Integer); cdecl;
    procedure setVideoTrack(n: Integer); cdecl;
  end;
        


{$endif}



{ these messages are callbacks that you can grab by subclassing the winamp window }

{ wParam = }
const
  IPC_CB_WND_EQ = 0; { use one of these for the param }
  IPC_CB_WND_PE = 1;
  IPC_CB_WND_MB = 2;
  IPC_CB_WND_VIDEO = 3;
  IPC_CB_WND_MAIN = 4;


  IPC_CB_ONSHOWWND = 600;
  IPC_CB_ONHIDEWND = 601;

  IPC_CB_GETTOOLTIP = 602;

  IPC_CB_MISC = 603;
    IPC_CB_MISC_TITLE     = 0;
    IPC_CB_MISC_VOLUME    = 1; // volume/pan
    IPC_CB_MISC_STATUS    = 2;
    IPC_CB_MISC_EQ        = 3;
    IPC_CB_MISC_INFO      = 4;
    IPC_CB_MISC_VIDEOINFO = 5;


  IPC_CB_CONVERT_STATUS = 604; // param value goes from 0 to 100 (percent)
  IPC_CB_CONVERT_DONE   = 605;

  IPC_ADJUST_FFWINDOWSMENUPOS = 606;
(* (requires Winamp 2.9+)
** int newpos=SendMessage(hwnd_winamp,WM_WA_IPC,(WPARAM)adjust_offset,IPC_ADJUST_FFWINDOWSMENUPOS);
** moves where winamp expects the freeform windows in the menubar windows main menu. Useful if you wish to insert a
** menu item above extra freeform windows.
*)

  IPC_ISDOUBLESIZE = 608;

  IPC_ADJUST_FFOPTIONSMENUPOS = 609;
(* (requires Winamp 2.9+)
** int newpos=SendMessage(hwnd_winamp,WM_WA_IPC,(WPARAM)adjust_offset,IPC_ADJUST_FFOPTIONSMENUPOS);
** moves where winamp expects the freeform preferences item in the menubar windows main menu. Useful if you wish to insert a
** menu item above preferences item.
*)

  IPC_GETTIMEDISPLAYMODE = 610; // returns 0 if displaying elapsed time or 1 if displaying remaining time

  IPC_SETVISWND = 611; // param is hwnd, setting this allows you to receive ID_VIS_NEXT/PREVOUS/RANDOM/FS wm_commands
  ID_VIS_NEXT                     = 40382;
  ID_VIS_PREV                     = 40383;
  ID_VIS_RANDOM                   = 40384;
  ID_VIS_FS                       = 40389;
  ID_VIS_CFG                      = 40390;
  ID_VIS_MENU                     = 40391;

  IPC_GETVISWND = 612; // returns the vis cmd handler hwnd
  IPC_ISVISRUNNING = 613;
  IPC_CB_VISRANDOM = 628; // param is status of random

  IPC_SETIDEALVIDEOSIZE = 614; // sent by winamp to winamp, trap it if you need it. width=HIWORD(param), height=LOWORD(param)

  IPC_GETSTOPONVIDEOCLOSE = 615;
  IPC_SETSTOPONVIDEOCLOSE = 616;

type
  PtransAccelStruct = ^TtransAccelStruct;
  TtransAccelStruct = record
    hwnd: HWND;
    uMsg: UINT;
    wParam: Integer;
    lParam: Integer;
  end;

const
  IPC_TRANSLATEACCELERATOR = 617;

type
  PwindowCommand = ^TwindowCommand;
  TwindowCommand = record
    cmd: Integer;
    x: Integer;
    y: Integer;
    align: Integer;
  end;
  // send this as param to an IPC_PLCMD, IPC_MBCMD, IPC_VIDCMD

const
  IPC_CB_ONTOGGLEAOT = 618;

  IPC_GETPREFSWND = 619;

  IPC_SET_PE_WIDTHHEIGHT = 620; // data is a pointer to a POINT structure that holds width & height

  IPC_GETLANGUAGEPACKINSTANCE = 621;

  IPC_CB_PEINFOTEXT = 622; // data is a string, ie: "04:21/45:02"

  IPC_CB_OUTPUTCHANGED = 623; // output plugin was changed in config

  IPC_GETOUTPUTPLUGIN = 625; {[Saivert] returns pointer to filename of
                              output plug-in. DirectSound is "out_ds.dll".}

  IPC_SETDRAWBORDERS = 626;
  IPC_DISABLESKINCURSORS = 627;
  IPC_CB_RESETFONT = 629;

  IPC_IS_FULLSCREEN = 630; // returns 1 if video or vis is in fullscreen mode
  IPC_SET_VIS_FS_FLAG = 631; // a vis should send this message with 1
                             // as param to notify winamp that it has gone to
                             // or has come back from fullscreen mode

  IPC_SHOW_NOTIFICATION = 632; {[Saivert] If the Default Winamp 5 Skin is loaded,
                                this will display the notification window.
                                Tip: Hook this in a subclass of Winamp to
                                intercept song and status changes (from
                                play->pause or stop->play, e.t.c).}

  IPC_GETSKININFO = 633; {[Saivert] Tip: Use this to detect if a Modern skin is
                          loaded and in use. If it returns 1, a classic skin
                          is in use. If it returns 0 a modern skin is used.
                          NOTE (new in rel. #3): This is higly unreliable and
                          may fail in a future version of Winamp. Don't
                          depend on it. }

// >>>>>>>>>>> Next is 634

  IPC_PLCMD  = 1000;

  PLCMD_ADD  = 0;
  PLCMD_REM  = 1;
  PLCMD_SEL  = 2;
  PLCMD_MISC = 3;
  PLCMD_LIST = 4;

  IPC_MBCMD  = 1001;

  MBCMD_BACK    = 0;
  MBCMD_FORWARD = 1;
  MBCMD_STOP    = 2;
  MBCMD_RELOAD  = 3;
  MBCMD_MISC  = 4;

  IPC_VIDCMD = 1002;

  VIDCMD_FULLSCREEN = 0;

  VIDCMD_1X         = 1;
  VIDCMD_2X         = 2;
  VIDCMD_LIB        = 3;
  VIDPOPUP_MISC     = 4;

  IPC_MBURL       = 1003; //sets the URL
  IPC_MBGETCURURL = 1004; //copies the current URL into wParam (have a 4096 buffer ready)
  IPC_MBGETDESC   = 1005; //copies the current URL description into wParam (have a 4096 buffer ready)
  IPC_MBCHECKLOCFILE = 1006; //checks that the link file is up to date (otherwise updates it). wParam=parent HWND
  IPC_MBREFRESH   = 1007; //refreshes the "now playing" view in the library
  IPC_MBGETDEFURL = 1008; //copies the default URL into wParam (have a 4096 buffer ready)

  IPC_STATS_LIBRARY_ITEMCNT = 1300; // updates library count status

// IPC 2000-3000 reserved for freeform messages, see gen_ff/ff_ipc.h
  IPC_FF_FIRST = 2000;
  IPC_FF_LAST  = 3000;

  IPC_GETDROPTARGET = 3001;

  IPC_PLAYLIST_MODIFIED = 3002; // sent to main wnd whenever the playlist is modified

  IPC_PLAYING_FILE = 3003; // sent to main wnd with the file as parm whenever a file is played
  IPC_FILE_TAG_MAY_HAVE_UPDATED = 3004; // sent to main wnd with the file as parm whenever a file tag might be updated


  IPC_ALLOW_PLAYTRACKING = 3007;
// send nonzero to allow, zero to disallow

  IPC_HOOK_OKTOQUIT = 3010; // return 0 to abort a quit, nonzero if quit is OK

  IPC_WRITECONFIG = 3011; // pass 2 to write all, 1 to write playlist + common, 0 to write common+less common

  IPC_GET_API_SERVICE = 3025; //
 // api_service* api_service = (api_service)SendMessage(hwnd_winamp,WM_WA_IPC,0,IPC_GET_API_SERVICE);
 // This api will return Winamp's api_service pointer (which is what Winamp3 used, heh).

// pass a string to be the name to register, and returns a value > 65536, which is a unique value you can use
// for custom WM_WA_IPC messages.

   IPC_GET_PLAYING_FILENAME = 3031;  // * Added at 2008-07-24
// returns wchar_t * of the currently playing filename

   IPC_GET_PLAYING_TITLE = 3034;     // * Added at 2008-07-24
// returns wchar_t * of the current title

  IPC_REGISTER_WINAMP_IPCMESSAGE = 65536;
{[Saivert] Check out included wa_hotkeys.pas for an example.}


(**************************************************************************)

(*
** Finally there are some WM_COMMAND messages that you can use to send
** Winamp misc commands.
**
** To send these, use:
**
** SendMessage(hwnd_winamp, WM_COMMAND,command_name,0);
*)

  WINAMP_OPTIONS_EQ               = 40036; // toggles the EQ window
  WINAMP_OPTIONS_PLEDIT           = 40040; // toggles the playlist window
  WINAMP_VOLUMEUP                 = 40058; // turns the volume up a little
  WINAMP_VOLUMEDOWN               = 40059; // turns the volume down a little
  WINAMP_FFWD5S                   = 40060; // fast forwards 5 seconds
  WINAMP_REW5S                    = 40061; // rewinds 5 seconds

// the following are the five main control buttons, with optionally shift 
// or control pressed
// (for the exact functions of each, just try it out)
  WINAMP_BUTTON1                  = 40044;
  WINAMP_BUTTON2                  = 40045;
  WINAMP_BUTTON3                  = 40046;
  WINAMP_BUTTON4                  = 40047;
  WINAMP_BUTTON5                  = 40048;
  WINAMP_BUTTON1_SHIFT            = 40144;
  WINAMP_BUTTON2_SHIFT            = 40145;
  WINAMP_BUTTON3_SHIFT            = 40146;
  WINAMP_BUTTON4_SHIFT            = 40147;
  WINAMP_BUTTON5_SHIFT            = 40148;
  WINAMP_BUTTON1_CTRL             = 40154;
  WINAMP_BUTTON2_CTRL             = 40155;
  WINAMP_BUTTON3_CTRL             = 40156;
  WINAMP_BUTTON4_CTRL             = 40157;
  WINAMP_BUTTON5_CTRL             = 40158;

  WINAMP_FILE_PLAY                = 40029; // pops up the load file(s) box
  WINAMP_FILE_DIR                 = 40187; // pops up the load directory box
  WINAMP_OPTIONS_PREFS            = 40012; // pops up the preferences
  WINAMP_OPTIONS_AOT              = 40019; // toggles always on top
  WINAMP_HELP_ABOUT               = 40041; // pops up the about box :)

  ID_MAIN_PLAY_AUDIOCD1           = 40323; // starts playing the audio CD in the first CD reader
  ID_MAIN_PLAY_AUDIOCD2           = 40323; // plays the 2nd
  ID_MAIN_PLAY_AUDIOCD3           = 40323; // plays the 3nd
  ID_MAIN_PLAY_AUDIOCD4           = 40323; // plays the 4nd

// IDs 42000 to 45000 are reserved for gen_ff
// IDs from 45000 to 57000 are reserved for library 

  { These ones were added by Saivert  }
  WINAMP_OPTIONS_ELAPSED          = 40037;
  WINAMP_OPTIONS_REMAINING        = 40038;


(*
** EOF.. Enjoy.
*)

(*
  [Saivert]
  The new feature where you can add your own page to the prefs in Winamp,
  only supports native Windows dialogs. So to add a TForm based window to
  the preferences dialog in winamp, you have to create a wrapper dialog.
  You can make such wrapper dialog by writing a resource script containing
  these lines:

    your_id DIALOG 0 0 144 154
    STYLE WS_CHILD | DS_CONTROL
    BEGIN
    END

  where your_id must be a number not used by any other DIALOG resource.
  Save this as a text file named something like "myprefs.rc" and save it
  in the same folder as your project.

  This file has to be compiled into a binary resource form.
  To do this type at the command prompt:
  (you must CD to your projects directory, and it assumes
  that your Delphi program directory is in the path)

    brcc32 myprefs.rc <ENTER>
  or if that didn't work...
    brc32 -r myprefs.rc <ENTER>

  This creates a file called "myprefs.res".

  Now it is time to import this resource into your Delphi project.
  Add a line anywhere in your source (e.g. your .DPR file) that looks like
  this:
    {$R myprefs.res}

  Now when you have set up a dialog resource and imported it into your project,
  it is time to write the code that actually make it work. Here it goes:

  Prepare your form for use in the prefs dialog. Be sure...
    ...that all controls fits within a rectangle of 287x309 in size.
    (Winamp 5 has a larger preferences dialog, so this may change.) 
    ...that the form is not loaded automatically at startup.
    E.g.: "Application.CreateForm(yourform, Tyourform)" does not exist.


  Now assuming your form is "yourform" write this dialog procedure:

    function myprefsDlgProc(dlg: HWND; msg: UINT;
      wParam: WPARAM; lParam: LPARAM): BOOL; stdcall;
    {very important with stdcall calling convention}
    var
      r: TRect;
    begin
      result := False;
      case msg of
        WM_INITDIALOG: begin
        { Make dlg fit in Winamp's preferences dialog }
          GetClientRect(GetNextWindow(dlg, GW_HWNDPREV), r);
          SetWindowPos(dlg, 0, r.Left, r.Top, r.Right, r.Bottom, SWP_NOZORDER);

          yourform := Tyourform.Create(nil);
          yourform.BorderStyle := bsNone;
          SetParent(yourform.Handle, dlg);
          yourform.SetBounds(0, 0, r.Right, r.Bottom);
        { Make sure the form behaves like it should (DS_CONTROL,WS_CHILD) }
          SetWindowLong(yourform.Handle, GWL_STYLE, WS_CHILD or DS_CONTROL);
          yourform.Show;
          result := False;
        end;
        WM_DESTROY: yourform.Free;
      end;
    end;

  Set up a TprefsDlgRec record (must be in a interface section):
    var
      myprefsRec: TprefsDlgRec;


  Fill the members of the record
  (do this in a Plugin initialization function):
    with myprefsrec do
    begin
      _hInst := HInstance; { or This_Mod^.hDLLInstance }
      dlgID := your_id; { set to id specified in resource script, must be a number }
      proc := @myprefsDlgProc;
      name := 'My Prefs'; {page name}
      where := 0; {0 means to place it under "Options". 1 means under "Plug-ins"...}
      { do not care about _id and/or _prefsDlgRec which is handled by Winamp }
    end;

  Now it is time to put it into winamp's preferences dialog, call this:
    SendMessage(hwnd_winamp,WM_WA_IPC,Integer(@myprefsrec),IPC_ADD_PREFS_DLG);

  That's it!
  PS! Any OnMouseEnter and OnMouseLeave events associated with VCL controls,
  won't be triggered because of compatibility issues, sorry!

*)

{ [Saivert] Here are some functions that makes your life easier. }

{ WASetEQData:
  Sets the band to the value (newvalue).
  Values range from 0 (+20dB) to 63 (-20db).
  Is Winamp version aware. Uses the new method for Winamp version 2.92+. }
function WASetEQData(mainwawnd: HWND; band, newvalue: Integer): Integer;

{ WAOpenURLBox:
  This function calls the IPC_OPENURLBOX for you so you don't have to.
  Basically pops up a small dialog box where you can type in an URL (the
  same one seen when you add a location to the playlist).
  Has a MRU (most recently used) feature. }
function WAOpenURLBox(mainwawnd: HWND; parentwnd: HWND; out url: string): Boolean;

{ WADownloadFile:
  Calls Winamp's internal HTTP download mechanism and displays a progress
  dialog. Everything is handled by this function. Just provide the URL and
  the destination filename. dlgtitle can be filled with the name/description
  of the download. Setting parentwnd ensures that the dialog will be modal
  to your window/dialog. }
function WADownloadFile(mainwawnd: HWND; url, filename, dlgtitle: string; parentwnd: HWND): Boolean;

{ WAFindSongPosition:
  Pass a song title and it scans through Winamp's playlist for "title".
  Returns the position in the playlist.
  If isfile is True, scans for a filename instead. }
function WAFindSongPosition(mainwawnd: HWND; title: string; isfile: Boolean): Integer;

{ WAGetPlaylistItem:
  Use this function to get an item in the playlist editor. Set getfile
  to true to get the filename instead of the title. Set index to the zero-based
  index of the item to retrieve.

  TIP: Set index to -1 to get the currently playing item.

  This function will retrive an empty string if the index passed is out
  of range (except if it is -1 which is a special case).
  
  This function was new in rel. #3 of the this SDK.
  Wraps the IPC_GETPLAYLISTFILE, IPC_GETPLAYLISTTITLE, IPC_GETLISTPOS and
  IPC_GETLISTLENGTH messages. }
function WAGetPlaylistItem(mainwawnd: HWND; getfile: Boolean; index: Integer): String;

{ WAAddFile:
  Adds the file (FilePath) to the playlist.
  Can be called from out-of-process. }
function WAAddFile(mainwawnd: HWND; FilePath: string): Integer;

{ WAChangeDir:
  Changes the current working directory for Winamp.
  Use it before a series of WAAddFile calls so you don't have to include
  the full path for all files. }
function WAChangeDir(mainwawnd: HWND; Path: string): Integer;

{ WAIsRightExe:
  Used by Winamp Agent. Read note above the implementation part
  of this function. }
function WAIsRightExe(mainwawnd: HWND; filename: string): Boolean;

{ This one is nice if you are calling from an external application,
  it simple retrieves the Winamp window title and strips the
  " - Winamp" suffix.
  You can get something like:  "12. Britney Spears - Toxic" }
function WAExtGetCurTitle(mainwawnd: HWND): string;

{ WAGetExtendedFileInfo:
  Calls Winamp's layer for retrieving metadata from input plugins that
  support this.
  Set filename to the file to query information about, metadata to the
  metadata item to retrieve (e.g.: "artist" or "title") and provide
  a string for value which will receieve the metadata.
  Wrapper for IPC_GET_EXTENDED_FILE_INFO }
function WAGetExtendedFileInfo(mainwawnd: HWND; filename: string; metadata: string; out value: string): Boolean;

{ SendMessageRetStr:
  This function can be used to send messages that
  return PChar (char* in C/C++) from external applications (not plug-ins)
  Tip: Use it to get title of song playing instead of WAExtGetCurTitle }
function SendMessageRetStr(wnd: HWND; uMsg: UINT; wParam: WPARAM; lParam: LPARAM): String;

implementation

{ pseudo "MACRO" }
function Video_MakeType(A, B, C, D: Integer): Integer;
begin
  result := ((A) or ((B) shr 8) or ((C) shr 16) or ((D) shr 24))
end;

function WASetEQData(mainwawnd: HWND; band, newvalue: Integer): Integer;
begin
  result := SendMessage(mainwawnd, WM_WA_IPC, band, IPC_GETEQDATA);
  { If we are working with a Winamp v2.92 or more recent we use the
    new way of setting the equalizer data.  }
  if SendMessage(mainwawnd, WM_WA_IPC, 0, IPC_GETVERSION) >= $2902 then
    SendMessage(mainwawnd, WM_WA_IPC,
      MAKEWPARAM(WORD(newvalue), MAKEWORD(band, $DB)),
      IPC_SETEQDATA)
  else
    SendMessage(mainwawnd, WM_WA_IPC, newvalue, IPC_SETEQDATA);
end;

function WAOpenURLBox(mainwawnd: HWND; parentwnd: HWND; out url: string): Boolean;
var
  hg: HGLOBAL;
  hp: PChar;
begin
  hg := SendMessage(mainwawnd, WM_WA_IPC, parentwnd, IPC_OPENURLBOX);
  hp := GlobalLock(hg);
  url := hp;
  GlobalFree(hg);
  result := hg > 0;
end;

function WADownloadFile(mainwawnd: HWND; url, filename, dlgtitle: string; parentwnd: HWND): Boolean;
var
  httpget: TFNhttpRetrieveFile;
begin
  httpget := TFNhttpRetrieveFile(SendMessage(mainwawnd, WM_WA_IPC, 0, IPC_GETHTTPGETTER));
  result := False;
  if @httpget <> nil then
    result := httpget(parentwnd, PChar(url), PChar(filename), PChar(dlgtitle)) = 0;
end;

function WAFindSongPosition(mainwawnd: HWND; title: string; isfile: Boolean): Integer;
var
  i: Integer;
  msg: Integer;
begin
  result := -1;

  if isfile then
    msg := IPC_GETPLAYLISTFILE
  else msg := IPC_GETPLAYLISTTITLE;

  for i := 0 to SendMessage(mainwawnd, WM_WA_IPC, 0, IPC_GETLISTLENGTH) do
  begin
    if lstrcmpi(PChar(SendMessage(mainwawnd, WM_WA_IPC, i, msg)), PChar(title)) = 0 then
    begin
      result := i;
      break;
    end;
  end;

end;

function WAGetPlaylistItem(mainwawnd: HWND; getfile: Boolean; index: Integer): String;
var
  msg,
  pos: Integer;
begin
  result := '';

  if getfile then
    msg := IPC_GETPLAYLISTFILE
  else msg := IPC_GETPLAYLISTTITLE;

  if (index >= 0) and (index < SendMessage(mainwawnd, WM_WA_IPC, 0, IPC_GETLISTLENGTH)) then
  begin
    pos := index;
  end
  else if index = -1 then
  begin
    pos := SendMessage(mainwawnd, WM_WA_IPC, 0, IPC_GETLISTPOS);
  end
  else
    Exit;

  result := PChar(SendMessage(mainwawnd, WM_WA_IPC, pos, msg));
end;

function int_cds(mainwawnd: HWND; text: string; msg: Integer): Integer;
var
  cds: COPYDATASTRUCT;
begin
 cds.dwData := msg;
 cds.lpData := PChar(text);
 cds.cbData := lstrlen(cds.lpData)+1; {include space for null char}
 result := SendMessage(mainwawnd, WM_COPYDATA, 0, LPARAM(@cds));
end;

function WAAddFile(mainwawnd: HWND; FilePath: string): Integer;
begin
  result := int_cds(mainwawnd, FilePath, IPC_PLAYFILE);
end;

function WAChangeDir(mainwawnd: HWND; Path: string): Integer;
begin
  result := int_cds(mainwawnd, Path, IPC_CHDIR);
end;

{ Pass a filename to winampa.exe (Winamp Agent) and it returns True if
  that is the right one. Can be used to check for a valid installation
  path for winamp. False is returned if file is not found or the
  winampa.exe is part of another installation of winamp not in use and/or
  running. Winamp Agent uses this to check for itself when you click the System
  Tray icon (so the right winamp is launched). That's what I think... }
function WAIsRightExe(mainwawnd: HWND; filename: string): Boolean;
begin
  int_cds(mainwawnd, filename, IPC_GETMODULENAME);
  result := SendMessage(mainwawnd, WM_WA_IPC, 0, IPC_EX_ISRIGHTEXE) > 0;
end;

function WAExtGetCurTitle(mainwawnd: HWND): string;
var
  len: Cardinal;
  p: PChar;
  this_title: PChar;
  tmp: array[0..8] of Char;
begin
  len := GetWindowTextLength(mainwawnd);
  GetMem(this_title, len);
  GetWindowText(mainwawnd, this_title, len);
  p := this_title+len-8;

  while p >= this_title do
  begin
    lstrcpyn(tmp, p, 9); {to avoid using StrLIComp}
    if lstrcmpi(tmp,'- Winamp') = 0 then Break;
    Dec(p);
  end;
  if p >= this_title then Dec(p);
  while (p >= this_title) and (p^ = #32) do Dec(p);
  Inc(p);
  p^ := #0;
  if lstrlen(this_title) > 0 then
    result := this_title
  else result := '';
  FreeMem(this_title, len);
end;

function WAGetExtendedFileInfo(mainwawnd: HWND; filename: string; metadata: string; out value: string): Boolean;
var
  efi: TextendedFileInfoStruct;
  retbuf: PChar;
begin
  efi.filename := PChar(filename);
  efi.metadata := PChar(metadata);
  GetMem(retbuf, 4096);
  efi.ret := retbuf;
  efi.retlen := 4096;
  result := False;
  if SendMessage(mainwawnd,WM_WA_IPC,Integer(@efi),IPC_GET_EXTENDED_FILE_INFO) > 0 then
  begin
    value := retbuf;
    result := True;
  end;
  FreeMem(retbuf, 4096);
end;

function SendMessageRetStr(wnd: HWND; uMsg: UINT; wParam: WPARAM; lParam: LPARAM): String;
var
  dwProcessId: DWORD;
  phandle: THandle;
  P: Pointer;
  C: Cardinal;
  PB: Pointer;
  B: Byte;
begin
  Result := '';
  GetWindowThreadProcessID(wnd, pointer(dwProcessId));
  phandle := OpenProcess(PROCESS_VM_READ, False, dwProcessId);
  if phandle = 0 then exit;
  P := Pointer(SendMessage(wnd, uMsg, wParam, lParam));
  PB := nil;
  B := 0;
  repeat
    if not ReadProcessMemory(phandle,P,@PB,1,C) then break;
    B := Byte(PB);
    if B <> 0 then begin
      Result := Result + Chr(B);
    end;
    P := Pointer(DWord(P)+1);
  until (B=0);

  CloseHandle(phandle);
end;

end.
