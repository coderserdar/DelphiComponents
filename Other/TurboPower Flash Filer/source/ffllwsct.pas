{*********************************************************}
{* FlashFiler: Winsock error string constants            *}
{*********************************************************}

(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is TurboPower FlashFiler
 *
 * The Initial Developer of the Original Code is
 * TurboPower Software
 *
 * Portions created by the Initial Developer are Copyright (C) 1996-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

{$I ffdefine.inc}

unit ffllwsct;

interface

{$I ffllwsct.inc}

{===Winsock error codes===}
const
  WsaNO_ADDRESS   = WSANO_DATA;
  Host_NOT_FOUND  = WSAHOST_NOT_FOUND;
  Try_AGAIN       = WSATRY_AGAIN;
  No_RECOVERY     = WSANO_RECOVERY;
  No_DATA         = WSANO_DATA;
  No_ADDRESS      = WSANO_ADDRESS;
  EWouldBLOCK     = WSAEWOULDBLOCK;
  EInPROGRESS     = WSAEINPROGRESS;
  EAlREADY        = WSAEALREADY;
  ENotSOCK        = WSAENOTSOCK;
  EDestADDRREQ    = WSAEDESTADDRREQ;
  EMsgSIZE        = WSAEMSGSIZE;
  EProtoTYPE      = WSAEPROTOTYPE;
  ENoPROTOOPT     = WSAENOPROTOOPT;
  EProtONOSUPPORT = WSAEPROTONOSUPPORT;
  ESockTNOSUPPORT = WSAESOCKTNOSUPPORT;
  EOpNOTSUPP      = WSAEOPNOTSUPP;
  EPfNOSUPPORT    = WSAEPFNOSUPPORT;
  EAfNOSUPPORT    = WSAEAFNOSUPPORT;
  EAddrINUSE      = WSAEADDRINUSE;
  EAddrNOTAVAIL   = WSAEADDRNOTAVAIL;
  ENetDOWN        = WSAENETDOWN;
  ENetUNREACH     = WSAENETUNREACH;
  ENetRESET       = WSAENETRESET;
  EConnABORTED    = WSAECONNABORTED;
  EConnRESET      = WSAECONNRESET;
  ENoBUFS         = WSAENOBUFS;
  EIsCONN         = WSAEISCONN;
  ENotCONN        = WSAENOTCONN;
  EShutDOWN       = WSAESHUTDOWN;
  ETooMANYREFS    = WSAETOOMANYREFS;
  ETimedOUT       = WSAETIMEDOUT;
  EConnREFUSED    = WSAECONNREFUSED;
  ELoop           = WSAELOOP;
  ENameTOOLONG    = WSAENAMETOOLONG;
  EHostDOWN       = WSAEHOSTDOWN;
  EHostUNREACH    = WSAEHOSTUNREACH;
  ENotEMPTY       = WSAENOTEMPTY;
  EProcLIM        = WSAEPROCLIM;
  EUsers          = WSAEUSERS;
  EDQuot          = WSAEDQUOT;
  EStale          = WSAESTALE;
  ERemote         = WSAEREMOTE;

implementation

end.
