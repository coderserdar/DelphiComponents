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

{===Winsock error codes===}
const
  WsaBASEERR = 10000;

  { Windows Sockets definitions of regular Microsoft C error constants }
  WsaEINTR  = 10004;
  WsaEBADF  = 10009;
  WsaEACCES = 10013;
  WsaEFAULT = 10014;
  WsaEINVAL = 10022;
  WsaEMFILE = 10024;

{ Windows Sockets definitions of regular Berkeley error constants }
  WsaEWOULDBLOCK     = 10035;
  WsaEINPROGRESS     = 10036;
  WsaEALREADY        = 10037;
  WsaENOTSOCK        = 10038;
  WsaEDESTADDRREQ    = 10039;
  WsaEMSGSIZE        = 10040;
  WsaEPROTOTYPE      = 10041;
  WsaENOPROTOOPT     = 10042;
  WsaEPROTONOSUPPORT = 10043;
  WsaESOCKTNOSUPPORT = 10044;
  WsaEOPNOTSUPP      = 10045;
  WsaEPFNOSUPPORT    = 10046;
  WsaEAFNOSUPPORT    = 10047;
  WsaEADDRINUSE      = 10048;
  WsaEADDRNOTAVAIL   = 10049;
  WsaENETDOWN        = 10050;
  WsaENETUNREACH     = 10051;
  WsaENETRESET       = 10052;
  WsaECONNABORTED    = 10053;
  WsaECONNRESET      = 10054;
  WsaENOBUFS         = 10055;
  WsaEISCONN         = 10056;
  WsaENOTCONN        = 10057;
  WsaESHUTDOWN       = 10058;
  WsaETOOMANYREFS    = 10059;
  WsaETIMEDOUT       = 10060;
  WsaECONNREFUSED    = 10061;
  WsaELOOP           = 10062;
  WsaENAMETOOLONG    = 10063;
  WsaEHOSTDOWN       = 10064;
  WsaEHOSTUNREACH    = 10065;
  WsaENOTEMPTY       = 10066;
  WsaEPROCLIM        = 10067;
  WsaEUSERS          = 10068;
  WsaEDQUOT          = 10069;
  WsaESTALE          = 10070;
  WsaEREMOTE         = 10071;

  WsaEDISCON         = 10101;

  { Extended Windows Sockets error constant definitions }
  WsaSYSNOTREADY     = 10091;
  WsaVERNOTSUPPORTED = 10092;
  WsaNOTINITIALISED  = 10093;

  { Error return codes from gethostbyname() and gethostbyaddr() (when using the
    resolver). Note that these errors are retrieved via WsaGetLastError() and
    must therefore follow the rules for avoiding clashes with error numbers from
    specific implementations or language run-time systems.  For this reason the
    codes are based at WsaBASEERR+1001. Note also that [Wsa]NO_ADDRESS is defined
    only for compatibility purposes. }

  { Authoritative Answer: Host not found }
  WsaHOST_NOT_FOUND = 11001;

  { Non-Authoritative: Host not found, or SERVERFAIL }
  WsaTRY_AGAIN = 11002;

  { Non recoverable errors, FORMERR, REFUSED, NOTIMP }
  WsaNO_RECOVERY = 11003;

  { Valid name, no data record of requested type }
  WsaNO_DATA = 11004;


