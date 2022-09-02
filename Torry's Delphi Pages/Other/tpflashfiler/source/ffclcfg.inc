{*********************************************************}
{* FlashFiler: Client configuration include file         *}
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


const

{ Client's retry timeout affects the amount of time that the server should
  spend attempting to process a message which is blocked by an active
  transaction or lock.  This value is passed to the server in the message
  header.  *** Not yet used ***}
  ffclRetryTimeout : longint = 10000;

{ to specify the server name to connect to}
  ffclServerName : TffNetAddress = '';

{ to programmatically specify username and password, change these from blank}
  ffclUsername : TffName = '';
  ffclPassword : TffName = '';

{ number of allowable client login retries}
  ffclLoginRetries : Byte = 3;

{ To select a default protocol for all apps.  This protocol is used for any
  Client Session.}

                        { valid choices: TffTCPIPProtocol,
                                         TffNetBIOSProtocol,
                                         TffIPXSPXProtocol,
                                         TffSingleUserProtocol
                                         TffDirectProtocol}
  ffclProtocol : TffCommsProtocolClass = TffSingleUserProtocol;
