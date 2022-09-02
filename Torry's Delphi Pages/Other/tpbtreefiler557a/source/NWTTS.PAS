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
 * The Original Code is TurboPower B-Tree Filer
 *
 * The Initial Developer of the Original Code is
 * TurboPower Software
 *
 * Portions created by the Initial Developer are Copyright (C) 1996-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Based in part on code written by Ralf Nagel
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

{--Conditional defines and compiler options}
  {$I BTDefine.Inc}
  {$B+,F-,I-,R-,S-,V-}
  {$IFDEF CanAllowOverlays}
    {$F+,O+,A-}
  {$ENDIF}
  {$IFDEF CanSetOvrflowCheck}
    {$Q-,P-}
  {$ENDIF}
  {all other compiler options are 'don't care'}

{$IFDEF Win32}
  !! Error - this unit cannot be compiled for 32-bit
{$ENDIF}

unit NWTTS;
  {-Unit to provide NetWare TTS functionality.}

{Note: major changes for 5.51 to support problem adapters.}

interface

uses
  NWBase;

function nwTTSAbort(Server : TnwServer) : TnwErrorCode;
  {-Abort (rollback) the current transaction.}

function nwTTSAvailable(Server : TnwServer) : Boolean;
  {-Return true if TTS is available.}

function nwTTSBegin(Server : TnwServer) : TnwErrorCode;
  {-Start a new transaction.}

function nwTTSDisable(Server : TnwServer) : Boolean;
  {-Disable TTS. Returns true if TTS was successfully disabled.}

function nwTTSEnable(Server : TnwServer) : Boolean;
  {-Enable TTS. Returns true if TTS was successfully enabled.}

function nwTTSEnd(Server : TnwServer; var ID : nwLong) : TnwErrorCode;
  {-End (commit) the current transaction.}

function nwTTSIsCommitted(Server : TnwServer; ID : nwLong) : Boolean;
  {-Return true if transaction given by ID has been committed to disk.}


{---Extra TTS routines---}
procedure nwTTSGetAppThresh(Server : TnwServer; var Logical, Physical : byte);
  {-Return the application's logical and physical lock threshold values
    for implicit transaction tracking to begin.}

procedure nwTTSGetWSThresh(Server : TnwServer; var Logical, Physical : byte);
  {-Return the workstation's logical and physical lock threshold values
    for implicit transaction tracking to begin.}

function nwTTSSetAppThresh(Server : TnwServer; Logical, Physical : byte) : TnwErrorCode;
  {-Set the application's logical and physical lock threshold values
    for implicit transaction tracking to begin.}

function nwTTSSetWSThresh(Server : TnwServer; Logical, Physical : byte) : TnwErrorCode;
  {-Set the workstation's logical and physical lock threshold values
    for implicit transaction tracking to begin.}

implementation

type
  OS = record O, S : word; end;                                 {!!.51}


function nwTTSAbort(Server : TnwServer) : TnwErrorCode;
  var
    Request : byte;
    Dummy   : byte;
  begin
    {API: TTS Abort Transaction}
    Request := $03;
    nwTTSAbort := nwServerCall(Server, $22,                       {!!.51}
                               sizeof(Request), Request, 0, Dummy);
  end;

function nwTTSAvailable(Server : TnwServer) : Boolean;
  var
    Request : byte;
    Dummy   : byte;
  begin
    {API: TTS Is Available}
    Request := $00;
    nwTTSAvailable := nwServerCall(Server, $22,                   {!!.51}
                                   sizeof(Request), Request, 0, Dummy) = $89FF;
  end;

function nwTTSBegin(Server : TnwServer) : TnwErrorCode;
  var
    Request : byte;
    Dummy   : byte;
  begin
    {API: TTS Begin Transaction}
    Request := $01;
    nwTTSBegin := nwServerCall(Server, $22,                       {!!.51}
                               sizeof(Request), Request, 0, Dummy);
  end;

function nwTTSEnd(Server : TnwServer; var ID : nwLong) : TnwErrorCode;
  var
    Request : byte;
    Reply : record
      EndID : longint;
      Dummy : byte;
    end;
    Status  : word;
  begin
    {Note: it has been discovered that nwTTSEnd will only work
           on some adapters if the reply packet is one more than
           is really required. Rewritten for !!.51}
    {API: TTS End Transaction}
    Request := $02;
    ID := 0;
    FillChar(Reply, sizeof(Reply), 0);
    Status := nwServerCall(Server, $22,                         {!!.51}
                           sizeof(Request), Request,
                           sizeof(Reply), Reply);
    if (Status = 0) then
      ID := Reply.EndID;
    nwTTSEnd := Status;
  end;

function nwTTSIsCommitted(Server : TnwServer; ID : nwLong) : Boolean;
  var
    Status : word;
    Request : record
      Func : byte;
      TTSID: nwLong;
    end;
    Dummy   : byte;
  begin
    {API: TTS Transaction Status}
    Request.Func := $04;
    Request.TTSID := ID;
    Status := nwServerCall(Server, $22, sizeof(Request), Request, {!!.51}
                                        0, Dummy);
    nwTTSIsCommitted := (Status = 0);
  end;

function nwTTSDisable(Server : TnwServer) : Boolean;
  var
    Request : record
      Len  : word;
      Func : byte;
    end;
    Dummy   : byte;
  begin
    {API: Disable Transaction Tracking}
    Request.Len := 1;
    Request.Func := $CF;
    nwTTSDisable := nwServerCall(Server, $17, sizeof(Request), Request,
                                              0, Dummy) = 0;      {!!.51}
  end;

function nwTTSEnable(Server : TnwServer) : Boolean;
  var
    Request : record
      Len  : word;
      Func : byte;
    end;
    Dummy   : byte;
  begin
    {API: Enable Transaction Tracking}
    Request.Len := 1;
    Request.Func := $D0;
    nwTTSEnable := nwServerCall(Server, $17, sizeof(Request), Request,
                                             0, Dummy) = 0;       {!!.51}
  end;

{---Extra TTS routines---}
procedure nwTTSGetAppThresh(Server : TnwServer; var Logical, Physical : byte);
  var
    Reply : record
      Log, Phy : byte;
    end;
    Request : byte;
  begin
    {API: TTS Get Application Thresholds}
    Request := $05;
    if nwServerCall(Server, $22, sizeof(Request), Request,        {!!.51}
                                 sizeof(Reply), Reply) = 0 then
      begin
        Logical := Reply.Log;
        Physical := Reply.Phy;
      end
    else
      begin
        Logical := 0;
        Physical := 0;
      end
  end;

procedure nwTTSGetWSThresh(Server : TnwServer; var Logical, Physical : byte);
  var
    Reply : record
      Log, Phy : byte;
    end;
    Request : byte;
  begin
    {API: TTS Get Workstation Thresholds}
    Request := $07;
    if nwServerCall(Server, $22, sizeof(Request), Request,        {!!.51}
                                 sizeof(Reply), Reply) = 0 then
      begin
        Logical := Reply.Log;
        Physical := Reply.Phy;
      end
    else
      begin
        Logical := 0;
        Physical := 0;
      end
  end;

function nwTTSSetAppThresh(Server : TnwServer; Logical, Physical : byte) : TnwErrorCode;
  var
    Request : record
      Func : byte;
      Log, Phy : byte;
    end;
    Dummy   : byte;
  begin
    {API: TTS Set Application Thresholds}
    Request.Func := $06;
    Request.Log := Logical;
    Request.Phy := Physical;
    nwTTSSetAppThresh := nwServerCall(Server, $22, sizeof(Request), Request,
                                                   0, Dummy);     {!!.51}
  end;

function nwTTSSetWSThresh(Server : TnwServer; Logical, Physical : byte) : TnwErrorCode;
  var
    Request : record
      Func : byte;
      Log, Phy : byte;
    end;
    Dummy   : byte;
  begin
    {API: TTS Set Workstation Thresholds}
    Request.Func := $08;
    Request.Log := Logical;
    Request.Phy := Physical;
    nwTTSSetWSThresh := nwServerCall(Server, $22, sizeof(Request), Request,
                                                  0, Dummy);      {!!.51}
  end;

end.
