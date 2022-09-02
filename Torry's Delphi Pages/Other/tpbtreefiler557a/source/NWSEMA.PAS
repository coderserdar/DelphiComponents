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


{!!.51 Note: because of changes to NWBASE and because of errors in Novell's
       Client API documentation, this unit has been drastically simplified.
       Hence there are no more !!.51 markers}

{$IFDEF Win32}
  !! Error - this unit cannot be compiled for 32-bit
{$ENDIF}

unit NWSema;
  {-NetWare semaphore API calls}

interface

uses
  {$IFDEF Windows}
   {$IFDEF Ver80}
   SysUtils,
   {$ELSE}
   Strings,
   {$ENDIF}
  {$ENDIF}
  nwBase,
  nwConn;

const
  {Semaphore error constants}
  nwsErrInvValue  = $7F21;  {Negative initial value on open operation}
  nwsErrInvName   = $7F22;  {Invalid semaphore name on open operation, eg null}
  nwsErrInvHandle = $89FF;  {Invalid semaphore handle passed to routine}
  nwsErrTimeOut   = $897F;  {Timeout on nwDecSema}
  nwsErrOverflow  = $8901;  {Overflow on nwIncSema}

type
  {A semaphore name string}
  TnwSemaName  = string[127];

function nwCloseSema(Server : TnwServer; Handle : nwLong) : TnwErrorCode;
  {-Close a semaphore.
    Notes: Server is the server's handle for the semaphore.
    Handle must have been obtained from nwOpenSema. This
    function decrements the open count for the semaphore, when it
    reaches zero the semaphore is deleted. The function returns
    0 or nwsErrInvHandle.}

function nwDecSema(Server : TnwServer; Handle  : nwLong;
                   TimeOut : word) : TnwErrorCode;
  {-Decrement the value of a semaphore.
    Notes: Server is the ID of the server for the semaphore.
    Handle must have been obtained from nwOpenSema. This
    function decrements the value of the semaphore. If the result
    is >= 0, the function returns 0. If negative, the
    workstation is put in a queue until another workstation
    increments the semaphore or TimeOut ticks go by. If the former
    the function returns 0, if the latter it reincrements
    the value and returns nwsErrTimeout.
    Known in the Novell literature as WaitOnSemaphore}

function nwExamineSema(Server        : TnwServer;
                       Handle        : nwLong;
                   var Value     : nwInt;
                   var OpenCount : word) : TnwErrorCode;
  {-Return the current value and the open count of a semaphore.
    Notes: Server is the ID of the server for the semaphore.
    Handle must have been obtained from nwOpenSema.}

function nwIncSema(Server : TnwServer; Handle : nwLong) : TnwErrorCode;
  {-Increment the value of a semaphore.
    Notes: Server is the ID of the server for the semaphore.
    Handle must have been obtained from nwOpenSema. If the
    value of the semaphore is 127, this function returns immediately
    with nwsErrOverflow, otherwise it adds 1 to the value and returns
    with 0.
    Known in the Novell literature as SignalSemaphore}

function nwOpenSema(Server       : TnwServer;
                    Name         : TnwSemaName;
                    InitialValue : nwInt;
                var OpenCount: word;
                var Handle   : nwLong) : TnwErrorCode;
  {-Open or create a semaphore.
    Notes: Server must be the server ID where the semaphore
    resides or will reside when created. Name will label the semaphore
    if it does not exist, or it is the (case insignificant) name of a
    currently open semaphore. If the semaphore is being created
    InitialValue must be in the range 0..127. The number of stations
    (including the caller's) will be returned in OpenCount, and the
    semaphore's handle which will be used in subsequent semaphore
    calls will be returned in Handle. The function returns
    0, nwsErrInvName or nwsErrInvValue.}

implementation

type
  LH = record L, H : word; end; {for splitting nwLong into hi/lo words}
  OS = record O, S : word; end; {for splitting pointer into seg/sel & ofs}

const
  nwsErrTimeOut2 = $89FE;

function nwCloseSema(Server : TnwServer; Handle : nwLong) : TnwErrorCode;
  var
    Request : record
      Func : byte;
      Hndl : nwLong;
    end;
    Dummy   : byte;
  begin
    {API: Close Semaphore}
    Request.Func := $04;
    Request.Hndl := Handle;
    nwCloseSema := nwServerCall(Server, $20, sizeof(Request), Request,
                                             0, Dummy);
  end;

function nwDecSema(Server : TnwServer; Handle  : nwLong;
                   TimeOut : word) : TnwErrorCode;
  var
    Request : record
      Func : byte;
      Hndl : nwLong;
      TOut : word;
    end;
    Dummy   : byte;
    Status : word;
  begin
    {API: Wait On Semaphore}
    Request.Func := $02;
    Request.Hndl := Handle;
    Request.TOut := swap(TimeOut);
    Status := nwServerCall(Server, $20, sizeof(Request), Request, 0, Dummy);
    if (Status = nwsErrTimeOut2) then
      Status := nwsErrTimeOut;
    nwDecSema := Status;
  end;

function nwExamineSema(Server : TnwServer;
                       Handle        : nwLong;
                   var Value     : nwInt;
                   var OpenCount : word) : TnwErrorCode;
  var
    Request : record
      Func : byte;
      Hndl : nwLong;
    end;
    Reply : record
      V : shortint;
      O : byte;
    end;
    Status : word;
  begin
    {API: Examine Semaphore}
    Request.Func := $01;
    Request.Hndl := Handle;
    Status := nwServerCall(Server, $20, sizeof(Request), Request,
                                        sizeof(Reply), Reply);
    if (Status = 0) then
      begin
        Value := Reply.V;
        OpenCount := Reply.O;
      end
    else
      begin
        Value := 0;
        OpenCount := 0;
      end;
    nwExamineSema := Status;
  end;

function nwIncSema(Server : TnwServer; Handle : nwLong) : TnwErrorCode;
  var
    Request : record
      Func : byte;
      Hndl : nwLong;
    end;
    Dummy   : byte;
  begin
    {API: Signal Semaphore}
    Request.Func := $03;
    Request.Hndl := Handle;
    nwIncSema := nwServerCall(Server, $20, sizeof(Request), Request, 0, Dummy);
  end;

function nwOpenSema(Server       : word;
                    Name         : TnwSemaName;
                    InitialValue : nwInt;
                var OpenCount: word;
                var Handle   : nwLong) : TnwErrorCode;
  var
    Request : record
      Func : byte;
      Init : byte;
      SNam : string;
    end;
    Reply : record
      Hndl : nwLong;
      Open : byte;
    end;
    Status : word;
  begin
    {API: Open Semaphore}
    if (InitialValue < 0) or (InitialValue > 127) then
      begin
        nwOpenSema := nwsErrInvValue;
        Exit;
      end;
    if (length(Name) = 0) then
      begin
        nwOpenSema := nwsErrInvName;
        Exit;
      end;
    with Request do
      begin
        Func := $00;
        Init := InitialValue;
        SNam := Name;
        nwUpperStr(SNam);
      end;
    Status := nwServerCall(Server, $20, sizeof(Request), Request,
                                        sizeof(Reply), Reply);
    if (Status = 0) then
      begin
        OpenCount := Reply.Open;
        Handle := Reply.Hndl;
      end;
    nwOpenSema := Status;
  end;

end.
