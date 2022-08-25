{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Description:  This demo show how to use the TPing object to ping any host.
Creation:     November 30, 1997
Version:      8.64
EMail:        http://www.overbyte.be        francois.piette@overbyte.be
Support:      https://en.delphipraxis.net/forum/37-ics-internet-component-suite/
Legal issues: Copyright (C) 1999-2020 by François PIETTE
              Rue de Grady 24, 4053 Embourg, Belgium.
              <francois.piette@overbyte.be>

              This software is provided 'as-is', without any express or
              implied warranty.  In no event will the author be held liable
              for any  damages arising from the use of this software.

              Permission is granted to anyone to use this software for any
              purpose, including commercial applications, and to alter it
              and redistribute it freely, subject to the following
              restrictions:

              1. The origin of this software must not be misrepresented,
                 you must not claim that you wrote the original software.
                 If you use this software in a product, an acknowledgment
                 in the product documentation would be appreciated but is
                 not required.

              2. Altered source versions must be plainly marked as such, and
                 must not be misrepresented as being the original software.

              3. This notice may not be removed or altered from any source
                 distribution.

              4. You must register this software by sending a picture postcard
                 to the author. Use a nice stamp and mention your name, street
                 address, EMail address and any comment you like to say.


Updates:
Dec 13, 1997 V1.01 Use the new OnEchoRequest and OnEchoReply events.
Dec 26, 1998 V1.02 Changed event handler for new TPing version (1.10)
Nov 10, 2002 V1.03 Added Reply.Status in display when failed
                   Changed argument name from Error to Status in EchoReply
                   event (same change has in component).
Mar 26, 2006 V6.00 Created new version 6.
Feb 20, 2013 V8.02 Angus - pings IPv4 or IPv6 addresses or host names
                   Added list box of IPv4 and IPv6 address and hosts to ping
                   Added Edit box to add new Host to List
                   Using new ReplyXXX properties
                   Report DNS lookup errors
                   Allow Socket Family to be selected for IPv4 or IPv6 only
                   Allow Source IP addresses to be selected
                   Added Ping (Async) - WARNING does not yet work
                   Added Ping (Thread) using TPingThread
                   Added Ping All to ping a list of addresses using TPingThread
                   Added Trace Route to list a route using TPingThread
Mar 10, 2020  V8.64 Added support for International Domain Names for Applications
                     (IDNA), i.e. using accents and unicode characters in domain names.


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsPingTst1;

{$I Include\OverbyteIcsDefs.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Forms, StdCtrls, Controls, ExtCtrls,
  OverbyteIcsWndControl, OverbyteIcsWSocket, OverbyteIcsWinsock,
  OverbyteIcsPing, OverbyteIcsIcmp;

const
  PingTestVersion    = 864;
  CopyRight : String = ' PingTest (c) 1997-2020 Francois Piette  V8.64 ';

type
  TPingTstForm = class(TForm)
    Ping1: TPing;
    DisplayMemo: TMemo;
    Panel1: TPanel;
    doPingSync: TButton;
    doAbort: TButton;
    doPingThread: TButton;
    doPingAllThread: TButton;
    doTraceRoute: TButton;
    SockFamily: TComboBox;
    SrcIpV4: TComboBox;
    SrcIpV6: TComboBox;
    doPingAsync: TButton;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    HostNames: TListBox;
    NewHost: TEdit;
    doNewHost: TButton;
    doExit: TButton;
    procedure doPingSyncClick(Sender: TObject);
    procedure Ping1DnsLookupDone(Sender: TObject; Error: Word);
    procedure doAbortClick(Sender: TObject);
    procedure Ping1EchoRequest(Sender: TObject; Icmp: TObject);
    procedure Ping1EchoReply(Sender: TObject; Icmp: TObject; Status: Integer);
    procedure doPingAsyncClick(Sender: TObject);
    procedure doPingThreadClick(Sender: TObject);
    procedure doTraceRouteClick(Sender: TObject);
    procedure doNewHostClick(Sender: TObject);
    procedure HostNamesDblClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure doExitClick(Sender: TObject);
  private
    { Private declarations }
    procedure SetButtons (Enable: Boolean);
    procedure PingThreadTermPing(Sender: TObject);
    procedure PingThreadTermTrace (Sender: TObject);
  end;

   var
    PingTstForm: TPingTstForm;

  // following for trace route demo
  const
    TraceMax = 32 ;
    MaxErrors = 8 ;
    MaskResponse1 = 'Ping of %d bytes took %d msecs' ;
    MaskResponse2 = '%2d  %4dms  %-24s  %s' ;
  var
    TraceAddr: array [1..TraceMax] of string ;
    ThreadIds: array [-1..TraceMax] of integer ;
    Trace1st: integer ;       // TMemo line for first trace
    TraceErrs: integer ;
    TraceSocketFamily: TSocketFamily;
    TraceIPAddr: string ;
    TraceDoneFlag: boolean ;
    RevLook1st: integer ;     // TMemo line for first reverse lookup
    StopFlag: boolean ;
    PendingPings: integer ;

implementation

{$R *.DFM}

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPingTstForm.FormShow(Sender: TObject);
var
    I: integer;
begin
    if IsIPv6Available then begin
        SrcIpV6.Items := LocalIPList(sfIPv6);
        if SrcIpV6.Items.Count > 0 then begin   //  found some IPv6 local IP addresses
            SrcIpV6.Items.Insert(0, 'Any');
            SrcIpV6.ItemIndex := 0;
            for I := 0 to Length(SocketFamilyNames)-1 do
                            SockFamily.Items.Add(SocketFamilyNames[TSocketFamily(I)]);
        end
        else
            SockFamily.Items.Add(SocketFamilyNames[sfIPv4]);
    end
    else
        SockFamily.Items.Add(SocketFamilyNames[sfIPv4]);
    SockFamily.ItemIndex := 0;
    SrcIpV4.Items := LocalIPList(sfIPv4);
    SrcIpV4.Items.Insert(0, 'Any');
    SrcIpV4.ItemIndex := 0;
    HostNames.ItemIndex := 0;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPingTstForm.HostNamesDblClick(Sender: TObject);
begin
    if HostNames.ItemIndex >= 0 then HostNames.DeleteSelected;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPingTstForm.SetButtons (Enable: Boolean);
begin
    doPingSync.Enabled   := Enable;
    doPingAsync.Enabled   := Enable;
    doPingThread.Enabled   := Enable;
    doPingAllThread.Enabled   := Enable;
    doTraceRoute.Enabled   := Enable;
    doAbort.Enabled := NOT Enable;

end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPingTstForm.doPingThreadClick(Sender: TObject);
var
    I, First, Total: integer ;
begin
    if HostNames.ItemIndex < 0 then exit;
    SetButtons (false);
    Total := HostNames.Items.Count ;
    if Total = 0 then exit ;
    First := 0;
    DisplayMemo.Lines.Add('');
    if Sender <> doPingAllThread then begin // only a single host
        First := HostNames.ItemIndex;
        Total := First + 1 ;
    end;
    for I := First to Total - 1 do begin
        if HostNames.Items [I] <> '' then begin
            with TPingThread.Create (True) do   // create suspended
            begin
             //   PingAddThread (ThreadId) ;      // keep threadid so it's freed
                FreeOnTerminate := True;
                PingId := succ (I) ;            // keep track of the results
                OnTerminate := PingThreadTermPing ;    // where we get the response
                PingSrcAddress := '';  // optionally specifiy source address for interface to ping from, or leave blank
                if SrcIpV4.ItemIndex > 0 then PingSrcAddress := SrcIpV4.Items [SrcIpV4.ItemIndex];
                PingSrcAddress6 := '';
                if SrcIpV6.ItemIndex > 0 then PingSrcAddress6 := SrcIpV6.Items [SrcIpV6.ItemIndex];
                PingSocketFamily := TSocketFamily(SockFamily.ItemIndex);
                PingHostName := HostNames.Items [I] ;  // host name or IP address to ping
                PingTimeout := 4000 ;           // ms
                PingTTL := 32 ;                 // hops
                PingLookupReply := false ;      // don't need response host name lookup
                {$IF CompilerVersion < 21}
                    Resume;    // start it now
                {$ELSE}
                    Start;
                {$IFEND}
            end ;
        end;
    end ;

end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPingTstForm.doTraceRouteClick(Sender: TObject);
var
    TraceHost, SourceAddr, SourceAddr6, Source: string;
    I: integer;
    EndTimer, Timeout: longword;
begin
    if HostNames.ItemIndex < 0 then exit;
    TraceHost := HostNames.Items[HostNames.ItemIndex];
    if TraceHost = '' then exit ;
    SetButtons (false);
    try // finally
    try // except
        StopFlag := false ;
        TraceDoneFlag := false ;
        DisplayMemo.Lines.Add ('') ;
        DisplayMemo.Lines.Add ('Trace Route to: ' + tracehost) ;
        Trace1st := DisplayMemo.Lines.Count - 1 ; // see where next line will be added
        TraceErrs := 0 ;
        timeout := 4000 ; // 4 seconds
        PendingPings := 0 ;
        for I := 1 to TraceMax do begin
            TraceAddr [I] := '';
            ThreadIds [I] := 0;
        end;
        SourceAddr := '';  // optionally specifiy source address for interface to ping from, or leave blank
        if SrcIpV4.ItemIndex > 0 then SourceAddr := SrcIpV4.Items [SrcIpV4.ItemIndex];
        SourceAddr6 := '';
        if SrcIpV6.ItemIndex > 0 then SourceAddr6 := SrcIpV6.Items [SrcIpV6.ItemIndex];
        if SockFamily.ItemIndex < 0 then SockFamily.ItemIndex := 0;
        TraceSocketFamily := TSocketFamily(SockFamily.ItemIndex);

    // ping host, need IP address first
        TraceIPAddr := '' ;
        with TPingThread.Create (True) do begin
            FreeOnTerminate := True;
            PingId := -1 ;  // flag to indicate checking host
            ThreadIds [PingId] := ThreadId;
            OnTerminate := PingThreadTermTrace ;
            PingSrcAddress := SourceAddr;
            PingSrcAddress6 := SourceAddr6;
            PingSocketFamily := TraceSocketFamily;
            PingHostName := TraceHost ;
            PingTimeout := timeout ;  // ms
            PingTTL := TraceMax ;     //  final hop
            PingLookupReply := false ;
            {$IF CompilerVersion < 21}
                Resume;    // start it now
            {$ELSE}
                Start;
            {$IFEND}
            inc (PendingPings) ;
        end ;

    // wait for host ping to finish, give up if no IP address found
        EndTimer := GetTickCount + timeout + 1000 ;
        while (PendingPings > 0) {and  (NOT StopFlag)} do
        begin
            Application.ProcessMessages ;
            if GetTickCount > EndTimer then break ;
        end ;
        if TraceIPAddr = '' then exit ;  // no address to ping
        if TraceSocketFamily = sfIPv6 then begin
            Source := SourceAddr6;
            if Source = '' then begin  // search first non-local address
                for I := 1 to SrcIpV4.Items.Count - 1 do begin
                    if Pos ('fe', SrcIpV6.Items [I]) = 0 then begin
                        Source := SrcIpV6.Items [I];
                        break ;
                    end;
                end;
            end;
         end
        else begin
            Source := SourceAddr;
            if Source = '' then Source := SrcIpV4.Items [1];  // first
       end;
        DisplayMemo.Lines.Add (Format (MaskResponse2,
                         [1, 0, Source, Lowercase (String(LocalHostName))])) ;

    // start all pings
        Trace1st := DisplayMemo.Lines.Count - 1 ; // see where next line will be added
        for I := 1 to TraceMax do
        begin
            with TPingThread.Create (True) do
            begin
                FreeOnTerminate := True;
                PingId := I ;
                ThreadIds [PingId] := ThreadId;
                OnTerminate := PingThreadTermTrace ;
                PingSrcAddress := SourceAddr;
                PingSrcAddress6 := SourceAddr6;
                PingSocketFamily := TraceSocketFamily;
                PingHostName := TraceIPAddr ;
                PingTimeout := timeout ;  // ms
                PingTTL := I ;           //  increasing TTL for each hop
                PingLookupReply := true ;
                {$IF CompilerVersion < 21}
                    Resume;    // start it now
                {$ELSE}
                    Start;
                {$IFEND}
                inc (PendingPings) ;
         // half second before next hop, unless ping finishes earlier
         // also restrict total threads, too many slows things down
                EndTimer := GetTickCount + 500 ;
                while (PendingPings > 0) do
                begin
                    Application.ProcessMessages ;
                    if (GetTickCount > EndTimer) and (PendingPings < 6) then break ;
                end ;
                if StopFlag then break ;
                if TraceDoneFlag then break ;  // reached host or too many errors
            end ;
        end ;

    // wait for pings to finish
        EndTimer := GetTickCount + 30000 ;   // 30 seconds
        while (PendingPings > 0) and (NOT StopFlag) do
        begin
            Application.ProcessMessages ;
            if GetTickCount > EndTimer then break ;
        end ;

    // not finished, terminate them cleanly
        if (PendingPings > 0) then
        begin
            for I := -1 to TraceMax do begin
                 if ThreadIds [I] > 0 then
                     PostThreadMessage (ThreadIds [I], WM_QUIT, 0, 0);  // terminate thread
            end ;
        end ;
        if StopFlag then DisplayMemo.Lines.Add ('Stopped by User') ;

    DisplayMemo.Lines.Add ('Trace Route Completed') ;
    beep ;
    except
        DisplayMemo.Lines.Add ('Error Sending Pings') ;
        beep ;
    end ;
    finally
        DisplayMemo.Lines.Add ('') ;
        SetButtons (true);
    end ;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// add a new host to the top of the ListBox
procedure TPingTstForm.doExitClick(Sender: TObject);
begin
    Close;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPingTstForm.doNewHostClick(Sender: TObject);
begin
    if NewHost.Text <> ''  then begin
        HostNames.Items.Insert (0, NewHost.Text);
        HostNames.ItemIndex := 0;
    end;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPingTstForm.doPingAsyncClick(Sender: TObject);
begin
    if HostNames.ItemIndex < 0 then exit;
    SetButtons (false);

    { V8.02 optionally specifiy source address for interface to ping from, or leave blank }
    Ping1.SrcAddress := '';
    if SrcIpV4.ItemIndex > 0 then Ping1.SrcAddress := SrcIpV4.Items [SrcIpV4.ItemIndex];
    Ping1.SrcAddress6 := '';
    if SrcIpV6.ItemIndex > 0 then Ping1.SrcAddress6 := SrcIpV6.Items [SrcIpV6.ItemIndex];

    Ping1.Address := HostNames.Items[HostNames.ItemIndex];
    Ping1.SocketFamily := TSocketFamily(SockFamily.ItemIndex);
    Ping1.PingAsync;   // warning PingAsync does not work yet!!
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPingTstForm.doPingSyncClick(Sender: TObject);
begin
    if HostNames.ItemIndex < 0 then exit;
    SetButtons (false);

    { V8.02 optionally specifiy source address for interface to ping from, or leave blank }
    Ping1.SrcAddress := '';
    if SrcIpV4.ItemIndex > 0 then Ping1.SrcAddress := SrcIpV4.Items [SrcIpV4.ItemIndex];
    Ping1.SrcAddress6 := '';
    if SrcIpV6.ItemIndex > 0 then Ping1.SrcAddress6 := SrcIpV6.Items [SrcIpV6.ItemIndex];

    { first do async DNS lookup, sync ping from event handler }
    DisplayMemo.Lines.Add('Resolving host ''' + HostNames.Items[HostNames.ItemIndex] + '''');
    Ping1.SocketFamily := TSocketFamily(SockFamily.ItemIndex);
    Ping1.DnsLookup(HostNames.Items[HostNames.ItemIndex]);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPingTstForm.Ping1DnsLookupDone(Sender: TObject; Error: Word);
var
    retvalue: integer;
begin
    if Error <> 0 then begin
        DisplayMemo.Lines.Add('DNS lookup error - ' + SysErrorMessage(Error));
        SetButtons (true);
        Exit;
    end;
  { V8.64 show real name looked up }
    DisplayMemo.Lines.Add('Host ''' + String(Ping1.PunycodeHost) + ''' is ' + Ping1.DnsResult);
    Ping1.Address := Ping1.DnsResult;
    retvalue := Ping1.Ping;
    if retvalue = 0 then
        DisplayMemo.Lines.Add('Ping failed: ' + Ping1.LastErrStr)
    else
        DisplayMemo.Lines.Add('Total ping responses: ' + IntToStr (retvalue));
    DisplayMemo.Lines.Add('');
    SetButtons (true);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPingTstForm.doAbortClick(Sender: TObject);
begin
    Ping1.CancelDnsLookup;
    StopFlag := true; // trace route
    SetButtons (true);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPingTstForm.Ping1EchoRequest(Sender: TObject; Icmp: TObject);
begin
    DisplayMemo.Lines.Add('Sending ' + IntToStr(Ping1.Size) + ' bytes to ' +
                          Ping1.HostName + ' (' + Ping1.HostIP + ')');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPingTstForm.Ping1EchoReply(
    Sender : TObject;
    Icmp   : TObject;
    Status : Integer);
begin
    if Status <> 0 then begin
        { Success }                         { V8.02 report actual reply IP }
        DisplayMemo.Lines.Add('Received ' + IntToStr(Ping1.ReplySize) + ' bytes from ' +
                            Ping1.ReplyIP + ' in ' + IntToStr(Ping1.ReplyRTT) + ' msecs');
    end
    else
        { Failure }
        DisplayMemo.Lines.Add('Cannot ping host (' + Ping1.HostIP + ') : ' +
                Ping1.LastErrStr + '. ReplyStatus = ' + IntToStr(Ping1.ReplyStatus));
    SetButtons (true);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPingTstForm.PingThreadTermPing (Sender: TObject);
const
    response1 = 'Thread %d for %s, %s [%d]';
    response2 = 'Thread %d for %s, received %d bytes from %s in %dms';
begin
    if Application.Terminated then exit ;
 // this event is thread safe, all publics from the thread are available here
    with Sender as TPingThread do begin
        if ReplyTotal <> 0 then
            DisplayMemo.Lines.Add (Format (response2, [PingId, PingHostName,
                                    ReplyDataSize, ReplyIPAddr, ReplyRTT]))
        else
            DisplayMemo.Lines.Add (Format (response1, [PingId,
                                        PingHostName, ErrString, ErrCode]));
    end ;
    SetButtons (true);
end ;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPingTstForm.PingThreadTermTrace (Sender: TObject);
var
    logline, addrstr: string;
    I: integer;
begin
    if PendingPings > 0 then dec (PendingPings);
    if stopflag then exit;
    if Application.Terminated then exit;
    with Sender as TPingThread do begin
        ThreadIds [PingId] := 0;  // clear id so we don't try and kill it later
        if ErrCode <> 0 then begin
            if PingId = -1 then begin
                TraceIPAddr := DnsHostIP;
                TraceSocketFamily := PingSocketFamily;
                DisplayMemo.Lines.Add ('Can Not Ping Host (' + DnsHostIP + ') : ' +
                                         ErrString + ' [' + IntToStr (ErrCode)+ ']');
                exit ;
            end ;
            if TraceDoneFlag then exit;
            logline := Format (MaskResponse2, [PingId + 1, 0, ' ', 'Request timed out']);
            inc (TraceErrs);
            if TraceErrs >= MaxErrors then begin
                DisplayMemo.Lines.Add ('Stopped Due to Excessive Errors');
                TraceDoneFlag := true;
            end ;
        end
        else begin
            if PingId = -1 then begin  // first ping to destination to find IP address
                TraceIPAddr := DnsHostIP;
                TraceSocketFamily := PingSocketFamily;
                DisplayMemo.Lines.Add (Format (MaskResponse1, [ReplyDataSize, ReplyRTT]));
                exit ;
            end ;
            addrstr := ReplyIPAddr;  // subsequent pings build route
            if addrstr <> '' then begin
                if TraceIPAddr = addrstr then TraceDoneFlag := true;
                for I := 1 to TraceMax do begin
                   if TraceAddr [I] = addrstr then exit;
                end;
            end;
            TraceAddr [PingId] := addrstr;
            logline := Format (MaskResponse2, [PingId + 1, ReplyRTT, addrstr, ReplyHostName]);
        end ;
        while DisplayMemo.Lines.Count <= (Trace1st + PingId) do
                                                DisplayMemo.Lines.Add ('');
        DisplayMemo.Lines [Trace1st + PingId] := TrimRight (logline);
    end;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.

