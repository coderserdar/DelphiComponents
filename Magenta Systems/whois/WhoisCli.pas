{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Description:  TWhoisCli is a Whois protocol client using TWSocket
              Conform to RFC-954 (which is not really very detailed)
Creation:     27 August 2002 by Angus Robertson, Magenta Systems Ltd
Version:      1.00
EMail:        http://www.overbyte.be       francois.piette@overbyte.be
              http://www.rtfm.be/fpiette   francois.piette@rtfm.be
              francois.piette@pophost.eunet.be
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 1997-2001 by François PIETTE
              Rue de Grady 24, 4053 Embourg, Belgium. Fax: +32-4-365.74.56
              <francois.piette@overbyte.be> <francois.piette@pophost.eunet.be>

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



Getting Whois server lists:

Using Whois to host 'sipb.mit.edu' query 'whois-servers' will return the MIT list
of about 250 worldwide WHOIS servers, dated 8 March 2000 at the time of writing.

The same MIT list formatted differently may be obtained from:
   ftp://sipb.mit.edu/pub/whois/whois-servers.list






 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit WhoisCli;

interface

{$B-}           { Enable partial boolean evaluation   }
{$T-}           { Untyped pointers                    }
{$X+}           { Enable extended syntax              }
{$IFNDEF VER80} { Not for Delphi 1                    }
    {$H+}       { Use long strings                    }
    {$J+}       { Allow typed constant to be modified }
{$ENDIF}
{$IFDEF VER110} { C++ Builder V3.0                    }
    {$ObjExportAll On}
{$ENDIF}
{$IFDEF VER125} { C++ Builder V4.0                    }
    {$ObjExportAll On}
{$ENDIF}

uses
    WinTypes, WinProcs, SysUtils, Messages, Classes, Graphics, Controls,
    WSocket;

const
    WhoisCliVersion            = 100;
    CopyRight    : String     = ' WhoisCli (c) 2002 F. Piette V1.00 ';

type
    TWhoisCli = class(TComponent)
        constructor Create(AOwner: TComponent); override;
        destructor  Destroy; override;
        procedure   StartQuery;
        function    Receive(Buffer : Pointer; Len : Integer) : Integer;
        procedure   Abort;
    protected
        FWSocket            : TWSocket;
        FQuery              : String;
        FHost               : String;
        FWhoisResp          : String;
        FQueryDoneFlag      : Boolean;
        FOnSessionConnected : TSessionConnected;
        FOnDataAvailable    : TDataAvailable;
        FOnQueryDone        : TSessionClosed;
        procedure WSocketDnsLookupDone(Sender: TObject; Error: Word);
        procedure WSocketSessionConnected(Sender: TObject; Error: Word);
        procedure WSocketDataAvailable(Sender: TObject; Error: Word);
        procedure WSocketSessionClosed(Sender: TObject; Error: Word);
        procedure TriggerQueryDone(Error: Word);
    published
        property Query : String                         read  FQuery
                                                        write FQuery;
        property Host  : String                         read  FHost
                                                        write FHost;
        property WhoisResp : String                     read  FWhoisResp;
        property OnSessionConnected : TSessionConnected read  FOnSessionConnected
                                                        write FOnSessionConnected;
        property OnQueryDone : TSessionClosed           read  FOnQueryDone
                                                        write FOnQueryDone;
    end;


procedure Register;

implementation

const 
{$IFDEF VER80}
    BufferSize = 255;     { Delphi 1 is limited to 255 bytes }
{$ELSE}
    BufferSize = 2048;
{$ENDIF}

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure Register;
begin
    RegisterComponents('FPiette', [TWhoisCli]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TWhoisCli.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);
    FWSocket                    := TWSocket.Create(Self);
    FWSocket.OnSessionConnected := WSocketSessionConnected;
    FWSocket.OnDataAvailable    := WSocketDataAvailable;
    FWSocket.OnSessionClosed    := WSocketSessionClosed;
    FWSocket.OnDnsLookupDone    := WSocketDnsLookupDone;
    FHost := 'whois.ripe.net' ;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TWhoisCli.Destroy;
begin
    if Assigned(FWSocket) then
        FWSocket.Destroy;
    inherited Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWhoisCli.StartQuery;
begin
   FQueryDoneFlag := FALSE ;
   FWhoisResp := '' ;
   FWSocket.DnsLookup(fHost);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWhoisCli.Abort;
begin
    FWSocket.CancelDnsLookup;
    FWSocket.Abort;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TWhoisCli.Receive(Buffer : Pointer; Len : Integer) : Integer;
begin
    Result := FWSocket.Receive(Buffer, Len);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWhoisCli.WSocketDnsLookupDone(Sender: TObject; Error: Word);
begin
    if Error <> 0 then
        TriggerQueryDone(Error)
    else begin
        FWSocket.Addr  := FWSocket.DnsResult;
        FWSocket.Proto := 'tcp';
        FWSocket.Port  := 'whois' ;
        FWSocket.Connect;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWhoisCli.WSocketSessionConnected(Sender: TObject; Error: Word);
begin
    if Assigned(FOnSessionConnected) then
        FOnSessionConnected(Self, Error);

    if Error <> 0 then begin
        TriggerQueryDone(Error);
        FWSocket.Close
    end
    else
    begin
        FWSocket.SendStr(fQuery + #13 + #10);
    end ;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWhoisCli.WSocketDataAvailable(Sender: TObject; Error: Word);
var
    Buffer : array [0..BufferSize - 1] of char;
    Len : Integer;
begin
    while TRUE do begin
        Len := FWSocket.Receive(@Buffer, SizeOf(Buffer) - 1);
        if Len <= 0 then break;
        Buffer[Len] := #0;
        FWhoisResp := FWhoisResp + StrPas (Buffer) ;
        Buffer[0] := #0;
    end ;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWhoisCli.TriggerQueryDone(Error: Word);
begin
    FQueryDoneFlag := TRUE;  // in case new query started in done event
    if Assigned(FOnQueryDone) then FOnQueryDone(Self, Error);
    // Whois result is in FWhoisResp
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWhoisCli.WSocketSessionClosed(Sender: TObject; Error: Word);
begin
    TriggerQueryDone(Error);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.

