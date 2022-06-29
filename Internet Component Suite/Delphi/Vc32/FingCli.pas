{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Description:  TFingerCli is a FINGER protocol client using TWSocket
              Conform to RFC-1288 (supercede RFCs 1196, 1194 and 742)
Creation:     December 18, 1997
Version:      1.01
EMail:        http://users.swing.be/francois.piette  francois.piette@swing.be
              http://www.rtfm.be/fpiette             francois.piette@rtfm.be
              francois.piette@pophost.eunet.be
Support:      Use the mailing list twsocket@rtfm.be See website for details.
Legal issues: Copyright (C) 1997, 1998, 1999 by François PIETTE
              Rue de Grady 24, 4053 Embourg, Belgium. Fax: +32-4-365.74.56
              <francois.piette@pophost.eunet.be>

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
Aug 20, 1999 V1.01 Added compile time options. Revised for BCB4.

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit FingCli;

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
    FingerCliVersion = 101;

type
    TFingerCli = class(TComponent)
        constructor Create(AOwner: TComponent); override;
        destructor  Destroy; override;
        procedure   StartQuery;
        function    Receive(Buffer : Pointer; Len : Integer) : Integer;
        procedure   Abort;
    protected
        FWSocket            : TWSocket;
        FQuery              : String;
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
        property OnSessionConnected : TSessionConnected read  FOnSessionConnected
                                                        write FOnSessionConnected;
        property OnDataAvailable : TDataAvailable       read  FOnDataAvailable
                                                        write FOnDataAvailable;
        property OnQueryDone : TSessionClosed           read  FOnQueryDone
                                                        write FOnQueryDone;
    end;

procedure Register;

implementation


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure Register;
begin
    RegisterComponents('FPiette', [TFingerCli]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TFingerCli.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);
    FWSocket                    := TWSocket.Create(Self);
    FWSocket.OnSessionConnected := WSocketSessionConnected;
    FWSocket.OnDataAvailable    := WSocketDataAvailable;
    FWSocket.OnSessionClosed    := WSocketSessionClosed;
    FWSocket.OnDnsLookupDone    := WSocketDnsLookupDone;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TFingerCli.Destroy;
begin
    if Assigned(FWSocket) then
        FWSocket.Destroy;
    inherited Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFingerCli.StartQuery;
var
    I    : Integer;
    Host : String;
begin
    I := Pos('@', FQuery);
    if I <= 0 then
         raise Exception.CreateFmt('TFingerCli, Invalid Query: %s', [FQuery]);
    Host := Copy(FQuery, I + 1, Length(FQuery));
    if Length(Host) <= 0 then
         raise Exception.CreateFmt('TFingerCli, Invalid Host in query: %s', [FQuery]);
    FQueryDoneFlag := FALSE;
    FWSocket.DnsLookup(Host);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFingerCli.Abort;
begin
    FWSocket.CancelDnsLookup;
    FWSocket.Abort;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TFingerCli.Receive(Buffer : Pointer; Len : Integer) : Integer;
begin
    Result := FWSocket.Receive(Buffer, Len);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFingerCli.WSocketDnsLookupDone(Sender: TObject; Error: Word);
begin
    if Error <> 0 then
        TriggerQueryDone(Error)
    else begin
        FWSocket.Addr  := FWSocket.DnsResult;
        FWSocket.Proto := 'tcp';
        FWSocket.Port  := 'finger';
        FWSocket.Connect;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFingerCli.WSocketSessionConnected(Sender: TObject; Error: Word);
begin
    if Assigned(FOnSessionConnected) then
        FOnSessionConnected(Self, Error);

    if Error <> 0 then begin
        TriggerQueryDone(Error);
        FWSocket.Close
    end
    else
        FWSocket.SendStr(FQuery + #13 + #10);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFingerCli.WSocketDataAvailable(Sender: TObject; Error: Word);
begin
    if Assigned(FOnDataAvailable) then
        FOnDataAvailable(Self, Error);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFingerCli.TriggerQueryDone(Error: Word);
begin
    if (FQueryDoneFlag = FALSE) and Assigned(FOnQueryDone) then
        FOnQueryDone(Self, Error);
    FQueryDoneFlag := TRUE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFingerCli.WSocketSessionClosed(Sender: TObject; Error: Word);
begin
    TriggerQueryDone(Error);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.

