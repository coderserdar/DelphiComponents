{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       Francois Piette
              Original code by Arno Garrels, used with his permission.
              Contact address <arno.garrels@gmx.de>
Description:  WinSock2 API subset for Delphi.
Creation:     October 2006
Version:      1.00
EMail:        francois.piette@overbyte.be  http://www.overbyte.be
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 2006 by François PIETTE
              Rue de Grady 24, 4053 Embourg, Belgium. Fax: +32-4-365.74.56
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

History:

Apr 12, 2008 *Temporary, non-breaking Unicode changes* AG.
Aug 05, 2008 F. Piette added a cast to avoid warning for AnsiString to
             String implicit convertion
Sep 21, 2008 Arno - Removed $EXTERNALSYM from some winsock2 symbols
             (CBuilder compat.)

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsWinsock2;

{$B-}           { Enable partial boolean evaluation   }
{$T-}           { Untyped pointers                    }
{$X+}           { Enable extended syntax              }
{$I OverbyteIcsDefs.inc}
{$IFDEF DELPHI6_UP}
    {$WARN SYMBOL_PLATFORM   OFF}
    {$WARN SYMBOL_LIBRARY    OFF}
    {$WARN SYMBOL_DEPRECATED OFF}
{$ENDIF}
{$IFDEF COMPILER12_UP}
    { These are usefull for debugging !}
    {$WARN IMPLICIT_STRING_CAST       OFF}
    {$WARN IMPLICIT_STRING_CAST_LOSS  ON}
    {$WARN EXPLICIT_STRING_CAST       OFF}
    {$WARN EXPLICIT_STRING_CAST_LOSS  OFF}
{$ENDIF}
{$IFDEF COMPILER2_UP}{ Not for Delphi 1                 }
    {$H+}         { Use long strings                    }
    {$J+}         { Allow typed constant to be modified }
{$ENDIF}
{$IFDEF BCB3_UP}
    {$ObjExportAll On}
{$ENDIF}

{ If NO_ADV_MT is defined, then there is less multithread code compiled.    }
{$IFDEF DELPHI1}
    {$DEFINE NO_ADV_MT}
{$ENDIF}

interface

uses
{$IFDEF CLR}
  System.Text,
  System.Runtime.InteropServices,
  //Borland.VCL.Classes,
{$ENDIF}
{$IFDEF WIN32}
  SysUtils,
{$IFDEF BCB}
  Windows, Winsock,
{$ENDIF}
{$ENDIF}
  OverbyteIcsLibrary,
  OverbyteIcsTypes,
  OverbyteIcsWinsock,
  OverbyteIcsWSocket; 

const
    WSocket2Version             = 100;
    
    {$EXTERNALSYM SIO_GET_INTERFACE_LIST}
    SIO_GET_INTERFACE_LIST      = $4004747F;
    {$EXTERNALSYM IFF_UP}
    IFF_UP                      = $00000001;
    {$EXTERNALSYM IFF_BROADCAST}
    IFF_BROADCAST               = $00000002;
    {$EXTERNALSYM IFF_LOOPBACK}
    IFF_LOOPBACK                = $00000004;
    {$EXTERNALSYM IFF_POINTTOPOINT}
    IFF_POINTTOPOINT            = $00000008;
    {$EXTERNALSYM IFF_MULTICAST}
    IFF_MULTICAST               = $00000010;

type
    {$EXTERNALSYM sockaddr}
    sockaddr = record
        sa_family   : u_short;                // address family
        sa_data     : array [0..13] of Char;  // up to 14 bytes of direct address
    end;

    //{$EXTERNALSYM in6_addr}
    in6_addr = record
    case Integer of
        0: (Byte     : array [0..15] of u_char);
        1: (Word     : array [0..7]  of u_short);
        2: (s6_bytes : array [0..15] of u_char);
        3: (s6_addr  : array [0..15] of u_char);
        4: (s6_words : array [0..7]  of u_short);
    end;
    TIn6Addr = in6_addr;
    PIn6Addr = ^in6_addr;

    //{$EXTERNALSYM sockaddr_in6_old}
    sockaddr_in6_old = record
        sin6_family   : short;    // AF_INET6
        sin6_port     : u_short;  // Transport level port number
        sin6_flowinfo : u_long;   // IPv6 flow information
        sin6_addr     : in6_addr; // IPv6 address
    end;
    TSockAddrIn6Old = sockaddr_in6_old;
    PSockAddrIn6Old = ^sockaddr_in6_old;

    //{$EXTERNALSYM sockaddr_gen}
    sockaddr_gen = record
    case Integer of
        0: (Address    : sockaddr);
        1: (AddressIn  : sockaddr_in);
        2: (AddressIn6 : sockaddr_in6_old);
    end;
    TSockAddrGen = sockaddr_gen;
    PSockAddrGen = ^sockaddr_gen;

    //{$EXTERNALSYM _INTERFACE_INFO}
    _INTERFACE_INFO = record
        iiFlags            : u_long;        // Interface flags
        iiAddress          : sockaddr_gen;  // Interface address
        iiBroadcastAddress : sockaddr_gen;  // Broadcast address
        iiNetmask          : sockaddr_gen;  // Network mask
    end;
    //{$EXTERNALSYM INTERFACE_INFO}
    INTERFACE_INFO = _INTERFACE_INFO;
    TInterfaceInfo = INTERFACE_INFO;
    PInterfaceInfo = ^INTERFACE_INFO;

 // Winsock2 utilities //
    TInterfaceList = class(TList)
    private
        FOwnsObjects: Boolean;
    protected
        procedure Notify(Ptr: Pointer; Action: TListNotification); override;
        function  GetItem(Index: Integer): PInterfaceInfo;
        procedure SetItem(Index: Integer; IInfo: PInterfaceInfo);
    public
        constructor Create; overload;
        constructor Create(AOwnsObjects: Boolean); overload;
        function  Add(IInfo: PInterfaceInfo): Integer;
        function  Extract(IInfo: PInterfaceInfo): PInterfaceInfo;
        function  Remove(IInfo: PInterfaceInfo): Integer;
        function  IndexOf(IInfo: PInterfaceInfo): Integer;
        procedure Insert(Index: Integer; IInfo: PInterfaceInfo);
        function  First: PInterfaceInfo;
        function  Last: PInterfaceInfo;
        property  OwnsObjects: Boolean read FOwnsObjects write FOwnsObjects;
        property  Items[Index: Integer]: PInterfaceInfo read GetItem write SetItem; default;
    end;

    procedure WSocket2GetInterfaceList(InterfaceList : TInterfaceList); overload;
    procedure WSocket2GetInterfaceList(StrList : TStrings); overload;
    function  WSocket2IsAddrInSubNet(saddr : TInAddr) : Boolean; overload;
    function  WSocket2IsAddrInSubNet(IpAddr : AnsiString) : Boolean; overload;

implementation

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TInterfaceList.Add(IInfo: PInterfaceInfo): Integer;
begin
    Result := inherited Add(IInfo);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TInterfaceList.Create;
begin
    inherited Create;
    FOwnsObjects := True;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TInterfaceList.Create(AOwnsObjects: Boolean);
begin
    inherited Create;
    FOwnsObjects := AOwnsObjects;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TInterfaceList.Extract(IInfo: PInterfaceInfo): PInterfaceInfo;
begin
    Result := PInterfaceInfo(inherited Extract(IInfo));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TInterfaceList.First: PInterfaceInfo;
begin
    Result := PInterfaceInfo(inherited First);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TInterfaceList.GetItem(Index: Integer): PInterfaceInfo;
begin
    Result := PInterfaceInfo(inherited Items[Index]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TInterfaceList.IndexOf(IInfo: PInterfaceInfo): Integer;
begin
    Result := inherited IndexOf(IInfo);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TInterfaceList.Insert(Index: Integer; IInfo: PInterfaceInfo);
begin
    inherited Insert(Index, IInfo);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TInterfaceList.Last: PInterfaceInfo;
begin
    Result := PInterfaceInfo(inherited Last);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TInterfaceList.Notify(Ptr: Pointer; Action: TListNotification);
begin
    if OwnsObjects then
        if Action = lnDeleted then
            Dispose(PInterfaceInfo(Ptr));
    inherited Notify(Ptr, Action);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TInterfaceList.Remove(IInfo: PInterfaceInfo): Integer;
begin
    Result := inherited Remove(IInfo);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TInterfaceList.SetItem(Index: Integer; IInfo: PInterfaceInfo);
begin
    inherited Items[Index] := IInfo;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure WSocket2GetInterfaceList(InterfaceList : TInterfaceList);
var
    NumInterfaces   : Integer;
    BytesReturned   : Cardinal;
    PBuf            : PChar;
    P               : PChar;
    I               : Integer;
    Err             : Integer;
    BufSize         : Cardinal;
    PInfo           : PInterfaceInfo;
    s               : TSocket;
begin
    if not Assigned(InterfaceList) then
        Exit;
    InterfaceList.Clear;
    BufSize := 20 * SizeOf(TInterfaceInfo);
    GetMem(PBuf, BufSize);
    try
        s := WSocket_Socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP);
        if (s = INVALID_SOCKET) then
            raise ESocketException.Create(
                           'WSocket2 GetInterfaceList: Socket creation failed');
        try
            while True do
            begin
                Err := WSocket_WSAIoCtl(s, SIO_GET_INTERFACE_LIST, nil, 0,
                                        PBuf, BufSize, BytesReturned,
                                        nil, nil);
                if Err = SOCKET_ERROR then
                    Err := WSocket_WSAGetLastError;
                case Err of
                    0 : Break;
                    WSAEFAULT :
                        begin
                            // How many interfaces make sense ??
                            if BufSize >= 10000 * SizeOf(TInterfaceInfo) then
                                raise ESocketException.Create(
                               'WSocket2 GetInterfaceList: Too many interfaces');
                            // No chance to get correct buffer size w/o probing
                            Inc(BufSize, 100 * SizeOf(TInterfaceInfo));
                            FreeMem(PBuf);
                            GetMem(PBuf, BufSize);
                        end;
                    else
                        raise ESocketException.Create('WSocket2 GetInterfaceList: ' +
                           GetWinsockErr(Err) + ' Error #' + IntToStr(Err));
                end;
            end;
        finally
            WSocket_Closesocket(s);
        end;
        if BytesReturned < SizeOf(TInterfaceInfo) then
            Exit;
        P := PBuf;
        NumInterfaces := BytesReturned div SizeOf(TInterfaceInfo);
        for I := 0 to NumInterfaces - 1 do
        begin
            New(PInfo);
            PInfo^ := PInterfaceInfo(P)^;
            InterfaceList.Add(PInfo);
            Inc(P, SizeOf(TInterfaceInfo));
        end;
    finally
        FreeMem(PBuf);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure WSocket2GetInterfaceList(StrList : TStrings); overload;
var
    iList : TInterfaceList;
    I     : Integer;
begin
    if not Assigned(StrList) then
        Exit;
    StrList.Clear;
    iList := TInterfaceList.Create;
    try
        WSocket2GetInterfaceList(iList);
        for I := 0 to IList.Count -1 do
            StrList.Add(String(WSocket_inet_ntoa(
                     IList[I]^.iiAddress.AddressIn.sin_addr)));
    finally
        iList.Free;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket2IsAddrInSubNet(saddr : TInAddr) : Boolean;
var
    iList : TInterfaceList;
    I     : Integer;
    iInfo : TInterfaceInfo;
begin
    Result  := FALSE;
    iList := TInterfaceList.Create;
    try
        WSocket2GetInterfaceList(iList);
        for I := 0 to iList.Count -1 do
        begin
            iInfo := IList[I]^;
            if (iInfo.iiAddress.addressIn.sin_addr.S_addr and
                iInfo.iiNetMask.addressIn.sin_addr.S_addr) =
               (saddr.S_addr and iInfo.iiNetMask.addressIn.sin_addr.S_addr) then
            begin
                Result := TRUE;
                Exit;
            end;
        end;
    finally
        iList.Free;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function  WSocket2IsAddrInSubNet(IpAddr : AnsiString) : Boolean;
var
    saddr : TInAddr;
begin
    if Length(IpAddr) = 0 then
        raise ESocketException.Create('Invalid address');
    saddr.S_addr := WSocket_inet_addr(PAnsiChar(IpAddr));
    Result := WSocket2IsAddrInSubNet(saddr);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
end.
