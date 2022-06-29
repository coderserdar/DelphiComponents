{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Description:  This unit encapsulate the ICMP.DLL into a VCL of type TPing.
              Using this object, you can easily ping any host on your network.
              Works only in 32 bits mode (no Delphi 1) under NT or 95.
              If you wants to build a console mode program, use the TICMP
              object. You'll have a much smaller program.
EMail:        http://users.swing.be/francois.piette  francois.piette@swing.be
              http://www.rtfm.be/fpiette             francois.piette@rtfm.be
              francois.piette@pophost.eunet.be
Creation:     January 6, 1997
Version:      1.11
Support:      Use the mailing list twsocket@rtfm.be See website for details.
Legal issues: Copyright (C) 1997 - 2000 by François PIETTE
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
Nov 30, 1997 V1.00 Added DNSLookup capability (taken from TWSocket)
Dec 13, 1997 V1.01 Added OnEchoRequest and OnEchoReply events and removed the
             corresponding OnDisplay event. This require to modify existing
             programs.
May 05, 1998 V1.02 Changed lpszClassName from 'XSocketWindowClass' to
             'ICSPingWindowClass' to avoid class name conflict with TWSocket.
             Thanks to Bill Parke <econmodel@econmodel.com> who found the
             problem.
Dec 26, 1998 V1.10 Changed all events to make sender reference TPing object
             and added an argument 'Icmp' which point to the underlaying TIcmp
             object (this was the sender in previous version). This require
             modification of existing code.
Jan 24, 1999 V1.11 Surfaced Flags property to allow fragmentation check
             (Flags = $02 to enable fragmentation check)


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit Ping;

{$IFDEF VER80}
// This source file is *NOT* compatible with Delphi 1 because it uses
// Win 32 features.
{$ENDIF}

interface

uses
  Windows, Messages, SysUtils, Classes, Winsock, Icmp;

const
  PingVersion           = 111;
  CopyRight : String    = ' TPing (c) 1997-2000 F. Piette V1.11 ';
  WM_ASYNCGETHOSTBYNAME = WM_USER + 2;

type
  TDnsLookupDone = procedure (Sender: TObject; Error: Word) of object;
  TPingDisplay   = procedure(Sender: TObject; Icmp: TObject; Msg : String) of object;
  TPingReply     = procedure(Sender: TObject; Icmp: TObject; Error : Integer) of object;
  TPingRequest   = procedure(Sender: TObject; Icmp: TObject) of object;
  TPing = class(TComponent)
  private
    FIcmp             : TICMP;
    FWindowHandle     : HWND;
    FDnsLookupBuffer  : array [0..MAXGETHOSTSTRUCT] of char;
    FDnsLookupHandle  : THandle;
    FDnsResult        : String;
    FOnDnsLookupDone  : TDnsLookupDone;
    FOnEchoRequest    : TPingRequest;
    FOnEchoReply      : TPingReply;
    FOnDisplay        : TPingDisplay;
  protected
    procedure   WndProc(var MsgRec: TMessage);
    procedure   WMAsyncGetHostByName(var msg: TMessage); message WM_ASYNCGETHOSTBYNAME;
    procedure   SetAddress(Value : String);
    function    GetAddress : String;
    procedure   SetSize(Value : Integer);
    function    GetSize : Integer;
    procedure   SetTimeout(Value : Integer);
    function    GetTimeout : Integer;
    function    GetReply : TIcmpEchoReply;
    function    GetErrorCode : Integer;
    function    GetErrorString : String;
    function    GetHostName : String;
    function    GetHostIP : String;
    procedure   SetTTL(Value : Integer);
    function    GetTTL : Integer;
    procedure   Setflags(Value : Integer);
    function    Getflags : Integer;
//    procedure   SetOnDisplay(Value : TICMPDisplay);
//    function    GetOnDisplay : TICMPDisplay;
//    procedure   SetOnEchoRequest(Value : TNotifyEvent);
//    function    GetOnEchoRequest : TNotifyEvent;
//    procedure   SetOnEchoReply(Value : TICMPReply);
//    function    GetOnEchoReply : TICMPReply;
    procedure   IcmpEchoReply(Sender: TObject; Error : Integer);
    procedure   IcmpEchoRequest(Sender: TObject);
    procedure   IcmpDisplay(Sender: TObject; Msg: String);
  public
    constructor Create(Owner : TComponent); override;
    destructor  Destroy; override;
    function    Ping : Integer;
    procedure   DnsLookup(HostName : String); virtual;
    procedure   CancelDnsLookup;

    property    Reply       : TIcmpEchoReply read GetReply;
    property    ErrorCode   : Integer        read GetErrorCode;
    property    ErrorString : String         read GetErrorString;
    property    HostName    : String         read GetHostName;
    property    HostIP      : String         read GetHostIP;
    property    Handle      : HWND           read FWindowHandle;
    property    DnsResult   : String         read FDnsResult;
  published
    property    Address     : String         read  GetAddress
                                             write SetAddress;
    property    Size        : Integer        read  GetSize
                                             write SetSize;
    property    Timeout     : Integer        read  GetTimeout
                                             write SetTimeout;
    property    TTL         : Integer        read  GetTTL
                                             write SetTTL;
    property    Flags       : Integer        read  Getflags
                                             write SetFlags;
    property    OnDisplay   : TPingDisplay   read  FOnDisplay
                                             write FOnDisplay;
    property    OnEchoRequest : TPingRequest read  FOnEchoRequest
                                             write FOnEchoRequest;
    property    OnEchoReply   : TPingReply   read  FOnEchoReply
                                             write FOnEchoReply;
    property    OnDnsLookupDone : TDnsLookupDone
                                             read  FOnDnsLookupDone
                                             write FOnDnsLookupDone;
  end;

procedure Register;

implementation


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure Register;
begin
    RegisterComponents('fpiette', [TPing]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This function is a callback function. It means that it is called by       }
{ windows. This is the very low level message handler procedure setup to    }
{ handle the message sent by windows (winsock) to handle messages.          }
function XSocketWindowProc(
    ahWnd   : HWND;
    auMsg   : Integer;
    awParam : WPARAM;
    alParam : LPARAM): Integer; stdcall;
var
    Obj    : TPing;
    MsgRec : TMessage;
begin
    { At window creation ask windows to store a pointer to our object       }
    Obj := TPing(GetWindowLong(ahWnd, 0));

    { If the pointer is not assigned, just call the default procedure       }
    if not Assigned(Obj) then
        Result := DefWindowProc(ahWnd, auMsg, awParam, alParam)
    else begin
        { Delphi use a TMessage type to pass paramter to his own kind of    }
        { windows procedure. So we are doing the same...                    }
        MsgRec.Msg    := auMsg;
        MsgRec.wParam := awParam;
        MsgRec.lParam := alParam;
        Obj.WndProc(MsgRec);
        Result := MsgRec.Result;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This global variable is used to store the windows class characteristic    }
{ and is needed to register the window class used by TWSocket               }
var
    XSocketWindowClass: TWndClass = (
        style         : 0;
        lpfnWndProc   : @XSocketWindowProc;
        cbClsExtra    : 0;
        cbWndExtra    : SizeOf(Pointer);
        hInstance     : 0;
        hIcon         : 0;
        hCursor       : 0;
        hbrBackground : 0;
        lpszMenuName  : nil;
        lpszClassName : 'ICSPingWindowClass');


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Allocate a window handle. This means registering a window class the first }
{ time we are called, and creating a new window each time we are called.    }
function XSocketAllocateHWnd(Obj : TObject): HWND;
var
    TempClass       : TWndClass;
    ClassRegistered : Boolean;
begin
    { Check if the window class is already registered                       }
    XSocketWindowClass.hInstance := HInstance;
    ClassRegistered := GetClassInfo(HInstance,
                                    XSocketWindowClass.lpszClassName,
                                    TempClass);
    if not ClassRegistered then begin
       { Not yet registered, do it right now                                }
       Result := Windows.RegisterClass(XSocketWindowClass);
       if Result = 0 then
           Exit;
    end;

    { Now create a new window                                               }
    Result := CreateWindowEx(WS_EX_TOOLWINDOW,
                           XSocketWindowClass.lpszClassName,
                           '',        { Window name   }
                           WS_POPUP,  { Window Style  }
                           0, 0,      { X, Y          }
                           0, 0,      { Width, Height }
                           0,         { hWndParent    }
                           0,         { hMenu         }
                           HInstance, { hInstance     }
                           nil);      { CreateParam   }

    { if successfull, the ask windows to store the object reference         }
    { into the reserved byte (see RegisterClass)                            }
    if (Result <> 0) and Assigned(Obj) then
        SetWindowLong(Result, 0, Integer(Obj));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Free the window handle                                                    }
procedure XSocketDeallocateHWnd(Wnd: HWND);
begin
    DestroyWindow(Wnd);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPing.WndProc(var MsgRec: TMessage);
begin
     with MsgRec do begin
         if Msg = WM_ASYNCGETHOSTBYNAME then
             WMAsyncGetHostByName(MsgRec)
         else
             Result := DefWindowProc(Handle, Msg, wParam, lParam);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPing.WMAsyncGetHostByName(var msg: TMessage);
var
    Phe     : Phostent;
    IPAddr  : TInAddr;
    Error   : Word;
begin
    if msg.wParam <> LongInt(FDnsLookupHandle) then
        Exit;
    FDnsLookupHandle := 0;
    Error := Msg.LParamHi;
    if Error = 0 then begin
        Phe        := PHostent(@FDnsLookupBuffer);
        IPAddr     := PInAddr(Phe^.h_addr_list^)^;
        FDnsResult := StrPas(inet_ntoa(IPAddr));
    end;
    if Assigned(FOnDnsLookupDone) then
        FOnDnsLookupDone(Self, Error);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TPing.Create(Owner : TComponent);
begin
    Inherited Create(Owner);
    FIcmp               := TICMP.Create;
    FIcmp.OnDisplay     := IcmpDisplay;
    FIcmp.OnEchoRequest := IcmpEchoRequest;
    FIcmp.OnEchoReply   := IcmpEchoReply;
    { Delphi 32 bits has threads and VCL is not thread safe.                }
    { We need to do our own way to be thread safe.                          }
    FWindowHandle := XSocketAllocateHWnd(Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TPing.Destroy;
begin
    CancelDnsLookup;                 { Cancel any pending dns lookup      }
    XSocketDeallocateHWnd(FWindowHandle);
    if Assigned(FIcmp) then begin
        FIcmp.Destroy;
        FIcmp := nil;
    end;
    inherited Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPing.IcmpDisplay(Sender: TObject; Msg: String);
begin
    if Assigned(FOnDisplay) then
        FOnDisplay(Self, Sender, Msg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPing.IcmpEchoReply(Sender: TObject; Error : Integer);
begin
    if Assigned(FOnEchoReply) then
        FOnEchoReply(Self, Sender, Error);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPing.IcmpEchoRequest(Sender: TObject);
begin
    if Assigned(FOnEchoRequest) then
        FOnEchoRequest(Self, Sender);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TPing.Ping : Integer;
begin
    if Assigned(FIcmp) then
        Result := FIcmp.Ping
    else
        Result := 0;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPing.CancelDnsLookup;
begin
    if FDnsLookupHandle = 0 then
        Exit;
    if WSACancelAsyncRequest(FDnsLookupHandle) <> 0 then
        raise Exception.CreateFmt('WSACancelAsyncRequest failed, error #%d',
                               [WSAGetLastError]);
    FDnsLookupHandle := 0;
    if Assigned(FOnDnsLookupDone) then
        FOnDnsLookupDone(Self, WSAEINTR);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPing.DnsLookup(HostName : String);
var
    IPAddr  : TInAddr;
begin
    { Cancel any pending lookup }
    if FDnsLookupHandle <> 0 then
        WSACancelAsyncRequest(FDnsLookupHandle);

    FDnsResult := '';

    IPAddr.S_addr := Inet_addr(@HostName[1]);
    if IPAddr.S_addr <> u_long(INADDR_NONE) then begin
        FDnsResult := StrPas(inet_ntoa(IPAddr));
        if Assigned(FOnDnsLookupDone) then
            FOnDnsLookupDone(Self, 0);
        Exit;
    end;

    FDnsLookupHandle := WSAAsyncGetHostByName(FWindowHandle,
                                              WM_ASYNCGETHOSTBYNAME,
                                              @HostName[1],
                                              @FDnsLookupBuffer,
                                              SizeOf(FDnsLookupBuffer));
    if FDnsLookupHandle = 0 then
        raise Exception.CreateFmt(
                  '%s: can''t start DNS lookup, error #%d',
                  [HostName, WSAGetLastError]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPing.SetAddress(Value : String);
begin
    if Assigned(FIcmp) then
        FIcmp.Address := Value;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TPing.GetAddress : String;
begin
    if Assigned(FIcmp) then
        Result := FIcmp.Address
    else
        Result := '';
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPing.SetSize(Value : Integer);
begin
    if Assigned(FIcmp) then
        FIcmp.Size := Value;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TPing.GetSize : Integer;
begin
    if Assigned(FIcmp) then
        Result := FIcmp.Size
    else
        Result := 0;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPing.SetTimeout(Value : Integer);
begin
    if Assigned(FIcmp) then
        FIcmp.Timeout := Value;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TPing.GetTimeout : Integer;
begin
    if Assigned(FIcmp) then
        Result := FIcmp.Timeout
    else
        Result := 0;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPing.SetTTL(Value : Integer);
begin
    if Assigned(FIcmp) then
        FIcmp.TTL := Value;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TPing.GetTTL : Integer;
begin
    if Assigned(FIcmp) then
        Result := FIcmp.TTL
    else
        Result := 0;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPing.SetFlags(Value : Integer);
begin
    if Assigned(FIcmp) then
        FIcmp.Flags := Value;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TPing.GetFlags : Integer;
begin
    if Assigned(FIcmp) then
        Result := FIcmp.flags
    else
        Result := 0;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TPing.GetReply : TIcmpEchoReply;
begin
    if Assigned(FIcmp) then
        Result := FIcmp.Reply
    else
        FillChar(Result, SizeOf(Result), 0);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TPing.GetErrorCode : Integer;
begin
    if Assigned(FIcmp) then
        Result := FIcmp.ErrorCode
    else
        Result := -1;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TPing.GetErrorString : String;
begin
    if Assigned(FIcmp) then
        Result := FIcmp.ErrorString
    else
        Result := '';
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TPing.GetHostName : String;
begin
    if Assigned(FIcmp) then
        Result := FIcmp.HostName
    else
        Result := '';
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TPing.GetHostIP : String;
begin
    if Assigned(FIcmp) then
        Result := FIcmp.HostIP
    else
        Result := '';
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{procedure TPing.SetOnDisplay(Value : TICMPDisplay);
begin
    if Assigned(FIcmp) then
        FIcmp.OnDisplay := Value;
end;
}

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{function TPing.GetOnDisplay : TICMPDisplay;
begin
    if Assigned(FIcmp) then
        Result := FIcmp.OnDisplay
    else
        Result := nil;
end;
}

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{procedure TPing.SetOnEchoRequest(Value : TNotifyEvent);
begin
    if Assigned(FIcmp) then
        FIcmp.OnEchoRequest := Value;
end;
}

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{function TPing.GetOnEchoRequest : TNotifyEvent;
begin
    if Assigned(FIcmp) then
        Result := FIcmp.OnEchoRequest
    else
        Result := nil;
end;
}

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{procedure TPing.SetOnEchoReply(Value : TICMPReply);
begin
    if Assigned(FIcmp) then
        FIcmp.OnEchoReply := Value;
end;
}

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{function TPing.GetOnEchoReply : TICMPReply;
begin
    if Assigned(FIcmp) then
        Result := FIcmp.OnEchoReply
    else
        Result := nil;
end;
}

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.

