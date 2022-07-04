{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Description:  This unit encapsulate the ICMP.DLL into a VCL of type TPing.
              Using this object, you can easily ping any host on your network.
              Works only in 32 bits mode (no Delphi 1) under NT or 95.
              If you wants to build a console mode program, use the TICMP
              object. You'll have a much smaller program.
Version:      6.00
Creation:     January 6, 1997
EMail:        francois.piette@overbyte.be  http://www.overbyte.be
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 1997-2007 by François PIETTE
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
Nov 10, 2002 V1.12 Changed argument name from Error to Status in OnEchoReply
             to better reflect his use. 0 means OK !
Jan 29, 2004 V1.13 Added ICMPDLLHandle property and made Ping method virtual.
May 31, 2004 V1.14 Used ICSDEFS.INC
Mar 26, 2006 V6.00 New version 6 started.
Jul 19, 2008 V6.00 F. Piette made some changes for Unicode. Address, HostName
                      and DnsResult properties made as an AnsiString.
                      

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsPing;

{$B-}           { Enable partial boolean evaluation   }
{$T-}           { Untyped pointers                    }
{$X+}           { Enable extended syntax              }
{$I OverbyteIcsDefs.inc}
{$IFDEF DELPHI6_UP}
    {$WARN SYMBOL_PLATFORM   OFF}
    {$WARN SYMBOL_LIBRARY    OFF}
    {$WARN SYMBOL_DEPRECATED OFF}
{$ENDIF}
{$IFNDEF VER80}   { Not for Delphi 1                    }
    {$H+}         { Use long strings                    }
    {$J+}         { Allow typed constant to be modified }
{$ENDIF}
{$IFDEF BCB3_UP}
    {$ObjExportAll On}
{$ENDIF}
{$IFDEF VER80}
// This source file is *NOT* compatible with Delphi 1 because it uses
// Win 32 features.
{$ENDIF}

interface

uses
    Messages,
{$IFDEF USEWINDOWS}
    Windows,
{$ELSE}
    WinTypes, WinProcs,
{$ENDIF}
    SysUtils, Classes,
    OverbyteIcsIcmp, OverbyteIcsWndControl,
    // Here we use Winsock directly. If we use OverbyteIcsWinsock, then
    // we must also use OverbyteIcsWSocket and make the comopnent a lot
    // bigger. The drawback is that winsock.dll is loaded statically.
    Winsock;

const
  PingVersion           = 600;
  CopyRight : String    = ' TPing (c) 1997-2007 F. Piette V6.00 ';

type
  TDnsLookupDone = procedure (Sender: TObject; Error: Word) of object;
  TPingDisplay   = procedure(Sender: TObject; Icmp: TObject; Msg : String) of object;
  TPingReply     = procedure(Sender: TObject; Icmp: TObject; Status : Integer) of object;
  TPingRequest   = procedure(Sender: TObject; Icmp: TObject) of object;
  TPing = class(TIcsWndControl)
  protected
    FIcmp             : TICMP;
    FDnsLookupBuffer  : array [0..MAXGETHOSTSTRUCT] of char;
    FDnsLookupHandle  : THandle;
    FDnsResult        : String;
    FOnDnsLookupDone  : TDnsLookupDone;
    FOnEchoRequest    : TPingRequest;
    FOnEchoReply      : TPingReply;
    FOnDisplay        : TPingDisplay;
    FMsg_WM_ASYNCGETHOSTBYNAME : UINT;
    procedure   AllocateMsgHandlers; override;
    procedure   FreeMsgHandlers; override;
    function    MsgHandlersCount: Integer; override;
    procedure   WndProc(var MsgRec: TMessage); override;
    procedure   HandleBackGroundException(E: Exception); override;
    procedure   WMAsyncGetHostByName(var msg: TMessage); virtual;
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
    function    GetICMPHandle : HModule;
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
    function    Ping : Integer; virtual;
    procedure   DnsLookup(HostName : String); virtual;
    procedure   CancelDnsLookup;

    property    Reply         : TIcmpEchoReply read GetReply;
    property    ErrorCode     : Integer        read GetErrorCode;
    property    ErrorString   : String         read GetErrorString;
    property    HostName      : String         read GetHostName;
    property    HostIP        : String         read GetHostIP;
    property    DnsResult     : String         read FDnsResult;
    property    ICMPDLLHandle : HModule        read GetICMPHandle;
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

implementation


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
{$IFDEF USEWINDOWS}
       Result := Windows.RegisterClass(XSocketWindowClass);
{$ELSE}
       Result := WinProcs.RegisterClass(XSocketWindowClass);
{$ENDIF}
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
function TPing.MsgHandlersCount : Integer;
begin
    Result := 1 + inherited MsgHandlersCount;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPing.AllocateMsgHandlers;
begin
    inherited AllocateMsgHandlers;
    FMsg_WM_ASYNCGETHOSTBYNAME := FWndHandler.AllocateMsgHandler(Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPing.FreeMsgHandlers;
begin
    if Assigned(FWndHandler) then begin
        FWndHandler.UnregisterMessage(FMsg_WM_ASYNCGETHOSTBYNAME);
    end;
    inherited FreeMsgHandlers;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPing.WndProc(var MsgRec: TMessage);
begin
    try
         with MsgRec do begin
             if Msg = FMsg_WM_ASYNCGETHOSTBYNAME then
                 WMAsyncGetHostByName(MsgRec)
             else
                 inherited WndProc(MsgRec);
        end;
    except
        on E:Exception do
            HandleBackGroundException(E);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ All exceptions *MUST* be handled. If an exception is not handled, the     }
{ application will be shut down !                                           }
procedure TPing.HandleBackGroundException(E: Exception);
var
    CanAbort : Boolean;
begin
    CanAbort := TRUE;
    { First call the error event handler, if any }
    if Assigned(FOnBgException) then begin
        try
            FOnBgException(Self, E, CanAbort);
        except
        end;
    end;
    { Then abort the component }
    if CanAbort then begin
        try
            AbortComponent;  { 06/12/2004: Abort replaced by AbortAsync }
        except
        end;
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
        FDnsResult := String(AnsiString(inet_ntoa(IPAddr)));
    end;
    if Assigned(FOnDnsLookupDone) then
        FOnDnsLookupDone(Self, Error);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TPing.Create(Owner : TComponent);
begin
    Inherited Create(Owner);
    AllocateHWnd;
    FIcmp               := TICMP.Create;
    FIcmp.OnDisplay     := IcmpDisplay;
    FIcmp.OnEchoRequest := IcmpEchoRequest;
    FIcmp.OnEchoReply   := IcmpEchoReply;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TPing.Destroy;
begin
    CancelDnsLookup;                 { Cancel any pending dns lookup      }
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
    IPAddr    : TInAddr;
    XHostName : AnsiString;
begin
    { Cancel any pending lookup }
    if FDnsLookupHandle <> 0 then
        WSACancelAsyncRequest(FDnsLookupHandle);

    FDnsResult := '';
    XHostName  := AnsiString(HostName);
    IPAddr.S_addr := Inet_addr(@XHostName[1]);
    if IPAddr.S_addr <> u_long(INADDR_NONE) then begin
        FDnsResult := String(AnsiString((inet_ntoa(IPAddr))));
        if Assigned(FOnDnsLookupDone) then
            FOnDnsLookupDone(Self, 0);
        Exit;
    end;

    FDnsLookupHandle := WSAAsyncGetHostByName(FHandle,
                                              FMsg_WM_ASYNCGETHOSTBYNAME,
                                              @XHostName[1],
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
function TPing.GetICMPHandle: HModule;
begin
     Result := FIcmp.ICMPdllhandle;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.

