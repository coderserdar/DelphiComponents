{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Program:      TNCNX.PAS
Object:       Delphi component which implement the TCP/IP telnet protocol
              including some options negociations.
              RFC854, RFC885, RFC779, RFC1091
Author:       François PIETTE
EMail:        http://users.swing.be/francois.piette  francois.piette@swing.be
              http://www.rtfm.be/fpiette             francois.piette@rtfm.be
              francois.piette@pophost.eunet.be
Creation:     April, 1996
Version:      2.07
Support:      Use the mailing list twsocket@rtfm.be See website for details.
Legal issues: Copyright (C) 1996-2000 by François PIETTE
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
Jul 22, 1997 Adapted to Delphi 3
Sep 5, 1997  Added version information, removed old code, added OnTermType
             Renamed some indentifier to be more standard.
Sep 24, 1997 V2.03 Added procedures to negociate options
May 12, 1998 V2.04 Changed NegociateOption to properly handle unwanted
             option as Jan Tomasek <xtomasej@feld.cvut.cz> suggested.
Aug 10, 1998 V2.05 Cleared strSubOption after NegociateSubOption as Jan
             Tomasek <xtomasej@feld.cvut.cz> suggested.
Aug 15, 1999 V2.06 Moved Notification procedure to public section for
             BCB4 compatibility
Aug 20, 1999 V2.07 Added compile time options. Revised for BCB4.


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit TnCnx;

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

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Controls, Forms,
  WSocket, Winsock;

const
  TnCnxVersion       = 207;
  CopyRight : String = ' TTnCnx (c) 1996-2000 F. Piette V2.07 ';

  { Telnet command characters                                             }
  TNCH_EOR        = #239;     { $EF End Of Record (preceded by IAC)       }
  TNCH_SE         = #240;     { $F0 End of subnegociation parameters      }
  TNCH_NOP        = #241;     { $F1 No operation                          }
  TNCH_DATA_MARK  = #242;     { $F2 Data stream portion of a Synch        }
  TNCH_BREAK      = #243;     { $F3 NVT charcater break                   }
  TNCH_IP         = #244;     { $F4 Interrupt process                     }
  TNCH_AO         = #245;     { $F5 Abort output                          }
  TNCH_AYT        = #246;     { $F6 Are you there                         }
  TNCH_EC         = #247;     { $F7 Erase character                       }
  TNCH_EL         = #248;     { $F8 Erase line                            }
  TNCH_GA         = #249;     { $F9 Go ahead                              }
  TNCH_SB         = #250;     { $FA Subnegociation                        }
  TNCH_WILL       = #251;     { $FB Will                                  }
  TNCH_WONT       = #252;     { $FC Wont                                  }
  TNCH_DO         = #253;     { $FD Do                                    }
  TNCH_DONT       = #254;     { $FE Dont                                  }
  TNCH_IAC        = #255;     { $FF IAC                                   }

  { Telnet options                                                        }
  TN_TRANSMIT_BINARY      = #0;   { $00 }
  TN_ECHO                 = #1;   { $01 }
  TN_RECONNECTION         = #2;   { $02 }
  TN_SUPPRESS_GA          = #3;   { $03 }
  TN_MSG_SZ_NEGOC         = #4;   { $04 }
  TN_STATUS               = #5;   { $05 }
  TN_TIMING_MARK          = #6;   { $06 }
  TN_NOPTIONS             = #6;   { $06 }
  TN_DET                  = #20;  { $14 }
  TN_SEND_LOC             = #23;  { $17 }
  TN_TERMTYPE             = #24;  { $18 }
  TN_EOR                  = #25;  { $19 }
  TN_NAWS                 = #31;  { $1F }
  TN_TERMSPEED            = #32;  { $20 }
  TN_TFC                  = #33;  { $21 }
  TN_XDISPLOC             = #35;  { $23 }
  TN_EXOPL                = #255; { $FF }

  TN_TTYPE_SEND		  = #1;
  TN_TTYPE_IS		  = #0;

type
  TTnCnx = class;

  TTnSessionConnected = procedure (Sender: TTnCnx; Error : word) of object;
  TTnSessionClosed    = procedure (Sender: TTnCnx; Error : word) of object;
  TTnDataAvailable    = procedure (Sender: TTnCnx; Buffer : PChar; Len : Integer) of object;
  TTnDisplay          = procedure (Sender: TTnCnx; Str : String) of object;

  TTnCnx= class(TComponent)
  public
    Socket              : TWSocket;
  private
    FPort               : String;
    FHost               : String;
    FLocation           : String;
    FTermType           : String;
    RemoteBinMode       : Boolean;
    LocalBinMode        : Boolean;
    FLocalEcho          : Boolean;
    Spga                : Boolean;
    FTType              : Boolean;
    FBuffer             : array [0..2048] of char;
    FBufferCnt          : Integer;
    FWindowHandle       : HWND;
    FOnSessionConnected : TTnSessionConnected;
    FOnSessionClosed    : TTnSessionClosed;
    FOnDataAvailable    : TTnDataAvailable;
    FOnDisplay          : TTnDisplay;
    FOnEOR              : TNotifyEvent;
    FOnSendLoc          : TNotifyEvent;
    FOnTermType         : TNotifyEvent;
    FOnLocalEcho        : TNotifyEvent;
    procedure WndProc(var MsgRec: TMessage);
    procedure SocketSessionConnected(Sender: TObject; Error : word);
    procedure SocketSessionClosed(Sender: TObject; Error : word);
    procedure SocketDataAvailable(Sender: TObject; Error : word);
    procedure Display(Str : String);
    procedure AddChar(Ch : Char);
    procedure ReceiveChar(Ch : Char);
    procedure Answer(chAns : Char; chOption : Char);
    procedure NegociateSubOption(strSubOption : String);
    procedure NegociateOption(chAction : Char; chOption : Char);
    procedure FlushBuffer;
    function  GetState : TSocketState;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    function    Send(Data : Pointer; Len : Integer) : integer;
    function    SendStr(Data : String) : integer;
    procedure   Connect;
    function    IsConnected : Boolean;
    procedure   WillOption(chOption : Char);
    procedure   WontOption(chOption : Char);
    procedure   DontOption(chOption : Char);
    procedure   DoOption(chOption : Char);
    procedure   Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure   Close;
    procedure   Pause;
    procedure   Resume;
    property    State : TSocketState                  read  GetState;
    property    Handle : HWND                         read  FWindowHandle;
  published
    property Port : String                            read  FPort
                                                      write FPort;
    property Host : String                            read  FHost
                                                      write FHost;
    property Location : String                        read  FLocation
                                                      write FLocation;
    property TermType : String                        read  FTermType
                                                      write FTermType;
    property LocalEcho : Boolean                      read  FLocalEcho
                                                      write FLocalEcho;
    property OnSessionConnected : TTnSessionConnected read  FOnSessionConnected
                                                      write FOnSessionConnected;
    property OnSessionClosed :    TTnSessionClosed    read  FOnSessionClosed
                                                      write FOnSessionClosed;
    property OnDataAvailable :    TTnDataAvailable    read  FOnDataAvailable
                                                      write FOnDataAvailable;
    property OnDisplay :          TTnDisplay          read  FOnDisplay
                                                      write FOnDisplay;
    property OnEndOfRecord :      TNotifyEvent        read  FOnEOR
                                                      write FOnEOR;
    property OnSendLoc :          TNotifyEvent        read  FOnSendLoc
                                                      write FOnSendLoc;
    property OnTermType :         TNotifyEvent        read  FOnTermType
                                                      write FOnTermType;
    property OnLocalEcho :        TNotifyEvent        read  FOnLocalEcho
                                                      write FOnLocalEcho;
  end;

procedure Register;

implementation

{$DEFINE Debug}      { Add or remove minus sign before dollar sign to }
                     { generate code for debug message output         }

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure Register;
begin
  RegisterComponents('FPiette', [TTnCnx]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure DebugString(Msg : String);
const
    Cnt : Integer = 0;
var
    Buf : String[20];
begin
{$IFDEF Debug}
    Cnt := Cnt + 1;
    Buf := IntToHex(Cnt, 4) + ' ' + #0;
    OutputDebugString(@Buf[1]);

{$IFNDEF WIN32}
    if Length(Msg) < High(Msg) then
        Msg[Length(Msg) + 1] := #0;
{$ENDIF}

    OutputDebugString(@Msg[1]);
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTnCnx.WndProc(var MsgRec: TMessage);
begin
     with MsgRec do
         Result := DefWindowProc(Handle, Msg, wParam, lParam);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TTnCnx.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);
    FWindowHandle             := AllocateHWnd(WndProc);
    FLocation                 := 'TNCNX';
    FTermType                 := 'VT100';
    FPort                     := '23';
    Socket                    := TWSocket.Create(Self);
    Socket.OnSessionConnected := SocketSessionConnected;
    Socket.OnDataAvailable    := SocketDataAvailable;
    Socket.OnSessionClosed    := SocketSessionClosed;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TTnCnx.Destroy;
begin
    if Assigned(Socket) then begin
        Socket.Free;
        Socket := nil;
    end;
    DeallocateHWnd(FWindowHandle);
    inherited Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTnCnx.Notification(AComponent: TComponent; Operation: TOperation);
begin
    inherited Notification(AComponent, Operation);
    if (AComponent = Socket) and (Operation = opRemove) then
        Socket := nil;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTnCnx.Pause;
begin
    if not Assigned(Socket) then
        Exit;
    Socket.Pause;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTnCnx.Resume;
begin
    if not Assigned(Socket) then
        Exit;
    Socket.Resume;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTnCnx.Connect;
begin
    if not Assigned(Socket) then
        Exit;

    if Socket.State <> wsClosed then
        Socket.Close;

    Socket.Proto := 'tcp';
    Socket.Port  := FPort;
    Socket.Addr  := FHost;
    Socket.Connect;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TTnCnx.IsConnected : Boolean;
begin
    Result := Socket.State = wsConnected;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTnCnx.Close;
begin
    if Assigned(Socket) then
        Socket.Close;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTnCnx.Display(Str : String);
begin
    if Assigned(FOnDisplay) then
        FOnDisplay(Self, Str);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TTnCnx.GetState : TSocketState;
begin
    if Assigned(Socket) then
        Result := Socket.State
    else
        Result := wsInvalidState;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTnCnx.SocketSessionConnected(Sender: TObject; Error : word);
begin
    if Assigned(FOnSessionConnected) then
        FOnSessionConnected(Self, Error);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTnCnx.SocketSessionClosed(Sender: TObject; Error : word);
begin
    if Socket.State <> wsClosed then
        Socket.Close;
    if Assigned(FOnSessionClosed) then
        FOnSessionClosed(Self, Error);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTnCnx.SocketDataAvailable(Sender: TObject; Error : word);
var
    Len, I : Integer;
    Buffer : array [1..2048] of char;
    Socket : TWSocket;
begin
    Socket := Sender as TWSocket;
    Len := Socket.Receive(@Buffer[1], High(Buffer));
    if Len = 0 then begin
        { Remote has closed }
        Display(#13 + #10 + '**** Remote has closed ****' + #13 + #10);
    end
    else if Len < 0 then begin
        { An error has occured }
        if Socket.LastError <> WSAEWOULDBLOCK then
            Display(#13 + #10 + '**** ERROR: ' + IntToStr(Socket.LastError) +
                    ' ****' + #13 + #10);
    end
    else begin
        for I := 1 to Len do
            ReceiveChar(Buffer[I]);
        FlushBuffer;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function  TTnCnx.Send(Data : Pointer; Len : Integer) : integer;
begin
    if Assigned(Socket) then
        Result := Socket.Send(Data, Len)
    else
        Result := -1;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TTnCnx.SendStr(Data : String) : integer;
begin
    Result := Send(@Data[1], Length(Data));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTnCnx.Answer(chAns : Char; chOption : Char);
var
    Buf   : String[3];
begin
{    DebugString('Answer ' + IntToHex(ord(chAns), 2) + ' ' + IntToHex(ord(ChOption), 2) + #13 + #10); }
    Buf := TNCH_IAC + chAns + chOption;
    Socket.Send(@Buf[1], Length(Buf));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTnCnx.WillOption(chOption : Char);
begin
    Answer(TNCH_WILL, chOption);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTnCnx.WontOption(chOption : Char);
begin
    Answer(TNCH_WONT, chOption);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTnCnx.DontOption(chOption : Char);
begin
    Answer(TNCH_DONT, chOption);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTnCnx.DoOption(chOption : Char);
begin
    Answer(TNCH_DO, chOption);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTnCnx.NegociateSubOption(strSubOption : String);
var
    Buf   : String;
begin
{    DebugString('SubNegociation ' +
                IntToHex(ord(strSubOption[1]), 2) + ' ' +
                IntToHex(ord(strSubOption[2]), 2) + #13 + #10); }

    case strSubOption[1] of
    TN_TERMTYPE:
        begin
            if strSubOption[2] = TN_TTYPE_SEND then begin
{                DebugString('Send TermType' + #13 + #10); }
                if Assigned(FOnTermType) then
                    FOnTermType(Self);
                Buf := TNCH_IAC + TNCH_SB + TN_TERMTYPE + TN_TTYPE_IS + FTermType + TNCH_IAC + TNCH_SE;
                Socket.Send(@Buf[1], Length(Buf));
            end;
        end;
    else
{        DebugString('Unknown suboption' + #13 + #10); }
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTnCnx.NegociateOption(chAction : Char; chOption : Char);
var
    Buf : String;
begin
{    DebugString('Negociation ' + IntToHex(ord(chAction), 2) + ' ' +
                                 IntToHex(ord(ChOption), 2) + #13 + #10); }

    case chOption of
    TN_TRANSMIT_BINARY:
        begin
            if chAction = TNCH_WILL then begin
                Answer(TNCH_DO, chOption);
                RemoteBinMode := TRUE;
                LocalBinMode  := TRUE;
            end
            else if chAction = TNCH_WONT then begin
                if RemoteBinMode then begin
                    RemoteBinMode := FALSE;
                    LocalBinMode  := FALSE;
                end;
            end;
        end;
    TN_ECHO:
        begin
            if chAction = TNCH_WILL then begin
                Answer(TNCH_DO, chOption);
                FLocalEcho := FALSE;
            end
            else if chAction = TNCH_WONT then begin
                FLocalEcho := TRUE;
            end;
            if Assigned(FOnLocalEcho) then
                FOnLocalEcho(self);
        end;
    TN_SUPPRESS_GA:
        begin
            if chAction = TNCH_WILL then begin
                Answer(TNCH_DO, chOption);
                spga := TRUE;
            end;
        end;
    TN_TERMTYPE:
        begin
            if chAction = TNCH_DO then begin
                Answer(TNCH_WILL, chOption);
                FTType := TRUE;
            end;
        end;
    TN_SEND_LOC:
        begin
            if chAction = TNCH_DO then begin
                Answer(TNCH_WILL, chOption);
                if Assigned(FOnSendLoc) then
                    FOnSendLoc(Self);
                Buf := TNCH_IAC + TNCH_SB + TN_SEND_LOC + FLocation + TNCH_IAC + TNCH_SE;
                Socket.Send(@Buf[1], Length(Buf));
            end;
        end;
    TN_EOR:
        begin
            if chAction = TNCH_DO then begin
                Answer(TNCH_WILL, chOption);
                FTType := TRUE;
            end;
        end;
    else
{        Answer(TNCH_WONT, chOption); }
        { Jan Tomasek <xtomasej@feld.cvut.cz> }
        if chAction = TNCH_WILL then
            Answer(TNCH_DONT, chOption)
        else
            Answer(TNCH_WONT, chOption);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTnCnx.FlushBuffer;
var
    Buffer : PChar;
    Count  : Integer;
begin
    try
        if FBufferCnt > 0 then begin
            if Assigned(FOnDataAvailable) then begin
                { We need to make a copy for the data because we can reenter   }
                { during the event processing                                  }
                Count := FBufferCnt;             { How much we received        }
                try
                    GetMem(Buffer, Count + 1);       { Alloc memory for the copy   }
                except
                    Buffer := nil;
                end;
                if Buffer <> nil then begin
                    try
                        Move(FBuffer, Buffer^, Count);   { Actual copy             }
                        Buffer[Count] := #0;             { Add a nul byte          }
                        FBufferCnt := 0;                 { Reset receivecounter    }
                        FOnDataAvailable(Self, Buffer, Count); { Call event handler  }
                    finally
                        FreeMem(Buffer, Count + 1);      { Release the buffer      }
                    end;
                end;
            end
            else begin
                FBufferCnt := 0
            end;
        end;
    except
        raise;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTnCnx.AddChar(Ch : Char);
begin
    FBuffer[FBufferCnt] := Ch;
    Inc(FBufferCnt);
    if FBufferCnt >= SizeOf(FBuffer) then
        FlushBuffer;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTnCnx.ReceiveChar(Ch : Char);
const
    bIAC         : Boolean = FALSE;
    chVerb       : Char    = #0;
    strSubOption : String  = '';
    bSubNegoc    : Boolean = FALSE;
begin
    if chVerb <> #0 then begin
        NegociateOption(chVerb, Ch);
        chVerb       := #0;
        strSubOption := '';
        Exit;
    end;

    if bSubNegoc then begin
        if Ch = TNCH_SE then begin
            bSubNegoc    := FALSE;
            NegociateSubOption(strSubOption);
            strSubOption := '';
        end
        else
            strSubOption := strSubOption + Ch;
        Exit;
    end;

    if bIAC then begin
        case Ch of
        TNCH_IAC: begin
                      AddChar(Ch);
                      bIAC := FALSE;
                  end;
        TNCH_DO, TNCH_WILL, TNCH_DONT, TNCH_WONT:
                  begin
                      bIAC   := FALSE;
                      chVerb := Ch;
                  end;
        TNCH_EOR:
            begin
                DebugString('TNCH_EOR' + #13 + #10);
                bIAC   := FALSE;
                if Assigned(FOnEOR) then
                    FOnEOR(Self);
            end;
        TNCH_SB:
            begin
{                DebugString('Subnegociation' + #13 + #10); }
                bSubNegoc := TRUE;
                bIAC      := FALSE;
            end;
        else
            DebugString('Unknown ' + IntToHex(ord(Ch), 2) + ' ''' + Ch + '''' + #13 + #10);
            bIAC := FALSE;
        end;

        Exit;
    end;

    case Ch of
    TNCH_EL:
        begin
            DebugString('TNCH_EL' + #13 + #10);
            AddChar(Ch);
        end;
    TNCH_EC:
        begin
            DebugString('TNCH_EC' + #13 + #10);
            AddChar(Ch);
        end;
    TNCH_AYT:
        begin
            DebugString('TNCH_AYT' + #13 + #10);
            AddChar(Ch);
        end;
    TNCH_IP:
        begin
            DebugString('TNCH_IP' + #13 + #10);
            AddChar(Ch);
        end;
    TNCH_AO:
        begin
            DebugString('TNCH_AO' + #13 + #10);
            AddChar(Ch);
        end;
    TNCH_IAC:
        begin
            bIAC := TRUE
        end;
    else
        AddChar(Ch);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.

