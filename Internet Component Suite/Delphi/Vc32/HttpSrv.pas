{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Description:  THttpServer implement the HTTP server protocol, that is a
              web server kernel.
Creation:     Oct 10, 1999
Version:      1.00 BETA 7
EMail:        http://users.swing.be/francois.piette  francois.piette@swing.be
              http://www.rtfm.be/fpiette             francois.piette@rtfm.be
              francois.piette@pophost.eunet.be
Support:      Use the mailing list twsocket@rtfm.be See website for details.
Legal issues: Copyright (C) 1996-2000 by François PIETTE
              Rue de Grady 24, 4053 Embourg, Belgium. Fax: +32-4-365.74.56
              <francois.piette@pophost.eunet.be><francois.piette@swing.be>

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
Nov 12, 1999 Beta 3 Added Linger properties
Apr 23, 2000 Beta 4 Added Delphi 1 compatibility
             Made everything public in THttpConnection because BCB has problems
             when deriving a component from Delphi and protected functions.
Oct 29, 2000 Beta 5 Added Client[] property and IsClient() method.
Nov 11, 2000 Beta 6 Added code from Sven <schmidts@cdesign.de> to set
             Last-Modified header line. Need some more changes !
Nov 12, 2000 Beta 7 Finished Last-Modified implementation.
             Corrected TriggerServerStopped.

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit HttpSrv;

interface

uses
  WinTypes, WinProcs, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, WSocket, WSocketS;

const
    THttpServerVersion = 100;
    CopyRight : String = ' THttpServer (c) 1999-2000 F. Piette V1.00 BETA 7 ';
    WM_HTTP_DONE       = WM_USER + 40;

type
    THttpConnection      = class;
    THttpConnectionClass = class of THttpConnection;
    THttpGetFlag         = (hgSendDoc, hgSendStream, hgWillSendMySelf, hg404, hgAcceptData);
    THttpSendType        = (httpSendHead, httpSendDoc);
    THttpGetEvent        = procedure (Sender    : TObject;
                                      Client    : TObject;
                                      var Flags : THttpGetFlag) of object;
    THttpGetConnEvent    = procedure (Sender    : TObject;
                                      var Flags : THttpGetFlag) of object;
    THttpConnectEvent    = procedure (Sender    : TObject;
                                      Client    : TObject;
                                      Error     : Word) of object;
    THttpPostedDataEvent = procedure (Sender    : TObject;
                                      Client    : TObject;
                                      Error     : Word) of object;
    THttpConnectionState = (hcRequest, hcHeader, hcPostedData);

    { THttpConnection is used to handle client connections }
    THttpConnection = class(TWSocketClient)
    public
        FRcvdLine              : String;
        FMethod                : String;
        FVersion               : String;
        FPath                  : String;
        FParams                : String;
        FRequestHeader         : TStringList;
        FState                 : THttpConnectionState;
        FDocDir                : String;
        FDefaultDoc            : String;
        FDocument              : String;
        FDocStream             : TStream;
        FDocBuf                : PChar;
        FLastModified          : TDateTime;
        FAnswerContentType     : String;
        FRequestContentLength  : Integer;
        FRequestContentType    : String;
        FRequestAccept         : String;
        FRequestReferer        : String;
        FRequestAcceptLanguage : String;
        FRequestAcceptEncoding : String;
        FRequestUserAgent      : String;
        FRequestHost           : String;
        FRequestConnection     : String;
        FAcceptPostedData      : Boolean;
        FOnGetDocument         : THttpGetConnEvent;
        FOnHeadDocument        : THttpGetConnEvent;
        FOnPostDocument        : THttpGetConnEvent;
        FOnPostedData          : TDataAvailable;
        procedure ConnectionDataAvailable(Sender: TObject; Error : Word);
        procedure ConnectionDataSent(Sender : TObject; Error : WORD);
        procedure ParseRequest;
        procedure ProcessRequest;
        procedure ProcessGet;
        procedure ProcessHead;
        procedure ProcessPost;
        procedure SendDocument(SendType : THttpSendType);
        procedure SendStream;
        procedure Answer404;
        procedure WndProc(var MsgRec: TMessage); override;
        procedure WMHttpDone(var msg: TMessage); message WM_HTTP_DONE;
        procedure TriggerGetDocument(var Flags : THttpGetFlag); virtual;
        procedure TriggerHeadDocument(var Flags : THttpGetFlag); virtual;
        procedure TriggerPostDocument(var Flags : THttpGetFlag); virtual;
    public
        constructor Create(AOwner: TComponent); override;
        destructor  Destroy; override;
        { Method contains GET/POST/HEAD as requested by client }
        property Method                    : String read  FMethod;
        { Version contains HTTP version from client request }
        property Version                   : String read  FVersion;
        { The whole header as received from client }
        property RequestHeader             : TStringList
                                                    read  FRequestHeader;
        { Stream used to send reply to client }
        property DocStream                 : TStream
                                                    read  FDocStream
                                                    write FDocStream;
        { All RequestXXX are header fields from request header }
        property RequestContentLength      : Integer
                                                    read  FRequestContentLength;
        property RequestContentType        : String read  FRequestContentType;
        property RequestAccept             : String read  FRequestAccept;
        property RequestReferer            : String read  FRequestReferer;
        property RequestAcceptLanguage     : String read  FRequestAcceptLanguage;
        property RequestAcceptEncoding     : String read  FRequestAcceptEncoding;
        property RequestUserAgent          : String read  FRequestUserAgent;
        property RequestHost               : String read  FRequestHost;
        property RequestConnection         : String read  FRequestConnection;
    published
        { Where all documents are stored. Default to c:\wwwroot }
        property DocDir         : String            read  FDocDir
                                                    write FDocDir;
        { Default document name. Default to index.html }
        property DefaultDoc     : String            read  FDefaultDoc
                                                    write FDefaultDoc;
        { Complete document path and file name on local file system }
        property Document       : String            read  FDocument
                                                    write FDocument;
        { Document path as requested by client }
        property Path           : String            read  FPath
                                                    write FPath;
        { Parameters in request (Question mark is separator) }
        property Params         : String            read  FParams
                                                    write FParams;
        { Triggered when client sent GET request }
        property OnGetDocument  : THttpGetConnEvent read  FOnGetDocument
                                                    write FOnGetDocument;
        { Triggered when client sent HEAD request }
        property OnHeadDocument : THttpGetConnEvent read  FOnHeadDocument
                                                    write FOnHeadDocument;
        { Triggered when client sent POST request }
        property OnPostDocument : THttpGetConnEvent read  FOnPostDocument
                                                    write FOnPostDocument;
        { Triggered when client sent POST request and data is available }
        property OnPostedData   : TDataAvailable    read  FOnPostedData
                                                    write FOnPostedData;
    end;

    { This is the HTTP server component handling all HTTP connection }
    { service. Most of the work is delegated to a TWSocketServer     }
    THttpServer = class(TComponent)
    protected
        { FWSocketServer will handle all client management work }
        FWSocketServer      : TWSocketServer;
        FPort               : String;
        FAddr               : String;
        FClientClass        : THttpConnectionClass;
        FDocDir             : String;
        FDefaultDoc         : String;
        FLingerOnOff        : TSocketLingerOnOff;
        FLingerTimeout      : Integer;              { In seconds, 0 = disabled }
        FOnServerStarted    : TNotifyEvent;
        FOnServerStopped    : TNotifyEvent;
        FOnClientConnect    : THttpConnectEvent;
        FOnClientDisconnect : THttpConnectEvent;
        FOnGetDocument      : THttpGetEvent;
        FOnHeadDocument     : THttpGetEvent;
        FOnPostDocument     : THttpGetEvent;
        FOnPostedData       : THttpPostedDataEvent;
        procedure Notification(AComponent: TComponent; operation: TOperation); override;
        procedure WSocketServerClientConnect(Sender : TObject;
                                             Client : TWSocketClient;
                                             Error  : Word);
        procedure WSocketServerClientCreate(Sender : TObject;
                                            Client : TWSocketClient);
        procedure WSocketServerClientDisconnect(Sender : TObject;
                                                Client : TWSocketClient;
                                                Error  : Word);
        procedure WSocketServerSessionClosed(Sender : TObject;
                                             Error  : Word);
        procedure WSocketServerChangeState(Sender : TObject;
                                           OldState, NewState : TSocketState);
        procedure TriggerServerStarted; virtual;
        procedure TriggerServerStopped; virtual;
        procedure TriggerClientConnect(Client : TObject; Error  : Word);
        procedure TriggerClientDisconnect(Client : TObject; Error : Word);
        procedure TriggerGetDocument(Sender     : TObject;
                                     var Flags  : THttpGetFlag);
        procedure TriggerHeadDocument(Sender     : TObject;
                                      var Flags  : THttpGetFlag);
        procedure TriggerPostDocument(Sender     : TObject;
                                      var Flags  : THttpGetFlag); virtual;
        procedure TriggerPostedData(Sender     : TObject;
                                    Error      : WORD); virtual;
        procedure SetPort(newValue : String);
        procedure SetAddr(newValue : String);
        function  GetClientCount : Integer;
        function  GetClient(nIndex : Integer) : THttpConnection;
    public
        constructor Create(AOwner: TComponent); override;
        destructor  Destroy; override;
        procedure   Start; virtual;
        procedure   Stop; virtual;
        { Check  if a given object is one of our clients }
        function    IsClient(SomeThing : TObject) : Boolean;
        { Runtime readonly property which gives number of connected clients }
        property    ClientCount : Integer        read  GetClientCount;
        { Client[] give direct access to anyone of our clients }
        property    Client[nIndex : Integer] : THttpConnection
                                                 read  GetClient;
        { Runtime property which tell the component class which has to be }
        { instanciated to handle client connection                        }
        property    ClientClass : THttpConnectionClass
                                                 read  FClientClass
                                                 write FClientClass;
    published
        { We will listen to that port. Default to 80 for http service }
        property Port          : String          read  FPort
                                                 write SetPort;
        { We will use that interface to listen. 0.0.0.0 means all     }
        { available interfaces                                        }
        property Addr          : String          read  FAddr
                                                 write SetAddr;
        { Where all documents are stored. Default to c:\wwwroot }
        property DocDir        : String          read  FDocDir
                                                 write FDocDir;
        { Default document name. Default to index.html }
        property DefaultDoc    : String          read  FDefaultDoc
                                                 write FDefaultDoc;
        property LingerOnOff   : TSocketLingerOnOff
                                                 read  FLingerOnOff
                                                 write FLingerOnOff;
        property LingerTimeout : Integer         read  FLingerTimeout
                                                 write FLingerTimeout;
        { OnServerStrated is triggered when server has started listening }
        property OnServerStarted    : TNotifyEvent
                                                 read  FOnServerStarted
                                                 write FOnServerStarted;
        { OnServerStopped is triggered when server has stopped listening }
        property OnServerStopped    : TNotifyEvent
                                                 read  FOnServerStopped
                                                 write FOnServerStopped;
        { OnClientConnect is triggered when a client has connected }
        property OnClientConnect    : THttpConnectEvent
                                                 read  FOnClientConnect
                                                 write FOnClientConnect;
        { OnClientDisconnect is triggered when a client is about to }
        { disconnect.                                               }
        property OnClientDisconnect : THttpConnectEvent
                                                 read  FOnClientDisconnect
                                                 write FOnClientDisconnect;
        { OnGetDocument is triggered when a client sent GET request    }
        { You can either do nothing and let server handle all work, or }
        { you can build a document on the fly or refuse access.        }
        property OnGetDocument      : THttpGetEvent
                                                 read  FOnGetDocument
                                                 write FOnGetDocument;
        { OnGetDocument is triggered when a client sent HEAD request   }
        { You can either do nothing and let server handle all work, or }
        { you can build a document header on the fly or refuse access. }
        property OnHeadDocument     : THttpGetEvent
                                                 read  FOnHeadDocument
                                                 write FOnHeadDocument;
        { OnGetDocument is triggered when a client sent POST request   }
        { You have to tell if you accept data or not. If you accept,   }
        { you'll get OnPostedData event with incomming data.           }
        property OnPostDocument     : THttpGetEvent
                                                 read  FOnPostDocument
                                                 write FOnPostDocument;
        { On PostedData is triggered when client post data and you     }
        { accepted it from OnPostDocument event.                       }
        { When you've got all data, you have to build a reply to be    }
        { sent to client.                                              }
        property OnPostedData       : THttpPostedDataEvent
                                                 read  FOnPostedData
                                                 write FOnPostedData;
    end;

{ Retrieve a single value by name out of an URL encoded data stream.        }
function ExtractURLEncodedValue(
    Msg       : PChar;             { URL Encoded stream                     }
    Name      : String;            { Variable name to look for              }
    var Value : String): Boolean;  { Where to put variable value            }
procedure Register;

implementation


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure Register;
begin
    RegisterComponents('FPiette', [THttpServer]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF VER80}
procedure SetLength(var S: string; NewLength: Integer);
begin
    S[0] := chr(NewLength);
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF VER80}
function TrimRight(Str : String) : String;
var
    i : Integer;
begin
    i := Length(Str);
    while (i > 0) and (Str[i] = ' ') do
        i := i - 1;
    Result := Copy(Str, 1, i);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TrimLeft(Str : String) : String;
var
    i : Integer;
begin
    if Str[1] <> ' ' then
        Result := Str
    else begin
        i := 1;
        while (i <= Length(Str)) and (Str[i] = ' ') do
            i := i + 1;
        Result := Copy(Str, i, Length(Str) - i + 1);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function Trim(Str : String) : String;
begin
    Result := TrimLeft(TrimRight(Str));
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor THttpServer.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);
    FWSocketServer := TWSocketServer.Create(Self);
    FClientClass   := THttpConnection;
    FAddr          := '0.0.0.0';
    FPort          := '80';
    FDefaultDoc    := 'index.html';
    FDocDir        := 'c:\wwwroot';
    FLingerOnOff   := wsLingerNoSet;
    FLingerTimeout := 0;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor THttpServer.Destroy;
begin
    if Assigned(FWSocketServer) then begin
        FWSocketServer.Destroy;
        FWSocketServer := nil;
    end;
    inherited Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Called by destructor when child component is created or destroyed.        }
procedure THttpServer.Notification(
    AComponent : TComponent;
    Operation  : TOperation);
begin
    inherited Notification(AComponent, Operation);
    if Operation = opRemove then begin
        if AComponent = FWSocketServer then
            FWSocketServer := nil;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Start the server. That is make FWSocketServer listening to the port.      }
procedure THttpServer.Start;
begin
    { Create a new FWSocketServer if needed }
    if not Assigned(FWSocketServer) then
        FWSocketServer := TWSocketServer.Create(Self);
    { If already listening, then do nothing }
    if FWSocketServer.State = wsListening then
        Exit;
    { Pass al parameters to FWSocketServer and make it listen }
    FWSocketServer.ClientClass        := FClientClass;
    FWSocketServer.OnClientCreate     := WSocketServerClientCreate;
    FWSocketServer.OnClientConnect    := WSocketServerClientConnect;
    FWSocketServer.OnClientDisconnect := WSocketServerClientDisconnect;
    FWSocketServer.OnSessionClosed    := WSocketServerSessionClosed;
    FWSocketServer.OnChangeState      := WSocketServerChangeState;
    FWSocketServer.Banner             := '';
    FWSocketServer.Proto              := 'tcp';
    FWSocketServer.Port               := FPort;
    FWSocketServer.Addr               := FAddr;
    FWSocketServer.Listen;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpServer.Stop;
var
    I : Integer;
begin
    if not Assigned(FWSocketServer) then
        Exit;
    FWSocketServer.Close;
    { Disconnect all clients }
    for I := FWSocketServer.ClientCount - 1 downto 0 do begin
        try
            FWSocketServer.Client[I].Abort;
        except
            { Ignore any exception here }
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpServer.SetPort(newValue : String);
begin
    if newValue = FPort then
        Exit;
    FPort := newValue;
    { If server is already listening, then stop it and restart it with      }
    { new port. Do not disconnect already connected clients.                }
    if Assigned(FWSocketServer) and
       (FWSocketServer.State = wsListening) then begin
        FWSocketServer.Close;
        Start;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpServer.SetAddr(newValue : String);
begin
    if newValue = FAddr then
        Exit;
    FAddr := newValue;
    { If server is already listening, then stop it and restart it with      }
    { new Addr. Do not disconnect already connected clients.                }
    if Assigned(FWSocketServer) and
       (FWSocketServer.State = wsListening) then begin
        FWSocketServer.Close;
        Start;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Get function for ClientCount property. Just return value from             }
{ FWSocketServer.                                                           }
function THttpServer.GetClientCount;
begin
    if not Assigned(FWSocketServer) then
        Result := 0
    else
        Result := FWSocketServer.ClientCount;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Get function for Client[] property. Just return value from                }
{ FWSocketServer.                                                           }
function THttpServer.GetClient(nIndex : Integer) : THttpConnection;
begin
    if not Assigned(FWSocketServer) then
        Result := nil
    else
        Result := THttpConnection(FWSocketServer.Client[nIndex]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Check is an object is one of our clients. Just return value from          }
{ FWSocketServer.                                                           }
function THttpServer.IsClient(SomeThing : TObject) : Boolean;
begin
    if not Assigned(FWSocketServer) then
        Result := FALSE
    else
        Result := FWSocketServer.IsClient(SomeThing);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This event handler is triggered when state of server socket has changed.  }
{ We use it to trigger our OnServerStarted event.                           }
procedure THttpServer.WSocketServerChangeState(
    Sender : TObject;
    OldState, NewState : TSocketState);
begin
    if newState = wsListening then
        TriggerServerStarted;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpServer.WSocketServerSessionClosed(
    Sender : TObject;
    Error  : Word);
begin
    TriggerServerStopped;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ A new client component has been created                                   }
procedure THttpServer.WSocketServerClientCreate(
    Sender : TObject;
    Client : TWSocketClient);
begin
    Client.LingerOnOff   := FLingerOnOff;
    Client.LingerTimeout := FLingerTimeout;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ A new client just connected. Setup things to handle his requests.         }
{ HTTP header is line oriented so we turn line mode on. We use LF as end of }
{ line character altough HTTP uses CR/LF pair as end of line, because many  }
{ Unix client do not respect standards and use single LF...                 }
{ HTTP is not interactive, so we turn line editing to false (faster).       }
procedure THttpServer.WSocketServerClientConnect(
    Sender : TObject;
    Client : TWSocketClient;
    Error  : Word);
begin
    THttpConnection(Client).LineMode       := TRUE;
    THttpConnection(Client).LineEdit       := FALSE;
    THttpConnection(Client).LineEnd        := #10;
    THttpConnection(Client).DocDir         := Self.DocDir;
    THttpConnection(Client).DefaultDoc     := Self.DefaultDoc;
    THttpConnection(Client).OnGetDocument  := TriggerGetDocument;
    THttpConnection(Client).OnHeadDocument := TriggerHeadDocument;
    THttpConnection(Client).OnPostDocument := TriggerPostDocument;
    THttpConnection(Client).OnPostedData   := TriggerPostedData;
    TriggerClientConnect(Client, Error);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ A client is about to disconnect.                                          }
procedure THttpServer.WSocketServerClientDisconnect(
    Sender : TObject;
    Client : TWSocketClient;
    Error  : Word);
begin
    TriggerClientDisconnect(Client, Error);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpServer.TriggerServerStarted;
begin
    if Assigned(FOnServerStarted) then
        FOnServerStarted(Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpServer.TriggerServerStopped;
begin
    if Assigned(FOnServerStopped) then
        FOnServerStopped(Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpServer.TriggerClientConnect(
    Client : TObject;
    Error  : Word);
begin
    if Assigned(FOnClientConnect) then
        FOnClientConnect(Self, Client, Error);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpServer.TriggerClientDisconnect(
    Client : TObject;
    Error  : Word);
begin
    if Assigned(FOnClientDisconnect) then
        FOnClientDisconnect(Self, Client, Error);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpServer.TriggerGetDocument(
     Sender     : TObject;
     var Flags  : THttpGetFlag);
begin
    if Assigned(FOnGetDocument) then
        FOnGetDocument(Self, Sender, Flags);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpServer.TriggerHeadDocument(
     Sender     : TObject;
     var Flags  : THttpGetFlag);
begin
    if Assigned(FOnHeadDocument) then
        FOnHeadDocument(Self, Sender, Flags);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpServer.TriggerPostedData(Sender     : TObject;
                                        Error      : WORD);
begin
    if Assigned(FOnPostedData) then
        FOnPostedData(Self, Sender, Error);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpServer.TriggerPostDocument(
     Sender     : TObject;
     var Flags  : THttpGetFlag);
begin
    if Assigned(FOnPostDocument) then
        FOnPostDocument(Self, Sender, Flags);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor THttpConnection.Create(AOwner : TComponent);
begin
    inherited Create(AOwner);
    LineMode        := TRUE;
    LineEdit        := FALSE;
    LineEnd         := #10;
    FRequestHeader  := TStringList.Create;
    FState          := hcRequest;
    OnDataAvailable := ConnectionDataAvailable;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor THttpConnection.Destroy;
begin
    if Assigned(FRequestHeader) then begin
        FRequestHeader.Destroy;
        FRequestHeader := nil;
    end;
    if Assigned(FDocStream) then begin
        FDocStream.Destroy;
        FDocStream := nil;
    end;
    if Assigned(FDocBuf) then begin
        FreeMem(FDocBuf, BufSize);
        FDocBuf := nil;
    end;
    inherited Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpConnection.WndProc(var MsgRec: TMessage);
begin
    with MsgRec do begin
        { We *MUST* handle all exception to avoid application shutdown }
        try
            if Msg = WM_HTTP_DONE then
                WMHttpDone(MsgRec)
            else
                inherited WndProc(MsgRec);
        except
            on E:Exception do
                HandleBackGroundException(E);
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpConnection.WMHttpDone(var msg: TMessage);
begin
     FState := hcRequest;
     if CompareText(FRequestConnection, 'Keep-Alive') <> 0 then
         CloseDelayed;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This procedure is called each time data is available from a client.       }
{ We use FState variable to keep track of the where we are in the http      }
{ protocol: request command, header line or posted data.                    }
procedure THttpConnection.ConnectionDataAvailable(Sender: TObject; Error : Word);
var
    Len     : Integer;
    I       : Integer;
begin
    { If we are in data state, then the application has to receive data }
    if FState = hcPostedData then begin
        if FAcceptPostedData and Assigned(FOnPostedData) then
            FOnPostedData(Self, Error)
        else
            { No one is willing data, received it and throw it away }
            FRcvdLine := ReceiveStr;
        Exit;
    end;
    { We use line mode. We will receive complete lines }
    FRcvdLine := ReceiveStr;
    { Remove trailing CR/LF }
    Len := Length(FRcvdLine);
    if (Len > 0) and (FRcvdLine[Len] = #10) then begin
        Dec(Len);
        if (Len > 0) and (FRcvdLine[Len] = #13) then
            Dec(Len);
        SetLength(FRcvdLine, Len);
    end;
    if FState = hcRequest then begin
        { We just start a new request. Initialize all header variables }
        FRequestContentType    := '';
        FRequestContentLength  := 0;
        FRequestContentType    := '';
        FRequestAccept         := '';
        FRequestReferer        := '';
        FRequestAcceptLanguage := '';
        FRequestAcceptEncoding := '';
        FRequestUserAgent      := '';
        FRequestHost           := '';
        FRequestConnection     := '';
        { The line we just received is HTTP command, parse it  }
        ParseRequest;
        { Next lines will be header lines }
        FState := hcHeader;
        Exit;
    end;
    { We can comes here only in hcHeader state }
    if FRcvdLine = '' then begin
        { Last header line is an empty line. Then we enter data state }
        FState := hcPostedData;
        { We will process request before receiving data because application }
        { has to setup things to be able to receive posted data             }
        ProcessRequest;
        Exit;
    end;
    { We comes here for normal header line. Extract some interesting variables }
    I := Pos(':', FRcvdLine);
    if I > 0 then begin
        try
            repeat
                Inc(I);
            until (I > Length(FRcvdLine)) or (FRcvdLine[I] <> ' ');
            if StrLIComp(@FRcvdLine[1], 'content-type:', 13) = 0 then
                FRequestContentType := Copy(FRcvdLine, I, Length(FRcvdLine))
            else if StrLIComp(@FRcvdLine[1], 'content-length:', 15) = 0 then
                FRequestContentLength := StrToInt(Copy(FRcvdLine, I, Length(FRcvdLine)))
            else if StrLIComp(@FRcvdLine[1], 'Accept:', 7) = 0 then
                FRequestAccept:= Copy(FRcvdLine, I, Length(FRcvdLine))
            else if StrLIComp(@FRcvdLine[1], 'Referer:', 8) = 0 then
                FRequestReferer := Copy(FRcvdLine, I, Length(FRcvdLine))
            else if StrLIComp(@FRcvdLine[1], 'Accept-Language:', 16) = 0 then
                FRequestAcceptLanguage := Copy(FRcvdLine, I, Length(FRcvdLine))
            else if StrLIComp(@FRcvdLine[1], 'Accept-Encoding:', 16) = 0 then
                FRequestAcceptEncoding := Copy(FRcvdLine, I, Length(FRcvdLine))
            else if StrLIComp(@FRcvdLine[1], 'User-Agent:', 11) = 0 then
                FRequestUserAgent := Copy(FRcvdLine, I, Length(FRcvdLine))
            else if StrLIComp(@FRcvdLine[1], 'Host:', 5) = 0 then
                FRequestHost := Copy(FRcvdLine, I, Length(FRcvdLine))
            else if StrLIComp(@FRcvdLine[1], 'Connection:', 11) = 0 then
                FRequestConnection := Copy(FRcvdLine, I, Length(FRcvdLine));
        except
            { Ignore any exception in parsing header line }
        end;
    end;
    FRequestHeader.Add(FRcvdLine);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Request is in FRcvdLine property.                                         }
{ Split it into FMethod, FPath, FVersion and parameters.                    }
procedure THttpConnection.ParseRequest;
var
    I, J : Integer;
begin
    I := 1;
    while (I <= Length(FRcvdLine)) and (FRcvdLine[I] <> ' ') do
        Inc(I);
    FMethod := UpperCase(Copy(FRcvdLine, 1, I - 1));
    Inc(I);
    while (I <= Length(FRcvdLine)) and (FRcvdLine[I] = ' ') do
        Inc(I);
    J := I;
    while (I <= Length(FRcvdLine)) and (FRcvdLine[I] <> ' ') do
        Inc(I);
    FPath := Copy(FRcvdLine, J, I - J);
    { Find parameters }
    J := Pos('?', FPath);
    if J <= 0 then
        FParams := ''
    else begin
        FParams := Copy(FPath, J + 1, Length(FPath));
        FPath   := Copy(FPath, 1, J - 1);
    end;
    Inc(I);
    while (I <= Length(FRcvdLine)) and (FRcvdLine[I] = ' ') do
        Inc(I);
    J := I;
    while (I <= Length(FRcvdLine)) and (FRcvdLine[I] <> ' ') do
        Inc(I);
    FVersion := Trim(UpperCase(Copy(FRcvdLine, J, I - J)));
    if FVersion = '' then
        FVersion := 'HTTP/1.0';
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpConnection.Answer404;
var
    Body : String;
begin
    Body := '<HTML><HEAD><TITLE>404 Not Found</TITLE></HEAD>' +
            '<BODY><H1>404 Not Found</H1>The requested URL ' + FPath +
            ' was not found on this server.<P></BODY></HTML>' + #13#10;
    PutStringInSendBuffer(FVersion + ' 404 Not Found' + #13#10 +
                          'Content-Type: text/html' + #13#10 +
                          'Content-Length: ' + IntToStr(Length(Body)) + #13#10 +
                          #13#10);
    SendStr(Body);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ As its name implies...                                                    }
procedure THttpConnection.ProcessRequest;
var
    I : Integer;
begin
    if FPath[1] = '/' then
        FDocument := FDocDir + FPath
    else
        FDocument := FDocDir + '\' + FPath;
    { Check for default document }
    if FDocument[Length(FDocument)] = '/' then
        FDocument := FDocument + FDefaultDoc;
    { Change slashes to backslashes }
    for I := 1 to Length(FDocument) do begin
        if FDocument[I] = '/' then
            FDocument[I] := '\';
    end;

    if FMethod = 'GET' then
        ProcessGet
    else if FMethod = 'POST' then
        ProcessPost
    else if FMethod = 'HEAD' then
        ProcessHead
    else begin
        Answer404;
        CloseDelayed;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpConnection.TriggerGetDocument(var Flags : THttpGetFlag);
begin
    if Assigned(FOnGetDocument) then
        FOnGetDocument(Self, Flags);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpConnection.TriggerHeadDocument(var Flags : THttpGetFlag);
begin
    if Assigned(FOnHeadDocument) then
        FOnHeadDocument(Self, Flags);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpConnection.TriggerPostDocument(var Flags : THttpGetFlag);
begin
    if Assigned(FOnPostDocument) then
        FOnPostDocument(Self, Flags);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpConnection.ProcessPost;
var
    Flags : THttpGetFlag;
begin
    Flags := hg404;
    TriggerPostDocument(Flags);
    if Flags = hg404 then begin
        Answer404;
        CloseDelayed;
        Exit;
    end
    else if Flags = hgAcceptData then
        FAcceptPostedData := TRUE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpConnection.ProcessHead;
var
    Flags : THttpGetFlag;
begin
    Flags := hgSendDoc;
    TriggerHeadDocument(Flags);
    case Flags of
    hg404:
        begin
            Answer404;
            CloseDelayed;
        end;
    hgSendDoc:
        begin
            if FileExists(FDocument) then
                SendDocument(httpSendHead)
            else begin
                Answer404;
                CloseDelayed;
            end;
        end;
    hgSendStream:
        SendStream;
    hgWillSendMySelf:
        { Nothing to do };
    else
        CloseDelayed;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpConnection.ProcessGet;
var
    Flags : THttpGetFlag;
begin
    Flags := hgSendDoc;
    TriggerGetDocument(Flags);
    case Flags of
    hg404:
        begin
            Answer404;
            CloseDelayed;
        end;
    hgSendDoc:
        begin
            if FileExists(FDocument) then
                SendDocument(httpSendDoc)
            else begin
                Answer404;
                CloseDelayed;
            end;
        end;
    hgSendStream:
        SendStream;
    hgWillSendMySelf:
        { Nothing to do };
    else
        CloseDelayed;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function DocumentToContentType(FileName : String) : String;
var
    Ext : String;
begin
    { We probably should the registry to find MIME type for known file types }
    Ext := LowerCase(ExtractFileExt(FileName));
    if Length(Ext) > 1 then
        Ext := Copy(Ext, 2, Length(Ext));
    if (Ext = 'htm') or (Ext = 'html') then
        Result := 'text/html'
    else if Ext = 'gif' then
        Result := 'image/gif'
    else if Ext = 'bmp' then
        Result := 'image/bmp'
    else if (Ext = 'jpg') or (Ext = 'jpeg') then
        Result := 'image/jpeg'
    else if Ext = 'txt' then
        Result := 'text/plain'
    else
        Result := 'application/binary';
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function RFC1123_Date(aDate : TDateTime) : String;
const
    StrWeekDay : String = 'MonTueWedThuFriSatSun';
    StrMonth   : String = 'JanFebMarAprMayJunJulAugSepOctNovDec';
var
    Year, Month, Day       : Word;
    Hour, Min,   Sec, MSec : Word;
    DayOfWeek              : Word;
begin
    DecodeDate(aDate, Year, Month, Day);
    DecodeTime(aDate, Hour, Min,   Sec, MSec);
    DayOfWeek := ((Trunc(aDate) - 2) mod 7);
    Result := Copy(StrWeekDay, 1 + DayOfWeek * 3, 3) + ', ' +
              Format('%2.2d %s %4.4d %2.2d:%2.2d:%2.2d',
                     [Day, Copy(StrMonth, 1 + 3 * (Month - 1), 3),
                      Year, Hour, Min, Sec]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Return document file date from document filename.                         }
{ Return 0 if file not found.                                               }
function FileDate(FileName : String) : TDateTime;
var
    SearchRec : TSearchRec;
    Status    : Integer;
begin
    Status := FindFirst(FileName, faAnyFile, SearchRec);
    try
        if Status <> 0 then
            Result := 0
        else
            Result := FileDateToDateTime(SearchRec.Time);
    finally
        FindClose(SearchRec);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ SendDocument will send FDocument file to remote client, build header and  }
{ sending data (if required)                                                }
procedure THttpConnection.SendDocument(SendType : THttpSendType);
var
    DocSize : Integer;
begin
    FLastModified      := FileDate(FDocument);
    FAnswerContentType := DocumentToContentType(FDocument);

    if Assigned(FDocStream) then begin
        FDocStream.Destroy;
        FDocStream := nil;
    end;
    FDocStream := TFileStream.Create(FDocument, fmOpenRead + fmShareDenyWrite);
    DocSize    := FDocStream.Size;
    { Seek to end of document because HEAD will not send actual document }
    if SendType = httpSendHead then
        FDocStream.Seek(0, soFromEnd);
    OnDataSent := ConnectionDataSent;
    { Send Header }
    PutStringInSendBuffer(
            FVersion + ' 200 OK' + #13#10 +
            'Content-Type: ' + FAnswerContentType + #13#10 +
            'Content-Length: ' + IntToStr(DocSize) + #13#10);
    if FLastModified <> 0 then
        PutStringInSendBuffer(
            'Last-Modified: ' + RFC1123_Date(FLastModified) + 'GMT' + #13#10);
    PutStringInSendBuffer(#13#10);
    Send(nil, 0);
    if SendType = httpSendDoc then
        SendStream;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpConnection.SendStream;
begin
    if not Assigned(FDocStream) then begin
        CloseDelayed;
        Exit;
    end;
    if not Assigned(FDocBuf) then
        GetMem(FDocBuf, BufSize);
    OnDataSent := ConnectionDataSent;
    ConnectionDataSent(Self, 0);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ All data in TWSocket has been sent. Read next lock from stream and send.  }
{ When end of stream is reached, closed communication.                      }
procedure THttpConnection.ConnectionDataSent(Sender : TObject; Error : WORD);
var
    Count : Integer;
begin
    if not Assigned(FDocStream) then begin
        { End of file has been reached }
        Exit;
    end;
    Count := FDocStream.Read(FDocBuf^, BufSize);
    if Count <= 0 then begin
        { End of file found }
        FDocStream.Destroy;
        FDocStream := nil;
        ShutDown(1);
{$IFNDEF VER80}
        Sleep(0);
{$ENDIF}
        PostMessage(Handle, WM_HTTP_DONE, 0, 0);
        Exit;
    end;
    Send(FDocBuf, Count);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function xdigit(Ch : char) : Integer;
begin
    if ch in ['0'..'9'] then
        Result := ord(Ch) - ord('0')
    else
        Result := (ord(Ch) and 15) + 9;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function isxdigit(Ch : char) : Boolean;
begin
    Result := (ch in ['0'..'9']) or (ch in ['a'..'z']) or (ch in ['A'..'Z']);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function htoin(value : PChar; len : Integer) : Integer;
var
    i : Integer;
begin
    Result := 0;
    i      := 0;
    while (i < len) and (Value[i] = ' ') do
        i := i + 1;
    while (i < len) and (isxDigit(Value[i])) do begin
        Result := Result * 16 + xdigit(Value[i]);
        i := i + 1;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function htoi2(value : PChar) : Integer;
begin
    Result := htoin(value, 2);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Retrieve a single value by name out of an URL encoded data stream         }
{ In the stream, every space is replaced by a '+'. The '%' character is     }
{ an escape character. The next two are 2 digits hexadecimal codes ascii    }
{ code value. The stream is consitueted by name=value couples separated     }
{ by a single '&' character. The special characters are coded by the '%'    }
{ followed by hex-ascii character code.                                     }
function ExtractURLEncodedValue(
    Msg       : PChar;        { URL Encoded stream                     }
    Name      : String;       { Variable name to look for              }
    var Value : String)       { Where to put variable value            }
    : Boolean;                { Found or not found that's the question }
var
    NameLen : Integer;
    Ch      : Char;
    P, Q    : PChar;
begin
    Result  := FALSE;
    Value   := '';
    if Msg = nil then         { Empty source }
        Exit;

    NameLen := Length(Name);

    P := Msg;
    while P^ <> #0 do begin
        Q := P;
        while (P^ <> #0) and (P^ <> '=') do
            Inc(P);
        if P^ = '=' then
            Inc(P);
        if StrLIComp(Q, @Name[1], NameLen) = 0 then begin
            while (P^ <> #0) and (P^ <> '&') do begin
                Ch := P^;
                if Ch = '%' then begin
                    Ch := chr(htoi2(P + 1));
                    Inc(P, 2);
                end
                else if Ch = '+' then
                    Ch := ' ';
                Value := Value + Ch;
                Inc(P);
            end;
            Result := TRUE;
            break;
         end;
         while (P^ <> #0) and (P^ <> '&') do
             Inc(P);
        if P^ = '&' then
            Inc(P);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.
