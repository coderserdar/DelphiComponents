{*********************************************************}
{* FlashFiler: Base unit for transports & cmd handlers   *}
{*********************************************************}

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
 * The Original Code is TurboPower FlashFiler
 *
 * The Initial Developer of the Original Code is
 * TurboPower Software
 *
 * Portions created by the Initial Developer are Copyright (C) 1996-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

{$I ffdefine.inc}

unit ffllcomm;

interface

uses
  classes,
  forms,
  windows,
  ffllbase,
  ffllcomp,
  fflleng,
  fflllog,
  ffllreq,
  ffllthrd,
  ffnetmsg;


type

  { TffDataMessage contains the message information passed from a transport
    to a server command handler, plugin command handler, or engine manager. }
  PffDataMessage = ^TffDataMessage;
  TffDataMessage = record
    dmMsg        : Longint;         { the unique ID identifying the msg type }
    dmClientID   : TffClientID;     { the client sending the message }
    dmRequestID  : Longint;         { the unique ID of the request }
    dmTime       : TffWord32;       { the time the message was received }
    dmRetryUntil : TffWord32;
    dmErrorCode  : TffResult;
    dmData       : pointer;
    dmDataLen    : TffMemSize;
  end;

  { The following options may be used to control logging in the transports.
    Values:
      fftpLogErrors - Write errors to the event log.
      fftpLogRequests - Write requests to the event log.  If in Send mode
        then logs all sent requests.  If in Listen mode then logs all received
        requests.
      fftpLogReplies - If in Send mode then logs all received replies.  If in
        Listen mode then logs all sent replies. }
  TffTransportLogOption = (fftpLogErrors,
                           fftpLogRequests, fftpLogReplies);
  TffTransportLogOptions = set of TffTransportLogOption;

  { A transport will send a request to the server.  When the reply is
    received, the transport must notify the object submitting the request.
    To be notified, the object submitting the request must define a procedure
    of type TffReplyCallback.  Parameters passed to this procedure are as
    follows:
      @param msgID The message identifier returned by the server.
      @param errorCode The error code returned by the server.
      @param replyData The data returned by the server.
      @param replyDataLen The length of the data returned by the server.
      @param replyType The format of the data: byteArray (e.g., packed record)
        or stream.
      @param replyCookie The replyCookie parameter originally supplied to the
        TffBaseTransport.Request method.  The meaning of this parameter is
        specific to the object submitting the request.  For the
        TffRemoteServerEngine, this is a pointer to TffProxyClient.
  }
  TffReplyCallback = procedure(msgID        : Longint;
                               errorCode    : TffResult;
                               replyData    : pointer;
                               replyDataLen : Longint;
                               replyCookie  : Longint);

  TffBasePluginCommandHandler = class;  { forward declaration }
  TffBaseEngineManager = class;         { forward declaration }
  TffBaseTransport = class;             { forward declaration }

  { This is the base class for the command handler.  A command handler
    receives requests from a transport and routes them to a destination.
    The base class supports routing of commands to plugins that have
    themselves with the command handler. }
  TffBaseCommandHandler = class(TffStateComponent)
  protected {private}

    FManager : TffBaseEngineManager;
      {-The engine manager that may receive shutdown and startup requests
        through this command handler.  Note that the command handler doesn't
        really know about shutdown and startup requests.  The engine manager
        is like a special plugin.  If a plugin does not handle the message,
        it is routed to the engine manager.  The engine manager may or may
        not handle the message. }

    FPlugins : TffThreadList;
      {-The list of plugins that reference the command handler. }

    FSkipInitial : Boolean;                                              {!!.01}
      {-Internal state that reflects whether the Engine Manager Wizard has
        created this component as a proxy (true) or not}
        
    FTransports : TffThreadList;
      {-The list of transports that reference the command handler. }

  protected

    procedure bchFreeMsg(msg : PffDataMessage); virtual;
      { When a transport passes off a request to the command handler, it
        becomes the command handler's responsibility to free the message
        data associated with the request.  This method frees the TffDataMessage
        structure as well as the message content contained by TffDataMessage.
        Command handlers should call this method, or find some other way of
        freeing the memory, once a request has been processed. }

    function  bchGetTransport(aInx : Integer) : TffBaseTransport; virtual;
      { Retrieves a transport from the command handler's list.}

    function  bchGetTransportCount : Longint; virtual;
      { Retrieves the number of transports owned by this command
        handler.}

    procedure bchSetEngineManager(aManager : TffBaseEngineManager); virtual;
      {-Used to set the manager to which messages may be routed. }

    procedure scSetState(const aState : TffState); override;
      { This method is called when the command handler's state changes.
        This implementation sets the state of the associated transports. }

    property SkipInitial : Boolean                                 {BEGIN !!.01}
       read FSkipInitial
       write FSkipInitial;
     { This property is used by the engine manager wizard. It's purpose is
       to keep the bchSetEngineManger routine from generating an access
       violation when the expert creates a new engine manager }      {END !!.01}
  public

    constructor Create(AOwner : TComponent); override;

    destructor Destroy; override;

    procedure FFAddDependent(ADependent : TffComponent); override;     {!!.11}
    procedure FFRemoveDependent(ADependent : TffComponent); override;  {!!.11}

    procedure Process(Msg : PffDataMessage); virtual;
      { This method is called by the transport in order to process a message.
        The default implementation forwards the message to the registered
        plugin(s).  If a plugin does not handle the message and an engine
        manager has been specified, the message is forwarded to the
        engine manager.  If the message is not handled, a reply is sent to
        the client stating the message is unrecognized. }

    property TransportCount : Longint read bchGetTransportCount;
      { The number of transports passing requests to this command handler.}

    property Transports[aInx : Longint] : TffBaseTransport
         read bchGetTransport;
      { Use this property to access the transports connected to the command
        handler. }
  published

    property EngineManager : TffBaseEngineManager
      read FManager write bchSetEngineManager;

  end;

  {This is the base class for a plugin engine.  All plugin engines inherit from
   this class.  A client application may interface with a plugin engine
   via direct calls to the plugin engine or via calls to a remote plugin
   engine.
   To create a custom plugin engine, you must do the following:
   1. Create an abstract plugin engine that defines the interface of your
      engine.
   2. From the abstract plugin engine, create a real plugin engine that
      implements the engine interface.
   3. From the abstract plugin engine, create a remote plugin engine.  Assign
      it a property Transport of type TffBaseTransport.  The remote plugin
      engine is placed on the client application and transfers the commands to
      a listener on the server.  The commands are routed from the listener to
      a plugin command handler to your real plugin engine.
   4. From the abstract TffBasePluginCommandHandler class, create a command
      handler for the plugin. }
  TffBasePluginEngine = class(TffStateComponent)
  private

    FPluginCmdHandlers : TffThreadList;
      {-The list of plugin command handlers registered with this engine. }

  protected

    procedure scSetState(const aState : TffState); override;
      {-Sets the state of the engine.  This will also set the state of any
        associated plugin command handlers. }

  public

    constructor Create(aOwner : TComponent); override;

    destructor Destroy; override;

    procedure FFAddDependent(ADependent : TffComponent); override;     {!!.11}
    procedure FFRemoveDependent(ADependent : TffComponent); override;  {!!.11}

  end;

  {This is the base class for a plugin command handler.  A plugin command
   handler receives requests through a standard command handler.  It passes
   the requests on to a plugin engine.
   As a plugin designer, you will need to create a class that inherits from
   TffBasePluginCommandHandler.  The class must recognize the messages to be
   handled by your real plugin engine.
   Note: Descendants of TffBaseCommandHandler must free the message data in
     their overridden Process methods.  However, this does not apply to
     plugin command handlers.  That is because they are typically passed a
     request from TffBaseCommandHandler.Process and
     TffBaseCommandHandler.Process handles the freeing of the message data
     on behalf of the plugin command handlers. }
  TffBasePluginCommandHandler = class(TffStateComponent)
  protected

    FCmdHandler : TffBaseCommandHandler;

    FPluginEngine : TffBasePluginEngine;
      {-The plugin engine receiving commands through this plugin. }

    procedure pchSetCmdHandler(aCmdHandler : TffBaseCommandHandler); virtual;
      {-The command handler forwarding commands to this plugin command
        handler. }

    procedure pchSetPluginEngine(anEngine : TffBasePluginEngine); virtual;
      {-The plugin engine receiving commands through this plugin.  This method
        calls TffBasePluginEngine.AddCmdHandler.  Because a plugin command
        handler is associated with a specific plugin engine class, the plugin
        designer must specify his own PluginEngine property.  The custom
        PluginEngine property should eventually call this SetPluginEngine
        method. }

  public

    constructor Create(aOwner : TComponent); override;

    destructor Destroy; override;

    procedure FFNotificationEx(const AOp : Byte; AFrom : TffComponent;  {!!.11}
                               const AData : TffWord32); override;      {!!.11}

    procedure Process(Msg : PffDataMessage; var handled : boolean); virtual; abstract;
      { This method is called by a command handler when it has a message that
        may be processed by a plugin.  If the plugin handles the message,
        set handled to True. }

  published

    property CommandHandler : TffBaseCommandHandler read FCmdHandler
                                                    write pchSetCmdHandler;
      { The command handler passing requests to this plugin command handler. }

  end;

  {The engine manager is a type of data module that contains one or more engines
   (e.g., TffBasePluginEngine or TffBaseServerEngine) and controls their
   startup and shutdown.  The manager can be controlled by the GUI of its
   parent application or remotely via startup and shutdown commands received
   through a command handler. }
  TffBaseEngineManager = class(TDataModule)
  private
    FCmdHandlers : TffThreadList;
      {-The command handlers registered with the engine manager. }

  protected

    procedure bemAddCmdHandler(aCmdHandler : TffBaseCommandHandler); virtual;
      {-When a command handler references an engine manager, it registers
        itself with the engine manager via this method. }

    function bemGetCmdHandler(aInx : Longint) : TffBaseCommandHandler; virtual;
      {-Returns a specified command handler registered with the engine
        manager. }

    function bemGetCmdHandlerCount : Longint;
      {-Returns the number of command handlers routing requests to the engine
        manager. }

    procedure bemRemoveCmdHandler(aCmdHandler : TffBaseCommandHandler); virtual;
      {-When a command handler no longer references an engine manager, it
        unregisters itself with the engine manager via this method. }

  public

    constructor Create(aOwner : TComponent); override;

    destructor Destroy; override;

    procedure Process(msg : PffDataMessage; var handled : boolean); virtual; abstract;
      { The command handler calls this method when it has a message that is
        not handled by another engine. }

    procedure Restart; virtual; abstract;
      { Use this method to stop and restart all engines and their associated
        components. }

    procedure Shutdown; virtual; abstract;
      { Use this method to stop all engines and their associated components.
        Because the associated components (i.e., the manager's command handler)
        are shutdown, the manager may not be instructed to restart.  The manager
        must be instructed to restart from the server GUI or the computer
        must be restarted. }

    procedure Startup; virtual; abstract;
      { Use this method to start all engines and their associated components. }

    procedure Stop; virtual; abstract;
      { Use this method to stop all engines but leave their associated
        components in an active state.  This allows a Startup command to be
        received from a remote client. }

  public
    property CmdHandler[aInx : Longint] : TffBaseCommandHandler
       read bemGetCmdHandler;

    property CmdHandlerCount : Longint read bemGetCmdHandlerCount;
    
  end;

  TffAddClientEvent = procedure(Listener : TffBaseTransport;
                          const userID : TffName;
                          const timeout : Longint;
                          const clientVersion : Longint;
                            var passwordHash : TffWord32;
                            var aClientID : TffClientID;
                            var errorCode : TffResult;
                            var isSecure : boolean;
                            var serverVersion : Longint) of object;
    { This is the type of event raised when a listening transport requires a
      new clientID in order to establish a new client connection.

      Inputs:
        UserID - Provided by the client application and assumed to be the
          login ID of an existing user.
        Timeout - The timeout value associated with client-level operations.
        ClientVersion - The client's version number.  The server should use
          this to determine if the client is compatible.
      Outputs:
       Passwordhash - The user's encrypted password, supplied by the event
         handler.  In situations where a secure connection is to be established,
         this hash can be used to encrypt the outgoing communications.
       aClientID - The unique identifier assigned to the client.  The client
         must supply this ID with each subsequent request sent to th server.
         If the value zero is returned for this parameter then it is assumed
         a failure occurred.
       errorCode - If an error occurred then the error code is returned in
         this parameter.
       isSecure - If True then the server requires this connection to be
         encrypted.  If False then no encryption is required.
       serverVersion - The server's version number.  Gives the client the
         opportunity to determine if any compatibility issues are present. }

  TffConnectionLostEvent = procedure(Sender : TffBaseTransport;
                                     aClientID : TffClientID) of object;
    { This is the type of event raised when a client connection is
      unexpectedly terminated by the other end of the connection.
      aClientID is the unique client identifier returned by
      EstablishConnection. }

  TffRemoveClientEvent = procedure(Listener : TffBaseTransport;
                             const aClientID : TffClientID;
                               var errorCode : TffResult) of object;
    { This is the type of event raised when a listening transport needs to
      disconnect a client.  AClientID is the unique client identifier returned
      by TffAddClientEvent when the connection was initially established.
      errorCode will be zero if the client was successfully removed or a non-zero
      value if an error occurred. }

  TffTransportMode = (fftmSend, fftmListen);
    { The valid modes for a transport.  Values:

      fftmSend - The transport sends messages.
      fftmListen - The transport listens for messages. }

  { This is the base transport class.  It includes support for sending and
    receiving requests.  A transport that receives requests is referred to as
    a listener. A transport that sends requests is to as a sender.

    To use a transport, you must do the following:

    1. Instantiate the transport.
    2. Set the ServerName property.
    3. Set the State to ffesInitializing, ffesStarting, and then ffesStarted.
       This normally occurs when a server engine starts up and sets the states
       of the command handlers connected to the server.  Each command handler
       then passes on the state to the transports connected to the command
       handler.
    4. Obtain a clientID by calling the EstablishConnection method.
    5. Submit requests to the transport using either the Post or Request
       methods.  You cannot call Post or Request unless you have a valid
       clientID.
    6. When you have finished using the transport, call
       TerminateConnection for each established connection.
    7. After terminating the connections, set the State to ffesShuttingDown
       and then ffesInactive. }
  TffBaseTransport = class(TffStateComponent)
  protected {private}
    { We need a scheme in the class to store potential properties, and
      then apply them. To do this we add BeginUpdate, and EndUpdate methods
      to the class. When BeginUpdate is called the _* fields will be set to
      match their associated fields. While updating, property set methods
      store their values in _* Fields. When EndUpdate is called the _*
      values are copied into their associated fields. BeginUpdate, and
      EndUpdate are reference counted. IOW if BeginUpdate is called twice,
      then EndUpdate must also be called twice.}

    FCmdHandler : TffBaseCommandHandler;
    _FCmdHandler : TffBaseCommandHandler;
      {-The command handler to which requests are routed. }

    FEnabled : boolean;
    _FEnabled : boolean;
      {-If True then the transport can send/receive messages.  Note that
        it will send/receive only if enabled and state = ffesStarted. }

    _FLogEnabled : Boolean;
      {-If True then event logging is enabled.  Defaults to False. }

    FLogOptions : TffTransportLogOptions;
    _FLogOptions : TffTransportLogOptions;
      {-The type of logging to be performed. }

    FMode : TffTransportMode;
    _FMode : TffTransportMode;
      {-The current mode of the transport. }

    FMsgCount : Longint;
      {-The number of messages processed by this transport. }

    FOnAddClient : TffAddClientEvent;
      {-Event handler to call when need to establish a new client. }

    FOnConnectionLost : TffConnectionLostEvent;
      {-Handler for OnConnectionLost. }

    FOnRemoveClient : TffRemoveClientEvent;
      {-Event handler to call when need to remove an existing client. }

    _FOnStateChange : TNotifyEvent;
      {-Event handler to call when the transport's state has changed. }

    FRespondToBroadcasts : boolean;
    _FRespondToBroadcasts : Boolean;
      {-If True and FListen := True then this transport will respond to
        broadcasts for active listeners. }

    FServerName : TffNetAddress;
    _FServerName : TffNetAddress;
      {-The name of the server to which this transport connects. }

    FServerNameRequired : boolean;
      {-This variable influences the btCheckServerName method.
        If set to True then a servername is required.  There may be some
        transports where a servername is not required (e.g., Single User
        Protocol in TffLegacyTransport) in which case those transports should
        set this variable to False. }

    _FState : TffState;
      {-The state of the transport. }

    FUpdateCount : Integer; { Update ReferenceCount field }
    
  protected

    { Property access methods }

    function btGetCmdHandler : TffBaseCommandHandler; virtual;
    procedure btSetCmdHandler(aCmdHandler : TffBaseCommandHandler); virtual;
      {-The command handler forwarding commands to this plugin command
        handler. }

    function btGetEnabled : boolean; virtual;
    procedure btSetEnabled(const aEnabled : boolean); virtual;
      {-Whether or not the transport is enabled. }

    function btGetLogOptions : TffTransportLogOptions; virtual;
    procedure btSetLogOptions(const anOptions : TffTransportLogOptions); virtual;
      {-The type of information to be logged. }

    function btGetMode : TffTransportMode; virtual;
    procedure btSetMode(const aMode : TffTransportMode); virtual;
      {-Whether or not the transport is to listen for requests.  For a Client
        set Mode to fftmSend.  For a Server, set Mode to fftmListen. }

    procedure btSetOnStateChange(const aHandler : TNotifyEvent); virtual;
      {-Event raised when transport's state changes. }

    function btGetRespondToBroadcasts : Boolean; virtual;
    procedure btSetRespondToBroadcasts(const aRespond : Boolean); virtual;
      {-Whether or not a transport in server mode (i.e., Listen = True) is
        to respond to broadcast messages. }

    function btGetServerName : string; virtual;                        {!!.10}
    procedure btSetServername(const aServername : string); virtual;    {!!.10}
      {-For a transport in Listen mode (i.e., Server), the server's name.  For
        a transport in Send mode (i.e., Client), the name of the server to
        which the client is to send information.  The implementation for this
        class does not perform any validation.  Transport subclasses should
        perform their own validation. }

    { Other protected methods }

    procedure btCheckListener;
      { When setting certain properties or calling certain methods, this
        method is called to ensure the transport is in listening mode.  If the
        transport is not listening then this method raises exception
        ffsce_MustBeListening. }

    procedure btCheckSender;
      { When setting certain properties or calling certain methods, this
        method is called to ensure the transport is in sending mode.  If the
        transport is not a sender then this method raises exception
        ffsce_MustBeSender. }

    procedure btCheckServerName;
      { Verifies the servername has been specified. }

    function btGetConnectionID(const anIndex : Longint) : TffClientID; virtual; abstract;
      { Used to obtain the IDs of the protocol's connections.  Handy for when
        a server wants to send messages to one or more client connections. }

    procedure btInternalReply(msgID          : Longint;
                              errorCode      : TffResult;
                              replyData      : pointer;
                              replyDataLen   : Longint); virtual;
      { This method is called from TffBaseTransport.Reply.  It must send the
        reply to the client.  The base implementation verifies the transport
        is started and is listening. }

    procedure btStoreSelfInThreadvar; virtual;
      {-This method stores Self in ffitvTransport.  This is isolated into
        its own function because an inherited class may need to Reply to
        a message (e.g., add client) before calling the inherited Process
        method where the setting of ffitvTransport is normally done. }

    procedure btBeginUpdatePrim; virtual;
    procedure btEndUpdatePrim; virtual;

    procedure lcSetLogEnabled(const aEnabled : boolean); override;

    property UpdateCount : Integer
       read FUpdateCount;
      {-This represents the current updating state. If updating is taking
        place this value will be > 0 }
        
  public

    constructor Create(aOwner : TComponent); override;

    destructor Destroy; override;

    procedure BeginUpdate;
      { redirect property set routines to _* fields }

    procedure CancelUpdate;
      { cancel the property changes. }

    procedure EndUpdate;
      { Apply the new properties. }

    procedure AutoConnectionLost(Sender : TffBaseTransport;
                                 aClientID : TffClientID);

    function ConnectionCount : Longint; virtual; abstract;
      { Returns the number of established connections.  For a sender (i.e.,
        client), this will be the number of connections to the remote server.
        For a listener (i.e., server), this will be the number of
        connections establshed by remote clients. }

    class function CurrentTransport : TffBaseTransport;
      { Returns the transport used by the current thread.  In other words,
        the transport pointed to by ffitvTransportID. }

    function EstablishConnection(const aUserName : TffName;
                                       aPasswordHash : integer;
                                       timeOut : Longint;
                                   var aClientID : TffClientID ) : TffResult; virtual; abstract;
      { Use this method to establish a connection with the server.  If the
        return code is DBIERR_NONE then aClientID will contain the clientID
        supplied by the server.  This clientID must be used in all subsequent
        requests to the server. }

    function GetName : string; virtual; abstract;
      { Retrieves the transport's name.  Must be specified for each subclass.
        Note that this is not a class function because we want the legacy
        transport to be able to return a name based upon the selected protocol.
      }

    procedure GetServerNames(aList : TStrings; const timeout : Longint); virtual; abstract;
      { Returns the list of servers available via this transport.  Timeout
        is the number of milliseconds in which all responses must be
        received. }

    function IsConnected : boolean; virtual; abstract;
      { This method returns True if the transport is connected to a server. }

    procedure FFNotificationEx(const AOp : Byte; AFrom : TffComponent; {!!.11}
                               const AData : TffWord32); override;     {!!.11}

    procedure Post(transportID : Longint;
                   clientID : TffClientID;
                   msgID : Longint;
                   requestData : pointer;
                   requestDataLen : Longint;
                   timeout : Longint;
                   replyMode : TffReplyModeType); virtual; abstract;
      { Call this method in order to submit a request to the transport.
        The request will be routed to the remote transport.  This method
        does not expect a reply and will return as soon as the request is
        handed off.  This method may be called when in Send or Listen mode.

        Parameters are as follows:

        @param transportID - For use by future protocols.
        @param clientID - The ID of the client submitting the request.  This
          must be the clientID originally supplied by the server or it may be
          zero for unsecured calls (e.g., initially asking for a connection
          to the server).
        @param msgID - The type of message being sent.
        @param requestData - Pointer to a data buffer containing the message
          data.
        requestDataLen - The length of requestData.
        timeout - The number of milliseconds in which the operation must
          complete.
        replyMode - Indicates whether or not the request should wait for the
          request to be sent to the server.
      }

    procedure Process(Msg : PffDataMessage); virtual;
      { When in listening mode, this method is called when a message is
        to be processed by the transport. }

    class procedure Reply(msgID          : Longint;
                          errorCode      : TffResult;
                          replyData      : pointer;
                          replyDataLen   : Longint); virtual;
      { When acting as a listener, this method is called to send a reply back
        to a client.  The base implementation stores a pointer to Self in
        the threadvar fftviTransportID.  This allows the command handler to
        call TffBaseTransport.Reply(...) without having to know which
        transport told it to process the command.

        Implementation:
          fftviTransport.InternalReply(...)

      }

    procedure Request(transportID : Longint;
                      clientID : TffClientID;
                      msgID : Longint;
                      timeout : Longint;
                      requestData : pointer;
                      requestDataLen : Longint;
                      replyCallback : TffReplyCallback;
                      replyCookie : Longint); virtual; abstract;
      { When the transport is in Send mode, call this method in order to
        submit a request to the transport.

        Parameters are as follows:

        @param transportID - For use by future transports.
        @param clientID - The ID of the client submitting the request.  This
          must be the clientID originally supplied by the server or it may be
          zero for unsecured calls (e.g., initially asking for a connection
          to the server).
        @param msgID - The type of message being sent.
        @param timeout - The number of milliseconds in which a reply must be
          received from the server.
        @param requestData - Pointer to a data buffer containing the message
          data.
        @param requestDataLen - The length of requestData.
        @param replyCallback - The procedure to be called when the reply
          has been received from the server.
        @param replyCookie - Whatever the calling object wants it to be.  This
          parameter is supplied to the replyCallback.
       }

    procedure ResetMsgCount; virtual;
      { Resets the MsgCount property to zero. }

    function Sleep(const timeOut : Longint) : boolean; virtual;
      { Use this function to have the client disconnect from the server but
        leave the server-side resources intact so that the client may
        reconnect at a later time.  Returns True if the Sleep was successful or
        False if the Sleep failed or is not supported.
        Note that any activity on the client side will cause the connection to
        be re-opened. }

    function Supported : boolean; virtual;
      { Returns True if the transport is supported on this workstation
        otherwise returns False. }

    procedure TerminateConnection(const aClientID : TffClientID;
                                  const timeout : Longint); virtual; abstract;
      { Use this method to terminate a connection with the server.  aClientID
        is the clientID originally returned in the call to
        EstablishConnection. }

    procedure Work; virtual; abstract;
      { Based upon the transport's mode, this method tells the transport to
        perform some work:

        1. When in sending mode, start sending requests and processing replies.
        2. When in listening mode, start listening for requests and passing
           requests off to the command handler.
      }

    property ConnectionIDs[const anIndex : Longint] : TffClientID
       read btGetConnectionID;
      { Use this to access the client IDs of a listening transport. }

  published

    property CommandHandler : TffBaseCommandHandler
       read btGetCmdHandler
       write btSetCmdHandler;
      { The command handler to which requests are routed. }

    property Enabled : boolean
       read btGetEnabled
       write btSetEnabled
       default False;
      { Use this property to control whether or not the transport can send
        or receive messages as per its Mode property.  If this property is
        set to True, the State property must still be set to ffesStarted
        before the transport will actually send or receive messages. }

    property EventLogOptions : TffTransportLogOptions
       read btGetLogOptions
       write btSetLogOptions
       default [];                                                       {!!.01}
      { The type of logging to be performed.  Applicable only when
        EventLogEnabled = True and EventLog is assigned. }

    property Mode : TffTransportMode
       read btGetMode
       write btSetMode
       default fftmSend;
      { Use this property to determine whether the transport should be used for
        sending requests or listening for requests. }

    property MsgCount : Longint
       read FMsgCount;
      { The number of messages processed by this transport. }

    property OnAddClient : TffAddClientEvent
       read FOnAddClient
       write FOnAddClient;
      { The handler for the event raised when a listening transport must
        establish a new connection. }

    property OnConnectionLost : TffConnectionLostEvent
       read FOnConnectionLost
       write FOnConnectionLost;
      { This event is raised when the other end of the connection unexpectedly
        hangs up on the transport. }

    property OnRemoveClient : TffRemoveClientEvent
       read FOnRemoveClient
       write FOnRemoveClient;
      { The handler for the event raised when a listening transport must
        disconnect an existing client. }

    property OnStateChange : TNotifyEvent
      read scOnStateChange
      write btSetOnStateChange;
      { Raised when the transport's state changes. }

    property RespondToBroadcasts : boolean
       read btGetRespondToBroadcasts
       write btSetRespondToBroadcasts
       default False;
      { Use this property to indicate wheher or not a listener should respond
        to a broadcast for active listeners. }

    property ServerName : string                                       {!!.10}
       read btGetServerName
       write btSetServerName;
      { The name and address of the server to be accessed by this transport. }

  end;

  { This class provides support for protocols requiring a thread pool. }
  TffThreadedTransport = class(TffBaseTransport)
  protected {private}

    FThreadPool : TffThreadPool;
      {-The thread pool providing threads to this transport. }

    FUnsentRequestQueue : TffThreadQueue;
      {-When in Send mode and a client submits a request, the transport creates
        a TffRequest object and places it in this queue.}

    FWaitingForReplyList : TffThreadList;
      {-When a request has been submitted to the server, the TffRequest
        object is appended to this list. }

  protected

    procedure SetThreadPool(aPool : TffThreadPool); virtual;
      {-Sets the thread pool to be used by this transport. }

    procedure tpInternalRequest(aRequest : TffRequest;
                                timeout  : Longint;
                                aCookie  : HWND); virtual;
      {-Internal method for sending a request. aRequest is the request to
        send. timeout is the number of milliseconds the transport should wait
        for a reply to the request. aCookie can be used as the transport sees
        fit. }

    procedure tpLogReq(aRequest : TffRequest;
                 const prefix : string); virtual;
      { Write a request to the event log. }

    procedure tpLogReq2(const aPrefix : string;
                        const aRequestID : Longint;
                        const aClientID : TffClientID;
                        const aMsgID : Longint;
                        const aData : pointer;
                        const aDataLen : Longint;
                        const aTimeout : Longint);
      { Write a reply to the event log.  Used by a transport in Listen mode. }

   procedure tpLogReqMisc(const aMsg : string); virtual;
      { Write a request-related string to the event log. }

    procedure tpLogReply(aRequest : TffRequest); virtual;
      { Write a reply to the event log. }

    procedure tpLogReply2(const aRequestID : Longint;
                          const aClientID : TffClientID;
                          const aMsgID : Longint;
                          const aDataLen : Longint;
                          const anError : TffResult);
      { Write a reply to the event log.  Used by a transport in Listen mode. }
  public

    constructor Create(aOwner : TComponent); override;

    destructor Destroy; override;

    procedure FFNotificationEx(const AOp : Byte; AFrom : TffComponent; {!!.11}
                               const AData : TffWord32); override;     {!!.11}
      {-Called when the thread pool we're referencing has been operated upon.
        We need to catch the case where the thread pool has been removed
        from the form. }

    procedure Post(transportID : Longint;
                   clientID : TffClientID;
                   msgID : Longint;
                   requestData : pointer;
                   requestDataLen : Longint;
                   timeout : Longint;
                   replyMode : TffReplyModeType); override;
      { This method is called when a request is to be sent but a reply is
        not needed.  This implementation does the following:

          1. Creates a TffRequest instance.
          2. Assigns the request data to the TffRequest instance.
          3. Adds the TffRequest instance to the Unsent Request Queue.
          4. Exits from this method since a reply is not needed. }

    procedure Request(transportID : Longint;
                      clientID : TffClientID;
                      msgID : Longint;
                      timeout : Longint;
                      requestData : pointer;
                      requestDataLen : Longint;
                      replyCallback : TffReplyCallback;
                      replyCookie : Longint); override;
      { This method is called when a proxy client submits a request to the
        transport.  This implementation does the following:

        1. Creates a TffRequest instance.
        2. Assigns the request data to the TffRequest instance.
        3. Adds the TffRequest instance to the Unsent Request Queue.
        4. Calls TffRequest.WaitForReply.  At this point, the calling
           thread is blocked until a reply is received or a timeout
           occurs.
        5. When TffRequest.WaitForReply returns, the reply is on the TffRequest
           object.  This method calls replyCallback, passing the message ID,
           error code, reply data, length, and cookie.
        6. The TffRequest instance is freed.  Could also be recycled to
           improve performance.  In either case, the TffRequest instance
           frees the memory occupied by the reply.
       }

  published

    property ThreadPool : TffThreadPool read FThreadPool write SetThreadPool;
      { The thread pool providing worker threads for this protocol. }

  end;

const
  ffc_Data = 'Data';
  ffc_ReqAborted = '*** Req %d aborted, Clnt %d, Err %d, Tmout %d';
  ffc_ReqLogString = '%s: %d, Clnt %d, Msg %d, Len %d, Tmout %d';
  ffc_ReplyLogString = 'Reply: %d, Clnt %d, Msg %d, Len %d, Err %d';
  ffc_SendErr = 'Snd Err %d: %s, Req %d, Clnt %d, Msg %d, Len %d, Tmout %d';

  ffcl_RequestLatencyAdjustment : Longint = 500;
    {-The number of additional milliseconds to wait for a reply. }

implementation

{Begin !!.03}
uses
  ffSrBase,                                                            {!!.13}
  SysUtils;
{End !!.03}

{$I ffconst.inc}
{$I ffllscst.inc}

{ The following thread variable is an optimization for the TffBaseTransport.
  A rule is that the thread that processes a request must be the
  thread to send a reply back to the client.  Since the reply is initiated
  outside the transport, we don't want to pass a lot of information
  about the connection.

  Our solution is to store a pointer to the transport issuing the request
  in a threadvar.  This allows a command handler to call TffBaseTransport.Reply
  without having to know the originating Transport. }
threadvar
  ffitvTransportID : Longint;  { Pointer to the transport that originally
                                 passed the request to the command handler. }


{===TffBaseCommandHandler============================================}
constructor TffBaseCommandHandler.Create(aOwner : TComponent);
begin
  inherited Create(aOwner);
  FManager := nil;
  FPlugins := TffThreadList.Create;
  FTransports := TffThreadList.Create;
end;
{--------}
destructor TffBaseCommandHandler.Destroy;
begin

  { Make sure we have a clean shutdown. }
  if scState <> ffesInactive then
    scSetState(ffesInactive);

  FFNotifyDependents(ffn_Destroy);                                     {!!.11}

  FPlugins.Free;                                                       {!!.11}
  FTransports.Free;                                                    {!!.11}

  if assigned(FManager) and (not FSkipInitial) then                    {!!.01}
    FManager.bemRemoveCmdHandler(Self);

  inherited Destroy;
end;
{--------}
procedure TffBaseCommandHandler.bchFreeMsg(msg : PffDataMessage);
begin
  if Msg^.dmDataLen > 0 then
    FFFreeMem(Msg^.dmData, Msg^.dmDataLen);
  FFFreeMem(Msg, SizeOf(TffDataMessage));
end;
{--------}
function TffBaseCommandHandler.bchGetTransportCount: Integer;
begin
  Result := FTransports.Count;
end;
{--------}
function TffBaseCommandHandler.bchGetTransport(aInx: Integer): TffBaseTransport;
begin
  Result := TffBaseTransport(TffIntListItem(FTransports[aInx]).KeyAsInt);
end;
{--------}
procedure TffBaseCommandHandler.bchSetEngineManager(aManager : TffBaseEngineManager);
      {-Used to set the manager to which messages may be routed. }
begin
  if FSkipInitial then begin                                       {BEGIN !!.01}
    FManager := aManager;
    Exit;
  end;                                                               {END !!.01}

  if assigned(FManager) then FManager.bemRemoveCmdHandler(Self);
  if assigned(aManager) then aManager.bemAddCmdHandler(Self);
end;
{Begin !!.11}
{--------}
procedure TffBaseCommandHandler.FFAddDependent(ADependent : TffComponent);
var
  aListItem : TffIntListItem;
begin
  inherited;

  if ADependent is TffBaseTransport then begin
    aListItem := TffIntListItem.Create(Longint(ADependent));
    with FTransports.BeginWrite do
      try
        Insert(aListItem);
      finally
        EndWrite;
      end;
  end
  else if ADependent is TffBasePluginCommandHandler then begin
    aListItem := TffIntListItem.Create(Longint(ADependent));
    with FPlugins.BeginWrite do
      try
        Insert(aListItem);
      finally
        EndWrite;
      end;
  end;
end;
{--------}
procedure TffBaseCommandHandler.FFRemoveDependent(ADependent : TffComponent);
begin
  inherited;
  if ADependent is TffBaseTransport then
    with FTransports.BeginWrite do
      try
        Delete(Longint(ADependent));
      finally
        EndWrite;
      end
  else if ADependent is TffBasePluginCommandHandler then
    with FPlugins.BeginWrite do
      try
        Delete(Longint(ADependent));
      finally
        EndWrite;
      end;
end;
{End !!.11}
{--------}
procedure TffBaseCommandHandler.Process(Msg : PffDataMessage);
var
  aPlugin : TffBasePluginCommandHandler;
  Handled : boolean;
  anIndex : Longint;
begin

  Handled := False;
  { See if a plugin recognizes the message. }
  if assigned(FPlugins) then
    with FPlugins.BeginRead do
      try
        for anIndex := 0 to pred(Count) do begin
          aPlugin := TffBasePluginCommandHandler(TffIntListItem(Items[anIndex]).KeyAsInt);
          aPlugin.Process(Msg, Handled);
          if Handled then break;
        end;
      finally
        EndRead;
      end;

  { If no plugin recognizes the message and we have an engine manager
    then see if the engine manager will handle the message. }
  if not Handled and assigned(FManager) then
    FManager.Process(Msg, Handled);

  { If the message has not been handled by this point, tell the client this
    is an unrecognized message.  Note that we are calling a TffBaseTransport
    class function which gets the reply to the correct transport. }
{Begin !!.13}
  if not Handled then begin
    lcLog(Format(ffStrResServer[ffErrUnknownMsg], [Msg.dmMsg]));
    TffBaseTransport.Reply(Msg.dmMsg, ffErrUnknownMsg, nil, 0);
  end;
{End !!.13}

end;
{--------}
procedure TffBaseCommandHandler.scSetState(const aState : TffState);
var
  aTransport : TffBaseTransport;
  anIndex : Longint;
  NextState : TffState;
  OldState : TffState;
begin

  if (aState = scState) or                                              {!!.01}
     (aState in [ffesStopping, ffesStopped]) then exit;                {!!.01}

  OldState := scState;
  aTransport := nil;

  try
    if assigned(FTransports) then
      with FTransports.BeginRead do
        try
          while scState <> aState do begin
            { Based upon our current state & the target state, get the next state. }
            NextState := ffStateDiagram[scState, aState];

            { Move all transports to the specified state. }
            try
              for anIndex := pred(Count) downto 0 do begin
                aTransport := TffBaseTransport(TffIntListItem(Items[anIndex]).KeyAsInt);
                if aTransport.Enabled then
                  aTransport.scSetState(NextState);
              end;
            except
              on E:Exception do begin
                { If a transport raises an exception, disable the transport.
                  The server must be restarted before we try this transport
                  again. }
                lcLog(format('Transport state failure: %s',
                             [aTransport.GetName, E.message]));
                try
                  aTransport.State := ffesFailed;
                  aTransport.Enabled := False;
                except
                  { Eat any exception raised by changing the state. }
                end;
              end;
            end;

            scState := NextState;
            { Call the appropriate internal method for this state. }
            case NextState of
              ffesInactive :
                scShutdown;
              ffesInitializing :
                scInitialize;
              ffesStarting :
                scStartup;
              ffesShuttingDown :
                scPrepareForShutdown;
            end;  { case }
            if assigned(scOnStateChange) then
              scOnStateChange(Self);
          end;  { while }
        finally
          EndRead;
        end;
      except
        scState := OldState;
        raise;
      end;

end;
{====================================================================}

{===TffBasePluginCommandHandler======================================}
constructor TffBasePluginCommandHandler.Create(aOwner : TComponent);
begin
  inherited Create(aOwner);
  FCmdHandler := nil;
  FPluginEngine := nil;
end;
{--------}
destructor TffBasePluginCommandHandler.Destroy;
begin
  if assigned(FCmdHandler) then
    FCmdHandler.FFRemoveDependent(Self);                               {!!.11}

  if assigned(FPluginEngine) then
    FPluginEngine.FFRemoveDependent(Self);                             {!!.11}

  inherited Destroy;
end;
{Begin !!.11}
{--------}
procedure TffBasePluginCommandHandler.FFNotificationEx
            (const AOp : Byte; AFrom : TffComponent;
             const AData : TffWord32);
begin
  inherited;
  if AOp in [ffn_Destroy, ffn_Remove] then begin
    if AFrom = FCmdHandler then begin
      FCmdHandler.FFRemoveDependent(Self);
      FCmdHandler := nil;
    end
    else if AFrom = FPluginEngine then begin
      FPluginEngine.FFRemoveDependent(Self);
      FPluginEngine := nil;
    end;
  end;
end;
{--------}
procedure TffBasePluginCommandHandler.pchSetCmdHandler(aCmdHandler : TffBaseCommandHandler);
      {-The command handler forwarding commands to this plugin command
        handler. }
begin
  if aCmdHandler <> FCmdHandler then begin
    if assigned(FCmdHandler) then
      FCmdHandler.FFRemoveDependent(Self);

    if assigned(aCmdHandler) then
      aCmdHandler.FFAddDependent(Self);

    FCmdHandler := aCmdHandler;
  end;

  {Note: It is entirely possible for the plugin command handler to be active
    and have its associated command handler set to nil.  In such a case, the
    plugin command handler never receives PrepareForShutdown and Shutdown
    commands. }
end;
{--------}
procedure TffBasePluginCommandHandler.pchSetPluginEngine(anEngine : TffBasePluginEngine);
begin
  if anEngine <> FPluginEngine then begin
    if assigned(FPluginEngine) then
      FPluginEngine.FFRemoveDependent(Self);

    if assigned(anEngine) then
      anEngine.FFAddDependent(Self);

    FPluginEngine := anEngine;
  end;
end;
{End !!.11}
{====================================================================}

{===TffBasePluginEngine==============================================}
constructor TffBasePluginEngine.Create(aOwner : TComponent);
begin
  inherited Create(aOwner);
  FPluginCmdHandlers := TffThreadList.Create;
end;
{--------}
destructor TffBasePluginEngine.Destroy;
{Begin !!.11}
begin
  scSetState(ffesInactive);
  FFNotifyDependents(ffn_Destroy);
  FPluginCmdHandlers.Free;
  inherited Destroy;
end;
{--------}
procedure TffBasePluginEngine.FFAddDependent(ADependent : TffComponent);
var
  aListItem : TffIntListItem;
begin
  inherited;

  if ADependent is TffBasePluginCommandHandler then begin
    aListItem := TffIntListItem.Create(Longint(ADependent));
    with FPluginCmdHandlers.BeginWrite do
      try
        Insert(aListItem);
      finally
        EndWrite;
      end;
  end;
end;
{--------}
procedure TffBasePluginEngine.FFRemoveDependent(ADependent : TffComponent);
begin
  inherited;
  if ADependent is TffBasePluginCommandHandler then
    with FPluginCmdHandlers.BeginWrite do
      try
        Delete(Longint(ADependent));
      finally
        EndWrite;
      end;
end;
{End !!.11}
{--------}
procedure TffBasePluginEngine.scSetState(const aState : TffState);
      {-Sets the state of the engine.  This will also set the state of any
        associated plugin command handlers. }
var
  aCmdHandler : TffBasePluginCommandHandler;
  anIndex : Longint;
  NextState : TffState;
  OldState : TffState;
begin
  { If we are at the specified state then exit without doing anything. }
  if aState = scState then exit;

  OldState := scState;

  try
    if assigned(FPluginCmdHandlers) then
      with FPluginCmdHandlers.BeginRead do
        try
          while scState <> aState do begin
            { Based upon our current state & the target state, get the next state. }
            NextState := ffStateDiagram[scState, aState];

            { Move all command handlers to that state. }
            for anIndex := 0 to pred(FPluginCmdHandlers.Count) do begin
              aCmdHandler := TffBasePluginCommandHandler(TffIntListItem(Items[anIndex]).KeyAsInt);
              if not (aState in [ffesStopping, ffesStopped,
                                 ffesUnsupported, ffesFailed]) then
                aCmdHandler.scSetState(aState);
            end;

            { Call the appropriate method for the new state. }
            case NextState of
              ffesInactive, ffesStopped :
                scShutdown;
              ffesInitializing :
                scInitialize;
              ffesStarting :
                scStartup;
              ffesStopping, ffesShuttingDown :
                scPrepareForShutdown;
            end;  { case }

            { Update our state. }
            scState := NextState;
            if assigned(scOnStateChange) then
              scOnStateChange(Self);
          end;
        finally
          EndRead;
        end;
  except
    { Some kind of failure occurred.  We need to rollback the engine to its
      original state.  We will leave the command handlers as is. }
    scState := OldState;
    raise;
  end;
end;
{====================================================================}

{===TffBaseEngineManager=============================================}
constructor TffBaseEngineManager.Create(aOwner : TComponent);
begin
  FCmdHandlers := TffThreadList.Create;
  inherited Create(aOwner);
end;
{--------}
destructor TffBaseEngineManager.Destroy;
var
  aCmdHandler : TffBaseCommandHandler;
  anIndex : Longint;
begin

  { Note: The real engine manager must do a graceful shutdown of the server
    engine. }
  if assigned(FCmdHandlers) then
    with FCmdHandlers.BeginWrite do
      try
        { Make sure none of the plugin command handlers reference this engine. }
        for anIndex := pred(Count) downto 0 do begin
          aCmdHandler := TffBaseCommandHandler(TffIntListItem(Items[anIndex]).KeyAsInt);
          aCmdHandler.bchSetEngineManager(nil);
        end;
      finally
        EndWrite;
        FCmdHandlers.Free;
      end;

  inherited Destroy;
end;
{--------}
procedure TffBaseEngineManager.bemAddCmdHandler(aCmdHandler : TffBaseCommandHandler);
var
  aListItem : TffIntListItem;
begin
  aListItem := TffIntListItem.Create(Longint(aCmdHandler));
  with FCmdHandlers.BeginWrite do
    try
      Insert(aListItem);
      aCmdHandler.FManager := Self;
    finally
      EndWrite;
    end;
end;
{--------}
function TffBaseEngineManager.bemGetCmdHandler(aInx : Longint) : TffBaseCommandHandler;
begin
  with FCmdHandlers.BeginRead do
    try
      Result := TffBaseCommandHandler(TffIntListItem(Items[aInx]).KeyAsInt);
    finally
      EndRead;
    end;
end;
{--------}
function TffBaseEngineManager.bemGetCmdHandlerCount : Longint;
begin
  Result := FCmdHandlers.Count;
end;
{--------}
procedure TffBaseEngineManager.bemRemoveCmdHandler(aCmdHandler : TffBaseCommandHandler);
begin
  aCmdHandler.FManager := nil;
  with FCmdHandlers.BeginWrite do
    try
      Delete(Longint(aCmdHandler));
    finally
      EndWrite;
    end;
end;
{====================================================================}


{===TffBaseTransport=================================================}

procedure TffBaseTransport.AutoConnectionLost(Sender : TffBaseTransport;
                                              aClientID : TffClientID);
begin
  Sender.FFNotifyDependentsEx(ffn_ConnectionLost, aClientID);
end;
{--------}
constructor TffBaseTransport.Create(aOwner : TComponent);
begin
  inherited Create(aOwner);
  FCmdHandler := nil;
  FEnabled := False;
  FMode := fftmSend;
  FRespondToBroadcasts := False;
  FServerName := '';
  FServerNameRequired := True;
  scState := ffesInactive;

  OnConnectionLost := AutoConnectionLost;
end;
{--------}
destructor TffBaseTransport.Destroy;
begin
  FFNotifyDependents(ffn_Destroy);
  if assigned(FCmdHandler) then
    FCmdHandler.FFRemoveDependent(Self);                               {!!.11}
  inherited Destroy;
end;
{--------}
procedure TffBaseTransport.BeginUpdate;
begin
  if FUpdateCount = 0 then begin
    { Give the descendent classes a chance to set their stored properties }
    btBeginUpdatePrim;

    { Set the _* fields to match their counterparts }
    _FCmdHandler          := FCmdHandler;
    _FEnabled             := FEnabled;
    _FLogEnabled          := FLogEnabled;
    _FLogOptions          := FLogOptions;
    _FMode                := FMode;
    _FOnStateChange       := scOnStateChange;
    _FRespondToBroadcasts := FRespondToBroadcasts;
    _FServerName          := FServerName;
    _FState               := scState;
  end;
  Inc(FUpdateCount);
end;
{--------}
procedure TffBaseTransport.btBeginUpdatePrim;
begin
  { do nothing }
end;
{--------}
procedure TffBaseTransport.CancelUpdate;
begin
  FUpdateCount := 0;
end;
{--------}
procedure TffBaseTransport.EndUpdate;
begin
  if FUpdateCount <> 0 then begin
    Dec(FUpdateCount);
    if FUpdateCount = 0 then begin

      { Let the descendent classes do their work }
      btEndUpdatePrim;

    { Update the fields with the new values in their _* counterparts }
    { We do not set the private field directly, since some processing may
      need to be done by a properties write method. }
      CommandHandler      := _FCmdHandler;
      { Make sure State is set prior to Enabled property and other
        state-dependent properties. }
      State               := _FState;
      Enabled             := _FEnabled;
      EventLogEnabled     := _FLogEnabled;
      EventLogOptions     := _FLogOptions;
      Mode                := _FMode;
      OnStateChange       := _FOnStateChange;
      RespondToBroadcasts := _FRespondToBroadcasts;
      ServerName          := _FServerName;

    end;
  end;
end;
{--------}
procedure TffBaseTransport.btEndUpdatePrim;
begin
  { do nothing }
end;
{--------}
function TffBaseTransport.btGetCmdHandler : TffBaseCommandHandler;
begin
  Result := FCmdHandler;
end;
{--------}
function TffBaseTransport.btGetEnabled : boolean;
begin
  Result := FEnabled;
end;
{--------}
function TffBaseTransport.btGetLogOptions : TffTransportLogOptions;
begin
  Result := FLogOptions;
end;
{--------}
function TffBaseTransport.btGetMode : TffTransportMode;
begin
  Result := FMode;
end;
{--------}
function TffBaseTransport.btGetRespondToBroadcasts : Boolean;
begin
  Result := FRespondToBroadcasts;
end;
{--------}
function TffBaseTransport.btGetServerName : string;                    {!!.10}
begin
  Result := FServerName;
end;
{--------}
procedure TffBaseTransport.btSetCmdHandler(aCmdHandler : TffBaseCommandHandler);
begin
  if (FUpdateCount > 0) then
    _FCmdHandler := aCmdHandler
  else begin
    {Check to make sure the new property is different.}
    if FCmdHandler = aCmdHandler then Exit;

    if assigned(FCmdHandler) then
      FCmdHandler.FFRemoveDependent(Self);                             {!!.11}

    if assigned(aCmdHandler) then
      aCmdHandler.FFAddDependent(Self);                                {!!.11}

    FCmdHandler := aCmdHandler;                                        {!!.11}
  end;
end;
{--------}
procedure TffBaseTransport.btSetEnabled(const aEnabled : Boolean);
begin
  if (FUpdateCount > 0) then
    _FEnabled := aEnabled
  else begin
    {Check to make sure the new property is different.}
    if FEnabled = aEnabled then Exit;
    { If the transport is being disabled but the State indicates some
      amount of activity then make sure the transport is inactive. }
    if (not aEnabled) and (scState <> ffesInactive) then begin
      FFNotifyDependents(ffn_Deactivate);
      scSetState(ffesInactive);
    end;  
    FEnabled := aEnabled;
  end;
end;
{--------}
procedure TffBaseTransport.btSetLogOptions(const anOptions : TffTransportLogOptions);
begin
  if (UpdateCount > 0) then
    _FLogOptions := anOptions
  else
    FLogOptions := anOptions;
end;
{--------}
procedure TffBaseTransport.btSetMode(const aMode : TffTransportMode);
begin
  if (FUpdateCount > 0) then
    _FMode := aMode
  else begin
    {Check to make sure the new property is different.}
    if FMode = aMode then Exit;
    scCheckInactive;
    FMode := aMode;
  end;
end;
{--------}
procedure TffBaseTransport.btSetOnStateChange(const aHandler : TNotifyEvent);
begin
  if (FUpdateCount > 0) then
    _FOnStateChange := aHandler
  else
    scOnStateChange := aHandler;
end;
{--------}
procedure TffBaseTransport.btSetRespondToBroadcasts(const aRespond : Boolean);
begin
  if (FUpdateCount > 0) then
    _FRespondToBroadcasts := aRespond
  else
    FRespondToBroadcasts := aRespond;
end;
{--------}
procedure TffBaseTransport.btSetServername(const aServername : string); {!!.10}
begin
  if (FUpdateCount > 0) then
    _FServerName := aServerName
  else begin
    {Check to make sure the new property is different.}
    if FServerName = aServername then Exit;
    scCheckInactive;
    FServerName := aServerName;
  end;
end;
{--------}
procedure TffBaseTransport.btCheckListener;
begin
  if FMode = fftmSend then
    RaiseSCErrorCode(ffsce_MustBeListener);
end;
{--------}
procedure TffBaseTransport.btCheckSender;
begin
  if FMode = fftmListen then
    RaiseSCErrorCode(ffsce_MustBeSender);
end;
{--------}
procedure TffBaseTransport.btCheckServerName;
begin
  if FServerNameRequired and (FServerName = '') then
    RaiseSCErrorCode(ffsce_MustHaveServerName);
end;
{--------}
procedure TffBaseTransport.btInternalReply(msgID          : Longint;
                                           errorCode      : TffResult;
                                           replyData      : pointer;
                                           replyDataLen   : Longint);
begin
  scCheckStarted;
end;
{--------}
procedure TffBaseTransport.lcSetLogEnabled(const aEnabled : Boolean);
begin
  if (UpdateCount > 0) then
    _FLogEnabled := aEnabled
  else
    FLogEnabled := aEnabled;
end;
{--------}
procedure TffBaseTransport.Process(Msg : PffDataMessage);
begin

  btStoreSelfInThreadvar;

  { If we have a command handler, tell the command handler to process the
    message. }
  if assigned(FCmdHandler) then begin
    { Increment the message count.  Note: This happens whether or not the
      message was handled by a command handler, plugin command handler, or
      server engine. }
    InterlockedIncrement(FMsgCount);
    FCmdHandler.Process(Msg);
  end;
end;
{--------}
class function TffBaseTransport.CurrentTransport : TffBaseTransport;
begin
  Result := TffBaseTransport(ffitvTransportID);
end;
{--------}
{Rewritten !!.11}
procedure TffBaseTransport.FFNotificationEx(const AOp : Byte; AFrom : TffComponent;
                                            const AData : TffWord32);
begin
  inherited;
  if AOp in [ffn_Destroy, ffn_Remove] then
    if (AFrom = FCmdHandler) then begin
      FCmdHandler.FFRemoveDependent(Self);
      FCmdHandler := nil
    end
    else if (AFrom = FEventLog) then begin
      FEventLog.FFRemoveDependent(Self);
      FEventLog := nil;
    end;
end;
{--------}
class procedure TffBaseTransport.Reply(msgID          : Longint;
                                       errorCode      : TffResult;
                                       replyData      : pointer;
                                       replyDataLen   : Longint);
begin
  CurrentTransport.btInternalReply(msgID, errorCode,
                                   replyData, replyDataLen);
end;
{--------}
procedure TffBaseTransport.ResetMsgCount;
begin
  FMsgCount := 0;
end;
{--------}
function TffBaseTransport.Sleep(const timeOut : Longint) : boolean;
begin
  Result := False;
end;
{--------}
function TffBaseTransport.Supported : boolean;
begin
  Result := True;
end;
{--------}
procedure TffBaseTransport.btStoreSelfInThreadvar;
begin
  { Store a pointer to this instance so the command handler may quickly
    find us and submit a reply. }
  ffitvTransportID := Longint(Self);
end;
{====================================================================}

{===TffThreadedTransport=============================================}
constructor TffThreadedTransport.Create(aOwner : TComponent);
begin
  inherited Create(aOwner);
  FThreadPool := nil;
  FUnsentRequestQueue := TffThreadQueue.Create;
  FWaitingForReplyList := TffThreadList.Create;
end;
{--------}
destructor TffThreadedTransport.Destroy;
var
  anIndex : Longint;
  aRequest : TffRequest;
begin
  FFNotifyDependents(ffn_Destroy);

  if assigned(FThreadPool) then
    FThreadPool.FFRemoveDependent(Self);                               {!!.11}

  if assigned(FUnsentRequestQueue) then
    with FUnsentRequestQueue.BeginWrite do
      try
        for anIndex := pred(Count) downto 0 do begin
          aRequest := TffRequest(TffIntListItem(Items[anIndex]).KeyAsInt);
          aRequest.Free;
        end;
      finally
        EndWrite;
        Free;
      end;

  if assigned(FWaitingForReplyList) then
    with FWaitingForReplyList.BeginWrite do
      try
        for anIndex := pred(Count) downto 0 do begin
          aRequest := TffRequest(TffIntListItem(Items[anIndex]).KeyAsInt);
          aRequest.Free;
        end;
      finally
        EndWrite;
        Free;
      end;

  inherited Destroy;
end;
{--------}
{Rewritten !!.11}
procedure TffThreadedTransport.FFNotificationEx(const AOp : Byte; AFrom : TffComponent;
                                                const AData : TffWord32);
begin
  inherited;
  if (AFrom = FThreadPool) and
     (AOp in[ffn_Destroy, ffn_Remove]) then begin
    FThreadPool.FFRemoveDependent(Self);
    FThreadPool := nil;
  end;
end;
{--------}
procedure TffThreadedTransport.SetThreadPool(aPool : TffThreadPool);
begin
  if aPool <> FThreadPool then begin
    if assigned(FThreadPool) then
      FThreadPool.FFRemoveDependent(Self);                             {!!.11}

    if Assigned(aPool) then begin
      FThreadPool := aPool;
      FThreadPool.FFAddDependent(Self);                                {!!.11}
    end;
  end;
end;
{--------}
procedure TffThreadedTransport.Post(transportID : Longint;
                                    clientID : TffClientID;
                                    msgID : Longint;
                                    requestData : pointer;
                                    requestDataLen : Longint;
                                    timeout : Longint;
                                    replyMode : TffReplyModeType);
var
  aRequest : TffRequest;
  anItem : TffIntListItem;
begin
  scCheckStarted;
  aRequest := TffRequest.Create(clientID, msgID, requestData,
                                requestDataLen, timeout, replyMode);
  anItem := TffIntListItem.Create(Longint(aRequest));
  with FUnsentRequestQueue.BeginWrite do
    try
      Enqueue(anItem);
    finally
      EndWrite;
    end;
  if replyMode = ffrmNoReplyWaitUntilSent then begin
    aRequest.WaitForReply(timeout);
    if not aRequest.Aborted then
      aRequest.Free
    else
      with aRequest do
        tpLogReqMisc(format(ffc_ReqAborted,[Longint(aRequest), ClientID,
                                            ErrorCode, Timeout]));
  end;
end;
{--------}
procedure TffThreadedTransport.Request(transportID : Longint;
                                       clientID : TffClientID;
                                       msgID : Longint;
                                       timeout : Longint;
                                       requestData : pointer;
                                       requestDataLen : Longint;
                                       replyCallback : TffReplyCallback;
                                       replyCookie : Longint);
var
  aRequest : TffRequest;

begin
  scCheckStarted;
  aRequest := TffRequest.Create(clientID, msgID, requestData, requestDataLen,
                                timeout, ffrmReplyExpected);
  tpInternalRequest(aRequest, timeout, 0);
  if assigned(replyCallback) then
    replyCallback(aRequest.ReplyMsgID, aRequest.ErrorCode,
                  aRequest.ReplyData, aRequest.ReplyDataLen,
                  replyCookie);
  if not aRequest.Aborted then
    aRequest.Free
  else
    with aRequest do
      tpLogReqMisc(format(ffc_ReqAborted,[Longint(aRequest), ClientID,
                                          ErrorCode, Timeout]));
end;
{--------}
procedure TffThreadedTransport.tpInternalRequest(aRequest : TffRequest;
                                                 timeout  : Longint;
                                                 aCookie  : HWND);
var
  anItem : TffIntListItem;
begin
  anItem := TffIntListItem.Create(Longint(aRequest));
  with FUnsentRequestQueue.BeginWrite do
    try
      Enqueue(anItem);
    finally
      EndWrite;
    end;

  { Wait for the reply.  If a timeout occurs, assume the request object
    will be freed by the transport thread at some point.  Timeout exceptions
    are raised to the calling object. }
  if timeout = 0 then
    aRequest.WaitForReply(timeout)
  else
    aRequest.WaitForReply(timeout + ffcl_RequestLatencyAdjustment);

end;
{--------}
procedure TffThreadedTransport.tpLogReq(aRequest : TffRequest;
                                  const prefix : string);
begin
  if FLogEnabled and (fftpLogRequests in FLogOptions) and
     assigned(FEventLog) and assigned(aRequest) then
    with aRequest do begin
      FEventLog.WriteStringFmt(ffc_ReqLogString,
                               [prefix, Longint(aRequest), ClientID, MsgID,
                                RequestDataLen, Timeout]);
      FEventLog.WriteBlock('Data', aRequest.RequestData,
                           aRequest.RequestDataLen);
    end;
end;
{--------}
procedure TffThreadedTransport.tpLogReq2(const aPrefix : string;
                                         const aRequestID : Longint;
                                         const aClientID : TffClientID;
                                         const aMsgID : Longint;
                                         const aData : pointer;
                                         const aDataLen : Longint;
                                         const aTimeout : Longint);
begin
  FEventLog.WriteStringFmt(ffc_ReqLogString,
                           [aPrefix, aRequestID, aClientID, aMsgID,
                            aDataLen, aTimeout]);
  FEventLog.WriteBlock(ffc_Data, aData, aDataLen);
end;
{--------}
procedure TffThreadedTransport.tpLogReqMisc(const aMsg : string);
begin
  if FLogEnabled and (fftpLogRequests in FLogOptions) and
     assigned(FEventLog) then
    FEventLog.WriteString(aMsg);
end;
{--------}
procedure TffThreadedTransport.tpLogReply(aRequest : TffRequest);
begin
  if FLogEnabled and (fftpLogReplies in FLogOptions) and
     assigned(FEventLog) and assigned(aRequest) then
    with aRequest do begin
      FEventLog.WriteStringFmt(ffc_ReplyLogString,
                               [Longint(aRequest), ClientID, ReplyMsgID,
                                ReplyDataLen, ErrorCode]);
      FEventLog.WriteBlock(ffc_Data, ReplyData, ReplyDataLen);
    end;
end;
{--------}
procedure TffThreadedTransport.tpLogReply2(const aRequestID : Longint;
                                           const aClientID : TffClientID;
                                           const aMsgID : Longint;
                                           const aDataLen : Longint;
                                           const anError : TffResult);
begin
  { Assumption: Calling routine will only call if it is legitimate to log
                the data.  We do it this way so that we avoid passing tons
                of data on the stack. }
  FEventLog.WriteStringFmt(ffc_ReplyLogString,
                           [aRequestID, aClientID, aMsgID, aDataLen, anError]);
end;

{====================================================================}
end.


