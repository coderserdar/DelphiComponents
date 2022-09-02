{$I fsdefine.inc}

Unit fsllcomm;

Interface

Uses
  Classes,
  Forms,
  windows,
  fsllbase,
  fssrbase,
  fsllcomp,
  fslleng,
  fslllog,
  fsllreq,
  fsllthrd,
  fsnetmsg;

Type

  { TfsDataMessage contains the message information passed from a transport
    to a server command handler, plugin command handler, or engine manager. }
  PfsDataMessage = ^TfsDataMessage;
  TfsDataMessage = Record
    dmMsg: Longint; { the unique ID identifying the msg type }
    dmClientID: TffClientID; { the client sending the message }
    dmRequestID: Longint; { the unique ID of the request }
    dmTime: TffWord32; { the time the message was received }
    dmRetryUntil: TffWord32;
    dmErrorCode: TffResult;
    dmData: pointer;
    dmDataLen: TffMemSize;
  End;

  { The following options may be used to control logging in the transports.
    Values:
      fftpLogErrors - Write errors to the event log.
      fftpLogRequests - Write requests to the event log.  If in Send mode
        then logs all sent requests.  If in Listen mode then logs all received
        requests.
      fftpLogReplies - If in Send mode then logs all received replies.  If in
        Listen mode then logs all sent replies. }
  TfsTransportLogOption = (fstpLogErrors,
    fstpLogRequests, fstpLogReplies);
  TfsTransportLogOptions = Set Of TfsTransportLogOption;

  { A transport will send a request to the server.  When the reply is
    received, the transport must notify the object submitting the request.
    To be notified, the object submitting the request must define a procedure
    of type TfsReplyCallback.  Parameters passed to this procedure are as
    follows:
      @param msgID The message identifier returned by the server.
      @param errorCode The error code returned by the server.
      @param replyData The data returned by the server.
      @param replyDataLen The length of the data returned by the server.
      @param replyType The format of the data: byteArray (e.g., packed record)
        or stream.
      @param replyCookie The replyCookie parameter originally supplied to the
        TFSBaseTransport.Request method.  The meaning of this parameter is
        specific to the object submitting the request.  For the
        TffRemoteServerEngine, this is a pointer to TffProxyClient.
  }
  TfsReplyCallback = Procedure(msgID: Longint;
    errorCode: TffResult;
    replyData: pointer;
    replyDataLen: Longint;
    replyCookie: Longint);

  TFSBasePluginCommandHandler = Class; { forward declaration }
  TFSBaseEngineManager = Class; { forward declaration }
  TFSBaseTransport = Class; { forward declaration }

  { This is the base class for the command handler.  A command handler
    receives requests from a transport and routes them to a destination.
    The base class supports routing of commands to plugins that have
    themselves with the command handler. }
  TFSBaseCommandHandler = Class(TfsStateComponent)
  Protected {private}

    FManager: TFSBaseEngineManager;
    {-The engine manager that may receive shutdown and startup requests
      through this command handler.  Note that the command handler doesn't
      really know about shutdown and startup requests.  The engine manager
      is like a special plugin.  If a plugin does not handle the message,
      it is routed to the engine manager.  The engine manager may or may
      not handle the message. }

    FPlugins: TFSSpecThreadList;
    {-The list of plugins that reference the command handler. }

    FSkipInitial: Boolean; {!!.01}
    {-Internal state that reflects whether the Engine Manager Wizard has
      created this component as a proxy (true) or not}

    FTransports: TFSSpecThreadList;
    {-The list of transports that reference the command handler. }

  Protected

    Procedure bchFreeMsg(msg: PfsDataMessage); Virtual;
    { When a transport passes off a request to the command handler, it
      becomes the command handler's responsibility to free the message
      data associated with the request.  This method frees the TfsDataMessage
      structure as well as the message content contained by TfsDataMessage.
      Command handlers should call this method, or find some other way of
      freeing the memory, once a request has been processed. }

    Function bchGetTransport(aInx: Integer): TFSBaseTransport; Virtual;
    { Retrieves a transport from the command handler's list.}

    Function bchGetTransportCount: Longint; Virtual;
    { Retrieves the number of transports owned by this command
      handler.}

    Procedure bchSetEngineManager(aManager: TFSBaseEngineManager); Virtual;
    {-Used to set the manager to which messages may be routed. }

    Procedure scSetState(Const aState: TfsState); Override;
    { This method is called when the command handler's state changes.
      This implementation sets the state of the associated transports. }

    Property SkipInitial: Boolean {BEGIN !!.01}
    Read FSkipInitial
      Write FSkipInitial;
    { This property is used by the engine manager wizard. It's purpose is
      to keep the bchSetEngineManger routine from generating an access
            violation when the expert creates a new engine manager }{END !!.01}
  Public

    Constructor Create(AOwner: TComponent); Override;

    Destructor Destroy; Override;

    Procedure FFAddDependent(ADependent: TFSSpecComp); Override; {!!.11}
    Procedure FFRemoveDependent(ADependent: TFSSpecComp); Override; {!!.11}

    Procedure Process(Msg: PfsDataMessage); Virtual;
    { This method is called by the transport in order to process a message.
      The default implementation forwards the message to the registered
      plugin(s).  If a plugin does not handle the message and an engine
      manager has been specified, the message is forwarded to the
      engine manager.  If the message is not handled, a reply is sent to
      the client stating the message is unrecognized. }

    Property TransportCount: Longint Read bchGetTransportCount;
    { The number of transports passing requests to this command handler.}

    Property Transports[aInx: Longint]: TFSBaseTransport
    Read bchGetTransport;
    { Use this property to access the transports connected to the command
      handler. }
  Published

    Property EngineManager: TFSBaseEngineManager
      Read FManager Write bchSetEngineManager;

  End;

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
      it a property Transport of type TFSBaseTransport.  The remote plugin
      engine is placed on the client application and transfers the commands to
      a listener on the server.  The commands are routed from the listener to
      a plugin command handler to your real plugin engine.
   4. From the abstract TFSBasePluginCommandHandler class, create a command
      handler for the plugin. }
  TFSBasePluginEngine = Class(TfsStateComponent)
  Private

    FPluginCmdHandlers: TFSSpecThreadList;
    {-The list of plugin command handlers registered with this engine. }

  Protected

    Procedure scSetState(Const aState: TfsState); Override;
    {-Sets the state of the engine.  This will also set the state of any
      associated plugin command handlers. }

  Public

    Constructor Create(aOwner: TComponent); Override;

    Destructor Destroy; Override;

    Procedure FFAddDependent(ADependent: TFSSpecComp); Override; {!!.11}
    Procedure FFRemoveDependent(ADependent: TFSSpecComp); Override; {!!.11}

  End;

  {This is the base class for a plugin command handler.  A plugin command
   handler receives requests through a standard command handler.  It passes
   the requests on to a plugin engine.
   As a plugin designer, you will need to create a class that inherits from
   TFSBasePluginCommandHandler.  The class must recognize the messages to be
   handled by your real plugin engine.
   Note: Descendants of TFSBaseCommandHandler must free the message data in
     their overridden Process methods.  However, this does not apply to
     plugin command handlers.  That is because they are typically passed a
     request from TFSBaseCommandHandler.Process and
     TFSBaseCommandHandler.Process handles the freeing of the message data
     on behalf of the plugin command handlers. }
  TFSBasePluginCommandHandler = Class(TfsStateComponent)
  Protected

    FCmdHandler: TFSBaseCommandHandler;

    FPluginEngine: TFSBasePluginEngine;
    {-The plugin engine receiving commands through this plugin. }

    Procedure pchSetCmdHandler(aCmdHandler: TFSBaseCommandHandler); Virtual;
    {-The command handler forwarding commands to this plugin command
      handler. }

    Procedure pchSetPluginEngine(anEngine: TFSBasePluginEngine); Virtual;
    {-The plugin engine receiving commands through this plugin.  This method
      calls TFSBasePluginEngine.AddCmdHandler.  Because a plugin command
      handler is associated with a specific plugin engine class, the plugin
      designer must specify his own PluginEngine property.  The custom
      PluginEngine property should eventually call this SetPluginEngine
      method. }

  Public

    Constructor Create(aOwner: TComponent); Override;

    Destructor Destroy; Override;

    Procedure FFNotificationEx(Const AOp: Byte; AFrom: TFSSpecComp; {!!.11}
      Const AData: TffWord32); Override; {!!.11}

    Procedure Process(Msg: PfsDataMessage; Var handled: boolean); Virtual; Abstract;
    { This method is called by a command handler when it has a message that
      may be processed by a plugin.  If the plugin handles the message,
      set handled to True. }

  Published

    Property CommandHandler: TFSBaseCommandHandler Read FCmdHandler
      Write pchSetCmdHandler;
    { The command handler passing requests to this plugin command handler. }

  End;

  {The engine manager is a type of data module that contains one or more engines
   (e.g., TFSBasePluginEngine or TffBaseServerEngine) and controls their
   startup and shutdown.  The manager can be controlled by the GUI of its
   parent application or remotely via startup and shutdown commands received
   through a command handler. }
  TFSBaseEngineManager = Class(TDataModule)
  Private
    FCmdHandlers: TFSSpecThreadList;
    {-The command handlers registered with the engine manager. }

  Protected

    Procedure bemAddCmdHandler(aCmdHandler: TFSBaseCommandHandler); Virtual;
    {-When a command handler references an engine manager, it registers
      itself with the engine manager via this method. }

    Function bemGetCmdHandler(aInx: Longint): TFSBaseCommandHandler; Virtual;
    {-Returns a specified command handler registered with the engine
      manager. }

    Function bemGetCmdHandlerCount: Longint;
    {-Returns the number of command handlers routing requests to the engine
      manager. }

    Procedure bemRemoveCmdHandler(aCmdHandler: TFSBaseCommandHandler); Virtual;
    {-When a command handler no longer references an engine manager, it
      unregisters itself with the engine manager via this method. }

  Public

    Constructor Create(aOwner: TComponent); Override;

    Destructor Destroy; Override;

    Procedure Process(msg: PfsDataMessage; Var handled: boolean); Virtual; Abstract;
    { The command handler calls this method when it has a message that is
      not handled by another engine. }

    Procedure Restart; Virtual; Abstract;
    { Use this method to stop and restart all engines and their associated
      components. }

    Procedure Shutdown; Virtual; Abstract;
    { Use this method to stop all engines and their associated components.
      Because the associated components (i.e., the manager's command handler)
      are shutdown, the manager may not be instructed to restart.  The manager
      must be instructed to restart from the server GUI or the computer
      must be restarted. }

    Procedure Startup; Virtual; Abstract;
    { Use this method to start all engines and their associated components. }

    Procedure Stop; Virtual; Abstract;
    { Use this method to stop all engines but leave their associated
      components in an active state.  This allows a Startup command to be
      received from a remote client. }

  Public
    Property CmdHandler[aInx: Longint]: TFSBaseCommandHandler
    Read bemGetCmdHandler;

    Property CmdHandlerCount: Longint Read bemGetCmdHandlerCount;

  End;

  TfsAddClientEvent = Procedure(Listener: TFSBaseTransport;
    Const userID: TffName;
    Const timeout: Longint;
    Const clientVersion: Longint;
    Var passwordHash: TffWord32;
    Var aClientID: TffClientID;
    Var errorCode: TffResult;
    Var isSecure: boolean;
    Var serverVersion: Longint;
    Var aRights: TffUserRights) Of Object;
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

  TfsConnectionLostEvent = Procedure(Sender: TFSBaseTransport;
    aClientID: TffClientID) Of Object;
  { This is the type of event raised when a client connection is
    unexpectedly terminated by the other end of the connection.
    aClientID is the unique client identifier returned by
    EstablishConnection. }

  TfsRemoveClientEvent = Procedure(Listener: TFSBaseTransport;
    Const aClientID: TffClientID;
    Var errorCode: TffResult) Of Object;
  { This is the type of event raised when a listening transport needs to
    disconnect a client.  AClientID is the unique client identifier returned
    by TfsAddClientEvent when the connection was initially established.
    errorCode will be zero if the client was successfully removed or a non-zero
    value if an error occurred. }

  TfsTransportMode = (fstmSend, fstmListen);
  { The valid modes for a transport.  Values:

    fstmSend - The transport sends messages.
    fstmListen - The transport listens for messages. }

{ This is the base transport class.  It includes support for sending and
  receiving requests.  A transport that receives requests is referred to as
  a listener. A transport that sends requests is to as a sender.

  To use a transport, you must do the following:

  1. Instantiate the transport.
  2. Set the ServerName property.
  3. Set the State to fsesInitializing, fsesStarting, and then fsesStarted.
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
  7. After terminating the connections, set the State to fsesShuttingDown
     and then fsesInactive. }
  TFSBaseTransport = Class(TfsStateComponent)
  Protected {private}
    { We need a scheme in the class to store potential properties, and
      then apply them. To do this we add BeginUpdate, and EndUpdate methods
      to the class. When BeginUpdate is called the _* fields will be set to
      match their associated fields. While updating, property set methods
      store their values in _* Fields. When EndUpdate is called the _*
      values are copied into their associated fields. BeginUpdate, and
      EndUpdate are reference counted. IOW if BeginUpdate is called twice,
      then EndUpdate must also be called twice.}

    FCmdHandler: TFSBaseCommandHandler;
    _FCmdHandler: TFSBaseCommandHandler;
    {-The command handler to which requests are routed. }

    FEnabled: boolean;
    _FEnabled: boolean;
    {-If True then the transport can send/receive messages.  Note that
      it will send/receive only if enabled and state = fsesStarted. }

    _FLogEnabled: Boolean;
    {-If True then event logging is enabled.  Defaults to False. }

    FLogOptions: TfsTransportLogOptions;
    _FLogOptions: TfsTransportLogOptions;
    {-The type of logging to be performed. }

    FMode: TfsTransportMode;
    _FMode: TfsTransportMode;
    {-The current mode of the transport. }

    FMsgCount: Longint;
    {-The number of messages processed by this transport. }

    FOnAddClient: TfsAddClientEvent;
    {-Event handler to call when need to establish a new client. }

    FOnConnectionLost: TfsConnectionLostEvent;
    {-Handler for OnConnectionLost. }

    FOnRemoveClient: TfsRemoveClientEvent;
    {-Event handler to call when need to remove an existing client. }

    _FOnStateChange: TNotifyEvent;
    {-Event handler to call when the transport's state has changed. }

    FRespondToBroadcasts: boolean;
    _FRespondToBroadcasts: Boolean;
    {-If True and FListen := True then this transport will respond to
      broadcasts for active listeners. }

    FServerName: TffNetAddress;
    _FServerName: TffNetAddress;
    {-The name of the server to which this transport connects. }

    FServerNameRequired: boolean;
    {-This variable influences the btCheckServerName method.
      If set to True then a servername is required.  There may be some
      transports where a servername is not required (e.g., Single User
      Protocol in TffLegacyTransport) in which case those transports should
      set this variable to False. }

    _FState: TfsState;
    {-The state of the transport. }

    FUpdateCount: Integer; { Update ReferenceCount field }

  Protected

    { Property access methods }

    Function btGetCmdHandler: TFSBaseCommandHandler; Virtual;
    Procedure btSetCmdHandler(aCmdHandler: TFSBaseCommandHandler); Virtual;
    {-The command handler forwarding commands to this plugin command
      handler. }

    Function btGetEnabled: boolean; Virtual;
    Procedure btSetEnabled(Const aEnabled: boolean); Virtual;
    {-Whether or not the transport is enabled. }

    Function btGetLogOptions: TfsTransportLogOptions; Virtual;
    Procedure btSetLogOptions(Const anOptions: TfsTransportLogOptions); Virtual;
    {-The type of information to be logged. }

    Function btGetMode: TfsTransportMode; Virtual;
    Procedure btSetMode(Const aMode: TfsTransportMode); Virtual;
    {-Whether or not the transport is to listen for requests.  For a Client
      set Mode to fstmSend.  For a Server, set Mode to fstmListen. }

    Procedure btSetOnStateChange(Const aHandler: TNotifyEvent); Virtual;
    {-Event raised when transport's state changes. }

    Function btGetRespondToBroadcasts: Boolean; Virtual;
    Procedure btSetRespondToBroadcasts(Const aRespond: Boolean); Virtual;
    {-Whether or not a transport in server mode (i.e., Listen = True) is
      to respond to broadcast messages. }

    Function btGetServerName: String; Virtual; {!!.10}
    Procedure btSetServername(Const aServername: String); Virtual; {!!.10}
    {-For a transport in Listen mode (i.e., Server), the server's name.  For
      a transport in Send mode (i.e., Client), the name of the server to
      which the client is to send information.  The implementation for this
      class does not perform any validation.  Transport subclasses should
      perform their own validation. }

  { Other protected methods }

    Procedure btCheckListener;
    { When setting certain properties or calling certain methods, this
      method is called to ensure the transport is in listening mode.  If the
      transport is not listening then this method raises exception
      fssce_MustBeListening. }

    Procedure btCheckSender;
    { When setting certain properties or calling certain methods, this
      method is called to ensure the transport is in sending mode.  If the
      transport is not a sender then this method raises exception
      fssce_MustBeSender. }

    Procedure btCheckServerName;
    { Verifies the servername has been specified. }

    Function btGetConnectionID(Const anIndex: Longint): TffClientID; Virtual; Abstract;
    { Used to obtain the IDs of the protocol's connections.  Handy for when
      a server wants to send messages to one or more client connections. }

    Procedure btInternalReply(msgID: Longint;
      errorCode: TffResult;
      replyData: pointer;
      replyDataLen: Longint); Virtual;
    { This method is called from TFSBaseTransport.Reply.  It must send the
      reply to the client.  The base implementation verifies the transport
      is started and is listening. }

    Procedure btStoreSelfInThreadvar; Virtual;
    {-This method stores Self in ffitvTransport.  This is isolated into
      its own function because an inherited class may need to Reply to
      a message (e.g., add client) before calling the inherited Process
      method where the setting of ffitvTransport is normally done. }

    Procedure btBeginUpdatePrim; Virtual;
    Procedure btEndUpdatePrim; Virtual;

    Procedure lcSetLogEnabled(Const aEnabled: boolean); Override;

    Property UpdateCount: Integer
      Read FUpdateCount;
    {-This represents the current updating state. If updating is taking
      place this value will be > 0 }

  Public

    Constructor Create(aOwner: TComponent); Override;

    Destructor Destroy; Override;

    Procedure BeginUpdate;
    { redirect property set routines to _* fields }

    Procedure CancelUpdate;
    { cancel the property changes. }

    Procedure EndUpdate;
    { Apply the new properties. }

    Procedure AutoConnectionLost(Sender: TFSBaseTransport;
      aClientID: TffClientID);

    Function ConnectionCount: Longint; Virtual; Abstract;
    { Returns the number of established connections.  For a sender (i.e.,
      client), this will be the number of connections to the remote server.
      For a listener (i.e., server), this will be the number of
      connections establshed by remote clients. }

    Class Function CurrentTransport: TFSBaseTransport;
    { Returns the transport used by the current thread.  In other words,
      the transport pointed to by fsitvTransportID. }

    Function EstablishConnection(Const aUserName: TffName;
      aPasswordHash: Integer;
      timeOut: Longint;
      Var aClientID: TffClientID;
      Var aRights: TffUserRights;
      Var aSecurityEnabled: boolean): TffResult; Virtual; Abstract;
    { Use this method to establish a connection with the server.  If the
      return code is DBIERR_NONE then aClientID will contain the clientID
      supplied by the server.  This clientID must be used in all subsequent
      requests to the server. }

    Function GetName: String; Virtual; Abstract;
    { Retrieves the transport's name.  Must be specified for each subclass.
      Note that this is not a class function because we want the legacy
      transport to be able to return a name based upon the selected protocol.
    }

    Procedure GetServerNames(aList: TStrings; Const timeout: Longint); Virtual; Abstract;
    { Returns the list of servers available via this transport.  Timeout
      is the number of milliseconds in which all responses must be
      received. }

    Function IsConnected: boolean; Virtual; Abstract;
    { This method returns True if the transport is connected to a server. }

    Procedure FFNotificationEx(Const AOp: Byte; AFrom: TFSSpecComp; {!!.11}
      Const AData: TffWord32); Override; {!!.11}

    Procedure Post(transportID: Longint;
      clientID: TffClientID;
      msgID: Longint;
      requestData: pointer;
      requestDataLen: Longint;
      timeout: Longint;
      replyMode: TfsReplyModeType); Virtual; Abstract;
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

    Procedure Process(Msg: PfsDataMessage); Virtual;
    { When in listening mode, this method is called when a message is
      to be processed by the transport. }

    Class Procedure Reply(msgID: Longint;
      errorCode: TffResult;
      replyData: pointer;
      replyDataLen: Longint); Virtual;
    { When acting as a listener, this method is called to send a reply back
      to a client.  The base implementation stores a pointer to Self in
      the threadvar fftviTransportID.  This allows the command handler to
      call TFSBaseTransport.Reply(...) without having to know which
      transport told it to process the command.

      Implementation:
        fftviTransport.InternalReply(...)

    }

    Procedure Request(transportID: Longint;
      clientID: TffClientID;
      msgID: Longint;
      timeout: Longint;
      requestData: pointer;
      requestDataLen: Longint;
      replyCallback: TfsReplyCallback;
      replyCookie: Longint); Virtual; Abstract;
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

    Procedure ResetMsgCount; Virtual;
    { Resets the MsgCount property to zero. }

    Function Sleep(Const timeOut: Longint): boolean; Virtual;
    { Use this function to have the client disconnect from the server but
      leave the server-side resources intact so that the client may
      reconnect at a later time.  Returns True if the Sleep was successful or
      False if the Sleep failed or is not supported.
      Note that any activity on the client side will cause the connection to
      be re-opened. }

    Function Supported: boolean; Virtual;
    { Returns True if the transport is supported on this workstation
      otherwise returns False. }

    Procedure TerminateConnection(Const aClientID: TffClientID;
      Const timeout: Longint); Virtual; Abstract;
    { Use this method to terminate a connection with the server.  aClientID
      is the clientID originally returned in the call to
      EstablishConnection. }

    Procedure Work; Virtual; Abstract;
    { Based upon the transport's mode, this method tells the transport to
      perform some work:

      1. When in sending mode, start sending requests and processing replies.
      2. When in listening mode, start listening for requests and passing
         requests off to the command handler.
    }

    Property ConnectionIDs[Const anIndex: Longint]: TffClientID
    Read btGetConnectionID;
    { Use this to access the client IDs of a listening transport. }

  Published

    Property CommandHandler: TFSBaseCommandHandler
      Read btGetCmdHandler
      Write btSetCmdHandler;
    { The command handler to which requests are routed. }

    Property Enabled: boolean
      Read btGetEnabled
      Write btSetEnabled
      Default False;
    { Use this property to control whether or not the transport can send
      or receive messages as per its Mode property.  If this property is
      set to True, the State property must still be set to fsesStarted
      before the transport will actually send or receive messages. }

    Property EventLogOptions: TfsTransportLogOptions
      Read btGetLogOptions
      Write btSetLogOptions
      Default []; {!!.01}
    { The type of logging to be performed.  Applicable only when
      EventLogEnabled = True and EventLog is assigned. }

    Property Mode: TfsTransportMode
      Read btGetMode
      Write btSetMode
      Default fstmSend;
    { Use this property to determine whether the transport should be used for
      sending requests or listening for requests. }

    Property MsgCount: Longint
      Read FMsgCount;
    { The number of messages processed by this transport. }

    Property OnAddClient: TfsAddClientEvent
      Read FOnAddClient
      Write FOnAddClient;
    { The handler for the event raised when a listening transport must
      establish a new connection. }

    Property OnConnectionLost: TfsConnectionLostEvent
      Read FOnConnectionLost
      Write FOnConnectionLost;
    { This event is raised when the other end of the connection unexpectedly
      hangs up on the transport. }

    Property OnRemoveClient: TfsRemoveClientEvent
      Read FOnRemoveClient
      Write FOnRemoveClient;
    { The handler for the event raised when a listening transport must
      disconnect an existing client. }

    Property OnStateChange: TNotifyEvent
      Read scOnStateChange
      Write btSetOnStateChange;
    { Raised when the transport's state changes. }

    Property RespondToBroadcasts: boolean
      Read btGetRespondToBroadcasts
      Write btSetRespondToBroadcasts
      Default False;
    { Use this property to indicate wheher or not a listener should respond
      to a broadcast for active listeners. }

    Property ServerName: String {!!.10}
    Read btGetServerName
      Write btSetServerName;
    { The name and address of the server to be accessed by this transport. }

  End;

  { This class provides support for protocols requiring a thread pool. }
  TfsThreadedTransport = Class(TFSBaseTransport)
  Protected {private}

    FThreadPool: TfsThreadPool;
    {-The thread pool providing threads to this transport. }

    FUnsentRequestQueue: TfsThreadQueue;
    {-When in Send mode and a client submits a request, the transport creates
      a TfsRequest object and places it in this queue.}

    FWaitingForReplyList: TFSSpecThreadList;
    {-When a request has been submitted to the server, the TfsRequest
      object is appended to this list. }

  Protected

    Procedure SetThreadPool(aPool: TfsThreadPool); Virtual;
    {-Sets the thread pool to be used by this transport. }

    Procedure tpInternalRequest(aRequest: TfsRequest;
      timeout: Longint;
      aCookie: HWND); Virtual;
    {-Internal method for sending a request. aRequest is the request to
      send. timeout is the number of milliseconds the transport should wait
      for a reply to the request. aCookie can be used as the transport sees
      fit. }

    Procedure tpLogReq(aRequest: TfsRequest;
      Const prefix: String); Virtual;
    { Write a request to the event log. }

    Procedure tpLogReq2(Const aPrefix: String;
      Const aRequestID: Longint;
      Const aClientID: TffClientID;
      Const aMsgID: Longint;
      Const aData: pointer;
      Const aDataLen: Longint;
      Const aTimeout: Longint);
    { Write a reply to the event log.  Used by a transport in Listen mode. }

    Procedure tpLogReqMisc(Const aMsg: String); Virtual;
    { Write a request-related string to the event log. }

    Procedure tpLogReply(aRequest: TfsRequest); Virtual;
    { Write a reply to the event log. }

    Procedure tpLogReply2(Const aRequestID: Longint;
      Const aClientID: TffClientID;
      Const aMsgID: Longint;
      Const aDataLen: Longint;
      Const anError: TffResult);
    { Write a reply to the event log.  Used by a transport in Listen mode. }
  Public

    Constructor Create(aOwner: TComponent); Override;

    Destructor Destroy; Override;

    Procedure FFNotificationEx(Const AOp: Byte; AFrom: TFSSpecComp; {!!.11}
      Const AData: TffWord32); Override; {!!.11}
    {-Called when the thread pool we're referencing has been operated upon.
      We need to catch the case where the thread pool has been removed
      from the form. }

    Procedure Post(transportID: Longint;
      clientID: TffClientID;
      msgID: Longint;
      requestData: pointer;
      requestDataLen: Longint;
      timeout: Longint;
      replyMode: TfsReplyModeType); Override;
    { This method is called when a request is to be sent but a reply is
      not needed.  This implementation does the following:

        1. Creates a TfsRequest instance.
        2. Assigns the request data to the TfsRequest instance.
        3. Adds the TfsRequest instance to the Unsent Request Queue.
        4. Exits from this method since a reply is not needed. }

    Procedure Request(transportID: Longint;
      clientID: TffClientID;
      msgID: Longint;
      timeout: Longint;
      requestData: pointer;
      requestDataLen: Longint;
      replyCallback: TfsReplyCallback;
      replyCookie: Longint); Override;
    { This method is called when a proxy client submits a request to the
      transport.  This implementation does the following:

      1. Creates a TfsRequest instance.
      2. Assigns the request data to the TfsRequest instance.
      3. Adds the TfsRequest instance to the Unsent Request Queue.
      4. Calls TfsRequest.WaitForReply.  At this point, the calling
         thread is blocked until a reply is received or a timeout
         occurs.
      5. When TfsRequest.WaitForReply returns, the reply is on the TfsRequest
         object.  This method calls replyCallback, passing the message ID,
         error code, reply data, length, and cookie.
      6. The TfsRequest instance is freed.  Could also be recycled to
         improve performance.  In either case, the TfsRequest instance
         frees the memory occupied by the reply.
     }

  Published

    Property ThreadPool: TfsThreadPool Read FThreadPool Write SetThreadPool;
    { The thread pool providing worker threads for this protocol. }

  End;

Const
  fsc_Data = 'Data';
  fsc_ReqAborted = '*** Req %d aborted, Clnt %d, Err %d, Tmout %d';
  fsc_ReqLogString = '%s: %d, Clnt %d, Msg %d, Len %d, Tmout %d';
  fsc_ReplyLogString = 'Reply: %d, Clnt %d, Msg %d, Len %d, Err %d';
  fsc_SendErr = 'Snd Err %d: %s, Req %d, Clnt %d, Msg %d, Len %d, Tmout %d';

  ffcl_RequestLatencyAdjustment: Longint = 500;
  {-The number of additional milliseconds to wait for a reply. }

Implementation

Uses
  SysUtils;

{$I fsconst.inc}
{$I fsllscst.inc}

{ The following thread variable is an optimization for the TFSBaseTransport.
  A rule is that the thread that processes a request must be the
  thread to send a reply back to the client.  Since the reply is initiated
  outside the transport, we don't want to pass a lot of information
  about the connection.

  Our solution is to store a pointer to the transport issuing the request
  in a threadvar.  This allows a command handler to call TFSBaseTransport.Reply
  without having to know the originating Transport. }
Threadvar
  fsitvTransportID: Longint; { Pointer to the transport that originally
  passed the request to the command handler. }

{===TFSBaseCommandHandler============================================}

Constructor TFSBaseCommandHandler.Create(aOwner: TComponent);
Begin
  Inherited Create(aOwner);
  FManager := Nil;
  FPlugins := TFSSpecThreadList.Create;
  FTransports := TFSSpecThreadList.Create;
End;
{--------}

Destructor TFSBaseCommandHandler.Destroy;
Begin

  { Make sure we have a clean shutdown. }
  If scState <> fsesInactive Then
    scSetState(fsesInactive);

  FFNotifyDependents(ffn_Destroy); {!!.11}

  FPlugins.Free; {!!.11}
  FTransports.Free; {!!.11}

  If assigned(FManager) And (Not FSkipInitial) Then {!!.01}
    FManager.bemRemoveCmdHandler(Self);

  Inherited Destroy;
End;
{--------}

Procedure TFSBaseCommandHandler.bchFreeMsg(msg: PfsDataMessage);
Begin
  If Msg^.dmDataLen > 0 Then
    FFFreeMem(Msg^.dmData, Msg^.dmDataLen);
  FFFreeMem(Msg, SizeOf(TfsDataMessage));
End;
{--------}

Function TFSBaseCommandHandler.bchGetTransportCount: Integer;
Begin
  Result := FTransports.Count;
End;
{--------}

Function TFSBaseCommandHandler.bchGetTransport(aInx: Integer): TFSBaseTransport;
Begin
  Result := TFSBaseTransport(TfsIntListItem(FTransports[aInx]).KeyAsInt);
End;
{--------}

Procedure TFSBaseCommandHandler.bchSetEngineManager(aManager: TFSBaseEngineManager);
{-Used to set the manager to which messages may be routed. }
Begin
  If FSkipInitial Then
    Begin {BEGIN !!.01}
      FManager := aManager;
      Exit;
    End; {END !!.01}

  If assigned(FManager) Then FManager.bemRemoveCmdHandler(Self);
  If assigned(aManager) Then aManager.bemAddCmdHandler(Self);
End;
{Begin !!.11}
{--------}

Procedure TFSBaseCommandHandler.FFAddDependent(ADependent: TFSSpecComp);
Var
  aListItem: TfsIntListItem;
Begin
  Inherited;

  If ADependent Is TFSBaseTransport Then
    Begin
      aListItem := TfsIntListItem.Create(Longint(ADependent));
      With FTransports.BeginWrite Do
        Try
          Insert(aListItem);
        Finally
          EndWrite;
        End;
    End
  Else If ADependent Is TFSBasePluginCommandHandler Then
    Begin
      aListItem := TfsIntListItem.Create(Longint(ADependent));
      With FPlugins.BeginWrite Do
        Try
          Insert(aListItem);
        Finally
          EndWrite;
        End;
    End;
End;
{--------}

Procedure TFSBaseCommandHandler.FFRemoveDependent(ADependent: TFSSpecComp);
Begin
  Inherited;
  If ADependent Is TFSBaseTransport Then
    With FTransports.BeginWrite Do
      Try
        Delete(Longint(ADependent));
      Finally
        EndWrite;
      End
  Else If ADependent Is TFSBasePluginCommandHandler Then
    With FPlugins.BeginWrite Do
      Try
        Delete(Longint(ADependent));
      Finally
        EndWrite;
      End;
End;
{End !!.11}
{--------}

Procedure TFSBaseCommandHandler.Process(Msg: PfsDataMessage);
Var
  aPlugin: TFSBasePluginCommandHandler;
  Handled: boolean;
  anIndex: Longint;
Begin

  Handled := False;
  { See if a plugin recognizes the message. }
  If assigned(FPlugins) Then
    With FPlugins.BeginRead Do
      Try
        For anIndex := 0 To pred(Count) Do
          Begin
            aPlugin := TFSBasePluginCommandHandler(TfsIntListItem(Items[anIndex]).KeyAsInt);
            aPlugin.Process(Msg, Handled);
            If Handled Then break;
          End;
      Finally
        EndRead;
      End;

  { If no plugin recognizes the message and we have an engine manager
    then see if the engine manager will handle the message. }
  If Not Handled And assigned(FManager) Then
    FManager.Process(Msg, Handled);

  { If the message has not been handled by this point, tell the client this
    is an unrecognized message.  Note that we are calling a TFSBaseTransport
    class function which gets the reply to the correct transport. }
{Begin !!.13}
  If Not Handled Then
    Begin
      lcLog(Format(fsStrResServer[fserrUnknownMsg], [Msg.dmMsg]));
      TFSBaseTransport.Reply(Msg.dmMsg, fserrUnknownMsg, Nil, 0);
    End;
  {End !!.13}

End;
{--------}

Procedure TFSBaseCommandHandler.scSetState(Const aState: TfsState);
Var
  aTransport: TFSBaseTransport;
  anIndex: Longint;
  NextState: TfsState;
  OldState: TfsState;
Begin

  If (aState = scState) Or {!!.01}
  (aState In [fsesStopping, fsesStopped]) Then Exit; {!!.01}

  OldState := scState;
  aTransport := Nil;

  Try
    If assigned(FTransports) Then
      With FTransports.BeginRead Do
        Try
          While scState <> aState Do
            Begin
              { Based upon our current state & the target state, get the next state. }
              NextState := fsStateDiagram[scState, aState];

              { Move all transports to the specified state. }
              Try
                For anIndex := pred(Count) Downto 0 Do
                  Begin
                    aTransport := TFSBaseTransport(TfsIntListItem(Items[anIndex]).KeyAsInt);
                    If aTransport.Enabled Then
                      aTransport.scSetState(NextState);
                  End;
              Except
                On E: Exception Do
                  Begin
                    { If a transport raises an exception, disable the transport.
                      The server must be restarted before we try this transport
                      again. }
                    lcLog(format('Transport state failure: %s',
                      [aTransport.GetName, E.message]));
                    Try
                      aTransport.State := fsesFailed;
                      aTransport.Enabled := False;
                    Except
                      { Eat any exception raised by changing the state. }
                    End;
                  End;
              End;

              scState := NextState;
              { Call the appropriate internal method for this state. }
              Case NextState Of
                fsesInactive:
                  scShutdown;
                fsesInitializing:
                  scInitialize;
                fsesStarting:
                  scStartup;
                fsesShuttingDown:
                  scPrepareForShutdown;
              End; { case }
              If assigned(scOnStateChange) Then
                scOnStateChange(Self);
            End; { while }
        Finally
          EndRead;
        End;
  Except
    scState := OldState;
    Raise;
  End;

End;
{====================================================================}

{===TFSBasePluginCommandHandler======================================}

Constructor TFSBasePluginCommandHandler.Create(aOwner: TComponent);
Begin
  Inherited Create(aOwner);
  FCmdHandler := Nil;
  FPluginEngine := Nil;
End;
{--------}

Destructor TFSBasePluginCommandHandler.Destroy;
Begin
  If assigned(FCmdHandler) Then
    FCmdHandler.FFRemoveDependent(Self); {!!.11}

  If assigned(FPluginEngine) Then
    FPluginEngine.FFRemoveDependent(Self); {!!.11}

  Inherited Destroy;
End;
{Begin !!.11}
{--------}

Procedure TFSBasePluginCommandHandler.FFNotificationEx
  (Const AOp: Byte; AFrom: TFSSpecComp;
  Const AData: TffWord32);
Begin
  Inherited;
  If AOp In [ffn_Destroy, ffn_Remove] Then
    Begin
      If AFrom = FCmdHandler Then
        Begin
          FCmdHandler.FFRemoveDependent(Self);
          FCmdHandler := Nil;
        End
      Else If AFrom = FPluginEngine Then
        Begin
          FPluginEngine.FFRemoveDependent(Self);
          FPluginEngine := Nil;
        End;
    End;
End;
{--------}

Procedure TFSBasePluginCommandHandler.pchSetCmdHandler(aCmdHandler: TFSBaseCommandHandler);
{-The command handler forwarding commands to this plugin command
  handler. }
Begin
  If aCmdHandler <> FCmdHandler Then
    Begin
      If assigned(FCmdHandler) Then
        FCmdHandler.FFRemoveDependent(Self);

      If assigned(aCmdHandler) Then
        aCmdHandler.FFAddDependent(Self);

      FCmdHandler := aCmdHandler;
    End;

  {Note: It is entirely possible for the plugin command handler to be active
    and have its associated command handler set to nil.  In such a case, the
    plugin command handler never receives PrepareForShutdown and Shutdown
    commands. }
End;
{--------}

Procedure TFSBasePluginCommandHandler.pchSetPluginEngine(anEngine: TFSBasePluginEngine);
Begin
  If anEngine <> FPluginEngine Then
    Begin
      If assigned(FPluginEngine) Then
        FPluginEngine.FFRemoveDependent(Self);

      If assigned(anEngine) Then
        anEngine.FFAddDependent(Self);

      FPluginEngine := anEngine;
    End;
End;
{End !!.11}
{====================================================================}

{===TFSBasePluginEngine==============================================}

Constructor TFSBasePluginEngine.Create(aOwner: TComponent);
Begin
  Inherited Create(aOwner);
  FPluginCmdHandlers := TFSSpecThreadList.Create;
End;
{--------}

Destructor TFSBasePluginEngine.Destroy;
{Begin !!.11}
Begin
  scSetState(fsesInactive);
  FFNotifyDependents(ffn_Destroy);
  FPluginCmdHandlers.Free;
  Inherited Destroy;
End;
{--------}

Procedure TFSBasePluginEngine.FFAddDependent(ADependent: TFSSpecComp);
Var
  aListItem: TfsIntListItem;
Begin
  Inherited;

  If ADependent Is TFSBasePluginCommandHandler Then
    Begin
      aListItem := TfsIntListItem.Create(Longint(ADependent));
      With FPluginCmdHandlers.BeginWrite Do
        Try
          Insert(aListItem);
        Finally
          EndWrite;
        End;
    End;
End;
{--------}

Procedure TFSBasePluginEngine.FFRemoveDependent(ADependent: TFSSpecComp);
Begin
  Inherited;
  If ADependent Is TFSBasePluginCommandHandler Then
    With FPluginCmdHandlers.BeginWrite Do
      Try
        Delete(Longint(ADependent));
      Finally
        EndWrite;
      End;
End;
{End !!.11}
{--------}

Procedure TFSBasePluginEngine.scSetState(Const aState: TfsState);
{-Sets the state of the engine.  This will also set the state of any
  associated plugin command handlers. }
Var
  aCmdHandler: TFSBasePluginCommandHandler;
  anIndex: Longint;
  NextState: TfsState;
  OldState: TfsState;
Begin
  { If we are at the specified state then exit without doing anything. }
  If aState = scState Then Exit;

  OldState := scState;

  Try
    If assigned(FPluginCmdHandlers) Then
      With FPluginCmdHandlers.BeginRead Do
        Try
          While scState <> aState Do
            Begin
              { Based upon our current state & the target state, get the next state. }
              NextState := fsStateDiagram[scState, aState];

              { Move all command handlers to that state. }
              For anIndex := 0 To pred(FPluginCmdHandlers.Count) Do
                Begin
                  aCmdHandler := TFSBasePluginCommandHandler(TfsIntListItem(Items[anIndex]).KeyAsInt);
                  If Not (aState In [fsesStopping, fsesStopped,
                    fsesUnsupported, fsesFailed]) Then
                    aCmdHandler.scSetState(aState);
                End;

              { Call the appropriate method for the new state. }
              Case NextState Of
                fsesInactive, fsesStopped:
                  scShutdown;
                fsesInitializing:
                  scInitialize;
                fsesStarting:
                  scStartup;
                fsesStopping, fsesShuttingDown:
                  scPrepareForShutdown;
              End; { case }

              { Update our state. }
              scState := NextState;
              If assigned(scOnStateChange) Then
                scOnStateChange(Self);
            End;
        Finally
          EndRead;
        End;
  Except
    { Some kind of failure occurred.  We need to rollback the engine to its
      original state.  We will leave the command handlers as is. }
    scState := OldState;
    Raise;
  End;
End;
{====================================================================}

{===TFSBaseEngineManager=============================================}

Constructor TFSBaseEngineManager.Create(aOwner: TComponent);
Begin
  FCmdHandlers := TFSSpecThreadList.Create;
  Inherited Create(aOwner);
End;
{--------}

Destructor TFSBaseEngineManager.Destroy;
Var
  aCmdHandler: TFSBaseCommandHandler;
  anIndex: Longint;
Begin
  If assigned(FCmdHandlers) Then
    With FCmdHandlers.BeginWrite Do
      Try
        For anIndex := pred(Count) Downto 0 Do
          Begin
            aCmdHandler := TFSBaseCommandHandler(TfsIntListItem(Items[anIndex]).KeyAsInt);
            aCmdHandler.bchSetEngineManager(Nil);
          End;
      Finally
        EndWrite;
        FCmdHandlers.Free;
      End;
End;
{--------}

Procedure TFSBaseEngineManager.bemAddCmdHandler(aCmdHandler: TFSBaseCommandHandler);
Var
  aListItem: TfsIntListItem;
Begin
  aListItem := TfsIntListItem.Create(Longint(aCmdHandler));
  With FCmdHandlers.BeginWrite Do
    Try
      Insert(aListItem);
      aCmdHandler.FManager := Self;
    Finally
      EndWrite;
    End;
End;
{--------}

Function TFSBaseEngineManager.bemGetCmdHandler(aInx: Longint): TFSBaseCommandHandler;
Begin
  With FCmdHandlers.BeginRead Do
    Try
      Result := TFSBaseCommandHandler(TfsIntListItem(Items[aInx]).KeyAsInt);
    Finally
      EndRead;
    End;
End;
{--------}

Function TFSBaseEngineManager.bemGetCmdHandlerCount: Longint;
Begin
  Result := FCmdHandlers.Count;
End;
{--------}

Procedure TFSBaseEngineManager.bemRemoveCmdHandler(aCmdHandler: TFSBaseCommandHandler);
Begin
  aCmdHandler.FManager := Nil;
  With FCmdHandlers.BeginWrite Do
    Try
      Delete(Longint(aCmdHandler));
    Finally
      EndWrite;
    End;
End;
{====================================================================}

{===TFSBaseTransport=================================================}

Procedure TFSBaseTransport.AutoConnectionLost(Sender: TFSBaseTransport;
  aClientID: TffClientID);
Begin
  Sender.FFNotifyDependentsEx(ffn_ConnectionLost, aClientID);
End;
{--------}

Constructor TFSBaseTransport.Create(aOwner: TComponent);
Begin
  Inherited Create(aOwner);
  FCmdHandler := Nil;
  FEnabled := False;
  FMode := fstmSend;
  FRespondToBroadcasts := False;
  FServerName := '';
  FServerNameRequired := True;
  scState := fsesInactive;

  OnConnectionLost := AutoConnectionLost;
End;
{--------}

Destructor TFSBaseTransport.Destroy;
Begin
  FFNotifyDependents(ffn_Destroy);
  If assigned(FCmdHandler) Then
    FCmdHandler.FFRemoveDependent(Self); {!!.11}
  Inherited Destroy;
End;
{--------}

Procedure TFSBaseTransport.BeginUpdate;
Begin
  If FUpdateCount = 0 Then
    Begin
      { Give the descendent classes a chance to set their stored properties }
      btBeginUpdatePrim;

      { Set the _* fields to match their counterparts }
      _FCmdHandler := FCmdHandler;
      _FEnabled := FEnabled;
      _FLogEnabled := FLogEnabled;
      _FLogOptions := FLogOptions;
      _FMode := FMode;
      _FOnStateChange := scOnStateChange;
      _FRespondToBroadcasts := FRespondToBroadcasts;
      _FServerName := FServerName;
      _FState := scState;
    End;
  Inc(FUpdateCount);
End;
{--------}

Procedure TFSBaseTransport.btBeginUpdatePrim;
Begin
  { do nothing }
End;
{--------}

Procedure TFSBaseTransport.CancelUpdate;
Begin
  FUpdateCount := 0;
End;
{--------}

Procedure TFSBaseTransport.EndUpdate;
Begin
  If FUpdateCount <> 0 Then
    Begin
      Dec(FUpdateCount);
      If FUpdateCount = 0 Then
        Begin

          { Let the descendent classes do their work }
          btEndUpdatePrim;

          { Update the fields with the new values in their _* counterparts }
          { We do not set the private field directly, since some processing may
            need to be done by a properties write method. }
          CommandHandler := _FCmdHandler;
          { Make sure State is set prior to Enabled property and other
            state-dependent properties. }
          State := _FState;
          Enabled := _FEnabled;
          EventLogEnabled := _FLogEnabled;
          EventLogOptions := _FLogOptions;
          Mode := _FMode;
          OnStateChange := _FOnStateChange;
          RespondToBroadcasts := _FRespondToBroadcasts;
          ServerName := _FServerName;

        End;
    End;
End;
{--------}

Procedure TFSBaseTransport.btEndUpdatePrim;
Begin
  { do nothing }
End;
{--------}

Function TFSBaseTransport.btGetCmdHandler: TFSBaseCommandHandler;
Begin
  Result := FCmdHandler;
End;
{--------}

Function TFSBaseTransport.btGetEnabled: boolean;
Begin
  Result := FEnabled;
End;
{--------}

Function TFSBaseTransport.btGetLogOptions: TfsTransportLogOptions;
Begin
  Result := FLogOptions;
End;
{--------}

Function TFSBaseTransport.btGetMode: TfsTransportMode;
Begin
  Result := FMode;
End;
{--------}

Function TFSBaseTransport.btGetRespondToBroadcasts: Boolean;
Begin
  Result := FRespondToBroadcasts;
End;
{--------}

Function TFSBaseTransport.btGetServerName: String; {!!.10}
Begin
  Result := FServerName;
End;
{--------}

Procedure TFSBaseTransport.btSetCmdHandler(aCmdHandler: TFSBaseCommandHandler);
Begin
  If (FUpdateCount > 0) Then
    _FCmdHandler := aCmdHandler
  Else
    Begin
      {Check to make sure the new property is different.}
      If FCmdHandler = aCmdHandler Then Exit;

      If assigned(FCmdHandler) Then
        FCmdHandler.FFRemoveDependent(Self); {!!.11}

      If assigned(aCmdHandler) Then
        aCmdHandler.FFAddDependent(Self); {!!.11}

      FCmdHandler := aCmdHandler; {!!.11}
    End;
End;
{--------}

Procedure TFSBaseTransport.btSetEnabled(Const aEnabled: Boolean);
Begin
  If (FUpdateCount > 0) Then
    _FEnabled := aEnabled
  Else
    Begin
      {Check to make sure the new property is different.}
      If FEnabled = aEnabled Then Exit;
      { If the transport is being disabled but the State indicates some
        amount of activity then make sure the transport is inactive. }
      If (Not aEnabled) And (scState <> fsesInactive) Then
        Begin
          FFNotifyDependents(ffn_Deactivate);
          scSetState(fsesInactive);
        End;
      FEnabled := aEnabled;
    End;
End;
{--------}

Procedure TFSBaseTransport.btSetLogOptions(Const anOptions: TfsTransportLogOptions);
Begin
  If (UpdateCount > 0) Then
    _FLogOptions := anOptions
  Else
    FLogOptions := anOptions;
End;
{--------}

Procedure TFSBaseTransport.btSetMode(Const aMode: TfsTransportMode);
Begin
  If (FUpdateCount > 0) Then
    _FMode := aMode
  Else
    Begin
      {Check to make sure the new property is different.}
      If FMode = aMode Then Exit;
      scCheckInactive;
      FMode := aMode;
    End;
End;
{--------}

Procedure TFSBaseTransport.btSetOnStateChange(Const aHandler: TNotifyEvent);
Begin
  If (FUpdateCount > 0) Then
    _FOnStateChange := aHandler
  Else
    scOnStateChange := aHandler;
End;
{--------}

Procedure TFSBaseTransport.btSetRespondToBroadcasts(Const aRespond: Boolean);
Begin
  If (FUpdateCount > 0) Then
    _FRespondToBroadcasts := aRespond
  Else
    FRespondToBroadcasts := aRespond;
End;
{--------}

Procedure TFSBaseTransport.btSetServername(Const aServername: String); {!!.10}
Begin
  If (FUpdateCount > 0) Then
    _FServerName := aServerName
  Else
    Begin
      {Check to make sure the new property is different.}
      If FServerName = aServername Then Exit;
      scCheckInactive;
      FServerName := aServerName;
    End;
End;
{--------}

Procedure TFSBaseTransport.btCheckListener;
Begin
  If FMode = fstmSend Then
    fsRaiseSCErrorCode(fssce_MustBeListener);
End;
{--------}

Procedure TFSBaseTransport.btCheckSender;
Begin
  If FMode = fstmListen Then
    fsRaiseSCErrorCode(fssce_MustBeSender);
End;
{--------}

Procedure TFSBaseTransport.btCheckServerName;
Begin
  If FServerNameRequired And (FServerName = '') Then
    fsRaiseSCErrorCode(fssce_MustHaveServerName);
End;
{--------}

Procedure TFSBaseTransport.btInternalReply(msgID: Longint;
  errorCode: TffResult;
  replyData: pointer;
  replyDataLen: Longint);
Begin
  scCheckStarted;
End;
{--------}

Procedure TFSBaseTransport.lcSetLogEnabled(Const aEnabled: Boolean);
Begin
  If (UpdateCount > 0) Then
    _FLogEnabled := aEnabled
  Else
    FLogEnabled := aEnabled;
End;
{--------}

Procedure TFSBaseTransport.Process(Msg: PfsDataMessage);
Begin

  btStoreSelfInThreadvar;

  { If we have a command handler, tell the command handler to process the
    message. }
  If assigned(FCmdHandler) Then
    Begin
      { Increment the message count.  Note: This happens whether or not the
        message was handled by a command handler, plugin command handler, or
        server engine. }
      InterlockedIncrement(FMsgCount);
      FCmdHandler.Process(Msg);
    End;
End;
{--------}

Class Function TFSBaseTransport.CurrentTransport: TFSBaseTransport;
Begin
  Result := TFSBaseTransport(fsitvTransportID);
End;
{--------}
{Rewritten !!.11}

Procedure TFSBaseTransport.FFNotificationEx(Const AOp: Byte; AFrom: TFSSpecComp;
  Const AData: TffWord32);
Begin
  Inherited;
  If AOp In [ffn_Destroy, ffn_Remove] Then
    If (AFrom = FCmdHandler) Then
      Begin
        FCmdHandler.FFRemoveDependent(Self);
        FCmdHandler := Nil
      End
    Else If (AFrom = FEventLog) Then
      Begin
        FEventLog.FFRemoveDependent(Self);
        FEventLog := Nil;
      End;
End;
{--------}

Class Procedure TFSBaseTransport.Reply(msgID: Longint;
  errorCode: TffResult;
  replyData: pointer;
  replyDataLen: Longint);
Begin
  CurrentTransport.btInternalReply(msgID, errorCode,
    replyData, replyDataLen);
End;
{--------}

Procedure TFSBaseTransport.ResetMsgCount;
Begin
  FMsgCount := 0;
End;
{--------}

Function TFSBaseTransport.Sleep(Const timeOut: Longint): boolean;
Begin
  Result := False;
End;
{--------}

Function TFSBaseTransport.Supported: boolean;
Begin
  Result := True;
End;
{--------}

Procedure TFSBaseTransport.btStoreSelfInThreadvar;
Begin
  { Store a pointer to this instance so the command handler may quickly
    find us and submit a reply. }
  fsitvTransportID := Longint(Self);
End;
{====================================================================}

{===TfsThreadedTransport=============================================}

Constructor TfsThreadedTransport.Create(aOwner: TComponent);
Begin
  Inherited Create(aOwner);
  FThreadPool := Nil;
  FUnsentRequestQueue := TfsThreadQueue.Create;
  FWaitingForReplyList := TFSSpecThreadList.Create;
End;
{--------}

Destructor TfsThreadedTransport.Destroy;
Var
  anIndex: Longint;
  aRequest: TfsRequest;
Begin
  FFNotifyDependents(ffn_Destroy);

  If assigned(FThreadPool) Then
    FThreadPool.FFRemoveDependent(Self); {!!.11}

  If assigned(FUnsentRequestQueue) Then
    With FUnsentRequestQueue.BeginWrite Do
      Try
        For anIndex := pred(Count) Downto 0 Do
          Begin
            aRequest := TfsRequest(TfsIntListItem(Items[anIndex]).KeyAsInt);
            aRequest.Free;
          End;
      Finally
        EndWrite;
        Free;
      End;

  If assigned(FWaitingForReplyList) Then
    With FWaitingForReplyList.BeginWrite Do
      Try
        For anIndex := pred(Count) Downto 0 Do
          Begin
            aRequest := TfsRequest(TfsIntListItem(Items[anIndex]).KeyAsInt);
            aRequest.Free;
          End;
      Finally
        EndWrite;
        Free;
      End;

  Inherited Destroy;
End;
{--------}
{Rewritten !!.11}

Procedure TfsThreadedTransport.FFNotificationEx(Const AOp: Byte; AFrom: TFSSpecComp;
  Const AData: TffWord32);
Begin
  Inherited;
  If (AFrom = FThreadPool) And
    (AOp In [ffn_Destroy, ffn_Remove]) Then
    Begin
      FThreadPool.FFRemoveDependent(Self);
      FThreadPool := Nil;
    End;
End;
{--------}

Procedure TfsThreadedTransport.SetThreadPool(aPool: TfsThreadPool);
Begin
  If aPool <> FThreadPool Then
    Begin
      If assigned(FThreadPool) Then
        FThreadPool.FFRemoveDependent(Self); {!!.11}

      If Assigned(aPool) Then
        Begin
          FThreadPool := aPool;
          FThreadPool.FFAddDependent(Self); {!!.11}
        End;
    End;
End;
{--------}

Procedure TfsThreadedTransport.Post(transportID: Longint;
  clientID: TffClientID;
  msgID: Longint;
  requestData: pointer;
  requestDataLen: Longint;
  timeout: Longint;
  replyMode: TfsReplyModeType);
Var
  aRequest: TfsRequest;
  anItem: TfsIntListItem;
Begin
  scCheckStarted;
  aRequest := TfsRequest.Create(clientID, msgID, requestData,
    requestDataLen, timeout, replyMode);
  anItem := TfsIntListItem.Create(Longint(aRequest));
  With FUnsentRequestQueue.BeginWrite Do
    Try
      Enqueue(anItem);
    Finally
      EndWrite;
    End;
  If replyMode = fsrmNoReplyWaitUntilSent Then
    Begin
      aRequest.WaitForReply(timeout);
      If Not aRequest.Aborted Then
        aRequest.Free
      Else
        With aRequest Do
          tpLogReqMisc(format(fsc_ReqAborted, [Longint(aRequest), ClientID,
            ErrorCode, Timeout]));
    End;
End;
{--------}

Procedure TfsThreadedTransport.Request(transportID: Longint;
  clientID: TffClientID;
  msgID: Longint;
  timeout: Longint;
  requestData: pointer;
  requestDataLen: Longint;
  replyCallback: TfsReplyCallback;
  replyCookie: Longint);
Var
  aRequest: TfsRequest;

Begin
  scCheckStarted;
  aRequest := TfsRequest.Create(clientID, msgID, requestData, requestDataLen,
    timeout, fsrmReplyExpected);
  tpInternalRequest(aRequest, timeout, 0);
  If assigned(replyCallback) Then
    replyCallback(aRequest.ReplyMsgID, aRequest.ErrorCode,
      aRequest.ReplyData, aRequest.ReplyDataLen,
      replyCookie);
  If Not aRequest.Aborted Then
    aRequest.Free
  Else
    With aRequest Do
      tpLogReqMisc(format(fsc_ReqAborted, [Longint(aRequest), ClientID,
        ErrorCode, Timeout]));
End;
{--------}

Procedure TfsThreadedTransport.tpInternalRequest(aRequest: TfsRequest;
  timeout: Longint;
  aCookie: HWND);
Var
  anItem: TfsIntListItem;
Begin
  anItem := TfsIntListItem.Create(Longint(aRequest));
  With FUnsentRequestQueue.BeginWrite Do
    Try
      Enqueue(anItem);
    Finally
      EndWrite;
    End;

  { Wait for the reply.  If a timeout occurs, assume the request object
    will be freed by the transport thread at some point.  Timeout exceptions
    are raised to the calling object. }
  If timeout = 0 Then
    aRequest.WaitForReply(timeout)
  Else
    aRequest.WaitForReply(timeout + ffcl_RequestLatencyAdjustment);

End;
{--------}

Procedure TfsThreadedTransport.tpLogReq(aRequest: TfsRequest;
  Const prefix: String);
Begin
  If FLogEnabled And (fstpLogRequests In FLogOptions) And
    assigned(FEventLog) And assigned(aRequest) Then
    With aRequest Do
      Begin
        FEventLog.WriteStringFmt(fsc_ReqLogString,
          [prefix, Longint(aRequest), ClientID, MsgID,
          RequestDataLen, Timeout]);
        FEventLog.WriteBlock('Data', aRequest.RequestData,
          aRequest.RequestDataLen);
      End;
End;
{--------}

Procedure TfsThreadedTransport.tpLogReq2(Const aPrefix: String;
  Const aRequestID: Longint;
  Const aClientID: TffClientID;
  Const aMsgID: Longint;
  Const aData: pointer;
  Const aDataLen: Longint;
  Const aTimeout: Longint);
Begin
  FEventLog.WriteStringFmt(fsc_ReqLogString,
    [aPrefix, aRequestID, aClientID, aMsgID,
    aDataLen, aTimeout]);
  FEventLog.WriteBlock(fsc_Data, aData, aDataLen);
End;
{--------}

Procedure TfsThreadedTransport.tpLogReqMisc(Const aMsg: String);
Begin
  If FLogEnabled And (fstpLogRequests In FLogOptions) And
    assigned(FEventLog) Then
    FEventLog.WriteString(aMsg);
End;
{--------}

Procedure TfsThreadedTransport.tpLogReply(aRequest: TfsRequest);
Begin
  If FLogEnabled And (fstpLogReplies In FLogOptions) And
    assigned(FEventLog) And assigned(aRequest) Then
    With aRequest Do
      Begin
        FEventLog.WriteStringFmt(fsc_ReplyLogString,
          [Longint(aRequest), ClientID, ReplyMsgID,
          ReplyDataLen, ErrorCode]);
        FEventLog.WriteBlock(fsc_Data, ReplyData, ReplyDataLen);
      End;
End;
{--------}

Procedure TfsThreadedTransport.tpLogReply2(Const aRequestID: Longint;
  Const aClientID: TffClientID;
  Const aMsgID: Longint;
  Const aDataLen: Longint;
  Const anError: TffResult);
Begin
  { Assumption: Calling routine will only call if it is legitimate to log
                the data.  We do it this way so that we avoid passing tons
                of data on the stack. }
  FEventLog.WriteStringFmt(fsc_ReplyLogString,
    [aRequestID, aClientID, aMsgID, aDataLen, anError]);
End;

{====================================================================}
End.

