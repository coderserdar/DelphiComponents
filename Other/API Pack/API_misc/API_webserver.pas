unit API_webserver;

(*
    Web Server Component
    --------------------
    This unit simplifies the usage of all web server related functions,
    uses indy http server component, as well as idrunner component. these
    component packs can be downloaded from.. (FILL LATER HERE!)
    first revision: 09042006, ari pikivirta
*)

interface

uses
  Classes, Windows, SysUtils, Dialogs, IdHttpServer, SyncObjs, IdComponent,
  IdTCPServer, IdCustomHTTPServer, ExtCtrls;

type
  // client data
  TAPI_WebServerClient = record
    Started: tdatetime;               // creation time
    Updated: tdatetime;               // last updated
    Address: string;                  // client address
    Port: integer;                    // client port
    Root: string;                     // home directory
    Document: string;                 // current document
    UserName: string;                 // user name
    PassWord: string;                 // password
  end;
  TAPI_Clients = record
    Lock: Tcriticalsection;
    Count: integer;
    Item: array of TAPI_WebServerClient;
  end;

  // web server events
  TAPIWebServer_OnStart = procedure(Sender: tobject) of object;
  TAPIWebServer_OnStop = procedure(Sender: tobject) of object;
  TAPIWebServer_OnConnect = procedure(Sender: tobject; var Client: TAPI_WebServerClient; var Allow: Boolean) of object;
  TAPIWebServer_OnFileNotFound = procedure(Sender: tobject; var Client: TAPI_WebServerClient; var Filename: string; var Content: string; Params: string) of object;
  TAPIWebServer_OnGetFile = procedure(Sender: tobject; var Client: TAPI_WebServerClient; FileName: string; var Content: string; Params: string) of object;
  TAPIWebServer_OnGetFileSize = procedure(Sender: tobject; var Client: TAPI_WebServerClient; Filename: string; var Size: int64; Params: string) of object;
  TAPIWebServer_OnPostFile = procedure(Sender: tobject; var Client: TAPI_WebServerClient; Filename: string; Content: string; Params: string) of object;
  TAPIWebServer_OnDisconnect = procedure(Sender: tobject; var Client: TAPI_WebServerClient) of object;
  TAPIWebServer_OnMaxClients = procedure(Sender: tobject; var Client: TAPI_WebServerClient; Content: string) of object;

  // domain aliases
  TAPI_DomainAlias = record
    Name: string;                     // domain name
    Root: String;                     // root folder for the domain
    DefaultPage: string;              // default page for this domain
  end;
  TAPI_DomainAliases = record
    Lock: TCriticalSection;
    Count: integer;                   // number of domains
    Item: array of TAPI_DomainAlias;  // domains data items list
  end;

  // statistics
  TAPI_WebServerStatistics = record
    Lock: TCriticalSection;
    Connections: int64;               // total number of connections
    BytesDownloaded: int64;           // total bytes downloaded
    BytesUploaded: int64;             // total bytes uploaded
  end;

  // web server object
  TAPI_WebServer = class(TComponent)
  private
    flock: tcriticalsection;
    fActive: boolean;
    fmultithreaded: boolean;          // multithreaded or not
    fDefaultPort: integer;            // web server port
    fDefaultPage: string;             // default page if not specified
    fDefaultRoot: string;             // default root path
    fDomains: TAPI_DomainAliases;     // domain aliases
    fClients: TAPI_Clients;           // active clients
    fMimeList: tstringlist;
    fStatistics: TAPI_WebServerStatistics;
    fserversoftware: string;
    fIdServer: TIdHttpServer;         // id http server component
    fSessionTimeout: integer;         // session timeout
    ftimer: TTimer;
    fOnStart: TAPIWebServer_OnStart;
    fOnStop: TAPIWebServer_OnStop;
    fOnConnect: TAPIWebServer_OnConnect;
    fOnFileNotFound: TAPIWebServer_OnFileNotFound;
    fOnGetFile: TAPIWebServer_OnGetFile;
    fOnGetFileSize: TAPIWebServer_OnGetFileSize;
    fOnPostFile: TAPIWebServer_OnPostFile;
    fOnDisconnect: TAPIWebServer_OnDisconnect;
    fOnMaxClients: TAPIWebServer_OnMaxClients;
    procedure SetActive(b: boolean);
    function  GetMime(fname: string): string;
    function  CreateFileName(root, fname: string): string;
    procedure setmultithreaded(b: boolean);
    procedure OnTimer(sender: tobject);
    // indy http server notifications
    procedure IdOnConnect(AThread: TIdPeerThread);
    procedure IdOnDisconnect(AThread: TIdPeerThread);
    procedure IdOnSessionStart(Sender: TIdHTTPSession);
    procedure IdOnSessionEnd(Sender: TIdHTTPSession);
    procedure IdOnCreatePostStream(ASender: TIdPeerThread; var VPostStream: TStream);
    procedure IdOnCommandGet(AThread: TIdPeerThread; ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
    procedure IdOnCommandOther(Thread: TIdPeerThread; const asCommand, asData, asVersion: string);
    procedure IdOnException(AThread: TIdPeerThread; AException: Exception);
  public
    constructor Create(AOwner: tcomponent); override;
    destructor Destroy; override;
    // domain aliases functions
    procedure ClearDomains;                       // clear domain list
    function  Domains: integer;                   // return number of listed domains
    function  GetDomain(Domain: string): integer; overload; // returns domain index
    function  GetDomain(Domain: string; var Root: string; var DefaultPage: string): boolean; overload;
    procedure AddDomain(Domain, Root, DefaultPage: String);   // add new domain for some root
    procedure DelDomain(Domain: String); overload;            // delete domain alias
    procedure DelDomain(index: integer); overload;            // delete domain alias
    // clients (mostly used internal)
    procedure ClearClients;
    procedure ClearClient(var Client: TAPI_WebServerClient);
    function  Clients: integer;
    function  GetClient(var Client: TAPI_WebServerClient): integer;
    procedure AddClient(Client: TAPI_WebServerClient);
    procedure DelClient(Client: TAPI_WebServerClient);
  published
    property ServerSoftware: string read fserversoftware write fserversoftware;
    property Active: boolean read fActive write setactive;
    property DefaultPort: integer read fDefaultport write fdefaultport;
    property DefaultPage: string read fdefaultpage write fdefaultpage;
    property DefaultRoot: string read fdefaultroot write fdefaultroot;
    property Multithreaded: boolean read fmultithreaded write setmultithreaded;
    property MimeList: tstringlist read fmimelist write fmimelist;
    property OnStart: TAPIWebServer_OnStart read fOnStart write fOnStart;
    property OnStop: TAPIWebServer_OnStop read fOnStop write fOnStop;
    property OnConnect: TAPIWebServer_OnConnect read fOnConnect write fOnConnect;
    property OnFileNotFound: TAPIWebServer_OnFileNotFound read fOnFileNotFound write fOnFileNotFound;
    property OnGetFile: TAPIWebServer_OnGetFile read fOnGetFile write fOnGetFile;
    property OnGetFileSize: TAPIWebServer_OnGetFileSize read fOnGetFileSize write fOnGetFileSize;
    property OnPostFile: TAPIWebServer_OnPostFile read fOnPostFile write fOnPostFile;
    property OnDisconnect: TAPIWebServer_OnDisconnect read fOnDisconnect write fOnDisconnect;
    property OnMaxClients: TAPIWebServer_OnMaxClients read fOnMaxClients write fOnMaxClients;
  end;

procedure Register;

implementation

(*------------------------------------------------------------------------------
function:   TAPI_WebServer.Create
abstract:
updated:
------------------------------------------------------------------------------*)
constructor TAPI_WebServer.Create(AOwner: tcomponent);
begin
  inherited Create(Aowner);
  flock:= tcriticalsection.create;
  fmultithreaded:= true;
  fActive:= false;
  fDefaultPort:= 80;
  fDefaultPage:= 'index.html';                  // set default page
  fDefaultRoot:= '';                            // no root defined
  fSessionTimeout:= 30000;                      // set session timeout to 30 secs
  fserversoftware:= 'API_WebServer';
  // create one domain
  fDomains.Lock:= tcriticalsection.create;
  ClearDomains;
  AddDomain('','',fDefaultPage);
  // clients
  fClients.Lock:= tcriticalsection.create;
  ClearClients;
  // statistics
  fStatistics.Lock:= tcriticalsection.create;
  fStatistics.Connections:= 0;
  fStatistics.BytesDownloaded:= 0;
  fStatistics.BytesUploaded:= 0;
  // idhttpserver
  fIdServer:= TIdHttpServer.Create(self);
  fIdServer.OnConnect:= IdOnConnect;
  fIdServer.OnDisconnect:= IdOnDisconnect;
  fIdServer.OnSessionStart:= IdOnSessionStart;
  fIdServer.OnSessionEnd:= IdOnSessionEnd;
  fIdServer.OnCreatePostStream:= IdOnCreatePostStream;
  fIdServer.OnCommandGet:= IdOnCommandGet;
  fIdServer.OnCommandOther:= IdOnCommandOther;
  fIdServer.OnException:= IdOnException;
  // mimelist
  fmimelist:= tstringlist.create;
  fmimelist.clear;
  // timer
  ftimer:= ttimer.create(self);
  ftimer.Interval:= 1000;
  ftimer.OnTimer:= OnTimer;
  ftimer.Enabled:= true;
end;

(*------------------------------------------------------------------------------
function:   TAPI_WebServer.Destroy
abstract:
updated:
------------------------------------------------------------------------------*)
destructor TAPI_WebServer.Destroy;
begin
  ftimer.Free;
  fIdServer.Free;
  fDomains.Lock.Free;
  fClients.Lock.Free;
  fStatistics.Lock.free;
  fmimelist.free;
  flock.free;
  inherited Destroy;
end;

(*------------------------------------------------------------------------------
function:   TAPI_WebServer.ClearDomains
abstract:
updated:
------------------------------------------------------------------------------*)
procedure TAPI_WebServer.ClearDomains;
begin
  fDomains.Lock.Acquire;
  try
    fDomains.Count:= 0;
    setlength(fDomains.Item, fDomains.Count);
  finally
    fDomains.Lock.Release;
  end;
end;

(*------------------------------------------------------------------------------
function:   TAPI_WebServer.
abstract:
updated:
------------------------------------------------------------------------------*)
function  TAPI_WebServer.Domains: integer;
begin
  fDomains.Lock.Acquire;
  try
    result:= fDomains.Count;
  finally
    fDomains.Lock.Release;
  end;
end;

(*------------------------------------------------------------------------------
function:   TAPI_WebServer.
abstract:
updated:
------------------------------------------------------------------------------*)
function TAPI_WebServer.GetDomain(Domain: string): integer;
var
  i: integer;
begin
  fDomains.Lock.Acquire;
  try
    result:= -1;
    for i:= 0 to FDomains.Count-1 do
    begin
      if ansipos(Domain, fDomains.item[i].Name)>0 then
      begin
        result:= i;
        break;
      end;
    end;
  finally
    fDomains.Lock.Release;
  end;
end;

(*------------------------------------------------------------------------------
function:   TAPI_WebServer.
abstract:
updated:
------------------------------------------------------------------------------*)
function  TAPI_WebServer.GetDomain(Domain: string; var Root: string; var DefaultPage: string): boolean;
var
  i: integer;
begin
  result:= false;
  Root:= '';
  DefaultPage:= '';
  i:= GetDomain( Domain );
  if i>-1 then
  begin
    fDomains.Lock.Acquire;
    try
      Root:= fDomains.item[i].Root;
      DefaultPage:= fDomains.item[i].DefaultPage;
      result:= true;
    finally
      fDomains.Lock.Release;
    end;
  end;
end;

(*------------------------------------------------------------------------------
function:   TAPI_WebServer.
abstract:
updated:
------------------------------------------------------------------------------*)
procedure TAPI_WebServer.AddDomain(Domain, Root, DefaultPage: String);
begin
  if GetDomain(Domain)>-1 then
  begin
    fDomains.Lock.Acquire;
    try
      fDomains.Count:= fDOmains.Count + 1;
      setlength(fDomains.Item, fDomains.Count);
      fDomains.item[fDomains.count-1].Name:= Domain;
      fDomains.item[fDomains.count-1].Root:= Root;
      fDomains.Item[fDomains.Count-1].DefaultPage:= DefaultPage;
    finally
      fDomains.Lock.Release;
    end;
  end;
end;

(*------------------------------------------------------------------------------
function:   TAPI_WebServer.
abstract:
updated:
------------------------------------------------------------------------------*)
procedure TAPI_WebServer.DelDomain(Domain: String);
var
  i: integer;
begin
  i:= GetDomain(Domain);
  if i>-1 then DelDomain(i);
end;

(*------------------------------------------------------------------------------
function:   TAPI_WebServer.
abstract:
updated:
------------------------------------------------------------------------------*)
procedure TAPI_WebServer.DelDomain(index: integer);
var
  j: integer;
begin
  fDomains.Lock.Acquire;
  try
    if (index>-1) and (index<fDomains.Count) then
    begin
      for j:=index to fDomains.count-2 do
        fDomains.Item[j]:= fDomains.item[j+1];
      fDomains.count:= fDomains.count - 1;
      setlength(fDomains.Item, fDomains.Count);
    end;
  finally
    fDomains.Lock.Release;
  end;
end;

(*------------------------------------------------------------------------------
function:   TAPI_WebServer.
abstract:
updated:
------------------------------------------------------------------------------*)
procedure TAPI_WebServer.ClearClients;
begin
  fclients.Lock.Acquire;
  try
    fclients.Count:= 0;
    setlength(fclients.Item, fclients.Count);
  finally
    fclients.Lock.Release;
  end;
end;

(*------------------------------------------------------------------------------
function:   TAPI_WebServer.
abstract:
updated:
------------------------------------------------------------------------------*)
function  TAPI_WebServer.Clients: integer;
begin
  fclients.Lock.Acquire;
  try
    result:= fclients.Count;
  finally
    fclients.Lock.Release;
  end;
end;

(*------------------------------------------------------------------------------
function:   TAPI_WebServer.
abstract:
updated:
------------------------------------------------------------------------------*)
function  TAPI_WebServer.GetClient(var Client: TAPI_WebServerClient): integer;
var
  i: integer;
begin
  fclients.Lock.Acquire;
  try
    result:= -1;
    // try to locate client that matches search
    for i:=0 to fclients.Count-1 do
    begin
      if (client.Started>0) and (fclients.Item[i].Started<client.Started) then result:= i;
      if (client.Updated>0) and (fclients.Item[i].Updated<client.Updated) then result:= i;
      if (client.Address<>'') and (ansipos(fclients.item[i].Address,client.Address)>0) then result:= i;
      if (client.Root<>'') and (ansipos(fclients.Item[i].Root,client.Root)>0) then result:= i;
      if (client.UserName<>'') and (fclients.item[i].UserName=client.UserName) then result:= i;
      if (client.PassWord<>'') and (fclients.Item[i].PassWord=client.PassWord) then result:= i;
      if (client.Document<>'') and (ansipos(fclients.item[i].Document,client.Document)>0) then result:= i;
      if (client.Port>0) and (fclients.Item[i].Port=client.Port) then result:= i;
    end;
    // return result if found
    if (result>-1) then client:= fclients.item[result];
  finally
    fclients.Lock.Release;
  end;
end;

(*------------------------------------------------------------------------------
function:   TAPI_WebServer.
abstract:
updated:
------------------------------------------------------------------------------*)
procedure TAPI_WebServer.ClearClient(var Client: TAPI_WebServerClient);
begin
  client.Started:= 0;
  client.Updated:= 0;
  client.Address:= '';
  client.Port:= 0;
  client.Root:= '';
  client.Document:= '';
  client.UserName:= '';
  client.PassWord:= '';
end;

(*------------------------------------------------------------------------------
function:   TAPI_WebServer.
abstract:
updated:
------------------------------------------------------------------------------*)
procedure TAPI_WebServer.AddClient(Client: TAPI_WebServerClient);
var
  tclient: TAPI_WebServerClient;
begin
  clearclient(tclient);
  tclient.Address:= client.Address;
  if GetClient(tclient)<0 then
  begin
    fclients.Lock.Acquire;
    try
      fclients.Count:= fclients.count + 1;
      setlength(fclients.Item, fclients.Count);
      fclients.item[fclients.Count-1]:= client;
      fclients.item[fclients.count-1].Started:= now;
      fclients.Item[fclients.Count-1].Updated:= now;
    finally
      fclients.Lock.Release;
    end;
  end;
end;

(*------------------------------------------------------------------------------
function:   TAPI_WebServer.
abstract:
updated:
------------------------------------------------------------------------------*)
procedure TAPI_WebServer.DelClient(Client: TAPI_WebServerClient);
var
  tclient: TAPI_WebServerClient;
  i,j: integer;
begin
  ClearClient(tclient);
  tclient.Address:= client.Address;
  i:= GetClient(tclient);
  if i>-1 then
  begin
    fclients.lock.Acquire;
    try
      for j:=i to fclients.Count-2 do
        fclients.Item[j]:= fclients.item[j+1];
      fclients.Count:= fclients.Count - 1;
      setlength(fclients.Item, fclients.Count);
    finally
      fclients.Lock.Release;
    end;
  end;
end;

(*------------------------------------------------------------------------------
function:   TAPI_WebServer.
abstract:   every second, we'll check if there is clients older than
            defined time in fsessiontimeout, all found clients are removed
            from the client list.
updated:
------------------------------------------------------------------------------*)
procedure TAPI_WebServer.OnTimer;
var
  tclient: TAPI_WebServerClient;
begin
  if (active) and (fsessiontimeout>0) then
  begin
    ClearClient(tclient);                                 // empty tempclient data
    tclient.Updated:= now - fSessionTimeout*24/3600000;   // get session time
    while Getclient(tclient)>0 do                         // while such clients exist
    begin
      DelClient(tclient);                                 // delete those
      clearclient(tclient);
      tclient.Updated:= now - fSessionTimeout*24/3600000;   // get session time
    end;
  end else
    if clients>0 then
      ClearClients;
end;

(*------------------------------------------------------------------------------
function:   TAPI_WebServer.
abstract:
updated:
------------------------------------------------------------------------------*)
procedure TAPI_WebServer.IdOnConnect(AThread: TIdPeerThread);
var
  client: TAPI_WebServerClient;
  allow: boolean;
begin
  if not fmultithreaded then flock.Acquire;
  ClearClient(client);                                              // clear temporary client's data
  client.Address:= athread.Connection.Socket.Binding.PeerIP;        // get user address
  client.Port:= athread.connection.Socket.Binding.peerport;         // get user port
  allow:= true;                                                     // assume we'll allow this client
  if assigned(fOnConnect) then fOnConnect(self, client, allow);     // do onconnect event
  if not fmultithreaded then flock.Release;
  if not allow then athread.Connection.Disconnect                   // if not allowed, disconnect
    else begin
      fStatistics.Lock.Acquire;
      try
        fStatistics.Connections:= fStatistics.Connections + 1;
      finally
        fStatistics.Lock.Release;
      end;
    end;
end;

(*------------------------------------------------------------------------------
function:   TAPI_WebServer.
abstract:
updated:
------------------------------------------------------------------------------*)
procedure TAPI_WebServer.IdOnDisconnect(AThread: TIdPeerThread);
var
  client: TAPI_WebServerClient;
begin
  if not fmultithreaded then flock.Acquire;
  (*
      indy sucks on this part, all connection data
      is already lost when we get here and so we
      cannot display any info here, nor delete
      clients from list etc..
  *)
  if assigned(fOnDisconnect) then fOnDisconnect(self, client);      // do ondisconnect event
  if not fmultithreaded then flock.Release;
end;

(*------------------------------------------------------------------------------
function:   TAPI_WebServer.
abstract:
updated:
------------------------------------------------------------------------------*)
procedure TAPI_WebServer.IdOnSessionStart(Sender: TIdHTTPSession);
begin
  // do nothing.. we'll manage sessions without cookies
end;

(*------------------------------------------------------------------------------
function:   TAPI_WebServer.
abstract:
updated:
------------------------------------------------------------------------------*)
procedure TAPI_WebServer.IdOnSessionEnd(Sender: TIdHTTPSession);
begin
  // do nothing.. we'll manage sessions without cookies
end;

(*------------------------------------------------------------------------------
function:   TAPI_WebServer.
abstract:
updated:
------------------------------------------------------------------------------*)
procedure TAPI_WebServer.IdOnCreatePostStream(ASender: TIdPeerThread; var VPostStream: TStream);
begin
  // not done yet
end;

(*------------------------------------------------------------------------------
function:   TAPI_WebServer.
abstract:
updated:
------------------------------------------------------------------------------*)
function TAPI_WebServer.CreateFileName(root, fname: string): string;
begin
  if pos('/',root)>0 then root:= stringreplace('\','/',root,[rfReplaceAll]);
  if pos('/',fname)>0 then fname:= stringreplace('\','/',fname,[rfReplaceAll]);
  while pos('\\',root)>0 do
    delete(root,pos('\\',root),1);
  while pos('\\',fname)>0 do
    delete(fname,pos('\\',root),1);
  if root<>'' then
    if root[length(root)]<>'\' then
      root:= root + '\';
  result:= root+fname;
end;

(*------------------------------------------------------------------------------
function:   TAPI_WebServer.
abstract:
updated:
------------------------------------------------------------------------------*)
function TAPI_WebServer.GetMime(fname: string): string;
begin
  result:= fmimelist.Values[lowercase(extractfileext(fname))];
end;

(*------------------------------------------------------------------------------
function:   TAPI_WebServer.
abstract:
updated:
------------------------------------------------------------------------------*)
procedure TAPI_WebServer.IdOnCommandGet(AThread: TIdPeerThread; ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  client: TAPI_WebServerClient;
  index: integer;
  filename, params, content, domain: string;
  fs: tfilestream;
  size: int64;
begin
  ClearClient(client);
  Client.Address:= athread.Connection.Socket.Binding.PeerIP;    // client ip-address
  domain:= arequestinfo.Host;                                   // domain connected

  // retrieve client's data
  index:= GetClient(client);

  if index<0 then
  begin
    client.port:= athread.connection.socket.binding.peerport;
    AddClient(Client);
    index:= 0;
  end;

  if index>-1 then
  begin
    if not fmultithreaded then flock.Acquire;
    content:= '';
    params:= arequestinfo.Params.Text;
    filename:= CreateFileName(client.Root, arequestinfo.Document);
    // 1st check
    if not fileexists(filename) then
    begin
      (*
          file not found
      *)
      if assigned(fOnFileNotFound) then
      begin
        fOnFileNotFound(self, client, filename, content, params);
        if content<>'' then
        begin
          aresponseinfo.ResponseNo:= 404;
          aresponseinfo.ContentType:= GetMime(filename);
          aresponseinfo.ContentText:= content;
          aresponseinfo.ContentLength:= length(content);
          // update statistics
          fStatistics.Lock.Acquire;
          try
            fstatistics.BytesDownloaded:= fstatistics.BytesDownloaded + aresponseinfo.ContentLength;
          finally
            fStatistics.Lock.Release;
          end;
        end;
      end;
    end;
    // 2nd check (may be fixed onfilenotfound event)
    if fileexists(filename) then
    begin
      if ansipos('HEAD', arequestinfo.Command)>0 then
      begin
        (*
            get file size
        *)
        if assigned(fOnGetFileSize) then
        begin
          fOnGetFileSize(self, client, filename, size, params);
          aresponseinfo.ResponseNo:= 200;
          aresponseinfo.ContentType:= GetMime(filename);
          aresponseinfo.ContentLength:= size;
        end else
        begin
          fs:= tfilestream.create(filename, fmopenread or fmsharedenywrite);
          try
            size:= fs.Size;
          finally
            fs.free;
          end;
          aresponseinfo.ResponseNo:= 200;
          aresponseinfo.ContentType:= GetMime(filename);
          aresponseinfo.ContentLength:= size;
        end;
      end else
      begin
        (*
            get file/document
        *)
        if assigned(fOnGetFile) then
          fOnGetFile(self, client, filename, content, params);
        if content<>'' then
        begin
          aresponseinfo.ContentType:= GetMime(filename);
          aresponseinfo.ContentText:= Content;
          aresponseinfo.ContentLength:= length(content);
        end else
          fidserver.ServeFile(athread, aresponseinfo, filename);
        // update statistics
        fStatistics.Lock.Acquire;
        try
          fstatistics.BytesDownloaded:= fstatistics.BytesDownloaded + aresponseinfo.ContentLength;
        finally
          fStatistics.Lock.Release;
        end;
      end;
    end;

    // update client data
    fclients.Lock.Acquire;
    try
      client.Updated:= now;
      client.Document:= filename;
    finally
      fclients.Lock.Release;
    end;

    if not fmultithreaded then flock.Release;
  end;
end;

(*------------------------------------------------------------------------------
function:   TAPI_WebServer.
abstract:
updated:
------------------------------------------------------------------------------*)
procedure TAPI_WebServer.IdOnCommandOther(Thread: TIdPeerThread; const asCommand, asData, asVersion: string);
begin
  // do something.. let user to deside
end;

(*------------------------------------------------------------------------------
function:   TAPI_WebServer.
abstract:
updated:
------------------------------------------------------------------------------*)
procedure TAPI_WebServer.IdOnException(AThread: TIdPeerThread; AException: Exception);
begin
  // damn exceptions
end;

(*------------------------------------------------------------------------------
function:   TAPI_WebServer.
abstract:
updated:
------------------------------------------------------------------------------*)
procedure TAPI_WebServer.SetActive(b: Boolean);
begin
  if b<>fActive then
  begin
    if b then
    begin
      // activate server
      try
        fIdServer.DefaultPort:= fdefaultport;
        fIdServer.ServerSoftware:= fserversoftware;
        fIdServer.MaxConnections:= 0;
        fIdServer.Active:= true;
        if assigned(fOnStart) then fOnStart(self);
      except
      end;
    end else
    begin
      // de-activate server
      try
        fIdServer.Active:= false;
        if assigned(fOnStop) then fOnStop(self);
      except
      end;
    end;
    // get actual state here..
    fActive:= fIdServer.Active;
  end;
end;

(*------------------------------------------------------------------------------
function:   TAPI_WebServer.
abstract:
updated:
------------------------------------------------------------------------------*)
procedure  TAPI_WebServer.setmultithreaded(b: boolean);
begin
  if not factive then fmultithreaded:= b;
end;

//------------------------------------------------------------------------------
procedure Register;
begin
  RegisterComponents('API Misc', [TAPI_webserver]);
end;

end.
