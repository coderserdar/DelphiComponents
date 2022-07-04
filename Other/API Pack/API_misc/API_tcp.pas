unit API_tcp;

//------------------------------------------------------------------------------
// component free to use and modify, subject to following restinctions:
// 1. do not mispresent the origin
// 2. altered revisions must be clearly marked as modified from the original
// 3. do not remove this notice from the source code
// 4. send email about bugs, features needed and features you would like
// * if you like this very much, feel free to donate and support supporting
// and developing the package at www.paypal.com - ari pikivirta@kolumbus.fi
//------------------------------------------------------------------------------
//
// r1.01/26072006, ari pikivirta
//  * changed sendfile function to wait response to wait until some response
//    is received (or application is terminated), meanwhile application is
//    processing messages. (this allows to use example also sending files
//    to itself)
//
// r1.00/19072006, ari pikivirta

interface

uses
  SysUtils, Classes, IdTCPServer, IdTCPClient, IdThread, SyncObjs, ExtCtrls;

const
  // server internal command lines
  TAPI_tcpCommand_PutFile = 'PFL1';               // client req sending file to server
  TAPI_tcpCommand_GetFile = 'GFL1';               // client requesting file from server
  TAPI_tcpCommand_PutText = 'PTXT';               // client putting text to server
  TAPI_tcpCommand_GetText = 'GTXT';

type
  // component events
  TAPI_OnServerStart = procedure(sender: tobject) of object;
  TAPI_OnServerError = procedure(sender: tobject; from, text: string; stamp: tdatetime) of object;
  TAPI_OnServerGetFile = procedure(sender: tobject; from: string; var filename: string; filesize: int64; var Accept: boolean) of object;
  TAPI_OnServerGetFileComplete = procedure(sender: tobject; from, filename: string) of object;
  TAPI_OnServerPutFile = procedure(sender: tobject; ipto: string; filename: string; var Accept: boolean) of object;
  TAPI_OnServerPutFileComplete = procedure(sender: tobject; ipto, filename: string) of object;
  TAPI_OnServerGetText = procedure(sender: tobject; from, text: string) of object;
  TAPI_OnServerClose = procedure(sender: tobject) of object;

  // error messages
  TAPI_tcpserver_erroritem = record
    from: string;
    text: string;
    stamp: tdatetime;
  end;

  // text messages
  TAPI_tcpserver_messageitem = record
    from: string;
    text: string;
    stamp: tdatetime;
  end;

  // server data record
  TAPI_tcpServerData = record
    lock: tcriticalsection;
    header: string;
    errors: integer;
    error: array of TAPI_tcpserver_erroritem;
    texts: integer;
    text: array of TAPI_tcpserver_erroritem;
  end;

  // component
  TAPI_tcp = class(TComponent)
  private
    { Private declarations }
    fversion: string;
    factive: boolean;
    ftimer: ttimer;
    ftcpserver: tidtcpserver;
    fserverdata: tapi_tcpserverdata;
    fserverport: integer;
    fonserverstart: tapi_onserverstart;
    fonservererror: tapi_onservererror;
    fOnServerGetFile: TAPI_OnServerGetFile;
    fonservergetfilecomplete: TAPI_onservergetfilecomplete;
    fOnServerPutFile: TAPI_OnServerPutFile;
    fonserverputfilecomplete: TAPI_onserverputfilecomplete;
    fonservergettext: TAPI_OnServerGetText;
    fonserverclose: tapi_onserverclose;
    ftcpclient: tidtcpclient;
    fclient_readtimeout: integer;
    procedure server_close;
    procedure server_setactive(b: boolean);
    function  server_getactive: boolean;
    procedure server_setport(p: integer);
    procedure server_setheader(s: string);
    function  server_getheader: string;
    procedure server_adderror(from, text: string);
    function  server_geterror: TAPI_tcpserver_erroritem;
    procedure server_addmessage(from, text: string);
    function  server_getmessage: TAPI_tcpserver_messageitem;
    procedure server_onexecute(AThread: TIdPeerThread);
    procedure server_putfile(amethod: integer; athread: TIdPeerThread);
    procedure server_getfile(amethod: integer; athread: TIdPeerThread);
    procedure server_puttext(athread: TIdPeerThread);
    procedure client_setreadtimeout(i: integer);
    procedure timer_ontimer(sender: tobject);
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(AOwner: tcomponent); override;
    destructor Destroy; override;
    function SendFile(recipient: string; port: integer; filename: string): boolean;
    function SendText(recipient: string; port: integer; text: string): boolean;
  published
    { Published declarations }
    property Version: string read fversion write fversion stored false;
    property ServerPort: integer read fserverport write server_setport;
    property Header: string read server_getheader write server_setheader;
    property Active: boolean read server_getactive write server_setactive;
    property ReadTimeout: integer read fclient_readtimeout write client_setreadtimeout;
    property OnServerStart: TAPI_OnServerStart read fonserverstart write fonserverstart;
    property OnServerError: TAPI_OnServerError read fonservererror write fonservererror;
    property OnServerGetFile: TAPI_OnServerGetFile read fonservergetfile write fonservergetfile;
    property OnServerGetFileComplete: TAPI_onservergetfilecomplete read fonservergetfilecomplete write fonservergetfilecomplete;
    property OnServerPutFile: TAPI_OnServerPutFile read fonserverputfile write fonserverputfile;
    property OnServerPutFileComplete: TAPI_onserverputfilecomplete read fonserverputfilecomplete write fonserverputfilecomplete;
    property OnServerGetText: TAPI_OnServerGetText read fonservergettext write fonservergettext;
    property OnServerClose: TAPI_OnServerClose read fonserverclose write fonserverclose;
  end;

procedure Register;

implementation

uses
  forms;

//------------------------------------------------------------------------------
constructor TAPI_tcp.Create(AOwner: TComponent);
begin
  inherited create(aowner);
  fversion:= 'r1.01/ari.pikivirta@kolumbus.fi';
  ftcpserver:= tidtcpserver.Create(self);           // create internally idtcpserver
  ftcpserver.DefaultPort:= 4000;                    // default port setting for the server
  ftcpserver.OnExecute:= server_onexecute;          // procedure to launch on execute
  fserverdata.lock:= tcriticalsection.create;       // server data locking
  fserverdata.header:= 'tapi_tcp_header';           // header to this server to accept anything
  ftcpclient:= tidtcpclient.Create(self);           // create internal tcp client
  ftcpclient.ReadTimeout:= 5000;                    // timeout, just in case
  ftimer:= ttimer.create(self);                     // create timer for messaging threadsafe
  ftimer.interval:= 100;                            // set timer interval to 100ms
  ftimer.ontimer:= timer_ontimer;                   // assign timer procedure
  factive:= false;                                  // something going on..
end;

//------------------------------------------------------------------------------
destructor TAPI_tcp.Destroy;
begin
  ftcpclient.Free;                                  // free client
  server_close;                                     // close server
  ftcpserver.Free;                                  // free server
  fserverdata.lock.free;                            // free server data locking
  inherited destroy;
end;

//------------------------------------------------------------------------------
procedure TAPI_tcp.server_setactive(b: boolean);
begin
  if ftcpserver.Active<>b then                    // if state is changed
  begin
    if ftcpserver.Active then server_close
      else
      begin
        try
          ftcpserver.Active:= true;
        except
          server_adderror('', 'Failed to activate server.');
        end;
        if assigned(fonserverstart) then
          fonserverstart(self);
      end;
  end;
end;

//------------------------------------------------------------------------------
function TAPI_tcp.server_getactive: boolean;
begin
  result:= ftcpserver.Active;                   // result tcp server state
end;

//------------------------------------------------------------------------------
procedure TAPI_tcp.server_close;
var
  wasactive: boolean;
begin
  wasactive:= ftcpserver.active;
  while ftcpserver.Active do
  try
    ftcpserver.Active:= false;
  except
    // we don't care about exceptions
    // just we try to cut all lines until
    // indy component succeeds
  end;
  if wasactive then
    if assigned(fonserverclose) then
      fonserverclose(self);
end;

//------------------------------------------------------------------------------
procedure TAPI_tcp.server_setport(p: Integer);
begin
  if p<>ftcpserver.defaultport then
  begin
    server_close;
    ftcpserver.DefaultPort:= p;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_tcp.server_setheader(s: string);
begin
  fserverdata.lock.acquire;
  try
    if fserverdata.header<>s then
      fserverdata.header:= s;
  finally
    fserverdata.lock.release;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_tcp.client_setreadtimeout(i: integer);
begin
  if ftcpclient.ReadTimeout<>i then
  begin
    ftcpclient.ReadTimeout:= i;
    fclient_readtimeout:= ftcpclient.readtimeout;
  end;
end;

//------------------------------------------------------------------------------
function  TAPI_tcp.server_getheader: string;
begin
  fserverdata.lock.acquire;
  try
    result:= fserverdata.header;
  finally
    fserverdata.lock.release;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_tcp.server_adderror(from, text: string);
begin
  fserverdata.lock.acquire;
  try
    fserverdata.errors:= fserverdata.errors + 1;
    setlength(fserverdata.error, fserverdata.errors);
    fserverdata.error[fserverdata.errors-1].from:= from;
    fserverdata.error[fserverdata.errors-1].Text:= text;
    fserverdata.error[fserverdata.errors-1].stamp:= now;
  finally
    fserverdata.lock.release;
  end;
end;

//------------------------------------------------------------------------------
function TAPI_tcp.server_geterror: TAPI_tcpserver_erroritem;
begin
  fserverdata.lock.acquire;
  try
    if fserverdata.errors>0 then
    begin
      result.from:= fserverdata.error[fserverdata.errors-1].from;
      result.text:= fserverdata.error[fserverdata.errors-1].Text;
      result.stamp:= fserverdata.error[fserverdata.errors-1].stamp;
      fserverdata.errors:= fserverdata.errors-1;
      setlength(fserverdata.error, fserverdata.errors);
    end else
    begin
      result.from:= '';
      result.text:= '';
      result.stamp:= 0;
    end;
  finally
    fserverdata.lock.release;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_tcp.server_addmessage(from, text: string);
begin
  fserverdata.lock.acquire;
  try
    fserverdata.texts:= fserverdata.texts + 1;
    setlength(fserverdata.text, fserverdata.texts);
    fserverdata.text[fserverdata.texts-1].from:= from;
    fserverdata.text[fserverdata.texts-1].Text:= text;
    fserverdata.text[fserverdata.texts-1].stamp:= now;
  finally
    fserverdata.lock.release;
  end;
end;

//------------------------------------------------------------------------------
function TAPI_tcp.server_getmessage: TAPI_tcpserver_messageitem;
begin
  fserverdata.lock.acquire;
  try
    if fserverdata.texts>0 then
    begin
      result.from:= fserverdata.text[fserverdata.texts-1].from;
      result.text:= fserverdata.text[fserverdata.texts-1].Text;
      result.stamp:= fserverdata.text[fserverdata.texts-1].stamp;
      fserverdata.texts:= fserverdata.texts-1;
      setlength(fserverdata.text, fserverdata.texts);
    end else
    begin
      result.from:= '';
      result.text:= '';
      result.stamp:= 0;
    end;
  finally
    fserverdata.lock.release;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_tcp.server_onexecute(AThread: TIdPeerThread);
var
  from: string;
  header: string;
  command: string;
begin
  from:= athread.Connection.Socket.Binding.PeerIP;                              // get peer ip

  header:= athread.connection.ReadLn;                                           // read header
  if server_getheader = header then                                             // check header
  begin

    command:= athread.connection.readln;                                        // read command

    if command = TAPI_tcpCommand_putFile then server_putfile(1, athread)        // request receiving
      else if command = TAPI_tcpCommand_GetFile then server_getfile(1, athread) // request send
      else if command = TAPI_tcpCommand_PutText then server_puttext(athread)    // receive text
      else
      begin
        athread.connection.writeln('- Unknown command.');
        server_adderror(from, 'Unknown command ('+command+').');                // unknown command
      end;

  end else
  begin
    athread.connection.writeln('- Header check failed.');                       // header check failed
    server_adderror(from, 'Header check failed.');
  end;

  if athread.Connection.Connected then                                          // if still connected
    athread.connection.Disconnect;                                              // disconnect
end;

//------------------------------------------------------------------------------
procedure TAPI_tcp.server_putfile(amethod: integer; athread: TIdPeerThread);
var
  from: string;
  filename: string;
  filesize: int64;
  accept: boolean;
  fs: tfilestream;
begin
  from:= athread.connection.Socket.Binding.peerip;                              // get ip address

  // get file information
  filename:= athread.Connection.ReadLn;                                         // read filename
  try filesize:= strtoint(athread.connection.readln);                           // read filesize
  except
    athread.connection.writeln('- File size information corrupted.');
    server_adderror(from, 'Failed to convert file size ('+filename+').');
    exit;
  end;

  // confirm download
  if assigned(fonservergetfile) then
  begin
    accept:= true;
    fonservergetfile(self, from, filename, filesize, accept);
  end else
  begin
    athread.connection.writeln('- Server event not assigned.');
    server_adderror(from, 'Cannot receive file because getfile event is not assigned.');
    exit;
  end;

  // complete download
  if accept then                                        // if accepted
  begin
    try
      fs:= tfilestream.Create(filename, fmCreate);      // create file
      try
        fs.seek(0,0);                                   // make sure we're on start
        athread.connection.WriteLn('+');                // send ok to continue reply
        athread.Connection.ReadStream(fs);              // receive and write stream
        // to be done: check that received
        // stream size matches with the
        // read filesize in the beginning
        server_addmessage('_FILERECV_',from+';'+filename);       // internal messaging!!
      finally
        fs.free;                                        // close file
      end;
    except
      server_adderror(from, 'Failed to create file stream for '+filename+'.');
    end;
  end else
    athread.Connection.WriteLn('- Access denied.');
end;

//------------------------------------------------------------------------------
procedure TAPI_tcp.server_getfile(amethod: integer; athread: TIdPeerThread);
var
  from: string;
  filename: string;
  filesize: int64;
  s: string;
  accept: boolean;
  fs: tfilestream;
begin
  from:= athread.connection.Socket.Binding.peerip;

  // get file information
  filename:= athread.Connection.ReadLn;                                         // read filename to send

  // confirm upload
  if assigned(fonserverputfile) then
  begin
    accept:= true;
    fonserverputfile(self, from, filename, accept);
  end else
  begin
    athread.Connection.Writeln('- Server event missing.');
    server_adderror(from, 'Cannot send file because putfile event is not assigned.');
    exit;
  end;

  // check that file exist
  if not fileexists(filename) then
  begin
    athread.Connection.Writeln('- File does not exist.');
    server_adderror(from, 'File '+filename+' does not exist.');
    exit;
  end;

  // complete upload
  if (accept) then                                          // if accepted upload
  begin
    try
      fs:= tfilestream.Create(filename, fmOpenRead);        // open file for reading
      try
        filesize:= fs.size;                                 // get file size
        athread.connection.writeln(inttostr(filesize));     // write file size
        fs.seek(0,0);                                       // go to beginning of stream
        s:= athread.connection.readln;                      // wait for response
        if (s<>'') then                      // if ok to upload
        begin
          if s[1]='+' then
          begin
            athread.Connection.writeStream(fs, true, true, fs.size);// send file stream
            server_addmessage('_FILESENT_',from+';'+filename+'!');           // internal messaging!!
          end;
        end else
          server_adderror(from, 'No response from the client.');
      finally
        fs.free;                                            // close file stream
      end;
    except
      server_adderror(from, 'Failed to open file stream for '+filename+'.');
    end;
  end else
    athread.Connection.WriteLn('- Access denied.');
end;

//------------------------------------------------------------------------------
procedure TAPI_tcp.server_puttext(athread: TIdPeerThread);
var
  from: string;
  text: string;
begin
  from:= athread.connection.socket.binding.peerip;
  text:= athread.connection.ReadLn;
  if text<>'' then
    server_addmessage(from, text);
end;

//------------------------------------------------------------------------------
function TAPI_tcp.SendFile(recipient: string; port: integer; filename: string): boolean;
var
  fs: tfilestream;
  filesize: int64;
  s: string;
begin
  result:= false;
  if (recipient<>'') and (filename<>'') and (not factive) then
  begin
    // check that file exists
    if not fileexists(filename) then
    begin
      server_adderror('','File '+filename+' does not exist.');
      exit;
    end;
    // send file
    ftcpclient.Host:= recipient;
    ftcpclient.Port:= port;
    try
      ftcpclient.Connect(1000);                                // connect recipient
      factive:= true;
      try
        ftcpclient.writeln(server_getheader);                 // write header
        ftcpclient.WriteLn(TAPI_tcpCommand_putFile);          // command
        ftcpclient.writeln(filename);                         // filename
        fs:= tfilestream.Create(filename, fmOpenRead);        // open file for reading
        try
          filesize:= fs.size;                                 // get file size
          ftcpclient.writeln(inttostr(filesize));             // write file size
          fs.seek(0,0);                                       // go to beginning of stream
          ftcpclient.ReadTimeout:= 200;                       // modify read timeout a bit
          repeat
            application.ProcessMessages;                      // processmessages here
            try
              s:= ftcpclient.readln;                            // wait for response
            except
              // we just don't care about timeout
              // because we're waiting for the
              // response - either + or -
            end;
          until (s<>'') or (application.terminated);          // until terminated or response
          ftcpclient.ReadTimeout:= fclient_readtimeout;       // reset back what it was
          if (s<>'') then
          begin
            if (s[1]='+') then                                // if ok to upload
            begin
              ftcpclient.writeStream(fs,true,true,fs.size);   // send file stream
              server_addmessage('_FILESENT_',recipient+';'+filename);  // internal messaging!!
            end;
          end else
            server_adderror(recipient, 'No response from the client.');
        finally
          fs.free;                                            // close file stream
        end;
      finally
        if ftcpclient.Connected then
            ftcpclient.Disconnect;
        factive:= false;
      end;
      result:= true;
    except
      server_adderror('','Failed to connect host '+recipient);
    end;
  end;
end;

//------------------------------------------------------------------------------
function TAPI_tcp.SendText(recipient: string; port: integer; text: string): boolean;
begin
  result:= false;
  if (recipient<>'') and (text<>'') and (not factive) then
  begin
    ftcpclient.Host:= recipient;
    ftcpclient.Port:= port;
    try
      ftcpclient.Connect(1000);
      factive:= true;
      try
        ftcpclient.writeln(server_getheader);                 // write header
        ftcpclient.WriteLn(TAPI_tcpCommand_puttext);          // command
        ftcpclient.writeln(text);                             // write text
      finally
        if ftcpclient.Connected then
          ftcpclient.Disconnect;
        factive:= false;
      end;
      result:= true;
    except
      server_adderror('','Failed to connect host '+recipient);
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_tcp.timer_ontimer(sender: tobject);
var
  error: tapi_tcpserver_erroritem;
  text: tapi_tcpserver_messageitem;
  i: integer;
begin
  // get all error messages
  repeat
    error:= server_geterror;
    if error.text<>'' then
      if assigned(fonServerError) then
        fonservererror(self, error.from, error.text, error.stamp);
  until error.text='';

  // get all text messages
  repeat
    text:= server_getmessage;
    if text.text<>'' then
    begin
      // file received
      if text.from = '_FILERECV_' then
      begin
        i:= pos(';',text.text);
        if assigned(fonservergetfilecomplete) then
          fonservergetfilecomplete(self, copy(text.text,1,i-1), copy(text.text,i+1,length(text.text)));
      end else
      // file sent
      if text.from = '_FILESENT_' then
      begin
        i:= pos(';',text.text);
        if assigned(fonserverputfilecomplete) then
          fonserverputfilecomplete(self, copy(text.text,1,i-1), copy(text.text,i+1,length(text.text)));
      end else
      // normal message from some client
        if assigned(fonservergettext) then
          fonservergettext(self, text.from, text.text);
    end;
  until text.text='';
end;

//------------------------------------------------------------------------------
procedure Register;
begin
  RegisterComponents('API Misc', [TAPI_tcp]);
end;

end.
