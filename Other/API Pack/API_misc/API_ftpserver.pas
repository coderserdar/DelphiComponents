unit API_ftpserver;

//==============================================================================
// customized ftp server running on indy components suites ftp server
//
// 19092004, r1.01, ari pikivirta
// * added updating of user data
// * added checking of user allowings (except change directory)
// * added after transfer event for file monitoring
// * added member lists + automated login defaults if event is not used!
//
// 14092004, r1.00, ari pikivirta
// * first revision, now works and includes some new items to home ftp server also, check todo list what was left for next release
// * user specific read/write speed
// * file transfers monitoring
// * integrated logs, ban lists..
// * using indy ftp server (remember to FIX the indy rename replies!)
//
// todo:
// * update user data (current dir etc.)
// * all needed server events (for file monitoring etc.)
// * open sll support
// * saving & opening settings
// * testing with different ftp clients
// * checking of user allows

//==============================================================================

interface

uses
  SysUtils, Classes, idTcpServer, idFtpServer, IdThread, SyncObjs, IdFtpList,
  IdGlobal, IdBaseComponent, IdComponent;

type
  tftp_filestream_onstart = procedure ( const filename: string; const pos, size: int64; var id: integer ) of object;
  tftp_filestream_onprogress = procedure ( const filename: string; const pos, size: int64; elapsed: longint; var id: integer ) of object;
  tftp_filestream_onstop = procedure ( const filename: string; const pos, size: int64; const elapsed: longint; const bytesread, byteswritten: int64; var id: integer ) of object;

  tftp_filestream = class (tfilestream)
  private
    fbytespersecond: longint;
    ffilename: string;
    fstarttime: longint;
    fbytesread: int64;
    fbyteswritten: int64;
    fid: integer;
    fonstart: tftp_filestream_onstart;
    fonprogress: tftp_filestream_onprogress;
    fonstop: tftp_filestream_onstop;
  public
    constructor Create(const FileName: string; Mode: Word; Rights: Cardinal; LimitBaudrate: longint);
    destructor Destroy; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    property OnStart: tftp_filestream_onstart read fonstart write fonstart;
    property OnProgress: tftp_filestream_onprogress read fonprogress write fonprogress;
    property OnStop: tftp_filestream_onstop read fonstop write fonstop;
  end;

  Tftpevent_onerror = procedure (
    sender: tobject;
    const address: string;
    const port: integer;
    const errormsg: string
    ) of object;

  Tftpevent_onconnect = procedure (
    sender: tobject;
    const address: string;          // connecting address
    const port: integer;            // connecting port
    var banned: boolean             // banned?
    ) of object;

  Tftpevent_ondisconnect = procedure (
    sender: tobject;
    const address: string;
    const port: integer
    ) of object;

  Tftpevent_onlogin = procedure (
    sender: tobject;
    const address: string;
    const port: integer;
    const username: string;
    const password: string;
    var homedir: string;
    var currentdir: string;
    var virtualdirs: string;
    var authenticated: boolean;
    var readspeed: integer;
    var writespeed: integer;
    var allow_chdir: boolean;
    var allow_mkdir: boolean;
    var allow_rmdir: boolean;
    var allow_rename: boolean;
    var allow_delete: boolean;
    var allow_get: boolean;
    var allow_put: boolean;
    var allow_hidden: boolean
    ) of object;

  Tftpevent_onaddlog = procedure (
    sender: tobject;
    const time: tdatetime;
    const text: string
    ) of object;

  Tftpevent_aftertransfer = procedure (
    sender: tobject;
    const filename: string;
    const size: int64;
    const elapsed: longint
    ) of object;

  Tftpuserdata = record
    name: string;                   // username
    pass: string;                   // password
    address: string;                // address in use
    port: integer;                  // port in use
    homedir: string;                // home directory
    currentdir: string;             // current directory
    virtualdirs: string;            // users virtual directories
    starttime: tdatetime;           // start time
    lasttime: tdatetime;            // last action
    readspeed: integer;             // read speed
    bytesread: int64;               // total bytes read
    filesread: integer;             // total files read
    writespeed: integer;            // write speed
    byteswritten: int64;            // total bytes written
    fileswritten: integer;          // total files written
    allow_chdir: boolean;           // allow to change directories
    allow_mkdir: boolean;           // allow to make directories
    allow_rmdir: boolean;           // allow to remove directories
    allow_rename: boolean;          // allow renaming files
    allow_delete: boolean;          // allow deleting files
    allow_get: boolean;             // allow reading files
    allow_put: boolean;             // allow writing files
    allow_hidden: boolean;          // allow showing of hidden files
  end;

  Tftpuserlist = record
    lock: tcriticalsection;         // data structure lock
    count: integer;                 // users listed in here
    maxcount: integer;              // maximum users allowed
    data: array of Tftpuserdata;    // user data (see above)
    allow_anonymous: boolean;       // allow anonymous > uses user without
                                    //   address, name = "anonymous"
    defaultreadspeed: integer;
    defaultwritespeed: integer;
  end;

  Tftpmemberlist = record
    lock: tcriticalsection;         // member data structure lock
    count: integer;                 // count
    data: array of Tftpuserdata;    // data as above
  end;

  Tftplogdata = record
    time: tdatetime;                // log line time
    text: string;                   // log message
  end;

  Tftploglist = record
    lock: tcriticalsection;         // data structure lock
    count: integer;                 // logged lines
    maxcount: integer;              // max lines to log
    data: array of Tftplogdata;
  end;

  Tftptransferdata = record
    id: integer;                    // unique id for this specific transfer
    user: integer;                  // owner of transfer
    filename: string;               // file being transferred
    size: int64;                    // file size
    pos: int64;                     // current position
    elapsed: longint;               // time elapsed
  end;

  Tftptransferlist = record
    lock: tcriticalsection;         // data structure lock
    count: integer;                 // file count being transferred
    maxcount: integer;              // maximum file transfers allowed
    data: array of Tftptransferdata;
  end;    

  Tftpbanneddata = record
    addr: string;                   // banned ip address
    text: string;                   // reason for ban
  end;

  Tftpbannedlist = record
    lock: tcriticalsection;         // data structure lock
    count: integer;                 // banned ips count
    data: array of Tftpbanneddata;
  end;

  Tftpstatistics = record
    lock: tcriticalsection;
    connections: integer;           // total connections handled
    sessions: integer;              // total sessions opened
    bytesread: int64;               // total bytes read
    byteswritten: int64;            // total bytes written
  end;

  TAPI_ftpserver = class(TComponent)
  private
    fversion: string;               // version information
    fidftpserver: tidftpserver;     // id ftp server!
    fserver: string;                // server name
    fserveraddr: string;            // server address
    fport: integer;                 // default port
    fdataport: integer;             // default data port
    factive: boolean;               // ftp server listening
    fstat: tftpstatistics;          // ftp statistics
    fusers: tftpuserlist;           // ftp users
    ftransfers: tftptransferlist;   // file transfers
    flog: tftploglist;              // log
    fbans: tftpbannedlist;          // banned ips
    fbanner: tstringlist;           // banner text
    fmembers: tftpmemberlist;       // member list
    // events..
    fonerror: tftpevent_onerror;
    fonconnect: tftpevent_onconnect;
    fondisconnect: tftpevent_ondisconnect;
    fonlogin: tftpevent_onlogin;
    fonaddlog: tftpevent_onaddlog;
    faftertransfer: tftpevent_aftertransfer;
    procedure dummys(s: string);
    procedure dummyi(i: integer);
    procedure dummyi64(i: int64);
    procedure dummysl(sl: tstringlist);
    procedure setactive(b: boolean);
    procedure AddUser( address: string; port: integer; username: string;
      password: string; homedir: string; currentdir: string; virtualdirs: string;
      readspeed: integer; writespeed: integer; allow_chdir: boolean;
      allow_mkdir: boolean; allow_rmdir: boolean; allow_rename: boolean;
      allow_delete: boolean; allow_get: boolean; allow_put: boolean;
      allow_hidden: boolean );
    procedure DelUser( username: string; password: string ); overload;
    procedure DelUser( address: string; port: integer ); overload;
    function  UserIndex( username, password: string ): integer; // internal use only
    function  MemberIndex( username, password: string ): integer; // internal use only
    function  buildfilepath( username, password, homedir, currentdir, apath: string ): string;
    function  getclientcount: integer;
    function  getbannedlist: tstringlist;
    procedure setbannedlist(sl: tstringlist);
    function  GetLoglist: tstringlist;
    function  getconnectioncount: integer;
    function  getsessioncount: integer;
    function  getbytesread: int64;
    function  getbyteswritten: int64;
    function  getreadspeed( username, password: string ): longint;
    function  getwritespeed( username, password: string ): longint;
    function  gettransfers: tstringlist;
    function  getuserlist: tstringlist;
    function  getmemberlist: tstringlist;
    // idftpserver events..
    procedure _AfterCommandHandler(ASender: TIdTCPServer; AThread: TIdPeerThread);
    procedure _AfterUserLogin(ASender: TIdFTPServerThread);
    procedure _BeforeCommandHandler(ASender: TIdTCPServer; const AData: String; AThread: TIdPeerThread);
    procedure _ChangeDirectory(ASender: TIdFTPServerThread; var VDirectory: String);
    procedure _Connect(AThread: TIdPeerThread);
    procedure _DeleteFile(ASender: TIdFTPServerThread; const APathName: String);
    procedure _Disconnect(AThread: TIdPeerThread);
    procedure _Exception(AThread: TIdPeerThread; AException: Exception);
    procedure _Execute(AThread: TIdPeerThread);
    procedure _GetCustomListFormat(ASender: TIdFTPServer; AItem: TIdFTPListItem; var VText: String);
    procedure _GetFileSize(ASender: TIdFTPServerThread; const AFilename: String; var VFileSize: Int64);
    procedure _ListDirectory(ASender: TIdFTPServerThread; const APath: String; ADirectoryListing: TIdFTPListItems);
    procedure _ListenException(AThread: TIdListenerThread; AException: Exception);
    procedure _MakeDirectory(ASender: TIdFTPServerThread; var VDirectory: String);
    procedure _NoCommandHandler(ASender: TIdTCPServer; const AData: String; AThread: TIdPeerThread);
    procedure _PASV(ASender: TIdFTPServerThread; var VIP: String; var VPort: Word);
    procedure _RemoveDirectory(ASender: TIdFTPServerThread; var VDirectory: String);
    procedure _RenameFile(ASender: TIdFTPServerThread; const ARenameFromFile, ARenameToFile: String);
    procedure _RetrieveFile(ASender: TIdFTPServerThread; const AFileName: String; var VStream: TStream);
    procedure _Status(ASender: TObject; const AStatus: TIdStatus; const AStatusText: String);    procedure _StoreFile(ASender: TIdFTPServerThread; const AFileName: String; AAppend: Boolean; var VStream: TStream);
    procedure _UserLogin(ASender: TIdFTPServerThread; const AUsername, APassword: String; var AAuthenticated: Boolean);
    // file stream
    procedure _StreamProgress( const filename: string; const pos, size: int64; elapsed: longint; var id: integer );
    procedure _StreamStop( const filename: string; const pos, size: int64; const elapsed: longint; const bytesread, byteswritten: int64;  var id: integer );
  protected
  public
    constructor create(aowner: tcomponent); override;
    destructor destroy; override;
    function  IsBanned( addr: string ): boolean;
    procedure AddtoLog( text: string );
    procedure ClearTransfers;
    procedure ClearLog;
    procedure ClearStats;
    procedure ClearUsers;
    procedure ClearBans;
    // members
    procedure ClearMembers;
    function  AddMember( address: string; port: integer; username: string;
      password: string; homedir: string; virtualdirs: string;
      readspeed: integer; writespeed: integer; allow_chdir: boolean;
      allow_mkdir: boolean; allow_rmdir: boolean; allow_rename: boolean;
      allow_delete: boolean; allow_get: boolean; allow_put: boolean;
      allow_hidden: boolean ): boolean;
    function ModMember( oldname, oldpass: string;
      address: string; port: integer; username: string;
      password: string; homedir: string; virtualdirs: string;
      readspeed: integer; writespeed: integer; allow_chdir: boolean;
      allow_mkdir: boolean; allow_rmdir: boolean; allow_rename: boolean;
      allow_delete: boolean; allow_get: boolean; allow_put: boolean;
      allow_hidden: boolean ): boolean;
    function GetMember( const name, pass: string;
      var address: string; var port: integer; var homedir: string; var virtualdirs: string;
      var readspeed: integer; var writespeed: integer; var allow_chdir: boolean;
      var allow_mkdir: boolean; var allow_rmdir: boolean; var allow_rename: boolean;
      var allow_delete: boolean; var allow_get: boolean; var allow_put: boolean;
      var allow_hidden: boolean ): boolean;
    function DelMember( name, pass: string ): boolean;
    // files..
    function Open( filename: string ): boolean;
    function Save( filename: string ): boolean;
  published
    property Version: string read fversion write dummys stored false;
    property Server: string read fserver write fserver;
    property ServerAddress: string read fserveraddr write dummys stored false;
    property Port: integer read fport write fport;
    property DataPort: integer read fdataport write fdataport;
    property Active: boolean read factive write setactive;
    property Banned: tstringlist read getbannedlist write setbannedlist;
    property Log: tstringlist read getloglist write dummysl stored false;
    property Banner: tstringlist read fbanner write fbanner;
    property Transfers: tstringlist read gettransfers write dummysl stored false;
    property Users: tstringlist read getuserlist write dummysl stored false;
    property Members: tstringlist read getmemberlist write dummysl stored false;
    // statistics..
    property Clients: integer read getclientcount write dummyi stored false;
    property Connections: integer read getconnectioncount write dummyi stored false;
    property Sessions: integer read getsessioncount write dummyi stored false;
    property BytesRead: int64 read getbytesread write dummyi64 stored false;
    property BytesWritten: int64 read getbyteswritten write dummyi64 stored false;
    // events..
    property OnConnect: tftpevent_onconnect read fonconnect write fonconnect;
    property OnDisconnect: tftpevent_ondisconnect read fondisconnect write fondisconnect;
    property OnError: tftpevent_onerror read fonerror write fonerror;
    property OnLogin: tftpevent_onlogin read fonlogin write fonlogin;
    property OnAddLog: tftpevent_onaddlog read fonaddlog write fonaddlog;
    property AfterTransfer: tftpevent_aftertransfer read faftertransfer write faftertransfer;
  end;

procedure Register;

//==============================================================================
implementation

uses
  inifiles;

const
  versioninfostring: string = 'r1.00/ari.pikivirta@kolumbus.fi';

//------------------------------------------------------------------------------
procedure Register;
begin
  RegisterComponents('API Misc', [TAPI_ftpserver]);
end;

//------------------------------------------------------------------------------
constructor TAPI_ftpserver.create(aowner: tcomponent);
begin
  inherited create(aowner);
  fversion:= versioninfostring;
  fbanner:= tstringlist.create;
  fbanner.Text:= 'Wellcome to API Ftp Server';
  // log
  flog.lock:= tcriticalsection.create;
  flog.maxcount:= 1000;
  clearlog;
  // users
  fusers.lock:= tcriticalsection.create;
  fusers.maxcount:= 100;
  fusers.allow_anonymous:= false;
  clearusers;
  // users
  fmembers.lock:= tcriticalsection.create;
  clearmembers;
  // transfers
  self.ftransfers.lock:= tcriticalsection.create;
  self.ftransfers.maxcount:= 50;
  cleartransfers;
  // ban list
  self.fbans.lock:= tcriticalsection.create;
  clearbans;
  // statistics
  fstat.lock:= tcriticalsection.create;
  clearstats;
  // idftpserver
  self.fidftpserver:= tidftpserver.Create( self );
  fidftpserver.EmulateSystem:= ftpsUnix;
  //fidftpserver.AllowAnonymousLogin:= false;
  fidftpserver.OnAfterUserLogin:= _afteruserlogin;
  fidftpserver.OnChangeDirectory:= _changedirectory;
  fidftpserver.OnGetCustomListFormat:= _getcustomlistformat;
  fidftpserver.OnGetFileSize:= _getfilesize;
  fidftpserver.OnUserLogin:= _userlogin;
  fidftpserver.OnListDirectory:= _listdirectory;
  fidftpserver.OnRenameFile:= _renamefile;
  fidftpserver.OnDeleteFile:= _deletefile;
  fidftpserver.OnRetrieveFile:= _retrievefile;
  fidftpserver.OnStoreFile:= _storefile;
  fidftpserver.OnMakeDirectory:= _makedirectory;
  fidftpserver.OnRemoveDirectory:= _removedirectory;
//  fidftpserver.OnPASV:= _pasv;
  fidftpserver.OnAfterCommandHandler:= _aftercommandhandler;
  fidftpserver.OnBeforeCommandHandler:= _beforecommandhandler;
  fidftpserver.OnConnect:= _connect;
  fidftpserver.OnExecute:= _execute;
  fidftpserver.OnDisconnect:= _disconnect;
  fidftpserver.onexception:= _exception;
  fidftpserver.OnListenException:= _listenexception;
  fidftpserver.OnNoCommandHandler:= _nocommandhandler;
  fidftpserver.OnStatus:= _status;
  self.fserver:= 'TAPI FTP server ('+fversion+')';
  self.fserveraddr:= fidftpserver.localname;
  self.fport:= 21;
  self.fdataport:= 20;
  self.factive:= false;
end;

//------------------------------------------------------------------------------
destructor TAPI_ftpserver.destroy;
begin
  self.fidftpserver.Free;
  self.fusers.lock.Free;
  self.fmembers.lock.free;
  self.ftransfers.lock.Free;
  self.fbans.lock.Free;
  self.flog.lock.Free;
  fbanner.Free;
  inherited destroy;
end;

//------------------------------------------------------------------------------
procedure TAPI_ftpserver.dummys(s: string);
begin
end;

procedure TAPI_ftpserver.dummyi(i: integer);
begin
end;

procedure TAPI_ftpserver.dummysl(sl: tstringlist);
begin
end;

procedure TAPI_ftpserver.dummyi64(i: int64);
begin
end;

//------------------------------------------------------------------------------
function TAPI_ftpserver.Open( filename: string ): boolean;
var
  f: tinifile;
begin
  result:=false;
  f:= tinifile.create( filename );
  // general settings
  // users
  // banned
end;

//------------------------------------------------------------------------------
function TAPI_ftpserver.Save( filename: string ): boolean;
var
  f: tinifile;
begin
  result:=False;
  f:= tinifile.create( filename );
  // general settings
  // users
  // banned
end;

//------------------------------------------------------------------------------
procedure TAPI_ftpserver.AddtoLog( text: string );
var
  i: integer;
begin
  flog.lock.Acquire;
  try
    if flog.count< flog.maxcount then
    begin
      flog.count:= flog.count +1;
      setlength( flog.data, flog.count );
    end else
    begin
      for i:=0 to flog.count-2 do
        flog.data[i]:= flog.data[i+1];
    end;
    flog.data[ flog.count-1 ].time:= now;
    flog.data[ flog.count-1 ].text:= text;
    // this is called from locked critical section point
    // so this is in the way synchronized already :)
    if assigned( fonaddlog ) then
      fonaddlog( self, now, text );
  finally
    flog.lock.Release;
  end;
end;

//------------------------------------------------------------------------------
function TAPI_ftpserver.getuserlist: tstringlist;
var
  i: integer;
begin
  fusers.lock.Acquire;
  try
    result:= tstringlist.create;
    result.clear;
    for i:= 0 to fusers.count-1 do
      result.add( fusers.data[i].address+', '+inttostr(fusers.data[i].port)+', '+fusers.data[i].name+', '+fusers.data[i].currentdir+', '+timetostr(fusers.data[i].lasttime));
  finally
    fusers.lock.Release;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_ftpserver.ClearMembers;
begin
  fmembers.lock.acquire;
  try
    fmembers.count:= 0;
    setlength( fmembers.data, fmembers.count );
  finally
    fmembers.lock.release;
  end;
end;

//------------------------------------------------------------------------------
function TAPI_ftpserver.getmemberlist: tstringlist;
var
  i: integer;
begin
  fmembers.lock.Acquire;
  try
    result:= tstringlist.create;
    result.clear;
    for i:= 0 to fusers.count-1 do
      result.add( fmembers.data[i].address+', '+inttostr(fmembers.data[i].port)+', '+fmembers.data[i].name+', '+timetostr(fmembers.data[i].lasttime));
  finally
    fmembers.lock.Release;
  end;
end;

//------------------------------------------------------------------------------
function TAPI_ftpserver.MemberIndex( username, password: string ): integer;
var
  i: integer;
  j: integer;
begin
  j:=-1;
  for i:=0 to fmembers.count-1 do
    if (fmembers.data[i].name = username) and (fmembers.data[i].pass = password) then
    begin
      j:= i;
      break;
    end;
  result:= j;
end;

//------------------------------------------------------------------------------
function TAPI_ftpserver.AddMember( address: string; port: integer; username: string;
      password: string; homedir: string; virtualdirs: string;
      readspeed: integer; writespeed: integer; allow_chdir: boolean;
      allow_mkdir: boolean; allow_rmdir: boolean; allow_rename: boolean;
      allow_delete: boolean; allow_get: boolean; allow_put: boolean;
      allow_hidden: boolean ): boolean;
var
  i: integer;
begin
  fmembers.lock.acquire;
  try
    result:=False;
    i:= memberindex( username, password );
    if i<0 then
    begin
      fmembers.count:= fmembers.count +1;
      setlength( fmembers.data, fmembers.count );
      fmembers.data[ fmembers.count-1 ].address:= address;
      fmembers.data[ fmembers.count-1 ].port:= port;
      fmembers.data[ fmembers.count-1 ].name:= username;
      fmembers.data[ fmembers.count-1 ].pass:= password;
      fmembers.data[ fmembers.count-1 ].homedir:= homedir;
      fmembers.data[ fmembers.Count-1 ].virtualdirs:= virtualdirs;
      fmembers.data[ fmembers.count-1 ].readspeed:= readspeed;
      fmembers.data[ fmembers.count-1 ].writespeed:= writespeed;
      fmembers.data[ fmembers.count-1 ].allow_chdir:= allow_chdir;
      fmembers.data[ fmembers.count-1 ].allow_mkdir:= allow_mkdir;
      fmembers.data[ fmembers.count-1 ].allow_rmdir:= allow_rmdir;
      fmembers.data[ fmembers.count-1 ].allow_rename:= allow_rename;
      fmembers.data[ fmembers.count-1 ].allow_delete:= allow_delete;
      fmembers.data[ fmembers.count-1 ].allow_get:= allow_get;
      fmembers.data[ fmembers.count-1 ].allow_put:= allow_put;
      fmembers.data[ fmembers.count-1 ].allow_hidden:= allow_hidden;
      addtolog('New member added ('+username+')');
      result:=true;
    end else
      addtolog('Cannot add new member, member with same name and pass already exists');
  finally
    fmembers.lock.release;
  end;
end;

//------------------------------------------------------------------------------
function TAPI_ftpserver.ModMember( oldname, oldpass: string;
      address: string; port: integer; username: string;
      password: string; homedir: string; virtualdirs: string;
      readspeed: integer; writespeed: integer; allow_chdir: boolean;
      allow_mkdir: boolean; allow_rmdir: boolean; allow_rename: boolean;
      allow_delete: boolean; allow_get: boolean; allow_put: boolean;
      allow_hidden: boolean ): boolean;
var
  i: integer;
begin
  fmembers.lock.acquire;
  try
    result:=False;
    i:= memberindex( oldname, oldpass );
    if i>-1 then
    begin
      fmembers.data[ i ].address:= address;
      fmembers.data[ i ].port:= port;
      fmembers.data[ i ].name:= username;
      fmembers.data[ i ].pass:= password;
      fmembers.data[ i ].homedir:= homedir;
      fmembers.data[ i ].virtualdirs:= virtualdirs;
      fmembers.data[ i ].readspeed:= readspeed;
      fmembers.data[ i ].writespeed:= writespeed;
      fmembers.data[ i ].allow_chdir:= allow_chdir;
      fmembers.data[ i ].allow_mkdir:= allow_mkdir;
      fmembers.data[ i ].allow_rmdir:= allow_rmdir;
      fmembers.data[ i ].allow_rename:= allow_rename;
      fmembers.data[ i ].allow_delete:= allow_delete;
      fmembers.data[ i ].allow_get:= allow_get;
      fmembers.data[ i ].allow_put:= allow_put;
      fmembers.data[ i ].allow_hidden:= allow_hidden;
      addtolog('Member '+oldname+' modified');
      result:=True;
    end else
      addtolog('Cannot modify member, member was not found');
  finally
    fmembers.lock.release;
  end;
end;

//------------------------------------------------------------------------------
function TAPI_ftpserver.GetMember( const name, pass: string;
      var address: string; var port: integer; var homedir: string; var virtualdirs: string;
      var readspeed: integer; var writespeed: integer; var allow_chdir: boolean;
      var allow_mkdir: boolean; var allow_rmdir: boolean; var allow_rename: boolean;
      var allow_delete: boolean; var allow_get: boolean; var allow_put: boolean;
      var allow_hidden: boolean ): boolean;
var
  i: integer;
begin
  fmembers.lock.acquire;
  try
    result:=False;
    i:= memberindex( name, pass );
    if i>-1 then
    begin
      address:= fmembers.data[ i ].address;
      port:= fmembers.data[ i ].port;
//      username:= fmembers.data[ i ].name;
//      fmembers.data[ i ].pass:= password;
      homedir:= fmembers.data[ i ].homedir;
      virtualdirs:= fmembers.data[ i ].virtualdirs;
      readspeed:= fmembers.data[ i ].readspeed;
      writespeed:= fmembers.data[ i ].writespeed;
      allow_chdir:= fmembers.data[ i ].allow_chdir;
      allow_mkdir:= fmembers.data[ i ].allow_mkdir;
      allow_rmdir:= fmembers.data[ i ].allow_rmdir;
      allow_rename:= fmembers.data[ i ].allow_rename;
      allow_Delete:= fmembers.data[ i ].allow_delete;
      allow_get:= fmembers.data[ i ].allow_get;
      allow_put:= fmembers.data[ i ].allow_put;
      allow_hidden:= fmembers.data[ i ].allow_hidden;
      result:=True;
    end;
  finally
    fmembers.lock.release;
  end;
end;

//------------------------------------------------------------------------------
function TAPI_ftpserver.DelMember( name, pass: string ): boolean;
var
  i: integer;
  j: integer;
begin
  fmembers.lock.acquire;
  try
    result:= false;
    i:= memberindex( name, pass );
    if i>-1 then
    begin
      for j:=i to fmembers.count-2 do
        fmembers.data[j]:= fmembers.data[j+1];
      fmembers.count:= fmembers.count -1;
      setlength( fmembers.data, fmembers.Count );
      addtolog('Member '+name+' deleted');
      result:=true;
    end else
      addtolog('Cannot delete member, member was not found');
  finally
    fmembers.lock.release;
  end;
end;

//------------------------------------------------------------------------------
function TAPI_ftpserver.getbannedlist: tstringlist;
var
  i: integer;
begin
  fbans.lock.Acquire;
  try
    result:=tstringlist.create;
    result.clear;
    for i:=0 to fbans.count-1 do
      result.Add( fbans.data[i].addr );
  finally
    fbans.lock.Release;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_ftpserver.setbannedlist(sl: tstringlist);
var
  i: integer;
begin
  fbans.lock.Acquire;
  try
    fbans.count:= sl.Count;
    setlength( fbans.data, fbans.count );
    for i:=0 to sl.count-1 do
      fbans.data[i].addr:= sl[i];
  finally
    fbans.lock.Release;
  end;
end;

//------------------------------------------------------------------------------
function TAPI_ftpserver.GetLoglist: tstringlist;
var
  i: integer;
begin
  flog.lock.Acquire;
  try
    result:= tstringlist.create;
    result.clear;
    for i:=0 to flog.count-1 do
      result.add( datetostr( flog.data[i].time ) + ' ' + timetostr( flog.data[i].time ) + ', '+ flog.data[i].text );
  finally
    flog.lock.Release;
  end;
end;

//------------------------------------------------------------------------------
function  TAPI_ftpserver.gettransfers: tstringlist;
var
  i: integer;
  speed: real;
begin
  ftransfers.lock.acquire;
  try
    result:= tstringlist.Create;
    result.clear;
    for i:=0 to ftransfers.count-1 do
    begin
      if ftransfers.data[i].elapsed<>0 then
        speed:= ftransfers.data[i].pos / (ftransfers.data[i].elapsed / 1000)
        else speed:= 0 ;
      result.add( ftransfers.data[i].filename+', '+inttostr(ftransfers.data[i].pos)+'b, '+inttostr(ftransfers.data[i].elapsed)+'ms, '+formatfloat('0.00', speed)+'bps');
    end;
  finally
    ftransfers.lock.release;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_ftpserver.ClearStats;
begin
  fstat.lock.Acquire;
  try
    fstat.connections:= 0;
    fstat.sessions:= 0;
    fstat.bytesread:= 0;
    fstat.byteswritten:= 0;
  finally
    fstat.lock.Release;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_ftpserver.ClearTransfers;
begin
  ftransfers.lock.Acquire;
  try
    ftransfers.count:=0;
    setlength( ftransfers.data, ftransfers.count );
  finally
    ftransfers.lock.Release;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_ftpserver.ClearBans;
begin
  fbans.lock.Acquire;
  try
    fbans.count:= 0;
    setlength( fbans.data, fbans.count );
  finally
    fbans.lock.Release;
  end;
end;

//------------------------------------------------------------------------------
function TAPI_ftpserver.getreadspeed( username, password: string ): longint;
var
  i: integer;
begin
  fusers.lock.Acquire;
  try
    i:= userindex( username, password );
    if i>-1 then
      result:= fusers.data[i].readspeed
      else result:= fusers.defaultreadspeed;
  finally
    fusers.lock.Release;
  end;
end;

//------------------------------------------------------------------------------
function TAPI_ftpserver.getwritespeed( username, password: string ): longint;
var
  i: integer;
begin
  fusers.lock.Acquire;
  try
    i:= userindex( username, password );
    if i>-1 then
      result:= fusers.data[i].writespeed
      else result:= fusers.defaultwritespeed;
  finally
    fusers.lock.Release;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_ftpserver.ClearLog;
begin
  flog.lock.Acquire;
  try
    flog.count:= 0;
    setlength( flog.data, flog.count );
  finally
    flog.lock.Release;
  end;
end;

//------------------------------------------------------------------------------
function TAPI_ftpserver.getconnectioncount: integer;
begin
  fstat.lock.Acquire;
  try
    result:= fstat.connections;
  finally
    fstat.lock.Release;
  end;
end;

//------------------------------------------------------------------------------
function TAPI_ftpserver.getsessioncount: integer;
begin
  fstat.lock.Acquire;
  try
    result:= fstat.sessions;
  finally
    fstat.lock.Release;
  end;
end;

//------------------------------------------------------------------------------
function TAPI_ftpserver.getbytesread: int64;
begin
  fstat.lock.Acquire;
  try
    result:= fstat.bytesread;
  finally
    fstat.lock.Release;
  end;
end;

//------------------------------------------------------------------------------
function TAPI_ftpserver.getbyteswritten: int64;
begin
  fstat.lock.Acquire;
  try
    result:= fstat.byteswritten;
  finally
    fstat.lock.Release;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_ftpserver.setactive(B: boolean);
begin
  if b<>factive then
  begin
    if b then
    begin
      fidftpserver.DefaultPort:= fport;
      fidftpserver.DefaultDataPort:= fdataport;
      fidftpserver.Greeting.Text.text:= fbanner.Text;
      try
        fidftpserver.Active:=true;
        clearusers;
        addtolog('Ftp server activated');
      except
        if assigned(fonerror) then
          fonerror( self, '', -1, 'Failed to activate ftp server');
        addtolog('Failed to activate Ftp server');
      end;
    end else
    begin
      try
        fidftpserver.Active:= false;
        clearusers;
        addtolog('Ftp server deactivated');
      except
        addtolog('Failed to deactivate Ftp server');
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------
function  TAPI_ftpserver.getclientcount: integer;
begin
  fusers.lock.Acquire;
  try
    result:= fusers.count;
  finally
    fusers.lock.Release;
  end;
end;

//------------------------------------------------------------------------------
function TAPI_ftpserver.IsBanned( addr: string ): boolean;
var
  i: integer;
begin
  fbans.lock.Acquire;
  try
    result:=false;
    for i:=0 to fbans.count-1 do
      if addr= fbans.data[i].addr then
      begin
        result:=true;
        break;
      end;
  finally
    fbans.lock.Release;
  end;
end;

//------------------------------------------------------------------------------
// builds new real directory from clients current data (situation)
// THIS REALLY is the hardest one with indy currently..
function TAPI_ftpserver.buildfilepath( username, password, homedir, currentdir, apath: string ): string;
var
  _homedir: string;
  _currentdir: string;
  _apath: string;
  sl: tstringlist;
  i: integer;
  p: integer;
  vir: boolean;
begin
  result:='';
  _homedir:= homedir;
  _currentdir:= currentdir;
  _apath:= apath;

  // prepare current dir
  if _currentdir<>'' then
  begin
    if _currentdir[1]<>'/' then
      _currentdir:='/'+_currentdir;
    if _currentdir[ length(_currentdir) ]<>'/' then
      _currentdir:= _currentdir + '/';
  end else
    _currentdir:='/';
  //addtolog('debug: current dir = '+_currentdir );

  // check if item is from root.
  if _apath<>'' then
    if _apath[1]<>'/' then
    begin
      // combine apath and current dir  "/current/item.exe"
      _apath:= _currentdir + _apath;
      //addtolog('debug: combined = '+_apath );
    end; // else
      //addtolog('debug: item = '+_apath );

  // check for virtual directories  "/
  fusers.lock.Acquire;
  try
    vir:=false;
    i:= userindex( username, password );
    // user was found, right?
    if (i>-1) then
    begin
      // update user data
      //fusers.data[i].homedir:= _homedir;
      fusers.data[i].currentdir:= _currentdir;
      fusers.data[i].lasttime:= now;
      // loook for virtual dirs
      if (fusers.data[i].virtualdirs<>'') then
      begin
        sl:=tstringlist.create;
        try
          sl.CommaText:= fusers.data[i].virtualdirs;
          for i:=0 to sl.Count-1 do
          begin
            if ansisametext( sl.Names[i], copy(_apath,2,length(sl.names[i])) ) then
            begin
              _apath:= stringreplace( _apath, copy(_apath,1,length(sl.names[i])+1), sl.Values[ sl.names[i] ], [rfreplaceall] );
              vir:= true;
              //addtolog( 'debug: virtual dir = '+sl.values[ sl.names[i] ] );
            end;
          end;
        finally
          sl.free;
        end;
      end;
    end;
  finally
    fusers.lock.Release;
  end;

  // replace unix slashes
  p:= pos('/', _apath);
  while p>0 do
  begin
    _apath[p]:='\';
    p:= pos('/', _apath);
  end;

  // result..
  if not vir then
  begin
    // remove "\" from homedir end if exists
    if _homedir[ length(_homedir) ] = '\' then
      delete(_homedir, length(_homedir), 1);
    result:= _homedir + _apath
  end else
    result:= _apath;

  // remove double slashes
  p:= pos('\\', result);
  while p>0 do
  begin
    delete( result, p, 1 );
    p:= pos('\\', result);
  end;

  //addtolog('debug: result = '+result);
end;

//------------------------------------------------------------------------------
procedure TAPI_ftpserver.ClearUsers;
begin
  fusers.lock.Acquire;
  try
    fusers.count:= 0;
    setlength( fusers.data, fusers.count );
  finally
    fusers.lock.Release;
  end;
  addtolog('User list cleared.');
end;

//------------------------------------------------------------------------------
// INTERNAL FUNCTION
function  TAPI_ftpserver.UserIndex( username, password: string ): integer;
var
  i: integer;
  j: integer;
begin
  j:=-1;
  for i:=0 to fusers.count-1 do
    if (fusers.data[i].name= username) and (fusers.data[i].pass= password) then
    begin
      j:= i;
      break;
    end;
  result:= j;
end;

//------------------------------------------------------------------------------
procedure TAPI_ftpserver.AddUser( address: string; port: integer; username: string;
      password: string; homedir: string; currentdir: string; virtualdirs: string;
      readspeed: integer; writespeed: integer; allow_chdir: boolean;
      allow_mkdir: boolean; allow_rmdir: boolean; allow_rename: boolean;
      allow_delete: boolean; allow_get: boolean; allow_put: boolean;
      allow_hidden: boolean );
var
  j: integer;
begin
  fusers.lock.Acquire;
  try
    // check if already exists
    j:= userindex( username, password );
    if j<0 then
    begin
      // add new user
      fusers.count:= fusers.count + 1;
      setlength( fusers.data, fusers.count );
      fusers.data[ fusers.count-1 ].name:= username;
      fusers.data[ fusers.count-1 ].pass:= password;
      fusers.data[ fusers.count-1 ].address:= address;
      fusers.data[ fusers.count-1 ].port:= port;
      fusers.data[ fusers.count-1 ].homedir:= homedir;
      fusers.data[ fusers.count-1 ].currentdir:= currentdir;
      fusers.data[ fusers.count-1 ].virtualdirs:= virtualdirs;
      fusers.data[ fusers.count-1 ].starttime:= now;
      fusers.data[ fusers.count-1 ].lasttime:= now;
      fusers.data[ fusers.count-1 ].readspeed:= readspeed;
      fusers.data[ fusers.count-1 ].writespeed:= writespeed;
      fusers.data[ fusers.count-1 ].byteswritten:= 0;
      fusers.data[ fusers.count-1 ].bytesread:= 0;
      fusers.data[ fusers.count-1 ].filesread:= 0;
      fusers.data[ fusers.count-1 ].fileswritten:= 0;
      fusers.data[ fusers.count-1 ].allow_chdir:= allow_chdir;
      fusers.data[ fusers.count-1 ].allow_mkdir:= allow_mkdir;
      fusers.data[ fusers.count-1 ].allow_rmdir:= allow_rmdir;
      fusers.data[ fusers.count-1 ].allow_rename:= allow_rename;
      fusers.data[ fusers.count-1 ].allow_delete:= allow_delete;
      fusers.data[ fusers.count-1 ].allow_get:= allow_get;
      fusers.data[ fusers.count-1 ].allow_put:= allow_put;
      fusers.data[ fusers.count-1 ].allow_hidden:= allow_hidden;
    end else
    begin
      // update existing user
      if username<>'' then fusers.data[ j ].name:= username;
      if password<>'' then fusers.data[ j ].pass:= password;
      //if address<>'' then fusers.data[ j ].address:= address;
      //if port>0 then fusers.data[ j ].port:= port;
      if homedir<>'' then fusers.data[ j ].homedir:= homedir;
      if currentdir<>'' then fusers.data[ j ].currentdir:= currentdir;
      if virtualdirs<>'' then fusers.data[ j ].virtualdirs:= virtualdirs;
      fusers.data[ j ].lasttime:= now;
    end;
  finally
    fusers.lock.Release;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_ftpserver.DelUser( username: string; password: string );
var
  i: integer;
  j: integer;
begin
  fusers.lock.Acquire;
  try
    // locate user
    j:= userindex( username, password );
    // remove if found
    if j>-1 then
    begin
      for i:=j to fusers.count-2 do
        fusers.data[i]:= fusers.data[i+1];
      fusers.count:= fusers.count - 1;
      setlength(fusers.data, fusers.count);
    end;
  finally
    fusers.lock.Release;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_ftpserver.DelUser( address: string; port: integer );
var
  i: integer;
  j: integer;
begin
  fusers.lock.Acquire;
  try
    // locate user
    j:= -1;
    for i:=0 to fusers.count-1 do
      if (fusers.data[i].address = address) and (fusers.data[i].port= port ) then
      begin
        j:= i;
        break;
      end;
    // remove if found
    if j>-1 then
    begin
      for i:=j to fusers.count-2 do
        fusers.data[i]:= fusers.data[i+1];
      fusers.count:= fusers.count - 1;
      setlength(fusers.data, fusers.count);
    end;
  finally
    fusers.lock.Release;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_ftpserver._AfterCommandHandler(ASender: TIdTCPServer; AThread: TIdPeerThread);
begin
  // does nothing..
end;

//------------------------------------------------------------------------------
procedure TAPI_ftpserver._AfterUserLogin(ASender: TIdFTPServerThread);
begin
  // does nothing..
end;

//------------------------------------------------------------------------------
procedure TAPI_ftpserver._BeforeCommandHandler(ASender: TIdTCPServer; const AData: String; AThread: TIdPeerThread);
begin
  // does nothing..
end;

//------------------------------------------------------------------------------
procedure TAPI_ftpserver._ChangeDirectory(ASender: TIdFTPServerThread; var VDirectory: String);
var
  addr: string;
  port: integer;
  path: string;
begin
  addr:= asender.Connection.Socket.Binding.PeerIP;
  port:= asender.connection.socket.Binding.PeerPort;
  path:= buildfilepath( asender.Username, asender.Password, asender.HomeDir, asender.CurrentDir, VDirectory );
  if directoryexists( path ) then
  begin
    addtolog( addr+':'+inttostr(port)+', '+asender.username+'> Change directory '+vdirectory);
  end else
    raise exception.Create( addr+':'+inttostr(port)+', '+asender.Username+'> Failed to change directory to '+vdirectory);
end;

//------------------------------------------------------------------------------
procedure TAPI_ftpserver._Connect(AThread: TIdPeerThread);
var
  addr: string;
  port: integer;
  banned: boolean;
begin
  addr:= athread.Connection.Socket.Binding.PeerIP;
  port:= athread.connection.socket.Binding.PeerPort;
  banned:= isbanned( addr );
  fstat.lock.Acquire;
  try
    fstat.connections:= fstat.connections + 1;
  finally
    fstat.lock.Release;
  end;
  addtolog( addr+':'+inttostr(port)+'> Connected.');
  if assigned(fonconnect) then
    fonconnect( self, addr, port, banned );
  if banned then
    athread.Terminate;
end;

//------------------------------------------------------------------------------
procedure TAPI_ftpserver._DeleteFile(ASender: TIdFTPServerThread; const APathName: String);
var
  addr: string;
  port: integer;
  path: string;
begin
  addr:= asender.Connection.Socket.Binding.PeerIP;
  port:= asender.connection.socket.Binding.PeerPort;
  path:= buildfilepath( asender.Username, asender.Password, asender.HomeDir, asender.CurrentDir, apathname );
  if fileexists( path ) then
  begin
    if deletefile( path ) then
    begin
      addtolog( addr+':'+inttostr(port)+', '+asender.username+'> Delete file '+apathname);
    end else
      raise exception.create( addr+':'+inttostr(port)+', '+asender.Username+'> Failed to delete file '+apathname);
  end else
    raise exception.create( addr+':'+inttostr(port)+', '+asender.Username+'> File to delete does not exist '+apathname);
end;

//------------------------------------------------------------------------------
procedure TAPI_ftpserver._Disconnect(AThread: TIdPeerThread);
var
  addr: string;
  port: integer;
begin
  addr:= athread.connection.socket.Binding.PeerIP;
  port:= athread.connection.socket.Binding.PeerPort;
  if assigned( fondisconnect ) then
    fondisconnect( self, addr, port );
  deluser( addr, port );
  addtolog( addr+':'+inttostr(port)+'> Disconnected.');
end;

//------------------------------------------------------------------------------
procedure TAPI_ftpserver._Exception(AThread: TIdPeerThread; AException: Exception);
var
  addr: string;
  port: integer;
begin
  addr:= athread.Connection.Socket.Binding.PeerIP;
  port:= athread.connection.Socket.Binding.peerport;
  if aexception.ClassName='EIdConnClosedGracefully'then
  begin
    // not needed to report..
  end else
  if (aexception.classname='EIdSocketError') then
  begin
    // hmm... how to deal with these?
  end else
  begin
    addtolog( addr+':'+inttostr(port)+'> '+aexception.Message);
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_ftpserver._Execute(AThread: TIdPeerThread);
begin
  // does nothing ..
end;

//------------------------------------------------------------------------------
procedure TAPI_ftpserver._GetCustomListFormat(ASender: TIdFTPServer; AItem: TIdFTPListItem; var VText: String);
begin
  // does nothing ..
end;

//------------------------------------------------------------------------------
procedure TAPI_ftpserver._GetFileSize(ASender: TIdFTPServerThread; const AFilename: String; var VFileSize: Int64);
var
  addr: string;
  port: integer;
  path: string;
begin
  addr:= asender.Connection.Socket.Binding.PeerIP;
  port:= asender.connection.socket.Binding.PeerPort;
  path:= buildfilepath( asender.Username, asender.Password, asender.HomeDir, asender.CurrentDir, afilename );
  if fileexists( path ) then
  begin
    try
      with tfilestream.Create( path, fmOpenRead, fmShareDenyNone ) do
      try
        addtolog( addr+':'+inttostr(port)+', '+asender.username+'> Get file size '+afilename);
      finally
        free;
      end;
    except
      raise exception.Create( addr+':'+inttostr(port)+', '+asender.Username+'> Failed to get file size '+afilename);
    end;
  end else
    raise exception.create( addr+':'+inttostr(port)+', '+asender.Username+'> Get file size, file does not exists '+afilename);
end;

//------------------------------------------------------------------------------
procedure TAPI_ftpserver._ListDirectory(ASender: TIdFTPServerThread; const APath: String; ADirectoryListing: TIdFTPListItems);
var
  addr: string;
  port: integer;
  path: string;
  item: tidftplistitem;
  srec: tsearchrec;
  i: integer;
  sl: tstringlist;
begin
  addr:= asender.Connection.Socket.Binding.PeerIP;
  port:= asender.connection.socket.Binding.PeerPort;
  path:= buildfilepath( asender.Username, asender.Password, asender.HomeDir, asender.CurrentDir, apath );
  if directoryexists( path ) then
  begin
    // check for normal files..
    if findfirst( path+'*', faAnyfile, srec) = 0 then
    begin
      repeat
        if not ((apath='/') and ((srec.name='.') or (srec.name='..'))) then
        begin
          item:= adirectorylisting.Add;
          item.FileName:= srec.Name;
          item.OwnerPermissions:= '---';
          item.GroupPermissions:= '---';
          item.UserPermissions:= '---';
          item.OwnerName:= asender.Username;
          item.GroupName:= 'ftp';
          item.Size:= srec.Size;
          item.ModifiedDate:= filedatetodatetime( srec.Time );
          if srec.Attr and faDirectory <> 0 then item.ItemType:= ditDirectory
            else item.ItemType:= ditFile;
        end;
      until findnext( srec )<>0;
      findclose( srec );
    end;
    // add virtual directories if in home dir (root)
    if ansisametext( path, asender.HomeDir ) then
    begin
      fusers.lock.Acquire;
      try
        i:= userindex( asender.Username, asender.Password );
        if (i>-1) and (fusers.data[i].virtualdirs<>'') then
        begin
          sl:= tstringlist.create;
          try
            sl.CommaText:= fusers.data[i].virtualdirs;
            //addtolog('debug: virtual dirs added = '+sl.commatext);
            for i:=0 to sl.Count-1 do
            begin
              item:= adirectorylisting.Add;
              item.FileName:= sl.names[i];
              item.OwnerPermissions:= '---';
              item.GroupPermissions:= '---';
              item.UserPermissions:= '---';
              item.OwnerName:= asender.username;
              item.GroupName:= 'Virtual';
              item.ItemType:= ditDirectory;
              item.ModifiedDate:= now;
            end;
          finally
            sl.free;
          end;
        end;
      finally
        fusers.lock.release;
      end;
      // log message
      addtolog( addr+':'+inttostr(port)+', '+asender.Username+'> List directory '+apath);
    end;
  end else
    raise exception.create( addr+':'+inttostr(port)+', '+asender.username+'> List directory does not exist '+apath);
end;

//------------------------------------------------------------------------------
procedure TAPI_ftpserver._ListenException(AThread: TIdListenerThread; AException: Exception);
begin
  // addtolog( 'Listen error '+aexception.Message);
end;

//------------------------------------------------------------------------------
procedure TAPI_ftpserver._MakeDirectory(ASender: TIdFTPServerThread; var VDirectory: String);
var
  addr: string;
  port: integer;
  path: string;
  i: integer;
  allo: boolean;
begin
  addr:= asender.Connection.Socket.Binding.PeerIP;
  port:= asender.connection.socket.Binding.PeerPort;
  path:= buildfilepath( asender.Username, asender.Password, asender.homedir, asender.CurrentDir, vdirectory );
  allo:= false;
  fusers.lock.Acquire;
  try
    i:= userindex( asender.username, asender.password );
    if i>-1 then allo:= fusers.data[i].allow_mkdir;
  finally
    fusers.lock.release;
  end;
  if allo then
  begin
    if not directoryexists( path ) then
    begin
      if forcedirectories( path ) then addtolog( addr+':'+inttostr(port)+', '+asender.Username+'> Make directory '+vdirectory)
        else raise exception.Create( addr+':'+inttostr(port)+', '+asender.username+'> Failed to make directory '+vdirectory);
    end else
      raise exception.Create( addr+':'+inttostr(port)+', '+asender.Username+'> Directory to make already exists '+vdirectory);
  end else
    raise exception.create( addr+':'+inttostr(port)+', '+asender.username+'> Not allowed to make directory');
end;

//------------------------------------------------------------------------------
procedure TAPI_ftpserver._NoCommandHandler(ASender: TIdTCPServer; const AData: String; AThread: TIdPeerThread);
var
  addr: string;
  port: integer;
begin
  addr:= athread.Connection.Socket.Binding.PeerIP;
  port:= athread.connection.socket.Binding.PeerPort;
  raise exception.create( addr+':'+inttostr(port)+'> No command handler for '+adata);
end;

//------------------------------------------------------------------------------
procedure TAPI_ftpserver._PASV(ASender: TIdFTPServerThread; var VIP: String; var VPort: Word);
begin
  // does nothing..
end;

//------------------------------------------------------------------------------
procedure TAPI_ftpserver._RemoveDirectory(ASender: TIdFTPServerThread; var VDirectory: String);
var
  addr: string;
  port: integer;
  path: string;
  i: integer;
  allo: boolean;
begin
  addr:= asender.Connection.Socket.Binding.PeerIP;
  port:= asender.connection.socket.Binding.PeerPort;
  path:= buildfilepath( asender.Username, asender.Password, asender.homedir, asender.CurrentDir, vdirectory );
  allo:= false;
  fusers.lock.Acquire;
  try
    i:= userindex( asender.username, asender.password );
    if i>-1 then allo:= fusers.data[i].allow_rmdir;
  finally
    fusers.lock.release;
  end;
  if allo then
  begin
    if removedir( path ) then
    begin
      addtolog( addr+':'+inttostr(port)+', '+asender.username+'> Remove directory '+vdirectory);
    end else
      raise exception.Create( addr+':'+inttostr(port)+', '+asender.username+'> Failed to remove directory '+vdirectory);
  end else
    raise exception.create( addr+':'+inttostr(port)+', '+asender.username+'> Not allowed to remove directory');
end;

//------------------------------------------------------------------------------
procedure TAPI_ftpserver._RenameFile(ASender: TIdFTPServerThread; const ARenameFromFile, ARenameToFile: String);
var
  addr: string;
  port: integer;
  path1: string;
  path2: string;
  i: integer;
  allo: boolean;
begin
  addr:= asender.Connection.Socket.Binding.PeerIP;
  port:= asender.connection.socket.Binding.PeerPort;
  path1:= buildfilepath( asender.Username, asender.Password, asender.homedir, asender.CurrentDir, arenamefromfile );
  path2:= buildfilepath( asender.Username, asender.Password, asender.homedir, asender.CurrentDir, arenametofile );
  allo:= false;
  fusers.lock.Acquire;
  try
    i:= userindex( asender.username, asender.password );
    if i>-1 then allo:= fusers.data[i].allow_rename;
  finally
    fusers.lock.release;
  end;
  if allo then
  begin
    if renamefile( path1, path2 ) then
    begin
      addtolog( addr+':'+inttostr(port)+', '+asender.username+'> Rename '+arenamefromfile+' to '+arenametofile);
    end else
      raise exception.Create( addr+':'+inttostr(port)+', '+asender.Username+'> Failed to rename '+arenamefromfile+' to '+arenametofile);
  end else
    raise exception.create( addr+':'+inttostr(port)+', '+asender.username+'> Not allowed to rename.');
end;

//------------------------------------------------------------------------------
procedure TAPI_ftpserver._RetrieveFile(ASender: TIdFTPServerThread; const AFileName: String; var VStream: TStream);
var
  addr: string;
  port: integer;
  path: string;
  i: integer;
  allo: boolean;
begin
  addr:= asender.Connection.Socket.Binding.PeerIP;
  port:= asender.connection.socket.Binding.PeerPort;
  path:= buildfilepath( asender.Username, asender.Password, asender.HomeDir, asender.CurrentDir, afilename );
  allo:= false;
  fusers.lock.Acquire;
  try
    i:= userindex( asender.username, asender.password );
    if i>-1 then allo:= fusers.data[i].allow_get;
  finally
    fusers.lock.release;
  end;
  if allo then
  begin
    if fileexists( path ) then
    begin
      try
        vstream:= tftp_filestream.Create( path, fmOpenRead, fmShareDenyWrite, getreadspeed( asender.username, asender.password ) );
        tftp_filestream(vstream).fonprogress:= _streamprogress;
        tftp_filestream(vstream).fonstop:= _streamstop;
        addtolog( addr+':'+inttostr(port)+', '+asender.username+'> Read file '+afilename);
      except
        raise exception.Create( addr+':'+inttostr(port)+', '+asender.username+'> File to read is not available '+AFilename);
      end;
    end else
      raise exception.create( addr+':'+inttostr(port)+', '+asender.username+'> File to read does not exist '+afilename);
  end else
    raise exception.create( addr+':'+inttostr(port)+', '+asender.username+'> Not allowed to read files.');
end;

//------------------------------------------------------------------------------
procedure TAPI_ftpserver._Status(ASender: TObject; const AStatus: TIdStatus; const AStatusText: String);
begin
  // does nothing..
end;

//------------------------------------------------------------------------------
procedure TAPI_ftpserver._StoreFile(ASender: TIdFTPServerThread; const AFileName: String; AAppend: Boolean; var VStream: TStream);
var
  addr: string;
  port: integer;
  path: string;
  i: integer;
  allo: boolean;
begin
  addr:= asender.Connection.Socket.Binding.PeerIP;
  port:= asender.connection.socket.Binding.PeerPort;
  path:= buildfilepath( asender.Username, asender.Password, asender.HomeDir, asender.CurrentDir, afilename );
  allo:= false;
  fusers.lock.Acquire;
  try
    i:= userindex( asender.username, asender.password );
    if i>-1 then allo:= fusers.data[i].allow_mkdir;
  finally
    fusers.lock.release;
  end;
  if allo then
  begin
    if aappend then
    begin
      try
        VStream := tftp_filestream.create( path, fmOpenWrite, fmShareExclusive, getwritespeed( asender.username, asender.Password ) );
        VStream.Seek( 0, soFromEnd );
        tftp_filestream(vstream).fonprogress:= _streamprogress;
        tftp_filestream(vstream).fonstop:= _streamstop;
        addtolog( addr+':'+inttostr(port)+', '+asender.username+'> Append file '+Afilename );
      except
        vstream:= nil;
        raise exception.Create( addr+':'+inttostr(port)+', '+asender.username+'> Append file failed '+AFilename );
      end;
    end else
    begin
      try
        VStream := tftp_filestream.Create( path, fmCreate, fmShareExclusive, getwritespeed( asender.username, asender.Password ) );
        VStream.seek( 0, soFromBeginning );
        tftp_filestream(vstream).fonprogress:= _streamprogress;
        tftp_filestream(vstream).fonstop:= _streamstop;
        addtolog( addr+':'+inttostr(port)+', '+asender.username+'> Write file '+Afilename );
      except
        vstream:= nil;
        raise exception.Create( addr+':'+inttostr(port)+', '+asender.username+'> Failed to write file '+afilename);
      end;
    end;
  end else
    raise exception.create( addr+':'+inttostr(port)+', '+asender.username+'> Not allowed to write files.');
end;

//------------------------------------------------------------------------------
procedure TAPI_ftpserver._UserLogin(ASender: TIdFTPServerThread; const AUsername, APassword: String; var AAuthenticated: Boolean);
var
  addr: string;
  port: integer;
  user: string;
  pass: string;
  home: string;
  curr: string;
  virt: string;
  auth: boolean;
  rspeed: integer;
  wspeed: integer;
  achdir: boolean;
  amkdir: boolean;
  armdir: boolean;
  arename: boolean;
  adelete: boolean;
  aget: boolean;
  aput: boolean;
  ahidden: boolean;
begin
  user:= ausername;
  pass:= apassword;

  // check if member exists and put the default in..
  if getmember( user, pass, addr, port, home, virt, rspeed, wspeed, achdir, amkdir, armdir, arename, adelete, aget, aput, ahidden ) then
  begin
    addr:= asender.Connection.Socket.Binding.PeerIP;
    port:= asender.connection.socket.Binding.PeerPort;
    auth:= true;
  end else
  begin
    addr:= asender.Connection.Socket.Binding.PeerIP;
    port:= asender.connection.socket.Binding.PeerPort;
    home:= '';
    curr:= '/';
    virt:= '';
    rspeed:= 0;
    wspeed:= 0;
    achdir:= true;
    amkdir:= true;
    armdir:= false;
    arename:= true;
    adelete:= false;
    aget:= true;
    aput:= true;
    ahidden:= false;
    auth:= false;
  end;

  // check users opinion if assigned
  if assigned( fonlogin ) then
    fonlogin( self, addr, port, user, pass, home, curr, virt, auth, rspeed, wspeed, achdir, amkdir, armdir, arename, adelete, aget, aput, ahidden );

  // what was the result?
  if auth then
  begin
    // just curious.. try to add as new member first
    if not addmember( addr, port, user, pass, home, virt, rspeed, wspeed, achdir, amkdir, armdir, arename, adelete, aget, aput, ahidden ) then
      // then we modify..
      if not modmember( user, pass, addr, port, user, pass, home, virt, rspeed, wspeed, achdir, amkdir, armdir, arename, adelete, aget, aput, ahidden ) then
      begin
        // all failed, but what'a hell!
      end;

    addtolog( addr+':'+inttostr(port)+'> Authenticated as '+user+'.' );
    aauthenticated:= true;
    asender.HomeDir:= home;
    asender.CurrentDir:= curr;
    fstat.lock.Acquire;
    try
      fstat.sessions:= fstat.sessions + 1;
    finally
      fstat.lock.Release;
    end;
    adduser( addr, port, user, pass, home, curr, virt, rspeed, wspeed, achdir, amkdir, armdir, arename, adelete, aget, aput, ahidden );
  end else
  begin
    addtolog( addr+':'+inttostr(port)+'> Failed to authenticate as '+user+'.' );
    aauthenticated:= false;
    asender.homedir:= '';
    asender.currentdir:= '';
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_ftpserver._StreamProgress( const filename: string; const pos, size: int64; elapsed: longint; var id: integer );
var
  i: integer;
  j: integer;
begin
  ftransfers.lock.Acquire;
  try
    j:=-1;
    for i:=0 to ftransfers.count-1 do
      if ftransfers.data[i].id = id then
      begin
        j:= i;
        ftransfers.data[i].size:= size;
        ftransfers.data[i].pos:= pos;
        ftransfers.data[i].elapsed:= elapsed;
        break;
      end;
    if j<0 then
    begin
      ftransfers.count:= ftransfers.count + 1;
      setlength( ftransfers.data, ftransfers.count );
      ftransfers.data[ ftransfers.count-1 ].id:= id;
      ftransfers.Data[ ftransfers.Count-1 ].filename:= filename;
      ftransfers.data[ ftransfers.count-1 ].size:= size;
      ftransfers.data[ ftransfers.count-1 ].pos:= pos;
      ftransfers.data[ ftransfers.count-1 ].elapsed:= elapsed;
      //addtolog('debug: started file = '+filename+', id = '+inttostr(id));
    end;
  finally
    ftransfers.lock.Release;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_ftpserver._StreamStop( const filename: string; const pos, size: int64; const elapsed: longint; const bytesread, byteswritten: int64;  var id: integer );
var
  i: integer;
  j: integer;
begin
  ftransfers.lock.Acquire;
  try
    //addtolog('debug: finished file = '+filename+', size = '+inttostr(size)+', elapsed = '+inttostr(elapsed));
    for i:=0 to ftransfers.count-1 do
      if ftransfers.data[i].id = id then
      begin
        ftransfers.data[i].size:= size;
        ftransfers.data[i].pos:= pos;
        ftransfers.data[i].elapsed:= elapsed;
        //addtolog('debug: file = '+ftransfers.data[i].filename+', size = '+inttostr(ftransfers.data[i].size)+', speed = '+inttostr(ftransfers.data[i].speed));
        for j:= i to ftransfers.count-2 do
          ftransfers.data[j]:= ftransfers.data[j+1];
        ftransfers.count:= ftransfers.count - 1;
        setlength( ftransfers.data, ftransfers.count );
        break;
      end;
    // update stats
    fstat.lock.Acquire;
    try
      fstat.bytesread:= fstat.bytesread + bytesread;
      fstat.byteswritten:= fstat.byteswritten + byteswritten;
    finally
      fstat.lock.Release;
    end;
    // event
    if assigned( faftertransfer ) then
      faftertransfer( self, filename, size, elapsed );
  finally
    ftransfers.lock.Release;
  end;
end;

//------------------------------------------------------------------------------
constructor tftp_filestream.Create(const FileName: string; Mode: Word; Rights: Cardinal; LimitBaudrate: longint );
begin
  inherited create( filename, mode, rights );
  fbytespersecond:= limitbaudrate;
  ffilename:= filename;
  fstarttime:= idglobal.GetTickCount;
  fbytesread:= 0;
  fbyteswritten:= 0;
  randomize;
  fid:= random( 2000000 ); // hopefully unique :=)
  if assigned( fonstart ) then
    fonstart( ffilename, self.Position, self.Size, fid );
end;

//------------------------------------------------------------------------------
function tftp_filestream.Read(var Buffer; Count: Longint): longint;
var
  t1: longint;
  t2: longint;
  wt: longint;
begin
  if fbytespersecond>0 then
  begin
    t1:= idglobal.GetTickCount;
    result:= inherited read( buffer, count );
    t2:= idglobal.GetTickCount;
    if result>0 then
    begin
      wt:= longint(1000*result) div fbytespersecond - (t2-t1);
      idglobal.Sleep( wt );
    end;
  end else
    result:= inherited read( buffer, count );
  fbytesread:= fbytesread + result;
  if assigned( fonprogress ) then
    fonprogress( ffilename, self.Position, self.Size, idglobal.GetTickDiff( fstarttime, idglobal.GetTickCount ), fid);
end;

//------------------------------------------------------------------------------
function tftp_filestream.Write(const Buffer; Count: Longint): Longint;
var
  t1: longint;
  t2: longint;
  wt: longint;
begin
  if fbytespersecond>0 then
  begin
    t1:= idglobal.GetTickCount;
    result:= inherited write( buffer, count );
    t2:= idglobal.GetTickCount;
    if result>0 then
    begin
      wt:= (1000*result) div fbytespersecond - (t2-t1);
      idglobal.Sleep( wt );
    end;
  end else
    result:= inherited write( buffer, count );
  fbyteswritten:= fbyteswritten + result;
  if assigned( fonprogress ) then
    fonprogress( ffilename, self.Position, self.Size, idglobal.GetTickDiff( fstarttime, idglobal.GetTickCount ), fid);
end;

//------------------------------------------------------------------------------
destructor tftp_filestream.Destroy;
begin
  if assigned( fonstop ) then
    fonstop( ffilename, self.Position, self.Size, idglobal.GetTickDiff( fstarttime, idglobal.GetTickCount ), fbytesread, fbyteswritten, fid);
  inherited destroy;
end;

end.
