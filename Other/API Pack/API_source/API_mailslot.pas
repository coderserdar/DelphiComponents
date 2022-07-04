unit API_mailslot;

//------------------------------------------------------------------------------
// component free to use and modify, subject to following restinctions:
// 1. do not mispresent the origin
// 2. altered revisions must be clearly marked as modified from the original
// 3. do not remove this notice from the source code
// 4. send email about bugs, features needed and features you would like
// * if you like this very much, feel free to donate and support supporting
// and developing the package at www.paypal.com - ari pikivirta@kolumbus.fi
//------------------------------------------------------------------------------
// component revision history
// r1.01, ari pikivirta
// * added more comments into the source
// r1.02, ari pikivirta
// * added nick names and leave events & messages

interface

uses
  Windows, Messages, SysUtils, Classes;

const
  content_LEAVE: string = 'CMD_LEAVE';
  content_MSG: string = 'CMD_MESSAGE';

type
  // mailslot contents
  TAPI_mailslot = class;

  tnewmessage = procedure(
    sender: tobject;
    Const Origin,
    Nick,
    Date,
    Time,
    Text: string) of Object;

  tleavemessage = procedure(
    sender: tobject;
    Const Origin,
    Nick,
    Date,
    Time,
    Text: string) of Object;

  // mail checking thread
  tcheckthread = class (tthread)
  private
    mailslot: tAPI_mailslot;  protected
    procedure Execute; override;
  end;

  // mailslot component
  tAPI_mailslot = class(tcomponent)
  private
    fversion: string;
    fmessageheader: string;
    fmailbox: string;
    factive: boolean;
    checkthread: tcheckthread;
    localpath: string;
    localhandle: thandle;
    remotepath: string;
    remotehandle: thandle;
    fcheckinterval: integer;
    messagesize: dword;
    messagecount: dword;
    inmessage: tstringlist;
    outmessage: tstringlist;
    fcomputer: string;
    checkthreadrunning: boolean;
    fthreadpriority: tthreadpriority;
    fnick: string;

    // events
    fnewmessage: tnewmessage;
    fleavemessage: tleavemessage;

    procedure startthread;
    procedure setactive(b:boolean);
    procedure readmessage;
    function  mailstrings(recipient, mailbox: string): boolean;
    procedure setmessageheader(value: string);
    procedure setmailbox(value: string);
    procedure setthreadpriority(value: tthreadpriority);

    procedure dummys(s: string);

  protected
  public
    constructor Create(AOwner: tcomponent); override;
    destructor Destroy; override;

    // private messaging
    function sendmessage(Const recipient, mailbox, text: string): boolean;
    function sendmessage_leave(Const recipient, mailbox, text: string): boolean;

    // broadcasted
    function broadcast(Const text, mailbox: string): boolean;
    function broadcast_leave(Const text, mailbox: string): boolean;

    procedure changemailbox(Const mailbox: string);

  published
    property Version: string read fversion write dummys stored false;
    property Computer: string read fcomputer write dummys stored false;
    property Nick: string read fnick write fnick;
    property Header: string read fmessageheader write setmessageheader;
    property Mailbox: string read fmailbox write setmailbox;
    property Active: boolean read factive write setactive;
    property CheckInterval: integer read fcheckinterval write fcheckinterval default 1000;
    property Priority: tthreadpriority read fthreadpriority write setthreadpriority default tpnormal;
    property onNewMessage: tnewmessage read fnewmessage write fnewmessage;
    property onLeaveMessage: tleavemessage read fleavemessage write fleavemessage;

  end;

procedure Register;

implementation

const
  versioninfo = 'r1.02/ari.pikivirta@kolumbus.fi';

{$include '..\API_source\inc\CompilerVersions.INC'}
{$R *.RES}

procedure TAPI_mailslot.dummys(s: string); begin end;

//-----------------------------------------------------------
procedure tcheckthread.execute;
var
  threadwaitinterval: integer;
begin
  threadwaitinterval:=mailslot.fcheckinterval;
  mailslot.checkthreadrunning:=true;

  while not terminated do
  begin
    getmailslotinfo(mailslot.localhandle,nil,mailslot.messagesize,@mailslot.messagecount,nil);

    // check if there is new mail to read
    if mailslot.messagecount>0 then
      synchronize(mailslot.readmessage);
    sleep(threadwaitinterval);
  end;

  mailslot.checkthreadrunning:=false;
end;

//-----------------------------------------------------------
constructor tAPI_mailslot.create(AOwner: TComponent);
var
  {$ifdef DELPHI2009UP}
  temp: pwidechar;
  {$else}
  temp: pchar;
  {$endif}
  size: dword;
begin
  inherited;
  fversion:=versioninfo;
  fmessageheader:='Message';
  fmailbox:='MailBox';
  fnick:='unknown';
  factive:=false;
  fcheckinterval:=1000;
  fthreadpriority:=tpnormal;
  outmessage:=tstringlist.Create;
  inmessage:=Tstringlist.Create;
  size:=255;
  temp:= '';
  getcomputername(temp, size);
  fcomputer:= strpas(temp);
  checkthread:=tcheckthread.Create(true);
  checkthread.mailslot:=self;
  checkthreadrunning:=false;
end;

//-----------------------------------------------------------
destructor tAPI_mailslot.destroy;
begin
  if active then active:=false;
  inmessage.Free;
  outmessage.Free;
  checkthread.Free;
  inherited;
end;

//-----------------------------------------------------------
function tAPI_mailslot.sendmessage(Const recipient, mailbox, text: string): boolean;
begin
  inmessage.add(fmessageheader);
  inmessage.add(content_msg);
  inmessage.add(datetostr(now));
  inmessage.add(timetostr(now));
  inmessage.add(fnick);
  inmessage.add(fcomputer);
  inmessage.add(text);

  // send message
  result:= mailstrings(recipient,mailbox);
end;

//-----------------------------------------------------------
function tAPI_mailslot.sendmessage_leave(Const recipient, mailbox, text: string): boolean;
begin
  inmessage.add(fmessageheader);
  inmessage.add(content_leave);
  inmessage.add(datetostr(now));
  inmessage.add(timetostr(now));
  inmessage.add(fnick);
  inmessage.add(fcomputer);
  inmessage.add(text);

  // send message
  result:= mailstrings(recipient,mailbox);
end;

//-----------------------------------------------------------
function tAPI_mailslot.broadcast(Const text,mailbox:string): boolean;
begin
  inmessage.add(fmessageheader);
  inmessage.add(content_msg);
  inmessage.add(datetostr(now));
  inmessage.add(timetostr(now));
  inmessage.add(fnick);
  inmessage.add(fcomputer);
  inmessage.add(text);

  // send message
  result:= mailstrings('*',mailbox);
end;

//-----------------------------------------------------------
function tAPI_mailslot.broadcast_leave(Const text, mailbox: string): boolean;
begin
  inmessage.add(fmessageheader);
  inmessage.add(content_leave);
  inmessage.add(datetostr(now));
  inmessage.add(timetostr(now));
  inmessage.add(fnick);
  inmessage.add(fcomputer);
  inmessage.add(text);

  // send message
  result:= mailstrings('*',mailbox);
end;

//-----------------------------------------------------------
function tAPI_mailslot.mailstrings(recipient, mailbox: string): boolean;
var
  bytes: dword;
begin
  result:=false;
  remotepath:='\\'+recipient+'\mailslot\'+mailbox;

  // get handle for message file
  remotehandle:=createfile(
    pchar(remotepath),
    generic_write,
    file_share_read,
    nil,
    create_always,
    file_attribute_normal,
    0);
  if remotehandle=invalid_handle_value then
    exit;

  // write file and free handle
  writefile(
    remotehandle,
    pointer(inmessage.text)^,
    length(inmessage.text),
    bytes,
    nil);
  closehandle(remotehandle);

  // delete message from buffer
  inmessage.Clear;
  //if bytes<>sizeof(inmessage.text) then exit;
  result:=true;
end;

//-----------------------------------------------------------
procedure tAPI_mailslot.setactive(b:boolean);
begin
  if factive<>b then
  begin
    factive:=b;
    if factive then
    begin
      if checkthreadrunning then exit;
      startthread;
    end else
    begin
      if not checkthreadrunning then exit;
      checkthread.Terminate;
      checkthread.WaitFor;
      closehandle(localhandle);
    end;
  end;
end;

//-----------------------------------------------------------
procedure tAPI_mailslot.changemailbox(Const mailbox: string);
begin
  setmailbox(mailbox);
end;

//-----------------------------------------------------------
procedure tAPI_mailslot.startthread;
begin
  // create local mailbox
  localpath:='\\.\mailslot\'+fmailbox;
  localhandle:=createmailslot(
    pchar(localpath),
    0,
    0,
    nil);
  if localhandle=invalid_handle_value then
  begin
    factive:=false;
  end else
  begin
    checkthread.Priority:= fthreadpriority;
    checkthread.Resume;
  end;
end;

//-----------------------------------------------------------
procedure tAPI_mailslot.readmessage;
var
  temptext: string;
  tempheader: string;
  tempcomputer: string;
  tempcontent: string;
  tempnick: string;
  tempdate: string;
  temptime: string;
  bytes: dword;
begin
  setlength(tempcontent,messagesize);
  readfile(
    localhandle,
    pchar(tempcontent)^,
    messagesize,
    bytes,
    nil);
  outmessage.Clear;
  outmessage.Text:=tempcontent;

  if outmessage.count>6 then
  begin

    tempheader:= outmessage[0];
    tempcontent:= outmessage[1];
    tempdate:= outmessage[2];
    temptime:= outmessage[3];
    tempnick:= outmessage[4];
    tempcomputer:= outmessage[5];

    outmessage.Delete(0);
    outmessage.delete(0);
    outmessage.delete(0);
    outmessage.delete(0);
    outmessage.delete(0);
    outmessage.delete(0);

    temptext:=outmessage.Text;
    outmessage.Clear;

    if (tempcontent=content_msg) and (assigned(fnewmessage)) then
    begin
      fnewmessage(
        self,
        tempcomputer,
        tempnick,
        tempdate,
        temptime,
        temptext);
    end else

    if (tempcontent=content_leave) and (assigned(fleavemessage)) then
    begin
      fleavemessage(
        self,
        tempcomputer,
        tempnick,
        tempdate,
        temptime,
        temptext);
    end;
  end;
end;

//-----------------------------------------------------------
procedure tAPI_mailslot.setmessageheader(value: string);
begin
  if fmessageheader<>value then
  begin
    fmessageheader:=Value;
    if factive then
    begin
      active:=false;
      active:=true;
    end;
  end;
end;

//-----------------------------------------------------------
procedure tAPI_mailslot.setmailbox(value: string);
begin
  if fmailbox<>value then
  begin
    fmailbox:=value;
    if factive then
    begin
      active:=false;
      active:=True;
    end;
  end;
end;

//-----------------------------------------------------------
procedure tAPI_mailslot.setthreadpriority(value:tthreadpriority);
begin
  if fthreadpriority<>value then
  begin
    fthreadpriority:=value;
    checkthread.Priority:=fthreadpriority;
  end;
end;

//-----------------------------------------------------------
procedure Register;
begin
  RegisterComponents('API Comm', [TAPI_mailslot]);
end;

end.
