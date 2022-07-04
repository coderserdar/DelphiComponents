unit API_eMail;

//------------------------------------------------------------------------------
// mail component which handles all copies, multiple recipients and
// accepts attachment files  
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
// r1.06, ari pikivirta
//  * added NEW_SESSIOn to on not showing without mapi login enabled
//
// r1.05, ari pikivirta
//  * removed autologon property since there was no use for that
//  * fixed possible memory leak with the file attachment display names list
//  * added events for error and success on sending
//
// r1.04, ari pikivirta
//  * fixed bug with filling stringlists directly at object inspector
//
// r1.03, ari pikivirta
// * added mailto as stringlist
// * added carbon coby and blind copy features
//
// r1.02, ari pikivirta
// * removed unnecessary sub (set)procedures
//
// r1.01, ari pikivirta
// * updated email information in version string
//
//------------------------------------------------------------------------------

interface

uses
  Windows, Messages, SysUtils, Classes, Forms, API_base;

type
  TMapiErrEvent = procedure(Sender: TObject; ErrCode: Integer) of object;
  (*
    Our own:
      MAPI_MODULE_MISSING = -2
    Mapi results.. (Mapi.H)
      MAPI_E_FAILURE = 2;
      MAPI_E_LOGON_FAILURE = 3;
      MAPI_E_LOGIN_FAILURE = MAPI_E_LOGON_FAILURE;
      MAPI_E_DISK_FULL = 4;
      MAPI_E_INSUFFICIENT_MEMORY = 5;
      MAPI_E_ACCESS_DENIED = 6;
      MAPI_E_TOO_MANY_SESSIONS = 8;
      MAPI_E_TOO_MANY_FILES = 9;
      MAPI_E_TOO_MANY_RECIPIENTS = 10;
      MAPI_E_ATTACHMENT_NOT_FOUND = 11;
      MAPI_E_ATTACHMENT_OPEN_FAILURE = 12;
      MAPI_E_ATTACHMENT_WRITE_FAILURE = 13;
      MAPI_E_UNKNOWN_RECIPIENT = 14;
      MAPI_E_BAD_RECIPTYPE = 15;
      MAPI_E_NO_MESSAGES = 16;
      MAPI_E_INVALID_MESSAGE = 17;
      MAPI_E_TEXT_TOO_LARGE = 18;
      MAPI_E_INVALID_SESSION = 19;
      MAPI_E_TYPE_NOT_SUPPORTED = 20;
      MAPI_E_AMBIGUOUS_RECIPIENT = 21;
      MAPI_E_AMBIG_RECIP = MAPI_E_AMBIGUOUS_RECIPIENT;
      MAPI_E_MESSAGE_IN_USE = 22;
      MAPI_E_NETWORK_FAILURE = 23;
      MAPI_E_INVALID_EDITFIELDS = 24;
      MAPI_E_INVALID_RECIPS = 25;
      MAPI_E_NOT_SUPPORTED = 26;
  *)

  TAPI_eMail = class(TAPI_Custom_Component)
  private
    fsubject: Ansistring;
    fmailto: tstringlist;
    fcopyto: tstringlist;
    fblindcopyto: tstringlist;
    ffromname: Ansistring;
    ffromemail: Ansistring;
    fshowonsend: boolean;
    fbody: tstringlist;
    fattachments: tstringlist;
    fonsend: tnotifyevent;
    FOnUserAbort: TNotifyEvent;
    FOnMapiError: TMapiErrEvent;
    FOnSuccess: TNotifyEvent;
    fprofilename: Ansistring;
    fpassword: Ansistring;
    fmapilogon: boolean;
    procedure SetMailTo(sl: tstringlist);
    procedure SetCopyTo(sl: tstringlist);
    procedure SetBlindCopyTo(sl: tstringlist);
    procedure SetBody(sl: tstringlist);
    procedure SetAttachments(sl: tstringlist);

  protected
  public
    constructor Create(aowner:tcomponent); override;
    destructor Destroy; override;
    function  GetDefaultProfile: string;
    function  SendEMail(Handle: THandle): boolean;

  published
    property Subject: Ansistring read fsubject write fsubject;
    property MailTo: tstringlist read fmailto write setmailto;
    property CopyTo: tstringlist read fcopyto write setcopyto;
    property BlindCopyTo: tstringlist read fblindcopyto write setblindcopyto;
    property FromName: Ansistring read ffromname write ffromname;
    property FromEmail: Ansistring read ffromemail write ffromemail;
    property Body: tstringlist read fbody write setbody;
    property Attachments: tstringlist read fattachments write setattachments;
    property Profilename: Ansistring read fprofilename write fprofilename;
    property Password: Ansistring read fpassword write fpassword;
    property ShowOnSend: boolean read fshowonsend write fshowonsend;
    property OnSend: tnotifyevent read fonsend write fonsend;
    property OnUserAbort: TNotifyEvent read FOnUserAbort write FOnUserAbort;
    property OnMapiError: TMapiErrEvent read FOnMapiError write FOnMapiError;
    property OnSuccess: TNotifyEvent read FOnSuccess write FOnSuccess;
    property LogonToMapi: boolean read fmapilogon write fmapilogon;

  end;

procedure Register;

implementation

{$include '..\API_source\inc\CompilerVersions.INC'}
{$r *.res}

uses
  mapi, registry;

const
  versioninfo = 'r1.07/ari.pikivirta@kolumbus.fi';

//------------------------------------------------------------------------------
function TAPI_email.GetDefaultProfile: string;

  // we need to check if NT platform or not
  function isNT: boolean;
  Var
    OS : TOSVERSIONINFO;
  Begin
    OS.dwOSVersionInfoSize := SizeOf(TOSVERSIONINFO);
    GetVersionEx(OS);
    Result := (os.dwPlatformId = VER_PLATFORM_WIN32_NT);
  End;

var
  reg: tregistry;
begin
  result:='';
  reg:=tregistry.Create(KEY_ALL_ACCESS);
  try
    if isNT then
    begin
      if reg.OpenKeyReadOnly('Software\Microsoft\Windows NT\CurrentVersion\Windows Messaging Subsystem\Profiles') then
        result:=reg.ReadString('DefaultProfile');
    end else
    begin
      if reg.OpenKeyReadOnly('Software\Microsoft\Windows Messaging Subsystem\Profiles') then
        result:=reg.ReadString('DefaultProfile');
    end;
  finally
    reg.Free;
  end;
end;

//------------------------------------------------------------------------------
function TAPI_email.sendemail(Handle: THandle): boolean;
var
  mapisession: longint;
  mapimessage: tmapimessage;
  precip, recipients: pmapirecipdesc;
  pfiles, attachments: pmapifiledesc;
  sender: tmapirecipdesc;
  i: integer;
  MAPIModule: HModule;
  res: integer;
begin
  PFiles := nil;
  result:= false;

  if (fmailto.Count<0) and (fcopyto.count<1) and (fblindcopyto.count<1) then exit;
  FillChar(MapiMessage, SizeOf(MapiMessage), #0);

  // recerve memory
  MapiMessage.nRecipCount:= fmailto.count + fcopyto.count + fblindcopyto.Count;
  GetMem(recipients, MapiMessage.nRecipCount * sizeof(TMapiRecipDesc));
  try

  // using mapi message
  with mapimessage do
  begin

    // set contents
    lpszSubject:= PAnsiChar(fsubject);                  // SUBJECT
    lpszNoteText:= PAnsiChar(ansistring(fbody.Text));               // BODY

    // miscellous flags
    lpszMessageType:= nil;
    lpszDateReceived:= nil;
    lpszConversationID:= nil;
    flFlags:= 0;

    // sender
    Sender.ulReserved:= 0;
    Sender.ulRecipClass:= MAPI_ORIG;
    Sender.lpszName:= PansiChar(ffromname);             // SENDER NAME
    Sender.lpszAddress:= PansiChar('SMTP:'+ffromemail);         // SENDER EMAIL
    Sender.ulEIDSize:= 0;
    Sender.lpEntryID:= nil;
    lpOriginator:= @Sender;

    // recipients
    precip:= recipients;                            // set recipients
    if nrecipcount>0 then
    begin
      for i:=0 to fmailto.Count-1 do                // MAILTO
      begin
        pRecip^.ulReserved:= 0;
        pRecip^.ulRecipClass:= MAPI_TO;
        pRecip^.lpszName:= StrNew(PAnsiChar(ansistring(fmailto[i])));
        pRecip^.lpszAddress:= StrNew(PansiChar('SMTP:'+ansistring(fmailto[i])));
        pRecip^.ulEIDSize:= 0;
        pRecip^.lpEntryID:= nil;
        inc(precip);
      end;
      for i:=0 to fcopyto.count-1 do                // COPY TO
      begin
        pRecip^.ulReserved:= 0;
        pRecip^.ulRecipClass:= MAPI_CC;
        pRecip^.lpszName:= StrNew(PansiChar(ansistring(fmailto[i])));
        pRecip^.lpszAddress:= StrNew(PansiChar('SMTP:'+ansistring(fmailto[i])));
        pRecip^.ulEIDSize:= 0;
        pRecip^.lpEntryID:= nil;
        inc(precip);
      end;
      for i:=0 to fblindcopyto.count-1 do           // BLIND COPY TO
      begin
        pRecip^.ulReserved:= 0;
        pRecip^.ulRecipClass:= MAPI_BCC;
        pRecip^.lpszName:= StrNew(PansiChar(ansistring(fmailto[i])));
        pRecip^.lpszAddress:= StrNew(PansiChar('SMTP:'+ansistring(fmailto[i])));
        pRecip^.ulEIDSize:= 0;
        pRecip^.lpEntryID:= nil;
        inc(precip);
      end;
    end;
    lpRecips:=recipients;

    // attachments
    nFileCount:=fattachments.Count;
    if nFileCount>0 then
    begin
      GetMem(Attachments, nFileCount * sizeof(TMapiFileDesc));
      PFiles:= Attachments;
      for i:=0 to fattachments.count-1 do
      begin
        {$ifdef DELPHI2009UP}
        Attachments^.lpszPathName:= PansiChar(ansistring(fattachments[i]));
        Attachments^.lpszFileName:= PansiChar(ansistring(extractfilename(fattachments[i])));
        {$else}
        Attachments^.lpszPathName:= PChar(fattachments[i]);
        Attachments^.lpszFileName:= PChar(extractfilename(fattachments[i]));
        {$endif}
        Attachments^.ulReserved:= 0;
        Attachments^.flFlags:= 0;
        Attachments^.nPosition:= Cardinal(-1);
        Attachments^.lpFileType:= nil;
        Inc(Attachments);
      end;
      lpFiles:= PFiles;
    end else
    begin
      nFileCount := 0;
      lpFiles := nil;
    end;
  end;

  // fire on send mail event
  if assigned(fonsend) then fonsend(self);

  // send using mapi library
  MAPIModule:= LoadLibrary(PChar(MAPIDLL));
  if MAPIModule<>0 then
  try
    if (fmapilogon) then
    begin
      // try start mapi session alongs profile
      res:= MapiLogOn(0, pansichar(fprofilename), pansichar(fpassword), MAPI_NEW_SESSION, 0, @MapiSession);
      if (res=SUCCESS_SUCCESS) then
      try
        if fshowonsend then
          res:= MapiSendMail(MapiSession, handle, MapiMessage, MAPI_DIALOG or MAPI_LOGON_UI or MAPI_NEW_SESSION, 0)
          else res:= MapiSendMail(MapiSession, Handle, MapiMessage, MAPI_NEW_SESSION, 0);
      finally
        // log off mapi session
        MapiLogOff(MapiSession, 0, 0, 0);
      end;
    end else
    begin
      // send without mapi login
      if fshowonsend then
        res:= MapiSendMail(0, handle, MapiMessage, MAPI_DIALOG or MAPI_LOGON_UI or MAPI_NEW_SESSION, 0)
        else res:= MapiSendMail(0, Handle, MapiMessage, MAPI_NEW_SESSION, 0);
    end;
  finally
    FreeLibrary(MAPIModule);
  end else
    res:= -2;

  // manage result of above
  case res of
    SUCCESS_SUCCESS:
      begin
        result:= true;
        if assigned(onsuccess) then OnSuccess(self);
      end;
    MAPI_E_USER_ABORT:
      if Assigned(FOnUserAbort) then FOnUserAbort(Self);
    else
      if Assigned(FOnMapiError) then FOnMapiError(Self, res);
  end;

  // finally
  finally
    // free mem
    PRecip:= Recipients;
    for i:= 1 to MapiMessage.nRecipCount do
    begin
      StrDispose(PRecip^.lpszAddress);
      inc(PRecip);
    end;
    FreeMem(Recipients, MapiMessage.nRecipCount * sizeof(TMapiRecipDesc));
    if Assigned(PFiles) then
      FreeMem(PFiles, MapiMessage.nFileCount * sizeof(TMapiFileDesc));
  end;
end;

//------------------------------------------------------------------------------
constructor tAPI_email.create(aowner:tcomponent);
begin
  inherited create(aowner);
  version:= versioninfo;
  fbody:= tstringlist.create;
  fattachments:= tstringlist.create;
  fattachments.Clear;
  fmailto:= tstringlist.create;
  fmailto.Clear;
  fcopyto:= tstringlist.create;
  fcopyto.Clear;
  fblindcopyto:= tstringlist.create;
  fblindcopyto.clear;
  fshowonsend:= false;
  fsubject:= '';
  fmapilogon:= true;
  //profilename:= getdefaultprofile;
end;

//------------------------------------------------------------------------------
destructor tAPI_email.destroy;
begin
  fbody.free;
  fattachments.Free;
  fmailto.Free;
  fcopyto.free;
  fblindcopyto.free;
  inherited destroy;
end;

procedure Tapi_email.SetMailTo(sl: tstringlist);
begin
  fmailto.Text:= sl.Text;
end;

procedure TAPI_email.SetCopyTo(sl: tstringlist);
begin
  fcopyto.Text:= sl.Text;
end;

procedure TAPI_email.SetBlindCopyTo(sl: tstringlist);
begin
  fblindcopyto.Text:= sl.Text;
end;

procedure TAPI_email.SetBody(sl: tstringlist);
begin
  fbody.Text:= sl.text;
end;

procedure TAPI_email.SetAttachments(sl: tstringlist);
begin
  fattachments.Text:= sl.text;
end;

//------------------------------------------------------------------------------
procedure Register;
begin
  RegisterComponents('API Comm', [TAPI_eMail]);
end;

end.
