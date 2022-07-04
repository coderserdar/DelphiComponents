{ Copyright (C) 1998-2008, written by Mike Shkolnik, Scalabium Software
  E-Mail:  mshkolnik@scalabium.com
  WEB: http://www.scalabium.com
}
unit SendMail;

interface

{$IFDEF VER200}
  {$DEFINE SMForDelphi3}
  {$DEFINE SMForDelphi4}
  {$DEFINE SMForDelphi5}
  {$DEFINE SMForDelphi6}
  {$DEFINE SMForDelphi7}
  {$DEFINE SMForDelphi2005}
  {$DEFINE SMForDelphi2006}
  {$IFDEF BCB}
    {$DEFINE SMForBCB2006}
    {$DEFINE SMForBCB2007}
    {$DEFINE SMForBCB2009}
  {$ENDIF}
  {$DEFINE SMForDelphi2007}
  {$DEFINE SMForRADStudio2007}
  {$DEFINE SMForDelphi2009}
{$ENDIF}

uses
  Classes, Dialogs;

type
  TMAPIMail = class(TComponent)
  private
    { Private declarations }
    FLastError: Integer;

    FSubject: string;
    FBody: string;

    FSenderName: string;
    FSenderAddress: string;

    FRecipients: TStrings;
    FAttachments: TStrings;
    FAttachmentNames: TStrings;

    FEditDialog: Boolean;
    FResolveNames: Boolean;
    FRequestReceipt: Boolean;

    procedure SetRecipients(Value: TStrings);
    procedure SetAttachments(Value: TStrings);
    procedure SetAttachmentNames(Value: TStrings);
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function Send: Boolean;

    property LastError: Integer read FLastError;
  published
    { Published declarations }
    property Subject: string read FSubject write FSubject;
    property Body: string read FBody write FBody;

    property Recipients: TStrings read FRecipients write SetRecipients;
    property Attachments: TStrings read FAttachments write SetAttachments;
    property AttachmentNames: TStrings read FAttachmentNames write SetAttachmentNames;

    property EditDialog: Boolean read FEditDialog write FEditDialog;
    property ResolveNames: Boolean read FResolveNames write FResolveNames;
    property RequestReceipt: Boolean read FRequestReceipt write FRequestReceipt;
    
    property SenderName: string read FSenderName write FSenderName;
    property SenderAddress: string read FSenderAddress write FSenderAddress;
  end;

function SendEMailByMAPI(SenderName, SenderAddress, Subject, Body: string; Recipients, Attachments, AttachmentNames: TStrings; WithOpenMessage, ResolveNames, NeedReceipt: Boolean; intMAPISession: Integer): Integer;
function MAPIErrorDescription(intErrorCode: Integer): string;

procedure Register;

implementation
uses Windows, SysUtils, MAPI, Registry, Forms;

procedure Register;
begin
  RegisterComponents('SMComponents', [TMAPIMail]);
end;

function MAPIErrorDescription(intErrorCode: Integer): string;
begin
   case intErrorCode of
     MAPI_E_USER_ABORT: Result := 'User cancelled request';
     MAPI_E_FAILURE: Result := 'General MAPI failure';
     MAPI_E_LOGON_FAILURE: Result := 'Logon failure';
     MAPI_E_DISK_FULL: Result := 'Disk full';
     MAPI_E_INSUFFICIENT_MEMORY: Result := 'Insufficient memory'; 
     MAPI_E_ACCESS_DENIED: Result := 'Access denied'; 
     MAPI_E_TOO_MANY_SESSIONS: Result := 'Too many sessions'; 
     MAPI_E_TOO_MANY_FILES: Result := 'Too many files open'; 
     MAPI_E_TOO_MANY_RECIPIENTS: Result := 'Too many recipients'; 
     MAPI_E_ATTACHMENT_NOT_FOUND: Result := 'Attachment not found'; 
     MAPI_E_ATTACHMENT_OPEN_FAILURE: Result := 'Failed to open attachment'; 
     MAPI_E_ATTACHMENT_WRITE_FAILURE: Result := 'Failed to write attachment';
     MAPI_E_UNKNOWN_RECIPIENT: Result := 'Unknown recipient'; 
     MAPI_E_BAD_RECIPTYPE: Result := 'Invalid recipient type'; 
     MAPI_E_NO_MESSAGES: Result := 'No messages'; 
     MAPI_E_INVALID_MESSAGE: Result := 'Invalid message'; 
     MAPI_E_TEXT_TOO_LARGE: Result := 'Text too large.';
     MAPI_E_INVALID_SESSION: Result := 'Invalid session'; 
     MAPI_E_TYPE_NOT_SUPPORTED: Result := 'Type not supported'; 
     MAPI_E_AMBIGUOUS_RECIPIENT: Result := 'Ambiguous recipient'; 
     MAPI_E_MESSAGE_IN_USE: Result := 'Message in use'; 
     MAPI_E_NETWORK_FAILURE: Result := 'Network failure'; 
     MAPI_E_INVALID_EDITFIELDS: Result := 'Invalid edit fields'; 
     MAPI_E_INVALID_RECIPS: Result := 'Invalid recipients'; 
     MAPI_E_NOT_SUPPORTED: Result := 'Not supported'; 
   else
     Result := 'Unknown Error Code: ' + IntToStr(intErrorCode);
   end; 
end; 

function GetDefaultLogon(var strDefaultLogon: string): Boolean;
const
  KEYNAME1 = 'Software\Microsoft\Windows Messaging Subsystem\Profiles';
  KEYNAME2 = 'Software\Microsoft\Windows NT\CurrentVersion\Windows Messaging Subsystem\Profiles';
  VALUESTR = 'DefaultProfile';
begin
  Result := False;
  strDefaultLogon := '';
  with TRegistry.Create do
    try
      RootKey := HKEY_CURRENT_USER;
      if OpenKey(KEYNAME1, False) then
      begin
        try
          strDefaultLogon := ReadString(VALUESTR);
          Result := True;
        except
        end;
        CloseKey;
      end
      else
      if OpenKey(KEYNAME2, False) then
      begin
        try
          strDefaultLogon := ReadString(VALUESTR);
          Result := True;
        except
        end;
        CloseKey;
      end
      else
    finally
      Free;
    end;
end;

function SendEMailByMAPI(SenderName, SenderAddress, Subject, Body: string; Recipients, Attachments, AttachmentNames: TStrings; WithOpenMessage, ResolveNames, NeedReceipt: Boolean; intMAPISession: Integer): Integer;
const
  RECIP_MAX  = MaxInt div SizeOf(TMapiRecipDesc);
  ATTACH_MAX = MaxInt div SizeOf(TMapiFileDesc);
type
  TRecipAccessArray = array [0..(RECIP_MAX - 1)] of TMapiRecipDesc;
  TlpRecipArray     = ^TRecipAccessArray;

  TAttachAccessArray = array [0..(ATTACH_MAX - 1)] of TMapiFileDesc;
  TlpAttachArray     = ^TAttachAccessArray;

  TszRecipName   = array[0..256] of Char;
  TlpszRecipName = ^TszRecipName;

  TszPathName   = array[0..256] of Char;
  TlpszPathname = ^TszPathname;

  TszFileName   = array[0..256] of Char;
  TlpszFileName = ^TszFileName;

var
  i: Integer;

  Message: TMapiMessage;
  lpRecipArray: TlpRecipArray;
  lpAttachArray: TlpAttachArray;


  function CheckRecipient(strRecipient: string): Integer;
  var
    lpRecip: PMapiRecipDesc;
  begin
    try
      Result := MapiResolveName(0, 0, PAnsiChar(strRecipient), 0, 0, lpRecip);
      if (Result in [MAPI_E_AMBIGUOUS_RECIPIENT,
                     MAPI_E_UNKNOWN_RECIPIENT]) then
        Result := MapiResolveName(0, 0, PAnsiChar(strRecipient), MAPI_DIALOG, 0, lpRecip);
      if Result = SUCCESS_SUCCESS then
      begin
        strRecipient := StrPas(lpRecip^.lpszName);
        with lpRecipArray^[i] do
        begin
          {$IFDEF SMForDelphi2009}
          {$ELSE}
          lpszName := StrCopy(new(TlpszRecipName)^, lpRecip^.lpszName);
          if lpRecip^.lpszAddress = nil then
            lpszAddress := StrCopy(new(TlpszRecipName)^, lpRecip^.lpszName)
          else
            lpszAddress := StrCopy(new(TlpszRecipName)^, lpRecip^.lpszAddress);
          {$ENDIF}
          ulEIDSize := lpRecip^.ulEIDSize;
          lpEntryID := lpRecip^.lpEntryID;
          MapiFreeBuffer(lpRecip);
        end
      end;
    finally
    end;
  end;

  function SendMess: Integer;
  const
    arrMAPIFlag: array[Boolean] of Word = (0, MAPI_DIALOG);
    arrReceipt: array[Boolean] of Word = (0, MAPI_RECEIPT_REQUESTED);
    arrLogon: array[Boolean] of Word = (0, MAPI_LOGON_UI or MAPI_NEW_SESSION);
  begin
    try
      Result := MAPISendMail(0, Application.Handle{0}, Message,
                    arrReceipt[NeedReceipt] or
                    arrMAPIFlag[WithOpenMessage] or
                    MAPI_LOGON_UI {or MAPI_NEW_SESSION} or
                    arrLogon[{True}intMAPISession = 0],
                    0);
    finally
    end;
  end;

var
  lpSender: TMapiRecipDesc;
  strDefaultProfile, s: string;
begin
  lpAttachArray := nil;
  Result := 0;
  FillChar(Message, SizeOf(Message), 0);
  with Message do
  begin
    strDefaultProfile := '';
    if GetDefaultLogon(strDefaultProfile) then
    begin
      try
        { try to logon with default profile }
        Result := MapiLogOn(0, PAnsiChar(strDefaultProfile), nil, MAPI_NEW_SESSION, 0, @intMAPISession);
      finally
        if (Result <> SUCCESS_SUCCESS) then
        begin
          intMAPISession := 0;

          raise Exception.CreateFmt('MAPI Error %d: %s', [Result, MAPIErrorDescription(Result)]);
        end;
      end
    end;


    if (SenderAddress <> '') then
    begin
      lpSender.ulRecipClass := MAPI_ORIG;
      if (SenderName <> '') then
        lpSender.lpszName := PAnsiChar(SenderAddress)
      else
        lpSender.lpszName := PAnsiChar(SenderName);
      lpSender.lpszAddress := PAnsiChar(SenderAddress);
      lpSender.ulReserved := 0;
      lpSender.ulEIDSize := 0;
      lpSender.lpEntryID := nil;
      lpOriginator := @lpSender;
    end;

    lpszSubject := PAnsiChar(Subject);
    lpszNoteText := PAnsiChar(Body);

    if Assigned(Attachments) and (Attachments.Count > 0) then
    begin
      nFileCount := Attachments.Count;

      lpAttachArray := TlpAttachArray(StrAlloc(nFileCount*SizeOf(TMapiFileDesc)));
      FillChar(lpAttachArray^, StrBufSize(PChar(lpAttachArray)), 0);
      for i := 0 to nFileCount-1 do
      begin
        lpAttachArray^[i].nPosition := Cardinal(-1); //Cardinal($FFFFFFFF); //ULONG(-1);
        {$IFDEF SMForDelphi2009}
        {$ELSE}
        lpAttachArray^[i].lpszPathName := StrPCopy(new(TlpszPathname)^, Attachments[i]);
        if i < AttachmentNames.Count then
          lpAttachArray^[i].lpszFileName := StrPCopy(new(TlpszFileName)^, AttachmentNames[i])
        else
          lpAttachArray^[i].lpszFileName := StrPCopy(new(TlpszFileName)^, ExtractFileName(Attachments[i]));
        {$ENDIF}
      end;
      lpFiles := @lpAttachArray^
    end
    else
      nFileCount := 0;
  end;


  if Assigned(Recipients) and (Recipients.Count > 0) then
  begin
    lpRecipArray := TlpRecipArray(StrAlloc(Recipients.Count*SizeOf(TMapiRecipDesc)));
    FillChar(lpRecipArray^, StrBufSize(PChar(lpRecipArray)), 0);
    for i := 0 to Recipients.Count-1 do
    begin
      s := Recipients[i];
      if (UpperCase(Copy(s, 1, 3)) = 'CC:') then
      begin
        lpRecipArray^[i].ulRecipClass := MAPI_CC;
        Delete(s, 1, 3);
      end
      else
      if (UpperCase(Copy(s, 1, 4)) = 'BCC:') then
      begin
        lpRecipArray^[i].ulRecipClass := MAPI_BCC;
        Delete(s, 1, 4);
      end
      else
        lpRecipArray^[i].ulRecipClass := MAPI_TO;

      if ResolveNames then
        CheckRecipient(s)
      else
      begin
        {$IFDEF SMForDelphi2009}
        {$ELSE}
        lpRecipArray^[i].lpszName := StrCopy(new(TlpszRecipName)^, PChar(s));
        lpRecipArray^[i].lpszAddress := StrCopy(new(TlpszRecipName)^, PChar(s));
        {$ENDIF}
      end;
    end;
    Message.nRecipCount := Recipients.Count;
    Message.lpRecips := @lpRecipArray^;
  end
  else
    Message.nRecipCount := 0;

  Result := SendMess;

  if Assigned(Attachments) and (Message.nFileCount > 0) then
  begin
    for i := 0 to Message.nFileCount-1 do
    begin
      Dispose(lpAttachArray^[i].lpszPathname);
      Dispose(lpAttachArray^[i].lpszFileName);
    end;
    StrDispose(PChar(lpAttachArray));
  end;

  if Assigned(Recipients) and (Recipients.Count > 0) then
  begin
    for i := 0 to Message.nRecipCount-1 do
    begin
      if Assigned(lpRecipArray^[i].lpszName) then
        Dispose(lpRecipArray^[i].lpszName);

      if Assigned(lpRecipArray^[i].lpszAddress) then
        Dispose(lpRecipArray^[i].lpszAddress);
    end;
    StrDispose(PChar(lpRecipArray));
  end;

  if intMAPISession <> 0 then
    try
      MapiLogOff(intMAPISession, 0, 0, 0);
    except
    end;
end;


{ TMAPIMail }
constructor TMAPIMail.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  EditDialog := True;
  FRecipients := TStringList.Create;
  FAttachments := TStringList.Create;
  FAttachmentNames := TStringList.Create;
end;

destructor TMAPIMail.Destroy;
begin
  FRecipients.Free;
  Attachments.Free;
  AttachmentNames.Free;

  inherited Destroy;
end;

procedure TMAPIMail.SetRecipients(Value: TStrings);
begin
  FRecipients.Assign(Value)
end;

procedure TMAPIMail.SetAttachments(Value: TStrings);
begin
  Attachments.Assign(Value)
end;

procedure TMAPIMail.SetAttachmentNames(Value: TStrings);
begin
  AttachmentNames.Assign(Value)
end;

function TMAPIMail.Send: Boolean;
begin
  FLastError := SendEMailByMAPI(SenderName, SenderAddress, Subject, Body, Recipients, Attachments, AttachmentNames, EditDialog, ResolveNames, RequestReceipt, 0);

  Result := (LastError = SUCCESS_SUCCESS);
end;

end.
