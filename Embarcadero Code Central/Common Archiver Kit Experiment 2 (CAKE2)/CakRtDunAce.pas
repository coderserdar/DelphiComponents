unit CakRtDunAce;

interface

uses
  Windows, Messages, SysUtils, Classes, CakDefs2, RTdunAce, UNACEV2, CakArchiver;

const CanExtract = TRUE;
      CanAdd = FALSE;
      CanList = TRUE;
      CanSFX = FALSE;
      Dllname = 'Unace.dll';
type
  TCakRtDunAce = class(TCakArchiver)
  private
    { Private declarations }
    Stopping : boolean;

  protected
    { Protected declarations }
  public
    { Public declarations }
    AceDir: TdACE;
    Cakdir : TComponent; 
    procedure SetCakdir(pCakdir : TComponent); override;
    procedure Process(dowhat : WorkType); override;
    function Cando(aWork: WorkType): Boolean; override;
    function Def_TreatAs : string; override;
    procedure Load_DLL; override;
    procedure UnLoad_DLL; override;
    function DllExists : Boolean; override;
    procedure DoStop(Stopp: Boolean); override;

    procedure AceDirList(Sender: TObject; eFile: TACEHeaderData; Result: Boolean);
    procedure AceDirError(Sender: TObject; Error: Integer);
    procedure AceDirExtracting(Sender: TObject; eFile: TACEHeaderData);
    function CallAceInitDll: Integer; 
    procedure Ace2HandleError(Error: Integer);

  published
    { Published declarations }
    property Stop: Boolean read Stopping write DoStop;
  end;

procedure Register;

implementation
uses CakUtils2, Cakdir2, Dialogs, Controls, Forms, Graphics;
var Processed_Files: Integer;
    Ace2Msg: String;
    Ace2Code : integer;
    StopProcess : boolean;
    currentArchiver : TCakRtDunAce;
procedure TCakRtDunAce.SetCakdir(pCakdir : TComponent);
begin
    Cakdir := TCakdir2(pCakdir);
end;
procedure TCakRtDunAce.DoStop(Stopp: Boolean);
begin
    AceDir.Stop := Stopp;
    Stopping := stopp;
end;

procedure TCakRtDunAce.Process(dowhat : WorkType);
var 
  i : Integer;
begin
  currentArchiver := Self;
  Load_DLL;
  with (Cakdir as TCakdir2) do
  case doWhat of
    wtLoadContents: 
     begin
        AceDir.Archivefilename := Archivename;
        AceDir.ListArchive;
     end;
    wtExtract:
     begin
        for i := 0 to Total_Contents - 1 do
          if (ExtractOptions.Extr_ExtractAll or Archive_Contents[i]._Selected) then
           begin
            StrCopy(UnaceV2.FileList, PChar(Archive_Contents[i]._FileDefPath +
              Archive_Contents[i]._FileName));
            Ace2Msg := '';
            Ace2HandleError(CallACEExtract(Archivename,
              ExtractOptions.Extr_to,
              Password, not ExtractOptions.Extr_DirNames));
           end;
     end;
    wtTest: begin
            Ace2Msg := '';
            Ace2HandleError(CallACETest(Archivename));
            end;
    else if Assigned(FOnMsg) then
      FOnMsg(NIL, 0, CODE_NOSUPPORT, Msg_Error, ERR_NOSUPPORT);
   end;
end;


function TCakRtDunAce.DllExists : boolean;
begin
        Result := Fileexists(GrabProgrampath+Dllname);
end;

function TCakRtDunAce.Cando(aWork: WorkType): Boolean;
begin
        Result := true;
        Case aWork of
        wtNone : Result := true;
        wtTest, wtExtract : Result := CanExtract;
        wtAdd, wtDelete : Result := CanAdd;
        wtSFX :  Result := CanSFX;
        wtLoadContents: Result := CanList;
        end;
        if Result then
          if not DLLExists then
            Result := false;
end;

function TCakRtDunAce.Def_TreatAs : string;
begin
end;

procedure TCakRtDunAce.AceDirList(Sender: TObject; eFile: TACEHeaderData; Result: Boolean);
var 
  loc:  Integer;
  Ext:  String;
  Icon: TICON;
begin
  with (Cakdir as TCakdir2) do
  begin
  DirectoryList.Clear;
  Icon := TICON.Create;
  try
    with efile do
      AddContents(Archivename,ExtractFilePath(FileName),
      ExtractFileName(FileName),UnpSize,PackSize,False,
      FileDateToDateTime(FileTime),IntToHex(FileCRC, 8)       );
  finally
    Icon.Free;
   end;
   end;
end;

procedure TCakRtDunAce.AceDirError(Sender: TObject; Error: Integer);
begin
  with (Cakdir as TCakdir2) do
  if Assigned(FOnMsg) then
    case Error of
      ACEERR_EXISTS  : FOnMsg(Sender, Error, CODE_EXIST, Msg_Error, ERR_EXIST);
      ACEERR_END,
       UnPack_OK     : FOnMsg(Sender, Error, CODE_NOERROR, Msg_OK, ERR_NOERROR);
      UnPack_Stopped : FOnMsg(Sender, Error, CODE_USERSKIP, Msg_Error, ERR_USERSKIP);
      Unpack_ReadErr : FOnMsg(Sender, Error, CODE_CANTREAD, Msg_Error, ERR_CANTREAD);
      ACEERR_MEM     : FOnMsg(Sender, Error, CODE_NOMEMORY, Msg_Error, ERR_NOMEMORY);
      ACEERR_FILES   : FOnMsg(Sender, Error, CODE_NOTSPECIFY, Msg_Error, ERR_NOTSPECIFY);
      ACEERR_FOUND   : FOnMsg(Sender, Error, CODE_NOTFOUNDARC, Msg_Error, ERR_NOTFOUNDARC);
      ACEERR_FULL    : FOnMsg(Sender, Error, CODE_NODISKSPACE, Msg_Error, ERR_NODISKSPACE);
      ACEERR_OPEN    : FOnMsg(Sender, Error, CODE_LISTERROR, Msg_Error, ERR_LISTERROR);
      ACEERR_READ    : FOnMsg(Sender, Error, CODE_CANTREAD, Msg_Error, ERR_CANTREAD);
      ACEERR_WRITE   : FOnMsg(Sender, Error, CODE_WRITE, Msg_Error, ERR_WRITE);
      ACEERR_CLINE   : FOnMsg(Sender, Error, CODE_DLLERROR, Msg_Error, ERR_DLLERROR);
      ACEERR_CRC     : FOnMsg(Sender, Error, CODE_CRC, Msg_Error, ERR_CRC);
      ACEERR_OTHER   : FOnMsg(Sender, Error, CODE_UNDEFERROR, Msg_Error, ERR_UNDEFERROR);
      ACEERR_NOPASSW : FOnMsg(Sender, Error, CODE_PASSWORD, Msg_Error, ERR_PASSWORD);
      ACEERR_USER    : FOnMsg(Sender, Error, CODE_USERSKIP, Msg_Error, ERR_USERSKIP);
      else
        FOnMsg(Sender, Error, CODE_UNDEFERROR , Msg_Error, ERR_UNDEFERROR);
     end;
end;

procedure TCakRtDunAce.AceDirExtracting(Sender: TObject; eFile: TACEHeaderData);
begin
  Inc(Processed_Files);
  with (Cakdir as TCakdir2) do
  if Assigned(FOnProg) then
    FOnProg(NIL, efile.FileName, efile.UnpSize, efile.UnpSize);
end;



procedure TCakRtDunAce.Ace2HandleError(Error: Integer);
begin
  with (Cakdir as TCakdir2) do
  begin
    if Ace2Msg <> '' then
    if Assigned(FOnMsg) then
     begin
      FOnMsg(NIL, Error, CODE_UNDEFERROR, Msg_Error, Ace2Msg);
      Ace2Msg := '';
      exit;
     end;

    if Assigned(FOnMsg) then
    case Error of
      ACEERR_EXISTS  : FOnMsg(Nil, Error, CODE_EXIST, Msg_Error, ERR_EXIST);
      UnPack_OK      : FOnMsg(Nil, Error, CODE_NOERROR, Msg_OK, ERR_NOERROR);
      UnPack_Stopped : FOnMsg(Nil, Error, CODE_USERSKIP, Msg_Error, ERR_USERSKIP);
      Unpack_ReadErr : FOnMsg(Nil, Error, CODE_CANTREAD, Msg_Error, ERR_CANTREAD);
      ACEERR_MEM     : FOnMsg(Nil, Error, CODE_NOMEMORY, Msg_Error, ERR_NOMEMORY);
      ACEERR_FILES   : FOnMsg(Nil, Error, CODE_NOTSPECIFY, Msg_Error, ERR_NOTSPECIFY);
      ACEERR_FOUND   : FOnMsg(Nil, Error, CODE_NOTFOUNDARC, Msg_Error, ERR_NOTFOUNDARC);
      ACEERR_FULL    : FOnMsg(Nil, Error, CODE_NODISKSPACE, Msg_Error, ERR_NODISKSPACE);
      ACEERR_OPEN    : FOnMsg(Nil, Error, CODE_LISTERROR, Msg_Error, ERR_LISTERROR);
      ACEERR_READ    : FOnMsg(Nil, Error, CODE_CANTREAD, Msg_Error, ERR_CANTREAD);
      ACEERR_WRITE   : FOnMsg(Nil, Error, CODE_WRITE, Msg_Error, ERR_WRITE);
      ACEERR_CLINE   : FOnMsg(Nil, Error, CODE_DLLERROR, Msg_Error, ERR_DLLERROR);
      ACEERR_CRC     : FOnMsg(Nil, Error, CODE_CRC, Msg_Error, ERR_CRC);
      ACEERR_OTHER   : FOnMsg(Nil, Error, CODE_UNDEFERROR, Msg_Error, ERR_UNDEFERROR);
      ACEERR_NOPASSW : FOnMsg(Nil, Error, CODE_PASSWORD, Msg_Error, ERR_PASSWORD);
      ACEERR_USER    : FOnMsg(Nil, Error, CODE_USERSKIP, Msg_Error, ERR_USERSKIP);
      else
        FOnMsg(Nil, Error, CODE_UNDEFERROR , Msg_Error, ERR_UNDEFERROR);
     end;
   end;
end;

procedure Ace2HandleError(Error: Integer);
begin
  with ((CurrentArchiver.Cakdir) as TCakdir2) do
  begin
    if Ace2Msg <> '' then
    if Assigned(FOnMsg) then
     begin
      FOnMsg(NIL, Error, CODE_UNDEFERROR, Msg_Error, Ace2Msg);
      Ace2Msg := '';
      exit;
     end;

    if Assigned(FOnMsg) then
    case Error of
      ACEERR_EXISTS  : FOnMsg(Nil, Error, CODE_EXIST, Msg_Error, ERR_EXIST);
      UnPack_OK      : FOnMsg(Nil, Error, CODE_NOERROR, Msg_OK, ERR_NOERROR);
      UnPack_Stopped : FOnMsg(Nil, Error, CODE_USERSKIP, Msg_Error, ERR_USERSKIP);
      Unpack_ReadErr : FOnMsg(Nil, Error, CODE_CANTREAD, Msg_Error, ERR_CANTREAD);
      ACEERR_MEM     : FOnMsg(Nil, Error, CODE_NOMEMORY, Msg_Error, ERR_NOMEMORY);
      ACEERR_FILES   : FOnMsg(Nil, Error, CODE_NOTSPECIFY, Msg_Error, ERR_NOTSPECIFY);
      ACEERR_FOUND   : FOnMsg(Nil, Error, CODE_NOTFOUNDARC, Msg_Error, ERR_NOTFOUNDARC);
      ACEERR_FULL    : FOnMsg(Nil, Error, CODE_NODISKSPACE, Msg_Error, ERR_NODISKSPACE);
      ACEERR_OPEN    : FOnMsg(Nil, Error, CODE_LISTERROR, Msg_Error, ERR_LISTERROR);
      ACEERR_READ    : FOnMsg(Nil, Error, CODE_CANTREAD, Msg_Error, ERR_CANTREAD);
      ACEERR_WRITE   : FOnMsg(Nil, Error, CODE_WRITE, Msg_Error, ERR_WRITE);
      ACEERR_CLINE   : FOnMsg(Nil, Error, CODE_DLLERROR, Msg_Error, ERR_DLLERROR);
      ACEERR_CRC     : FOnMsg(Nil, Error, CODE_CRC, Msg_Error, ERR_CRC);
      ACEERR_OTHER   : FOnMsg(Nil, Error, CODE_UNDEFERROR, Msg_Error, ERR_UNDEFERROR);
      ACEERR_NOPASSW : FOnMsg(Nil, Error, CODE_PASSWORD, Msg_Error, ERR_PASSWORD);
      ACEERR_USER    : FOnMsg(Nil, Error, CODE_USERSKIP, Msg_Error, ERR_USERSKIP);
      else
        FOnMsg(Nil, Error, CODE_UNDEFERROR , Msg_Error, ERR_UNDEFERROR);
     end;
   end;
end;


procedure Ace2ErrorMsg(acode: Integer; amessage: string);
begin
  if amessage <> '' then
   begin
    Ace2Msg  := amessage;
    Ace2Code := acode;
   end;
end;

procedure Ace2Progress(FileSize, TotalSize: Integer);
begin
end;

function Ace2InfoProc(Info: pACEInfoCallbackProcStruc): Integer;
var
  InfoStr: String;
begin
  case Info^.Global.Code of
    ACE_CALLBACK_INFO_FILELISTCREATE:
     begin
      InfoStr := 'Creating file list';
     end;
    ACE_CALLBACK_INFO_FILELISTCREATEEND:
      InfoStr := 'Finished creating file list';
    ACE_CALLBACK_INFO_FILELISTADD:
      InfoStr := 'adding file to file list';
    else
      InfoStr := '';
   end;
  Result := ACE_CALLBACK_RETURN_OK;
end;

function Ace2HandleErrorGlobal(Error: pACECallbackGlobalStruc): Integer;
var
  ErrorStr: String;
begin
  Result := ACE_CALLBACK_RETURN_OK;

  case Error^.Code of
    ACE_CALLBACK_ERROR_MEMORY:
      ErrorStr := 'not enough memory';
    ACE_CALLBACK_ERROR_UNCSPACE:
      ErrorStr := 'could not detect available space on network drive';
    else
     begin
      ErrorStr := 'unknown';
      Result   := ACE_CALLBACK_RETURN_CANCEL;
     end;
   end;

   Ace2Msg := ErrorStr;
   Ace2HandleError(Error^.Code);
   //MessageDlg('Error: ' + Errorstr, mtError, [mbOK], 0);
end;

function Ace2HandleErrorArchive(Error: pACECallbackArchiveStruc): Integer;
var
  ErrorStr: String;
begin
  Result := ACE_CALLBACK_RETURN_OK;
  case Error^.Code of
    ACE_CALLBACK_ERROR_AV:
      ErrorStr := 'AV of archive %s invalid';
    ACE_CALLBACK_ERROR_OPENARCHIVEREAD:
      ErrorStr := 'could not open archive %s for reading';
    ACE_CALLBACK_ERROR_READARCHIVE:
      ErrorStr := 'error reading from archive %s';
    ACE_CALLBACK_ERROR_ARCHIVEBROKEN:
      ErrorStr := 'archive %s is broken';
    ACE_CALLBACK_ERROR_NOFILES:
      ErrorStr := 'no files specified';
    ACE_CALLBACK_ERROR_ISNOTANARCHIVE:
      ErrorStr := 'file is not an ACE archive';
    ACE_CALLBACK_ERROR_HIGHERVERSION:
      ErrorStr := 'this Dll version is not able to handle the archive';
    else
     begin
      ErrorStr := 'unknown';
      Result   := ACE_CALLBACK_RETURN_CANCEL;
     end;
   end;
   Ace2Msg := ErrorStr;
   Ace2HandleError(Error^.Code);
    //MessageDlg(ErrorStr + Error^.ArchiveData^.ArchiveName, mtError, [mbOK], 0);
end;

function Ace2HandleErrorArchivedFile(Error: pACECallbackArchivedFileStruc): Integer;
var
  ErrorStr: String;
begin
  Result := ACE_CALLBACK_RETURN_OK;
  case Error^.Code of
    ACE_CALLBACK_ERROR_CREATIONNAMEINUSE:
      ErrorStr := 'could not extract %s: name used by directory';
    ACE_CALLBACK_ERROR_WRITE:
      ErrorStr := 'error writing %s';
    ACE_CALLBACK_ERROR_OPENWRITE:
      ErrorStr := 'error opening %s for writing';
    ACE_CALLBACK_ERROR_METHOD:
      ErrorStr := 'compression method not known to this Dll version';
    ACE_CALLBACK_ERROR_EXTRACTSPACE:
      ErrorStr := 'not enough space to extract %s';
    ACE_CALLBACK_ERROR_CREATION:
      ErrorStr := 'creation of %s failed (write-protection?)';
    else
     begin
      ErrorStr := 'unknown';
      Result   := ACE_CALLBACK_RETURN_CANCEL;
     end;
   end;
   Ace2Msg := ErrorStr;
   Ace2HandleError(Error^.Code);
    //MessageDlg(ErrorStr + Error^.FileData^.SourceFileName, mtError, [mbOK], 0);
end;

function Ace2HandleErrorRealFile(Error: pACECallbackRealFileStruc): Integer;
var
  ErrorStr: String;
begin
  ErrorStr := 'unknown';
  Result   := ACE_CALLBACK_RETURN_CANCEL;
  Ace2Msg := ErrorStr;
  Ace2HandleError(CODE_UNDEFERROR);
  //MessageDlg(ErrorStr + Error^.FileName, mtError, [mbOK], 0);
end;

function Ace2HandleErrorSpace(Error: pACECallbackSpaceStruc): Integer;
var
  ErrorStr: String;
begin
  ErrorStr := 'unknown';
  Result   := ACE_CALLBACK_RETURN_CANCEL;
  Ace2Msg := ErrorStr;
  Ace2HandleError(CODE_UNDEFERROR);
  //MessageDlg(ErrorStr + Error^.Directory, mtError, [mbOK], 0);
end;

function Ace2HandleErrorSFXFile(Error: pACECallbackSFXFileStruc): Integer;
var
  ErrorStr: String;
begin
  ErrorStr := 'unknown';
  Result   := ACE_CALLBACK_RETURN_CANCEL;
  Ace2Msg := ErrorStr;
  Ace2HandleError(CODE_UNDEFERROR);
  //MessageDlg(ErrorStr + Error^.SFXFileName, mtError, [mbOK], 0);
end;

function Ace2ErrorProc(Error: pACEErrorCallbackProcStruc): Integer;
begin
  //ShowMessage('ErrorProc');
  case Error^.StructureType of
    ACE_CALLBACK_TYPE_GLOBAL:
      Result := Ace2HandleErrorGlobal(@Error^.Global);
    ACE_CALLBACK_TYPE_ARCHIVE:
      Result := Ace2HandleErrorArchive(@Error^.Archive);
    ACE_CALLBACK_TYPE_ARCHIVEDFILE:
      Result := Ace2HandleErrorArchivedFile(@Error^.ArchivedFile);
    ACE_CALLBACK_TYPE_REALFILE:
      Result := Ace2HandleErrorRealFile(@Error^.RealFile);
    ACE_CALLBACK_TYPE_SPACE:
      Result := Ace2HandleErrorSpace(@Error^.Space);
    ACE_CALLBACK_TYPE_SFXFILE:
      Result := Ace2HandleErrorSFXFile(@Error^.SFXFile);
    else
      Result := ACE_CALLBACK_RETURN_CANCEL;
   end;
end;

function Ace2HandleRequestGlobal(Request: pACECallbackGlobalStruc): Integer;
begin
  MessageDlg('unknown request', mtError, [mbOK], 0);
  Result := ACE_CALLBACK_RETURN_CANCEL;
end;

function Ace2HandleRequestArchive(Request: pACECallbackArchiveStruc): Integer;
var
  RequestStr: String;
begin
  case Request^.Code of
    ACE_CALLBACK_REQUEST_CHANGEVOLUME:
      RequestStr := 'ready to process next volume'
      else
       begin
        MessageDlg('unknown request', mtError, [mbOK], 0);
        Result := ACE_CALLBACK_RETURN_CANCEL;
        Exit;
       end;
   end;
  if MessageDlg(RequestStr, mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    Result := 1
  else
    Result := 0; // False
end;

function Ace2HandleRequestArchivedFile(Request: pACECallbackArchivedFileStruc): Integer;
var
  RequestStr: String;
begin
  case Request^.Code of
    ACE_CALLBACK_REQUEST_OVERWRITE:
      RequestStr := 'overwrite existing file ' + Request^.FileData^.SourceFileName;

    ACE_CALLBACK_REQUEST_PASSWORD:
     begin
      RequestStr := Request^.FileData^.SourceFileName +
        ' is encrypted, using "testpassword" as password';
      Request^.GlobalData^.DecryptPassword := 'testpassword';
     end
    else
     begin
      MessageDlg('unknown request', mtError, [mbOK], 0);
      Result := ACE_CALLBACK_RETURN_CANCEL;
      Exit;
     end
   end;
  if MessageDlg(RequestStr, mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    Result := ACE_CALLBACK_RETURN_OK
  else
    Result := ACE_CALLBACK_RETURN_NO; // False
end;

function Ace2HandleRequestRealFile(Request: pACECallbackRealFileStruc): Integer;
begin
  MessageDlg('unknown request', mtError, [mbOK], 0);
  Result := ACE_CALLBACK_RETURN_CANCEL;
end;

function Ace2RequestProc(Request: pACERequestCallbackProcStruc): Integer;
begin
  case Request^.StructureType of
    ACE_CALLBACK_TYPE_GLOBAL:
      Result := Ace2HandleRequestGlobal(@Request^.Global);
    ACE_CALLBACK_TYPE_ARCHIVE:
      Result := Ace2HandleRequestArchive(@Request^.Archive);
    ACE_CALLBACK_TYPE_ARCHIVEDFILE:
      Result := Ace2HandleRequestArchivedFile(@Request^.ArchivedFile);
    ACE_CALLBACK_TYPE_REALFILE:
      Result := Ace2HandleRequestRealFile(@Request^.RealFile);
    else
      Result := ACE_CALLBACK_RETURN_CANCEL;
   end;
end;

function Ace2HandleStateStartArchive(Archive: pACECallbackArchiveStruc): Integer;
var
  ActionStr: String;
begin
  case Archive^.Operation of
    ACE_CALLBACK_OPERATION_LIST:
      ActionStr := 'Listing ' + Archive^.ArchiveData^.ArchiveName;
    ACE_CALLBACK_OPERATION_TEST:
      ActionStr := 'Testing ' + Archive^.ArchiveData^.ArchiveName;
    ACE_CALLBACK_OPERATION_EXTRACT:
      ActionStr := 'Extracting ' + Archive^.ArchiveData^.ArchiveName;
    else
      ActionStr := 'unknown operation on ' + Archive^.ArchiveData^.ArchiveName;
   end;

  Result := ACE_CALLBACK_RETURN_OK;
end;

function Ace2HandleStateStartFile(ArchivedFile: pACECallbackArchivedFileStruc): Integer;
var
  ActionStr: String;
begin
  case ArchivedFile^.Operation of
    ACE_CALLBACK_OPERATION_LIST:
     begin
      ActionStr := 'Found';
     end;
    ACE_CALLBACK_OPERATION_TEST:
      ActionStr := 'Testing';
    ACE_CALLBACK_OPERATION_ANALYZE:
      ActionStr := 'Analyzing';
    ACE_CALLBACK_OPERATION_EXTRACT:
     begin
      ActionStr := 'Extracting';
      Ace2ErrorMsg(0, ActionStr + ' ' + ArchivedFile^.FileData^.SourceFileName);
      //Form1.Gauge1.MaxValue:=ArchivedFile^.FileData^.Size;
     end;
    else
      ActionStr := 'unknown operation on';
   end;

  Result := ACE_CALLBACK_RETURN_OK;
end;

procedure Ace2DisplayProgress(FileProcessedSize,
  FileSize,
  TotalProcessedSize,
  TotalSize: Int64);


var
  //s          : string;
  lKBWritten: Int64;
begin
  // Display/calculate progress for ACE extracting
  Application.ProcessMessages;
  lKBWritten := TotalProcessedSize;

  Ace2Progress(lKBwritten, TotalSize);

  Application.ProcessMessages;
end; // AceDisplayProgress

function Ace2StateProc(State: pACEStateCallbackProcStruc): Integer;
begin
  if StopProcess then
   begin
    Result := ACE_CALLBACK_RETURN_CANCEL;
    Exit;
   end;

  case State^.StructureType of
    ACE_CALLBACK_TYPE_ARCHIVE:
     begin
      if (State^.Archive.Code = ACE_CALLBACK_STATE_STARTARCHIVE) and
        (State^.Archive.Operation = ACE_CALLBACK_OPERATION_EXTRACT) then
       begin
        //        frmUnpack.lblCurrentFile.Caption:=State^.Archive.ArchiveData^.ArchiveName;
        // nixe
       end;
     end;
    ACE_CALLBACK_TYPE_ARCHIVEDFILE:
     begin
      case State^.ArchivedFile.Code of
        ACE_CALLBACK_STATE_STARTFILE:
         begin
          Result := Ace2HandleStateStartFile(@State^.ArchivedFile);
          Exit;
         end;
        ACE_CALLBACK_STATE_ENDNOCRCCHECK:
         begin
         end;
       end;
     end;
    ACE_CALLBACK_TYPE_PROGRESS:
     begin
      if State^.Progress.Code = ACE_CALLBACK_STATE_PROGRESS then
       begin
        Ace2DisplayProgress(State^.Progress.ProgressData^.FileProcessedSize,
          State^.Progress.ProgressData^.FileSize,
          State^.Progress.ProgressData^.TotalProcessedSize,
          State^.Progress.ProgressData^.TotalSize);

        // nixe
        //      ShowMessage('nixe    processed: ' + IntToStr(State^.Progress.ProgressData^.FileProcessedSize) +
        //                    ' of ' + IntToStr(State^.Progress.ProgressData^.FileSize)  +
        //                    ' bytes (' + IntToStr(State^.Progress.ProgressData^.TotalProcessedSize) +
        //                    ' of ' + IntToStr(State^.Progress.ProgressData^.TotalSize) + ' bytes)');
       end;
     end;
    ACE_CALLBACK_TYPE_CRCCHECK:
     begin
      if State^.CRCCheck.Code = ACE_CALLBACK_STATE_ENDCRCCHECK then
       begin
        if not State^.CRCCheck.CRCOk then
          MessageDlg('CRC-check error', mtError, [mbOK], 0);
       end;
     end;
   end;

  Result := ACE_CALLBACK_RETURN_OK;
end;

function TCakRtDunAce.CallAceInitDll: Integer;
var
  DllData:  tACEInitDllStruc;
  zTempDir: array[0..255] of Char;
begin
  FillChar(DllData, SizeOf(DllData), 0);
  DllData.GlobalData.MaxArchiveTestBytes := $1ffFF;
  DllData.GlobalData.MaxFileBufSize      := $2ffFF;
  DllData.GlobalData.Comment.BufSize     := SizeOf(CommentBuf) - 1;
  DllData.GlobalData.Comment.Buf         := @CommentBuf;

  GetTempPath(255, @zTempDir);
  DllData.GlobalData.TempDir := @zTempDir;

  DllData.GlobalData.InfoCallbackProc    := @Ace2InfoProc;
  DllData.GlobalData.ErrorCallbackProc   := @Ace2ErrorProc;
  DllData.GlobalData.RequestCallbackProc := @Ace2RequestProc;
  DllData.GlobalData.StateCallbackProc   := @Ace2StateProc;
  
  Result := ACEInitDll(@DllData);
end;

procedure TCakRtDunAce.Load_DLL;
var
  i: Integer;
begin
  if not Assigned(AceDir) then
    AceDir := TdAce.Create(self);
  AceDir.Path2UnAceDll := ExtractFilePath(ParamStr(0));
  AceDir.OnList       := AceDirList;
  AceDir.OnError      := AceDirError;
  AceDir.OnExtracting := AceDirExtracting;
  if LoadAceDll('') then
   begin
    i := CallAceInitDll;
    if i <> 0 then
      Ace2ErrorMsg(0, 'Unable to initialize unace2.dll. Error code: ' + IntToStr(i));
   end
  else
    Ace2ErrorMsg(0, 'Unable to load unace2.dll!');
end;


procedure TCakRtDunAce.UnLoad_DLL;
begin
  if not Assigned(AceDir) then Exit;
  AceDir.OnList       := NIL;
  AceDir.OnError      := NIL;
  AceDir.OnExtracting := NIL;
  UnLoadAceDll
end;


procedure Register;
begin
  //RegisterComponents('QZip', [TCakRtDunAce]);
end;

end.
