unit CakRtDunRar;

interface

uses
  Windows, Messages, SysUtils, Classes, CakDefs2, RTdunRar, RTdunRar3, CakArchiver;

const CanExtract = TRUE;
      CanAdd = FALSE;
      CanList = TRUE;
      CanSFX = FALSE;
      Dllname = 'unrar.dll';
type
  TCakRtDunRar = class(TCakArchiver)
  private
    { Private declarations }
    Stopping : boolean;

  protected
    { Protected declarations }
  public
    { Public declarations }
    RarDir: TdRar3;
    Cakdir : TComponent;
    procedure SetCakdir(pCakdir : TComponent); override;
    procedure Process(dowhat : WorkType); override;
    function Cando(aWork: WorkType): Boolean; override;
    function Def_TreatAs : string; override;
    procedure Load_DLL; override;
    procedure UnLoad_DLL; override;
    function DllExists : Boolean; override;
    procedure DoStop(Stopp: Boolean); override;

    procedure RarDirList(Sender: TObject; eFile: TRarHeaderData; Result: Boolean);
    procedure RarDirError(Sender: TObject; Error: Integer);
    procedure RarDirExtracting(Sender: TObject; eFile: TRarHeaderData);
    function RarDirVolumeChange(Sender: TObject; ArcName: PChar; Mode: Integer): Integer;
    procedure RarOnReqPassword(Sender: TObject; eFile: TRARHeaderData; var Pass: string);
    procedure Rar3OnReqPassword(Sender: TObject; var Pass: PChar);
    procedure Rar3DirList(Sender: TObject; eFile: TRARHeaderData; OK: Boolean;ResultCode: Integer);
    function Rar3ProcessData(Sender: TObject; Addr: PChar; BlockSize,Position: Integer): Integer;
  published
    { Published declarations }
    property Stop: Boolean read Stopping write DoStop;
  end;

procedure Register;

implementation
uses CakUtils2, Cakdir2, Forms, Graphics;
var Processed_Files : integer;
    Rar_LastProcess: String;
    Rar_FileSize, Rar_Completed: Integer;

procedure TCakRtDunRar.RarDirExtracting(Sender: TObject; eFile: TRARHeaderData);
var
  k:      String;
  buffer: PChar;
begin
  with (Cakdir as TCakdir2) do
  begin
  Inc(Processed_Files);
  Rar_LastProcess := efile.FileName;
  Rar_FileSize    := efile.UnpSize;
  Rar_Completed   := 0;
  if Assigned(FOnProg) then
    FOnProg(NIL, efile.FileName, efile.UnpSize, efile.UnpSize);
  GetMem(Buffer, Length(efile.FileName) + 1);
  k := ExtractFileName(string(efile.FileName));
  StrCopy(Buffer, PChar(k));
  if Assigned(FOnMsg) then
    FOnMsg(Sender, Error, CODE_NOERROR, Msg_OK, ERR_NOERROR);
  FreeMem(Buffer);
  end;
end;

procedure TCakRtDunRar.RarDirError(Sender: TObject; Error: Integer);
begin
  with (Cakdir as TCakdir2) do
  if Assigned(FOnMsg) then
    case Error of
      132: FOnMsg(Sender, Error, CODE_NOSUPPORT, Msg_Error, ERR_NOSUPPORT);
      UnPack_OK,128            : FOnMsg(Sender, Error, CODE_NOERROR, Msg_OK, ERR_NOERROR);
      UnPack_UnRarDllNotLoaded : FOnMsg(Sender, Error, CODE_DLLERROR, Msg_Error, ERR_DLLERROR);
      Unpack_ReadErr : FOnMsg(Sender, Error, CODE_READERROR, Msg_Error, ERR_READERROR);
      UnPack_Opened  : FOnMsg(Sender, Error, CODE_OPENED, Msg_Error, ERR_OPENED);
      UnPack_Stopped,11 : FOnMsg(Sender, Error, CODE_USERSKIP, Msg_Error, ERR_USERSKIP);
      else
        FOnMsg(NIL, Error, CODE_UNDEFERROR, Msg_Error, RarDir.GetRarErrorString(Error));
     end;
end;

function TCakRtDunRar.RarDirVolumeChange(Sender: TObject; ArcName: PChar;
  Mode: Integer): Integer;
begin
  with (Cakdir as TCakdir2) do
  if Assigned(FOnMsg) then
    FOnMsg(Sender, Error, CODE_VOLUMECHANGE, Msg_OK,
              Format(ERR_VOLUMECHANGE,[Arcname])      );
  Result := 0;
end;


procedure TCakRtDunRar.Rar3OnReqPassword(Sender: TObject; var Pass: PChar);
var
  pwd: String;
begin
  with (Cakdir as TCakdir2) do
  if Password <> '' then
    pass := PChar(Password)
  else if Assigned(FOnPwd) then
   begin
    FOnPwd(NIL, ArchiveName, Rar_LastProcess, pwd);
    pass := PChar(pwd);
   end;
end;


procedure TCakRtDunRar.RarOnReqPassword(Sender: TObject; eFile: TRARHeaderData;
  var Pass: string);
begin
  with (Cakdir as TCakdir2) do 
  if Password <> '' then
    pass := Password
  else if Assigned(FOnPwd) then
    FOnPwd(NIL, ArchiveName, efile.FileName, pass);
end;
function TCakRtDunRar.Rar3ProcessData(Sender: TObject; Addr: PChar; BlockSize,
  Position: Integer): Integer;
begin
  with (Cakdir as TCakdir2) do
  if Assigned(FOnProg) then
    FOnProg(NIL, ExtractFileName(Rar_LastProcess), Rar_Completed - Rar_FileSize,
      position);
  Rar_Completed := Rar_FileSize;
  if not Stop then Result := 1;
  Application.ProcessMessages;
  Result := 0;
end;

procedure TCakRtDunRar.Rar3DirList(Sender: TObject; eFile: TRARHeaderData;
  OK: Boolean; ResultCode: Integer);
begin
  RarDirList(Sender, efile, OK);
end;
procedure TCakRtDunRar.RarDirList(Sender: TObject; eFile: TRARHeaderData;
  Result: Boolean);
var 
  loc:    Integer;
  Ext, k: String;
  Icon:   TICON;
  Buffer: PChar;
begin
  with (Cakdir as TCakdir2) do
  begin
  Icon := TICON.Create;
  with efile do
  try
     GetMem(Buffer, Length(FileName) + 1);
     k := string(FileName);
     StrCopy(Buffer, PChar(k));
     AddContents(Archivename,ExtractFilePath(Buffer),
       ExtractFileName(Buffer),UnpSize,PackSize,False,
       FileDateToDateTime(FileTime),IntToHex(FileCRC, 8)       );
     FreeMem(Buffer);
  finally
    Icon.Free;
   end;
  end;
end;


procedure TCakRtDunRar.SetCakdir(pCakdir : TComponent);
begin
    Cakdir := TCakdir2(pCakdir);
end;
procedure TCakRtDunRar.DoStop(Stopp: Boolean);
begin
   Rardir.Stop := Stopp;
end;

procedure TCakRtDunRar.Process(dowhat : WorkType);
var 
  i, j: Integer;
begin
  Load_DLL;
  with (Cakdir as TCakdir2) do
  case doWhat of
    wtLoadContents: 
     begin
        RarDir.Archivefilename := Archivename;
        RarDir.ListArchive;
       end;
    wtExtract: 
     begin
      RarDir.TargetDirectory := ExtractOptions.Extr_to;
        if ExtractOptions.Extr_ExtractAll or
          (Get_Selected_Count > 0) then
         begin
          RarDir.Archivefilename := Archivename;
          RarDir.FileListToProcess.Clear;
          for i := 0 to Total_Contents - 1 do
            if ExtractOptions.Extr_ExtractAll or Archive_Contents[i]._Selected then
              if Archive_Contents[i]._FileArchive = Archivename then
                RarDir.FilelistToProcess.Add(Archive_Contents[i]._FileDefPath +
                  Archive_Contents[i]._FileName);
          i := RarDir.ExtractArchive;
          
          if i <> 0 then
          if Assigned(FOnMsg) then
            RarDirError(nil,i);
         end;
     end;
    wtTest:
       begin
        RarDir.Archivefilename := Archivename;
        j          := RarDir.TestArchive;
        if j <> 0 then 
        if Assigned(FOnMsg) then
             RarDirError(nil,j);
       end;
    else if Assigned(FOnMsg) then
        FOnMsg(NIL, 0, CODE_NOSUPPORT, Msg_Error, ERR_NOSUPPORT);
   end;
end;


function TCakRtDunRar.DllExists : boolean;
begin
        Result := Fileexists(GrabProgrampath+Dllname);
end;

function TCakRtDunRar.Cando(aWork: WorkType): Boolean;
begin
        Case aWork of
        wtNone : Result := true;
        wtTest, wtExtract : Result := CanExtract;
        wtAdd, wtDelete : Result := CanAdd;
        wtSFX :  Result := CanSFX;
        wtLoadContents: Result := CanList;
        else Result := false;
        end;
        if Result then
          if not DLLExists then
            Result := false;
end;

function TCakRtDunRar.Def_TreatAs : string;
begin
end;

procedure TCakRtDunRar.Load_DLL;
begin
  if not Assigned(RarDir) then
    RarDir := TdRar3.Create(Self);
  RarDir.Path2UnrarDll := ExtractFilePath(ParamStr(0));
  RarDir.OnError          := RarDirError;
  RarDir.OnFileDoneListed := Rar3DirList;
  RarDir.OnFileAboutToBeExtracted := RarDirExtracting;
  RarDir.OnFileAboutToBeTested := RarDirExtracting;
  RarDir.OnVolumeChange   := RarDirVolumeChange;
  RarDir.OnReqPassword    := Rar3OnReqPassword;
  RarDir.OnProcessData    := Rar3ProcessData;
end;

procedure TCakRtDunRar.UnLoad_DLL;
begin
  if not Assigned(RarDir) then Exit;
  RarDir.OnError          := NIL;
  RarDir.OnFileDoneListed := NIL;
  RarDir.OnFileAboutToBeExtracted := NIL;
  RarDir.OnFileAboutToBeTested := NIL;
  RarDir.OnVolumeChange   := NIL;
  RarDir.OnReqPassword    := NIL;
  RarDir.OnProcessData    := NIL;
end;

procedure Register;
begin
  //RegisterComponents('QZip', [TCakRtDunRar]);
end;

end.
 