unit CakDfUnRar;

interface

uses
  Dialogs, Windows, Messages, SysUtils, Classes, CakDefs2, CakArchiver, DFUnRar;

const CanExtract = TRUE;
      CanAdd = FALSE;
      CanList = TRUE;
      CanSFX = FALSE;
      Dllname = 'unrar.dll';
type
  TCakDfUnRar = class(TCakArchiver)
  private
    { Private declarations }
    Stopping : boolean;

  protected
    { Protected declarations }
  public
    { Public declarations }
    RarDir : TdfUnRar;
    Cakdir : TComponent;
    procedure SetCakdir(pCakdir : TComponent); override;
    procedure Process(dowhat : WorkType); override;
    function Cando(aWork: WorkType): Boolean; override;
    function Def_TreatAs : string; override;
    procedure Load_DLL; override;
    procedure UnLoad_DLL; override;
    function DllExists : Boolean; override;
    procedure DoStop(Stopp: Boolean); override;

    procedure RarDirRarStatus(Sender: TObject; Message: String; status: TRarStatus);
    procedure RarDirComment(Sender: TObject; Comment: String);
    procedure RarDirError(Sender: TObject; Message: String; MessageID: Integer);
    procedure RarDirFileProcessing(Sender: TObject; hdrData: TDFRARHeaderData; status: Integer);
    procedure RarDirPassword(Sender: TObject; var Password: String);
    procedure RarDirProgress(Sender: TObject; FilesProcessed, FileCount, SizeProcessed, SizeCount: Cardinal);
    procedure RarDirVolChange(Sender: TObject; ArcName: PAnsiChar; Mode: Integer);
  published
    { Published declarations }
    property Stop: Boolean read Stopping write DoStop;
  end;

procedure Register;

implementation
uses CakUtils2, Cakdir2;

procedure TCakDfUnRar.SetCakdir(pCakdir : TComponent);
begin
  Cakdir := TCakdir2(pCakdir);
end;
procedure TCakDfUnRar.DoStop(Stopp: Boolean);
begin
  RarDir.StopProcessing := stopp;
end;

procedure TCakDfUnRar.Process(dowhat : WorkType);
var i : integer;
procedure CheckPassword;
var Password : string;
begin
with (Cakdir as TCakdir2) do
if ArchiveNeedPassword and (RarDir.Password = '') then
  if Password <> '' then RarDir.Password := Password else
          if Assigned(FOnPwd) then
          begin
          FOnPwd(nil,ArchiveName,'',Password);
          RarDir.password := Password;
          end;
end;
begin
    Load_DLL;
    with (Cakdir as TCakdir2) do
    with RarDir do
    Case doWhat of
    wtLoadContents :
      begin
       FileName := ArchiveName;
       Mode := DFRAR_LIST;
       CanProgress := false;
       OverrideEvent := OR_EVENT;
       Extract;
      end;
    wtExtract :
      begin
        FileName := ArchiveName;
        RarDir.FileList.Clear;
        if Get_Selected_Count > 0 then
          for i := 0 to Total_Contents -1 do
            with Archive_Contents[i] do
            if _Selected then
            RarDir.FileList.Add(AppendSlash(Extractoptions.Extr_to)+_FileDefPath + _Filename);
        Directory := RemoveSlash(Extractoptions.Extr_to);
        Mode := DFRAR_EXTRACT;
        CanProgress := false;
        CheckPassword;
        Extract;
      end;
    wtTest :
      begin
        FileName := ArchiveName;
        CanProgress := false;
        Mode := DFRAR_EXTRACT;
        CheckPassword;
        Test;
      end;
    end;
end;

function TCakDfUnRar.DllExists : boolean;
begin
        Result := Fileexists(GrabProgrampath+Dllname);
end;

function TCakDfUnRar.Cando(aWork: WorkType): Boolean;
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

function TCakDfUnRar.Def_TreatAs : string;
begin
end;

procedure TCakDfUnRar.Load_DLL;
begin
  if not Assigned(RarDir) then
    RarDir := TDFUnRar.Create(Self);
  RarDir.OnRarStatus := RarDirRarStatus;
  RarDir.OnComment := RarDirComment;
  RarDir.OnError := RarDirError;
  RarDir.OnPassword := RarDirPassword;
  RarDir.OnFileProcessing := RarDirFileProcessing;
  RarDir.OnProgress := RarDirProgress;
  RarDir.OnVolChange := RarDirVolChange;
  RarDir.OverrideEvent := OR_EVENT;
  RarDir.StopProcessing := false;
  RarDir.PromptForPass := true;
end;

procedure TCakDfUnRar.UnLoad_DLL;
begin
  RarDir.Free;
end;

procedure TCakDfUnRar.RarDirRarStatus(Sender: TObject; Message: String; status: TRarStatus);
begin
  with (Cakdir as TCakdir2) do
    if Assigned(FOnMsg) then
      FOnMsg(Sender,-1,-1,Msg_Unknown,Message);
end;

procedure TCakDfUnRar.RarDirComment(Sender: TObject; Comment: String);
begin
  (Cakdir as TCakdir2).ArchiveComment := Comment;
end;

procedure TCakDfUnRar.RarDirError(Sender: TObject; Message: String; MessageID: Integer);
begin
  with (Cakdir as TCakdir2) do
    if Assigned(FOnMsg) then
      FOnMsg(Sender,MessageID,-1,msg_Error,Message);
end;

procedure TCakDfUnRar.RarDirFileProcessing(Sender: TObject; hdrData: TDFRARHeaderData; status: Integer);
begin
  with (Cakdir as TCakdir2) do
  AddContents(ArchiveName,ExtractFilePath(hdrData.FileName),
                     ExtractFilename(hdrData.FileName),hdrData.UnpSize,hdrData.PackSize,
                     hdrData.FlagNeedPassword,hdrData.FileTime,hdrData.FileCRC);
end;

procedure TCakDfUnRar.RarDirPassword(Sender: TObject; var Password: String);
begin
  with (Cakdir as TCakdir2) do
    if Assigned(FOnPwd) then
      FOnPwd(Sender,ArchiveName,'',password);
  RarDir.Password := Password;
end;

procedure TCakDfUnRar.RarDirProgress(Sender: TObject; FilesProcessed, FileCount, SizeProcessed, SizeCount: Cardinal);
begin
  with (Cakdir as TCakdir2) do
    if Assigned(FOnProg) then
      FOnProg(Sender,'',FileCount,FilesProcessed);
end;

procedure TCakDfUnRar.RarDirVolChange(Sender: TObject; ArcName: PAnsiChar; Mode: Integer);
begin
  with (Cakdir as TCakdir2) do
    if Assigned(FOnMsg) then
      FOnMsg(Sender,-1,CODE_VOLUMECHANGE,Msg_Ok,ArcName + ':' + ERR_VOLUMECHANGE);  
end;

procedure Register;
begin
  //RegisterComponents('QZip', [TCakDfUnRar]);
end;

end.
