unit CakZipForge;

interface

uses
  Windows, Messages, SysUtils, Classes, CakDefs2, CakArchiver, ZipForge, Dialogs;

const CanExtract = TRUE;
      CanAdd = TRUE;
      CanList = TRUE;
      CanSFX = FALSE;
type
  TCakZipForge = class(TCakArchiver)
  private
    { Private declarations }
    Stopping : boolean;

  protected
    { Protected declarations }
  public
    { Public declarations }
    ZipDir : TZipForge;
    currentProgress : integer;
    currentFilename : string;
    procedure SetCakdir(pCakdir : TComponent); override;
    procedure Process(dowhat : WorkType); override;
    function Cando(aWork: WorkType): Boolean; override;
    function Def_TreatAs : string; override;
    procedure Load_DLL; override;
    procedure UnLoad_DLL; override;
    function DllExists : Boolean; override;
    procedure DoStop(Stopp: Boolean); override;

    function GetComment : string; override;
    procedure SetComment(text : string); override;
    

    procedure ZipDirConfirmOverwrite(Sender: TObject; SourceFileName: String; var DestFileName: String; var Confirm: Boolean);
    procedure ZipdirDiskFull(Sender: TObject; VolumeNumber: Integer; VolumeFileName: String; var Cancel: Boolean);
    procedure ZipdirFileProgress(Sender: TObject; FileName: String;Progress: Double; Operation: TZFProcessOperation;  ProgressPhase: TZFProgressPhase; var Cancel: Boolean);
    procedure ZipdirOverallProgress(Sender: TObject;Progress: Double; Operation: TZFProcessOperation; ProgressPhase: TZFProgressPhase; var Cancel: Boolean);
    procedure ZipdirPassword(Sender: TObject; FileName: String; var NewPassword: String; var SkipFile: Boolean);
    procedure ZipdirProcessFileFailure(Sender: TObject;  FileName: String; Operation: TZFProcessOperation; NativeError,  ErrorCode: Integer; ErrorMessage: String; var Action: TZFAction);
    procedure ZipdirRequestBlankVolume(Sender: TObject;  VolumeNumber: Integer; var VolumeFileName: String; var Cancel: Boolean);
    procedure ZipdirRequestFirstVolume(Sender: TObject; var VolumeFileName: String; var Cancel: Boolean);
    procedure ZipdirRequestLastVolume(Sender: TObject; var VolumeFileName: String; var Cancel: Boolean);
    procedure ZipdirRequestMiddleVolume(Sender: TObject;  VolumeNumber: Integer; var VolumeFileName: String; var Cancel: Boolean);
  published
    { Published declarations }
    property Stop: Boolean read Stopping write DoStop;
  end;

procedure Register;

implementation
uses CakUtils2, Cakdir2;
var Cakdir : TCakdir2;
procedure TCakZipForge.SetCakdir(pCakdir : TComponent);
begin
    Cakdir := TCakdir2(pCakdir);
end;
procedure TCakZipForge.DoStop(Stopp: Boolean);
begin
  stopping := stopp;
end;

procedure TCakZipForge.Process(dowhat : WorkType);
var ArchiveItem: TZFArchiveItem;
    i : integer;
begin
    Load_DLL;
    currentprogress := 0;
    Case dowhat of
    wtLoadContents :begin
                      Zipdir.FileName := Cakdir.ArchiveName;
                      Zipdir.OpenArchive;
                      if (Zipdir.FindFirst('*.*',ArchiveItem,faAnyFile-faDirectory)) then
                      repeat
                        with ArchiveItem do
                          Cakdir.AddContents(Cakdir.Archivename,StoredPath+FileName,StoredPath,
                          FileName,UncompressedSize,CompressedSize,Encrypted,
                          DosDateTimeToDateTime(LastModFileDate,LastModFileTime),IntToHex(CRC,8));
                      until (not Zipdir.FindNext(ArchiveItem));
                     end;
    wtExtract     :begin
                      if Cakdir.Extractoptions.Extr_DirNames then
                      begin
                      Zipdir.FileMasks.Clear;
                      for i := 0 to Cakdir.Total_Contents -1 do
                        with Cakdir.archive_Contents[i] do
                          if _Selected then
                          Zipdir.FileMasks.Add(_FileDefPath+_Filename );

                        Zipdir.BaseDir := Cakdir.Extractoptions.Extr_to;
                        Zipdir.Options.CreateDirs := true;
                        Zipdir.ExtractFiles;
                      end else
                      begin
                        Zipdir.FileMasks.Clear;
                        for i := 0 to Cakdir.Total_Contents -1 do
                        with Cakdir.archive_Contents[i] do
                          if _Selected then
                          Zipdir.FileMasks.Add(_FileDefPath+_Filename );

                        Zipdir.BaseDir := GrabTempPath;
                        Zipdir.Options.CreateDirs := true;
                        Zipdir.ExtractFiles;

                        for i := 0 to Cakdir.Total_Contents -1 do
                        with Cakdir.archive_Contents[i] do
                          if _Selected then
                            begin
                              MoveFile(PChar(GrabTempPath+_FileDefPath+_Filename),PChar(Appendslash(Cakdir.Extractoptions.Extr_to)+_Filename));
                            end;
                      end;

                   end;
    wtAdd         :begin
                    Zipdir.FileName := Cakdir.ArchiveName;
                    if fileexists(Zipdir.Filename) then
                    ZipDir.OpenArchive(fmOpenReadWrite) else
                    ZipDir.OpenArchive(fmCreate);
                    Zipdir.FileMasks.Assign(Cakdir.AddOptions.Add_Files);
                    Zipdir.ExclusionMasks.Assign(Cakdir.AddOptions.Add_Exclude);
                    Zipdir.Zip64Mode := zmAuto;
                    Case Cakdir.AddOptions.Add_CompLevel of
                    0,1,2 : Zipdir.CompressionLevel := clNone;
                    3,4,5 : Zipdir.CompressionLevel := clFastest;
                    6,7,8 : Zipdir.CompressionLevel := clNormal;
                    9     : Zipdir.CompressionLevel := clMax;
                    end;
                    if Cakdir.AddOptions.Add_UsePath then
                      if Cakdir.AddOptions.Add_Relative then
                        Zipdir.Options.StorePath := spRelativePath else
                        Zipdir.Options.StorePath := spFullPath else
                        Zipdir.Options.StorePath := spNoPath;
                    Zipdir.BaseDir := Cakdir.AddOptions.Add_BaseDir;
                    if Cakdir.AddOptions.Add_UseEncrypt then
                      Zipdir.Password := Cakdir.AddOptions.Add_Encrypt else
                      Zipdir.Password := '';
                    Zipdir.BeginUpdate;
                    Zipdir.AddFiles;
                    Zipdir.EndUpdate;
                    Zipdir.CloseArchive;
                   end;
    wtDelete      :begin
                    for i := 0 to Cakdir.Total_Contents -1 do
                        with Cakdir.archive_Contents[i] do
                          if _Selected then
                              Zipdir.FileMasks.Add(_Filedefpath+_Filename);
                    Zipdir.DeleteFiles;
                   end;
    wtTest        :begin
                    ZipDir.TestFiles;
                   end; 
    end;
end;

function TCakZipForge.DllExists : boolean;
begin
        Result := True;
end;

function TCakZipForge.Cando(aWork: WorkType): Boolean;
begin
        Case aWork of
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

function TCakZipForge.Def_TreatAs : string;
begin
end;

procedure TCakZipForge.Load_DLL;
begin
  if not Assigned(Zipdir) then
    ZipDir := TZipForge.Create(Self);
  stopping := false;
  Zipdir.OnFileProgress := ZipDirFileProgress;
  Zipdir.OnOverallProgress := ZipDirOVerallProgress;
  Zipdir.OnConfirmOverwrite := ZipDirConfirmOverwrite;
  Zipdir.OnPassword := ZipDirPassword;
  Zipdir.OnProcessFileFailure := ZipDirProcessFileFailure;
  Zipdir.OnDiskFull := ZipDirDiskFull;
  Zipdir.OnRequestFirstVolume := ZipDirRequestFirstVolume;
  Zipdir.OnRequestLastVolume := ZipDirRequestLastVolume;
  Zipdir.OnRequestMiddleVolume := ZipDirRequestMiddleVolume;
  Zipdir.OnRequestBlankVolume := ZipDirRequestBlankVolume;
end;

procedure TCakZipForge.UnLoad_DLL;
begin
  //Zipdir.Free;
end;

function TCakZipForge.GetComment : string;
begin
  Load_DLL;
  Result := ZipDir.Comment;
end;

procedure TCakZipForge.SetComment(text : string);
begin
  Load_DLL;
  ZipDir.Comment := Text;
  Zipdir.CloseArchive;
  with (Cakdir as TCakdir2) do
  ZipDir.OpenArchive;
end;


procedure TCakZipForge.ZipDirConfirmOverwrite(Sender: TObject;
  SourceFileName: String; var DestFileName: String; var Confirm: Boolean);
var b : boolean;
begin
  with Cakdir do
  if Assigned(FOnOver) then
    FOnOver(Sender,SourceFilename,confirm,b);
end;

procedure TCakZipForge.ZipdirDiskFull(Sender: TObject; VolumeNumber: Integer;
  VolumeFileName: String; var Cancel: Boolean);
begin
  Cancel := true;
  with Cakdir do
  if Assigned(FOnMsg) then
    FOnMsg(Sender,CODE_DiskFull,-1,Msg_Error,ERR_DiskFull);
end;

procedure TCakZipForge.ZipdirFileProgress(Sender: TObject; FileName: String;
  Progress: Double; Operation: TZFProcessOperation;
  ProgressPhase: TZFProgressPhase; var Cancel: Boolean);
begin
  if currentprogress = 100 then currentprogress := 0;
  with Cakdir do
  begin
  if Assigned(FOnProg) then
    FOnProg(Sender,Filename,100,currentProgress);
  if progress = 0 then
  if Assigned(FOnMsg) then
    FOnMsg(Sender,CODE_NOERROR,-1,Msg_OK,Filename);
  end;
  currentprogress := 0;
  currentFilename := Filename;
end;

procedure TCakZipForge.ZipdirOverallProgress(Sender: TObject;
  Progress: Double; Operation: TZFProcessOperation;
  ProgressPhase: TZFProgressPhase; var Cancel: Boolean);
begin
  if Progress <> 0 then
    currentProgress := Trunc(Progress / 1);
  with Cakdir do
  if Assigned(FOnProg) then
    FOnProg(Sender,currentFilename,100,0);

  Cancel := stopping;
end;

procedure TCakZipForge.ZipdirPassword(Sender: TObject; FileName: String;
  var NewPassword: String; var SkipFile: Boolean);
begin
  with Cakdir do
  if Assigned(FOnPwd) then
    FOnPwd(Sender,ArchiveName,Filename,NewPassword);
end;

procedure TCakZipForge.ZipdirProcessFileFailure(Sender: TObject;
  FileName: String; Operation: TZFProcessOperation; NativeError,
  ErrorCode: Integer; ErrorMessage: String; var Action: TZFAction);
begin
  with Cakdir do
  if Assigned(FOnMsg) then
    FOnMsg(Sender,CODE_UNDEFERROR,ErrorCode,Msg_Error,ErrorMessage);
end;

procedure TCakZipForge.ZipdirRequestBlankVolume(Sender: TObject;
  VolumeNumber: Integer; var VolumeFileName: String; var Cancel: Boolean);
begin
  VolumeFilename := Cakdir.ArchiveName;
end;

procedure TCakZipForge.ZipdirRequestFirstVolume(Sender: TObject;
  var VolumeFileName: String; var Cancel: Boolean);
begin
  VolumeFilename := Cakdir.ArchiveName;
end;

procedure TCakZipForge.ZipdirRequestLastVolume(Sender: TObject;
  var VolumeFileName: String; var Cancel: Boolean);
begin
  VolumeFilename := Cakdir.ArchiveName;
end;

procedure TCakZipForge.ZipdirRequestMiddleVolume(Sender: TObject;
  VolumeNumber: Integer; var VolumeFileName: String; var Cancel: Boolean);
begin
  VolumeFilename := Cakdir.ArchiveName;
end;

procedure Register;
begin
  //RegisterComponents('QZip', [TCakNewArchiver]);
end;

end.
