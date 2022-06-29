unit CakAbbrevia;

interface

uses
  Windows, Messages, SysUtils, Classes, CakDefs2, AbBase, AbBrowse, CakUtils2,
  AbZBrows, AbZipper, AbUnzper, AbZipKit, AbArcTyp, CakArchiver,  AbUtils;

const CanExtract = TRUE;
      CanAdd = TRUE;
      CanList = TRUE;
      CanSFX = FALSE;
      Dllname = '';
type
  TCakAbbrevia = class(TCakArchiver)
  private
    { Private declarations }
    Stopping : boolean;

  protected
    { Protected declarations }
  public
    { Public declarations }
    AbBrowDir : TAbZipBrowser;
    AbUnzDir : TAbZipKit;
    AbZipDir : TAbZipKit;
    Cakdir : TComponent;
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

    procedure AbConfirmOverwrite(var Name: String; var Confirm: Boolean);
    procedure AbArchiveProgress(Sender: TObject;Progress: Byte; var Abort: Boolean);
    procedure AbConfirmProcessItem(Sender: TObject;Item: TAbArchiveItem; ProcessType: TAbProcessType; var Confirm: Boolean);
    procedure AbProcessItemFailure(Sender: TObject;Item: TAbArchiveItem; ProcessType: TAbProcessType;  ErrorClass: TAbErrorClass; ErrorCode: Integer);
    procedure AbArchiveItemProgress(Sender: TObject;Item: TAbArchiveItem; Progress: Byte; var Abort: Boolean);
  published
    { Published declarations }
    property Stop: Boolean read Stopping write DoStop;
  end;

procedure Register;

implementation
uses CakUtils, Cakdir2, Filters, Forms;
var Lastfile : string;

procedure TCakAbbrevia.AbArchiveItemProgress(Sender: TObject;Item: TAbArchiveItem; Progress: Byte; var Abort: Boolean);
begin
        if Assigned(TCakdir2(Cakdir).fonProg) then
                if Lastfile <> item.FileName then
                TCakdir2(Cakdir).FOnProg(Sender,Item.Filename,0,Progress);
        Abort := Stopping;
        if Abort then
        if Assigned(TCakdir2(Cakdir).fonMsg) then
          TCakdir2(Cakdir).FOnMsg(Sender,CODE_USERSKIP,0,Msg_Error,ERR_USERSKIP);
end;

procedure TCakAbbrevia.AbProcessItemFailure(Sender: TObject;Item: TAbArchiveItem; ProcessType: TAbProcessType;  ErrorClass: TAbErrorClass; ErrorCode: Integer);
begin
        if Assigned(TCakdir2(Cakdir).fonMsg) then
          with TCakdir2(Cakdir) do
            Case ErrorCode of
            0 : FOnMsg(Sender,CODE_UNDEFERROR,ErrorCode,Msg_Error,Item.FileName + ' FAILED');
            1 : FOnMsg(Sender,CODE_READONLY,ErrorCode,Msg_Error, Item.FileName +' '+ERR_READONLY);
            2 : FOnMsg(Sender,CODE_READERROR,ErrorCode,Msg_Error, Item.FileName +' '+ ERR_READERROR);
            3 : FOnMsg(Sender,CODE_CANTCREATE,ErrorCode,Msg_Error, Item.FileName +' '+ ERR_CANTCREATE);
            4 : FOnMsg(Sender,CODE_READERROR,ErrorCode,Msg_Error, Item.FileName +' '+ ERR_READERROR);
            5 : FOnMsg(Sender,CODE_UNDEFERROR,ErrorCode,Msg_Error,Item.FileName +' '+ ERR_UNDEFERROR);
            end;
end;

procedure TCakAbbrevia.AbConfirmProcessItem(Sender: TObject;Item: TAbArchiveItem; ProcessType: TAbProcessType; var Confirm: Boolean);
begin
        Confirm := true;
        if Assigned(TCakdir2(Cakdir).FonMsg) then
                TCakdir2(Cakdir).FOnMsg(Sender,CODE_PROCESSEXTR,0,Msg_OK,Item.FileName + ' ' + ERR_NOERROR);
end;

procedure TCakAbbrevia.AbConfirmOverwrite(var Name: String; var Confirm: Boolean);
begin
        Confirm := TCakdir2(Cakdir).AskOverwrite(Name);
end;

procedure TCakAbbrevia.AbArchiveProgress(Sender: TObject;Progress: Byte; var Abort: Boolean);
begin
        Abort := stopping;
end;

procedure TCakAbbrevia.SetCakdir(pCakdir : TComponent);
begin
    Cakdir := TCakdir2(pCakdir);
end;
procedure TCakAbbrevia.DoStop(Stopp: Boolean);
begin
        Stopping := stopp;
end;

procedure TCakAbbrevia.Process(dowhat : WorkType);
var i : integer;
    k,fn : string;
begin
    Load_DLL;
    Stopping := false;
    with (Cakdir as TCakdir2) do
    Case dowhat of
    wtDelete : begin
                abZipDir.LogFile := 'c:\test.txt';
                abZipDir.Logging := true;
                abZipDir.FileName := Archivename;
                abZipDir.AutoSave := true;
                for i := 0 to Total_Contents -1 do
                 with Archive_Contents[i] do
                   if _Selected then
                     abZipDir.DeleteFiles(_Filedefpath + _Filename);
                 abZipDir.Save;
                 abZipDir.CloseArchive;
               end;
    wtTest : begin
                abUnzDir.FileName := Archivename;
                abUnzDir.TagItems('*.*');
                abUnzDir.TestTaggedItems;
             end;
    wtExtract : begin
                  abUnzDir.FileName := Archivename;
                  if ArchiveNeedPassword then
                    if (Password = '') then
                    if Assigned(FOnPwd) then
                      FOnPwd(Self,Archivename,'',Password);
                  if Password <> '' then
                  abUnzDir.Password := Password;
                  if Length(ExtractOptions.Extr_to) > 3 then
                  abUnzDir.BaseDirectory := RemoveSlash(ExtractOptions.Extr_to) + '\'
                        else
                  abUnzDir.BaseDirectory := RemoveSlash(ExtractOptions.Extr_to);

                  abUnzDir.ExtractOptions := [eoCreateDirs];
                  if ExtractOptions.Extr_DirNames then
                        abUnzDir.ExtractOptions := abUnzDir.ExtractOptions + [eoRestorePath];
                  for i := 0 to Total_Contents - 1 do
                   if Archive_Contents[i]._Selected then
                    begin
                     k := AppendSlash(ExtractOptions.Extr_to) +
                     Archive_Contents[i]._FileDefPath;
                    if not DirectoryExists(k) then
                     MakeDirectory(k);
                    abUnzDir.ExtractFiles(Archive_Contents[i]._FileDefPath +
                    Archive_Contents[i]._FileName);
                   end;
                  abUnzDir.CloseArchive;
                end;
    wtAdd : begin
                abZipDir.FileName := Archivename;
                abZipDir.DOSMode := AddOptions.Add_DosFormat;
                abZipDir.Password := TCakdir2(Cakdir).AddOptions.Add_Encrypt;

                abZipDir.BaseDirectory := '';
                abZipDir.autoSave := true;

                abZipDir.StoreOptions := [soFreshen,soReplace];
                if not Addoptions.Add_UsePath then
                abZipDir.StoreOptions := abZipDir.StoreOptions + [soStripPath];
                for i := 0 to addoptions.Add_Files.Count -1 do
                  begin
                    abZipDir.BaseDirectory := ExtractFilePath(TCakdir2(cakdir).addoptions.Add_Files.Strings[i]);
                    abZipDir.AddFiles(Extractfilename(addoptions.Add_Files.Strings[i]),faAnyFile);
                  end;
                abZipDir.CloseArchive;
            end;
    wtLoadContents :
        begin
          abBrowDir.FileName := ArchiveName;
          for i := 0 to abBrowDir.Count -1 do
           With abBrowDir.Items[i] do
             if abBrowDir.Items[i].FileName <> '' then
                with (Cakdir as TCakdir2) do
                 begin
                  fn := ModifySlash(abBrowDir.Items[i].FileName,'/','\');
                  AddContents(ArchiveName,Extractfilepath(fn),
                  Extractfilename(fn),
                  UnCompressedSize,CompressedSize,IsEncrypted,
                  DosDatetimetoDatetime(LastModFileDate,LastModFileTime),
                  InttoHex(CRC32,8));
                 end;
           abBrowDir.CloseArchive;
        end;
    end;
    UnLoad_DLL;
end;

function TCakAbbrevia.DllExists : boolean;
begin
        Result := True;
end;

function TCakAbbrevia.Cando(aWork: WorkType): Boolean;
begin
        Case aWork of
        wtNone : Result := true;
        wtExtract, wtTest : Result := CanExtract;
        wtAdd, wtDelete : Result := CanAdd;
        wtSFX :  Result := CanSFX;
        wtLoadContents: Result := CanList;
        else Result := false;
        end;
        if Result then
          if not DLLExists then
            Result := false;
end;

function TCakAbbrevia.Def_TreatAs : string;
begin
end;

procedure TCakAbbrevia.Load_DLL;
begin
        stopping := false;
        if not Assigned(abBrowDir) then
          abBrowDir := TabZipBrowser.Create(self);
        if not Assigned(abZipDir) then
          abZipDir := TAbZipKit.Create(self);
        if not Assigned(abUnzDir) then
        AbUnzDir := TAbZipKit.Create(self);

        abBrowDir.OnArchiveProgress := AbArchiveProgress;
        abBrowDir.OnArchiveItemProgress := AbArchiveItemProgress;
        abBrowDir.OnConfirmProcessItem := AbConfirmProcessItem;
        abBrowDir.OnProcessItemFailure := AbProcessItemFailure;

        abUnzDir.OnArchiveProgress := AbArchiveProgress;
        abUnzDir.OnArchiveItemProgress := AbArchiveItemProgress;
        abUnzDir.OnConfirmProcessItem := AbConfirmProcessItem;
        abUnzDir.OnProcessItemFailure := AbProcessItemFailure;
        abUnzDir.OnConfirmOverwrite := AbConfirmOverwrite;

        abZipDir.OnArchiveProgress := AbArchiveProgress;
        abZipDir.OnArchiveItemProgress := AbArchiveItemProgress;
        abZipDir.OnConfirmProcessItem := AbConfirmProcessItem;
        abZipDir.OnProcessItemFailure := AbProcessItemFailure;
end;

procedure TCakAbbrevia.UnLoad_DLL;
begin
//        abBrowDir.Free;
//        abZipDir.Free;
//        abUnzDir.Free;
end;

function TCakAbbrevia.GetComment : string;
begin
  Load_DLL;
  Result := abBrowDir.ZipfileComment;
end;

procedure TCakAbbrevia.SetComment(text : string);
begin
  Load_DLL;
  abBrowDir.ZipfileComment := text;
end;

procedure Register;
begin
  //RegisterComponents('QZip', [TCakAbbrevia]);
end;

end.
