{$R ZipMsgUS.res}
unit CakDelphiZip;

interface

uses
  Windows, Messages, SysUtils, Classes, CakDefs2, CakArchiver, ZipMstr;

const CanExtract = TRUE;
      CanAdd = TRUE;
      CanList = TRUE;
      CanSFX = TRUE;
      Dllname = 'unzdll.dll';
type
  TCakDelphiZip = class(TCakArchiver)
  private
    { Private declarations }
    Stopping : boolean;
  protected
    { Protected declarations }
  public
    { Public declarations }
    ZipDir: TZipMaster;
    Cakdir : TComponent;
    procedure Zipdirrename(SourceName, DestName: string);
    procedure Zipdirrenamedir(SourceName, DestName: string);

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

    procedure ZipDirMessage(Sender: TObject; ErrCode: Integer; Message: string);
    procedure ZipDirProgress(Sender: TObject; ProgrType: ProgressType;
      FileName: string; FileSize: Cardinal);
    procedure ZipDirPwdErr(Sender: TObject; IsZipAction: Boolean;
      var NewPassword: string; ForFile: string; var RepeatCount: Cardinal;
      var Action: TPasswordButton);
    procedure ZipDirExtrOver(Sender: TObject; ForFile: string; Older: Boolean;
      var DoOverwrite: Boolean; DirIndex: Integer);
  published
    { Published declarations }
    property Stop: Boolean read Stopping write DoStop;
  end;

procedure Register;

implementation
uses CakUtils2, Cakdir2, Graphics, CakDateTime, Dialogs;

procedure TCakDelphiZip.SetCakdir(pCakdir : TComponent);
begin
    Cakdir := TCakdir2(pCakdir);
end;
procedure TCakDelphiZip.DoStop(Stopp: Boolean);
begin
    Zipdir.Cancel := Stopp;
end;

procedure TCakDelphiZip.Process(dowhat : WorkType);
var 
  i, j, loc, l: Integer;
  Ext, k:       String;
  Icon:         TICON;
  timestr, k2, k3: String;
  aFileList:    TStrings;
begin
    Load_DLL;
    with (Cakdir as TCakdir2) do
    case doWhat of
    wtSfx:
     begin
      k := Extractfilepath(Archivename) + '~' + Extractfilename(Archivename);
      Copyfile(pchar(Archivename),Pchar(k),True);
      ZipDir.zipfilename    := Archivename;
      ZipDir.sfxMessage     := SfxOptions.Sfx_Message;
      ZipDir.sfxCaption     := SfxOptions.Sfx_Caption;
      ZipDir.sfxcommandline := SfxOptions.Sfx_CommandLine;
      Zipdir.SFXPath := GrabProgramPath + 'ZipSFX.bin';
      ZipDir.SfxOptions     := [];
      if SfxOptions.Sfx_AutoRun then
        ZipDir.SfxOptions := ZipDir.SfxOptions + [SFXAutoRun];

      ZipDir.SFXOverWriteMode := OvrConfirm;
      if SfxOptions.Sfx_Overwrite then
        ZipDir.SFXOverWriteMode := OvrAlways;

      ZipDir.SFXDefaultDir := SfxOptions.Sfx_ExtractTo;
      ZipDir.ConvertSFX;
      Copyfile(Pchar(k),Pchar(Archivename),True);
     end;
    wtTest: 
     begin
      //Zipdir.TempDir := ExtractOptions.extr_to;
      ZipDir.ExtrOptions := [ExtrTest];
      ZipDir.ZipFileName := Archivename;
      ZipDir.Extract;
     end;
    wtExtract: 
     begin
      //if Length(ExtractOptions.Extr_to) > 3 then
        ZipDir.ExtrBaseDir := RemoveSlash(ExtractOptions.Extr_to) + '\';
      //else
      //  ZipDir.ExtrBaseDir := RemoveSlash(ExtractOptions.Extr_to);
      SetCurrentDir(RemoveSlash(ExtractOptions.Extr_to));
      if (ExtractOptions.Extr_ExtractAll) or (Get_Selected_Count > 0) then
         begin
          ZipDir.ZipFileName := Archivename;
          ZipDir.FSpecArgs.Clear;
          if ExtractOptions.Extr_ExtractAll then
            ZipDir.FSpecArgs.Add('*.*')
          else
            for i := 0 to Total_Contents - 1 do
              if Archive_Contents[i]._Selected then
                 begin
                  k := AppendSlash(ExtractOptions.Extr_to) +
                    Archive_Contents[i]._FileDefPath;
                  if not DirectoryExists(k) then
                    MakeDirectory(k);
                  ZipDir.FSpecArgs.Add(Archive_Contents[i]._FileDefPath +
                    Archive_Contents[i]._FileName);
                 end;

          ZipDir.ExtrOptions := [];
          if ExtractOptions.Extr_DirNames then
            ZipDir.ExtrOptions := ZipDir.ExtrOptions + [ExtrDirNames];
          if ExtractOptions.Extr_Overwrite then
            ZipDir.ExtrOptions := ZipDir.ExtrOptions + [ExtrOverwrite];
          OverwriteAll := 0;
          ZipDir.Extract;
         end;
     end;
    wtAdd: 
     begin
      ZipDir.ZipFileName  := Archivename;
      ZipDir.AddCompLevel := AddOptions.Add_CompLevel;
      aFileList           := TStringList.Create();
      ZipDir.AddOptions   := [];
      if AddOptions.Add_DosFormat then
        ZipDir.AddOptions := ZipDir.AddOptions + [AddForceDos];
      if AddOptions.Add_Hidden then
        ZipDir.AddOptions := ZipDir.AddOptions + [AddHiddenFiles];
      if _refresh in AddOptions.Add_Mode then
        ZipDir.AddOptions := ZipDir.AddOptions + [AddFreshen] 
      else if _update in AddOptions.Add_Mode then
        ZipDir.AddOptions := ZipDir.AddOptions + [AddUpdate] 
      else if _move in AddOptions.Add_Mode then
        ZipDir.AddOptions := ZipDir.AddOptions + [AddMove];
      if AddOptions.Add_UsePath then
        ZipDir.AddOptions := ZipDir.AddOptions + [AddDirnames];
      if AddOptions.Add_UseEncrypt then
        if AddOptions.Add_Encrypt <> '' then
         begin
          ZipDir.AddOptions := ZipDir.AddOptions + [AddEncrypt];
          ZipDir.Password   := AddOptions.Add_Encrypt;
         end;
      aFileList.Clear;

      for i := 0 to AddOptions.Add_Files.Count - 1 do
        aFileList.AddStrings(PollFileList(AddOptions.Add_Files.Strings[i],
          AddOptions.Add_SubDir));

      ZipDir.RootDir := '\';

      if not VersionControl then
        begin
        if AddOptions.Add_Relative then
          begin
          ZipDir.RootDir := RemoveSlash(ExtractFilePath(Archivename));
          if AddOptions.add_basedir <> '' then Zipdir.RootDir := Removeslash(AddOptions.add_basedir);
          end;
        end;

      if not VersionControl then
        if AddOptions.Add_Relative then
          for i := 0 to aFileList.Count - 1 do
            if Copy(UpperCase(aFileList.Strings[i]), 0,
              Length(ZipDir.rootdir)) = UpperCase(ZipDir.rootdir) then
              if Length(ZipDir.RootDir) > 4 then
                aFileList.Strings[i] :=
                  Copy(aFileList.Strings[i], Length(ZipDir.rootdir) + 1,
                  Length(aFileList.Strings[i]) - Length(ZipDir.rootdir));


      if not VersionControl then
       begin
        for i := 0 to AddOptions.Add_Exclude.Count - 1 do
         begin
          j := AddOptions.Add_Files.IndexOf(AddOptions.Add_Exclude.Strings[i]);
          if j <> -1 then AddOptions.Add_Files.Delete(j);
         end;

        try
           begin
            ZipDir.FSpecArgs.Clear;
            ZipDir.FSpecArgs.AddStrings(aFileList);
            ZipDir.Add;
           end;
        finally
          AddOptions.Add_Files.Clear;
         end;
       end 
      else
       begin {VERSIONCONTROL}

        if TimeStrFormat = '' then
          timestr := ModifySlash(ModifySlash(DateTimeToStr(Now),'/','-'),'\','-')
        else
          timestr := DecodeTimeStr(TimeStrFormat);

        timestr := ModifySlash(ModifySlash(timestr,'/','-'),'\','-');

        //for i := 0 to AddOptions.Add_files.Count -1 do
        //         afilelist.AddStrings(pollfilelist(Addoptions.add_files.strings[i],Addoptions.add_SubDir));

        aFileList.Clear;
        aFileList.AddStrings(AddOptions.Add_Files);

        for i := 0 to aFileList.Count - 1 do
         begin
          Load_DLL;
          ZipDir.ZipFileName := Archivename;
          k  := aFileList.Strings[i];
          k2 := AppendSlash(ExtractFilePath(k)) + '+' + ExtractFileName(k);
          k3 := k2;

          copyfile(PChar(k), PChar(k2), True);
          if AddOptions.Add_UsePath then
            ZipDir.AddOptions := ZipDir.AddOptions + [AddDirnames] 
          else
            ZipDir.AddOptions := ZipDir.AddOptions - [AddDirnames];
          ZipDir.FSpecArgs.Add(k2);
          ZipDir.Add;
          if AddOptions.Add_UsePath then
           begin
            k2 := RemoveDrive(k2);
            k  := RemoveDrive(k);
           end 
          else
           begin
            k2 := ExtractFileName(RemoveDrive(k2));
            k  := ExtractFileName(RemoveDrive(k));
           end;
          ZipDirRename(k2, timestr + '\' + k);
          sysutils.DeleteFile(k3);
         end;
       end;

      AddOptions.Add_Files.Clear;
      aFileList.Free;
     end;
    wtDelete: 
     begin
        ZipDir.ZipFileName := Archivename;
        ZipDir.FSpecArgs.Clear;
        for i := 0 to Total_Contents - 1 do
          if Archive_Contents[i]._Selected then
              ZipDir.FSpecArgs.Add(Archive_Contents[i]._FileDefPath +
                Archive_Contents[i]._FileName);
        ZipDir.Delete;
     end;
    wtRename :
     begin
      ZipDirRename(renameOptions.Rename_From, renameOptions.Rename_To);
     end;
    wtLoadContents:
     begin
      Icon := TICON.Create;
      DirectoryList.Clear;
      l := -1;
      try
        //Total_Contents := 0;
        ZipDir.ZipFileName := Archivename;
        if ZipDir.ZipFileName = '' then ArchiveType := _WIT;
          ArchiveNeedPassword := False;
          //SetLength(Archive_Contents, Total_Contents + ZipDir.Count + 5);
          for i := 0 to ZipDir.Count - 1 do
            with ZipDirEntry(ZipDir.ZipContents[i]^) do
             try
             TCakdir2(Cakdir).AddContents(ArchiveName,
                ExtractfilePath(Filename),Extractfilename(Filename),
                UnCompressedSize,CompressedSize,Encrypted,
                FileDateToDateTime(DateTime), IntToHex(CRC32, 8));
            except on EConvertError do
             TCakdir2(Cakdir).AddContents(ArchiveName,
                ExtractfilePath(Filename),Extractfilename(Filename),
                UnCompressedSize,CompressedSize,Encrypted,
                now, IntToHex(CRC32, 8));

            end;

       finally
        Icon.Free;
 {       if Total_Contents > 0 then
          Total_Contents := l + 1;
        SetLength(Archive_Contents, Total_Contents + 5);}
       end;
     end;

    else if Assigned(FOnMsg) then
      FOnMsg(NIL, 0, CODE_NOSUPPORT, Msg_Error, ERR_NOSUPPORT);
   end;
end;

function TCakDelphiZip.DllExists : boolean;
begin
        Result := Fileexists(GrabProgrampath+Dllname);
        if not Result then
          if assigned(TCakdir2(Cakdir).FOnMsg) then
                TCakdir2(Cakdir).FOnMsg(nil,-1,CODE_DLLNOTFOUND, msg_error,ERR_DLLNOTFOUND);
end;

function TCakDelphiZip.Cando(aWork: WorkType): Boolean;
begin
        Result := false;
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

function TCakDelphiZip.Def_TreatAs : string;
begin
end;

procedure TCakDelphiZip.Load_DLL;
begin
  if Assigned(ZipDir) then Exit;
  ZipDir           := TZipMaster.Create(self);
  ZipDir.OnProgress := ZipDirProgress;
  ZipDir.OnMessage := ZipDirMessage;
  ZipDir.OnPasswordError := ZipDirPwdErr;
  ZipDir.OnExtractOverwrite := ZipDirExtrOver;
{  if FDllPath <> '' then
   begin
    if FDllPath[Length(FDllPath)] <> '\' then
      ZipDir.DLLDirectory := FDllPath + '\';
    ZipDir.DLLDirectory := FDllPath;
   end;
 }
  //Zipdir.Unattended := false;
  ZipDir.Unattended := True;
  //Zipdir.Password := 'PASS';
end;

procedure TCakDelphiZip.UnLoad_DLL;
begin
  if Assigned(ZipDir) then
    ZipDir.Free;
  ZipDir := NIL;
end;

procedure TCakDelphiZip.ZipDirRename(SourceName, DestName: string);
var
  ZipRenameList: TList;
  RenRec:        pZipRenameRec;
begin
  ZipRenameList := TList.Create();
  New(RenRec);
  RenRec^.Source   := SourceName;
  RenRec^.Dest     := DestName;
  RenRec^.DateTime := 0;

  ZipRenameList.Add(RenRec);

  ZipDir.Rename(ZipRenameList, 0);
  Dispose(RenRec);
  ZipRenameList.Free();
  
  UnLoad_DLL;
  Load_DLL;
  TCakdir2(Cakdir).List;
end;

procedure TCakDelphiZip.ZipDirRenameDir(SourceName, DestName: string);
var
  j, k: String;
  i:    Integer;
begin
  with (Cakdir as TCakdir2) do
  for i := 0 to Total_Contents - 1 do
    if (UpperCase(Archive_Contents[i]._FileDefPath) =
      UpperCase(AppendSlash(SourceName))) then
     begin
      j := Archive_Contents[i]._FileDefPath + Archive_Contents[i]._FileName;
      k := AppendSlash(DestName) + Archive_Contents[i]._FileName;
      ZipDirRename(j, k);
     end;
end;

procedure TCakDelphiZip.ZipDirMessage(Sender: TObject; ErrCode: Integer;
  Message: string);
begin
  with (Cakdir as TCakdir2) do
  if Assigned(FOnMsg) then
    if Errcode = 0 then
      FOnMsg(Sender, CODE_NOERROR, Errcode, Msg_OK, Message)
    else
      Case Errcode of
      10101 : FOnMsg(Sender, CODE_DLLERROR, Errcode, Msg_Error, ERR_DLLERROR);
      10102 : FOnMsg(Sender, CODE_NOTFOUNDARC, Errcode, Msg_Error, ERR_NOTFOUNDARC);
      10104 : FOnMsg(Sender, CODE_PASSWORD, Errcode, Msg_Error, ERR_PASSWORD);
      10140 : FOnMsg(Sender, CODE_SFXHEADER, Errcode, Msg_Error, ERR_SFXHEADER);
      10151 : FOnMsg(Sender, CODE_PASSWORD, Errcode, Msg_Error, ERR_PASSWORD);
      10201 : FOnMsg(Sender, CODE_CANTREAD, Errcode, Msg_Error, ERR_CANTREAD);
      10202 : FOnMsg(Sender, CODE_LISTERROR, Errcode, Msg_Error, ERR_LISTERROR);
      10604, 10703 :
              FOnMsg(Sender, CODE_DLLNOTFOUND, Errcode, Msg_Error, ERR_DLLNOTFOUND);
      11007 : FOnMsg(Sender, CODE_USERSKIP, Errcode, Msg_Error, ERR_USERSKIP);
{      101 : FOnMsg(Sender, CODE_, Errcode, Msg_Error, ERR_);
      101 : FOnMsg(Sender, CODE_, Errcode, Msg_Error, ERR_);
      101 : FOnMsg(Sender, CODE_, Errcode, Msg_Error, ERR_);
      101 : FOnMsg(Sender, CODE_, Errcode, Msg_Error, ERR_);
}
      10105, 10142, 10143, 10181..10186 :
              FOnMsg(Sender, CODE_NOERROR , Errcode, Msg_OK, Message);
      else FOnMsg(Sender, CODE_UNDEFERROR , Errcode, Msg_Error, Message);
      end;
end;

procedure TCakDelphiZip.ZipDirExtrOver(Sender: TObject;
  ForFile: string; Older: Boolean; var DoOverwrite: Boolean; DirIndex: Integer);
begin
  DoOverwrite := TCakdir2(Cakdir).AskOverwrite(Forfile);
end;

procedure TCakDelphiZip.ZipDirProgress(Sender: TObject; ProgrType: ProgressType;
  FileName: string; FileSize: Cardinal);  // ## GC Cardinal replaced by Integer
begin
  with (Cakdir as TCakdir2) do
  begin
  case ProgrType of
    EndOfBatch, NewFile, TotalSize2Process:
      TotalProgress := 0;
    ProgressUpdate:
      TotalProgress := FileSize;
   end;
  if Assigned(FOnProg) then
    FOnProg(Sender, FileName, FileSize, TotalProgress);
  end;
end;

procedure TCakDelphiZip.ZipDirPwdErr(Sender: TObject;
  IsZipAction: Boolean; var NewPassword: string; ForFile: string;
  var RepeatCount: Cardinal; var Action: TPasswordButton);
var
  pwd: String;
begin
  with (Cakdir as TCakdir2) do
  if (Password <> pwd) and (Password <> '') then
   begin
    newpassword := Password;
    RepeatCount := 1;
   end
  else
   begin
    if Assigned(FOnPwd) then
      FOnPwd(NIL, ZipDir.ZipFileName, forfile, pwd)
    else
      pwd := Inputbox(MSG_PWD, MSG_PLZENTERPWD4 + forfile, pwd);
    ZipDir.Password := pwd;
    Newpassword     := pwd;
    Password        := pwd;
    RepeatCount     := 0;
   end;
end;

function TCakDelphiZip.GetComment : string;
begin
  Load_DLL;
  Result := ZipDir.ZipComment;
end;

procedure TCakDelphiZip.SetComment(text : string);
begin
  Load_DLL;
  ZipDir.ZipComment := Text;
  Zipdir.ZipFileName := '';
  with (Cakdir as TCakdir2) do
  ZipDir.ZipFileName := Archivename;
end;

procedure Register;
begin
  //RegisterComponents('QZip', [TCakDelphiZip]);
end;

end.
