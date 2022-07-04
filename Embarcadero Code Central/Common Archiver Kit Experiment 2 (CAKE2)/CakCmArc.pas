unit CakCmarc;

interface

uses
  Windows, Messages, SysUtils, Classes, CakDefs2, CakArchiver, Archives, Filters;

const CanExtract = TRUE;
      CanAdd = TRUE;
      CanList = TRUE;
      CanSFX = TRUE;
      //Dllname = 'dummydll.dll';
type
  TCakCmArc = class(TCakArchiver)
  private
    { Private declarations }
    Stopping : boolean;

  protected
    { Protected declarations }
  public
    { Public declarations }
    ArcDir: TArchiveFile;
    Cakdir : TComponent;
    procedure SetCakdir(pCakdir : TComponent); override;
    procedure Process(dowhat : WorkType); override;
    function Cando(aWork: WorkType): Boolean; override;
    function Def_TreatAs : string; override;
    procedure Load_DLL; override;
    procedure UnLoad_DLL; override;
    function DllExists : Boolean; override;
    procedure DoStop(Stopp: Boolean); override;

    procedure ArcDirProgress(Sender: TObject; State: Integer; lpEis: LPEXTRACTINGINFOEX;var Abort: Boolean);
    procedure ArcHandleError(Code: Integer);

  published
    { Published declarations }
    property Stop: Boolean read Stopping write DoStop;
  end;
function ReturnarchiveType(FileName: string): TArchiverType;
procedure Register;

implementation
uses CakUtils2, Cakdir2, CAB32, Forms;
var Total_Unpacked, TotalSize: Longint;
    ArcLastNames: TStrings;

function ReturnarchiveType(FileName: string): TArchiverType;
  var k : string;
   begin
    k := UpperCase(ExtractFileExt(FileName));
    if k = '.ZIP' then
      Result := atZip 
    else if (k = '.LZH') or (k = '.LHA') then
      Result := atLha 
    else if k = '.CAB' then
      Result := atCab 
    else if k = '.TAR' then
      Result := atTar 
    else if (k = '.TAZ') or (k = '.TGZ') or
      (k = '.GZ') or (k = '.Z') then
     begin 
      Result := atTgz;
      {$IFDEF HIDEMSGWINDOW} ArcDir.Options.UserOptions :=
        '--display-dialog=0'; {$ENDIF}
     end 
    else if k = '.BZ2' then
      Result := atBz2 
    else if k = '.RAR' then
      Result := atRar
    else if (k = '.BGA') or (k = 'BZA') or (k = '.GZA') then
      Result := atBga 
    else if k = '.YZ1' then
      Result := atYz1 
    else if k = '.BEL' then
      Result := atBel 
    else if k = '.GCA' then
      Result := atGca 
    else if k = '.ARJ' then
      Result := atArj
    else if k = '.7Z' then
      Result := at7z
    else
      Result := atXacRett;
   end;

procedure TCakCmArc.SetCakdir(pCakdir : TComponent);
begin
    Cakdir := TCakdir2(pCakdir);
end;
procedure TCakCmArc.DoStop(Stopp: Boolean);
begin

end;

procedure TCakCmArc.Process(dowhat : WorkType);
const
  SEVENZIPCMP9 = ' -m0=BCJ2 -m1=LZMA -m2=LZMA -m3=LZMA -mb0:1 -mb0s1:2 -mb0s2:3 -m1d=23';
  SEVENZIPCMP8 = ' -m0=BCJ -m1=LZMA -m1d=21 -ms';
  SEVENZIPCMP0 = ' -m0';
var
  i, j, done:     Integer;
  IndivisualInfo: TIndivisualInfo;
  sfiles:         TStrings;
  k, Dummy, ArcBaseDir : String;
  CABDIR:         TCAB32;
  aFileList:      TStrings;
  TextFileList, BinFileList, ExeFileList : TStrings;
  //tf : textfile;
  function GetArcBaseDir(newstr, oldstr: String): String;
   var
    MaxLen, i, j: Integer;
   begin
    if Length(newstr) > Length(oldstr) then
      MaxLen := Length(newstr)
    else
      MaxLen := Length(oldstr);

    j := 0;
    i := 1;
    while (i <= MaxLen) and (newstr[i] = oldstr[i]) do
       begin
        if newstr[i] = '\' then
        j := i;
      inc(i);
     end;

    Result := Copy(newstr, 1, j);
   end;

begin
  Load_DLL;
  {$IFDEF NOT_CMD_LINE}Timer1.Enabled := True;{$ENDIF}
  ArcDir.Options.n  := 0;  {Showing Extracting Dialog}
  ArcDir.OutputSize := 8192;
  with (Cakdir as TCakdir2) do
  case doWhat of
    wtSfx:
     begin
      if (ArchiveType = _7z) then
       begin
        k := GrabTempPath;
                {assignfile(tf,k+'sfx.txt');
                rewrite(tf);
                Writeln(tf,';!@Install@!UTF-8!');
                if sfxoptions.sfx_caption <> '' then
                Writeln(tf,Format('Title="%s"',[sfxoptions.sfx_caption]));
                if sfxoptions.sfx_message <> '' then
                Writeln(tf,Format('BeginPrompt="%s"',[sfxoptions.sfx_message]));
                if sfxoptions.sfx_commandline <> '' then
                Writeln(tf,Format('RunProgram="%s"',[sfxoptions.sfx_commandline]));
                Writeln(tf,';!@InstallEnd@!');
                closefile(tf);
                }
        //if sfxoptions.sfx_autorun then
        //combine(grabprogrampath+'7zC.sfx',k+'sfx.txt',archivename,removefileext(archivename) + '.exe') else
        Combine(grabprogrampath + 'Stub\7zC.sfx', '', ArchiveName,
          RemoveFileExt(ArchiveName) + '.exe');
       end
      else
        ArcDir.Options.gw := 3;
      ArcDir.FileName := Archivename;
      k := ExtractFilePath(Archivename);
      ArcHandleError(ArcDir.MakeSfx(Application.handle, NIL, k));
     end;
    wtLoadContents:
     begin

      ArcDir.FileName := Archivename;
      ArcDir.FindOpen(Application.handle, 0);
      ArcDir.ArchiverType := ReturnarchiveType(Archivename);
                
        done := ArcDir.FindFirst('*.*', IndivisualInfo);
        while done = 0 do
           begin
           if ExtractFileName(ModifySlash(IndivisualInfo.szFileName)) <> '' then
            AddContents(ArchiveName,IndivisualInfo.szFileName,
            ExtractFilePath(RemoveFrontSlash(ModifySlash(IndivisualInfo.szFileName, '/', '\'))),
            ExtractFileName(ModifySlash(IndivisualInfo.szFileName, '/', '\')),
            IndivisualInfo.dwOriginalSize,IndivisualInfo.dwCompressedSize,
            False,
            DosDateTimeToDateTime(IndivisualInfo.wDate,IndivisualInfo.wtime),
            IntToHex(IndivisualInfo.dwCRC, 8));
            done := ArcDir.FindNext(IndivisualInfo);
           end;

     end;
    wtAdd:
     begin
      TotalSize := 0;
      ArcDir.Options.a := 1;
      ArcDir.FileName := ArchiveName;
      ArcDir.ArchiverType := ReturnarchiveType(ArchiveName);
      sfiles := TStringList.Create;
      AFileList := TStringList.Create;
      TextFileList := TStringList.Create;
      BinFileList := TStringList.Create;
      ExeFileList := TStringList.Create;

      try
        if AddOptions.Add_UsePath then
          ArcDir.Options.x := 1
        else
          ArcDir.Options.x := 0;

        for i := 0 to AddOptions.Add_Files.Count - 1 do         
           AFileList.addstrings(PollFileList(AddOptions.Add_Files.strings[i], AddOptions.Add_SubDir));

        AddOptions.Add_Files.Clear;
        AddOptions.Add_Files.AddStrings(AFileList);

        for i := 0 to AddOptions.Add_Exclude.Count - 1 do
         begin
          j := AddOptions.Add_Files.IndexOf(AddOptions.Add_Exclude.Strings[i]);
          if j <> -1 then
            AddOptions.Add_Files.Delete(j);
         end;

        if ArcDir.ArchiverType = atCAB then
        {this code let you add more than 1 file @ a time}
         begin
          k := '-a -mx';
          k := space + '"' + ArcDir.FileName + '"';
          for i := 0 to AddOptions.Add_Files.Count - 1 do
            k := k + space + '"' + AddOptions.Add_Files.strings[i] + '"';
          CabDir := TCab32.Create;
          try
            CabDir.Cab(application.Handle, k, dummy);
          finally
            CabDir.Free;
           end;
         end
        else if ArcDir.ArchiverType = at7z then //atSevenZip then
         begin
          ArcBaseDir := Extractfilepath(Addoptions.add_files.Strings[0]);
          for i := 1 to Addoptions.add_files.Count - 1 do
            ArcBaseDir := GetArcBaseDir(ArcBaseDir,Extractfilepath(Addoptions.add_files.Strings[i]));
         if not addoptions.add_relative then
         ArcBaseDir := Addoptions.add_files.Strings[0][1] + ':\';//Extractfilepath(ArchiveName);

         TextFileList.Clear;
         BinFileList.Clear;
         ExeFileList.Clear;

         for i := 0 to Addoptions.add_files.Count - 1 do
           case {GetFileTypeFor7ZUse(Addoptions.add_files.strings[i])}1 of
             0:
              begin
               if ArcBaseDir = '' then
	       TextFileList.Add(Addoptions.add_files.strings[i])
               else
	       TextFileList.Add(ExtractRelativePath(ArcBaseDir,Addoptions.add_files.strings[i]));
              end;
             1:
              begin
               if ArcBaseDir = '' then
               	BinFileList.Add(Addoptions.add_files.strings[i])
               else
                BinFileList.Add(ExtractRelativePath(ArcBaseDir,
                   Addoptions.add_files.strings[i]));
               end;
             2:
              begin
               if ArcBaseDir = '' then
                ExeFileList.Add(Addoptions.add_files.strings[i])
               else
		ExeFileList.Add(ExtractRelativePath(ArcBaseDir,
               Addoptions.add_files.strings[i]));
               end;
           end;

           //Addoptions.add_files.SaveToFile('c:\111\all.txt');

           //TextFileList.SaveToFile('c:\111\text.txt');
           //BinFileList.SaveToFile('c:\111\bin.txt');
           //ExeFileList.SaveToFile('c:\111\exe.txt');
          //ArcDir.Options.SevenMethod := 0;
           if TextFileList.Count <> 0 then
            ArcHandleError(ArcDir.PackFiles(Application.Handle,NIL,
               ArcBaseDir, [TextFileList]));
          //ArcDir.Options.SevenMethod := 1;
           if BinFileList.Count <> 0 then
            ArcHandleError(ArcDir.PackFiles(Application.Handle,NIL,
               ArcBaseDir, [BinFileList]));
          //ArcDir.Options.SevenMethod := 2;
           if ExeFileList.Count <> 0 then
            ArcHandleError(ArcDir.PackFiles(Application.Handle,NIL,
               ArcBaseDir, [ExeFileList]));
         end
        else{ if (ArcDir.ArchiverType = atTgz) or (ArcDir.ArchiverType = atTar) then
         begin
          sfiles.Clear;
          for i := 0 to AddOptions.Add_Files.Count - 1 do
           sfiles.Add(AddOptions.Add_Files.strings[i]);

         ARCHandleError(ArcDir.PackFiles(Application.Handle,NIL,
            '', [sfiles]));
         end
        else
          for i := 0 to AddOptions.Add_Files.Count - 1 do
           begin
            sfiles.Clear;
           sfiles.Add(ExtractFileName(AddOptions.Add_Files.strings[i]));

           ARCHandleError(ArcDir.PackFiles(Application.Handle,NIL,
             ExtractFilePath(AddOptions.Add_Files.Strings[i]),[sfiles]));
           end;}
         begin
          sfiles.Clear;

          ArcBaseDir := Extractfilepath(Addoptions.add_files.Strings[0]);
          for i := 1 to Addoptions.add_files.Count - 1 do
            ArcBaseDir := GetArcBaseDir(ArcBaseDir,
             Extractfilepath(Addoptions.add_files.Strings[i]));
          
          if ArcBaseDir = '' then
            for i := 0 to Addoptions.add_files.Count - 1 do
             sfiles.Add(Addoptions.add_files.strings[i])
          else
            for i := 0 to Addoptions.add_files.Count -1 do
             sfiles.Add(ExtractRelativePath(ArcBaseDir,
                Addoptions.add_files.strings[i]));

         ArcHandleError(ArcDir.PackFiles(Application.Handle,NIL,
            ArcBaseDir, [sfiles]));
         end;
      finally
        sfiles.Free;
        AFileList.Free;
        TextFileList.Free;
        BinFileList.Free;
        ExeFileList.Free;
       end;
     end;
    wtExtract:
        if ExtractOptions.Extr_ExtractAll or
          (Get_Selected_Count > 0) then
         begin
          TotalSize := 0;
          sfiles    := TStringList.Create;
          try
            ArcDir.FileName := Archivename;
            if ExtractOptions.Extr_DirNames then
              ArcDir.Options.x := 1
            else
              ArcDir.Options.x := 0;
            sfiles.Clear;

            if (ArchiveType = _7z) and (Password <> '') then
              ArcDir.Options.UserOptions := ' -p' + Password;

            if (ArchiveType = _7z) then
              with ArcDir.Options do
               begin
                UserOptions := UserOptions + ' -hide';
               end;

            for i := 0 to Total_Contents - 1 do
              if ExtractOptions.Extr_ExtractAll or Archive_Contents[i]._Selected then
                  sfiles.Add(Archive_Contents[i]._FileFullPath);


            for i := sfiles.Count - 1 downto 0 do
              if FileExists(AppendSlash(ExtractOptions.Extr_to) +
                sfiles.Strings[i]) then
                if AskOverwrite(sfiles.Strings[i]) then
                  DeleteFile(ExtractOptions.Extr_to +
                    sfiles.Strings[i])
              else
                sfiles.Delete(i);


            ArcHandleError(ArcDir.UnpackFiles(Application.handle,
              NIL, ExtractOptions.Extr_to, [sfiles]));
          finally
            sfiles.Free;
           end;
         end;
    wtDelete:
       begin
        TotalSize := 0;
        sfiles    := TStringList.Create;
        try
          ArcDir.FileName := Archivename;
          if ExtractOptions.Extr_DirNames then
            ArcDir.Options.x := 1 
          else
            ArcDir.Options.x := 0;
          sfiles.Clear;
          for i := 0 to Total_Contents - 1 do
            if Archive_Contents[i]._Selected then
              if Archive_Contents[i]._FileArchive = Archivename then
               begin
                sfiles.Clear;
                sfiles.Add(Archive_Contents[i]._FileDefPath + Archive_Contents[i]._FileName);
                ArcHandleError(ArcDir.Removeitems(Application.handle, NIL,
                  Archive_Contents[i]._FileDefPath, [sfiles]));
               end;
        finally
          sfiles.Free;
         end;
       end;
    wtTest:
       begin
        ArcDir.FileName := Archivename;
        ArcHandleError(ArcDir.CheckArchive(CHECKARCHIVE_FULLCRC, 0));
        //ARCHandleError(ArcDir.UnpackFiles( Application.Handle,nil,'TEST\',[nil] ));
       end;
   end;
  {$IFDEF NOT_CMD_LINE}Timer1.Enabled := False;{$ENDIF}
  Unload_dll;
end;

function TCakCmArc.DllExists : boolean;
begin
        Result := Fileexists(GrabProgrampath+Dllname);
end;

function TCakCmArc.Cando(aWork: WorkType): Boolean;
begin
        Case aWork of
        wtNone : Result := true;
        wtExtract : Result := CanExtract;
        wtAdd, wtDelete : Result := CanAdd;
        wtSFX :  Result := CanSFX;
        wtLoadContents: Result := CanList;
        else Result := false;
        end;
        //if Result then
        //  if not DLLExists then
        //    Result := false;
end;

function TCakCmArc.Def_TreatAs : string;
begin
end;

procedure TCakCmArc.Load_DLL;
begin
  if not Assigned(ArcLastNames) then
    ArcLastNames := TStringList.Create();
  if not Assigned(ArcDir) then
   begin
    ArcDir           := TArchiveFile.Create(Application);
    ArcDir.OnProgress := ArcDirProgress;
    ArcDir.Options.c := 1;
    {$IFDEF HIDEMSGWINDOW} ArcDir.Options.n := 0; {$ENDIF}
   end;
   //ArcDir.Options.jse := -1;
   //ArcDir.Options.jso := -1;
end;

procedure TCakCmArc.UnLoad_DLL;
begin
  //if assigned(Arcdir) then   //Crash here...
  //        begin
  //        ArcDir.OnProgress := nil;
  //        Arcdir.Free;
  //        end;
  ArcDir := NIL;
  ArcLastNames.Free;
  ArcLastNames := NIL;
  Archives.ReleaseArchiverDLL; // <<<< Line added
end;

procedure TCakCmArc.ArcDirProgress(Sender: TObject; State: Integer;
  lpEis: LPEXTRACTINGINFOEX; var Abort: Boolean);
var 
  completedsize: Longint;
begin
  Application.ProcessMessages;
  Abort := Stopping;
  //completedsize := 0;
  if lpEis = NIL then Exit;
  with lpEis^, lpEis^.exinfo do
  with (Cakdir as TCakdir2) do
    if ArcLastNames.IndexOf(szSourceFileName) = -1 then
     begin
      if FileExists(szSourceFileName) then
        completedsize := Get_File_Size(szSourceFileName)
      else
        completedsize := dwFilesize;
      ArcLastNames.Add(szSourceFilename);

      if Assigned(FOnProg) then
        FOnProg(NIL, ExtractFileName(szSourceFileName), dwFileSize, completedsize);
     end;
end;

procedure TCakCmArc.ArcHandleError(Code: Integer);
begin
  with (Cakdir as TCakdir2) do
  if Assigned(FOnMsg) then
    case Code of
      0, 1: FOnMsg(NIL, Error, CODE_NOERROR, Msg_OK, ERR_NOERROR);
      ERROR_DISK_SPACE: FOnMsg(NIL, ERROR_DISK_SPACE, CODE_NODISKSPACE, Msg_Error, ERR_NODISKSPACE);
      ERROR_READ_ONLY: FOnMsg(NIL, ERROR_READ_ONLY, CODE_READONLY, Msg_Error, ERR_READONLY);
      ERROR_USER_SKIP, ERROR_USER_CANCEL:
                       FOnMsg(NIL, Error, CODE_USERSKIP, Msg_Error, ERR_USERSKIP);
      ERROR_FILE_CRC: FOnMsg(NIL, ERROR_FILE_CRC, CODE_CRC, Msg_Error, ERR_CRC);
      ERROR_UNKNOWN_TYPE: FOnMsg(NIL, ERROR_UNKNOWN_TYPE, CODE_UNKTYPE, Msg_Error, ERR_UNKTYPE);
      ERROR_METHOD: FOnMsg(NIL, ERROR_METHOD, CODE_NOSUPPORT, Msg_Error, ERR_NOSUPPORT);
      ERROR_PASSWORD_FILE: FOnMsg(NIL, ERROR_PASSWORD_FILE, CODE_PASSWORD, Msg_Error, ERR_PASSWORD);
      ERROR_LONG_FILE_NAME: FOnMsg(NIL, ERROR_LONG_FILE_NAME, CODE_LONGFN, Msg_Error, ERR_LONGFN);
      ERROR_VERSION: FOnMsg(NIL, ERROR_VERSION, CODE_WRONGVER, Msg_Error, ERR_WRONGVER);
      ERROR_FILE_OPEN: FOnMsg(NIL, ERROR_FILE_OPEN, CODE_OPENED, Msg_Error, ERR_OPENED);
      ERROR_MORE_FRESH: FOnMsg(NIL, ERROR_MORE_FRESH, CODE_NEWER, Msg_Error, ERR_NEWER);
      ERROR_NOT_EXIST: FOnMsg(NIL, ERROR_NOT_EXIST, CODE_NOTEXIST, Msg_Error, ERR_NOTEXIST);
      ERROR_ALREADY_EXIST: FOnMsg(NIL, ERROR_ALREADY_EXIST, CODE_EXIST, Msg_Error, ERR_EXIST);
      ERROR_TOO_MANY_FILES: FOnMsg(NIL, ERROR_TOO_MANY_FILES, CODE_TOOMANYFILE, Msg_Error,
          ERR_TOOMANYFILE);
      ERROR_MAKEDIRECTORY: FOnMsg(NIL, ERROR_MAKEDIRECTORY, CODE_MAKEDIR, Msg_OK, ERR_MAKEDIR);
      ERROR_CANNOT_WRITE: FOnMsg(NIL, ERROR_CANNOT_WRITE, CODE_WRITE, Msg_Error, ERR_WRITE);
      ERROR_HUFFMAN_CODE: FOnMsg(NIL, ERROR_HUFFMAN_CODE, CODE_HUFFAN, Msg_Error, ERR_HUFFAN);
      ERROR_COMMENT_HEADER: FOnMsg(NIL, ERROR_COMMENT_HEADER, CODE_HEADER, Msg_Error, ERR_HEADER);
      ERROR_HEADER_CRC: FOnMsg(NIL, ERROR_HEADER_CRC, CODE_CRCHEADER, Msg_Error, ERR_CRCHEADER);
      ERROR_HEADER_BROKEN: FOnMsg(NIL, ERROR_HEADER_BROKEN, CODE_HEADERBROKE, Msg_Error,
          ERR_HEADERBROKE);
      ERROR_ARC_FILE_OPEN: FOnMsg(NIL, ERROR_ARC_FILE_OPEN, CODE_OPENED, Msg_Error, ERR_OPENED);
      ERROR_NOT_ARC_FILE: FOnMsg(NIL, ERROR_NOT_ARC_FILE, CODE_NOTARC, Msg_Error, ERR_NOTARC);
      ERROR_CANNOT_READ: FOnMsg(NIL, ERROR_CANNOT_READ, CODE_CANTREAD, Msg_Error, ERR_CANTREAD);
      ERROR_FILE_STYLE: FOnMsg(NIL, ERROR_FILE_STYLE, CODE_WRONGTYPE, Msg_Error, ERR_WRONGTYPE);
      ERROR_COMMAND_NAME: FOnMsg(NIL, ERROR_COMMAND_NAME, CODE_WRONGCMD, Msg_Error, ERR_WRONGCMD);
      ERROR_MORE_HEAP_MEMORY: FOnMsg(NIL, ERROR_MORE_HEAP_MEMORY, CODE_MOREHEAP, Msg_Error, ERR_MOREHEAP);
      ERROR_ENOUGH_MEMORY: FOnMsg(NIL, ERROR_ENOUGH_MEMORY, CODE_NOMEMORY, Msg_Error, ERR_NOMEMORY);
      ERROR_ALREADY_RUNNING: FOnMsg(NIL, ERROR_ALREADY_RUNNING, CODE_RUNNING, Msg_Error,
          ERR_RUNNING);
      ERROR_HARC_ISNOT_OPENED: FOnMsg(NIL, ERROR_HARC_ISNOT_OPENED, CODE_HARC, Msg_Error, ERR_HARC);
      ERROR_NOT_SEARCH_MODE: FOnMsg(NIL, ERROR_NOT_SEARCH_MODE, CODE_SEARCH, Msg_Error, ERR_SEARCH);
      ERROR_NOT_SUPPORT: FOnMsg(NIL, ERROR_NOT_SUPPORT, CODE_NOSUPPORT, Msg_Error, ERR_NOSUPPORT);
      ERROR_TIME_STAMP: FOnMsg(NIL, ERROR_TIME_STAMP, CODE_TIMESTAMP, Msg_Error, ERR_TIMESTAMP);
      ERROR_ARC_READ_ONLY: FOnMsg(NIL, ERROR_ARC_READ_ONLY, CODE_ARCREADONLY, Msg_Error,
          ERR_ARCREADONLY);
      ERROR_TMP_OPEN: FOnMsg(NIL, ERROR_TMP_OPEN, CODE_TMPOPEN, Msg_Error, ERR_TMPOPEN);
      ERROR_SAME_NAME_FILE: FOnMsg(NIL, ERROR_SAME_NAME_FILE, CODE_SAMENAME, Msg_Error, ERR_SAMENAME);
      ERROR_NOT_FIND_ARC_FILE: FOnMsg(NIL, ERROR_NOT_FIND_ARC_FILE, CODE_NOTFOUNDARC,
          Msg_Error, ERR_NOTFOUNDARC);
      ERROR_RESPONSE_READ: FOnMsg(NIL, ERROR_RESPONSE_READ, CODE_NORESPONSE, Msg_Error, ERR_NORESPONSE);
      ERROR_NOT_FILENAME: FOnMsg(NIL, ERROR_NOT_FILENAME, CODE_NOTVALID, Msg_Error, ERR_NOTVALID);
      ERROR_TMP_COPY: FOnMsg(NIL, ERROR_TMP_COPY, CODE_COPYTEMP, Msg_Error, ERR_COPYTEMP);
      ERROR_EOF: FOnMsg(NIL, ERROR_EOF, CODE_EOF, Msg_Error, ERR_EOF);
     else FOnMsg(NIL, Error, CODE_UNDEFERROR , Msg_Error, ERR_UNDEFERROR); 
     end
end;



procedure Register;
begin
  //RegisterComponents('QZip', [TCakCmArc]);
end;

end.
 