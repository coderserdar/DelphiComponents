unit CakExtension;

interface                                      

uses
  Windows, Messages, SysUtils, Classes, CakDefs2, CakArchiver,
  Shellapi;

const CanExtract = FALSE;
      CanAdd = FALSE;
      CanList = FALSE;
      CanSFX = FALSE;
      Dllname = 'dummydll.dll';

const
  SCRIPTEXT    = '.INI';
  SCRIPTFILTER = '*' + SCRIPTEXT;
  MACRO        = '%';
  SPACE        = ' ';
  DOT          = '.';
  INFO         = 'Info';
  FEATURES     = 'Features';
  PATHS        = 'Paths';
  STRINGS      = 'Strings';
  PARAM        = 'Param';
  COMMANDS     = 'Commands';
  LIST         = 'List';
  ERROR        = 'Error!';

type
  WorkTypeEx           = (Ex_None, //Donothing
    Ex_LoadContents, //List Archive
    Ex_Extract, //Extract Archive
    Ex_Test, //Test Archive
    Ex_Add, //Add file to archive
    Ex_Delete, //Delete file from archive
    Ex_SFX                //Create Self extractables
    );
  ExtractOptionstypeEx = record
    Extr_To: String;
    extract_files: String;
  end;
  AddOptionstypeEx = record
    Add_Files: String;
  end;
  DeleteOptionstypeEx = record
    del_files: String;
  end;
  ContenttypeEx = record
    _Filename, _FileArchive: String;
    _FileSize, _FilePackedSize: Integer;
    _FileRatio: Integer;
  end;

type
  TCakExtension = class(TCakArchiver)
  private
    { Private declarations }
    Stopping : boolean;
    SupportType: String;
    ScriptPath: String;
    Log: String;
    batfilename: String;
  protected
    { Protected declarations }
    procedure RunAndWait(ProgramPath, ProgramParam: String);
  public
    { Public declarations }
    DosOutput: TStrings;
    ExtractOptionsEx: ExtractOptionstypeEx;
    AddOptionsEx: AddOptionstypeEx;
    DeleteOptionsEx: DeleteOptionstypeEx;
    Total_Contents: Integer;
    Archive_Contents: array of ContenttypeEx;

    procedure SetCakdir(pCakdir : TComponent); override;
    procedure Process(dowhat : WorkType); override;
    function Cando(aWork: WorkType): Boolean; override;
    function Def_TreatAs : string; override;
    procedure Load_DLL; override;
    procedure UnLoad_DLL; override;
    function DllExists : Boolean; override;
    procedure DoStop(Stopp: Boolean); override;

    function Supportactions(Archivetype: String; Action: WorkTypeEx): Boolean;
    procedure RePollScriptDirectory;
    procedure ProcessEXT(ArchiveName: String; Action: WorkTypeEx);
    function TranslateString(Inifilename, Macroname, ArchiveName: String;
      var Executename: String): String;
    constructor Create(AOwner: TComponent); OVERRIDE;
    destructor Destroy; OVERRIDE;

  published
    { Published declarations }
    property Stop: Boolean read Stopping write DoStop;
    property ScriptDirectory: String READ ScriptPath WRITE ScriptPath;
    property Supportformats: String READ SupportType WRITE SupportType;
    property Logfile: String READ log WRITE log;
    property Batchfilename: String READ batfilename;
  end;

procedure Register;

implementation
uses CakUtils2, Cakdir2, IniFiles, ConsoleApp;
var Cakdir : TCakdir2;
function GetStringInIni(FileName: String; Section: String; Key: String;
  Default: String): String;
var 
  Ini: TInifile;
begin
  Ini := TIniFile.Create(FileName);
  try
    with Ini do
      Result := ReadString(Section, Key, '');
  finally
    Ini.Free;
   end;
  if Result = '' then 
    Result := Default;
end;

function GetIntegerInIni(FileName: String; Section: String; Key: String;
  Default: Integer): Integer;
var 
  Ini: TInifile;
begin
  Ini := TIniFile.Create(FileName);
  try
    with Ini do
      Result := ReadInteger(Section, Key, Default);
  finally
    Ini.Free;
   end;
  //if result =  then result := default;
end;

procedure TCakExtension.RunAndWait(ProgramPath, ProgramParam: String);
var
  exInfo:   TShellExecuteInfo;
  exitcode: DWORD;
begin
  FillChar(exInfo, Sizeof(exInfo), 0);

  with exInfo do
   begin
    cbSize       := Sizeof(exInfo); // required!
    fMask        := SEE_MASK_NOCLOSEPROCESS or SEE_MASK_FLAG_DDEWAIT;
    Wnd          := 0;
    lpVerb       := 'open';
    lpFile       := PChar(ProgramPath);
    lpParameters := PChar(ProgramParam);
    nShow        := SW_HIDE;
   end;
  if ShellExecuteEx(@exInfo) then
   begin
    while GetExitCodeProcess(exinfo.hProcess, exitcode) and (exitcode = STILL_ACTIVE) do
        Sleep(500);
    CloseHandle(exinfo.hProcess);
   end
end;

procedure TCakExtension.SetCakdir(pCakdir : TComponent);
begin
    Cakdir := TCakdir2(pCakdir);
    with Cakdir do
    CakArchiverList[Locate_Archiver(_Ext)].SupportExtension := Supporttype;
end;
procedure TCakExtension.DoStop(Stopp: Boolean);
begin
end;

procedure TCakExtension.Process(dowhat : WorkType);
var i : integer;
begin
     Case doWhat of
        wtTest : processExt(Cakdir.Archivename,Ex_Test);
        wtExtract :
        begin
        ExtractOptionsEx.Extr_To := Cakdir.Extractoptions.Extr_to;
        for i := 0 to cakdir.Total_Contents - 1 do
        if Cakdir.Archive_Contents[i]._Selected then
         begin
          with Cakdir.Archive_contents[i] do
            ExtractOptionsEx.extract_files := _Filedefpath + _Filename;
          processExt(Cakdir.Archivename,Ex_Extract);
         end;
        end;
        wtAdd :
        begin
        for i := 0 to cakdir.AddOptions.Add_Files.Count -1 do
        begin
          AddOptionsEx.Add_Files := Cakdir.AddOptions.Add_Files.Strings[i];
          processExt(Cakdir.Archivename,Ex_Add);
        end;
        end;
        wtDelete :
        begin
        for i := 0 to cakdir.Total_Contents - 1 do
        if Cakdir.Archive_Contents[i]._Selected then
         begin
          with Cakdir.Archive_contents[i] do
            DeleteOptionsEx.del_files := _Filedefpath+_Filename;
          processExt(Cakdir.Archivename,Ex_Delete);
         end;
        end;
        wtSFX :  processExt(Cakdir.Archivename,Ex_Sfx);
        wtLoadContents:
        begin
        processExt(Cakdir.Archivename,Ex_LoadContents);
        Cakdir.Total_Contents := Total_Contents;
        SetLength(Cakdir.Archive_Contents,Total_contents);
        for i := 0 to Total_Contents -1 do
         with Archive_Contents[i] do
          begin
            CakDir.AddContents(CakDir.Archivename,Extractfilepath(_Filename),
            Extractfilename(_Filename), _FileSize, _FilePackedSize, False,
            Now, 'FFFFFFFF' );
          end;
        end;
     end;

    if assigned(Cakdir.FOnMsg) then
    for i := 0 to DosOutput.Count -1 do
      Cakdir.FOnMsg(NIL, 0, CODE_UNDEFERROR, Msg_OK, DosOutput.strings[i]);
end;

function TCakExtension.DllExists : boolean;
begin
        Result := TRUE;
end;

function TCakExtension.Cando(aWork: WorkType): Boolean;
begin
        Result := false;
        if pos(Extractfileext(uppercase(Cakdir.Archivename)),Def_TreatAs) <> 0 then
        Case aWork of
        wtNone : Result := true;
        wtTest : result := SupportActions(Extractfileext(Cakdir.Archivename),Ex_Test);
        wtExtract : result := SupportActions(Extractfileext(Cakdir.Archivename),Ex_Extract);
        wtAdd : result := SupportActions(Extractfileext(Cakdir.Archivename),Ex_Add);
        wtDelete : result := SupportActions(Extractfileext(Cakdir.Archivename),Ex_Delete);
        wtSFX :  result := SupportActions(Extractfileext(Cakdir.Archivename),Ex_Sfx);
        wtLoadContents: result := SupportActions(Extractfileext(Cakdir.Archivename),Ex_LoadContents);
        else Result := false;
        end;
end;

function TCakExtension.Def_TreatAs : string;
begin
  result := SupportType;
end;

procedure TCakExtension.Load_DLL;
begin
end;

procedure TCakExtension.UnLoad_DLL;
begin
end;


constructor TCakExtension.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  DosOutput   := TStringList.Create();
  ScriptPath := GrabProgrampath + 'Extension\';
  RepollScriptDirectory;
  log         := GrabProgramPath + 'CakExtension.log';  //if not specified...hungry?
  batfilename := GrabProgramPath + 'CakExtension.bat';
end;

destructor TCakExtension.Destroy; 
begin
  DosOutput.Free;
  inherited Destroy;
end;

/////////////////////////////////////////////////////////////////////////
procedure TCakExtension.ProcessEXT(ArchiveName: String; Action: WorkTypeEx);
var 
  Archivetype: String;
  File2Run, Param: String;
  IniFilename: String;
  ExitCode: Integer;
  Mode: Integer;
  k:    String;
  function Calculatesize(sizestring: String): Integer;
  var 
    i:    Integer;
    k, l: String;
   begin
    k := '';
    l := TrimRight(TrimLeft(sizestring));
    for i := 0 to Length(l) do
      if StrToIntDef(l[i], - 1) <> -1 then
        k := k + l[i];
    Result := StrToIntDef(k, 0);
   end;
  procedure Loadfilelist;
  var 
    i: Integer;
    linestart, linestop: Integer;
    filenamestart, filenamestop: Integer;
    filesizestart, filesizestop: Integer;
    filepackedsizestart, filepackedsizestop: Integer;
   begin
    linestart          := GetIntegerInINI(Inifilename, LIST, 'LINESTART', - 1);
    linestop           := GetIntegerInINI(Inifilename, LIST, 'LINESTOP', - 1);
    filenamestart      := GetIntegerInINI(Inifilename, LIST, 'FILENAME-START', - 1);
    filenamestop       := GetIntegerInINI(Inifilename, LIST, 'FILENAME-STOP', - 1);
    filesizestart      := GetIntegerInINI(Inifilename, LIST, 'FILESIZE-START', - 1);
    filesizestop       := GetIntegerInINI(Inifilename, LIST, 'FILESIZE-STOP', - 1);
    filepackedsizestart := GetIntegerInINI(Inifilename, LIST,
      'FILEPACKEDSIZE-START', - 1);
    filepackedsizestop := GetIntegerInINI(Inifilename, LIST,
      'FILEPACKEDSIZE-STOP', - 1);

    Total_Contents := DosOutput.Count + linestop - linestart;
    if Total_Contents > 0 then
     begin
      SetLength(Archive_Contents, Total_Contents);
      for i := linestart to DosOutput.Count + linestop - 1 do
        with Archive_Contents[i - linestart] do
         begin
          if (filenamestart <> filenamestop) then
            _Filename := TrimRight(TrimLeft(Copy(DosOutput.strings[i],
              filenamestart, filenamestop - filenamestart)));

          if (filesizestart <> filesizestop) then
            _FileSize := calculatesize(Copy(DosOutput.strings[i],
              filesizestart, filesizestop - filesizestart));
                
          if (filepackedsizestart <> filepackedsizestop) then
            _FilePackedSize := calculatesize(Copy(DosOutput.strings[i],
              filepackedsizestart, filepackedsizestop - filepackedsizestart));

          _FileRatio := 100;

          if _FileSize > 0 then
            _FileRatio := Trunc(_FilePackedSize / _FileSize * 100);


          _FileArchive := ArchiveName;
         end;
     end;
   end;
  procedure MakeBatch(batfilename, file2run: String);
  var 
    tf: textfile;
   begin
    Assignfile(tf, batfilename);
    Rewrite(tf);
    Writeln(tf, '@' + file2run);
    Writeln(tf, '@' + 'Exit');
    Closefile(tf);
   end;
begin
  Archivetype := ExtractFileExt(ArchiveName);
  Archivetype := Copy(Archivetype, 2, Length(Archivetype) - 1);
  Inifilename := AppendSlash(ScriptPath) + Archivetype + SCRIPTEXT;
  DosOutput.CLEAR;
  if not SupportActions(Archivetype, Action) then 
    Exit;


  case Action of
    Ex_LoadContents: 
      Param := TranslateString(IniFilename, 'LIST', ArchiveName, File2Run);
    Ex_Extract: 
      Param := TranslateString(IniFilename, 'EXTRACT', ArchiveName, File2Run);
    Ex_Test: 
      Param := TranslateString(IniFilename, 'TEST', ArchiveName, File2Run);
    Ex_Add: 
      Param := TranslateString(IniFilename, 'ADD', ArchiveName, File2Run);
    Ex_SFX: 
      Param := TranslateString(IniFilename, 'SFX', ArchiveName, File2Run);
    Ex_Delete: 
      Param := TranslateString(IniFilename, 'DELETE', ArchiveName, File2Run);
   end;

  Mode := GetIntegerInINI(IniFilename, COMMANDS, 'MODE', 0);

  if FileExists(File2Run) and (File2Run <> '') then
   begin
    case Mode of
      0: 
        ExitCode := ExecConsoleApp(File2Run, Param, DosOutput, NIL);
      1: 
       begin
        MakeBatch(Batfilename, File2run + SPACE + Param + ' >' + log);
        RunAndWait(Batfilename, '');
        if FileExists(log) then
          DosOutput.LoadFromFile(log);
        exitCode := 0;
       end;
      else
        ExitCode := -1;
     end;

    case Action of
      Ex_LoadContents: 
        Loadfilelist;
     end;
   end 
  else
   begin
    ExitCode := -1;
    k        := GetStringInINI(IniFilename, INFO, 'DOWNLOAD', '<none>');
    MessageBox(0,
      PChar('Cannot found executable specified in Extension script' + #13 +
      'D/L Info > ' + k),
      PChar('Not found!'),
      0);
   end;

  DosOutput.Add('Exitcode = ' + IntToStr(Exitcode))
end;

function TCakExtension.Supportactions(Archivetype: String; Action: WorkTypeEx): Boolean;
var 
  IniFilename, AType: String;
begin
  if Archivetype = '' then
   begin
    Result := False;
    Exit;
   end;
  if Archivetype[1] = '.' then
    AType := copy(archivetype, 2, Length(archivetype) - 1) 
  else
    AType := archivetype;
  Inifilename := AppendSlash(ScriptPath) + AType + SCRIPTEXT;
  case Action of
    Ex_None: 
      Result := False;
    Ex_LoadContents: 
      Result := (GetIntegerInIni(inifilename, FEATURES, 'LIST', 0) = 1);
    Ex_Extract: 
      Result := (GetIntegerInIni(inifilename, FEATURES, 'EXTRACT', 0) = 1);
    Ex_Test: 
      Result := (GetIntegerInIni(inifilename, FEATURES, 'TEST', 0) = 1);
    Ex_Add: 
      Result := (GetIntegerInIni(inifilename, FEATURES, 'ADD', 0) = 1);
    Ex_Delete: 
      Result := (GetIntegerInIni(inifilename, FEATURES, 'DELETE', 0) = 1);
    Ex_SFX: 
      Result := (GetIntegerInIni(inifilename, FEATURES, 'SFX', 0) = 1);
    else 
      Result := False;
   end;
end;

function TCakExtension.TranslateString(Inifilename, Macroname, ArchiveName: String;
  var Executename: String): String;
var
  k, l:     String;
  i, j:     Integer;
  Newmacro: String;

  function Locatemacro(Macroname: String): String;
  var 
    k: String;
   begin
    k := '';
    if (Macroname = 'ADD') or (Macroname = 'EXTRACT') or
      (Macroname = 'LIST') or (Macroname = 'TEST') or
      (Macroname = 'DELETE') or (Macroname = 'SFX') then
      k := GetStringInIni(Inifilename, COMMANDS, Macroname, '') 
    else if (Macroname = 'EXEPATH') or (Macroname = 'UNEXEPATH') then
      k := GetStringInIni(Inifilename, PATHS, Macroname, '') 
    else if (Macroname = 'ARCHIVE-NAME') then
      k := RemoveFileExt(ArchiveName) 
    else if (Macroname = 'ARCHIVE-EXT') then
      k := ExtractFileExt(ArchiveName) 
    else if (Macroname = 'ADDFILE') or (Macroname = 'FILE2ADD') then
      k := AddOptionsEx.Add_Files 
    else if (Macroname = 'EXTRACTTO') then
      k := ExtractOptionsEx.Extr_To 
    else if (Macroname = 'FILE2EXTR') then
      k := ExtractOptionsEx.extract_files
    else if (Macroname = 'FILE2DEL') then
      k := DeleteOptionsEx.del_files
    else if (Macroname = 'PASSWORD') then
      k := Cakdir.AddOptions.Add_Encrypt
    else if (Macroname = 'OPTIONS') then
      begin
         k := '';
         if (Cakdir.AddOptions.Add_Encrypt <> '') and Cakdir.AddOptions.Add_UseEncrypt then
          k := k + ' ' + TranslateString(inifilename,'USEPASSWORD',Archivename,ExecuteName);
      end
    else
      if MacroName <> '' then
        k := GetStringInIni(Inifilename, STRINGS, Macroname, '');
    Result := k;
   end;
  procedure Translate;
   begin
    NewMacro := Copy(k, i + 1, j - i - 1);
    if (NewMacro = 'EXEPATH') or (NewMacro = 'UNEXEPATH') then
     begin
      Executename := LocateMacro(NewMacro);
      l           := '';
     end 
    else
      l := LocateMacro(NewMacro);
    k := Copy(k, 0, i - 1) + l + Copy(k, j + 1, Length(k) - j);
   end;
  procedure Looptranslate;
   begin
    i := pos(MACRO, k);
    while i <> 0 do
     begin
      j := i + 1;
      while (k[j] <> MACRO) and (j < Length(k)) do
        Inc(j);

      if k[j] <> MACRO then
       begin
        Result := ERROR;
        Exit;
       end
      else
        Translate;
                        
      i := pos(MACRO, k);
     end;
   end;
begin
  k := LocateMacro(Macroname);
  looptranslate;
  Result := k;
end;


procedure TCakExtension.RePollScriptDirectory;
var 
  sr:        TSearchRec;
  k:         String;
  FileAttrs: Integer;
begin
  k           := AppendSlash(Scriptpath) + SCRIPTFILTER;
  FileAttrs   := 0;
  FileAttrs   := FileAttrs + faAnyFile;
  SupportType := '';
  if FindFirst(k, FileAttrs, sr) = 0 then
   begin
    SupportType := SupportType + SPACE + DOT + RemoveFileExt(sr.Name);
    while (FindNext(sr) = 0) do
        SupportType := SupportType + SPACE + DOT + RemoveFileExt(sr.Name);
   end;
  FindClose(sr);
  SupportType := Uppercase(Supporttype);
end;


procedure Register;
begin
  //RegisterComponents('QZip', [TCakExtension]);
end;

end.
