unit CakCabinet;

interface

uses
  Windows, Messages, SysUtils, Classes, CakDefs2, CakArchiver, Cabinet, fci, fdi, fcntl;

const CanExtract = TRUE;
      CanAdd = TRUE;
      CanList = TRUE;
      CanSFX = FALSE;
      //Dllname = 'dummydll.dll';
type
  TCakCabinet = class(TCakArchiver)
  private
    { Private declarations }
    Stopping : boolean;

  protected
    { Protected declarations }
  public
    { Public declarations }
    CabWDir: TCabinetWriter;
    CabRDir: TCabinetReader;
    CabFH: TStreamCabinetFileHandler;
    Cakdir : TComponent;
    procedure SetCakdir(pCakdir : TComponent); override;
    procedure Process(dowhat : WorkType); override;
    function Cando(aWork: WorkType): Boolean; override;
    function Def_TreatAs : string; override;
    procedure Load_DLL; override;
    procedure UnLoad_DLL; override;
    function DllExists : Boolean; override;
    procedure DoStop(Stopp: Boolean); override;

    procedure CabRCopyFile(Sender: TObject; const FileName: string;
      UncompressedSize: Integer; Date, Time,
      Attribs: Smallint; var Action: TFileCopyAction;
      var DestFileHandle: Integer);
    procedure CabRDirCloseCopied(Sender: TObject;
      const FileName: string; FileHandle: Integer; Date, Time,
      Attribs: Smallint; FolderIndex: Integer; Execute: Boolean;
      var Abort: Boolean);
    procedure CabWFilePlaced(Sender: TObject; var CabParameters: TCCAB;
      const FileName: string; FileLength: Integer;
      Continuation: Boolean; var AbortProcessing: Boolean);
    procedure CabRNextCab(Sender: TObject;
      const NextCabinetName, NextCabinetDisk: string;
      var CabinetPath: string;
      ErrorIndication: TFDIERROR; var Abort: Boolean);
  published
    { Published declarations }
    property Stop: Boolean read Stopping write DoStop;
  end;

procedure Register;

implementation
uses CakUtils2, Cakdir2, Dialogs, Controls;
var CabMode: CabModeType;
    Cab_Extr_To: string;
    
procedure TCakCabinet.SetCakdir(pCakdir : TComponent);
begin
    Cakdir := TCakdir2(pCakdir);
end;
procedure TCakCabinet.DoStop(Stopp: Boolean);
begin
end;

procedure TCakCabinet.Process(dowhat : WorkType);
var 
  i, j: Integer;
  aFileList, apathlist: TStrings;
  k:    String;
begin
  Load_DLL;
  with (Cakdir as TCakdir2) do
  case doWhat of
    wtLoadContents: 
     begin
      CabMode        := _CFList;
      Total_Contents := 0;
      DirectoryList.Clear;
      CabRDir.ExtractFiles(Archivename, GrabTempPath, _O_RDWR);
     end;
    wtExtract: 
     begin
      CabMode := _CFExtract;
        if ExtractOptions.Extr_ExtractAll or
          (Get_Selected_Count > 0) then
         begin
          Cab_Extr_To   := NewTempPath;
          TotalProgress := 0;
          for j := 0 to Total_Contents - 1 do
            if ExtractOptions.Extr_ExtractAll or Archive_Contents[j]._Selected then
              if not DirectoryExists(Cab_Extr_To + Archive_Contents[j]._FileDefPath) then
                MakeDirectory(Cab_Extr_To + Archive_Contents[j]._FileDefPath);

          CabRDir.ExtractFiles(Archivename, Cab_Extr_To, 0);
          UNLOAD_DLL;
          for j := 0 to Total_Contents - 1 do
            if ExtractOptions.Extr_ExtractAll or Archive_Contents[j]._Selected then
              with Archive_Contents[j] do
                if FileExists(Cab_Extr_To + _FileDefPath + _FileName) then
                  if ExtractOptions.Extr_DirNames = True then
                   begin
                    if not DirectoryExists(ExtractOptions.Extr_to +
                      _FileDefPath) then
                      MakeDirectory(ExtractOptions.Extr_to +
                        _FileDefPath);
                    MoveFile(PChar(Cab_Extr_To +
                      _FileDefPath + _FileName),
                      PChar(ExtractOptions.Extr_to + _FileDefPath + _FileName));
                   end 
              else
                MoveFile(PChar(Cab_Extr_To +
                  _FileDefPath + _FileName), PChar(ExtractOptions.Extr_to + _FileName));

          for j := 0 to Total_Contents - 1 do
            if ExtractOptions.Extr_ExtractAll or Archive_Contents[j]._Selected then
              with Archive_Contents[j] do
                if DirectoryExists(Cab_Extr_To + _FileDefPath) then
                  RemoveDirectory(PChar(Cab_Extr_To + _FileDefPath));
                             
          RemoveDirectory(PChar(Cab_Extr_To));
         end;
     end;
    wtTest: 
     begin
      SelectALL;
      CabMode := _CFExtract;
      Cab_Extr_To := NewTempPath;
      MakeDirectory(Cab_Extr_To);
      TotalProgress := 0;
      for j := 0 to Total_Contents - 1 do
          if Archive_Contents[j]._Selected then
            if not DirectoryExists(Cab_Extr_To + Archive_Contents[j]._FileDefPath) then
              MakeDirectory(Cab_Extr_To + Archive_Contents[j]._FileDefPath);

        CabRDir.ExtractFiles(Archivename, Cab_Extr_To, 0);
        UnLoad_DLL;

        for j := 0 to Total_Contents - 1 do
          if Archive_Contents[j]._Selected then
            with Archive_Contents[j] do
             begin
              if Assigned(FOnMsg) then
              if FileExists(Cab_Extr_To + _FileDefPath + _FileName) then
                  FOnMsg(NIL, Error, CODE_PROCESSOK, Msg_OK,
                     Format(ERR_PROCESSOK,[_Filedefpath + _FileName])) 
              else
                  FOnMsg(NIL, Error, CODE_PROCESSFAIL, Msg_Error,
                     Format(ERR_PROCESSFAIL,[_Filedefpath + _FileName]));
             end;

        for j := 0 to Total_Contents - 1 do
          if Archive_Contents[j]._Selected then
            with Archive_Contents[j] do
              if FileExists(Cab_Extr_To + _FileDefPath + _FileName) then
                DeleteFile(PChar(Cab_Extr_To + _FileDefPath + _FileName));


        for j := 0 to Total_Contents - 1 do
          if Archive_Contents[j]._Selected then
            with Archive_Contents[j] do
              if DirectoryExists(Cab_Extr_To + _FileDefPath) then
                RemoveDirectory(PChar(Cab_Extr_To + _FileDefPath));

        RemoveDirectory(PChar(Cab_Extr_To));
     end;

    wtAdd: 
     begin
      if Total_Contents > 0 then
        if MessageDlg('Are you sure? Origional Cab content will be removed!',
          mtWarning, [mbYes, mbNo], 0) = mrNo then
          Exit;

      aFileList := TStringList.Create;
      aFileList.Clear;
      apathlist := TStringList.Create;
      apathlist.Clear;
      TotalProgress := 0;
      try
        //if  then
        for i := 0 to AddOptions.Add_Files.Count - 1 do
          aFileList.AddStrings(PollFileList(AddOptions.Add_Files.Strings[i],
            AddOptions.Add_SubDir));
        AddOptions.Add_Files.Clear;
        AddOptions.Add_Files.AddStrings(aFileList);
        aFileList.Clear;

        for i := 0 to AddOptions.Add_Exclude.Count - 1 do
         begin
          j := AddOptions.Add_Files.IndexOf(AddOptions.Add_Exclude.Strings[i]);
          if j <> -1 then AddOptions.Add_Files.Delete(j);
         end;

        for i := 0 to AddOptions.Add_Files.Count - 1 do
         begin
          aFileList.Add(AddOptions.Add_Files.Strings[i]);
          apathlist.Add(ExtractFileName(AddOptions.Add_Files.Strings[i]));
         end;

        CabWDir.Open(Archivename, 'Disk', 0, 900000, 60);

        for i := 0 to aFileList.Count - 1 do
         begin
          k := apathlist.Strings[i];

          if AddOptions.Add_DosFormat then
            k := ExtractFileName(ConvFNameToDos(aFileList.Strings[i]));

          if AddOptions.Add_UsePath then
            k := ModifySlash(RemoveDrive(ExtractFilePath(aFileList.Strings[i]) + k), '\',
              '/');

          case AddOptions.Add_CompLevel of
            0: CabWDir.AddFile(aFileList.Strings[i], k, [], MakeNoCompression);
            1, 2: CabWDir.AddFile(aFileList.Strings[i], k, [], MakeMsZipCompression);
            3..9: CabWDir.AddFile(aFileList.Strings[i], k, [],
                MakeLzxcompression(AddOptions.Add_CompLevel + 12));
           end;
         end;

        CabWDir.FlushCabinet(True);
        CabWDir.Close;

      finally
        aFileList.Free;
        apathlist.Free;
       end;
     end;

   end;
end;


function TCakCabinet.DllExists : boolean;
begin
        Result := True;
end;

function TCakCabinet.Cando(aWork: WorkType): Boolean;
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

function TCakCabinet.Def_TreatAs : string;
begin
end;

procedure TCakCabinet.Load_DLL;
begin
  if not Assigned(CabFH) then
    CabFH := TStreamCabinetFileHandler.Create(Self);
  if not Assigned(CabWDir) then
   begin
    CabWDir := TCabinetWriter.Create(Self);
    CabWDir.FileHandler := CabFH;
    CabWDir.OnFilePlacedEvent := CabWFilePlaced;
   end;
  if not Assigned(CabRDir) then
   begin
    CabRDir := TCabinetReader.Create(Self);
    CabRDir.FileHandler := CabFH;
    CabRDir.OnCloseCopiedFile := CabRDirCloseCopied;
    CabRDir.OnCopyFile := CabRCopyFile;
    CabRDir.OnNextCabinet := CabRNextCab;
   end;
  CabMode := _CFList;
  stopping := false;
end;

procedure TCakCabinet.UnLoad_DLL;
begin
  if Assigned(CabWDir) then
   begin
    CabWDir.Free;
    CabWDir := NIL
   end;
  if Assigned(CabRDir) then
   begin
    CabRDir.Free;
    CabRDir := NIL
   end;
  if Assigned(CabFH) then
   begin
    CabFH.Free;
    CabFH := NIL
   end;
end;


procedure TCakCabinet.CabRCopyFile(Sender: TObject; const FileName: string;
  UncompressedSize: Integer; Date, Time,
  Attribs: Smallint; var Action: TFileCopyAction;
  var DestFileHandle: Integer);
var
  i: Integer;
begin
  with (Cakdir as TCakdir2) do
  case CabMode of
    _CFList:
     begin
      Inc(Total_Contents);
      SetLength(Archive_Contents, Total_Contents + 5);
      AddContents(ArchiveName,ExtractFilePath(ModifySlash(FileName, '/', '\')),
      ExtractFileName(ModifySlash(FileName, '/', '\')),
      UncompressedSize, UncompressedSize, False,Now,
      'FFFFFF'
      );
     end;
    _CFExtract: if Stopping then Action := fcaSkip
      else
       begin
        i := Locate(ModifySlash(FileName));

        if (i = -1) then Action := fcaSkip 
        else if (Archive_Contents[i]._Selected or ExtractOptions.Extr_ExtractAll) then
         begin
          TotalProgress := TotalProgress + UnCompressedSize;
          if Assigned(FOnProg) then
            FOnProg(NIL, FileName, UncompressedSize, UnCompressedSize);
          Action := fcaDefaultCopy;
         end 
        else
          Action := fcaSkip;
       end;
   end;
end;

procedure TCakCabinet.CabRDirCloseCopied(Sender: TObject;
  const FileName: string; FileHandle: Integer; Date, Time,
  Attribs: Smallint; FolderIndex: Integer; Execute: Boolean;
  var Abort: Boolean);
begin
  with (Cakdir as TCakdir2) do
  begin
  if Assigned(FOnProg) then
    FOnProg(Sender, FileName, 0, 0);
  if Assigned(FOnMsg) then
    FOnMsg(NIL, Error, CODE_PROCESSOK, Msg_OK,
                     Format(ERR_PROCESSOK,[FileName]));
  Abort := Stopping;
  end;
end;

{
procedure TCakCabinet.CabWGetOpenInfo(Sender: TObject; const FileName: String; var Date, Time, Attributes: Smallint;
var FileHandle, ResultCode: Integer);
begin
        if assigned(FOnProg) then
                FOnProg(nil,Filename,0,0);
        if assigned(FOnMsg) then
        Case ResultCode of
        0 : FOnMsg(Sender,ResultCode,NOERR);
        1 : FOnMsg(Sender,ResultCode,ERR_CANTREAD);
        // Failure opening file to be stored in cabinet
        //  erf.erfTyp has C run-time *errno* value
        2 : FOnMsg(Sender,ResultCode,ERR_CANTREAD);
        // Failure reading file to be stored in cabinet
        //  erf.erfTyp has C run-time *errno* value
        3 : FOnMsg(Sender,ResultCode,ERR_NOMEMORY);
        // Out of memory in FCI
        4 : FOnMsg(Sender,ResultCode,ERR_COPYTEMP);
        // Could not create a temporary file
        //  erf.erfTyp has C run-time *errno* value
        5 : FOnMsg(Sender,ResultCode,ERR_NOSUPPORT );
        // Unknown compression type
        6 : FOnMsg(Sender,ResultCode,ERR_WRITE  );
        // Could not create cabinet file
        //  erf.erfTyp has C run-time *errno* value
        7 : FOnMsg(Sender,ResultCode,ERR_USERSKIP  );
        // Client requested abort
        8 : FOnMsg(Sender,ResultCode,ERR_WRITE  );
        // Failure compressing data
        end;
end;        }

procedure TCakCabinet.CabWFilePlaced(Sender: TObject; var CabParameters: TCCAB;
  const FileName: string; FileLength: Integer;
  Continuation: Boolean; var AbortProcessing: Boolean);
begin
  with (Cakdir as TCakdir2) do
  begin
  Inc(TotalProgress, FileLength);
  if Assigned(FOnMsg) then
    FOnMsg(NIL, Error, CODE_PROCESSOK, Msg_OK,
                     Format(ERR_PROCESSOK,[FileName]));
  if Assigned(FOnProg) then
    FOnProg(NIL, FileName, FileLength, FileLength);
  abortProcessing := Stopping;
  end;
end;

procedure TCakCabinet.CabRNextCab(Sender: TObject;
  const NextCabinetName, NextCabinetDisk: string; var CabinetPath: string;
  ErrorIndication: TFDIERROR; var Abort: Boolean);
var 
  Opendialog: TOpendialog;
begin
  Opendialog := TOpendialog.Create(NIL);
  Opendialog.Title := 'Please locate ' + NextCabinetDisk + ' (' +
    NextCabinetName + ')';
  Opendialog.Filter := 'Cabinet|*.cab';
  Abort := False;
  if opendialog.Execute then
    cabinetpath := Opendialog.FileName 
  else
    Abort := True;
end;


procedure Register;
begin
  //RegisterComponents('QZip', [TCakCabinet]);
end;

end.
 