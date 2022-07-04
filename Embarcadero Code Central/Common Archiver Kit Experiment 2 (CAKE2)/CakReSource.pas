unit CakReSource;

interface

uses
  Windows, Messages, SysUtils, Classes, CakDefs2, CakArchiver,
  ResourceCompUnit, RsSupp, ArchiveHeadersUnit, Contnrs;

const CanExtract = TRUE;
      CanAdd = TRUE;
      CanList = TRUE;
      CanSFX = FALSE;
      Dllname = 'dummydll.dll';
type
  TCakReSource = class(TCakArchiver)
  private
    { Private declarations }
    Stopping : boolean;

  protected
    { Protected declarations }
  public
    { Public declarations }
    RsDir: TResource;
    Cakdir : TComponent;
    procedure SetCakdir(pCakdir : TComponent); override;
    procedure Process(dowhat : WorkType); override;
    function Cando(aWork: WorkType): Boolean; override;
    function Def_TreatAs : string; override;
    procedure Load_DLL; override;
    procedure UnLoad_DLL; override;
    function DllExists : Boolean; override;
    procedure DoStop(Stopp: Boolean); override;

    procedure RsDirAddLog(Sender: TObject; s: string);
    procedure RsDirCDChange(Sender: TObject);
  published
    { Published declarations }
    property Stop: Boolean read Stopping write DoStop;
  end;

procedure Register;

implementation
uses CakUtils, Cakdir2;
var Total_Unpacked, TotalProgress : integer;
procedure TCakReSource.SetCakdir(pCakdir : TComponent);
begin
    Cakdir := TCakdir2(pCakdir);
end;
procedure TCakReSource.DoStop(Stopp: Boolean);
begin

end;

procedure TCakReSource.Process(dowhat : WorkType);
var
  aList:   TList;
  i:      Integer;
  k:      String;
  ColMan: TObjectList; // ## GC replacing TObjList;
  DummyStrings: TStrings;
begin
  Load_DLL;
  with (Cakdir as TCakdir2) do
  begin
  if RsDir.ArchiveMan.archive_file_full_path <> ArchiveName then
   begin
    RsDir.ArchiveMan.TempDir := TempPath;
    RsDir.ArchiveMan.OpenArchive(ArchiveName, True);
   end;

  case doWhat of
    wtLoadContents:
     begin {DoNothing}
     end;
    wtAdd: 
     begin
      DummyStrings := TStringList.Create;
      RsDir.ArchiveMan.use_folder_names := AddOptions.Add_UsePath;
      for i := 0 to AddOptions.Add_Files.Count - 1 do
       begin
        DummyStrings.Clear;
        DummyStrings.Add(ExtractFileName(AddOptions.Add_Files.Strings[i]));
        RsDir.ArchiveMan.AddFiles(DummyStrings,
          ExtractFilePath(AddOptions.Add_Files.Strings[i]));
       end;
      DummyStrings.Free;
     end;
    wtExtract: 
     begin
      RsDir.ArchiveMan.dest_dir := ExtractOptions.Extr_to;
      RsDir.ArchiveMan.use_folder_names := False; //Extract_sc.Usefolder;
      aList   := TList.Create;
      ColMan := TObjectList.Create;
      ColMan.Add(TNameColDataExtr.Create);
      try
        for i := 0 to Total_Contents - 1 do
         begin
          with RsDir.ArchiveMan.ArchiveFile do
            k := TColDataExtr(ColMan[0]).Extract(TCentralFileHeader(CentralDir[i]));

          if ExtractOptions.Extr_ExtractAll or
            Archive_Contents[Locate(k)]._Selected then
            aList.Add(RsDir.ArchiveMan.ArchiveFile.CentralDir[i]);
         end;
        RsDir.ArchiveMan.ExtractList(aList, Total_Unpacked, TotalProgress);
      finally
        aList.Free;
        ColMan.Free;
        if Assigned(FOnProg) then
          FOnProg(NIL, '', Total_Unpacked, Total_Unpacked);
       end;
     end
    else if Assigned(FOnMsg) then
      FOnMsg(Nil, 0, CODE_NOSUPPORT, Msg_Error, ERR_NOSUPPORT);
   end;
   end;
end;


function TCakReSource.DllExists : boolean;
begin
        Result := TRUE;
end;

function TCakReSource.Cando(aWork: WorkType): Boolean;
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

function TCakReSource.Def_TreatAs : string;
begin
end;

procedure TCakReSource.Load_DLL;
begin
  if not Assigned(RsDir) then
    RsDir := TResource.Create(Self);
  RsDir.OnaddLog           := RsDirAddLog;
  RsDir.OnCentralDirChange := RsDirCDChange;
end;

procedure TCakReSource.UnLoad_DLL;
begin
  if not Assigned(RsDir) then Exit;
  RsDir.OnaddLog := NIL;
  RsDir.Free;
  RsDir := NIL;
end;

procedure TCakReSource.RsDirAddLog(Sender: TObject; s: string);
begin
  with (Cakdir as TCakdir2) do
  if Assigned(FOnMsg) then
    FOnMsg(Sender, 0, CODE_UNDEFERROR, Msg_Unknown, s);
end;

procedure TCakReSource.RsDirCDChange(Sender: TObject);
var
  i, loc: Integer;
  CentralFileHeader: TCentralFileHeader;
  ColMan: TObjectList; // ## GC
  k:      String;
begin
  ColMan := TObjectList.Create; // ## GC
  ColMan.Add(TNameColDataExtr.Create);
  ColMan.Add(TSizeColDataExtr.Create);
  ColMan.Add(TTypeNameColDataExtr.Create);
  ColMan.Add(TRatioColDataExtr.Create);
  ColMan.Add(TPackedColDataExtr.Create);
  ColMan.Add(TTimeColDataExtr.Create);
  ColMan.Add(TNumBlocksColDataExtr.Create);
  with (Cakdir as TCakdir2) do
  with RsDir.ArchiveMan.ArchiveFile do
   begin
    Total_Contents := CentralDir.Count;
    SetLength(Archive_Contents, Total_Contents + 5);
    for i := 0 to CentralDir.Count - 1 do
      with Archive_Contents[i] do
       begin
        CentralFileHeader := TCentralFileHeader(CentralDir[i]);
        AddContents(ArchiveName,
        ExtractFilePath(TColDataExtr(ColMan[0]).Extract(CentralFileHeader)),
        ExtractFileName(TColDataExtr(ColMan[0]).Extract(CentralFileHeader)),
        StrToIntDef(TColDataExtr(ColMan[1]).Extract(CentralFileHeader), 1),
        StrToIntDef(TColDataExtr(ColMan[4]).Extract(CentralFileHeader), 1),
        False,StrToDateTime(TColDataExtr(ColMan[5]).Extract(CentralFileHeader)),'FFFFFFFF');
       end;
   end;
  ColMan.Free;
end;



procedure Register;
begin
  //RegisterComponents('QZip', [TCakReSource]);
end;

end.
 