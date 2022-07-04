unit CakDir2;
{$R Cake2.res}
// Common Archiver Kit Experiment(CAKE)2
// Common Interface for Compression/Decompression components.

//Copyright (C) Joseph Leung 2001 - 2003 (lycj@yahoo.com)
//
//This library is free software; you can redistribute it and/or
//modify it under the terms of the GNU Lesser General Public
//License as published by the Free Software Foundation; either
//version 2.1 of the License, or (at your option) any later version.
//
//This library is distributed in the hope that it will be useful,
//but WITHOUT ANY WARRANTY; without even the implied warranty of
//MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
//Lesser General Public License for more details.
//
//You should have received a copy of the GNU Lesser General Public
//License along with this library; if not, write to the Free Software
//Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

{CAKE 2}
{$DEFINE NOT_CMD_LINE}
{.$DEFINE USE_PLUGIN_ONLY}
interface
uses
  Windows, Messages, Controls, SysUtils, Classes, CakArchiver, CakDefs2, DateUtils;
type
  CArchiver = class of TCakArchiver;
  CakArchiverType = record
                      ArchiveType : string;
                      CakArchiverClass : CArchiver;
                      CakArchiver : TCakArchiver;
                      SupportExtension : String;
                      SupportWork : SupportWorkType;
                      BaseType    : SupportType;
                      Enabled : boolean;
                    end;
  ContentArray = array of Contenttype;
  TCakDir2 = class(TComponent)
  private
  Arctype : supporttype;
  ArcNeedPassword : boolean;
  Stopping : boolean;
  EnableSort : boolean;
  SortType : SortbyType;
  SortAccending : boolean;
  TargetSize : integer;

  Listview  : TComponent;
  Treeview  : TComponent;
  Treelist  : TComponent;
  Treecombobox : TComponent;
  procedure DoStopp(stop : boolean);
  procedure LoadArch(filename : string);
  protected
  Completed : integer;
  procedure OnProgEvent(Sender: TObject; FileName: string; FileSize: Longint; JustCompleted: Longint);
  public
    Arcname : string;
    FOnOver    : TCOverEvent;
    FOnPwd     : TCPwdEvent;
    FOnCZipPwd : TCCZIPPwdEvent;
    FOnMsg     : TCMsgEvent;
    FOnProg,
      FonProg2 : TCProgEvent;
    FOnFound   : TCFoundEvent;
    FOnUnkCmd  : TCUnkCmdEvent;

    CakArchiverList : array[1..MaxArchiver] of CakArchiverType;
    TotalArchiverList : integer;

    PercentCompleted : Integer;
    TempPath : string;
    ImageS: TImageList;
    ImageL: TImageList;
    FileType, FileExt, DirectoryList, SubDirectoryList, 
          Abouttext, NewDirList, ScriptParam : TStringList;
    MRUList : TStrings;
    MaxMRU : integer;
    Total_Contents, FullContentCount : integer;
    Archive_Contents, temp_Contents, Full_Contents : ContentArray;
    SubDir_RootPath : string;
    exploremode : boolean;
    OverwriteAll: Integer;
    TimeStrFormat : string;
    TotalProgress : integer;
    Password : string;
    CZPassKey : CZKeyType;
    Extractoptions : ExtractOptionsType;
    EncryptOptions : EncryptOptionsType;
    RenameOptions : RenameOptionsType;
    AddOptions : AddOptionsType;
    SfxOptions : SfxOptionsType;
    FinderOptions : FinderOptionsType;
    FilelistOptions : FilelistOptionstype;

    versioncontrol : boolean;
    BaseDir : string;
    
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddContents(
       FileArchive, FileFullPath, FileDefPath, FileName : String;
       FileSize, FilePackedSize : Integer;
       Encrypted        : Boolean;
       FileTime         : TDateTime;
       FileCRC          : String                        ); overload;
    procedure AddContents(
       FileArchive, FileDefPath, FileName : String;
       FileSize, FilePackedSize : Integer;
       Encrypted        : Boolean;
       FileTime         : TDateTime;
       FileCRC          : String                        ); overload;
    procedure AddContents(
       FileArchive, FileDefPath, FileName : String;
       FileSize, FilePackedSize : Integer               ); overload;
    procedure AddContents(
       FileArchive, FileDefPath, FileName : String      ); overload;
    procedure Prepare_Archiver;
    procedure Add_Archiver(ArchiverClass : CArchiver; Archiver : TCakArchiver; BaseTyp : SupportType; SupportExt : String; SupportWk : SupportWorkType); overload;
    procedure Add_Archiver(ArchiverClass : CArchiver; BaseTyp : SupportType; SupportExt : String; SupportWk : SupportWorkType; ArchType : string); overload;
    procedure Add_Plugin(Dllpath : string);
    procedure Enable_Archiver(ArchiverClass : CArchiver; BaseTyp : SupportType);
    function Locate_Archiver(ext : string; aWork : worktype) : integer; overload;
    function Locate_Archiver(arctype : SupportType; aWork : worktype) : integer; overload;
    function Locate_Archiver(arctype : SupportType) : integer; overload;
    function GetarcString(atype : supporttype) : string;
    function ReturnIconType(FileName: string): Integer;
    function AskOverwrite(forfile: string): Boolean;

    function cando(fn : string; aWork : worktype) : boolean;
    procedure Process(aWork : worktype);
    procedure Sort(accending : boolean; atype: Sortbytype);
    procedure Extract;
    procedure Add;
    procedure Test;
    procedure Delete;
    procedure SFX;
    procedure Encrypt;
    procedure List;overload;
    procedure List(Mask : string; Subdir : boolean);overload;
    procedure List(SearchRec : SearchRecType); overload;
    procedure New(filename : string);
    procedure Load(filename : string);
    procedure Close;
    function ArchiveContents(index : integer) : ContentType;

    //Selected List function
    procedure UnSelectALL;
    procedure Select(FileMasks, Filearchive: string); overload;
    procedure Select(filename : tstrings; archivename : string); overload;
    procedure Select(Index : integer); overload;
    procedure SelectALL;

    function GrabArchivePath : string;
    function Get_Selected_Count : integer;
    function Get_Selected_Size : Int64;
    //function Get_Directory_Size(directory : string) : Longint;
    function Get_Target_Size : Int64;
    function Get_Selected_CompSize : Int64;
    function Get_Top_Selected : string;
    function Get_Top_index : integer;
    function Get_Total_Count : integer;
    function Get_Total_Size : Int64;
    function Get_Total_Packed_Size : Int64;
    
    function Locate(filename : string) : integer; overload; //Get_Archive_Code
    function Locate(filename : string; checkdir : boolean) : integer; overload;
    function GetArchiveType(filename : string) : supporttype;
    function NewTempPath: string;

    procedure HotEdit(FileName: string);

    function InitContentType: ContentType;

    procedure _SetListview(aListview : TComponent);
    procedure _SetTreeview(aTreeview : TComponent);
    procedure _SetTreeList(aTreelist : TComponent);
    procedure _SetTreeComboBox(aTreecombo : TComponent);
    procedure ReloadCAK(ReloadLv, ReloadTv, ReloadTl : boolean);

    function GetComment : string;
    procedure SetComment(text : string);

    function CanExtract : boolean;
    function CanAdd : boolean;

    function OpenFilter_All       : string;
    function OpenFilter_Seperated : string;
    function OpenFilter           : string;
  published
    property ArchiveName: string read Arcname write LoadArch;
    property ArchiveType : supporttype read arctype write arctype;
    property ArchiveNeedPassword : boolean read arcNeedPassword write arcNeedPassword;
    property ArchiveComment : string read GetComment write SetComment;
    property SortAuto : boolean read EnableSort write EnableSort;
    property SortIndex : SortbyType read SortType write SortType;
    property SortAccend : boolean read SortAccending write SortAccending;
    property SelectedCount : integer read Get_Selected_Count;
    property SelectedIndex : integer read Get_Top_index;
    property ItemIndex : integer read Get_Top_index;
    property SelectedSize : Int64 read Get_Selected_Size;
    property OnCMessage: TCMsgEvent read FOnMsg write FOnMsg;
    property OnCProgress: TCProgEvent read FOnProg2 write FOnProg2;
    property OnCArchiveFound: TCFoundEvent read FOnFound write FOnFound;
    property OnCOverwrite: TCOverEvent read FOnOver write FOnOver;
    property OnCPassword: TCPwdEvent read FOnPwd write FOnPwd;
    property OnCCZipPassword : TCCZIPPwdEvent read FOnCZipPwd write FOnCZipPwd;
    property OnCUnkCmd: TCUnkCmdEvent read FOnUnkCmd write FOnUnkCmd;
    property Stop : boolean write DoStopp;
  end;
procedure Register;
implementation
uses {$IFNDEF USE_PLUGIN_ONLY} CakDelphiZip, CakRtDunAce, CakRtDunRar, CakCmArc, CakCmArcArj, CakCabinet,
     CakQuakePak, CakDescentHog, CakSqxSdk, CakReSource, {CakIndy8, }CakDfUnRar,
     CakAbbrevia, CakCryptoZip, CakQuickZipEncode, CakZipForge, {$ENDIF}
     CakPlugins, CakExtension, Masks, CakUtils2, Graphics, Dialogs, Shellapi,
     CakListView2, CakTreeView2, CakTreeList2, CakTreeComboBox2;

procedure TCakdir2.Prepare_Archiver;
// A List of archive format details
// http://www.nemu.to/Beta/list_t.html
begin
    {$IFNDEF USE_PLUGIN_ONLY}
    Add_Archiver(TCakArchiver      ,_WIT ,'---------------'   ,sNone        ,'Unknown Archive     ');
    Add_Archiver(TCakExtension     ,_EXT ,'---------------'   ,sAllExceptSfx,'Cak Extensions      ');
    Add_Archiver(TCakCryptoZip     ,_CZIP,DefaultTreatAsCzip  ,sEncrypt     ,'Crypto Zip 2          ');
    Add_Archiver(TCakDelphiZip     ,_ZIP ,DefaultTreatAsZip   ,sAll         ,'Zip Archive            ');
    Add_Archiver(TCakDelphiZip     ,_SFX ,'.EXE'              ,sExtractOnly ,'Zip SFX               ');
    Add_Archiver(TCakDfunRar       ,_RAR ,DefaultTreatAsRar   ,sExtractOnly ,'Rar Archive           ');
    Add_Archiver(TCakRtDunAce      ,_ACE ,DefaultTreatAsAce   ,sExtractOnly ,'Ace Archive           ');
    Add_Archiver(TCakRtDunRar      ,_RAR ,DefaultTreatAsRar   ,sExtractOnly ,'Rar Archive      ');
    Add_Archiver(TCakCabinet       ,_CAB ,DefaultTreatAsCab   ,sAllExceptSfx,'Cabinet Archive     ');
    Add_Archiver(TCakCmArc         ,_LHA ,DefaultTreatAsLha   ,sAllExceptSfx,'Lha Archive           ');
    Add_Archiver(TCakCmArcArj      ,_ARJ ,DefaultTreatAsArj   ,sExtractOnly ,'Arj Archive            ');
    Add_Archiver(TCakCmArc         ,_TAR ,DefaultTreatAsTar   ,sAllExceptSfx,'Tar Archive          ');
    Add_Archiver(TCakCmArc         ,_TGZ ,DefaultTreatAsTGZ   ,sAllExceptSfx,'Tgz Archive         ');
    Add_Archiver(TCakCmArc         ,_BZ2 ,DefaultTreatAsBz2   ,sAllExceptSfx,'Binary Zip Archive');
    Add_Archiver(TCakCmArc         ,_BZA ,DefaultTreatAsBza   ,sAllExceptSfx,'Binary Zip Archive');
    Add_Archiver(TCakCmArc         ,_7Z  ,DefaultTreatAs7z    ,sAllExceptSfx,'Seven Zip Archive ');
    Add_Archiver(TCakCmArc         ,_GCA ,DefaultTreatAsGca   ,sExtractOnly ,'Gca Archive           ');
    Add_Archiver(TCakCmArc         ,_BEL ,DefaultTreatAsBel   ,sExtractOnly ,'Belon Archive        ');

  //Add_Archiver(TCakCmArc         ,_IMP ,DefaultTreatAsImp   ,sExtractOnly ,'Imp Archive         ');
  //Add_Archiver(TCakCmArc         ,_ARC ,DefaultTreatAsArc   ,sExtractOnly ,'Arc Archive         ');
    Add_Archiver(TCakCmArc         ,_ZOO ,DefaultTreatAsZoo   ,sExtractOnly ,'Zoo Archive         ');
    Add_Archiver(TCakCmArc         ,_CPT ,DefaultTreatAsCpt   ,sExtractOnly ,'Cpt Archive           ');
    Add_Archiver(TCakCmArc         ,_PIT ,DefaultTreatAsPit   ,sExtractOnly ,'Pit Archive             ');
    Add_Archiver(TCakCmArc         ,_ARG ,DefaultTreatAsArg   ,sExtractOnly ,'Arg Archive           ');
    Add_Archiver(TCakCmArc         ,_ASD ,DefaultTreatAsAsd   ,sExtractOnly ,'Asd Archive           ');
    Add_Archiver(TCakCmArc         ,_DZ  ,DefaultTreatAsDz    ,sExtractOnly ,'Dzip Archive          ');
    Add_Archiver(TCakCmArc         ,_SH  ,DefaultTreatAsSH    ,sExtractOnly ,'Shar Archive         ');
    Add_Archiver(TCakCmArc         ,_ZAC ,DefaultTreatAsZac   ,sExtractOnly ,'Zac Archive         ');
    Add_Archiver(TCakCmArc         ,_BIN ,DefaultTreatAsBin   ,sExtractOnly ,'Mac Binary            ');
    Add_Archiver(TCakCmArc         ,_CAR ,DefaultTreatAsCar   ,sExtractOnly ,'Compress archive   ');
    Add_Archiver(TCakCmArc         ,_FRZ ,DefaultTreatAsFrz   ,sExtractOnly ,'Freeze Archive       ');
    Add_Archiver(TCakCmArc         ,_SPL ,DefaultTreatAsSpl   ,sExtractOnly ,'Splint Archive       ');
    Add_Archiver(TCakCmArc         ,_JAM ,DefaultTreatAsJam   ,sExtractOnly ,'Jam Archive           ');

    Add_Archiver(TCakCmArc         ,_BQX ,DefaultTreatAsBqx   ,sExtractOnly ,'Bqx Encode         ');
    Add_Archiver(TCakCmArc         ,_UUE ,DefaultTreatAsUue   ,sExtractOnly ,'Uue Encode         ');
    Add_Archiver(TCakCmArc         ,_XXE ,DefaultTreatAsXxe   ,sExtractOnly ,'Xxe Encode         ');
    Add_Archiver(TCakCmArc         ,_B64 ,DefaultTreatAsB64   ,sExtractOnly ,'B64 Encode         ');

    Add_Archiver(TCakSqxSdk        ,_SQX ,DefaultTreatAsSqx   ,sAllExceptSfx,'Sqx Archive          ');
    Add_Archiver(TCakQuakePak      ,_PAK ,DefaultTreatAsPak   ,sExtractOnly ,'Pak Archive         ');
    Add_Archiver(TCakDescentHog    ,_HOG ,DefaultTreatAsHog   ,sExtractOnly ,'Hog Archive          ');
    Add_Archiver(TCakReSource      ,_RS  ,DefaultTreatAsRs    ,sAllExceptSfx,'Rs Archive            ');

    Add_Archiver(TCakAbbrevia      ,_ZIP ,DefaultTreatAsZip   ,sAllExceptSfx,'Zip Archive        ');
    Add_Archiver(TCakAbbrevia      ,_TAR ,DefaultTreatAsTar   ,sAllExceptSfx,'Tar Archive        ');
    Add_Archiver(TCakAbbrevia      ,_TGZ ,DefaultTreatAsTGZ   ,sAllExceptSfx,'Tgz Archive        ');
    Add_Archiver(TCakZipForge      ,_ZIP ,DefaultTreatAsZip   ,sAllExceptSfx,'Zip Archive        ');
  //Add_Archiver(TCakIndy          ,_UUE ,DefaultTreatAsUUE   ,sAllExceptSfx,'Uue Encode         ');
  //Add_Archiver(TCakIndy          ,_XXE ,DefaultTreatAsXXE   ,sAllExceptSfx,'Xxe Encode         ');
  //Add_Archiver(TCakIndy          ,_B64 ,DefaultTreatAsB64   ,sAllExceptSfx,'B64 Encode         ');
    Add_Archiver(TCakQuickZipEncode,_QZE ,DefaultTreatAsQze   ,sEncrypt     ,'Qzip Encode        ');
    {$ENDIF}
end;

constructor TCakdir2.Create(AOwner: TComponent);
procedure AddFolderIcon;
var TmpBmp16,TmpBmp32   : TBitmap;
begin
   TmpBmp16 := TBitmap.Create;
   try
   TmpBmp16.LoadFromResourceName(HInstance,'FOLDER_CLOSE_16');
   ImageS.Add(TmpBmp16,nil);
   finally
   TmpBmp16.Free;
   end;
                                          
   TmpBmp32 := TBitmap.Create;
   try
   TmpBmp32.LoadFromResourceName(HInstance,'FOLDER_CLOSE_32');
   ImageL.Add(TmpBmp32,nil);
   finally
   TmpBmp32.Free;
   end;
   FileType.add('<Directory>');
   FileExt.add('*DIR*');
end;
begin
    inherited Create(AOwner);
    ExploreMode   := False;
    ImageS        := TImageList.Create(self);
    ImageS.Width  := 16;
    ImageS.Height := 16;
    ImageS.BkColor := $FFFFFE;
    ImageL        := TImageList.Create(self);
    ImageL.Width  := 32;
    ImageL.Height := 32;
    FileType      := TStringList.Create();
    FileExt       := TStringList.Create();
    Password      := '';
    AddFolderIcon;

    NewDirList    := TStringList.Create();
    DirectoryList := TStringList.Create();
    DirectoryList.Sorted := True;
    SubDirectoryList := TStringList.Create();
    SubDirectoryList.Sorted := True;
    MRUList       := TStringList.Create();
    FonProg := OnProgEvent;
    ExtractOptions.Extr_ArcInArc := False;
    AddOptions.Add_Exclude := TStringList.Create();
    AddOptions.Add_Files := TStringList.Create();
    AddOptions.Add_CompLevel := 9;
    ScriptParam   := TStringList.Create();
    MaxMRU          := 9;
    AddOptions.Add_Files.Clear;
    Tag        := StrToIntDef(MINORVER, 0);
    VersionControl := False;
    ExtractOptions.Extr_ExtractAll := False;
    Prepare_Archiver;
end;

destructor TCakdir2.Destroy;
var i : integer;
begin
    ImageS.Free;
    ImageL.Free;
    FileType.Free;
    FileExt.Free;
    MRUList.Free;
    AddOptions.add_files.Free;
    AddOptions.add_exclude.Free;
    NewDirList.free;
    FinderOptions.af_targetname.Free;
    DirectoryList.Free;
    SubDirectoryList.Free;
    for i := 1 to TotalArchiverList do
        CakArchiverList[TotalArchiverList].CakArchiver.UnLoad_DLL;
    inherited Destroy;
end;

procedure TCakdir2.Add_Archiver(ArchiverClass : CArchiver; Archiver : TCakArchiver; BaseTyp : SupportType; SupportExt : String; SupportWk : SupportWorkType);
var Enb : boolean;
begin
    Enb := True;
    if TotalArchiverList = MaxArchiver then exit;
    Inc(TotalArchiverList);
    with CakArchiverList[TotalArchiverList] do
      begin
        CakArchiverClass := ArchiverClass;
        SupportExtension := Uppercase(SupportExt);
        BaseType := BaseTyp;
        SupportWork := SupportWk;
        CakArchiver := Archiver;
        CakArchiver.SetCakdir(Self);
        Enabled := Enb;
      end;
end;

procedure TCakdir2.Add_Archiver(ArchiverClass : CArchiver; BaseTyp : SupportType; SupportExt : String; SupportWk : SupportWorkType; ArchType : string);
var Enb : boolean;
begin
    Enb := (Locate_Archiver(BaseTyp) = 1);
    if TotalArchiverList = MaxArchiver then exit;
    Inc(TotalArchiverList);
    with CakArchiverList[TotalArchiverList] do
      begin
        CakArchiverClass := ArchiverClass;
        SupportExtension := Uppercase(SupportExt);
        BaseType := BaseTyp;
        SupportWork := SupportWk;
        ArchiveType := ArchType;
        CakArchiver := ArchiverClass.Create(Self);
        CakArchiver.SetCakdir(Self);
        Enabled := Enb;
      end;
end;

procedure TCakdir2.Add_Plugin(Dllpath : string);
var CakPlugins : TCakPlugins;
begin
  if fileexists(Dllpath) then
    begin
    CakPlugins := TCakPlugins.Create(Self,self,DLLpath);
    CakPlugins.Tag := 0; //It does nothing
    end;
end;

procedure TCakdir2.Enable_Archiver(ArchiverClass : CArchiver; BaseTyp : SupportType);
var i : integer;
begin
  for i := 1 to TotalArchiverList  do
    with CakArchiverList[i] do
    begin
      if BaseTyp = BaseType then
        if CakArchiverClass = ArchiverClass then
          Enabled := true else
          Enabled := false;
    end;
end;

function TCakdir2.cando(fn : string; aWork : worktype) : boolean;
begin
  Result := (Locate_Archiver(extractfileext(fn),aWork) <> 1);
end;

function TCakdir2.CanExtract : boolean;
begin
  IF ArchiveName <> '' then
  Result := Cando(ArchiveName,wtExtract) else
  Result := False;
end;

function TCakdir2.CanAdd : boolean;
begin
  IF ArchiveName <> '' then
  Result := Cando(ArchiveName,wtAdd) else
  Result := False;
end;

function TCakdir2.Locate_Archiver(arctype : SupportType) : integer;
var i : integer;
begin
   Result := 1;
   i := 1;
   while (i < TotalArchiverList) and (arctype <> CakArchiverList[i].BaseType)  do
        Inc(i);
  if (arctype = CakArchiverList[i].BaseType) then
        Result := i;
end;

function TCakdir2.Locate_Archiver(arctype : SupportType; aWork : worktype) : integer;
var i : integer;
function support(i : integer) : boolean;
begin
  result := false;
  if CakArchiverList[i].BaseType = arctype then
    result := true;
  if result then
     result :=  CakArchiverList[i].CakArchiver.Cando(aWork);
end;
begin
   Result := 1;
   i := 1;
   while (i < TotalArchiverList) and (not support(i)) do
        Inc(i);
  if (support(i)) then
        Result := i;
end;


function TCakdir2.Locate_Archiver(ext : string; aWork : Worktype) : integer;
var i : integer;
function support(i : integer) : boolean;
begin
  result := false;
  if Pos(uppercase(ext),CakArchiverList[i].SupportExtension) <> 0 then
    result := true;

  if result then
    begin
     CakArchiverList[i].CakArchiver.SetCakdir(Self);
     result := CakArchiverList[i].CakArchiver.Cando(aWork);
    end;
  if result then
     result := CakArchiverList[i].Enabled;
end;

begin
   Result := 1;
   i := 1;
   while (i <= TotalArchiverList) and (not support(i)) do
        Inc(i);
  if (support(i)) then
        Result := i;
end;

function TCakdir2.GetarcString(atype : supporttype) : string;
var astrings : tstrings;
begin
        aStrings := TStringList.create;
        astrings.CommaText := CakArchiverList[Locate_Archiver(aType,wtLoadContents)].SupportExtension;
        if astrings.count > 0 then
        result := astrings.strings[0];
        aStrings.free;
end;


procedure TCakdir2.List;
var i,j : integer;
    k : string;
    Dirsize : Int64;
begin
  Total_Contents := 0;
  ReloadCak(True,True,True);
  BaseDir := '';
  if FullContentCount = 0 then
   begin
    DirectoryList.Clear;
    SubDirectoryList.Clear;
    process(wtLoadContents);
    FullContentCount := Total_Contents;
    Full_Contents := Archive_Contents;

    for i := 0 to DirectoryList.Count -1 do
     begin
      k := ExtractRootPath(DirectoryList.strings[i]);
      if SubDirectoryList.IndexOf(k) = -1 then
        begin
           DirSize := Int64(DirectoryList.Objects[i]);
           SubDirectoryList.AddObject(k,TObject(DirSize));
        end else
        begin
           j := SubDirectoryList.IndexOf(k);
           DirSize := Int64(DirectoryList.Objects[i]);
           SubDirectoryList.Objects[j] := TObject(Int64(SubDirectoryList.Objects[j])+Int64(DirSize));
        end;
     end;

    ReloadCAK(True,True,True);
   end else
   begin
    Total_Contents   := FullContentCount;
    Archive_Contents := Full_Contents;
   end;
  if SortAuto then
     Sort(SortAccend,SortIndex);

end;

procedure TCakdir2.List(Mask : string; Subdir : boolean);
var
  i, j : Integer;
  amask: TMask;
  Count: Integer;
  k  : String;
  DirSize : Int64;
begin
  List;
  Count := -1;
  SubDirectoryList.Clear;
  BaseDir := ExtractFilePath(Mask);
  aMask := TMask.Create(mask);
  SetLength(Temp_Contents, Total_Contents + 5);
  for i := Total_Contents - 1 downto 0 do
    with Archive_Contents[i] do
      if amask.Matches(_FileDefPath + _FileName) then
       begin
        if ((Subdir) or
          (UpperCase(_FileDefPath) = UpperCase(ExtractFilePath(Mask)))) then
         begin
          Inc(Count);
          Temp_Contents[Count] := Archive_Contents[i];
         end;
        if (UpperCase(_FileDefPath) <> UpperCase(ExtractFilePath(Mask))) then
         begin
          k := Copy(_FileDefPath,
            Length(ExtractFilePath(Mask)) + 1,
            Length(_FileDefPath) - Length(ExtractFilePath(Mask)));
          j := Pos('\', k);
          if j <> 0 then
            k := Copy(k, 0, j);
          end;
        SubDir_RootPath := ExtractFilePath(Mask);
       end;

       for i := 0 to DirectoryList.Count -1 do
       if (UpperCase(Copy(DirectoryList.Strings[i],1,Length(SubDir_RootPath))) = UpperCase(ExtractFilePath(SubDir_RootPath))) then
        begin
          k := Copy(DirectoryList.Strings[i],
            Length(ExtractFilePath(Mask)) + 1,
            Length(DirectoryList.Strings[i]) - Length(ExtractFilePath(Mask)));
          k := ExtractRootPath(Removeslash(k));
          if k <> '' then
          if SubDirectoryList.IndexOf(k) = -1 then
          begin
           DirSize := Int64(DirectoryList.Objects[i]);
           SubDirectoryList.AddObject(k,TObject(DirSize));
          end else
          begin
           j := SubDirectoryList.IndexOf(k);
           DirSize := Int64(DirectoryList.Objects[i]);
           SubDirectoryList.Objects[j] := TObject(Int64(SubDirectoryList.Objects[j])+Int64(DirSize));
          end;
        end;

  Total_Contents   := Count+1;      
  Archive_Contents := Temp_Contents;
  ReloadCAK(True,False,True);
  aMask.Free;
end;

procedure TCakdir2.List(SearchRec : SearchRecType);
var MaskList : TStrings;
    i : integer;
    Count : integer;
function MatchMasks(Filename : string) : boolean;
var aMask : TMask;
    i : integer;
begin
  Result := false;
  i := 0;
  if SearchRec.filenameMasks = '' then Result := true;
  While (i <= MaskList.Count - 1) and not Result do
    try
      aMask := TMask.Create(MaskList.strings[i]);
      if aMask.Matches(Filename) then
        Result := true;
    finally
       aMask.Free;
      Inc(i);
    end;
end;

function MatchPath(FilePath : string) : boolean;
begin
    if (SearchRec.filepath = '') or (SearchRec.filepath = '\') then
      Result := true else
      Result := (Lowercase(SearchRec.filepath) = Lowercase(FilePath));
end;

function MatchSize(Filesize : integer) : boolean;
begin
  Result := ((Filesize >= SearchRec.MinSize) or (SearchRec.MaxSize = -1)) and
            ((Filesize <= SearchRec.MaxSize) or (SearchRec.MaxSize = -1));
end;

function MatchDate(Filedate : TDateTime) : boolean;
begin
  Result := (CompareDate(Filedate, SearchRec.MinDate) >= 0) and
            (CompareDate(Filedate, SearchRec.MaxDate) <= 0);
end;
begin
  List;
  Count := 0;
  BaseDir :='';
  SubDir_RootPath := '';
  MaskList := TStringList.Create;
  MaskList.CommaText := SearchRec.filenameMasks;

  SetLength(Temp_Contents, Total_Contents + 5);
  for i := Total_Contents - 1 downto 0 do
    with Archive_Contents[i] do
      if MatchMasks(_Filedefpath+_Filename) and
         MatchPath(_FiledefPath) and
         MatchSize(_Filesize) and
         MatchDate(_FileTime) then
           begin
             Inc(Count);
             Temp_Contents[Count-1] := Archive_Contents[i];
           end;

  Total_Contents   := Count;
  Archive_Contents := Temp_Contents;

  MaskList.Free;
  ReloadCAK(True,False,True);
end;

procedure TCakdir2.LoadArch(filename : string);
begin
  Close;
  Load(filename);
end;

procedure TCakdir2.Load(filename : string);
begin
  Arcname := filename;
  Password := '';
  CZPassKey.Part1 := -1;
  CZPassKey.Part2 := -1;
  CZPassKey.Part3 := -1;
  if fileexists(filename) then
   begin
    Total_Contents := 0;
    FullContentCount := 0;
    List;
   end;
end;

procedure TCakdir2.New(filename : string);
begin
  Arcname := filename;
  Total_Contents := 0;
  FullContentCount := 0;
end;

procedure TCakdir2.Extract;
begin
 if not directoryexists(Extractoptions.Extr_to) then
        MakeDirectory(Extractoptions.Extr_to);
  process(wtExtract);
end;

procedure TCakdir2.Add;
begin
  process(wtAdd);
  Total_Contents := 0;
  FullContentCount := 0;
  List;
end;

procedure TCakdir2.Test;
begin
  process(wtTest);
end;

procedure TCakdir2.Delete;
begin
  process(wtDelete);
  Total_Contents := 0;
  FullContentCount := 0;
  List;
end;

procedure TCakdir2.SFX;
begin
  process(wtSFX);
end;

procedure TCakdir2.Encrypt;
begin
  process(wtEncrypt);
end;

procedure TCakdir2.Sort(accending : boolean; atype: Sortbytype);
  function Compare(Item1, Item2: ContentType; FSortForward: Boolean;
      aType: SortByType): Integer;
  var
    Resu: Integer;
  begin
    try
      resu := 0;
      case aType of
        (* Filename Column 	*)
        _FName:
          Resu := CompareText(item1._FileName, Item2._FileName);
        _FType:
          Resu := CompareText(item1._Filetype, Item2._Filetype);
        _FDefPath:
          Resu := CompareText(item1._FileDefPath, item2._FileDefPath);
        _FArchive:
          CompareText(item1._FileArchive, Item2._FileArchive);
        _FSize:
          Resu := (Item1._FileSize - Item2._FileSize);
        _FPSize:
          Resu := (Item1._FilePackedSize - Item2._FilePackedSize);
        _FTime:
          Resu := Round(item1._FileTime - item2._FileTime);
        _FCRC:
          CompareText(item1._FileCRC, Item2._FileCRC);
        _FRatio:
          Resu := (Item1._FileRatio - Item2._FileRatio);
       end;
    except
      Resu := 0;
     end;
    if resu = 0 then
      Resu := CompareText(item1._FileName, Item2._FileName);
    if resu = 0 then
      Resu := CompareText(item1._FileDefPath, Item2._FileDefPath);
    if FSortForward then Result := resu
    else
      Result := -Resu;
  end;
  procedure QuickSort(var SortArray: array of ContentType; Size: Integer;
    FSortForward: Boolean; aType: SortByType);
    var
    array1, array2, array3: array of ContentType;
    middle: ContentType;
    pivot, size1, size2, size3, i, j: Integer;
  begin
    if Size <= 1 then Exit;
    pivot  := Size div 2;
    middle := SortArray[pivot];
    SetLength(array1, Size);
    SetLength(array2, Size);
    SetLength(array3, Size);

    size1 := 0;
    size2 := 0;
    size3 := 0;
    for i := 0 to Size - 1 do
      if pivot <> i then
       begin
        j := Compare(SortArray[i], middle, FSortForward, aType);
        if j > 0 then
         begin
          array1[size1] := SortArray[i];
          size1         := size1 + 1;
         end;
        if j < 0 then
         begin
          array2[size2] := SortArray[i];
          size2         := size2 + 1;
         end;
       if j = 0 then
         begin
          array3[size3] := SortArray[i];
          size3         := size3 + 1;
         end;
      end;


    if (size1 > 1) then
      QuickSort(array1, size1, FSortForward, aType);
    if (size2 > 1) then
      QuickSort(array2, size2, FSortForward, aType);

    SetLength(array1, size1);
    SetLength(array2, size2);
    SetLength(array3, size3);

    SortArray[size1] := middle;

    if size1 > 0 then
      for i := 0 to size1 - 1 do
        SortArray[i] := array1[i];

    if size3 > 0 then
      for i := 0 to size3 - 1 do
        SortArray[size1 + i + 1] := array3[i];

    if size2 > 0 then
      for i := 0 to size2 - 1 do
        SortArray[size1 + size3 + i + 1] := array2[i];
  end;
begin
  QuickSort(Archive_Contents, Total_Contents, not accending, aType);
end;

function TCakdir2.GetComment : string;
var ArcCode : integer;
begin
  ArcCode := Locate_Archiver(Extractfileext(archivename),wtLoadContents);
  with CakArchiverList[ArcCode] do
    Result := CakArchiver.GetComment;
end;

procedure TCakdir2.SetComment(text : string);
var ArcCode : integer;
begin
  ArcCode := Locate_Archiver(Extractfileext(archivename),wtLoadContents);
  with CakArchiverList[ArcCode] do
    CakArchiver.SetComment(text);
end;

procedure TCakdir2.Process(aWork : worktype);
var ArcCode : integer;
begin
  Stopping := false;
  Targetsize := 0;
  Completed := 0;
  Targetsize := Get_Target_Size;
  
  ArcCode := Locate_Archiver(Extractfileext(archivename),aWork);
  Case aWork of
  wtLoadContents : begin
                    ArcNeedPassword := false;
                    Password := '';
                   end;
  wtEncrypt : begin
                Case EncryptOptions.EncryptMethod of
                  ceCZip,ceBlowFish,ceRijndael,ceTwoFish : ArcCode := Locate_Archiver('.czip',wtEncrypt);
                  qeBlowfish,qeDes,qe3des,qeRijndael     : ArcCode := Locate_Archiver('.qze',wtEncrypt);
                end;
              end;
  end;
  ArcType := CakArchiverList[ArcCode].BaseType;

  with CakArchiverList[ArcCode] do
  with CakArchiver do
    if Cando(aWork) then
      begin
      CakArchiver.SetCakdir(Self);
      Process(aWork);
      end;
  Stopping := false;
  if aWork = wtAdd then List;
  ReloadCAK(True,True,True);
end;

procedure TCakDir2.SelectALL;
var
  i: Integer;
begin
  for i := 0 to Total_Contents - 1 do
    Archive_Contents[i]._Selected := True;
end;

procedure TCakDir2.UnSelectALL;
var
  i: Integer;
begin
  for i := 0 to Total_Contents - 1 do
    Archive_Contents[i]._Selected := False;
end;

procedure TCakDir2.Select(Index : integer); 
begin
  Archive_Contents[Index]._Selected := true;
end;

procedure TCakDir2.Select(FileName: TStrings; ArchiveName: string);
var
  i: Integer;
begin
  for i := 0 to Total_Contents - 1 do
    with Archive_Contents[i] do
      if not _Selected then
        if _FileArchive = ArchiveName then
          if FileName.IndexOf(_FileDefPath + _FileName) <> -1 then
            _Selected := True;
end;

procedure TCakDir2.Select(FileMasks, FileArchive: string);
var
  i:     Integer;
  AMask: TMask;
begin
  AMask := TMask.Create(FileMasks);
  for i := 0 to Total_Contents - 1 do
    with Archive_Contents[i] do
      if AMask.Matches(_FileDefPath + _FileName) then
        if (Archive_Contents[i]._FileArchive = FileArchive) or (FileArchive = '') then
         begin
          Archive_Contents[i]._Selected := True;
         end;
  AMask.Free;
end;

function TCakDir2.ReturnIconType(FileName: string): Integer;
var
  loc:  Integer;
  Ext:  String;
  {$IFDEF NOT_CMD_LINE} shinfo: TSHFileInfo; {$ENDIF}
  IconS,IconL: TIcon;
begin
  IconS := TIcon.Create();
  IconL := TIcon.Create();
  if FileName <> '*DIR*' then
    Ext := ExtractFileExt(FileName)
  else  // Ext already includes '.'.
    Ext := FileName;
  loc := FileExt.IndexOf(Ext);
  if (loc = -1) then
  {Use Cache}
   begin
    {$IFDEF NOT_CMD_LINE}
    SHGetFileInfo(PChar(Ext), 0, shInfo, SizeOf(shInfo), // GC -Oct 14 2001
    (SHGFI_SYSICONINDEX or SHGFI_USEFILEATTRIBUTES) or
    (SHGFI_ICON or SHGFI_TYPENAME));
    IconL.Handle := shinfo.hIcon;

    SHGetFileInfo(PChar(Ext), 0, shInfo, SizeOf(shInfo), // GC -Oct 14 2001
    (SHGFI_SYSICONINDEX or SHGFI_USEFILEATTRIBUTES) or
    (SHGFI_SMALLICON or SHGFI_ICON or SHGFI_TYPENAME));
    IconS.Handle := shinfo.hIcon;

    ImageS.AddIcon(IconS);
    loc := ImageL.AddIcon(IconL);
    FileExt.Add(Ext);
    FileType.Add(Shinfo.szTypeName);
    {$ELSE}
    //Icon := TIcon.Create;
    //ImageS.AddIcon(Icon);
    //ImageL.AddIcon(Icon);
    loc := FileExt.Add(Ext);
    FileType.Add('Type - ' + Ext);

    {$ENDIF}
   end;
  Result := loc;
  IconS.Free;
  IconL.Free;
end;

function TCakDir2.GrabArchivePath : string;
begin
  Result := '';
  if ArchiveName <> '' then
  Result := Appendslash(Extractfilepath(ArchiveName));
end;

function TCakDir2.Get_Selected_Count: Integer;
var
  i: integer;
begin
  Result := 0;
  for i := 0 to Total_Contents - 1 do
    if Archive_Contents[i]._Selected then
      Inc(Result);
end;

function TCakDir2.Get_Target_Size: Int64;
var
  i, j:         Integer;
  tempfilelist: TStrings;
begin
  Result := 0;
  {if TargetSize <> 0 then
   begin
    Result := TargetSize;
    Exit;
   end;  }
  if AddOptions.Add_Files.Count > 0 then
   begin
    for i := 0 to AddOptions.Add_Files.Count - 1 do
      if FileExists(AddOptions.Add_Files.Strings[i]) then
        Result := Result + Get_File_Size(AddOptions.Add_Files.Strings[i])
    else
     begin
      tempfilelist := PollFileList(AddOptions.Add_Files.Strings[i],
        True);
      for j := 0 to tempfilelist.Count - 1 do
        Result := Result + Get_File_Size(tempfilelist.Strings[j]);
     end;
   end
  else if Get_Selected_Count <> 0 then
    Result := Get_Selected_Size
  else
    Result := Get_Total_Size;

  TargetSize := Result;
end;

function TCakDir2.Get_Selected_Size: Int64;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to Total_Contents - 1 do
    if Archive_Contents[i]._Selected then
      Inc(Result, Archive_Contents[i]._FileSize);
  if Result = 0 then
    Result := -1;
end;

function TCakDir2.Get_Selected_CompSize: Int64;
var 
  i: Integer;
begin
  Result := 0;
  for i := 0 to Total_Contents - 1 do
    if Archive_Contents[i]._Selected then
      Inc(Result, Archive_Contents[i]._FilePackedSize);
  if Result = 0 then
    Result := -1;
end;

function TCakDir2.Get_Total_Count: Integer;
begin
  Result := FullContentCount;
end;

function TCakDir2.Get_Total_Size: Int64;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to FullContentCount - 1 do
    Inc(Result, Full_Contents[i]._FileSize);
  if FullContentCount = 0 then Result := -1; //Prevent crash...
end;

function TCakDir2.Get_Total_Packed_Size: Int64;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to FullContentCount - 1 do
    Inc(Result, Full_Contents[i]._FilePackedSize);
  if FullContentCount = 0 then Result := -1; //Prevent crash...
end;

function TCakdir2.Get_Top_Selected: string;
var
  i, j: Integer;
begin
  j := Total_Contents + 1;
  for i := Total_Contents - 1 downto 0 do
    if Archive_Contents[i]._Selected then
      j := i;
  if j >= Total_Contents + 1 then
    Result := ''
  else
    Result := Archive_Contents[j]._FileDefPath + Archive_Contents[j]._FileName;
end;

function TCakdir2.Get_Top_Index: Integer;
var
  i, j: Integer;
begin
  j := Total_Contents + 1;
  for i := Total_Contents - 1 downto 0 do
    if Archive_Contents[i]._Selected then
      j := i;
  if j >= Total_Contents + 1 then
    Result := -1
  else
    Result := j;
end;

function TCakDir2.AskOverwrite(forfile: string): Boolean;
var
  i:           Integer;
  DoOverwrite: Boolean;
  overwrite, applytoall: Boolean;
begin
  DoOverwrite := False;
  if ExtractOptions.Extr_Overwrite then DoOverwrite := True
  else if OverwriteAll = 1 then DoOverwrite := True
  else if OverwriteAll = 2 then DoOverwrite := False
  else if Assigned(FOnOver) then
   begin
    FOnOver(NIL, ForFile, overwrite, applytoall);
    Dooverwrite := overwrite;
    if applytoall then
      if overwrite then
        OverwriteAll := 1
      else
        OverwriteAll := 2;
   end
  else
   begin
    i := MessageDlg('Overite ' + Forfile + '?', mtWarning,
      [mbYes, mbNo, mbYesToAll, mbNoToAll], 0);
    case i of
      mrYes: DoOverwrite := True;
      mrNo: DoOverwrite  := False;
      mrYesToAll: 
       begin
        DoOverwrite  := True; 
        OverwriteAll := 1; 
       end;
      mrNoToAll:
       begin 
        DoOverwrite  := False; 
        OverwriteAll := 2; 
       end;
     end;
   end;
  Result := DoOverwrite;
end;

Procedure TCakdir2.Close;
begin
  Arcname := '';
  Total_contents := 0;
  process(wtClose);
  ReloadCAK(True,True,True);
end;

function TCakdir2.ArchiveContents(index : integer) : ContentType;
begin
  Result := InitContentType;
  if index <= Total_Contents then
    Result := Archive_Contents[index];
end;

Procedure TCakdir2.ReloadCAK(ReloadLv, ReloadTv, ReloadTl : boolean);
begin
  if ReloadLv then
  if Assigned(Listview) then
        TCakListview2(Listview).ReloadCAK;
  if ReloadTv then
  if Assigned(Treeview) then
        TCakTreeview2(Treeview).ReloadCAK;
  if ReloadTl then
  begin
  if Assigned(Treelist) then
        TCakTreelist2(Treelist).ReloadCAK;
  if Assigned(TreeCombobox) then
        TCakTreeCombobox2(TreeCombobox).ReloadCAK;
  end;
end;

function TCakDir2.NewTempPath: string;
var
  i: Integer;
  k: String;
begin
  i := Gettickcount;
  while DirectoryExists(GrabTempPath + IntToStr(i)) do
    Inc(i);
  k := GrabTempPath + IntToStr(i) + '\';
  MakeDirectory(k);
  NewDirList.Add(k);
  Result := k;
end;

function Tcakdir2.Locate(filename : string; checkdir : boolean) : integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Total_Contents - 1 do
    if not checkdir or (UpperCase(Archive_Contents[i]._FileDefPath) = UpperCase(Extractfilepath(FileName))) then
      if UpperCase(Archive_Contents[i]._FileName) = UpperCase(Extractfilename(FileName)) then
        Result := i;
end;

function TCakdir2.Locate(filename : string) : integer;
begin
        result := Locate(filename,true);
end;

function TCakDir2.InitContentType: ContentType;
var
  Content: ContentType;
begin
  with Content do
   begin
    _FileIcon    := 0;
    _FileRatio   := 0;
    _Tag         := 0;
    _FileSize    := 0;
    _FilePackedSize := 0;
    _FileTime    := 0;
    _FileName    := '';
    _Filetype    := '';
    _FileCRC     := '';
    _FileDefPath := '';
    _FileArchive := '';
    _Encrypted   := False;
    _Selected    := False;
   end;
  Result := Content;
end;

procedure TCakdir2.OnProgEvent(Sender: TObject; FileName: string; FileSize: Longint; JustCompleted: Longint);
begin

       Inc(Completed,JustCompleted);
       if Targetsize <> 0 then
       PercentCompleted := Integer( Int64(Completed) * Int64(100) div Int64(TargetSize) ) else
       PercentCompleted := 100;

       if PercentCompleted = 0 then
          PercentCompleted := 100;

        if Assigned(FonProg2) then
                FOnProg2(Sender,Filename,Filesize,JustCompleted);
end;

procedure TCakdir2._SetListview(aListview : TComponent);
begin
   if not TCakListview2(aListview).Passive then
   Listview := TCakListview2(aListview);
end;

procedure TCakdir2._SetTreeview(aTreeview : TComponent);
begin
   if not TCakTreeview2(aTreeview).Passive then
   Treeview := TCakTreeview2(aTreeview);
end;

procedure TCakdir2._SetTreeList(aTreelist : TComponent);
begin
   if not TCakTreeList2(aTreeList).Passive then
   Treelist := TCakTreeList2(aTreeList);
end;

procedure TCakdir2._SetTreeComboBox(aTreeCombo : TComponent);
begin
   if not TCakTreeComboBox2(aTreeCombo).Passive then
   Treecombobox := TCakTreeComboBox2(aTreeCombo);
end;

procedure TCakdir2.DoStopp(stop : boolean);
begin
        with CakArchiverList[Locate_Archiver(Extractfileext(archivename),wtNone)].CakArchiver do
        DoStop(true);

end;

procedure TCakDir2.HotEdit(FileName: string);
var
  i:         Integer;
  k, fn:     String;
//  encrypted: Boolean;
begin
  //encrypted := ArcNeedPassword;
  if (ExtractFilePath(FileName) <> '\') and (ExtractFilePath(FileName) <> '') then
   begin
    if Assigned(FOnMsg) then
      FOnMsg(NIL, 0, CODE_HOTEDIT, Msg_Error, ERR_HOTEDIT);
    Exit;
   end;

  fn := FileName;
  //if (ExtractFilePath(FileName) = '\') then
  // begin
   // ZipDirRename(FileName, ExtractFileName(FileName));
   // fn := ExtractFileName(FileName);
   //end;


  k := GrabTempPath + 'Checkout\';
  with ExtractOptions do
   begin
    Extr_Overwrite := True;
    Extr_DirNames  := False;
    Extr_to        := k;
   end;

  UnSelectAll;
  Select(FileName,'');
  OverwriteAll := 1;
  if Get_Selected_Count = 0 then
   begin
    if Assigned(FOnMsg) then
      FOnMsg(NIL, 0, CODE_NOTEXIST, Msg_Error, ERR_NOTEXIST);
    Exit;
   end;
  Extract;

  ExploreFolder(k);

  i := MessageDlg('Hot Edit' + #13 + #10 + '--------------------------------------' +
    #13 + #10 + 'File is now located at :' + #13 + #10 + k + #13 + #10 +
    '--------------------------------------' + #13 + #10 +
    'When you finished editing, press <OK>.' + #13 + #10 +
    'Archive will then be updated.' + #13 + #10 +
    'If you don`t want to save changes, press <Cancel>.',
    mtWarning, [mbOK, mbCancel], 0);

  if i = mrOk then
   begin
    if FileExists(k + fn) then
     begin
      //Clear_Selected_List;
      //Add_Selected_List(filename, Archive_list[arc]._ARCname);
      //Delete;

      with AddOptions do
       begin
        Add_UsePath := False;
        //AddMode     := [];
        Add_Files.Clear;
        Add_Files.Add(k + fn);
       end;
      Add;
     end;
   end 
  else
    ShowMessage(k + fn + ' is deleted, update ABORT');
  DeleteFile(k + fn);
  RemoveDir(k);
end;

procedure TCakDir2.AddContents(
       FileArchive, FileFullPath, FileDefPath, FileName : String;
       FileSize, FilePackedSize : Integer;
       Encrypted        : Boolean;
       FileTime         : TDateTime;
       FileCRC          : String                             );
var DirSize : Int64;
    i : integer;       
begin
  if locate(FileDefPath+FileName) <> -1 then
    with Archive_Contents[locate(FileDefPath+FileName)] do
      begin
        Inc(_FileSize,FileSize);
        Inc(_FilePackedSize,FilePackedSize);
      end
  else
  begin
  Inc(Total_Contents);
  SetLength(Archive_Contents, Total_Contents);
  with Archive_Contents[Total_Contents-1] do
    begin
     _FileArchive    := FileArchive;
     _FileIcon       := ReturnIconType(FileName);
     _FileFullPath   := FileFullPath;
     _Filetype       := FileType.Strings[_FileIcon];
     _FileDefPath    := Appendslash(FileDefPath);
     if FileDefPath <> '' then
     if DirectoryList.IndexOf(FileDefPath) = -1 then
      begin
       //DirectoryList.Add(FileDefpath);
       DirSize := FilePackedSize;
       DirectoryList.AddObject(FileDefPath,TObject(DirSize));
      end else
      begin
       i := DirectoryList.IndexOf(FileDefPath);
       DirSize := Int64(DirectoryList.Objects[i]) + Int64(FilePackedSize);
       DirectoryList.Objects[i] := TObject(DirSize);
      end;
     _FileName       := FileName;
     _FileSize       := FileSize;
     _FilePackedSize := FilePackedSize;
     if FileSize <> 0 then
     _FileRatio :=
        Trunc((1 - (FilePackedSize / FileSize)) * 100) else
     _FileRatio := 100;
     if _FileRatio > 100 then _FileRatio := 0;
     _Encrypted      := Encrypted;
     if Encrypted then ArcNeedPassword := true;
     _FileTime       := FileTime;
     _FileCRC        := FileCRC;
    end;
  end;
end;

procedure TCakDir2.AddContents(
       FileArchive, FileDefPath, FileName : String;
       FileSize, FilePackedSize : Integer;
       Encrypted        : Boolean;
       FileTime         : TDateTime;
       FileCRC          : String                        );
begin
       AddContents(FileArchive,'',FileDefPath,FileName,
       FileSize, FilePackedSize, Encrypted, FileTime, FileCRC);
end;

procedure TCakDir2.AddContents(
       FileArchive, FileDefPath, FileName : String;
       FileSize, FilePackedSize : Integer);
begin
       AddContents(FileArchive,'',FileDefPath,FileName,
       FileSize, FilePackedSize, False, Now, 'FFFFFFFF');
end;

procedure TCakDir2.AddContents(
       FileArchive, FileDefPath, FileName : String);
begin
       AddContents(FileArchive,'',FileDefPath,FileName,
       0, 0, False, Now, 'FFFFFFFF');
end;

function TCakdir2.GetArchiveType(filename : string) : supporttype;
begin
  Result := CakArchiverList[Locate_Archiver(Extractfileext(filename),wtLoadContents)].BaseType;
end;

function TCakDir2.OpenFilter_All       : string;
var s : SupportType;
    i : integer;
begin
  Result := '';
  for s := _ZIP to _WIT do
    Case s of
    _WIT, _AKS, _XXE, _UUE, _B64, _BQX, _HOG, _Yz1,  _Zac,  _Zoo : begin end;
    else
    begin
      i := Self.Locate_Archiver(s,wtLoadContents);
      if i > 1 then
        begin
          if s = _ZIP then
          Result := Result + '*' + CakArchiverList[i].SupportExtension else
          Result := Result + ';*' + CakArchiverList[i].SupportExtension;
          Result := Replace(Result,' ',';*');
        end;
    end;
    end;
end;

function TCakDir2.OpenFilter_Seperated : string;
var s : SupportType;
    i : integer;
    ext : string;
begin
  Result := '';
  for s := _ZIP to _WIT do
    if (s <> _WIT) and (s <> _AKS) then
    begin
      i := Self.Locate_Archiver(s,wtLoadContents);
      if i > 1 then
        with CakArchiverList[i] do
        begin
          ext := '*' + Replace(SupportExtension,' ',';*');
          if s = _ZIP then
          Result := Result + Format('%s ( %s )|%s',[ArchiveType,lowercase(Replace(ext,';',',')),ext]) else
          Result := Result + Format('|%s ( %s )|%s',[ArchiveType,lowercase(Replace(ext,';',',')),ext]);
        end;
    end;
end;

function TCakDir2.OpenFilter : string;
var k : string;
begin
  k := CakArchiverList[Locate_Archiver(_Ext)].SupportExtension;
  k := Replace(k,'.','*.');
  Result := 'All Readable files|'+OpenFilter_All + '|' + OpenFilter_Seperated;
  if k <> '' then
    Result := Format('%s|%s( %s )|%s|%s( *.* )|*.*',[Result,'CakExtension       ',lowercase(k),Replace(k,' ',';'),'All files                 ']);
end;




procedure Register;
begin
  RegisterComponents('CAKE', [TCakDir2]);
end;

end.
