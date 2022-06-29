unit CakPlugins;

interface

uses
  Windows, Messages, SysUtils, Classes, CakDefs2, CakArchiver, Dialogs, Forms;

const CanExtract = FALSE;
      CanAdd = FALSE;
      CanList = FALSE;
      CanSFX = FALSE;

type
  TCakPlugins = class(TCakArchiver)
  private
    { Private declarations }
    Stopping : boolean;
  protected
    { Protected declarations }
  public
    { Public declarations }

    Dllpath : string;
    DllHandle: Thandle;
    DllLoad_DLL : function (_msgEvent     : TCMsgEvent;
                  _progEvent    : TCProgEvent;
                  _overEvent    : TCOverEvent;
                  _pwdEvent     : TCPwdEvent;
                  _CzipPwdEvent : TCCZIPPwdEvent) : boolean; register;

    DLLUnLoad_DLL : function : boolean; register;
       DLLProcess : function (dowhat         : WorkType; Archivename : string; 
                              SelectedList   : TStrings; password : string;
                              VersionControl : boolean; TimeStrFormat : string;
                              ExtractOptions : ExtractOptionsType;
                              SfxOptions     : SfxOptionsType;
                              AddOptions     : AddOptionsType;
                              RenameOptions  : RenameOptionsType;
                              Encryptoptions : EncryptoptionsType) : boolean; register;
  DLLGetComment   : function : string; register;
  DLLSetComment   : procedure (value : string); register;
DLLReturnContents : function (var content : Contenttype) : boolean; register;
DLLGetSupportType : function (index : integer; var BaseTyp : SupportType;var SupportExt : String; var SupportWk : SupportWorkType) : boolean; register;
       DLLDoStopp : procedure; register;


    procedure SetCakdir(pCakdir : TComponent); override;
    procedure Process(dowhat : WorkType); override;
    function Cando(aWork: WorkType): Boolean; override;
    function Def_TreatAs : string; override;
    procedure Load_DLL; override;
    procedure UnLoad_DLL; override;
    function DllExists : Boolean; override;
    procedure DoStop(Stopp: Boolean); override;
    constructor Create(AOwner: TComponent; Cakdir2 : TComponent; DLLPath : string);
    destructor Destroy; override;
    function GetComment : string; override;
    procedure SetComment(text : string); override;
  published
    { Published declarations }
    property Stop: Boolean read Stopping write DoStop;
  end;

procedure Register;

implementation
uses CakUtils2, Cakdir2;
var Cakdir : TCakdir2;
procedure TCakPlugins.SetCakdir(pCakdir : TComponent);
begin
    Cakdir := TCakdir2(pCakdir);
end;
procedure TCakPlugins.DoStop(Stopp: Boolean);
begin
  if Stopp then
    DLLDoStopp;
end;

procedure TCakPlugins.Process(dowhat : WorkType);
var content : Contenttype;
    SelectedList : TStrings;
    i : integer;
    k : string;
begin
    Load_DLL;
    SelectedList := TStringList.Create;

    with TCakdir2(Cakdir) do
    begin


    Case doWhat of
      wtExtract : begin
                    OverwriteAll := 0;
                    if Get_Selected_Count = 0 then ExtractOptions.Extr_ExtractAll := true;
                    for i := 0 to Total_Contents -1 do
                      with Archive_Contents[i] do
                        if _Selected or ExtractOptions.Extr_ExtractAll then
                          SelectedList.AddObject(_FileFullPath,TObject(i));
                    for i := SelectedList.Count -1  downto 0 do
                      with Archive_Contents[Integer(SelectedList.Objects[i])] do
                      begin
                      k := Appendslash(Extractoptions.Extr_to) + _FileDefPath + _Filename;
                      if FileExists(k) then
                        if AskOverwrite(k) then
                          DeleteFile(k) else
                          SelectedList.Delete(i);
                      end;

                  end;
      wtLoadContents : Total_Contents := 0;
    end;

    DLLProcess(doWhat,Archivename,SelectedList,Password,VersionControl,TimeStrFormat,
               ExtractOptions,SfxOptions,AddOptions,RenameOptions,EncryptOptions);
    if doWhat = wtLoadContents then
      while DLLReturnContents(content) do
      begin
        AddContents(Archivename,Content._FileFullPath,Content._FileDefPath,Content._FileName,Content._FileSize,
        Content._FilePackedSize,Content._Encrypted,Content._FileTime,Content._FileCRC);
      end;
    end;
    SelectedList.Free;
end;

function TCakPlugins.DllExists : boolean;
begin
        Result := true;
end;

function TCakPlugins.Cando(aWork: WorkType): Boolean;
begin
        Result := true;
end;

function TCakPlugins.Def_TreatAs : string;
begin
  result := '';
end;

procedure TCakPlugins.Load_DLL;
begin
  with TCakdir2(Cakdir) do
    DLLLoad_DLL(FOnMsg,FOnProg,FOnOver,FOnPwd,FOnCZipPwd);
end;

procedure TCakPlugins.UnLoad_DLL;
begin
  DLLUnLoad_DLL;
end;

constructor TCakPlugins.Create(AOwner: TComponent; Cakdir2 : TComponent; DLLPath : string);
var BaseTyp : SupportType;SupportExt : String; SupportWk : SupportWorkType;
    i : integer;
begin
  inherited Create(AOwner);

  DLLHandle := LoadLibrary(PChar(DLLPath));
  if DLLHandle >= 32 then
    begin
      @DllLoad_DLL       := GetProcAddress(DLLHandle,'Load_DLL');
      @DLLUnLoad_DLL     := GetProcAddress(DLLHandle,'UnLoad_DLL');
      @DLLProcess        := GetProcAddress(DLLHandle,'Process');
      @DLLGetComment     := GetProcAddress(DLLHandle,'GetComment');
      @DLLSetComment     := GetProcAddress(DLLHandle,'SetComment');
      @DLLReturnContents := GetProcAddress(DLLHandle,'ReturnContents');
      @DLLGetSupportType := GetProcAddress(DLLHandle,'GetSupportType');
      @DLLDoStopp        := GetProcAddress(DLLHandle,'DoStopp');

      SetCakdir(Cakdir2);
      Load_DLL;

      i := 0;
      while DLLGetSupportType(i,BaseTyp,SupportExt,SupportWk) do
       begin
        with TCakdir2(Cakdir2) do Add_Archiver(TCakPlugins,Self,BaseTyp,SupportExt,SupportWk);
        Inc(i);
       end;
    end;
end;


destructor TCakPlugins.Destroy;
begin
    Unload_DLL;
    @DllLoad_DLL       := nil;
    @DLLUnLoad_DLL     := nil;
    @DLLProcess        := nil;
    @DLLGetComment     := nil;
    @DLLSetComment     := nil;
    @DLLReturnContents := nil;
    @DLLGetSupportType := nil;
    @DLLDoStopp        := nil;

    if DLLHandle >= 32 then
      FreeLibrary(DLLHandle);
    DLLHandle := 0;
    inherited Destroy;
end;

function TCakPlugins.GetComment : string;
begin
  Result := DLLGetComment;
end;

procedure TCakPlugins.SetComment(text : string);
begin
  DLLSetComment(text);
end;


procedure Register;
begin
  //RegisterComponents('QZip', [TCakPlugins]);
end;

end.
