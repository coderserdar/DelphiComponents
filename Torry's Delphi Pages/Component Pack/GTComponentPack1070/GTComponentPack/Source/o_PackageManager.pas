{*******************************************************}
{                                                       }
{       GT Delphi Components                            }
{       TgtPackageManager                               }
{                                                       }
{       Copyright (c) GT Delphi Components              }
{       http://www.gtdelphicomponents.gr                }
{                                                       }
{                                                       }
{*******************************************************}
unit o_PackageManager;

interface

uses
   Classes
  ,Windows
  ;
type
{------------------------------------------------------------------------------}
  TgtPackageItem = class(TPersistent)
  private
    FLoaded      : Boolean;
    FModule      : HModule;
    FFileName    : string;
    FDescription : string;
    FVersion     : string;
    FVerSize     : Integer;
    FVerBuf      : PChar;
    FVerBufValue : Pointer;
    FVerHandle   : Cardinal;
    FVerBufLen   : Cardinal;
    FVerKey      : string;
    procedure SetFileName(const Value: string);
  protected
    procedure AssignTo  (Dest: TPersistent );override;
    procedure ParsePackageInfo;
    function  GetInfo   (const aKey: string): string;
    function  QueryValue(S : string):string ;
  public
    property Module      : HModule read FModule      write FModule;
    property Loaded      : Boolean read FLoaded      write FLoaded;
    property FileName    : string  read FFileName    write SetFileName;
    property Description : string  read FDescription write FDescription;
    property Version     : string  read FVersion     write FVersion;
  end;
{------------------------------------------------------------------------------}
  TgtPackageList = class(TStringList)
  private
    function  GetPackage(Index: Integer): TgtPackageItem;
    procedure SetPackage(Index: Integer; const Value: TgtPackageItem);
    { Private declarations }
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create;
    destructor  Destroy;override;
  public
    procedure AddPackage(FileName : string ; AModule:HModule);overload;
    procedure AddPackage(Package  : TgtPackageItem);overload;
    procedure AddPackageInfo(const FileName :string; const AModule : HModule; const Loaded : Boolean);
    procedure RemovePackage(FileName : string );overload;
    procedure RemovePackage(AModule  : TgtPackageItem);overload;
    property  Packages[Index: Integer]: TgtPackageItem read GetPackage write SetPackage;
  end;
{------------------------------------------------------------------------------}
  TPackageEvent = procedure (Sender :TObject ; APackage : TgtPackageItem) of Object;
{------------------------------------------------------------------------------}
  TgtPackageManager = class(TComponent)
  private
    FPackageList      : TgtPackageList;
    FPackageExtension : string;
    FPackageRepository: string;
    FOnPackageUnload  : TPackageEvent;
    FOnPackageLoad    : TPackageEvent;
    { Private declarations }
  protected
    { Protected declarations }
    FModule : HModule;
  public
    { Public declarations }
    constructor Create(AOwner:TComponent);override;
    destructor  Destroy;override;
  public
    property PackageList       : TgtPackageList read FPackageList;
    property PackageRepository : string         read FPackageRepository write FPackageRepository;
    property PackageExtension  : string         read FPackageExtension  write FPackageExtension;
  public
    procedure LoadPackages;
    procedure UnLoadPackages;
    procedure LoadPackage  (FileName : string);overload;
    procedure UnLoadPackage(FileName : string);overload;
    procedure LoadPackage  (Index    : Integer);overload;
    procedure UnLoadPackage(Index    : Integer);overload;
    procedure RegisterPackages;
    procedure UnRegisterPackages;
    procedure ListPackages   (const PackList : TStrings;OnlyLoaded : Boolean = False);
    procedure SavePackageList(FileName : string ; OnlyLoaded : Boolean = False);
    function  IsPackageLoaded(FileName:string):Boolean;overload;
    function  IsPackageLoaded(Index   :Integer):Boolean;overload;
    function  IsPackageLoaded(Package :TgtPackageItem):Boolean;overload;
  published
    { Published declarations}
    property  OnPackageLoad   : TPackageEvent read FOnPackageLoad   write FOnPackageLoad;
    property  OnPackageUnload : TPackageEvent read FOnPackageUnload write FOnPackageUnload;    
  end;
{------------------------------------------------------------------------------}

function PackageManager:TgtPackageManager;

implementation

uses
  SysUtils
  ;
const
  PACKAGE_INFO_STRING=
  'FileName    :[%s] '#13#10+
  'Description :[%s] '#13#10+
  'Version     :[%s] '#13#10+
  'Loaded      :[%s] '#13#10+
  'ModuleHandle:[%d] '#13#10+
  '';
var
  _PackageManager : TgtPackageManager;

function PackageManager:TgtPackageManager;
begin
  if not Assigned(_PackageManager) then
    _PackageManager := TgtPackageManager.Create(nil);
  Result := _PackageManager;
end;


{ TPackageItem }
{------------------------------------------------------------------------------}
procedure TgtPackageItem.AssignTo(Dest: TPersistent);
begin
  if Dest is TgtPackageItem then
  begin
    Module      := TgtPackageItem(Dest).Module;
    FileName    := TgtPackageItem(Dest).FileName;
    Loaded      := TgtPackageItem(Dest).Loaded;
    Description := TgtPackageItem(Dest).Description;
    Version     := TgtPackageItem(Dest).Version;
  end
  else
    inherited AssignTo(Dest);
end;
{------------------------------------------------------------------------------}
function TgtPackageItem.GetInfo(const aKey: string): string;
begin
  Result := '';
  FVerKey := Format('\StringFileInfo\%.4x%.4x\%s',
    [LoWord(Integer(FVerBufValue^)),
    HiWord(Integer(FVerBufValue^)), aKey]);
  if VerQueryValue(FVerBuf, PChar(FVerKey), FVerBufValue, FVerBufLen) then
   // Result := SysUtils.StrPas(FVerBufValue);
end;
{------------------------------------------------------------------------------}
function TgtPackageItem.QueryValue(S: string): string;
begin
  Result := '';
  if GetFileVersionInfo(PChar(FileName), FVerHandle, FVerSize, FVerBuf) and
      VerQueryValue(FVerBuf, '\VarFileInfo\Translation', FVerBufValue, FVerBufLen) then
   Result := GetInfo(S);
end;
{------------------------------------------------------------------------------}
procedure TgtPackageItem.ParsePackageInfo;
begin
 if not FileExists(FileName) then
    raise Exception.CreateFmt('File %s does not exist',[FileName]);
  FVerSize := GetFileVersionInfoSize(PChar(FileName), FVerHandle);
  if FVerSize > 0 then
  begin
    try
      FVerBuf           := AllocMem(FVerSize);
      FVersion          := QueryValue('FileVersion');
      FDescription      := QueryValue('FileDescription');
    finally
      FreeMem(FVerBuf);
    end;
  end;
end;
{------------------------------------------------------------------------------}
procedure TgtPackageItem.SetFileName(const Value: string);
begin
  FFileName := Value;
  ParsePackageInfo;
end;
{------------------------------------------------------------------------------}




{ TPackageList }
{------------------------------------------------------------------------------}
constructor TgtPackageList.Create;
begin
  inherited Create;
end;
{------------------------------------------------------------------------------}
destructor TgtPackageList.Destroy;
begin
  inherited;
end;
{------------------------------------------------------------------------------}
procedure TgtPackageList.AddPackage(Package: TgtPackageItem);
begin
  if Assigned(Package) then
  begin
    AddPackage(Package.FileName,Package.Module);
  end;
end;
{------------------------------------------------------------------------------}
procedure TgtPackageList.AddPackage(FileName: string; AModule: HModule);
var
 FPackageItem : TgtPackageItem;
begin
  if IndexOf(FileName) = -1 then
  begin
    FPackageItem          := TgtPackageItem.Create;
    FPackageItem.FileName := FileName;
    FPackageItem.Module   := 0;
    FPackageItem.Loaded   := False;
    AddObject(FileName,FPackageItem);
  end;
end;
{------------------------------------------------------------------------------}
procedure TgtPackageList.AddPackageInfo(const FileName: string;const AModule: HModule; const Loaded: Boolean);
var
  Idx : Integer;
begin
  Idx := IndexOf(FileName);
  if Idx <> -1 then
  begin
    Packages[Idx].Module := AModule;
    Packages[Idx].Loaded := Loaded;
  end;
end;
{------------------------------------------------------------------------------}
procedure TgtPackageList.RemovePackage(FileName: string);
var
  Idx : Integer;
begin
  Idx := IndexOf(FileName);
  if Idx <> -1 then
    Delete(Idx);
end;
{------------------------------------------------------------------------------}
procedure TgtPackageList.RemovePackage(AModule: TgtPackageItem);
var
  Idx : Integer;
begin
  Idx := IndexOf(AModule.FileName);
  if Idx <> -1 then
    Delete(Idx);
end;
{------------------------------------------------------------------------------}


//Getters - Setters \\
{------------------------------------------------------------------------------}
function TgtPackageList.GetPackage(Index: Integer): TgtPackageItem;
begin
  if Index <> -1 then
    Result := TgtPackageItem(GetObject(Index))
  else
    Result := nil;  
end;
{------------------------------------------------------------------------------}
procedure TgtPackageList.SetPackage(Index: Integer; const Value: TgtPackageItem);
begin
  Packages[Index].AssignTo(Value);
end;
{------------------------------------------------------------------------------}




{ TPackageManager }
{------------------------------------------------------------------------------}
constructor TgtPackageManager.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPackageList := TgtPackageList.Create;
end;
{------------------------------------------------------------------------------}
destructor TgtPackageManager.Destroy;
begin
  FreeAndNil(FPackageList);
  inherited;
end;
{------------------------------------------------------------------------------}
procedure TgtPackageManager.RegisterPackages;
var
  Rec : TSearchRec;
begin
  if FindFirst(PackageRepository + '*' + PackageExtension,faAnyFile - faDirectory, Rec)=0 then
  begin
   try
     repeat
      FPackageList.AddPackage(PackageRepository+Rec.Name,0);
     until FindNext(Rec) <> 0;
   finally
     FindClose(Rec);
   end;  
  end;
end;
{------------------------------------------------------------------------------}
procedure TgtPackageManager.UnRegisterPackages;
begin
  FPackageList.Clear;
end;
{------------------------------------------------------------------------------}
procedure TgtPackageManager.LoadPackage(Index: Integer);
begin
  if FPackageList.Count >=1 then
  begin
    if not FPackageList.Packages[Index].Loaded then
    begin
      try
        FModule :=   SysUtils.LoadPackage(FPackageList.Packages[Index].FFileName);
        if FModule <> 0 then
        begin
          FPackageList.AddPackageInfo(FPackageList.Packages[Index].FFileName,FModule,True);
          if Assigned(FOnPackageLoad) then
            FOnPackageLoad(Self,FPackageList.Packages[Index]);
        end;
      except
        UnLoadPackage(Index);
        raise;
      end;
    end;
  end;
end;
{------------------------------------------------------------------------------}
procedure TgtPackageManager.LoadPackage(FileName: string);
var
  Idx : Integer;
begin
  Idx := FPackageList.IndexOf(FileName);
  if Idx <> -1 then
    LoadPackage(Idx);
end;
{------------------------------------------------------------------------------}
procedure TgtPackageManager.LoadPackages;
var
  i : Integer;
begin
  for i := 0 to Pred(FPackageList.Count) do
  begin
    LoadPackage(i);
  end;
end;
{------------------------------------------------------------------------------}
procedure TgtPackageManager.UnLoadPackage(Index: Integer);
begin
  try
    if IsPackageLoaded(Index) then
    begin
      SysUtils.UnloadPackage(FPackageList.Packages[Index].Module);
      FPackageList.AddPackageInfo(FPackageList.Packages[Index].FFileName,0,False);
      if Assigned(FOnPackageUnLoad) then
        FOnPackageUnLoad(Self,FPackageList.Packages[Index]);
    end;
  except
    raise;
  end;
end;
{------------------------------------------------------------------------------}
procedure TgtPackageManager.UnLoadPackage(FileName: string);
var
  Idx : Integer;
begin
  Idx := FPackageList.IndexOf(FileName);
  if Idx <> -1 then
    UnLoadPackage(Idx);
end;
{------------------------------------------------------------------------------}
procedure TgtPackageManager.UnLoadPackages;
var
  i : Integer;
begin
  if Assigned(FPackageList) then
  for i := 0 to Pred(FPackageList.Count) do
  begin
    UnLoadPackage(i);
  end;
end;
{------------------------------------------------------------------------------}
function  TgtPackageManager.IsPackageLoaded(Index   :Integer):Boolean;
begin
  Result := FPackageList.Packages[Index].Loaded and (FPackageList.Packages[Index].Module <> 0);
end;
{------------------------------------------------------------------------------}
function TgtPackageManager.IsPackageLoaded(FileName: string): Boolean;
var
  Idx : integer;
begin
  Result := False;
  Idx := FPackageList.IndexOf(FileName);
  if Idx <> -1  then
    Result := IsPackageLoaded(Idx);
end;
{------------------------------------------------------------------------------}
function TgtPackageManager.IsPackageLoaded(Package: TgtPackageItem): Boolean;
var
  Idx : integer;
begin
  Result := False;
  if Assigned(Package) then
  begin
    Idx    := FPackageList.IndexOf(Package.FileName);
    if Idx <> -1 then
      Result := IsPackageLoaded(Idx);
  end;
end;
{------------------------------------------------------------------------------}
procedure TgtPackageManager.ListPackages(const PackList: TStrings;OnlyLoaded: Boolean);
var
  i : Integer;
begin
  if Assigned(PackList) then
  begin
    if OnlyLoaded then
    begin
      PackList.Clear;
      for i:= 0 to Pred(FPackageList.Count) do
      begin
        if Assigned(FPackageList.Packages[i]) then
          if FPackageList.Packages[i].Loaded then
          PackList.Add(Format(PACKAGE_INFO_STRING,
                      [FPackageList.Packages[i].FileName
                      ,FPackageList.Packages[i].Description
                      ,FPackageList.Packages[i].Version
                      ,BoolToStr(FPackageList.Packages[i].Loaded,True)
                      ,FPackageList.Packages[i].Module]));

      end;
    end
    else
    begin
      PackList.Clear;
      for i:= 0 to Pred(FPackageList.Count) do
      begin
        if Assigned(FPackageList.Packages[i]) then
          PackList.Add(Format(PACKAGE_INFO_STRING,
                      [FPackageList.Packages[i].FileName
                      ,FPackageList.Packages[i].Description
                      ,FPackageList.Packages[i].Version
                      ,BoolToStr(FPackageList.Packages[i].Loaded,True)
                      ,FPackageList.Packages[i].Module]));
      end;
    end;
  end;
end;
{------------------------------------------------------------------------------}
procedure TgtPackageManager.SavePackageList(FileName: string;OnlyLoaded: Boolean);
var
  TempStrings : TStrings;
begin
  try
    TempStrings := TStringList.Create;
    ListPackages(TempStrings,OnlyLoaded);
    TempStrings.SaveToFile(FileName);
  finally
    FreeAndNil(TempStrings);
  end;
end;
{------------------------------------------------------------------------------}



initialization


finalization
  if Assigned(_PackageManager) then
  begin
    _PackageManager.UnLoadPackages;
    FreeAndNil(_PackageManager);
  end;





end.
