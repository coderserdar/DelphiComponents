unit nuServer;

interface

uses
  Windows, SysUtils, ComCtrls, Classes, Graphics, StrUtils;

type
  TnuServer = class;

  TnuUpgradeMethod = (umSelfUpgrade, umUseExternalSetup, umRedirectToURL);
  TnuTargetDir = (tdApp, tdProgramFiles, tdWindows, tdSystem, tdCommonAppData, tdCommonDesktop, tdCommonProgramFiles, tdTempDir);
  TnuMiscFilesUpgradeBehavior = (mfuAlwaysDownload, mfuOnlyIfSizeDifferent);

  TServerFile = class(TCollectionItem)
  private
    FFileName: string;
    FFileSize: longint;
    FTargetDir: TnuTargetDir;
    FSubDir: string;
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property FileName : string read FFileName write FFileName;
    property FileSize : longint read FFileSize write FFileSize;
    property TargetDir : TnuTargetDir read FTargetDir write FTargetDir;
    property SubDir: string read FSubDir write FSubDir;
 end;

  TServerFiles = class(TCollection)
  private
    FOwner: TnuServer;
    function GetItem(Index:Integer): TServerFile;
    procedure SetItem(Index:Integer; const Value: TServerFile);
  protected
    function GetOwner:TPersistent; override;
    procedure Update(Item:TServerFile); reintroduce;
  public
    constructor Create(AOwner:TnuServer);
    function Add(): TServerFile;
    function Insert(Index:Integer): TServerFile;
    property ServerFile[Index:Integer]: TServerFile read GetItem write SetItem; default;
  end;

  TnuServer = class(TComponent)
  private
    FServerFiles: TServerFiles;
    FProtocol: string;
    FFilePath: string;
    FPort: integer;
    FLogin: boolean;
    FUserID: string;
    FPassword: string;
    FVersionDate: string;
    FVersionNumber: string;
    FUpgradeMsg: string;
    FUpgradeMethod: TnuUpgradeMethod;
    FExeFile: string;
    FExtractParams: string;
    FExitCode: longword;
    FCloseNetUpdate: boolean;
    FMiscFilesUpgrade: TnuMiscFilesUpgradeBehavior;
    FSecurityCode: string;
    procedure SetServerFiles(Value:TServerFiles);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Load(const AFileName: string);
    procedure Store(const AFileName: string);
  published
    property ServerFiles: TServerFiles read FServerFiles write SetServerFiles;
    property Protocol: string read FProtocol write FProtocol;
    property FilePath: string read FFilePath write FFilePath;
    property Port: integer read FPort write FPort;
    property Login: boolean read FLogin write FLogin;
    property UserID: string read FUserID write FUserID;
    property Password: string read FPassword write FPassword;
    property VersionDate: string read FVersionDate write FVersionDate;
    property VersionNumber: string read FVersionNumber write FVersionNumber;
    property UpgradeMsg: string read FUpgradeMsg write FUpgradeMsg;
    property UpgradeMethod: TnuUpgradeMethod read FUpgradeMethod write FUpgradeMethod;
    property ExeFile: string read FExeFile write FExeFile;
    property ExtractParams: String read FExtractParams write FExtractParams;
    property ExitCode: longword read FExitCode write FExitCode;
    property CloseNetUpdate: boolean read FCloseNetUpdate write FCloseNetUpdate;
    property MiscFilesUpgrade: TnuMiscFilesUpgradeBehavior read FMiscFilesUpgrade write FMiscFilesUpgrade;
    property SecurityCode: string read FSecurityCode write FSecurityCode;
  end;

//procedure Register;

implementation

//procedure Register;
//begin
//  RegisterComponents('Kidmoses', [TnuClient]);
//end;


{ ********************************************************************* }
{TServerFile}
constructor TServerFile.Create(Collection:TCollection);
begin
  inherited Create(Collection);
  FTargetDir := tdApp;
end;

destructor TServerFile.Destroy;
begin
  inherited;
end;

function TServerFile.GetDisplayName: string;
begin
 // Result := FileName;
  if Result = '' then Result := inherited GetDisplayName;
end;

procedure TServerFile.Assign(Source: TPersistent);
begin
  if Source is TServerFile then
    begin
    FileName  := TServerFile(Source).FileName;
    FileSize  := TServerFile(Source).FFileSize;
    TargetDir := TServerFile(Source).TargetDir;
    SubDir    := TServerFile(Source).SubDir;
    end
  else
    inherited; //raises an exception
end;

{ ********************************************************************* }
{TServerFiles}
constructor TServerFiles.Create(AOwner: TnuServer);
begin
  inherited Create(TServerFile);
  FOwner := AOwner;
end;

function TServerFiles.Add: TServerFile;
begin
  Result := TServerFile(inherited Add);
end;

function TServerFiles.GetItem(Index: Integer): TServerFile;
begin
  Result := TServerFile(inherited GetItem(Index));
end;

procedure TServerFiles.SetItem(Index: Integer; const Value: TServerFile);
begin
  inherited SetItem(Index, Value);
end;

// Note: You must override GetOwner in Delphi 3.x to get correct streaming behavior.
function TServerFiles.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TServerFiles.Update(Item: TServerFile);
begin
  inherited Update(Item);
end;

function TServerFiles.Insert(Index: Integer): TServerFile;
begin
  Result := TServerFile(inherited Insert(Index));
end;

{ ********************************************************************* }
{TnuServer}
constructor TnuServer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FServerFiles := TServerFiles.Create(Self);
  FUpgradeMethod := umSelfUpgrade;
end;

destructor TnuServer.Destroy;
begin
  FServerFiles.Clear;
  FServerFiles.Free;
  inherited Destroy;
end;

procedure TnuServer.SetServerFiles(Value: TServerFiles);
begin
  FServerFiles.Assign(Value);
end;

procedure TnuServer.Load(const AFileName: string);
var
  eStr: string;
  strm : TFileStream;
begin
  if FileExists(AFileName) then begin
    strm := nil;
    try
    strm := TFileStream.Create(AFileName, fmOpenRead);
    strm.ReadComponent(self);
    except
      on E: Exception do begin
        eStr := E.Message;
        end;
    end;
    strm.Free;
  end;
end;

procedure TnuServer.Store(const AFileName: string);
var
  strm : TFileStream;
begin
  strm := TFileStream.Create(AFileName, fmCreate);
  strm.WriteComponent(self);
  strm.Free;
end;

end.
