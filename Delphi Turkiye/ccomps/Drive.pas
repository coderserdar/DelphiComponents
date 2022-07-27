unit Drive;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs;

type
  TDriverType = (dtNotDetermined, dtNoRoot, dtRemovable, dtFixed, dtNetwork,
                 dtCD, dtRamDisk);

  TSizeType = (stByte, stKB, stMB, stGB);
  TDisk = class(TComponent)
  private
    FSizeType          : TSizeType;
    FDrive             : Char;
    FDriverType        : TDriverType;
    FFileSystem,
    FSerial,
    FVolume            : String;
    FFreeSpace,
    FDiskSize          : Double;
    FNumberOfFreeClusters,
    FTotalCluster,
    FBytesPerSector,
    FSectorsPerCluster : DWord;
    { Private declarations }
  protected
    { Protected declarations }
    procedure SetDrive(Value: Char);

    procedure SetDriverType( Value : TDriverType);
    function GetDriverType : TDriverType;

    procedure SetVolume ( Value : String);
    function GetVolume : String;

    procedure SetFileSystem( Value : String);
    function GetFileSystem : String;

    procedure SetDiskSize ( Value : Double);
    function GetDisksize : Double;

    procedure SetFreeSpace ( Value : Double);
    function GetFreespace : Double;

    procedure SetSizeType ( Value : TSizeType);

    procedure SetSectorsPerCluster ( Value : DWord);
    function GetSectorsPerCluster : DWord;

    procedure SetBytesPerSector( Value : DWord);
    function GetBytesPerSector : DWord;

    procedure SetNumberOfFreeClusters( Value : DWord);
    function GetNumberOfFreeClusters : DWord;

    procedure SetTotalCluster( Value : DWord);
    function GetTotalCluster : DWord;

    procedure SetSerial ( Value : String);
    function GetSerial : String;
  public
    { Public declarations }
    constructor Create (Owner : TComponent); override;
  published
    property Drive : Char Read FDrive Write SetDrive;
    property DriverType : TDriverType read GetDriverType write SetDriverType;
    property Volume : String read GetVolume write SetVolume;
    property FileSystem : String read GetFileSystem write SetFileSystem;
    property DiskSize : Double read GetDiskSize write SetDiskSize;
    property FreeSpace : Double read GetFreeSpace write SetFreeSpace;
    property SizeType : TSizeType read FSizeType write SetSizeType;
    property SectorsPerCluster : DWord read GetSectorsPerCluster write SetSectorsPerCluster;
    property BytesPerSector : DWord read GetBytesPerSector write SetBytesPerSector;
    property NumberOfFreeClusters : DWord read GetNumberOfFreeClusters write SetNumberOfFreeClusters;
    property TotalCluster : DWord read GetTotalCluster write SetTotalCluster;
    property Serial : String read GetSerial write SetSerial;
    { Published declarations }
  end;

procedure Register;

implementation

constructor TDisk.Create ( Owner : TComponent);
begin
  inherited;
  FDrive:='C';
  GetFileSystem;
  GetDriverType;
  GetVolume;
  GetSectorsPerCluster;
  Drive := 'C';
end;

function TDisk.Getvolume : String;
var
  FileSystem,
  Name,
  Tmp      : PChar;
  PSer     : PDWord;
  Flags,
  NameSize : DWord;
  B        : BOOL;
begin
    New(Name);
    New(Tmp);
    New(PSer);
    New(FileSystem);
  try
    StrPCopy(Tmp,FDrive+':\');
    B:=GetVolumeInformation(Tmp,Name,255,PSer, NameSize, Flags, FileSystem, 20);
  finally
    if B Then
       Result:= StrPas(Name)
    else
       Result:='';
    FVolume:=Result;
    dispose(tmp);
    dispose(Name);
    dispose(PSer);
    Dispose(FileSystem);
  end;
end;

function TDisk.GetFileSystem : String;
var
  FileSystem,
  Name,
  Tmp      : PChar;
  PSer     : PDWord;
  Flags,
  NameSize : DWord;
begin
    New(Name);
    New(Tmp);
    New(PSer);
    New(FileSystem);
  try
    StrPCopy(Tmp,FDrive+':\');
    GetVolumeInformation(Tmp,Name,255,PSer, NameSize, Flags, FileSystem, 20);
  finally
    Result:= StrPas(FileSystem);
    FFileSystem:=Result;
    dispose(tmp);
    dispose(Name);
    dispose(PSer);
    Dispose(FileSystem);
  end;
end;

procedure TDisk.SetVolume ( Value : String);
var
  Tmp,
  Name    : PChar;
begin
  New(Tmp);
  New(Name);
  try
     FVolume:=Value;
     StrPCopy(Tmp,FDrive+':\');
     StrPCopy(Name,Value);
     SetVolumeLabel(Tmp,Name);
  finally
     Dispose(Tmp);
     Dispose(Name);
  end;
end;

function TDisk.GetDriverType : TDriverType;
Var
  Tmp : PChar;
begin
  New(Tmp);
  try
    StrPCopy(Tmp,FDrive+':\');
    case GetDriveType(Tmp) of
         0: Result:=dtNotDetermined;
         1: Result:=dtNoRoot;
         2: Result:=dtRemovable;
         3: Result:=dtFixed;
         4: Result:=dtNetwork;
         5: Result:=dtCD;
         6: Result:=dtRamDisk;
    end;
    FDriverType:=Result;
  finally
    dispose(tmp);
  end;
end;

procedure TDisk.SetDrive(Value: Char);
var
  Tmp : Integer;
begin
  Tmp:=Ord(Value);
  Tmp:=Tmp - 64;
  if SysUtils.DiskSize(Tmp)>= 0 then
     begin
       if (Value <> FDrive) then
          begin
            FDrive := UpCase(Value);
            SetDriverType(dtNoRoot);
            GetVolume;
            SetVolume (FVolume);
          end;
       end;
end;

procedure TDisk.SetDriverType( Value : TDriverType);
begin
  FDriverType:=GetDriverType;
end;

procedure TDisk.SetFileSystem( Value : String);
begin
  FFileSystem := GetFileSystem;
end;

procedure TDisk.SetDiskSize ( Value : Double);
begin
  FDiskSize := GetDiskSize;
end;

function TDisk.GetDisksize: Double;
var
  Tmp : Integer;
begin
  Tmp:=Ord(FDrive);
  Tmp:=Tmp - 64;
  case FSizeType of
       stByte:begin
                Result:=SysUtils.DiskSize(Tmp);
              end;
         stKB:begin
                Result:=(SysUtils.DiskSize(Tmp) / 1024);
              end;
         stMB:begin
                Result:=(SysUtils.DiskSize(Tmp) / (1024*1024));
              end;
         stGB:begin
                Result:=(SysUtils.DiskSize(Tmp) / (1024*1024*1024));
              end;
    end;
  Result:=StrToFloat(FormatFloat('0.00',Result));
end;

procedure TDisk.SetFreeSpace ( Value : Double);
begin
  FFreeSpace := GetFreeSpace;
end;

function TDisk.GetFreeSpace: Double;
var
  Tmp : Integer;
begin
  Tmp:=Ord(FDrive);
  Tmp:=Tmp - 64;
  case FSizeType of
       stByte:begin
                Result:=SysUtils.DiskFree(Tmp);
              end;
         stKB:begin
                Result:=(SysUtils.DiskFree(Tmp) / 1024);
              end;
         stMB:begin
                Result:=(SysUtils.DiskFree(Tmp) / (1024*1024));
              end;
         stGB:begin
                Result:=(SysUtils.DiskFree(Tmp) / (1024*1000*1024));
              end;
  end;
end;

procedure TDisk.SetSizeType ( Value : TSizeType);
begin
  if Value <> FSizeType then
     begin
       FSizeType := Value;
     end;
end;

procedure TDisk.SetSectorsPerCluster ( Value : DWord);
begin
  FSectorsPerCluster:=GetSectorsPerCluster;
end;

function TDisk.GetSectorsPerCluster : DWord;
var
  Tmp    : PChar;
  BPS,
  NOFC,
  NC,
  SPC    : DWORD;
begin
 New(Tmp);
 try
    StrPCopy(Tmp,FDrive+':\');
    if GetDiskFreeSpace(Tmp, SPC, BPS, NOFC, NC) then
       result:=SPC
    else
       result:=0;
    FSectorsPerCluster:=Result;
 finally
    dispose(Tmp);
 end;
end;

procedure TDisk.SetBytesPerSector ( Value : DWord);
begin
  FBytesPerSector:=GetBytesPerSector;
end;

function TDisk.GetBytesPerSector : DWord;
var
  Tmp    : PChar;
  BPS,
  NOFC,
  NC,
  SPC    : DWORD;
begin
 New(Tmp);
 try
    StrPCopy(Tmp,FDrive+':\');
    if GetDiskFreeSpace(Tmp, SPC, BPS, NOFC, NC) then
       result:=BPS
    else
       result:=0;
    FBytesPerSector:=Result;
 finally
    dispose(Tmp);
 end;
end;

procedure TDisk.SetNumberOfFreeClusters ( Value : DWord);
begin
  FNumberOfFreeClusters :=GetNumberOfFreeClusters;
end;

function TDisk.GetNumberOfFreeClusters : DWord;
var
  Tmp    : PChar;
  BPS,
  NOFC,
  NC,
  SPC    : DWORD;
begin
 New(Tmp);
 try
    StrPCopy(Tmp,FDrive+':\');
    if GetDiskFreeSpace(Tmp, SPC, BPS, NOFC, NC) then
       result:=NOFC
    else
       result:=0;
    FNumberOfFreeClusters:=Result;
 finally
    dispose(Tmp);
 end;
end;

procedure TDisk.SetTotalCluster ( Value : DWord);
begin
  FTotalCluster :=GetTotalCluster;
end;

function TDisk.GetTotalCluster : DWord;
var
  Tmp    : PChar;
  BPS,
  NOFC,
  NC,
  SPC    : DWORD;
begin
 New(Tmp);
 try
    StrPCopy(Tmp,FDrive+':\');
    if GetDiskFreeSpace(Tmp, SPC, BPS, NOFC, NC) then
       result:=NC
    else
       result:=0;
    FTotalCluster:=Result;
 finally
    dispose(Tmp);
 end;
end;

procedure TDisk.SetSerial ( Value : String);
begin
  FSerial := GetSerial;
end;


function TDisk.GetSerial : String;
var
  FileSystem,
  Name,
  Tmp      : PChar;
  PSer     : PDWord;
  Flags,
  NameSize : DWord;
  A        : String;
begin
    New(Name);
    New(Tmp);
    New(PSer);
    New(FileSystem);
  try
    StrPCopy(Tmp,FDrive+':\');
    GetVolumeInformation(Tmp,Name,255,PSer, NameSize, Flags, nil, 0);
  finally
    A:=IntToHex(PSer^,8);
    Insert('-',A,5);
    Result:= A;
    FSerial:=Result;
    dispose(tmp);
    dispose(Name);
    dispose(PSer);
    Dispose(FileSystem);
  end;
end;

procedure Register;
begin
  RegisterComponents('Cc', [TDisk]);
end;

end.
