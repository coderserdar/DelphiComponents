unit CakCryptoZip;

interface

uses
  Windows, Messages, SysUtils, Classes, CakDefs2, CakArchiver, CakDelphiZip, Dialogs;

const CanExtract = TRUE;
      CanAdd = TRUE;
      CanList = TRUE;
      CanSFX = FALSE;
      Dllname = 'czip2.dll';

const   // REMEMBER to add this const section!!!

        // These are the possible results to GetArcType function
        Standard=10;
        SelfLock=11;
        SelfErase=12;

        // These are the possible result to GetEncType function
        BlowFish=13;
        Rijndael=14;
        TwoFish=15;
        CZIP2=16;

(* DLL ERROR CODES

 - 0: action successful
 - 1: Key matching
 - 5: Key doesn't match

 - 10: not a CZIP 2.1 file
 - 20: Valid CZIP 2.1 file

 - 100: wrong key used while decrypting
 - 101: wrong key used while decrypting a self-locking archive
 - 102: wrong key used while decrypting a self-erasing archive
 - 110: archive locked
 - 111: archive erased

 - 255: encryption failed 
*)

Type
  CZKey = record Part1, Part2, Part3 : Integer; end;

  TCakCryptoZip = class(TCakDelphiZip)
  private
    { Private declarations }
    Stopping : boolean;
    TempZipFileName : string;
  protected
    { Protected declarations }
  public
    { Public declarations }
    procedure SetCakdir(pCakdir : TComponent); override;
    procedure Process(dowhat : WorkType); override;
    function Cando(aWork: WorkType): Boolean; override;
    function Def_TreatAs : string; override;
    procedure Load_DLL; override;
    procedure UnLoad_DLL; override;
    function DllExists : Boolean; override;
    destructor Destroy; override;
    //procedure DoStop(Stopp: Boolean); override;
  published
    { Published declarations }
    property Stop: Boolean read Stopping write DoStop;
  end;

var
HCZip2 : THandle;
GetAuthorName     : function (Filename: PChar): PChar;
GetAuthorMail     : function (Filename: PChar): PChar;
GetFiles          : function (Filename: PChar): Integer;
GetChances        : function (Filename: PChar): Integer;
GetArcType        : function (Filename: PChar): Integer;
GetEncType        : function (Filename: PChar): Integer;
GetPassPhrase     : function (Filename: PChar): PChar;
GetKey            : function (Filename: PChar): CZKey;
ComparePassPhrase : function (Filename, PassPhrase: PChar): integer;
CompareKey        : function (Filename: PChar; Key1, Key2, Key3: integer): integer;
ResetChances      : function (Filename: PChar): integer; 
DecCZ             : function (Filename, Dest: PChar; Key1, Key2, Key3: integer; Progress, UseHD: boolean): integer; 
DecBF             : function (Filename, Dest, PassPhrase: PChar): integer; 
DecTF             : function (Filename, Dest, PassPhrase: PChar): integer;
DecAes            : function (Filename, Dest, PassPhrase: PChar): integer; 
EncCZ             : function (InFile, OutFile, AuthorName, AuthorMail: PChar; Key1, Key2, Key3: integer;
                    FileDate: TDateTime; Files, ArcType: integer; Progress, UseHD: boolean): integer; 
EncTF             : function (InFile, OutFile, AuthorName, AuthorMail: PChar; PassPhrase: PChar;
                    FileDate: TDateTime; Files, ArcType: integer; Progress: boolean): integer;
EncBF             : function (InFile, OutFile, AuthorName, AuthorMail: PChar; PassPhrase: PChar;
                    FileDate: TDateTime; Files, ArcType: integer; Progress: boolean): integer;
EncAes            : function (InFile, OutFile, AuthorName, AuthorMail: PChar; PassPhrase: PChar;
                    FileDate: TDateTime; Files, ArcType: integer; Progress: boolean): integer;

procedure Register;

implementation
uses CakUtils2, Cakdir2;
var Cakdir : TCakdir2;

procedure TCakCryptoZip.SetCakdir(pCakdir : TComponent);
begin
  Cakdir := TCakdir2(pCakdir);
  inherited SetCakdir(pCakdir);
end;

procedure TCakCryptoZip.Process(dowhat : WorkType);
var ArchivName : string;
    continue : boolean;
    targetName : string;
procedure HandleError(rescode : integer);
begin
continue := false;
case rescode of
        0: continue := true;
        110: showmessage('Archive locked');
        111: showmessage('Archive erased');
        101,102: showmessage('Wrong key, chances reduced by 1');
        100: showmessage('Wrong key');
        end;
end;
procedure ProcessArchive;
begin
    TempZipFileName := GrabTempPath + RemoveFileExt(Extractfilename(TCakdir2(Cakdir).ArchiveName)) + '.zip';

    with TCakdir2(Cakdir) do
    Case GetEncType(PChar(ArchiveName)) of
    CZip2 :
    if (CZPassKey.Part1 = -1) or (CZPassKey.Part2 = -1) or (CZPassKey.Part3 = -1) then
      begin
        CZPassKey.Part1 := 0;
        CZPassKey.Part2 := 0;
        CZPassKey.Part3 := 0;

        if Assigned(FOnCZipPwd) then FOnCZipPwd(nil,ArchiveName,'',CZPassKey.Part1,CZPassKey.Part2,CZPassKey.Part3);

      end;
      Else if Assigned(FOnPwd) then FOnPwd(nil,ArchiveName,'',Password);
    end;


    with (Cakdir as TCakdir2) do
    if not fileexists(TempZipFileName) then
    Case GetEncType(PChar(ArchiveName)) of
     Czip2 : HandleError(DecCZ(PCHar(Archivename),PChar(GrabTempPath),
                  CZPassKey.Part1,CZPassKey.Part2,CZPassKey.Part3,True,True));
     BlowFish : HandleError(DecBF(PChar(ArchiveName),PChar(GrabTempPath), PChar(Password)));
     Rijndael : HandleError(DecAES(PChar(ArchiveName),PChar(GrabTempPath), PChar(Password)));
     TwoFish  : HandleError(DecTF(PChar(ArchiveName),PChar(GrabTempPath), PChar(Password)));
    end else continue := true;

    Archivname := TCakdir2(Cakdir).ArchiveName;
    if continue then
    try
    TCakdir2(Cakdir).Arcname := TempZipFileName;
    inherited Process(DoWhat);
    finally
    TCakdir2(Cakdir).Arcname := Archivname;
    end;
end;
begin
    Load_DLL;

    Case doWhat of
    wtClose : if TempZipFileName <> '' then
                if fileexists(TempZipFileName) then Deletefile(TempZipFileName);
    wtLoadContents, wtExtract, wtTest : ProcessArchive;
    wtEncrypt : begin
                  with (Cakdir as TCakdir2) do
                  begin
                  TargetName := ChangeFileExt(EncryptOptions.File2Encrypt,'.czip');

                  Case EncryptOptions.EncryptMethod of
                  ceCZip : EncCZ(PChar(EncryptOptions.File2Encrypt),PChar(TargetName),'','',CZPassKey.Part1,
                                 CZPassKey.Part2,CZPassKey.Part3,now,0,0,true,false);
                  ceBlowFish : EncBF(PChar(EncryptOptions.File2Encrypt),PChar(TargetName),'','',PChar(Password),now,0,0,true);
                  ceRijndael : EncAES(PChar(EncryptOptions.File2Encrypt),PChar(TargetName),'','',PChar(Password),now,0,0,true);
                  ceTwoFish  : EncTF(PChar(EncryptOptions.File2Encrypt),PChar(TargetName),'','',PChar(Password),now,0,0,true);
                  end;
                  If Assigned(FOnMsg) then
                    FOnMsg(Nil,CODE_NOERROR,-1,Msg_OK,ERR_NOERROR);
                  end;
                end;
    wtAdd, wtDelete : begin
                        ProcessArchive;
                        with (Cakdir as TCakdir2) do
                        Case GetEncType(PChar(ArchiveName)) of
                        Czip2 : HandleError(EncCZ(PCHar(TempZipFileName),PChar(Archivename),'','',
                                CZPassKey.Part1,CZPassKey.Part2,CZPassKey.Part3,now,0,0,True,True));
                        BlowFish : HandleError(EncBF(PChar(TempZipFileName),PChar(Archivename),'','', PChar(Password),now,0,0,true));
                        Rijndael : HandleError(EncAES(PChar(TempZipFileName),PChar(Archivename),'','', PChar(Password),now,0,0,true));
                        TwoFish  : HandleError(EncTF(PChar(TempZipFileName),PChar(Archivename),'','', PChar(Password),now,0,0,true));
                        end;
                      end;
    end;
end;

function TCakCryptoZip.DllExists : boolean;
begin
        Result := Fileexists(GrabProgrampath+Dllname);
end;

function TCakCryptoZip.Cando(aWork: WorkType): Boolean;
begin
        Case aWork of
        wtNone : Result := true;
        wtTest, wtExtract : Result := CanExtract;
        wtAdd, wtDelete : Result := CanAdd;
        wtSFX :  Result := CanSFX;
        wtLoadContents: Result := CanList;
        wtEncrypt : Result := true;
        else Result := false;
        end;
        if Result then
          if not DLLExists then
            Result := false;
end;

function TCakCryptoZip.Def_TreatAs : string;
begin
end;

procedure TCakCryptoZip.Load_DLL;
begin
inherited Load_DLL;
if Fileexists(GrabProgramPath + 'CZip2.dll') then
begin
HCZip2 := LoadLibrary(PChar(GrabProgramPath + 'CZip2.dll'));
if HCZip2 >= 32 then
  begin
    GetAuthorName := GetProcAddress(HCZip2, 'GetAuthorName');
    GetAuthorMail := GetProcAddress(HCZip2, 'GetAuthorMail');
    GetFiles      := GetProcAddress(HCZip2, 'GetFiles');
    GetChances    := GetProcAddress(HCZip2, 'GetChances');
    GetArcType    := GetProcAddress(HCZip2, 'GetArcType');
    GetEncType    := GetProcAddress(HCZip2, 'GetEncType');
    GetPassPhrase := GetProcAddress(HCZip2, 'GetPassPhrase');
    GetKey        := GetProcAddress(HCZip2, 'GetKey');
    ComparePassPhrase := GetProcAddress(HCZip2, 'ComparePassPhrase');
    CompareKey    := GetProcAddress(HCZip2, 'CompareKey');
    ResetChances  := GetProcAddress(HCZip2, 'ResetChances');
    DecCZ         := GetProcAddress(HCZip2, 'DecCZ');
    DecBF         := GetProcAddress(HCZip2, 'DecBF');
    DecTF         := GetProcAddress(HCZip2, 'DecTF');
    DecAES        := GetProcAddress(HCZip2, 'DecAES');
    EncCZ         := GetProcAddress(HCZip2, 'EncCZ');
    EncBF         := GetProcAddress(HCZip2, 'EncBF');
    EncTF         := GetProcAddress(HCZip2, 'EncTF');
    EncAES        := GetProcAddress(HCZip2, 'EncAES');
  end;
end;
end;

destructor TCakCryptoZip.Destroy;
begin
    if fileexists(TempZipFileName) then Deletefile(TempZipFileName);
    inherited Destroy;
end;


procedure TCakCryptoZip.UnLoad_DLL;
begin
  FreeLibrary(HCZip2);
  inherited UnLoad_DLL;
end;

procedure Register;
begin
  //RegisterComponents('QZip', [TCakCryptoZip]);
end;

end.
