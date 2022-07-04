unit SFXGenerator;

interface
uses
  Windows,
  Messages,
  Classes,
  SysUtils,
  ArchiverMisc,
  ArchiverRoot,
  CustSFXGenerator;

type
  DWord = Integer;
  TOverwritemode=(confirm,overwrite,skip,update,existing,updateexisting);
  TCommentMode=(none,Before,After,Both);

  TTagInfo = packed record
                ExecuteFileAfterExtract:boolean;
                UserChooseFilesToExtract:boolean;
                UserChooseOverwriteMode:boolean;
                UserAllowedToDontRunTheFile:boolean;
                DefaultOwerwriteMode:TOverwritemode;
                SFXFileSize:DWord; //Dateigröße des fertigen SFX, also: SFX.EXE-Code, sizeof(TTagefile) and Archivesize
                CommandLine:string[80];
                Caption:string[60];
                DefaultExtractPath:string[80];
                CopyrightLine:string[80];
                Language:TLanguage;
                Comment:TCommentMode;
  end;


  TSFXGenerator = class( TCustomSFXGenerator )
  protected
    function GetSFXCodeSize : Integer;
    procedure UpdateLanguage; override;

  public
    TagInfo : TTagInfo;

    // Creators & Destructor
    constructor Create( AOwner : TComponent ); override;
    destructor  Destroy; override;

    procedure WriteSFXCodeToStream( S : TStream ); override;
    procedure UpdateTagInfos( S : TStream ); override;
  end;

procedure Register;

implementation
{$R SFXCode.res}

procedure Register;
begin
  RegisterComponents('Backup Tools', [TSFXGenerator]);
end;

function TSFXGenerator.GetSFXCodeSize : Integer;
var
    ResHandle : THandle;
begin
   ResHandle  := FindResource( hinstance, PChar('SFXEXE_1'),'SFXEXE' );
   if ResHandle <> 0 then
     Result := SizeOfResource( hinstance, ResHandle )
   else
     Result := 0;
end;

procedure TSFXGenerator.UpdateLanguage;
begin
  inherited;
  TagInfo.Language := Language;
end;

constructor TSFXGenerator.Create( AOwner : TComponent );
begin
  inherited;
  NewSFXCodeSize := GetSFXCodeSize;
  NewTagInfoSize := sizeof(TTagInfo);
  TagInfo.ExecuteFileAfterExtract := True;
  TagInfo.UserChooseFilesToExtract := True;
  TagInfo.UserChooseOverwriteMode := True;
  TagInfo.UserAllowedToDontRunTheFile := True;
  TagInfo.DefaultOwerwriteMode := confirm;
  TagInfo.SFXFileSize := 0;
  TagInfo.CommandLine := '';
  TagInfo.Caption := 'SFX Archive';
  TagInfo.DefaultExtractPath := '<TD>';
  TagInfo.CopyrightLine := 'This is the CopyrightLine. © by Oliver Buschjost (autor_oliver@iname.com)';
  TagInfo.Language := Language;
  TagInfo.Comment := none;
end;

destructor  TSFXGenerator.Destroy;
begin
  inherited;
end;

procedure TSFXGenerator.WriteSFXCodeToStream( S : TStream );
var
    ResHandle, MemHandle : THandle;
    ResSize : Integer;
    pRes : Pointer;
begin
   ResHandle  := FindResource( hinstance, PChar('SFXEXE_1'),'SFXEXE' );
   if ResHandle = 0 then
     Exit;
   ResSize    := SizeOfResource( hinstance, ResHandle );
   if ResSize = 0 then
     Exit;
   MemHandle  := LoadResource( hinstance, ResHandle );
   if MemHandle = 0 then
     Exit;
   pRes       := LockResource( MemHandle );
   if not Assigned(pRes) then
     Exit;
   S.WriteBuffer( pRes^, ResSize );
   TagInfo.SFXFileSize := 0;
   S.WriteBuffer( TagInfo, sizeof(TagInfo) );
end;

procedure TSFXGenerator.UpdateTagInfos( S : TStream );
var
  TagInfo : TTaginfo;
  tmp : Integer;
begin
  // Update the SFXFileSize of the TagInfo
  S.Position := CurrentSFXCodeSize;
  tmp := Min( sizeof(TagInfo), CurrentTagInfoSize );
  S.ReadBuffer( TagInfo, tmp );
  TagInfo.SFXFileSize := S.Size;
  S.Position := CurrentSFXCodeSize;
  S.WriteBuffer( TagInfo, tmp );
end;

end.

