unit MyArchBackup;
(*-----------------------------------------------------------------------------
  DESCRIPTION

  This component is freeware. You may use it, distribute it and modify it, but
  you may not charge for it.
  In case of modifications you must mail me (mmm@imaginet.fr) a copy of
  the modifications. The reason are simple: Any changes that improve this free-
  ware component should be to benefit for everybody, not only you. That way you
  can be pretty sure, that this component has few errors and much functionality.
  In case of modifications, you will be on the credits list beneath:

  This is an example of how to use an interface between TMyBackup and an
  archiving component like TArchiver or like DelZip.

  If you don't define the property Archiver of TMyArchiverInt, then the
  component will automatically create an instance of TArchiver.
  But if you drop a TArchiver and connect it to the TMyArchiverInt with
  its Archiver property, you'll be able to adjust TArchiver settings.
*)

interface
uses Classes, ArchiverRoot, CustExtractor, CustArchiver, Archiver, MyBackup;
type
  TMyArchiverInt = class(TArchiverInterface)
  protected
    FArchiver : TArchiver;

    procedure SetFileName( const aFileName : String ); override;
    function  GetFileName : String; override;
    procedure SetExtractPath( const aPath : String ); override;
    function  GetExtractPath : String; override;
    procedure SetLanguage( lang : TLanguage );
    function  GetLanguage : TLanguage;

    procedure AddFileEvent( Sender : TObject; var FileEntry : TFileEntry; var Accept : Boolean );
    procedure ExtractFileEvent( Sender : TObject; const FileEntry : TFileEntry;
                                var DestPath : String; var Accept : Boolean );
    procedure FileProgressEvent( Sender : TObject; Percent : Integer );
    procedure FileExtractedFromArchive( Sender : TObject; const FileEntry : TFileEntry; const DestPath : String );

    procedure Loaded; override;
    procedure CheckArchiver;

  public
    constructor Create( AOwner : TComponent ); override;
    destructor  Destroy; override;

    procedure Open; override;
    procedure Close; override;
    function  AddFile( const aFileName : String ) : Boolean; override;
    function  AddFiles( files : TStrings ) : Boolean; override;
    function  AddDirectory( const Directory : String ) : Boolean; override;
    procedure ExtractFiles; override;
    procedure SetRecursive( val : Boolean ); override;
    procedure Delete; override;

  published
    property Archiver : TArchiver read FArchiver write FArchiver;
  end;

  TMyArchBackup = class( TMyBackup )
  protected
    function  CreateArchiver : TArchiverInterface; override;
  public
  end;

  procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Backup Tools', [TMyArchBackup, TMyArchiverInt]);
end;

procedure TMyArchiverInt.SetFileName( const aFileName : String );
begin
  FArchiver.FileName := aFileName;
end;

function  TMyArchiverInt.GetFileName : String;
begin
  Result := FArchiver.FileName;
end;

procedure TMyArchiverInt.SetExtractPath( const aPath : String );
begin
  FArchiver.ExtractPath := aPath;
end;

function  TMyArchiverInt.GetExtractPath : String;
begin
  Result := FArchiver.ExtractPath;
end;

procedure TMyArchiverInt.SetLanguage( lang : TLanguage );
begin
    case lang of
    lgEnglish:    FArchiver.Language := ArchiverRoot.lgEnglish;
    lgFrench:     FArchiver.Language := ArchiverRoot.lgFrench;
    lgGerman:     FArchiver.Language := ArchiverRoot.lgGerman;
    lgSpanish:    FArchiver.Language := ArchiverRoot.lgSpanish;
    lgItalian:    FArchiver.Language := ArchiverRoot.lgItalian;
    lgPortuguese: FArchiver.Language := ArchiverRoot.lgPortuguese;
    lgChinese:    FArchiver.Language := ArchiverRoot.lgChinese;
    lgChineseGB:  FArchiver.Language := ArchiverRoot.lgChineseGB;
    lgDutch:      FArchiver.Language := ArchiverRoot.lgDutch;
    lgCzech:      FArchiver.Language := ArchiverRoot.lgCzech;
    else
      FArchiver.Language := ArchiverRoot.lgEnglish;
    end;
end;

function  TMyArchiverInt.GetLanguage : TLanguage;
begin
  case FArchiver.Language of
  ArchiverRoot.lgEnglish:    Result := MyBackup.lgEnglish;
  ArchiverRoot.lgFrench:     Result := MyBackup.lgFrench;
  ArchiverRoot.lgGerman:     Result := MyBackup.lgGerman;
  ArchiverRoot.lgSpanish:    Result := MyBackup.lgSpanish;
  ArchiverRoot.lgItalian:    Result := MyBackup.lgItalian;
  ArchiverRoot.lgPortuguese: Result := MyBackup.lgPortuguese;
  ArchiverRoot.lgChinese:    Result := MyBackup.lgChinese;
  ArchiverRoot.lgChineseGB:  Result := MyBackup.lgChineseGB;
  ArchiverRoot.lgDutch:      Result := MyBackup.lgDutch;
  ArchiverRoot.lgCzech:      Result := MyBackup.lgCzech;
  else
    Result := MyBackup.lgEnglish;
  end;
end;

constructor TMyArchiverInt.Create( AOwner : TComponent );
begin
  inherited;
end;

destructor  TMyArchiverInt.Destroy;
begin
  inherited;
end;

procedure TMyArchiverInt.Open;
begin
  CheckArchiver;
  FArchiver.Open;
end;

procedure TMyArchiverInt.Close;
begin
  CheckArchiver;
  FArchiver.Close;
end;

function  TMyArchiverInt.AddFile( const aFileName : String ) : Boolean;
begin
  CheckArchiver;
  Result := FArchiver.AddFile( aFileName );
end;

function  TMyArchiverInt.AddFiles( files : TStrings ) : Boolean;
begin
  CheckArchiver;
  Result := FArchiver.AddFiles( files );
end;

function  TMyArchiverInt.AddDirectory( const Directory : String ) : Boolean;
begin
  CheckArchiver;
  Result := FArchiver.AddDirectory( Directory );
end;

procedure TMyArchiverInt.ExtractFiles;
begin
  CheckArchiver;
  FArchiver.ExtractFiles;
end;

procedure TMyArchiverInt.SetRecursive( val : Boolean );
begin
  CheckArchiver;
  if val then
    FArchiver.Options := FArchiver.Options + [oRecurseFolders]
  else
    FArchiver.Options := FArchiver.Options - [oRecurseFolders];
end;

procedure TMyArchiverInt.Delete;
begin
  CheckArchiver;
  FArchiver.Delete;
end;

procedure TMyArchiverInt.AddFileEvent( Sender : TObject; var FileEntry : TFileEntry; var Accept : Boolean );
begin
  if Assigned(FOnAddFile) then
    FOnAddFile( Self, FileEntry.Name, FileEntry.ArchiveInfo.Size );
end;

procedure TMyArchiverInt.ExtractFileEvent( Sender : TObject; const FileEntry : TFileEntry;
                                var DestPath : String; var Accept : Boolean );
begin
  if Assigned(FOnExtractFile) then
    FOnExtractFile( Self, FileEntry.Name, FileEntry.ArchiveInfo.Size );
end;

procedure TMyArchiverInt.FileProgressEvent( Sender : TObject; Percent : Integer );
begin
  if Assigned(FOnFileProgress) then
    FOnFileProgress( Self, Percent );
end;

procedure TMyArchiverInt.FileExtractedFromArchive( Sender : TObject; const FileEntry : TFileEntry; const DestPath : String );
begin
  if Assigned(FOnFileExtracted) then
    FOnFileExtracted( Self, FileEntry.Name, DestPath, FileEntry.ArchiveInfo.Size );
end;

procedure TMyArchiverInt.Loaded;
begin
  inherited;
  CheckArchiver;
end;

procedure TMyArchiverInt.CheckArchiver;
begin
  if not Assigned(FArchiver) then
    begin
      FArchiver := TArchiver.Create( Owner );
      FArchiver.OnAddFile := AddFileEvent;
      FArchiver.OnExtractFile := ExtractFileEvent;
      FArchiver.OnFileExtracted := FileExtractedFromArchive;
      FArchiver.OnFileProgress := FileProgressEvent;
    end;
end;

function  TMyArchBackup.CreateArchiver : TArchiverInterface;
begin
  Result := TMyArchiverInt.Create(Self);
end;

end.
