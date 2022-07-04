unit CustArchiver;

interface
uses
  Windows,
  SysUtils,
  Classes,
  ArchiverMisc,
  ArchiverRoot,
  CustExtractor;

type
  TPathStorage = (psNone, psWhole, psRelative);

  TCompressionLevel = (clMaximum, clNormal, clFast, clSuperFast, clNone );

  TAddMode = (amAdd, amAddAndReplace, amUpdate, amFreshen);

  TOnAddFileEvent          = procedure ( Sender : TObject; var FileEntry : TFileEntry; var Accept : Boolean ) of Object;
  TOnFileAddedEvent        = procedure ( Sender : TObject; const FileEntry : TFileEntry ) of Object;
  TOnCompressBlockEvent    = function ( Sender : TObject; DestBlock : PChar; var DestSize : Integer; SrcBlock : PChar; SrcSize : Integer; Level : TCompressionLevel) : Boolean of Object;
  TOnDeleteFileEvent       = procedure ( Sender : TObject; const FileEntry : TFileEntry; var Accept : Boolean ) of Object;
  TOnDeleteFileByIndexEvent= procedure ( Sender : TObject; Index : Integer; var Accept : Boolean ) of Object;
  TOnCryptBlockEvent       = procedure ( Sender : TObject; DestBlock, SrcBlock : PChar; var DestSize : Integer; SrcSize : Integer) of Object;
  TOnNeedNewDiskEvent      = procedure ( Sender : TObject; Segment : Integer; var Drive : String ) of Object;
  TOnNeedNewFolderEvent    = procedure ( Sender : TObject; Segment : Integer; var Path : String ) of Object;
  TOnClearDiskEvent        = procedure ( Sender : TObject; const Drive : String ) of Object;
  TOnWriteSFXCodeEvent     = procedure ( Sender : TObject; Stream : TStream ) of Object;

  EArchiverCompress = class( EArchiver );

  TArchMessages = class(TExtrMessages)
    protected
      FCouldNotCompressBlock : String;
      FDeleteFileEventNeeded : String;
      FCouldNotRenameArchive : String;
      FInsertNewDisk : String;
      FSelectNewPlace : String;
      FNotEnoughFreeSpaceOn : String;
      FUnableToDeleteFiles : String;
      FConfirmFileDeletion : String;
      FMaxSegmentSizeTooSmall : String;
      FCantPerformThisOp : String;
      FAddingFile : String;
      FDeletingFiles : String;
      FDeleteFile : String;
      FMakingSFXArchive : String;
      FCompressingSolidArchive : String;
      FCopyingArchive : String;
      FCouldNotCopyArchive : String;
      FUpdatingFile : String;
      FReplacingFile : String;
      FCantDeleteInExtStr : String;

      procedure AssignTo(Dest: TPersistent); override;

    public
      procedure SetLanguage( language : TLanguage ); override;

    published
      property CouldNotCompressBlock : String read FCouldNotCompressBlock write FCouldNotCompressBlock;
      property DeleteFileEventNeeded : String read FDeleteFileEventNeeded write FDeleteFileEventNeeded;
      property CouldNotRenameArchive : String read FCouldNotRenameArchive write FCouldNotRenameArchive;
      property InsertNewDisk : String read FInsertNewDisk write FInsertNewDisk;
      property SelectNewPlace : String read FSelectNewPlace write FSelectNewPlace;
      property NotEnoughFreeSpaceOn : String read FNotEnoughFreeSpaceOn write FNotEnoughFreeSpaceOn;
      property UnableToDeleteFiles : String read FUnableToDeleteFiles write FUnableToDeleteFiles;
      property ConfirmFileDeletion : String read FConfirmFileDeletion write FConfirmFileDeletion;
      property MaxSegmentSizeTooSmall : String read FMaxSegmentSizeTooSmall write FMaxSegmentSizeTooSmall;
      property CantPerformThisOp : String read FCantPerformThisOp write FCantPerformThisOp;
      property AddingFile : String read FAddingFile write FAddingFile;
      property DeletingFiles : String read FDeletingFiles write FDeletingFiles;
      property DeleteFile : String read FDeleteFile write FDeleteFile;
      property MakingSFXArchive : String read FMakingSFXArchive write FMakingSFXArchive;
      property CompressingSolidArchive : String read FCompressingSolidArchive write FCompressingSolidArchive;
      property CopyingArchive : String read FCopyingArchive write FCopyingArchive;
      property CouldNotCopyArchive : String read FCouldNotCopyArchive write FCouldNotCopyArchive;
      property UpdatingFile : String read FUpdatingFile write FUpdatingFile;
      property ReplacingFile : String read FReplacingFile write FReplacingFile;
      property CantDeleteInExtStr : String read FCantDeleteInExtStr write FCantDeleteInExtStr;
  end;

  TCustomArchiver = class( TCustomExtractor )
  protected
    FPathStorage : TPathStorage;
    FRelativePath : String;
    FMinFreeSpace : TFileSize;
    FCompressionLevel : TCompressionLevel;
    FReserveSpace : TFileSize;
    FAddMode : TAddMode;

    FOnAddFile : TOnAddFileEvent;
    FOnFileAdded : TOnFileAddedEvent;
    FOnCompressBlock : TOnCompressBlockEvent;
    FOnDeleteFile : TOnDeleteFileEvent;
    FOnDeleteFileByIndex : TOnDeleteFileByIndexEvent;
    FOnCryptBlock : TOnCryptBlockEvent;
    FOnNeedNewDisk : TOnNeedNewDiskEvent;
    FOnNeedNewFolder : TOnNeedNewFolderEvent;
    FOnClearDisk : TOnClearDiskEvent;
    FOnWriteSFXCode : TOnWriteSFXCodeEvent;

    procedure AssignTo(Dest: TPersistent); override;
    function  CreateMessages : TMessages; override;
    function  GetMessages : TArchMessages;
    procedure SetMessages( val : TArchMessages );
    procedure CompressStream( src : TStream );
    function  CompressBlock( DestBlock : PChar; var DestSize : Integer; SrcBlock : PChar; SrcSize : Integer ) : Boolean; virtual;
    procedure CryptBlock( DestBlock, SrcBlock : PChar; var DestSize : Integer; SrcSize : Integer); virtual;
    procedure SetMaxSegmentSize( val : TFileSize );
    function  RequestSpace( val : Integer ) :  Boolean; override;
    function  IsValidDrive( const drive : String ) : Boolean;
    function  CanUseDrive( const drive : String ) : Boolean;
    procedure AskNewDisk;
    procedure NextSegment;
    procedure CreateSegment;
    procedure CreateArchive; override;
    function  GetFreeSpace( const drive : String ) : TFileSize;
    function  CompressionLevelAsInteger : Integer; virtual;
    procedure WriteSFXCode( S : TStream ); virtual;
    procedure AfterUpdate; override;
    function  CompressSolidData : String;
    procedure CloseSolidData; override;
    function  GetOpenMode : Integer; override;
    procedure UpdateArchiveSize;
    procedure BeforeClose; override;
    function  DoAddFile( const FileName : String ) : Boolean;
    function  DoAddFiles( files : TStrings ) : Boolean;
    function  DoAddDirectory( const Directory : String ) : Boolean;
    procedure DoDeleteFiles;
    function  DoMakeSFX : Boolean;
    procedure DoSetArchiveComment( const comment : String );

  public
    // Creators & Destructor
    constructor Create( AOwner : TComponent ); override;

    // Public methods

    function  AddFile( const FileName : String ) : Boolean;
    function  AddFiles( files : TStrings ) : Boolean;
    function  AddDirectory( const Directory : String ) : Boolean;
    procedure DeleteFiles;
    function  MakeSFX : Boolean;
    procedure SetArchiveComment( const comment : String );

    // Misc methods
    procedure EraseDrive( const drive : String );

    // Public properties
    property ReserveSpace : Integer read FReserveSpace write FReserveSpace;

  published
    // Properties
    property AddMode : TAddMode read FAddMode write FAddMode;
    property CompressionLevel : TCompressionLevel read FCompressionLevel write FCompressionLevel;
    property MaxSegmentSize : TFileSize read GetMaxSegmentSize write SetMaxSegmentSize;
    property Messages : TArchMessages read GetMessages write SetMessages;
    property MinFreeSpace : TFileSize read FMinFreeSpace write FMinFreeSpace;
    property PathStorage : TPathStorage read FPathStorage write FPathStorage;
    // Events
    property OnAddFile : TOnAddFileEvent read FOnAddFile write FOnAddFile;
    property OnCompressBlock : TOnCompressBlockEvent read FOnCompressBlock write FOnCompressBlock;
    property OnDeleteFile : TOnDeleteFileEvent read FOnDeleteFile write FOnDeleteFile;
    property OnDeleteFileByIndex : TOnDeleteFileByIndexEvent read FOnDeleteFileByIndex write FOnDeleteFileByIndex;
    property OnFileAdded : TOnFileAddedEvent read FOnFileAdded write FOnFileAdded;
    property OnCryptBlock : TOnCryptBlockEvent read FOnCryptBlock write FOnCryptBlock;
    property OnNeedNewDisk : TOnNeedNewDiskEvent read FOnNeedNewDisk write FOnNeedNewDisk;
    property OnNeedNewFolder : TOnNeedNewFolderEvent read FOnNeedNewFolder write FOnNeedNewFolder;
    property OnClearDisk : TOnClearDiskEvent read FOnClearDisk write FOnClearDisk;
    property OnWriteSFXCode : TOnWriteSFXCodeEvent read FOnWriteSFXCode write FOnWriteSFXCode;
  end;


implementation

////////////////////////////////////////////////////////////

procedure TArchMessages.AssignTo(Dest: TPersistent);
begin
  if Dest is TMessages then
    with TMessages( Dest ) do begin
      FCouldNotCompressBlock   := Self.FCouldNotCompressBlock;
      FDeleteFileEventNeeded   := Self.FDeleteFileEventNeeded;
      FCouldNotRenameArchive   := Self.FCouldNotRenameArchive;
      FInsertNewDisk           := Self.FInsertNewDisk;
      FSelectNewPlace          := Self.FSelectNewPlace;
      FNotEnoughFreeSpaceOn    := Self.FNotEnoughFreeSpaceOn;
      FUnableToDeleteFiles     := Self.FUnableToDeleteFiles;
      FConfirmFileDeletion     := Self.FConfirmFileDeletion;
      FMaxSegmentSizeTooSmall  := Self.FMaxSegmentSizeTooSmall;
      FCantPerformThisOp       := Self.FCantPerformThisOp;
      FAddingFile              := Self.FAddingFile;
      FDeletingFiles           := Self.FDeletingFiles;
      FDeleteFile              := Self.FDeleteFile;
      FMakingSFXArchive        := Self.FMakingSFXArchive;
      FCompressingSolidArchive := Self.FCompressingSolidArchive;
      FCopyingArchive          := Self.FCopyingArchive;
      FCouldNotCopyArchive     := Self.FCouldNotCopyArchive;
      FUpdatingFile            := Self.FUpdatingFile;
      FReplacingFile           := Self.FReplacingFile;
      FCantDeleteInExtStr      := Self.FCantDeleteInExtStr;
    end;
  inherited AssignTo( Dest );
end;

procedure TArchMessages.SetLanguage( language : TLanguage );
var
  lang : TLanguage;
begin
  inherited;
  if FLanguage = lgAutomatic then
    lang := GetUserLanguage
  else
    lang := FLanguage;
  case lang of
    lgEnglish:
      begin
        FCouldNotCompressBlock := 'Could not compress Block';
        FDeleteFileEventNeeded := 'You must define the event OnDeleteFile in order to filter the files that must be deleted.';
        FCouldNotRenameArchive := 'Could not rename archive from "%s" to "%s"';
        FInsertNewDisk := 'Insert new disk for segment #%d in drive %s';
        FSelectNewPlace := 'Select a new place for storing the segment #%d of Archive %s';
        FNotEnoughFreeSpaceOn := 'There''s not enough free space on drive %s';
        FUnableToDeleteFiles := 'Unable to delete files from a segmented archive';
        FConfirmFileDeletion := 'Drive %s contains files. Do you want to delete them ?';
        FMaxSegmentSizeTooSmall := 'Maximum size of a Segment is too small (property MaxSegmentSize)';
        FCantPerformThisOp := 'Can''t perform this operation on a segmented archive';
        FAddingFile := 'Adding %s (%.0n)';
        FDeletingFiles := 'Deleting files...';
        FDeleteFile := 'Delete file %s';
        FMakingSFXArchive := 'Making a SFX archive...';
        FCompressingSolidArchive := 'Compressing solid archive';
        FCopyingArchive := 'Copying archive...';
        FCouldNotCopyArchive := 'Could not copy archive';
        FUpdatingFile := 'Updating %s (%.0n)';
        FReplacingFile := 'Replacing %s (%.0n)';
        FCantDeleteInExtStr := 'Can''t delete files in an external stream';
     end;
    lgFrench:
      begin
        FCouldNotCompressBlock := 'Impossible de compresser le bloc de données';
        FDeleteFileEventNeeded := 'Vous devez définir l''événement OnDeleteFile afin de filtrer les fichiers devant être supprimés.';
        FCouldNotRenameArchive := 'Impossible de renommer l''archive "%s" en "%s"';
        FInsertNewDisk := 'Insérez un nouveau disque pour le segment #%d dans l''unité %s';
        FSelectNewPlace := 'Choisissez un nouveau répertoire pour stocker le segment #%d de l''Archive %s...';
        FNotEnoughFreeSpaceOn := 'Il n''y a plus assez de place sur l''unité %s';
        FUnableToDeleteFiles := 'Il n''est pas possible de supprimer des fichiers dans une archive segmentée';
        FConfirmFileDeletion := 'L''unité %s contient des fichiers. Voulez-vous réellement les supprimer ?';
        FMaxSegmentSizeTooSmall := 'La taille maximum d''un segment est trop petite (propriété MaxSegmentSize)';
        FCantPerformThisOp := 'Impossible d''effectuer cette opération sur une archive segmentée';
        FAddingFile := 'Ajout de %s (%.0n)';
        FDeletingFiles := 'Suppression de fichiers...';
        FDeleteFile := 'Suppression de %s';
        FMakingSFXArchive := 'Création d''une archive Auto-Extractible...';
        FCompressingSolidArchive := 'Compression de l''archive solide';
        FCopyingArchive := 'Copie de l''archive...';
        FCouldNotCopyArchive := 'Could not copy archive';
        FUpdatingFile := 'Mise à jour de %s (%.0n)';
        FReplacingFile := 'Remplacement de %s (%.0n)';
        FCantDeleteInExtStr := 'Impossible de supprimer des fichiers dans un Stream externe';
      end;
    lgChinese: // Thanks to Huanlin Tsai (Taiwan)
      begin
        FCouldNotCompressBlock := 'µLªkÀ£ÁY°Ï¶ô¡C';
        FDeleteFileEventNeeded := '±z¥²¶·©w¸q OnDeleteFile ¨Æ¥ó¥H¿z¿ï¥X­n§R°£ªºÀÉ®×¡C';
        FCouldNotRenameArchive := 'µLªk±NÀÉ®×¦WºÙ¥Ñ "%s" §ó§ï¬° "%s"¡C';
        FInsertNewDisk := '½Ð´¡¤JªÅ¥ÕºÏ¤ù(²Ä %d ¤ù)¦Ü %s ºÏºÐ¾÷¤¤¡C';
        FSelectNewPlace := '½Ð«ü©w¤@­Ó¦ì¸m¥HÀx¦sÀ£ÁYÀÉªº²Ä %d ¤ù¤À³ÎÀÉ¡C'#10#13'(¤À³Î¦ÛÀ£ÁYÀÉ %s)...';
        FNotEnoughFreeSpaceOn := 'ºÏºÐªÅ¶¡¤£¨¬: ºÏºÐ¾÷ %s';
        FUnableToDeleteFiles := 'µLªk±q¤@­Ó¤À³ÎªºÀ£ÁYÀÉ®×¤¤§R°£¨ä¤¤ªºÀÉ®×¡C';
        FConfirmFileDeletion := 'ºÏºÐ¾÷ %s ¸Ì­±¦³ÀÉ®×¦s¦b, ¬O§_­n§R°£¥¦­Ì?';
        FMaxSegmentSizeTooSmall := 'ÄÝ©Ê MaxSegmentSize ¼Æ­È¤Ó¤p¡C';
        FCantPerformThisOp := 'µLªk¹ï¤@­Ó¤À³ÎªºÀ£ÁYÀÉ°õ¦æ¦¹°Ê§@';
        FAddingFile := '¥[¤JÀÉ®× %s (%.0n)';
        FDeletingFiles := '¥¿¦b§R°£ÀÉ®×...';
        FDeleteFile := '§R°£ÀÉ®× %s';
        FMakingSFXArchive := '«Ø¥ß¦Û§Ú¸ÑÀ£ÁYÀÉ®×...';
        FCompressingSolidArchive := 'À£ÁYÀÉ®×';
        FCopyingArchive := '½Æ»sÀ£ÁYÀÉ...';
        FCouldNotCopyArchive := 'µLªk½Æ»sÀ£ÁYÀÉ';
        FUpdatingFile := '§ó·s %s (%.0n)';
        FReplacingFile := '¸m´« %s (%.0n)';
        FCantDeleteInExtStr := 'Can''t delete files in an external stream';
      end;
    lgChineseGB: // Thanks to Huanlin Tsai (Taiwan)
      begin
        FCouldNotCompressBlock := 'ÎÞ·¨Ñ¹ËõÇø¿é¡£';
        FDeleteFileEventNeeded := 'Äú±ØÐë¶¨Òå OnDeleteFile ÊÂ¼þÒÔÉ¸Ñ¡³öÒªÉ¾³ýµÄµµ°¸¡£';
        FCouldNotRenameArchive := 'ÎÞ·¨½«µµ°¸Ãû³ÆÓÉ "%s" ¸ü¸ÄÎª "%s"¡£';
        FInsertNewDisk := 'Çë²åÈë¿Õ°×´ÅÆ¬(µÚ %d Æ¬)ÖÁ %s ´Åµú»úÖÐ¡£';
        FSelectNewPlace := 'ÇëÖ¸¶¨Ò»¸öÎ»ÖÃÒÔ´¢´æÑ¹ËõµµµÄµÚ %d Æ¬·Ö¸îµµ¡£'#10#13'(·Ö¸î×ÔÑ¹Ëõµµ %s)...';
        FNotEnoughFreeSpaceOn := '´Åµú¿Õ¼ä²»×ã: ´Åµú»ú %s';
        FUnableToDeleteFiles := 'ÎÞ·¨´ÓÒ»¸ö·Ö¸îµÄÑ¹Ëõµµ°¸ÖÐÉ¾³ýÆäÖÐµÄµµ°¸¡£';
        FConfirmFileDeletion := '´Åµú»ú %s ÀïÃæÓÐµµ°¸´æÔÚ, ÊÇ·ñÒªÉ¾³ýËüÃÇ?';
        FMaxSegmentSizeTooSmall := 'ÊôÐÔ MaxSegmentSize ÊýÖµÌ«Ð¡¡£';
        FCantPerformThisOp := 'ÎÞ·¨¶ÔÒ»¸ö·Ö¸îµÄÑ¹ËõµµÖ´ÐÐ´Ë¶¯×÷';
        FAddingFile := '¼ÓÈëµµ°¸ %s (%.0n)';
        FDeletingFiles := 'ÕýÔÚÉ¾³ýµµ°¸...';
        FDeleteFile := 'É¾³ýµµ°¸ %s';
        FMakingSFXArchive := '½¨Á¢×ÔÎÒ½âÑ¹Ëõµµ°¸...';
        FCompressingSolidArchive := 'Ñ¹Ëõµµ°¸';
        FCopyingArchive := '¸´ÖÆÑ¹Ëõµµ...';
        FCouldNotCopyArchive := 'ÎÞ·¨¸´ÖÆÑ¹Ëõµµ';
        FUpdatingFile := '¸üÐÂ %s (%.0n)';
        FReplacingFile := 'ÖÃ»» %s (%.0n)';
        FCantDeleteInExtStr := 'Can''t delete files in an external stream';
      end;    lgPortuguese: // Thanks to Hugo Souza
      begin
        FCouldNotCompressBlock := 'Impossível compactar bloco';
        FDeleteFileEventNeeded := 'Favor definir o evento OnDeleteFile para filtrar os arquivos que serão excluídos.';
        FCouldNotRenameArchive := 'Impossível renomear arquivo "%s" para "%s"';
        FInsertNewDisk := 'Insira um novo disco para o segmento #%d no drive %s';
        FSelectNewPlace := 'Selecione um novo lugar para guardar o segmento #%d do Arquivo %s';
        FNotEnoughFreeSpaceOn := 'Espaço insuficiente no drive %s';
        FUnableToDeleteFiles := 'Impossível excluir arquivos de um Arquivo segmentado';
        FConfirmFileDeletion := 'Drive %s contém arquivos. Deseja excluí-los ?';
        FMaxSegmentSizeTooSmall := 'Tamanho máximo do Segmento muito pequeno (property MaxSegmentSize)';
        FCantPerformThisOp := 'Impossivel executar esta operação em um arquivo segmentado';
        FAddingFile := 'Adicionando %s (%.0n)';
        FDeletingFiles := 'Excluindo arquivos...';
        FDeleteFile := 'Excluir arquivo %s';
        FMakingSFXArchive := 'Criando arquivo SFX...';
        FCompressingSolidArchive := 'Comprimindo arquivo sólido';
        FCopyingArchive := 'Copiando arquivo...';
        FCouldNotCopyArchive := 'Impossível copiar arquivo';
        FUpdatingFile := 'Atualizando %s (%.0n)';
        FReplacingFile := 'Substituindo %s (%.0n)';
        FCantDeleteInExtStr := 'Can''t delete files in an external stream';
      end;
    lgGerman: // Thanks to Oliver Buschjost (buschjost@geocities.com)
      begin
        FCouldNotCompressBlock := 'Der Block konnte nicht gepackt werden';
        FDeleteFileEventNeeded := 'Das Ereignis OnDeleteFile muß festgelegt werden, um die Dateien zu Filtern, die gelöscht werden sollen.';
        FCouldNotRenameArchive := 'Das Archiv konnte nicht von "%s" nach "%s" umbenannt werden';
        FInsertNewDisk := 'Bitte legen Sie einen neuen Datenträger für das Segment #%d in das Laufwerk %s';
        FSelectNewPlace := 'Bitte wählen Sie einen neuen Ort, um das Segment #%d des Archives %s zu speichern';
        FNotEnoughFreeSpaceOn := 'Es ist nicht genug freier Platz auf dem Laufwerk %s verfügbar';
        FUnableToDeleteFiles := 'Es können keine Dateien von einem segmentierten Archiv gelöscht werden';
        FConfirmFileDeletion := 'Auf dem Datenträger %s befinden sich Dateien. Sollen diese gelöscht werden ?';
        FMaxSegmentSizeTooSmall := 'Die maximale Größe für ein Segment ist zu klein (Eigenchaft MaxSegmentSize)';
        FCantPerformThisOp := 'Diese Funktion kann nicht in Verbindung mit segmentierten Archiven genutzt werden';
        FAddingFile := 'Adding %s (%.0n)';
        FDeletingFiles := 'Deleting files...';
        FDeleteFile := 'Löschen der Datei ''%s''';
        FMakingSFXArchive := 'Erstellen des SFX-Archives...';
        FCompressingSolidArchive := 'Kompression des solid archive';
        FCopyingArchive := 'Kopieren des Archives...';
        FCouldNotCopyArchive := 'Fehler beim kopieren des Archives';
        FUpdatingFile := 'Update von %s (%.0n)';
        FReplacingFile := 'Ersetzen von %s (%.0n)';
        FCantDeleteInExtStr := 'Can''t delete files in an external stream';
      end;
    lgItalian: // Thanks to Gabriele Bigliardi (gbigliardi@manord.com)
               // Modified by Mauro Favagrossa (mrfava@tin.it)
      begin
        FCouldNotCompressBlock := 'Impossibile comprimere il blocco';
        FDeleteFileEventNeeded := 'Devi definire l''evento OnDeleteFile per filtrare i files da cancellare.';
        FCouldNotRenameArchive := 'Impossibile rinominare l''archivio da "%s" a "%s"';
        FInsertNewDisk := 'Inserire il nuovo disco per il segmento #%d nell''unità %s';
        FSelectNewPlace := 'Selezionare un nuovo posto per registrare il segmento #%d dell''archivio %s';
        FNotEnoughFreeSpaceOn := 'Spazio insufficiente sull''unità %s';
        FUnableToDeleteFiles := 'Impossibile cancellare file in un archivio segmentato';
        FConfirmFileDeletion := 'L''unità %s contiene dei file. Vuoi cancellarli ?';
        FMaxSegmentSizeTooSmall := 'La massima ampiezza di un segmento è troppo piccola (proprietà MaxSegmentSize)';
        FCantPerformThisOp := 'Impossibile eseguire questa operazione su di un archivio segmentato';
        FAddingFile := 'Inserimento di "%s" (%.0n)';
        FDeletingFiles := 'Cancellazione Files...';
        FDeleteFile := 'Cancellazione del file %s';
        FMakingSFXArchive := 'Creazione di un archivio SFX...';
        FCompressingSolidArchive := 'Compressione di archivio solido';
        FCopyingArchive := 'Copia dell''archivio...';
        FCouldNotCopyArchive := 'Impossibile copiare l''archivio';
        FUpdatingFile := 'Aggiornamento di "%s" (%.0n)';
        FReplacingFile := 'Sostituzione di "%s" (%.0n)';
        FCantDeleteInExtStr := 'Impossibile cancellare file in uno "stream" esterno';
      end;
    lgRussian:
      begin
        FCouldNotCompressBlock := 'Íå óäàåòñÿ óïàêîâàòü áëîê äàííûõ';
        FDeleteFileEventNeeded := 'Âû äîëæíû îïèñàòü ñîáûòèå OnDeleteFile äëÿ ïðîâåðêè äîïóñòèìîñòè óäàëåíèÿ ôàéëîâ.';
        FCouldNotRenameArchive := 'Íå ìîãó ïåðåèìåíîâàòü àðõèâ "%s" â "%s"';
        FInsertNewDisk := 'Âñòàâüòå íîâûé äèñê äëÿ ÷àñòè #%d â äèñêîâîä %s';
        FSelectNewPlace := 'Âûáåðèòå íîâîå ìåñòî äëÿ ñîõðàíåíèÿ ÷àñòè #%d Àðõèâà %s';
        FNotEnoughFreeSpaceOn := 'Íà äèñêå %s íå õâàòàåò ñâîáîäíîãî ìåñòà';
        FUnableToDeleteFiles := 'Íåëüçÿ óäàëèòü ôàéëû èç ñåãìåíòèðîâàííîãî àðõèâà';
        FConfirmFileDeletion := 'Äèñê %s ñîäåðæèò ôàéëû. Õîòèòå èõ óäàëèòü ?';
        FMaxSegmentSizeTooSmall := 'Ìàêñèìàëüíîå çíà÷åíèå ñåãìåíòà ñëèøêîì ìàëî (ñâîéñòâî MaxSegmentSize)';
        FCantPerformThisOp := 'Íåâîçìîæíî âûïîëíèòü îïåðàöèþ íà ñåãìåíòèðîâàííîì àðõèâå';
        FAddingFile := 'Äîáàâëåíèå %s (%.0n)';
        FDeletingFiles := 'Óäàëåíèå ôàéëîâ...';
        FDeleteFile := 'Óäàëèòü ôàéë %s';
        FMakingSFXArchive := 'Ãåíåðàöèÿ ñàìîðàñïàêîâûâàþùåãîñÿ àðõèâà...';
        FCompressingSolidArchive := 'Ñæàòèå ñïëîøíîãî àðõèâà';
        FCopyingArchive := 'Êîïèðîâàíèå àðõèâà...';
        FCouldNotCopyArchive := 'Íåâîçìîæíî ñêîïèðîâàòü àðõèâ';
        FUpdatingFile := 'Îáíîâëåíèå %s (%.0n)';
        FReplacingFile := 'Çàìåíà %s (%.0n)';
        FCantDeleteInExtStr := 'Can''t delete files in an external stream';
     end;
    lgSpanish:
      begin
        FCouldNotCompressBlock := 'No se puede comprimir el bloque';
        FDeleteFileEventNeeded := 'Define el manejador de evento OnDeleteFile para filtrar los archivos que deben borrarse.';
        FCouldNotRenameArchive := 'No puedo renombrar el archivo "%s" a "%s"';
        FInsertNewDisk := 'Inserta un disco nuevo para el segmento #%d en el drive %s';
        FSelectNewPlace := 'Selecciona otro lugar para el segmento #%d del archivo %s';
        FNotEnoughFreeSpaceOn := 'No hay suficiente espacio en la unidad %s';
        FUnableToDeleteFiles := 'Imposible borrar archivos de un archivo segmentado';
        FConfirmFileDeletion := 'La unidad %s contiene archivos.¿ Desea borrarlos ?';
        FMaxSegmentSizeTooSmall := 'El tamaño máximo del segmento es muy pequeño (propiedad MaxSegmentSize)';
        FCantPerformThisOp := 'No se puede efectuar la operación sobre un archivo segmentado';
        FAddingFile := 'Agregando %s (%.0n)';
        FDeletingFiles := 'Borrando archivos...';
        FDeleteFile := 'Borre el archivo %s';
        FMakingSFXArchive := 'Creando un archivo SFX...';
        FCompressingSolidArchive := 'Comprimiendo un archivo solido';
        FCopyingArchive := 'Copiando archivo...';
        FCouldNotCopyArchive := 'No es posible copiar el archivo';
        FUpdatingFile  := 'Actualizando %s (%.0n)';
        FReplacingFile := 'Reemplazando %s (%.0n)';
        FCantDeleteInExtStr := 'Can''t delete files in an external stream';
     end;
    lgDanish:
      begin
        FCouldNotCompressBlock := 'Kunne ikke komprimere blokken';
        FDeleteFileEventNeeded := 'Du skal definere OnDeleteFile for at filtrere filene der skal slettes.';
        FCouldNotRenameArchive := 'Kunne ikke omdøbe arkivet fra "%s" til "%s"';
        FInsertNewDisk := 'Indset ny diskette for segment #%d i drev %s';
        FSelectNewPlace := 'Vælg en ny plads til at gemme segment #%d af arkiv %s';
        FNotEnoughFreeSpaceOn := 'Der er ikke nok plads på drev %s';
        FUnableToDeleteFiles := 'Kan ikke slætte filer fra et segmenteret arkiv';
        FConfirmFileDeletion := 'Drev %s indeholder filer. Ønsker du at slette dem ?';
        FMaxSegmentSizeTooSmall := 'Maximal størrelse af segment er for lille (Egenskab         MaxSegmentSize)';
        FCantPerformThisOp := 'Kan ikke udføre denne operation på et segmenteret arkiv';
        FAddingFile := 'Tilføjer %s (%.0n)';
        FDeletingFiles := 'Sletter filer...';
        FDeleteFile := 'Sletter fil %s';
        FMakingSFXArchive := 'Maker et SFX arkiv...';
        FCompressingSolidArchive := 'Komprimere fast arkiv';
        FCopyingArchive := 'Kopier arkiv...';
        FCouldNotCopyArchive := 'Kunne ikke kopiere arkiv';
        FUpdatingFile := 'Opdater %s (%.0n)';
        FReplacingFile := 'Erstat %s (%.0n)';
        FCantDeleteInExtStr := 'Can''t delete files in an external stream';
     end;
    lgDutch:
      begin
        FCouldNotCompressBlock := 'Kon blok niet comprimeren';
        FDeleteFileEventNeeded := 'U moet gebeurtenis OnDeleteFile definiëren om de bestanden die moeten worden verwijderd te filteren.';
        FCouldNotRenameArchive := 'Kon archief "%s" niet hernoemen naar "%s"';
        FInsertNewDisk := 'Steek een nieuwe schijf voor segment #%d in station %s';
        FSelectNewPlace := 'Selecteer een nieuwe plaats voor het opslaan van segment #%d van archief %s';
        FNotEnoughFreeSpaceOn := 'Er is niet genoeg vrije plaats op de schijf in station %s';
        FUnableToDeleteFiles := 'Kan geen bestanden wissen uit een gesegmenteerd archief';
        FConfirmFileDeletion := 'Schijf %s bevat bestanden.  Wilt u deze verwijderen?';
        FMaxSegmentSizeTooSmall := 'De maximumgrootte van een segment is te klein (eigenschap MaxSegmentSize)';
        FCantPerformThisOp := 'Deze actie kan niet worden uitgevoerd op een gesegmenteerd archief';
        FAddingFile := 'Toevoegen: %s (%.0n)';
        FDeletingFiles := 'Verwijder bestanden...';
        FDeleteFile := 'Verwijder bestand %s';
        FMakingSFXArchive := 'Creatie van een SFX-archief...';
        FCompressingSolidArchive := 'Comprimeren bestendig archief';
        FCopyingArchive := 'Kopiëren van archief...';
        FCouldNotCopyArchive := 'Kon archief niet kopiëren';
        FUpdatingFile := 'Bijwerken %s (%.0n)';
        FReplacingFile := 'Vervangen %s (%.0n)';
        FCantDeleteInExtStr := 'Can''t delete files in an external stream';
     end;
    lgCzech: // Thanks to Hana Krizova
      begin
        FCouldNotCompressBlock := 'Blok nelze komprimovat';
        FDeleteFileEventNeeded := 'Musíte definovat obsluhu události OnDelete.';
        FCouldNotRenameArchive := 'Archive "%s" nelze pøejmenovat na "%s"';
        FInsertNewDisk := 'Segment #%d bude uložen na další disk. Vložte ho do mechaniky %s';
        FSelectNewPlace := 'Zvolte nové umístìní pro segment #%d archivu %s';
        FNotEnoughFreeSpaceOn := 'Na disku %s není dost místa';
        FUnableToDeleteFiles := 'Z archivu s více segmenty nelze mazat soubory';
        FConfirmFileDeletion := 'Disk %s obsahuje soubory. Chcete je smazat?';
        FMaxSegmentSizeTooSmall := 'Maximální velikost segmentu je pøíliš malá (property MaxSegmentSize)';
        FCantPerformThisOp := 'Požadovanou operaci nelze provést na archivu s více segmenty';
        FAddingFile := 'Pøidává se %s (%.0n)';
        FDeletingFiles := 'Mažou se soubory ...';
        FDeleteFile := 'Smazat soubor %s';
        FMakingSFXArchive := 'Vytváøí se SFX archiv ...';
        FCompressingSolidArchive := 'Komprimuje se SOLID archiv';
        FCopyingArchive := 'Kopíruje se archiv ...';
        FCouldNotCopyArchive := 'Nelze zkopírovat archiv';
        FUpdatingFile := 'Aktualizuje se %s (%.0n)'; { Updating }
        FReplacingFile := 'Nahrazuje se %s (%.0n)';
        FCantDeleteInExtStr := 'Can''t delete files in an external stream';
     end;
  end;
end;

////////////////////////////////////////////////////////////
// Protected section

procedure TCustomArchiver.AssignTo(Dest: TPersistent);
begin
  if Dest is TCustomArchiver then
    with TCustomArchiver(Dest) do
      begin
        ReserveSpace := Self.ReserveSpace;
        CompressionLevel := Self.CompressionLevel;
        MaxSegmentSize := Self.MaxSegmentSize;
        MinFreeSpace := Self.MinFreeSpace;
        PathStorage := Self.PathStorage;
      end;
  inherited AssignTo(Dest);
end;

function  TCustomArchiver.CreateMessages : TMessages;
begin
  Result := TArchMessages.Create;
end;

function  TCustomArchiver.GetMessages : TArchMessages;
begin
  Result := FMessages as TArchMessages;
end;

procedure TCustomArchiver.SetMessages( val : TArchMessages );
begin
  FMessages.Assign( val );
end;

procedure TCustomArchiver.EraseDrive( const drive : String );
var
  mr : Integer;
begin
  if not IsRemovableDisk( drive ) then
    Exit;
  if GetDirectorySize( drive, '*.*' ) = 0 then
    Exit;
  if oConfirmFileDeletion in Options then
    begin
      mr := MessageDlg( Format(Messages.ConfirmFileDeletion, [drive]), mtConfirmation, [mbYes, mbNo, mbCancel], 0 );
      if mr = mrCancel then
        Abort;
    end
  else
    mr := mrYes;
  if mr = mrYes then
    begin
      if Assigned( FOnClearDisk ) then
        FOnClearDisk( Self, drive )
      else
        DeleteDriveContent( drive );
    end;
end;

procedure TCustomArchiver.CompressStream( src : TStream );
var
  toRead, bytesProcessed, written : Integer;
  compressedBlockSize, lastOffset : Integer;
  crc : Integer;
  IsCompressed : Boolean;
begin
  Include( FInternalOperation, ioCompressingStream );
  try
    if (afCrypted in FHeader.ArchiveFlag) or
       (oEncryptFiles in Options) then
      Include( FCurrentFileEntry.FileFlag, ffCrypted );

    bytesProcessed := 0;
    FCurrentFileEntry.ArchiveInfo.CompressedSize := 0;
    while bytesProcessed < FCurrentFileEntry.ArchiveInfo.Size do
      begin
        // Read block of data from the source stream
        toRead := Min( FHeader.BlockSize, FCurrentFileEntry.ArchiveInfo.Size - bytesProcessed );
        lastOffset := src.Position;
        src.ReadBuffer( FSrcBlock^, toRead );
        // Compress the block of data
        compressedBlockSize := NeededBlockSize;
        if not CompressBlock( FDestBlock, compressedBlockSize, FSrcBlock, toRead ) then
          raise EArchiverCompress.Create( Messages.CouldNotCompressBlock );
        // Compare compression to uncompressed size
        if not (afCompressed in FHeader.ArchiveFlag) or (compressedBlockSize >= toRead) then
          begin
            // Compression was not interressant, so use uncompressed block
            IsCompressed := False;
            compressedBlockSize := toRead;
            Move( FSrcBlock^, FDestBlock^, compressedBlockSize);
          end
        else
          IsCompressed := True;
        // Calc CRC
        crc := not CRC32R( Longint($FFFFFFFF), FDestBlock^, compressedBlockSize );
        // Crypt block of data
        if ffCrypted in FCurrentFileEntry.FileFlag then
          begin
            CheckKey;
            CryptBlock( FDestBlock, FDestBlock, compressedBlockSize, compressedBlockSize );
          end;
        // Request space for storing the block of data inside our archive
        if RequestSpace( compressedBlockSize + sizeof(Integer)*2 ) then
          FCurrentFileEntry.FileOffset := lastOffset;
        // Write the block of data inside our archive
        WriteInteger( FStream, EncodeBlockSize( IsCompressed, compressedBlockSize ) );
        WriteInteger( FStream, crc );
        FStream.WriteBuffer( FDestBlock^, compressedBlockSize );
        // Update all counters
        written := compressedBlockSize + sizeof(Integer)*2;
        Inc( FCurrentFileEntry.SegmentInfo.Size, toRead );
        Inc( FCurrentFileEntry.SegmentInfo.CompressedSize, written );
        Inc( FCurrentFileEntry.ArchiveInfo.CompressedSize, written );
        FHeader.SegmentInfo.Size := FHeader.SegmentInfo.Size + toRead;
        FHeader.SegmentInfo.CompressedSize := FHeader.SegmentInfo.CompressedSize + written;
        FBytesProcessed := FBytesProcessed + toRead;
        Inc( bytesProcessed, toRead );
        UpdateProgress;
      end;
  finally
    Exclude( FInternalOperation, ioCompressingStream );
  end;
end;

function TCustomArchiver.CompressBlock( DestBlock : PChar; var DestSize : Integer; SrcBlock : PChar; SrcSize : Integer ) : Boolean;
begin
  if Assigned( FOnCompressBlock ) then
    Result := FOnCompressBlock( Self, DestBlock, DestSize, SrcBlock, SrcSize, CompressionLevel )
  else
    begin
      // Do nothing special : copy the src to the dest !
      DestSize := SrcSize;
      move( SrcBlock^, DestBlock^, SrcSize );
      Result := True;
    end;
end;

procedure TCustomArchiver.CryptBlock( DestBlock, SrcBlock : PChar; var DestSize : Integer; SrcSize : Integer);
begin
  if Assigned(FOnCryptBlock) then
    FOnCryptBlock( Self, DestBlock, SrcBlock, DestSize, SrcSize );
end;

procedure TCustomArchiver.SetMaxSegmentSize(Val: TFileSize);
begin
  if (Val > 0) and (Val < FBlockSize*3) then
      raise EArchiver.Create(Messages.MaxSegmentSizeTooSmall);
  FMaxSegmentSize := Val;
end;

function TCustomArchiver.RequestSpace( val : TFileSize ) : Boolean;
var
  free : TFileSize;
begin
  Result := False;
  // avoid recursion
  if ioSwappingSegment in FInternalOperation then
    Exit;

  // Check the option oNoSpanning and exit if it is set.
  if oNoSpanning in Options then
    Exit;

  // If we came back to rewrite a record, then
  // we don't need to check free space, as the record was
  // already written once.
  if FStream.Position + val <= FStream.Size then
    Exit;

  // Get free space and check it
  free := GetFreeSpace( ExtractFileDrive( FileName ) );
  if (free >= 0) and (val > free) then
    begin
      NextSegment; // Create and open a new segment
      Result := True;
    end;
end;

function TCustomArchiver.IsValidDrive( const drive : String ) : Boolean;
var
  free : TFileSize;
begin
  free := GetFreeSpace( drive );
  Result := (free = -1) or (free >= MinFreeSpace) or
            ( (free >= MaxSegmentSize) and (MaxSegmentSize > 0) );
end;

function TCustomArchiver.CanUseDrive( const drive : String ) : Boolean;
begin
  try
    if IsRemovableDisk( drive ) and
       (oEraseNewDisk in Options) and
       ( (FHeader.Segment <> 1) or (oEraseFirstDisk in Options) ) then
      EraseDrive( drive );
    Result := IsValidDrive( drive );
    if not Result then
      if MessageDlg( Format(Messages.NotEnoughFreeSpaceOn, [drive]), mtWarning, [mbOk, mbCancel], 0 ) <> mrOK then
        Abort;
  except
    on EAbort do
      Raise
    else
      Result := False;
  end;
end;

// This method was rewritten by Mauro Favagrossa
procedure TCustomArchiver.AskNewDisk;
var
  msg : String;
  tmp : String;
  DiskQuery : Boolean;
  FirstIteration: Boolean;
  sDummy: string;
  serialNum, serialNumNewDisk: DWord;
  dFileDate : TDateTime;
  iFileSize : Integer;

  function DiskOk : Boolean;
  begin
    Result := GetVolumeInfo(FArchiveDrive, sDummy, serialNumNewDisk); {disk present and formatted}
    if Result then
      if (serialNumNewDisk <> 0) then
        Result := (FHeader.Segment = 1) or (serialNum <> serialNumNewDisk)  {if not first seg: disk <> disk prec}
      else if FileExists(FileName) then
        Result := (dFileDate <> GetFileDate(FileName)) or
                  (iFileSize <> GetFileSize(FileName));
    Result := Result and CanUseDrive( FArchiveDrive ); {free space on disk}
  end;

begin
  FirstIteration:=true;
  msg := Format( Messages.InsertNewDisk, [FHeader.Segment, FArchiveDrive]);
  GetVolumeInfo(FArchiveDrive, sDummy, serialNum);
  dFileDate := GetFileDate(FileName);
  iFileSize := GetFileSize(FileName);
  DiskQuery:= (FHeader.Segment <> 1) or (oEraseFirstDisk in Options);
              // case oEraseFirstDisk: display Dialog is safe else risk of erasing disk actually present in drive.
  repeat
    if DiskQuery then
      begin
        if not FirstIteration then
          Beep; {error!}
        tmp := FArchiveDrive;
        if Assigned(FOnNeedNewDisk) then
          FOnNeedNewDisk( Self, FHeader.Segment, FArchiveDrive )
        else
          if not InputQuery( Messages.SystemMessage, msg, FArchiveDrive ) then
            Abort;
        if (Length(FArchiveDrive)<2) or (FArchiveDrive[2]<>':') then
          FArchiveDrive := tmp;
      end
    else
      DiskQuery := True; // toggle for next loop
    FirstIteration := False;
    if FArchiveDrive = '' then
      Continue; {next loop}
  until DiskOk;
end;

procedure TCustomArchiver.NextSegment;

  procedure AskNewPlace;
  var
    path : String;
  begin
    while True do
      begin
        path := FArchiveDrive + FArchiveDir;
        if Assigned(FOnNeedNewFolder) then
          FOnNeedNewFolder( Self, FHeader.Segment, path )
        else
          begin
            MessageDlg( Format(Messages.SelectNewPlace, [FHeader.Segment, FArchiveName]), mtInformation, [mbOk], 0 );
            if not SelectDirectory( path, [sdAllowCreate, sdPerformCreate, sdPrompt], 0 ) then
              Abort;
          end;
        path := AppendSlash( path );
        FArchiveDrive := ExtractFileDrive(path);
        FArchiveDir   := AppendSlash( ExtractFileDir( path ) );
        if Length(FArchiveDrive) > 0 then
          System.Delete( FArchiveDir, 1, Length(FarchiveDrive) );
        if IsValidDrive( FArchiveDrive ) then
          Break
        else
          MessageDlg( Format(Messages.NotEnoughFreeSpaceOn, [FArchiveDrive]), mtWarning, [mbOk], 0 );
      end;
  end;

begin
  Include( FInternalOperation, ioSwappingSegment );
  try
    // Update file entry before closing
    if  ioCompressingStream in FInternalOperation then
      begin
        // Update EndOffset only if we started compressing the file
        if FCurrentFileEntry.SegmentInfo.CompressedSize > 0 then
          FHeader.EndOffset := FStream.Position - GetStartOffset;
        FStream.Seek( (FCurrentFileEntry.Offset+GetStartOffset) - FStream.Position, soFromCurrent );
        WriteFileEntry( FCurrentFileEntry );
      end;
    // Update header of current segment before closing
    Exclude( FHeader.ArchiveFlag, afFinalSegment );
    WriteHeader;
    // Close current segment
    CloseSegment;
    // Increment the segment count
    Inc( FHeader.Segment );
    // Reset infos about the new segment
    FillChar( FHeader.SegmentInfo, sizeof(FHeader.SegmentInfo), 0 );
    StopTimer;
    // if the archive is on a removable disk, then ask the user to change it
    if IsRemovableDisk( FArchiveDrive ) then
      AskNewDisk
    else
      begin
        // if there's not enough space on the fixed disk
        if not IsValidDrive( FArchiveDrive ) then
          // ask the user to select a new place to continue
          AskNewPlace;
      end;
  finally
    Exclude( FInternalOperation, ioSwappingSegment );
  end;
  StartTimer;
  // Create the new segment
  CreateSegment;
  if Assigned( FOnSegmentChanged ) then
    FOnSegmentChanged( Self );
end;

procedure TCustomArchiver.CreateSegment;
begin
  CloseSegment;
  // Generate a name for the new segment
  FFileName := GetSegmentName( FHeader.Segment );
  if FileExists( FFileName ) then
    DeleteFile( FFileName );
  // Create a new file for the segment
  FHeader.SegmentInfo.FileCount := 0;
  Include( FHeader.ArchiveFlag, afFinalSegment );
  FHeader.EndOffset := GetHeaderSize;
  CreateStream;
  // Write the current file entry again
  if ioCompressingStream in FInternalOperation then
    begin
      FillChar( FCurrentFileEntry.SegmentInfo, sizeof(FCurrentFileEntry.SegmentInfo), 0 );
      WriteFileEntry( FCurrentFileEntry );
    end;
end;

procedure TCustomArchiver.CreateArchive;
begin
  Exclude( FOptions, oOpenSingleSegment );
  FillChar( FHeader, sizeof(FHeader), 0 );
  if IsRemovableDisk( FArchiveDrive ) then
    begin
      FHeader.Segment := 1;
      AskNewDisk;
    end;

  with FHeader do
    begin
      Segment := 1;
      BlockSize := FBlockSize;
      Version := kVersion;
      RandomID := GetTickCount;
      Include( ArchiveFlag, afFinalSegment );
      EndOffset := GetHeaderSize;
      if oCompress in Options then
        Include( ArchiveFlag, afCompressed );
      if oCreateSolidArchives in Options then
        Include( ArchiveFlag, afSolid );
      if oCrypt in Options then
        begin
          EnterCryptKey;
          Include( FHeader.ArchiveFlag, afCrypted );
        end;
    end;
  CreateStream;
  if (oWriteSFXCode in Options) or IsExeFile(FileName) then
    begin
      WriteSFXCode( Stream );
      Stream.Position := GetStartOffset;
      WriteHeader;
    end;
end;

function  TCustomArchiver.GetFreeSpace( const drive : String ) : TFileSize;

  function GetStreamSize : Integer;
  begin
    if IsStreamOpen then
      Result := FStream.Size
    else
      Result := 0;
  end;

  function GetReserveSpace : Integer;
  begin
    if FHeader.Segment = 1 then
      begin
        Result := ReserveSpace;
        if ReserveSpace > MaxSegmentSize*0.75 then
          Result := Round(MaxSegmentSize*0.75);
      end
    else
      Result := 0;
  end;

const
  kMinDiskSpace = 64 * 1024;
var
  size, avail : Double;
  dr : String;
begin
  Result := -1;
  dr := UpperCase(drive);
  if FCheckAvailableSpace and
     not ExternalStream and
     (Length(dr) >= 2) then
    begin
      if (Length(dr)>=2) then
        begin
          GetDiskSizeAvail( PChar(AppendSlash(dr)), size, avail );
          if avail > MaxInt then
            Result := MaxInt
          else
            Result := Round( avail ) - kMinDiskSpace - GetReserveSpace;
        end;
      if Result < -1 then
        Result := 0;
    end;

  if (MaxSegmentSize > 0) then
    begin
      if (Result >= 0) and (MaxSegmentSize - GetStreamSize <= Result) then
        Result := MaxSegmentSize - GetStreamSize - GetReserveSpace
      else if (Result < 0) then
        Result := MaxSegmentSize - GetStreamSize - GetReserveSpace;
    end;
  if Result < -1 then
    Result := -1;
end;

function  TCustomArchiver.CompressionLevelAsInteger : Integer;
begin
  Result := Integer(CompressionLevel);
end;

procedure TCustomArchiver.WriteSFXCode( S : TStream );
begin
  S.Position := 0;
  if Assigned(FSFXGenerator) then
    FSFXGenerator.WriteSFXCodeToStream( S )
  else if Assigned(FOnWriteSFXCode) then
    FOnWriteSFXCode( Self, S )
end;

procedure TCustomArchiver.AfterUpdate;
begin
  if Assigned( FSFXGenerator ) and IsExeFile( FileName ) then
    FSFXGenerator.UpdateTagInfos( Stream );
  inherited;
end;

function TCustomArchiver.CompressSolidData : String;
var
  tempName : String;
  tmpDate : TDateTime;
  tmpComment : String;
begin
  Result := '';
  if FIsSolidArchive then
    begin
      tmpComment := FHeader.Comment;
      CloseStream;
      MaxSegmentSize := FOldMaxSegmentSize;
      FOptions := FOldOptions;
      FMaxSegmentSize := FOldMaxSegmentSize;
      if FFileName <> FOldFileName then
        begin
          tempName := FFileName;
          Result := FFileName;
          FFileName := FOldFileName;
          ExplodeFileName;
          tmpDate := GetFileDate( tempName );
          try
            Include( FInternalOperation, ioCloseSolid );
            // If archive changed, then Update the solid archive
            if (FTmpFileDate <> 0) and (FTmpFileDate <> tmpDate) then
              begin
                DeleteFile( GetSegmentName(1) );
                CreateArchive;
                Include( FHeader.ArchiveFlag, afSolid );
                SetArchiveComment( tmpComment );
                Start;
                try
                  DisplayMessage( Messages.CompressingSolidArchive );
                  AddToLog( Messages.CompressingSolidArchive );
                  DoAddFile( tempName );
                  UpdateArchiveSize;
                finally
                  Finish;
                end;
              end;
          finally
            Exclude( FInternalOperation, ioCloseSolid );
          end;
        end;
    end;
end;

procedure TCustomArchiver.CloseSolidData;
var
  tempName : String;
begin
  tempName := '';
  try
    tempName := CompressSolidData;
  finally
    if tempName <> '' then
      DeleteFile( tempName );
  end;

end;

function  TCustomArchiver.GetOpenMode : Integer;
begin
  Result := fmOpenReadWrite;
end;

procedure TCustomArchiver.UpdateArchiveSize;
var
  header : TArchiveHeader;
begin
  if not IsSegmented or
     not ArchiveChanged or
     (oDoNotUpdateFirstSegment in Options) then
    Exit;
  header := FHeader;
  try
    NeedFirstSegment;
    FHeader.ArchiveInfo := header.ArchiveInfo;
    WriteHeader;
  except
  end;
end;

procedure TCustomArchiver.BeforeClose;
begin
  UpdateArchiveSize;
  inherited;
end;


function  TCustomArchiver.DoAddFile( const FileName : String ) : Boolean;

  function DefFileName( const FileName : String ) : String;
  var
    tmp : String;
  begin
    case PathStorage of
      psNone:
        Result := ExtractFileName( FileName );
      psWhole:
        Result := FileName;
      psRelative:
        begin
          if FRelativePath <> '' then
            begin
              if oIncludeStartingDirectory in Options then
                begin
                  tmp := RemoveSlash( FRelativePath );
                  if Pos( '\', tmp ) > 0 then
                    begin
                      while (Length(tmp)> 0) and (tmp[Length(tmp)] <> '\') do
                        System.Delete( tmp, Length(tmp), 1 );
                    end
                  else
                    begin
                      Result := tmp + '\' + ExtractFileName( FileName );
                      Exit;
                    end;
                end
              else
                tmp := FRelativePath;
              if CompareText( System.Copy(FileName, 1, Length(tmp)), tmp) = 0 then
                begin
                  Result := System.Copy( FileName, Length(tmp)+1, Length(FileName) );
                end
              else
                Result := FileName;
            end
          else
            Result := ExtractFileName( FileName );
        end;
    end;
  end;

  procedure AddStream( S : TStream; var fileEntry : TFileEntry );
  begin
    FStream.Seek( FHeader.EndOffset + GetStartOffset, soFromBeginning );
    // Keep position of the fileEntry, because we'll need
    // to update it later (for the compressedSize) !
    WriteFileEntry( fileEntry );
    // Copy the whole file inside the archive
    CompressStream( S );
    FHeader.EndOffset := FStream.Position - GetStartOffset;
    // The file is completely written.
    Include( fileEntry.FileFlag, ffFinalSegment );
    // Update the compressed size a
    FStream.Seek( (fileEntry.Offset+GetStartOffset) - FStream.Position, soFromCurrent );
    WriteFileEntry( fileEntry );
    Inc( FCurrentFileIdx );
    // Update archive header
    with FHeader do
      begin
        Inc( ArchiveInfo.FileCount );
        Inc( SegmentInfo.FileCount );
        ArchiveInfo.Size := ArchiveInfo.Size + fileEntry.ArchiveInfo.Size;
        ArchiveInfo.CompressedSize := ArchiveInfo.CompressedSize + fileEntry.ArchiveInfo.CompressedSize;
        FCompressedArchiveSize := FCompressedArchiveSize + fileEntry.ArchiveInfo.CompressedSize;
      end;
    WriteHeader;
    // File is added
    FArchiveChanged := True;
    if (oMaintainFileDirectory in FOptions) then
      AddFileToList( FCurrentFileEntry );
    if Assigned( FOnFileAdded ) then
      FOnFileAdded( Self, FCurrentFileEntry );
  end;

var
  S : TStream;
  mode : Word;
  accept : Boolean;
begin
  FOperation := opAdd;
  Start;
  try
    CheckOpen;
    CheckReadOnly;
    Result := FileExists( FileName );
    if (Result = False) and (ExtractFileName(FileName) <> '.')  then
      Exit;
    NeedLastSegment;
    // Prepare record
    FillChar( FCurrentFileEntry, sizeof(FCurrentFileEntry), 0 );
    with FCurrentFileEntry do
      begin
        Name := DefFileName( FileName );
        ArchiveInfo.Size := GetFileSize( FileName );
        Attr := FileGetAttr( FileName );
        Date := GetFileDate( FileName );
        Segment := FHeader.Segment;
        if ExtractFileName(FileName) = '.' then
          begin
            Include( FileFlag, ffEmptyFolder );
            Include( FileFlag, ffFinalSegment );
          end
        else
          Include( FileFlag, ffFile);
      end;
    // Check special case of an empty directory
    if ffEmptyFolder in FCurrentFileEntry.FileFlag then
      begin
        FStream.Seek( FHeader.EndOffset + GetStartOffset, soFromBeginning );
        WriteFileEntry( FCurrentFileEntry );
        FHeader.EndOffset := FStream.Position - GetStartOffset;
        Inc( FHeader.ArchiveInfo.FileCount );
        Inc( FHeader.SegmentInfo.FileCount );
        WriteHeader;
        Result := True;
        Exit;
      end;
    // Prepare progress infos if not already done
    if FBytesToProcess = 0 then
      FBytesToProcess := FCurrentFileEntry.ArchiveInfo.Size;
    // Confirm action
    accept := True;
    if not(ioCloseSolid in FInternalOperation) and Assigned( FOnAddFile ) then
      FOnAddFile( Self, FCurrentFileEntry, Accept );
    if not accept then
      begin
        // skip it and update progress information
        FBytesProcessed := FBytesProcessed + FCurrentFileEntry.ArchiveInfo.Size;
        UpdateProgress;
        Exit;
      end;
    if not(ioCloseSolid in FInternalOperation) then
      begin
        DisplayMessage( Format( Messages.FAddingFile, [ExtractFileName(FileName), FCurrentFileEntry.ArchiveInfo.Size*1.0] ) );
        AddToLog( Format( Messages.FAddingFile, [FileName, FCurrentFileEntry.ArchiveInfo.Size*1.0] ) );
      end;
    // Open file
    mode := fmOpenRead;
    if oSecureAccess in Options then
      mode := mode or fmShareDenyWrite
    else
      mode := mode or fmShareDenyNone;
    try
      S := NewStreamObject( FileName, mode );
      try
        // Write data to Archive
        AddStream( S, FCurrentFileEntry);
      finally
        S.Free;
      end;
    except
      on E: Exception do
        begin
          // Skip the Abort exception
          if E is EAbort then
            raise;
          if not CanContinue( E ) then
            if (ErrorAction <> eaAsk) and not Assigned(FOnError) then
              raise
            else
              Abort;
        end;
    end;
  finally
    Finish;
  end;
end;

function  TCustomArchiver.DoAddFiles( files : TStrings ) : Boolean;

  procedure CalcSize;
  var
    i : Integer;
  begin
    FBytesToProcess := 0;
    for i := 0 to files.Count - 1 do
      begin
        if DirectoryExists( RemoveSlash(files.Strings[i]) ) then
          FBytesToProcess := FBytesToProcess + GetDirectorySize( files.Strings[i], Filter )
        else
          FBytesToProcess := FBytesToProcess + GetFileSize( files.Strings[i] );
      end;
  end;

var
  i : Integer;
begin
  Result := False;
  FOperation := opAdd;
  Start;
  try
    CheckOpen;
    // Prepare progress infos if not already done
    if FBytesToProcess = 0 then
      CalcSize;
    for i := 0 to files.Count - 1 do
      begin
        if DirectoryExists( RemoveSlash(files.Strings[i]) ) then
          Result := DoAddDirectory( files.Strings[i] )
        else
          Result := DoAddFile( files.Strings[i] );
        if not Result then
          Break;
      end;
  finally
    Finish;
  end;
end;

function  TCustomArchiver.DoAddDirectory( const Directory : String ) : Boolean;

  function DoAdd( path : String ) : Boolean;
  var
    SR : TSearchRec;
    Found : Integer;
    source : String;
    empty : Boolean;
  begin
    source := FRelativePath + path;
    Result := True;
    empty := True;
    // First add files of the current folder
    Found := FindFirst( source+Filter, faAnyFile, SR );
    try
      while (Found = 0) and Result do
        begin
          if (SR.Name <> '.') and (SR.Name <> '..') then
            begin
              // if it's not a directory and
              // if it's not the archive itself
              if ((SR.Attr and faDirectory) = 0) and
                 (source+SR.Name <> FFileName) then
                begin
                  // Add file
                  Result := Result and DoAddFile( source+SR.Name );
                  empty := False;
                end;
            end;
          Found := FindNext( SR );
        end;
    finally
      FindClose(SR);
    end;

    if oRecurseFolders in Options then
      begin
        // If there was a filter, then we could not see the folders,
        // so we look at every folders with a '*.*' filter, but we
        // don't look at files, because we did it just before with
        // the right filter.
        Found := FindFirst( source+'*.*', faDirectory, SR );
        try
          while (Found = 0) and Result do
            begin
              if (SR.Name <> '.') and (SR.Name <> '..') then
                begin
                  if (SR.Attr and faDirectory) <> 0 then
                    begin
                      // Add subdirectory
                      Result := Result and DoAdd( path+SR.Name+'\' );
                      empty := False;
                    end;
                end;
              Found := FindNext( SR );
            end;
        finally
          FindClose(SR);
        end;
      end;
    if empty and (oStoreEmptyFolders in Options) then
      Result := DoAddFile( source+'.' );
  end;

begin
  Result := False;
  FOperation := opAdd;
  Start;
  try
    CheckOpen;
    FRelativePath := AppendSlash( Directory );
    if not DirectoryExists( RemoveSlash(Directory) ) then
      Exit;
    // Prepare progress infos if not already done
    if FBytesToProcess = 0 then
      FBytesToProcess := GetDirectorySize( Directory, Filter );
    Result := DoAdd( '' );
  finally
    FRelativePath := '';
    Finish;
  end;
end;

procedure TCustomArchiver.DoDeleteFiles;

  function EnoughSpace : Boolean;
  var
    TotalBytes : double;
    TotalFree : double;
  begin
    GetDiskSizeAvail( PChar(ArchiveDrive), TotalBytes, TotalFree );
    Result := TotalFree > Stream.Size;
  end;

  procedure TransferData( src, dest : TCustomArchiver );
  var
    toRead, crc, bytesProcessed : Integer;
    IsCompressed : Boolean;
    blockSize : Integer;
  begin
    dest.WriteFileEntry( src.FCurrentFileEntry );
    // Copy the whole file from src archive to dest archive
    bytesProcessed := 0;
    while bytesProcessed < src.CurrentFileEntry.ArchiveInfo.CompressedSize do
      begin
        toRead := ReadInteger( src.Stream );
        DecodeBlockSize( toRead, IsCompressed, blockSize );
        crc := ReadInteger( src.Stream );
        WriteInteger( dest.Stream, toRead );
        WriteInteger( dest.Stream, crc );
        dest.Stream.CopyFrom( src.Stream, blockSize );
        FBytesProcessed := FBytesProcessed + blockSize + sizeof(Integer)*2;
        Inc( bytesProcessed, blockSize + sizeof(Integer)*2 );
        UpdateProgress;
      end;
    // Update archive header
    with dest.FHeader do
      begin
        EndOffset := dest.Stream.Position - GetStartOffset;
        Inc( ArchiveInfo.FileCount );
        Inc( SegmentInfo.FileCount );
        ArchiveInfo.Size := ArchiveInfo.Size + src.CurrentFileEntry.ArchiveInfo.Size;
        SegmentInfo.Size := SegmentInfo.Size + src.CurrentFileEntry.SegmentInfo.Size;
        ArchiveInfo.CompressedSize := ArchiveInfo.CompressedSize + src.CurrentFileEntry.ArchiveInfo.CompressedSize;
        SegmentInfo.CompressedSize := SegmentInfo.CompressedSize + src.CurrentFileEntry.SegmentInfo.CompressedSize;
      end;
    dest.FCompressedArchiveSize := dest.FCompressedArchiveSize + src.CurrentFileEntry.ArchiveInfo.CompressedSize;
  end;

var
  A : TCustomArchiver;
  accept : Boolean;
  i : Integer;
  IsSFX : Boolean;
begin
  if not Assigned(FOnDeleteFile) and not Assigned(FOnDeleteFileByIndex) then
    raise EArchiver.Create( Messages.DeleteFileEventNeeded );
  if ExternalStream then
    raise EArchiver.Create( Messages.CantDeleteInExtStr );
  IsSFX := IsExeFile( FileName );
  FOperation := opDelete;
  Start;
  try
    CheckOpen;
    CheckReadOnly;
    if IsSegmented then
      raise EArchiver.Create( Messages.UnableToDeleteFiles );
    NeedFirstSegment;
    FBytesToProcess := FCompressedArchiveSize;
    // Get a new clone of our instance
    A := TCustomArchiver(NewInstance);
    A.Create( Self );
    try
      //A.FileName := FileName + '.tmp';
      A.Options := Options;
      Exclude( A.FOptions, oMaintainFileDirectory );
      A.StartOffset := StartOffset;
      if EnoughSpace then
        begin
          A.FileName :=  ExtractFilePath(FileName)+'~'+ExtractFileName(FileName);
          A.Open;
        end
      else
        A.CreateTempFile;
      try
        DisplayMessage( Messages.DeletingFiles );
        if IsSFX and Assigned(FSFXGenerator) then
          begin
            // Copy the SFX code inside the new archive
            Stream.Position := 0;
            A.Stream.Position := 0;
            FSFXGenerator.WriteSFXCodeToStream( A.Stream );
            A.Stream.CopyFrom( Stream, StartOffset );
            A.WriteHeader;
          end;
        // Skip the header
        ReadHeader;
        A.FHeader := FHeader;
        FillChar( A.FHeader.ArchiveInfo, sizeof(A.FHeader.ArchiveInfo), 0 );
        FillChar( A.FHeader.SegmentInfo, sizeof(A.FHeader.SegmentInfo), 0 );
        A.FHeader.EndOffset := GetHeaderSize;
        // For each file stored in the archive
        for i := 0 to FHeader.ArchiveInfo.FileCount - 1 do
          begin
            DisplayMessage( Messages.DeletingFiles );
            ReadFileEntry( FCurrentFileEntry );
            if ffEmptyFolder in FCurrentFileEntry.FileFlag then
              begin
                A.WriteFileEntry( FCurrentFileEntry );
                Continue;
              end;
            // Confirm deletion
            accept := False;
            if Assigned( FOnDeleteFile ) then
               FOnDeleteFile( Self, FCurrentFileEntry, accept );
            if Assigned( FOnDeleteFileByIndex ) then
               FOnDeleteFileByIndex( Self, i, accept );
            if Accept then
              begin
                DisplayMessage( Format( Messages.DeleteFile, [ExtractFileName(FCurrentFileEntry.Name)]) );
                AddToLog( Format( Messages.DeleteFile, [FCurrentFileEntry.Name]) );
                SkipFile( FCurrentFileEntry.Offset );
                Continue;
              end;
            // Transfer data from this archive to the temp archive
            DisplayMessage( Messages.DeletingFiles );
            TransferData( Self, A );
          end;
        // Write the header in the dest archive
        A.WriteHeader;
        // Update Tag info if needed
        if IsSFX and Assigned(FSFXGenerator) then
          FSFXGenerator.UpdateTagInfos( A.Stream );
        CloseStream;
        A.Close;
        if UpperCase(ExtractFilePath(FileName)) = UpperCase(ExtractFilePath(A.FileName)) then
          begin
            DeleteFile(FileName);
            RenameFile( A.FileName, FileName );
          end
        else
          begin
            DisplayMessage( Messages.CopyingArchive );
            AddToLog( Messages.CopyingArchive );
            if not CopyFile( A.FileName, FileName, False, True ) then
              raise EArchiver.Create( Messages.CouldNotCopyArchive );
            A.Delete;
          end;
        FArchiveChanged := True;
        OpenStream;
        FCompressedArchiveSize := FHeader.ArchiveInfo.CompressedSize;
      except
        A.Delete;
        raise;
      end;
    finally
      A.Free;
    end;
    if oMaintainFileDirectory in FOptions then
      begin
        FTotTicks := 0;
        StartTimer;
        DoEnumerateFiles;
      end;
  finally
    Finish;
  end;
end;

function  TCustomArchiver.DoMakeSFX : Boolean;

  function MakeIt : Boolean;
  var
    fname : String;
    S : TStream;
  begin
    try
      FOperation := opMakeSFX;
      Start;
      // Create Destination SFX
      fname := ChangeFileExt( FileName, '.exe' );
      if FileExists( fname ) then
        DeleteFile( fname );
      DisplayMessage( Messages.MakingSFXArchive );
      AddToLog( Messages.MakingSFXArchive );
      CheckSFX( fname );
      CheckOpen;
      S := NewStreamObject( fname, fmCreate );
      try
        // Copy the SFXCode
        WriteSFXCode( S );
        // append the archive
        CopyStream( Stream, S, False );
        // update the SFXArchiveSize
        if Assigned(FSFXGenerator) then
          FSFXGenerator.UpdateTagInfos( S );
        S.Free;
      except
        S.Free;
        DeleteFile( fname );
      end;
    finally
      StartOffset := 0;
      Finish;
    end;
    Result := True;
  end;

var
  tempName : String;
begin
  Result := False;
  if IsSegmented then
    raise EArchiver.Create( Messages.CantPerformThisOp );
  if IsExeFile( FileName ) then
    Exit;
  if IsSolidArchive then
    begin
      tempName := FFileName;
      try
        CompressSolidData;
        Result := MakeIt;
      finally
        CloseStream;
        FFileName := tempName;
        AdjustSolidOptions;
        OpenStream;
      end;
    end
  else
    Result := MakeIt;
end;

procedure TCustomArchiver.DoSetArchiveComment( const comment : String );
begin
  if not IsEmpty then
    Exit;
  FHeader.Comment := comment;
  FHeader.EndOffset := GetHeaderSize;
  WriteHeader;
end;

//////////////////////////////////////////////////
//  Public section

constructor TCustomArchiver.Create( AOwner : TComponent );
begin
  inherited;
  FPathStorage := psRelative;
  FMaxSegmentSize := 0;
  FMinFreeSpace := 1024 * 1024;
  FCompressionLevel := clNormal;
  FReserveSpace := 0;
  FAddMode := amAdd;
end;

function  TCustomArchiver.AddFile( const FileName : String ) : Boolean;
begin
  Result := False;
  if not CheckBusy then
    DoAddFile( FileName );
end;

function  TCustomArchiver.AddFiles( files : TStrings ) : Boolean;
begin
  Result := False;
  if not CheckBusy then
    DoAddFiles( files );
end;

function  TCustomArchiver.AddDirectory( const Directory : String ) : Boolean;
begin
  Result := False;
  if not CheckBusy then
    Result := DoAddDirectory( Directory );
end;

procedure TCustomArchiver.DeleteFiles;
begin
  if not CheckBusy then
    DoDeleteFiles;
end;

function  TCustomArchiver.MakeSFX : Boolean;
begin
  Result := False;
  if not CheckBusy then
    Result := DoMakeSFX;
end;

procedure TCustomArchiver.SetArchiveComment( const comment : String );
begin
  if not CheckBusy then
    DoSetArchiveComment( comment );
end;



end.


