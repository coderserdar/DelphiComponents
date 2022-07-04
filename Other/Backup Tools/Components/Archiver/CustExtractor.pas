unit CustExtractor;
{
  TArchiver by Morgan Martinet (C) 1998 - mmm@imaginet.fr or mmm@mcom.fr

  COPYRIGHT
  ---------

  This component is email-ware. You may use it, distribute it and modify it, but
  you may not charge for it. Please send me a mail if you use it, I'll be happy
  to see in which country it is used, and I'll be able to mail you the updates.

  In case of modifications you must mail me a copy of the modifications.
  The reason are simple: Any changes that improve this free-ware component should
  be to benefit for everybody, not only you. That way you can be pretty sure,
  that this component has few errors and much functionality.
  In case of modifications, you will be on the credits list beneath.

  DESCRIPTION
  -----------

  This component lets you add/extract files to/from an archive.

}

interface

uses
  Windows,
  SysUtils,
  ArchiverMisc,
  ArchiverRoot,
  CustSFXGenerator,
  Classes;

type
  TRestoreAction = (raOverwrite, raSkip, raUpdate, raAsk, raExistingOnly, raUpdateExisting);

  TOnEnumerationEvent        = procedure ( Sender : TObject; const FileEntry : TFileEntry ) of Object;
  TOnExtractFileEvent        = procedure ( Sender : TObject; const FileEntry : TFileEntry;
                                           var DestPath : String; var Accept : Boolean ) of Object;
  TOnExtractFileByIndexEvent = procedure ( Sender : TObject; Index : Integer;
                                           var DestPath : String; var Accept : Boolean ) of Object;
  TOnFileExtractedEvent      = procedure ( Sender : TObject; const FileEntry : TFileEntry; const DestPath : String ) of Object;
  TOnUncompressBlockEvent    = function ( Sender : TObject; DestBlock : PChar; var DestSize : Integer; SrcBlock : PChar; SrcSize : Integer) : Boolean of Object;
  TOnDecryptBlockEvent       = procedure ( Sender : TObject; DestBlock, SrcBlock : PChar; var DestSize : Integer; SrcSize : Integer) of Object;
  TOnInsertDiskEvent         = procedure ( Sender : TObject; Segment : Integer; var Drive : String ) of Object;
  TOnInsertLastDiskEvent     = procedure ( Sender : TObject; var Drive : String ) of Object;
  TOnLocateSegmentEvent      = procedure ( Sender : TObject; Segment : Integer; var FileName : String ) of Object;
  TOnLocateLastSegmentEvent  = procedure ( Sender : TObject; var Path : String ) of Object;

  EArchiverUncompress = class( EArchiver );
  EArchiverBadCRC     = class( EArchiver );
  EArchiverBadKey     = class( EArchiver );

  TExtrMessages = class(TMessages)
    protected
      FCouldNotUncompressBlock : String;
      FAskOverwrite : String;
      FNeedExtractPath : String;
      FInsertDisk : String;
      FLocateSegment : String;
      FWrongSegment : String;
      FInsertLastSegment : String;
      FLocateLastSegment : String;
      FWrongLastSegment : String;
      FWrongNextSegment : String;
      FBadCRC : String;
      FBadKey : String;
      FReplaceFile : String;
      FWithFile : String;
      FConfirmFileOverwrite : String;
      FExtractingFile : String;
      FLoadingArchiveContnent : String;
      FUncompressingSolidArchive : String;
      FCheckingFile : String;

      procedure AssignTo(Dest: TPersistent); override;
      procedure SetGlobalStrings; override;

    public
      procedure SetLanguage( language : TLanguage ); override;

    published
      property CouldNotUncompressBlock : String read FCouldNotUncompressBlock write FCouldNotUncompressBlock;
      property AskOverwrite : String read FAskOverwrite write FAskOverwrite;
      property NeedExtractPath : String read FNeedExtractPath write FNeedExtractPath;
      property InsertDisk : String read FInsertDisk write FInsertDisk;
      property LocateSegment : String read FLocateSegment write FLocateSegment;
      property WrongSegment : String read FWrongSegment write FWrongSegment;
      property InsertLastSegment : String read FInsertLastSegment write FInsertLastSegment;
      property LocateLastSegment : String read FLocateLastSegment write FLocateLastSegment;
      property WrongLastSegment : String read FWrongLastSegment write FWrongLastSegment;
      property WrongNextSegment : String read FWrongNextSegment write FWrongNextSegment;
      property BadCRC : String read FBadCRC write FBadCRC;
      property BadKey : String read FBadKey write FBadKey;
      property ReplaceFile : String read FReplaceFile write FReplaceFile;
      property WithFile : String read FWithFile write FWithFile;
      property ConfirmFileOverwrite : String read FConfirmFileOverwrite write FConfirmFileOverwrite;
      property ExtractingFile : String read FExtractingFile write FExtractingFile;
      property LoadingArchiveContnent : String read FLoadingArchiveContnent write FLoadingArchiveContnent;
      property UncompressingSolidArchive : String read FUncompressingSolidArchive write FUncompressingSolidArchive;
      property CheckingFile : String read FCheckingFile write FCheckingFile;
  end;

  TCustomExtractor = class( TArchiverRoot )
  protected
    FExtractPath : String;
    FRestoreAction : TRestoreAction;
    FSFXGenerator : TCustomSFXGenerator;
    FAlwaysOverwrite : Boolean;

    FOnEnumeration : TOnEnumerationEvent;
    FOnExtractFile : TOnExtractFileEvent;
    FOnExtractFileByIndex : TOnExtractFileByIndexEvent;
    FOnFileExtracted : TOnFileExtractedEvent;
    FOnUncompressBlock : TOnUncompressBlockEvent;
    FOnDecryptBlock : TOnDecryptBlockEvent;
    FOnInsertDisk : TOnInsertDiskEvent;
    FOnInsertLastDisk : TOnInsertLastDiskEvent;
    FOnLocateSegment : TOnLocateSegmentEvent;
    FOnLocateLastSegment : TOnLocateLastSegmentEvent;
    FOnSegmentChanged : TNotifyEvent;

    procedure AssignTo(Dest: TPersistent); override;
    function  CreateMessages : TMessages; override;
    function  GetMessages : TExtrMessages;
    procedure SetMessages( val : TExtrMessages );
    procedure UncompressStream( dest : TStream );
    function  UncompressBlock( DestBlock : PChar; var DestSize : Integer; SrcBlock : PChar; SrcSize : Integer) : Boolean; virtual;
    procedure DecryptBlock( DestBlock, SrcBlock : PChar; var DestSize : Integer; SrcSize : Integer); virtual;
    procedure SkipFile( anOffset : TFileSize );
    function  Eof( S : TStream ) : Boolean;
    function  SegmentBelongsToArchive( const aFileName : String; var AHeader : TArchiveHeader ) : Boolean;
    procedure OpenSegment( val : Integer );
    procedure CloseSegment;
    procedure NeedFirstSegment;
    procedure NeedLastSegment;
    function  CheckEOF : Boolean; override;
    procedure GetProgressInformations; override;
    procedure ExtractFileData( const fileEntry : TFileEntry; const DestFileName : String );
    function  GetDestinationPath( const fileEntry : TFileEntry ) : String;
    procedure CheckCurrentFile( const last, new : TFileEntry );
    procedure AfterOpen; override;
    procedure BeforeClose; override;
    procedure CheckSFX( const aFileName : String ); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure AdjustSolidOptions;
    procedure OpenSolidData; override;
    procedure CloseSolidData; override;
    function  GetOpenMode : Integer; override;
    procedure Start; override;
    procedure DoEnumerateFiles;
    procedure DoExtractFile( aSegment : Word; anOffset, compressedSize : Integer );
    procedure DoExtractFileTo( aSegment : Word; anOffset, compressedSize : Integer; const DestFileName : String );
    procedure DoExtractFiles;
    procedure DoCheckIntegrity;
    function  GetMaxSegmentSize : Integer;

  public
    // Creators & Destructor
    constructor Create( AOwner : TComponent ); override;

    // Public methods

    procedure EnumerateFiles;
    procedure ExtractFile( aSegment : Word; anOffset, compressedSize : Integer );
    procedure ExtractFileTo( aSegment : Word; anOffset, compressedSize : Integer; const DestFileName : String );
    procedure ExtractFiles;
    procedure CheckIntegrity;

  published
    // Properties
    property ExtractPath : String read FExtractPath write FExtractPath;
    property Messages : TExtrMessages read GetMessages write SetMessages;
    property RestoreAction : TRestoreAction read FRestoreAction write FRestoreAction;
    property SFXGenerator : TCustomSFXGenerator read FSFXGenerator write FSFXGenerator;

    // Events
    property OnEnumeration : TOnEnumerationEvent read FOnEnumeration write FOnEnumeration;
    property OnExtractFile : TOnExtractFileEvent read FOnExtractFile write FOnExtractFile;
    property OnExtractFileByIndex : TOnExtractFileByIndexEvent read FOnExtractFileByIndex write FOnExtractFileByIndex;
    property OnFileExtracted : TOnFileExtractedEvent read FOnFileExtracted write FOnFileExtracted;
    property OnUncompressBlock : TOnUncompressBlockEvent read FOnUncompressBlock write FOnUncompressBlock;
    property OnDecryptBlock : TOnDecryptBlockEvent read FOnDecryptBlock write FOnDecryptBlock;
    property OnInsertDisk : TOnInsertDiskEvent read FOnInsertDisk write FOnInsertDisk;
    property OnInsertLastDisk : TOnInsertLastDiskEvent read FOnInsertLastDisk write FOnInsertLastDisk;
    property OnLocateSegment : TOnLocateSegmentEvent read FOnLocateSegment write FOnLocateSegment;
    property OnLocateLastSegment : TOnLocateLastSegmentEvent read FOnLocateLastSegment write FOnLocateLastSegment;
    property OnSegmentChanged : TNotifyEvent read FOnSegmentChanged write FOnSegmentChanged;
  end;

implementation

////////////////////////////////////////////////////////////

procedure TExtrMessages.AssignTo(Dest: TPersistent);
begin
  if Dest is TExtrMessages then
    with TExtrMessages( Dest ) do begin
      FCouldNotUncompressBlock   := Self.FCouldNotUncompressBlock;
      FAskOverwrite              := Self.FAskOverwrite;
      FNeedExtractPath           := Self.FNeedExtractPath;
      FInsertDisk                := Self.FInsertDisk;
      FLocateSegment             := Self.FLocateSegment;
      FWrongSegment              := Self.FWrongSegment;
      FInsertLastSegment         := Self.FInsertLastSegment;
      FLocateLastSegment         := Self.FLocateLastSegment;
      FWrongLastSegment          := Self.FWrongLastSegment;
      FWrongNextSegment          := Self.FWrongNextSegment;
      FBadCRC                    := Self.FBadCRC;
      FBadKey                    := Self.FBadKey;
      FReplaceFile               := Self.FReplaceFile;
      FWithFile                  := Self.FWithFile;
      FConfirmFileOverwrite      := Self.FConfirmFileOverwrite;
      FExtractingFile            := Self.FExtractingFile;
      FLoadingArchiveContnent    := Self.FLoadingArchiveContnent;
      FUncompressingSolidArchive := Self.FUncompressingSolidArchive;
      FCheckingFile              := Self.FCheckingFile;
    end;
  inherited AssignTo( Dest );
end;

procedure TExtrMessages.SetLanguage( language : TLanguage );
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
        FCouldNotUncompressBlock := 'Could not uncompress Block';
        FNeedExtractPath := 'I need a path for the extraction (property ExtractPath)';
        FInsertDisk := 'Insert disk for segment #%d in drive %s';
        FLocateSegment := 'Locate segment #%d of Archive %s';
        FWrongSegment := 'This was not the segment #%d of Archive %s !';
        FInsertLastSegment := 'Insert disk containing the last segment in drive %s';
        FLocateLastSegment := 'Locate last segment of Archive %s';
        FWrongLastSegment := 'This is not the last segment !';
        FWrongNextSegment := 'This segment is not the next in the sequence';
        FBadCRC := 'Bad CRC: data stored in the archive is corrupted';
        FBadKey := 'The file could not be decrypted. You entered a bad key';
        FReplaceFile := 'Replace File :';
        FWithFile := 'With File :';
        FConfirmFileOverwrite := 'Confirm File Overwrite';
        FExtractingFile := 'Extracting %s (%d)';
        FLoadingArchiveContnent := 'Loading archive content...';
        FUncompressingSolidArchive := 'Uncompressing solid archive...';
        FCheckingFile := 'Checking %s (%d)';
      end;
    lgFrench:
      begin
        FCouldNotUncompressBlock := 'Impossible de décompresser le bloc de données';
        FNeedExtractPath := 'J''ai besoin d''un répertoire pour extraire les fichiers (propriété ExtractPath)';
        FInsertDisk := 'Insérez le disque contenant le segment #%d dans l''unité %s';
        FLocateSegment := 'Localisez le segment #%d de l''Archive %s';
        FWrongSegment := 'Ce n''était pas le segment #%d de l''Archive %s !';
        FInsertLastSegment := 'Insérez le disque contenant le dernier segment dans l''unité %s';
        FLocateLastSegment := 'Localisez le dernier segment de l''Archive %s';
        FWrongLastSegment := 'Ce n''est pas le dernier segment !';
        FWrongNextSegment := 'Ce segment n''est pas la suite du précédent';
        FBadCRC := 'Erreur de CRC: les données stockées dans l''archive sont corrompues';
        FBadKey := 'Le fichier n''a pas pu être décrypté. Vous avez saisi une mauvaise clé';
        FReplaceFile := 'Remplacer :';
        FWithFile := 'Par :';
        FConfirmFileOverwrite := 'Confirmer le remplacement de fichiers';
        FExtractingFile := 'Extraction de %s (%d)';
        FLoadingArchiveContnent := 'Chargement du contenu de l''archive...';
        FUncompressingSolidArchive := 'Décompression de l''archive solide...';
        FCheckingFile := 'Vérification de %s (%d)';
       end;
    lgChinese:
      begin
        FCouldNotUncompressBlock := 'µLªk¸ÑÀ£ÁY°Ï¶ô¡C';
        FNeedExtractPath := '½Ð´£¨Ñ¸ÑÀ£ÁY¸ô®| (ÄÝ©Ê ExtractPath)¡C';
        FInsertDisk := '½Ð´¡¤J²Ä %d ¤ù¦Ü %s ºÏºÐ¾÷¤¤¡C';
        FLocateSegment := '«ü©w²Ä %d ¤ù¤À³ÎÀÉ (À£ÁYÀÉ¬° %s) ªº¦ì¸m';
        FWrongSegment := '³o¤£¬O²Ä %d ­Ó¤À³ÎÀÉ (À£ÁYÀÉ¬° %s)!';
        FInsertLastSegment := '½Ð´¡¤J³Ì«á¤@¤ù¦Ü %s ºÏºÐ¾÷¤¤¡C';
        FLocateLastSegment := '«ü©w³Ì«á¤@¤ù¤À³ÎÀÉ (À£ÁYÀÉ¬° %s) ªº¦ì¸m';
        FWrongLastSegment := '³o¤£¬O³Ì«áªº¤À³ÎÀÉ!';
        FWrongNextSegment := '¦¹¤À³ÎÀÉµLªk»P¤W¤@­Ó¤À³ÎÀÉ³s±µ¡C';
        FBadCRC := 'CRC ¿ù»~: À£ÁYÀÉ·lÃa¡C';
        FBadKey := '±z¿é¤Jªº¸Ñ±KÁä­È¤£¥¿½T, ÀÉ®×µLªk¸Ñ±K¡C';
        FReplaceFile := '§â¥Ø«e³o­ÓÀÉ®× :';
        FWithFile := '¥H³o­ÓÀÉ®×¨ú¥N :';
        FConfirmFileOverwrite := '½T»{ÀÉ®×ÂÐ»\';
        FLoadingArchiveContnent := 'Åª¨úÀ£ÁYÀÉ¤º®e...';
        FUncompressingSolidArchive := '¥¿¦b¸ÑÀ£ÁY...';
        FCheckingFile := 'ÀË¬d %s (%d)';
      end;
    lgChineseGB:
      begin
        FCouldNotUncompressBlock := 'ÎÞ·¨½âÑ¹ËõÇø¿é¡£';
        FNeedExtractPath := 'ÇëÌá¹©½âÑ¹ËõÂ·¾¶ (ÊôÐÔ ExtractPath)¡£';
        FInsertDisk := 'Çë²åÈëµÚ %d Æ¬ÖÁ %s ´Åµú»úÖÐ¡£';
        FLocateSegment := 'Ö¸¶¨µÚ %d Æ¬·Ö¸îµµ (Ñ¹ËõµµÎª %s) µÄÎ»ÖÃ';
        FWrongSegment := 'Õâ²»ÊÇµÚ %d ¸ö·Ö¸îµµ (Ñ¹ËõµµÎª %s)!';
        FInsertLastSegment := 'Çë²åÈë×îááÒ»Æ¬ÖÁ %s ´Åµú»úÖÐ¡£';
        FLocateLastSegment := 'Ö¸¶¨×îááÒ»Æ¬·Ö¸îµµ (Ñ¹ËõµµÎª %s) µÄÎ»ÖÃ';
        FWrongLastSegment := 'Õâ²»ÊÇ×îááµÄ·Ö¸îµµ!';
        FWrongNextSegment := '´Ë·Ö¸îµµÎÞ·¨ÓëÉÏÒ»¸ö·Ö¸îµµÁ¬½Ó¡£';
        FBadCRC := 'CRC ´íÎó: Ñ¹ËõµµËð»µ¡£';
        FBadKey := 'ÄúÊäÈëµÄ½âÃÜ¼üÖµ²»ÕýÈ·, µµ°¸ÎÞ·¨½âÃÜ¡£';
        FReplaceFile := '°ÑÄ¿Ç°Õâ¸öµµ°¸ :';
        FWithFile := 'ÒÔÕâ¸öµµ°¸È¡´ú :';
        FConfirmFileOverwrite := 'È·ÈÏµµ°¸¸²¸Ç';
        FLoadingArchiveContnent := '¶ÁÈ¡Ñ¹ËõµµÄÚÈÝ...';
        FUncompressingSolidArchive := 'ÕýÔÚ½âÑ¹Ëõ...';
        FCheckingFile := '¼ì²é %s (%d)';
      end;
    lgPortuguese:
      begin
        FCouldNotUncompressBlock := 'Impossível descompactar bloco';
        FNeedExtractPath := 'Favor fornecer o caminho para extração (property ExtractPath)';
        FInsertDisk := 'Insira um disco para o segmento #%d no drive %s';
        FLocateSegment := 'Localize segmento #%d do Arquivo %s';
        FWrongSegment := 'Este não é o segmento #%d do Arquivo %s !';
        FInsertLastSegment := 'Insira o disco que contém o último segmento no drive %s';
        FLocateLastSegment := 'Localize o último segmento do Arquivo %s';
        FWrongLastSegment := 'Este não é o último segmento !';
        FWrongNextSegment := 'Este segmento não é a sequência do último.';
        FBadCRC := 'CRC incorreto: dados incluídos no Arquivo estão corrompidos';
        FBadKey := 'Arquivo não pode ser desencriptado. Senha incorreta';
        FReplaceFile := 'Substitur arquivo :';
        FWithFile := 'Pelo arquivo :';
        FConfirmFileOverwrite := 'Confirme atualizar arquivo';
        FExtractingFile := 'Extraindo %s (%d)';
        FLoadingArchiveContnent := 'Carregando conteudo do backup...';
        FUncompressingSolidArchive := 'Descompactando backup...';
        FCheckingFile := 'Verificando %s (%d)';
      end;
    lgGerman:
      begin
        FCouldNotUncompressBlock := 'Der Block konnte nicht entgepackt werden';
        FNeedExtractPath := 'Ich benötige einen Pfad zum entpacken (Eigenschaft ExtractPath)';
        FInsertDisk := 'Bitte legen Sie den Datenträger mit dem Segment #%d in das Laufwerk %s ein';
        FLocateSegment := 'Bitte geben Sie den Pfad zum Segment #%d des Archives %s ein';
        FWrongSegment := 'Dies war nicht das Segment #%d des Archives %s !';
        FInsertLastSegment := 'Bitte legen Sie den Datenträger mit dem letzten Segment in das Laufwerk %s';
        FLocateLastSegment := 'Bitte geben Sie den Pfad zum letzten Segment des Archives %s ein';
        FWrongLastSegment := 'Dies ist nicht das letzte Segment !';
        FWrongNextSegment := 'Dieses Segment führt nicht das vorherige fort';
        FBadCRC := 'Falsche Checksumme: Die im Archiv gespeicherten Daten sind ungültig';
        FBadKey := 'Die Datei konnte nicht entschlüsselt werden. Das Passwort ist falsch.';
        FReplaceFile := 'Datei ersetzen :';
        FWithFile := 'Mit der Datei :';
        FConfirmFileOverwrite := 'Datei überschreiben bestätigen';
        FExtractingFile := 'Entpacken von %s (%d)';
        FLoadingArchiveContnent := 'Einlesen des Archiv Inhalts...';
        FUncompressingSolidArchive := 'Einlesen des Archiv Inhalts...';
        FCheckingFile := 'Überprüfen von %s (%d)';
      end;
    lgItalian: // Thanks to Gabriele Bigliardi (gbigliardi@manord.com)
               // Modified by Mauro Favagrossa (mrfava@tin.it)
      begin
        FCouldNotUncompressBlock := 'Impossibile decomprimere il blocco';
        FNeedExtractPath := 'Serve un indirizzo per l''estrazione (proprietà ExtractPath)';
        FInsertDisk := 'Inserire il disco per il segmento #%d nell''unità %s';
        FLocateSegment := 'Localizzare il segmento #%d dell''archivio %s';
        FWrongSegment := 'Questo non è il segmento #%d dell''archivio %s !';
        FInsertLastSegment := 'Inserire il disco contenente l''ultimo segmento nell''unità %s';
        FLocateLastSegment := 'Localizzare l''ultimo segmento dell''archivio %s';
        FWrongLastSegment := 'Questo non è l''ultimo segmento !';
        FWrongNextSegment := 'Questo segmento non continua quello precedente';
        FBadCRC := 'CRC non valido: i dati registrati nell''archivio sono danneggiati';
        FBadKey := 'Il file non può essere decriptato. Hai inserito una chiave non valida';
        FReplaceFile := 'Sostituire il file :';
        FWithFile := 'Con il File :';
        FConfirmFileOverwrite := 'Confermare la sovrascrittura del File';
        FExtractingFile := 'Estrazione %s (%d)';
        FLoadingArchiveContnent := 'Caricamento del contenuto dell''archivio...';
        FUncompressingSolidArchive := 'Decompressione archivio...';
        FCheckingFile := 'Verifica %s (%d)';
      end;
    lgRussian:
      begin
        FCouldNotUncompressBlock := 'Íå óäàåòñÿ ðàçïàêîâàòü áëîê äàííûõ';
        FNeedExtractPath := 'Íåîáõîäèì ïóòü äëÿ ðàñïàêîâêè (ñâîéñòâî ExtractPath)';
        FInsertDisk := 'Âñòàâüòå äèñê ñ ÷àñòüþ #%d â äèñêîâîä %s';
        FLocateSegment := 'Íàéäåíà ÷àñòü #%d àðõèâà %s';
        FWrongSegment := 'Ýòî íå ÷àñòü #%d àðõèâà %s !';
        FInsertLastSegment := 'Âñòàâüòå äèñê ñ ïîñëåäíåé ÷àñòüþ â äèñêîâîä %s';
        FLocateLastSegment := 'Íàéäèòå ïîñëåäíþþ ÷àñòü àðõèâà %s';
        FWrongLastSegment := 'Ýòî íå ïîñëåäíÿÿ ÷àñòü àðõèâà !';
        FWrongNextSegment := 'Ýòà ÷àñòü íå ÿâëÿåòñÿ ïðîäîëæåíèåì ïðåäûäóùèõ ÷àñòåé àðõèâà';
        FBadCRC := 'Îøèáêà CRC: äàííûå â àðõèâå èñïîð÷åíû (íàðóøåíà öåëîñòíîñòü àðõèâà)';
        FBadKey := 'Ôàéë íå ìîæåò áûòü äåêîäèðîâàí. Âû ââåëè íåïðàâèëüíûé êëþ÷';
        FReplaceFile := 'Çàìåñòèòü ôàéë :';
        FWithFile := '...ôàéëîì :';
        FConfirmFileOverwrite := 'Ïîäòâåðäèòå ïåðåçàïèñü ôàéëà';
        FExtractingFile := 'Èçâëå÷åíèå %s (%d)';
        FLoadingArchiveContnent := 'Çàãðóçêà ñîäåðæèìîãî àðõèâà...';
        FUncompressingSolidArchive := 'Ðàçàðõèâèðîâàíèå ñïëîøíîãî àðõèâà...';
        FCheckingFile := 'Ïðîâåðêà %s (%d)';
      end;
    lgSpanish:
      begin
        FCouldNotUncompressBlock := 'No es posible descomprimir el bloque';
        FNeedExtractPath := 'Es necesario la trayectoria de extracción (property ExtractPath)';
        FInsertDisk := 'Inserta el disco del segmento #%d en la unidad %s';
        FLocateSegment := 'Localiza el segmento #%d del archivo %s';
        FWrongSegment := 'No es el segmento #%d del archivo %s !';
        FInsertLastSegment := 'Inserta el disco que contiene el ultimo segmento en la unidad %s';
        FLocateLastSegment := 'Localiza el ultimo segmento del archivo %s';
        FWrongLastSegment := '¡ Este no es el último segmento !';
        FWrongNextSegment := 'El segmento no es el siguente en la sequencia';
        FBadCRC := 'CRC incorrecto: los datos dentro del archivo estan corrompidos';
        FBadKey := 'El archivo no podra desencriptarse. Introdujo una clave invalida';
        FReplaceFile := 'Reemplazar :';
        FWithFile := 'Con :';
        FConfirmFileOverwrite := 'Confirma sobreescribir archivo';
        FExtractingFile := 'Extrayendo %s (%d)';
        FLoadingArchiveContnent := 'Cargando contenido del archivo...';
        FUncompressingSolidArchive := 'Descomprimiendo archivo solido...';
        FCheckingFile := 'Verificando %s (%d)';
      end;
    lgDanish:
      begin
        FCouldNotUncompressBlock := 'Kunne ikke dekomprimere Blokken';
        FNeedExtractPath := 'Jag ønsker en sti for udpakningen (property ExtractPath)';
        FInsertDisk := 'Indsæt diskette for segment #%d i drev %s';
        FLocateSegment := 'Find segment #%d af arkivet %s';
        FWrongSegment := 'dette var ikke segmentet #%d fra arkivet %s !';
        FInsertLastSegment := 'Indsæt diskette indeholdende sidste segment i drev %s';
        FLocateLastSegment := 'Finde sidste segment af arkivet %s';
        FWrongLastSegment := 'Dette er ikke det sidste segment !';
        FWrongNextSegment := 'Dette segment er ikke næste i rækken';
        FBadCRC := 'Forkert CRC: data gemt i arkivet er fejl behæftet';
        FBadKey := 'Filen kunne ikke blive dekrypteret. Du angav en forkert nøgle';
        FReplaceFile := 'Udskift fil :';
        FWithFile := 'Skriv fil :';
        FConfirmFileOverwrite := 'Bekræft fil overskrivning';
        FExtractingFile := 'Udpak %s (%d)';
        FLoadingArchiveContnent := 'Læser arkiv indhold...';
        FUncompressingSolidArchive := 'Dekomprimer fast arkiv...';
        FCheckingFile := 'Checker %s (%d)';
      end;
    lgDutch:
      begin
        FCouldNotUncompressBlock := 'Kon blok niet decomprimeren';
        FNeedExtractPath := 'Ik heb een pad nodig voor de extractie (eigenschap ExtractPath)';
        FInsertDisk := 'Steek schijf voor segment #%d in station %s';
        FLocateSegment := 'Localiseer segment #%d van archief %s';
        FWrongSegment := 'Dit is niet segment #%d van archief %s!';
        FInsertLastSegment := 'Steek de schijf met het laatste segment in station %s';
        FLocateLastSegment := 'Localiseer het laatste segment van archief %s';
        FWrongLastSegment := 'Dit is niet het laatste segment!';
        FWrongNextSegment := 'Dit is niet het eerstvolgende segment';
        FBadCRC := 'Foute CRC: de gegevens opgeslagen in dit archief zijn verminkt';
        FBadKey := 'Het bestand kon niet worden gedecodeerd.  U gaf een verkeerd wachtwoord op';
        FReplaceFile := 'Vervang bestand:';
        FWithFile := 'door bestand:';
        FConfirmFileOverwrite := 'Bevestig het overschrijven van bestand';
        FExtractingFile := 'Terugzetten van %s (%d)';
        FLoadingArchiveContnent := 'Ophalen inhoudsopgave archief...';
        FUncompressingSolidArchive := 'Decomprimering van bestendig archief...';
        FCheckingFile := 'Testen van  %s (%d)';
      end;
    lgCzech: // thanks to Hana Krizova
      begin
        FCouldNotUncompressBlock := 'Blok nelze dekomprimovat';
        FNeedExtractPath := 'Zadejte cestu pro uložení rozbalených souborù (property ExtractPath)';
        FInsertDisk := 'Vložte disk obsahující segment #%d do mechaniky %s';
        FLocateSegment := 'Zadejte jméno souboru se segmentem #%d archivu %s';
        FWrongSegment := 'Tento soubor není segment #%d archivu %s!';
        FInsertLastSegment := 'Vložte disk obsahující poslední segment do mechaniky %s';
        FLocateLastSegment := 'Zadejte jméno souboru posledního segmentu archivu %s';
        FWrongLastSegment := 'Tento soubor není poslední segment!';
        FWrongNextSegment := 'Tento segment není následující v posloupnosti';
        FBadCRC := 'Špatná hodnota CRC: data uložená v archivu jsou narušena';
        FBadKey := 'Nelze dešifrofat soubor. Zadaný klíè je nesprávný';
        FReplaceFile := 'Nahradit soubor:';
        FWithFile := 'Souborem:';
        FConfirmFileOverwrite := 'Potvrdit pøepsání souboru';
        FExtractingFile := 'Rozbaluje se %s (%d)';
        FLoadingArchiveContnent := 'Naèítá se obsah archivu ...';
        FUncompressingSolidArchive := 'Dekomprimuje se SOLID archiv ...';
        FCheckingFile := 'Kontroluje se %s (%d)';
      end;
  end;
end;

procedure TExtrMessages.SetGlobalStrings;
begin
  inherited;
  strReplaceFile             := ReplaceFile;
  strWithFile                := WithFile;
  strConfirmFileOverwrite    := ConfirmFileOverwrite;
end;

////////////////////////////////////////////////////////////
// Protected section

procedure TCustomExtractor.AssignTo(Dest: TPersistent);
begin
  if Dest is TCustomExtractor then
    with TCustomExtractor(Dest) do
      begin
        ExtractPath := Self.ExtractPath;
        RestoreAction := Self.RestoreAction;
        SFXGenerator := Self.SFXGenerator;
      end;
  inherited AssignTo(Dest);
end;

function  TCustomExtractor.CreateMessages : TMessages;
begin
  Result := TExtrMessages.Create;
end;

function  TCustomExtractor.GetMessages : TExtrMessages;
begin
  Result := FMessages as TExtrMessages;
end;

procedure TCustomExtractor.SetMessages( val : TExtrMessages );
begin
  FMessages.Assign( val );
end;

// See CheckStream in CheckIntegrity that does the same but don't write to
// an external file.
procedure TCustomExtractor.UncompressStream( dest : TStream );
var
  toRead, bytesProcessed : Integer;
  uncompressedBlockSize : Integer;
  crc, crcRead : Integer;
  IsCompressed : Boolean;
  blockSize : Integer;
begin
  // If this is an empty file then, do nothing
  if FCurrentFileEntry.ArchiveInfo.Size = 0 then
    Exit;
  Include( FInternalOperation, ioUncompressingStream );
  try
    dest.Position := FCurrentFileEntry.FileOffset;
    // Read all sections of the file, until the last one
    repeat
      bytesProcessed := 0;
      isCompressed := True;
      // Read the section of the file written in this segment
      repeat
        // Check if we are at the end of the segment.
        // If we chained to the next segment, then offset the file
        // beeing currently extracted to the place when extraction
        // should begin.
        if CheckEOF then
          dest.Position := FCurrentFileEntry.FileOffset;
        // If we are at the end of the segment, but are opening only
        // one segment at a time, then exit.
        if (oOpenSingleSegment in Options) and Eof(FStream) then
          Exit;
        // Read the block of data
        toRead  := ReadInteger( FStream );
        DecodeBlockSize( toRead, IsCompressed, blockSize );
        toRead := blockSize;
        crcRead := ReadInteger( FStream );
        FStream.ReadBuffer( FSrcBlock^, blockSize );
        // Decrypt the block of data
        if ffCrypted in FCurrentFileEntry.FileFlag then
          begin
            CheckKey;
            DecryptBlock( FSrcBlock, FSrcBlock, blockSize, blockSize );
          end;
        // Check the CRC
        crc := not CRC32R( Longint($FFFFFFFF), FSrcBlock^, blockSize );
        if crc <> crcRead then
          if ffCrypted in FCurrentFileEntry.FileFlag then
            raise EArchiverBadKey.Create( Messages.BadKey )
          else
            raise EArchiverBadCRC.Create( Messages.BadCRC );
        // Uncompress the block of data, if it was compressed
        if IsCompressed then
          begin
            uncompressedBlockSize := FHeader.BlockSize;
            if not UncompressBlock( FDestBlock, uncompressedBlockSize, FSrcBlock, blockSize ) then
              raise EArchiverUncompress.Create( Messages.CouldNotUncompressBlock );
          end
        else
          begin // if it was not, then just copy it
            uncompressedBlockSize := blockSize;
            Move( FSrcBlock^, FDestBlock^, uncompressedBlockSize );
          end;
        // Write the uncompressed and decryoted block to the file
        dest.WriteBuffer( FDestBlock^, uncompressedBlockSize );
        // Update all counters
        FBytesProcessed := FBytesProcessed + toRead + sizeof(Integer)*2;
        Inc( bytesProcessed, toRead + sizeof(Integer)*2 );
        UpdateProgress;
      until bytesProcessed >= FCurrentFileEntry.SegmentInfo.CompressedSize;
    until (ffFinalSegment in FCurrentFileEntry.FileFlag) or (oOpenSingleSegment in Options);
  finally
    Exclude( FInternalOperation, ioUncompressingStream );
  end;
end;

function TCustomExtractor.UncompressBlock( DestBlock : PChar; var DestSize : Integer; SrcBlock : PChar; SrcSize : Integer) : Boolean;
begin
  if Assigned( FOnUncompressBlock ) then
    Result := FOnUncompressBlock( Self, DestBlock, DestSize, SrcBlock, SrcSize )
  else
    begin
      // Do nothing special : copy the src to the dest !
      DestSize := SrcSize;
      move( SrcBlock^, DestBlock^, SrcSize );
      Result := True;
    end;
end;

procedure TCustomExtractor.DecryptBlock( DestBlock, SrcBlock : PChar; var DestSize : Integer; SrcSize : Integer);
begin
  if Assigned(FOnDecryptBlock) then
    FOnDecryptBlock( Self, DestBlock, SrcBlock, DestSize, SrcSize );
end;

// anOffset is the offset where the FileEntry begins
procedure TCustomExtractor.SkipFile( anOffset : TFileSize );
var
  tmp : TFileEntry;
begin
  Include( FInternalOperation, ioSkippingStream );
  try
    // Go at the beginning of the file
    FStream.Seek( anOffset + GetStartOffset, soFromBeginning );
    // Read file entry
    ReadFileEntry( FCurrentFileEntry );
    while True do
      begin
        // skip file data
        FStream.Seek( FCurrentFileEntry.SegmentInfo.CompressedSize, soFromCurrent );
        FBytesProcessed := FBytesProcessed + FCurrentFileEntry.SegmentInfo.CompressedSize;
        UpdateProgress;
        if (ffFinalSegment in FCurrentFileEntry.FileFlag) or (oOpenSingleSegment in Options) then
          Break
        else
          begin
            tmp := FCurrentFileEntry;
            CheckEOF;
            // Restore the Segment# and the Offset.
            FCurrentFileEntry.Segment := tmp.Segment;
            FCurrentFileEntry.Offset := tmp.Offset;
          end;
      end;
    Inc( FCurrentFileIdx );
  finally
    Exclude( FInternalOperation, ioSkippingStream );
  end;
  CheckEOF;
end;

function  TCustomExtractor.Eof( S : TStream ) : Boolean;
begin
  Result := Assigned(S) and (S.Position >= FHeader.EndOffset + GetStartOffset);
end;

function TCustomExtractor.SegmentBelongsToArchive( const aFileName : String; var AHeader : TArchiveHeader ) : Boolean;
var
  tmp : String;
begin
  Result := False;
  tmp := ExtractFileName( aFileName );
  // Check if we've got the same root for the Archive name
  //if UpperCase(Copy(tmp, 1, Length(FArchiveName))) <> UpperCase(FArchiveName) then
    //Exit;
  // Check the Random id
  if not ReadHeaderOfFile( aFileName, AHeader ) or (AHeader.RandomID <> FHeader.RandomID) then
    Exit;
  // It's OK
  Result := True;
end;

procedure TCustomExtractor.OpenSegment( val : Integer );
var
  AHeader : TArchiveHeader;
  AFileEntry : TFileEntry;

  procedure AskDisk;
  var
    msg : String;
    tmp : String;
  begin
    msg := Format(Messages.InsertDisk, [val, FArchiveDrive]);
    while True do
      begin
        tmp := FArchiveDrive;
        if Assigned(FOnInsertDisk) then
          FOnInsertDisk( Self, val, FArchiveDrive )
        else
          if not InputQuery(Messages.SystemMessage, msg, FArchiveDrive ) then
            Abort;
        if (Length(FArchiveDrive)<2) or (FArchiveDrive[2]<>':') then
          FArchiveDrive := tmp;
        if (Length(FArchiveDrive)>0) and DiskInDrive(FArchiveDrive[1]) and
           SegmentBelongsToArchive( GetSegmentName(val), AHeader ) and
           (AHeader.Segment = val) then
          Break;
      end;
    // Generate a name for segment #val, if FArchiveDrive was changed
    FFileName := GetSegmentName( val );
  end;

  procedure LocateSegment;
  var
    fName : String;
    title : String;
  begin
    while True do
      begin
        title := Format( Messages.LocateSegment, [val, FArchiveName]);
        fName := GetSegmentName( val );
        if Assigned(FOnLocateSegment) then
          FOnLocateSegment( Self, val, fname )
        else
          if not SelectFile( title, fName ) then
            Abort;
        if SegmentBelongsToArchive( fName, AHeader ) and (AHeader.Segment = val) then
          begin
            Self.FFileName := fName;
            ExplodeFileName;
            Break;
          end
        else
          MessageDlg( Format(Messages.WrongSegment, [val, FArchiveName]), mtWarning, [mbOk], 0 );
      end;
  end;

begin
  // If the requested segment is already opened, then exit
  if val = FHeader.Segment then
    Exit;
  StopTimer;
  CloseSegment;
  // Generate a name for segment #val
  FFileName := GetSegmentName( val );
  AHeader.Segment := 0;
  // check if segment exists in the current folder
  if not ( SegmentBelongsToArchive( FFileName, AHeader ) and (AHeader.Segment = val) ) then
    // if it was not in the current folder
    begin
      // Else if archive is on a removable disk, then ask user to change the disk
      if IsRemovableDisk(FArchiveDrive) then
        AskDisk
      else
      // Else ask user to locate it
        LocateSegment;
    end;

  // Open the segment
  OpenStream;
  AdjustArchiveSize;
  if Assigned( FOnSegmentChanged ) then
    FOnSegmentChanged( Self );
  if (ioUncompressingStream in FInternalOperation) or
     (ioSkippingStream in FInternalOperation) then
    begin
      AFileEntry := FCurrentFileEntry;
      ReadFileEntry( FCurrentFileEntry );
      CheckCurrentFile( AFileEntry, FCurrentFileEntry );
    end;
  StartTimer;
end;

procedure TCustomExtractor.CloseSegment;
begin
  CloseStream;
end;

procedure TCustomExtractor.NeedFirstSegment;
begin
  if not (oOpenSingleSegment in Options) then
    OpenSegment( 1 );
end;

procedure TCustomExtractor.NeedLastSegment;
var
  AHeader : TArchiveHeader;
  seg : Integer;

  function IsLastSegment( const aFileName : String ) : Boolean;
  begin
    Result := SegmentBelongsToArchive( aFileName, AHeader ) and
              (afFinalSegment in AHeader.ArchiveFlag);
  end;

  procedure AskLastDisk;
  var
    SR : TSearchRec;
    Found : Integer;
    tmp, msg : String;
  begin
    msg := Format(Messages.InsertLastSegment, [FArchiveDrive]);
    while True do
      begin
        tmp := FArchiveDrive;
        if Assigned( FOnInsertLastDisk ) then
          FOnInsertLastDisk( Self, FArchiveDrive )
        else
          if not InputQuery(Messages.SystemMessage, msg, FArchiveDrive ) then
            Abort;
        if (Length(FArchiveDrive)<2) or (FArchiveDrive[2]<>':') then
          FArchiveDrive := tmp;
        if (Length(FArchiveDrive)>0) and DiskInDrive(FArchiveDrive[1]) then
          begin
            // Examine ArchiveName*.ArchiveExt
            tmp := FArchiveDrive+FArchiveDir+FArchiveName+'*'+kDefaultExt;
            Found := FindFirst( tmp, faAnyFile, SR );
            try
              while (Found = 0)  do
                begin
                  if ((SR.Attr and faDirectory) = 0) and IsLastSegment(FArchiveDrive+FArchiveDir+SR.Name) then
                    begin
                      FFileName := FArchiveDrive+FArchiveDir+SR.Name;
                      Exit;
                    end;
                  Found := FindNext( SR );
                end;
            finally
              FindClose(SR);
            end;
          end;
      end;
  end;

  procedure LocateLastSegment;
  var
    fName, title : String;
  begin
    while True do
      begin
        title := Format(Messages.LocateLastSegment, [FArchiveName]);
        fName := GetSegmentName( seg );
        if Assigned( FOnLocateLastSegment ) then
          FOnLocateLastSegment( Self, fName )
        else
          if not SelectFile( title, fName ) then
            Abort;
        if IsLastSegment(fName) then
          begin
            Self.FFileName := fName;
            ExplodeFileName;
            Break;
          end
        else
          MessageDlg( Messages.WrongLastSegment, mtWarning, [mbOk], 0 );
      end;
  end;

begin
  if (afFinalSegment in FHeader.ArchiveFlag) or (oOpenSingleSegment in Options) then
    Exit;
  StopTimer;
  seg := FHeader.Segment;
  CloseSegment;
  Exclude( AHeader.ArchiveFlag, afFinalSegment );
  while not (afFinalSegment in AHeader.ArchiveFlag) do
    begin
      // Generate a name for segment #n+1
      Inc( seg );
      FFileName := GetSegmentName( seg );
      // check if segment exists in the current folder
      if SegmentBelongsToArchive( FFileName, AHeader ) then
        Continue
      else
        Break;
    end;
  // if we could not find the final segment
  if not (afFinalSegment in AHeader.ArchiveFlag) then
    // Else if archive is on a removable disk, then ask user to change the disk
    if IsRemovableDisk(FArchiveDrive) then
      AskLastDisk
    else
      // Else ask user to locate it
      LocateLastSegment;
  OpenStream;
  StartTimer;
end;

function TCustomExtractor.CheckEOF : Boolean;
begin
  Result := False;
  if not EOF(FStream) or (afFinalSegment in FHeader.ArchiveFlag) or
     (oOpenSingleSegment in Options) then
    Exit;
  Include( FInternalOperation, ioSwappingSegment );
  try
    OpenSegment( FHeader.Segment + 1 );
    Result := True;
  finally
    Exclude( FInternalOperation, ioSwappingSegment );
  end;
end;

procedure TCustomExtractor.GetProgressInformations;
begin
  FTotTicks := 0;
  if (oOpenSingleSegment in Options) then
    FCompressedArchiveSize := 0
  else
    begin
      // First, we try to instpect the archive size of the first segment.
      // if it is greater than the segment size, it means that it was updated
      if (FHeader.Segment = 1) and
         (FHeader.ArchiveInfo.CompressedSize > FHeader.SegmentInfo.CompressedSize ) then
        FCompressedArchiveSize := FHeader.ArchiveInfo.CompressedSize
      else // else we ask for the last segment and we'll be sure to have the true archive size.
        begin
          NeedLastSegment;
          FCompressedArchiveSize := FHeader.ArchiveInfo.CompressedSize;
        end;
    end;
end;

// If you call this function, you must ensure that you already read the FileEntry
// and that the position of the stream is just after this entries.
procedure TCustomExtractor.ExtractFileData( const fileEntry : TFileEntry; const DestFileName : String );

  function AskOverwrite( const fname : String ) : Boolean;
  var
    id : Integer;
  begin
    if FAlwaysOverwrite then
      begin
        Result := True;
        Exit;
      end;
    StopTimer;
    id := QueryFileOverwrite( AdjustPath(fName, 50), AdjustPath( fileEntry.Name, 50),
                              GetFileSize(fname), fileEntry.ArchiveInfo.Size,
                              GetFileDate(fname), fileEntry.Date );
    StartTimer;
    if id = mrCancel then
      Abort;
    FAlwaysOverwrite := (id = 102); // 102 = button "Yes to all"
    Result := (id = mrYes) or FAlwaysOverwrite;
  end;

var
  destPath : String;
  S : TStream;
  mode : Integer;
  tmpBytesProcessed : TArchiveSize;
begin
  // Check if the destination file already exists
  if FileExists( DestFileName ) then
    case RestoreAction of
      raOverwrite:
        begin
          // Do nothing !
        end;
      raSkip:
        begin
          SkipFile( fileEntry.Offset );
          Exit;
        end;
      raUpdate, raUpdateExisting:
        begin
          if GetFileDate( DestFileName ) >= fileEntry.Date then
            begin
              SkipFile( fileEntry.Offset );
              Exit;
            end;
        end;
      raAsk:
        begin
          if not AskOverwrite( DestFileName ) then
            begin
              SkipFile( fileEntry.Offset );
              Exit;
            end;
        end;
    end // end of case
  else
    case RestoreAction of
      raExistingOnly, raUpdateExisting:
        begin
          SkipFile( fileEntry.Offset );
          Exit;
        end;
    end;
  if ioOpenSolid in FInternalOperation then
    begin
      DisplayMessage( Messages.UncompressingSolidArchive );
      AddToLog( Messages.UncompressingSolidArchive );
    end
  else
    begin
      DisplayMessage( Format( Messages.ExtractingFile, [ExtractFileName(DestFileName), fileEntry.ArchiveInfo.Size]) );
      AddToLog( Format( Messages.ExtractingFile, [DestFileName, fileEntry.ArchiveInfo.Size]) );
    end;
  tmpBytesProcessed := FBytesProcessed;
  // Extract file data
  try
    // Prepare path for destination
    destPath := ExtractFilePath( DestFileName );
    ForceDirectories( destPath );
    if ffEmptyFolder in fileEntry.FileFlag then
      Exit;
    // Delete an existing file if we start extraction from the begining
    if FileExists( DestFileName ) then
      begin
        if (fileEntry.FileOffset = 0) and not(oOpenSingleSegment in Options) then
          mode := fmCreate
        else
          mode := fmOpenReadWrite or fmShareDenyWrite;
      end
    else
      mode := fmCreate;
    // Remove ReadOnly, Hidden, System attributes
    // Otherwise we can't overwrite the file, or write in it.
    FileSetAttr( DestFileName, FileGetAttr(DestFileName) and (faVolumeID or faDirectory) );
    // Create destination stream
    S := NewStreamObject( DestFileName, mode );
    try
      UncompressStream( S );
      Inc( FCurrentFileIdx );
      // Update date of the file
      FileSetDate( (S as TFileStream).Handle, DateTimeToFileDate(FCurrentFileEntry.Date) );
    finally
      S.Free;
    end;
    // Update attributes of the file
    FileSetAttr( DestFileName, FCurrentFileEntry.Attr );
    // File is extracted
    if Assigned( FOnFileExtracted ) then
      FOnFileExtracted( Self, FCurrentFileEntry, destPath );
    CheckEOF;
  except
    // if an exception occurred
    on E : Exception do
      begin
        // Skip the Abort exception
        if E is EAbort then
          raise;
        // if we can continue, then skip this file
        if CanContinue( E ) then
          begin
            FBytesProcessed := tmpBytesProcessed;
            SkipFile( fileEntry.Offset );
          end
        else
          // if the user was not informed of the error, then raise it
          if (ErrorAction <> eaAsk) and not Assigned(FOnError) then
            raise
          else // abort action because user already knows the error
            Abort;
      end;
  end; // end of try...except
end;


function TCustomExtractor.GetDestinationPath( const fileEntry : TFileEntry ) : String;
var
  idx : Integer;
  drive : String;
  path, server, resource, tmp : String;
begin
  // If we must restore the path
  if oRestorePath in Options then
    begin
      path := ExtractFilePath( fileEntry.Name );
      // if the stored path is a whole path : Drive:\path1\path...\filename
      idx := Pos( ':', path );
      if idx > 0 then
        begin
          // check if the drive of the path is valid.
          // If it is not then use the drive of the extract path if it is valid.
          // If it is not then use C:\
          drive := System.Copy( path, 1, idx - 1 );
          if (Length(drive) = 1) and DirectoryExists(drive+':\') then
            Result := path
          else if (Length(FExtractPath)>1) and (FExtractPath[2]=':') and
                  DirectoryExists(FExtractPath[1]+':\') then
            Result := FExtractPath[1]+System.Copy( path, 2, Length(path) )
          else
            Result := 'C'+System.Copy( path, 2, Length(path) );
        end
      // else if the path is a network path : \\server\resource\path1\path...\filename
      else if Copy(path, 1, 2) = '\\' then
        begin
          tmp := Copy(path, 3, Length(path));
          idx := Pos( '\', tmp );
          if idx > 0 then
            begin
              server := Copy( tmp, 1, idx-1 );
              tmp := Copy(tmp, idx+1, Length(tmp) );
              idx := Pos( '\', tmp );
              if idx > 0 then
                begin
                  resource := Copy(tmp, 1, idx-1);
                  if DirectoryExists( Format('\\%s\%s', [server, resource]) ) then
                    Result := path
                  else
                    Result := AppendSlash(ExtractPath) + Copy(path, 3, Length(path));
                end
              else
                Result := AppendSlash(ExtractPath) + Copy(path, 3, Length(path));
            end
          else
            Result := AppendSlash(ExtractPath) + Copy(path, 3, Length(path));
        end
      else // use ExtractPath + relative path
        Result := AppendSlash(ExtractPath) + path;
    end
  else // use ExtractPath as default path
    Result := ExtractPath;
end;

procedure TCustomExtractor.CheckCurrentFile( const last, new : TFileEntry );
begin
  if (last.Name <> new.Name) or
     (last.ArchiveInfo.Size <> new.ArchiveInfo.Size) or
     (last.Date <> new.Date) then
    raise EArchiver.Create( Messages.WrongNextSegment );
end;

procedure TCustomExtractor.AfterOpen;
begin
  // Show the comment
  if Assigned(FOnShowComment) and (FHeader.Comment <> '') then
    FOnShowComment( Self, FHeader.Comment );
  // Test special case of a solid archive
  OpenSolidData;
  inherited;
  // Enumerate files
  if (oEnumerateAfterOpen in Options) or
     (oMaintainFileDirectory in Options) then
    begin
      Include( FInternalOperation, ioEnumAfterOpen );
      try
        EnumerateFiles;
      finally
        Exclude( FInternalOperation, ioEnumAfterOpen );
      end;
    end;
end;

procedure TCustomExtractor.BeforeClose;
begin
  inherited;
  // Test special case of a solid archive
  CloseSolidData;
end;

procedure TCustomExtractor.CheckSFX( const aFileName : String );
begin
  if Assigned( FSFXGenerator ) then
    begin
      if IsExeFile( aFileName ) then
        begin
          FSFXGenerator.DefineSizeFromFile( aFileName );
          SFXCodeSize := FSFXGenerator.CurrentSFXCodeSize + FSFXGenerator.CurrentTagInfoSize;
        end
    end;
  inherited;
end;

procedure TCustomExtractor.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
    if AComponent = FSFXGenerator then
      FSFXGenerator := nil;
end;

procedure TCustomExtractor.AdjustSolidOptions;
begin
  Exclude( FOptions, oCreateSolidArchives );
  Exclude( FOptions, oWriteSFXCode );
  Exclude( FOptions, oCrypt );
  Exclude( FOptions, oCompress );
  FMaxSegmentSize := 0; // Forbid segmentation
end;

procedure TCustomExtractor.OpenSolidData;
var
  tempName : String;
begin
  inherited;
  FIsSolidArchive := afSolid in Header.ArchiveFlag;
  if FIsSolidArchive then
    begin
      FTmpFileDate := 0;
      tempName := GetTempFileName;
      Include( FInternalOperation, ioOpenSolid );
      try
        if Header.SegmentInfo.FileCount > 0 then
          begin
            ReadHeader;
            ReadFileEntry( FCurrentFileEntry );
            try
              DoExtractFileTo( 1,
                             FCurrentFileEntry.Offset,
                             Round(FCompressedArchiveSize*1.0),
                             tempName );
              FBytesProcessed := 0;
              FTotTicks := 0;
              FTmpFileDate := GetFileDate( tempName );
            except
              DeleteFile( tempName );
              raise;
            end;
          end;
      finally
        Exclude( FInternalOperation, ioOpenSolid );
        CloseStream;
      end;
      AdjustSolidOptions;
      FFileName := tempName;
      //ExplodeFileName;
      if FileExists( FFileName ) then
        begin
          OpenStream;
          GetProgressInformations;
        end
      else
        begin
          CreateArchive;
          FTmpFileDate := 1;
        end;
    end;
end;

procedure TCustomExtractor.CloseSolidData;
var
  tempName : String;
begin
  if FIsSolidArchive then
    begin
      tempName := FFileName;
      CloseStream;
      FFileName := FOldFileName;
      ExplodeFileName;
      FOptions := FOldOptions;
      DeleteFile( tempName );
    end;
end;

function  TCustomExtractor.GetOpenMode : Integer;
begin
  Result := fmOpenRead or fmShareDenyWrite;
end;

procedure TCustomExtractor.Start;
begin
  if FStartCount = 0 then
    begin
      FAlwaysOverwrite := False;
    end;
  inherited;
end;

procedure TCustomExtractor.DoEnumerateFiles;
begin
  FOperation := opEnumerate;
  Start;
  try
    CheckOpen;
    FBytesToProcess := FCompressedArchiveSize;
    FBytesProcessed := 0;
    NeedFirstSegment;
    // Skip the header
    ReadHeader;
    ClearFiles;
    // Display operation
    DisplayMessage( Messages.LoadingArchiveContnent );
    AddToLog( Messages.LoadingArchiveContnent );
    // For each segment
    repeat
      // For each file stored in the archive
      while not Eof(FStream) do
        begin
          ReadFileEntry( FCurrentFileEntry );
          AddFileToList( FCurrentFileEntry );
          // Skip the file
          SkipFile( FCurrentFileEntry.Offset );
          // Call Enumerate event
          if Assigned( FOnEnumeration ) and
             (not(ffEmptyFolder in FCurrentFileEntry.FileFlag) or (oShowEmptyFolders in Options)) then
            FOnEnumeration( Self, FCurrentFileEntry );
        end;
    until (afFinalSegment in FHeader.ArchiveFlag) or (oOpenSingleSegment in Options);
  finally
    Finish;
    FOperation := opNone;
  end;
end;

procedure TCustomExtractor.DoExtractFile( aSegment : Word; anOffset, compressedSize : Integer );
var
  destPath, destName : String;
begin
  if ExtractPath = '' then
    raise EArchiver.Create(Messages.NeedExtractPath);
  CheckOpen;
  OpenSegment( aSegment );
  // Go at the beginning of the file
  FStream.Seek( anOffset + GetStartOffset, soFromBeginning );
  // Read file entry
  ReadFileEntry( FCurrentFileEntry );
  // Prepare destination file name
  destPath := GetDestinationPath( FCurrentFileEntry );
  destName := AppendSlash(destPath)+ExtractFileName(FCurrentFileEntry.Name);
  // extract file data to destination
  DoExtractFileTo( aSegment, anOffset, compressedSize, destName );
end;

procedure TCustomExtractor.DoExtractFileTo( aSegment : Word; anOffset, compressedSize : Integer; const DestFileName : String );
begin
  FOperation := opExtract;
  Start;
  try
    CheckOpen;
    if FBytesToProcess = 0 then
      FBytesToProcess := compressedSize;
    OpenSegment( aSegment );
    // Go at the beginning of the file
    FStream.Seek( anOffset + GetStartOffset, soFromBeginning );
    // Read file entry
    ReadFileEntry( FCurrentFileEntry );
    // extract file data to destination
    ExtractFileData( FCurrentFileEntry, DestFileName );
  finally
    Finish;
  end;
end;

procedure TCustomExtractor.DoExtractFiles;
var
  accept : Boolean;
  destPath, destName: String;
  cpt : Integer;
begin
  if ExtractPath = '' then
    raise EArchiver.Create(Messages.NeedExtractPath);
  cpt := 0;
  FOperation := opExtract;
  Start;
  try
    CheckOpen;
    FBytesToProcess := FCompressedArchiveSize;
    NeedFirstSegment;
    // Skip the header
    ReadHeader;
    // For each segment
    repeat
      // For each file stored in the archive
      while not EOF(FStream) do
        begin
          ReadFileEntry( FCurrentFileEntry );
          // Prepare destination path
          destPath := GetDestinationPath( FCurrentFileEntry );
          // Confirm extraction
          accept := True;
          if Assigned( FOnExtractFile ) then
             FOnExtractFile( Self, FCurrentFileEntry, destPath, accept );
          if Assigned( FOnExtractFileByIndex ) then
             FOnExtractFileByIndex( Self, cpt, destPath, accept );
          if not Accept then
            begin
              SkipFile( FCurrentFileEntry.Offset );
              Inc( cpt );
              Continue;
            end;
          // Prepare destination file name
          destName := AppendSlash(destPath)+ExtractFileName(FCurrentFileEntry.Name);
          // extract file data to destination
          ExtractFileData( FCurrentFileEntry, destName );
          Inc( cpt );
        end; // end of while
    until (afFinalSegment in FHeader.ArchiveFlag) or (oOpenSingleSegment in Options);
  finally
    Finish;
  end;
end;

procedure TCustomExtractor.DoCheckIntegrity;

  procedure CheckStream;
  var
    toRead, bytesProcessed : Integer;
    uncompressedBlockSize : Integer;
    crc, crcRead : Integer;
    IsCompressed : Boolean;
    blockSize : Integer;
  begin
    // This procedure is copied and simplied from the UncompressStream procedure.

    // If this is an empty file then, do nothing
    if FCurrentFileEntry.ArchiveInfo.Size = 0 then
      Exit;
    Include( FInternalOperation, ioUncompressingStream );
    try
      // Read all sections of the file, until the last one
      repeat
        bytesProcessed := 0;
        isCompressed := True;
        // Read the section of the file written in this segment
        repeat
          // Check if we are at the end of the segment.
          CheckEOF;
          // If we are at the end of the segment, but are opening only
          // one segment at a time, then exit.
          if (oOpenSingleSegment in Options) and Eof(FStream) then
            Exit;
          // Read the block of data
          toRead  := ReadInteger( FStream );
          DecodeBlockSize( toRead, IsCompressed, blockSize );
          toRead := blockSize;
          crcRead := ReadInteger( FStream );
          FStream.ReadBuffer( FSrcBlock^, blockSize );
          // Decrypt the block of data
          if ffCrypted in FCurrentFileEntry.FileFlag then
            begin
              CheckKey;
              DecryptBlock( FSrcBlock, FSrcBlock, blockSize, blockSize );
            end;
          // Check the CRC
          crc := not CRC32R( Longint($FFFFFFFF), FSrcBlock^, blockSize );
          if crc <> crcRead then
            if ffCrypted in FCurrentFileEntry.FileFlag then
              raise EArchiverBadKey.Create( Messages.BadKey )
            else
              raise EArchiverBadCRC.Create( Messages.BadCRC );
          // Uncompress the block of data, if it was compressed
          if IsCompressed then
            begin
              uncompressedBlockSize := FHeader.BlockSize;
              if not UncompressBlock( FDestBlock, uncompressedBlockSize, FSrcBlock, blockSize ) then
                raise EArchiverUncompress.Create( Messages.CouldNotUncompressBlock );
            end
          else
            begin // if it was not, then just copy it
              uncompressedBlockSize := blockSize;
              Move( FSrcBlock^, FDestBlock^, uncompressedBlockSize );
            end;
          // Update all counters
          FBytesProcessed := FBytesProcessed + toRead + sizeof(Integer)*2;
          Inc( bytesProcessed, toRead + sizeof(Integer)*2 );
          UpdateProgress;
        until bytesProcessed >= FCurrentFileEntry.SegmentInfo.CompressedSize;
      until (ffFinalSegment in FCurrentFileEntry.FileFlag) or (oOpenSingleSegment in Options);
    finally
      Exclude( FInternalOperation, ioUncompressingStream );
    end;
  end;

  procedure CheckFile( const FileEntry : TFileEntry );
  var
    tmpBytesProcessed : TArchiveSize;
  begin
    DisplayMessage( Format( Messages.CheckingFile, [ExtractFileName(fileEntry.Name), fileEntry.ArchiveInfo.Size]) );
    AddToLog( Format( Messages.CheckingFile, [fileEntry.Name, fileEntry.ArchiveInfo.Size]) );
    tmpBytesProcessed := FBytesProcessed;
    // Extract file data
    try
      if ffEmptyFolder in fileEntry.FileFlag then
        Exit;
        CheckStream;
        Inc( FCurrentFileIdx );
      CheckEOF;
    except
      // if an exception occurred
      on E : Exception do
        begin
          // Skip the Abort exception
          if E is EAbort then
            raise;
          // if we can continue, then skip this file
          if CanContinue( E ) then
            begin
              FBytesProcessed := tmpBytesProcessed;
              SkipFile( fileEntry.Offset );
            end
          else
            // if the user was not informed of the error, then raise it
            if (ErrorAction <> eaAsk) and not Assigned(FOnError) then
              raise
            else // abort action because user already knows the error
              Abort;
        end;
    end; // end of try...except
  end;

begin
  FOperation := opCheck;
  Start;
  try
    CheckOpen;
    FBytesToProcess := FCompressedArchiveSize;
    NeedFirstSegment;
    // Skip the header
    ReadHeader;
    // For each segment
    repeat
      // For each file stored in the archive
      while not EOF(FStream) do
        begin
          ReadFileEntry( FCurrentFileEntry );
          // extract file data to destination
          CheckFile( FCurrentFileEntry );
        end; // end of while
    until (afFinalSegment in FHeader.ArchiveFlag) or (oOpenSingleSegment in Options);
  finally
    Finish;
  end;
end;

//////////////////////////////////////////////////
//  Public section

constructor TCustomExtractor.Create( AOwner : TComponent );
begin
  inherited;
  FRestoreAction := raAsk;
end;

procedure TCustomExtractor.EnumerateFiles;
begin
  if not CheckBusy then
    DoEnumerateFiles;
end;

procedure TCustomExtractor.ExtractFile( aSegment : Word; anOffset, compressedSize : Integer );
begin
  if not CheckBusy then
    DoExtractFile( aSegment, anOffset, compressedSize );
end;

procedure TCustomExtractor.ExtractFileTo( aSegment : Word; anOffset, compressedSize : Integer; const DestFileName : String );
begin
  if not CheckBusy then
    DoExtractFileTo( aSegment, anOffset, compressedSize, DestFileName );
end;

procedure TCustomExtractor.ExtractFiles;
begin
  if not CheckBusy then
    DoExtractFiles;
end;

procedure TCustomExtractor.CheckIntegrity;
begin
  if not CheckBusy then
    DoCheckIntegrity;
end;

function  TCustomExtractor.GetMaxSegmentSize : Integer;
begin
  Result := FMaxSegmentSize;
end;


end.

