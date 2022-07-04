unit ArchiverRoot;
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
  Classes,
  ArchiverMisc;

const
  kVersion = 1;
  kMaxCryptBuffer = 8;
  kMinKeySize = 10;
  kDefaultExt = '.mmm';

type
  TUserData = packed record
    UserName         : String[20];
    Company          : String[20];
    SerialNumber     : String[20];
    BackupName       : String[20];
    Date             : TDateTime;
    ProductId        : Integer;
    ProductVersion   : Integer;
    Free             : array [0..31] of Byte; // Free space for you
  end;
  PUserData = ^TUserData;

  TArchiveSize = Extended;
  TFileSize = Integer;

  TDataInfo = packed record
    FileCount      : Integer;
    Size           : TArchiveSize;
    CompressedSize : TArchiveSize;
  end;

  TArchiveFlag = set of (afCrypted, afCompressed, afSolid, afReadOnly, afFinalSegment);

  TArchiveHeader = packed record
    Signature             : Integer;
    Version               : Integer;
    RandomID              : Integer; // Used to check the segments
    BlockSize             : Integer;
    EndOffset             : Integer;
    Segment               : Word;
    ArchiveFlag           : TArchiveFlag;
    ArchiveInfo           : TDataInfo; // Infos about the whole archive
    SegmentInfo           : TDataInfo; // Infos about the current segment only
    UserData              : TUserData;
    Reserved              : array [0..63] of Byte;
    Comment               : String;
  end;

  TFileInfo = packed record
    Size            : TFileSize;
    CompressedSize  : TFileSize;
  end;

  TFileFlag = set of (ffFile, ffEmptyFolder, ffFinalSegment, ffCrypted, ffPatch);

  TFileEntry = packed record
    Name             : String;
    Date             : TDateTime;
    Attr             : Integer;
    Segment          : Word;       // First segment containing this file
    Offset           : TFileSize;    // Offset to the file in the current segment
    FileOffset       : TFileSize;    // Offset in the source file beeing archived
    FileFlag         : TFileFlag;
    ArchiveInfo      : TFileInfo;  // Informations for the whole file stored in the archive
    SegmentInfo      : TFileInfo;  // Informations on the part of this file stored in the current segment
  end;
  PFileEntry = ^TFileEntry;

  TFileObject = class
  public
    FileEntry : TFileEntry;
    DirectoryIndex : Integer;    // Used only by WinArchiver
    ImageIndex : Integer;        // Used only by WinArchiver
    StateIndex : Integer;        // Used only by WinArchiver
    Tag : Integer;               // Free for your own use
  end;

  TErrorAction = (eaContinue, eaAbort, eaAsk);
  TOperation = (opNone, opAdd, opExtract, opEnumerate, opDelete, opMakeSFX, opCheck);
  TInternalOperationEnum = (ioCompressingStream, ioUncompressingStream,
                            ioSkippingStream, ioSwappingSegment,
                            ioOpening, ioClosing, ioOpenSolid, ioCloseSolid,
                            ioEnumAfterOpen);
  TInternalOperation = set of TInternalOperationEnum;
  TOption = (oStoreEmptyFolders, oShowEmptyFolders, oCreateReadOnly,
             oCreateSolidArchives, oCompress, oCrypt, oEraseFirstDisk,
             oEraseNewDisk, oConfirmFileDeletion, oEnumerateAfterOpen,
             oIncludeStartingDirectory, oRecurseFolders, oOpenSingleSegment,
             oRestorePath, oSecureAccess, oWriteSFXCode, oEncryptFiles,
             oMaintainFileDirectory, oNoSpanning, oShowBusyMessage,
             oDoNotUpdateFirstSegment);

  TOptions = set of TOption;
  TLanguage = (lgAutomatic, lgEnglish, lgFrench, lgChinese, lgChineseGB,
               lgPortuguese,
               lgGerman, lgItalian, lgRussian, lgSpanish, lgDanish, lgDutch,
               lgCzech);

  TMySelectDirOpt = (sdAllowCreate, sdPerformCreate, sdPrompt);
  TMySelectDirOpts = set of TMySelectDirOpt;

  TOnFileProgressEvent      = procedure ( Sender : TObject; Percent : Integer ) of Object;
  TOnErrorEvent             = procedure ( Sender : TObject; E : Exception; const FileEntry : TFileEntry;
                                          var ErrorAction : TErrorAction ) of Object;
  TOnAcceptArchiveEvent     = procedure ( Sender : TObject; const Header : TArchiveHeader; var Accept : Boolean ) of Object;
  TOnWriteUserDataEvent     = procedure ( Sender : TObject; var UserData : TUserData ) of Object;
  TOnEnterCryptKeyEvent     = procedure ( Sender : TObject; var Key : String ) of Object;
  TOnRequestCryptKeyEvent   = procedure ( Sender : TObject; var Key : String ) of Object;
  TOnGetSignatureEvent      = procedure ( Sender : TObject; var Signature : Integer ) of Object;
  TOnShowCommentEvent       = procedure ( Sender : TObject; const Comment : String ) of Object;
  TOnShowTimingEvent        = procedure ( Sender : TObject; ElapsedTime, RemainingTime : TDateTime ) of Object;
  TOnDisplayMessageEvent    = procedure ( Sender : TObject; const msg : String ) of Object;
  TOnAddToLogEvent          = procedure ( Sender : TObject; const msg : String ) of Object;

  EArchiver = class( Exception );
  EArchiverBusy = class( EArchiver );

  TMessages = class(TPersistent)
    protected
      FLanguage : TLanguage;
      FBadSignature : String;
      FFileNameNeeded : String;
      FSystemMessage : String;
      FAcceptArchiveFailed : String;
      FEnterCryptKey : String;
      FEnterDecryptKey : String;
      FKeyTooShort : String;
      FConfirmCryptKey : String;
      FKeyNotConfirmed : String;
      FArchiveIsReadOnly : String;
      FCanNotCreateArchive : String;
      FCannotCreateDir : String;
      FSelectADirectory : String;
      FOk : String;
      FCancel : String;
      FInformation : String;
      FWarning : String;
      FConfirmation : String;
      FError : String;
      FCanNotBuildTempFileName : String;
      FYes : String;
      FYesToAll : String;
      FNo : String;
      FFile : String;
      FCanContinue : String;
      FUnknownVersion : String;
      FCannotCreateFile : String;
      FCannotOpenFile : String;
      FArchiverBusy : String;
      FCantUseSolidArchiveWithExternalStream: String;

      procedure AssignTo(Dest: TPersistent); override;
      procedure PropSetLanguage( language : TLanguage );
      procedure SetLanguage( language : TLanguage ); virtual;
      procedure SetGlobalStrings; virtual;

    public
      constructor Create;

      property Language : TLanguage read FLanguage write PropSetLanguage;

    published
      property BadSignature : String read FBadSignature write FBadSignature;
      property FileNameNeeded : String read FFileNameNeeded write FFileNameNeeded;
      property SystemMessage : String read FSystemMessage write FSystemMessage;
      property AcceptArchiveFailed : String read FAcceptArchiveFailed write FAcceptArchiveFailed;
      property EnterCryptKey : String read FEnterCryptKey write FEnterCryptKey;
      property EnterDecryptKey : String read FEnterDecryptKey write FEnterDecryptKey;
      property KeyTooShort : String read FKeyTooShort write FKeyTooShort;
      property ConfirmCryptKey : String read FConfirmCryptKey write FConfirmCryptKey;
      property KeyNotConfirmed : String read FKeyNotConfirmed write FKeyNotConfirmed;
      property ArchiveIsReadOnly : String read FArchiveIsReadOnly write FArchiveIsReadOnly;
      property CanNotCreateArchive : String read FCanNotCreateArchive write FCanNotCreateArchive;
      property CannotCreateDir : String read FCannotCreateDir write FCannotCreateDir;
      property SelectADirectory : String read FSelectADirectory write FSelectADirectory;
      property Ok : String read FOk write FOk;
      property Cancel : String read FCancel write FCancel;
      property Information : String read FInformation write FInformation;
      property Warning : String read FWarning write FWarning;
      property Confirmation : String read FConfirmation write FConfirmation;
      property Error : String read FError write FError;
      property CanNotBuildTempFileName : String read FCanNotBuildTempFileName write FCanNotBuildTempFileName;
      property Yes : String read FYes write FYes;
      property YesToAll : String read FYesToAll write FYesToAll;
      property No : String read FNo write FNo;
      property AFile : String read FFile write FFile;
      property CanContinue : String read FCanContinue write FCanContinue;
      property UnknownVersion : String read FUnknownVersion write FUnknownVersion;
      property CannotCreateFile : String read FCannotCreateFile write FCannotCreateFile;
      property CannotOpenFile : String read FCannotOpenFile write FCannotOpenFile;
      property ArchiverBusy : String read FArchiverBusy write FArchiverBusy;
      property CantUseSolidArchiveWithExternalStream : String read FCantUseSolidArchiveWithExternalStream
               write FCantUseSolidArchiveWithExternalStream;
    end;

  TArchiverRoot = class( TComponent )
  protected
    FHeader : TArchiveHeader;
    FFileName : String;
    FStream : TStream;
    FFilter : String;
    FBytesToProcess : TArchiveSize; // Used for calculating a progress ratio
    FBytesProcessed : TArchiveSize; // Used for calculating a progress ratio
    FPercent : Integer; // progress ratio
    FStartCount : Integer;     // Number of call of the Start/Finish methods
    FBlockSize : Integer;
    FSrcBlock : PChar;
    FDestBlock : PChar;
    FErrorAction : TErrorAction;
    FCurrentFileEntry : TFileEntry;
    FCurrentFileIdx : Integer;
    FOperation : TOperation;
    FMessages : TMessages;
    FMaxSegmentSize : TFileSize;
    FCheckAvailableSpace : Boolean;
    FInternalOperation : TInternalOperation;
    FArchiveDrive : String;
    FArchiveName : String;
    FArchiveDir : String;
    FArchiveExt : String;
    FCompressedArchiveSize : TArchiveSize; // Used for progress
    FIsOpen : Boolean;
    FCryptKey : String;
    FReadOnly : Boolean;
    FStartOffset : TFileSize;
    FOptions : TOptions;
    FOldFileName : String;
    FOldOptions : TOptions;
    FOldMaxSegmentSize : TFileSize;
    FSegmentNeeded : Integer;
    FSFXCodeSize : TFileSize;
    FIsSolidArchive : Boolean;
    FTmpFileDate : TDateTime;
    FAlwaysContinue : Boolean;
    FFiles : TList;
    FArchiveChanged : Boolean;
    FMustAbort : Boolean;
    FExternalStream : Boolean;
    // Timing informations
    FStartTime : TDateTime;
    FEndTime : TDateTime;
    FBytesPerMSec : Extended;
    FLastTicks : Integer;
    FTotTicks : Integer;

    // Events
    FOnFileProgress : TOnFileProgressEvent;
    FOnStartOperation : TNotifyEvent;
    FOnFinishOperation : TNotifyEvent;
    FOnError : TOnErrorEvent;
    FOnAcceptArchive : TOnAcceptArchiveEvent;
    FOnWriteUserData : TOnWriteUserDataEvent;
    FOnEnterCryptKey : TOnEnterCryptKeyEvent;
    FOnRequestCryptKey : TOnRequestCryptKeyEvent;
    FOnBeforeOpen : TNotifyEvent;
    FOnAfterOpen : TNotifyEvent;
    FOnBeforeClose : TNotifyEvent;
    FOnAfterClose : TNotifyEvent;
    FOnGetSignature : TOnGetSignatureEvent;
    FOnAfterHeaderUpdate : TNotifyEvent;
    FOnShowComment : TOnShowCommentEvent;
    FOnShowTiming : TOnShowTimingEvent;
    FOnDisplayMessage : TOnDisplayMessageEvent;
    FOnClearFileList : TNotifyEvent;
    FOnAddToLog : TOnAddToLogEvent;

    procedure AssignTo(Dest: TPersistent); override;
    function  CreateMessages : TMessages; virtual;
    function  GetSignature : Integer; virtual;
    function  GetHeaderSize : Integer; virtual;
    procedure WriteHeader; virtual;
    procedure ReadHeader;
    function  ReadHeaderOfFile( const fileName : String; var AHeader : TArchiveHeader ) : Boolean;
    function  ReadHeaderOfStream( S : TStream; var AHeader : TArchiveHeader ) : Boolean; virtual;
    procedure CheckOpen;
    function  GetDirectorySize( const dir, filter : String ) : Integer;
    procedure Start; virtual;
    procedure Finish; virtual;
    procedure UpdateProgress;
    procedure InitCompression; virtual;
    procedure InitCrypting; virtual;
    procedure EnterCryptKey; virtual;
    procedure RequestCryptKey; virtual;
    function  GetMinKeySize : Integer; virtual;
    procedure SetBlockSize( val : Integer );
    procedure AllocBlocks;
    procedure DeallocBlocks;
    function  CanContinue( E : Exception ) : Boolean;
    function  NeededBlockSize : Integer; virtual;
    function  GetLanguage : TLanguage;
    procedure SetLanguage( val : TLanguage );
    procedure SetMessages( val : TMessages );
    procedure SetFileName( const val : String );
    procedure ExplodeFileName;
    function  GetFileEntrySize( const fileEntry : TFileEntry ) : Integer;
    procedure WriteFileEntry( var fileEntry : TFileEntry );
    procedure ReadFileEntry( var fileEntry : TFileEntry );
    function  IsRemovableDisk( const drive : String ) : Boolean;
    function  GetSegmentName( segment : Word ) : String; virtual;
    function  NewStreamObject( const FileName : String; mode : Word ) : TStream;
    procedure CreateStream;
    procedure OpenStream;
    procedure CloseStream;
    procedure CheckReadOnly;
    procedure CheckKey;
    procedure BeforeOpen; virtual;
    procedure AfterOpen; virtual;
    procedure BeforeClose; virtual;
    procedure AfterClose; virtual;
    procedure AfterUpdate; virtual;
    function  GetTempFileName : String;
    procedure GetProgressInformations; virtual;
    procedure CreateArchive; virtual;
    function  GetOpenMode : Integer; virtual;
    function  RequestSpace( val : Integer ) :  Boolean; virtual;
    function  CheckEOF : Boolean; virtual;
    function  SelectDirectory(var Directory: string; Options: TMySelectDirOpts; HelpCtx: Longint):Boolean; virtual;
    procedure ForceDirectories(Dir: string);
    function  SelectFile( const Title : String; var FileName : String ) : Boolean; virtual;
    function  MessageDlg( const Msg: string; DlgType: TMyMsgDlgType;
                          Buttons: TMyMsgDlgButtons; HelpCtx: Longint): Integer; virtual;
    function  InputQuery(const ACaption, APrompt: string; var AValue: string): Boolean; virtual;
    function  QueryPassword(const ACaption, APrompt: string; var AValue: string): Boolean; virtual;
    function  GetStartOffset : Integer;
    procedure CheckSFX( const aFileName : String ); virtual;
    procedure Loaded; override;
    procedure OpenSolidData; virtual;
    procedure CloseSolidData; virtual;
    procedure StartTimer;
    procedure StopTimer;
    procedure ShowTiming;
    function  GetElapsedTime : TDateTime;
    procedure DisplayMessage( const msg : String );
    procedure AddToLog( const msg : String );
    procedure CopyStream( Src, Dest : TStream; trapExceptions : Boolean );
    function  CopyFile( const srcName, destName : String; failIfExists, trapExceptions : Boolean ) : Boolean;
    procedure ClearFiles;
    procedure AddFileToList( const entry : TFileEntry );
    function  GetFiles( idx : Integer ) : TFileObject;
    function  GetFileCount : Integer;
    procedure AdjustArchiveSize;
    function CheckBusy : Boolean;
    procedure SetStream( val : TStream );

  public
    // Creators & Destructor
    constructor Create( AOwner : TComponent ); override;
    destructor  Destroy; override;

    // Public methods

    // Open/close Archive
    procedure Open;
    procedure OpenNew;
    procedure CreateTempFile;
    procedure Close;
    function  Reset : Boolean;
    function  Delete : Boolean;
    function  Rename( const NewName : String ) : Boolean;

    // Misc methods
    function  IsStreamOpen : Boolean;
    function  DeleteDirectory( const dir : String ) : Boolean;
    function  DeleteDriveContent( const drive : String ) : Boolean;
    function  IsSegmented : Boolean;
    function  IsEmpty : Boolean;
    function  IsBusy : Boolean;
    function  IndexOfFile( const FileName : String ) : Integer;
    function  CanAbort : Boolean;
    procedure RequestAbort;

    // Public properties
    property ArchiveDrive : String read FArchiveDrive;
    property ArchiveName : String read FArchiveName;
    property ArchiveDir : String read FArchiveDir;
    property ArchiveExt : String read FArchiveExt;
    property CheckAvailableSpace : Boolean read FCheckAvailableSpace write FCheckAvailableSpace;
    property CurrentFileEntry : TFileEntry read FCurrentFileEntry;
    property Operation : TOperation read FOperation;
    property Stream : TStream read FStream write SetStream;
    property Header : TArchiveHeader read FHeader;
    property IsOpen : Boolean read FIsOpen write FIsOpen;
    property ReadOnly : Boolean read FReadOnly;
    property StartOffset : TFileSize read FStartOffset write FStartOffset;
    property SFXCodeSize : TFileSize read FSFXCodeSize write FSFXCodeSize;
    property IsSolidArchive : Boolean read FIsSolidArchive;
    property ElapsedTime : TDateTime read GetElapsedTime;
    property StartTime : TDateTime read FStartTime;
    property EndTime : TDateTime read FEndTime;
    property BytesPerMSec : Extended read FBytesPerMSec;
    property BytesToProcess : TArchiveSize read FBytesToProcess;
    property BytesProcessed : TArchiveSize read FBytesProcessed;
    property Percent : Integer read FPercent;
    property FileCount : Integer read GetFileCount;
    property Files[ idx : Integer ] : TFileObject read GetFiles;
    property ArchiveChanged : Boolean read FArchiveChanged;
    property MinKeySize : Integer read GetMinKeySize;
    property ExternalStream : Boolean read FExternalStream;

  published
    // Properties
    property BlockSize : Integer read FBlockSize write SetBlockSize;
    property ErrorAction : TErrorAction read FErrorAction write FErrorAction;
    property FileName : String read FFileName write SetFileName;
    property Filter : String read FFilter write FFilter;
    property Language : TLanguage read GetLanguage write SetLanguage;
    property Messages : TMessages read FMessages write SetMessages;
    property Options : TOptions read FOptions write FOptions;
     // Events
    property OnAcceptArchive : TOnAcceptArchiveEvent read FOnAcceptArchive write FOnAcceptArchive;
    property OnWriteUserData : TOnWriteUserDataEvent read FOnWriteUserData write FOnWriteUserData;
    property OnError : TOnErrorEvent read FOnError write FOnError;
    property OnFileProgress : TOnFileProgressEvent read FOnFileProgress write FOnFileProgress;
    property OnFinishOperation : TNotifyEvent read FOnFinishOperation write FOnFinishOperation;
    property OnStartOperation : TNotifyEvent read FOnStartOperation write FOnStartOperation;
    property OnEnterCryptKey : TOnEnterCryptKeyEvent read FOnEnterCryptKey write FOnEnterCryptKey;
    property OnRequestCryptKey : TOnRequestCryptKeyEvent read FOnRequestCryptKey write FOnRequestCryptKey;
    property OnBeforeOpen : TNotifyEvent read FOnBeforeOpen write FOnBeforeOpen;
    property OnAfterOpen : TNotifyEvent read FOnAfterOpen write FOnAfterOpen;
    property OnBeforeClose : TNotifyEvent read FOnBeforeClose write FOnBeforeClose;
    property OnAfterClose : TNotifyEvent read FOnAfterClose write FOnAfterClose;
    property OnGetSignature : TOnGetSignatureEvent read FOnGetSignature write FOnGetSignature;
    property OnAfterHeaderUpdate : TNotifyEvent read FOnAfterHeaderUpdate write FOnAfterHeaderUpdate;
    property OnShowComment : TOnShowCommentEvent read FOnShowComment write FOnShowComment;
    property OnShowTiming : TOnShowTimingEvent read FOnShowTiming write FOnShowTiming;
    property OnDisplayMessage : TOnDisplayMessageEvent read FOnDisplayMessage write FOnDisplayMessage;
    property OnClearFileList : TNotifyEvent read FOnClearFileList write FOnClearFileList;
    property OnAddToLog : TOnAddToLogEvent read FOnAddToLog write FOnAddToLog;
  end;

  function  GetUserLanguage : TLanguage;

implementation

function  GetUserLanguage : TLanguage;
var
  xLanguage : Integer;
begin
  xLanguage := (LoWord(GetUserDefaultLangID) and $3ff);
  case xLanguage of
    LANG_GERMAN    : Result := lgGerman;
    LANG_ENGLISH   : Result := lgEnglish;
    LANG_SPANISH   : Result := lgSpanish;
    LANG_RUSSIAN   : Result := lgRussian;
    LANG_ITALIAN   : Result := lgItalian;
    LANG_FRENCH    : Result := lgFrench;
    LANG_PORTUGUESE: Result := lgPortuguese;
    LANG_CHINESE   : Result := lgChinese;
//    LANG_CHINESE   : Result := lgChineseGB;
    LANG_DANISH    : Result := lgDanish;
    LANG_DUTCH     : Result := lgDutch;
    LANG_CZECH     : Result := lgCZech;
  else
                     Result := lgEnglish;
  end;
end;

////////////////////////////////////////////////////////////

procedure TMessages.AssignTo(Dest: TPersistent);
begin
  if Dest is TMessages then
    with TMessages( Dest ) do begin
      FBadSignature        := Self.FBadSignature;
      FFileNameNeeded      := Self.FFileNameNeeded;
      FSystemMessage       := Self.FSystemMessage;
      FAcceptArchiveFailed := Self.FAcceptArchiveFailed;
      FEnterCryptKey       := Self.FEnterCryptKey;
      FEnterDecryptKey     := Self.FEnterDecryptKey;
      FArchiveIsReadOnly   := Self.FArchiveIsReadOnly;
      FKeyTooShort         := Self.FKeyTooShort;
      FConfirmCryptKey     := Self.FConfirmCryptKey;
      FKeyNotConfirmed     := Self.FKeyNotConfirmed;
      FCanNotCreateArchive := Self.FCanNotCreateArchive;
      FCannotCreateDir     := Self.FCannotCreateDir;
      FSelectADirectory    := Self.FSelectADirectory;
      FOk                  := Self.FOK;
      FCancel              := Self.FCancel;
      FInformation         := Self.FInformation;
      FWarning             := Self.FWarning;
      FConfirmation        := Self.FConfirmation;
      FError               := Self.FError;
      FCanNotBuildTempFileName := Self.FCanNotBuildTempFileName;
      FYes                     := Self.FYes;
      FYesToAll                := Self.FYesToAll;
      FNo                      := Self.FNo;
      FFile                    := Self.FFile;
      FCanContinue             := Self.FCanContinue;
      FUnknownVersion          := Self.FUnknownVersion;
      FCannotCreateFile        := Self.FCannotCreateFile;
      FCannotOpenFile          := Self.FCannotOpenFile;
      FArchiverBusy            := Self.FArchiverBusy;
      FCantUseSolidArchiveWithExternalStream := Self.FCantUseSolidArchiveWithExternalStream;
    end;
  SetGlobalStrings;
  inherited AssignTo( Dest );
end;

constructor TMessages.Create;
begin
  inherited;
  Language := lgAutomatic;
end;

procedure TMessages.PropSetLanguage( language : TLanguage );
begin
  SetLanguage( language );
  SetGlobalStrings;
end;

procedure TMessages.SetLanguage( language : TLanguage );
var
  lang : TLanguage;
begin
  FLanguage := language;
  if FLanguage = lgAutomatic then
    lang := GetUserLanguage
  else
    lang := FLanguage;
  case lang of
    lgEnglish:
      begin
        FBadSignature := 'Bad signature: file "%s" is not a valid archive';
        FFileNameNeeded := '"Archiver" needs property "FileName"';
        FSystemMessage := 'System Message';
        FAcceptArchiveFailed := 'This archive can not be used';
        FEnterCryptKey := 'Enter the encrypt key:';
        FEnterDecryptKey := 'Enter the decrypt key:';
        FArchiveIsReadOnly := 'Archive is ReadOnly';
        FKeyTooShort := 'The key is too short. It must contain %d chars at least.';
        FConfirmCryptKey := 'Confirm the encrypt key:';
        FKeyNotConfirmed := 'The key could not be confirmed. Enter it again.';
        FCanNotCreateArchive := 'This component can not create an archive';
        FCannotCreateDir := 'Can not create directory';
        FSelectADirectory := 'Select a directory:';
        FOk := 'OK';
        FCancel := 'Cancel';
        FInformation := 'Information';
        FWarning := 'Warning';
        FConfirmation := 'Confirmation';
        FError := 'Error';
        FCanNotBuildTempFileName := 'Can not build a temporary FileName';
        FYes := '&Yes';
        FYesToAll := 'Yes to &All';
        FNo := '&No';
        FFile := 'File';
        FCanContinue := 'Do you want to continue ?';
        FUnknownVersion := 'Unknown version';
        FCannotCreateFile := 'Can not create %s';
        FCannotOpenFile := 'Can not open %s';
        FArchiverBusy := 'Component %s is busy. Abort current operation and try again';
        FCantUseSolidArchiveWithExternalStream := 'Can''t use a solid Archive with an external stream';
      end;
    lgFrench:
      begin
        FBadSignature := 'Mauvaise signature: le fichier "%s" n''est pas une archive valide';
        FFileNameNeeded := '"Archiver" nécessite la propriété "FileName"';
        FSystemMessage := 'Message système';
        FAcceptArchiveFailed := 'Cette archive ne peut être utilisée';
        FEnterCryptKey := 'Saisissez la clé de cryptage:';
        FEnterDecryptKey := 'Saisissez la clé de décryptage:';
        FArchiveIsReadOnly := 'L''Archive est en lecture seule';
        FKeyTooShort := 'La clé est trop courte. Elle doit contenir au moins %d caractères.';
        FConfirmCryptKey := 'Confirmez la clé de cryptage:';
        FKeyNotConfirmed := 'La clé n''a pas pu être confirmée. Resaisissez-la.';
        FCanNotCreateArchive := 'Ce composant ne peut pas créer d''archives';
        FCannotCreateDir := 'Impossible de créer le répertoire';
        FSelectADirectory := 'Sélectionnez un répertoire:';
        FOk := 'OK';
        FCancel := 'Annuler';
        FInformation := 'Information';
        FWarning := 'Avertissement';
        FConfirmation := 'Confirmation';
        FError := 'Erreur';
        FCanNotBuildTempFileName := 'Impossible de construire un nom de fichier temporaire';
        FYes := '&Oui';
        FYesToAll := 'Oui à &Tout';
        FNo := '&Non';
        FFile := 'Fichier';
        FCanContinue := 'Voulez-vous continuer ?';
        FUnknownVersion := 'Version inconnue';
        FCannotCreateFile := 'Impossible de créer %s';
        FCannotOpenFile := 'Impossible d''ouvrir %s';
        FArchiverBusy := 'Le composant %s est occupé. Interrompez l''opération en cours et recommencez';
        FCantUseSolidArchiveWithExternalStream := 'Impossible d''utliser une archive solide avec un stream externe';
      end;
    lgChinese:
      begin
        // Traditional Chinese constants (in BIG-5 code).
        FBadSignature := 'µLªkÃÑ§O: ÀÉ®× "%s" ¤£¬O¤@­Ó¦³®ÄªºÀ£ÁYÀÉ¡C';
        FSystemMessage := '¨t²Î°T®§';
        FFileNameNeeded := '"Archiver" »Ý­n³]©w "FileName" ÄÝ©Ê¡C';
        FAcceptArchiveFailed := '¦¹À£ÁYÀÉµLªk¨Ï¥Î¡C';
        FEnterCryptKey := '¿é¤J¥[±KÁä­È:';
        FEnterDecryptKey := '¿é¤J¸Ñ±KÁä­È:';
        FArchiveIsReadOnly := 'À£ÁYÀÉ¬°°ßÅª¡C';
        FKeyTooShort := 'Áä­Èªºªø«×¦Ü¤Ö­n¦³ %d ­Ó¦r¤¸¡C';
        FConfirmCryptKey := '½T»{¥[±KÁä­È:';
        FKeyNotConfirmed := '¿é¤JªºÁä­È©M½T»{ªºÁä­È¤£¦P, ½Ð­«·s¿é¤J¡C';
        FCanNotCreateArchive := 'µLªk«Ø¥ßÀ£ÁYÀÉ';
        FCannotCreateDir := 'µLªk«Ø¥ß¸ê®Æ§¨';
        FSelectADirectory := '¿ï¾Ü¸ê®Æ§¨:';
        FOk := '½T©w';
        FCancel := '¨ú®ø';
        FInformation := '°T®§';
        FWarning := 'Äµ§i';
        FConfirmation := '½T»{';
        FError := '¿ù»~';
        FCanNotBuildTempFileName := 'µLªk«Ø¥ß¼È¦sÀÉ¦W';
        FYes := '¬O(&Y)';
        FYesToAll := '¥þ³¡¬Ò¬O(&A)';
        FNo := '§_(&N)';
        FFile := 'ÀÉ®×';
        FCanContinue := '±z¬O§_­nÄ~Äò?';
        FUnknownVersion := 'µLªk±oª¾ª©¥»¸¹½X';
        FCannotCreateFile := 'Can not create %s';
        FCannotOpenFile := 'Can not open %s';
        FArchiverBusy := 'Component %s is busy. Abort current operation and try again';
        FCantUseSolidArchiveWithExternalStream := 'Can''t use a solid Archive with an external stream';
      end;
    lgChineseGB:
      begin
        // Traditional Chinese constants (in BIG-5 code).
        FBadSignature := 'ÎÞ·¨Ê¶±ð: µµ°¸ "%s" ²»ÊÇÒ»¸öÓÐÐ§µÄÑ¹Ëõµµ¡£';
        FSystemMessage := 'ÏµÍ³Ñ¶Ï¢';
        FFileNameNeeded := '"Archiver" ÐèÒªÉè¶¨ "FileName" ÊôÐÔ¡£';
        FAcceptArchiveFailed := '´ËÑ¹ËõµµÎÞ·¨Ê¹ÓÃ¡£';
        FEnterCryptKey := 'ÊäÈë¼ÓÃÜ¼üÖµ:';
        FEnterDecryptKey := 'ÊäÈë½âÃÜ¼üÖµ:';
        FArchiveIsReadOnly := 'Ñ¹ËõµµÎªÎ¨¶Á¡£';
        FKeyTooShort := '¼üÖµµÄ³¤¶ÈÖÁÉÙÒªÓÐ %d ¸ö×ÖÔª¡£';
        FConfirmCryptKey := 'È·ÈÏ¼ÓÃÜ¼üÖµ:';
        FKeyNotConfirmed := 'ÊäÈëµÄ¼üÖµºÍÈ·ÈÏµÄ¼üÖµ²»Í¬, ÇëÖØÐÂÊäÈë¡£';
        FCanNotCreateArchive := 'ÎÞ·¨½¨Á¢Ñ¹Ëõµµ';
        FCannotCreateDir := 'ÎÞ·¨½¨Á¢×ÊÁÏ¼Ð';
        FSelectADirectory := 'Ñ¡Ôñ×ÊÁÏ¼Ð:';
        FOk := 'È·¶¨';
        FCancel := 'È¡Ïû';
        FInformation := 'Ñ¶Ï¢';
        FWarning := '¾¯¸æ';
        FConfirmation := 'È·ÈÏ';
        FError := '´íÎó';
        FCanNotBuildTempFileName := 'ÎÞ·¨½¨Á¢ÔÝ´æµµÃû';
        FYes := 'ÊÇ(&Y)';
        FYesToAll := 'È«²¿½ÔÊÇ(&A)';
        FNo := '·ñ(&N)';
        FFile := 'µµ°¸';
        FCanContinue := 'ÄúÊÇ·ñÒª¼ÌÐø?';
        FUnknownVersion := 'ÎÞ·¨µÃÖª°æ±¾ºÅÂë';
        FCannotCreateFile := 'ÎÞ·¨´´½¨ %s';
        FCannotOpenFile := 'ÎÞ·¨´ò¿ª %s';
        FArchiverBusy := '×é¼þ %s ÕýÃ¦. È¡Ïûµ±Ç°²Ù×÷ÔÙÖØÊÔ';
        FCantUseSolidArchiveWithExternalStream := 'Can''t use a solid Archive with an external stream';
      end;
    lgPortuguese:
      begin
        // If it looks strange do not change. It's designed for page 850
        FBadSignature := 'Assinatura incorreta: arquivo "%s" não é válido.';
        FFileNameNeeded := '"Archiver" necessita a propriedade "FileName"';
        FSystemMessage := 'Mensagem do Sistema';
        FAcceptArchiveFailed := 'Este arquivo está inutilizado!';
        FEnterCryptKey := 'Digite a chave de encriptação:';
        FEnterDecryptKey := 'Digite a chave de desencriptação:';
        FArchiveIsReadOnly := 'Arquivo com atributo Somente Leitura';
        FKeyTooShort := 'Senha muito curta. Utilizar, pelo menos, %d caracteres.';
        FConfirmCryptKey := 'Confirme a senha:';
        FKeyNotConfirmed := 'Senha não pode ser confirmada. Digite novamente.';
        FCanNotCreateArchive := 'Este componente não pode criar o arquivo';
        FCannotCreateDir := 'Impossível criar diretorio';
        FSelectADirectory := 'Selecione um diretorio:';
        FOk := 'OK';
        FCancel := 'Cancelar';
        FInformation := 'Informação';
        FWarning := 'Alerta';
        FConfirmation := 'Confirmação';
        FError := 'Erro';
        FCanNotBuildTempFileName := 'Impossivel criar "FileName" temporário';
        FYes := '&Sim';
        FYesToAll := 'Sim para &Todos';
        FNo := '&Não';
        FFile := 'Arquivo';
        FCanContinue := 'Deseja continuar ?';
        FUnknownVersion := 'Versão desconhecida';
        FCannotCreateFile := 'Can not create %s';
        FCannotOpenFile := 'Can not open %s';
        FArchiverBusy := 'Component %s is busy. Abort current operation and try again';
        FCantUseSolidArchiveWithExternalStream := 'Can''t use a solid Archive with an external stream';
      end;
    lgGerman:
      begin
        FBadSignature := 'Falsche Signatur: Die Datei "%s" ist kein Archiv';
        FFileNameNeeded := '"Archiver" benötigt die Eigenschaft "FileName"';
        FSystemMessage := 'System Nachricht';
        FAcceptArchiveFailed := 'Dieses Archiv kann nicht gelesen werden';
        FEnterCryptKey := 'Bitte geben sie das Passwort zum verschlüsseln ein:';
        FEnterDecryptKey := 'Bitte geben Sie das Passwort zum entschlüsseln ein:';
        FArchiveIsReadOnly := 'Das Archiv ist Schreibgeschützt';
        FKeyTooShort := 'Das Passwort ist zu kurz. Es muß aus mindestens %d Zeichen bestehen.';
        FConfirmCryptKey := 'Wiederholung des Passwortes:';
        FKeyNotConfirmed := 'Das Passwort konnte nicht bestätigt werden. Bitte geben Sie das Passwort erneut ein.';
        FCanNotCreateArchive := 'Dieses Komponente kann kein Archiv erstellen';
        FCannotCreateDir := 'Fehler beim Erstellen des Verzeichnisses';
        FSelectADirectory := 'Wählen Sie ein Verzeichnis:';
        FOk := 'OK';
        FCancel := 'Abbruch';
        FInformation := 'Information';
        FWarning := 'Warnung';
        FConfirmation := 'Bestätigung';
        FError := 'Fehler';
        FCanNotBuildTempFileName := 'Die temporäre Datei kann nicht erstellt werden.';
        FYes := '&Ja';
        FYesToAll := '&Immer Ja';
        FNo := '&Nein';
        FFile := 'Datei';
        FCanContinue := 'Möchten Sie fortfahren?';
        FUnknownVersion := 'Unbekannte Version';
        FCannotCreateFile := 'Can not create %s';
        FCannotOpenFile := 'Can not open %s';
        FArchiverBusy := 'Component %s is busy. Abort current operation and try again';
        FCantUseSolidArchiveWithExternalStream := 'Can''t use a solid Archive with an external stream';
      end;
    lgItalian: // Thanks to Gabriele Bigliardi (gbigliardi@manord.com)
               // Modified by Mauro Favagrossa (mrfava@tin.it)
      begin
        FBadSignature := 'Bad signature: file "%s" non è un archivio valido';
        FFileNameNeeded := '"Archiver" deve avere la proprietà "FileName"';
        FSystemMessage := 'Messaggio di sistema';
        FAcceptArchiveFailed := 'Questo archivio non può essere usato';
        FEnterCryptKey := 'Digitare la encrypt key:';
        FEnterDecryptKey := 'Digitare la decrypt key:';
        FArchiveIsReadOnly := 'Archivio in sola lettura';
        FKeyTooShort := 'La chiave è troppo corta. Deve contenere almeno %d caratteri';
        FConfirmCryptKey := 'Confermare la encrypt key:';
        FKeyNotConfirmed := 'La chiave non può essere confermata. Ridigitarla.';
        FCanNotCreateArchive := 'Questo componente non può creare un archivio';
        FCannotCreateDir := 'Non si riesce a creare una directory';
        FSelectADirectory := 'Selezionare una directory:';
        FOk := 'OK';
        FCancel := 'Annulla';
        FInformation := 'Informazione';
        FWarning := 'Attenzione';
        FConfirmation := 'Conferma';
        FError := 'Errore';
        FCanNotBuildTempFileName := 'Non si riesce a creare un nome file temporaneo';
        FYes := '&Si';
        FYesToAll := 'Si a &Tutti';
        FNo := '&No';
        FFile := 'File';
        FCanContinue := 'Vuoi continuare ?';
        FUnknownVersion := 'Versione sconosciuta';
        FCannotCreateFile := 'Impossibile creare %s';
        FCannotOpenFile := 'Impossibile aprire %s';
        FArchiverBusy := 'Il componente %s è occupato. Interrompere l''operazione in corso prima di riprovare';
        FCantUseSolidArchiveWithExternalStream := 'Can''t use a solid Archive with an external stream';
      end;
    lgRussian:  {Windows 1251}
      begin
        FBadSignature := 'Íåâåðíàÿ ñèãíàòóðà: ôàéë "%s" íå ÿâëÿåòñÿ àðõèâîì';
        FFileNameNeeded := '"Archiver" òðåáóåò ñâîéñòâî "FileName"';
        FSystemMessage := 'Ñîîáùåíèå ñèñòåìû';
        FAcceptArchiveFailed := 'Äàííûé àðõèâ íå ìîæåò áûòü èñïîëüçîâàí';
        FEnterCryptKey := 'Ââåäèòå êîäèðóþùèé êëþ÷:';
        FEnterDecryptKey := 'Ââåäèòå äåêîäèðóþùèé êëþ÷:';
        FArchiveIsReadOnly := 'Àðõèâ çàêðûò äëÿ çàïèñè (òîëüêî ÷òåíèå)';
        FKeyTooShort := 'Ñëèøêîì êîðîòêèé êëþ÷. Êëþ÷ äîëæåí ñîäåðæàòü íå ìåíåå %d ñèìâîëîâ.';
        FConfirmCryptKey := 'Ïîäòâåðäèòå êîäèðóþùèé êëþ÷:';
        FKeyNotConfirmed := 'Êëþ÷ íå ïîäòâåðæäåí. Ïîâòîðèòå ñíîâà.';
        FCanNotCreateArchive := 'Ýòîò êîìïîíåíò íå ìîæåò ñîçäàòü àðõèâû';
        FCannotCreateDir := 'Íå ìîãó ñîçäàòü êàòàëîã';
        FSelectADirectory := 'Âûáåðèòå êàòàëîã:';
        FOk := 'OK';
        FCancel := 'Îòìåíèòü';
        FInformation := 'Èíôîðìàöèÿ';
        FWarning := 'Âíèìàíèå';
        FConfirmation := 'Ïîäòâåðæäåíèå';
        FError := 'Îøèáêà';
        FCanNotBuildTempFileName := 'Íå ìîãó ïîñòðîèòü èìÿ âðåìåííîãî ôàéëà';
        FYes := '&Äà';
        FYesToAll := 'Äà äëÿ âñ&åõ';
        FNo := '&Íåò';
        FFile := 'Ôàéë';
        FCanContinue := 'Õîòèòå ïðîäîëæèòü ?';
        FUnknownVersion := 'Íåèçâåñòíàÿ âåðñèÿ';
        FCannotCreateFile := 'Can not create %s';
        FCannotOpenFile := 'Can not open %s';
        FArchiverBusy := 'Component %s is busy. Abort current operation and try again';
        FCantUseSolidArchiveWithExternalStream := 'Can''t use a solid Archive with an external stream';
      end;
    lgSpanish:
      begin
        FBadSignature := 'Firma Erronea: el archivo "%s" no es valido';
        FFileNameNeeded := '"Archiver" necesita la propiedad "FileName"';
        FSystemMessage := 'Mensaje de Sistema';
        FAcceptArchiveFailed := 'El archivo no puede usarse';
        FEnterCryptKey := 'Clave de encriptamiento:';
        FEnterDecryptKey := 'Clave de desencripción:';
        FArchiveIsReadOnly := 'Archivo con atributo de solo lectura';
        FKeyTooShort := 'La clave es muy corta. Debe contener almenos %d caracteres.';
        FConfirmCryptKey := 'Confirma clave de encripción:';
        FKeyNotConfirmed := 'La clave no se confirmo. Introducela de nuevo.';
        FCanNotCreateArchive := 'Este componente no puede crear un archivo';
        FCannotCreateDir := 'No puedo crear el directorio';
        FSelectADirectory := 'Seleccione un directorio:';
        FOk := 'Aceptar';
        FCancel := 'Cancelar';
        FInformation := 'Información';
        FWarning := 'Advertencia';
        FConfirmation := 'Confirmación';
        FError := 'Error';
        FCanNotBuildTempFileName := 'No puedo construir archivo temporal';
        FYes := '&Si';
        FYesToAll := 'Si a &Todo';
        FNo := '&No';
        FFile := 'Archivo';
        FCanContinue := '¿ Desea continuar ?';
        FUnknownVersion := 'Versión desconocida';
        FCannotCreateFile := 'Can not create %s';
        FCannotOpenFile := 'Can not open %s';
        FArchiverBusy := 'Component %s is busy. Abort current operation and try again';
        FCantUseSolidArchiveWithExternalStream := 'Can''t use a solid Archive with an external stream';
      end;
   lgDanish:
      begin
        FBadSignature := 'Forkert signatur: fil "%s" det er ikke en arkiv fil';
        FFileNameNeeded := '"Arkiv" skal have "Fil Navn"';
        FSystemMessage := 'System besked';
        FAcceptArchiveFailed := 'Dette arkiv kan ikke bruges';
        FEnterCryptKey := 'Angiv krypterings nøglen:';
        FEnterDecryptKey := 'Angiv dekrypterings nøglen:';
        FArchiveIsReadOnly := 'Arkivet er Skrivebeskyttet';
        FKeyTooShort := 'Nøglen er for kort. Den skal indeholde mindst %d bogstaver';
        FConfirmCryptKey := 'Bekræft krypterings nøglen:';
        FKeyNotConfirmed := 'Nøglen kunne ikke bekræftes. Prøv igen.';
        FCanNotCreateArchive := 'Dette Komponent kan ikke lave et arkiv';
        FCannotCreateDir := 'Kan ikke lave et bibliotek';
        FSelectADirectory := 'Vælg et bibliotek:';
        FOk := 'OK';
        FCancel := 'Annuller';
        FInformation := 'Information';
        FWarning := 'Advarsel';
        FConfirmation := 'Bekræftelse';
        FError := 'Fejl';
        FCanNotBuildTempFileName := 'kan ikke lave et midlertidigt fil navn';
        FYes := '&Ja';
        FYesToAll := 'Ja til &alle';
        FNo := '&Nej';
        FFile := 'Fil';
        FCanContinue := 'Ønsker du at fortsætte ?';
        FUnknownVersion := 'Ukendt version';
        FCannotCreateFile := 'Can not create %s';
        FCannotOpenFile := 'Can not open %s';
        FArchiverBusy := 'Component %s is busy. Abort current operation and try again';
        FCantUseSolidArchiveWithExternalStream := 'Can''t use a solid Archive with an external stream';
      end;
    lgDutch:
      begin
        FBadSignature := 'Slechte handtekening: bestand "%s" is geen geldig archief';
        FFileNameNeeded := '"Archiver" heeft de eigenschap "FileName" nodig';
        FSystemMessage := 'Systeembericht';
        FAcceptArchiveFailed := 'Dit archief kan niet worden gebruikt';
        FEnterCryptKey := 'Geeft het wachtwoord voor codering:';
        FEnterDecryptKey := 'Geeft het wachtwoord voor decodering:';
        FArchiveIsReadOnly := 'Het archief staat alleen-lezen';
        FKeyTooShort := 'Het wachtwoord is te kort.  Het moet ten minste %d tekens tellen.';
        FConfirmCryptKey := 'Bevestig het wachtwoord voor codering:';
        FKeyNotConfirmed := 'Het wachtwoord werd niet bevestigd.  Voer het opnieuw in.';
        FCanNotCreateArchive := 'Deze component kan geen archief creëren';
        FCannotCreateDir := 'Kan directory niet aanmaken';
        FSelectADirectory := 'Selecteer een directory:';
        FOk := 'OK';
        FCancel := 'Annuleer';
        FInformation := 'Informatie';
        FWarning := 'Waarschuwing';
        FConfirmation := 'Bevestiging';
        FError := 'Fout';
        FCanNotBuildTempFileName := 'Kan geen tijdelijk bestand maken';
        FYes := '&Ja';
        FYesToAll := 'Ja op &Alles';
        FNo := '&Neen';
        FFile := 'Bestand';
        FCanContinue := 'Wilt u verder gaan?';
        FUnknownVersion := 'Onbekende versie';
        FCannotCreateFile := 'Kan  bestand %s niet aanmaken';
        FCannotOpenFile := 'Kan bestand %s niet openen';
        FArchiverBusy := 'Component %s is bezig. Annuleer de huidige actie en probeer het opnieuw';
        FCantUseSolidArchiveWithExternalStream := 'Can''t use a solid Archive with an external stream';
      end;
    lgCzech: // Thanks to Hana Krizova
      begin
        FBadSignature := 'Chybná signatura: soubor "%s" není platný archiv.';
        FFileNameNeeded := 'Zadejte název archivu (property "FileName")';
        FSystemMessage := 'Systémové hlášení';
        FAcceptArchiveFailed := 'Tento archiv nelze použít';
        FEnterCryptKey := 'Zadejte klíè pro šifrování:';
        FEnterDecryptKey := 'Zadejte klíè pro dešifrování:';
        FArchiveIsReadOnly := 'Archiv je urèen pouze pro ètení';
        FKeyTooShort := 'Klíè je pøíliš krátký. Požadovaná délka je nejménì %d znakù.';
        FConfirmCryptKey := 'Potvrïte klíè pro šifrování:';
        FKeyNotConfirmed := 'Klíè nelze potvrdit. Zadejte ho znovu.';
        FCanNotCreateArchive := 'Tato komponenta nemùže vytvoøit archiv';
        FCannotCreateDir := 'Adresáø nelze vytvoøit';
        FSelectADirectory := 'Zvolte adresáø:';
        FOk := 'OK';
        FCancel := 'Storno';
        FInformation := 'Informace';
        FWarning := 'Varování';
        FConfirmation := 'Potvrzení';
        FError := 'Chyba';
        FCanNotBuildTempFileName := 'Nelze vytvoøit pracovní archiv';
        FYes := '&Ano';
        FYesToAll := 'Ano pro vš&echny';
        FNo := '&Ne';
        FFile := 'Soubor';
        FCanContinue := 'Pokraèovat?';
        FUnknownVersion := 'Neznámá verze';
        FCannotCreateFile := 'Soubor %s nelze vytvoøit';
        FCannotOpenFile := 'Can not open %s';
        FArchiverBusy := 'Component %s is busy. Abort current operation and try again';
        FCantUseSolidArchiveWithExternalStream := 'Can''t use a solid Archive with an external stream';
      end;
  end;
end;

procedure TMessages.SetGlobalStrings;
begin
  strOK           := Ok;
  strCancel       := Cancel;
  strInformation  := Information;
  strWarning      := Warning;
  strConfirmation := Confirmation;
  strError        := Error;
  strYes          := FYes;
  strYesToAll     := FYesToAll;
  strNo           := FNo;
  strFile         := FFile;
  strCanContinue  := FCanContinue;
end;


////////////////////////////////////////////////////////////
// Protected section

procedure TArchiverRoot.AssignTo(Dest: TPersistent);
begin
  if Dest is TArchiverRoot then
    with TArchiverRoot(Dest) do
      begin
        CheckAvailableSpace := Self.CheckAvailableSpace;
        IsOpen := Self.IsOpen;
        StartOffset := Self.StartOffset;
        SFXCodeSize := Self.SFXCodeSize;
        BlockSize := Self.BlockSize;
        ErrorAction := Self.ErrorAction;
        FileName := Self.FileName;
        Filter := Self.Filter;
        Language := Self.Language;
        Messages := Self.Messages;
        Options := Self.Options;
      end
  else
    inherited AssignTo(Dest);
end;

function  TArchiverRoot.CreateMessages : TMessages;
begin
  Result := TMessages.Create;
end;

function  TArchiverRoot.GetSignature : Integer;
begin
  Result := $1238;
  if Assigned(FOnGetSignature) then
    FOnGetSignature( Self, Result );
end;

function TArchiverRoot.GetHeaderSize : Integer;
begin
  Result := sizeof(Integer)   + // Signature
            sizeof(Integer)   + // Version
            sizeof(Integer)   + // RandomID
            sizeof(Integer)   + // BlockSize
            sizeof(Integer)   + // EndOffset
            sizeof(Integer)   + // Segment
            sizeof(Boolean)   + // Crypted
            sizeof(TDataInfo) + // ArchiveInfo
            sizeof(TDataInfo) + // SegmentInfo
            sizeof(TUserData) + // User DataBuffer
            sizeof(FHeader.Reserved) + // Reserved area for future use
            Length(FHeader.Comment)+sizeof(Integer); // Archive comment
end;

procedure TArchiverRoot.WriteHeader;
var
  extra : Integer;
  tmp : TDataInfo;
begin
  FStream.Seek( GetStartOffset, soFromBeginning );
  // Add 1 to the segment file count if a file is beeing written
  if ioCompressingStream in FInternalOperation then
    extra := 1
  else
    extra := 0;
  with FHeader do
    begin
      WriteInteger( FStream, GetSignature );
      WriteInteger( FStream, Version );
      WriteInteger( FStream, RandomID );
      WriteInteger( FStream, BlockSize );
      WriteInteger( FStream, EndOffset );
      WriteInteger( FStream, Segment );
      FStream.WriteBuffer( ArchiveFlag, sizeof(ArchiveFlag) );
      FStream.WriteBuffer( ArchiveInfo, sizeof(ArchiveInfo) );
      // We add 1 to the FileCount of a segment if a file is beeing written
      tmp := SegmentInfo;
      Inc( tmp.FileCount, extra );
      FStream.WriteBuffer( tmp, sizeof(tmp) );
      FillChar( UserData, sizeof(UserData), 0 );
      if Assigned(FOnWriteUserData) then
        FOnWriteUserData( Self, UserData );
      FStream.WriteBuffer( UserData, sizeof(UserData) );
      FStream.WriteBuffer( Reserved, sizeof(Reserved) );
      WriteString( FStream, Comment );
    end;
  AfterUpdate;
end;

procedure TArchiverRoot.ReadHeader;
begin
  FStream.Seek( GetStartOffset, soFromBeginning );
  if not ReadHeaderOfStream( FStream, FHeader ) then
    raise EArchiver.CreateFmt( Messages.BadSignature, [FFileName] );
  if FHeader.Version > kVersion then
    raise Exception.Create( Messages.UnknownVersion );
end;

function TArchiverRoot.ReadHeaderOfFile( const fileName : String; var AHeader : TArchiveHeader ) : Boolean;
var
  S : TStream;
begin
  if not FileExists( fileName ) then
    begin
      Result := False;
      Exit;
    end;
  try
    S := NewStreamObject( fileName, fmOpenRead or fmShareDenyWrite );
    try
      CheckSFX( fileName );
      S.Position := GetStartOffset;
      Result := ReadHeaderOfStream( S, AHeader );
    finally
      S.Free;
    end;
  except
    Result := False;
  end;
end;

function TArchiverRoot.ReadHeaderOfStream( S : TStream; var AHeader : TArchiveHeader ) : Boolean;
begin
  Result := True;
  with AHeader do
    begin
      fillchar( AHeader, sizeof(AHeader), 0 );
      Signature := ReadInteger( S );
      if Signature <> GetSignature then
        begin
          Result := False;
          Exit;
        end;
      Version               := ReadInteger( S );
      RandomID              := ReadInteger( S );
      BlockSize             := ReadInteger( S );
      EndOffset             := ReadInteger( S );
      Segment               := ReadInteger( S );
      S.ReadBuffer( ArchiveFlag, sizeof(ArchiveFlag) );
      S.ReadBuffer( ArchiveInfo, sizeof(ArchiveInfo) );
      S.ReadBuffer( SegmentInfo, sizeof(SegmentInfo) );
      S.ReadBuffer( UserData, sizeof(UserData) );
      S.ReadBuffer( Reserved, sizeof(Reserved) );
      Comment := ReadString( S );
    end;
end;

procedure TArchiverRoot.CheckOpen;
begin
  if not IsOpen then
    Open
  else if not IsStreamOpen then
    OpenStream;
end;

function  TArchiverRoot.GetDirectorySize( const dir, filter : String ) : Integer;
  function DoCalcSize( const dir, path : String ) : Integer;
  var
    SR : TSearchRec;
    Found : Integer;
    source : String;
  begin
    Result := 0;
    source := dir + path;
    if not DirectoryExists(source) then
      Exit;
    try
      Result := 0;
      // First look at files
      Found := FindFirst( source+'\'+Filter, faAnyFile, SR );
      try
        while Found = 0  do
          begin
            if (SR.Name <> '.') and (SR.Name <> '..') then
              begin
                if (SR.Attr and faDirectory) = 0 then
                   begin
                    // Add file
                    Inc( Result, SR.Size );
                  end;
              end;
            Found := FindNext( SR );
          end;
      finally
        FindClose(SR);
      end;
      // Then look at folders
      if oRecurseFolders in Options then
        begin
          Found := FindFirst( source+'\*.*', faDirectory, SR );
          try
            while Found = 0  do
              begin
                if (SR.Name <> '.') and (SR.Name <> '..') then
                  begin
                    if (SR.Attr and faDirectory) <> 0 then
                       Result := result + DoCalcSize( dir, path+'\'+SR.Name );
                  end;
                Found := FindNext( SR );
              end;
          finally
            FindClose(SR);
          end;
        end;
    except
    end;
  end;

begin
  Result := DoCalcSize( RemoveSlash(dir), '' );
end;

function TArchiverRoot.DeleteDirectory( const dir : String ) : Boolean;

  procedure DoDeleteDirectory( const dir, path : String; var Result : Boolean );
  var
    SR : TSearchRec;
    Found : Integer;
    source : String;
  begin
    if not Result or not DirectoryExists( dir ) then
      Exit;

    source := dir + path;
    // Remove the files in the directory
    Found := FindFirst( source+'\*.*', faAnyFile, SR );
    try
      while Result and (Found = 0)  do
        begin
          if (SR.Name<>'.') and (SR.Name <> '..') then
            begin
              if (SR.Attr and faDirectory) <> 0 then
                begin
                  DoDeleteDirectory( dir, path+'\'+SR.Name, Result );
                end
              else
                begin
                  // Remove attributes that could prevent us from deleting the file
                  FileSetAttr( source+'\'+SR.Name, FileGetAttr(source+'\'+SR.Name) and
                                                   not (faReadOnly or faHidden) );
                  // Delete file
                  if not DeleteFile( source+'\'+SR.Name ) then
                    Result := False;
                end;
            end;
          Found := FindNext( SR );
        end;
    finally
      FindClose(SR);
    end;
    if Result then
      // Delete the empty directory
      Result := Result and RemoveDir( source );
  end;
begin
  Result := True;
  DoDeleteDirectory( RemoveSlash(dir), '', Result );
end;

procedure TArchiverRoot.Start;
begin
  Inc(FStartCount);
  if FStartCount = 1 then
    begin
      FMustAbort := False;
      FBytesToProcess := 0;
      FBytesProcessed := 0;
      FCurrentFileIdx := 0;
      FAlwaysContinue := False;
      FStartTime := Now;
      FTotTicks := 0;
      StartTimer;
      try
        AllocBlocks;
        if Assigned(FOnStartOperation) then
          FOnStartOperation( Self );
        UpdateProgress;
      except
        Finish;
        raise;
      end;
    end;
end;

procedure TArchiverRoot.Finish;
begin
  if FStartCount = 0 then
    Exit;
  Dec(FStartCount);
  if FStartCount = 0 then
    begin
      DeallocBlocks;
      FEndTime := Now;
      StopTimer;
      if Assigned(FOnFinishOperation) then
        FOnFinishOperation( Self );
      FBytesToProcess := 0;
      FOperation := opNone;
      if not(afCrypted in FHeader.ArchiveFlag) then
        FCryptKey := '';
    end;
end;

procedure TArchiverRoot.UpdateProgress;
begin
  // Check if abortion was requested
  if FMustAbort then
    Abort;
  // Calc ratio
  if oOpenSingleSegment in Options then
    begin
      if FHeader.SegmentInfo.FileCount > 0 then
        FPercent := Round( FCurrentFileIdx / FHeader.SegmentInfo.FileCount * 100 )
      else
        FPercent := 0;
    end
  else
    begin
      if FBytesToProcess > 0 then
        FPercent := Round( FBytesProcessed / FBytesToProcess * 100 )
      else
        FPercent := 0;
    end;
  if (ioOpenSolid in FInternalOperation) then
    FPercent := FPercent div 2
  else if (ioEnumAfterOpen in FInternalOperation) then
    FPercent := 50 + (FPercent div 2);
  if Assigned( FOnFileProgress ) then
    FOnFileProgress( Self, FPercent );
  ShowTiming;
end;

procedure TArchiverRoot.InitCompression;
begin
end;

procedure TArchiverRoot.InitCrypting;
begin
  // Do nothing special. It will be overriden in the subclasses.
end;

procedure TArchiverRoot.EnterCryptKey;
var
  ConfirmCryptKey : String;
begin
  StopTimer;
  FCryptKey := '';
  if Assigned(FOnEnterCryptKey) then
    FOnEnterCryptKey( Self, FCryptKey )
  else
    repeat
      // Ask a key until it is confirmed
      repeat
        // Ask a key until there's enough chars
        if not QueryPassword( Messages.SystemMessage, Messages.EnterCryptKey, FCryptKey ) then
          Abort;
        if Length(FCryptKey) < GetMinKeySize then
          MessageDlg( Format(Messages.KeyTooShort, [GetMinKeySize]), mtWarning, [mbOk], 0 );
      until Length(FCryptKey) >= GetMinKeySize;
      // Ask a confirmation of the key
      ConfirmCryptKey := '';
      if not QueryPassword( Messages.SystemMessage, Messages.ConfirmCryptKey, ConfirmCryptKey ) then
        Abort;
      if ConfirmCryptKey <> FCryptKey then
        begin
          MessageDlg( Messages.KeyNotConfirmed, mtWarning, [mbOk], 0 );
          FCryptKey := '';
        end;
    until ConfirmCryptKey = FCryptKey;
  StartTimer;
end;

procedure TArchiverRoot.RequestCryptKey;
begin
  StopTimer;
  FCryptKey := '';
  if Assigned(FOnRequestCryptKey) then
    FOnRequestCryptKey( Self, FCryptKey )
  else
    begin
      if not QueryPassword( Messages.SystemMessage, Messages.EnterDecryptKey, FCryptKey ) then
        Abort;
    end;
  StartTimer;
end;

function  TArchiverRoot.GetMinKeySize : Integer;
begin
  Result := kMinKeySize;
end;

procedure TArchiverRoot.SetBlockSize( val : Integer );
begin
  if (val <> FBlockSize) and (val >= 1024) then
    begin
      FBlockSize := val;
    end;
end;

procedure TArchiverRoot.AllocBlocks;
begin
  DeallocBlocks;
  GetMem( FSrcBlock, NeededBlockSize );
  GetMem( FDestBlock, NeededBlockSize);
end;

procedure TArchiverRoot.DeallocBlocks;
begin
  if Assigned( FSrcBlock ) then
    begin
      FreeMem( FSrcBlock );
      FSrcBlock := nil;
    end;
  if Assigned( FDestBlock ) then
    begin
      FreeMem( FDestBlock );
      FDestBlock := nil;
    end;
end;

function TArchiverRoot.CanContinue( E : Exception ) : Boolean;

  function Ask : Boolean;
  var
    id : Integer;
  begin
    if FAlwaysContinue then
      begin
        Result := True;
        Exit;
      end;
    StopTimer;
    id := QueryContinue( E.Message, AdjustPath(FCurrentFileEntry.Name, 50),
                         FCurrentFileEntry.ArchiveInfo.Size,
                         FCurrentFileEntry.Date );
    StartTimer;
    FAlwaysContinue := (id = 102); // 102 = button "Yes to all"
    Result := (id = mrYes) or FAlwaysContinue;
  end;

var
  errorAction : TErrorAction;
begin
  Result := False;
  errorAction := FErrorAction;
  if Assigned( FOnError ) then
    FOnError( Self, E, FCurrentFileEntry, errorAction );
  case errorAction of
    eaContinue:  Result := True;
    eaAbort:     Result := False;
    eaAsk:       Result := Ask;
  end;
  if Result then
    FBytesProcessed := FBytesProcessed + FCurrentFileEntry.ArchiveInfo.Size;
end;

function  TArchiverRoot.NeededBlockSize : Integer;
begin
  Result := FHeader.BlockSize;
end;

function TArchiverRoot.GetLanguage : TLanguage;
begin
  Result := FMessages.Language;
end;

procedure TArchiverRoot.SetLanguage( val : TLanguage );
begin
  if val <> FMessages.Language then
    begin
      FMessages.Language := val;
    end;
end;

procedure TArchiverRoot.SetMessages( val : TMessages );
begin
  FMessages.Assign( val );
end;

procedure TArchiverRoot.SetFileName( const val : String );
begin
  if val <> FFileName then
    begin
      Close;
      FFileName := val;
      ExplodeFileName;
    end;
end;

procedure TArchiverRoot.ExplodeFileName;
var
  i : Integer;
begin
  // Prepare Archive drive, name, dir, ext
  FArchiveDrive := ExtractFileDrive( FFileName );
  FArchiveDir   := AppendSlash( ExtractFileDir( FFileName ) );
  if Length(FArchiveDrive) > 0 then
    System.Delete( FArchiveDir, 1, Length(FarchiveDrive) );
  FArchiveExt   := ExtractFileExt( FFileName );
  FArchiveName  := ExtractFileName( FFileName );
  // Remove extension
  if (Length(FArchiveExt) > 0) and (Length(FArchiveName) > 0) then
  System.Delete( FArchiveName, Length(FArchiveName)-Length(FArchiveExt)+1, Length(FArchiveExt) );
  // Remove Segment number attached to the name
  i := Length(FArchiveName);
  if i = 0 then
    Exit;
  while (i>0) and (FArchiveName[i] in ['0'..'9']) do
    Dec(i);
  if (i>0) and (FArchiveName[i] = '-') then
    begin
      System.Delete( FArchiveName, i, Length(FArchiveName) );
    end;
end;

function TArchiverRoot.GetFileEntrySize( const fileEntry : TFileEntry ) : Integer;
begin
  with fileEntry do
    Result := (Length(Name)+Sizeof(Integer)) + // Name
              sizeof(Extended)    +  // Date
              sizeof(Integer)     +  // Attr
              sizeof(Word)        +  // Segment
              sizeof(Integer)     +  // Offset
              sizeof(Integer)     +  // File Offset
              sizeof(Boolean)     +  // FinalSegment
              sizeof(ArchiveInfo) +  // ArchiveInfo
              sizeof(SegmentInfo) ;  // SegmentInfo
end;

procedure TArchiverRoot.WriteFileEntry( var fileEntry : TFileEntry );
begin
  RequestSpace( GetFileEntrySize( fileEntry ) );
  with fileEntry do
    begin
      Offset := FStream.Position - GetStartOffset;
      WriteString( FStream, Name );
      WriteFloat( FStream, Date );
      WriteInteger( FStream, Attr );
      WriteWord( FStream, Segment );
      WriteInteger( FStream, Offset );
      WriteInteger( FStream, FileOffset );
      FStream.WriteBuffer( FileFlag, sizeof(FileFlag) );
      FStream.WriteBuffer( ArchiveInfo, sizeof(ArchiveInfo) );
      FStream.WriteBuffer( SegmentInfo, sizeof(SegmentInfo) );
    end;
end;

procedure TArchiverRoot.ReadFileEntry( var fileEntry : TFileEntry );
begin
  CheckEOF;
  with fileEntry do
    begin
      Name := ReadString( FStream );
      Date := ReadFloat( FStream );
      Attr := ReadInteger( FStream );
      Segment := ReadWord( FStream );
      Offset := ReadInteger( FStream );
      FileOffset := ReadInteger( FStream );
      FStream.ReadBuffer( FileFlag, sizeof(FileFlag) );
      FStream.ReadBuffer( ArchiveInfo, sizeof(ArchiveInfo) );
      FStream.ReadBuffer( SegmentInfo, sizeof(SegmentInfo) );
    end;
end;

function TArchiverRoot.IsRemovableDisk( const drive : String ) : Boolean;
begin
  Result := GetDriveType(PChar(drive)) = DRIVE_REMOVABLE;
end;

function  TArchiverRoot.GetSegmentName( segment : Word ) : String;
var
  count : String;
begin
  if segment = 1 then
    Result := Format('%s%s%s%s', [FArchiveDrive, FArchiveDir, FArchiveName, FArchiveExt])
  else
    begin
      count := IntToStr(segment);
      while Length(count) < 3 do
        Insert( '0', count, 1 );
      Result := Format('%s%s%s-%s%s', [FArchiveDrive, FArchiveDir, FArchiveName, count, kDefaultExt]);
    end;
  FSegmentNeeded := segment;
end;

function TArchiverRoot.NewStreamObject( const FileName : String; mode : Word ) : TStream;
begin
  try
    Result := TFileStream.Create( FileName, mode );
  except
    if (mode and fmCreate) <> 0 then
      raise EArchiver.CreateFmt( Messages.CanNotCreateFile, [FileName] )
    else
      raise EArchiver.CreateFmt( Messages.CanNotOpenFile, [FileName] );
  end;
end;

procedure TArchiverRoot.CreateStream;
begin
  if not ExternalStream then
    begin
      CloseStream;
      ForceDirectories( ExtractFilePath( FileName ) );
      CheckSFX( FileName );
      FStream := NewStreamObject( FileName, fmCreate );
    end
  else
    Exclude( FHeader.ArchiveFlag, afSolid ); // forbidden with an external stream
  WriteHeader;
  FCurrentFileIdx := 0;
  FReadOnly := False;
end;

procedure TArchiverRoot.OpenStream;
var
  mode : Integer;
begin
  if not ExternalStream then
    begin
      CloseStream;
      CheckSFX( FileName );
      FReadOnly := (FileGetAttr( FileName ) and faReadOnly) <> 0;
      if FReadOnly then
        mode := fmOpenRead or fmShareDenyWrite
      else
        mode := GetOpenMode;
      try
        // Try to open the file with the selected mode
        FStream := NewStreamObject( FileName, mode );
      except
        // If we're already in ReadOnly, then just raise again the exception
        if FReadOnly then
          raise;
        // If we could not open, then try again in ReadOnly mode
        mode := fmOpenRead or fmShareDenyWrite;
        FReadOnly := True;
        FStream := NewStreamObject( FileName, mode );
      end;
    end;
  try
    ReadHeader;
    FCurrentFileIdx := 0;
    if ExternalStream and (afSolid in Header.ArchiveFlag) then
      raise Exception.Create(Messages.CantUseSolidArchiveWithExternalStream);
  except
    CloseStream;
    raise;
  end;
end;

procedure TArchiverRoot.CloseStream;
begin
  if Assigned( FStream ) then
    begin
      if (oCreateReadOnly in Options) and
         not(afReadOnly in FHeader.ArchiveFlag) then
        begin
          Include( FHeader.ArchiveFlag, afReadOnly );
          WriteHeader;
        end;
      if not ExternalStream then
        FStream.Free;
      FStream := nil;
      FExternalStream := False;
    end;
end;

procedure TArchiverRoot.CheckReadOnly;
begin
  if ReadOnly or (afReadOnly in FHeader.ArchiveFlag) then
    raise EArchiver.Create( Messages.ArchiveIsReadOnly );
end;

procedure TArchiverRoot.CheckKey;
begin
  if FCryptKey <> '' then
    Exit;
  if FOperation = opAdd then
    EnterCryptKey
  else if FOperation in [opExtract, opCheck] then
    RequestCryptKey;
end;

procedure TArchiverRoot.BeforeOpen;
begin
  ClearFiles;
  FOldFileName := FileName;
  FOldOptions := FOptions;
  FOldMaxSegmentSize := FMaxSegmentSize;
  if Assigned(FOnBeforeOpen) then
    FOnBeforeOpen( Self );
end;

procedure TArchiverRoot.AfterOpen;
begin
  if Assigned(FOnAfterOpen) then
    FOnAfterOpen( Self );
end;

procedure TArchiverRoot.BeforeClose;
begin
  if Assigned(FOnBeforeClose) then
    FOnBeforeClose( Self );
end;

procedure TArchiverRoot.AfterClose;
begin
  if oCreateSolidArchives in FOldOptions then
    begin
      FOptions := FOldOptions;
      FFileName := FOldFileName;
      FMaxSegmentSize := FOldMaxSegmentSize;
      ExplodeFileName;
    end;
  ClearFiles;
  if not(csDestroying in ComponentState) and Assigned(FOnAfterClose) then
    FOnAfterClose( Self );
end;

procedure TArchiverRoot.AfterUpdate;
begin
  if Assigned(FOnAfterHeaderUpdate) then
    FOnAfterHeaderUpdate( Self );
end;

function TArchiverRoot.GetTempFileName : String;
var
  TmpPath : String;
  number : Integer;
  count : Integer;
begin
  count := 0;
  TmpPath := GetTempDir;
  number := GetTickCount;
  repeat
    if count > 1000 then
      raise Exception.Create( Messages.CanNotBuildTempFileName );
    Inc(number);
    Inc(count);
    Result :=  Format('%s~Archiv-%x.tmp', [TmpPath, number]);
  until not FileExists(Result);
end;

procedure TArchiverRoot.GetProgressInformations;
begin
  // Will be overriden in TCustomExtractor
end;

procedure TArchiverRoot.CreateArchive;
begin
  raise EArchiver.Create( Messages.CanNotCreateArchive );
end;

function  TArchiverRoot.GetOpenMode : Integer;
begin
  Result := 0;
end;

function  TArchiverRoot.RequestSpace( val : Integer ) :  Boolean;
begin
  // This function will be overriden in TCustomArchiver
  Result := True;
end;

function  TArchiverRoot.CheckEOF : Boolean;
begin
  // This function will be overriden in TCustomExtractor
  Result := False;
end;

function  TArchiverRoot.SelectDirectory(var Directory: string; Options: TMySelectDirOpts; HelpCtx: Longint):Boolean;
begin
  Result := InputQuery( Messages.SystemMessage, Messages.SelectADirectory, Directory );
end;

// Copied from unit FileCtrl.pas
procedure TArchiverRoot.ForceDirectories(Dir: string);

  function LastChar( const s : String ) : Char;
  begin
    if Length(s) > 0 then
      Result := s[Length(s)]
    else
      Result := #0;
  end;

begin
  if Length(Dir) = 0 then
    raise Exception.Create(Messages.CannotCreateDir);
  if LastChar(Dir) = '\' then
    System.Delete(Dir, Length(Dir), 1);
  if (Length(Dir) < 3) or DirectoryExists(Dir)
    or (ExtractFilePath(Dir) = Dir) then Exit; // avoid 'xyz:\' problem.
  ForceDirectories(ExtractFilePath(Dir));
  CreateDir(Dir);
end;

function  TArchiverRoot.SelectFile( const Title : String; var FileName : String ) : Boolean;
begin
  Result := InputQuery( Messages.SystemMessage, Title, FileName );
end;

function TArchiverRoot.MessageDlg( const Msg: string; DlgType: TMyMsgDlgType;
                                   Buttons: TMyMsgDlgButtons; HelpCtx: Longint): Integer;
begin
  Result := ArchiverMisc.MessageDlg( Msg, DlgType, Buttons, HelpCtx );
end;

function TArchiverRoot.InputQuery(const ACaption, APrompt: string; var AValue: string): Boolean;
begin
  Result := ArchiverMisc.InputQuery( ACaption, APrompt, AValue );
end;

function TArchiverRoot.QueryPassword(const ACaption, APrompt: string; var AValue: string): Boolean;
begin
  Result := ArchiverMisc.QueryPassword( ACaption, APrompt, AValue );
end;

function  TArchiverRoot.GetStartOffset : Integer;
begin
  if (FSegmentNeeded = 1) or (SFXCodeSize > 0) then
    Result := StartOffset
  else
    Result := 0;
end;

procedure TArchiverRoot.CheckSFX( const aFileName : String );
begin
  if SFXCodeSize > 0 then
    begin
      if IsExeFile( aFileName ) then
        StartOffset := SFXCodeSize
      else
        StartOffset := 0;
    end;
end;

procedure TArchiverRoot.Loaded;
begin
  inherited;
  // if we choosed the automatic language, then set it again
  // in order to force the selection of the strings according
  // to the user language. Otherwise the message strings are stored
  // in the .dfm
  if Language = lgAutomatic then
    begin
      Language := lgEnglish;
      Language := lgAutomatic;
    end;

end;

procedure TArchiverRoot.OpenSolidData;
begin
end;

procedure TArchiverRoot.CloseSolidData;
begin
end;

procedure TArchiverRoot.StartTimer;
begin
  FLastTicks := GetTickCount;
end;

procedure TArchiverRoot.StopTimer;
var
  tmp : Integer;
begin
  tmp := GetTickCount;
  Inc( FTotTicks, tmp - FLastTicks );
  if FTotTicks > 0 then
    FBytesPerMSec := FBytesProcessed / FTotTicks
  else
    FBytesPerMSec := FBytesProcessed;
  FLastTicks := tmp;
end;

procedure TArchiverRoot.ShowTiming;
var
  remainingTime : TDateTime;
  remainingMSec : Extended;
begin
  StopTimer;
  if (FBytesPerMSec <> 0) and (FBytesToProcess > FBytesProcessed) then
    remainingMSec := (FBytesToProcess - FBytesProcessed) / FBytesPerMSec
  else
    remainingMSec := 0;
  remainingTime := MSecsAsTime( Round(remainingMSec) );
  if Assigned( FOnShowTiming ) then
    FOnShowTiming( Self, ElapsedTime, remainingTime );
  StartTimer;
end;

function TArchiverRoot.GetElapsedTime : TDateTime;
begin
  Result := MSecsAsTime( FTotTicks );
end;

procedure TArchiverRoot.DisplayMessage( const msg : String );
begin
  if Assigned( FOnDisplayMessage ) then
    FOnDisplayMessage( Self, msg );
end;

procedure TArchiverRoot.AddToLog( const msg : String );
begin
  if Assigned( FOnAddToLog ) then
    FOnAddToLog( Self, msg );
end;

procedure TArchiverRoot.CopyStream( Src, Dest : TStream; trapExceptions : Boolean );
const
  kBuffSize = 32 * 1024;
var
  buffer : PChar;
  bytes : Integer;
begin
  GetMem( buffer, kBuffSize );
  try
    Start;
    try
      Src.Position := 0;
      FBytesToProcess := Src.Size - Src.Position;
      FBytesProcessed := 0;
      FTotTicks := 0;
      StartTimer;
      while Src.Position < Src.Size do
        begin
          bytes := Min( kBuffSize, Src.Size - Src.Position );
          Src.ReadBuffer( buffer^, bytes );
          Dest.WriteBuffer( buffer^, bytes );
          FBytesProcessed := FBytesProcessed + bytes;
          try
            UpdateProgress;
          except
            if not trapExceptions then
              raise;
          end;
        end;
    finally
      Finish;
    end;
  finally
    FreeMem( buffer );
  end;
end;

function TArchiverRoot.CopyFile( const srcName, destName : String;
                                 failIfExists, trapExceptions : Boolean ) : Boolean;
var
  Src, Dest : TStream;
begin
  Result := False;
  if FileExists(destName) then
    begin
      if failIfExists then
        Exit
      else
        DeleteFile(destName);
    end;
  Src := NewStreamObject( srcName, fmOpenRead or fmShareDenyWrite );
  try
    Dest := NewStreamObject( destName, fmCreate );
    try
      CopyStream( Src, Dest, trapExceptions );
      Result := True;
    finally
      Dest.Free;
    end;
  finally
    Src.Free;
  end;
end;

procedure TArchiverRoot.ClearFiles;
var
  i : Integer;
begin
  for i := 0 to FFiles.Count - 1 do
    TObject(FFiles.Items[i]).Free;
  FFiles.Clear;
  if not(csDestroying in ComponentState) and Assigned( FOnClearFileList ) then
    FOnClearFileList( Self );
end;

procedure TArchiverRoot.AddFileToList( const entry : TFileEntry );
var
  fo : TFileObject;
begin
  if not(oMaintainFileDirectory in FOptions) then
    Exit;
  fo := TFileObject.Create;
  fo.FileEntry := entry;
  FFiles.Add( fo );
end;

function TArchiverRoot.GetFiles( idx : Integer ) : TFileObject;
begin
  Result := TFileObject(FFiles.Items[idx]);
end;

function TArchiverRoot.GetFileCount : Integer;
begin
  Result := FFiles.Count;
end;

function TArchiverRoot.IndexOfFile( const FileName : String ) : Integer;
var
  i : Integer;
begin
  Result := -1;
  for i := 0 to FileCount - 1 do
    with Files[i] do
      if CompareStr( FileEntry.Name, FileName ) = 0 then
        begin
          Result := i;
          Break;
        end;
end;

procedure TArchiverRoot.AdjustArchiveSize;
begin
  if (Operation in [opExtract, opEnumerate, opCheck]) and
     (FHeader.ArchiveInfo.CompressedSize > FCompressedArchiveSize) then
    begin
      if FBytesToProcess = FCompressedArchiveSize then
        FBytesToProcess := FHeader.ArchiveInfo.CompressedSize;
      FCompressedArchiveSize := FHeader.ArchiveInfo.CompressedSize;
      UpdateProgress;
    end;
end;

function TArchiverRoot.CheckBusy : Boolean;
begin
  Result := IsBusy;
  if Result and (oShowBusyMessage in Options) then
    MessageDlg( Format( Messages.ArchiverBusy, [ClassName]), mtWarning, [mbOk], 0 );
end;

procedure TArchiverRoot.SetStream( val : TStream );
begin
  if FStream <> val then
    begin
      if CheckBusy then
        Exit;
      Close;
      FStream := val;
      FExternalStream := True;
    end;
end;

///////////////////////////////////////////////////////
// Public methods

constructor TArchiverRoot.Create( AOwner : TComponent );
begin
  inherited;
  FFilter := '*.*';
  FBlockSize := 1024 * 64;
  FErrorAction := eaAsk;
  FOperation := opNone;
  FMessages := CreateMessages;
  FCheckAvailableSpace := True;
  FInternalOperation := [];
  FIsOpen := False;
  FCryptKey := '';
  FStartOffset := 0;
  FExternalStream := False;
  FOptions := [oStoreEmptyFolders, oCompress, oRecurseFolders, oRestorePath,
               oSecureAccess, oEraseNewDisk, oConfirmFileDeletion, oShowBusyMessage];
  InitCompression;
  InitCrypting;
  FFiles := TList.Create;
end;

destructor  TArchiverRoot.Destroy;
begin
  Close;
  FMessages.Free;
  ClearFiles;
  FFiles.Free;
  inherited;
end;

function  TArchiverRoot.IsStreamOpen : Boolean;
begin
  Result := Assigned(FStream);
end;

function TArchiverRoot.IsSegmented : Boolean;
begin
  with FHeader do
    Result := not(afFinalSegment in FHeader.ArchiveFlag) or (Segment > 1);
end;

function TArchiverRoot.IsEmpty : Boolean;
begin
  Result := (FHeader.ArchiveInfo.FileCount + FHeader.SegmentInfo.FileCount) = 0;
end;

function TArchiverRoot.IsBusy : Boolean;
begin
  Result := FStartCount > 0;
end;

function TArchiverRoot.DeleteDriveContent( const drive : String ) : Boolean;
var
  SR : TSearchRec;
  Found : Integer;
  path : String;
begin
  Result := True;
  path := UpperCase( RemoveSlash( ExtractFileDrive(drive) ) );
  if not IsRemovableDisk( path ) then
    Exit;
  // Remove the files in the directory
  Found := FindFirst( path+'\*.*', faAnyFile, SR );
  try
    while Result and (Found = 0)  do
    begin
      if (SR.Name<>'.') and (SR.Name <> '..') then
      begin
        if (SR.Attr and faDirectory) <> 0 then
          // Delete subdirectory
          Result := Result and DeleteDirectory( path+'\'+SR.Name )
        else
        begin
          // Remove attributes that could prevent us from deleting the file
          FileSetAttr( path+'\'+SR.Name, FileGetAttr(path+'\'+SR.Name) and
                                           not (faReadOnly or faHidden) );
          // Delete file
          Result := DeleteFile( path+'\'+SR.Name );
        end;
      end;
      Found := FindNext( SR );
    end;
  finally
    FindClose(SR);
  end;
end;

procedure TArchiverRoot.Open;
var
  accept : Boolean;
  shouldCreate : Boolean;
begin
  if CheckBusy then
    Exit;
  if IsStreamOpen and not ExternalStream then
    Exit;
  if IsOpen and not ExternalStream then
    begin
      // If Archive is open, but the current segment is closed,
      // certainly because of an exception during swapping of the segment,
      // then we try to reopen the last opened segment.
      FFileName := GetSegmentName( FHeader.Segment );
      OpenStream;
      Exit;
    end;
  Include( FInternalOperation, ioOpening );
  try
    FArchiveChanged := False;
    FSegmentNeeded := 1;
    BeforeOpen;
    if not ExternalStream and (FileName = '') then
      raise EArchiver.Create( Messages.FileNameNeeded );
    if ExternalStream then
      ShouldCreate := Stream.Size = 0
    else
      ShouldCreate := not FileExists( FileName );
    if shouldCreate then
      CreateArchive
    else
      OpenStream;
    accept := True;
    try
      if Assigned(FOnAcceptArchive) then
        FOnAcceptArchive( Self, FHeader, accept );
      if not accept then
        raise EArchiver.Create( Messages.AcceptArchiveFailed );
      GetProgressInformations;
      FIsOpen := True;
      AllocBlocks;
      AfterOpen;
    except
      Close;
      raise;
    end;
  finally
    Exclude( FInternalOperation, ioOpening );
  end;
end;

procedure TArchiverRoot.OpenNew;
begin
  if CheckBusy then
    Exit;
  if not ExternalStream then
    Delete;
  Open;
end;

procedure TArchiverRoot.CreateTempFile;
begin
  if CheckBusy then
    Exit;
  Close;
  FileName := GetTempFileName;
  FCheckAvailableSpace := False; // Don't check free space when creating a temporay file
  Open;
end;

procedure TArchiverRoot.Close;
begin
  if CheckBusy then
    Exit;
  Include( FInternalOperation, ioClosing );
  try
    if IsOpen then
      begin
        BeforeClose;
        CloseStream;
        FCryptKey := '';
        FIsOpen := False;
        AfterClose;
      end
    else
      CloseStream;
  finally
    Exclude( FInternalOperation, ioClosing );
  end;
end;

function TArchiverRoot.Reset : Boolean;
var
  tmp : String;
begin
  if ExternalStream then
    begin
      Result := False;
      Exit;
    end;
  tmp := GetSegmentName( 1 );
  Result := Delete;
  FileName := tmp;
  if Result then
    Open;
end;

function TArchiverRoot.Delete : Boolean;
var
  tmp : String;
begin
  Result := False;
  if CheckBusy then
    Exit;
  if ExternalStream then
    Exit;
  tmp := FFileName;
  if IsSolidArchive then
    tmp := FOldFileName;
  Close;
  if FileExists( tmp ) then
     Result := DeleteFile( tmp )
end;

function TArchiverRoot.Rename( const NewName : String ) : Boolean;
begin
  Result := False;
  if CheckBusy then
    Exit;
  if ExternalStream then
    Exit;
  if FileExists( NewName ) then
    Exit;
  if not IsOpen then
    Exit;
  CloseStream;
  Result := RenameFile( FFileName, NewName );
  if Result then
    begin
      FFileName := NewName;
      ExplodeFileName;
    end;
  OpenStream;
end;

function TArchiverRoot.CanAbort : Boolean;
begin
  Result := not (ioCloseSolid in FInternalOperation);
end;

procedure TArchiverRoot.RequestAbort;
begin
  if CanAbort then
    FMustAbort := True;
end;

end.


