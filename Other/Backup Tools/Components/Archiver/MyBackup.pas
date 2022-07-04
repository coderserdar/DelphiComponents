unit MyBackup;
(*-----------------------------------------------------------------------------
  DESCRIPTION

  This component is freeware. You may use it, distribute it and modify it, but
  you may not charge for it.
  In case of modifications you must mail me (mmm@imaginet.fr) a copy of
  the modifications. The reason are simple: Any changes that improve this free-
  ware component should be to benefit for everybody, not only you. That way you
  can be pretty sure, that this component has few errors and much functionality.
  In case of modifications, you will be on the credits list beneath:

  HISTORY

  Version 1.0 (25/06/97) by Morgan Martinet (France):

  This version is the base version.
  This Component lets you backup/restore a set of files in a directory. It will
  split the files if they don't fit in the Backup unit. I use this component
  with the Delphi Zip component that implements PK(UN)ZIP v2.04g, and it works
  perfectly ! So I first make a zipped archive of my directories and then
  I use MyBackup to to put this archives on disks because DelphiZip doesn't
  implement segmented archives.

  Properties:
    DrivePath       Sets the path of the backup unit (by default it is A:\)
    FilesPath       Sets the path from where the files will be backed up or
                      to where the files will be restored.
    BackupCaption   Sets a title for the Backup operation.
    RestoreCaption  Sets a title for the Restore operation.
    ConfirmDelete   Lets you specify if the user will be warned when the
                      component will need to erase the files of DrivePath.
    Version         Sets the Version of your software that did the
                      backup, in order to update your files if the backup is
                      older than the current version.
    ID              Sets a personal ID in order to identify the product that
                      did the backup and avoid restoring the backup of another
                      product.
    IdLabel         Sets a label for the Id of the Backup. This label will be
                      displayed when an Id's disk does not match the Id of
                      this component (cf. ID).
    BackupName      Sets a name for the backup.
    UserName        Sets the name of the registered user of your software in
                      order to check the origin of a backup.
    UserCompany     Sets the company of the registered user of your software
                      in order to check the origin of a backup.
    UserLicence     Sets the licence of the registered user of your software
                      in order to check the origin of a backup.
    InfosFileName   Sets the name of the file that contains the informations
                      about the backup and that will be written on each disk.
    Language        Sets the language used by the component
    Messages        String Messages used by the component
    DisplayAbort    if true, then it raises an exception with an abort message,
                    else it uses the delphi procedure Abort.
    OnDiskChanged   Event triggered when the disk is changed.
                    It lets you format the disk before anything is done for instance.

    Filter          Lets you filter the files from the FilesPath to be backed up.

    FilesToBackup   Lets you define a list of files or directories to be backed up,
                    instead of using the property FilesPath.

    FilesRestored   Contains the list of files that were restored.

    UseArchiver     If true, we use a TArchiverInterface for the backup.

    Archiver        Lets you connect the TMyBackup to a TArchiverInterface. You can then
                    define some options in the TArchiverInterface.
                    Otherwise, TMyBackup will create its own TArchiverInterface, but an
                    empty one ! So, use TMyArchBackup if you want TMyBackup to create
                    an interface with TArchiver, or drop a TArchiverInt and connect it
                    to the TMyBackup with the Archiver property of TMyBackup.
                    TArchiverInt will create automatically an instance of TArchiver,
                    but if you drop a TArchiver and you connect it to the TArchiverInt
                    with ist property Archiver, then TArchiverInt will use this component.
                    It's better if you want to customize TArchiver.
                    But, the best solution is to use directly TArchiver, because it does
                    all what TMyBackup does, but even better !!!!!
                    This was implemented if you don't want to change you settings, or
                    if you want to keep with DelZip.



  For this properties look at the comments in the CopyFile component
    Progress
    OnStartOperation
    OnFinishOperation
    OnOperationProgress
    OnEachFile

  Methods:
    Backup = start the backup process
    Restore = start the restore process
    CheckFirstDisk = check if the disk inserted contains
                     the first disk of a backup
    GetInfos = get the file that contains the informations
               about the backup into a list of strings (TStrings).
               You can then get the informations like:
               var
                 SL : TStringList;
               begin
                 SL := TStringList.Create;
                 try
                   if GetInfos( SL ) then
                     begin
                       version := SL.Values['Version'];
                       disk_num := StrToInt(SL.Values['Disk#']);
                       name := SL.Values['Backup.Name'];
                     end
                   else
                     // error !
                 finally
                   SL.Free;
                 end;
               end;
  ----------------------------------------------------------------------------
  Version 1.1 (09/10/97) Morgan Martinet (France):

  Added property Filter in order to filter the files of the FilesPath to be
        backed up.
  Added property FilesToBackup (StringList) in order to backup a list of files
        instead of the content of the FilesPath directory.
  Added property FilesRestored (StringList) in order to know which files
        were restored.
  Modified Backup procedure: if the FilesToBackup list is not empty, we use
        this list in order to do the backup, otherwise we use the files of the
        FilesPath directory.
  Modified Restore procedure: we update the FilesRestored list, as we restore
        a new file.
  ----------------------------------------------------------------------------
  Version 1.2 (03/26/98) by Huanlin Tsai (Taiwan).
    E-Mail: huanlin@geocities.com  or  easyman@ms2.seeder.net

  - Added traditional Chinese messages.
  - DrivePath now can be a hard disk path (ex: c:\data\) and will auto detect
    if need asking insert disk #.
  - Calls to 'Abort' are replaced by 'raise Exception.Create(sAborted)'.
  - Bugs found in procedure 'CheckDrive' and 'CheckFirstDisk':
    When restoring files and drive A:\ has a disk which is not the first disk,
    after asking 'Insert disk #1 to drive A:\', the user can't choose 'Cancel'
    to abort the operation, it just keep on asking. The bug is fixed now.
  - Local 'AskForDisk' procedures are moved out as one procedure.
  - Some comments and code style are slightly changed (sorry if ugly!).
  - Added a Demo project
  ----------------------------------------------------------------------------
  Version 1.3 (08/04/98) Morgan Martinet (France):
  - Fixed a bug when using a list of files to backup, the component
    did calc the size of the content of the directory instead of the sum
    of the size of the files in the list (see procedure PrecalcDirSize).
  - Fixed a bug that prevented from showing the Backup/Restore caption
  - Fixed a bug that calced the size of the directory and its subdirectories
    instead of only calculate the size of the directory, because we don't
    backup the subdirectories.
  - Extended the Demo application
  ----------------------------------------------------------------------------
  Version 1.4 (01/06/98) Morgan Martinet (France):
  - Added class TMessages that stores the string messages used by the
    component, and the different translations.
  - Added property Language, that lets you select a specific language
    (It's an idea of Oliver Buschjost).
  - Added property Messages that lets you redefine the messages used by
    the component, for a language that's not already known.
  - Added event OnDiskChanged triggered after that the disk has changed,
    but before that the content is checked for deletion.
    It's an idea of Hugo Souza (hsouza@spcb.com.br)
  - Added Portuguese thanks to Hugo Souza (hsouza@spcb.com.br)
  ----------------------------------------------------------------------------
  Version 1.5 (02/06/98) Morgan Martinet (France):
  - Forgot to free the object TMessage !
  - changed SetLanguage: do not call FMessages.SetLanguage when the component
    is loading because it would overwrite the changes to the messages made
    by the user.
  - Added German thanks to Oliver Buschjost (buschjost@geocities.com)
  - Added property DisplayAbort: if true, then it raises an exception with
    an abort message, else it uses the delphi procedure Abort.
  ----------------------------------------------------------------------------
  Version 1.6 (10/06/98) Morgan Martinet (France):
  - Used a TFileStream instead of Basic IO. It resolves the
    difference between Delphi2 and Delphi3.
  - Fixed a bug : when backing up, if we swap the current disk while writing
    a file, and if the disk content must be deleted, then the message progress
    would display "deleting files...", and not restore to "copying file...".
  - Fixed a bug : bad progress ratio when restoring files
  - Fixed a bug : when the file was split, the restore process did open
    the file and seek to the end, but if the file did already exist,
    it made it bigger than it should !
    The trick : we store each file restored in FilesRestored, so before
    we try to open the destination file, we check if it already exists
    in this list: if yes, it means that we already started restoring it,
    so we do nothing. Else, we delete it, so we can garantee that appending
    to this file started with an empty file.
  - Do not copy the infos.txt file when restoring files.
  - Added TArchiver support:
    You allow the Archiver use with the property UseArchiver.
    You can give your TArchiver to TMyBackup with the property Archiver.
    When MyBackup needs an archiver (GetArchiver), it will look if this property is set,
    else it will create its own with (CreateArchiver a virtual method, so you can
    create any of the descendants of TArchiver).
    It's usefull to connect your MyBackup on a TArchiver that you dropped on
    the form, because you can define some options and some events (MyBackup
    uses the events OnAddFile, OnExtractFile, OnFileProgress and OnFileExtracted,
    so don't try to define them, they will be overwritten).
  ----------------------------------------------------------------------------
  Version 1.7 (10/08/98) Morgan Martinet (France):
  - Added property ArchiverOptions if you need to define some options
    of the TArchiver component. You can deactivate the compression,
    or activate the encryption for instance.
  - Added italian language thanks to Gabriele Bigliardi (gbigliardi@manord.com)
  ----------------------------------------------------------------------------
  Version 1.8 (24/11/98) Morgan Martinet (France):
  - Removed direct reference to my component TArchiver. I replaced it with
    a component wrapper (TArchiverInterface) that contains only virtual methods.
    So, you can derive it in order to interface it with some other components
    that makes archives, like DelZip or mine.


-----------------------------------------------------------------------------*)

interface

uses
  Windows, Messages, SysUtils, Graphics, Controls, Forms, Dialogs,
  CopyFile, Classes;


const
  kMinDiskSpace = 1024*8;
  kInfosFile = 'infos.txt';

type
  TOperation = (opBackup, opRestore, opNone);

  TLanguage = (lgEnglish, lgFrench, lgChinese, lgChineseGB, lgPortuguese,
               lgGerman, lgItalian, lgSpanish, lgDutch, lgCzech);

  TOnExtractFileEvent = procedure ( Sender : TObject; const FileName : String; Size : Integer ) of Object;
  TOnAddFileEvent = procedure ( Sender : TObject; const FileName : String; Size : Integer ) of Object;
  TOnFileExtractedEvent = procedure ( Sender : TObject; const FileName, DestPath : String; Size : Integer ) of Object;
  TOnFileProgress = procedure ( Sender : TObject; percent : Integer ) of Object;

  TArchiverInterface = class(TComponent)
  protected
    FOnExtractFile : TOnExtractFileEvent;
    FOnAddFile : TOnAddFileEvent;
    FOnFileExtracted : TOnFileExtractedEvent;
    FOnFileProgress : TOnFileProgress;

    procedure SetFileName( const aFileName : String ); virtual;
    function  GetFileName : String; virtual;
    procedure SetExtractPath( const aPath : String ); virtual;
    function  GetExtractPath : String; virtual;
    procedure SetLanguage( lang : TLanguage );
    function  GetLanguage : TLanguage;
  public
    procedure Open; virtual;
    procedure Close; virtual;
    function  AddFile( const aFileName : String ) : Boolean; virtual;
    function  AddFiles( files : TStrings ) : Boolean; virtual;
    function  AddDirectory( const Directory : String ) : Boolean; virtual;
    procedure ExtractFiles; virtual;
    procedure SetRecursive( val : Boolean ); virtual;
    procedure Delete; virtual;

    property FileName : String read GetFileName write SetFileName;
    property ExtractPath : String read GetExtractPath write SetExtractPath;
    property Language : TLanguage read GetLanguage write SetLanguage;
    property OnExtractFile : TOnExtractFileEvent read FOnExtractFile write FOnExtractFile;
    property OnAddFile : TOnAddFileEvent read FOnAddFile write FOnAddFile;
    property OnFileExtracted : TOnFileExtractedEvent read FOnFileExtracted write FOnFileExtracted;
    property OnFileProgress : TOnFileProgress read FOnFileProgress write FOnFileProgress;
  end;

  TMessages = class(TPersistent)
    protected
      FSystemMessage            : String;
      FNeedValidDrivePath       : String;
      FInsertDiskXInUnit        : String;
      FDeletingFilesOfDrivePath : String;
      FAskForDeletionOfFiles    : String;
      FCouldNotDeleteAllFiles   : String;
      FThisDiskIsNotTheFirstOne : String;
      FThePathDoesNotExist      : String;
      FCopyOf                   : String;
      FCouldNotWriteInFile      : String;
      FThisDiskContainsNoBackup : String;
      FWrongBackupSet           : String;
      FWrongBackupName          : String;
      FAborted                  : String;
      FReopeningTables          : String;
      FCompressingFile          : String;
      FUncompressingFile        : String;
      FNeedBackupName           : String;

      procedure AssignTo(Dest: TPersistent); override;

    public
      constructor Create;

      procedure SetLanguage( language : TLanguage );

    published
      property SystemMessage : String read FSystemMessage write FSystemMessage;
      property NeedValidDrivePath : String read FNeedValidDrivePath write FNeedValidDrivePath;
      property InsertDiskXInUnit : String read FInsertDiskXInUnit write FInsertDiskXInUnit;
      property DeletingFilesOfDrivePath : String read FDeletingFilesOfDrivePath write FDeletingFilesOfDrivePath;
      property AskForDeletionOfFiles : String read FAskForDeletionOfFiles write FAskForDeletionOfFiles;
      property CouldNotDeleteAllFiles : String read FCouldNotDeleteAllFiles write FCouldNotDeleteAllFiles;
      property ThisDiskIsNotTheFirstOne : String read FThisDiskIsNotTheFirstOne write FThisDiskIsNotTheFirstOne;
      property ThePathDoesNotExist : String read FThePathDoesNotExist write FThePathDoesNotExist;
      property CopyOf : String read FCopyOf write FCopyOf;
      property CouldNotWriteInFile : String read FCouldNotWriteInFile write FCouldNotWriteInFile;
      property ThisDiskContainsNoBackup : String read FThisDiskContainsNoBackup write FThisDiskContainsNoBackup;
      property WrongBackupSet : String read FWrongBackupSet write FWrongBackupSet;
      property WrongBackupName : String read FWrongBackupName write FWrongBackupName;
      property Aborted : String read FAborted write FAborted;
      property ReopeningTables : String read FReopeningTables write FReopeningTables;
      property CompressingFile : String read FCompressingFile write FCompressingFile;
      property UncompressingFile : String read FUncompressingFile write FUncompressingFile;
      property NeedBackupName : string read FNeedBackupName write FNeedBackupName;
  end;

  TMyBackup = class(TCustomCopyFile)
  private
  protected
    FDrivePath : String;
    FFilesPath : String;
    FBackupCaption : String;
    FRestoreCaption : String;
    FConfirmDelete : Boolean;
    FOperation : TOperation;
    FVersion : String;
    FID : String;
    FIdLabel : String;
    FBackupName : String;
    FDrive : Char;
    FUserName : String;
    FUserCompany : String;
    FUserLicence : String;
    FDiskCount : Integer;
    FCurrentFile : String;
    FInfosFileName : String;
    FOneMoreDisk : Boolean;
    FCurBackup : String;
    FFilesToBackup : TStringList; // Contains the list of the files to be backed up.
    FFilesRestored : TStringList; // Contains the list of the files restored.
    FMessages : TMessages;
    FLanguage : TLanguage;
    FOnDiskChanged:TNotifyEvent;
    FDisplayAbort : Boolean;
    FUseArchiver : Boolean;
    FArchiver : TArchiverInterface;

    procedure SetFilesToBackup( val : TStringList );
    procedure SetFilesRestored( val : TStringList );
    procedure CheckDrive;
    procedure NextDisk;
    function  CheckDiskContent : Boolean;
    procedure CheckPath;
    procedure WriteFiles;
    procedure WriteFile( const FileName : String );
    procedure WriteInfos;
    procedure FileExtractedFromArchive( Sender : TObject; const FileName, DestPath : String; Size : Integer );
    procedure ReadFiles;
    procedure ReadFile( const FileName : String );
    procedure AskForDisk;
    procedure PrecalcDirSize( const dir : String ); override;
    procedure SetLanguage( lang : TLanguage );
    procedure SetMessages( msg : TMessages );
    procedure DoAbort;
    function  CreateArchiver : TArchiverInterface; virtual;
    function  GetArchiver : TArchiverInterface;
    function  GetTempDir : String;
    procedure AddFileEvent( Sender : TObject; const FileName : String; Size : Integer );
    procedure ExtractFileEvent( Sender : TObject; const FileName : String; Size : Integer );
    procedure FileProgressEvent( Sender : TObject; Percent : Integer );
    function  GetUseArchiver : Boolean;
    procedure Notification(AComponent: TComponent; Operation: Classes.TOperation); override;

  public
    constructor Create( AOwner : TComponent ); override;
    destructor  Destroy; override;

    procedure Backup;
    procedure Restore;

    function  CheckFirstDisk : Boolean;
    function  GetInfos( SL : TStringList ) : Boolean;
    procedure Finish; override;

  published
    property DrivePath : String read FDrivePath write FDrivePath;
    property FilesPath : String read FFilesPath write FFilesPath;
    property FilesToBackup : TStringList read FFilesToBackup write SetFilesToBackup;
    property FilesRestored : TStringList read FFilesRestored write SetFilesRestored;
    property BackupCaption : String read FBackupCaption write FBackupCaption;
    property RestoreCaption : String read FRestoreCaption write FRestoreCaption;
    property ConfirmDelete : Boolean read FConfirmDelete write FConfirmDelete default True;
    property Version : String read FVersion write FVersion;
    property ID : String read FID write FID;
    property IdLabel : String read FIdLabel write FIdLabel;
    property BackupName : String read FBackupName write FBackupName;
    property UserName : String read FUserName write FUserName;
    property UserCompany : String read FUserCompany write FUserCompany;
    property UserLicence : String read FUserLicence write FUserLicence;
    property InfosFileName : String read FInfosFileName write FInfosFileName;
    property Language : TLanguage read FLanguage write SetLanguage;
    property Messages : TMessages read FMessages write SetMessages;
    property DisplayAbort : Boolean read FDisplayAbort write FDisplayAbort;
    property UseArchiver : Boolean read GetUseArchiver write FUseArchiver;
    property Archiver : TArchiverInterface read FArchiver write FArchiver;
    Property OnDiskChanged:TNotifyEvent read FOnDiskChanged write FOnDiskChanged;

    // Properties herited and moved in the published section
    property Filter;
    property Progress;
    property Recursive;
    property OnStartOperation;
    property OnFinishOperation;
    property OnOperationProgress;
    property OnEachFile;
  end;


procedure Register;

implementation

uses FileCtrl, consts;

procedure Register;
begin
  RegisterComponents('Backup Tools', [TMyBackup]);
end;

////////////////////////////////////////////////////////////

procedure TArchiverInterface.SetFileName( const aFileName : String );
begin
end;

function  TArchiverInterface.GetFileName : String;
begin
  Result := '';
end;

procedure TArchiverInterface.SetExtractPath( const aPath : String );
begin
end;

function  TArchiverInterface.GetExtractPath : String;
begin
  Result := '';
end;

procedure TArchiverInterface.SetLanguage( lang : TLanguage );
begin
end;

function  TArchiverInterface.GetLanguage : TLanguage;
begin
  Result := lgEnglish;
end;

procedure TArchiverInterface.Open;
begin
end;

procedure TArchiverInterface.Close;
begin
end;

function  TArchiverInterface.AddFile( const aFileName : String ) : Boolean;
begin
  Result := False;
end;

function  TArchiverInterface.AddFiles( files : TStrings ) : Boolean;
begin
  Result := False;
end;

function  TArchiverInterface.AddDirectory( const Directory : String ) : Boolean;
begin
  Result := False;
end;

procedure TArchiverInterface.ExtractFiles;
begin
end;

procedure TArchiverInterface.SetRecursive( val : Boolean );
begin
end;

procedure TArchiverInterface.Delete;
begin
end;

////////////////////////////////////////////////////////////

procedure TMessages.AssignTo(Dest: TPersistent);
begin
  if Dest is TMessages then
    with TMessages( Dest ) do begin
      SystemMessage            := Self.SystemMessage;
      NeedValidDrivePath       := Self.NeedValidDrivePath;
      InsertDiskXInUnit        := Self.InsertDiskXInUnit;
      DeletingFilesOfDrivePath := Self.DeletingFilesOfDrivePath;
      AskForDeletionOfFiles    := Self.AskForDeletionOfFiles;
      CouldNotDeleteAllFiles   := Self.CouldNotDeleteAllFiles;
      ThisDiskIsNotTheFirstOne := Self.ThisDiskIsNotTheFirstOne;
      ThePathDoesNotExist      := Self.ThePathDoesNotExist;
      CopyOf                   := Self.CopyOf;
      CouldNotWriteInFile      := Self.CouldNotWriteInFile;
      ThisDiskContainsNoBackup := Self.ThisDiskContainsNoBackup;
      WrongBackupSet           := Self.WrongBackupSet;
      WrongBackupName          := Self.WrongBackupName;
      Aborted                  := Self.Aborted;
      ReopeningTables          := Self.ReopeningTables;
      FCompressingFile         := Self.FCompressingFile;
      FUncompressingFile       := Self.FUncompressingFile;
      FNeedBackupName          := Self.FNeedBackupName;
    end;
  inherited AssignTo( Dest );
end;

constructor TMessages.Create;
begin
  inherited;
  SetLanguage( lgEnglish );
end;

procedure TMessages.SetLanguage( language : TLanguage );
begin
  case language of
    lgEnglish:
      begin
        SystemMessage            := 'System Message';
        NeedValidDrivePath       := 'You must give a valid path to the backup drive';
        InsertDiskXInUnit        := 'Please, insert disk #%d in drive %s:';
        DeletingFilesOfDrivePath := 'Deleting files of backup drive...';
        AskForDeletionOfFiles    := 'Backup drive contains files. Do you want to delete them ?';
        CouldNotDeleteAllFiles   := 'Could not delete all files !';
        ThisDiskIsNotTheFirstOne := 'This disk is not the first of the backup disks set.';
        ThePathDoesNotExist      := 'The path "%s" does not exist';
        CopyOf                   := 'Copy of %s';
        CouldNotWriteInFile      := 'Could not write in file %s';
        ThisDiskContainsNoBackup := 'This disk does not contain any backup.';
        WrongBackupSet           := 'This disk does not belong to the backup set: %s';
        WrongBackupName          := 'This disk contains a backup of "%s" instead of "%s".';
        Aborted                  := 'Aborted!';
        ReopeningTables          := 'Reopening Tables...';
        CompressingFile          := 'Compression of %s';
        UncompressingFile        := 'Decompression of %s';
        NeedBackupName           := 'I need a name for the Backup (property BackupName)';
      end;
    lgFrench:
      begin
        SystemMessage            := 'Message système';
        NeedValidDrivePath       := 'Vous devez fournir un chemin d''accès valide pour l''unité de sauvegarde';
        InsertDiskXInUnit        := 'Veuillez insérer la disquette n° %d dans l''unité %s:';
        DeletingFilesOfDrivePath := 'Suppression des fichiers sur l''unité de sauvegarde...';
        AskForDeletionOfFiles    := 'L''unité de sauvegarde contient des fichiers. Désirez-vous les supprimer ?';
        CouldNotDeleteAllFiles   := 'Impossible de supprimer tous les fichiers !';
        ThisDiskIsNotTheFirstOne := 'Cette disquette n''est pas la première du jeu de sauvegarde.';
        ThePathDoesNotExist      := 'Le chemin "%s" n''existe pas';
        CopyOf                   := 'Copie de %s';
        CouldNotWriteInFile      := 'Impossible d''écrire dans le fichier %s';
        ThisDiskContainsNoBackup := 'Cette disquette ne contient pas de sauvegarde';
        WrongBackupSet           := 'Cette disquette n''appartient pas au jeu de sauvegardes: %s';
        WrongBackupName          := 'Cette disquette contient une sauvegarde de "%s" au lieu de "%s".';
        Aborted                  := 'Abandon !';
        ReopeningTables          := 'Réouverture des tables...';
        CompressingFile          := 'Compression de %s';
        UncompressingFile        := 'Decompression de %s';
        NeedBackupName           := 'J''ai besoin d''un nom pour la Sauvegarde (propriété BackupName)';
      end;
    lgChinese: // Thanks to Huanlin Tsai (Taiwan)
      begin
        // Traditional Chinese constants (in BIG-5 code).
        SystemMessage            := '°T®§';
        NeedValidDrivePath       := '¥Øªº¸ô®|¤£¦s¦b!';
        InsertDiskXInUnit        := '½Ð±N²Ä #%d ±iºÏ¤ù©ñ¤JºÏºÐ¾÷ %s:';
        DeletingFilesOfDrivePath := '§R°£³Æ¥÷ºÏºÐ¾÷¤¤ªº©Ò¦³ÀÉ®×...';
        AskForDeletionOfFiles    := '³Æ¥÷ºÏ¤ù¤¤¦³¨ä¥LÀÉ®×¦s¦b, ­n§R°£¥L­Ì¶Ü?';
        CouldNotDeleteAllFiles   := 'µLªk§R°£©Ò¦³ªºÀÉ®×!';
        ThisDiskIsNotTheFirstOne := '³o±iºÏ¤ù¤£¬O³Æ¥÷ºÏ¤ùªº²Ä¤@¤ù¡C';
        ThePathDoesNotExist      := '¥Ø¿ý "%s" ¤£¦s¦b¡C';
        CopyOf                   := '¥¿¦b«þ¨© %s';
        CouldNotWriteInFile      := 'µLªk¼g¤JÀÉ®× %s¡C';
        ThisDiskContainsNoBackup := '³o±iºÏºÐ¤ù¨S¦³¥ô¦ó³Æ¥÷¸ê®Æ¡C';
        WrongBackupSet           := '³o±iºÏºÐ¤ù¤£ÄÝ©ó³o­Ó³Æ¥÷ÀÉ: %s¡C';
        WrongBackupName          := '³o±iºÏºÐ¤ù¬OÄÝ©ó "%s" ªº³Æ¥÷¸ê®Æ, ¦Ó¤£¬O "%s" ªº¡C';
        Aborted                  := '§@·~¨ú®ø!';
        ReopeningTables          := '­«·s¶}±Ò¸ê®Æªí...';
        FCompressingFile         := '¥¿¦bÀ£ÁY %s';
        FUncompressingFile       := '¥¿¦b¸ÑÀ£ÁY %s';
        FNeedBackupName          := '½Ð´£¨Ñ³Æ¥÷¦WºÙ (ÄÝ©Ê BackupName)¡C';
      end;
    lgChineseGB :
      begin
        // Traditional Chinese constants (in BIG-5 code).
        SystemMessage            := 'Ñ¶Ï¢';
        NeedValidDrivePath       := 'Ä¿µÄÂ·¾¶²»´æÔÚ!';
        InsertDiskXInUnit        := 'Çë½«µÚ #%d ÕÅ´ÅÅÌ·ÅÈë´Åµú»ú %s:';
        DeletingFilesOfDrivePath := 'É¾³ý±¸·Ý´Åµú»úÖÐµÄËùÓÐµµ°¸...';
        AskForDeletionOfFiles    := '±¸·Ý´ÅÆ¬ÖÐÓÐÆäËûµµ°¸´æÔÚ, ÒªÉ¾³ýËûÃÇÂð?';
        CouldNotDeleteAllFiles   := 'ÎÞ·¨É¾³ýËùÓÐµÄµµ°¸!';
        ThisDiskIsNotTheFirstOne := 'ÕâÕÅ´ÅÅÌ²»ÊÇ±¸·Ý´ÅÅÌµÄµÚÒ»Æ¬¡£';
        ThePathDoesNotExist      := 'Ä¿Â¼ "%s" ²»´æÔÚ¡£';
        CopyOf                   := 'ÕýÔÚ¿½±´ %s';
        CouldNotWriteInFile      := 'ÎÞ·¨Ð´Èëµµ°¸ %s¡£';
        ThisDiskContainsNoBackup := 'ÕâÕÅ´ÅµúÅÌÃ»ÓÐÈÎºÎ±¸·Ý×ÊÁÏ¡£';
        WrongBackupSet           := 'ÕâÕÅ´ÅµúÅÌ²»Êôì¶Õâ¸ö±¸·Ýµµ: %s¡£';
        WrongBackupName          := 'ÕâÕÅ´ÅµúÅÌÊÇÊôì¶ "%s" µÄ±¸·Ý×ÊÁÏ, ¶ø²»ÊÇ "%s" µÄ¡£';
        Aborted                  := '×÷ÒµÈ¡Ïû!';
        ReopeningTables          := 'ÖØÐÂ¿ªÆô×ÊÁÏ±í...';
        FCompressingFile         := 'ÕýÔÚÑ¹Ëõ %s';
        FUncompressingFile       := 'ÕýÔÚ½âÑ¹Ëõ %s';
        FNeedBackupName          := 'ÇëÌá¹©±¸·ÝÃû³Æ (ÊôÐÔ BackupName)¡£';
      end;
    lgPortuguese: // Thanks to Hugo Souza
      begin
        // If it looks strange do not change. It's designed for page 850
        SystemMessage            := 'Mensagem do sistema';
        NeedValidDrivePath       := 'Forneça um caminho válido para o drive de backup';
        InsertDiskXInUnit        := 'Por favor, insira o disco #%d no drive %s:';
        DeletingFilesOfDrivePath := 'Excluindo arquivos no drive de backup...';
        AskForDeletionOfFiles    := 'Disco destino contém arquivos. Deseja excluí-los ?';
        CouldNotDeleteAllFiles   := 'Não foi possível excluir todos os arquivos !';
        ThisDiskIsNotTheFirstOne := 'Este não é o primeiro disco do backup.';
        ThePathDoesNotExist      := 'O caminho "%s" não existe';
        CopyOf                   := 'Copia de %s';
        CouldNotWriteInFile      := 'Impossível gravar no arquivo %s';
        ThisDiskContainsNoBackup := 'Este disco não contém backup.';
        WrongBackupSet           := 'Este disco não pertence ao backup: %s';
        WrongBackupName          := 'Este disco pertence ao backup "%s" e NÃO ao "%s".';
        Aborted                  := 'Cancelado!';
        ReopeningTables          := 'Reabrindo Tabelas...';
        FCompressingFile         := 'Compressão de %s';
        FUncompressingFile       := 'Decompressão de %s';
        FNeedBackupName          := 'Preciso de um nome para o Backup (propriedade BackupName)';
      end;
    lgGerman: // Thanks to Oliver Buschjost (buschjost@geocities.com)
      begin
        SystemMessage            := 'Systemnachricht';
        NeedValidDrivePath       := 'Sie müssen einen gültigen Pfad als Backuppfad angeben';
        InsertDiskXInUnit        := 'Bitte Diskette #%d in Laufwerk %s einlegen';
        DeletingFilesOfDrivePath := 'Löschen von Dateien auf dem Backuplaufwerk';
        AskForDeletionOfFiles    := 'Es existieren Dateien auf dem Backuplaufwerk. Sollen die Dateien gelöscht werden? ';
        CouldNotDeleteAllFiles   := 'Konnte nicht alle Dateien löschen...';
        ThisDiskIsNotTheFirstOne := 'Dies ist nicht die 1. Diskette des Backup-Sets.';
        ThePathDoesNotExist      := 'Das Verzeichnis "%s" Existiert nicht';
        CopyOf                   := 'Kopiere %s...';
        CouldNotWriteInFile      := 'Konnte nicht in folgende Datei schreiben: %s';
        ThisDiskContainsNoBackup := 'Auf dieser Diskette befindet sich kein Backup.';
        WrongBackupSet           := 'Diese Diskette gehört nicht zu diesem Backup-Set: %s';
        WrongBackupName          := 'Diese Diskette enthält ein Backup von "%s" anstatt von "%s".';
        Aborted                  := 'Abgebrochen!';
        ReopeningTables          := 'Erneutes Öffnen der Tabellen...';
        CompressingFile          := 'Komprimieren von %s';
        UncompressingFile        := 'Entkomprimieren von %s';
        NeedBackupName           := 'Ich benötige einen Namen für das Backup (Eigenschaft BackupName)';
      end;
    lgItalian: // Thanks to Gabriele Bigliardi (gbigliardi@manord.com)
      begin
        SystemMessage            := 'Messaggio di sistema';
        NeedValidDrivePath       := 'Devi dare un indirizzo valido per l''unità di backup';
        InsertDiskXInUnit        := 'Per favore, inserire il disco #%d nel drive %s:';
        DeletingFilesOfDrivePath := 'Cancellazione in corso dall''unità di backup...';
        AskForDeletionOfFiles    := 'L''unità di backup contiene dei files. Vuoi cancellarli ?';
        CouldNotDeleteAllFiles   := 'Non è possibile cancellare tutti i files !';
        ThisDiskIsNotTheFirstOne := 'Questo disco non è il primo del set dei dischi di backup';
        ThePathDoesNotExist      := 'L''indirizzo "%s" non esiste';
        CopyOf                   := 'Copia di %s';
        CouldNotWriteInFile      := 'Non riesco a scrivere nel file %s';
        ThisDiskContainsNoBackup := 'Questo disco non contiene un backup.';
        WrongBackupSet           := 'Questo disco non appartiene al set di backup: %s';
        WrongBackupName          := 'Questo disco contiene un backup di "%s" invece che "%s".';
        Aborted                  := 'Errore!';
        ReopeningTables          := 'Riapertura delle tabelle...';
        CompressingFile          := 'Compressione di %s';
        UncompressingFile        := 'Decompressione di %s';
        NeedBackupName           := 'Serve un nome per il backup (proprietà BackupName)';
      end;
    lgSpanish:
      begin
        FSystemMessage := 'Mensaje de Sistema';
        NeedValidDrivePath := 'Es necesario una ruta valida para respaldar la unidad';
        InsertDiskXInUnit := 'Favor de insertar el disco #%d en la unidad %s:';
        DeletingFilesOfDrivePath := 'Borrando archivos de respaldo de la unidad...';
        AskForDeletionOfFiles    := 'El disco destino tiene archivos. ¿ Deseas borrarlos ?';
        CouldNotDeleteAllFiles   := '¡ No es posible borrar todos los archivos !';
        ThisDiskIsNotTheFirstOne := 'El disco no es el primero del respaldo.';
        ThePathDoesNotExist      := 'La ruta "%s" no existe';
        CopyOf                   := 'Copia de %s';
        CouldNotWriteInFile      := 'No se puede escribir el archivo %s';
        ThisDiskContainsNoBackup := 'El disco no contiene ningun respaldo.';
        WrongBackupSet           := 'El dico no pertenece al respaldo: %s';
        WrongBackupName          := 'El disco contiene el respaldo "%s" en vez de "%s".';
        Aborted                  := '¡ Abortado !';
        ReopeningTables          := 'Reabriendo Tablas...';
        CompressingFile          := 'Comprimiendo a %s';
        UncompressingFile        := 'Decomprimiendo a %s';
        NeedBackupName           := 'Es necesario el nombre del respaldo (propiedad BackupName)';
      end;
    lgDutch:
      begin
        SystemMessage            := 'Systeembericht';
        NeedValidDrivePath       := 'U moet een geldig pad naar de back-up schijf opgeven ';
        InsertDiskXInUnit        := 'Steek schijf #%d in station %s: alstublieft';
        DeletingFilesOfDrivePath := 'Verwijder bestanden van back-up schijf...';
        AskForDeletionOfFiles    := 'De back-up schijf bevat bestanden.  Wilt u die verwijderen?';
        CouldNotDeleteAllFiles   := 'Kon niet alle bestanden verwijderen!';
        ThisDiskIsNotTheFirstOne := 'Deze schijf is niet de eerste schijf van de back-up verzameling.';
        ThePathDoesNotExist      := 'Het pad "%s" bestaat niet';
        CopyOf                   := 'Kopie van %s';
        CouldNotWriteInFile      := 'Kon niet schrijven in bestand  %s';
        ThisDiskContainsNoBackup := 'Deze schijf bevat geen back-ups.';
        WrongBackupSet           := 'Deze schijf behoort niet tot back-up verzameling: %s';
        WrongBackupName          := 'Deze schijf bevat een back-up van "%s" in plaats van "%s".';
        Aborted                  := 'Geannuleerd!';
        ReopeningTables          := 'Heropenen tabellen...';
        CompressingFile          := 'Compressie van %s';
        UncompressingFile        := 'Decompressie van %s';
        NeedBackupName           := 'Ik heb een naam nodig voor de back-up (eigenschap BackupName)';
      end;
    lgCzech:
      begin
        SystemMessage            := 'Systémové hlášení';
        NeedValidDrivePath       := 'Musíte správnì zadat cestu k zálohovacímu disku';
        InsertDiskXInUnit        := 'Prosím, vložte disk #%d do mechaniky %s:';
        DeletingFilesOfDrivePath := 'Mažou se soubory ze zálohovacího disku ...';
        AskForDeletionOfFiles    := 'Zálohovací disk obsahuje soubory. Smazat?';
        CouldNotDeleteAllFiles   := 'Nelze smazat všechny soubory!';
        ThisDiskIsNotTheFirstOne := 'Tento disk není první v posloupnosti.';
        ThePathDoesNotExist      := 'Cesta "%s" neexistuje';
        CopyOf                   := 'Kopíruje se %s';
        CouldNotWriteInFile      := 'Nelze zapisovat do souboru %s';
        ThisDiskContainsNoBackup := 'Tento disk neobsahuje žádnou zálohu.';
        WrongBackupSet           := 'Tento disk není souèástí zálohy: %s';
        WrongBackupName          := 'Tento disk obsahuje zálohu "%s", nikoli "%s".';
        Aborted                  := 'Pøerušeno!';
        ReopeningTables          := 'Otevírají se tabulky ...';
        CompressingFile          := 'Komprimuje se %s';
        UncompressingFile        := 'Dekomprimuje se %s';
        NeedBackupName           := 'Zadejte název zálohy (property BackupName)';
      end;
  end;
end;

////////////////////////////////////////////////////////////

procedure TMyBackup.Finish;
begin
  FDiskCount := 1;
  inherited;
end;

procedure TMyBackup.AskForDisk;
var
  msg : String;
begin
  msg := Format(Messages.InsertDiskXInUnit, [FDiskCount, FDrive]);
  if not InputQuery(Messages.SystemMessage, msg, FDrivePath) then
    DoAbort;
end;

constructor TMyBackup.Create( AOwner : TComponent );
begin
  inherited;
  FConfirmDelete    := True;
  FDrivePath        := 'A:\';
  FFilesPath        := 'C:\';
  ShowFileNames     := True;
  FInfosFileName    := kInfosFile;
  FOperation        := opNone;
  FDiskCount        := 1;
  FDisplayAbort     := True;
  FUseArchiver      := False;
  FMessages         := TMessages.Create;
  FLanguage         := lgEnglish;
  FFilesToBackup    := TStringList.Create;
  FFilesRestored    := TStringList.Create;
  FFilesToBackup.Duplicates := dupIgnore;
  FFilesRestored.Duplicates := dupIgnore;
end;

destructor  TMyBackup.Destroy;
begin
  FMessages.Free;
  FFilesToBackup.Free;
  FFilesRestored.Free;
  inherited;
end;

procedure TMyBackup.SetFilesToBackup( val : TStringList );
begin
  FFilesToBackup.Assign( val );
end;

procedure TMyBackup.SetFilesRestored( val : TStringList );
begin
  FFilesRestored.Assign( val );
end;

procedure TMyBackup.Backup;
begin
  if BackupName = '' then
    raise Exception.Create( Messages.NeedBackupName );
  FDiskCount := 1;
  FOperation := opBackup;
  Start;    // after start, the ProgressForm should be created.
  try
    SetCaption( BackupCaption );
    WriteFileName('');
    CheckPath;
    CheckDrive;
    PrecalcDirSize( FFilesPath );
    WriteFiles;
  finally
    FOperation := opNone;
    Finish;
  end;
end;

procedure TMyBackup.Restore;
begin
  FDiskCount := 1;
  FOperation := opRestore;
  Start;    // after start, the ProgressForm should be created.
  try
    SetCaption( RestoreCaption );
    WriteFileName('');
    CheckPath;
    CheckDrive;
    ReadFiles;
  finally
    FOperation := opNone;
    Finish;
  end;
end;

procedure TMyBackup.CheckDrive;
var
  sRoot: string;
  isRemovable : Boolean;
begin
  while True do
  begin
    if length(FDrivePath) < 2 then
      raise Exception.Create(Messages.NeedValidDrivePath);
    sRoot := AppendSlash(ExtractFileDrive(FDrivePath));
    isRemovable := GetDriveType(PChar(sRoot)) = DRIVE_REMOVABLE;
    FDrive := UpperCase(FDrivePath)[1];
    if isRemovable and not DiskInDrive(FDrive) then
      begin
        AskForDisk;
        Continue;
      end;
    if not DirectoryExists(FDrivePath) then
      raise Exception.Create(Messages.NeedValidDrivePath);
    if not (UpperCase(FDrivePath)[1] in ['A'..'Z']) or (FDrivePath[2] <> ':') then
      raise Exception.Create(Messages.NeedValidDrivePath);
    case FOperation of
      opBackup:
        begin
          if CheckDiskContent then
            Break
          else
            AskForDisk;
        end;
      opRestore:
        begin
          Break;
        end
    else
      Break;
    end;
  end;
end;

procedure TMyBackup.NextDisk;
var
  SL : TStringList;
  i : Integer;
begin
  while True do
  begin
    AskForDisk;
    if length(FDrivePath) < 2 then
      raise Exception.Create(Messages.NeedValidDrivePath);
    if (UpperCase(FDrivePath)[1] in ['A'..'Z']) and (FDrivePath[2]=':') then
    begin
      FDrive := UpperCase(FDrivePath)[1];
      if DiskInDrive(FDrive) then
      begin
        if (FOperation = opBackup) then
        begin
          If Assigned(FOnDiskChanged) Then
            FOnDiskChanged(Self);
          if CheckDiskContent then
            Break;
        end
        else if (FOperation = opRestore) then
        begin
          SL := TStringList.Create;
          try
            GetInfos( SL );
            i := 0;
            if (SL.Values['Disk#'] <> '') then
              i := StrToInt(SL.Values['Disk#']);
            if (SL.Values['Backup.Name'] <> FCurBackup) then
            begin
              i := 0;
              MessageDlg( Format(Messages.WrongBackupName, [SL.Values['Backup.Name'], FCurBackup]),
                          mtError, [mbOk], 0 );
            end;
            if (SL.Values['Id'] <> Id) then
            begin
              i := 0;
              MessageDlg( Format(Messages.WrongBackupSet, [IdLabel]), mtError, [mbOk], 0 );
            end;
          finally
            SL.Free;
          end;
          if i = FDiskCount then
            Break;
        end;
      end;
    end
    else
      raise Exception.Create(Messages.NeedValidDrivePath);
  end;
end;

function TMyBackup.CheckDiskContent : Boolean;
  function DeleteFiles : Boolean;
  var
    SR : TSearchRec;
    Found : Integer;
    path : String;
    old, old_rec : Boolean;
  begin
    result := True;
    path := RemoveSlash( FDrivePath );
    if UpperCase(path) = 'C:' then
      begin
        Result := False;
        Exit;
      end;
    // Remove the files in the directory
    WriteFileName( Messages.DeletingFilesOfDrivePath );
    SendEvents := False;
    old := FProgressForm.ProgressBar1.Visible;
    FProgressForm.ProgressBar1.Visible := False;
    Found := FindFirst( path+'\*.*', faAnyFile, SR );
    try
      while Result and (Found = 0)  do
      begin
        if (SR.Name<>'.') and (SR.Name <> '..') then
        begin
          if (SR.Attr and faDirectory) <> 0 then
          begin
            // Delete subdirectory
            old_rec := Recursive;
            Recursive := True;
            try
              result := result and DeleteDirectory( path+'\'+SR.Name );
            finally
              Recursive := old_rec;
            end;
          end
          else
          begin
            // Remove attributes that could prevent us from deleting the file
            FileSetAttr( path+'\'+SR.Name, FileGetAttr(path+'\'+SR.Name) and
                                             not (faReadOnly or faHidden) );
            // Delete file
            if not DeleteFile( path+'\'+SR.Name ) then
              result := False;
          end;
        end;
        Found := FindNext( SR );
      end;
    finally
      FProgressForm.ProgressBar1.Visible := old;
      SendEvents := True;
      FindClose(SR);
    end;
  end;
var
  sRoot: string;
begin
  result := True;
  sRoot := AppendSlash(ExtractFileDrive(FDrivePath));
  // If target is not a removable drive (maybe a hard disk), then don't delete files.
  if GetDriveType(PChar(sRoot)) <> DRIVE_REMOVABLE then
    Exit;
  if not IsDirectoryEmpty( FDrivePath ) then
  begin
    if ConfirmDelete and
       (MessageDlg( Messages.AskForDeletionOfFiles, mtConfirmation, [mbYes, mbNo], 0 ) = mrNo) then
    begin
      result := False;
      Exit;
    end;
    if not DeleteFiles then
    begin
      result := False;
      MessageDlg( Messages.CouldNotDeleteAllFiles, mtError, [mbOk], 0 );
    end;
  end;
end;

function TMyBackup.CheckFirstDisk : Boolean;
var
  old : TOperation;
  SL : TStringList;
begin
  result := False;
  old := FOperation;
  FOperation := opNone;
  try
    while not Result do
    begin
      CheckDrive;
      SL := TStringList.Create;
      try
        if GetInfos(SL) then
        begin
          if (SL.Values['Id'] <> Id) then
          begin
            MessageDlg( Format(Messages.WrongBackupSet, [IdLabel]), mtError, [mbOk], 0 );
          end
          else
            begin
            FCurBackup := SL.Values['Backup.Name'];
            if (SL.Values['Disk#'] <> '') and
               (StrToInt(SL.Values['Disk#']) = 1) then
            begin
              result := True;
              if (SL.Values['ArchiveSize'] <> '') then
                begin
                  FBytesToCopy := StrToInt(SL.Values['ArchiveSize']);
                  FBytesCopied := 0;
                end
              else
                FBytesToCopy := 0;
            end
            else
            begin
              MessageDlg( Messages.ThisDiskIsNotTheFirstOne, mtWarning, [mbOk], 0);
            end;
          end;
        end;
      finally
        SL.Free;
      end;
      if not Result then
        AskForDisk;
    end;
  finally
    FOperation := old;
  end;
end;

procedure TMyBackup.CheckPath;
begin
  case FOperation of
    opBackup:
      begin
        if (FilesToBackup.Count = 0) and
           not DirectoryExists( FFilesPath ) then
          raise Exception.CreateFmt(Messages.ThePathDoesNotExist, [FFilesPath]);
      end;
    opRestore:
      begin
        ForceDirectories(FFilesPath);
      end;
  end;
end;

procedure TMyBackup.WriteFiles;
var
  SR : TSearchRec;
  Found : Integer;
  path : String;
  i : Integer;
  tmp : String;
begin
  // Check if we use the Archiver
  if UseArchiver then
    begin
      tmp := GetTempDir;
      GetArchiver.FileName := tmp+'\'+BackupName;
      try
        if FilesToBackup.Count > 0 then
          GetArchiver.AddFiles( FilesToBackup )
        else
          GetArchiver.AddDirectory( FilesPath );
        GetArchiver.Close;
        FBytesToCopy := GetFileSize( GetArchiver.FileName );
        FCurrentFile := GetArchiver.FileName;
        WriteFile( GetArchiver.FileName );
        FCurrentFile := '';
        WriteInfos;
      finally
        GetArchiver.Delete;
        RemoveDir( tmp );
      end;
      Exit;
    end;
  // Use first the list of files for backup
  if FilesToBackup.Count > 0 then
  begin
    for i := 0 to FilesToBackup.Count - 1 do
    begin
      FCurrentFile := ExtractFileName(FilesToBackup.Strings[i]);
      WriteFile( FilesToBackup.Strings[i] );
    end;
    FCurrentFile := '';
    WriteInfos;
    Exit;
  end;
  // Otherwise, backup all the filtered file from
  // the FilesPath directory
  path := RemoveSlash(FilesPath);
  Found := FindFirst( path+'\'+Filter, faAnyFile, SR );
  try
    while (Found = 0) do
    begin
      if (SR.Name<>'.') and (SR.Name <> '..') then
      begin
        if (SR.Attr and faDirectory) = 0 then
        begin
          FCurrentFile := SR.Name;
          WriteFile( path + '\' + SR.Name );
        end;
      end;
      Found := FindNext( SR );
    end;
    FCurrentFile := '';
    WriteInfos;
  finally
    FindClose(SR);
  end;
end;

procedure TMyBackup.WriteFile( const FileName : String );
  procedure ChangeDisk;
  begin
    WriteInfos;
    Inc( FDiskCount );
    NextDisk;
  end;

  function ComputeDiskSpace : Integer;
  begin
    result := DiskFree(Ord(FDrive) - $40) - kMinDiskSpace;
  end;

const
  ChunkSize = 8192;
var
  Source, Dest   : TFileStream;
  size           : Longint;
  toCopy         : Longint;
  DestFileName   : String;
  DiskSpace      : Integer;
  ChangingDest   : Boolean;
  Processed      : Integer;
  Buffer         : array [0..ChunkSize] of Byte;
begin
  ChangingDest := False;
  { compute the freespace left on the dest disk }
  DiskSpace := ComputeDiskSpace;
  Size := FBytesToCopy;
  WriteFileName( Format( Messages.CopyOf, [ExtractFilename(FileName)]));

  source := TFileStream.Create( FileName, fmOpenRead or fmShareDenyWrite );
  try
    DestFileName := RemoveSlash(FDrivePath)+'\'+ExtractFileName(FileName);
    Dest := TFileStream.Create( DestFileName, fmCreate );
    try
      repeat
        if FCancelOperation then
          DoAbort;
        if (Source.Size-Source.Position) < ChunkSize then
          toCopy := Source.Size-Source.Position
        else
          toCopy := ChunkSize;
        source.ReadBuffer( Buffer, toCopy );
        // if there's not enough space
        if DiskSpace < toCopy then
        begin
          ChangingDest := True;
          // close previous file
          Dest.Free;
          // change the disk
          ChangeDisk;
          WriteFileName( Format( Messages.CopyOf, [ExtractFilename(FileName)]));
          // reopen previous file on new disk
          Dest := TFileStream.Create( DestFileName, fmCreate );
          ChangingDest := False;
          DiskSpace := ComputeDiskSpace;
        end;
        if Dest.Write( Buffer, toCopy ) <> toCopy then
          raise Exception.CreateFmt(Messages.CouldNotWriteInFile, [DestFileName]);
        Inc( FBytesCopied, toCopy );
        if Size > 0 then
          Processed := Round(FBytesCopied/Size*100)
        else
          Processed := 0;
        SetProgress( Round(Processed) );
        Dec( DiskSpace, toCopy );
      until source.Position = source.Size;
    finally
      if not ChangingDest then
        Dest.Free;
    end;
  finally
    Source.Free;
  end;
end;

procedure TMyBackup.WriteInfos;
begin
  with TStringList.Create do
  begin
    try
      Values['User.Name'] := UserName;
      Values['User.Company'] := UserCompany;
      Values['User.Licence'] := UserLicence;
      Values['Version'] := Version;
      Values['ID'] := ID;
      Values['Date'] := DateToStr(Now);
      Values['Time'] := TimeToStr(Now);
      Values['Backup.Name'] := BackupName;
      Values['Disk#'] := IntToStr(FDiskCount);
      Values['ArchiveSize'] := IntToStr(FBytesToCopy);
      if FCurrentFile <> '' then
        Values['FileIncompletelySaved'] := FCurrentFile;
      SaveToFile( RemoveSlash(DrivePath)+'\'+InfosFileName );
    finally
      Free;
    end;
  end;
end;

procedure TMyBackup.FileExtractedFromArchive( Sender : TObject; const FileName, DestPath : String; Size : Integer );
begin
  FilesRestored.Add( AppendSlash(DestPath) + ExtractFileName(FileName) );
end;

procedure TMyBackup.ReadFiles;
var
  SR : TSearchRec;
  Found : Integer;
  path : String;
  tmp, oldPath: String;
begin
  if not CheckFirstDisk then
    Exit;
  FilesRestored.Clear;
  if UseArchiver then
    begin
      tmp := GetTempDir;
      oldPath := FFilesPath;
      FFilesPath := tmp;
    end;
  try
    while True do
    begin
      path := RemoveSlash(DrivePath);
      Found := FindFirst( path+'\'+Filter, faAnyFile, SR );
      try
        while (Found = 0) do
        begin
          if (SR.Name<>'.') and (SR.Name <> '..') then
          begin
            if (SR.Attr and faDirectory) = 0 then
            begin
              FCurrentFile := SR.Name;
              if SR.Name <> InfosFileName then
                ReadFile( path + '\' + SR.Name );
              FilesRestored.Add( FFilesPath + '\' + SR.Name );
            end;
          end;
          Found := FindNext( SR );
        end;
        FCurrentFile := '';
      finally
        FindClose(SR);
      end;
      if FOneMoreDisk then
      begin
        Inc(FDiskCount);
        NextDisk;
      end
      else
        Break;
    end;
    if UseArchiver then
      begin
        GetArchiver.FileName := tmp + '\' + BackupName;
        FilesRestored.Clear;
        GetArchiver.ExtractPath := oldPath;
        GetArchiver.ExtractFiles;
      end;
  finally
    if UseArchiver then
      begin
        FFilesPath := oldPath;
        GetArchiver.Delete;
        RemoveDir( tmp );
      end;
  end;
end;

procedure TMyBackup.ReadFile( const FileName : String );
const
  ChunkSize : Integer = 8192;
var
  toCopy         : Longint;
  Source, Dest   : TFileStream;
  size           : Longint;
  DestFileName   : String;
  Processed      : Integer;
begin
  if FBytesToCopy = 0 then
  begin
    { Compute the length of the FCopyFrom file }
    Size := GetFileSize( FileName );
    FBytesCopied := 0;
  end
  else
    Size := FBytesToCopy;
  WriteFileName( Format(Messages.CopyOf, [ExtractFilename(FileName)]));

  source := TFileStream.Create( FileName, fmOpenRead or fmShareDenyWrite );
  try
    DestFileName := RemoveSlash(FFilesPath)+'\'+ExtractFileName(FileName);
    // Check if file was already started otherwise delete it
    if FilesRestored.IndexOf( DestFileName ) < 0 then
      DeleteFile( DestFileName );
    if FileExists(DestFileName) then
      Dest := TFileStream.Create( DestFileName, fmOpenWrite or fmShareDenyRead or fmShareDenyWrite )
    else
      Dest := TFileStream.Create( DestFileName, fmCreate );
    try
      Dest.Seek( 0, soFromEnd );
      repeat
        if FCancelOperation then
          DoAbort;
        if (Source.Size-Source.Position) < ChunkSize then
          toCopy := Source.Size-Source.Position
        else
          toCopy := ChunkSize;
        Dest.CopyFrom( source, toCopy );
        Inc( FBytesCopied, toCopy );
        if Size > 0 then
          Processed := Round(FBytesCopied*100/Size)
        else
          Processed := 0;
        SetProgress( Round(Processed) );
      until source.Position = source.Size;
    finally
      Dest.Free;
    end;
  finally
    Source.Free;
  end;
end;

function TMyBackup.GetInfos( SL : TStringList ) : Boolean;
var
  tmp : String;
  old : TOperation;
begin
  result := False;
  old := FOperation;
  FOperation := opNone;
  try
    CheckDrive;
    tmp := RemoveSlash(FDrivePath)+'\'+InfosFileName;
    if FileExists(tmp) then
    begin
      with SL do
      begin
        LoadFromFile( tmp );
        result := True;
        FOneMoreDisk := Values['FileIncompletelySaved'] <> '';
      end;
    end
    else
      MessageDlg(Messages.ThisDiskContainsNoBackup, mtWarning, [mbOk], 0 );
  finally
    FOperation := old;
  end;
end;

procedure TMyBackup.PrecalcDirSize( const dir : String );
var
  i : Integer;
  oldRec : Boolean;
begin
  // Use first the list of files for backup
  if FilesToBackup.Count > 0 then
  begin
    FBytesCopied := 0;
    FBytesToCopy := 0;
    for i := 0 to FilesToBackup.Count - 1 do
      if FileExists( FilesToBackup.Strings[i] ) then
        Inc( FBytesToCopy, GetFileSize(FilesToBackup.Strings[i]) )
      else if UseArchiver and Recursive and DirectoryExists( FilesToBackup.Strings[i] ) then
        Inc( FBytesToCopy, GetDirectorySize(FilesToBackup.Strings[i]) );
    Exit;
  end;
  // otherwise calc the content of the dir
  oldRec := Recursive;
  try
    Recursive := not UseArchiver;
    inherited;
  finally
    Recursive := oldRec;
  end;
end;

procedure TMyBackup.SetLanguage( lang : TLanguage );
begin
  if FLanguage <> lang then
    begin
      FLanguage := lang;
      if not( csLoading in ComponentState ) then
        FMessages.SetLanguage( lang );
    end;
end;

procedure TMyBackup.SetMessages( msg : TMessages );
begin
  FMessages.Assign( msg );
end;

procedure TMyBackup.DoAbort;
begin
  if DisplayAbort then
    raise Exception.Create(Messages.Aborted)
  else
    Abort;
end;

function  TMyBackup.CreateArchiver : TArchiverInterface;
begin
  Result := TArchiverInterface.Create( Self );
end;

function  TMyBackup.GetArchiver : TArchiverInterface;
begin
  if not Assigned( FArchiver ) then
    begin
      FArchiver := CreateArchiver;
    end;
  FArchiver.Language := Language;
  FArchiver.OnAddFile := AddFileEvent;
  FArchiver.OnExtractFile := ExtractFileEvent;
  FArchiver.OnFileProgress := FileProgressEvent;
  FArchiver.OnFileExtracted := FileExtractedFromArchive;
  FArchiver.SetRecursive( Recursive );
  Result := FArchiver;
end;

function TMyBackup.GetTempDir : String;
var
  TmpPath : String;
  TmpPathLen : Integer;
begin
  TmpPathLen := 512;
  SetLength( TmpPath, TmpPathLen + 1 );
  TmpPathLen := GetTempPath( TmpPathLen, PChar(TmpPath) );
  if TmpPathLen = 0 then
    TmpPath := 'c:\'
  else
    SetLength( TmpPath, TmpPathLen );
  if TmpPath[length(TmpPath)] <> '\' then
    TmpPath := TmpPath + '\';
  Result :=  Format('%s~Backup-%d', [TmpPath, GetTickCount]);
  CreateDir( Result );
end;

procedure TMyBackup.AddFileEvent( Sender : TObject; const FileName : String; Size : Integer );
begin
  WriteFileName( Format( Messages.CompressingFile, [ExtractFilename(FileName)]));
end;

procedure TMyBackup.ExtractFileEvent( Sender : TObject; const FileName : String; Size : Integer);
begin
  WriteFileName( Format( Messages.UncompressingFile, [ExtractFilename(FileName)]));
end;

procedure TMyBackup.FileProgressEvent( Sender : TObject; Percent : Integer );
begin
  SetProgress( Percent );
end;

function  TMyBackup.GetUseArchiver : Boolean;
begin
  if not(csDesigning in ComponentState) then
    begin
      if GetArchiver.ClassName = 'TArchiverInterface' then
        Result := False
      else
        Result := FUseArchiver;
    end
  else
    Result := FUseArchiver;
end;

procedure TMyBackup.Notification(AComponent: TComponent;
          Operation: Classes.TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FArchiver) then
    Archiver := nil;
end;

end.
