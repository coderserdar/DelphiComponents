unit CopyFile;

{ This components are freeware. You may use it, distribute it and modify it, but
you may not charge for it.
In case of modifications you must mail me (Lars_Nielsen@dk-online.dk) a copy of
the modifications. The reason are simple: Any changes that improve this free-
ware component should be to benefit for everybody, not only you. That way you
can be pretty sure, that this component has few errors and much functionality.
In case of modifications, you will be on the credits list beneath:}

{ Version 1.0 by Lars Fløe Nielsen:
          This version is the base version. You can copy and move. The following
          properties are included:
                     Caption
                     CopyFrom
                     CopyTo
                     MoveFile
                     Name
                     OnNotExists
                     Progress
                     ShowFileNames
                     Tag}

{ Version 1.1. (10-04-97) Bug found and corrected by Jinsuck, Choi, South Korea:
          The component can now copy from a network drive where your rights are
          Read-only. Before this, the component made an exception-error. }

{ Version 1.2 (21-04-97) Bas Swemle, Netherlands added lines to the component:
          The component can now checks if the directory exists. If not, a
          dialog pop up, asking you if you want to create the dir. If not, a
          warning will be displayed.}
{ Version 1.2 (21-04-97) Russel Havens, USA. Suggested the following:
          A line that could transfer the file's original time and date to the
          destination file.
          It's not all the time the user want's this, so I added a new property
          TransferTimeDate of Boolean, which by default is set to True.}
{ Version 1.2 (21-04-97) Lars Fløe Nielsen. Complete help file added to the
          component.}
{ Version 1.3 (09-05-97) David Pilcher, ???. He found a fatal bug. If you
          want to move the file, but by some unexpected error it cannot, you
          may loose the original. The reason is simple: The delete call was
          in the finally, which means that the file would be deleted in every
          case. The Delete call is now still in the finally, but will only be
          executed if the try was exited normally.}
{ Version 1.4 (09-06-97) Extended by Morgan Martinet (France) mmm@imaginet.fr
          Use Start/Finish if you wish to copy multiple files and avoid
          the flickering of the progress form caused by Create/Free at
          each copy.
          Uses Events to be notified of the begin/end of a process (copy, deletion...)
          You can use this events to show the progress in a form you designed
          especially instead of the default progress form (set the property Progress to False).
          You can temporarily suspend this events by setting the property
          SendEvents to False.
          I added some useful functions :
            CopyDirectory : copies a directory and its subdirectories
            GetDirectorySize : calc the size of a directory, including its subdirectories
            GetDirectoryCount : calc the number of files contained in a directory, including its subdirectories
            DeleteDirectory : delete a directory, including its subdirectories
            IsDirectoryEmpty : check if a directory is empty
            IsDirectoryInUse : This function checks if a Directory is in use.
                               It travels each subdirectory and tries to open each file
                               in exclusive mode. If it fails, it means that someone has
                               already locked this file, and it won't be possible to delete
                               the directory containing it.
            FindFile : This function tries to search for a file/directory recursively
                       from a specified directory

          All these functions use the property Recursive in order to handle the
          subdirectories or not.
          There's a new property TransferFileAttributes which transfers the attributes
          (ReadOnly/Hidden...) of the original file to the copy.
          You can specify the kind of progression during operations with the property ProgressKind :
            pkFile = the progress ration is calculated file by file
            pkDirectory = the progress ratio is calculated for the whole directory to copy
                          it avoids a the flickering of the progressbar when there are a lot
                          of small files to copy.

}
{ Version 1.5 (09/10/97)  Morgan Martinet (France) mmm@imaginet.fr
          - property Filter added, in order to let you filter the functions that
            works on directories and sub-directories like CopyDirectory and CalcDirectorySize.

          - procedure CopyFiles added, that let you copy a list of files in a directory
}

{ Version 1.6 (08/04/998)  Morgan Martinet (France) mmm@imaginet.fr
          - procedure PrecalcDirSize made virtual
          - added function GetFileSize
          - added procedure BringToFront
}
{ Version 1.7 (10/06/998)  Morgan Martinet (France) mmm@imaginet.fr
          - procedures Start and Finish made virtual
          - Used a TFileStream instead of Basic IO. It resolves the
            difference between Delphi2 and Delphi3.
}
{$IFNDEF WIN32}
  ERROR! This unit should only be used on 32 - bits systems!
{$ENDIF}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Extctrls, Filectrl, prgform; { PrgForm is the progress bar form }

const
  sDirectoryDoesNotExist = 'Le répertoire "%s" n''existe pas';

type
  // processed is a percentage
  TOperationProgressEvent = procedure ( Sender : TObject;
                                        processed : Integer;
                                        var Cancel : Boolean ) of object;
  // called once for each file
  TEachFileEvent = procedure ( Sender : TObject; const FileName : String ) of object;

  TProgressKind = (pkFile, pkDirectory);

  TCustomCopyFile = class(TComponent) { This class is a new base class }
  private
    { Private declarations }
  protected
    { Protected declarations }
    FCopyFrom : String;         { Source file }
    FCopyTo   : String;         { Target file }
    FShowProgress : Boolean;    { If True the progress bar is shown }
    FProcessed : Longint;       { How much is processed is stored here
                                 (in percent 1% to 100%). This is mostly for
                                 the user which want to make his own progress
                                 bar }
    FOnNotExists : String;      { Errormessage if the file do not exists }
    FShowFileNames : Boolean;   { File names if true and FShowProgress is true}
    FCaption : String;          { Caption on the progress bar form}
    FMoveFile : Boolean;        { Moves the file if true }
    FTransferTimeDate : Boolean;{ Transfers the file time and date if true }
    FStartCount : Integer;      { Number of call of the Start/Finish methods }
    FOnStartOperation : TNotifyEvent;
    FOnFinishOperation : TNotifyEvent;
    FOnOperationProgress : TOperationProgressEvent;
    FOnEachFile : TEachFileEvent;
    FCopyMultipleFiles : Boolean;
    FProgressForm   : TProgform;
    FTransferFileAttributes : Boolean;
    FRecursive : Boolean;
    FSendEvents : Boolean;
    FCancelOperation : Boolean; // Set it to True if you want to cancel the current operation
    FProgressKind : TProgressKind;
    FBytesToCopy : Integer;  // Total of bytes to copy
    FBytesCopied : Integer;  // Total of bytes already copied
    FFilter : String; // MSDos filter

    procedure PrecalcDirSize( const dir : String ); virtual;
    function GetIsWorking : Boolean;
    procedure BringToFront;

  public
    constructor Create(AOWner: TComponent); override; { MUST BE IN EVERY COMPONENT }
    destructor Destroy; override;                     { MUST BE IN EVERY COMPONENT }
    procedure CopyNow;                                { Main procedure }
    procedure Start; virtual;                         { Start copy of multiple files, so we'll create the Progress form only once}
    procedure Finish; virtual;                        { Finish the copy of multiple files, so we'll destroy the Progress form}
    procedure WriteFileName( const filename : String );
    procedure SetProgress( progress : Integer );
    procedure SetCaption( const str : String );
    function  AppendSlash(const sDir : String): String;
    function  RemoveSlash(const sDir : String): String;
    function  CopyDirectory( const from_dir, to_dir : String ) : Boolean;
    function  GetDirectorySize( const dir : String ) : Integer;
    function  GetDirectoryCount( const dir : String ) : Integer;
    function  GetFileSize( const fileName : String ) : Integer;
    function  DeleteDirectory( const dir : String ) : Boolean;
    function  IsDirectoryEmpty( const dir : String ) : Boolean;
    function  IsDirectoryInUse( const dir : String ) : Boolean;
    function  FindFile( const FileName, DirectoryStart : String ) : Boolean;
    procedure CopyFilesWithJoker( const FileName, DestDirectory : String );
    procedure CopyFiles( AList : TStrings; const DestDirectory : String );
    function  DiskInDrive(Drive: Char): Boolean;

    { Below is the properties as created in the component }
    property CopyFrom: string read FCopyFrom write FCopyFrom;
    property CopyTo: string read FCopyTo write FCopyTo;
    property Filter : String read FFilter write FFilter;
    property IsWorking: Boolean read GetIsWorking;
    property Progress: Boolean read FShowProgress write FShowProgress;
    property OnNotExists : String read FOnNotExists write FOnNotExists;
    property ShowFileNames : Boolean read FShowFileNames write FShowFileNames;
    property Caption : String read FCaption write FCaption;
    property Movefile : Boolean read FMovefile write FMovefile;
    property Processed : Longint read FProcessed;
    property TransferTimeDate : Boolean read FTransferTimeDate write FTransferTimeDate;
    property TransferFileAttributes : Boolean read FTransferFileAttributes write FTransferFileAttributes default True;
    property Recursive : Boolean read FRecursive write FRecursive default True;
    property SendEvents : Boolean read FSendEvents write FSendEvents default True;
    property ProgressKind : TProgressKind read FProgressKind write FProgressKind default pkDirectory;
    property CancelOperation : Boolean read FCancelOperation write FCancelOperation;
    property OnStartOperation : TNotifyEvent read FOnStartOperation write FOnStartOperation;
    property OnFinishOperation : TNotifyEvent read FOnFinishOperation write FOnFinishOperation;
    property OnOperationProgress : TOperationProgressEvent read FOnOperationProgress write FOnOperationProgress;
    property OnEachFile : TEachFileEvent read FOnEachFile write FOnEachFile;
  end;

  TCopyFile = class(TCustomCopyFile)
    published
      property CopyFrom;
      property CopyTo;
      property Filter;
      property Progress;
      property OnNotExists;
      property ShowFileNames;
      property Caption;
      property Movefile;
      property Processed;
      property TransferTimeDate;
      property TransferFileAttributes;
      property Recursive;
      property SendEvents;
      property ProgressKind;
      property OnStartOperation;
      property OnFinishOperation;
      property OnOperationProgress;
      property OnEachFile;
  end;

procedure Register;

implementation

uses
  Consts;

procedure Register;
begin
  RegisterComponents('Backup Tools', [TCopyFile]);
end;

constructor TCustomCopyFile.Create(AOwner: TComponent);
begin
  FShowProgress := True;
  FTransferTimeDate := True;
  progress := FShowProgress;
  FTransferFileAttributes := True;
  FRecursive := True;
  FSendEvents := True;
  FProgressKind := pkDirectory;
  FFilter := '*.*';
  inherited Create(AOwner);
end;

destructor TCustomCopyFile.Destroy;
begin
  if FCopyMultipleFiles then
    Finish;
  inherited Destroy;
end;

procedure TCustomCopyFile.CopyNow;
var
  Source, Dest   : TFileStream;
  size           : Longint;
  toCopy         : Longint;
  D_Dir          : string; { <- Suggested by Bas Swemle, Netherlands}
  CanDelete      : Boolean;
const
  ChunkSize : Integer = 8192;
begin
  CanDelete := False;
  { - The following was suggested by Bas Swemle, Netherlands - }
  if (Fileexists(CopyFrom)) and (FCopyFrom<>'') then
  begin
    D_dir := ExtractFilePath(FCopyTo);
    if not DirectoryExists(D_Dir) then
      if messagedlg('Destination Directory '+d_dir+' doesn''t exists...'+#13#13+
                    'Do you want to create the directory ?',mtConfirmation,
                    [mbYes,mbNo],0) = mrYes then
                      ForceDirectories(D_Dir) else
                      begin
                        messagedlg('Process Aborted',mtWarning,[mbOk],0);
                        exit;
                      end;
  end;
  { ----- End of suggestion from Bas Swemle, Netherlands ----- }

  if (fileexists(FCopyFrom)) and (FCopyTo<>'') then
  begin
    if (FProgressKind = pkFile) or (FBytesToCopy=0) then
      begin
        FBytesCopied := 0;
        { Compute the length of the FCopyFrom file }
        Size := GetFileSize( FCopyFrom );
      end
    else // if kind is Directory
      Size := FBytesToCopy;

    { Show the progressform if this is what the user wants }
    Start;
    WriteFileName( ExtractFilename(FCopyFrom) );

    try
      source := TFileStream.Create( FCopyFrom, fmOpenRead or fmShareDenyWrite );
      try
        Dest := TFileStream.Create( FCopyTo, fmCreate );
        try
          repeat
            if FCancelOperation then
              Break;
            if (Source.Size-Source.Position) < ChunkSize then
              toCopy := Source.Size-Source.Position
            else
              toCopy := ChunkSize;
            Dest.CopyFrom( source, toCopy );
              Inc( FBytesCopied, toCopy );
            if Size > 0 then
              FProcessed := Round(FBytesCopied*100/Size)
            else
              FProcessed := 0;
            SetProgress( Round(FProcessed) );
          until Dest.Size = Source.Size;
          If FTransferTimeDate=True then FileSetDate(Dest.Handle,FileGetDate(Source.Handle));
             { Above line was suggested by Russel Havens, USA.}
          CanDelete := True;
        finally
          Dest.Free;
          if FCancelOperation then
            DeleteFile( FCopyTo );
        end;
      finally
        Source.Free;
      end;
    finally
      if not FCancelOperation then
        begin
          if TransferFileAttributes then
            FileSetAttr( FCopyTo, FileGetAttr( FCopyFrom ) );
          If FMoveFile and CanDelete Then
             DeleteFile(FCopyFrom);
        end;
      Finish;
    end;
  end
  else
    If FOnNotExists <> '' then Showmessage(FOnNotExists);
end;

procedure TCustomCopyFile.Start;
begin
  Inc(FStartCount);
  if FStartCount = 1 then
    begin
      FCopyMultipleFiles := True;
      FCancelOperation := False;
      if progress and not Assigned(FProgressForm) then
        begin
          FProgressForm := TProgForm.Create(Application);
          FProgressForm.Caption := FCaption;
          FProgressForm.Show;
        end;

      if SendEvents and Assigned(FOnStartOperation) then
        FOnStartOperation( Self );
    end
  else if Assigned(FProgressForm) then
    FProgressForm.Caption := FCaption;
end;

procedure TCustomCopyFile.Finish;
begin
  Dec(FStartCount);
  if FStartCount = 0 then
    begin
      FBytesToCopy := 0;
      FCopyMultipleFiles := False;
      if SendEvents and Assigned(FOnFinishOperation) then
        FOnFinishOperation( Self );
      if Assigned(FProgressForm) then
        begin
          FProgressForm.Free;
          FProgressForm := nil;
        end;
      BringToFront;
    end;
end;

function TCustomCopyFile.GetIsWorking : Boolean;
begin
  result := FStartCount > 0;
end;

procedure TCustomCopyFile.BringToFront;
begin
  if Assigned(Owner) and (Owner is TForm) then
    TForm(Owner).BringToFront;
end;

procedure TCustomCopyFile.WriteFileName( const filename : String );
begin
  if Assigned(FProgressForm) then
    begin
      if FShowFileNames then
        FProgressForm.Lfilename.Caption := filename
      else
        FProgressForm.Lfilename.Caption := '';
      FProgressForm.Lfilename.Update;
      Application.ProcessMessages;
    end;
  if SendEvents and Assigned( FOnEachFile ) then
    FOnEachFile( Self, filename );
end;

procedure TCustomCopyFile.SetProgress( progress : Integer );
begin
  if Assigned(FProgressForm) then
    begin
      FProgressForm.progressbar1.position:= progress;
      FProgressForm.progressbar1.Update;
      Application.ProcessMessages;
      FCancelOperation := FProgressForm.ModalResult = mrCancel;
    end;
  if SendEvents and Assigned(FOnOperationProgress) then
    FOnOperationProgress( Self, progress, FCancelOperation );
end;

procedure TCustomCopyFile.SetCaption( const str : String );
begin
  FCaption := str;
  if Assigned(FProgressForm) then
    begin
      FProgressForm.Caption := str;
      FProgressForm.Update;
    end;
end;

function TCustomCopyFile.AppendSlash(const sDir : String): String;
begin
  Result := sDir;
  if (Length(sDir)>0) and (sDir[Length(sDir)]<>'\') then
     Result := Result+'\';
end;

function TCustomCopyFile.RemoveSlash(const sDir : String): String;
begin
  Result := sDir;
  if (Length(sDir)>0) and (sDir[Length(sDir)]='\') then
     Delete( Result, length(sDir), 1 );
end;

procedure TCustomCopyFile.PrecalcDirSize( const dir : String );
var
  oldProgress, oldSendEvents : Boolean;
begin
  if FProgressKind = pkDirectory then
    begin
      FBytesCopied := 0;
      oldProgress := Progress;
      oldSendEvents := SendEvents;
      Progress := False;
      SendEvents := False;
      try
        FBytesToCopy := GetDirectorySize(dir);
      finally
        Progress := oldProgress;
        SendEvents := oldSendEvents;
      end;
    end;
end;

function TCustomCopyFile.CopyDirectory( const from_dir, to_dir : String ) : Boolean;
  function DoCopy( const from_dir, to_dir, path : String ) : Boolean;
  var
    SR : TSearchRec;
    Found : Integer;
    source, dest : String;
  begin
    result := False;
    source := from_dir + path;
    dest   := to_dir + path;
    if not DirectoryExists(source) then
      Exit;
    try
      ForceDirectories(dest);
      if not DirectoryExists(dest) then
        Exit;
      result := True;
      Found := FindFirst( source+'\'+Filter, faAnyFile, SR );
      try
        while Found = 0  do
          begin
            if (SR.Name <> '.') and (SR.Name <> '..') then
              begin
                if (SR.Attr and faDirectory) <> 0 then
                  begin
                    // Copy subdirectory
                    if Recursive then
                      result := result and DoCopy( from_dir, to_dir, path+'\'+SR.Name );
                  end
                else
                  begin
                    // Copy file
                    CopyFrom := source+'\'+SR.Name;
                    CopyTo   := dest+'\'+SR.Name;
                    CopyNow;
                  end;
              end;
            Found := FindNext( SR );
          end;
      finally
        FindClose(SR);
      end;
    except
    end;
  end;

begin
  PrecalcDirSize( from_dir );
  Start;
  try
    result := DoCopy( RemoveSlash(from_dir), RemoveSlash(to_dir), '' );
  finally
    Finish;
  end;
end;

function  TCustomCopyFile.GetDirectorySize( const dir : String ) : Integer;
  function DoCalcSize( const dir, path : String ) : Integer;
  var
    SR : TSearchRec;
    Found : Integer;
    source : String;
  begin
    result := 0;
    source := dir + path;
    if not DirectoryExists(source) then
      Exit;
    try
      result := 0;
      Found := FindFirst( source+'\'+Filter, faAnyFile, SR );
      try
        while Found = 0  do
          begin
            if (SR.Name <> '.') and (SR.Name <> '..') then
              begin
                WriteFileName( SR.Name );
                if (SR.Attr and faDirectory) <> 0 then
                  begin
                    // Copy subdirectory
                    if Recursive then
                      result := result + DoCalcSize( dir, path+'\'+SR.Name );
                  end
                else
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
    except
    end;
  end;

begin
  Start;
  if Assigned(FProgressForm) then
    FProgressForm.ProgressBar1.Visible := False;
  try
    result := DoCalcSize( RemoveSlash(dir), '' );
  finally
    if Assigned(FProgressForm) then
      FProgressForm.ProgressBar1.Visible := True;
    Finish;
  end;
end;

function  TCustomCopyFile.GetDirectoryCount( const dir : String ) : Integer;
  function DoCalcCount( const dir, path : String ) : Integer;
  var
    SR : TSearchRec;
    Found : Integer;
    source : String;
  begin
    result := 0;
    source := dir + path;
    if not DirectoryExists(source) then
      Exit;
    try
      result := 0;
      Found := FindFirst( source+'\'+Filter, faAnyFile, SR );
      try
        while Found = 0  do
          begin
            if (SR.Name <> '.') and (SR.Name <> '..') then
              begin
                WriteFileName( SR.Name );
                if (SR.Attr and faDirectory) <> 0 then
                  begin
                    // Copy subdirectory
                    if Recursive then
                      result := result + DoCalcCount( dir, path+'\'+SR.Name );
                  end
                else
                  begin
                    // Add file
                    Inc( Result );
                  end;
              end;
            Found := FindNext( SR );
          end;
      finally
        FindClose(SR);
      end;
    except
    end;
  end;

begin
  Start;
  if Assigned(FProgressForm) then
    FProgressForm.ProgressBar1.Visible := False;
  try
    result := DoCalcCount( RemoveSlash(dir), '' );
  finally
    if Assigned(FProgressForm) then
      FProgressForm.ProgressBar1.Visible := True;
    Finish;
  end;
end;

function  TCustomCopyFile.GetFileSize( const fileName : String ) : Integer;
begin
  Result := 0;
  if FileExists( fileName ) then
    begin
      try
        with TFileStream.Create( fileName, fmOpenRead or fmShareDenyNone) do
          try
            Result := Size;
          finally
            Free;
          end;
      except
      end;
    end;
end;

function TCustomCopyFile.DeleteDirectory( const dir : String ) : Boolean;

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
          SetProgress( MulDiv(FBytesCopied, 100, FBytesToCopy) );
          if (SR.Name<>'.') and (SR.Name <> '..') then
            begin
              WriteFileName( SR.Name );
              if (SR.Attr and faDirectory) <> 0 then
                begin
                  // Delete subdirectory
                  if Recursive then
                    DoDeleteDirectory( dir, path+'\'+SR.Name, Result );
                end
              else
                begin
                  // Remove attributes that could prevent us from deleting the file
                  FileSetAttr( source+'\'+SR.Name, FileGetAttr(source+'\'+SR.Name) and
                                                   not (faReadOnly or faHidden) );
                  // Delete file
                  if not DeleteFile( source+'\'+SR.Name ) then
                    result := False;
                end;
            end;
          Inc( FBytesCopied, SR.Size );
          Found := FindNext( SR );
        end;
    finally
      FindClose(SR);
    end;
    if Result then
      // Delete the empty directory
      result := result and RemoveDir( source );
  end;
begin
  result := True;
  PrecalcDirSize( dir );
  Start;
  try
    DoDeleteDirectory( RemoveSlash(dir), '', result );
  finally
    Finish;
  end;
end;

function TCustomCopyFile.IsDirectoryEmpty( const dir : String ) : Boolean;
var
  SR : TSearchRec;
  Found : Integer;
begin
  if not DirectoryExists( dir ) then
    raise Exception.CreateFmt('Le répertoire "%s" n''existe pas',[dir]);
  result := True;
  Found := FindFirst( RemoveSlash(dir)+'\*.*', faAnyFile, SR );
  try
    while Found = 0  do
      begin
        if (SR.Name<>'.') and (SR.Name <> '..') then
          begin
            result := False;
            Break;
          end;
        Found := FindNext( SR );
      end;
  finally
    FindClose(SR);
  end;
end;

// This function checks if a Directory is in use.
// It travels each subdirectory and tries to open each file
// in exclusive mode. If it fails, it means that someone has
// already locked this file, and it won't be possible to delete
// the directory containing it.

function  TCustomCopyFile.IsDirectoryInUse( const dir : String ) : Boolean;
  function DoIsDirectoryInUse( const dir, path : String ) : Boolean;
  var
    SR : TSearchRec;
    Found : Integer;
    source : String;
    f : Integer;
  begin
    source := dir + path;
    if not DirectoryExists(source) then
      raise Exception.CreateFmt(sDirectoryDoesNotExist,[source]);
    result := True;
    Found := FindFirst( source+'\*.*', faAnyFile, SR );
    try
      while (Found = 0) and Result  do
        begin
          if (SR.Name <> '.') and (SR.Name <> '..') then
            begin
              WriteFileName( SR.Name );
              if (SR.Attr and faDirectory) <> 0 then
                begin
                  // Copy subdirectory
                  if Recursive then
                    result := result and DoIsDirectoryInUse( dir, path+'\'+SR.Name );
                end
              else
                begin
                  // Check if the file is locked
                  f := FileOpen( source+'\'+SR.Name, fmShareExclusive );
                  if f < 0 then
                    Result := False
                  else
                    FileClose(f);
                end;
            end;
          Found := FindNext( SR );
        end;
    finally
      FindClose(SR);
    end;
  end;

begin
  Start;
  if Assigned(FProgressForm) then
    FProgressForm.ProgressBar1.Visible := False;
  try
    result := not DoIsDirectoryInUse( RemoveSlash(dir), '' );
  finally
    if Assigned(FProgressForm) then
      FProgressForm.ProgressBar1.Visible := True;
    Finish;
  end;
end;

function  TCustomCopyFile.FindFile( const FileName, DirectoryStart : String ) : Boolean;
  function DoFindFile( const dir, path : String ) : Boolean;
  var
    SR : TSearchRec;
    Found : Integer;
    source : String;
  begin
    source := dir + path;
    if not DirectoryExists(source) then
      raise Exception.CreateFmt(sDirectoryDoesNotExist,[source]);
    result := True;
    Found := FindFirst( source+'\*.*', faAnyFile, SR );
    try
      while (Found = 0) and Result  do
        begin
          if (SR.Name <> '.') and (SR.Name <> '..') then
            begin
              WriteFileName( SR.Name );
              if SR.Name = FileName then
                begin
                  Result := False;
                  Break;
                end;
              if (SR.Attr and faDirectory) <> 0 then
                begin
                  // Copy subdirectory
                  if Recursive then
                    result := result and DoFindFile( dir, path+'\'+SR.Name );
                end;
            end;
          Found := FindNext( SR );
        end;
    finally
      FindClose(SR);
    end;
  end;

begin
  Start;
  if Assigned(FProgressForm) then
    FProgressForm.ProgressBar1.Visible := False;
  try
    result := not DoFindFile( RemoveSlash(DirectoryStart), '' );
  finally
    if Assigned(FProgressForm) then
      FProgressForm.ProgressBar1.Visible := True;
    Finish;
  end;
end;

procedure TCustomCopyFile.CopyFilesWithJoker( const FileName, DestDirectory : String );
var
  SR : TSearchRec;
  Found : Integer;
  dest : String;
begin
  Start;
  try
    dest := AppendSlash(DestDirectory);
    Found := FindFirst( FileName, faAnyFile, SR );
    try
      while (Found = 0) do
        begin
          if (SR.Name <> '.') and (SR.Name <> '..') then
            begin
              if (SR.Attr and faDirectory) = 0 then
                begin
                  WriteFileName( SR.Name );
                  CopyFrom := ExtractFilePath(FileName)+SR.Name;
                  CopyTo   := dest+SR.Name;
                  CopyNow;
                end;
            end;
          Found := FindNext( SR );
        end;
    finally
      FindClose(SR);
    end;
  finally
    Finish;
  end;
end;

procedure TCustomCopyFile.CopyFiles( AList : TStrings; const DestDirectory : String );
var
  i : Integer;
begin
  if not DirectoryExists(DestDirectory) then
    raise Exception.CreateFmt(sDirectoryDoesNotExist,[DestDirectory]);
  Start;
  try
    for i := 0 to AList.Count - 1 do
      begin
        if AList.Strings[i] <> '' then
          CopyFilesWithJoker( AList.Strings[i], DestDirectory );
      end;
  finally
    Finish;
  end;
end;

function TCustomCopyFile.DiskInDrive(Drive: Char): Boolean;
var
  ErrorMode: word;
begin
  // make it upper case

  if Drive in ['a'..'z'] then Dec(Drive, $20);
  // make sure it's a letter
  if not (Drive in ['A'..'Z']) then
    raise EConvertError.Create('Not a valid drive ID');
  // turn off critical errors
  ErrorMode := SetErrorMode(SEM_FailCriticalErrors);
  try
    // drive 1 = a, 2 = b, 3 = c, etc.
    if DiskSize(Ord(Drive) - $40) = -1 then
      Result := False
    else
      Result := True;
  finally
    // restore old error mode
    SetErrorMode(ErrorMode);
  end;
end;


end.
