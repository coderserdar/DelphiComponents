unit ZLIBArchive;

//***********************************************
// ZLBArchive Control.  Ver. 1.52
//
// Requires the ZLIB Compression routines found  
// on the Delphi CD.
//***********************************************

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ZLib, FileCtrl;

type
  TFileStatus = (fsCompressed, fsStored);
  TFileCompressionLevel = (fcNone, fcFastest, fcDefault, fcMaximum);
  TInternalFlags = (ifOpening, ifExtracting, ifAdding, ifUpdating, ifWritingRegistry, ifClear);

  TReplaceFileEvent = procedure(Filename:string; var Replace:boolean) of object;
  TFileExtractedEvent = procedure(filename : string) of object;
  TErrorLogEvent = procedure(Msg:string) of object;
  TProgressEvent = procedure(Msg:string; Percentage:longint) of object;

  EFileCorruption = Exception;
  EFileNotFound = Exception;
  EArchiveRegistry = Exception;
  EGeneralArchiveException = Exception;
  EExtractionError = Exception;
  ENoArchive = Exception;
  EArchiveIndexBounds = Exception;
  EFileNotFoundInArchive = Exception;

  TZLBFileRec = record
                  name : shortstring;
                  path : shortstring;
                  osize : longint;
                  csize : longint;
                  start : longint;
                  CRC   : longint;
                  Status : TFileStatus;
                  CompressionLevel:TFileCompressionLevel;
  end;

  TZLBArchiveRec = record
                  CRC : longint;
                  FileCount:longint;
                  Version : longint;
                  RegistrySize : longint;
                  ArchiveSize : longint;
                  CompressedRegistry:Boolean;
                  Reserved:array[0..11] of byte;
  end;

  TZLBArchive = class(TComponent)
  private
    { Private declarations }
    fNewArchive : boolean;
    fArchive : string;
    fArchiveDetails : TZLBArchiveRec;
    fArchiveRegistry : TList;
    fCompressionLevel : TFileCompressionLevel;
    fExtractWithPath : boolean;
    fRecursePaths : boolean;
    fSavePaths : boolean;
    fCheckCRC : boolean;
    fCRC32Table : array[0..255] of longint;
    fProgressFlag : TInternalFlags;
    fProgressText : string;
    fProgressTotal,
    fProgressIndex : longint;
    fOnReplaceFile : TReplaceFileEvent;
    fOnFileExtracted : TFileExtractedEvent;
    fOnStreamExtracted : TFileExtractedEvent;
    fOnArchiveOpened : TNotifyEvent;
    fOnErrorLog : TErrorLogEvent;
    fOnProgress : TProgressEvent;
    procedure InitCRC32;
    function GetCRC32(DataStream: TStream; IgnoreCount: longint): longint;
    function GetFileCount: longint;
    function GetFileItem(index: longint): TZLBFileRec;
    function Extract(Index: longint; OutFile: TStream): boolean;
    procedure InternalProgress(Sender:TObject);
    Procedure UpdateRegistryRec(FileIndex:longint; FileInfo:TZLBFileRec);
    Procedure WriteRegistry(DataStrm:TStream);
  protected
    { Protected declarations }
    function GetTmpFileName:string; virtual;
    procedure SearchTree(path: string; var files: TStrings); virtual;
  public
    { Public declarations }
    constructor create(AOwner:TComponent); override;
    destructor destroy; override;
    property ArchiveName : string read fArchive;
    function OpenArchive(filename:string):boolean;
    procedure CreateArchive(filename:string);
    procedure CloseArchive;
    procedure AddFile(Filename: string);
    Procedure AddFiles(FileList:TStrings);
    procedure AddStream(Filename:string; InStream:TStream);
    procedure DeleteFiles(FileList: TStrings);
    procedure ExtractAll(Destination:string);
    procedure ExtractFileByIndex(Destination:string; index:longint);
    procedure ExtractStreamByIndex(OutFile:TStream; index:longint);
    procedure ExtractFileByName(Destination, Filename:string);
    procedure ExtractStreamByName(OutFile:TStream; Filename:string);
    procedure RenameFileByName(Original, New:string);
    property FileCount:longint read GetFileCount;
    property Files[index:longint]:TZLBFileRec read GetFileItem;
    function FileInArchive(filename : string):longint;
    Procedure UpdateFile(FileName:string; FileIndex:longint);
    procedure UpdateFiles(FileList: TStrings);
    procedure UpdateStream(FileName:string; InStream:TStream);
  published
    { Published declarations }
    property CompressionLevel:TFileCompressionLevel read FCompressionLevel write FCompressionLevel default fcDefault;
    property ExtractWithPath:boolean read fExtractWithPath write fExtractWithPath default false;
    property CheckCRC:boolean read fCheckCRC write fCheckCRC default false;
    property SavePaths : boolean read fSavePaths write fSavePaths default false;
    property RecursePaths : boolean read fRecursePaths write fRecursePaths default false;
    property OnReplaceFile:TReplaceFileEvent read fOnReplaceFile write fOnReplaceFile;
    property OnFileExtracted : TFileExtractedEvent read fOnFileExtracted write fOnFileExtracted;
    property OnArchiveOpened : TNotifyEvent read fOnArchiveOpened write fOnArchiveOpened;
    property OnStreamExtracted : TFileExtractedEvent read fOnStreamExtracted write fOnStreamExtracted;
    property OnErrorLogged : TErrorLogEvent read fOnErrorLog write fOnErrorLog;
    property OnProgress : TProgressEvent read fOnProgress write fOnProgress;
  end;

procedure Register;

implementation

const
     ArchiveFileNotFound = 'Archive file not found';
     ArchiveCorrupt = 'The archive file appears to be corrupted';
     RegistryReadError = 'Unable to read archive file list';
     NoArchiveOpen = 'No archive is currently open';
     IndexBounds = 'Index out of bounds';
     PathCreation = 'Unable to create destination path';
     FileCorrupted = 'File (%d) is corrupt';
     FileCRC       = 'File (%d) failed CRC check';
     FileNotFound = 'File not found in archive';
     NoArchiveSpecified = 'No archive file specified';
     FileAlreadyExists = 'File already exists';
     FileAlreadyExists2 = '%s already exists in the archive';
     UnableToOpenFile = 'Unable to open file (%s)';

     ExtractingRegistry = 'Extracting archive registry';
     ExtractingFile = 'Extracting %s';
     UpdatingFile = 'Updating %s';
     AddingFile = 'Adding %s';
     WritingRegistry = 'Writing archive registry';

procedure Register;
begin
  RegisterComponents('StdComp', [TZLBArchive]);
end;

constructor TZLBArchive.create(AOwner:TComponent);
begin
     inherited create(AOwner);
     fNewArchive := false;
     FCompressionLevel := fcdefault;
     fExtractWithPath := false;
     fArchive := '';
     fArchiveRegistry := TList.create;
     fCheckCRC := false;
     fSavePaths := false;
     fRecursePaths := false;
     InitCRC32;
end;

destructor TZLBArchive.destroy;
begin
     if farchive <> '' then CloseArchive;
     fArchiveRegistry.free;
     inherited;
end;

Procedure TZLBArchive.InitCRC32;
var
   crc, poly : longint;
   i, j : longint;
begin
     poly := longint($EDB88320);
     for i := 0 to 255 do
     begin
          crc := i;
          for j := 8 downto 1 do
          begin
               if (crc and 1) = 1 then
                  crc := (crc shr 1) xor poly
               else
                  crc := crc shr 1;
          end;
          fcrc32table[i] := crc;
     end;
end;

function TZLBArchive.GetCRC32(DataStream:TStream; IgnoreCount:longint): longint;
var
   crc, checked, buffersize, fsize, count : longint;
   BufferArray : array[0..10239] of byte;
   originalposition : longint;
begin
     originalposition := datastream.Position;
     dataStream.Seek(0,soFromBeginning);
     crc := longint($ffffffff);
     fsize := datastream.size-IgnoreCount;
     while True do
     begin
          if fsize <= 0 then break;
          if fsize >= 10240 then
               buffersize :=10240
          else
               buffersize := fsize;
          Count := DataStream.Read(BufferArray, BufferSize);
          checked := 0;
          while checked < Count do
          begin
               crc := ((crc shr 8) and $ffffff) xor fcrc32table[ (crc xor bufferArray[checked]) and $ff ];
               inc(checked);
          end;
          dec(fsize,buffersize);
     end;
     result := (crc xor longint($ffffffff));
     datastream.seek(originalposition,soFromBeginning);
end;

function TZLBArchive.OpenArchive(filename: string): boolean;
var
   InFile: TStream;
   TmpFile : TStream;
   ZStream: TCustomZLibStream;
   loop, count : longint;
   Data : ^TZLBFileRec;
   CheckValue, fsize, buffersize : longint;
   buffer : pointer;
begin
     CloseArchive;
     fArchive := FileName;
     result := false;
     if (FileName = '') then
     begin
          fArchive := '';
          exit;
     end;

     if not FileExists(FileName) then
        Raise EFileNotFound.create(ArchiveFileNotFound);

     try
        InFile := TFileStream.Create(FileName, fmOpenRead+fmShareDenyWrite);
     except
        Raise EGeneralArchiveException.create(format(UnableToOpenFile,[filename]));
     end;
     
     CheckValue := GetCRC32(InFile, sizeof(TZLBArchiveRec));
     try
        InFile.Seek(-sizeof(TZLBArchiveRec),soFromEnd);
        InFile.read(fArchiveDetails,sizeof(TZLBArchiveRec));
        if fArchiveDetails.CRC <> CheckValue then
        begin
             fArchive := '';
             fillchar(fArchiveDetails,sizeof(TZLBArchiveRec),0);
             raise EFileCorruption.create(ArchiveCorrupt);
        end;

        try
           if not fArchiveDetails.CompressedRegistry then
           begin
                InFile.Seek(fArchiveDetails.ArchiveSize,soFromBeginning);
                for loop := 0 to fArchiveDetails.FileCount-1 do
                begin
                     getmem(Data,sizeof(TZLBFileRec));
                     InFile.read(Data^,sizeof(TZLBFileRec));
                     fArchiveRegistry.add(Data);
                end;
           end
           else
           begin
                InFile.Seek(fArchiveDetails.ArchiveSize,soFromBeginning);
                tmpfile := tmemorystream.Create;
                try
                   tmemorystream(tmpfile).SetSize(sizeof(TZLBFileRec)*fArchiveDetails.filecount);
                   ZStream := TDecompressionStream.Create(InFile);
                   try
                      fsize := sizeof(TZLBFileRec)*fArchiveDetails.filecount;
                      fProgressFlag := ifOpening;
                      fProgressTotal := fSize;
                      fProgressIndex := 0;
                      fProgressText := ExtractingRegistry;
                      while True do
                      begin
                           if fsize <= 0 then break;
                           if fsize >= 4096 then
                              buffersize :=4096
                           else
                               buffersize := fsize;
                           getmem(buffer,buffersize);
                           try
                              try
                                 Count := ZStream.Read(Buffer^, BufferSize);
                                 inc(fProgressIndex,Count);
                                 InternalProgress(self);
                                 if Count <> 0 then TmpFile.WriteBuffer(Buffer^, Count);
                              except
                                 Raise EArchiveRegistry.create(RegistryReadError);
                              end;
                           finally
                              freemem(buffer,buffersize);
                           end;
                           dec(fsize,buffersize);
                      end;
                   finally
                      zstream.free;
                   end;
                   tmpfile.seek(0,soFromBeginning);
                   for loop := 0 to fArchiveDetails.filecount-1 do
                   begin
                        getmem(Data,sizeof(TZLBFileRec));
                        TmpFile.read(Data^,sizeof(TZLBFileRec));
                        fArchiveRegistry.add(Data);
                   end;
                finally
                   tmpfile.free;
                end
           end;
           if assigned(fOnArchiveOpened) then fOnArchiveOpened(self);
        except
             CloseArchive;
             Raise EArchiveRegistry.create(RegistryReadError);
        end;
     finally
        InFile.free;
     end;
     result := true;
     fProgressFlag := ifClear;
     InternalProgress(self);
end;

procedure TZLBArchive.CloseArchive;
var
   data : pointer;
   loop : longint;
begin
     for loop := fArchiveRegistry.Count-1 downto 0 do
     begin
          data := fArchiveRegistry.Items[loop];
          fArchiveRegistry.Delete(loop);
          freemem(data,sizeof(TZLBFileRec));
     end;
     fNewArchive := false;
     fArchiveRegistry.Clear;
     fillchar(fArchiveDetails,sizeof(TZLBArchiveRec),0);
     fArchive := '';
     fProgressFlag := ifClear;
     InternalProgress(self);
end;

function TZLBArchive.GetFileCount: longint;
begin
     if fArchive <> '' then
        Result := fArchiveRegistry.count
     else
         Raise ENoArchive.create(NoArchiveOpen);
end;

function TZLBArchive.GetFileItem(index: longint): TZLBFileRec;
begin
     if (index >= 0)and (index < fArchiveRegistry.count) then
        result := TZLBFileRec(fArchiveRegistry.items[index]^)
     else
        Raise EArchiveIndexBounds.create(IndexBounds);
end;

procedure TZLBArchive.ExtractAll(Destination:string);
var
   InFile: TStream;
   OutFile: TStream;
   ZStream: TCustomZLibStream;
   data : TZLBFileRec;
   fsize : longint;
   count : longint;
   buffer : pointer;
   buffersize : longint;
   loop : longint;
   successfull : boolean;
   extractionpath : string;
   overwrite : boolean;
begin
  if fArchive = '' then
     Raise ENoArchive.create(NoArchiveOpen);

  try
     InFile := TFileStream.Create(farchive, fmOpenRead+fmShareDenyWrite);
  except
     Raise EGeneralArchiveException.create(format(UnableToOpenFile,[fArchive]));
  end;

  try
     for loop := 0 to filecount-1 do
     begin
          successfull := false;
          data := Files[loop];

          extractionpath := destination;
          if ExtractWithPath then
             extractionpath := extractionpath + data.path;
          while pos('\\',extractionpath) > 0 do
                delete(extractionpath,pos('\\',extractionpath),1);
          if extractionpath[length(extractionpath)] = '\' then
             delete(extractionpath,length(extractionpath),1);
          ForceDirectories(extractionpath);
          if not DirectoryExists(extractionpath) then
             raise EExtractionError.create(PathCreation);

          if (fileexists(extractionpath+'\'+data.name) and assigned(fonreplacefile)) then
          begin
               overwrite := false;
               fonreplacefile(extractionpath+'\'+data.name, overwrite);
          end
          else
          overwrite := true;

          if overwrite then
          begin
               OutFile := TFileStream.Create(extractionpath+'\'+data.name, fmCreate);
               try
                  InFile.Seek(data.start,soFromBeginning);
                  ZStream := TDecompressionStream.Create(InFile);
                  try
                     fsize := data.osize;
                     fProgressFlag := ifExtracting;
                     fProgressTotal := fSize;
                     fProgressIndex := 0;
                     fProgressText := format(ExtractingFile,[data.name]);
                     while True do
                     begin
                          if fsize <= 0 then break;
                          if fsize >= 4096 then
                             buffersize :=4096
                          else
                              buffersize := fsize;
                          getmem(buffer,buffersize);
                          try
                             try
                                if data.Status = fsCompressed then
                                   Count := ZStream.Read(Buffer^, BufferSize)
                                else
                                   Count := InFile.Read(Buffer^, BufferSize);
                                inc(fProgressIndex,Count);
                                InternalProgress(self);
                                if Count <> 0 then OutFile.WriteBuffer(Buffer^, Count);
                             except
                                raise EFileCorruption.create(Format(FileCorrupted,[loop]));
                             end;
                          finally
                             freemem(buffer,buffersize);
                          end;
                          dec(fsize,buffersize);
                     end;
                  finally
                     ZStream.Free;
                  end;

                  if (CheckCRC) and (GetCRC32(OutFile,0) <> data.CRC) then
                     raise EFileCorruption.create(Format(FileCorrupted,[loop]));

                  successfull := true;
               finally
                  OutFile.Free;
                  if successfull and assigned(fOnFileExtracted) then fOnFileExtracted(destination+data.name);
               end;
          end;
     end;
  finally
    InFile.Free;
    fProgressFlag := ifClear;
    InternalProgress(self);
  end;
end;

function TZLBArchive.Extract(Index:longint; OutFile:TStream):boolean;
var
   InFile: TStream;
   data : TZLBFileRec;
   successfull : boolean;
   ZStream: TCustomZLibStream;
   fsize, count, buffersize : longint;
   buffer : pointer;
begin
     successfull := false;
     data := Files[index];

     try
        InFile := TFileStream.Create(farchive, fmOpenRead+fmShareDenyWrite);
     except
        Raise EGeneralArchiveException.create(format(UnableToOpenFile,[fArchive]));
     end;
     
     try
        InFile.Seek(data.start,soFromBeginning);
        ZStream := TDecompressionStream.Create(InFile);
        try
           fsize := data.osize;
           fProgressFlag := ifExtracting;
           fProgressTotal := fSize;
           fProgressIndex := 0;
           fProgressText := format(ExtractingFile,[data.name]);
           while True do
           begin
                if fsize <= 0 then break;
                if fsize >= 4096 then
                   buffersize :=4096
                else
                    buffersize := fsize;
                getmem(buffer,buffersize);
                try
                   try
                      if data.Status = fsCompressed then
                         Count := ZStream.Read(Buffer^, BufferSize)
                      else
                         Count := InFile.Read(Buffer^, BufferSize);
                      inc(fProgressIndex);
                      InternalProgress(self);
                      if Count <> 0 then OutFile.WriteBuffer(Buffer^, Count);
                   except
                      raise EFileCorruption.create(Format(FileCorrupted,[index]));
                   end;
                finally
                   freemem(buffer,buffersize);
                end;
                dec(fsize,buffersize);
           end;
        finally
           ZStream.Free;
        end;

        if (CheckCRC) and (GetCRC32(OutFile,0) <> data.CRC) then
           raise EFileCorruption.create(Format(FileCRC,[index]));

        successfull := true;
     finally
        InFile.free;
        result := successfull;
     end;
end;

procedure TZLBArchive.ExtractFileByIndex(Destination : String; index:longint);
var
   OutFile: TStream;
   data : TZLBFileRec;
   extractionpath : string;
   overwrite : boolean;
begin
  if fArchive = '' then
     Raise ENoArchive.create(NoArchiveOpen);

  if not ((index >= 0) and (index < fArchiveRegistry.count)) then
     Raise EArchiveIndexBounds.create(IndexBounds);

  data := Files[index];

  extractionpath := destination;
  if ExtractWithPath then
  begin
       extractionpath := extractionpath + data.path;
       while pos('\\',extractionpath) > 0 do
             delete(extractionpath,pos('\\',extractionpath),1);
       if extractionpath[length(extractionpath)] = '\' then
          delete(extractionpath,length(extractionpath),1);
       ForceDirectories(extractionpath);
       if not DirectoryExists(extractionpath) then
          raise EExtractionError.create(PathCreation);
  end;
  if (fileexists(extractionpath+'\'+data.name) and assigned(fonreplacefile)) then
  begin
       overwrite := false;
       fonreplacefile(extractionpath+'\'+data.name, overwrite);
  end
  else
  overwrite := true;
  if overwrite then
  begin
       OutFile := TFileStream.Create(extractionpath+'\'+data.name, fmCreate);
       try
          if Extract(index,Outfile) and assigned(fOnFileExtracted) then fOnFileExtracted(destination+data.name);
       finally
          OutFile.free;
       end;
  end;
  fProgressFlag := ifClear;
  InternalProgress(self);
end;

procedure TZLBArchive.ExtractFileByName(Destination, Filename:string);
var
   OutFile: TStream;
   data : TZLBFileRec;
   index, loop : longint;
   extractionpath : string;
   overwrite : boolean;
begin
  if fArchive = '' then
     Raise ENoArchive.create(NoArchiveOpen);

  index := -1;
  for loop := 0 to filecount-1 do
  begin
       data := Files[loop];
       if lowercase(data.path+data.name) = lowercase(filename) then
       begin
            index := loop;
            break;
       end;
  end;

  if index = -1 then
     raise EExtractionError.create(FileNotFound);

  extractionpath := destination;
  if ExtractWithPath then
     extractionpath := extractionpath + data.path;
  while pos('\\',extractionpath) > 0 do
        delete(extractionpath,pos('\\',extractionpath),1);
  if extractionpath[length(extractionpath)] = '\' then
     delete(extractionpath,length(extractionpath),1);
  ForceDirectories(extractionpath);
  if not DirectoryExists(extractionpath) then
     raise EGeneralArchiveException.create(PathCreation);

  if (fileexists(extractionpath+'\'+data.name) and assigned(fonreplacefile)) then
  begin
       overwrite := false;
       fonreplacefile(extractionpath+'\'+data.name, overwrite);
  end
  else
  overwrite := true;
  if overwrite then
  begin
       OutFile := TFileStream.Create(extractionpath+'\'+data.name, fmCreate);
       try
          if Extract(index,Outfile) and assigned(fOnFileExtracted) then fOnFileExtracted(destination+data.name);
       finally
          OutFile.free;
       end;
  end;
  fProgressFlag := ifClear;
  InternalProgress(self);
end;

procedure TZLBArchive.ExtractStreamByIndex(OutFile:TStream; index:longint);
var
   data : TZLBFileRec;
begin
  if fArchive = '' then
     Raise ENoArchive.create(NoArchiveOpen);

  if (index < 0) and (index >= fArchiveRegistry.count) then
     Raise EArchiveIndexBounds.create(IndexBounds);

  data := Files[index];
  if Extract(index,Outfile) and assigned(fOnStreamExtracted) then fOnStreamExtracted(data.path+data.name);
  fProgressFlag := ifClear;
  InternalProgress(self);
end;

procedure TZLBArchive.ExtractStreamByName(OutFile:TStream; Filename:string);
var
   data : TZLBFileRec;
   index, loop : longint;
begin
  if fArchive = '' then
     Raise ENoArchive.create(NoArchiveOpen);

  index := -1;
  for loop := 0 to filecount-1 do
  begin
       data := Files[loop];
       if lowercase(data.path+data.name) = lowercase(filename) then
       begin
            index := loop;
            break;
       end;
  end;

  if index = -1 then
     raise EFileNotFoundInArchive.create(FileNotFound);

  data := Files[index];
  if Extract(index,Outfile) and assigned(fOnStreamExtracted) then fOnStreamExtracted(data.path+data.name);
  fProgressFlag := ifClear;
  InternalProgress(self);
end;

procedure TZLBArchive.SearchTree(path:string; var files:TStrings);

procedure ScanPath(Path:string);
var
   data : TSearchRec;
   ds : longint;
   FoundFiles:tstringlist;
begin
     if (Path[length(Path)] = '\') then delete(Path,length(Path),1);
     FoundFiles := tstringlist.create;
     ds := findfirst(Path+'\*.*',faAnyFile,data);
     while ds = 0 do
     begin
          if (data.attr and faDirectory = faDirectory) then
          begin
               if (data.name <> '.') and
                  (data.name <> '..') then
               begin
                    if fRecursePaths then ScanPath(Path+'\'+data.name);
                    if fSavePaths then FoundFiles.add(Path+'\'+data.name);
               end;
          end
          else
          FoundFiles.add(Path+'\'+data.name);
          ds := findnext(data);
     end;
     findClose(data);
     files.addstrings(FoundFiles);
     FoundFiles.Free;
end;

begin
     scanPath(path);
end;

procedure TZLBArchive.AddFile(Filename:string);
var
   ArchiveFile, InFile, NewFile, TmpStrm : TStream;
   ZStream : TCustomZLibStream;
   TmpFileName : string;
   FileInfo : ^TZLBFileRec;
begin
     if fArchive = '' then
        Raise ENoArchive.create(NoArchiveOpen);

     TmpFileName := '';
     if savepaths then TmpFileName := extractfilepath(filename);
     TmpFileName := TmpFileName + ExtractFilename(filename);

     if fileinarchive(TmpFileName) > -1 then
        raise EGeneralArchiveException.create(FileAlreadyExists);

     TmpFileName := GetTmpFileName;
     NewFile := TFileStream.Create(TmpFileName,fmOpenReadWrite + fmShareExclusive);
     try
        if not fnewarchive then
        begin
             try
                ArchiveFile := TFileStream.Create(fArchive,fmOpenRead+fmShareDenyWrite);
             except
                Raise EGeneralArchiveException.create(format(UnableToOpenFile,[fArchive]));
             end;
             try
                ArchiveFile.Position := 0;
                NewFile.CopyFrom(ArchiveFile, fArchiveDetails.ArchiveSize);
             finally
                ArchiveFile.Free;
             end;
        end;

        try
           InFile := TFileStream.Create(filename, fmOpenRead+fmShareDenyWrite);
        except
           Raise EGeneralArchiveException.create(format(UnableToOpenFile,[filename]));
        end;

        try
           TmpStrm := TMemoryStream.create;
           try
              ZStream := TCompressionStream.Create(TCompressionLevel(fCompressionLevel), TmpStrm);
              try
                 fProgressFlag := ifAdding;
                 fProgressTotal := Infile.Size;
                 fProgressIndex := 0;
                 fProgressText := format(UpdatingFile,[filename]);
                 TCompressionStream(ZStream).OnProgress := InternalProgress;
                 ZStream.CopyFrom(InFile, 0);
              finally
                 ZStream.Free;
              end;
              TmpStrm.position := 0;

              getmem(FileInfo,sizeof(TZLBFileRec));
              fillchar(FileInfo^,sizeof(TZLBFileRec),0);
              FileInfo^.CRC := GetCRC32(InFile,0);
              FileInfo^.name := extractfilename(filename);
              FileInfo^.path := extractfilepath(filename);
              if SavePaths then
                 delete(FileInfo^.path,1,pos(':',FileInfo^.path))
              else
                 fillchar(FileInfo^.path,sizeof(FileInfo^.path),#0);

              FileInfo^.osize := InFile.size;
              FileInfo^.start := NewFile.Position;

              if TmpStrm.Size >= InFile.size then
              begin
                   NewFile.CopyFrom(InFile,0);
                   FileInfo^.CSize := InFile.Size;
                   FileInfo^.Status := fsStored;
                   FileInfo^.CompressionLevel := fcNone;
              end
              else
              begin
                   NewFile.CopyFrom(TmpStrm,0);
                   FileInfo^.CSize := TmpStrm.Size;
                   FileInfo^.Status := fsCompressed;
                   FileInfo^.CompressionLevel := fCompressionLevel;
              end;

           finally
              TmpStrm.free;
           end;

           fArchiveRegistry.add(FileInfo);

        finally
           InFile.free;
        end;

        WriteRegistry(NewFile);

        NewFile.Free;
        DeleteFile(fArchive);
        RenameFile(TmpFileName,fArchive);
        TmpFileName := fArchive;
        CloseArchive;
        OpenArchive(TmpFileName);
     except
        DeleteFile(TmpFileName)
     end;
end;

procedure TZLBArchive.AddFiles(FileList: TStrings);
var
   ArchiveFile, InFile, NewFile, TmpStrm : TStream;
   ZStream : TCustomZLibStream;
   TmpFileName, TestName : string;
   FileInfo : ^TZLBFileRec;
   Loop : longint;
   MemStrm : Boolean;
   TmpFile : String;
begin
     if fArchive = '' then
        Raise ENoArchive.create(NoArchiveOpen);

     loop := 0;
     while loop < Filelist.Count do
     begin
          if directoryexists(FileList[loop]) then
          begin
               if RecursePaths then
                  searchtree(FileList[loop],filelist);
               filelist.Delete(loop);
          end
          else
          inc(loop);
     end;

     TmpFileName := GetTmpFileName;
     NewFile := TFileStream.Create(TmpFileName,fmOpenReadWrite + fmShareExclusive);
     try
        try
           if not fnewarchive then
           begin
                ArchiveFile := TFileStream.Create(fArchive,fmOpenRead+fmShareDenyWrite);
                try
                   ArchiveFile.Position := 0;
                   NewFile.CopyFrom(ArchiveFile,fArchiveDetails.ArchiveSize);
                finally
                   ArchiveFile.Free;
                end;
           end;
        except
           Raise EGeneralArchiveException.create(format(UnableToOpenFile,[fArchive]));
        end;

        While FileList.count > 0 do
        begin
             TestName := ExtractFilePath(filelist[0]);
             if SavePaths then
                delete(TestName,1,pos(':',TestName))
             else
                TestName := '';
             TestName := TestName + ExtractFilename(FileList[0]);

             if fileinarchive(TestName) > -1 then
                if assigned(fOnErrorLog) then fOnErrorLog(format(FileAlreadyExists2,[FileList[0]]));

             Try
                InFile := TFileStream.Create(FileList[0], fmOpenRead+fmShareDenyWrite);
                try
                   if InFile.Size <= (20 * (1024*1024)) then
                   begin
                      TmpStrm := TMemoryStream.create;
                      MemStrm := true;
                      TmpFile := '';
                   end
                   else
                   begin
                      TmpFile := GetTmpFileName;
                      TmpStrm := TFileStream.create(TmpFile, fmCreate);
                      MemStrm := false;
                   end;
                   try
                      ZStream := TCompressionStream.Create(TCompressionLevel(fCompressionLevel), TmpStrm);
                      try
                         fProgressFlag := ifAdding;
                         fProgressTotal := Infile.Size;
                         fProgressIndex := 0;
                         fProgressText := format(UpdatingFile,[FileList[0]]);
                         TCompressionStream(ZStream).OnProgress := InternalProgress;
                         ZStream.CopyFrom(InFile, 0);
                      finally
                         ZStream.Free;
                      end;
                      TmpStrm.position := 0;

                      getmem(FileInfo,sizeof(TZLBFileRec));
                      fillchar(FileInfo^,sizeof(TZLBFileRec),0);
                      FileInfo^.CRC := GetCRC32(InFile,0);
                      FileInfo^.name := extractfilename(filelist[0]);
                      FileInfo^.path := extractfilepath(filelist[0]);
                      if SavePaths then
                         delete(FileInfo^.path,1,pos(':',FileInfo^.path))
                      else
                         fillchar(FileInfo^.path,sizeof(FileInfo^.path),#0);
                      FileInfo^.osize := InFile.size;
                      FileInfo^.start := NewFile.Position;

                      if TmpStrm.Size >= InFile.size then
                      begin
                           NewFile.CopyFrom(InFile,0);
                           FileInfo^.CSize := InFile.Size;
                           FileInfo^.Status := fsStored;
                           FileInfo^.CompressionLevel := fcNone;
                      end
                      else
                      begin
                           NewFile.CopyFrom(TmpStrm,0);
                           FileInfo^.CSize := TmpStrm.Size;
                           FileInfo^.Status := fsCompressed;
                           FileInfo^.CompressionLevel := fCompressionLevel;
                      end;
                   finally
                      TmpStrm.free;
                      if not MemStrm then
                      begin
                         DeleteFile(TmpFile);
                         tmpfile := '';
                      end;
                   end;
                   fArchiveRegistry.add(FileInfo);
                finally
                   Infile.Free;
                end;
             except
                if assigned(fOnErrorLog) then fOnErrorLog(format(UnableToOpenFile,[FileList[0]]));
             end;

             FileList.Delete(0);
        end;
        WriteRegistry(NewFile);
        NewFile.Free;
        DeleteFile(fArchive);
        RenameFile(TmpFileName,fArchive);
        TmpFileName := fArchive;
        CloseArchive;
        OpenArchive(TmpFileName);
     except
        DeleteFile(TmpFileName)
     end;
end;

procedure TZLBArchive.AddStream(Filename: string; InStream: TStream);
var
   ArchiveFile, NewFile, TmpStrm : TStream;
   ZStream : TCustomZLibStream;
   TmpFileName : string;
   FileInfo : ^TZLBFileRec;
begin
     if fArchive = '' then
        Raise ENoArchive.create(NoArchiveOpen);

     TmpFileName := '';
     if savepaths then TmpFileName := extractfilepath(filename);
     TmpFileName := TmpFileName + ExtractFilename(filename);

     if fileinarchive(TmpFileName) > -1 then
        raise EGeneralArchiveException.create(FileAlreadyExists);

     TmpFileName := GetTmpFileName;
     NewFile := TFileStream.Create(TmpFileName,fmOpenReadWrite + fmShareExclusive);
     try

        try
           if not fnewarchive then
           begin
                ArchiveFile := TFileStream.Create(fArchive,fmOpenRead+fmShareExclusive);
                try
                   ArchiveFile.Position := 0;
                   NewFile.CopyFrom(ArchiveFile, fArchiveDetails.ArchiveSize);
                finally
                   ArchiveFile.Free;
                end;
           end;
        except
           Raise EGeneralArchiveException.create(format(UnableToOpenFile,[fArchive]));
        end;

        TmpStrm := TMemoryStream.create;
        try
           ZStream := TCompressionStream.Create(TCompressionLevel(fCompressionLevel), TmpStrm);
           try
              fProgressFlag := ifAdding;
              fProgressTotal := InStream.Size;
              fProgressIndex := 0;
              fProgressText := format(UpdatingFile,[filename]);
              TCompressionStream(ZStream).OnProgress := InternalProgress;
              ZStream.CopyFrom(InStream, 0);
           finally
              ZStream.Free;
           end;
           TmpStrm.position := 0;

           getmem(FileInfo,sizeof(TZLBFileRec));
           fillchar(FileInfo^,sizeof(TZLBFileRec),0);
           FileInfo^.CRC := GetCRC32(InStream,0);
           FileInfo^.name := extractfilename(filename);
           FileInfo^.path := extractfilepath(filename);
           if SavePaths then
              delete(FileInfo^.path,1,pos(':',FileInfo^.path))
           else
              fillchar(FileInfo^.path,sizeof(FileInfo^.path),#0);

           FileInfo^.osize := InStream.size;
           FileInfo^.start := NewFile.Position;

           if TmpStrm.Size >= InStream.size then
           begin
                NewFile.CopyFrom(InStream,0);
                FileInfo^.CSize := InStream.Size;
                FileInfo^.Status := fsStored;
                FileInfo^.CompressionLevel := fcNone;
           end
           else
           begin
                NewFile.CopyFrom(TmpStrm,0);
                FileInfo^.CSize := TmpStrm.Size;
                FileInfo^.Status := fsCompressed;
                FileInfo^.CompressionLevel := fCompressionLevel;
           end;

        finally
           TmpStrm.free;
        end;

        fArchiveRegistry.add(FileInfo);
        WriteRegistry(NewFile);

        NewFile.Free;
        DeleteFile(fArchive);
        RenameFile(TmpFileName,fArchive);
        TmpFileName := fArchive;
        CloseArchive;
        OpenArchive(TmpFileName);
     except
        DeleteFile(TmpFileName)
     end;
end;

Procedure TZLBArchive.UpdateFile(FileName:string; FileIndex:longint);
var
   ArchiveFile, InFile, NewFile, TmpStrm : TStream;
   ZStream : TCustomZLibStream;
   TmpFileName : string;
   FileInfo : TZLBFileRec;
   loop : longint;
begin
     if fArchive = '' then
        Raise ENoArchive.create(NoArchiveOpen);

     TmpFileName := '';
     if savepaths then TmpFileName := extractfilepath(filename);
     TmpFileName := TmpFileName + ExtractFilename(filename);

     if fileinarchive(TmpFileName) <> FileIndex then
        raise EGeneralArchiveException.create(FileNotFound);

     FileInfo := Files[FileIndex];

     TmpFileName := GetTmpFileName;
     NewFile := TFileStream.Create(TmpFileName,fmOpenReadWrite + fmShareExclusive);
     try
        try
           ArchiveFile := TFileStream.Create(fArchive,fmOpenRead+fmShareDenyWrite);
        except
           Raise EGeneralArchiveException.create(format(UnableToOpenFile,[fArchive]));
        end;

        try
           ArchiveFile.Position := 0;
           if FileInfo.Start > 0 then
              NewFile.CopyFrom(ArchiveFile,FileInfo.start);

           ArchiveFile.Position := ArchiveFile.Position + FileInfo.csize;
           try
              InFile := TFileStream.Create(filename, fmOpenRead+fmShareDenyWrite);
           except
              Raise EGeneralArchiveException.create(format(UnableToOpenFile,[filename]));
           end;

           try
              TmpStrm := TMemoryStream.create;
              try
                 ZStream := TCompressionStream.Create(TCompressionLevel(fCompressionLevel), TmpStrm);
                 try
                    fProgressFlag := ifUpdating;
                    fProgressTotal := Infile.Size;
                    fProgressIndex := 0;
                    fProgressText := format(UpdatingFile,[filename]);
                    TCompressionStream(ZStream).OnProgress := InternalProgress;
                    ZStream.CopyFrom(InFile, 0);
                 finally
                    ZStream.Free;
                 end;
                 TmpStrm.position := 0;
                 FileInfo.CRC := GetCRC32(InFile,0);
                 FileInfo.osize := Infile.Size;

                 if TmpStrm.Size >= InFile.size then
                 begin
                      NewFile.CopyFrom(InFile,0);
                      FileInfo.CSize := InFile.Size;
                      FileInfo.Status := fsStored;
                      FileInfo.CompressionLevel := fcNone;
                 end
                 else
                 begin
                      NewFile.CopyFrom(TmpStrm,0);
                      FileInfo.CSize := TmpStrm.Size;
                      FileInfo.Status := fsCompressed;
                      FileInfo.CompressionLevel := fCompressionLevel;
                 end;
              finally
                 TmpStrm.free;
              end;
              UpdateRegistryRec(FileIndex, FileInfo);
           finally
              InFile.free;
           end;

           for loop := FileIndex+1 to FileCount-1 do
           begin
                FileInfo := files[loop];
                FileInfo.start := NewFile.Position;
                UpdateRegistryRec(loop, FileInfo);
                if FileInfo.csize > 0 then
                  NewFile.CopyFrom(ArchiveFile,FileInfo.csize);
           end;

           WriteRegistry(NewFile);

        finally
           ArchiveFile.free;
        end;
        NewFile.Free;
        DeleteFile(fArchive);
        RenameFile(TmpFileName,fArchive);
        TmpFileName := fArchive;
        CloseArchive;
        OpenArchive(TmpFileName);
     except
        DeleteFile(TmpFileName)
     end;
end;

procedure TZLBArchive.UpdateFiles(FileList: TStrings);
var
   ArchiveFile, InFile, NewFile, TmpStrm : TStream;
   ZStream : TCustomZLibStream;
   TmpFileName, TestName : string;
   FileInfo : TZLBFileRec;
   loop, loop2 : longint;
   FileFound : boolean;
begin
     if fArchive = '' then
        Raise ENoArchive.create(NoArchiveOpen);

     loop := 0;
     while loop < Filelist.Count do
     begin
          if directoryexists(FileList[loop]) then
          begin
               if RecursePaths then
                  searchtree(FileList[loop],filelist);
               filelist.Delete(loop);
          end
          else
          inc(loop);
     end;

     TmpFileName := GetTmpFileName;
     NewFile := TFileStream.Create(TmpFileName,fmOpenReadWrite + fmShareExclusive);
     try
        try
           ArchiveFile := TFileStream.Create(fArchive,fmOpenRead+fmShareDenyWrite);
        except
           Raise EGeneralArchiveException.create(format(UnableToOpenFile,[fArchive]));
        end;
        try
           loop := 0;
           while loop < filecount do
           begin
                FileInfo := Files[loop];
                loop2 := 0;
                FileFound := false;
                while loop2 < filelist.count do
                begin
                     TestName := extractfilepath(filelist[loop2]);
                     if SavePaths then delete(TestName,1,pos(':',TestName))
                     else
                     TestName := '';
                     TestName := TestName + extractfilename(filelist[loop2]);
                     if lowercase(files[loop].path+files[loop].name) = lowercase(TestName) then
                     begin
                          filefound := true;
                          break;
                     end;
                     inc(loop2);
                end;

                if filefound then
                begin  //updating file from original archive with new file.
                     try
                        InFile := TFileStream.Create(filelist[loop2], fmOpenRead+fmShareDenyWrite);
                        try
                           TmpStrm := TMemoryStream.Create;
                           try
                              ZStream := TCompressionStream.Create(TCompressionLevel(fCompressionLevel), TmpStrm);
                              try
                                 fProgressFlag := ifUpdating;
                                 fProgressTotal := Infile.Size;
                                 fProgressIndex := 0;
                                 fProgressText := format(UpdatingFile,[filelist[loop2]]);
                                 TCompressionStream(ZStream).OnProgress := InternalProgress;
                                 ZStream.CopyFrom(InFile, 0);
                              finally
                                 ZStream.Free;
                              end;

                              FileInfo.CRC := GetCRC32(InFile,0);
                              FileInfo.osize := InFile.size;
                              FileInfo.start := NewFile.Position;

                              if TmpStrm.Size >= InFile.size then
                              begin
                                   NewFile.CopyFrom(InFile,0);
                                   FileInfo.CSize := InFile.Size;
                                   FileInfo.Status := fsStored;
                                   FileInfo.CompressionLevel := fcNone;
                              end
                              else
                              begin
                                   NewFile.CopyFrom(TmpStrm,0);
                                   FileInfo.CSize := TmpStrm.Size;
                                   FileInfo.Status := fsCompressed;
                                   FileInfo.CompressionLevel := fCompressionLevel;
                              end;
                           finally
                              TmpStrm.Free;
                              filelist.Delete(loop2);
                           end;
                           UpdateRegistryRec(Loop, FileInfo);
                        finally
                           InFile.free;
                        end;
                     except
                        Raise EGeneralArchiveException.create(format(UnableToOpenFile,[fArchive]));
                     end;
                end
                else
                begin  //Moving file from original archive.
                     ArchiveFile.Seek(FileInfo.start,soFromBeginning);
                     FileInfo.start := NewFile.Position;
                     if (FileInfo.csize > 0) then
                        NewFile.CopyFrom(ArchiveFile,FileInfo.csize);
                     UpdateRegistryRec(loop,FileInfo);
                end;
                inc(loop);
           end;

           WriteRegistry(NewFile);

        finally
           ArchiveFile.free;
        end;
        NewFile.Free;
        DeleteFile(fArchive);
        RenameFile(TmpFileName,fArchive);
        TmpFileName := fArchive;
        CloseArchive;
        OpenArchive(TmpFileName);
     except
        DeleteFile(TmpFileName)
     end;
end;

procedure TZLBArchive.UpdateStream(FileName: string; InStream: TStream);
var
   ArchiveFile, NewFile, TmpStrm : TStream;
   ZStream : TCustomZLibStream;
   TmpFileName : string;
   FileInfo : TZLBFileRec;
   loop, FileIndex : longint;
begin
     if fArchive = '' then
        Raise ENoArchive.create(NoArchiveOpen);

     TmpFileName := '';
     if savepaths then TmpFileName := extractfilepath(filename);
     TmpFileName := TmpFileName + ExtractFilename(filename);

     FileIndex := fileinarchive(TmpFileName);
     if FileIndex = -1 then
        raise EGeneralArchiveException.create(FileNotFound);

     FileInfo := Files[FileIndex];

     TmpFileName := GetTmpFileName;
     NewFile := TFileStream.Create(TmpFileName,fmOpenReadWrite + fmShareExclusive);
     try
        try
           ArchiveFile := TFileStream.Create(fArchive,fmOpenRead+fmShareDenyWrite);
        except
           Raise EGeneralArchiveException.create(format(UnableToOpenFile,[fArchive]));
        end;

        try
           ArchiveFile.Position := 0;
           if FileInfo.Start > 0 then
              NewFile.CopyFrom(ArchiveFile,FileInfo.start);

           ArchiveFile.Position := ArchiveFile.Position + FileInfo.csize;

           TmpStrm := TMemoryStream.create;
           try
              ZStream := TCompressionStream.Create(TCompressionLevel(fCompressionLevel), TmpStrm);
              try
                 fProgressFlag := ifUpdating;
                 fProgressTotal := InStream.Size;
                 fProgressIndex := 0;
                 fProgressText := format(UpdatingFile,[filename]);
                 TCompressionStream(ZStream).OnProgress := InternalProgress;
                 ZStream.CopyFrom(InStream, 0);
              finally
                 ZStream.Free;
              end;
              TmpStrm.position := 0;
              FileInfo.CRC := GetCRC32(InStream,0);
              FileInfo.osize := InStream.Size;

              if TmpStrm.Size >= InStream.size then
              begin
                   NewFile.CopyFrom(InStream,0);
                   FileInfo.CSize := InStream.Size;
                   FileInfo.Status := fsStored;
                   FileInfo.CompressionLevel := fcNone;
              end
              else
              begin
                   NewFile.CopyFrom(TmpStrm,0);
                   FileInfo.CSize := TmpStrm.Size;
                   FileInfo.Status := fsCompressed;
                   FileInfo.CompressionLevel := fCompressionLevel;
              end;
           finally
              TmpStrm.free;
           end;
           UpdateRegistryRec(FileIndex, FileInfo);

           for loop := FileIndex+1 to FileCount-1 do
           begin
                FileInfo := files[loop];
                FileInfo.start := NewFile.Position;
                UpdateRegistryRec(loop, FileInfo);

                if FileInfo.csize > 0 then
                   NewFile.CopyFrom(ArchiveFile,FileInfo.csize);
           end;

           WriteRegistry(NewFile);

        finally
           ArchiveFile.free;
        end;
        NewFile.Free;
        DeleteFile(fArchive);
        RenameFile(TmpFileName,fArchive);
        TmpFileName := fArchive;
        CloseArchive;
        OpenArchive(TmpFileName);
     except
        DeleteFile(TmpFileName)
     end;
end;

procedure TZLBArchive.DeleteFiles(FileList:TStrings);
var
   TmpFile, OriginalFile : TStream;
   loop, loop2 : longint;
   filefound : boolean;
   TmpStr : string;
   TmpFileName : string;
   FileRec : TZLBFileRec;
begin
     if fArchive = '' then
        Raise ENoArchive.create(NoArchiveOpen);

     loop := 0;
     while loop < filecount do
     begin
          loop2 := 0;
          filefound := false;
          while loop2 < filelist.count do
          begin
               TmpStr := extractfilepath(filelist[loop2]);
               delete(TmpStr,1,pos(':',TmpStr));
               TmpStr := TmpStr + extractfilename(filelist[loop2]);
               if lowercase(files[loop].path+files[loop].name) = lowercase(TmpStr) then
               begin
                    filefound := true;
                    break;
               end;
               inc(loop2);
               filefound := false;
          end;
          if filefound then
          begin
               filelist.delete(loop2);
               Freemem(fArchiveRegistry[loop],sizeof(TZLBFileRec));
               fArchiveRegistry.Delete(loop);
          end
          else
          inc(loop);
     end;

     TmpFileName := GetTmpFileName;
     TmpFile := TFileStream.Create(TmpFileName,fmOpenReadWrite + fmShareExclusive);
     try
        try
           OriginalFile := TFileStream.Create(fArchive,fmOpenRead+fmShareDenyWrite);
           try
              if filecount > 0 then
              begin
                   loop := 0;
                   while loop < filecount do
                   begin
                        FileRec := files[loop];

                        OriginalFile.Seek(FileRec.start,soFromBeginning);
                        FileRec.Start := TmpFile.Position;
                        if FileRec.csize > 0 then
                           TmpFile.CopyFrom(OriginalFile,FileRec.csize);

                        UpdateRegistryRec(loop,filerec);

                        inc(loop);
                   end;
              end;
           finally
              OriginalFile.Free;
           end;
        except
           Raise EGeneralArchiveException.create(format(UnableToOpenFile,[fArchive]));
        end;

        WriteRegistry(TmpFile);

        TmpFile.Free;
        DeleteFile(fArchive);
        RenameFile(TmpFileName,fArchive);
     finally
        TmpFileName := fArchive;
        CloseArchive;
        OpenArchive(TmpFileName);
        fProgressFlag := ifClear;
        InternalProgress(self);
     end;
end;

procedure TZLBArchive.CreateArchive(filename: string);
begin
     CloseArchive;
     fArchive := FileName;
     if (FileName = '') then
     begin
          fArchive := '';
          exit;
     end;
     if fileexists(filename) then
        Raise EGeneralArchiveException.create(FileAlreadyExists);
     fNewArchive := true;
     if assigned(fOnArchiveOpened) then fOnArchiveOpened(self);
end;

function TZLBArchive.FileInArchive(filename: string): longint;
var
   loop : longint;
   tmpstr:string;
begin
     if fArchive = '' then
        Raise ENoArchive.create(NoArchiveOpen);

     result := -1;
     loop := 0;

     TmpStr := extractfilepath(filename);
     if SavePaths then delete(TmpStr,1,pos(':',TmpStr));
     TmpStr := TmpStr + extractfilename(filename);

     while loop < fArchiveRegistry.count do
     begin
          if lowercase(Files[loop].Path + Files[loop].Name) = lowercase(tmpstr) then
          begin
               result := loop;
               break;
          end;
          inc(loop);
     end;
end;

function TZLBArchive.GetTmpFileName: string;
var
   tmppath : string;
   tmpfile : string;
begin
     setlength(tmppath, max_path);
     setlength(tmpfile, max_path);
     GetTempPath(max_path, pchar(tmppath));
     GetTempFilename(pchar(tmppath),'ZLB',0,pchar(tmpfile));
     result := tmpfile;
end;

procedure TZLBArchive.InternalProgress(Sender:TObject);
begin
     if not (csdestroying in componentstate) then
     begin
          case fProgressFlag of
            ifOpening:if assigned(fOnProgress) then fOnprogress(fProgressText,round((fprogressIndex / fProgressTotal)*100));
            ifExtracting:if assigned(fOnProgress) then fOnprogress(fProgressText,round((fprogressIndex / fProgressTotal)*100));
            ifAdding:if assigned(fOnProgress) then fOnprogress(fProgressText,round((TCustomZLibStream(Sender).Position / fProgressTotal)*100));
            ifUpdating:if assigned(fOnProgress) then fOnprogress(fProgressText,round((TCustomZLibStream(Sender).Position / fProgressTotal)*100));
            ifWritingRegistry:if assigned(fOnProgress) then fOnprogress(fProgressText,round((fprogressIndex / fProgressTotal)*100));
            ifClear:if assigned(fOnProgress) then fOnprogress(fArchive,0);
          end;
     end;
end;

procedure TZLBArchive.UpdateRegistryRec(FileIndex: longint;
  FileInfo: TZLBFileRec);
begin
     TZLBFileRec(fArchiveRegistry[FileIndex]^) := FileInfo;
end;

procedure TZLBArchive.WriteRegistry(DataStrm: TStream);
var
   TmpStrm : TStream;
   loop : longint;
   ArchiveRec : TZLBArchiveRec;
   ZStream: TCustomZLibStream;
begin
     TmpStrm := tmemorystream.Create;
     try
        try
           Fillchar(ArchiveRec,sizeof(TZLBArchiveRec),#0);
           ArchiveRec.ArchiveSize := DataStrm.Size;
           tmemorystream(TmpStrm).SetSize(sizeof(TZLBFileRec)*fArchiveRegistry.count);
           for loop := 0 to fArchiveRegistry.count-1 do
               TmpStrm.WriteBuffer(TZLBFileRec(fArchiveRegistry.items[loop]^),sizeof(TZLBFileRec));
           ZStream := TCompressionStream.Create(TCompressionLevel(fCompressionLevel), DataStrm);
           try
              fProgressFlag := ifWritingRegistry;
              fProgressTotal := TmpStrm.Size;
              fProgressIndex := 0;
              fProgressText := WritingRegistry;
              TCompressionStream(ZStream).OnProgress := InternalProgress;
              ZStream.CopyFrom(TmpStrm, 0);
           finally
              ZStream.Free;
           end;
           ArchiveRec.RegistrySize := DataStrm.size - ArchiveRec.ArchiveSize;
           ArchiveRec.CompressedRegistry := true;
        except
           for loop := 0 to fArchiveRegistry.count-1 do
               DataStrm.Writebuffer(TZLBFileRec(fArchiveRegistry.items[loop]^),sizeof(TZLBFileRec));
           ArchiveRec.CompressedRegistry := false;
        end;
        ArchiveRec.FileCount := fArchiveRegistry.count;
        ArchiveRec.Version := 1;
        ArchiveRec.CRC := GetCRC32(DataStrm,0);

        DataStrm.WriteBuffer(ArchiveRec,SizeOf(TZLBArchiveRec));
     finally
        TmpStrm.free;
     end;
end;

procedure TZLBArchive.RenameFileByName(Original, New: string);
var
   ArchiveFile, NewFile : TStream;
   TmpFileName : string;
   FileInfo : TZLBFileRec;
   index : integer;
begin
     if fArchive = '' then
        Raise ENoArchive.create(NoArchiveOpen);

     index := fileinarchive(original);
     if index <> -1 then
     begin
          TmpFileName := GetTmpFileName;
          NewFile := TFileStream.Create(TmpFileName,fmOpenReadWrite + fmShareExclusive);
          try
             if not fnewarchive then
             begin
                  try
                     ArchiveFile := TFileStream.Create(fArchive,fmOpenRead+fmShareDenyWrite);
                  except
                     Raise EGeneralArchiveException.create(format(UnableToOpenFile,[fArchive]));
                  end;
                  try
                     ArchiveFile.Position := 0;
                     NewFile.CopyFrom(ArchiveFile, fArchiveDetails.ArchiveSize);
                  finally
                     ArchiveFile.Free;
                  end;
             end;

             FileInfo := Files[index];
             FileInfo.name := extractfilename(new);
             FileInfo.path := extractfilepath(new);
             if FileInfo.path[length(FileInfo.path)] <> '\' then FileInfo.path := FileInfo.path + '\';
             UpdateRegistryRec(index,FileInfo);

             WriteRegistry(NewFile);

             NewFile.Free;
             DeleteFile(fArchive);
             RenameFile(TmpFileName,fArchive);
             TmpFileName := fArchive;
             CloseArchive;
             OpenArchive(TmpFileName);
          except
             DeleteFile(TmpFileName)
          end;



     end;
end;

end.
