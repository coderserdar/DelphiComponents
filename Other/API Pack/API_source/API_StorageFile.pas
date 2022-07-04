unit API_StorageFile;

//------------------------------------------------------------------------------
// component free to use and modify, subject to following restinctions:
// 1. do not mispresent the origin
// 2. altered revisions must be clearly marked as modified from the original
// 3. do not remove this notice from the source code
// 4. send email about bugs, features needed and features you would like
// * if you like this very much, feel free to donate and support supporting
// and developing the package at www.paypal.com - ari pikivirta@kolumbus.fi
//------------------------------------------------------------------------------

{
  API_storagefile
  ---------------
  with this component you can make one file containing many
  other files and extract them later on one by one back to
  actual file or memorystream, depending on which one is
  needed. Uses ZLIB for compression, so can be used as any atchiver available.

  History
  -------

  03112009, r1.04, ari pikivirta
    * replaced all chars with ansichar for or because unicode stuff on d2009 and above

  12102009, r1.03, ari pikivirta
    * fixed warning on fileage (deprecated)

  11102008, r1.02, ari pikivirta
    * added FileIndex(Filename) function
    * added one overload for the EXtractFile function
    * note! all of the filenames allow wildcards ? and * in name

  07102008, r1.01, ari pikivirta
    * added several events to make use of this component even more nice

  28032006, r1.00, Ari Pikivirta
    * first revision
}

interface

uses
  Windows, SysUtils, Classes, Dialogs, SyncObjs;

type
  // events
  TAPI_Storagefile_AddFilesProgress = procedure(Sender: TComponent;
    const AFilename: String;
    const AIndex: Integer;
    const ATotalAmount: Integer) of object;

  TAPI_Storagefile_FileProgress = procedure(Sender: TComponent;
    const AFilename: String;
    const AFileSize: Int64;
    const APosition: Int64) of object;

  // header structure of storage file
  TAPI_StorageFile_Header = record
    filename:   array[0..512] of Ansichar;  // filename (with path)
    date:       array[0..32] of Ansichar;   // integer, filetime
    attributes: array[0..32] of Ansichar;   // file attributes
    crc32:      array[0..32] of Ansichar;   // check sum
    sizeorig:   array[0..32] of Ansichar;   // original file size
    sizecomp:   array[0..32] of Ansichar;   // size compressed
    //content.. compressed using zlib..
  end;

  TAPI_StorageFile = class(TComponent)
  private
    { Private declarations }
    flock: tcriticalsection;
    fversion: string;
    ffilename: string;
    fonfileprogress: TAPI_Storagefile_FileProgress;
    fonaddfiles: TAPI_Storagefile_AddFilesProgress;
    procedure dummys(s: string);
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(AOwner: tcomponent); override;
    destructor  Destroy; override;
    function    AddFiles(Const files: tstrings): boolean;
    procedure   FileList(List: tstrings);
    function    FileIndex(var AFilename: string): integer; // -1 if doesn't exist
    function    ExtractFile(
                  Const IndexToExtract: integer;
                  var AFileName: string;
                  var AFileSize: Int64;
                  var Attr: Word;
                  var AFileDate: TDateTime;
                  var AChecksum: cardinal;
                  var AStream: tstream): boolean;
                  overload;
    function    ExtractFile(Const fname, tofolder: string): boolean; overload;
    function    ExtractFile(Const fname: string; var AStream: tstream): boolean; overload;
  published
    { Published declarations }
    property Version: string read fversion write dummys stored false;
    property Filename: string read ffilename write ffilename;
    property OnAddFiles: TAPI_Storagefile_AddFilesProgress
              read fonaddfiles
              write fonaddfiles;
    property OnFileProgress: TAPI_Storagefile_FileProgress
              read fonfileprogress
              write fonfileprogress;
  end;

procedure Register;

implementation

{$r *.res}

uses
  api_strings, api_compress, api_files;

const
  // storage file header
  TSTORAGEFILEHEADER: shortstring = 'SFH2';
  TSTORAGEFILEHEADER_OLD: shortstring = 'SFH1';
  TRANSFERBUFFERSIZE = high(word) div 2;

//------------------------------------------------------------------------------
procedure TAPI_StorageFile.dummys(s: string);
begin
  // does nothing
end;

//------------------------------------------------------------------------------
constructor TAPI_StorageFile.Create(aowner: tcomponent);
begin
  inherited create(aowner);
  fversion:= 'r1.04/ari.pikivirta]at[kolumbus.fi';
  flock:= tcriticalsection.create;
end;

//------------------------------------------------------------------------------
destructor TAPI_StorageFile.Destroy;
begin
  flock.Free;
  inherited destroy;
end;

//------------------------------------------------------------------------------
function TAPI_StorageFile.AddFiles(Const files: tstrings): boolean;
var
  fs, rs: tfilestream;
  ms: tmemorystream;
  i: integer;
  buf: array[0..TRANSFERBUFFERSIZE] of Ansichar;
  bread: int64;
  head: TAPI_StorageFile_Header;
  root: string;
  ATempFileDate: TDateTime;
  ffs: tformatsettings;
begin
  flock.Acquire;
  try
    ffs.DecimalSeparator:= '.'; // dot as decimal separator
    ffs.ThousandSeparator:= #0; // no thousand sepazator
    //
    result:= false;
    if (ffilename<>'') and (files.count>0) then
    begin
      fs:= tfilestream.create( ffilename, fmCreate );
      try
        // get root for the files..
        root:= extractfiledir( ffilename );
        // write file header
        fs.writebuffer(TStorageFileHeader, sizeof(TSTORAGEFILEHEADER));
        // add files
        for i:=0 to files.count-1 do
        begin
          // fire event if assigned
          if assigned(fonaddfiles) then
            fonaddfiles(self, files[i], i, files.count);
          // make header info
          strPcopy( head.filename, extractrelativepath(root ,files[i])+#0 );
          fileage(files[i], ATempFileDate); // using only one parameter is deprecated!
          strPcopy( head.date, floattostr(ATempFileDate, ffs)+#0 );
          strPcopy( head.attributes, inttostr(GetFileAttributes(pchar(files[i])))+#0 ); // SetFileAttributes(pchar(filename), attribs);
          strPcopy( head.crc32, inttostr(api_files.CRC32(files[i]))+#0 );
          // showmessage( strpas( head.crc32 ) );
          rs:= tfilestream.create(files[i], fmOpenRead);
          try
            ms:= tmemorystream.create;
            try
              api_compress.CompressStream(rs, ms);
              strPcopy( head.sizeorig, inttostr(rs.size)+#0 );
              strPcopy( head.sizecomp, inttostr(ms.size)+#0 );
              // write header
              fs.Write(head, sizeof(head));
              // write file content compressed
              ms.Seek(0, soFromBeginning);
              while (ms.Position<ms.Size) do
              begin
                bread:= ms.Read(buf, sizeof(buf));
                fs.WriteBuffer(buf, bread);
                // fire event
                if assigned(fonfileprogress) then
                  fonfileprogress(self, files[i], ms.size, ms.Position);
              end;
            finally
              freeandnil(ms); //ms.free;
            end;
          finally
            freeandnil(rs); //rs.free;
          end;
        end;
        result:= true;
      finally
        freeandnil(fs); //fs.free;
      end;
    end;
  finally
    flock.Release;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_StorageFile.FileList(List: tstrings);
var
  fs: tfilestream;
  i64: int64;
  s: shortstring;
  head: TAPI_StorageFile_Header;
begin
  flock.Acquire;
  try
    list.clear;
    if sysutils.fileexists(ffilename) then // if storagefile exists
    begin
      // open file for reading
      fs:= tfilestream.Create(ffilename, fmopenread or fmShareDenyWrite);
      try
        // read file header
        fs.Readbuffer(s, sizeof(Tstoragefileheader));
        if (s<>TSTORAGEFILEHEADER) then
        begin
          messagedlg('Storage file header check failed.', mterror, [mbok], 0);
          exit;
        end;
        // read all files
        while (fs.Position<fs.Size) do
        begin
          // read header
          fs.readbuffer(head, sizeof(head));
          // add filename to list
          list.Add(strPas(head.filename));
          // seek to next file header
          i64:= strtoint64(strpas(head.sizecomp));
          fs.seek(i64, soFromCurrent);
        end;
      finally
        freeandnil(fs); //fs.free;
      end;
    end;
  finally
    flock.Release;
  end;
end;

//------------------------------------------------------------------------------
function TAPI_StorageFile.ExtractFile(
  Const IndexToExtract: integer;
  var AFileName: string;
  var AFileSize: Int64;
  var Attr: Word;
  var AFileDate: TDateTime;
  var AChecksum: cardinal;
  var AStream: tstream): boolean;
var
  fs: tfilestream;
  ms: tmemorystream;
  i64: int64;
  s: shortstring;
  head: TAPI_StorageFile_Header;
  curindex: integer;
  buf: array[0..TRANSFERBUFFERSIZE] of Ansichar;
  bread: int64;
  ffs: tformatsettings; // for decimal separator
begin
  flock.Acquire;
  try
    ffs.DecimalSeparator:= '.'; // dot as decimal separator
    ffs.ThousandSeparator:= #0; // no thousand sepazator
    //
    result:= false;
    if (sysutils.fileexists(FFilename)) and (Assigned(Astream)) then
    begin
      fs:= tfilestream.Create(Ffilename, fmopenread or fmShareDenyWrite);
      try
        // read file header
        fs.Readbuffer(s, sizeof(Tstoragefileheader));
        if s<>tstoragefileheader then
        begin
          messagedlg('Storage file header check failed.', mterror, [mbok], 0);
          exit;
        end;
        // read files until we find the correct one
        // to extract from the package
        CurIndex:= 0;
        while (fs.Position<fs.Size) do
        begin
          fs.readbuffer(head, sizeof(head));
          i64:= strtoint64(strpas(head.sizecomp));
          if (IndexToExtract = CurIndex) then
          begin
            // get rest of header data
            AFileName:= strpas(head.filename);
            AFileSize:= strtoint64(strpas(head.sizeorig));
            AFileDate:= strtofloat(strpas(head.date), ffs); // !!
            Attr:= strtoint(strpas(head.attributes));
            AChecksum:= strtoint(strpas(head.crc32));
            // extract and decompres file
            ms:= tmemorystream.create;
            try
              ms.seek(0, SoFromBeginning);
              while (ms.position<AFileSize) do
              begin
                bread:= (AFileSize - ms.position); // get remaining bytes
                if (bread>sizeof(buf)) then bread:= sizeof(buf); // limit to buffer size
                bread:= fs.Read(buf, bread); // read from storage file
                ms.WriteBuffer(buf, bread); // write into memory stream
                // fire file event
                if assigned(fonfileprogress) then
                  fonfileprogress(self, afilename, ms.position, AFileSize);
              end;
              // decompress output stream
              ms.seek(0, SoFromBeginning);
              DecompressStream(ms, astream);
              // important note!
              // file attributes must be set by the
              // procedure that called this function,
              // as well as the checksum check!
            finally
              freeandnil(ms); //ms.free;
            end;
            result:= true;
            break;
          end else
          begin
            // seek to next file
            fs.seek(i64, soFromCurrent);
            CurIndex:= CurIndex + 1;
          end;
        end;
      finally
        freeandnil(fs); //fs.free;
      end;
    end;
  finally
    flock.Release;
  end;
end;

//------------------------------------------------------------------------------
function TAPI_storagefile.FileIndex(var AFilename: string): integer;
var
  list: tstringlist;
  i: integer;
begin
  result:= -1;
  if AFilename='' then exit;

  // get list of all files in archive
  list:= tstringlist.create;
  try
    filelist( list );

    // get index of file specified using
    // wildcards (allows to use * and ? on FNAME)
    for i:=0 to list.count-1 do
      if api_strings.Match(ansistring(AFileName), ansistring(list[i])) then
      begin
        AFilename:= list[i];
        result:= i;
        break;
      end;
  finally
    list.free;
  end;
end;

//------------------------------------------------------------------------------
function TAPI_StorageFile.ExtractFile(Const fname: string; var AStream: tstream): boolean;
var
  index: integer;
  afilename: string;
  afilesize: int64;
  attr: word;
  afiledate: Tdatetime;
  achecksum: cardinal;
begin
  afilename:= fname;
  index:= FileIndex(afilename);

  // if file was found, extract it
  if (index>-1) and (AStream<>nil) then
  begin
    result:= ExtractFile( index, afilename, afilesize, attr, afiledate, achecksum, AStream );
  end else
  begin
    AStream:= nil;
    result:= false;
  end;
end;

//------------------------------------------------------------------------------
function TAPI_StorageFile.ExtractFile(Const fname, tofolder: string): boolean;
var
  index: integer;
  fs: tstream;
  afilename: string;
  attr: word;
  afilesize: int64;
  afiledate: tdatetime;
  achecksum, acrc32: cardinal;
begin
  result:= false;
  afilename:= fname;
  index:= FileIndex(afilename);

  if (index>-1) then
  begin
    // create directories if needed
    forcedirectories( extractfiledir(addbackslash(tofolder)+afilename) );
    // save file into
    fs:= tfilestream.create( addbackslash(tofolder)+afilename, fmCreate );
    try
      result:= ExtractFile( index, afilename, afilesize, attr, afiledate, achecksum, fs );
    finally
      fs.free;
    end;
    if result then
    begin
      // apply file date
      filedatetime(addbackslash(tofolder)+afilename, afiledate);
      // apply file attributes
      SetFileAttributes(pchar(addbackslash(tofolder)+afilename), attr);
      // check checksum for proper file content!
      acrc32:= api_files.CRC32(addbackslash(tofolder)+afilename);
      result:= (acrc32 = achecksum);
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure Register;
begin
  RegisterComponents('API Misc', [TAPI_StorageFile]);
end;

end.
