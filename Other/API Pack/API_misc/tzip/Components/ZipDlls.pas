//------------------------------------------------------------------------------
// Module            ZipDlls                                                   .
// Version:          1.4                                                       .
// Date:             6 April 2003                                              .
// Compilers:        Delphi 3 - Delphi 7, C++ Builder 3 - C++ Builder 5        .
// Authors:          Angus Johnson - angusj-AT-myrealbox-DOT-com               .
//                   Eric W. Engler                                            .
// Copyright:        © 2001-2003                                               .
//                                                                             .
// Description:      Delphi interface to the ZipDll.dll & UnzDll.dll libraries .
//                   created by Eric W. Engler                                 .
// -----------------------------------------------------------------------------

Unit ZipDlls;

{$IFDEF VER120} //delphi 4
  {$DEFINE VER120_PLUS}
{$ENDIF}
{$IFDEF VER125} //bcb 4
  {$DEFINE VER120_PLUS}
{$ENDIF}
{$IFDEF VER130} //delphi 5
  {$DEFINE VER120_PLUS}
{$ENDIF}
{$IFDEF VER135} //bcb 5
  {$DEFINE VER120_PLUS}
{$ENDIF}
{$IFDEF VER140} //delphi 6
  {$DEFINE VER120_PLUS}
{$ENDIF}
{$IFDEF VER150} //delphi 7
  {$DEFINE VER120_PLUS}
{$ENDIF}

Interface

Uses Windows;

Type

{$WARNINGS OFF} //hides 'Unsafe Type' compiler warnings

{$IfDef VER100}
  LongWord = Cardinal;
{$EndIf}

//---------------------------------------------------------------------
// Callback record structure used by both ZIPDLL.DLL & UNZDLL.DLL ...
//---------------------------------------------------------------------

  pZCallBackStruct = ^TZCallBackStruct;
  TZCallBackStruct = packed record
     Handle: THandle;
     Caller: Pointer;          //pointer to "self"
     Version: LongInt;         //expected dll version
     IsOperationZip: LongBool; //true=zip, false=unzip
     ActionCode: LongInt;
     ErrorCode: LongInt;
     Size: LongInt;
     Msg: Array[0..511] of ansiChar;
  end;

  ZFunctionPtrType = function(ZCallbackRec: PZCallBackStruct): LongBool; stdcall;

//---------------------------------------------------------------------
// record structures used by ZIPDLL.DLL ...
//---------------------------------------------------------------------

  pZipFileData = ^TZipFileData;
  TZipFileData = packed record
    FileSpec:       PansiChar;
    FileComment:    PansiChar;
    FileAltName:    PansiChar;
    Password:       PansiChar;
    Encrypt:        LongBool;
    Recurse:        Word;
    NoRecurseFile:  Word;
    DateUsed:       LongBool;
    Date:           Array[0..7] of AnsiChar;
    AddRoot:        PansiChar;
    NotUsed:        Array[0..15] of Cardinal;
  end;

  pExclFileData = ^TExclFileData;
  TExclFileData = packed record
   fFileSpec:        PAnsiChar;
  end;

  //Record structure passed to ZipDll.dll ...
  pZipParams = ^TZipParams;
  TZipParams = packed record
    Handle:          THandle;
    Caller:          Pointer;  //used as pointer to self in callback function
    Version:         LongInt;  //expected dll version
    ZCallbackFunc:   ZFunctionPtrType;
    TraceEnabled:   LongBool;
    ZipPassword:    PAnsiChar;    //pointer to password
    Suffix:         LongBool; //unused
    Encrypt:        LongBool; //encrypt new files
    SystemFiles:    LongBool; //include system & hidden files
    Volume:         LongBool; //save volume label
    Extra:          LongBool; //save file attributes
    NoDirEntries:   LongBool; //don't save directory names (see also fJunkDir)
    UseDate:        LongBool; //use date array to replace updated files
    VerboseEnabled: LongBool; //feedback extra info from dll with messages
    Quiet:          LongBool; //dll shows no messages, ie: handle messages via callbacks
    Level:          LongInt;  //compression level: 0=nil, 9=max
    ComprSpecial:   LongBool; //try and compress compressed file types eg: gif, zip
    CRLF_LF:        LongBool; //translate textfile eol's
    JunkDir:        LongBool; //ignore saving directories
    Recurse:        WordBool; //recurse subdirectories
    NoRecurseFiles: Word;
    Grow:           LongBool; //allow append of files
    ForceDOS:       LongBool; //for 8.3 filenames -> PKUNZIP v2.04g compatibility
    Move:           LongBool; //delete uncompressed originals after added to zip.
    DeleteEntries:  LongBool; //delete specified files from archive
    Update:         LongBool; //add - update
    Freshen:        LongBool; //add - freshen
    JunkSFX:        LongBool; //strip sfx prefix for self-extracting archives
    LatestTime:     LongBool; //set zip archive to datetime of latest zipped file
    Date:           Array[0..7] of AnsiChar; //if fDate=true then add newer than: MMDDYY**
    FileDataCount:  LongInt;  //number of Files (or FileSpecs) to add/delete
    ZipFilename:    PAnsiChar;    //pointer to archive name

    TempPath:       PAnsiChar;    //pointer to temp path
    FileComment:    PAnsiChar;    //pointer to comments
    ArchivedOnly:   WordBool; //only zip when archive bit set (v1.60)
    ResetArchive:   WordBool; //reset the archive bit after a successfull zip (v1.60)
    FileData:       pZipFileData; //pointer to array of FileData
    ForceWin:       LongBool;
    ExclDataCount:  LongInt;  //number of excluded FileSpecs
    ExclFileData:   pExclFileData; // pointer to array of FileSpecs to exclude from zipping.

    UseOutStream:     LongBool;   // NEW component v160M, dll v1.6015 Use memory stream as output.
    OutStream:        Pointer;    // NEW component v160M, dll v1.6015 Pointer to the start of the output stream data.
    OutStreamSize:    LongWord;   // NEW component v160M, dll v1.6015 Size of the Output data.
    UseInStream:      LongBool;   // NEW component v160M, dll v1.6015 Use memory stream as input.
    InStream:         Pointer;    // NEW component v160M, dll v1.6015 Pointer to the start of the input stream data.
    InStreamSize:     LongWord;   // NEW component v160M, dll v1.6015 Size of the input data.
    StrFileAttr:      DWORD;      // NEW component v160M, dll v1.6015 File attributes of the file stream.
    StrFileDate:      DWORD;      // NEW component v160M, dll v1.6015 File date/time to set for the streamed file.
    HowToMove:        LongBool;
    WantedCodePage:   SmallInt;
    NotUsed0:         SmallInt;
    NotUsed:        Array[0..3] of Cardinal;
    Seven:          Integer;  //$0007
  end;

//---------------------------------------------------------------------
// record structures used by UNZDLL.DLL ...
//---------------------------------------------------------------------

  pUnzipFileData = ^TUnzipFileData;
  TUnzipFileData = packed record
    fFileSpec:       PAnsiChar;
    fFileAltName:    PAnsiChar;
    fPassword:       PAnsiChar;
    fNotUsed:        Array[0..14] of Cardinal;
  end;

  pUnzipExFileData = ^TUnzipExFileData;
  TUnzipExFileData = packed record
    fFileSpec:       PAnsiChar;
    fNotUsed:        Array[0..2] of Cardinal;
  end;

  //Record structure passed to UnzDll.dll ...
  pUnZipParams = ^TUnZipParams;
  TUnZipParams = packed record
    Handle:             THandle;
    Caller:             Pointer;  //used as pointer to self in callback function
    Version:            LongInt;  //expected dll version
    ZCallbackFunc:      ZFunctionPtrType;
    TraceEnabled:      LongBool;
    PromptToOverwrite: LongBool;
    ZipPassword:       PAnsiChar;
    Test:              LongBool;  //test that files extract then trash extracted files
    Comments:          LongBool;
    Convert:           LongBool;  //if true, do ASCII/EBCDIC or EOL translation
    Quiet:             LongBool;
    VerboseEnabled:    LongBool;
    Update:            LongBool;  //"update" (extract only newer files & brand new files)
    Freshen:           LongBool;  //"freshen" (extract only newer files that already exist)
    Directories:       LongBool;  //if true, recreate dir structure
    Overwrite:         LongBool;  //if true, overwrite existing (no asking)
    FileDataCount:     LongInt;   //number of files to extract
    ZipFilename:       PAnsiChar;     //pointer to archive name

    {Pointer to an Array of UnzipFileData records}
    UnzipFileData:     pUnzipFileData;
    {Pointer to an Array of ExUnzipFileData records}
    UnzipExFileData:   pUnzipExFileData;
    UseOutStream:      LongBool;  //Use Memory stream as output. (v1.60)
    OutStream:         Pointer;   //Pointer to the start of streaam data. (v1.60)
    OutStreamSize:     LongInt;   //Size of the output data. (v1.60)
    UseInStream:       LongBool;  //Use memory stream as input. (v1.60)
    InStream:          Pointer;   //Pointer to the start of the input stream data. (v1.60)
    InStreamSize:      LongInt;   //Size of the input data. (v1.60)
    PwdReqCount:       Cardinal;  //No. of times a password will be asked for per file (v1.60)
    ExtractRoot:       PAnsiChar;     //NEW VER160p: pointer to Extract Base Dir.
    NotUsed:           Array[0..7] of Cardinal;
    Seven:             LongInt;   //$0007
  end;

//---------------------------------------------------------------------
// ZIP record strucures (as defined in PKWare's documentation) ...
//---------------------------------------------------------------------


//Local file structure:
//  [local file header + filename+ ExtraLen + file data +/- data_descriptor] . . .


  pLocalHeader = ^TLocalHeader;
  TLocalHeader = packed record
    HeaderSig          : Cardinal; // $04034b50 (4)
    VersionNeed        : Word;
    Flag               : Word;
    ComprMethod        : Word;
    FileTime           : Word;
    FileDate           : Word;
    CRC32              : Cardinal;
    ComprSize          : integer;
    UnComprSize        : integer;
    FileNameLen        : Word;
    ExtraLen           : Word;
  end;


  TDataDescriptor = packed record  //Exists only if bit 3 of LocalHeader.Flag is set.
    DescriptorSig      : Cardinal; //field not defined in PKWare's docs but used by WinZip
    CRC32              : Cardinal;
    ComprSize          : integer;
    UnComprSize        : integer;
  end;

(*
Central directory structure:
  [file header] . . .  end of central dir record
*)

  //array of TCentralFileHeaders constitute the Central Header directory...
  TCentralFileHeader = packed record  // fixed part size = 46 bytes
    HeaderSig          : Cardinal; // $02014b50 { 'PK'#1#2 } (4)
    MadeByVersion      : Byte;    //(1)
    HostVersionNo      : Byte;    //(1)
    Version            : Word;    //version needed to extract(2)
    Flag               : Word;    //(2)
    CompressionMethod  : Word;    //(2)
    FileDate           : Integer; //convert with FileDateToDateTime (4)
    CRC32              : Integer; //(4)
    CompressedSize     : Integer; //(4)
    UncompressedSize   : Integer; //(4)
    FileNameLength     : Word;    //(2)
    ExtraFieldLength   : Word;    //(2)
    FileCommentLen     : Word;    //(2)
    StartOnDisk        : Word;    //disk # on which file starts (2)
    IntFileAttrib      : Word;    //internal file attr. ie: Text/Binary (2)
    ExtFileAttrib      : Cardinal;//external file attr. (4)
    RelOffLocalHdr     : integer; //relative offset of LocalHeader (4)
  end;

  TEndOfCentralHeader = packed record  //Fixed part size = 22 bytes
    HeaderSig          : Cardinal; //$06054B50 (4)
    ThisDiskNo         : Word;     //This disk's number (zero based) (2)
    CentralDiskNo      : Word;     //Disk number on which central dir starts (2)
    ThisDiskEntries    : Word;     //Number of central dir entries on this disk (2)
    TotalEntries       : Word;     //Total entries in central dir (2)
    CentralSize        : integer;  //Size of central directory (4)
    CentralOffset      : integer;  //offset of central dir on CentralDiskNo (4)
    ZipCommentLen      : Word;     //(2)
  end;

const
  MULTIPLE_DISK_SIG      = $08074b50; // 'PK'#7#8
  DATA_DESCRIPT_SIG      = MULTIPLE_DISK_SIG; //!!
  LOCAL_HEADERSIG        = $04034b50; // 'PK'#3#4
  CENTRAL_HEADERSIG      = $02014b50; // 'PK'#1#2
  EOC_HEADERSIG          = $06054b50; // 'PK'#5#6
  PASSWORD_MAXLEN        = 80;        //Limit set by PKWare's Zip specs.

//---------------------------------------------------------------------

  ZIP_VERSION            = 160;       //DelZip's Dll version number

//---------------------------------------------------------------------
//Functions exported by DelZip's Dlls ...
//(These functions are dynamically loaded by TZip if and when needed)
//---------------------------------------------------------------------

var
  ZipDllExec: function( ZipRec: pZipParams ): DWord; stdcall;
  GetZipDllVersion: function : DWord; stdcall;
  ZipDllHandle: THandle;

  UnzipDllExec: function( UnZipRec: pUnZipParams ): DWord; stdcall;
  GetUnzipDllVersion: function: DWord; stdcall;
  UnzipDllHandle: THandle;

implementation

end.

