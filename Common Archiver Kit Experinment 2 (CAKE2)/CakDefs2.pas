unit CakDefs2;
{*********************************************************}
{*                      CakDefs2.pas                     *}
{*             Common Archiver Kit Experinment           *}
{*   Copyright (c) 2002 - 2004 Joseph Leung Yat Chun     *}
{*                 All rights reserved.                  *}
{*********************************************************}
interface

uses Classes, Graphics, Controls;

const
  LineBreak = #10; 
  MaxArchiver = 40;
  TMPDIR     = 'QZTEMP\'; // ## GC 'ZZTEMP\'
  LEADCHAR    = 'QZ4.';   //'CAKE2.'; // ## GC 'ZipZag.';
  PRODUCT     = 'Quick Zip '; // ## GC 'ZipZag '
  FUNCNOTAVIL = 'Function not avaliable';
  ACEINTERR   = 'Ace Internal Error : File already Exists';
  NOERR       = 'Operation OK';

  ABOUTSTR = 'Common Archiver Kit Experiment Ver 2' + #13#10 +
    '(c) Joseph Leung (2001 - 2004) - http://www.quickzip.org' + #13#10 +
    'OpenSource under GNU Library License';

  CODE_NOERROR          = 0;
  CODE_UNDEFERROR       = 9999;
  CODE_PROCESSFAIL      = 2999;
  CODE_PROCESSOK        = 2001;
  CODE_PROCESSEXTR      = 2002;
  CODE_PROCESSTEST      = 2003;
  CODE_PROCESSADD       = 2004;
  CODE_PROCESSREADD     = 2005;
  CODE_PROCESSDEL       = 2006;

  CODE_HOTEDIT          = 3001;

  CODE_DLLERROR         = 1001;
  CODE_NOTEXIST         = 1002;
  CODE_PASSWORD         = 1003;
  CODE_SFXHEADER        = 1004;
  CODE_NOSUPPORT        = 1005;
  CODE_LISTERROR        = 1006;
  CODE_DLLNOTFOUND      = 1007;
  CODE_CANTREAD         = 1008;
  CODE_EXIST            = 1009;
  CODE_USERSKIP         = 1010;
  CODE_NOMEMORY         = 1011;
  CODE_NOTSPECIFY       = 1012;
  CODE_NOTFOUNDARC      = 1013;
  CODE_NODISKSPACE      = 1014;
  CODE_WRITE            = 1015;
  CODE_CRC              = 1016;
  CODE_READERROR        = 1017;
  CODE_OPENED           = 1018;
  CODE_VOLUMECHANGE     = 1019;
  CODE_READONLY         = 1020;
  CODE_UNKTYPE          = 1021;
  CODE_LONGFN           = 1022;
  CODE_WRONGVER         = 1023;
  CODE_NEWER            = 1024;
  CODE_TOOMANYFILE      = 1025;
  CODE_MAKEDIR          = 1026;
  CODE_HUFFAN           = 1027;
  CODE_HEADER           = 1028;
  CODE_CRCHEADER        = 1029;
  CODE_HEADERBROKE      = 1030;
  CODE_NOTARC           = 1031;
  CODE_WRONGTYPE        = 1032;
  CODE_WRONGCMD         = 1033;
  CODE_MOREHEAP         = 1034;
  CODE_RUNNING          = 1035;
  CODE_HARC             = 1036;
  CODE_SEARCH           = 1037;
  CODE_TIMESTAMP        = 1038;
  CODE_ARCREADONLY      = 1039;
  CODE_TMPOPEN          = 1040;
  CODE_SAMENAME         = 1041;
  CODE_NORESPONSE       = 1042;
  CODE_NOTVALID         = 1043;
  CODE_COPYTEMP         = 1044;
  CODE_EOF              = 1045;
  CODE_CANTCREATE       = 1046;
  CODE_DISKFULL         = 1047; 

  ERR_UNDEFERROR  = 'Unknown error';
  ERR_NOERROR     = 'OK';
  ERR_DLLERROR    = 'Internal error (related to dll)';
  ERR_LISTERROR   = 'Internal error (cant list)';
  ERR_SFXHEADER   = 'Problem copying SFX header';
  ERR_DLLNOTFOUND = 'Internal error (required dll not found)';
  ERR_NOTSPECIFY  = 'No file specified.';
  ERR_READERROR   = 'Read Error';
  ERR_VOLUMECHANGE= 'Volume change %s';
  ERR_TIMESTAMP   = 'Wrong timestamp';
  ERR_PROCESSOK   = 'Processing %s Correctly';
  ERR_PROCESSEXTR = 'Extracting %s';
  ERR_PROCESSTEST = 'Testing %s';
  ERR_PROCESSADD  = 'Compressing %s';
  ERR_PROCESSREADD= 'Re-Compressing %s';
  ERR_PROCESSDEL  = 'Deleting %s';
  ERR_PROCESSFAIL = 'Processing %s with Error';
  ERR_HOTEDIT     = 'File with path, cannot HotEdit';

  ERR_NODISKSPACE = 'Out of Disk space';
  ERR_READONLY    = 'Read Only';
  ERR_USERSKIP    = 'User Skip';
  ERR_CRC         = 'CRC Error';
  ERR_UNKTYPE     = 'Unknown File type';
  ERR_NOSUPPORT   = 'Method not support';
  ERR_PASSWORD    = 'Wrong password';
  ERR_LONGFN      = 'This archive type dont support long filename';
  ERR_WRONGVER    = 'Wrong Version';
  ERR_OPENED      = 'File Opened';
  ERR_NEWER       = 'Current file are newer';
  ERR_NOTEXIST    = 'File not exist';
  ERR_EXIST       = 'File already exist';
  ERR_TOOMANYFILE = 'Too Many files';
  ERR_MAKEDIR     = 'Require make directory';
  ERR_WRITE       = 'Cannot write';
  ERR_HUFFAN      = 'Huffan code';
  ERR_HEADER      = 'Problem reading comment header';
  ERR_CRCHEADER   = 'Problem reading CRC header';
  ERR_HEADERBROKE = 'Header broken';
  ERR_NOTARC      = 'Not an archive';
  ERR_CANTREAD    = 'Can`t Read';
  ERR_WRONGTYPE   = 'Wrong file style';
  ERR_WRONGCMD    = 'Wrong command name';
  ERR_MOREHEAP    = 'More heap memory';
  ERR_NOMEMORY    = 'Not enough memory';
  ERR_RUNNING     = 'Already Running';
  ERR_HARC        = 'HARC havent opened';
  ERR_SEARCH      = 'Not in search mode';
  ERR_ARCREADONLY = 'Archive read only';
  ERR_TMPOPEN     = 'Unknown error ERROR_TMP_OPEN';
  ERR_SAMENAME    = 'File with same name';
  ERR_NOTFOUNDARC = 'Cant find Archive file';
  ERR_NORESPONSE  = 'Unknown error ERROR_RESPONSE_READ';
  ERR_NOTVALID    = 'Not valid filename';
  ERR_COPYTEMP    = 'Unable to copy to temp';
  ERR_EOF         = 'End of file';
  ERR_CANTCREATE  = 'Cannot Create';
  ERR_DISKFULL    = 'Disk Full';

  MSG_PWD          = 'Password';
  MSG_PLZENTERPWD4 = 'Please Enter password for ';
  MSG_SHOWAGAIN    = 'Show this dialog again next time';
  MSG_BEGINLOG     = 'Begin Log';

  METHODNOTSUPPORT = 'Ace Compress Method not support ';
  Archive = 'Archives';
  SPACE = ' ';

  WINEXT_ZIP  = 'ZIP';
  WINEXT_CAB  = 'CAB';
  WINEXT_LHA  = 'LZH';
  WINEXT_ARJ  = 'ARJ';
  WINEXT_ACE  = 'ACE';
  WINEXT_RAR  = 'RAR';
  WINEXT_TAR  = 'TGZ';
  WINEXT_BZ2  = 'BZI';
  SEVENZIPDLL = '7-ZIP32.DLL';
  BZADLL      = 'BGA32.DLL';
  BZ2DLL      = 'BZ2LIB.DLL';
  BELDLL      = 'UNBEL32.DLL';
  GCADLL      = 'UNGCA32.DLL';
  LHADLL      = 'UNLHA32.DLL';
  UNARJDLL    = 'UNARJ32J.DLL';
  ZIPDLL      = 'ZIPDLL.DLL';
  UNZIPDLL    = 'UNZDLL.DLL';
  UNACEDLL    = 'UNACE.DLL';
  UNRARDLL    = 'UNRAR32.DLL';
  TARDLL      = 'TAR32.DLL';
  YZ1DLL      = 'YZ1.DLL';
  SQXDLL      = 'SQX.DLL';

  MAJORVER = '2';
  MINORVER = '0';
  BUILD    = '10';

  CAKVER           = MAJORVER + '.' + MINORVER + '.' + BUILD;
  DefaultTreatAsZip = '.ZIP .PK3 .JAR .WSZ .SIT .EAR .WAR';
  DefaultTreatAsRar = '.RAR';
  DefaultTreatAsCab = '.CAB';
  DefaultTreatAsLha = '.LHA .LZH';
  DefaultTreatAsArj = '.ARJ';
  DefaultTreatAsAce = '.ACE';
  DefaultTreatAsTar = '.TAZ .TAR .RPM';
  DefaultTreatAsTgz = '.TGZ .GZ .Z';
  DefaultTreatAsBz2 = '.BZ2 .TB2';
  DefaultTreatAsBza = '.BZA .GZA';
  DefaultTreatAsCzip = '.CZIP';
  DefaultTreatAsRs = '.RS';
  DefaultTreatAsYz1 = '.YZ1';
  DefaultTreatAsUue = '.UUE .UU .ENC';
  DefaultTreatAsXxe = '.XXE';
  DefaultTreatAsB64 = '.B64';
  DefaultTreatAsPak = '.PAK .WAD';
  DefaultTreatAsBel = '.BEL';
  DefaultTreatAsImp = '.IMP';
  DefaultTreatAsArc = '.ARC';
  DefaultTreatAsZoo = '.ZOO';
  DefaultTreatAsCpt = '.CPT';
  DefaultTreatAsPit = '.PIT';
  DefaultTreatAsArg = '.ARG';
  DefaultTreatAsAsd = '.ASD';
  DefaultTreatAsDz  = '.DZ';
  DefaultTreatAsSH  = '.SH';
  DefaultTreatAsZac = '.ZAC';
  DefaultTreatAsBin = '.BIN';
  DefaultTreatAsCar = '.CAR';
  DefaultTreatAsFrz = '.FRZ';
  DefaultTreatAsSpl = '.SPL';
  DefaultTreatAsJam = '.JAM';
  DefaultTreatAsBqx = '.BQX';
  DefaultTreatAsGcA = '.GCA';
  DefaultTreatAsQze = '.QZE';
  DefaultTreatAsAks = '.AKS';
  DefaultTreatAs7z  = '.7Z .7ZIP';
  DefaultTreatAsHog = '.HOG';
  DefaultTreatAsPgp = '.PGP';
  DefaultTreatAsSqx = '.SQX';

  Totalcolumns = 9;
  Columns : array[1..totalcolumns] of string =
      ('Ext.','Filename', 'Type', 'Size','Date','Pack',
      '%','Crc','Path');
  Startat : array[1..totalcolumns] of integer =
      (0,70,140,240,270,360,390,410,460);

  TotalMeta = 18;
  HtmlCodePageMeta : array[1..TotalMeta] of string =
  ('Latin1','Shift JIS','JIS','EUC','Big5','GB2312','KUC-KR','ISO-8859-2',
   'MacCE','Windows-1250','ISO-8859-5','KOI8-R','MacCyrillic','Windows-1251',
   'ISO-8859-7','MacGreek','Windows-1253','ISO-8859-1');
  HtmlCodePageMetaDescription : array[1..TotalMeta] of string =
  ('Western','Japanese','Japanese','Japanese','Traditional Chinese','Simplified Chinese',
   'Korean','Central European','Central European','Central European','Cyrillic',
   'Cyrillic','Cyrillic','Cyrillic','Greek','Greek','Greek','Icelandic Mac');

type
  SupportType       = ( _Zip, _7z, _Ace, _Arc, _Asd, _Arg, _Arj, _Bel, _Bin, _Bz2, _Bza,
  _Cab, _Car, _Cpt, _Czip,_Dz, _Frz, _Gca,_Hog, _Imp, _Jam, _Lha, _Pit, _Qze, _Rar, _Rs, _Sh, _Spl,
  _Sqx, _Tar, _Tgz, _Yz1,  _Zac,  _Zoo, _Bqx, _Uue, _Xxe, _B64, _Pak, _Ext, _Sfx, _Aks, _WIT, _PGP);

  FileListType      = (_RawTxt, _Txt, _Htm, _Htm2, _Pdf, _Pdf2, _Csv);
  SortByType        = (_FName, _FType, _FSize, _FPSize, _FCRC, _FRatio, _FDefPath,
    _FTime, _FArchive);
  CabModeType       = (_CFList, _CFExtract);
  AddModeTypeList   = (_refresh, _update, _move);
  AddModeType       = set of AddModeTypeList;
  MessageType       = (Msg_OK, Msg_Error, Msg_Warning, Msg_Unknown);
  FileListItemType  = (Fl_fext, Fl_fname, Fl_ftype, Fl_fsize, Fl_fdate, Fl_fPsize,
    Fl_fCRC, Fl_fratio, Fl_fdefpath);
  FileListItemTypes = set of FileListItemType;
  EncryptMethodType = (ceCZip,ceBlowFish,ceRijndael,ceTwoFish,qeBlowfish,qeDes,qe3des,qeRijndael);

  TCOverEvent = procedure(Sender: TObject; FileName: string;
    var overwrite: Boolean; var applytoall: Boolean) of object;
  TCPwdEvent = procedure(Sender: TObject; archive, FileName: string;
    var newpassword: string) of object;
  TCCZIPPwdEvent = procedure(Sender: TObject; archive, FileName: string;
    var key1, key2, key3 : integer) of object;
  TCMsgEvent = procedure(Sender: TObject; CErrCode, ErrCode: Integer;
    Msgtype: MessageType; Message: string) of object;
  TCSimpleMsgEvent = procedure(Sender: TObject; Message: string) of object;
  TCProgEvent = procedure(Sender: TObject; FileName: string;
    FileSize: Longint; JustCompleted: Longint) of object;
  TCFoundEvent = procedure(Sender: TObject; FileName: string; FileSize: Integer) of
  object;
  TCCrytoEvent = procedure(Sender: TObject; var Key1, Key2, Key3: Integer) of object;
  TCUnkCmdEvent = procedure(Sender: TObject; command: string; param: TStrings) of object;
  TCReqColumnCaptionEvent = procedure(Sender: TObject;Column : integer; var Caption: string) of object;

  ArcType = record
    _ArcName: string;
    _ArcType: SupportType;
    _ArcSize: Integer;
    _ArcHaveInst,
    _ArcHaveComm,
    _ArcNeedPassword: Boolean;
    _ArcTime: TDatetime;
  end;
  SearchRecType = record
                  filenameMasks : string;
                  filepath : string;
                  MinSize, MaxSize : integer;
                  MinDate, MaxDate : TDate;
                  end;

  RegnodeType = record
    IsKey: Boolean;
    FullPath: string;
    KeyName: string;
          {//   valuetype : TRegDataType;
                dataS : String;
                dataES : ANSIString;
                dataI : integer;
                dataB : integer; //}
    SubKey: TList;
  end;
  PRegnodeType = ^RegnodeType;
  ContentType  = packed record
    _FileRatio: Word;
    _FileIcon: Word;
    _Tag:Word;
    _FileSize : LongWord;
    _FilePackedSize: LongWord;
    _FileTime: TDatetime;
    _FileCRC: string;
    _FileName:string;
    _FileFullPath:string;
    _FileDefPath:string;
    _FileArchive: String;
    _Filetype: string;
    _Encrypted:Boolean;
    _Selected: Boolean;
  end;
  FileListOptionsType = record
    Item2Show: FileListItemTypes;
    BackgroundColor, HeaderFontColor,
    HeaderBackground, FListBackground,
    NormalFontColor: TColor;
    SizeInK, UseBorder: Boolean;
  end;
  SfxOptionsType = record
    Sfx_To: Integer;
    Sfx_Message: string;
    Sfx_CommandLine: string;
    Sfx_Caption: string;
    Sfx_ExtractTo: string;
    Sfx_AutoRun: Boolean;
    Sfx_Overwrite: Boolean;
  end;
  ExtractOptionsType = record
    Extr_to: string;
    Extr_DirNames: Boolean;
    Extr_Overwrite: Boolean;
    Extr_ArcInArc: Boolean;
    Extr_ExtractAll: Boolean;
  end;
  AddOptionsType = record
    Add_To: Integer;
    Add_DictSize: Integer;
    Add_Encrypt: string;
    Add_SubDir: Boolean;
    Add_UseEncrypt: Boolean;
    Add_UsePath: Boolean;
    Add_Mode: AddModeType;
    Add_Hidden: Boolean;
    Add_FileList: Boolean;
    Add_Files: TStrings;
    Add_BaseDir: string;
    Add_Exclude: TStrings;
    Add_DosFormat: Boolean;
    Add_Relative: Boolean; //zip only!!
    Add_CompLevel: 0..9;
  end;
  RenameOptionsType = record //Zip only!!
                      Rename_From : string;
                      Rename_To : string;
                      RenameNow : boolean;
                      end;
  EncryptoptionsType = record
                        EncryptMethod : EncryptMethodType;
                        File2Encrypt : string;
                       end;
  FinderOptionsType = record
    Af_TargetName: TStrings;
    Af_SourceDir: string;
    Af_SubDir: Boolean;
    Af_ArcFilter: string;
    Af_ArcType: set of SupportType;
    Af_ContainText: string;
    Af_Text2Look: string;
  end;
  WorkType = (wtNone, //Donothing
    wtLoadContents, //List Archive
    wtExtract, //Extract Archive
    wtTest, //Test Archive
    wtAdd, //Add file to archive
    wtDelete, //Delete file from archive
    wtSfx, //Create Self extractables
    wtRename,
    wtEncrypt,
    wtClose
    );
  SupportWorkType = Set of WorkType;
  AVILTYPE = array[WorkType] of Boolean;
  ShortcutType = (stDESKTOP, stQUICKLAUNCH, stSENDTO, stSTARTMENU, stPROGRAMS,
    stOTHERFOLDER);
  CZKeyType = record Part1, Part2, Part3 : Integer; end;
const
  sAll = [wtAdd,wtExtract,wtDelete,wtLoadContents,wtSFX,wtNone];
  sAllExceptSfx = sAll - [wtSFX];
  sExtractOnly  = sAll - [wtAdd,wtDelete,wtSFX];
  sEncrypt = sAllExceptSfx + [wtEncrypt];
  sNone = [];

  T = True;
  F = False;
//  FuncCheck: array[SupportType, WorkType] of Boolean = (
//    (T, T, T, T, T, T, T, T ,T ,F), (T, T, T, T, F, F, F, F ,F ,F), {_Zip,_Rar}
//    (T, T, T, T, T, F, F, F ,F ,F), (T, T, T, T, F, F, F, F ,F ,F), (T, T, T, T, T, T, T, F ,F ,F), {_Cab,_Arj,_Lha}
//    (T, T, T, T, T, T, F, F ,F ,F), (T, T, T, T, T, T, F, F ,F ,F), (T, T, T, T, F, F, F, F ,F ,F), {_Tar,_Tgz,_Ace}
//    (T, T, T, T, T, T, F, F ,F ,F), (T, T, T, T, F, F, F, F ,F ,F), (T, T, T, T, F, F, F, F ,F ,F), {_Bz2,_Bel,_Gca}
//    (T, T, T, T, T, T, F, F ,F ,F), (T, T, T, F, T, T, F, F ,F ,F), (T, T, T, F, F, F, F, F ,F ,F), {_Bza,_Rs,_Czip}
//    (T, T, T, F, T, F, F, F ,F ,F), (T, T, T, F, F, F, F, F ,F ,F), (T, T, T, F, F, F, F, F ,F ,T), {_Yz1,_Uue,_Xxe}
//    (T, T, T, F, F, F, F, F ,F ,F), (T, T, T, F, F, F, F, F ,F ,F), (T, T, T, T, T, T, T, F ,F ,F), {_B64,_Pak,_Ext}
//    (T, T, T, T, T, T, T, F ,F ,F), (T, T, T, T, T, T, T, F ,F ,F), (T, T, T, F, F, F, F, F ,F ,F), {_7z,_Sqx,_Hog}
//    (T, T, T, F, F, F, F, F ,F ,T), (F, F, F, F, F, F, F, F ,F ,F), (T, T, T, T, T, F, T, T ,F ,F), {_Qze,_Aks,_WIT}
//    (T, T, T, F, F, F, F, F ,F ,F));                                                                   {_Pgp}

  {None,LoadContents,Extract,Test,Add,Delete,Sfx,CrytoZip}
    // ShortcutType???
implementation

end.
