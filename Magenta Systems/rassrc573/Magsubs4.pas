unit magsubs4;

// Delphi 7 and later version

{$IFNDEF VER140}
  {$WARN UNSAFE_TYPE off}
  {$WARN UNSAFE_CAST off}
  {$WARN UNSAFE_CODE off}
{$ENDIF}
{$WARN SYMBOL_PLATFORM OFF}
{$WARN SYMBOL_LIBRARY OFF}
{$WARN SYMBOL_DEPRECATED OFF}

{ Magenta Systems File Transfer Components.
Updated by Angus Robertson, Magenta Systems Ltd, England, 29th July 2014
delphi@magsys.co.uk, http://www.magsys.co.uk/delphi/
Copyright Magenta Systems Ltd

various common subroutines, Magenta Systems Ltd
29 Jan 98 - correct crypt giving range errors
31 Jan 98 - improved GetExceptMess
07 Feb 98 - added sBreakApart and pBreakApart
20 Feb 98 - added Str2DateTime and Str2Time
25 Feb 98 - made TimeToInt more reliable
3  Mar 98 - added Int2MBytes
21 Mar 98 - added GetVolType, etc
8 Apr 98  - added regCurrentVersion
14 Apr 98 - corrected sysdelay, added GetSpaceOnDrive (FAT32 compatible)
15 Apr 98 - IsDir now allows blank directory
20 Apr 98 - added strD/B/Crypt routines, added base64 routines
21 Apr 98 - added regGetString
23 Apr 98 - Str2Int and Str2Date/Time now check a little to avoid convert errors
24 Apr 98 - added isDigit and isDigits, validate numerics
1  May 98 - added ConIntHex
11 May 98 - GetSpaceOnDrive and Int2Mbytes use comp type for more than 4G
7  Jun 98 - added HoursToTime, MinsToTime, SecsToTime
22 Jun 98 - added GetDiskFreeSpaceFAT32, not tested yet
27 Jun 98 - recognise Windows98, corrected GetVolInfo serial number
01 Jul 98 - fixed Int2Meg for larger than 1G spare
28 Jul 98 - Delphi 4 compatible
9  Aug 98 - removed API calls for GetSpaceOnDrive since D4 works properly
24 Aug 98 - added TimerToStr
01 Sep 98 - correct Int2Mbytes to int64
01 Oct 98 - IntToKbyte, more character literals
02 Oct 98 - added IsDigitsDec
11 Oct 98 - added StripQuotes
19 Nov 98 - using DecimalSeparator
26 Nov 98 - added GetEnvirVar and IndexFiles
14 Dec 98 - corrected TimerToStr to use TimeSeparator
28 Jan 99 - corrected ReplString
10 Feb 99 - corrected getlastch
25 Feb 99 - added MidStr (= copy)
04 Mar 99 - added StripSpaces, UpAndLower
04 Apr 99 - moved most program execution stuff to TaskWins
06 Jun 99 - added packed date and time functions
15 July 99 - added IsWin2K
4 Aug 1999 - added sysWindowsDir
23 Aug 1999 - corrected IndexFiles args
14 Nov 1999 - corrected sysWindowsDir, Packed2Date now does date only
7 Dec 1999 - added Match, PosNext, PostPrev, etc
9 Dec 1999 - removed LeftStr, RightStr since duplicate RX StrUtils with different function
9 Dec 1999 - removed lots of other stuff that was never used
1 Jan 2000 - correct timeToInt so it returns -1 if date too large for integer, ie today
24 Jan 2000 - Join now StrArrayJoin, etc
11 Feb 2000 - added Windows ME
10 March 2000 - added StrArrayPosOf, corrected IndexFiles not finding anything
10 March 2000 - added AddThouSeps
24 April 2000 - added AscToInt (simple version of StrToInt)
25 July 2000 - added enkeyWebForm and enkeyComCap
3 August 2000 - added GetMACAddress - removed and replaced with GetMacAddresses in magsub1
24 August 2000 - correct file type masks for file searching for W2K
18 Sept 2000 - added ConvLongDate, DeleteOldFiles, TumbleDigits
9 Oct 2000 - moved sysDelay to MAGSUBA to allow FORMS to be removed from here
29 Dec 2000 - recognise Windows Whistler (5.1)
27 Feb 2001 - GetSizeFile returns -1 if file not found
07 March 2001 - added GetMaskedName, UnpackFtpDir, SelectDownFiles, GetLocDir
                UpdateFileAge, FmtFtpDir
4 April 2001 - added StrLoadNoLockFile
24 April 2001 - added Windows XP literal
1 May    2001 - using Val for AscToInt
9 July 2001   - added WindowsShutdown

following when D6 unit created
19 July 2001 - added InetParseDate
23 July 2001 - added MaxLongWord, TFindList (from MagCopy), Int642CEStr
25 July 2001 - removed DoEncodeTime and DoEncodeDate, D6 has Try versions
               Str2Time/DateTime now uses D6 Def version not exceptions
4 Sept 2001  - added StripChars, StripCommas
20 Sept 2001 - added IsWinXP
1 Oct 2001   - added URLEncode
29 Oct 2001  - added BoolLit
19 Nov 2001  - improved WindowsShutdown
10 Dec 2001  - added regWriteInteger and regReadInteger
18 Dec 2001  - added IsCPUIDAvailable, GetCPUID, GetCPUVendor, GetCPUSerial
31 Dec 2001  - added StringTranCh, StringCtrlSafe, StringCtrlRest, StringFileTran
2 Feb 2002   - added UNICODESIG
9 Feb 2002   - changed WindowsShutdown to accept powerdown paramater
21 Feb 2002  - added GetAgeSizeFile, FileTimeToDateTime and DateTimeToFileTime
27 Feb 2002  - corrected Int2Mbytes to show six digits for 10gig >
08 Mar 2002  - added StringRemCntls
18 Mar 2002  - added PackedISO2Date
3 April 2002 - added RegDeleteKeyIncludingSubkeys
19 May 2002  - added AlphaDTtoISODT, Quote, QuoteNull
7 June 2002  - added GetFAgeSizeFile
10 July 2002 - corrected StringTranCh and StringRemCntls adding UniqueString to ensure
                  only the correct string is changed...
9 Aug 2002   - corrected registry stuff to read only so HLM can be accessed by non-adminstrators
               regWriteString/regWriteInteger now functions returning error
               removed regGetString
               GetOSInfo now get service pack/product info, improved text string
20 Aug 2002 - added Bleep, Beep1K, Beep500, Click, LockWindow,
              UnlockWindow, RedrawWin, CharPos, DownCase
28 Aug 2002 - added StripChar, StripNulls, StripAllCntls (using pointers)
5 Sept 2002 - added DTtoISODT
12 Sep 2002 - added enkeyPassDir
23 Sep 2002 - added exception handling around reg... read/write functions
13 Oct 2002 - added GetFUAgeSizeFile, GetUAgeSizeFile - UTC file times
2 Nov 2002  - added strBXEncrypt and strBXDecrypt
8 Nov 2002  - added FullDateTimeMask
19 Nov 2002 - added QuoteSQLDate
30 Dec 2002 - StripQuotes now also removes <>
8 Jan 2003  - added NowPC - temp removed
4 Feb 2003  - added SecsToMinStr
26 Mar 2003 - added TEventLogger, ServiceSetComm, EventsSetReg
1 May 2003  - added ISODTtoPacked
5 May 2003  - messing with windows version reporting
29 May 2003 - added QuoteSQLTime
9 June 2003 - added URLDecode
30 Jun 2003 - added ConvHexQuads
16 Jul 2003 - added AscToInt64
24 Jul 2003 - added ConvertIPAddr (Addr: u_long)
31 Jul 2003 - improved OS detect for more Windows 2003 versions and XP embedded
3 Aug 2003  - added PackedDTtoISODT
8 Sept 2003 - added Date2XPacked
22 Sep 2003 - added NowPC, GetPerfCountsPerSec, PerfCountCurrent, PerfCountToMilli
              added PerfCountGetMilli, PerfCountGetMillStr - for millisecond timing
24 Sep 2003 - added FileTimeToInt64 and FileTimeToSecs2K
30 Sep 2003 - added ConvUKDate, PackedISO2UKStr
7 Nov 2003  - added TicksPerDay, TicksPerMin and TicksPerSec
31 Dec 2003 - split a lot of stuff to magsubs1 for public distribution
20 Jan 2004 - added SetVolLabel
5 Feb 2004  - allow Int2Kbytes to show 7 digits, trim IntToKbyte
16 Feb 2004 - stop trapped exceptions in regReadxxx
11 May 2004 - GetVolInfo safer
26 Nov 2004 - regWriteString now deletes value name if blank rather than setting blank
15 Dec 2004 - added TBuffLogStream class
2 Mar 2005  - added FFullName to TBuffLogStream
5 Apr 2005  - added retries to flush logs for TBuffLogStream, don't overwrite existing file
26 Apr 2005 - return real error if flush/open fails
15 May 2005 - added power management literals
4 June 2005 - added CreditCardCheck
5 Aug 2005  - added regValueExists
31 aug 2005 - added LastBootDateTime
14 Oct 2005 - added FileInUse and TestPathAccess
17 Oct 2005 - TBuffLogStream ignore exceptions flushing during write, might work next time
14 Nov 2005 - added SFBuffOpenError, minor changes for Delphi 5
19 Feb 2006 - added IpToStr, StrToIP, IsIPStr, IsFmtIPStr, Str2IP from magrasedt
14 Mar 2006 - better errors in TestPathAccess
24 Mar 2006 - added CheckUniqueInstance, GetIOErrorStr, GetDriveTypeStr
29 Apr 2006 - added IsProgAdmin
9 Aug 2006  - added SimpleLogging
21 Sep 2006 - added LoadStringsFromFile
12 Nov 2006 - added CleanEmailAddr
31 Jan 2007 - SimpleLogging now allows path or drive letter
2 May 2007  - added AntiAliasBitmap
22 Aug 2007 - added IsValidEmail
07 Dec 2007 - added EnumSerialPorts and TSerialPorts
18 Jan 2008 - TBuffLogStream now allows flush in seconds as well as minutes
4 Feb 2008  - added ReverseEmail
18 Mar 2008 - clean up TBuffLogStream.WriteLine and ReverseEmail
07 Aug 2008 - made functions compatible with unicode strings in Delphi 2009
              note: before Delphi 2009 String=AnsiString, now String=UnicodeString (not WideString)
              now using wide versions of Windows APIs and letting Delphi convert to string
              removed duplicate IsProgAdmin
              added EnableClearType
6 Oct 2008  - FileInUse now UnicodeString
18 Jun 2009 - added IsHtmlTags
4 Sept 2009 - removed u_long=integer, ConvertIPAddr, IpToStr, StrToIP, Str2IP now all LongWord
              added EndianLong, IsBitSet, BitOn, BitOff and BitToggle
1 Mar 2010  - added EndianSwap64
7 Aug 2010  - various cast fixed for D2009 and later
15 Oct 2010 - added strBXDecryptEx
21 Mar 2011 - added GetProcessHandles (XP SP1 and later only)
15 Jul 2011 - no longer using ASM for 64-bit compiler, GetCPUSerial etc not on 64-bit
10 Sep 2012 - using MyFormatSettings to replace formatting public vars removed in XE3
              builds with D7 again, missing wow64 keys
25 Mar 2013 - added LongAddr2Dotted
14 Jul 2014 - moved EnumSerialPorts and TSerialPort to MagSerPorts.pas
29 Jul 2014 - added EnableOrDisableChildren (Container: TWinControl; Enabled: Boolean) ;


}
interface

uses
  Windows, Messages, SysUtils, Classes, Registry, ShellAPI, Magsubs1,
  Controls, ExtCtrls, Graphics, Math ;

const
 { important registry keys / items }
  REG_CURRENT_VERSION95 = 'Software\Microsoft\Windows\CurrentVersion';     // HLM
  REG_CURRENT_VERSIONNT = 'Software\Microsoft\Windows NT\CurrentVersion';  // HLM
  REG_CURRENT_USER    = 'RegisteredOwner';
  REG_CURRENT_COMPANY = 'RegisteredOrganization';

  PRIME_16       = 65521;
  PRIME_32       = 2147483647;

resourcestring
  SFBuffOpenError = 'Cannot open buffer file %s';

type
  { Search and Replace options }
  TSROption     = (srWord,srCase,srAll);
  TSROptions    = set of TsrOption;

var
  xLanguage : Integer;
  xLangOfs  : Integer;
  InstanceMutexHandle: THandle = 0;  // 19 March 2006


type
        TBitList = Word; // Used for a Bit List of 16 bits from 15 -> 0

type
        String16        =        string [16];
//        u_long = Longint;  // removed 4 Sept 2009

{                                                                              }
{ Character set constants                                                      }
{                                                                              }
const
  cs_Everything       = [#0..#255];
  cs_WhiteSpace       = [#0..#32];
  cs_AlphaLow         = ['a'..'z'];
  cs_AlphaHigh        = ['A'..'Z'];
  cs_Numeric          = ['0'..'9'];
  cs_Alpha            = cs_AlphaLow + cs_AlphaHigh;
  cs_AlphaNumerical   = cs_Numeric + cs_Alpha;
  cs_Exponent         = ['E', 'e'];
  cs_HexDigit         = cs_Numeric + ['A'..'F', 'a'..'f'];
  cs_OctalDigit       = ['0'..'7'];
  cs_BinaryDigit      = ['0'..'1'];
  cs_Sign             = ['+', '-'];
  cs_Parentheses      = ['(', ')'];
  cs_CurlyBrackets    = ['{', '}'];
  cs_BlockBrackets    = ['[', ']'];
  cs_Punctuation      = ['.', ',', ':', '/', '?', '<', '>', ';', '"', '''',
                         '[', ']', '{', '}', '+', '=', '-', '\', '(', ')', '*',
                         '&', '^', '%', '$', '#', '@', '!', '`', '~'];
  cs_LowerA           = ['a', 'â', 'ä', 'à', 'å', 'á'];
  cs_LowerE           = ['e', 'é', 'ê', 'ë', 'è'];
  cs_LowerI           = ['i', 'ï', 'î', 'ì', 'í'];
  cs_LowerN           = ['n', 'ñ'];
  cs_LowerO           = ['o', 'ô', 'ö', 'ò', 'ó'];
  cs_LowerU           = ['u', 'ü', 'û', 'ù', 'ú'];
  cs_LowerY           = ['y', 'ÿ'];
  cs_Lower            = cs_AlphaLow + cs_LowerA + cs_LowerE + cs_LowerI +
                        cs_LowerN + cs_LowerO + cs_LowerU + cs_LowerY;
  cs_UpperA           = ['A', 'Ä', 'Å'];
  cs_UpperE           = ['E', 'É'];
  cs_UpperN           = ['N', 'Ñ'];
  cs_UpperO           = ['O', 'Ö'];
  cs_UpperU           = ['U', 'Ü'];
  cs_Upper            = cs_AlphaHigh + cs_UpperA + cs_UpperE + cs_UpperN +
                        cs_UpperO + cs_UpperU;
  cs_Word             = cs_Lower + cs_Upper + ['-', '='];
  cs_Text             = cs_Word + cs_Punctuation + cs_WhiteSpace + cs_Numeric;
  cs_WordDelim        = cs_Text - cs_Word;


// Intel processor stuff

const
 ID_BIT =   $200000;            // EFLAGS ID bit

{ Intel Feature Flags }

  FPU_FLAG   = $00000001; // Floating-Point unit on chip
  VME_FLAG   = $00000002; // Virtual Mode Extention
  DE_FLAG    = $00000004; // Debugging Extention
  PSE_FLAG   = $00000008; // Page Size Extention
  TSC_FLAG   = $00000010; // Time Stamp Counter
  MSR_FLAG   = $00000020; // Model Specific Registers
  PAE_FLAG   = $00000040; // Physical Address Extention
  MCE_FLAG   = $00000080; // Machine Check Exception
  CX8_FLAG   = $00000100; // CMPXCHG8 Instruction
  APIC_FLAG  = $00000200; // Software-accessible local APIC on Chip
  BIT_10     = $00000400; // Reserved, do not count on value
  SEP_FLAG   = $00000800; // Fast System Call
  MTRR_FLAG  = $00001000; // Memory Type Range Registers
  PGE_FLAG   = $00002000; // Page Global Enable
  MCA_FLAG   = $00004000; // Machine Check Architecture
  CMOV_FLAG  = $00008000; // Conditional Move Instruction
  PAT_FLAG   = $00010000; // Page Attribute Table
  PSE36_FLAG = $00020000; // 36-bit Page Size Extention
  PSN_FLAG   = $00040000; // Processor serial number present and enabled
  CLFSH_FLAG = $00080000; // CLFLUSH instruction
  BIT_20     = $00100000; // Reserved, do not count on value
  DS_FLAG    = $00200000; // Debug store
  ACPI_FLAG  = $00400000; // Thermal monitor and software controlled clock
  MMX_FLAG   = $00800000; // MMX technology
  FXSR_FLAG  = $01000000; // Fast Floating Point Save and Restore
  SSE_FLAG   = $02000000; // Streaming SIMD extensions
  SSE2_FLAG  = $04000000; // Streaming SIMD 2 extensions
  SS_FLAG    = $08000000; // Self snoop
  BIT_28     = $10000000; // Reserved, do not count on value
  TM_FLAG    = $20000000; // Thermal monitor supported
  BIT_30     = $40000000; // Reserved, do not count on value
  BIT_31     = DWORD($80000000); // Reserved, do not count on value

type
  THalfWords = record
    case Integer of
    0: (
      Low: Word;
      High: Word);
    1: (
      Full: LongWord);
  end;

 TCPUID = array[1..4] of THalfWords;
 TVendor    = array [0..11] of AnsiChar;
 TPartSerial = array [1..4] of THalfWords;

// serial COM ports  7 Dec 2007, 7 Apr 2014 more elements
{  TSerialPort = record
    ComName: string ;
    NumPort: string ;
    IntName: string ;
    FriendlyName: string ;
    Desc: string ;
    Manufacturer: string ;
    HardwareId: string ;
    Location: string ;
    StatInfo: string ;
    Enabled: boolean ;  // USB ports may be configured but hidden and disabled
 end;
 TSerialPorts = array of TSerialPort ;  }

// power management stuff from pbt.h
const
    PBT_APMQUERYSUSPEND         =   $0000 ;
    PBT_APMQUERYSTANDBY         =   $0001 ;
    PBT_APMQUERYSUSPENDFAILED   =   $0002 ;
    PBT_APMQUERYSTANDBYFAILED   =   $0003 ;
    PBT_APMSUSPEND              =   $0004 ;
    PBT_APMSTANDBY              =   $0005 ;
    PBT_APMRESUMECRITICAL       =   $0006 ;
    PBT_APMRESUMESUSPEND        =   $0007 ;
    PBT_APMRESUMESTANDBY        =   $0008 ;
    PBTF_APMRESUMEFROMFAILURE   =   $00000001 ;
    PBT_APMBATTERYLOW           =   $0009 ;
    PBT_APMPOWERSTATUSCHANGE    =   $000A ;
    PBT_APMOEMEVENT             =   $000B ;
    PBT_APMRESUMEAUTOMATIC      =   $0012 ;   // 31 Jan 2003

type
    TWMPOWERBROADCAST = record
        Msg: Cardinal;
        PowerEvent: DWORD;
        Data: DWORD;
    end ;

// this lot written by Magenta (or heavily modified)

function ProcDate (text: string; var DateVal: TDateTime): boolean ;
Function PutStr(inString,putString: String; where: integer) : String;
Procedure ReplStr(var inString: string; putString: String; where: integer);
Procedure TranStr(var inString: string; TranString: String);
procedure InitStr (var inString: String; where, count: integer; ch: char);
function FixDirDrv (filename: string): string ;
function IsDrive (const drive: AnsiChar): boolean ;
FUNCTION IsDir( Dir: STRING ) : BOOLEAN;
procedure GetVolInfo(const Rootdir: AnsiString; var volname: string;
        var sernumb, maxcomlen, flags: longword; var filesys: string) ;
function SetVolLabel (const Rootdir, Volname: AnsiString): boolean ;
function DriveExists(Drive : Byte) : Boolean;
function DriveExists1(Drive : Byte) : Boolean;
function CheckDriveType(const Rootdir: AnsiString): integer;
function GetDriveTypeStr (DriveType: integer): string ;
function GetVolNameLen (const Rootdir: AnsiString): integer ;

// borrowed from various places

{*** Extended Reals to Strings ***}
function Ext2EStr (const E: Extended; const Decimals: Byte): String;
{ Converts an Extended Real into an exact String, No padding,
        with given number of Decimal Places }
function Ext2EStr2 (const E: Extended; const Decimals: Byte): String;
{ Converts an Extended Real into an exact String, No padding,
        with at most given number of Decimal Places }
function Ext2CEStr (const E: Extended; const Decimals: Byte): String;
{ Converts an Extended Real into an exact String, No padding,
        with given number of Decimal Places, with Commas separating
        thousands }

function Double2EStr (const D: Double; const Decimals: Byte): String;
{ Converts a Double Real into an exact String, No padding,
        with given number of Decimal Places }
function Single2EStr (const S: Single; const Decimals: Byte): String;
{ Converts a Single Real into an exact String, No padding,
        with given number of Decimal Places }
function Comp2EStr (const C: Comp): String;
{ Converts a Comp (Integral) Real into an exact String, No padding }
function Comp2CStr (const C : Comp; const Len : Byte): string;
{ Converts a Comp (Integral) Real into a Comma'ed String of
        specified Length, Len, NumPadCh used for Left padding }
function Comp2CEStr (const C : Comp): string;
{ Converts a Comp (Integral) Real into a Comma'ed String
        without Padding }
function Ext2Str (const E: Extended; const Len, Decimals: Byte): String;
{ Converts an Extended Real into a String of specified Length, using
        NumPadCh for Left Padding, and with Specified number of Decimals }
function Double2Str (const D: Double; const Len, Decimals: Byte): String;
{ Converts a Double Real into a String of specified Length, using
        NumPadCh for Left Padding, and with Specified number of Decimals }
function Single2Str (const S: Single; const Len, Decimals: Byte): String;
{ Converts an Single Real into a String of specified Length, using
        NumPadCh for Left Padding, and with Specified number of Decimals }
function Comp2Str (const C: Comp; const Len : Byte): String;
{ Converts a Comp (Integral) Real into a String of specified Length, using
        NumPadCh for Left Padding }

{*** Strings to Extended Reals ***}
function Str2Ext (const S: String): Extended;
{ Converts a String into an Extended Real }

{*** Extra String Operations ***}
function MidStr (const S : string; const F, N : Integer): string;
{ Returns the substring consisting of the N characters of S from F.
        If N > Length (S) then the substring = S. }
function LeftTillStr (const S : string; const Ch : Char): string;
{ Returns the substring consisting of the characters from S
        up to but not including the specified one.  If the specified
        character is not found then a null string is returned. }
function RightAfterStr (const S : String; const N : Integer): String;
        { Returns the sub-string to the right AFTER the first
                N Characters. if N >= Length (S) then a Null string
                is returned. }
function RightAfterChStr (const S : String; const Ch : Char): String;
        { Returns the sub-string to the right AFTER the first
                ocurrence of specifiec character.  If Ch not found then
                a Null String is returned. }
function StripTChStr (const S : string; const Ch : Char): string;
{ Returns the String with all specified trailing characters removed. }
function StripLChStr (const S : string; const Ch : Char): string;
{ Returns the String with all specified leading characters removed. }
function StripChStr (const S : string; const Ch : Char): string;
{ Returns the String with all specified leading and trailing
        characters removed. }
function ReplaceChStr (const S : string; const OldCh, NewCh : Char): string;
{ Returns the String with all occurrences of OldCh character
        replaced with NewCh character. }
function LeftAlignStr (const S : string; const N : Integer): string;
function RightAlignStr (const S : string; const N : Integer): string;

// found in xprocs.pas

{ String functions }
function  strHash(const S: AnsiString; LastBucket: Integer): Integer;
function  strCut(const S: String; Len: Integer): String;
function  strTrim(const S: String): String;
function  strTrimA(const S: String): String;
function  strTrimChA(const S: String; C: Char): String;
function  strTrimChL(const S: String; C: Char): String;
function  strTrimChR(const S: String; C: Char): String;
function  strLeft(const S: String; Len: Integer): String;
function  strLower(const S: String): String;
function  strMake(C: Char; Len: Integer): String;
function  strPadChL(const S: String; C: Char; Len: Integer): String;
function  strPadChR(const S: String; C: Char; Len: Integer): String;
function  strPadChC(const S: String; C: Char; Len: Integer): String;
function  strPadL(const S: String; Len: Integer): String;
function  strPadR(const S: String; Len: Integer): String;
function  strPadC(const S: String; Len: Integer): String;
function  strPadZeroL(const S: String; Len: Integer): String;
procedure strChange(var S:String; const Source, Dest: String);
function  strRight(const S: String; Len: Integer): String;
function  strSpace(Len: Integer): String;
function  strContains(const S1,S2: String): Boolean;
function  strContainsU(const S1,S2: String): Boolean;
function  strReplace(const S: String; C: Char; const Replace: String): String;
function  strEncrypt(const S: AnsiString; Key: Word): AnsiString;
function  strDecrypt(const S: AnsiString; Key: Word): AnsiString;

{ file functions - moved some to TaskWins }
function  fileTemp(const aExt: String): String;
function  fileLongName(const aFile: String): String;
function  fileShortName(const aFile: String): String;
function  fileShellOpen(const aFile: String): Boolean;
function  fileShellPrint(const aFile: String): Boolean;
function  ExtractName(const Filename: String): String;

{ system functions }
function  sysColorDepth: Integer;
procedure sysSaverRunning(Active: Boolean);

{ registry functions }

function regValueExists(aKey: HKEY; const Path: String): boolean ;
function regReadString(aKey: HKEY; const Path: String; Rights: LongWord = KEY_QUERY_VALUE): String;
function regWriteString(aKey: hKey; const Path,Value: String; Rights: LongWord = KEY_ALL_ACCESS): Boolean;
function regWriteInteger(aKey: HKEY; const Path: String; Value: Integer): Boolean;
function regReadInteger(aKey: HKEY; const Path: String): Integer;
function regInfoString(const Value: String; Win64Reg: boolean = false): String;
function regCurrentUser (Win64Reg: boolean = false): String;
function regCurrentCompany (Win64Reg: boolean = false): String;
procedure regWriteShellExt(const aExt,aCmd,aMenu,aExec: String);

// disk space functions, FAT32 compatible
procedure GetSpaceOnDrive(const Drive: AnsiString; var lngFreeSpace,
                                                    lngTotalBytes : int64);
function GetDiskFreeSpaceFAT32(const Drive: AnsiString ;
                        var lngFreeSpace, lngTotalBytes : int64): boolean ;

TYPE
tripel_at = ARRAY [1..3] OF Byte;
quadrupel_at = ARRAY [1..4] OF Byte;

FUNCTION codeb64 (cnt: integer; t: tripel_at): quadrupel_at;
FUNCTION decodeb64 (inp: quadrupel_at; VAR cnt: integer): tripel_at ;
function base64encode (const instr: AnsiString): AnsiString ;
function base64decode (const instr: AnsiString): AnsiString ;
function strREncrypt(const S: AnsiString; Key: Word): AnsiString;
function strRDecrypt(const S: AnsiString; Key: Word): AnsiString;
function strBEncrypt(const S: AnsiString; Key: Word): AnsiString;
function strBDecrypt(const S: AnsiString; Key: Word): AnsiString;
function strBXEncrypt(const S: AnsiString; Key: Word): AnsiString;
function strBXDecrypt(const S: AnsiString; Key: Word): AnsiString;
function strBXDecryptEx(const S: AnsiString; Key: Word): AnsiString;

const

padd = 64;
code64 : STRING[65] = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ' +
         'abcdefghijklmnopqrstuvwxyz' + '0123456789+/=';

// encoding keys for various purposes
enkeyStrings = 21111 ;
enkeyCamCollect = 8743 ;
enkeyMailMaint = 777 ;
enkeyPasswords = 17693 ;
enkeyDunManager = 25637 ;
enkeyWebForm = 61341 ;
enkeyComCap = 45657 ;
enkeyPassDir = 39377 ;
enkeyCeyes = 59176 ;

//function CoCreateGuid(var guid: TGUID): HResult; stdcall;far external 'ole32.dll';
function TumbleDigits (input: AnsiString): AnsiString ;
procedure StrLoadNoLockFile (Obj: TStrings; const FileName: string);
procedure LoadStringsFromFile( AStrings : TStrings; const FileName: string);
function WindowsShutdown (const Machine: AnsiString ;
                                    Force, Reboot, PowerOff: Boolean): Boolean ;
function BoolLit (opt: boolean): string ;

{$IFNDEF CPUX64}
function IsCPUIDAvailable: Boolean ;
function GetCPUID: TCPUID ;
function GetCPUVendor: TVendor ;
function GetCPUSerial: string ;    // Intel Pentium 2 (or 3?) and later only
{$ENDIF}

function RegDeleteKeyIncludingSubkeys (const Key: HKEY; const Name: PChar): Boolean;

procedure Bleep( AFreq, ADurationMS : DWORD );
procedure Beep1K;
procedure Beep500;
procedure Click;
Procedure LockWindow(Const Hnd:HWnd);
Procedure UnlockWindow(Const Hnd:HWND;Const UpdateNow: Boolean);
procedure RedrawWin( const Win : HWnd );

procedure EventsSetReg (const Source, AppFile: String);
procedure ServiceSetComm (const Source, Comment: String);
function ConvertIPAddr (Addr: LongWord): string ;
function CreditCardCheck (ccnumber: string; simple: boolean): string ;
function LastBootDateTime : TDateTime;
function FileInUse(FileName: UnicodeString): Boolean;
function TestPathAccess (FilePath: String; var ErrInfo: string): Boolean;
function IpToStr (Addr: LongWord): string ;
function StrToIP (strIP: string): LongWord ;
function IsIPStr (strIP: string): boolean ;
function IsFmtIPStr (var strIP: string): boolean ;
function Str2IP (strIP: string; var IPAddr: LongWord): boolean ;
function LongAddr2Dotted (IPAddr: string): string ;
function CheckUniqueInstance: boolean ;
function GetIOErrorStr (code: integer): string ;
procedure SimpleLogging (const FNameMask, Msg: String);
function CleanEmailAddr (oldaddr: string): string ;
procedure AntiAliasBitmap(K:byte; bitmap: TBitmap);
function IsValidEmail(const Value: AnsiString): Boolean;
// function EnumSerialPorts (var SerialPorts: TSerialPorts): integer;
function ReverseEmail (const AString: AnsiString): AnsiString ;
procedure EnableClearType(AControlFont: TFont; Enable: boolean) ;
function IsHtmlTags (const S: string): boolean ;
function EndianLong (L: LongWord) : LongWord;
function IsBitSet(const val: longint; const TheBit: byte): boolean;
function BitOn(const val: longint; const TheBit: byte): LongInt;
function BitOff(const val: longint; const TheBit: byte): LongInt;
function BitToggle(const val: longint; const TheBit: byte): LongInt;
procedure EndianSwap64(var Data);
function GetProcessHandles (ProcId: DWORD): integer ;
procedure EnableOrDisableChildren (Container: TWinControl; Enabled: Boolean) ;

type
  { TEventLogger }
  TEventLogger = class(TObject)
  private
    FName: String;
    FEventLog: Integer;
  public
    constructor Create(Name: String);
    destructor Destroy; override;
    procedure LogMessage(Message: String; EventType: DWord = 1;
      Category: Word = 0; ID: DWord = 0);
  end;

// descedent of TMemoryStream designed to buffer and write log files

TBuffLogStream = class(TMemoryStream)
  private
    FIdleSecs: Integer ;  // 18 Jan 2008 was minutes
    FIdleTimer: TTimer ;
    FHeader: String ;
    FBuffMax: Integer ;
    FNameMask: String ;
    FLogSize: integer ;
    FFullName: string ;
  protected
    procedure Timer (Sender: TObject) ;
  public
    constructor Create (const NameMask, Header: string) ;
    destructor Destroy; override;
    procedure SetIdleMins (IdleMins: Integer) ;
    procedure SetIdleSecs (IdleSecs: Integer) ;
    procedure SetBuffMax (BuffMax: Integer) ;
    function FlushFile: integer ;
    function WriteLine (const Line: string): Longint;
    property Capacity;
    property LogSize: integer read FLogSize ;
    property FullName: string read FFullName ;
  end;

//var
//   num4: longword ;       // used for converting IP addresses
//   num1: array [1..4] of byte absolute num4 ;

// handle for DLL
var
    ProcHandleModule: THandle;

GetProcessHandleCount: function (hProcess: THandle; var HandleCount: DWORD): BOOL; stdcall;

// ---------------------------------------------------------------------


implementation

// replace part of one  string with another string

Function PutStr(inString, putString: String; where: integer) : String;
Var
  index, j : integer;
Begin
  index := length (putString);    // get size of input string
  if index <> 0 then
  begin
      For j := where to where + (index-1) do
                            inString[j] := putString[(j+1)-where];
  end ;
  PutStr := inString;
End;

// replace part of one  string with another string

Procedure ReplStr(var inString: string; putString: String; where: integer);
Var
  last, count : integer;
Begin
  count := length (putString);    // get size of input string
  if count = 0 then exit ;
  last := length (inString) ;
  if last = 0 then exit ;
  if (where + count - 1) > last then count := last - where + 1 ;
   move (putString [1], inString [where], count) ;
End;

// translate characters in string
// TranString is pairs of characters, if first found changed to second

Procedure TranStr(var inString: string; TranString: String);
Var
  len, j, pairs, count, offset : integer;
Begin
  pairs := length (TranString) ;
  if pairs < 2 then exit ;
  pairs := pairs div 2 ;  // number of translation pairs
  len := length (inString) ;
  if (len = 0) or (pairs = 0) then exit ;
  offset := 1 ;
  for count := 1 to pairs do
  begin
      For j := 1 to len do
      begin
          if inString [j] = TranString [offset] then
                              inString [j] := TranString [succ (offset)] ;
     end ;
     inc (offset, 2) ;
  end ;
End;


// initialse part of one string with a character

procedure InitStr (var inString: String; where, count: integer; ch: char);
Var
  last: integer ;
Begin
  last := length (inString) ;    { get size of input string}
  if last = 0 then exit ;
  if (where + count - 1) > last then count := last - where + 1 ;
//  last := where + count - 1 ;
  FillChar (inString [where], count * SizeOf (Char), Ch);  // Unicode
End;

{ function to convert ASCII date in internal value }

function ProcDate (text: string; var DateVal: TDateTime): boolean ;
  var
   Year, Month, Day: word ;
   LocalDate: TDateTime ;
   begin

   result := true ;   { true=error }
   DateVal := 0 ;
   try
       LocalDate := StrToDate (text) ;
       DecodeDate(LocalDate, Year, Month, Day);
       if (month >= 1) and (month <= 12) and (day > 0) and (day <= 31) then
       begin
           DateVal := LocalDate ;
           result := false ;
           exit ;
       end ;
   except
       on EConvertError do result := true ;
   end ;
end ;


// check if drive letter (A-Z) exists

function IsDrive (const drive: AnsiChar): boolean ;
begin
   result := DiskSize (Ord (Upcase (Drive)) - 64) > 0 ;
end ;

// from SWAG

{*****************************************************************************
 * Function ...... IsDir()
 * Purpose ....... To check for the existance of a directory
 * Parameters .... Dir        Dir to check for
 * Returns ....... TRUE if Dir exists
 * Notes ......... None
 * Author ........ Martin Richardson
 * Date .......... May 13, 1992
 *****************************************************************************}
FUNCTION IsDir( Dir: STRING ) : BOOLEAN;
VAR
   wAttr: integer ;
BEGIN
    result := false ;
    if Dir = '' then exit ;
   Dir := strAddSlash (Dir) + '.' ;
    wAttr := FileGetAttr (Dir) ;
    if wAttr <> -1 then result := ( (wAttr AND faDIRECTORY) = faDIRECTORY );
END;

// validate file name, ensure drive and directory exists
// else rest to blank file

function FixDirDrv (filename: string): string ;
begin
    result := filename ;
    if FileExists (filename) then exit ;
    if length (filename) > 1 then
    begin
  // assume file name if . found in name, otherwise a directory
        if pos ('.', filename) > 0 then filename := ExtractFileDir (filename) ;
        if isDir (filename) then exit ;
    end ;
    result := ' ' ;
end ;

// check for a drive using WINAPI (bitmap of drives)
function DriveExists(Drive : Byte) : Boolean;
begin
 Result := Boolean(GetLogicalDrives AND(1 SHL Drive))
end;

// ??
function DriveExists1(Drive : Byte) : Boolean;
var
 LogDrives : set of 0..25;
begin
    Integer (LogDrives) := GetLogicalDrives;
    Result := Drive IN LogDrives;
end;

// drive type

function CheckDriveType(const Rootdir: AnsiString): integer;
begin
    result := GetDriveTypeA(PAnsiChar(Rootdir));
end ;

function GetDriveTypeStr (DriveType: integer): string ;
begin
     case DriveType of
      0               : Result := '?';
      1               : Result := 'Path does not exists';
      DRIVE_REMOVABLE : Result := 'Removable';
      DRIVE_FIXED     : Result := 'Fixed';
      DRIVE_REMOTE    : Result := 'Remote';
      DRIVE_CDROM     : Result := 'CD-ROM';
      DRIVE_RAMDISK   : Result := 'RAMDISK'
     else
      Result := 'Unknown';
     end;
end;


{GetVolumeInformation}

procedure GetVolInfo (const Rootdir: AnsiString; var volname: string;
            var sernumb, maxcomlen, flags: longword; var filesys: string) ;
var
    FileSysName  : Array[0..MAX_PATH] of WideChar;
    VolumeName   : Array[0..MAX_PATH] of WideChar;
    WideName: WideString ;  // Unicode
begin
    WideName := WideString (Copy (rootdir, 1, 3)) ;  // 7 Aug 2010
    volname := '' ;
    sernumb := 0 ;
    filesys := '' ;
    if NOT GetVolumeInformationW (PWideChar (WideName), VolumeName,
                Length(VolumeName), @sernumb, maxcomlen, flags,
                        FileSysName, Length(FileSysName)) then exit ;
   volname := volumename ;
   filesys := filesysname ;
end;

function SetVolLabel (const Rootdir, Volname: AnsiString): boolean ;
begin
    result := SetVolumeLabelA (PAnsiChar (Rootdir), PAnsiChar (VolName)) ;
end;

function GetVolNameLen (Const Rootdir: AnsiString): integer ;
var
    maxcomlen, temp: longword ;
begin
    result := 0 ;
    if NOT GetVolumeInformationA (PAnsiChar(rootdir), nil, 0, nil,
                                maxcomlen, temp, nil, 0) then exit ;
    result := maxcomlen ;
end ;

// ================================================================

function Ext2EStr (const E: Extended; const Decimals: Byte): String;
begin
        try
                Result := FloatToStrF (E, ffFixed, 18, Decimals)
        except
                Result := '';
        end;
end;

function Ext2EStr2 (const E: Extended; const Decimals: Byte): String;
begin
        Result := Ext2EStr (E, Decimals);
        Result := StripTChStr (Result, '0');
        if Length (Result) > 0 then
        begin
             if Result [Length (Result)] = MyFormatSettings.DecimalSeparator then
                                 Result := CopyLeft (Result, Length (Result) - 1);
        end ;
end;

function Ext2CEStr (const E: Extended; const Decimals: Byte): String;
begin
        try
                Result := FloatToStrF (E, ffNumber, 18, Decimals)
        except
                Result := '';
        end;
end;

function Double2EStr (const D: Double; const Decimals: Byte): String;
begin
        try
                Result := FloatToStrF (D, ffFixed, 15, Decimals)
        except
                Result := '';
        end;
end;

function Single2EStr (const S: Single; const Decimals: Byte): String;
begin
        try
                Result := FloatToStrF (S, ffFixed, 7, Decimals)
        except
                Result := '';
        end;
end;

function Comp2EStr (const C: Comp): String;
begin
        try
                Result := FloatToStrF (C, ffFixed, 18, 0)
        except
                Result := '';
        end;
end;

function Str2Ext (const S: String): Extended;
begin
        try
                Result := StrToFloat (S);
        except
                Result := 0;
        end;
end;


function Comp2CStr (const C : Comp; const Len : Byte): string;
begin
        Result := Comp2CEStr (C);
        Result := PadChLeftStr (CopyLeft (Result, Len), NumPadCh, Len);
end;

function Comp2CEStr (const C : Comp): string;
var
        LS, L, I : Integer;
        Temp : string;
begin
        Result := Comp2EStr (C);
        LS := Length (Result);
        L := (LS - 1) div 3;
        Temp := '';
        for I := 1 to L do
                Temp :=  MyFormatSettings.ThousandSeparator +
                             Copy (Result, LS - 3 * I + 1, 3) + Temp;
        Result := Copy (Result, 1, (LS - 1) mod 3 + 1) + Temp;
end;

function Ext2Str (const E: Extended; const Len, Decimals: Byte): String;
begin
        try
                Result := FloatToStrF (E, ffFixed, 18, Decimals)
        except
                Result := '';
        end;
        Result := PadChLeftStr (CopyLeft (Result, Len), NumPadCh, Len);
end;

function Double2Str (const D: Double; const Len, Decimals: Byte): String;
begin
        try
                Result := FloatToStrF (D, ffFixed, 15, Decimals)
        except
                Result := '';
        end;
        Result := PadChLeftStr (CopyLeft (Result, Len), NumPadCh, Len);
end;

function Single2Str (const S: Single; const Len, Decimals: Byte): String;
begin
        try
                Result := FloatToStrF (S, ffFixed, 7, Decimals)
        except
                Result := '';
        end;
        Result := PadChLeftStr (CopyLeft (Result, Len), NumPadCh, Len);
end;

function Comp2Str (const C: Comp; const Len: Byte): String;
begin
        try
                Result := FloatToStrF (C, ffFixed, 18, 0)
        except
                Result := '';
        end;
        Result := PadChLeftStr (CopyLeft (Result, Len), NumPadCh, Len);
end;

function MidStr (const S : string; const F, N : Integer): string;
begin
        Result := Copy (S, F, N);
end;

function LeftAlignStr (const S : string; const N : Integer): string;
begin
        Result := PadRightStr (Copy (S, 1, N), N);
end;

function RightAlignStr (const S : string; const N : Integer): string;
begin
        Result := PadLeftStr (Copy (S, 1, N), N);
end;

function LeftTillStr (const S : string; const Ch : Char): string;
var
        M: Integer;
begin
        M := Pos (Ch, S);
        if M < 2 then
                Result := ''
        else
                Result := Copy (S, 1, M - 1);
end;

function RightAfterStr (const S : String; const N : Integer): String;
begin
        Result := Copy (S, N + 1, Length (S) - N );
end;

function RightAfterChStr (const S : String; const Ch : Char): String;
var
        M: Integer;
begin
        M := Pos (Ch, S);
        if M = 0 then
                Result := ''
        else
                Result := Copy (S, M + 1, Length (S) - M);
end;

function StripChStr (const S : string; const Ch: Char): string;
begin
        Result := StripTChStr (StripLChStr (S, Ch), Ch);
end;

function StripTChStr (const S : string; const Ch: Char): string;
var
        Len: Integer;
begin
        Len := Length (S);
        while (Len > 0) and (S [Len] = Ch) do
                Dec (Len);
        if Len = 0 then
                Result := ''
        else
                Result := Copy (S, 1, Len);
end;

function StripLChStr (const S : string; const Ch: Char): string;
var
        I, Len: Integer;
begin
        Len := Length (S);
        I := 1;
        while (I <= Len) and (S [I] = Ch) do
                Inc (I);
        if (I > Len) then
                Result := ''
        else
                Result := Copy (S, I, Len - I + 1);
end;

function ReplaceChStr (const S : string;
        const OldCh, NewCh : Char): string;
var
        I: Integer;
begin
        Result := S;
        if OldCh = NewCh then
                Exit;
        for I := 1 to Length (S) do
                if S [I] = OldCh then
                        Result [I] := NewCh;
end;

function strHash(const S: AnsiString; LastBucket: Integer): Integer;
var
  i: Integer;
begin
  Result:=0;
  for i := 1 to Length(S) do
    Result := ((Result shl 3) xor Ord(S[i])) mod LastBucket;
end;

function strTrim(const S: String): String;
begin
  Result:=StrTrimChR(StrTrimChL(S,BLANK),BLANK);
end;

function strTrimA(const S: String): String;
begin
  Result:=StrTrimChA(S,BLANK);
end;

function strTrimChA(const S: String; C: Char): String;
var
  I               : Word;
begin
  Result:=S;
  for I:=Length(Result) downto 1 do
    if Result[I]=C then Delete(Result,I,1);
end;

function strTrimChL(const S: String; C: Char): String;
begin
  Result:=S;
  while (Length(Result)>0) and (Result[1]=C) do Delete(Result,1,1);
end;

function strTrimChR(const S: String; C: Char): String;
begin
  Result:=S;
  while (Length(Result)> 0) and (Result[Length(Result)]=C) do
    Delete(Result,Length(Result),1);
end;

function strLeft(const S: String; Len: Integer): String;
begin
  Result:=Copy(S,1,Len);
end;

function strLower(const S: String): String;
begin
  Result:=AnsiLowerCase(S);
end;

function strMake(C: Char; Len: Integer): String;
begin
  Result:=strPadChL('',C,Len);
end;

function strPadChL(const S: String; C: Char; Len: Integer): String;
begin
  Result:=S;
  while Length(Result)<Len do Result:=C+Result;
end;

function strPadChR(const S: String; C: Char; Len: Integer): String;
begin
  Result:=S;
  while Length(Result)<Len do Result:=Result+C;
end;

function strPadChC(const S: String; C: Char; Len: Integer): String;
begin
  Result:=S;
  while Length(Result)<Len do
  begin
    Result:=Result+C;
    if Length(Result)<Len then Result:=C+Result;
  end;
end;

function strPadL(const S: String; Len: Integer): String;
begin
  Result:=strPadChL(S,BLANK,Len);
end;

function strPadC(const S: String; Len: Integer): String;
begin
  Result:=strPadChC(S,BLANK,Len);
end;


function strPadR(const S: String; Len: Integer): String;
begin
  Result:=strPadChR(S,BLANK,Len);
end;

function strPadZeroL(const S: String; Len: Integer): String;
begin
  Result:=strPadChL(strTrim(S),ZERO,Len);
end;

function strCut(const S: String; Len: Integer): String;
begin
  Result:=strLeft(strPadR(S,Len),Len);
end;

function strRight(const S: String; Len: Integer): String;
begin
  if Len>=Length(S) then
    Result:=S
  else
    Result:=Copy(S,Succ(Length(S))-Len,Len);
end;


function strSpace(Len: Integer): String;
begin
  Result:=StrMake(BLANK,Len);
end;

function strContains(const S1,S2: String): Boolean;
begin
  Result:=Pos(S1,S2) > 0;
end;

function strContainsU(const S1,S2: String): Boolean;
begin
  Result:=strContains(AnsiUpperCase(S1),AnsiUpperCase(S2));
end;


function strReplace(const S: String; C: Char; const Replace: String): String;
var
  i : Integer;
begin
  Result:='';
  for i:=Length(S) downto 1 do
    if S[i]=C then
        Result:=Replace+Result
    else
        Result:=S[i]+Result;
end;

procedure strChange(var S:String; const Source, Dest: String);
var
  P : Integer;
begin
  P:=Pos(Source,S);
  while P<>0 do
  begin
    Delete(S,P,Length(Source));
    Insert(Dest,S,P);
    P:=Pos(Source,S);
  end;
end;

// must turn off range checking or encyption stuff dies !!!!!
{$R-}
{$Q-}

const
  C1 = 52845;
  C2 = 22719;

function strEncrypt(const S: AnsiString; Key: Word): AnsiString;
var
  I: Integer;
begin
//  try
        SetLength(Result,Length(S));
        for I := 1 to Length(S) do begin
            Result[I] := AnsiChar(Ord(S[I]) xor (Key shr 8));
            Key := (Ord(Result[I]) + Key) * C1 + C2;
        end;
//  except
//      result := '' ;
//  end ;
end;

function strDecrypt(const S: AnsiString; Key: Word): AnsiString;
var
  I: Integer;
begin
    try
        SetLength(Result,Length(S));
        for I := 1 to Length(S) do begin
            Result[I] := AnsiChar(Ord(S[I]) xor (Key shr 8));
            Key := (Ord(S[I]) + Key) * C1 + C2;
        end;
    except
        result := '' ;
    end ;
end;

{$R+}
{$Q+}

function fileTemp (const aExt: String): String;
var
    Buffer: array[0..1023] of WideChar;    // Unicode
    aFile : String;
begin
    GetTempPathW (Length (Buffer) - 1, Buffer) ;
    GetTempFileNameW (Buffer, 'TMP', 0, Buffer) ;
//  SetString(aFile, Buffer, StrLen(Buffer)) ;
    AFile := Buffer ;  // Unicode
    if aExt = '' then
        Result := aFile
    else
    begin
        Result := ChangeFileExt (aFile, aExt) ;
        RenameFile (aFile, Result) ;
    end ;
end;

function fileShellOpen(const aFile: String): Boolean;
begin
    Result := ShellExecute (hInstance, 'open', PChar (aFile),
                                                         nil, nil, SW_NORMAL) <= 32;
end;

function fileShellPrint(const aFile: String): Boolean;
begin
    Result := ShellExecute (hInstance, 'print', PChar (aFile),
                                                     nil, nil, SW_HIDE) <= 32;
end;

function fileLongName(const aFile: String): String;
var
    aInfo: TSHFileInfo ;
begin
    if SHGetFileInfo (PChar (aFile), 0, aInfo, SizeOf (aInfo),
                                                        SHGFI_DISPLAYNAME) <> 0 then
        Result:= aInfo.szDisplayName
    else
        Result:=aFile;
end;

function fileShortName(const aFile: String): String;
var
    aTmp: array[0..255] of WideChar;
    WideName: WideString ;  // Unicode
begin
    WideName := aFile ;
    if GetShortPathNameW (PWideChar (WideName), aTmp, Length (aTmp) - 1) = 0 then
        Result := aFile
    else
        Result := aTmp ;
end;

function ExtractName(const Filename: String): String;
var
  aExt : String;
  aPos : Integer;
begin
  aExt:=ExtractFileExt(Filename);
  Result:=ExtractFileName(Filename);
  if aExt <> '' then
  begin
    aPos:=Pos(aExt,Result);
    if aPos>0 then
       Delete(Result,aPos,Length(aExt));
  end;
end;

{ system functions }



function sysColorDepth: Integer;
var
  aDC: hDC;
begin
  aDC := GetDC(0);
  try
    Result:=1 shl (GetDeviceCaps(aDC,PLANES) * GetDeviceCaps(aDC, BITSPIXEL));
  finally
    ReleaseDC(0,aDC);
  end;
end;


procedure sysSaverRunning(Active: Boolean);
var
  aParam: Longint;
begin
  SystemParametersInfo (SPI_SCREENSAVERRUNNING, Word(Active),@aParam,0);
end;

{ registry functions }
// 23 Sept 2002 - exception handling around reading and writing
// 7 Sept 2011 - allow specific access to Wow64 for 32- and 64-bit registries

function regValueExists(aKey: HKEY; const Path: String): boolean ;
var
  aRegistry : TRegistry;
  aPath     : String;
  aValue    : String;
begin
  result := false ;
  aRegistry:=TRegistry.Create;
  try
  try
    with aRegistry do
    begin
      RootKey:=aKey;
      Access := KEY_QUERY_VALUE ;   // 8 Aug 2002, allows HLM for non-administrators
      aPath:=Path;
      aValue:='';
      while (Length(aPath)>0) and (strLastCh(aPath)<>'\') do
      begin
        aValue:=strLastCh(aPath)+aValue;
        strStripLast(aPath);
      end;
      if OpenKey(aPath,False) then   // 7 Sept 2011 don't want to create key
      begin
         result := ValueExists (aValue) ;
         CloseKey ;
      end ;
    end;
  except
  end ;
  finally
    aRegistry.Free;
  end;
end;

function regReadString(aKey: HKEY; const Path: String; Rights: LongWord = KEY_QUERY_VALUE): String;
var
  aRegistry : TRegistry;
  aPath     : String;
  aValue    : String;
begin
  result := '' ;
  aRegistry:=TRegistry.Create;
  try
  try
    with aRegistry do
    begin
      RootKey:=aKey;
      Access := Rights ; // 7 Sept 2011  KEY_QUERY_VALUE ;   // 8 Aug 2002, allows HLM for non-administrators
      aPath:=Path;
      aValue:='';
      while (Length(aPath)>0) and (strLastCh(aPath)<>'\') do
      begin
        aValue:=strLastCh(aPath)+aValue;
        strStripLast(aPath);
      end;
      OpenKey(aPath,False);  // 7 Sept 2011 don't want to create key
      if ValueExists (aValue) then   // 16 Feb 2004, avoid exception if no value
                  Result:=ReadString(aValue);
      CloseKey ;
    end;
  except
  end ;
  finally
    aRegistry.Free;
  end;
end;

function regReadInteger(aKey: HKEY; const Path: String): Integer;
var
  aRegistry : TRegistry;
  aPath     : String;
  aValue    : String;
begin
  result := 0 ;
  aRegistry:=TRegistry.Create;
  try
  try
    with aRegistry do
    begin
      RootKey:=aKey;
      Access := KEY_QUERY_VALUE ;   // 8 Aug 2002, allows HLM for non-administrators
      aPath:=Path;
      aValue:='';
      while (Length(aPath)>0) and (strLastCh(aPath)<>'\') do
      begin
        aValue:=strLastCh(aPath)+aValue;
        strStripLast(aPath);
      end;
      if OpenKey(aPath,False) then
      begin
         if ValueExists (aValue) then   // 16 Feb 2004, avoid exception if no value
                Result:=ReadInteger(aValue);
         CloseKey ;
      end ;
    end;
  except
  end ;
  finally
    aRegistry.Free;
  end;
end;

// a blank value deletes the value name, ignoring the value type

function regWriteString(aKey: HKEY; const Path,Value: String; Rights: LongWord = KEY_ALL_ACCESS): boolean ;
var
  aRegistry : TRegistry;
  aPath     : String;
  aValue    : String;
begin
  result := false ;
  aRegistry:=TRegistry.Create;
  try
  try
    with aRegistry do
    begin
      RootKey:=aKey;
      Access := Rights ; // 7 Sept 2011
      aPath:=Path;
      aValue:='';
      while (Length(aPath)>0) and (strLastCh(aPath)<>'\') do
      begin
        aValue:=strLastCh(aPath)+aValue;
        strStripLast(aPath);
      end;
      if OpenKey(aPath,True) then
      begin
         if Length (Value) = 0 then
             DeleteValue (aValue)      // 25 Nov 2004
         else
             WriteString(aValue,Value);
         CloseKey ;
         result := true ;
      end ;
    end;
  except
  end ;
  finally
    aRegistry.Free;
  end;
end;

function regWriteInteger(aKey: HKEY; const Path: String; Value: Integer): boolean ;
var
  aRegistry : TRegistry;
  aPath     : String;
  aValue    : String;
begin
  result := false ;
  aRegistry:=TRegistry.Create;
  try
  try
    with aRegistry do
    begin
      RootKey:=aKey;
      aPath:=Path;
      aValue:='';
      while (Length(aPath)>0) and (strLastCh(aPath)<>'\') do
      begin
        aValue:=strLastCh(aPath)+aValue;
        strStripLast(aPath);
      end;
      if OpenKey(aPath,True) then
      begin
          WriteInteger(aValue,Value);
          CloseKey ;
          result := true ;
      end ;
    end;
  except
  end ;
  finally
    aRegistry.Free;
  end;
end;


function regInfoString(const Value: String; Win64Reg: boolean = false): String;
var
  path: string ;
  rights: longword ;
const
  KEY_WOW64_32KEY        = $0200;  // missing from D7
  KEY_WOW64_64KEY        = $0100;
  KEY_WOW64_RES          = $0300;

begin
  path := REG_CURRENT_VERSION95 ;
  if IsWinNT then path := REG_CURRENT_VERSIONNT ;
  path := path + '\' + Value ;
  rights := KEY_QUERY_VALUE ;
  if IsWin64 or IsWow64 then   // 7 Sept 2011 access specified registry keys
  begin
      if Win64Reg then
          rights := rights OR KEY_WOW64_64KEY
      else
          rights := rights OR KEY_WOW64_32KEY ;
  end;
  Result := regReadString (HKEY_LOCAL_MACHINE, path, rights) ;  // 8 Aug 2002, was regGetString, but only admin on NT
end;

function regCurrentUser (Win64Reg: boolean = false): String;
begin
  Result:=regInfoString(REG_CURRENT_USER, Win64Reg);
end;

function regCurrentCompany (Win64Reg: boolean = false): String;
begin
  Result:=regInfoString(REG_CURRENT_COMPANY, Win64Reg);
end;

{ Add a shell extension to the registry }
procedure regWriteShellExt(const aExt,aCmd,aMenu,aExec: String);
var
  s, aPath : String;
begin
  with TRegistry.Create do
  try
    RootKey := HKEY_CLASSES_ROOT;
    aPath   := aExt;
    if KeyExists(aPath) then
    begin
      OpenKey(aPath,False);
      S:=ReadString('');
      CloseKey;
      if S<>'' then
         if KeyExists(S) then
            aPath:=S;
    end;

    OpenKey(aPath+'\Shell\'+aCmd,True);
    WriteString('',aMenu);
    CloseKey;

    OpenKey(aPath+'\Shell\'+aCmd+'\Command',True);
    WriteString('',aExec + ' %1');
    CloseKey;
  finally
    Free;
  end;
end;



{-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-}
//Structures used in GetDiskFreeSpaceFAT32
{-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-}

type
  //DeviceIoControl registers structure...
  TDevIoCtl_Registers = Record
    Reg_EBX   : DWord;
    Reg_EDX   : DWord;
    Reg_ECX   : DWord;
    Reg_EAX   : DWord;
    Reg_EDI   : DWord;
    Reg_ESI   : DWord;
    Reg_Flags : DWord;
  end;

  //Structure passed in Get_ExtFreeSpace ...
  TExtGetDskFreSpcStruc = Record
    ExtFree_Size                      : Word;
    ExtFree_Level                     : Word;
    ExtFree_SectorsPerCluster         : Integer;
    ExtFree_BytesPerSector            : Integer;
    ExtFree_AvailableClusters         : Integer;
    ExtFree_TotalClusters             : Integer;
    ExtFree_AvailablePhysSectors      : Integer;
    ExtFree_TotalPhysSectors          : Integer;
    ExtFree_AvailableAllocationUnits  : Integer;
    ExtFree_TotalAllocationUnits      : Integer;
    ExtFree_Rsvd                      : array [0..1] of Integer;
  end;

{-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-}
//My Delphi implimentation of - Int 21h Function 7303h Get_ExtFreeSpace (FAT32)
// this version works on networked FAT32 drives, the GetxxxEx does not

{-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-}

function GetDiskFreeSpaceFAT32(const Drive: AnsiString ;
                        var lngFreeSpace, lngTotalBytes : int64): boolean ;
const
  VWIN32_DIOC_DOS_DRIVEINFO = 6;
var
  DevIoHandle         : THandle;
  BytesReturned       : longWord;
  Reg                 : TDevIoCtl_Registers;
  ExtGetDskFreSpcStruc: TExtGetDskFreSpcStruc;
  Secs                : int64 ;
begin

// pending check for OSR2

    result := false;
    FillChar(ExtGetDskFreSpcStruc, sizeof(TExtGetDskFreSpcStruc),0);
    FillChar(Reg, sizeof(TDevIoCtl_Registers),0);
    with Reg do
    begin
        reg_EAX :=  $7303;
        reg_EDX := Dword(PAnsiChar(Drive)) ; //DS:DX  C:/ in ASCII
        Reg_EDI := DWord(@ExtGetDskFreSpcStruc); //ES:DI
        reg_ECX := sizeof(TExtGetDskFreSpcStruc);
        reg_Flags := 1; //set carry flag to assume error.
    end;

    DevIoHandle := CreateFile( '\\.\vwin32', Generic_Read,
    File_Share_Read or File_Share_Write, nil, Open_Existing, File_Attribute_Normal, 0);

    if DevIoHandle <> Invalid_Handle_Value then
    begin
        result := DeviceIoControl(DevIoHandle, VWIN32_DIOC_DOS_DRIVEINFO,
         @Reg, SizeOf(Reg), @Reg, SizeOf(Reg), BytesReturned, nil);
        CloseHandle(DevIoHandle);
    if not result then
            exit //error
        else if (Reg.reg_Flags and 1 <> 0) then
        begin
            result := false; //If carry flag not cleared then => error
            exit;
        end
        else
        with ExtGetDskFreSpcStruc do
        begin
            secs := ExtFree_BytesPerSector * ExtFree_SectorsPerCluster ;
            lngTotalBytes := secs * ExtFree_TotalClusters ;
            lngFreeSpace := secs * ExtFree_AvailableClusters;
            result := true;
        end;
     end;
end; {GetDiskFreeSpaceFAT32}


// C:, etc
procedure GetSpaceOnDrive(const Drive: AnsiString; var lngFreeSpace,
                                                lngTotalBytes : int64);
var
    dnum: byte ;    //  1 = A, 2 = B, et
begin
    if length (drive) = 0 then exit ;
    dnum := ord (upcase (drive [1])) - 64 ;
    lngFreeSpace := DiskFree (dnum) ;
    lngTotalBytes := DiskSize (dnum) ;
end ;

// base64 encode a string

function base64encode (const instr: AnsiString): AnsiString ;
var
    inlen, inoff, outoff, curlen: integer ;
    outstr: AnsiString ;
    triple: tripel_at ;
    quad: quadrupel_at;
begin
    inlen := length (instr) ;
    inoff := 1 ;
    outoff := 1 ;
    outstr := '' ;
    SetLength (outstr, inlen * 2);
    while (inoff <= inlen) do
    begin
        Move(instr [inoff], triple, 3);
        curlen := inlen - inoff + 1 ;
        if (curlen > 3) then curlen := 3 ;
        quad := codeb64 (curlen, triple) ;
        Move (quad, outstr [outoff], 4) ;
        Inc (inoff, 3) ;
        Inc (outoff, 4) ;
    end ;
    SetLength (outstr, outoff - 1);
    result := outstr ;
end ;

// base64 encode three bytes into four bytes

FUNCTION codeb64(cnt: integer; t: tripel_at): quadrupel_at;  //STRING;
VAR        { *codeb64* }
    q, out: quadrupel_at;
   idx: integer;
BEGIN      { *codeb64* }
    IF (cnt < 3) THEN
    BEGIN
       t[3] := 0;
       q[4] := padd;
   END
   ELSE
        q[4] := (t[3] AND $3f);
      IF (cnt < 2) THEN
    BEGIN
       t[2] := 0;
       q[3] := padd;
   END
   ELSE
        q[3] := Byte (((t[2] SHL 2) OR (t[3] SHR 6)) AND $3f);
   q[2] := Byte (((t[1] SHL 4) OR (t[2] SHR 4)) AND $3f);
   q[1] := ((t[1] SHR 2) AND $3f);
   FOR idx := 1 TO 4 DO
          out [idx] := byte (code64[(q[idx] + 1)]) ;
   codeb64 := out;
END;       { *codeb64* }

// base64 decode a string

function base64decode (const instr: AnsiString): AnsiString ;
var
    inlen, inoff, outoff, actlen: integer ;
    outstr: AnsiString ;
    triple: tripel_at ;
    quad: quadrupel_at;
begin
    inlen := length (instr) ;
    inoff := 1 ;
    outoff := 1 ;
    outstr := '' ;
    SetLength (outstr, inlen);
    while (inoff <= inlen) do
    begin
        Move(instr [inoff], quad, 4);
        triple := decodeb64 (quad, actlen) ;
        if actlen = -1 then    // illegal base64 character
        begin
            result := '' ;
            exit ;
        end ;
        Move (triple, outstr [outoff], actlen) ;
        Inc (inoff, 4) ;
        Inc (outoff, actlen) ;
    end ;
    SetLength (outstr, outoff - 1);
    result := copy (outstr, 1, outoff - 1) ;
end ;

// base64 decode four bytes into three

function decodeb64 (inp: quadrupel_at; VAR cnt: integer): tripel_at ;
VAR        { *decodeb64* }
   idx, val: integer;
   t: tripel_at ;
   q: quadrupel_at;
BEGIN      { *decodeb64* }
   cnt := 3;
   FOR idx := 1 TO 4 DO
   BEGIN
        val := CharPos (AnsiChar(inp[idx]), code64) ;
        if val = 0 then    // found an illegal base64 character
        begin
            cnt := - 1 ;
            exit ;
        end ;
       q[idx] := val - 1 ;
       IF (q[idx] = padd) THEN Dec (cnt);
   END;
   t[1] := Byte ((q[1] SHL 2) OR ((q[2] SHR 4) AND $03));
   t[2] := Byte ((q[2] SHL 4) OR ((q[3] SHR 2) AND $0f));
   t[3] := Byte ((q[3] SHL 6) OR (q[4] AND $3f));
    result := t ;
END;       { *decodeb64* }


// must turn off range checking or encyption stuff dies !!!!!
{$R-}
{$Q-}

// more encryption stuff, based on Borland but a little more random
// encrypted string is binary values

const
  D2 = 52845;

function strREncrypt(const S: AnsiString; Key: Word): AnsiString;
var
    I, len: integer;
    D1: word ;
    outstr: string [255] ;
begin
    len := Length(S) ;
    if len <> 0 then
    begin
        Randomize;
        D1 := Random (MaxWord) ;
        Key := Key + D1 ;
        outstr [0] := AnsiChar (Len + 2) ;
        Move (D1, outstr [1], 2) ;   // keep multiply value
        for I := 1 to Len do begin
            outstr [I + 2] := AnsiChar (Ord (S [I]) xor (Key shr 8));
            Key := (Ord (outstr [I + 2]) + Key) * D1 + D2;
        end ;
    end ;
    result := copy (outstr, 1, len + 2) ;
end;

function strRDecrypt(const S: AnsiString; Key: Word): AnsiString;
var
    I, len: Integer;
    D1: word ;
    outstr: string [255] ;
begin
    len := Length(S) ;
    if len > 2 then
    begin
        Move (S [1], D1, 2) ;   // keep multiply value
        Key := Key + D1 ;
        outstr [0] := AnsiChar (len - 2) ;
        for I := 1 to Len do begin
            outstr[I] := AnsiChar (Ord (S [I + 2]) xor (Key shr 8));
            Key := (Ord (S [I + 2]) + Key) * D1 + D2;
        end;
    end ;
    result := copy (outstr, 1, len - 2) ;
end;

{$R+}
{$Q+}

// encryption stuff converted into base64 alphanbetic, with length, nax 98 ish
// information, returns blank if illegal

function strBEncrypt(const S: AnsiString; Key: Word): AnsiString;
var
    temp: AnsiString ;
begin
    result := '' ;
    if Length (S) > 98 then exit ;
    temp := strREncrypt (S, Key) ;
    temp := AnsiString (Int2StrZ (length (temp), 2)) + temp ;  // add length to start // 7 Aug 2010
    result := base64encode (temp) ;
end ;

function strBDecrypt(const S: AnsiString; Key: Word): AnsiString;
var
  keylen: integer;
  temp: AnsiString ;
begin
   result := '' ;
    try
        temp := base64decode (S) ;
        if length (temp) < 3 then exit ;
        keylen := AscToIntAnsi (copy (temp, 1, 2)) ;  // get length from start  // 7 Aug 2010
        if keylen = 0 then exit ;
        if (keylen + 2) <> length (temp) then exit ;
        temp :=  copy (temp, 3, 250) ;
        result := strRDecrypt (temp, Key) ;
    except ;
        result := '' ;
    end ;
end ;

// encryption stuff converted into base64 alphanbetic, with length, max 254 ish
// information, returns blank if illegal

function strBXEncrypt(const S: AnsiString; Key: Word): AnsiString;
var
    temp: AnsiString ;
begin
    result := '' ;
    if Length (S) > 254 then exit ;
    temp := strREncrypt (S, Key) ;
    temp :=  AnsiString (Int2StrZ (length (temp), 3)) + temp ;  // add length to start // 7 Aug 2010
    result := base64encode (temp) ;
end ;

function strBXDecrypt(const S: AnsiString; Key: Word): AnsiString;
var
    keylen: integer;
    temp: AnsiString ;
begin
   result := '' ;
    try
        temp := base64decode (S) ;
        if length (temp) < 4 then exit ;
        keylen := AscToIntAnsi (copy (temp, 1, 3)) ;  // get length from start  // 7 Aug 2010
        if keylen = 0 then exit ;
        if (keylen + 3) <> length (temp) then exit ;
        temp :=  copy (temp, 4, 999) ;
        result := strRDecrypt (temp, Key) ;
    except ;
        result := '' ;
    end ;
end ;

// 15 Oct 2010 if decryption fails, assume it was clear text

function strBXDecryptEx(const S: AnsiString; Key: Word): AnsiString;
var
    keylen, strlen: integer;
    temp: AnsiString ;
begin
    result := '' ;
    if S = '' then exit ;
    try
        temp := base64decode (S) ;
        strlen := length (temp) ;
        if (strlen = 3) and (temp = '000') then exit ;  // blank encoded string
        if (strlen > 3) then
        begin
            keylen := AscToIntAnsi (Copy (temp, 1, 3)) ;  // get length from start  // 7 Aug 2010
            if (keylen + 3) = strlen then
            begin
                temp := Copy (temp, 4, 999) ;
                result := strRDecrypt (temp, Key) ;
            end;
        end;
        if result = '' then result := S ;  // blank
    except ;
        result := '' ;
    end ;
end;

// simple encoding by adding different number to each digit

const
tumblevals: array [1..17] of integer = (7,11,9,14,6,12,14,1,7,3,13,12,9,4,7,4,2) ;

function TumbleDigits (input: AnsiString): AnsiString ;
var
    len, I, D: integer ;
begin
    result := '' ;
    len := length (input) ;
    if len = 0 then exit ;
    SetLength (result, len) ;
    for I := 1 to len do
    begin
        D := Ord (input [I]) + 16 ;
        if I <= 16 then D := D + tumblevals [I] ;
        result [I] := AnsiChar (D) ;
    end ;
end ;


// version of LoadFromFile that does not prevent writing to file

procedure StrLoadNoLockFile (Obj: TStrings; const FileName: string);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  try
    Obj.LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure LoadStringsFromFile( AStrings : TStrings; const FileName: string);
// This loads a strings from a file, treating any embedded #0 characters
// as line ends, not end of file.

  procedure MyLoadFromStream(Stream: TStream);

    procedure SetTextStr(const Value: string);

      function IsEndOfLine( AChar : Char ) : boolean;
      begin
        Result := (AChar = #0) or (AChar = #10) or (AChar = #13) ;  // Unicode
      end;

    var
      P, Start, LastChar: PChar;
      S: string;
    begin
      AStrings.BeginUpdate;
      try
        AStrings.Clear;
        P := Pointer(Value);
        LastChar := P + Length( Value );

        if P <> nil then
          While P < LastChar do
            begin
            Start := P;
            while not IsEndOfLine( P^ ) do Inc(P);
            SetString( S, Start, P - Start);
            AStrings.Add(Trim(S));
            while (P < LastChar) and IsEndOfLine( P^ ) do Inc(P);
            end;
      finally
        AStrings.EndUpdate;
      end;
    end;

  var
    Size: Integer;
    S: string;
  begin
    AStrings.BeginUpdate;
    try
      Size := Stream.Size - Stream.Position;
      SetString(S, nil, Size);
      Stream.Read(Pointer(S)^, Size);
      SetTextStr(S);
    finally
      AStrings.EndUpdate;
    end;
  end;

var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    MyLoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

// following borrowed from KOL
// also see stopice.pas which has something similar
// must supply machine name if used in service

function WindowsShutdown (const Machine: AnsiString ;
                                    Force, Reboot, PowerOff: Boolean): Boolean ;
var
    hToken: THandle;
    tkp, tkp_prev: TTokenPrivileges;
    dwRetLen :DWORD;
    Flags: Integer;
begin
    Result := False ;
    Flags := EWX_SHUTDOWN ;
    if Reboot then Flags := Flags or EWX_REBOOT ;
    if PowerOff then Flags := Flags or EWX_POWEROFF ;
    if Force then Flags := Flags or EWX_FORCE ;
    if IsWin95 then // Windows95/98/Me
    begin
        if Machine <> '' then Exit ;
        Result := ExitWindowsEx (Flags, 0) ;
        Exit ;
    end ;
    OpenProcessToken (GetCurrentProcess(), TOKEN_ADJUST_PRIVILEGES or
                                                    TOKEN_QUERY, hToken) ;
    if not LookupPrivilegeValueA (PAnsiChar(Machine), 'SeShutdownPrivilege',
                                        tkp.Privileges[0].Luid) then Exit;
    tkp_prev := tkp ;
    tkp.PrivilegeCount := 1;
    tkp.Privileges[0].Attributes := SE_PRIVILEGE_ENABLED ;
    AdjustTokenPrivileges (hToken, FALSE, tkp, sizeof(tkp), tkp_prev, dwRetLen);
    if not LookupPrivilegeValueA (PAnsiChar(Machine), 'SeRemoteShutdownPrivilege',
                                         tkp.Privileges[0].Luid) then Exit;
    tkp.PrivilegeCount := 1;
    tkp.Privileges[0].Attributes := SE_PRIVILEGE_ENABLED ;
    AdjustTokenPrivileges (hToken, FALSE, tkp, sizeof(tkp), tkp_prev, dwRetLen) ;
    if (Machine <> '') then
        Result := InitiateSystemShutdownA (PAnsiChar(Machine), nil, 0, Force, Reboot)
    else
        Result := ExitWindowsEx (Flags, 0) ;
end;


function BoolLit (opt: boolean): string ;
begin
    if opt then
        result := 'TRUE'
    else
        result := 'FALSE' ;
end ;

// Intel
{$IFNDEF CPUX64}
function IsCPUIDAvailable : Boolean; register;
asm
  PUSHFD                {direct access to flags no possible, only via stack}
  POP     EAX               {flags to EAX}
  MOV     EDX,EAX           {save current flags}
  XOR     EAX,ID_BIT                    {not ID bit}
  PUSH    EAX               {onto stack}
  POPFD                 {from stack to flags, with not ID bit}
  PUSHFD                {back to stack}
  POP     EAX               {get back to EAX}
  XOR     EAX,EDX           {check if ID bit affected}
  JZ      @exit             {no, CPUID not availavle}
  MOV     AL,True           {Result=True}
@exit:
end;

function GetCPUID : TCPUID; assembler; register;
asm
  PUSH    EBX         {Save affected register}
  PUSH    EDI
  MOV     EDI,EAX     {@Result}
  MOV     EAX,1       // get processor signature and flags
  DW      $A20F       {CPUID Command}
  STOSD               {CPUID[1] processor signature }
  MOV     EAX,EBX
  STOSD               {CPUID[2] Brand Id, APIC, etc  }
  MOV     EAX,ECX
  STOSD               {CPUID[3] reserved bit array }
  MOV     EAX,EDX
  STOSD               {CPUID[4] bit array  }
  POP     EDI         {Restore registers}
  POP     EBX
end;

function GetCPUVendor : TVendor; assembler; register;
asm
  PUSH    EBX           {Save affected register}
  PUSH    EDI
  MOV     EDI,EAX       {@Result (TVendor)}
  MOV     EAX,0         // get vendor Id and highest integer value
  DW      $A20F         {CPUID Command}
  MOV     EAX,EBX
  XCHG    EBX,ECX               {save ECX result}
  MOV     ECX,4
@1:
  STOSB
  SHR     EAX,8
  LOOP    @1
  MOV     EAX,EDX
  MOV     ECX,4
@2:
  STOSB
  SHR     EAX,8
  LOOP    @2
  MOV     EAX,EBX
  MOV     ECX,4
@3:
  STOSB
  SHR     EAX,8
  LOOP    @3
  POP     EDI               {Restore registers}
  POP     EBX
end;

function GetCPUPartSerial : TPartSerial; assembler; register;
asm
  PUSH    EBX         {Save affected register}
  PUSH    EDI
  MOV     EDI,EAX     {@Result}
  MOV     EAX,3      // get processor serial number
  DW      $A20F       {CPUID Command}
  STOSD               {ProcSerial[1] - ignore }
  MOV     EAX,EBX
  STOSD               {ProcSerial[2] - ignore }
  MOV     EAX,ECX
  STOSD               {ProcSerial[3] - bits 31-00 of serial }
  MOV     EAX,EDX
  STOSD               {ProcSerial[4] - bits 63-32 of serial }
  POP     EDI         {Restore registers}
  POP     EBX
end;

function GetCPUSerial: string ;    // Intel Pentium 3 and later only
var
    XCPUID: TCPUID ;
    XVendor: TVendor ;
    XPartSerial: TPartSerial ;
begin
    result := 'No Serial Number' ;
    if NOT IsCPUIDAvailable then exit ;
    XCPUID := GetCPUID ;
    XVendor := GetCPUVendor ;
    if (XVendor = 'GenuineIntel') and
                        ((XCPUID [4].Full and PSN_FLAG) = PSN_FLAG) then
    begin
        XPartSerial := GetCPUPartSerial ;
    // result is 96-bits, XCPUID [1] + XProcSerial [4] + XProcSerial [3]
        result := Format ('%4.4x-%4.4x-%4.4x-%4.4x-%4.4x-%4.4x',
          [XCPUID [1].High, XCPUID [1].Low, XPartSerial [4].High,
            XPartSerial [4].Low, XPartSerial [3].High, XPartSerial [3].Low]) ;
    end ;
end;
{$ENDIF}

function RegDeleteKeyIncludingSubkeys (const Key: HKEY; const Name: PChar): Boolean;
var
    H: HKEY;
    KeyName: String;
    KeyNameCount, MaxCount: DWORD;
    FT: TFileTime;
    I: Integer;
begin
    if Win32Platform = VER_PLATFORM_WIN32_NT then
    begin
        Result := False;
        if RegOpenKeyEx (Key, Name, 0, KEY_ENUMERATE_SUB_KEYS or
                                         KEY_QUERY_VALUE, H) <> ERROR_SUCCESS then Exit;
        if RegQueryInfoKey (H, nil, nil, nil, nil, @MaxCount, nil, nil, nil, nil,
                                                        nil, nil) = ERROR_SUCCESS then
        begin
            if MaxCount < 1 then MaxCount := 1 ;
            SetLength (KeyName, MaxCount);
            I := 0;
            while True do
            begin
                KeyNameCount := MaxCount + 1 ;
                if RegEnumKeyEx (H, I, PChar(KeyName), KeyNameCount, nil,
                                            nil, nil, @FT) <> ERROR_SUCCESS then Break;
                if not RegDeleteKeyIncludingSubkeys (H, PChar(KeyName)) then Inc (I);
            end;
        end;
        RegCloseKey (H);
    end;
    Result := RegDeleteKey(Key, Name) = ERROR_SUCCESS;
end;

procedure Bleep( AFreq, ADurationMS : DWORD );
// Produces a sound on the PC speaker
begin
  Windows.Beep( AFreq, ADurationMS );
end;

procedure Beep1K;
// Produces a 1kHz tone for 100mS
begin
  Bleep( 1000, 100 );
end;

procedure Beep500;
// Produces a 500Hz tone for 100mS
begin
  Bleep( 500, 100 );
end;

procedure Click;
// Produces a click from the PC speaker
begin
  Bleep( 1000, 1 );
end;

// procs to stop a window flickering while it's updated

Procedure LockWindow(Const Hnd:HWnd);
{ Prevent redrawing of a window }
begin
  SendMessage(Hnd, WM_SETREDRAW, 0, 0);
end;

Procedure UnlockWindow(Const Hnd:HWND;Const UpdateNow: Boolean);
{ Restores redrawing of a window }
var
  Flags: Word;
begin
  SendMessage(Hnd, WM_SETREDRAW, 1, 0);
  Flags := RDW_FRAME + RDW_INVALIDATE +
    RDW_ALLCHILDREN + RDW_NOINTERNALPAINT;
  If UpdateNow then
     Inc(Flags, RDW_UPDATENOW);
  RedrawWindow(Hnd, nil, 0, flags);
end;


procedure RedrawWin( const Win : HWnd );
// Redraws this window
begin
   RedrawWindow( Win,
                 nil,
                 0,
                 RDW_FRAME + RDW_INVALIDATE + RDW_ALLCHILDREN);
end ;


// set-up event log registry keys for NT logs

procedure EventsSetReg (const Source, AppFile: String);
const
  RegPath = 'SYSTEM\CurrentControlSet\Services\Eventlog\Application\';
  TypesSupported = EVENTLOG_INFORMATION_TYPE or EVENTLOG_ERROR_TYPE;
begin
  with TRegistry.Create do
    try
      RootKey := HKEY_LOCAL_MACHINE;
      if not OpenKey(RegPath + Source, True) then
        raise Exception.Create('Cannot create registry key');
      WriteString('CategoryMessageFile', '');
      WriteInteger('CategoryCount', 0);
      WriteString('EventMessageFile', AppFile) ;
      WriteInteger('TypesSupported', TypesSupported);
    finally
      Free
    end
end;

procedure ServiceSetComm (const Source, Comment: String);
const
  RegPath = 'SYSTEM\CurrentControlSet\Services\';
begin
  with TRegistry.Create do
    try
      RootKey := HKEY_LOCAL_MACHINE;
      if not OpenKey(RegPath + Source, True) then
        raise Exception.Create('Cannot create registry key');
      WriteString('Description', Comment) ;
    finally
      Free
    end
end;

{ TEventLogger - borrowed from Borland version but added EventsSetReg  }

constructor TEventLogger.Create(Name: String);
begin
  FName := Name;
  FEventLog := 0;
end;

destructor TEventLogger.Destroy;
begin
  if FEventLog <> 0 then
    DeregisterEventSource(FEventLog);
  inherited Destroy;
end;

procedure TEventLogger.LogMessage(Message: String; EventType: DWord;
  Category: Word; ID: DWord);
var
  P: Pointer;
begin
  P := PChar(Message);
  if FEventLog = 0 then
    FEventLog := RegisterEventSource(nil, PChar(FName));
  ReportEvent(FEventLog, EventType, Category, ID, nil, 1, 0, @P, nil);
end;

{ code to put into main program

    EventLogger: TEventLogger ;

procedure AddEventMessage(Message: String; EventType: DWord; Category, ID: DWord);
begin
  if EventLogger = nil then
  begin
    EventsSetReg (fServiceName, Application.ExeName) ;
    EventLogger := TEventLogger.Create (fServiceName);
  end ;
  EventLogger.LogMessage(Message, EventType, Category, ID);
end;  }


// convert IP address into ASCII and vice versa

// must turn off range checking off
{$R-}
{$Q-}

function ConvertIPAddr (Addr: LongWord): string ;
var
   I: integer ;
   num4: longword ;
   num1: array [1..4] of byte absolute num4 ;
begin
    num4 := Addr ;
    result := '' ;
    for I := 1 to 4 do
    begin
        result := result + IntToStr (num1 [I]) ;
        if I = 4 then exit ;
        result := result + '.' ;
    end ;
end ;

function IpToStr (Addr: LongWord): string ;
begin
    result := ConvertIPAddr (Addr) ;
end;

function StrToIP (strIP: string): LongWord ;
begin
    Str2IP (strIP, result) ;
end ;

function IsIPStr (strIP: string): boolean ;
var
    IPAddr: LongWord ;
begin
   result := Str2IP (strIP, IPAddr) ;
end ;

function IsFmtIPStr (var strIP: string): boolean ;
var
    IPAddr: LongWord ;
begin
    result := Str2IP (strIP, IPAddr) ;
    if result then strIP := ConvertIPAddr (IPAddr) ;  // formats less space, zeros, etc.
end ;

function Str2IP (strIP: string; var IPAddr: LongWord): boolean ;
var
    I, len, value, startpos, dotpos: Integer;
    MyIPAddr: array [1..4] of byte ;
    nonzeroflag: boolean ;
begin
    result := false ;
    IPAddr := 0;
    len := Length (strIP) ;
    if len < 7 then exit ;    // 0.0.0.0 bare IP address

// read each dotted number
    nonzeroflag := false ;
    startpos := 1 ;
    for I := 1 to 4 do
    begin
        if len <= 0 then exit ;
        if I < 4 then
            dotpos := Pos ('.', Copy (strIP, startpos, len))
        else
            dotpos := len + 1 ;
        if dotpos <= 0 then exit ;   // not enough dots
        if dotpos > 1 then
            value := AscToInt (Copy (strIP, startpos, Pred (dotpos)))
        else
            value := 0 ;  // allow for blank
        if value > 255 then exit ;   // number invalid for conversion
        if value > 0 then nonzeroflag := true ;
        MyIPAddr [I] := value ;
        startpos := startpos + dotpos ;
        len := len - dotpos ;
    end ;

// checl valid IP address, only allowed all zeroes
    if (MyIPAddr [1] = 0) and nonzeroflag then exit ;

// found a valid IP address, keep it
    Move (MyIPAddr, IPAddr, SizeOf(LongWord)) ;
    result := true ;
end ;

// convert long IP numeric address (ie 12345678) into a dotted address

var
    num4: longword ;   // used for converting long IP address to dots
    num1: array [1..4] of byte absolute num4 ;

function LongAddr2Dotted (IPAddr: string): string ;
var
    I: integer ;
begin
    result := trim (IPAddr) ;
    if pos ('.', IPAddr) > 0 then exit ;  // got a dotted address
    try
        num4 := StrToInt64Def (IPAddr, 0) ;  // March 2013 no exception
        if num4 = 0 then exit ;
    except ;
        exit ;
    end ;
    if num4 = 0 then exit ;
    result := '' ;
    for I := 4 downto 1 do
    begin
        result := result + IntToStr (num1 [I]) ;
        if I = 1 then exit ;
        result := result + '.' ;
    end ;
end ;

{$R+}
{$Q+}

constructor TBuffLogStream.Create (const NameMask, Header: string) ;
begin
    inherited Create;
    FNameMask := NameMask ;
    FHeader := Header + CRLF_ ;    // 7 Aug 2010
    FIdleTimer := TTimer.Create (Nil) ;
    FIdleTimer.OnTimer := Timer ;
    FBuffMax := 50000 ;
    FIdleSecs := 60 ;  // 18 Jan 2008 was minutes
    FLogSize := -1 ;
    FIdleTimer.Interval := Longword (FIdleSecs) * TicksPerSecond ;
    FFullName := FormatDateTime (FNameMask, Now) ;
    Clear ;
end ;

destructor TBuffLogStream.Destroy;
begin
    FlushFile ;
    if Assigned (FIdleTimer) then
    begin
        FIdleTimer.Enabled := false ;
        FIdleTimer.Destroy ;
    end ;
    inherited Destroy;
end;

procedure TBuffLogStream.Timer (Sender: TObject) ;
begin
    FIdleTimer.Enabled := false ;
    try
        FlushFile ;
    except
        // ignore error, buffer will grow if needed
    end ;
end ;

procedure TBuffLogStream.SetIdleSecs (IdleSecs: Integer) ;
begin
    FIdleSecs := IdleSecs ;
    FIdleTimer.Interval := Longword (FIdleSecs) * TicksPerSecond ;
    if FIdleTimer.Enabled then
    begin
        FIdleTimer.Enabled := false ;
        FIdleTimer.Enabled := true ;
    end ;
end ;

procedure TBuffLogStream.SetIdleMins (IdleMins: Integer) ;
begin
    SetIdleSecs (IdleMins * 60) ;
end ;

procedure TBuffLogStream.SetBuffMax (BuffMax: Integer) ;
begin
    FBuffMax := BuffMax ;
    Capacity :=  FBuffMax * 2 ;  // 17 Oct 2005, was + 10000
end ;

function TBuffLogStream.FlushFile: integer ;
var
    logline: string;
    loghandle, attempts: integer ;
    TickCount: longword;
begin
    result := 0 ;
    FIdleTimer.Enabled := false ;
    if Size = 0 then exit ;
    if FNameMask = '' then exit ;
    loghandle := -1 ;

    try // finally

    // create log file name
        FFullName := FormatDateTime (FNameMask, Now) ;

      // open log file
        for attempts := 1 to 20 do   // 1 second total  5 Apr 2005 retries
        begin
            if FileExists (FFullName) then
            begin
                loghandle := FileOpen (FFullName, fmOpenReadWrite OR fmShareDenyWrite) ;
                if loghandle >= 0 then
                begin
                    result := FileSeek (loghandle, 0, soFromEnd) ;  // end of file
                    if result < 0 then exit ;
                end ;
            end ;
            if (loghandle < 0) and (NOT (FileExists (FFullName))) then  // 5 Apr 2005 not if exists
            begin
                ForceDirs (ExtractFileDir (FFullName)) ;
//                loghandle := FileCreate (FFullName, fmOpenReadWrite OR fmShareDenyWrite) ;
                loghandle := FileCreate (FFullName) ;  // 14 Nov 2005, keep D5 happy
                if loghandle >= 0 then
                begin
                    if (Pos ('"', FHeader) > 0) and (Pos ('","', FHeader) = 0) then
                        logline := FormatDateTime (FHeader, Now)  // warning max 255 characters may be formatted
                    else
                        logline := FHeader ;
                    FileWrite (loghandle, PChar (logline)^, Length (logline)) ;
                end ;
            end ;
            if loghandle >= 0 then break ;
            TickCount := GetTickCount ;
            while ((GetTickCount - TickCount) < 50) do  // 50ms delay
        end ;
        if loghandle < 0 then
        begin
            result := -1 ;
         //   logline := Magsubs1.FormatLastError + space + fullname ;
            logline := SysErrorMessage (GetLastError) + ' [' + IntToCStr (GetLastError) +
                                                                         '] ' + fullname ;
            raise EFOpenError.CreateResFmt(@SFBuffOpenError, [logline]);
            exit ;
        end ;

    // write buffered text, clear buffer, reset timer
        Seek (0, soFromBeginning) ;
        result := FileWrite (loghandle, Pointer (Memory)^, Size) ;
        FLogSize := FileSeek (loghandle, 0, soFromEnd) ;  // end of file
        Clear ;
    finally
        if loghandle >= 0 then FileClose (loghandle) ;
    end ;
end ;

// return exception, unless error flushing, might work next time

function TBuffLogStream.WriteLine (const Line: string): Longint;
begin
    result := 0 ;
    if (Size + Length (Line) + 5 ) >= FBuffMax then
    begin  // 18 March 2008 clean up
        try
            result := FlushFile ;  // 6 Jan 2005
        except
            // ignore error, buffer will grow if needed
        end ;
    end ;
    Write (PChar (Line)^, Length (Line)) ;
    Write (CRLF^, 2) ;
    if Size > FBuffMax then
    begin
        try
            result := FlushFile ;  // 6 Jan 2005
        except
            // ignore error, buffer will grow if needed
        end ;
    end ;
    if NOT FIdleTimer.Enabled then FIdleTimer.Enabled := true ;
end ;

// check credit card number, OK if result blank (simple=true skips issuer and length)

function CreditCardCheck (ccnumber: string; simple: boolean): string ;
var
    cclen, fieldlen, index, digitval, digittot, numbtot: integer ;
    testdigit: integer ;
    zeroflag: boolean ;
// 3=Amex (15), 4=Visa (13 or 16), 5=Mastercard (16), 4/6=Switch (> 16)

begin
    Result := '' ;
    zeroflag := true ;
    fieldlen := length (CCNumber) ;
    if fieldlen = 0 then
    begin
        Result := 'Credit Card Number Must Be Entered' ;
        exit ;
    end ;

// test check count of all digits
    cclen := 0 ;
    numbtot := 0 ;
    testdigit := 1 ;
    for index := fieldlen downto 1 do
    begin
        digitval := ord (CCNumber [index]) ;
        if (digitval <> 32) and (digitval <> 45) then   // ignore spaces and hyphens
        begin
            if (digitval < 48) or (digitval > 57) then  // check if number
            begin
                Result := 'Credit Card Number Contains Invalid Character' ;
                beep ;
                exit ;
            end ;
            inc (cclen) ;
            if digitval <> 48 then zeroflag := false ;
            digittot := (digitval - 48) * testdigit ;
            numbtot := numbtot + (digittot div 10) ;
            numbtot := numbtot + (digittot mod 10) ;
            inc (testdigit) ;
            if testdigit > 2 then testdigit := 1 ;
        end ;
    end ;
    if ((numbtot mod 10) <> 0) or (cclen < 13) or zeroflag then
    begin
        Result := 'Credit Card Number Is Invalid' ;
        exit ;
    end ;

// check number length against type of card

    Result := '' ;
end;

Function LastBootDateTime : TDateTime;
Var
  C, F : Int64;
Begin
  QueryPerformanceCounter(C);
  QueryPerformanceFrequency(F);
  If (C <> 0) And (F <> 0) Then
    Result := Now - (C Div F) / SecsPerDay
  Else
    Result := Now - (GetTickCount / MSecsPerDay);
End;

function FileInUse(FileName: UnicodeString): Boolean;
var
    hFileRes: HFILE;
begin
    Result := False;
    if GetSizeFileW (FileName) < 0 then exit;  // unicode
    hFileRes := CreateFileW (PWideChar (FileName), GENERIC_READ or GENERIC_WRITE, 0,
                                   nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0) ;
    Result := (hFileRes = INVALID_HANDLE_VALUE);
    if NOT Result then CloseHandle(hFileRes);
end;

// test if can create and write file in specified path, creating path if needed

function TestPathAccess (FilePath: String; var ErrInfo: string): Boolean;
var
    hFileRes: HFILE;
    Buffer: array[0..1023] of AnsiChar;
    WideName: WideString ;  // Unicode
begin
    Result := False;
    ErrInfo := '' ;
    if (Length (FilePath) <= 2) or ((Pos ('\', FilePath) <= 0) and
                                     (Pos (':', FilePath) <= 0)) then  // 7 Aug 2010
    begin
        ErrInfo := 'No Path or Drive Specified' ;
        exit ;
    end ;
    if NOT DirectoryExists (FilePath) then
    begin
        if NOT ForceDirs (FilePath) then
        begin
            ErrInfo := '(path) ' + FormatLastError ;
            exit ;
        end ;
    end ;
    WideName := IntToStr (GetTickCount) ;
    WideName := strAddSlash (FilePath) + 'T' + Copy (WideName, 1, 7) + '.tmp' ;
    hFileRes := CreateFileW (PWideChar (WideName), GENERIC_READ or GENERIC_WRITE, 0,
             nil, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL OR FILE_FLAG_DELETE_ON_CLOSE, 0) ;
    if (hFileRes = INVALID_HANDLE_VALUE) then
    begin
        ErrInfo := FormatLastError ;
        exit ;
    end ;
    if FileWrite (hFileRes, Buffer, 200) = 200 then
        result := true
    else
        ErrInfo := '(write) ' + FormatLastError ;
    FileClose (hFileRes);
    DeleteFile (WideName) ;   // should have been deleted when closed, but in case...
end;

function CheckUniqueInstance: boolean ;
var
    AppString: WideString ;  // Unicode
    I: integer ;
begin
    AppString := Uppercase (ExtractFileName (ParamStr(0))) ;
    if AppString = '' then AppString := 'UNKNOWN' ;
    AppString := 'APPLICATION-' + AppString ;
    for I := 1 to Length (AppString) do
    begin
        if AppString [I] = '\' then AppString [I] := '_';
    end;

// Check to see if the mutex is already there
    InstanceMutexHandle := Windows.OpenMutexW (MUTEX_ALL_ACCESS, false,
                                                             PWideChar (AppString)) ;

// no handle, this is the first instance, create new mutex
    if InstanceMutexHandle = 0 then
    begin
        InstanceMutexHandle := Windows.CreateMutexW (nil, false, PWideChar (AppString)) ;
    // Error checking to see if anyone beat us...
        if InstanceMutexHandle = 0 then
            result := false
        else
            result := true;
    end
    else
        result := false;
end;

function GetIOErrorStr (code: integer): string ;
begin
    result := '' ;
    if code = 0   then result := 'No error code (0)' ;
    if code = 100 then result := 'Disk read error' ;   // Reported by Read on a typed file if you attempt to read past the end of the file.
    if code = 101 then result := 'Disk write error' ;  // Reported by CloseFile, Write, WriteIn, or Flush if the disk becomes full.
    if code = 102 then result := 'File not assigned' ; // Reported by Reset, Rewrite, Append, Rename, or Erase if the file variable has not been assigned a name through a call to Assign or AssignFile.
    if code = 103 then result := 'File not open' ;     // Reported by CloseFile, Read Write, Seek, Eof, FilePos, FileSize, Flush, BlockRead, or BlockWrite if the file is not open.
    if code = 104 then result := 'File not open for input' ;  // Reported by Read, Readln, Eof, Eoln, SeekEof, or SeekEoln on a text file if the file is not open for input.
    if code = 105 then result := 'File not open for output' ; // Reported by Write or Writeln on a text file if you do not generate a Console application.
    if code = 106 then result := 'Invalid numeric format' ;   // Reported by Read or Readln if a numeric value read
    if code = 29  then result := 'File write fault' ;
    if code = 30  then result := 'File read fault' ;
    if code = 31  then result := 'File general failure' ;
    if code = 32  then result := 'File sharing violation' ;
    if code = 33  then result := 'File lock violation' ;
    if result = '' then result := SysErrorMessage (code) ;
end ;

// simple log file, writes Msg to text file in progam directory with FNameMask
// unless mask includes // or x:
// mask generally includes date only - "mylog-"yyyymmdd".log"
// path needed quote at start - "c:\temp\mylog-"yyyymmdd".log"

procedure SimpleLogging (const FNameMask, Msg: String);
var
    LogFileName, S: String;
    f: TextFile;
begin
    S := FormatDateTime (ISOTimeMask, Time) + ' ' + Msg;
    try
        LogFileName := FormatDateTime (FNameMask, Date) ;
        if (Pos ('//', LogFileName) <> 1) and (Pos (':', LogFileName) <> 2) then
            LogFileName := strAddSlash (ExtractFileDir (ParamStr (0))) + LogFileName ;
        AssignFile (f, LogFileName);
        if FileExists (LogFileName) then
        begin
            Append (f);
        end
        else
        begin
            Rewrite (f);
        end;
        Writeln (f, S);
        CloseFile(f);
    except
    end;
end;

// convert angus@magsys.co.uk (Angus Robertson) to "Angus Robertson" <angus@magsys.co.uk>
// convert angus@magsys.co.uk to <angus@magsys.co.uk>
// convert to Angus Robertson <angus@magsys.co.uk> to "Angus Robertson" <angus@magsys.co.uk>

function CleanEmailAddr (oldaddr: string): string ;
var
    I, J, K, L: integer ;
begin
    result := oldaddr ;
    I := Pos ('(', oldaddr) ;
    J := Pos ('<', oldaddr) ;
    K := Pos ('@', oldaddr) ;
    L := Pos (')', oldaddr) ;
    if (J = 0) and (I > K) then
    begin
        if L > 0 then Delete (oldaddr, L, 1) ;
        result := DQUOTE + trim (copy (oldaddr, Succ (I), 99)) + DQUOTE +
                         ' <' + trim (copy (oldaddr, 1, Pred (I))) + '>' ;
    end
    else if J = 0 then
        result := '<' + trim (oldaddr) + '>'
    else if oldaddr [1] <> DQUOTE then
    begin
        if J > 2 then result := DQUOTE + trim (copy (oldaddr, 1, Pred (J))) +
                                DQUOTE + SPACE + copy (oldaddr, J, 999) ;
    end ;
end ;

const
  PIXELCOUNTMAX = 32768;

type
  pRGBArray = ^TRGBArray;
  TRGBArray = array[0..PIXELCOUNTMAX - 1] of TRGBTriple;

procedure AntiAliasBitmap(K:byte; bitmap: TBitmap);
var
  x, y, cx, cy, cxi: Integer;
  totr, totg, totb: Integer;
  Row1, Row2, Row3, Row4, DestRow: PRGBArray;
  i: Integer;
  kk:byte;
  FAABitmap:TBitmap;
begin
    kk:=k;
    FAABitmap:=TBitmap.Create;
    FAABitmap.Width:=bitmap.Width;
    FAABitmap.Height:=bitmap.Height;
    FAABitmap.PixelFormat := pf24bit;
    bitmap.PixelFormat := pf24bit;
    // For each row
    Row2 := nil; Row3 := nil; Row4 := nil;
    for Y := 0 to bitmap.Height-1 do begin
    // We compute samples of MAX K x K pixels
    cy := y;
    K:=min(kk,bitmap.Height-Y); //I don't want go outside of bitmap
    // Get pointers to actual, previous and next rows in supersampled bitmap
    Row1 := Bitmap.ScanLine[cy];
    if K > 1 then Row2 := Bitmap.ScanLine[cy+1];
    if K > 2 then Row3 := Bitmap.ScanLine[cy+2];
    if K > 3 then Row4 := Bitmap.ScanLine[cy+3];
    // Get a pointer to destination row in output bitmap
    DestRow := FAABitmap.ScanLine[cy];
    // For each column...
    for x := 0 to FAABitmap.Width - 1 do begin
      // We compute samples of 3 x 3 pixels
      cx := x;
      // Initialize result color
      totr := 0; totg := 0; totb := 0;
      if K > 3 then begin
        for i := 0 to 3 do begin
          cxi:=min(cx+i,bitmap.width-1);  //I don't want go outside of bitmap
          totr := totr + Row1[cxi].rgbtRed + Row2[cxi].rgbtRed + Row3[cxi].rgbtRed + Row4[cxi].rgbtRed;
          totg := totg + Row1[cxi].rgbtGreen + Row2[cxi].rgbtGreen + Row3[cxi].rgbtGreen + Row4[cxi].rgbtGreen;
          totb := totb + Row1[cxi].rgbtBlue + Row2[cxi].rgbtBlue + Row3[cxi].rgbtBlue + Row4[cxi].rgbtBlue;
        end;
        DestRow[x].rgbtRed := totr div 16;
        DestRow[x].rgbtGreen := totg div 16;
        DestRow[x].rgbtBlue := totb div 16;
      end else if K > 2 then begin
        for i := 0 to 2 do begin
          cxi:=min(cx+i,bitmap.width-1);  //I don't want go outside of bitmap
          totr := totr + Row1[cxi].rgbtRed + Row2[cxi].rgbtRed + Row3[cxi].rgbtRed;
          totg := totg + Row1[cxi].rgbtGreen + Row2[cxi].rgbtGreen + Row3[cxi].rgbtGreen;
          totb := totb + Row1[cxi].rgbtBlue + Row2[cxi].rgbtBlue + Row3[cxi].rgbtBlue;
        end;
        DestRow[x].rgbtRed := totr div 9;
        DestRow[x].rgbtGreen := totg div 9;
        DestRow[x].rgbtBlue := totb div 9;
      end else if K > 1 then begin
        for i := 0 to 1 do begin
          cxi:=min(cx+i,bitmap.width-1);  //I don't want go outside of bitmap
          totr := totr + Row1[cxi].rgbtRed + Row2[cxi].rgbtRed;
          totg := totg + Row1[cxi].rgbtGreen + Row2[cxi].rgbtGreen;
          totb := totb + Row1[cxi].rgbtBlue + Row2[cxi].rgbtBlue;
        end;
        DestRow[x].rgbtRed := totr div 4;
        DestRow[x].rgbtGreen := totg div 4;
        DestRow[x].rgbtBlue := totb div 4;
      end else begin
        DestRow[x].rgbtRed   := Row1[cx].rgbtRed;
        DestRow[x].rgbtGreen := Row1[cx].rgbtGreen;
        DestRow[x].rgbtBlue  := Row1[cx].rgbtBlue;
      end;
    end;
  end;
  Bitmap.Assign(FAABitmap);
  FAABitmap.Free;
end;

function IsValidEmail(const Value: AnsiString): Boolean;
var
    i: Integer;
    NamePart, ServerPart: AnsiString;

    function CheckAllowed(const s: AnsiString): Boolean;
    var i: Integer;
    begin
        Result:= false;
        for i:= 1 to Length(s) do
          if not (s[i] in ['a'..'z', 'A'..'Z', '0'..'9', '_', '-', '.']) then Exit;
        Result:= true;
    end;

begin
    Result:= False;
    i:=Pos('@', string (Value));  // 7 Aug 2010
    if i=0 then Exit;
    NamePart:=Copy(Value, 1, i-1);
    ServerPart:=Copy(Value, i+1, Length(Value));
    if (Length(NamePart)=0) or ((Length(ServerPart)<5)) then Exit;
    i:=Pos('.', string (ServerPart));  // 7 Aug 2010
    if (i=0) or (i>(Length(serverPart)-2)) then Exit;
    Result:= CheckAllowed(NamePart) and CheckAllowed(ServerPart);
end;

{ moved to MagSerPorts.pas
function EnumSerialPorts (var SerialPorts: TSerialPorts): integer;     // 7 Dec 2007
var
    I, J, PortLen: Integer;
    SortStr, PortName: string;
    SortList, ValueList: TStringList;
    aRegistry : TRegistry;
begin
    result := 0 ;
    aRegistry:=TRegistry.Create;
    ValueList := TStringList.Create;
    SortList := TStringList.Create;
    try
    // 16 Feb 2014, total rewrite using registry functions since old version was
    // having trouble reading registry keys reliably causing some corrupted names
        try
            with aRegistry do
            begin
                RootKey := HKEY_LOCAL_MACHINE;
                Access := KEY_QUERY_VALUE ;
                OpenKey ('HARDWARE\DEVICEMAP\SERIALCOMM', False);
                GetValueNames (ValueList);
                if ValueList.Count = 0 then exit ;
                for I := 0 to ValueList.Count - 1 do
                begin
                    if ValueExists (ValueList [I]) then   // avoid exception if no value
                    begin
                        PortName := ReadString (ValueList [I]);
                        PortLen := Length (PortName) ;
                        if Pos ('COM', PortName) = 1 then
                        begin
                            SortStr := Copy (PortName, 4, 9) ;
                            if PortLen = 4 then
                                SortStr := '00' + SortStr
                            else if PortLen = 5 then
                                SortStr := '0' + SortStr ;
                        end
                        else
                            SortStr := 'xxx' ;
                        SortStr := SortStr + PortName + '|' ;
                        if Pos ('\Device\', ValueList [I]) = 1 then     // \Device\OXMF0, \Device\BthModem0
                            SortStr := SortStr + Copy (ValueList [I], 9, 99)
                        else
                            SortStr := SortStr + ValueList [I] ;       // Winachsf0
                        SortList.Add (SortStr) ;
                      end;
                end;
                CloseKey ;
            end;
            result := SortList.Count ;
            if result = 0 then exit ;
            SortList.Sort ;
            SetLength (SerialPorts, result) ;
            for I := 0 to (Pred (result)) do
            begin
                SortStr := SortList [I] ;
                J := Pos ('|', SortStr) ;
                serialPorts [I].ComName := Copy (SortStr, 4, J - 4) ;
                serialPorts [I].NumPort := Copy (SortStr, 1, 3) ;
                SerialPorts [I].IntName := Copy (SortStr, Succ (J), 99) ;
                SerialPorts [I].Desc := SerialPorts [I].IntName ;  // may get better name later
                SerialPorts [I].Enabled := true ;
            end;
        except
        end ;
    finally
        SortList.Free;
        ValueList.Free;
        aRegistry.Free;
  end;

end;     }

// 1 Feb 2008 - converts angus@magsys.co.uk to uk.co.magsys@angus for sorting

function ReverseEmail (const AString: AnsiString): AnsiString ;  // took 1.1 secs for 35K emails
var
    Ch: AnsiChar;
    I, J, L, M: Integer;
    Source, OldSrc, Dest: PAnsiChar;
begin
    L := Length (AString) ;
    result := AString ;
    if L <= 0 then exit ;
    UniqueString (Result) ;
    Source := Pointer (AString) ;
    Source := Source + L ; // start at end of string
    Dest := Pointer (Result) ;
    M := L ;
    while M >= 0 do
    begin
        Ch := Source^ ;
        if (Ch = '.') or (Ch = '@') or (M <= 0) then // node separators
        begin
            OldSrc := Source ;
            J := L - M ;
            L := M ;
            if M > 0 then  //  don't copy node separator
            begin
                inc (Source) ;
                dec (J) ;
            end ;
            for I := 1 to J do  // copy node to output string
            begin
                Dest^ := Source^ ;
                inc (Dest) ;
                inc (Source) ;
            end ;
            if M <= 0 then exit ;
            Dest^ := Ch ;      // add node separator
            inc (Dest) ;
            Source := OldSrc ;
        end ;
        Dec(Source);
        Dec(M);
    end;
end ;

{
function ReverseEmail(Email : String) : String;    // took 1.8 secs for 35K
var
  P : Integer;
begin
  P := Pos('@', Email);
  Result := '@' + Copy(Email, 1, P - 1);
  Delete(Email, 1, P);

  Repeat
    P := Pos('.', Email);
    If P > 0 Then Begin
      Result := '.' + Copy(Email, 1, P - 1) + Result;
      Delete(Email, 1, P);
    End
    Else Begin
      Result := Email + Result;
    End;
  Until P = 0;
end;  }

{
function ReverseEmail(Const AString : String) : String;  // took 4.2 secs for 35K emails
var
  BeforeAt : Boolean;
  I : Integer;
  NamePart : String;
begin
  BeforeAt := TRUE;
  Result := '';
  For I := 1 To Length(AString) Do Begin
    If AString[I] = '@' Then Begin
      BeforeAt := FALSE;
      Result := '@' + Result;
    End
    Else If BeforeAt Then Begin
      Result := Result + AString[I];
    End
    Else If AString[I] = '.' Then Begin
      Result := '.' + NamePart + Result;
      NamePart := '';
    End
    Else Begin
      NamePart := NamePart + AString[I];
    End;
  End;
  Result := NamePart + Result;
end;   }

procedure EnableClearType(AControlFont: TFont; Enable: boolean) ;
const
    DEFAULT_QUALITY = 0 ;
    DRAFT_QUALITY = 1 ;
    PROOF_QUALITY = 2 ;
    NONANTIALIASED_QUALITY = 3 ;
    ANTIALIASED_QUALITY = 4 ;
    CLEARTYPE_QUALITY = 5 ;
    CLEARTYPE_NATURAL_QUALITY = 6 ;
var
    lf: TLogFont;
begin
    if GetObject(AControlFont.Handle, SizeOf(TLogFont), @lf) <> 0 then
    begin
        if Enable then
            lf.lfQuality := CLEARTYPE_QUALITY
        else
            lf.lfQuality := NONANTIALIASED_QUALITY ;
        AControlFont.Handle := CreateFontIndirect(lf);
    end;
end;

// does a string contain any common HTML tags, used for email body validation to stop spammers using HTML

function IsHtmlTags (const S: string): boolean ;
var
    S2: string ;
begin
    result := false ;
    S2 := Lowercase (S) ;
    if Pos ('<a', S2) > 0 then result := true
    else if Pos ('</a', S2) > 0 then result := true
    else if Pos ('href', S2) > 0 then result := true
    else if Pos ('<img', S2) > 0 then result := true
    else if Pos ('[/url]', S2) > 0 then result := true ;
end;

// swap 32-bit longword endian, useful for IP addresses so they sort

function EndianLong (L: LongWord): LongWord;
begin
  result := Swap (L shr 16) or (LongWord (Swap (L and $ffff)) shl 16);
end;

// bitwise handlers

function IsBitSet(const val: longint; const TheBit: byte): boolean;
begin
    result := (val and (1 shl TheBit)) <> 0;
end;

function BitOn(const val: longint; const TheBit: byte): LongInt;
begin
    result := val or (1 shl TheBit);
end;

function BitOff(const val: longint; const TheBit: byte): LongInt;
begin
    result := val and ((1 shl TheBit) xor $FFFFFFFF);
end;

function BitToggle(const val: longint; const TheBit: byte): LongInt;
begin
    result := val xor (1 shl TheBit);
end;

// swap 64-bit endian

procedure EndianSwap64(var Data); register;
asm
   mov edx, [eax]
   bswap edx
   mov ecx, [eax+4]
   bswap ecx
   mov [eax+4], edx
   mov [eax], ecx
end;

function GetProcessHandles (ProcId: DWORD): integer ;  // 21 March 2011
var
    HandleCount: DWORD ;
begin
    result := 0 ;
    if MagRasOSVersion < OSWXP then exit ;
    if NOT Assigned (GetProcessHandleCount) then
    begin
        GetProcessHandleCount := GetProcAddress (GetModuleHandle(kernel32), 'GetProcessHandleCount') ;
        if NOT Assigned (GetProcessHandleCount) then exit ;
    end;
    HandleCount := 0 ;
    GetProcessHandleCount (GetCurrentProcess, HandleCount) ;  // XP SP1 and later
    result := HandleCount ;
end;

// enable or disable controls on a tab, box or something - July 2014

procedure EnableOrDisableChildren (Container: TWinControl; Enabled: Boolean) ;
var
    index: integer;
    aControl: TControl;
    isContainer: boolean;
begin
    for index := 0 to -1 + Container.ControlCount do
    begin
        aControl := Container.Controls [index] ;
        isContainer := (csAcceptsControls in aControl.ControlStyle) ;
        if NOT isContainer then
            aControl.Enabled := Enabled;

     //recursive for child controls
       if (isContainer) AND (aControl is TWinControl) then
        begin
            EnableOrDisableChildren (TWinControl (aControl), Enabled) ;
       end;
    end;
end;


// must be after any procedures

Initialization
    if InstanceMutexHandle <> 0 then
    begin
        CloseHandle (InstanceMutexHandle) ;
        InstanceMutexHandle := 0 ;
    end ;
    ProcHandleModule := 0 ;
finalization
    if ProcHandleModule <> 0 then
    begin
        FreeLibrary (ProcHandleModule) ;
        ProcHandleModule := 0 ;
    end ;
end.


