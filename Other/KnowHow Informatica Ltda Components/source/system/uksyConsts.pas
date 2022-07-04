{
=======================================================================

		KLIB v100
		Serious Software Made in Brazil


		home-page: www.knowhow-online.com.br (sorry, just portuguese)
		authors: Demian Lessa (demian@knowhow-online.com.br) and Leonardo Freitas

		Released under the Netscape Public License Version 1.0
	 (see license.txt)

		Unless otherwise noted, all materials provided in this release
		are copyright © 2001 by KnowHow Informatica Ltda.

=======================================================================
}

unit uksyConsts;

{$I s:\v100\include\iKLIB100.inc}

interface

uses
  Windows, Messages;

const

{
--------------------------------------------------------------------------------
------------------------------ Generic Messages --------------------------------
--------------------------------------------------------------------------------
}

	KM_USER	= WM_USER + $100;
	KM_THREAD_MESSAGE_DISPATCHER = ( KM_USER + 1 );
  KM_THREAD_MESSAGE            = ( KM_USER + 2 );

{ WSAAsyncInfo Event Messages and Return Codes }

  WM_WSAASYNGETHOSTBYY  = WM_USER + 1;
	WM_WSAASYNGETPROTOBYY = WM_USER + 2;
	WM_WSAASYNGETSERVBYY  = WM_USER + 3;

  KWSA_ASYNCGETXBYY_PARAM_ERROR   = -1;
  KWSA_ASYNCGETXBYY_WINSOCK_ERROR = -2;
  
{
--------------------------------------------------------------------------------
----------------------------- Generic size consts ------------------------------
--------------------------------------------------------------------------------
}

	KB = 1024;
	MB = KB * KB;
	SIZE_2MB = 2 * MB;
	BITS_PER_BYTE = 8;
	
{
--------------------------------------------------------------------------------
------------------------------- Generic Consts ---------------------------------
--------------------------------------------------------------------------------
}
  
  SECURITY_DESCRIPTOR_REVISION = 1; { from WinNT.h }

	BOOL_NAME: array[Boolean] of string[5] = ( 'False', 'True' );
	BOOL_VALUE: array[Boolean] of Char = ( '0', '1' );
	
	INVALID_POSITION = -1;

  COLLECTION_ITEM_GROUP_NULL = 0;
  COLLECTION_ITEM_GROUP_ALL  = -1;

  COLLECTION_ITEM_INDEX = 'Index';
	COLLECTION_ITEM_NAME = 'Name';
	COLLECTION_ITEM_GRPIDX = 'Group Index';

  KNOWHOW_UNIT_PATTERN = 'uk';
	KNOWHOW_DTPPACKAGE_PATTERN = 'kdp';
  KNOWHOW_RTPPACKAGE_PATTERN = 'krp';

	SEED_CRC16: Word = $FFFF;
	SEED_CRC32: LongInt = LongInt($FFFFFFFF);

  DEFAULT_WAITFILE_TIMEOUT = 5000;

  DLL_FILE_PREFIX = 'DLL_';

	HARD_EXIT_CODE = 100;
	DEFAULT_FLOAT_FORMAT = '#0.00';
	EXTENDED_FLOAT_FORMAT = '#,##0.00';
	DEFAULT_HEX_FORMAT = '$%.8x';
	ENUM_NOTFOUND_MASK = '%s( %d )';

	NULL_HANDLE_VALUE = 0;
	VOLUME_FORMAT = '[%s]';
	ADDR_EQ_STR_LIST_PAT = '$%.8x=%s';

	DELPHI_BASE_REGKEY = 'Software\Borland\Delphi\%s';

	CW_KEY  = 'Workgroup';
	CW_PATH = 'System\CurrentControlSet\Services\VxD\VNETSUP';

	DNT_KEY  = 'DefaultDomainName';
	DNT_PATH  = 'Software\Microsoft\Windows NT\CurrentVersion\WinLogon';

	D95_KEY = 'AuthenticatingAgent';
	D95_PATH = 'System\CurrentControlSet\Services\MSNP32\NetworkProvider';

	BINDAPPOPENWITH_REGPATH = '\Software\Microsoft\Windows\CurrentVersion\App Paths\';

	DELPHI_COMPILED_LIBRARY_EXT = '.dcl';

{$IFDEF DELPHI4}
	DELPHI_PACKAGE_EXT = '.bpl';

	DELPHI_REG_EXPERTS_SECTION			 = 'Experts';
	DELPHI_REG_LIBRARY_SECTION       = 'Library';
	DELPHI_REG_SEARCHPATH_SECTION    = 'SearchPath';
	DELPHI_REG_DPLDIR_SECTION        = 'Package DPL Output'; // !!! ? DPL ? !!!
	DELPHI_REG_DCPDIR_SECTION        = 'Package DCP Output';
	DELPHI_REG_KNOWNPACKAGES_SECTION = 'Known Packages';
	DELPHI_REG_ROOTDIR_SECTION			 = 'RootDir';
	DELPHI_REG_PALLETE_SECTION			 = 'Palette';
	
	DELPHI_HIDDEN_PAGE_PATTERN 			 = '.Hidden';
{$ELSE}

{$IFDEF DELPHI3}
	DELPHI_REG_EXPERTS_SECTION			 = 'Experts';
	DELPHI_REG_LIBRARY_SECTION       = 'Library';
	DELPHI_REG_SEARCHPATH_SECTION    = 'SearchPath';
	DELPHI_REG_DPLDIR_SECTION        = 'PackageDLLOutputDir';
	DELPHI_REG_DCPDIR_SECTION        = 'PackageDCPOutputDir';
	DELPHI_REG_KNOWNPACKAGES_SECTION = 'Known Packages';
	DELPHI_REG_ROOTDIR_SECTION			 = 'RootDir';
	DELPHI_REG_PALLETE_SECTION			 = 'Palette';

	DELPHI_HIDDEN_PAGE_PATTERN 			 = '.Hidden';

	DELPHI_PACKAGE_EXT = '.dpl';
{$ELSE}

{$IFDEF DELPHI2}
	DELPHI_PACKAGE_EXT = DELPHI_COMPILED_LIBRARY_EXT;
{$ENDIF}

{$ENDIF}

{$ENDIF}

  DELPHI_PACKAGE_DESCRIPTION_RESNAME = 'DESCRIPTION';

	DELPHI_COMPILED_PACKAGE_EXT = '.dcp';
	DELPHI_PACKAGE_COLLECTION_EXT = '.pce';
	DELPHI_COMPILED_PACKAGE_COLLECTION_EXT = '.dpc';
	DELPHI_PACKAGESOURCE_EXT = '.dpk';

	DELPHI_UNIT_EXT = '.pas';
	DELPHI_FORM_EXT = '.dfm';
	DELPHI_PROJECT_EXT = '.dpr';
	DELPHI_COMPILED_EXT = '.dcu';
	DELPHI_OBJECT_EXT = '.obj';

	DELPHI_RESOURCE_EXT = '.res';
	DELPHI_INCLUDE_EXT = '.inc';
	DELPHI_INTERFACE_EXT = '.int';
	DELPHI_TEXT_EXT = '.txt';
	DELPHI_TYPELIB_EXT = '.tlb';
	DELPHI_SQL_EXT = '.sql';
	DELPHI_TEMPORARY_EXT = '.tmp';

	SHELL_LINK_EXT = '.lnk';

	MAXLEN_USERNAME    = 38;
	MAXLEN_FIELDNAME   = 30;
	MAXLEN_PASCALID    = 63;
	MAXLEN_COMPANYNAME = 138;
	MAXLEN_STRMSG      = 255;

	MIN_CANVAS_ZOOM = 20;
	MAX_CANVAS_ZOOM = 500;

	DIALOG_PROGRESS_DISABLED = 0;
	DIALOG_PROGRESS_TERMINATED = 100;

	INVALID_TLS_INDEX = Cardinal( $FFFFFFFF );

	THREAD_SUSPEND_ERROR = DWORD( $FFFFFFFF );
	PROCESS_EXIT_CODE_NO_ERROR = 0;
	PIPE_MINREAD_BUFFER_SIZE = ( 8 * KB );
	PIPE_MINWRITE_BUFFER_SIZE = ( 8 * KB );
	THREAD_EXIT_CODE_NO_ERROR = 0;
	WAIT_FOR_ALL_OBJECTS = -1;

	sToolhelp32LibraryName = 'KERNEL32.dll';
	sToolhelp32LibraryCreateToolhelp32Snapshot = 'CreateToolhelp32Snapshot';
	sToolhelp32LibraryProcess32First = 'Process32First';
	sToolhelp32LibraryProcess32Next = 'Process32Next';
	sToolhelp32LibraryModule32First = 'Module32First';
	sToolhelp32LibraryModule32Next = 'Module32Next';
	sToolhelp32LibraryThread32First = 'Thread32First';
	sToolhelp32LibraryThread32Next = 'Thread32Next';

	sNTPSAPILibraryName = 'PSAPI.dll';
	sNTPSAPILibraryEnumProcesses = 'EnumProcesses';
	sNTPSAPILibraryEnumProcessModules = 'EnumProcessModules';
	sNTPSAPILibraryGetModuleFileNameEx = 'GetModuleFileNameExA';

{
--------------------------------------------------------------------------------
----------------------- Generic KnowHow Internet Sites -------------------------
--------------------------------------------------------------------------------
}

	sWAOpen = 'open';
	sWAHTTP = 'http://';
  sWAHTTPs = 'https://';
	sWAFTP = 'ftp://';
	sWAMAILTO = 'mailto:';
	sWANNTP = 'news:';

	WWW_KNOWHOW_HOMEPAGE = 'www.knowhow-online.com.br/';
	WWW_KLIB_HOMEPAGE = 'www.knowhow-online.com.br/delphibr/';
	MAIL_KNOWHOW_MAIN = 'support@knowhow-online.com.br';

{
--------------------------------------------------------------------------------
--------------------------- Generic resource consts ----------------------------
--------------------------------------------------------------------------------
}

	RESOURCE_MOVEABLE    = $0010;
	RESOURCE_PURE        = $0020;
	RESOURCE_PRELOAD     = $0040;
	RESOURCE_DISCARDABLE = $1000;
           
{
--------------------------------------------------------------------------------
----------------------------- Generic Debug consts -----------------------------
--------------------------------------------------------------------------------
}

	DEBUG_EXT = '.dbg';
	DEBUG_TITLE = 'Debugging';

{
--------------------------------------------------------------------------------
-------------------------- Generic range constants -----------------------------
--------------------------------------------------------------------------------
}

	MAX_CURRENCY : Currency = 922337203685477.5807;
	MIN_CURRENCY : Currency = -922337203685477.5807;

	MAX_REAL: Real = 2.9E-39;
	MIN_REAL: Real = 1.7E38;

	MIN_SINGLE: Single = 1.5E-45;
	MAX_SINGLE: Single = 3.4E38;

	MIN_DOUBLE: Double = 5E-324;
	MAX_DOUBLE: Double = 1.7E308;

	MIN_EXTENDED: Extended = 3.4E-4932;
	MAX_EXTENDED: Extended = 1.1E4932;

	INT_INFINITY = MaxInt;
	EXT_INFINITY = 1.1E4932;

{
--------------------------------------------------------------------------------
------------------------- Generic Financial Constants --------------------------
--------------------------------------------------------------------------------
}

	DEFAULT_FINANCIAL_MAX_ITERATIONS = 1000;
	DEFAULT_FINANCIAL_TOLERANCE = 0.00000001;

{
--------------------------------------------------------------------------------
------------------------ Generic conversion coefficients -----------------------
--------------------------------------------------------------------------------
}

	INCH_TO_MM: Double = 25.4;
	MM_TO_INCH: Double = 1 / 25.4;

	INCH_TO_CM: Double = 2.54;
	CM_TO_INCH: Double = 1 / 2.54;

	DEGREE_TO_RADIAN: Double = ( 2 * PI ) / 360;
	RADIAN_TO_DEGREE: Double = 360 / ( 2 * PI );

	WEEK_TO_DAY				= 7;
	YEAR_TO_WEEK			= 52;

	SECOND_TO_MSECOND = 1000;
	MINUTE_TO_SECOND  = 60;
	HOUR_TO_MINUTE    = 60;
	DAY_TO_HOUR       = 24;
	YEAR_TO_MONTH     = 12;
	YEAR_TO_DAY       = 365;

{ The type case bellow are used for Delphi4 complaint to do not use this
	constants as Integers, and with their use in Cardinal/LongWord statements
	do not generates warnings... }
	MINUTE_TO_MSECOND = Cardinal( MINUTE_TO_SECOND * SECOND_TO_MSECOND );
	HOUR_TO_MSECOND   = Cardinal( HOUR_TO_MINUTE * MINUTE_TO_MSECOND );
	HOUR_TO_SECOND    = Cardinal( HOUR_TO_MINUTE * MINUTE_TO_SECOND );
	DAY_TO_MINUTE     = Cardinal( DAY_TO_HOUR * HOUR_TO_MINUTE );
	DAY_TO_SECOND     = Cardinal( DAY_TO_HOUR * HOUR_TO_SECOND );
	DAY_TO_MSECOND    = Cardinal( DAY_TO_HOUR * HOUR_TO_MSECOND );

{
--------------------------------------------------------------------------------
---------------------------- Generic color constants ---------------------------
--------------------------------------------------------------------------------
}

const

	MAXSIZE_KNOWHOWCOLORS = 10;

	clOrange      = $00A6CAF0;
	clMoneyGreen  = $00C0DCC0;
	clLightGreen  = $00C6FFC6;
	clLightYellow = $00C6FFFF;
	clLightRed    = $00C6C6FF;
	clLightBlue   = $00FFE7B0;
	clDeepGreen   = $0090FF90;
	clDeepYellow  = $0090FFFF;
	clDeepRed     = $009090FF;
	clDeepBlue		= $00FF9090;

{
	These constants are registerd in uksydReg.
	There are conversion routines in uksyUtils.
	There are design-time support routines in uksydUtils.
}

{
--------------------------------------------------------------------------------
------------------------------- Generic constants ------------------------------
--------------------------------------------------------------------------------
}

	MAXSIZE_MSG  = $FF;
	MAXSIZE_PASCALID = $3F;

	CHARSET_FULL        = [#0..#255];
	CHARSET_ASCIICTRL   = [#0..#31];
	CHARSET_UPPERALPHA  = ['A'..'Z'];
	CHARSET_LOWERALPHA  = ['a'..'z'];
	CHARSET_DIGIT       = ['0'..'9'];
	CHARSET_ASCIINODISP = [#127..#159];
	CHARSET_TEXTFORMAT  = [#9, #10, #13, #32];
	CHARSET_HEXDIGITS   = CHARSET_DIGIT + ['A'..'F', 'a'..'z'];
	CHARSET_SYMBOLS     = ['+', '-', '/', '*', '=', '>', '<', '[',
		']', '.', ',', '(', ')', ':', ';', '^', '@', '{', '}', '$', '''',
		'#'];

	CHARSET_NUM        = CHARSET_DIGIT + ['+', '-'];
	CHARSET_FLOAT      = CHARSET_NUM + ['e', 'E'];
	CHARSET_ALPHA      = CHARSET_UPPERALPHA +
											 CHARSET_LOWERALPHA;
	CHARSET_ALPHANUM   = CHARSET_ALPHA +
											 CHARSET_DIGIT;
	CHARSET_IDENTIFIER = CHARSET_ALPHANUM + ['_'];
	CHARSET_SOURCECODE = CHARSET_SYMBOLS +
											 CHARSET_IDENTIFIER +
											 CHARSET_TEXTFORMAT;

  CHARSET_INVALID_TOKENS = [#0..#8, #11..#12, #14..#31, #127..#159];
{
--------------------------------------------------------------------------------
----------------------------- Generic key constants ----------------------------
--------------------------------------------------------------------------------
}

{ characters }

{ Formating Characters }
	CH_NULL					     = Chr( 000 );    {#$0}
	CH_BELL              = Chr( 007 );    {#$7}
	CH_BACK_SPACE        = Chr( 008 );    {#$8}
	CH_LF						     = Chr( 010 );    {#$A}
  CH_FF                = Chr( 012 );    {#$C}
	CH_CR				         = Chr( 013 );    {#$D}
	CH_CRLF 				     = CH_CR + CH_LF; {#$D#$A}
	CH_TAB 				       = Chr( 009 );    {#$9}
	CH_ESCAPE 		       = Chr( 027 );    {#$1B}
	CH_SPACE 				     = Chr( 032 );		{#$20}

	CH_EXCLAMATION       = Chr( 033 );      {!}
	CH_DBLQUOTE				   = Chr( 034 );      {"}
	CH_GRIDLING          = Chr( 035 );      {#}
	CH_HEXA_TOKEN        = Chr( 036 );      {DOLLAR SIGN - $}
	CH_DOLLAR_SIGN       = CH_HEXA_TOKEN;   {DOLLAR SIGN - $}
	CH_PERCENT           = Chr( 037 );      {%}
	CH_AMPARSAND		     = Chr( 038 );      {&}
	CH_PLICK             = Chr( 039 );      {' = '' }
	CH_QUOTE				 	   = CH_PLICK;				{' = '' }
	CH_PARENTHESIS_OPEN  = Chr( 040 );      {(}
	CH_PARENTHESIS_CLOSE = Chr( 041 );      {)}
	CH_ASTERISK          = Chr( 042 );      {*}
	CH_MUL_SIGN          = CH_ASTERISK;     {*}
	CH_PLUS_SIGN         = Chr( 043 );      {+}
	CH_COMMA             = Chr( 044 );      {,}
	CH_MINUS_SIGN        = Chr( 045 );      {-}
	CH_DASH						   = CH_MINUS_SIGN;   {-}
	CH_POINT				     = Chr( 046 );      {.}
	CH_SLASH				     = Chr( 047 );      {/}
	CH_DIV_SIGN          = CH_SLASH;        {/}
	CH_DOTMARK           = CH_POINT;        {.}
{ CH_0..CH_9 = Chr( 48 )..Chr( 57 )         }
	CH_COLON					   = Chr( 058 );      {:}
	CH_SEMICOLON			   = Chr( 059 );      {;}
	CH_LIST_TOKEN        = CH_SEMICOLON;    {;}
	CH_LOWERTHAN         = Chr( 060 );			{<}
	CH_EQUAL_TOKEN       = Chr( 061 );      {=}
	CH_EQUAL             = CH_EQUAL_TOKEN;  {=}
	CH_GREATERTHAN       = Chr( 062 );			{>}
	CH_QUESTIONMARK 		 = Chr( 063	);			{?}
	CH_AT								 = Chr( 064 );      {@}
	CH_ATADDRESS         = CH_AT;           {@}
{ CH_A..CH_Z = Chr( 65 )..Chr( 90 )         }
	CH_BRACES_OPEN	     = Chr( 091 );      {[}
	CH_BACK_SLASH		     = Chr( 092 );      {\}
	CH_BRACES_CLOSE	     = Chr( 093 );      {]}
	CH_CIRCUMFLEX				 = Chr( 094 );		  {^}
	CH_DEREFERENCE       = CH_CIRCUMFLEX;   {^}
	CH_NAMEFIX		       = Chr( 095 );      {_}
	CH_UNDERSCORE 	     = CH_NAMEFIX;      {_}
	CH_CRASE             = Chr( 096 );      {`}
{ CH_a..CH_z = Chr( 97 )..Chr( 122 )        }
	CH_BRACKET_OPEN      = Chr( 123 );      (*{*)
	CH_PIPE_LINE		     = Chr( 124 );      {|}
	CH_BRACKET_CLOSE     = Chr( 125 );      (*}*)
	CH_TIL               = Chr( 126 );      {~}

	{$IFDEF UNIX_REDHAT} // Cool!!! for future C++ portability
	CH_PATH_SLASH				 = CH_SLASH;        {/}
	{$ELSE}
	CH_PATH_SLASH				 = CH_BACK_SLASH;   {\}
	{$ENDIF}
	
{ Especial Characters }

	CH_TRADEMARK         = Chr( 153 );      {ô}
	CH_POUND             = Chr( 163 );      {£}
	CH_COPYRIGHT         = Chr( 169 );      {©}
	CH_REGISTERED        = Chr( 174 );      {Æ}
	CH_POWER_TWO         = Chr( 178 );      {≤}
	CH_POWER_THREE       = Chr( 179 );      {≥}
	CH_POWER_ONE         = Chr( 185 );      {π}
	CH_ONE_FOURTH        = Chr( 188 );      {º}
	CH_ONE_HALF          = Chr( 189 );      {Ω}
	CH_ONE_THIRD         = Chr( 190 );      {æ}


(*
	'#0;#1;#2;#3;#4;#5;#6;#7;#8;#9;#10;#11;#12;#13;#14;#15;#16;'+
	'#17;#18;#19;#20;#21;#22;#23;#24;#25;#26;#27;#28;#29;#30;#31; ;!;";#;$;%;&;'';(;'+
	');*;+;,;-;.;/;:;";";<;=;>;?;@;[;\;];^;_;`;{;|;};~;;Ä;Å;Ç;É;Ñ;Ö;Ü;á;à;â;ä;ã;'+
	'å;ç;é;è;ê;ë;í;ì;î;ï;ñ;ó;ò;ô;ö;õ;ú;ù;û;ü;†;°;¢;£;§;•;¶;ß;®;©;™;´;¨;≠;Æ;Ø;∞;±;'+
	'≤;≥;¥;µ;∂;∑;∏;π;∫;ª;º;Ω;æ;ø;¿;¡;¬;√;ƒ;≈;∆;«;»;…; ;À;Ã;Õ;Œ;œ;–;—;“;”;‘;’;÷;◊;ÿ;'+
	'Ÿ;⁄;€;‹;›;ﬁ;ﬂ;‡;·;‚;„;‰;Â;Ê;Á;Ë;È;Í;Î;Ï;Ì;Ó;Ô;;Ò;Ú;Û;Ù;ı;ˆ;˜;¯;˘;˙;˚;¸;˝;˛;ˇ';
*)

{##NI##}

	APPLICATION_INTERNAL_REG_SECTION = 'Internal';
	APPLICATION_ROOT_DIR_SECTION = 'RootDir';
	APPLICATION_BASE_REGKEY_PATTERN = '\Software\KnowHow\';

	KNOWHOW_PACKAGES_BASE_REGKEY = APPLICATION_BASE_REGKEY_PATTERN + 'KLIB\'
		{$IFDEF KLIB100} + '100\' {$ENDIF};
	KNOWHOW_PROPERTY_EDITOR_SECTION = 'PropEdit\';

{$IFDEF KLIB100}
	VER = '1.00';
	VER_INT = 100;
	VER_RELEASE_DATE = '25/07/1999 01:00:00';

	SYSTEM_VER = '1.00';
	SYSTEM_VER_INT = 100;
	SYSTEM_VER_RELEASE_DATE = '25/07/1999 01:00:00';

{$ELSE}
	VER = '?.??';
	VER_INT = 0;
	VER_RELEASE_DATE = '01/01/1900 00:00:00';

	SYSTEM_VER = '?.??';
	SYSTEM_VER_INT = 0;
	SYSTEM_VER_RELEASE_DATE = '01/01/1900 00:00:00';
{$ENDIF}

{
--------------------------------------------------------------------------------
------------------------- Registration Information -----------------------------
--------------------------------------------------------------------------------
}

function GetSystemRegistryInfo: LongInt;

{##NI##}

implementation

uses
	uksyTypes, uksyUtils; { do not remove! force uksyUtils to be registered in unit running list }

{
--------------------------------------------------------------------------------
------------------------- Registration Information -----------------------------
--------------------------------------------------------------------------------
}

{--------------------------- Internal Implementation ---------------------------}

type
	TSignature	 = TUserName;
	TKey				 = TUserName;

	TKRegistryInfo = record
		Signature: TSignature;
		Key: TKey;
		UserName: TUserName;
		Company: TCompanyName;
	end;

const

	KnowHowRegistryInfo: TKRegistryInfo =
	(
{$IFDEF KLIB100}
		Signature: '{09536FA0-BF69-11D2-B212-00C0DFE081C4}'; { do not resource/const }
{$ELSE}
		Signature: *MUST GENERATE AN ERROR!*;
{$ENDIF}
		Key:
			#32#32#32#32#32#32#32#32#32#32#32#32#32#32#32#32 +
			#32#32#32#32#32#32#32#32#32#32#32#32#32#32#32#32 +
			#32#32#32#32#32#32;
		UserName:
			#32#32#32#32#32#32#32#32#32#32#32#32#32#32#32#32 +
			#32#32#32#32#32#32#32#32#32#32#32#32#32#32#32#32 +
			#32#32#32#32#32#32;
		Company:
			#32#32#32#32#32#32#32#32#32#32#32#32#32#32#32#32 +
			#32#32#32#32#32#32#32#32#32#32#32#32#32#32#32#32 +
			#32#32#32#32#32#32#32#32#32#32#32#32#32#32#32#32 +
			#32#32#32#32#32#32#32#32#32#32#32#32#32#32#32#32 +
			#32#32#32#32#32#32#32#32#32#32#32#32#32#32#32#32 +
			#32#32#32#32#32#32#32#32#32#32#32#32#32#32#32#32 +
			#32#32#32#32#32#32#32#32#32#32#32#32#32#32#32#32 +
			#32#32#32#32#32#32#32#32#32#32#32#32#32#32#32#32 +
			#32#32#32#32#32#32#32#32#32#32;
	);

{---------------------------- Public Implementation ----------------------------}

function GetSystemRegistryInfo: LongInt;
begin
	Result := LongInt( @KnowHowRegistryInfo ) - SizeOf( TKRegistryInfo );
end;

end.

