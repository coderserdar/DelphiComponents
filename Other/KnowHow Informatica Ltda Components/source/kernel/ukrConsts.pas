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

unit ukrConsts;

{$I s:\v100\include\iKLIB100.inc}

interface

uses
	Controls, uksyConsts;

{
--------------------------------------------------------------------------------
---------------------------- Generic Constants ---------------------------------
--------------------------------------------------------------------------------
}

const

	CM_TITLEFONTCHANGED = CM_BASE + $100;

	K_DEF_PASSWORD = '12345';
	K_DEF_LOGEXT   = '.log';

  KLE_DEF_REPORT_EXTENTION = '.klr';
  KLE_DEF_REPORT_FIELD_EXTENTION =   '.kfr';


{ Auditory Event }

	aeRequestFileName = $00;

{ Caption Buttons }

{ synchronize the maximize/minimize/restore buttons }
	KCBS_DEFAULT = 0;
	KCBS_RESTORE = 1;
{ synchronize the rollup/rolldown button }
	KCBS_INDICATEDN = 1;

{$IFDEF KLIB100}
	KERNEL_VER = '1.00';
	KERNEL_VER_INT = 100;
	KERNEL_VER_RELEASE_DATE = '25/07/1999 01:00:00';
{$ELSE}
	KERNEL_VER = '?.??';
	KERNEL_VER_INT = 0;
	KERNEL_VER_RELEASE_DATE = '01/01/1900 00:00:00';
{$ENDIF}

{
--------------------------------------------------------------------------------
------------------------- Generic Parser Architecture --------------------------
--------------------------------------------------------------------------------
}

	DEFAULT_PARSER_BUFFER_SIZE = $2000;

	SYMBOL_CHARSET_IDX = 0;
	STRING_CHARSET_IDX = 1;
	HEXA_CHARSET_IDX = 2;
	INTEGER_CHARSET_IDX = 3;
	FLOAT_CHARSET_IDX = 4;
	COMMENT_CHARSET_IDX = 5;
	RELOP_CHARSET_IDX = 6;
	SPECIAL_CHARSET_IDX = 7;
	BLANK_CHARSET_IDX = 8;

	LOW_CHARSET_IDX = SYMBOL_CHARSET_IDX;
	HIGH_CHARSET_IDX = BLANK_CHARSET_IDX;

	DEFAULT_PARSER_SYMBOL_CHARSET = CHARSET_IDENTIFIER;
	DEFAULT_PASCALPARSER_STRING_CHARSET = [CH_GRIDLING, CH_PLICK];
	DEFAULT_PARSER_HEXA_CHARSET = CHARSET_HEXDIGITS;
	DEFAULT_PASCALPARSER_INTEGER_CHARSET = CHARSET_NUM;
	DEFAULT_PARSER_FLOAT_CHARSET = CHARSET_FLOAT + ['.'];
	DEFAULT_PASCALPARSER_COMMENT_CHARSET = ['{', '}', '/', '(', ')', '*'];
	DEFAULT_PASCALPARSER_RELOP_CHARSET = ['<', '>', '='];
	DEFAULT_PASCALPARSER_SPECIAL_CHARSET = ['(', ':', '.'];
	DEFAULT_PARSER_BLANK_CHARSET = [#0..#32];

	PASCALPARSER_KEYWORDS =
		'absolute'#13#10'abstract'#13#10'and'#13#10'array'#13#10'as'#13#10'asm'#13#10 +
		'assembler'#13#10'at'#13#10'automated'#13#10'begin'#13#10'case'#13#10'cdecl'#13#10 +
		'class'#13#10'const'#13#10'constructor'#13#10'contains'#13#10'default'#13#10 +
		'destructor'#13#10'dispid'#13#10'dispinterface'#13#10'div'#13#10'do'#13#10 +
		'downto'#13#10'dynamic'#13#10'else'#13#10'end'#13#10'except'#13#10'exports'#13#10 +
		'external'#13#10'file'#13#10'finalization'#13#10'finally'#13#10'for'#13#10 +
		'forward'#13#10'function'#13#10'goto'#13#10'if'#13#10'implementation'#13#10 +
		'in'#13#10'index'#13#10'inherited'#13#10'initialization'#13#10'inline'#13#10 +
		'interface'#13#10'is'#13#10'label'#13#10'library'#13#10'message'#13#10'mod'#13#10 +
		'name'#13#10'nil'#13#10'nodefault'#13#10'not'#13#10'object'#13#10'of'#13#10 +
		'on'#13#10'or'#13#10'override'#13#10'packed'#13#10'pascal'#13#10'private'#13#10 +
		'procedure'#13#10'program'#13#10'property'#13#10'protected'#13#10'public'#13#10 +
		'published'#13#10'raise'#13#10'read'#13#10'record'#13#10'register'#13#10'repeat'#13#10 +
		'requires'#13#10'resident'#13#10'set'#13#10'shl'#13#10'shr'#13#10'stdcall'#13#10 +
		'stored'#13#10'string'#13#10'then'#13#10'threadvar'#13#10'to'#13#10'try'#13#10 +
		'type'#13#10'unit'#13#10'until'#13#10'uses'#13#10'var'#13#10'virtual'#13#10 +
		'while'#13#10'with'#13#10'write'#13#10'xor';

	DFMPARSER_KEYWORDS =
		'end'#13#10'inherited'#13#10'object';
                      
{##NI##}

{
--------------------------------------------------------------------------------
------------------------- Registration Information -----------------------------
--------------------------------------------------------------------------------
}

function GetKernelRegistryInfo: LongInt;

{##NI##}

implementation

uses
	uksyTypes, ukrUtils;

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
	
function GetKernelRegistryInfo: LongInt;
begin
	Result := LongInt( @KnowHowRegistryInfo ) - SizeOf( TKRegistryInfo );
end;

end.
