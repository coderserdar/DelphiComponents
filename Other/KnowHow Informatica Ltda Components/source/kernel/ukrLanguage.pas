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

unit ukrLanguage;

{$I s:\v100\include\iKLIB100.inc}

interface

uses
	Windows, Classes, uksyConsts, uksyUtils;

{
--------------------------------------------------------------------------------
--------------------------- Multi Language Support -----------------------------
--------------------------------------------------------------------------------
}

type

	EKRLanguage = class( EKKernel );

	PLanguage = ^TLanguage;
	TLanguage = type LANGID;

const
	MAX_K_LANGUAGES					= 22;

	K_LANG_NEUTRAL 		 = SUBLANG_NEUTRAL or LANG_NEUTRAL shl BITS_PER_BYTE; { language neutral }
	K_LANG_NEUTRAL_DEF = SUBLANG_DEFAULT or LANG_NEUTRAL shl BITS_PER_BYTE; { user default }
	K_LANG_NEUTRAL_SYS = SUBLANG_SYS_DEFAULT or LANG_NEUTRAL shl BITS_PER_BYTE; { system default }

	K_LANG_ENGLISH_US  = SUBLANG_ENGLISH_US or LANG_ENGLISH shl BITS_PER_BYTE; { English ( USA ) }
	K_LANG_ENGLISH_UK  = SUBLANG_ENGLISH_UK or LANG_ENGLISH shl BITS_PER_BYTE; { English ( UK ) }
	K_LANG_ENGLISH_AUS = SUBLANG_ENGLISH_AUS or LANG_ENGLISH shl BITS_PER_BYTE; { English ( Australian ) }
	K_LANG_ENGLISH_CAN = SUBLANG_ENGLISH_CAN or LANG_ENGLISH shl BITS_PER_BYTE; { English ( Canadian ) }

	K_LANG_FRENCH          = SUBLANG_FRENCH or LANG_FRENCH shl BITS_PER_BYTE; { French }
	K_LANG_FRENCH_CANADIAN = SUBLANG_FRENCH_CANADIAN or LANG_FRENCH shl BITS_PER_BYTE; { French ( Canadian ) }
	K_LANG_FRENCH_SWISS    = SUBLANG_FRENCH_SWISS or LANG_FRENCH shl BITS_PER_BYTE; { French ( Swiss ) }

	K_LANG_GERMAN          = SUBLANG_GERMAN or LANG_GERMAN shl BITS_PER_BYTE; { German }
	K_LANG_GERMAN_SWISS    = SUBLANG_GERMAN_SWISS or LANG_GERMAN shl BITS_PER_BYTE; { German ( Swiss ) }
	K_LANG_GERMAN_AUSTRIAN = SUBLANG_GERMAN_AUSTRIAN or LANG_GERMAN shl BITS_PER_BYTE; { German ( Austrian ) }

	K_LANG_ITALIAN       = SUBLANG_ITALIAN or LANG_ITALIAN shl BITS_PER_BYTE; { Italian }
	K_LANG_ITALIAN_SWISS = SUBLANG_ITALIAN_SWISS or LANG_ITALIAN shl BITS_PER_BYTE; { Italian ( Swiss ) }

	K_LANG_PORTUGUESE        = SUBLANG_PORTUGUESE or LANG_PORTUGUESE shl BITS_PER_BYTE; { Portuguese }
	K_LANG_PORTUGUESE_BRAZIL = SUBLANG_PORTUGUESE_BRAZILIAN or LANG_PORTUGUESE shl BITS_PER_BYTE; { Portuguese ( Brazilian ) }

	K_LANG_SPANISH           = SUBLANG_SPANISH or LANG_SPANISH shl BITS_PER_BYTE; { Spanish ( Castilian ) }
	K_LANG_SPANISH_MEXICAN   = SUBLANG_SPANISH_MEXICAN or LANG_SPANISH shl BITS_PER_BYTE; { Spanish ( Mexican ) }
	K_LANG_SPANISH_ARGENTINA = SUBLANG_SPANISH_ARGENTINA or LANG_SPANISH shl BITS_PER_BYTE; { Spanish ( Argentina ) }
	K_LANG_SPANISH_CHILE     = SUBLANG_SPANISH_CHILE or LANG_SPANISH shl BITS_PER_BYTE; { Spanish ( Chile ) }
	K_LANG_SPANISH_URUGUAY   = SUBLANG_SPANISH_URUGUAY or LANG_SPANISH shl BITS_PER_BYTE; { Spanish ( Uruguay ) }

function LanguageToString( Language: TLanguage ): string;
function StringToLanguage( const Source: string ): TLanguage;
function LanguageToIdent( Language: Integer; var Ident: string ): Boolean;
function IdentToLanguage( const Ident: string; var Language: Integer ): Boolean;

procedure GetLanguageValues( Proc: TGetStrProc );
procedure ForceLanguageName( const Name: string );
procedure ForceLanguageValue( Value: TLanguage );

implementation

uses
	ukrResStr, ukrUtils;

{
--------------------------------------------------------------------------------
--------------------------- Multi Language Support -----------------------------
--------------------------------------------------------------------------------
}

const
	LanguagesMap: array[0..MAX_K_LANGUAGES-1] of TIdentMapEntry =
	(
		( Value: K_LANG_NEUTRAL; 		 Name: 'K_LANG_NEUTRAL' ),
		( Value: K_LANG_NEUTRAL_DEF; Name: 'K_LANG_NEUTRAL_DEF' ),
		( Value: K_LANG_NEUTRAL_SYS; Name: 'K_LANG_NEUTRAL_SYS' ),

		( Value: K_LANG_ENGLISH_US;  Name: 'K_LANG_ENGLISH_US' ),
		( Value: K_LANG_ENGLISH_UK;  Name: 'K_LANG_ENGLISH_UK' ),
		( Value: K_LANG_ENGLISH_AUS; Name: 'K_LANG_ENGLISH_AUS' ),
		( Value: K_LANG_ENGLISH_CAN; Name: 'K_LANG_ENGLISH_CAN' ),

		( Value: K_LANG_FRENCH; 				 Name: 'K_LANG_FRENCH' ),
		( Value: K_LANG_FRENCH_CANADIAN; Name: 'K_LANG_FRENCH_CANADIAN' ),
		( Value: K_LANG_FRENCH_SWISS; 	 Name: 'K_LANG_FRENCH_SWISS' ),

		( Value: K_LANG_GERMAN; 				 Name: 'K_LANG_GERMAN' ),
		( Value: K_LANG_GERMAN_SWISS; 	 Name: 'K_LANG_GERMAN_SWISS' ),
		( Value: K_LANG_GERMAN_AUSTRIAN; Name: 'K_LANG_GERMAN_AUSTRIAN' ),

		( Value: K_LANG_ITALIAN; 			 Name: 'K_LANG_ITALIAN' ),
		( Value: K_LANG_ITALIAN_SWISS; Name: 'K_LANG_ITALIAN_SWISS' ),

		( Value: K_LANG_PORTUGUESE; 			 Name: 'K_LANG_PORTUGUESE' ),
		( Value: K_LANG_PORTUGUESE_BRAZIL; Name: 'K_LANG_PORTUGUESE_BRAZIL' ),

		( Value: K_LANG_SPANISH; 					 Name: 'K_LANG_SPANISH' ),
		( Value: K_LANG_SPANISH_MEXICAN; 	 Name: 'K_LANG_SPANISH_MEXICAN' ),
		( Value: K_LANG_SPANISH_ARGENTINA; Name: 'K_LANG_SPANISH_ARGENTINA' ),
		( Value: K_LANG_SPANISH_CHILE;     Name: 'K_LANG_SPANISH_CHILE' ),
		( Value: K_LANG_SPANISH_URUGUAY;   Name: 'K_LANG_SPANISH_URUGUAY' )
	);

function LanguageToString( Language: TLanguage ): string;
begin
	if ( not LanguageToIdent( Language, Result ) ) then
		RaiseExceptionFmt( EKRLanguage, sErrInvLangValue, [Language] );
end;

function StringToLanguage( const Source: string ): TLanguage;
var
	i: Integer;
begin
	if ( not IdentToLanguage( Source, i ) ) then
		RaiseExceptionFmt( EKRLanguage, sErrInvLangName, [Source] );
	Result := TLanguage( i );
end;

procedure GetLanguageValues( Proc: TGetStrProc );
var
	i: Integer;
begin
	for i := Low( LanguagesMap ) to High( LanguagesMap ) do
		Proc( LanguagesMap[i].Name );
end;

function LanguageToIdent( Language: Integer; var Ident: string ): Boolean;
begin
	Result := ( ValueBetween( Language, Low( TLanguage ), High( TLanguage ), True )
	  and IntToIdent( Language, Ident, LanguagesMap ) );
end;

function IdentToLanguage( const Ident: string; var Language: Integer ): Boolean;
begin
	Result := ( IdentToInt( Ident, Language, LanguagesMap ) and
		ValueBetween( Language, Low( TLanguage ), High( TLanguage ), True ) );
end;

procedure ForceLanguageValue( Value: TLanguage );
begin
	LanguageToString( Value );
end;

procedure ForceLanguageName( const Name: string );
begin
	StringToLanguage( Name );
end;

{
--------------------------------------------------------------------------------
------------------- Initialization and Finalization Routines -------------------
--------------------------------------------------------------------------------
}

procedure Init;
begin
{ Integer Constants }
	RegisterIntegerConsts( TypeInfo( TLanguage ), IdentToLanguage, LanguageToIdent );
end;

procedure Done;
begin
end;

initialization
	Init;

finalization
	Done;

end.
