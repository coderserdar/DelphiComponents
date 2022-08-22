{ ************************************************************************************** }
{                                                                                        }
{ Wrapper for NHunspell for Delphi 2007+                                                 }
{ Version 1.1.0                                                                          }
{                                                                                        }
{ The contents of this file are subject to the Mozilla Public License Version 1.1        }
{ (the "License"); you may not use this file except in compliance with the License.      }
{ You may obtain a copy of the License at http://www.mozilla.org/MPL/                    }
{                                                                                        }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT     }
{ WARRANTY OF ANY KIND, either express or implied. See the License for the specific      }
{ language governing rights and limitations under the License.                           }
{                                                                                        }
{ History:                                                                               }
{                                                                                        }
{ Version 1.1.0 (2011-03-18)                                                             }
{ Complete rewrite of the TNHunspell class to cover the fact that OXT containers         }
{ often contain more than one dictionary (e.g. English UK, USA, Australian in one OXT).  }
{ Support for older dictionaries (OpenOffice 2, uncompressed dictionary files) has       }
{ been dropped.                                                                          }
{                                                                                        }
{ Version 1.0.0 (2010-08-09)                                                             }
{ Initial release.                                                                       }
{                                                                                        }
{ Acknowledgments:                                                                       }
{                                                                                        }
{ NHunspell Copyright (C) Thomas Maierhofer (http://nhunspell.sourceforge.net)           }
{ PasZip Copyright (C) 2010 Arnaud Bouchez (http://synopse.info)                         }
{ Delphi implementation Copyright (C) Alexander Halser (http://www.ec-software.com)      }
{                                                                                        }
{ ************************************************************************************** }


unit NHunspell;

{
  NHunspell ist a DotNet implementation for Hunspell by Thomas Mayerhofer. You
  can download the original DLLs from http://nhunspell.sourceforge.net.

  From the DLL distribution, Delphi needs the file "HunspellX86.DLL" only, which is
  a 32 bit unmanaged code version of the Hunspell DLL. Contrary to other versions
  of Hunspell, NHunspell is strictly Unicode (all DLL functions require widestrings).

  The advantage of NHunspell over other implementations is, that the DLL handles codepage
  conversions of the various dictionaries internally and includes better memory management.
  It furthermore implements the hyphenation functions in the same DLL and can be initialized
  with a memory pointer instead of the file name of a dictionary. The latter is mandatory to
  support OpenOffice 3 dictionaries (.oxt).

  # Files ##############################################################################

  NHunspell.pas  (this unit) implements the TNHunspell class that encapsulates loading
  of OXT dictionaries and implements spelling and hyphenatin functions
  NHunXml.pas    Tiny XML parser for OXT dictionaries
  PasZip.pas     Zip archive reader, copyright (C) 2010 Arnaud Bouchez (http://synopse.info)


  # Quickhelp ##########################################################################

  Please refer to NHUNSPELL.CHM for detailed information.
  Usage example:

  var
    spell: TNHunspell;
    tmpStr: TWidestrings;
  begin
    spell := TNHunspell.Create;
    spell.Load(<OXTfilename>, [dtSpelling, dtHyphenation]);
    if spell.Active then
    begin
     if spell.Spell('Abracadabra') then
       Showmessage('correctly spelt')
     else
       begin
         tmpStr := TWidestringlist.create;
         spell.Suggest('Abracadabra', tmpStr);
         if tmpStr.count = 0 then
           showmessage('No suggestions')
         else
           showmessage('Suggestions: ' + tmpStr.Text);
         FreeAndNil(tmpStr);
       end;
     end;
     spell.free;
  end;


}

interface

uses
  Types, Classes, Windows, SysUtils, {$IFNDEF UNICODE}Widestrings,{$ENDIF}
  NHunXml;

const
  HunspellDLLName = 'HunspellX86.dll';

type
  HyphenResult = record
    hyphenatedWord: PWideChar;
    hyphenationPoints: PInt;
    repData: ^PWideChar;
    posData: PInt;
    cutData: PInt;
  end;

  PHyphenResult = ^HyphenResult;

  {$IFDEF UNICODE}
  TUnicodeStrings = TStrings;
  TUnicodeStringList = TStringList;
  {$ELSE}
  TUnicodeStrings = TWideStrings;
  TUnicodeStringList = TWideStringList;
  UnicodeString = Widestring;
  {$ENDIF}

var
  HunspellDll: HMODULE;
  HunspellInit: function(const AffixBuffer: PAnsiChar; AffixBufferSize: Integer; const DictionaryBuffer: PAnsiChar; DictionaryBufferSize: Integer; key: PAnsiChar): Pointer; cdecl;
  HunspellFree: procedure(Handle: Pointer); cdecl;
  HunspellSpell: function(Handle: Pointer; const Word: PWideChar): Integer; cdecl;
  HunspellSuggest: function(Handle: Pointer; const Word: PWideChar): Pointer; cdecl;
  HunspellAnalyze: function(Handle: Pointer; const Word: PWideChar): Pointer; cdecl;
  HunspellStem: function(Handle: Pointer; const Word: PWideChar): Pointer; cdecl;
  HunspellGenerate: function(Handle: Pointer; const Word: PWideChar; const SampleWord: PWideChar): Pointer; cdecl;
  HunspellAdd: function(Handle: Pointer; const Word: PWideChar): Integer; cdecl;
  HunspellAddWithAffix: function(Handle: Pointer; const Word, Affix: PWideChar): Integer; cdecl;
  HyphenInit: function(const DictionaryBuffer: PAnsiChar; DictionaryBufferSize: Integer): Pointer; cdecl;
  HyphenFree: procedure(Handle: Pointer); cdecl;
  HyphenHyphenate: function(Handle: Pointer; const Word: PWideChar): PHyphenResult; cdecl;

type
  THunspellDictionaryType = (dtSpelling, dtHyphenation, dtThesaurus);
  THunspellDictionarySupport = set of THunspellDictionaryType;

  TNHCustomDictionary = class(TObject)
  private
    FActive: Boolean;
    FLoaded: Boolean;
    FDictionaryFilename: String;   {OXT container}
    FID: UnicodeString;
    FDisplayName: UnicodeString;
    FVersion: UnicodeString;
    FPublisher: UnicodeString;
    FLangID: WORD;
    FLanguageName: UnicodeString;
    FRefCount: Integer;
  protected
    function  GetInternalFileID: String; virtual;
    function  Load: Boolean; virtual;
    procedure Unload; virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    property Active: Boolean read FActive write FActive;
    property Loaded: Boolean read FLoaded;
    property DictionaryFileName: String read FDictionaryFilename;
    property DisplayName: UnicodeString read FDisplayName;
    property LangID: WORD read FLangID;
    property LanguageName: UnicodeString read FLanguageName;
    property ID: UnicodeString read FID;
    property InternalFileID: String read GetInternalFileID;
    property Publisher: UnicodeString read FPublisher;
    property Version: UnicodeString read FVersion;
  end;

  TNHSpellDictionary = class(TNHCustomDictionary)
  private
    FHunspellHandle: Pointer;
    FInternalAffixPath: String;
    FInternalDictPath: String;
    {DEPRICATED function InternalLoadSpellDirect(const AffixFileName, DictionaryFileName: String; const key: AnsiString = ''): Boolean;}
    function InitSpelling(const AffixBuffer: PAnsiChar; AffixBufferSize: Integer; const DictionaryBuffer: PAnsiChar; DictionaryBufferSize: Integer;
      key: PAnsiChar): Boolean;
  protected
  public
    constructor Create; override;
    function  GetInternalFileID: String; override;
    function  Load: Boolean; override;
    procedure Unload; override;

    function  Add(const Word: UnicodeString): Boolean;
    function  AddWithAffix(const Word, AffixModel: UnicodeString): Boolean;
    function  Spell(const Word: UnicodeString): Boolean;
    procedure Suggest(const Word: UnicodeString; AWordList: TUnicodeStrings);
    procedure Analyze(const Word: UnicodeString; AWordList: TUnicodeStrings);
    procedure Stem(const Word: UnicodeString; AWordList: TUnicodeStrings);
  end;

  TNHHyphenDictionary = class(TNHCustomDictionary)
  private
    FHyphenHandle: Pointer;
    FInternalHyphenPath: String;
  protected
    {DEPRICATED function InternalLoadHyphenDirect(const HyphenFileName: String): Boolean;}
    function InitHyphenation(const hyphenBuffer: PAnsiChar; hyphenBufferSize: Integer): Boolean;
  public
    constructor Create; override;
    function  GetInternalFileID: String; override;
    function  Load: Boolean; override;
    procedure Unload; override;

    function Hyphenate(const Word: UnicodeString): UnicodeString; overload;
    function Hyphenate(const Word: PWideChar): PHyphenResult; overload;
  end;

  TNHunspell = class(TObject)
  private
    FSpellDictionaries: TStringList;
    FHyphenDictionaries: TStringList;
    function  GetSpellDictionaryCount: Integer;
    function  GetHyphenDictionaryCount: Integer;
    function  GetSpellDictionary(Index: Integer): TNHSpellDictionary;
    function  GetHyphenDictionary(Index: Integer): TNHHyphenDictionary;
    procedure InternalReadDescription(const ADescription: RawByteString;
                                      var AVersion, AID, ADisplayName, APublisher: Widestring;
                                      const PreferLocalized: Boolean = False);
  protected
  public
    constructor Create;
    destructor Destroy; override;

    function  ReadFolder(FolderName: String): Boolean;
    function  ReadOXT(const FileName: String): Boolean;
    function  FindSpellDictionary(const AInternalID: String): TNHSpellDictionary;
    function  FindHyphenDictionary(const AInternalID: String): TNHHyphenDictionary;

    procedure Dictionary_Addref(const AInternalID: String);
    procedure Dictionary_Release(const AInternalID: String);

    procedure ClearDictionaries;
    procedure RemoveSpellDictionary(index: integer);
    procedure RemoveHyphenDictionary(index: integer);
    procedure UpdateAndLoadDictionaries;

    property  SpellDictionaryCount: Integer read GetSpellDictionaryCount;
    property  HyphenDictionaryCount: Integer read GetHyphenDictionaryCount;
    property  SpellDictionaries[Index: Integer]: TNHSpellDictionary read GetSpellDictionary;
    property  HyphenDictionaries[Index: Integer]: TNHHyphenDictionary read GetHyphenDictionary;
  end;

var
  Hunspell: TNHunspell;

implementation

uses
  PasZip;

const
  cFileExtAff = '.aff';
  cFileExtDic = '.dic';
  cFileExtOxt = '.oxt';
  cLangEN = 'en';

  { ----------------------------------- Common functions ---------------------------------- }

function DLLInitialize: Boolean;
begin
  Result := False;

  if HunspellDll = 0 then
  begin
    HunspellDll := SafeLoadLibrary(HunspellDLLName);

    if HunspellDll <> 0 then
    begin
      @HunspellInit := GetProcAddress(HunspellDll, 'HunspellInit');
      if not Assigned(HunspellInit) then
        Exit;
      @HunspellFree := GetProcAddress(HunspellDll, 'HunspellFree');
      if not Assigned(HunspellFree) then
        Exit;
      @HunspellSpell := GetProcAddress(HunspellDll, 'HunspellSpell');
      if not Assigned(HunspellSpell) then
        Exit;
      @HunspellSuggest := GetProcAddress(HunspellDll, 'HunspellSuggest');
      if not Assigned(HunspellSuggest) then
        Exit;
      @HunspellAnalyze := GetProcAddress(HunspellDll, 'HunspellAnalyze');
      if not Assigned(HunspellAnalyze) then
        Exit;
      @HunspellStem := GetProcAddress(HunspellDll, 'HunspellStem');
      if not Assigned(HunspellStem) then
        Exit;
      @HunspellGenerate := GetProcAddress(HunspellDll, 'HunspellGenerate');
      if not Assigned(HunspellGenerate) then
        Exit;
      @HunspellAdd := GetProcAddress(HunspellDll, 'HunspellAdd');
      if not Assigned(HunspellAdd) then
        Exit;
      @HunspellAddWithAffix := GetProcAddress(HunspellDll, 'HunspellAddWithAffix');
      if not Assigned(HunspellAddWithAffix) then
        Exit;

      @HyphenInit := GetProcAddress(HunspellDll, 'HyphenInit');
      if not Assigned(HyphenInit) then
        Exit;
      @HyphenFree := GetProcAddress(HunspellDll, 'HyphenFree');
      if not Assigned(HyphenFree) then
        Exit;
      @HyphenHyphenate := GetProcAddress(HunspellDll, 'HyphenHyphenate');
      if not Assigned(HyphenHyphenate) then
        Exit;
    end;
  end;

  Result := HunspellDll <> 0;
end;

const
  LangCodes: array [0 .. 108] of Word = (
    { LANG_SYSTEM_DEFAULT,
      LANG_USER_DEFAULT, }
    LANG_AFRIKAANS or SUBLANG_DEFAULT shl 10,
    LANG_ALBANIAN or SUBLANG_DEFAULT shl 10,
    LANG_ARABIC or SUBLANG_ARABIC_SAUDI_ARABIA shl 10,
    LANG_ARABIC or SUBLANG_ARABIC_IRAQ shl 10,
    LANG_ARABIC or SUBLANG_ARABIC_EGYPT shl 10,
    LANG_ARABIC or SUBLANG_ARABIC_LIBYA shl 10,
    LANG_ARABIC or SUBLANG_ARABIC_ALGERIA shl 10,
    LANG_ARABIC or SUBLANG_ARABIC_MOROCCO shl 10,
    LANG_ARABIC or SUBLANG_ARABIC_TUNISIA shl 10,
    LANG_ARABIC or SUBLANG_ARABIC_OMAN shl 10,
    LANG_ARABIC or SUBLANG_ARABIC_YEMEN shl 10,
    LANG_ARABIC or SUBLANG_ARABIC_SYRIA shl 10,
    LANG_ARABIC or SUBLANG_ARABIC_JORDAN shl 10,
    LANG_ARABIC or SUBLANG_ARABIC_LEBANON shl 10,
    LANG_ARABIC or SUBLANG_ARABIC_KUWAIT shl 10,
    LANG_ARABIC or SUBLANG_ARABIC_UAE shl 10,
    LANG_ARABIC or SUBLANG_ARABIC_BAHRAIN shl 10,
    LANG_ARABIC or SUBLANG_ARABIC_QATAR shl 10,
    LANG_BASQUE or SUBLANG_DEFAULT shl 10,
    LANG_BELARUSIAN or SUBLANG_DEFAULT shl 10,
    LANG_BULGARIAN or SUBLANG_DEFAULT shl 10,
    LANG_CATALAN or SUBLANG_DEFAULT shl 10,
    LANG_CHINESE or SUBLANG_DEFAULT shl 10,
    LANG_CHINESE or SUBLANG_CHINESE_TRADITIONAL shl 10,
    LANG_CHINESE or SUBLANG_CHINESE_SIMPLIFIED shl 10,
    LANG_CHINESE or SUBLANG_CHINESE_HONGKONG shl 10,
    LANG_CHINESE or SUBLANG_CHINESE_SINGAPORE shl 10,
    LANG_CROATIAN or SUBLANG_DEFAULT shl 10,
    LANG_CZECH or SUBLANG_DEFAULT shl 10,
    LANG_DANISH or SUBLANG_DEFAULT shl 10,
    LANG_DUTCH or SUBLANG_DUTCH shl 10,
    LANG_DUTCH or SUBLANG_DUTCH_BELGIAN shl 10,
    LANG_ENGLISH or SUBLANG_ENGLISH_US shl 10,
    LANG_ENGLISH or SUBLANG_ENGLISH_UK shl 10,
    LANG_ENGLISH or SUBLANG_ENGLISH_UK shl 10,  //twice!
    LANG_ENGLISH or SUBLANG_ENGLISH_AUS shl 10,
    LANG_ENGLISH or SUBLANG_ENGLISH_CAN shl 10,
    LANG_ENGLISH or SUBLANG_ENGLISH_NZ shl 10,
    LANG_ENGLISH or SUBLANG_ENGLISH_EIRE shl 10,
    LANG_ENGLISH or SUBLANG_ENGLISH_SOUTH_AFRICA shl 10,
    LANG_ENGLISH or SUBLANG_ENGLISH_JAMAICA shl 10,
    LANG_ENGLISH or SUBLANG_ENGLISH_CARIBBEAN shl 10,
    LANG_ENGLISH or SUBLANG_ENGLISH_BELIZE shl 10,
    LANG_ENGLISH or SUBLANG_ENGLISH_TRINIDAD shl 10,
    LANG_ESTONIAN or SUBLANG_DEFAULT shl 10,
    LANG_FAEROESE or SUBLANG_DEFAULT shl 10,
    LANG_FARSI or SUBLANG_DEFAULT shl 10,
    LANG_FINNISH or SUBLANG_DEFAULT shl 10,
    LANG_FRENCH or SUBLANG_FRENCH shl 10,
    LANG_FRENCH or SUBLANG_FRENCH_BELGIAN shl 10,
    LANG_FRENCH or SUBLANG_FRENCH_CANADIAN shl 10,
    LANG_FRENCH or SUBLANG_FRENCH_SWISS shl 10,
    LANG_FRENCH or SUBLANG_FRENCH_LUXEMBOURG shl 10,
    LANG_GERMAN or SUBLANG_GERMAN shl 10,
    LANG_GERMAN or SUBLANG_GERMAN_SWISS shl 10,
    LANG_GERMAN or SUBLANG_GERMAN_AUSTRIAN shl 10,
    LANG_GERMAN or SUBLANG_GERMAN_LUXEMBOURG shl 10,
    LANG_GERMAN or SUBLANG_GERMAN_LIECHTENSTEIN shl 10,
    LANG_GREEK or SUBLANG_DEFAULT shl 10,
    LANG_HEBREW or SUBLANG_DEFAULT shl 10,
    LANG_HUNGARIAN or SUBLANG_DEFAULT shl 10,
    LANG_ICELANDIC or SUBLANG_DEFAULT shl 10,
    LANG_INDONESIAN or SUBLANG_DEFAULT shl 10,
    LANG_ITALIAN or SUBLANG_ITALIAN shl 10,
    LANG_ITALIAN or SUBLANG_ITALIAN_SWISS shl 10,
    LANG_JAPANESE or SUBLANG_DEFAULT shl 10,
    LANG_KOREAN or SUBLANG_KOREAN shl 10,
    LANG_KOREAN or SUBLANG_KOREAN shl 10,
    LANG_KOREAN or SUBLANG_KOREAN_JOHAB shl 10,
    LANG_LATVIAN or SUBLANG_DEFAULT shl 10,
    LANG_LITHUANIAN or SUBLANG_DEFAULT shl 10,
    LANG_NORWEGIAN or SUBLANG_NORWEGIAN_BOKMAL shl 10,
    LANG_NORWEGIAN or SUBLANG_NORWEGIAN_NYNORSK shl 10,
    LANG_POLISH or SUBLANG_DEFAULT shl 10,
    LANG_PORTUGUESE or SUBLANG_PORTUGUESE shl 10,
    LANG_PORTUGUESE or SUBLANG_PORTUGUESE_BRAZILIAN shl 10,
    LANG_ROMANIAN or SUBLANG_DEFAULT shl 10,
    LANG_RUSSIAN or SUBLANG_DEFAULT shl 10,
    LANG_SERBIAN or SUBLANG_DEFAULT shl 10,
    LANG_SERBIAN or SUBLANG_SERBIAN_LATIN shl 10,
    LANG_SERBIAN or SUBLANG_SERBIAN_CYRILLIC shl 10,
    LANG_SLOVAK or SUBLANG_DEFAULT shl 10,
    LANG_SLOVENIAN or SUBLANG_DEFAULT shl 10,
    LANG_SPANISH or SUBLANG_SPANISH shl 10,
    LANG_SPANISH or SUBLANG_SPANISH_MEXICAN shl 10,
    LANG_SPANISH or SUBLANG_SPANISH_MODERN shl 10,
    LANG_SPANISH or SUBLANG_SPANISH_GUATEMALA shl 10,
    LANG_SPANISH or SUBLANG_SPANISH_COSTA_RICA shl 10,
    LANG_SPANISH or SUBLANG_SPANISH_PANAMA shl 10,
    LANG_SPANISH or SUBLANG_SPANISH_DOMINICAN_REPUBLIC shl 10,
    LANG_SPANISH or SUBLANG_SPANISH_VENEZUELA shl 10,
    LANG_SPANISH or SUBLANG_SPANISH_COLOMBIA shl 10,
    LANG_SPANISH or SUBLANG_SPANISH_PERU shl 10,
    LANG_SPANISH or SUBLANG_SPANISH_ARGENTINA shl 10,
    LANG_SPANISH or SUBLANG_SPANISH_ECUADOR shl 10,
    LANG_SPANISH or SUBLANG_SPANISH_CHILE shl 10,
    LANG_SPANISH or SUBLANG_SPANISH_URUGUAY shl 10,
    LANG_SPANISH or SUBLANG_SPANISH_PARAGUAY shl 10,
    LANG_SPANISH or SUBLANG_SPANISH_BOLIVIA shl 10,
    LANG_SPANISH or SUBLANG_SPANISH_EL_SALVADOR shl 10,
    LANG_SPANISH or SUBLANG_SPANISH_HONDURAS shl 10,
    LANG_SPANISH or SUBLANG_SPANISH_NICARAGUA shl 10,
    LANG_SPANISH or SUBLANG_SPANISH_PUERTO_RICO shl 10,
    LANG_SWEDISH or SUBLANG_SWEDISH shl 10,
    LANG_SWEDISH or SUBLANG_SWEDISH_FINLAND shl 10,
    LANG_THAI or SUBLANG_DEFAULT shl 10,
    LANG_TURKISH or SUBLANG_DEFAULT shl 10,
    LANG_UKRAINIAN or SUBLANG_DEFAULT shl 10,
    LANG_VIETNAMESE or SUBLANG_DEFAULT shl 10);

const
  LangShortcuts: array [0 .. 108] of AnsiString = (
    'af', // LANG_AFRIKAANS or SUBLANG_DEFAULT shl 10,
    'sq', // LANG_ALBANIAN or SUBLANG_DEFAULT shl 10,
    'ar-sa', // LANG_ARABIC or SUBLANG_ARABIC_SAUDI_ARABIA shl 10,
    'ar-iq', // LANG_ARABIC or SUBLANG_ARABIC_IRAQ shl 10,
    'ar-eg', // LANG_ARABIC or SUBLANG_ARABIC_EGYPT shl 10,
    'ar-ly', // LANG_ARABIC or SUBLANG_ARABIC_LIBYA shl 10,
    'ar-dz', // LANG_ARABIC or SUBLANG_ARABIC_ALGERIA shl 10,
    'ar-ma', // LANG_ARABIC or SUBLANG_ARABIC_MOROCCO shl 10,
    'ar-tn', // LANG_ARABIC or SUBLANG_ARABIC_TUNISIA shl 10,
    'ar-om', // LANG_ARABIC or SUBLANG_ARABIC_OMAN shl 10,
    'ar-ye', // LANG_ARABIC or SUBLANG_ARABIC_YEMEN shl 10,
    'ar-sy', // LANG_ARABIC or SUBLANG_ARABIC_SYRIA shl 10,
    'ar-jo', // LANG_ARABIC or SUBLANG_ARABIC_JORDAN shl 10,
    'ar-lb', // LANG_ARABIC or SUBLANG_ARABIC_LEBANON shl 10,
    'ar-kw', // LANG_ARABIC or SUBLANG_ARABIC_KUWAIT shl 10,
    'ar-ae', // LANG_ARABIC or SUBLANG_ARABIC_UAE shl 10,
    'ar-bh', // LANG_ARABIC or SUBLANG_ARABIC_BAHRAIN shl 10,
    'ar-qa', // LANG_ARABIC or SUBLANG_ARABIC_QATAR shl 10,
    'eu', // LANG_BASQUE or SUBLANG_DEFAULT shl 10,
    'be', // LANG_BELARUSIAN
    'bg', // LANG_BULGARIAN
    'ca', // LANG_CATALAN
    'zh', // LANG_CHINESE
    'zh-tw', // LANG_CHINESE or SUBLANG_CHINESE_TRADITIONAL shl 10,
    'zh-cn', // LANG_CHINESE or SUBLANG_CHINESE_SIMPLIFIED shl 10,
    'zh-hk', // LANG_CHINESE or SUBLANG_CHINESE_HONGKONG shl 10,
    'zh-sg', // LANG_CHINESE or SUBLANG_CHINESE_SINGAPORE shl 10,
    'hr', // LANG_CROATIAN
    'cs', // LANG_CZECH
    'da', // LANG_DANISH
    'nl-nl', // LANG_DUTCH or SUBLANG_DUTCH shl 10,
    'nl-be', // LANG_DUTCH or SUBLANG_DUTCH_BELGIAN shl 10,
    'en-us', // LANG_ENGLISH or SUBLANG_ENGLISH_US shl 10,
    'en-uk', // LANG_ENGLISH or SUBLANG_ENGLISH_UK shl 10,
    'en-gb', // LANG_ENGLISH or SUBLANG_ENGLISH_UK shl 10,
    'en-au', // LANG_ENGLISH or SUBLANG_ENGLISH_AUS shl 10,
    'en-ca', // LANG_ENGLISH or SUBLANG_ENGLISH_CAN shl 10,
    'en-nz', // LANG_ENGLISH or SUBLANG_ENGLISH_NZ shl 10,
    'en-ie', // LANG_ENGLISH or SUBLANG_ENGLISH_EIRE shl 10,
    'en-za', // LANG_ENGLISH or SUBLANG_ENGLISH_SOUTH_AFRICA shl 10,
    'en-jm', // LANG_ENGLISH or SUBLANG_ENGLISH_JAMAICA shl 10,
    'en-cb', // LANG_ENGLISH or SUBLANG_ENGLISH_CARIBBEAN shl 10,
    'en-bz', // LANG_ENGLISH or SUBLANG_ENGLISH_BELIZE shl 10,
    'en-tt', // LANG_ENGLISH or SUBLANG_ENGLISH_TRINIDAD shl 10,
    'et', // LANG_ESTONIAN
    'fo', // LANG_FAEROESE
    'fa', // LANG_FARSI
    'fi', // LANG_FINNISH
    'fr-fr', // LANG_FRENCH or SUBLANG_FRENCH shl 10,
    'fr-be', // LANG_FRENCH or SUBLANG_FRENCH_BELGIAN shl 10,
    'fr-ca', // LANG_FRENCH or SUBLANG_FRENCH_CANADIAN shl 10,
    'fr-ch', // LANG_FRENCH or SUBLANG_FRENCH_SWISS shl 10,
    'fr-lu', // LANG_FRENCH or SUBLANG_FRENCH_LUXEMBOURG shl 10,
    'de-de', // LANG_GERMAN or SUBLANG_GERMAN shl 10,
    'de-ch', // LANG_GERMAN or SUBLANG_GERMAN_SWISS shl 10,
    'de-at', // LANG_GERMAN or SUBLANG_GERMAN_AUSTRIAN shl 10,
    'de-lu', // LANG_GERMAN or SUBLANG_GERMAN_LUXEMBOURG shl 10,
    'de-li', // LANG_GERMAN or SUBLANG_GERMAN_LIECHTENSTEIN shl 10,
    'el', // LANG_GREEK
    'he', // LANG_HEBREW
    'hu', // LANG_HUNGARIAN
    'is', // LANG_ICELANDIC
    'id', // LANG_INDONESIAN
    'it-it', // LANG_ITALIAN or SUBLANG_ITALIAN shl 10,
    'it-ch', // LANG_ITALIAN or SUBLANG_ITALIAN_SWISS shl 10,
    'ja', // LANG_JAPANESE
    'ko', // LANG_KOREAN or SUBLANG_KOREAN shl 10,
    'ko-ko', // LANG_KOREAN or SUBLANG_KOREAN shl 10,
    'ko-kp', // LANG_KOREAN or SUBLANG_KOREAN_JOHAB shl 10,
    'lv', // LANG_LATVIAN or SUBLANG_DEFAULT shl 10,
    'lt', // LANG_LITHUANIAN or SUBLANG_DEFAULT shl 10,
    'no', // LANG_NORWEGIAN or SUBLANG_NORWEGIAN_BOKMAL shl 10,
    'no-no', // LANG_NORWEGIAN or SUBLANG_NORWEGIAN_NYNORSK shl 10,
    'pl', // LANG_POLISH or SUBLANG_DEFAULT shl 10,
    'pt-pt', // LANG_PORTUGUESE or SUBLANG_PORTUGUESE shl 10,
    'pt-br', // LANG_PORTUGUESE or SUBLANG_PORTUGUESE_BRAZILIAN shl 10,
    'ro', // LANG_ROMANIAN or SUBLANG_DEFAULT shl 10,
    'ru', // LANG_RUSSIAN or SUBLANG_DEFAULT shl 10,
    'sr', // LANG_SERBIAN or SUBLANG_DEFAULT shl 10,
    'sr', // LANG_SERBIAN or SUBLANG_SERBIAN_LATIN shl 10,
    'sr', // LANG_SERBIAN or SUBLANG_SERBIAN_CYRILLIC shl 10,
    'sk', // LANG_SLOVAK or SUBLANG_DEFAULT shl 10,
    'sl', // LANG_SLOVENIAN or SUBLANG_DEFAULT shl 10,
    'es', // LANG_SPANISH or SUBLANG_SPANISH shl 10,
    'es-mx', // LANG_SPANISH or SUBLANG_SPANISH_MEXICAN shl 10,
    'es', // LANG_SPANISH or SUBLANG_SPANISH_MODERN shl 10,
    'es-gt', // LANG_SPANISH or SUBLANG_SPANISH_GUATEMALA shl 10,
    'es-cr', // LANG_SPANISH or SUBLANG_SPANISH_COSTA_RICA shl 10,
    'es-pa', // LANG_SPANISH or SUBLANG_SPANISH_PANAMA shl 10,
    'es-do', // LANG_SPANISH or SUBLANG_SPANISH_DOMINICAN_REPUBLIC shl 10,
    'es-ve', // LANG_SPANISH or SUBLANG_SPANISH_VENEZUELA shl 10,
    'es-co', // LANG_SPANISH or SUBLANG_SPANISH_COLOMBIA shl 10,
    'es-pe', // LANG_SPANISH or SUBLANG_SPANISH_PERU shl 10,
    'es-ar', // LANG_SPANISH or SUBLANG_SPANISH_ARGENTINA shl 10,
    'es-ec', // LANG_SPANISH or SUBLANG_SPANISH_ECUADOR shl 10,
    'es-cl', // LANG_SPANISH or SUBLANG_SPANISH_CHILE shl 10,
    'es-uy', // LANG_SPANISH or SUBLANG_SPANISH_URUGUAY shl 10,
    'es-py', // LANG_SPANISH or SUBLANG_SPANISH_PARAGUAY shl 10,
    'es-bo', // LANG_SPANISH or SUBLANG_SPANISH_BOLIVIA shl 10,
    'es-sv', // LANG_SPANISH or SUBLANG_SPANISH_EL_SALVADOR shl 10,
    'es-hn', // LANG_SPANISH or SUBLANG_SPANISH_HONDURAS shl 10,
    'es-ni', // LANG_SPANISH or SUBLANG_SPANISH_NICARAGUA shl 10,
    'es-pr', // LANG_SPANISH or SUBLANG_SPANISH_PUERTO_RICO shl 10,
    'sv', // LANG_SWEDISH or SUBLANG_SWEDISH shl 10,
    'sv-fi', // LANG_SWEDISH or SUBLANG_SWEDISH_FINLAND shl 10,
    'th', // LANG_THAI or SUBLANG_DEFAULT shl 10,
    'tr', // LANG_TURKISH or SUBLANG_DEFAULT shl 10,
    'uk', // LANG_UKRAINIAN or SUBLANG_DEFAULT shl 10,
    'vi'); // LANG_VIETNAMESE or SUBLANG_DEFAULT shl 10);

function LangIDToLangShortcut(LangID: Word): AnsiString;
var
  i: Integer;
begin
  Result := '';
  for i := Low(LangCodes) to High(LangCodes) do
    if LangID = LangCodes[i] then
    begin
      Result := LangShortcuts[i];
      Exit;
    end;
  for i := Low(LangCodes) to High(LangCodes) do
    if high(LangID) = LangCodes[i] then
    begin
      Result := LangShortcuts[i];
      Exit;
    end;
end;

function LangShortCutToLangShortID(LangShortcut: AnsiString): Word;
var
  i: Integer;
begin
  Result := LANG_ENGLISH or SUBLANG_ENGLISH_US shl 10;

  for i := Low(LangShortcuts) to High(LangShortcuts) do
    if AnsiCompareText(LangShortcut, LangShortcuts[i]) = 0 then
    begin
      Result := LangCodes[i];
      Exit;
    end;

  // second try, primary language code only
  LangShortcut := copy(LangShortcut, 1, 2);
  for i := Low(LangShortcuts) to High(LangShortcuts) do
    if AnsiCompareText(LangShortcut, Copy(LangShortcuts[i], 1, 2)) = 0 then
    begin
      Result := LangCodes[i];
      Exit;
    end;
end;

function GetLanguageName(LangID: WORD): String;
var
  Buf: array [0 .. 79] of Char;
begin
  VerLanguageName(LangID, @Buf, SizeOf(Buf) - 1);
  Result := Buf;
end;

{ ----------------------------------- End Common functions ---------------------------------- }

{ #####   TNHCustomDictionary   ##### }

constructor TNHCustomDictionary.Create;
begin
  DLLInitialize;

  FRefCount := 0;
  FActive := False;
  FLoaded := False;
  FDictionaryFilename := '';
  FID := '';
  FDisplayName := '';
  FLangID := LANG_SYSTEM_DEFAULT;
  FLanguageName := GetLanguageName(FLangID);
end;

destructor TNHCustomDictionary.Destroy;
begin
  Unload;
  inherited Destroy;
end;

function TNHCustomDictionary.Load: Boolean;
begin
  Result := false;
end;

procedure TNHCustomDictionary.Unload;
begin
  FLoaded := false;
end;

function TNHCustomDictionary.GetInternalFileID: String;
begin
  result := '';
end;


{ #####   TNHSpellDictionary   ##### }

constructor TNHSpellDictionary.Create;
begin
  inherited Create;
  FHunspellHandle := nil;
  FInternalAffixPath := '';
  FInternalDictPath := '';
end;

function TNHSpellDictionary.Load: Boolean;
var
  zip: TZipRead;
  intItem: Integer;
  tmpAffixData, tmpDictionaryData: RawByteString;
begin
  if not FLoaded then
  begin
    zip := TZipRead.Create(fDictionaryFilename);
    try
      intItem := zip.NameToIndex(fInternalAffixPath);
      if intItem > -1 then
      begin
        tmpAffixData := zip.Unzip(intItem);
        intItem := zip.NameToIndex(fInternalDictPath);
        if intItem > -1 then
        begin
          tmpDictionaryData := zip.Unzip(intItem);
          FLoaded := InitSpelling(PAnsiChar(tmpAffixData), length(tmpAffixData), PAnsiChar(tmpDictionaryData), length(tmpDictionaryData), '');
        end;
      end;
    finally
      zip.free;
    end;
  end;
  Result := FLoaded;
end;

procedure TNHSpellDictionary.Unload;
begin
  if (HunspellDll <> 0) and Assigned(FHunspellHandle) then
    HunspellFree(FHunspellHandle);

  FHunspellHandle := nil;
  FActive := False;
  FLoaded := False;
end;

function TNHSpellDictionary.GetInternalFileID: String;
begin
  result := Format('%s::%s::%s', [ExtractFileName(fDictionaryFileName), fInternalAffixPath, fInternalDictPath]);
end;

function TNHSpellDictionary.InitSpelling(const AffixBuffer: PAnsiChar; AffixBufferSize: Integer; const DictionaryBuffer: PAnsiChar;
  DictionaryBufferSize: Integer; key: PAnsiChar): Boolean;
begin
  if HunspellDll <> 0 then
    FHunspellHandle := HunspellInit(AffixBuffer, AffixBufferSize, DictionaryBuffer, DictionaryBufferSize, key);
  Result := Assigned(FHunspellHandle);
end;

{ InternalLoadSpellDirect - depricated!
  This function loads an uncompressed dictionary (until OO2) from file.
function TNHSpellDictionary.InternalLoadSpellDirect(const AffixFileName, DictionaryFileName: String; const key: AnsiString = ''): Boolean;
var
  tmpDictionary, tmpAffix: TMemoryStream;
begin
  Result := False;
  if Assigned(FHunspellHandle) then
    Unload;

  tmpDictionary := TMemoryStream.Create;
  try
    tmpDictionary.LoadFromFile(DictionaryFileName);

    FDisplayName := copy(ExtractFileName(DictionaryFileName), 1, length(DictionaryFileName) - length(ExtractFileExt(DictionaryFileName)));
    FID := '';
    FLangID := LANG_SYSTEM_DEFAULT;
    FLanguageName := GetLanguageName(FLangID);
    FVersion := '';

    tmpAffix := TMemoryStream.Create;
    try
      tmpAffix.LoadFromFile(AffixFileName);

      Result := InitSpelling(tmpAffix.Memory, tmpAffix.size, tmpDictionary.Memory, tmpDictionary.size, PAnsiChar(key));
    finally
      FreeAndNil(tmpAffix);
    end;
  finally
    FreeAndNil(tmpDictionary);
  end;
  FActive := Result;
end;
}

{ ----- Hunspell spelling and hyphenation functions ----- }

function TNHSpellDictionary.Spell(const Word: UnicodeString): Boolean;
begin
  if Assigned(FHunspellHandle) then
    Result := HunspellSpell(FHunspellHandle, PWideChar(Word)) <> 0
  else
    Result := False;
end;

procedure TNHSpellDictionary.Suggest(const Word: UnicodeString; AWordList: TUnicodeStrings);
var
  tmpWideCharPointerPointer: ^PWideChar;
begin
  if Assigned(FHunspellHandle) then
  begin
    tmpWideCharPointerPointer := HunspellSuggest(FHunspellHandle, PWideChar(Word));
    while tmpWideCharPointerPointer^ <> nil do
    begin
      AWordList.Add(tmpWideCharPointerPointer^);
      Inc(tmpWideCharPointerPointer);
    end;
  end;
end;

function TNHSpellDictionary.Add(const Word: UnicodeString): Boolean;
begin
  if Assigned(FHunspellHandle) then
    Result := HunspellAdd(FHunspellHandle, PWideChar(Word)) <> 0
  else
    Result := False;
end;

function TNHSpellDictionary.AddWithAffix(const Word, AffixModel: UnicodeString): Boolean;
begin
  if Assigned(FHunspellHandle) then
    Result := HunspellAddWithAffix(FHunspellHandle, PWideChar(Word), PWideChar(AffixModel)) <> 0
  else
    Result := False;
end;

procedure TNHSpellDictionary.Analyze(const Word: UnicodeString; AWordList: TUnicodeStrings);
var
  tmpWideCharPointerPointer: ^PWideChar;
begin
  if Assigned(FHunspellHandle) then
  begin
    tmpWideCharPointerPointer := HunspellAnalyze(FHunspellHandle, PWideChar(Word));
    while tmpWideCharPointerPointer^ <> nil do
    begin
      AWordList.Add(tmpWideCharPointerPointer^);
      Inc(tmpWideCharPointerPointer);
    end;
  end;
end;

procedure TNHSpellDictionary.Stem(const Word: UnicodeString; AWordList: TUnicodeStrings);
var
  tmpWideCharPointerPointer: ^PWideChar;
begin
  if Assigned(FHunspellHandle) then
  begin
    tmpWideCharPointerPointer := HunspellStem(FHunspellHandle, PWideChar(Word));
    while tmpWideCharPointerPointer^ <> nil do
    begin
      AWordList.Add(tmpWideCharPointerPointer^);
      Inc(tmpWideCharPointerPointer);
    end;
  end;
end;


{ #####   TNHHypenDictionary   ##### }

constructor TNHHyphenDictionary.Create;
begin
  inherited Create;
  FHyphenHandle := nil;
  FInternalHyphenPath := '';
end;

function TNHHyphenDictionary.Load: Boolean;
var
  zip: TZipRead;
  intItem: Integer;
  tmpHyphenData: RawByteString;
begin
  if not FLoaded then
  begin
    zip := TZipRead.Create(fDictionaryFilename);
    try
      intItem := zip.NameToIndex(fInternalHyphenPath);
      if intItem > -1 then
      begin
        tmpHyphenData := zip.Unzip(intItem);
        if HunspellDll <> 0 then
          FHyphenHandle := HyphenInit(PAnsiChar(tmpHyphenData), length(tmpHyphenData));
        FLoaded := FHyphenHandle <> nil;
      end;
    finally
      zip.free;
    end;
  end;
  Result := FLoaded;
end;

procedure TNHHyphenDictionary.Unload;
begin
  if (HunspellDll <> 0) and Assigned(FHyphenHandle) then
    HyphenFree(FHyphenHandle);

  FHyphenHandle := nil;
  FActive := False;
  FLoaded := False;
end;

function TNHHyphenDictionary.GetInternalFileID: String;
begin
  result := Format('%s::%s', [ExtractFileName(fDictionaryFileName), fInternalHyphenPath])
end;

{ InternalLoadHyphenDirect - depricated!
  This function loads an uncompressed dictionary (until OO2) from file.
function TNHHyphenDictionary.InternalLoadHyphenDirect(const HyphenFileName: String): Boolean;
var
  tmpHyphenData: TMemoryStream;
begin
  Result := False;

  if Assigned(FHyphenHandle) then
    Unload;

  tmpHyphenData := TMemoryStream.Create;
  try
    tmpHyphenData.LoadFromFile(HyphenFileName);
    Result := InitHyphenation(PAnsiChar(tmpHyphenData.Memory), tmpHyphenData.size);
  finally
    FreeAndNil(tmpHyphenData);
  end;
  FActive := Result;
end;
}

function TNHHyphenDictionary.InitHyphenation(const hyphenBuffer: PAnsiChar; hyphenBufferSize: Integer): Boolean;
begin
  if HunspellDll <> 0 then
    FHyphenHandle := HyphenInit(PAnsiChar(hyphenBuffer), hyphenBufferSize);
  Result := Assigned(FHyphenHandle);
end;


function TNHHyphenDictionary.Hyphenate(const Word: UnicodeString): UnicodeString;
begin
  if Active then
    Result := HyphenHyphenate(FHyphenHandle, PWideChar(Word)).hyphenatedWord
  else
    Result := Word;
end;

function TNHHyphenDictionary.Hyphenate(const Word: PWideChar): PHyphenResult;
begin
  if Active then
    Result := HyphenHyphenate(FHyphenHandle, Word)
  else
    Result := nil;
end;



{ #####   TNHunspell   ##### }

constructor TNHunspell.Create;
begin
  FSpellDictionaries := TStringList.create;
  FSpellDictionaries.Sorted := true;

  FHyphenDictionaries := TStringList.create;
  FHyphenDictionaries.Sorted := true;
end;

destructor TNHunspell.Destroy;
begin
  ClearDictionaries;
  FSpellDictionaries.free;
  FHyphenDictionaries.free;
  inherited Destroy;
end;

procedure TNHunspell.Dictionary_Addref(const AInternalID: String);
var
  tmpDict: TNHCustomDictionary;
begin
  tmpDict := FindSpellDictionary(AInternalID);
  if not assigned(tmpDict) then
    tmpDict := FindHyphenDictionary(AInternalID);
  if assigned(tmpDict) then
    inc(tmpDict.fRefCount);
end;

procedure TNHunspell.Dictionary_Release(const AInternalID: String);
var
  intIndex: integer;
  tmpDict: TNHCustomDictionary;
begin
  tmpDict := nil;

  for intIndex := 0 to fSpellDictionaries.Count - 1 do
    if GetSpellDictionary(intIndex).GetInternalFileID = AInternalID then
    begin
      tmpDict := GetSpellDictionary(intIndex);
      inc(tmpDict.fRefCount);
      if tmpDict.fRefCount < 1 then
        RemoveSpellDictionary(intIndex);
      break;
    end;

  if not assigned(tmpDict) then
    for intIndex := 0 to fHyphenDictionaries.Count - 1 do
      if GetHyphenDictionary(intIndex).GetInternalFileID = AInternalID then
      begin
        tmpDict := GetHyphenDictionary(intIndex);
        inc(tmpDict.fRefCount);
        if tmpDict.fRefCount < 1 then
          RemoveHyphenDictionary(intIndex);
        break;
      end;
end;

procedure TNHunspell.ClearDictionaries;
var
  intIndex: Integer;
begin
  for intIndex := 0 to FSpellDictionaries.count - 1 do
    TNHSpellDictionary(FSpellDictionaries.Objects[intIndex]).free;
  FSpellDictionaries.Clear;

  for intIndex := 0 to FHyphenDictionaries.count - 1 do
    TNHHyphenDictionary(FHyphenDictionaries.Objects[intIndex]).free;
  FHyphenDictionaries.Clear;
end;

procedure TNHunspell.RemoveSpellDictionary(index: integer);
begin
  TNHSpellDictionary(FSpellDictionaries.Objects[index]).free;
  FSpellDictionaries.Delete(index);
end;

procedure TNHunspell.RemoveHyphenDictionary(index: integer);
begin
  TNHHyphenDictionary(FHyphenDictionaries.Objects[index]).free;
  FHyphenDictionaries.Delete(index);
end;

function TNHunspell.GetSpellDictionaryCount: Integer;
begin
  result := fSpellDictionaries.Count;
end;

function TNHunspell.GetHyphenDictionaryCount: Integer;
begin
  result := fHyphenDictionaries.Count;
end;

function TNHunspell.GetSpellDictionary(Index: Integer): TNHSpellDictionary;
begin
  result := TNHSpellDictionary(fSpellDictionaries.Objects[Index]);
end;

function TNHunspell.GetHyphenDictionary(Index: Integer): TNHHyphenDictionary;
begin
  result := TNHHyphenDictionary(fHyphenDictionaries.Objects[Index]);
end;

function TNHunspell.FindSpellDictionary(const AInternalID: String): TNHSpellDictionary;
var
  intIndex: integer;
begin
  result := nil;
  for intIndex := 0 to fSpellDictionaries.Count - 1 do
    if GetSpellDictionary(intIndex).GetInternalFileID = AInternalID then
    begin
      result := GetSpellDictionary(intIndex);
      break;
    end;
end;

function TNHunspell.FindHyphenDictionary(const AInternalID: String): TNHHyphenDictionary;
var
  intIndex: integer;
begin
  result := nil;
  for intIndex := 0 to fHyphenDictionaries.Count - 1 do
    if GetHyphenDictionary(intIndex).GetInternalFileID = AInternalID then
    begin
      result := GetHyphenDictionary(intIndex);
      break;
    end;
end;


function TNHunspell.ReadFolder(FolderName: String): Boolean;
var
  FindResult: Integer;
  SearchRec: TSearchRec;
  intIndex: Integer;
begin
  Result := false;
  if copy(Foldername, length(Foldername), 1) <> '\' then Foldername := FolderName + '\';
  if DirectoryExists(FolderName) then
  begin
    FindResult := FindFirst(FolderName + '*.oxt', faAnyFile, SearchRec );
    while (FindResult = 0) do
    begin
      if ReadOXT( FolderName + SearchRec.Name ) then
        Result := true;
      FindResult := FindNext( SearchRec );
    end;
    FindClose( SearchRec );
  end;
end;

function TNHunspell.ReadOXT(const Filename: String): Boolean;
type
  TDictType = (dtNone, dtSpell, dtHyph, dtThes);
var
  zip: TZipRead;
  intItem: Integer;
  aNode: TOxtNode;

  tmpDictionariesXcu: Ansistring;

  aList: TList;
  tmpNodeName: Widestring;
  tmpLocations: WideString;
  tmpLangID: WORD;
  tmpVersion, tmpID, tmpDisplayName, tmpPublisher: Widestring;
  tmpDictType: TDictType;

  NewSpellDictionary: TNHSpellDictionary;
  NewHyphenDictionary: TNHHyphenDictionary;

  tmpDictionaryPath: WideString;
  tmpAffixPath: WideString;
  tmpHyphenPath: WideString;
begin
  Result := False;

  zip := TZipRead.Create(Filename);
  try
    intItem := zip.NameToIndex('description.xml');
    if intItem > -1 then
      InternalReadDescription(zip.Unzip(intItem), tmpVersion, tmpID, tmpDisplayName, tmpPublisher, true);

    tmpDictionariesXcu := '';
    intItem := zip.NameToIndex('META-INF\manifest.xml');
    if intItem > -1 then
      with TOxtParser.Create(zip.Unzip(intItem)) do
      try
        aNode := GetFirst;
        while aNode <> nil do
        begin
          tmpDictionariesXcu := aNode.Attrib('manifest:full-path');
          if tmpDictionariesXcu <> '' then
            break
          else
            aNode := GetNext(aNode);
        end;
      finally
        free;
      end;

    intItem := zip.NameToIndex(StringReplace(tmpDictionariesXcu, '/', '\', [rfReplaceAll]));
    if intItem > -1 then
      with TOxtParser.Create(zip.Unzip(intItem)) do
      try
        aList := TList.Create;
        try
          aNode := GetFirst;

          while aNode <> nil do
          begin
            if (Lowercase(aNode.Attrib('oor:op')) = 'fuse') then
            begin
              aList.clear;
              aNode.NodesByName('value', aList);
              tmpDictType := dtNone;
              tmpLocations := '';

              for intItem := 0 to aList.count - 1 do
                if Assigned(TOxtNode(aList[intItem]).Parent) then
                begin
                  tmpNodeName := lowercase(TOxtNode(aList[intItem]).Parent.Attrib('oor:name'));

                  if (tmpNodeName = 'locations') then
                    tmpLocations := TOxtNode(aList[intItem]).Value
                  else if (tmpNodeName = 'locales') then
                    tmpLangID := LangShortCutToLangShortID(TOxtNode(aList[intItem]).Value) // implicit wide->ansi conversion
                  else if (tmpNodeName = 'format') then
                    if (UpperCase(TOxtNode(aList[intItem]).Value) = 'DICT_SPELL') then
                      tmpDictType := dtSpell
                    else if (UpperCase(TOxtNode(aList[intItem]).Value) = 'DICT_HYPH') then
                      tmpDictType := dtHyph
                    else if (UpperCase(TOxtNode(aList[intItem]).Value) = 'DICT_THES') then
                      tmpDictType := dtThes;
                end;

              if (tmpLocations <> '') then
              case tmpDictType of
              dtSpell:
                begin
                  tmpAffixPath := Trim(Copy(tmpLocations, 1, Pos(' ', tmpLocations) - 1));
                  tmpAffixPath := StringReplace(tmpAffixPath, '/', '\', [rfReplaceAll]);
                  tmpAffixPath := StringReplace(tmpAffixPath, '%origin%\', '', [rfReplaceAll, rfIgnoreCase]);

                  tmpDictionaryPath := Trim(copy(tmpLocations, pos(' ', tmpLocations) + 1, 255));
                  tmpDictionaryPath := StringReplace(tmpDictionaryPath, '/', '\', [rfReplaceAll]);
                  tmpDictionaryPath := StringReplace(tmpDictionaryPath, '%origin%\', '', [rfReplaceAll, rfIgnoreCase]);

                  NewSpellDictionary := TNHSpellDictionary.create;
                  with NewSpellDictionary do
                  begin
                    fVersion := tmpVersion;
                    fID :=tmpID;
                    fPublisher := tmpPublisher;
                    fDisplayName := tmpDisplayName;
                    fLangID := tmpLangID;
                    FLanguageName := GetLanguageName(FLangID);
                    FDictionaryFilename := Filename;
                    FInternalAffixPath := tmpAffixPath;
                    FInternalDictPath := tmpDictionaryPath;
                  end;

                  if FindSpellDictionary(NewSpellDictionary.GetInternalFileID) = nil then
                    fSpellDictionaries.AddObject(Format('%s %s', [NewSpellDictionary.LanguageName, NewSpellDictionary.GetInternalFileID]), NewSpellDictionary)
                  else
                    NewSpellDictionary.free;

                  Result := true;
                end;
              dtHyph:
                begin
                  tmpHyphenPath := StringReplace(tmpLocations, '/', '\', [rfReplaceAll]);
                  tmpHyphenPath := StringReplace(tmpHyphenPath, '%origin%\', '', [rfReplaceAll, rfIgnoreCase]);

                  NewHyphenDictionary := TNHHyphenDictionary.create;
                  with NewHyphenDictionary do
                  begin
                    fVersion := tmpVersion;
                    fID :=tmpID;
                    fPublisher := tmpPublisher;
                    fDisplayName := tmpDisplayName;
                    fLangID := tmpLangID;
                    FLanguageName := GetLanguageName(FLangID);
                    FDictionaryFilename := Filename;
                    FInternalHyphenPath := tmpHyphenPath;
                  end;

                  if FindHyphenDictionary(NewHyphenDictionary.GetInternalFileID) = nil then
                    fHyphenDictionaries.AddObject(Format('%s %s', [NewHyphenDictionary.LanguageName, NewHyphenDictionary.GetInternalFileID]), NewHyphenDictionary)
                  else
                    NewHyphenDictionary.free;

                  Result := true;
                end;
              dtThes:
                begin
                  //not implemented
                end;
              end; {case}

              aNode := TOxtNode(aList[aList.count-1]); //proceed at last node
            end;
            aNode := GetNext(aNode);
          end;  {aNode <> nil}

        finally
          FreeAndNil(aList);
        end;
      finally
        Free;  {TOxtParser}
      end;

  finally
    FreeAndNil(zip);
  end;
end;

procedure TNHunspell.InternalReadDescription(const ADescription: RawByteString;
                                             var AVersion, AID, ADisplayName, APublisher: Widestring;
                                             const PreferLocalized: Boolean = False);
var
  aNode: TOxtNode;
  aList: TList;
  intItem: Integer;
begin
  with TOxtParser.Create(ADescription) do
    try
      aNode := Root.NodeByName('version');
      if Assigned(aNode) then
        AVersion := aNode.Attrib('value');

      aNode := Root.NodeByName('identifier');
      if Assigned(aNode) then
        AID := aNode.Attrib('value');

      ADisplayName := '';
      aList := TList.Create;
      try
        aNode := Root.NodeByName('display-name');
        if Assigned(aNode) then
        begin
          aNode.NodesByName('name', aList);
          for intItem := 0 to aList.count - 1 do
            with TOxtNode(aList[intItem]) do
              if (ADisplayName = '') or ((Attrib('lang') = cLangEN) and NOT PreferLocalized) or
                ((Attrib('lang') <> cLangEN) and PreferLocalized) then
                ADisplayName := Value;
        end;
      finally
        FreeAndNil(aList);
      end;

      APublisher := '';
      aNode := Root.NodeByName('publisher');
      if Assigned(aNode) then
        aNode := aNode.NodeByName('name');
      if Assigned(aNode) then
        APublisher := aNode.Value;

    finally
      Free;
    end;

end;

procedure TNHunspell.UpdateAndLoadDictionaries;
var
  intIndex: integer;
begin
  for intIndex := 0 to SpellDictionaryCount-1 do
    with SpellDictionaries[intIndex] do
      if (Active and not Loaded) then
        Load
      else if (not Active and Loaded) then
        Unload;

  for intIndex := 0 to HyphenDictionaryCount-1 do
    with HyphenDictionaries[intIndex] do
      if (Active and not Loaded) then
        Load
      else if (not Active and Loaded) then
        Unload;
end;


initialization
  Hunspell := TNHunspell.create;

finalization
  FreeAndNil(Hunspell);
  if HunspellDll <> 0 then
    FreeLibrary(HunspellDll);

end.
