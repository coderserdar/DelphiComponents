{   Unit cyStrUtils

    Description:
    Unit with string functions.

    $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
    $  €€€ Accept any PAYPAL DONATION $$$  €
    $      to: mauricio_box@yahoo.com      €
    €€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€

    * ***** BEGIN LICENSE BLOCK *****
    *
    * Version: MPL 1.1
    *
    * The contents of this file are subject to the Mozilla Public License Version
    * 1.1 (the "License"); you may not use this file except in compliance with the
    * License. You may obtain a copy of the License at http://www.mozilla.org/MPL/
    *
    * Software distributed under the License is distributed on an "AS IS" basis,
    * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
    * the specific language governing rights and limitations under the License.
    *
    * The Initial Developer of the Original Code is Mauricio
    * (https://sourceforge.net/projects/tcycomponents/).
    *
    * Donations: see Donation section on Description.txt
    *
    * Alternatively, the contents of this file may be used under the terms of
    * either the GNU General Public License Version 2 or later (the "GPL"), or the
    * GNU Lesser General Public License Version 2.1 or later (the "LGPL"), in which
    * case the provisions of the GPL or the LGPL are applicable instead of those
    * above. If you wish to allow use of your version of this file only under the
    * terms of either the GPL or the LGPL, and not to allow others to use your
    * version of this file under the terms of the MPL, indicate your decision by
    * deleting the provisions above and replace them with the notice and other
    * provisions required by the LGPL or the GPL. If you do not delete the
    * provisions above, a recipient may use your version of this file under the
    * terms of any one of the MPL, the GPL or the LGPL.
    *
    * ***** END LICENSE BLOCK *****}

unit cyStrUtils;

interface

uses
  classes,
  {$IFDEF UNICODE}
  SysUtils, Character;
  {$ELSE}
  SysUtils, Windows;   // Only if unicode it is important to not declare Windows in uses in order to compile in other platforms ...
  {$ENDIF}

type
  TStrLocateOption = (strloCaseInsensitive, strloPartialKey, strloPartialKeyRelativePos, strloAccentsInsensitive);
  TStrLocateOptions = set of TStrLocateOption;

  // Keep equivalent to TStrLocateOption !
  TStrFilterOption = (strfoCaseInsensitive, strfoPartialKey, strfoPartialKeyRelativePos, strfoAccentsInsensitive);
  TStrFilterOptions = set of TStrFilterOption;

  TStringRead = (srFromLeft, srFromRight);
  TStringReads = Set of TStringRead;
  TCaseSensitive = (csCaseSensitive, csCaseNotSensitive);
  TWordsOption = (woOnlyFirstWord, woOnlyFirstCar);
  TWordsOptions = Set of TWordsOption;
  TCarType = (ctAlphabeticUppercase, ctAlphabeticLowercase, ctNumeric, ctOther);
  TCarTypes = Set of TCarType;

{$IFDEF UNICODE}
  TUnicodeCategories = Set of TUnicodeCategory;

  USASCIIString = type AnsiString(20127);//20127 = us ascii
{$ENDIF}

  TMaskPartType = (mtUndefined, mtMaskPartStringValue, mtMaskPartCustomChar, mtAnyChar, mtAlphaNumChar, mtNumber, mtAlphaChar, mtUppLetter, mtLowLetter, mtOtherChar);



const
  CarTypeAlphabetic = [ctAlphabeticUppercase, ctAlphabeticLowercase];

  WebSiteLetters = ['/', ':', '.', '_', '-', '0'..'9', 'a'..'z', 'A'..'Z'];
  WebMailletters = ['a'..'z', '@', '.', '_', '-', '0'..'9', 'A'..'Z'];

{$IFDEF UNICODE}
  CharCategoriesNumbers = [TUnicodeCategory.ucDecimalNumber];
  CharCategoriesLetters = [TUnicodeCategory.ucLowercaseLetter, TUnicodeCategory.ucUppercaseLetter];
  CharCategoriesLetters_Space = [TUnicodeCategory.ucLowercaseLetter, TUnicodeCategory.ucUppercaseLetter, TUnicodeCategory.ucSpaceSeparator];
{$ENDIF}

  // Mask :
  cQuoteChar = '''';

  MaskPrefixOptional   = '%';   // Prefix with %% for optional and if any, must be removed on merge
  MaskBeginCustomChars = '[';
  MaskEndCustomChars   = ']';
  MaskAnyChar          = '*';
  MaskAlphaNumChar     = 'X';
  MaskNumber           = '0';
  MaskoptionalNumber   = '9';
  MaskAlphaChar        = 'A';
  MaskUppercaseChar    = 'U';
  MaskLowercaseChar    = 'L';
  MaskOtherChar        = 'S';

  MaskRulesMsg =
   '% - Prefix any rule to mark it as Optional'                                  + #13#10 +
   '''Hello''   - string ''Hello'' (quoted) expected'                            + #13#10 +
   '[;,-]  - Custom chars set ";,-" expected, first one will be used as default' + #13#10 +
   '%      - If first char on custom chars set, it deactivate default'           + #13#10 +
   '%%     - If on custom chars set, it remove char'                             + #13#10 +
   '0      - Number expected'                                                    + #13#10 +
   '9      - Number expected. 0 will be added if no number found.'               + #13#10 +
   'A      - Alphabetic expected'                                                + #13#10 +
   'U      - Uppercase letter expected'                                          + #13#10 +
   'L      - Lowercase letter expected'                                          + #13#10 +
   'X      - Alpha numeric expected'                                             + #13#10 +
   'S      - Other char expected'                                                + #13#10 +
   '*      - Any char';


// *** Char funtions ***//
function Char_GetType(aChar: Char): TCarType;

// *** SubString functions *** //
  { English: Substring definition:
    A string can be defined by substrings that are separated by a specified caracter.
    We admit that if a string = '', it has a single substring element with '' value.
    So, Substring count is 1 or superior.
    The first Substring element on a string has index 1, the second has index 2 and so on.

    Exemple (';' is the substring separator) :
    Exemple: Str = ''                 1 empty substring
    Exemple: Str = 'xxx'              1 substring with 'xxx' value
    Exemple: Str = ';'                2 empty substrings
    Exemple: Str = 'xxx;'             2 substrings ('xxx' and '')
    Exemple: Str = ';xxx'             2 substrings ('' and 'xxx')
    Exemple: Str = 'xxx;yyy'          2 substrings ('xxx' and 'yyy')
  }

  { Français: Definition d' une sous-chaine, appelé élément:
    Une chaine de caractères peut être définit de plusieurs éléments séparés par un caractère définit.
    On admet que si une string = '', celle-ci contient un seul élément appelé subString de valeur ''.
    Donc, le nombre d' éléments ne peut être inférieur à 1.
    Le 1er élément dans une string a pour Index 1, le second a pour Index 2 etc ...

    Exemple (lorsque ';' est le séparateurs des éléments) :
    Si Str = ''                 on a 1 élément vide
    Si Str = 'xxx'              on a 1 élément de valeur 'xxx'
    Si Str = ';'                on a 2 éléments vide
    Si Str = 'xxx;'             on a 2 éléments  de valeur 'xxx' et ''
    Si Str = ';xxx'             on a 2 éléments  de valeur '' et 'xxx'
    Si Str = 'xxx;yyy'          on a 2 éléments  de valeur 'xxx' et 'yyy'
  }

  function SubString_Count(const Str: String; const Separator: Char): Integer;
  // English: Retrieve the number of substring
  // Français: La fonction renvoie le nombre d' éléments dans Str

  function SubString_AtPos(const Str: String; const Separator: Char; const SubStringIndex: Word): Integer;
  // English: Retrieve the substring (specified by its index) position on the string
  // Français: Renvoie la position du 1er caractère de l' élément d' indexe SubStringIndex

  function SubString_Get(const Str: String; const Separator: Char; const SubStringIndex: Word): String;
  // English: Retrieve substring element by index
  // Français: Renvoie l' élément d' indexe SubStringIndex

  function SubString_Length(const Str: String; const Separator: Char; const SubStringIndex: Word): Integer;
  // English: Retrieve the number of caracters of the substring specified by SubStringIndex
  // Français: Renvoie le nombre de caractères de l' élément d' indexe SubStringIndex

  procedure SubString_Add(var Str: String; const Separator: Char; const Value: String);
  // English: Add a substring at the end. For the first substring, use str := 'Value'
  // Français: Ajoute un élément à la fin de la string. Pour le 1er élément, utilisez Str := 'exemple'

  procedure SubString_Insert(var Str: String; const Separator: Char; const SubStringIndex: Word; const Value: String);
  // English: Insert a substring at SubStringIndex position
  // Français: Insère un élément à la position SubStringIndex

  procedure SubString_Edit(Var Str: String; const Separator: Char; const SubStringIndex: Word; const NewValue: String);
  // English: Modify/initialize the specified substring at SubStringIndex position (don' t need to already exists)
  // Français: Modifie/initialise l' élément définit par SubStringIndex.

  function SubString_Remove(var Str: string; const Separator: Char; const SubStringIndex: Word): Boolean;
  // English: Delete the specified substring
  // Français: Élimine l' élément à l' index SubStringIndex

  function SubString_Locate(Str: string; const Separator: Char; SubString: String; const Options: TStrLocateOptions): Integer;
  // English: Retrieve SubString index
  // Français: Renvoie la position de l' élément spécifié

  function SubString_Ribbon(const Str: string; const Separator: Char; const Current: Word; const MoveBy: Integer): Integer; overload;
  // English: Retrieve substring index from a current substring and moving by MoveBy.
  // If there's no more substrings, it continues to move from the first one.
  // Français: Renvoie la position d' un élément par rapport à un autre en ce déplaceant de "MoveBy" éléments.
  // Si on se retrouve à la fin, on revient au début et vice versa.

  function SubString_Ribbon(const Str: string; const Separator: Char; const Current: String; const MoveBy: Integer): String; overload;
  // English: Like prior but SubString can be specified by its value
  // Français: comme la fonction antérieure, si ce n' est que l' élément est cette fois définit par sa valeur



// *** String functions *** //
  function String_Quote(const Str: String): String;

  function String_GetLineCount(const Str: String): integer;

  function String_GetLine(const Str: String; const Line: Word; const ReturnIfNotExists: string = ''): String;

  procedure String_SetLine(var InOutStr: String; const Line: Word; const LineValue: string);

  function String_GetLinesFromRange(const Str: String; const FirstLine, LastLine: Word; const ReturnIfNotExists: string = ''): String;

  function String_GetCar(const Str: String; const Position: Word; const ReturnCarIfNotExists: Char): Char;
  // English: Returns caracter at specified position
  // Français: Renvoie le caractère à la position "Position"

  function String_ExtractCars(const fromStr: String; const CarTypes: TCarTypes; const IncludeCars, ExcludeCars: String): String; {$IFDEF UNICODE} overload; {$ENDIF}

  {$IFDEF UNICODE}
  function String_ExtractCars(const Str: String; const CharCategories: TUnicodeCategories): String; overload;
  // English: Returns caracters of specified categories (numbers, letters etc ...)
  // Français: Renvoie les types de caractères spécifié avec CharCategories (nombres, lettres etc ...)
  {$ENDIF}

  function String_GetWord(const Str: String; const StringRead: TStringRead): String;
  // English: Returns a string with any word found on the string
  // Français: Renvoie une string qui peut être convertie en type Word si le résultat de la fonction n' est pas vide

  function String_GetInteger(const Str: String; const StringRead: TStringRead): String;
  // English: Returns a string with any integer found on the string
  // Français: Renvoie une string qui peut être convertie en type Integer si le résultat de la fonction n' est pas vide

  function String_ToInt(const Str: String; const IfCantConvert: Integer = 0): Integer;
  // English: Always convert a string to an integer, returns IfCantConvert if not valid integer
  // Français: Converti une String en Integer si possible, sinon renvoie 0

  function TryString_ToFloat(Str: String; var RsltValue: Extended): Boolean;

  function String_ToFloat(const Str: String; const IfCantConvert: Extended = 0): Extended;

  function String_Uppercase(Str: String; const Options: TWordsOptions) : String;
  // English: Uppercase string by Options
  // Français: Renvoie la string en majuscule selon Options (1ère lettre de string, 1ère lettre de chaque mot ou toute la string)

  function String_Lowercase(const Str: String; const Options: TWordsOptions): String;
  // English: Lowercase string by Options
  // Français: Renvoie la string en minuscule selon Options (1ère lettre de string, 1ère lettre de chaque mot ou toute la string)

  function String_Reverse(const Str: String): String;
  // English: Revert a string, String_Reverse('Hello ') returns 'olleH'
  // Français: Renvoie l' ordre des caratères inversé de la string

  function String_Pos(SubStr: String; Str: String; fromPos: Integer; const CaseSensitive: TCaseSensitive): Integer; overload;
  // English: Retrieve subString position in string since fromPos position
// Français: Renvoie la position d' une substring depuis une certaine position

  function String_Pos(SubStr: String; Str: String; const StringRead: TStringRead; const Occurrence: Word; const CaseSensitive: TCaseSensitive): Integer; overload;
  // English: Retrieve subString position
  // Français: Renvoie la position d' une substring selon son ocurrence

  function String_Copy(const Str: String; const fromIndex: Integer; toIndex: Integer; const StringRead: TStringRead = srFromLeft): String; overload;
  // English: Copy caracters in a string from fromIndex to toIndex
  // Français: Renvoie la copie d' une string selon la position du 1er et dernier caractère

  function String_Copy(const Str: String; const StringRead: TStringRead; const UntilFind: String; const _Inclusive: Boolean = false): String; overload;
  // English: Copy caraters in a string until UntilFind found
  // Français: Renvoie la copie d' une string tant qu' elle ne trouve pas UntilFind

  function String_Copy(const Str: String; Between1: String; const Between1MustExist: Boolean; Between2: String; const Between2MustExist: Boolean; const CaseSensitive: TCaseSensitive): String; overload;
  // English: Retrieve caracters between 2 strings
  // Français: Renvoie les caractères d' uyne string entre 2 substrings.

  function String_Delete(const Str: String; const fromIndex, toIndex: Integer): String; overload;
  // English: Returns a string cutted from fromIndex to to Index
  // Français: Élimine les caractères d' une string entre la position du 1er et dernier caractère spécifié

  function String_Delete(const Str: String; const delStr: String; const CaseSensitive: TCaseSensitive): String; overload;
  // English: Returns a string Removing substrings specified in delStr
  // Français: Renvoie la string après avoir retiré toute SubString specifiée avec delStr

  function String_BoundsCut(const aStr: String; const CutCar: Char; const Bounds: TStringReads): String;
  // English: Remove specified caracter from string bounds
  // Français: Permet de retirer un caractère tant que la string commence ou fini para celui-ci.

  function String_BoundsAdd(const Str: String; const AddCar: Char; const ReturnLength: Integer): String;
  // English: Add specified caracter until string length is equal to ReturnLength
  // Français: Permet d' ajouter un caractère au début ou/et à la fin d' une string jusqu' à ce que la string se retrouve de la taille de "ReturnLength" caractères

  function String_Add(const Str: String; const StringRead: TStringRead; const aCar: Char; const ReturnLength: Integer) : String;
  // English: Add specified caracter at the beginning/end until string length is equal to ReturnLength
  // Français: Permet d' ajouter un caractère au début ou à la fin d' une string jusqu' à ce que la string se retrouve de la taille de "ReturnLength" caractères

  function String_End(const Str: String; const Cars: Word): String;
  // English: Get last string caracters
  // Français: Renvoie les derniers caractères d' une string

  function String_SubstFast(const OldStr, NewStr: String; var Str: String): Integer;
  // English: Like StringReplace() but more rapid
  // Français: Fonction identique à StringReplace mais plus rapide.

  function String_Subst(OldStr: String; NewStr: String; Str: String; const CaseSensitive: TCaseSensitive = csCaseSensitive; AlwaysFindFromBeginning: Boolean = false): String; overload;
  // English: Like StringReplace() but with some parameters
  // Français: Fonction identique à StringReplace avec paramètres.
  // Permet cependant de toujours remplacer en recherchant depuis le début de la string. Utile lorsque New est contenu par OldStr

  function String_Subst(const OldStr, NewStr, Str: String; fromPosition, ToPosition: Word; const CaseSensitive: TCaseSensitive = csCaseSensitive; const AlwaysFindFromBeginning: Boolean = false): String; overload;

  function String_SubstCar(const Str: String; const Old, New: Char): String;
  // English: Replace a caracter by another
  // Français: Remplace un caractère par un autre dans une string

  function String_Count(Str, SubStr: String; const CaseSensitive: TCaseSensitive): Integer;
  // English: Retrieve the number of occurrences of SubStr
  // Français: Renvoie le nombre d' occurences d' une SubString

  function String_SameCars(Str1, Str2: String; const StopCount_IfDiferent: Boolean; const CaseSensitive: TCaseSensitive): Integer;
  // English: Retrieve number of identical caracters at the same position
  // Français: Compte le nombre de caractères identiques à la même position entre 2 strings

  function String_IsNumbers(const Str: String) : Boolean;
  // English: Returns True if the string contains only numeric caracters
  // Français: Permet de savoir si la string ne possède que des caractères de type chiffre.

  procedure String_RemoveChars(var aStr: String; const RemoveChars: string);

  function String_RemoveAccentsFromChars(const aStr: String): String;

  function String_LoadFromFile(const FromFile: string {$IFDEF UNICODE}; const aEncoding: TEncoding {$ENDIF}): string;
  procedure String_SaveToFile(const aStr, ToFile: string {$IFDEF UNICODE}; const aEncoding: TEncoding {$ENDIF});

  function SearchPos(const SubStr: String; const Str: String; MaxErrors: Integer): Integer;
  // English: Search a string into another if diference tolerence
  // Français: Recherche une chaîne de caractères à l' intérieur d' une autre avec une tolérence de diferences

  function String_MatchKeywordsInput(aStr, aKeywordsInput: String; const StrFilterOptions: TStrFilterOptions = [strfoCaseInsensitive, strfoPartialKeyRelativePos, strfoAccentsInsensitive]): Boolean;

  function String_MatchInput(aStr, aInput: String; const StrFilterOptions: TStrFilterOptions = [strfoCaseInsensitive, strfoPartialKeyRelativePos, strfoAccentsInsensitive]): Boolean;

  function String_MatchFilter(aStr, aFilter: String; const StrFilterOptions: TStrFilterOptions = [strfoCaseInsensitive, strfoPartialKeyRelativePos, strfoAccentsInsensitive]; const SomethingChar: Char = '%'): Boolean;

  function StringToCsvCell(const aStr: String; const SeparationChar: Char = ';'; const asText: Boolean = false): String;

  function GetCsvCellsContent(const fromCsvLine: String; Values: TStrings; const SeparationChar: Char = ';'): Integer;

  function isValidWebSiteChar(const aChar: Char): Boolean;

  function isValidWebMailChar(const aChar: Char): Boolean;

  function isValidwebSite(aStr: String): Boolean;

  function isValidWebMail(const aStr: String): Boolean;



// *** Mask functions ***

(*

 %       - Optional rule come next
 'Hello' - string 'Hello' (quoted) expected
 [;,-]   - Custom chars set ";,-" expected, first one will be used as default
 %       - Optional char for custom chars set
 %%      - If on custom chars set, it remove char
 0       - Number expected
 9       - Optional number expected
 A       - Alphabetic expected
 U       - Uppercase letter expected
 L       - Lowercase letter expected
 X       - Alpha numeric expected
 S       - Other char expected
 *       - Any char



*)

function GetNextMaskPart(var InOutMask, RsltMaskPartParamStr: string; var RsltMaskPartType: TMaskPartType; var RsltOptional: Boolean): Boolean;
function IsValidMask(const aMask: String): Boolean;
function MaskPartCount(aMask: string; var RsltOptionalCount: Integer): Integer;
function MaskCharsCount(aMask: string; var RsltOptionalCount: Integer): Integer;
function DetectPartialMatchMask(var InOutMask: string; const aString: string): Boolean;
function MaskPartTypeCompatible(const aMaskPartType, withMaskPartType: TMaskPartType): Boolean;
function IsMatchMask(var aMask: string; var aString: string): Boolean;
function IsMatchMask2(const aMask: string; const aString: string): Boolean;
function MergeMaskToMatchedString(var InOutMask: string; aString: string): String;
function MergeMaskToMatchedString2(const aMask: string; const aString: string): String;

implementation

function Char_GetType(aChar: Char): TCarType;
begin
  Result := ctOther;
  {$IFDEF UNICODE}
  if CharInSet(aChar, ['a'..'z', 'é', 'è', 'ê', 'ë', 'á', 'à', 'â', 'ä', 'ã', 'í', 'ì', 'î', 'ï', 'ó', 'ò', 'ô', 'ö', 'õ', 'ú', 'ù', 'û', 'ü']) then
  {$ELSE}
  if aChar in ['a'..'z', 'é', 'è', 'ê', 'ë', 'á', 'à', 'â', 'ä', 'ã', 'í', 'ì', 'î', 'ï', 'ó', 'ò', 'ô', 'ö', 'õ', 'ú', 'ù', 'û', 'ü'] then
  {$ENDIF}
    Result := ctAlphabeticLowercase
  else
    {$IFDEF UNICODE}
    if CharInSet(aChar, ['A'..'Z', 'É', 'È', 'Ê', 'Ë', 'Á', 'À', 'Â', 'Ä', 'Ã', 'Í', 'Ì', 'Î', 'Ï', 'Ó', 'Ò', 'Ô', 'Ö', 'Õ', 'Ú', 'Ù', 'Û', 'Ü']) then
    {$ELSE}
    if aChar in ['A'..'Z', 'É', 'È', 'Ê', 'Ë', 'Á', 'À', 'Â', 'Ä', 'Ã', 'Í', 'Ì', 'Î', 'Ï', 'Ó', 'Ò', 'Ô', 'Ö', 'Õ', 'Ú', 'Ù', 'Û', 'Ü'] then
    {$ENDIF}
      Result := ctAlphabeticUppercase
    else
      {$IFDEF UNICODE}
      if CharInSet(aChar, ['0'..'9']) then
      {$ELSE}
      if aChar in ['0'..'9'] then
      {$ENDIF}
        Result := ctNumeric;
end;

function SubString_Count(const Str: String; const Separator: Char): Integer;
var i, nbCars: Integer;
begin
  Result := 1;
  nbCars := length(Str);

  for i := 1 to nbCars do
    if Str[i] = Separator
    then Result := Result + 1;
end;

function SubString_AtPos(const Str: String; const Separator: Char; const SubStringIndex: Word): Integer;
var
  nbCars, curSubStringIndex, i: Integer;
begin
  Result := 0;
  if SubStringIndex = 0 then raise ERangeError.Create('SubStringIndex = 0!');

  nbCars := length(Str);
  curSubStringIndex := 1;
  i := 0;

  while (curSubStringIndex <> SubStringIndex) and (i < nbCars) do
  begin
    i := i + 1;

    if Str[i] = Separator
    then curSubStringIndex := curSubStringIndex + 1;
  end;

  if curSubStringIndex = SubStringIndex
  then Result := i + 1;    // 'Sauter' le separateur ou i = 0...
end;

function SubString_Get(const Str: String; const Separator: Char; const SubStringIndex: Word): String;
var
  nbCars, i: Integer;
begin
  Result := '';
  i := SubString_AtPos(Str, Separator, SubStringIndex);

  if i <> 0 then     // Substring exists
  begin
    nbCars := length(Str);

    while i <= nbCars do
    begin
      if Str[i] <> Separator
      then Result := Result + Str[i]
      else i := nbCars;

      i := i + 1;
    end;
  end;
end;

function SubString_Length(const Str: String; const Separator: Char; const SubStringIndex: Word): Integer;
begin
  Result := Length(SubString_Get(Str, Separator, SubStringIndex));
end;

procedure SubString_Add(var Str: String; const Separator: Char; const Value: String);
begin
  Str := Str + Separator + Value;
end;

procedure SubString_Insert(var Str: String; const Separator: Char; const SubStringIndex: Word; const Value: String);
var
  i, SubStrCount: Integer;
begin
  SubStrCount := SubString_Count(Str, Separator);

  if (SubStringIndex > 0) and (SubStringIndex <= SubStrCount)
  then begin
    i := SubString_AtPos(Str, Separator, SubStringIndex);
    Insert(Value + Separator, Str, i);
  end
  else
    raise ERangeError.CreateFmt('%d is not within the valid range of %d..%d', [SubStringIndex, 1, SubStrCount]);
end;

procedure SubString_Edit(var Str: string; const Separator: Char; const SubStringIndex: Word; const NewValue: string);
var
  i, SubStrCount, nbCars: Integer;
begin
  i := SubString_AtPos(Str, Separator, SubStringIndex);

  if i = 0          // Substring does not exists ...
  then begin
    SubStrCount := SubString_Count(Str, Separator);

    while SubStrCount < SubStringIndex do
    begin
      Str := Str + Separator;          // Add empty substring
      SubStrCount := SubStrCount + 1;
    end;

    Str := Str + NewValue;
  end
  else begin
    nbCars := length(Str);
    // Remove current  value :
    while i <= nbCars do
      if Str[i] <> Separator
      then begin
        Delete(Str, i, 1);
        nbCars := nbCars - 1;
      end
      else
        nbCars := 0;   // End of the substring

    // Insert new substring value :
    Insert(NewValue, Str, i);
  end;
end;

function SubString_Remove(var Str: string; const Separator: Char; const SubStringIndex: Word): Boolean;
var
  SubStringStartPos, nbCars: Integer;
begin
  Result := false;
  SubStringStartPos := SubString_AtPos(Str, Separator, SubStringIndex);

  if SubStringStartPos <> 0 then
  begin
    Result := True;
    nbCars := length(Str);

    // Remove all chars of SubStringIndex :
    while SubStringStartPos <= nbCars do
    begin
      if Str[SubStringStartPos] <> Separator
      then nbCars := nbCars - 1
      else nbCars := 0;             // end of the substring

      Delete(Str, SubStringStartPos, 1);
    end;

    // 2017-05-08 Remove separator:
    Delete(Str, SubStringStartPos-1, 1);
  end;
end;

function SubString_Locate(Str: string; const Separator: Char; SubString: String; const Options: TStrLocateOptions): Integer;
var
  SubStrCount, CurSubStrIndex: Integer;
  FindPartialKey, FindPartialKeyRelativePos, FindCaseInsensitive, FindAccentsInsensitive: Boolean;
begin
  Result := 0;
  CurSubStrIndex := 1;
  SubStrCount := SubString_Count(Str, Separator);
  FindPartialKey := strloPartialKey in Options;
  FindPartialKeyRelativePos := strloPartialKeyRelativePos in Options;
  FindCaseInsensitive := strloCaseInsensitive in Options;
  FindAccentsInsensitive := strloAccentsInsensitive in Options;

  if FindCaseInsensitive then
  begin
    Str := AnsiUpperCase(Str);
    SubString := AnsiUpperCase(SubString);
  end;

  if FindAccentsInsensitive then
  begin
    Str := String_RemoveAccentsFromChars(Str);
    SubString := String_RemoveAccentsFromChars(SubString);
  end;

  while (Result = 0) and (CurSubStrIndex <= SubStrCount) do
  begin
    if FindPartialKey then
    begin
      if Pos(SubString, SubString_Get(Str, Separator, CurSubStrIndex)) = 1 then
        Result := CurSubStrIndex;
    end
    else
      if FindPartialKeyRelativePos then
      begin
        if Pos(SubString, SubString_Get(Str, Separator, CurSubStrIndex)) <> 0 then
          Result := CurSubStrIndex;
      end
      else
        if SubString = SubString_Get(Str, Separator, CurSubStrIndex) then
          Result := CurSubStrIndex;

    inc(CurSubStrIndex, 1);
  end;
end;

function SubString_Ribbon(const Str: string; const Separator: Char; const Current: Word; const MoveBy: Integer): Integer;
var
  Count: Integer;
begin
  Count := SubString_Count(Str, Separator);

  Result := Current + MoveBy;

  if Result > 0
  then begin
    while Result > Count do
      Result := Result - Count;
  end
  else begin
    while Result <= 0 do
      Result := Result + Count;
  end;
end;

function SubString_Ribbon(const Str: string; const Separator: Char; const Current: String; const MoveBy: Integer): String;
var
  SubStringIndex: Integer;
begin
  SubStringIndex := SUBSTRING_LOCATE(Str, Separator, Current, []);

  if SubStringIndex = 0
  then SubStringIndex := 1
  else SubStringIndex := SubString_Ribbon(Str, Separator, SubStringIndex, MoveBy);

  Result := SubString_Get(Str, Separator, SubStringIndex);
end;

function String_Quote(const Str: String): String;
begin
  Result := '''' + Str + '''';
end;

function String_ExtractCars(const fromStr: String; const CarTypes: TCarTypes; const IncludeCars, ExcludeCars: String): String;

        function IncludeCar(aCar: Char): Boolean;
        var c: Integer;
        begin
          Result := false;
          for c := 1 to length(IncludeCars) do
            if IncludeCars[c] = aCar then
            begin
              Result := true;
              Break;
            end;
        end;

        function ExcludeCar(aCar: Char): Boolean;
        var c: Integer;
        begin
          Result := false;
          for c := 1 to length(ExcludeCars) do
            if ExcludeCars[c] = aCar then
            begin
              Result := true;
              Break;
            end;
        end;


var
  i: Integer;
  AddCar: Boolean;
begin
  Result := '';

  for i := 1 to length(fromStr) do
  begin
    AddCar := false;

    if IncludeCar(fromStr[i]) then
      AddCar := true
    else
      if not ExcludeCar(fromStr[i]) then
        if Char_GetType(fromStr[i]) in CarTypes then
          AddCar := true;

    if AddCar then
      Result := Result + fromStr[i];
  end;
end;

{$IFDEF UNICODE}
function String_ExtractCars(const Str: String; const CharCategories: TUnicodeCategories): String;
var
  I: Integer;
  UnicodeCategory: TUnicodeCategory;
begin
  Result := '';

  for i := 1 to Length(Str) do
  begin
    UnicodeCategory := GetUnicodeCategory(Str, I);
    if UnicodeCategory in CharCategories then
      Result := Result + Str[I];
  end;
end;
{$ENDIF}

function String_GetLineCount(const Str: String): integer;
begin
  if Str = ''
  then Result := 0
  else Result := String_Count(Str, #$D#$A, csCaseSensitive) + 1;
end;

function String_GetLine(const Str: String; const Line: Word; const ReturnIfNotExists: string = ''): String;
var
  CurrentLine, i: Integer;
begin
  Result := '';
  CurrentLine := 1;

  for i := 1 to length(Str) do
    if Str[i] = #$D then        // New line ...
    begin
      Inc(CurrentLine);

      if CurrentLine > Line then
        Break;
    end
    else
      if CurrentLine = Line then
        if Str[i] <> #$A then
          Result := Result + Str[i];

  if CurrentLine < Line then
    Result := ReturnIfNotExists;
end;

procedure String_SetLine(var InOutStr: String; const Line: Word; const LineValue: string);
var
  NewValue: String;
  LineReplaced: Boolean;
  CurrentLine, i: Integer;
begin
  NewValue := '';
  CurrentLine := 1;
  LineReplaced := False;

  for i := 1 to length(InOutStr) do
    if InOutStr[i] = #$D then        // New line ...
    begin
      Inc(CurrentLine);
      NewValue := NewValue + InOutStr[i];
    end
    else
      if (CurrentLine = Line) and (InOutStr[i] <> #$A) then
      begin
        if not LineReplaced then
        begin
          NewValue := NewValue + LineValue;
          LineReplaced := True;
        end;
      end
      else
        NewValue := NewValue + InOutStr[i];

  // Add lines to string :
  while not LineReplaced do
  begin
    NewValue := NewValue + #$D#$A;
    Inc(CurrentLine);

    if CurrentLine = Line then
    begin
      NewValue := NewValue + LineValue;
      LineReplaced := True;
    end;
  end;

  InOutStr := NewValue;
end;

function String_GetLinesFromRange(const Str: String; const FirstLine, LastLine: Word; const ReturnIfNotExists: string = ''): String;
var
  CurrentLine, i: Integer;
begin
  Result := '';
  CurrentLine := 1;

  for i := 1 to length(Str) do
    if Str[i] = #$D then        // New line ...
    begin
      // Keep "Enters" between lines :
      if Result <> '' then
        Result := Result + Str[i];

      Inc(CurrentLine);

      if (CurrentLine > LastLine) and (LastLine > 0) then
        Break;
    end
    else
      if CurrentLine >= FirstLine then
        if (CurrentLine <= LastLine) or (LastLine < 1) then
          if (Str[i] <> #$A) or (Result <> '') then      // Keep "Enters" between lines !
            Result := Result + Str[i];

  if CurrentLine < FirstLine then
    Result := ReturnIfNotExists;
end;

function String_GetCar(const Str: String; const Position: Word; const ReturnCarIfNotExists: Char): Char;
begin
  if Position in [1..Length(Str)] // Returns false if Length(Str) = 0
  then Result := Str[Position]
  else Result := ReturnCarIfNotExists;
end;

function String_ToInt(const Str: String; const IfCantConvert: Integer = 0): Integer;
begin
  if not TryStrToInt(Str, Result) then
    Result := IfCantConvert;
end;

function TryString_ToFloat(Str: String; var RsltValue: Extended): Boolean;
var
  ThousandSepar, DecSepar: String;
  i: Integer;
  fs: TFormatSettings;
begin
  // Remove spaces :
  String_substFast(' ', '', Str);

  ThousandSepar := '';
  DecSepar := '';

  for i := length(Str) downto 1 do
    {$IFDEF UNICODE}
    if CharInSet(Str[i], ['.', ',']) then
    {$ELSE}
    if (Str[i] in ['.', ',']) then
    {$ENDIF}
    begin
      if DecSepar = '' then
      begin
        if ThousandSepar = '' then
          DecSepar := Str[i];
      end
      else
        if DecSepar = Str[i] then   // Separator twice in Str !
        begin
          ThousandSepar := DecSepar;   // Finally, it' s not decimal separator but thousand separator !
          DecSepar := '';
        end
        else
          ThousandSepar := Str[i];
    end;

  // * Format settings ** //

  // Ignore thousand separator for conversion :
  fs.ThousandSeparator := ' ';

  if ThousandSepar <> '' then
    String_substFast(ThousandSepar, '', Str);

  // Change decimal separator on fs :
  if DecSepar <> '' then
    fs.DecimalSeparator := DecSepar[1];

  Result := TryStrToFloat(Str, RsltValue, fs);
end;

function String_ToFloat(const Str: String; const IfCantConvert: Extended = 0): Extended;
begin
  if not TryString_ToFloat(Str, Result) then
    Result := IfCantConvert;
end;

function String_Uppercase(Str: String; const Options: TWordsOptions) : String;
var
  i: Integer;
  b: Boolean;
begin
  if Str = ''
  then
    Result := ''
  else
    if woOnlyFirstCar in Options     // Word first letter
    then begin
      Str := AnsiLowerCase(Str);

      if woOnlyFirstWord in Options
      then
        Result := AnsiUpperCase(Str[1]) + Copy(Str, 2, Length(Str) -1)
      else begin
        b := True;
        Result := '';

        for i := 1 to Length(Str) do
        begin
          if b
          then Result := Result + AnsiUpperCase(Str[i])
          else Result := Result + Str[i];

          b := Str[i] = ' ';
        end;
      end;
    end
    else begin                       // All word
      Str := AnsiLowerCase(Str);

      if not (woOnlyFirstWord in Options)
      then
        Result := AnsiUpperCase(Str)
      else begin
        b := True;
        Result := '';

        for i := 1 to Length(Str) do
        begin
          if b
          then begin
            Result := Result + AnsiUpperCase(Str[i]);
            b := Str[i] <> ' ';
          end
          else
            Result := Result + Str[i];
        end;
      end
    end;
end;

function String_Lowercase(const Str : String; const Options: TWordsOptions): String;
var
  i: Integer;
  b: Boolean;
begin
  if Str = ''
  then
    Result := ''
  else
    if woOnlyFirstCar in Options     // Word first letter
    then begin
      if woOnlyFirstWord in Options
      then
        Result := AnsiLowerCase(Str[1]) + Copy(Str, 2, Length(Str) -1)
      else begin
        b := True;
        Result := '';

        for i := 1 to Length(Str) do
        begin
          if b
          then Result := Result + AnsiLowerCase(Str[i])
          else Result := Result + Str[i];

          b := Str[i] = ' ';
        end;
      end;
    end
    else begin                       // All word
      if not (woOnlyFirstWord in Options)
      then
        Result := AnsiLowerCase(Str)
      else begin
        b := True;
        Result := '';

        for i := 1 to Length(Str) do
        begin
          if b
          then begin
            Result := Result + AnsiLowerCase(Str[i]);
            b := Str[i] <> ' ';
          end
          else
            Result := Result + Str[i];
        end;
      end
    end;
end;

function String_Reverse(const Str: String): String;
var i: Integer;
begin
  Result := '';

  for i := 1 to Length(Str) do
    Result := Str[i] + Result;
end;

function String_Pos(SubStr: String; Str: String; fromPos: Integer; const CaseSensitive: TCaseSensitive): Integer;
begin
  if fromPos < 1 then fromPos := 1;   // From first char ...

  if CaseSensitive = csCaseNotSensitive then
  begin
    SubStr := AnsiUpperCase(SubStr);
    Str := AnsiUpperCase(Str);
  end;

  // Remove the beginning :
  Delete(Str, 1, fromPos - 1);

  // Get "relative" position :
  Result := pos(SubStr, Str);

  // Get absolute position :
  if Result <> 0 then
    Inc(Result, fromPos - 1);
end;

function String_Pos(SubStr: String; Str: String; const StringRead: TStringRead; const Occurrence: Word; const CaseSensitive: TCaseSensitive): Integer;
var
  CurCar, LengthStr, LengthSubStr, FoundCount: Integer;
begin
  Result := 0;

  if CaseSensitive = csCaseNotSensitive
  then begin
    Str  := AnsiUpperCase(Str);
    SubStr := AnsiUpperCase(SubStr);
  end;

  if (Occurrence > 0) and (SubStr <> '')
  then begin
    if StringRead = srFromRight
    then begin
      SubStr := String_Reverse(SubStr);
      Str  := String_Reverse(Str);
    end;

    FoundCount   := 0;
    CurCar       := 1;
    LengthStr    := Length(Str);
    LengthSubStr := Length(SubStr);

    while CurCar <= LengthStr do
    begin
      if Copy(Str, CurCar, LengthSubStr) = SubStr
      then begin
        FoundCount := FoundCount + 1;

        if FoundCount = Occurrence
        then begin
          if StringRead = srFromLeft
          then Result := CurCar
          else Result := LengthStr - CurCar + (2 - LengthSubStr);   // Calc correct position because of String reverse()

          CurCar := LengthStr;
        end
        else
          CurCar := CurCar + LengthSubStr;
      end
      else
        CurCar := CurCar + 1;
    end;
  end;
end;

function String_Copy(const Str: String; const fromIndex: Integer; toIndex: Integer; const StringRead: TStringRead = srFromLeft): String;
begin
  if StringRead = srFromRight then
  begin
    // 2016-09-15 !!! NOT WORKING !!! Result := Copy(Str, Length(Str) - fromIndex, (toIndex - fromIndex + 1));

    // Correct toIndex :
    if toIndex > Length(Str) then
      toIndex := Length(Str);

    // Calc StartPos from left with toIndex :
    Result := Copy(Str, Length(Str) - toIndex + 1, toIndex - fromIndex + 1);
  end
  else
    Result := Copy(Str, fromIndex, toIndex - fromIndex + 1);
end;

function String_Copy(const Str: String; const StringRead: TStringRead; const UntilFind: String; const _Inclusive: Boolean = false): String;
var
  x: Integer;
begin
  Result := '';

  if Pos(UntilFind, Str) > 0 then
    if StringRead = srFromLeft then
    begin
      x := Pos(UntilFind, Str);
      if _Inclusive then
        Inc(x, Length(UntilFind));
      Result := Copy(Str, 1, x - 1);
    end
    else begin
      x := String_Pos(UntilFind, Str, srFromRight, 1, csCaseSensitive);
      if not _Inclusive then
        Inc(x, Length(UntilFind));
      Result := Copy(Str, x, Length(Str));
    end;
end;

function String_Copy(const Str: String; Between1: String; const Between1MustExist: Boolean; Between2: String; const Between2MustExist: Boolean; const CaseSensitive: TCaseSensitive): String;
var
  posStr1, posStr2, StartPos, EndPos: Integer;
  WorkingStr: String;
begin
  Result := '';

  if Str <> '' then
  begin
    if CaseSensitive = csCaseNotSensitive then
    begin
      Between1 := AnsiUpperCase(Between1);
      Between2 := AnsiUpperCase(Between2);
      WorkingStr := AnsiUpperCase(Str);
    end
    else
      WorkingStr := Str;

    StartPos := 0;
    EndPos   := 0;
    posStr1  := pos(Between1, WorkingStr);

    if Between1 = Between2    // Locate the 2nd occurrence :
    then posStr2 := String_Pos(Between2, WorkingStr, srFromLeft, 2, csCaseNotSensitive)
    else posStr2 := String_Pos(Between2, WorkingStr, posStr1 + length(Between1), csCaseNotSensitive);

    if posStr1 = 0
    then begin
      if not Between1MustExist
      then StartPos := 1;
    end
    else
      StartPos := posStr1 + length(Between1);

    if posStr2 = 0
    then begin
      if not Between2MustExist
      then EndPos := Length(Str);
    end
    else
      EndPos := posStr2 - 1;

    if (StartPos <> 0) and (EndPos <> 0) then
      Result := String_Copy(Str, StartPos, EndPos);
  end;
end;

function String_BoundsCut(const aStr: String; const CutCar: Char; const Bounds: TStringReads): String;
var
  Cont: Boolean;
  NbCars: Integer;
begin
  Result := aStr;

  if srFromLeft in Bounds then
  begin
    Cont := true;
    NbCars := Length(Result);
    while (Cont) and (NbCars > 0) do
      if Result[1] = CutCar
      then begin
        Delete(Result, 1, 1);
        NbCars := nbCars - 1;
      end
      else
        Cont := false;
  end;

  if srFromRight in Bounds then
  begin
    Cont := true;
    NbCars := Length(Result);
    while (Cont) and (NbCars > 0) do
      if Result[NbCars] = CutCar
      then begin
        Delete(Result, NbCars, 1);
        NbCars := nbCars - 1;
      end
      else
        Cont := false;
  end;
end;

function String_BoundsAdd(const Str: String; const AddCar: Char; const ReturnLength: Integer): String;
var
  Orig, i, AddCarCount: Integer;
  ToTheRight: Boolean;
begin
  ToTheRight := True;
  Orig      := Length(Str);
  AddCarCount    := ReturnLength - Orig;
  Result    := Str;

  for i := 1 to AddCarCount do
  begin
    if ToTheRight
    then Result := Result + AddCar
    else Result := AddCar + Result;

    ToTheRight := not ToTheRight;
  end;
end;

function String_GetWord(const Str: String; const StringRead: TStringRead): String;
var
  I: Integer;
  Cont: Boolean;
begin
  Cont   := True;
  Result := '';

  if StringRead = srFromLeft then
  begin
    for i := 1 to Length(Str) do
      {$IFDEF UNICODE}
      if Cont and CharInSet(Str[i], ['0'..'9'])
      {$ELSE}
      if Cont and (Str[i] in ['0'..'9'])
      {$ENDIF}
      then Result := Result + Str[i]
      else Cont := Result = '';
  end
  else begin
    for i := Length(Str) downto 1 do
      {$IFDEF UNICODE}
      if Cont and CharInSet(Str[i], ['0'..'9'])
      {$ELSE}
      if Cont and (Str[i] in ['0'..'9'])
      {$ENDIF}
      then Result := Str[i] + Result
      else Cont := Result = '';
  end;
end;

function String_GetInteger(const Str: String; const StringRead: TStringRead): String;
var
  I: Integer;
  Cont: Boolean;
begin
  Cont   := True;
  Result := '';

  if StringRead = srFromLeft then
  begin
    for i := 1 to Length(Str) do
      {$IFDEF UNICODE}
      if Cont and CharInSet(Str[i], ['-', '0'..'9']) then
      {$ELSE}
      if Cont and (Str[i] in ['-', '0'..'9']) then
      {$ENDIF}
       begin
         if Str[i] = '-'
         then begin
           if (Result = '') and (i < length(Str)) then
           begin
            {$IFDEF UNICODE}
             if CharInSet(Str[i+1], ['0'..'9']) then // Next car is a number !!!
            {$ELSE}
             if Str[i+1] in ['0'..'9'] then // Next car is a number !!!
            {$ENDIF}
               Result := '-';
           end
           else
             Cont := false;
         end
         else
           Result := Result + Str[i];
       end
       else
         Cont := Result = '';
  end
  else begin
     for i := Length(Str) downto 1 do
      {$IFDEF UNICODE}
      if Cont and CharInSet(Str[i], ['-', '0'..'9']) then
      {$ELSE}
      if Cont and (Str[i] in ['-', '0'..'9']) then
      {$ENDIF}
       begin
         if Str[i] = '-'
         then begin
           if Result <> ''
           then begin
             Result := '-' + Result;
             Cont := false;
           end;
         end
         else
           Result := Str[i] + Result;
       end
       else
         Cont := Result = '';
  end;
end;

function String_Add(const Str: String; const StringRead: TStringRead; const aCar: Char; const ReturnLength: Integer) : String;
var NbCars: Integer;
begin
  NbCars := Length(Str);
  Result := Str;

  while NbCars < ReturnLength do
  begin
    if StringRead = srFromRight
    then Result := Result + aCar
    else Result := aCar + Result;

    NbCars := NbCars + 1;
  end;
end;

function String_End(const Str: String; const Cars: Word): String;
begin
  Result := Copy(Str, Length(Str) - Cars + 1, Cars);
end;

function String_Delete(const Str: String; const fromIndex, toIndex: Integer): String;
begin
  Result := Copy(Str, 1, fromIndex - 1) + Copy(Str, toIndex + 1, Length(Str) - toIndex);
end;

function String_Delete(const Str: String; const delStr: String; const CaseSensitive: TCaseSensitive): String;
begin
  Result := String_Subst(delStr, '', Str, CaseSensitive);
end;

function String_SubstFast(const OldStr, NewStr: String; var Str: String): Integer;
var
  LengthStr, LengthOldStr, lengthNewStr: Integer;
  SearchUntilCar, i, f: Integer;
  Match: Boolean;
begin
  Result := 0;

  if OldStr = '' then Exit;
  if OldStr = NewStr then Exit;

  LengthStr := Length(Str);
  LengthOldStr := Length(OldStr);
  lengthNewStr := Length(NewStr);

  i := 1;
  SearchUntilCar := (LengthStr - LengthOldStr) + 1;
  while i <= SearchUntilCar do
  begin
    if Str[i] = OldStr[1] then
    begin
      Match := true;
      for f := 2 to LengthOldStr do     // Search OldStr into Str ...
        if Str[f+i-1] <> OldStr[f] then
        begin
          Match := false;
          Break;
        end;

      if Match then
      begin
        Inc(Result);

        // Replace into Str :
        Delete(Str, i, LengthOldStr);

        if lengthNewStr <> 0 then
          Insert(NewStr, Str, i);

        // New limit :
        SearchUntilCar := SearchUntilCar + lengthNewStr - LengthOldStr;

        i := i + lengthNewStr - 1;  // Need to put cursor on last replaced char - 1 because of inc(i) ...
      end;  // Else we test since next char !
    end;

    inc(i);
  end;
end;

function String_Subst(OldStr: String; NewStr: String; Str: String; const CaseSensitive: TCaseSensitive = csCaseSensitive; AlwaysFindFromBeginning: Boolean = false): String;
var
  AnsiUpperCaseNewStr: String;
  LengthStr, LengthOldStr, lengthNewStr: Integer;
  SearchUntilCar, i, f: Integer;
  Match: Boolean;
begin
  Result := Str;

  if OldStr = '' then Exit;
  if OldStr = NewStr then Exit;

  LengthStr := Length(Str);
  LengthOldStr := Length(OldStr);
  lengthNewStr := Length(NewStr);

  if CaseSensitive = csCaseNotSensitive then
  begin
    Str := AnsiUpperCase(Str);
    OldStr := AnsiUpperCase(OldStr);

    AnsiUpperCaseNewStr := AnsiUpperCase(NewStr);

    if AlwaysFindFromBeginning then
      AlwaysFindFromBeginning := Pos(OldStr, AnsiUpperCaseNewStr) = 0;
  end
  else
    if AlwaysFindFromBeginning then
      AlwaysFindFromBeginning := Pos(OldStr, NewStr) = 0;


  i := 1;
  SearchUntilCar := (LengthStr - LengthOldStr) + 1;
  while i <= SearchUntilCar do
  begin
    if Str[i] = OldStr[1] then
    begin
      Match := true;
      for f := 2 to LengthOldStr do     // Search OldStr into Str ...
        if Str[f+i-1] <> OldStr[f] then
        begin
          Match := false;
          Break;
        end;

      if Match then
      begin
        // Replace into Result and Str :
        Delete(Result, i, LengthOldStr);
        Delete(Str, i, LengthOldStr);

        if lengthNewStr <> 0 then
        begin
          Insert(NewStr, Result, i);

          if CaseSensitive = csCaseNotSensitive
          then Insert(AnsiUpperCaseNewStr, Str, i)
          else Insert(NewStr, Str, i);
        end;

        // New limit :
        SearchUntilCar := SearchUntilCar + lengthNewStr - LengthOldStr;

        if AlwaysFindFromBeginning
        then i := i - LengthOldStr       // Go back
        else i := i + lengthNewStr - 1;  // Need to put cursor on last replaced char - 1 because of inc(i) ...

        if i < 0 then i := 0;
      end;
    end;

    inc(i);
  end;
end;

function String_Subst(const OldStr, NewStr, Str: String; fromPosition, ToPosition: Word; const CaseSensitive: TCaseSensitive = csCaseSensitive; const AlwaysFindFromBeginning: Boolean = false): String;
var
  Length_Str: Integer;
begin
  Length_Str := Length(Str);

  if fromPosition = 0 then
    fromPosition := 1;

  if ToPosition = 0 then
    ToPosition := Length_Str;

  Result := Copy(Str, 1, fromPosition-1)
            + String_Subst( OldStr, NewStr, Copy(Str, fromPosition, ToPosition), CaseSensitive, AlwaysFindFromBeginning )
            + Copy(Str, ToPosition + 1, Length_Str);
end;

function String_SubstCar(const Str: String; const Old, New: Char): String;
var
  LengthStr, i: Integer;
begin
  Result := Str;
  LengthStr := length(Str);

  for i := 1 to LengthStr do
    if Result[i] = Old then
      Result[i] := New;
end;

function String_Count(Str, SubStr: String; const CaseSensitive: TCaseSensitive): Integer;
var i, L_Str, L_SubStr: Integer;

   function SubStr_Na_Pos(_P: Integer): Boolean;
   begin
     Result := Copy(Str, _P, L_SubStr) = SubStr;
   end;

begin
  Result := 0;

  if SubStr <> '' then
  begin
    if CaseSensitive = csCaseNotSensitive then
    begin
      Str  := AnsiUpperCase(Str);
      SubStr := AnsiUpperCase(SubStr);
    end;

    L_Str    := Length(Str);
    L_SubStr := Length(SubStr);
    i := 1;

    while i <= L_Str do
       if SubStr_Na_Pos(i)
       then begin
         Result := Result + 1;
         i := i + L_SubStr;
       end
       else
         i := i + 1;
  end;
end;

function String_SameCars(Str1, Str2: String; const StopCount_IfDiferent: Boolean; const CaseSensitive: TCaseSensitive): Integer;
var i, MaxCars: Integer;
begin
  Result := 0;

  if CaseSensitive = csCaseNotSensitive
  then begin
    Str1 := AnsiUpperCase(Str1);
    Str2 := AnsiUpperCase(Str2);
  end;

  if Length(Str1) > Length(Str2)
  then MaxCars := Length(Str2)
  else MaxCars := Length(Str1);

  for i := 1 to MaxCars do
    if Str1[i] = Str2[i]
    then
      Result := Result + 1
    else
      if StopCount_IfDiferent
      then Break;
end;

function String_IsNumbers(const Str: String) : Boolean;
var i: Integer;
begin
  Result := true;

  if Str <> '' then
  begin
    for i := 1 to Length(Str) do
      {$IFDEF UNICODE}
      if not CharInSet(Str[i], ['0'..'9']) then
      {$ELSE}
      if not (Str[i] in ['0'..'9']) then
      {$ENDIF}
        Result := false;
  end
  else
    Result := false;
end;

function SearchPos(const SubStr: String; const Str: String; MaxErrors: Integer): Integer;
var
  i, p, LengthSubStr: Integer;
  ErrorCount: Integer;
begin
  Result := 0;
  LengthSubStr := Length(SubStr);

  // Navigate on Str searching for SubStr :
  for i := 1 to (Length(Str) - LengthSubStr) + 1 do
  begin
    ErrorCount := 0;

    // Compare all SubStr chars :
    for p := 1 to LengthSubStr do
      if SubStr[p] <> Str[i + p - 1] then
      begin
        Inc(ErrorCount);
        if ErrorCount > MaxErrors then
          Break;
      end;

    if ErrorCount <= MaxErrors then
    begin
      Result := i;
      MaxErrors := ErrorCount-1; // Try to locate with less errors ...
    end;
  end;
end;

procedure String_RemoveChars(var aStr: String; const RemoveChars: string);
var
  i, p: Integer;
begin
  for i := 1 to length(RemoveChars) do
  begin
    repeat
      p := pos(RemoveChars[i], aStr);

      if p <> 0 then
        Delete(aStr, p, 1);

    until p = 0;
  end;
end;

function String_RemoveAccentsFromChars(const aStr: String): String;
{$IFDEF UNICODE}
begin
  Result := String(USASCIIString(aStr));
end;
{$ELSE}
var
  aStrLength, StrFLength: Integer;
  StrF: String;
  s, f: Integer;
begin
  Result := '';

  // Calc FoldString result length (FoldString will isolate as char all accents) :
  StrFLength := Windows.FoldString(MAP_COMPOSITE, PChar(aStr), -1, nil, 0);
  SetLength(StrF, StrFLength);
  FoldString(MAP_COMPOSITE, PChar(aStr), -1, PChar(StrF), StrFLength);

  aStrLength := Length(aStr);
  s := 1;
  f := 1;
  while (s <= aStrLength) and (f <= StrFLength) do
  begin
    if aStr[s] = StrF[f] then
    begin
      Result := Result + StrF[f];
      inc(s);
    end
    else
      if not (StrF[f] in ['´', '`', '~', '^', '¨']) then
      begin
        Result := Result + StrF[f];
        inc(s);
      end;

    inc(f);  // Analyse next char
  end;
end;
{$ENDIF}

function String_LoadFromFile(const FromFile: string {$IFDEF UNICODE}; const aEncoding: TEncoding {$ENDIF}): string;
{$IFDEF UNICODE}
var
  StringStream: TStringStream;
begin
  StringStream := TStringStream.Create('', aEncoding);
  StringStream.LoadFromFile(FromFile);
  Result := StringStream.DataString;
  StringStream.Free;
end;
{$ELSE}
var
  FileStream: TFileStream;
  chr: AnsiChar;
begin
  Result := '';
  FileStream := TFileStream.Create(FromFile, fmOpenRead);
  try
    FileStream.Position := 0;
    // FileStream.Read(Result, FileStream.Size);

    while FileStream.Read(chr, 1) = 1 do
      Result := Result + chr;
  finally
    FileStream.Free;
  end;
end;
{$ENDIF}

procedure String_SaveToFile(const aStr, ToFile: string {$IFDEF UNICODE}; const aEncoding: TEncoding {$ENDIF});
{$IFDEF UNICODE}
var
  StringStream: TStringStream;
begin
  StringStream := TStringStream.Create(aStr, aEncoding);
  StringStream.SaveToFile(ToFile);
  StringStream.Free;
end;
{$ELSE}
var
  StringStream: TStringStream;
  FileStream: TFileStream;
begin
  StringStream := TStringStream.Create(aStr);

  FileStream := TFileStream.Create(ToFile, fmCreate);
  try
    if StringStream.Size <> 0 then
      FileStream.CopyFrom(StringStream, StringStream.Size);
  finally
    FileStream.Free;
  end;

  StringStream.Free;
end;
{$ENDIF}

function String_MatchKeywordsInput(aStr, aKeywordsInput: String; const StrFilterOptions: TStrFilterOptions = [strfoCaseInsensitive, strfoPartialKeyRelativePos, strfoAccentsInsensitive]): Boolean;
var
  KeyWord: string;
  W: Integer;
begin
  Result := true;

  if aKeywordsInput = '' then
    Exit;

  if aStr = '' then
  begin
    Result := false;
    Exit;
  end;

  for w := 1 to SubString_Count(aKeywordsInput, ' ') do
  begin
    KeyWord := SubString_Get(aKeywordsInput, ' ', w);
    if KeyWord <> '' then
      if not String_MatchInput(aStr, KeyWord, StrFilterOptions) then
      begin
        Result := false;
        Break;
      end;
  end;
end;

function String_MatchInput(aStr, aInput: String; const StrFilterOptions: TStrFilterOptions = [strfoCaseInsensitive, strfoPartialKeyRelativePos, strfoAccentsInsensitive]): Boolean;
var
  s: Integer;
  aWordInput: String;
begin
  // Initialisation //
  Result := true;

  if aInput = '' then
    Exit;

  if aStr = '' then
  begin
    Result := false;
    Exit;
  end;

  if strfoCaseInsensitive in StrFilterOptions then
  begin
    aStr := AnsiUpperCase(aStr);
    aInput := AnsiUpperCase(aInput);
  end;

  if strfoAccentsInsensitive in StrFilterOptions then
  begin
    aStr := String_RemoveAccentsFromChars(aStr);
    aInput := String_RemoveAccentsFromChars(aInput);
  end;

  // Traitement //
  if strfoPartialKey in StrFilterOptions then
  begin
    if pos(aInput, aStr) <> 1 then
      Result := false;
  end
  else
    if strfoPartialKeyRelativePos in StrFilterOptions then
    begin
      if pos(aInput, aStr) = 0 then
        Result := false;
    end
    else begin
      // find each word in aInput :
      for s := 1 to SubString_Count(aInput, ' ') do
      begin
        aWordInput := SubString_Get(aInput, ' ', s);

        if aWordInput <> '' then
          if pos(aWordInput, aStr) = 0 then
          begin
            Result := false;
            Break;
          end;
      end;
    end;
end;

function String_MatchFilter(aStr, aFilter: String; const StrFilterOptions: TStrFilterOptions = [strfoCaseInsensitive, strfoPartialKeyRelativePos, strfoAccentsInsensitive]; const SomethingChar: Char = '%'): Boolean;
var
  PortionMask: String;
  AnyPosition: Boolean;
  LengthMask, PortionPos, PortionLength: Integer;

      function GetPortionFilter: string;
      var
        c: integer;
      begin
        Result := '';
        for c := 1 to lengthMask do
          if aFilter[c] = SomethingChar
          then Break
          else Result := Result + aFilter[c];
      end;

begin
  Result := true;
  if aFilter = SomethingChar then Exit;
  if aFilter = '' then Exit;

  if strfoCaseInsensitive in StrFilterOptions then
  begin
    aStr := AnsiUpperCase(aStr);
    aFilter := AnsiUpperCase(aFilter);
  end;

  if strfoPartialKeyRelativePos in StrFilterOptions then
    if aFilter[1] <> SomethingChar then
      aFilter := SomethingChar + aFilter;

  LengthMask := Length(aFilter);
  AnyPosition := false;

  while (Result) and (LengthMask <> 0) do
    if aFilter[1] = SomethingChar then
    begin
      system.Delete(aFilter, 1, 1);
      LengthMask := LengthMask - 1;
      AnyPosition := true;
    end
    else begin
      PortionMask := GetPortionFilter;
      PortionLength := Length(PortionMask);

      if AnyPosition then
      begin
        AnyPosition := false;

        if LengthMask - PortionLength = 0 then // No more mask, so, aStr must end with PortionMask!
        begin
          Result := String_End(aStr, PortionLength) = PortionMask;
          // Result := RightStr(aStr, PortionLength) = PortionMask;
        end
        else begin
          PortionPos := Pos(PortionMask, aStr);
          Result := PortionPos <> 0;
        end;
      end
      else begin
        PortionPos := Pos(PortionMask, aStr);
        Result := PortionPos = 1;
      end;

      if Result then
      begin
        system.Delete(aFilter, 1, PortionLength);
        LengthMask := LengthMask - PortionLength;
        system.Delete(aStr, 1, PortionPos + PortionLength - 1);
      end;
    end;
end;

function StringToCsvCell(const aStr: String; const SeparationChar: Char = ';'; const asText: Boolean = false): String;
var
  AddQuote: Boolean;
begin
  Result := aStr;

  // Any string cell containing <"> or <;> chars must be quoted !
  AddQuote := pos(SeparationChar, aStr) <> 0;
  if not AddQuote then
    AddQuote := pos('"', aStr) <> 0;

  // Any <"> char in string must be double :
  String_SubstFast('"', '""', Result);

  // 2014-12-22 Remove enters :
  String_SubstFast(#13#10, '', Result);

  if (AddQuote or asText) and (Result <> '') then
    Result := '"' + Result + '"';
end;

function GetCsvCellsContent(const fromCsvLine: String; Values: TStrings; const SeparationChar: Char = ';'): Integer;
var
  NbCells, c: Integer;
  isPreviousQuoteChar, isQuoteChar, isSeparatorChar, QuotedCell, QuotedOpened: Boolean;
  CellValue: String;
begin
  Result := 0;
  NbCells := 0;
  Values.Clear;

  if fromCsvLine = '' then Exit;

  isPreviousQuoteChar := false;
  QuotedCell := false;
  QuotedOpened := false;
  CellValue := '';

  for c := 1 to length(fromCsvLine) do
  begin
    if c <> 1 then
      isPreviousQuoteChar := isQuoteChar;

    isQuoteChar := fromCsvLine[c] = '"';
    isSeparatorChar := fromCsvLine[c] = SeparationChar;

    if isSeparatorChar and (not QuotedOpened) then
    begin
      // *** New cell *** //

      // Save previous cell :
      inc(NbCells);
      Values.Add(CellValue);
      Inc(Result);
      CellValue := '';
      QuotedCell := false;
      QuotedOpened := false;  // Normally, it's already = false "
    end
    else begin
      // *** Same cell *** //
      if isQuoteChar then
        QuotedOpened := not QuotedOpened;

      if QuotedCell then
      begin
        if not isQuoteChar then
          CellValue := CellValue + fromCsvLine[c];  // Add any char !!
      end
      else begin
        if isQuoteChar then
        begin
          if CellValue = '' then
          begin
            // Cell starting with quote char must be ignored ...
            QuotedCell := true;
            isQuoteChar := false;  // Avoid insert <"> char if next char is <"> ...
          end;
        end
        else
          CellValue := CellValue + fromCsvLine[c];
      end;

      // General rule : 2 quoted char must be export as one unless first one is the first cell char !
      if isPreviousQuoteChar and isQuoteChar then
      begin
        CellValue := CellValue + '"';
        isQuoteChar := false;         // Avoid insert another <"> char if next char is <"> ...
      end;
    end;
  end;

  // Last cell :
  // inc(NbCells);
  Values.Add(CellValue);
  Inc(Result);
end;

function isValidWebSiteChar(const aChar: Char): Boolean;
begin
  {$IFDEF UNICODE}
  Result := CharInSet(aChar, WebSiteLetters);
  {$ELSE}
  Result := aChar in WebSiteLetters;
  {$ENDIF}
end;

function isValidWebMailChar(const aChar: Char): Boolean;
begin
  {$IFDEF UNICODE}
  Result := CharInSet(aChar, WebMailletters);
  {$ELSE}
  Result := aChar in WebMailletters;
  {$ENDIF}
end;

// Valid webSite:
// 'http://' + domain + '.' + country       (We consider 'www.' as part of domain)
// domain + '.' + country
function isValidwebSite(aStr: String): Boolean;
var
  i: Integer;
  WasSpecialChar, FoundDomain, FoundCountry: Boolean;

        // 2014-10-28 We can have  "http://sourceforge.net/projects/tcycomponents/"
        // Country will return "net/projects/tcycomponents/".
        // So, we have to cut all after "/"
        function ValidCountry(StrCountry: String): Boolean;
        var i: Integer;
        begin
          Result := false;

          if pos('/', StrCountry) <> 0 then
            StrCountry := Copy(StrCountry, 1, pos('/', StrCountry)-1);

          if Length(StrCountry) in [2, 3]  then
          begin
            Result := True;
            for i := 1 to Length(StrCountry) do
              {$IFDEF UNICODE}
              if not CharInSet(StrCountry[i], ['a'..'z', 'A'..'Z']) then
              {$ELSE}
              if not (StrCountry[i] in ['a'..'z', 'A'..'Z']) then
              {$ENDIF}
                Result := false;
          end;
        end;

begin
  Result := false;

  aStr := AnsiLowercase(aStr);
  if pos('http://', aStr) = 1 then
    Delete(aStr, 1, 7);

  if pos('https://', aStr) = 1 then
    Delete(aStr, 1, 7);

  if pos('www.', aStr) = 1 then
    Delete(aStr, 1, 4);

  if pos(':', aStr) > 0 then
    Exit;             // Result is false

  {$IFDEF UNICODE}
  if CharInSet((aStr + ' ')[1], ['/', '.']) then
  {$ELSE}
  if (aStr + ' ')[1] in ['/', '.'] then
  {$ENDIF}
    Exit;             // Result is false

  WasSpecialChar := false;
  FoundDomain := false;
  FoundCountry := false;

  for i := length(aStr) downto 1 do
  begin
    if not isValidWebSiteChar(aStr[i]) then
      Exit;       // Result is false

    // Test 2 special chars besides :
    {$IFDEF UNICODE}
    if CharInSet(aStr[i], ['/', '.']) then
    {$ELSE}
    if aStr[i] in ['/', '.'] then
    {$ENDIF}
    begin
      if WasSpecialChar
      then Exit       // Result is false
      else WasSpecialChar := true;
    end
    else
      WasSpecialChar := false;

    case aStr[i] of
      '.' :
      begin
        if not FoundCountry then
        begin
          if (FoundDomain) or (i = length(aStr)) or (i = 1)
          then
            Exit   // Result is false
          else
            if ValidCountry( copy(aStr, i+1, length(aStr)) )
            then FoundCountry := true
            else Exit;
        end
        else
          if i=1 then
            Exit;     // Result is false
      end
    else
      begin
        if FoundCountry then
          FoundDomain := true;
      end;
    end;
  end;

  Result := FoundDomain and FoundCountry;
end;

// Valid @mail:
// Body + '@' + domain + '.' + country
function isValidWebMail(const aStr: String): Boolean;
var
  i: Integer;
  WasSpecialChar, FoundBody, FoundDomain, FoundCountry: Boolean;
begin
  Result := false;

  WasSpecialChar := false;
  FoundBody := false;
  FoundDomain := false;
  FoundCountry := false;

  for i := length(aStr) downto 1 do
  begin
    // Test 2 special chars besides :
    {$IFDEF UNICODE}
    if CharInSet(aStr[i], ['@', '.']) then
    {$ELSE}
    if aStr[i] in ['@', '.'] then
    {$ENDIF}
    begin
      if WasSpecialChar
      then Exit       // Result is false
      else WasSpecialChar := true;
    end
    else
      WasSpecialChar := false;

    case aStr[i] of
      '@' :
      begin
        if FoundDomain
        then
          Exit        // Result is false
        else
          if not FoundCountry
          then Exit   // Result is false
          else FoundDomain := true;
      end;

      '.' :
      begin
        if not FoundCountry then
        begin
          if (FoundBody) or (FoundDomain) or (i = length(aStr)) or (i = 1)
          then Exit   // Result is false
          else FoundCountry := true;
        end
        else
          if i=1 then
            Exit;     // Result is false
      end
    else
      begin
        if not isValidWebMailChar(aStr[i]) then
          Exit;       // Result is false

        if FoundDomain and FoundCountry then
          FoundBody := true;
      end;
    end;
  end;

  Result := FoundBody and FoundDomain and FoundCountry;
end;

// Mask functions *---------------------------------------------------------
function IsValidMask(const aMask: String): Boolean;      // v2.0 ...
var
  i: Integer;
  MaskPartStringValueStarted, MaskPartCustomCharsValueStarted: Boolean;
begin
  Result := true;
  MaskPartStringValueStarted := false;
  MaskPartCustomCharsValueStarted := false;

  for i := 1 to length(aMask) do
  begin
    if aMask[i] = cQuoteChar then
      MaskPartStringValueStarted := not MaskPartStringValueStarted
    else
      if (aMask[i] = MaskBeginCustomChars) and (not MaskPartCustomCharsValueStarted) then
        MaskPartCustomCharsValueStarted := true
      else
        if (aMask[i] = MaskEndCustomChars) and (MaskPartCustomCharsValueStarted) then
          MaskPartCustomCharsValueStarted := false
        else
          if (not MaskPartStringValueStarted) and (not MaskPartCustomCharsValueStarted) then
            {$IFDEF UNICODE}
            if not CharInSet(aMask[i], [cQuoteChar, MaskPrefixOptional, MaskAnyChar, MaskAlphaNumChar, MaskNumber, MaskOptionalNumber, MaskAlphaChar, MaskUppercaseChar, MaskLowercaseChar, MaskOtherChar]) then
            {$ELSE}
            if not (aMask[i] in [cQuoteChar, MaskPrefixOptional, MaskAnyChar, MaskAlphaNumChar, MaskNumber, MaskOptionalNumber, MaskAlphaChar, MaskUppercaseChar, MaskLowercaseChar, MaskOtherChar]) then
            {$ENDIF}
              Result := false;

    if not Result then
      Break;
  end;

  if Result then
    Result := (not MaskPartStringValueStarted) and (not MaskPartCustomCharsValueStarted);
end;

function GetNextMaskPart(var InOutMask, RsltMaskPartParamStr: string; var RsltMaskPartType: TMaskPartType; var RsltOptional: Boolean): Boolean; // v2.0 ...
begin
  Result := false;
  if InOutMask = '' then Exit;

  if InOutMask[1] = MaskPrefixOptional then
  begin
    RsltOptional := true;
    Delete(InOutMask, 1, 1);
    if InOutMask = '' then Exit;
  end
  else
    RsltOptional := false;


  RsltMaskPartParamStr := '';

  case InOutMask[1] of
    cQuoteChar:           RsltMaskPartType := mtMaskPartStringValue;
    MaskPrefixOptional:   RsltMaskPartType := mtUndefined;
    MaskBeginCustomChars: RsltMaskPartType := mtMaskPartCustomChar;
    MaskAnyChar:          RsltMaskPartType := mtAnyChar;
    MaskAlphaNumChar:     RsltMaskPartType := mtAlphaNumChar;
    MaskNumber:           RsltMaskPartType := mtNumber;
    MaskOptionalNumber:
    begin
                          RsltMaskPartType := mtNumber;
                          RsltOptional := true;
    end;
    MaskAlphaChar:        RsltMaskPartType := mtAlphaChar;
    MaskUppercaseChar:    RsltMaskPartType := mtUppLetter;
    MaskLowercaseChar:    RsltMaskPartType := mtLowLetter;
    MaskOtherChar:        RsltMaskPartType := mtOtherChar;
    else
      RsltMaskPartType := mtUndefined;
  end;

  if RsltMaskPartType = mtUndefined then
    RsltMaskPartParamStr := InOutMask[1]
  else
    if RsltMaskPartType = mtMaskPartStringValue then
    begin
      Delete(InOutMask, 1, 1);  // Remove first quote char ...

      while InOutMask <> '' do
      begin
        if InOutMask[1] <> cQuoteChar then
        begin
          RsltMaskPartParamStr := RsltMaskPartParamStr +  InOutMask[1];
          Delete(InOutMask, 1, 1);
        end
        else begin
          Delete(InOutMask, 1, 1);
          Break;    // end of string value ...
        end;
      end;

      Result := RsltMaskPartParamStr <> '';
    end
    else
      if RsltMaskPartType = mtMaskPartCustomChar then
      begin
        Delete(InOutMask, 1, 1);  // Remove MaskBeginCustomChars ...

        while InOutMask <> '' do
        begin
          if InOutMask[1] <> MaskEndCustomChars then
          begin
            RsltMaskPartParamStr := RsltMaskPartParamStr +  InOutMask[1];
            Delete(InOutMask, 1, 1);
          end
          else begin
            Delete(InOutMask, 1, 1);
            Break;    // end of Custom chars ...
          end;
        end;

        RsltOptional := RsltOptional or (pos('%', RsltMaskPartParamStr) <> 0);
        Result := RsltMaskPartParamStr <> '';
      end
      else begin
        RsltMaskPartParamStr := InOutMask[1];
        Delete(InOutMask, 1, 1);
        Result := true;
      end;
end;

function MaskPartCount(aMask: string; var RsltOptionalCount: Integer): Integer;     // v2.0 ...
var
  Mask, MaskPartParamStr: string;
  MaskPartType: TMaskPartType;
  RsltMaskPartOptional: Boolean;
begin
  Result := 0;
  RsltOptionalCount := 0;
  Mask := aMask;
  while GetNextMaskPart(Mask, MaskPartParamStr, MaskPartType, RsltMaskPartOptional) do
  begin
    Inc(Result);

    if RsltMaskPartOptional then
      Inc(RsltOptionalCount);
  end;
end;

function MaskCharsCount(aMask: string; var RsltOptionalCount: Integer): Integer;
var
  Mask, MaskPartParamStr: string;
  MaskPartType: TMaskPartType;
  RsltMaskPartOptional: Boolean;
  nbCars: Integer;
begin
  Result := 0;
  RsltOptionalCount := 0;
  Mask := aMask;
  while GetNextMaskPart(Mask, MaskPartParamStr, MaskPartType, RsltMaskPartOptional) do
  begin
    if MaskPartType = mtMaskPartStringValue
    then nbCars := length(MaskPartParamStr)
    else nbCars := 1;

    Result := Result + nbCars;

    if RsltMaskPartOptional then
      RsltOptionalCount := RsltOptionalCount + nbCars;
  end;
end;

function DetectPartialMatchMask(var InOutMask: string; const aString: string): Boolean;  // v2.0 ...
var
  RsltMaskPartParamStr: string;
  RsltMaskPartType: TMaskPartType;
  RsltMaskPartOptional: Boolean;
  _InOutMask, _aString: String;
begin
  Result := false;

  while (not Result) and (InOutMask <> '') and (aString <> '') do
  begin
    _InOutMask := InOutMask;
    _aString := aString;

    IsMatchMask(_InOutMask, _aString);

    if _aString = '' then    // if aString = '', it is because all string matched partially the mask !
    begin
      if _InOutMask <> '' then   // if _InOutMask <> '', some residual mask not matched, we will cut it from InOutMask !
        InOutMask := copy(InOutMask, 1, Length(InOutMask) - Length(_InOutMask));

      Result := true;
    end
    else
      GetNextMaskPart(InOutMask, RsltMaskPartParamStr, RsltMaskPartType, RsltMaskPartOptional);  // Remove single mask part from left and try again ...
  end;
end;

function MaskPartTypeCompatible(const aMaskPartType, withMaskPartType: TMaskPartType): Boolean;
begin
  case aMaskPartType of
    mtMaskPartStringValue: Result := withMaskPartType = mtMaskPartStringValue;
    mtMaskPartCustomChar:  Result := false;
    mtAnyChar:             Result := true;
    mtAlphaNumChar:        Result := withMaskPartType in [mtAlphaNumChar, mtNumber, mtAlphaChar, mtUppLetter, mtLowLetter];
    mtNumber:              Result := withMaskPartType in [mtNumber];
    mtAlphaChar:           Result := withMaskPartType in [mtAlphaChar, mtUppLetter, mtLowLetter];
    mtUppLetter:           Result := withMaskPartType in [mtUppLetter];
    mtLowLetter:           Result := withMaskPartType in [mtLowLetter];
    mtOtherChar:           Result := withMaskPartType = mtOtherChar;
    else
      Result := false;
  end;
end;

function IsMatchMask(var aMask: string; var aString: string): Boolean;                           // v2.0 ...
var
  MaskPartParamStr: string;
  MaskPartType: TMaskPartType;
  RsltMaskPartOptional: Boolean;

  OptionalType: TMaskPartType;
  OptionalCount: Integer;

      function DecOptionalCount: Boolean;
      begin
        Result := false;

        if (OptionalType = MaskPartType) or (MaskPartTypeCompatible(MaskPartType, OptionalType)) then
          if OptionalCount > 0 then
          begin
            Dec(OptionalCount);
            Result := true;
          end;
      end;

      function ResidualMaskIsOptional: Boolean;
      begin
        Result := true;
        if aMask = '' then Exit;


        while Result and GetNextMaskPart(aMask, MaskPartParamStr, MaskPartType, RsltMaskPartOptional) do
        begin
          if (OptionalType <> MaskPartType) and (not MaskPartTypeCompatible(MaskPartType, OptionalType)) then
          begin
            OptionalType := MaskPartType;
            OptionalCount := 0;
          end;

          if RsltMaskPartOptional then
            Inc(OptionalCount);


          if OptionalCount > 0
          then dec(OptionalCount)
          else Result := false;
        end;

        if Result then
          Result := aMask = '';
      end;

begin
  Result := true;
  if aMask = '' then Exit;

  OptionalType := mtUndefined;
  OptionalCount := 0;

  while GetNextMaskPart(aMask, MaskPartParamStr, MaskPartType, RsltMaskPartOptional) do
  begin
    if (OptionalType <> MaskPartType) and (not MaskPartTypeCompatible(MaskPartType, OptionalType)) then
    begin
      OptionalType := MaskPartType;
      OptionalCount := 0;
    end;

    if RsltMaskPartOptional then
      Inc(OptionalCount);


    if aString = '' then
    begin
      Result := false;
      if MaskPartType = mtMaskPartCustomChar then MaskPartParamStr := '[' + MaskPartParamStr + ']';
      if MaskPartType = mtMaskPartStringValue then MaskPartParamStr := cQuoteChar + MaskPartParamStr + cQuoteChar;
      aMask := MaskPartParamStr + aMask;
    end
    else
      case MaskPartType of
        mtUndefined:
          Result := false;

        mtMaskPartStringValue:
        begin
          Result := pos(MaskPartParamStr, aString) = 1;

          if Result then
            Delete(aString, 1, length(MaskPartParamStr))
          else
            if DecOptionalCount
            then Result := true
            else aMask := MaskPartParamStr + aMask;
        end;

        mtMaskPartCustomChar:
        begin
          Result := pos(aString[1], MaskPartParamStr) <> 0;

          if Result then
            Delete(aString, 1, 1)
          else
            if DecOptionalCount // if RsltMaskPartOptional
            then Result := true
            else aMask := MaskPartParamStr + aMask;
        end;

        mtAnyChar:
          Delete(aString, 1, 1);

        mtAlphaNumChar:
        begin
          {$IFDEF UNICODE}
          Result := CharInSet(aString[1], ['0'..'9', 'a'..'z', 'A'..'Z']);
          {$ELSE}
          Result := aString[1] in ['0'..'9', 'a'..'z', 'A'..'Z'];
          {$ENDIF}

          if Result then
            Delete(aString, 1, 1)
          else
            if DecOptionalCount
            then Result := true
            else aMask := MaskPartParamStr + aMask;
        end;

        mtNumber:
        begin
          {$IFDEF UNICODE}
          Result := CharInSet(aString[1], ['0'..'9']);
          {$ELSE}
          Result := aString[1] in ['0'..'9'];
          {$ENDIF}

          if Result then
            Delete(aString, 1, 1)
          else
            if DecOptionalCount then
              Result := true;
        end;

        mtAlphaChar:
        begin
          {$IFDEF UNICODE}
          Result := CharInSet(aString[1], ['a'..'z', 'A'..'Z']);
          {$ELSE}
          Result := aString[1] in ['a'..'z', 'A'..'Z'];
          {$ENDIF}

          if Result then
            Delete(aString, 1, 1)
          else
            if DecOptionalCount
            then Result := true
            else aMask := MaskPartParamStr + aMask;
        end;

        mtUppLetter:
        begin
          {$IFDEF UNICODE}
          Result := CharInSet(aString[1], ['A'..'Z']);
          {$ELSE}
          Result := aString[1] in ['A'..'Z'];
          {$ENDIF}

          if Result then
            Delete(aString, 1, 1)
          else
            if DecOptionalCount
            then Result := true
            else aMask := MaskPartParamStr + aMask;
        end;

        mtLowLetter:
        begin
          {$IFDEF UNICODE}
          Result := CharInSet(aString[1], ['a'..'z']);
          {$ELSE}
          Result := aString[1] in ['a'..'z'];
          {$ENDIF}

          if Result then
            Delete(aString, 1, 1)
          else
            if DecOptionalCount
            then Result := true
            else aMask := MaskPartParamStr + aMask;
        end;

        else begin // mtOtherChar !!!
          {$IFDEF UNICODE}
          if CharInSet(aString[1], ['a'..'z', 'A'..'Z']) then
          {$ELSE}
          if aString[1] in ['a'..'z', 'A'..'Z'] then
          {$ENDIF}
            Result := false
          else
            {$IFDEF UNICODE}
            if CharInSet(aString[1], ['0'..'9']) then
            {$ELSE}
            if aString[1] in ['0'..'9'] then
            {$ENDIF}
              Result := false;

          if Result then
            Delete(aString, 1, 1)
          else
            if DecOptionalCount
            then Result := true
            else aMask := MaskPartParamStr + aMask;
        end;
      end;

    if not Result then
      Break;
  end;

  if MaskPartType = mtUndefined then
  begin
    aMask := 'Char ' + QuotedStr(MaskPartParamStr) + ' not allowed. See below allowed chars: ' + #13#10 +
             MaskRulesMsg;

    raise Exception.Create(aMask);
  end;

  if aMask <> '' then
  begin
    if aString = '' then
      Result := ResidualMaskIsOptional;
  end
  else
    if Result then
      Result := (aMask = '') and (aString = '');
end;

function IsMatchMask2(const aMask: string; const aString: string): Boolean;     // v2.0 ...
var
  RsltMask, RsltString: string;
begin
  RsltMask := aMask;
  RsltString := aString;
  Result := IsMatchMask(RsltMask, RsltString);
end;

function MergeMaskToMatchedString(var InOutMask: string; aString: string): String;       // v2.0 ...
var
  MaskPartParamStr: string;
  MaskPartType: TMaskPartType;
  RsltMaskPartOptional: Boolean;
  MaskPartMatch: Boolean;
  i: Integer;
  ReplaceChar: Boolean;

  OptionalType: TMaskPartType;
  OptionalCount, OptionalNeeded, OptionalAdded, OptionalStartPos: Integer;


      function DecOptionalCount: Boolean;
      begin
        Result := false;

        if (OptionalType = MaskPartType) or (MaskPartTypeCompatible(MaskPartType, OptionalType)) then
          if OptionalCount > 0 then
          begin
            Dec(OptionalCount);
            Result := true;
          end;
      end;

const
  cStringNotMatch = 'String does not match mask !';

begin
  Result := '';

  OptionalType := mtUndefined;
  OptionalCount := 0;
  OptionalNeeded := 0;
  OptionalAdded := 0;

  while GetNextMaskPart(InOutMask, MaskPartParamStr, MaskPartType, RsltMaskPartOptional) do
  begin
    if (OptionalType <> MaskPartType) and (not MaskPartTypeCompatible(MaskPartType, OptionalType)) then
    begin
      // Insert optional numbers :
      if OptionalType = mtNumber then
        for i := OptionalAdded to OptionalNeeded - 1 do
          Insert('0', Result, OptionalStartPos);

      OptionalStartPos := Length(Result) + 1;
      OptionalType := MaskPartType;
      OptionalCount := 0;
      OptionalNeeded := 0;
      OptionalAdded := 0;
    end;

    if RsltMaskPartOptional then
      Inc(OptionalCount);

    if MaskPartType = mtNumber then
    begin
      // Only fill with zero if '9'. If %0, used, we don' t add zero.
      if MaskPartParamStr = MaskoptionalNumber then
        Inc(OptionalNeeded);
    end;



    if aString = '' then
    begin
      if not DecOptionalCount then
        raise Exception.Create(cStringNotMatch);
    end
    else
      case MaskPartType of
        mtUndefined:
        begin
          InOutMask := 'Char ' + QuotedStr(MaskPartParamStr) + ' not allowed. See below allowed chars: ' + #13#10 +
                   MaskRulesMsg;

          raise Exception.Create(InOutMask);
        end;

        mtMaskPartStringValue:
        begin
          MaskPartMatch := pos(MaskPartParamStr, aString) = 1;

          if MaskPartMatch then
          begin
            Result := Result + MaskPartParamStr;
            Delete(aString, 1, length(MaskPartParamStr));
          end
          else
            if not DecOptionalCount then
              raise Exception.Create(cStringNotMatch);
        end;

        mtMaskPartCustomChar:
        begin
          MaskPartMatch := pos(aString[1], MaskPartParamStr) <> 0;

          // We consider that the first char is the good one :
          ReplaceChar := MaskPartParamStr[1] <> '%';

          if MaskPartMatch then  // Char in Set?
          begin
            if not ReplaceChar then
              if pos('%%', MaskPartParamStr) = 0 then         // 2016-11-18  '%%' for optional and if any, must be removed on merge
                Result := Result + aString[1];   // We keep this one ...

            Delete(aString, 1, 1);
          end
          else
            if not DecOptionalCount then
              raise Exception.Create(cStringNotMatch);

          if ReplaceChar then
            Result := Result + MaskPartParamStr[1];
        end;

        mtAnyChar:
        begin
          MaskPartMatch := true;
          Result := Result + aString[1];
          Delete(aString, 1, 1);
        end;

        mtAlphaNumChar:
        begin
          {$IFDEF UNICODE}
          MaskPartMatch := CharInSet(aString[1], ['0'..'9', 'a'..'z', 'A'..'Z']);
          {$ELSE}
          MaskPartMatch := aString[1] in ['0'..'9', 'a'..'z', 'A'..'Z'];
          {$ENDIF}

          if MaskPartMatch then
          begin
            Result := Result + aString[1];
            Delete(aString, 1, 1);
          end
          else
            if not DecOptionalCount then
              raise Exception.Create(cStringNotMatch);
        end;

        mtNumber:
        begin
          {$IFDEF UNICODE}
          MaskPartMatch := CharInSet(aString[1], ['0'..'9']);
          {$ELSE}
          MaskPartMatch := aString[1] in ['0'..'9'];
          {$ENDIF}

          if MaskPartMatch then
          begin
            Result := Result + aString[1];
            Delete(aString, 1, 1);

            Inc(OptionalAdded);
          end
          else
            if not DecOptionalCount then
              raise Exception.Create(cStringNotMatch);
        end;

        mtAlphaChar:
        begin
          {$IFDEF UNICODE}
          MaskPartMatch := CharInSet(aString[1], ['a'..'z', 'A'..'Z']);
          {$ELSE}
          MaskPartMatch := aString[1] in ['a'..'z', 'A'..'Z'];
          {$ENDIF}

          if MaskPartMatch then
          begin
            Result := Result + aString[1];
            Delete(aString, 1, 1);
          end
          else
            if not DecOptionalCount then
              raise Exception.Create(cStringNotMatch);
        end;

        mtUppLetter:
        begin
          {$IFDEF UNICODE}
          MaskPartMatch := CharInSet(aString[1], ['A'..'Z']);
          {$ELSE}
          MaskPartMatch := aString[1] in ['A'..'Z'];
          {$ENDIF}

          if MaskPartMatch then
          begin
            Result := Result + aString[1];
            Delete(aString, 1, 1);
          end
          else
            if not DecOptionalCount then
              raise Exception.Create(cStringNotMatch);
        end;

        mtLowLetter:
        begin
          {$IFDEF UNICODE}
          MaskPartMatch := CharInSet(aString[1], ['a'..'z']);
          {$ELSE}
          MaskPartMatch := aString[1] in ['a'..'z'];
          {$ENDIF}

          if MaskPartMatch then
          begin
            Result := Result + aString[1];
            Delete(aString, 1, 1);
          end
          else
            if not DecOptionalCount then
              raise Exception.Create(cStringNotMatch);
        end;


        else begin      // mtOtherChar !!!
          MaskPartMatch := true;

          {$IFDEF UNICODE}
          if CharInSet(aString[1],  ['a'..'z', 'A'..'Z']) then
          {$ELSE}
          if aString[1] in ['a'..'z', 'A'..'Z'] then
          {$ENDIF}
            MaskPartMatch := false
          else
            {$IFDEF UNICODE}
            if CharInSet(aString[1], ['0'..'9']) then
            {$ELSE}
            if aString[1] in ['0'..'9'] then
            {$ENDIF}
              MaskPartMatch := false;

          if MaskPartMatch then
          begin
            Result := Result + aString[1];
            Delete(aString, 1, 1);
          end
          else
            if not DecOptionalCount then
              raise Exception.Create(cStringNotMatch);
        end;
      end;
  end;

  // Insert optional numbers :
  if OptionalType = mtNumber then
    for i := OptionalAdded to OptionalNeeded - 1 do
      Insert('0', Result, OptionalStartPos);
end;

function MergeMaskToMatchedString2(const aMask: string; const aString: string): String;    // v2.0 ...
var
  RsltMask, ParamString: string;
begin
  RsltMask := aMask;
  ParamString := aString;

  Result := MergeMaskToMatchedString(RsltMask, ParamString);
end;

end.
