(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is TurboPower XMLPartner
 *
 * The Initial Developer of the Original Code is
 * TurboPower Software
 *
 * Portions created by the Initial Developer are Copyright (C) 1997-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)
{*********************************************************}
{* XMLPartner: XpBase.PAS                                *}
{*********************************************************}
{* XMLPartner: Base types and conversion routines        *}
{*********************************************************}

{$I XpDefine.inc}

unit XpBase;

interface

uses
  classes;

type                                                                   {!!.51}
  DOMString = WideString;                                              {!!.51}

const
  { Product name}
{Begin !!.51}
{$IFDEF XPDSTD}
  {$IFDEF XPTrialRun}
  XpProductName = 'XMLPartner Trial Run';
  {$ELSE}
  XpProductName = 'XMLPartner';
  {$ENDIF}
{$ELSE}
  {$IFDEF XPTrialRun}
  XpProductName = 'XMLPartner Pro Trial Run';
  {$ELSE}
  XpProductName = 'XMLPartner Professional';
  {$ENDIF}
{$ENDIF}
{End !!.51}

  XpVendor = 'TurboPower Software Company';
  XpVendorURL = 'http://www.turbopower.com';

  { Version numbers }
  XpVersionNumber : Longint = 25700; {2.5.7.0}
  {$IFDEF XPDSTD}
  XpXSLImplementation = '0.0';                             {!!.51}
  {$ELSE}
  XpXSLImplementation = '1.0';                             {!!.51}
  {$ENDIF}
  XpXMLSpecification = '1.0';                              {!!.51}

  { IDE under which the EXE/components were compiled. }
  {$IFDEF Delphi3}
  XpReleaseString : string = 'Release (D3)';
  {$ENDIF}
  {$IFDEF Delphi4}
  XpReleaseString : string = 'Release (D4)';
  {$ENDIF}
  {$IFDEF Delphi5}
  XpReleaseString : string = 'Release (D5)';
  {$ENDIF}
  {$IFDEF Delphi6}
  {$IFDEF LINUX}
  XpReleaseString : string = 'Release (KYLIX)';
  {$ELSE}
  XpReleaseString : string = 'Release (D6)';
  {$ENDIF}
  {$ENDIF}
  {$IFDEF Delphi7}
  {$IFDEF LINUX}
  XpReleaseString : string = 'Release (KYLIX)';
  {$ELSE}
  XpReleaseString : string = 'Release (D7)';
  {$ENDIF}
  {$ENDIF}
  {$IFDEF CBuilder3}
  XpReleaseString : string = 'Release (C3)';
  {$ENDIF}
  {$IFDEF CBuilder4}
  XpReleaseString : string = 'Release (C4)';
  {$ENDIF}
  {$IFDEF CBuilder5}
  XpReleaseString : string = 'Release (C5)';
  {$ENDIF}
  {$IFDEF CBuilder6}                                                   {!!.56}
  XpReleaseString : string = 'Release (C6)';                           {!!.56}
  {$ENDIF}                                                             {!!.56}

{Begin !!.52}
  { Special element names. }
  XpsStylePI  = 'xml-stylesheet';
  XpsXSLText = 'xsl:text';
  XpsXSLVersion = 'xsl:version';

{$IFDEF MSWINDOWS}
  PathDelim = '\';
{$ENDIF}
{End !!.52}

{Begin !!.53}
  { Special attribute names. }
  XpsXMLBase  = 'xml:base';
  XpsXMLLang  = 'xml:lang';
  XpsXMLSpace = 'xml:space';

  XpsPreserve = 'preserve';

  { Character encoding strings. }
  XpsUnknown  = 'Unknown';
  XpsUTF8     = 'UTF-8';
  XpsUTF16    = 'UTF-16';
  XpsISO88591 = 'ISO-8859-1';
{End !!.53}

{Begin !!.53}
{$IFDEF MSWINDOWS}
  XpsLinebreak = #13#10;
{$ELSE}
  XpsLinebreak = #10;
{$ENDIF}
{End !!.53}

{Notes: In TurboPower XML Partner the UCS-4 character type is
        deliberately made into a 32-bit *signed* integer. We only
        allow the lower 31-bits to store the character, the upper bit
        (the sign bit) will be used internally to signify a literal
        character. This goes against the UCS-4 spec, however we should
        be fairly safe: at the time of writing there are plans for
        only 17 'planes' of characters (each plane being 65536
        characters) and hence the most significant byte of a UCS-4
        character will not be used for a long time.}

const
  {The following constants are the tokens needed to parse an XML
   document. The tokens are stored in UCS-4 format to reduce the
   number of conversions needed by the filter.}
  Xpc_BracketAngleLeft : array[0..0] of Longint = (60); {<}
  Xpc_BracketAngleRight : array[0..0] of Longint = (62); {>}
  Xpc_BracketSquareLeft : array[0..0] of Longint = (91); {[}
  Xpc_BracketSquareRight : array[0..0] of Longint = (93); {]}
  Xpc_CDATAStart :
    array[0..5] of Longint = (67, 68, 65, 84, 65, 91); {CDATA[}
  Xpc_CharacterRef : array[0..0] of Longint = (35); {#}
  Xpc_CharacterRefHex : array[0..0] of Longint = (120); {x}
  Xpc_CommentEnd : array[0..2] of Longint = (45, 45, 62); {-->}
  Xpc_CommentStart : array[0..3] of Longint = (60, 33, 45, 45); {<!--}
  Xpc_ConditionalEnd : array[0..2] of Longint = (93, 93, 62); {]]>}
  Xpc_ConditionalIgnore :
    array[0..5] of Longint = (73, 71, 78, 79, 82, 69); {IGNORE}
  Xpc_ConditionalInclude :
    array[0..6] of Longint = (73, 78, 67, 76, 85, 68, 69); {INCLUDE}
  Xpc_ConditionalStart :
    array[0..2] of Longint = (60, 33, 91); {<![}
  Xpc_Dash : array[0..0] of Longint = (45); {-}
  Xpc_DTDAttFixed :
    array[0..4] of Longint = (70, 73, 88, 69, 68); {FIXED}
  Xpc_DTDAttImplied :
    array[0..6] of Longint = (73, 77, 80, 76, 73, 69, 68); {IMPLIED}
  Xpc_DTDAttlist :
    array[0..8] of Longint =
      (60, 33, 65, 84, 84, 76, 73, 83, 84); {<!ATTLIST}
  Xpc_DTDAttRequired :
    array[0..7] of Longint =
      (82, 69, 81, 85, 73, 82, 69, 68); {REQUIRED}
  Xpc_DTDDocType :
    array[0..8] of Longint =
      (60, 33, 68, 79, 67, 84, 89, 80, 69); {<!DOCTYPE}
  Xpc_DTDElement :
    array[0..8] of Longint =
      (60, 33, 69, 76, 69, 77, 69, 78, 84); {<!ELEMENT}
  Xpc_DTDElementAny : array[0..2] of Longint = (65, 78, 89); {ANY}
  Xpc_DTDElementCharData :
    array[0..6] of Longint = (35, 80, 67, 68, 65, 84, 65); {#PCDATA}
  Xpc_DTDElementEmpty :
    array[0..4] of Longint = (69, 77, 80, 84, 89); {EMPTY}
  Xpc_DTDEntity :
    array[0..7] of Longint =
      (60, 33, 69, 78, 84, 73, 84, 89); {<!ENTITY}
  Xpc_DTDNotation :
    array[0..9] of Longint =
      (60, 33, 78, 79, 84, 65, 84, 73, 79, 78); {<!NOTATION}
  Xpc_Encoding : array[0..7] of Longint =
    (101, 110, 99, 111, 100, 105, 110, 103); {encoding}
  Xpc_Equation : array[0..0] of Longint = (61); {=}
  Xpc_ExternalPublic :
    array[0..5] of Longint = (80, 85, 66, 76, 73, 67); {PUBLIC}
  Xpc_ExternalSystem :
    array[0..5] of Longint = (83, 89, 83, 84, 69, 77); {SYSTEM}
  Xpc_GenParsedEntityEnd : array[0..0] of Longint = (59); {;}
  Xpc_ListOperator : array[0..0] of Longint = (124); {|}
  Xpc_MixedEnd : array[0..1] of Longint = (41, 42); {)*}
  Xpc_OneOrMoreOpr : array[0..0] of Longint = (42); {*}
  Xpc_ParamEntity : array[0..0] of Longint = (37); {%}
  Xpc_ParenLeft : array[0..0] of Longint = (40); {(}
  Xpc_ParenRight : array[0..0] of Longint = (41); {)}
  Xpc_ProcessInstrEnd : array[0..1] of Longint = (63, 62); {?>}
  Xpc_ProcessInstrStart : array[0..1] of Longint = (60, 63); {<?}
  Xpc_QuoteDouble : array[0..0] of Longint = (34); {"}
  Xpc_QuoteSingle : array[0..0] of Longint = (39); {'}
  Xpc_Standalone :
    array[0..9] of Longint =
      (115, 116, 97, 110, 100, 97, 108, 111, 110, 101); {standalone}
  Xpc_UnparsedEntity :
    array[0..4] of Longint = (78, 68, 65, 84, 65); {NDATA}
  Xpc_Version :
    array[0..6] of Longint =
      (118, 101, 114, 115, 105, 111, 110); {version}

  XpsEncoding = 'encoding';                                            {!!.53}
  XpsNaN = 'NaN';                                                      {!!.53}
  XpsStandalone = 'standalone';                                        {!!.53}
  XpsVersion = 'version';                                              {!!.53}
  XpsXML = 'xml';                                                      {!!.52}
  XpsXMLPartner = 'XMLPartner';

const
  LIT_CHAR_REF = 1;
  LIT_ENTITY_REF = 2;
  LIT_PE_REF = 4;
  LIT_NORMALIZE = 8;

  CONTEXT_NONE = 0;
  CONTEXT_DTD = 1;
  CONTEXT_ENTITYVALUE = 2;
  CONTEXT_ATTRIBUTEVALUE = 3;

  CONTENT_UNDECLARED = 0;
  CONTENT_ANY = 1;
  CONTENT_EMPTY = 2;
  CONTENT_MIXED = 3;
  CONTENT_ELEMENTS = 4;

  OCCURS_REQ_NOREPEAT = 0;
  OCCURS_OPT_NOREPEAT = 1;
  OCCURS_OPT_REPEAT = 2;
  OCCURS_REQ_REPEAT = 3;

  REL_OR = 0;
  REL_AND = 1;
  REL_NONE = 2;

  ATTRIBUTE_UNDECLARED = 0;
  ATTRIBUTE_CDATA = 1;
  ATTRIBUTE_ID = 2;
  ATTRIBUTE_IDREF = 3;
  ATTRIBUTE_IDREFS = 4;
  ATTRIBUTE_ENTITY = 5;
  ATTRIBUTE_ENTITIES = 6;
  ATTRIBUTE_NMTOKEN = 7;
  ATTRIBUTE_NMTOKENS = 8;
  ATTRIBUTE_ENUMERATED = 9;
  ATTRIBUTE_NOTATION = 10;

  ATTRIBUTE_DEFAULT_UNDECLARED = 0;
  ATTRIBUTE_DEFAULT_SPECIFIED = 1;
  ATTRIBUTE_DEFAULT_IMPLIED = 2;
  ATTRIBUTE_DEFAULT_REQUIRED = 3;
  ATTRIBUTE_DEFAULT_FIXED = 4;

  ENTITY_UNDECLARED = 0;
  ENTITY_INTERNAL = 1;
  ENTITY_NDATA = 2;
  ENTITY_TEXT = 3;

  CONDITIONAL_INCLUDE = 0;
  CONDITIONAL_IGNORE = 1;

  {Internet related constants}
  Xpc_INTERNET_SERVICE_URL = 0;
  Xpc_INTERNET_SERVICE_FTP = 1;
  Xpc_INTERNET_SERVICE_GOPHER = 2;
  Xpc_INTERNET_SERVICE_HTTP = 3;
  Xpc_HTTP_VERSION       = 'HTTP/1.0';
  Xpc_INTERNET_FLAG_RELOAD = $80000000;          { retrieve the original item }
  Xpc_INTERNET_FLAG_NO_CACHE_WRITE = $04000000;  { don't write this item to the cache }
  Xpc_INTERNET_FLAG_DONT_CACHE = Xpc_INTERNET_FLAG_NO_CACHE_WRITE;
  Xpc_FTP_TRANSFER_TYPE_ASCII     = 00000001;
  Xpc_FTP_TRANSFER_TYPE_BINARY    = 00000002;

{!!.52 - System functions moved from XpDOM. }
{===System functions=================================================}
const
  xpsBoolean      = 'boolean';
  xpsCeiling      = 'ceiling';
  xpsConcat       = 'concat';
  xpsContains     = 'contains';
  xpsCount        = 'count';
  xpsCurrent      = 'current';
  xpsDocument     = 'document';
  xpsElementAvail = 'element-available';
  xpsFalse        = 'false';
  xpsFloor        = 'floor';
  xpsFormatNumber = 'format-number';
  xpsFuncAvail    = 'function-available';
  xpsGenID        = 'generate-id';
  xpsID           = 'id';
  xpsKey          = 'key';
  xpsLang         = 'lang';
  xpsLast         = 'last';
  xpsLocalName    = 'local-name';
  xpsName         = 'name';
  xpsNamespURI    = 'namespace-uri';
  xpsNormSpace    = 'normalize-space';
  xpsNot          = 'not';
  xpsNumber       = 'number';
  xpsPosition     = 'position';
  xpsRound        = 'round';
  xpsStartsWith   = 'starts-with';
  xpsString       = 'string';
  xpsStringLen    = 'string-length';
  xpsSubstring    = 'substring';
  xpsSubstrAfter  = 'substring-after';
  xpsSubstrBefore = 'substring-before';
  xpsSum          = 'sum';
  xpsSystemProp   = 'system-property';
  xpsTranslate    = 'translate';
  xpsTrue         = 'true';
  xpsUnprseEntURI = 'unparsed-entity-uri';
  xpsXmlns        = 'xmlns';
{====================================================================}

type
  TXpUcs4Char = Longint;
  TXpUtf8Char = string[6];
  {DOMString = WideString;}                                            {!!.51 - Moved above}
  DOMChar = WideChar;
  PDOMChar = PWideChar;

  { Character encoding types}
  TXpCharEncoding = (ceUnknown, ceUTF8, ceUTF16LE, ceUTF16BE, ceISO88591);  {!!.51}

  {The TXpMemoryStream class is used to expose TMemoryStream's SetPointer
   method.}
  TXpMemoryStream = class(TMemoryStream)
  public
    procedure SetPointer(Ptr : Pointer; Size : Longint);
  end;

  TXpFileStream = class(TFileStream)
    FFileName : string;
  public
    constructor CreateEx(Mode : Word; const FileName : string);

    property Filename : string read FFileName;
  end;

{===Utility methods===}
function XpCeiling(const aValue : Double): Integer;
function XpCopy(const sSrc   : DOMString;
                const iIndex,
                      iCount : Integer)
                             : DOMString;
procedure XpDOMBufferAppend(NewText : DOMString;
                        var Text    : DOMString;
                        var TextLen : Integer);                        {!!.57}
procedure XpDOMBufferDelete(var aText      : DOMString;
                                Index,
                                Count      : Integer;
                            var CurrLength : Integer);                 {!!.57}
procedure XpDOMBufferInsert(NewText : DOMString;
                        var Text    : DOMString;
                            Index   : Integer;
                        var TextLen : Integer);                        {!!.57}
function XpEvaluateColor(sColor : string;
                     var wRed,
                         wGreen,
                         wBlue  : Integer) : Integer;
function XpFloor(const aValue : Double): Integer;
function XpGetWord(const sTerm : string) : string;
function XpIsNumeric(c : Char) : Boolean;
function XpIsRelativePath(const Path : string) : Boolean;              {!!.57}
function XpMakeAbsolutePath(const sPath, sStyleURL : string) : string; {!!.57}
function XpMakeFilePath(const sPath, sFile : DOMString) : DOMString;   {!!.57}
function XpMapCharEncToString(const oEncoding : TXpCharEncoding) : DOMString; {!!.53}
function XpMapStringToCharEnc(const sEncodeStr : DOMString) : TXpCharEncoding; {!!.53}
function XpMin(a, b : Longint) : Longint;
function XpPos(const aSubStr, aString : DOMString) : Integer;
function XpRPos(const sSubStr, sTerm : DOMString) : Integer;
function XpStartsWith(const aSubStr, aString : DOMString) : Boolean;   {!!.52}
  { Returns True if aString starts with aSubStr. }                     {!!.52}
function XpStrEquals(const sStr1, sStr2 : DOMString; const iLen : Integer) : Boolean;
  { Perform case-insensitive comparison of first iLen characters in strings
    sStr1 and sStr2. Returns True if equal or False if not equal. }
function XpStrIEquals(const sStr1, sStr2 : DOMString; const iLen : Integer) : Boolean;
  { Perform case-insensitive comparison of first iLen characters in strings
    sStr1 and sStr2. Returns True if equal or False if not equal. }
function XpStringReplaceAll(const aString,                             {!!.52 - Start}
                                  aOldSubStr,
                                  aNewSubStr : DOMString) : DOMString; {!!.52 - End}
function XpStrToFloat(const aString : string) : Double;                {!!.56 - Added}
function XpFloatToStr(const aFloat  : Double) : DOMString;             {!!.56 - Added}
  { The two previous methods are required because the system
    StrToFloat and FloatToStr both use the systems DecimalSeparator
    which should not be used in XML. Character "." is the only allowed
    decimal separator in XML. }
{=====================================================================}


{===character conversion routines===}
function XPIso88591ToUcs4(aInCh  : AnsiChar;
                      var aOutCh : TXpUcs4Char) : Boolean;
function XPUcs4ToIso88591(aInCh  : TXpUcs4Char;
                      var aOutCh : AnsiChar) : Boolean;
function XPUcs4ToUtf16(aInCh : TXpUcs4Char;
                   var aOutChI, aOutChII : DOMChar) : Boolean;         {!!.52}
function XpUcs4ToWideChar(const aInChar : TXpUcs4Char;                 {!!.52}
                      var aOutWS  : DOMChar) : Boolean;                {!!.52}
function XPUtf16ToUcs4(aInChI,
                       aInChII   : DOMChar;                            {!!.52}
                   var aOutCh    : TXpUcs4Char;                        {!!.51}
                   var aBothUsed : Boolean) : Boolean;                 {!!.51}
function XPUcs4ToUtf8(aInCh  : TXpUcs4Char;
                  var aOutCh : TXpUtf8Char) : Boolean;
function XPUtf8ToUcs4(const aInCh  : TXpUtf8Char;
                            aBytes : Integer;
                        var aOutCh : TXpUcs4Char) : Boolean;

{===UTF specials===}
function XPGetLengthUtf8(const aCh : AnsiChar) : byte;                 {!!.52}
function XPIsUtf8Trail(const aCh : AnsiChar) : boolean;                {!!.52}

{===character classes===}
function XPIsBaseChar(aCh : TXpUcs4Char) : Boolean;
function XPIsChar(const aCh : TXpUcs4Char) : Boolean;                  {!!.52}
function XPIsCombiningChar(aCh : TXpUcs4Char) : Boolean;
function XPIsDigit(aCh : TXpUcs4Char) : Boolean;
function XPIsExtender(aCh : TXpUcs4Char) : Boolean;
function XPIsIdeographic(aCh : TXpUcs4Char) : Boolean;
function XPIsLetter(aCh : TXpUcs4Char) : Boolean;
function XPIsNameChar(aCh : TXpUcs4Char) : Boolean;
function XPIsNameCharFirst(aCh : TXpUcs4Char) : Boolean;
function XPIsPubidChar(aCh : TXpUcs4Char) : Boolean;
function XPIsSpace(aCh : TXpUcs4Char) : Boolean;
function XPIsSurrogate(aCh : DOMChar) : Boolean;                       {!!.52}
function XpIsValidXPathNumber(const aNumber : DOMString) : Boolean;    {!!.56}
function XpValidName(const Name : DOMString) : Boolean;                {!!.57}
function XpValidNameChar(const First : Boolean;                        {!!.52}
                         const Char  : DOMChar) : Boolean;             {!!.52}

{===Base component class===}
type
  TXpComponent = class(TComponent)
  protected
    function xcGetVersion : string;
    procedure xcSetVersion(const Value : string);
  published
    property Version : string
      read xcGetVersion
      write xcSetVersion
      stored False;
  end;

implementation

uses
  sysutils;

{== Utility methods ==================================================}
function XpCeiling(const aValue : Double): Integer;
begin
  Result := Integer(Trunc(aValue));
  if Frac(aValue) > 0 then
    Inc(Result);
end;
{--------}
function XpCopy(const sSrc : DOMString; const iIndex, iCount : Integer) : DOMString;
var
  iCountTmp,
  iIndx,
  iLen,
  iResultIndx : Integer;
begin
  iLen := Length(sSrc);
  if (iIndex < 1) or (iCount = 0) or (iIndex > iLen) then
    Result := ''
  else begin
    if iIndex + iCount > iLen then begin
      iCountTmp := iLen - iIndex + 1;
      SetLength(Result, iLen - iIndex + 1)
    end
    else begin
      iCountTmp := iCount;
      SetLength(Result, iCount);
    end;
    iResultIndx := 1;
    for iIndx := iIndex to iIndex + iCountTmp do begin
      Result[iResultIndx] := sSrc[iIndx];
      inc(iResultIndx);
    end;
  end;
end;
{--------}
procedure XpDOMBufferAppend(NewText   : DOMString;
                        var Text      : DOMString;
                        var TextLen  : Integer);                        {!!.57}
var
  BuffSize,
  InsertionPoint,
  InsertionSize : Integer;
begin
  BuffSize := Length(Text);
  InsertionPoint := TextLen + 1;
  InsertionSize := Length(NewText);
  { Is the buffer big enough to hold the new text? }
  if ((TextLen + InsertionSize) > BuffSize) then begin
    { Nope. So we'll grow it some. }
    BuffSize := (BuffSize * 2) + InsertionSize;
    SetLength(Text, BuffSize);
  end;

  { If we're not writing to the very end of the string, we'll have to
    make a hole for the new text. }
  if (TextLen <> 0) and (InsertionPoint <= TextLen) then
    Move(Text[InsertionPoint],
         Text[InsertionPoint + InsertionSize],
         InsertionSize * SizeOf(DOMChar));
  { Move the new text into that hole. }
  Move(NewText[1],
       Text[InsertionPoint],
       InsertionSize * SizeOf(DOMChar));

  { Update the length of data in the buffer. }
  TextLen := TextLen + InsertionSize;
end;

procedure XpDOMBufferDelete(var aText      : DOMString;
                                Index,
                                Count      : Integer;
                            var CurrLength : Integer);                 {!!.57}
var
  DeletionEndPoint : Integer;
begin
  DeletionEndPoint := (Index + Count);

  { Is the deletion within the buffer? }
  if (DeletionEndPoint <= CurrLength) then begin
    { If we're deleting upto the last character, we don't need to
      shift any characters. }
    if (DeletionEndPoint < CurrLength) then
      Move(aText[DeletionEndPoint + 1],
           aText[Index],
           (CurrLength - DeletionEndPoint + 1) * SizeOf(DOMChar));
    { If everything was successfull, update the current length. }
    CurrLength := CurrLength - Count;
  end;
end;

procedure XpDOMBufferInsert(NewText   : DOMString;
                        var Text      : DOMString;
                            Index   : Integer;
                        var TextLen  : Integer);                        {!!.57}
var
  BuffSize,
  InsertionSize : Integer;
begin
  BuffSize := Length(Text);
  InsertionSize := Length(NewText);
  { Is the buffer big enough to hold the new text? }
  if ((TextLen + InsertionSize) > BuffSize) then begin
    { Nope. So we'll grow it some. }
    BuffSize := (Trunc(BuffSize * 1.2)) + InsertionSize;
    SetLength(Text, BuffSize);
  end;

  { If we're not writing to the very end of the string, we'll have to
    make a hole for the new text. }
  if ((TextLen <> 0) and
      (Index <= TextLen)) then
    Move(Text[Index],
         Text[Index + InsertionSize],
         (TextLen - Index + 1) * SizeOf(DOMChar));
  { Move the new text into that hole. }
  Move(NewText[1],
       Text[Index],
       InsertionSize * SizeOf(DOMChar));

  { Update the length of data in the buffer. }
  TextLen := TextLen + InsertionSize;
end;

function XpEvaluateColor(sColor : string;
                     var wRed,
                         wGreen,
                         wBlue  : Integer) : Integer;
begin
  Result := 0;
  wRed := 0;
  wGreen := 0;
  wBlue := 0;

  if sColor = '' then
    Exit;
  sColor := LowerCase(sColor);
  if sColor[1] = '#' then begin
    Delete(sColor, 1, 1);
    wRed := StrToIntDef('$' + Copy(sColor, 1, 2), 0);
    wGreen := StrToIntDef('$' + Copy(sColor, 3, 2), 0);
    wBlue := StrToIntDef('$' + Copy(sColor, 5, 2), 0);
  end else if sColor[1] < 'd' then begin
    if sColor = 'aliceblue' then begin
      wRed := $f0;
      wGreen := $f8;
      wBlue := $ff;
    end else if sColor = 'antiquewhite' then begin
      wRed := $fa;
      wGreen := $eb;
      wBlue := $d7;
    end else if sColor = 'auqa' then begin
      wRed := $00;
      wGreen := $ff;
      wBlue := $ff;
    end else if sColor = 'auqamarine' then begin
      wRed := $7f;
      wGreen := $ff;
      wBlue := $d4;
    end else if sColor = 'azure' then begin
      wRed := $f0;
      wGreen := $ff;
      wBlue := $ff;
    end else if sColor = 'beige' then begin
      wRed := $f5;
      wGreen := $f5;
      wBlue := $dc;
    end else if sColor = 'bisque' then begin
      wRed := $ff;
      wGreen := $e4;
      wBlue := $c4;
    end else if sColor = 'blanchdalmond' then begin
      wRed := $ff;
      wGreen := $eb;
      wBlue := $cd;
    end else if sColor = 'blue' then begin
      wRed := $00;
      wGreen := $00;
      wBlue := $ff;
    end else if sColor = 'blueviolet' then begin
      wRed := $8a;
      wGreen := $2b;
      wBlue := $e2;
    end else if sColor = 'brown' then begin
      wRed := $a5;
      wGreen := $2a;
      wBlue := $2a;
    end else if sColor = 'burlywood' then begin
      wRed := $de;
      wGreen := $b8;
      wBlue := $87;
    end else if sColor = 'cadetblue' then begin
      wRed := $5f;
      wGreen := $9e;
      wBlue := $a0;
    end else if sColor = 'chartreuse' then begin
      wRed := $7f;
      wGreen := $ff;
      wBlue := $00;
    end else if sColor = 'chocolate' then begin
      wRed := $d2;
      wGreen := $69;
      wBlue := $1e;
    end else if sColor = 'coral' then begin
      wRed := $ff;
      wGreen := $7f;
      wBlue := $50;
    end else if sColor = 'cornflowerblue' then begin
      wRed := $64;
      wGreen := $95;
      wBlue := $ed;
    end else if sColor = 'cornsilk' then begin
      wRed := $ff;
      wGreen := $f8;
      wBlue := $dc;
    end else if sColor = 'crimson' then begin
      wRed := $dc;
      wGreen := $14;
      wBlue := $3c;
    end else if sColor = 'cyan' then begin
      wRed := $00;
      wGreen := $ff;
      wBlue := $ff;
    end;
  end else if sColor[1] < 'm' then begin
    if sColor = 'darkblue' then begin
      wRed := $00;
      wGreen := $00;
      wBlue := $8b;
    end else if sColor = 'darkcyan' then begin
      wRed := $00;
      wGreen := $8b;
      wBlue := $8b;
    end else if sColor = 'darkgoldenrod' then begin
      wRed := $b8;
      wGreen := $86;
      wBlue := $0b;
    end else if sColor = 'darkgray' then begin
      wRed := $a9;
      wGreen := $a9;
      wBlue := $a9;
    end else if sColor = 'darkgreen' then begin
      wRed := $00;
      wGreen := $64;
      wBlue := $00;
    end else if sColor = 'darkkhaki' then begin
      wRed := $bd;
      wGreen := $b7;
      wBlue := $6b;
    end else if sColor = 'darkmagenta' then begin
      wRed := $8b;
      wGreen := $00;
      wBlue := $8b;
    end else if sColor = 'darkolivegreen' then begin
      wRed := $55;
      wGreen := $6b;
      wBlue := $2f;
    end else if sColor = 'darkorange' then begin
      wRed := $ff;
      wGreen := $8c;
      wBlue := $00;
    end else if sColor = 'darkorchid' then begin
      wRed := $99;
      wGreen := $32;
      wBlue := $cc;
    end else if sColor = 'darkred' then begin
      wRed := $8b;
      wGreen := $00;
      wBlue := $00;
    end else if sColor = 'darksalmon' then begin
      wRed := $e9;
      wGreen := $96;
      wBlue := $7a;
    end else if sColor = 'darkseagreen' then begin
      wRed := $8f;
      wGreen := $bc;
      wBlue := $8f;
    end else if sColor = 'darkslateblue' then begin
      wRed := $48;
      wGreen := $3d;
      wBlue := $8b;
    end else if sColor = 'darkslategray' then begin
      wRed := $2f;
      wGreen := $4f;
      wBlue := $4f;
    end else if sColor = 'darkturquoise' then begin
      wRed := $00;
      wGreen := $ce;
      wBlue := $d1;
    end else if sColor = 'darkviolet' then begin
      wRed := $94;
      wGreen := $00;
      wBlue := $d3;
    end else if sColor = 'deeppink' then begin
      wRed := $ff;
      wGreen := $14;
      wBlue := $93;
    end
    else if sColor = 'deepskyblue' then begin
      wRed := $00;
      wGreen := $bf;
      wBlue := $ff;
    end else if sColor = 'dimgray' then begin
      wRed := $69;
      wGreen := $69;
      wBlue := $69;
    end else if sColor = 'dodgerblue' then begin
      wRed := $1e;
      wGreen := $90;
      wBlue := $ff;
    end else if sColor = 'firebrick' then begin
      wRed := $b2;
      wGreen := $22;
      wBlue := $22;
    end else if sColor = 'floralwhite' then begin
      wRed := $ff;
      wGreen := $fa;
      wBlue := $f0;
    end else if sColor = 'forestgreen' then begin
      wRed := $22;
      wGreen := $8b;
      wBlue := $22;
    end else if sColor = 'fuchsia' then begin
      wRed := $ff;
      wGreen := $00;
      wBlue := $ff;
    end else if sColor = 'gainsboro' then begin
      wRed := $dc;
      wGreen := $dc;
      wBlue := $dc;
    end else if sColor = 'ghostwhite' then begin
      wRed := $f8;
      wGreen := $f8;
      wBlue := $ff;
    end else if sColor = 'gold' then begin
      wRed := $ff;
      wGreen := $d7;
      wBlue := $00;
    end else if sColor = 'goldenrod' then begin
      wRed := $da;
      wGreen := $a5;
      wBlue := $20;
    end else if sColor = 'gray' then begin
      wRed := $80;
      wGreen := $80;
      wBlue := $80;
    end else if sColor = 'green' then begin
      wRed := $00;
      wGreen := $80;
      wBlue := $00;
    end else if sColor = 'greenyellow' then begin
      wRed := $ad;
      wGreen := $ff;
      wBlue := $2f;
    end else if sColor = 'honeydew' then begin
      wRed := $f0;
      wGreen := $ff;
      wBlue := $f0;
    end else if sColor = 'hotpink' then begin
      wRed := $ff;
      wGreen := $69;
      wBlue := $b4;
    end else if sColor = 'indianred' then begin
      wRed := $cd;
      wGreen := $5c;
      wBlue := $5c;
    end else if sColor = 'indigo' then begin
      wRed := $4b;
      wGreen := $00;
      wBlue := $82;
    end else if sColor = 'ivory' then begin
      wRed := $ff;
      wGreen := $ff;
      wBlue := $f0;
    end else if sColor = 'khaki' then begin
      wRed := $f0;
      wGreen := $e6;
      wBlue := $8c;
    end else if sColor = 'lavender' then begin
      wRed := $e6;
      wGreen := $e6;
      wBlue := $fa;
    end else if sColor = 'lavenderblush' then begin
      wRed := $ff;
      wGreen := $f0;
      wBlue := $f5;
    end else if sColor = 'lawngreen' then begin
      wRed := $7c;
      wGreen := $fc;
      wBlue := $00;
    end else if sColor = 'lemonchiffon' then begin
      wRed := $ff;
      wGreen := $fa;
      wBlue := $cd;
    end else if sColor = 'lightblue' then begin
      wRed := $ad;
      wGreen := $d8;
      wBlue := $e6;
    end else if sColor = 'lightcoral' then begin
      wRed := $f0;
      wGreen := $80;
      wBlue := $80;
    end else if sColor = 'lightcyan' then begin
      wRed := $e0;
      wGreen := $ff;
      wBlue := $ff;
    end else if sColor = 'lightgoldenrodyellow' then begin
      wRed := $fa;
      wGreen := $fa;
      wBlue := $d2;
    end else if sColor = 'lightgreen' then begin
      wRed := $90;
      wGreen := $ee;
      wBlue := $90;
    end else if sColor = 'lightgray' then begin
      wRed := $d3;
      wGreen := $d3;
      wBlue := $d3;
    end else if sColor = 'lightpink' then begin
      wRed := $ff;
      wGreen := $b6;
      wBlue := $c1;
    end else if sColor = 'lightsalmon' then begin
      wRed := $ff;
      wGreen := $a0;
      wBlue := $7a;
    end else if sColor = 'lightseagreen' then begin
      wRed := $20;
      wGreen := $b2;
      wBlue := $aa;
    end else if sColor = 'lightskyblue' then begin
      wRed := $87;
      wGreen := $ce;
      wBlue := $fa;
    end else if sColor = 'lightslategray' then begin
      wRed := $77;
      wGreen := $88;
      wBlue := $99;
    end else if sColor = 'lightsteelblue' then begin
      wRed := $b0;
      wGreen := $c4;
      wBlue := $de;
    end else if sColor = 'lightyellow' then begin
      wRed := $ff;
      wGreen := $ff;
      wBlue := $e0;
    end else if sColor = 'lime' then begin
      wRed := $00;
      wGreen := $ff;
      wBlue := $00;
    end else if sColor = 'limegreen' then begin
      wRed := $32;
      wGreen := $cd;
      wBlue := $32;
    end else if sColor = 'linen' then begin
      wRed := $fa;
      wGreen := $f0;
      wBlue := $e6;
    end;
  end else if sColor[1] < 's' then begin
    if sColor = 'magenta' then begin
      wRed := $ff;
      wGreen := $00;
      wBlue := $ff;
    end else if sColor = 'maroon' then begin
      wRed := $80;
      wGreen := $00;
      wBlue := $00;
    end else if sColor = 'mediumaquamarine' then begin
      wRed := $66;
      wGreen := $cd;
      wBlue := $aa;
    end else if sColor = 'mediumblue' then begin
      wRed := $00;
      wGreen := $00;
      wBlue := $cd;
    end else if sColor = 'mediumorchid' then begin
      wRed := $ba;
      wGreen := $55;
      wBlue := $d3;
    end else if sColor = 'mediumpurple' then begin
      wRed := $93;
      wGreen := $70;
      wBlue := $db;
    end else if sColor = 'mediumseagreen' then begin
      wRed := $3c;
      wGreen := $b3;
      wBlue := $71;
    end else if sColor = 'mediumslateblue' then begin
      wRed := $7b;
      wGreen := $68;
      wBlue := $ee;
    end else if sColor = 'mediumspringgreen' then begin
      wRed := $00;
      wGreen := $fa;
      wBlue := $9a;
    end else if sColor = 'mediumturquoise' then begin
      wRed := $48;
      wGreen := $d1;
      wBlue := $cc;
    end else if sColor = 'mediumvioletred' then begin
      wRed := $c7;
      wGreen := $15;
      wBlue := $85;
    end else if sColor = 'midnightblue' then begin
      wRed := $19;
      wGreen := $19;
      wBlue := $70;
    end else if sColor = 'mintcream' then begin
      wRed := $f5;
      wGreen := $ff;
      wBlue := $fa;
    end else if sColor = 'mistyrose' then begin
      wRed := $ff;
      wGreen := $e4;
      wBlue := $e1;
    end else if sColor = 'moccasin' then begin
      wRed := $ff;
      wGreen := $e4;
      wBlue := $b5;
    end else if sColor = 'navajowhite' then begin
      wRed := $ff;
      wGreen := $de;
      wBlue := $ad;
    end else if sColor = 'navy' then begin
      wRed := $00;
      wGreen := $00;
      wBlue := $80;
    end else if sColor = 'oldlace' then begin
      wRed := $fd;
      wGreen := $f5;
      wBlue := $e6;
    end else if sColor = 'olive' then begin
      wRed := $80;
      wGreen := $80;
      wBlue := $00;
    end else if sColor = 'olivedrab' then begin
      wRed := $6b;
      wGreen := $8e;
      wBlue := $23;
    end else if sColor = 'orange' then begin
      wRed := $ff;
      wGreen := $a5;
      wBlue := $00;
    end else if sColor = 'orangered' then begin
      wRed := $ff;
      wGreen := $45;
      wBlue := $00;
    end else if sColor = 'orchid' then begin
      wRed := $da;
      wGreen := $70;
      wBlue := $d6;
    end else if sColor = 'palegoldenrod' then begin
      wRed := $ee;
      wGreen := $e8;
      wBlue := $aa;
    end else if sColor = 'palegreen' then begin
      wRed := $98;
      wGreen := $fb;
      wBlue := $98;
    end else if sColor = 'paleturquoise' then begin
      wRed := $af;
      wGreen := $ee;
      wBlue := $ee;
    end else if sColor = 'palevioletred' then begin
      wRed := $db;
      wGreen := $70;
      wBlue := $93;
    end else if sColor = 'papayawhip' then begin
      wRed := $ff;
      wGreen := $ef;
      wBlue := $d5;
    end else if sColor = 'peachpuff' then begin
      wRed := $ff;
      wGreen := $da;
      wBlue := $b9;
    end else if sColor = 'peru' then begin
      wRed := $cd;
      wGreen := $85;
      wBlue := $3f;
    end else if sColor = 'pink' then begin
      wRed := $ff;
      wGreen := $c0;
      wBlue := $cb;
    end else if sColor = 'plum' then begin
      wRed := $dd;
      wGreen := $a0;
      wBlue := $dd;
    end else if sColor = 'powderblue' then begin
      wRed := $b0;
      wGreen := $e0;
      wBlue := $e6;
    end else if sColor = 'purple' then begin
      wRed := $80;
      wGreen := $00;
      wBlue := $80;
    end else if sColor = 'red' then begin
      wRed := $ff;
      wGreen := $00;
      wBlue := $00;
    end else if sColor = 'rosybrown' then begin
      wRed := $bc;
      wGreen := $8f;
      wBlue := $8f;
    end else if sColor = 'royalblue' then begin
      wRed := $41;
      wGreen := $69;
      wBlue := $e1;
    end;
  end else begin
    if sColor = 'saddlebrown' then begin
      wRed := $8b;
      wGreen := $45;
      wBlue := $13;
    end else if sColor = 'salmon' then begin
      wRed := $fa;
      wGreen := $80;
      wBlue := $72;
    end else if sColor = 'sandybrown' then begin
      wRed := $f4;
      wGreen := $a4;
      wBlue := $60;
    end else if sColor = 'seagreen' then begin
      wRed := $2e;
      wGreen := $8b;
      wBlue := $57;
    end else if sColor = 'seashell' then begin
      wRed := $ff;
      wGreen := $f5;
      wBlue := $ee;
    end else if sColor = 'sienna' then begin
      wRed := $a0;
      wGreen := $52;
      wBlue := $2d;
    end else if sColor = 'silver' then begin
      wRed := $c0;
      wGreen := $c0;
      wBlue := $c0;
    end else if sColor = 'skyblue' then begin
      wRed := $87;
      wGreen := $ce;
      wBlue := $eb;
    end else if sColor = 'slateblue' then begin
      wRed := $6a;
      wGreen := $5a;
      wBlue := $cd;
    end else if sColor = 'slategray' then begin
      wRed := $70;
      wGreen := $80;
      wBlue := $90;
    end else if sColor = 'snow' then begin
      wRed := $ff;
      wGreen := $fa;
      wBlue := $fa;
    end else if sColor = 'springgreen' then begin
      wRed := $00;
      wGreen := $ff;
      wBlue := $7f;
    end else if sColor = 'steelblue' then begin
      wRed := $46;
      wGreen := $82;
      wBlue := $b4;
    end else if sColor = 'tan' then begin
      wRed := $d2;
      wGreen := $b4;
      wBlue := $8c;
    end else if sColor = 'teal' then begin
      wRed := $00;
      wGreen := $80;
      wBlue := $80;
    end else if sColor = 'thistle' then begin
      wRed := $d8;
      wGreen := $bf;
      wBlue := $d8;
    end else if sColor = 'tomato' then begin
      wRed := $ff;
      wGreen := $63;
      wBlue := $47;
    end else if sColor = 'turquiose' then begin
      wRed := $40;
      wGreen := $e0;
      wBlue := $d0;
    end else if sColor = 'violet' then begin
      wRed := $ee;
      wGreen := $82;
      wBlue := $ee;
    end else if sColor = 'wheat' then begin
      wRed := $f5;
      wGreen := $de;
      wBlue := $b3;
    end else if sColor = 'white' then begin
      wRed := $ff;
      wGreen := $ff;
      wBlue := $ff;
    end else if sColor = 'whitesmoke' then begin
      wRed := $f5;
      wGreen := $f5;
      wBlue := $f5;
    end else if sColor = 'yellow' then begin
      wRed := $ff;
      wGreen := $ff;
      wBlue := $00;
    end else if sColor = 'yellowgreen' then begin
      wRed := $9a;
      wGreen := $cd;
      wBlue := $32;
    end;
  end;

  Result := (wBlue shl 16) or
            (wGreen shl 8) or
            (wRed);
end;
{--------}
function XpGetWord(const sTerm : string) : string;
var
  i : Integer;
begin
  Result := '';
  for i := 1 to Length(sTerm) do begin
    if (sTerm[i] = ' ') or
       (sTerm[i] = #$0d) then
      Exit;
    Result := Result + sTerm[i];
  end;
end;
{--------}
function XpFloor(const aValue : Double): Integer;
begin
  Result := Integer(Trunc(aValue));
  if Frac(aValue) < 0 then
    Dec(Result);
end;
{--------}
{!!.57 - Begin}
function XpIsRelativePath(const Path : string) : Boolean;
begin
  Result := ((Path[1] = '.') or
             ((Pos(':', Path) = 0) and
              (Pos('\', Path) > 1)));
end;
{!!.57 - End}
{--------}
function XpIsNumeric(c : Char) : Boolean;
begin
  Result := (c >= '0') and
            (c <= '9');
end;
{--------}
function XpMakeAbsolutePath(const sPath, sStyleURL : string) : string; {!!.57}
{ Construct absolute path based on style file location }
begin
  Result := sPath;
  if XpPos(':', sPath) <> 0 then
    Exit;
  Result := XpMakeFilePath(ExtractFilePath(sStyleURL), sPath);
end;
{--------}
function XpMakeFilePath(const sPath, sFile : DOMString) : DOMString;   {!!.57}
{ Construct a valid file path with a passed in path and file name. }
begin
  if Length(sPath) > 0 then begin
    if sPath[Length(sPath)] = PathDelim then
      Result := sPath + sFile
    else
      Result := sPath + PathDelim + sFile;
  end else
    Result := '.' + PathDelim + sFile;
end;
{Begin !!.53}
{--------}
function XpMapCharEncToString(const oEncoding : TXpCharEncoding) : DOMString;
begin
  case oEncoding of
    ceUnknown  : Result := XpsUnknown;
    ceUTF8     : Result := XpsUTF8;
    ceUTF16LE,
    ceUTF16BE  : Result := XpsUTF16;
    ceISO88591 : Result := XpsISO88591;
  end;  { case }
end;
{--------}
function XpMapStringToCharEnc(const sEncodeStr : DOMString) : TXpCharEncoding;
begin
  if sEncodeStr = XpsUTF8 then
    Result := ceUTF8
  else if sEncodeStr = XpsUTF16 then
    Result := ceUTF16LE
  else if sEncodeStr = XpsISO88591 then
    Result := ceISO88591
  else
    Result := ceUTF8;
end;
{End !!.53}
{--------}
function XpMin(a, b : Longint) : Longint;
begin
  if (a < b) then
    Result := a
  else
    Result := b;
end;
{--------}
function XpPos(const aSubStr, aString : DOMString) : Integer;
begin
  Result := AnsiPos(aSubStr, aString);
end;
{--------}
function XpRPos(const sSubStr, sTerm : DOMString) : Integer;
var
  cLast : DOMChar;
  i, j  : Integer;
begin
  j := Length(sSubStr);
  cLast := sSubStr[j];
  for i := Length(sTerm) downto j do begin
    if (sTerm[i] = cLast) and
       (Copy(sTerm, i - j + 1, j) = sSubStr) then begin
      Result := i - j + 1;
      Exit;
    end;
  end;
  Result := 0;
end;
{Begin !!.52}
{--------}
function XpStartsWith(const aSubStr, aString : DOMString) : Boolean;
var
  i : Integer;
  iLen : Integer;
begin
  Result := False;
  iLen := Length(aSubStr);
  if (iLen > 0) and (iLen <= Length(aString)) then begin
    Result := True;
    for i := 1 to Length(aSubStr) do begin
      if aSubStr[i] <> aString[i] then begin
        Result := False;
        break;
      end;
    end;
  end;
end;
{End !!.52}
{--------}
function XpStrEquals(const sStr1, sStr2 : DOMString; const iLen : Integer) : Boolean;
var
  iInx : Integer;
begin
  Result := True;
  for iInx := 1 to iLen do                                             {!!.55}
    if sStr1[iInx] <> sStr2[iInx] then begin
      Result := False;
      break;
    end;
end;
{--------}
function XpStrIEquals(const sStr1, sStr2 : DOMString; const iLen : Integer) : Boolean;
var
  iInx : Integer;
  sStrU1, sStrU2 : DOMString;
begin
  Result := True;
  sStrU1 := AnsiUpperCase(sStr1);
  sStrU2 := AnsiUpperCase(sStr2);
  for iInx := 1 to iLen do                                             {!!.55}
    if sStrU1[iInx] <> sStrU2[iInx] then begin
      Result := False;
      break;
    end;
end;
{--------}
function XpStringReplaceAll(const aString,                             {!!.52 - Start}
                                  aOldSubStr,
                                  aNewSubStr : DOMString) : DOMString;
var
  Tmp       : DOMString;
  P1        : Longint;
  OldStrLen : Longint;
begin
  Result := aString;
  if (aString <> '') and
     (aOldSubStr <> '') and
     (XpPos(aOldSubStr, aString) <> 0) then begin
    Tmp := aString;
    P1 := XpPos(aOldSubStr, aString);
    if (P1 > 0) then begin
      OldStrLen := Length(aOldSubStr);
      Result := XpCopy(Tmp, 1, P1-1) + aNewSubStr;
      while (P1 > 0) do begin
        Tmp := XpCopy(Tmp, P1+OldStrLen, (Length(Tmp) - (P1 - 1)));
        P1 := XpPos(aOldSubStr, Tmp);
        if (P1 > 0) then
          Result := Result + XpCopy(Tmp, 1, P1-1) + aNewSubStr
        else
          Result := Result + Tmp;
      end;
    end;
  end;
end;                                                                   {!!.52 - End}
{--------}
function XpStrToFloat(const aString : string) : Double;                {!!.56 - Added}
var
  i : Integer;
begin
  if (XpIsValidXPathNumber(aString)) then
    Val(aString, Result, i)
  else begin
    i := 1;
    Result := 0;
  end;

  if (i <> 0) then begin
    raise EConvertError.Create(aString +
                               ' can''t be converted to a valid XML number.');
  end;
end;
{--------}
function XpFloatToStr(const aFloat  : Double) : DOMString;             {!!.56 - Added}
var
  OldSep : Char;
begin
  OldSep := DecimalSeparator;
  DecimalSeparator := '.';
  try
    Result := FloatToStr(aFloat);
    if (not XpIsValidXPathNumber(Result)) then
      raise EConvertError.Create(FloatToStr(aFloat) +
                                 ' isn''t a valid number for XPath.');
  finally
    DecimalSeparator := OldSep;
  end;
end;
{=====================================================================}

{== Character conversion routines ====================================}
function XPIso88591ToUcs4(aInCh  : AnsiChar;
                      var aOutCh : TXpUcs4Char) : boolean;
begin
  {Note: the conversion from ISO-8859-1 to UCS-4 is very simple: the
         result is the original character}
  aOutCh := ord(aInCh);
  Result := true; {cannot fail}
end;
{--------}
function XPUcs4ToIso88591(aInCh  : TXpUcs4Char;
                      var aOutCh : AnsiChar) : Boolean;
begin
  {Note: the conversion from UCS-4 to ISO-8859-1 is very simple: if
         the character is contained in a byte, the result is the
         original character; otherwise the conversion cannot be done}
  aInCh := abs(aInCh);
  if (($00 <= aInCh) and (aInCh <= $FF)) then begin
    aOutCh := AnsiChar(aInCh and $FF);
    Result := true;
  end
  else begin
    Result := false;
    aOutCh := #0;
  end;
end;
{--------}
function XPUcs4ToUtf16(aInCh : TXpUcs4Char;
                   var aOutChI, aOutChII : DOMChar) : Boolean;         {!!.52}
begin
  aInCh := abs(aInCh);
  if (aInCh < $10000) then begin
    aOutChI := DOMChar(aInCh);                                         {!!.52}
    aOutChII := #0;
    Result := True;
  end
  else if (aInCh <= $10FFFF) then begin
    dec(aInCh, $10000);
    aOutChI := DOMChar($D800 or (aInCh shr 10));                       {!!.52}
    aOutChII := DOMChar($DC00 or (aInCh and $3FF));                    {!!.52}
    Result := True;
  end
  else begin
    aOutChI := #0;
    aOutChII := #0;
    Result := False;
  end;
end;
{--------}
function XpUcs4ToWideChar(const aInChar : TXpUcs4Char;
                            var aOutWS  : DOMChar) : Boolean;
begin                                                                  {!!.55 - Start}
  if (aInChar < $10000) then begin
    aOutWS := DOMChar(aInChar);
    Result := True;
  end else begin
    aOutWS := #0;
    Result := False;
  end;
end;                                                                   {!!.55 - End}
{--------}
function XPUtf16ToUcs4(aInChI,
                       aInChII   : DOMChar;                            {!!.52}
                   var aOutCh    : TXpUcs4Char;                        {!!.51}
                   var aBothUsed : Boolean) : Boolean;                 {!!.51}
begin
  aBothUsed := False;                                                  {!!.51}
  if (aInChI < #$D800) or (aInChI > #$DFFF) then begin
    aOutCh := Integer(aInChI);
    Result := True;
  end
  else if (aInChI < #$DC00) and
          ((#$DC00 <= aInChII) and (aInChII <= #$DFFF)) then begin
    aOutCh := ((integer(aInChI) and $3FF) shl 10) or
              (integer(aInChII) and $3FF);
    aBothUsed := True;                                                 {!!.51}
    Result := True;
  end
  else begin
    Result := False;
    aOUtCh := 0;
  end;
end;
{--------}
function XPUcs4ToUtf8(aInCh  : TXpUcs4Char;
                  var aOutCh : TXpUtf8Char) : Boolean;
begin
  aInCh := abs(aInCh);
  {if the UCS-4 value is $00 to $7f, no conversion is required}
  if (aInCh < $80) then begin
    aOutCh[0] := #1;
    aOutCh[1] := AnsiChar(aInCh);
  end
  {if the UCS-4 value is $80 to $7ff, a two character string is
   produced}
  else if (aInCh < $800) then begin
    aOutCh[0] := #2;
    aOutCh[1] := AnsiChar($C0 or (aInCh shr 6));
    aOutCh[2] := AnsiChar($80 or (aInCh and $3F));
  end
  {if the UCS-4 value is $800 to $ffff, a three character string is
   produced}
  else if (aInCh < $10000) then begin
    aOutCh[0] := #3;
    aOutCh[1] := AnsiChar($E0 or (aInCh shr 12));
    aOutCh[2] := AnsiChar($80 or ((aInCh shr 6) and $3F));
    aOutCh[3] := AnsiChar($80 or (aInCh and $3F));
  end
  {NOTE: the following if clauses will be very rarely used since the
         majority of characters will be unicode characters: $0000 to
         $FFFF}
  {if the UCS-4 value is $10000 to $1fffff, a four character string
   is produced}
  else if (aInCh < $200000) then begin
    aOutCh[0] := #4;
    aOutCh[1] := AnsiChar($F0 or (aInCh shr 18));
    aOutCh[2] := AnsiChar($80 or ((aInCh shr 12) and $3F));
    aOutCh[3] := AnsiChar($80 or ((aInCh shr 6) and $3F));
    aOutCh[4] := AnsiChar($80 or (aInCh and $3F));
  end
  {if the UCS-4 value is $200000 to $3ffffff, a five character
   string is produced}
  else if (aInCh < $4000000) then begin
    aOutCh[0] := #5;
    aOutCh[1] := AnsiChar($F8 or (aInCh shr 24));
    aOutCh[2] := AnsiChar($80 or ((aInCh shr 18) and $3F));
    aOutCh[3] := AnsiChar($80 or ((aInCh shr 12) and $3F));
    aOutCh[4] := AnsiChar($80 or ((aInCh shr 6) and $3F));
    aOutCh[5] := AnsiChar($80 or (aInCh and $3F));
  end
  {for all other UCS-4 values, a six character string is produced}
  else begin
    aOutCh[0] := #6;
    aOutCh[1] := AnsiChar($FC or (aInCh shr 30));
    aOutCh[2] := AnsiChar($80 or ((aInCh shr 24) and $3F));
    aOutCh[3] := AnsiChar($80 or ((aInCh shr 18) and $3F));
    aOutCh[4] := AnsiChar($80 or ((aInCh shr 12) and $3F));
    aOutCh[5] := AnsiChar($80 or ((aInCh shr 6) and $3F));
    aOutCh[6] := AnsiChar($80 or (aInCh and $3F));
  end;
  Result := True; {cannot fail}
end;
{--------}
function XPUtf8ToUcs4(const aInCh  : TXpUtf8Char;                      {!!.52 - Rewrite - Start}
                            aBytes : Integer;
                        var aOutCh : TXpUcs4Char) : Boolean;
var
  InFirstByte : AnsiChar;
  InCharLen   : Integer;
  i           : Integer;
begin
  InFirstByte := aInCh[1];
  InCharLen := Length(aInCh);
  {the length of the UTF-8 character cannot be zero and must match
   that of the first ASCII character in the string}
  if ((InCharLen = 0) or
      (InCharLen <> aBytes)) then begin
    Result := False;
    aOutCh := 0;
    Exit;
  end;
  {all subsequent characters must have the most significant bit set
   and the next to most significant digit clear; we'll test for this
   as we go along}
  {get the bits from the first ASCII character}
  if (InFirstByte <= #$7F) then
    aOutCh := Ord(InFirstByte)
  else if (InFirstByte <= #$DF) then
    aOutCh := Ord(InFirstByte) and $1F
  else if (InFirstByte <= #$EF) then
    aOutCh := Ord(InFirstByte) and $0F
  else if (InFirstByte <= #$F7) then
    aOutCh := Ord(InFirstByte) and $07
  else if (InFirstByte <= #$FB) then
    aOutCh := Ord(InFirstByte) and $03
  else
    aOutCh := Ord(InFirstByte) and $01;
  {get the bits from the remaining ASCII characters}
  for i := 2 to InCharLen do begin
    if ((Byte(aInCh[i]) and $C0) <> $80) then begin
      Result := False;
      aOutCh := 0;
      Exit;
    end;
    aOutCh := (aOutCh shl 6) or (Byte(aInCh[i]) and $3F);
  end;
  {success}
  Result := True;
end;                                                                   {!!.52 - Rewrite - End}
{====================================================================}


{===UTF specials=====================================================}
function XPGetLengthUtf8(const aCh : AnsiChar) : Byte;                 {!!.52}
begin
  if (aCh <= #$7F) then
    Result := 1
  else if (aCh <= #$BF) then
    Result := 0              { $80--$BF is an error }
  else if (aCh <= #$DF) then
    Result := 2
  else if (aCh <= #$EF) then
    Result := 3
  else if (aCh <= #$F7) then
    Result := 4
  else if (aCh <= #$FB) then
    Result := 5
  else if (aCh <= #$FD) then
    Result := 6
  else
    Result := 0;             { $FE, $FF is an error }
end;
{--------}
function XPIsUtf8Trail(const aCh : AnsiChar) : boolean;                {!!.52}
begin
  Result := (#$80 <= aCh) and (aCh <= #$BF);
end;
{====================================================================}


{===character classes================================================}
function XPIsBaseChar(aCh : TXpUcs4Char) : boolean;
begin
  {[85] BaseChar ::=
                [#x0041-#x005A] | [#x0061-#x007A] | [#x00C0-#x00D6]
                | [#x00D8-#x00F6] | [#x00F8-#x00FF] | [#x0100-#x0131]
                | [#x0134-#x013E] | [#x0141-#x0148] | [#x014A-#x017E]
                | [#x0180-#x01C3] | [#x01CD-#x01F0] | [#x01F4-#x01F5]
                | [#x01FA-#x0217] | [#x0250-#x02A8] | [#x02BB-#x02C1]
                | #x0386 | [#x0388-#x038A] | #x038C | [#x038E-#x03A1]
                | [#x03A3-#x03CE] | [#x03D0-#x03D6] | #x03DA | #x03DC
                | #x03DE | #x03E0 | [#x03E2-#x03F3] | [#x0401-#x040C]
                | [#x040E-#x044F] | [#x0451-#x045C] | [#x045E-#x0481]
                | [#x0490-#x04C4] | [#x04C7-#x04C8] | [#x04CB-#x04CC]
                | [#x04D0-#x04EB] | [#x04EE-#x04F5] | [#x04F8-#x04F9]
                | [#x0531-#x0556] | #x0559 | [#x0561-#x0586]
                | [#x05D0-#x05EA] | [#x05F0-#x05F2] | [#x0621-#x063A]
                | [#x0641-#x064A] | [#x0671-#x06B7] | [#x06BA-#x06BE]
                | [#x06C0-#x06CE] | [#x06D0-#x06D3] | #x06D5
                | [#x06E5-#x06E6] | [#x0905-#x0939] | #x093D
                | [#x0958-#x0961] | [#x0985-#x098C] | [#x098F-#x0990]
                | [#x0993-#x09A8] | [#x09AA-#x09B0] | #x09B2
                | [#x09B6-#x09B9] | [#x09DC-#x09DD] | [#x09DF-#x09E1]
                | [#x09F0-#x09F1] | [#x0A05-#x0A0A] | [#x0A0F-#x0A10]
                | [#x0A13-#x0A28] | [#x0A2A-#x0A30] | [#x0A32-#x0A33]
                | [#x0A35-#x0A36] | [#x0A38-#x0A39] | [#x0A59-#x0A5C]
                | #x0A5E | [#x0A72-#x0A74] | [#x0A85-#x0A8B] | #x0A8D
                | [#x0A8F-#x0A91] | [#x0A93-#x0AA8] | [#x0AAA-#x0AB0]
                | [#x0AB2-#x0AB3] | [#x0AB5-#x0AB9] | #x0ABD | #x0AE0
                | [#x0B05-#x0B0C] | [#x0B0F-#x0B10] | [#x0B13-#x0B28]
                | [#x0B2A-#x0B30] | [#x0B32-#x0B33] | [#x0B36-#x0B39]
                | #x0B3D | [#x0B5C-#x0B5D] | [#x0B5F-#x0B61]
                | [#x0B85-#x0B8A] | [#x0B8E-#x0B90] | [#x0B92-#x0B95]
                | [#x0B99-#x0B9A] | #x0B9C | [#x0B9E-#x0B9F]
                | [#x0BA3-#x0BA4] | [#x0BA8-#x0BAA] | [#x0BAE-#x0BB5]
                | [#x0BB7-#x0BB9] | [#x0C05-#x0C0C] | [#x0C0E-#x0C10]
                | [#x0C12-#x0C28] | [#x0C2A-#x0C33] | [#x0C35-#x0C39]
                | [#x0C60-#x0C61] | [#x0C85-#x0C8C] | [#x0C8E-#x0C90]
                | [#x0C92-#x0CA8] | [#x0CAA-#x0CB3] | [#x0CB5-#x0CB9]
                | #x0CDE | [#x0CE0-#x0CE1] | [#x0D05-#x0D0C]
                | [#x0D0E-#x0D10] | [#x0D12-#x0D28] | [#x0D2A-#x0D39]
                | [#x0D60-#x0D61] | [#x0E01-#x0E2E] | #x0E30
                | [#x0E32-#x0E33] | [#x0E40-#x0E45] | [#x0E81-#x0E82]
                | #x0E84 | [#x0E87-#x0E88] | #x0E8A | #x0E8D
                | [#x0E94-#x0E97] | [#x0E99-#x0E9F] | [#x0EA1-#x0EA3]
                | #x0EA5 | #x0EA7 | [#x0EAA-#x0EAB] | [#x0EAD-#x0EAE]
                | #x0EB0 | [#x0EB2-#x0EB3] | #x0EBD | [#x0EC0-#x0EC4]
                | [#x0F40-#x0F47] | [#x0F49-#x0F69] | [#x10A0-#x10C5]
                | [#x10D0-#x10F6] | #x1100 | [#x1102-#x1103]
                | [#x1105-#x1107] | #x1109 | [#x110B-#x110C]
                | [#x110E-#x1112] | #x113C | #x113E | #x1140 | #x114C
                | #x114E | #x1150 | [#x1154-#x1155] | #x1159
                | [#x115F-#x1161] | #x1163 | #x1165 | #x1167 | #x1169
                | [#x116D-#x116E] | [#x1172-#x1173] | #x1175 | #x119E
                | #x11A8 | #x11AB | [#x11AE-#x11AF] | [#x11B7-#x11B8]
                | #x11BA | [#x11BC-#x11C2] | #x11EB | #x11F0 | #x11F9
                | [#x1E00-#x1E9B] | [#x1EA0-#x1EF9] | [#x1F00-#x1F15]
                | [#x1F18-#x1F1D] | [#x1F20-#x1F45] | [#x1F48-#x1F4D]
                | [#x1F50-#x1F57] | #x1F59 | #x1F5B | #x1F5D
                | [#x1F5F-#x1F7D] | [#x1F80-#x1FB4] | [#x1FB6-#x1FBC]
                | #x1FBE | [#x1FC2-#x1FC4] | [#x1FC6-#x1FCC]
                | [#x1FD0-#x1FD3] | [#x1FD6-#x1FDB] | [#x1FE0-#x1FEC]
                | [#x1FF2-#x1FF4] | [#x1FF6-#x1FFC] | #x2126
                | [#x212A-#x212B] | #x212E | [#x2180-#x2182]
                | [#x3041-#x3094] | [#x30A1-#x30FA] | [#x3105-#x312C]
                | [#xAC00-#xD7A3] }
  Result := (($0041 <= aCh) and (aCh <= $005A)) or
            (($0061 <= aCh) and (aCh <= $007A)) or
            (($00C0 <= aCh) and (aCh <= $00D6)) or
            (($00D8 <= aCh) and (aCh <= $00F6)) or
            (($00F8 <= aCh) and (aCh <= $00FF)) or
            (($0100 <= aCh) and (aCh <= $0131)) or
            (($0134 <= aCh) and (aCh <= $013E)) or
            (($0141 <= aCh) and (aCh <= $0148)) or
            (($014A <= aCh) and (aCh <= $017E)) or
            (($0180 <= aCh) and (aCh <= $01C3)) or
            (($01CD <= aCh) and (aCh <= $01F0)) or
            (($01F4 <= aCh) and (aCh <= $01F5)) or
            (($01FA <= aCh) and (aCh <= $0217)) or
            (($0250 <= aCh) and (aCh <= $02A8)) or
            (($02BB <= aCh) and (aCh <= $02C1)) or (aCh = $0386) or
            (($0388 <= aCh) and (aCh <= $038A)) or (aCh = $038C) or
            (($038E <= aCh) and (aCh <= $03A1)) or
            (($03A3 <= aCh) and (aCh <= $03CE)) or
            (($03D0 <= aCh) and (aCh <= $03D6)) or
            (aCh = $03DA) or (aCh = $03DC) or
            (aCh = $03DE) or (aCh = $03E0) or
            (($03E2 <= aCh) and (aCh <= $03F3)) or
            (($0401 <= aCh) and (aCh <= $040C)) or
            (($040E <= aCh) and (aCh <= $044F)) or
            (($0451 <= aCh) and (aCh <= $045C)) or
            (($045E <= aCh) and (aCh <= $0481)) or
            (($0490 <= aCh) and (aCh <= $04C4)) or
            (($04C7 <= aCh) and (aCh <= $04C8)) or
            (($04CB <= aCh) and (aCh <= $04CC)) or
            (($04D0 <= aCh) and (aCh <= $04EB)) or
            (($04EE <= aCh) and (aCh <= $04F5)) or
            (($04F8 <= aCh) and (aCh <= $04F9)) or
            (($0531 <= aCh) and (aCh <= $0556)) or (aCh = $0559) or
            (($0561 <= aCh) and (aCh <= $0586)) or
            (($05D0 <= aCh) and (aCh <= $05EA)) or
            (($05F0 <= aCh) and (aCh <= $05F2)) or
            (($0621 <= aCh) and (aCh <= $063A)) or
            (($0641 <= aCh) and (aCh <= $064A)) or
            (($0671 <= aCh) and (aCh <= $06B7)) or
            (($06BA <= aCh) and (aCh <= $06BE)) or
            (($06C0 <= aCh) and (aCh <= $06CE)) or
            (($06D0 <= aCh) and (aCh <= $06D3)) or (aCh = $06D5) or
            (($06E5 <= aCh) and (aCh <= $06E6)) or
            (($0905 <= aCh) and (aCh <= $0939)) or (aCh = $093D) or
            (($0958 <= aCh) and (aCh <= $0961)) or
            (($0985 <= aCh) and (aCh <= $098C)) or
            (($098F <= aCh) and (aCh <= $0990)) or
            (($0993 <= aCh) and (aCh <= $09A8)) or
            (($09AA <= aCh) and (aCh <= $09B0)) or (aCh = $09B2) or
            (($09B6 <= aCh) and (aCh <= $09B9)) or
            (($09DC <= aCh) and (aCh <= $09DD)) or
            (($09DF <= aCh) and (aCh <= $09E1)) or
            (($09F0 <= aCh) and (aCh <= $09F1)) or
            (($0A05 <= aCh) and (aCh <= $0A0A)) or
            (($0A0F <= aCh) and (aCh <= $0A10)) or
            (($0A13 <= aCh) and (aCh <= $0A28)) or
            (($0A2A <= aCh) and (aCh <= $0A30)) or
            (($0A32 <= aCh) and (aCh <= $0A33)) or
            (($0A35 <= aCh) and (aCh <= $0A36)) or
            (($0A38 <= aCh) and (aCh <= $0A39)) or
            (($0A59 <= aCh) and (aCh <= $0A5C)) or (aCh = $0A5E) or
            (($0A72 <= aCh) and (aCh <= $0A74)) or
            (($0A85 <= aCh) and (aCh <= $0A8B)) or (aCh = $0A8D) or
            (($0A8F <= aCh) and (aCh <= $0A91)) or
            (($0A93 <= aCh) and (aCh <= $0AA8)) or
            (($0AAA <= aCh) and (aCh <= $0AB0)) or
            (($0AB2 <= aCh) and (aCh <= $0AB3)) or
            (($0AB5 <= aCh) and (aCh <= $0AB9)) or
            (aCh = $0ABD) or (aCh = $0AE0) or
            (($0B05 <= aCh) and (aCh <= $0B0C)) or
            (($0B0F <= aCh) and (aCh <= $0B10)) or
            (($0B13 <= aCh) and (aCh <= $0B28)) or
            (($0B2A <= aCh) and (aCh <= $0B30)) or
            (($0B32 <= aCh) and (aCh <= $0B33)) or
            (($0B36 <= aCh) and (aCh <= $0B39)) or (aCh = $0B3D) or
            (($0B5C <= aCh) and (aCh <= $0B5D)) or
            (($0B5F <= aCh) and (aCh <= $0B61)) or
            (($0B85 <= aCh) and (aCh <= $0B8A)) or
            (($0B8E <= aCh) and (aCh <= $0B90)) or
            (($0B92 <= aCh) and (aCh <= $0B95)) or
            (($0B99 <= aCh) and (aCh <= $0B9A)) or (aCh = $0B9C) or
            (($0B9E <= aCh) and (aCh <= $0B9F)) or
            (($0BA3 <= aCh) and (aCh <= $0BA4)) or
            (($0BA8 <= aCh) and (aCh <= $0BAA)) or
            (($0BAE <= aCh) and (aCh <= $0BB5)) or
            (($0BB7 <= aCh) and (aCh <= $0BB9)) or
            (($0C05 <= aCh) and (aCh <= $0C0C)) or
            (($0C0E <= aCh) and (aCh <= $0C10)) or
            (($0C12 <= aCh) and (aCh <= $0C28)) or
            (($0C2A <= aCh) and (aCh <= $0C33)) or
            (($0C35 <= aCh) and (aCh <= $0C39)) or
            (($0C60 <= aCh) and (aCh <= $0C61)) or
            (($0C85 <= aCh) and (aCh <= $0C8C)) or
            (($0C8E <= aCh) and (aCh <= $0C90)) or
            (($0C92 <= aCh) and (aCh <= $0CA8)) or
            (($0CAA <= aCh) and (aCh <= $0CB3)) or
            (($0CB5 <= aCh) and (aCh <= $0CB9)) or (aCh = $0CDE) or
            (($0CE0 <= aCh) and (aCh <= $0CE1)) or
            (($0D05 <= aCh) and (aCh <= $0D0C)) or
            (($0D0E <= aCh) and (aCh <= $0D10)) or
            (($0D12 <= aCh) and (aCh <= $0D28)) or
            (($0D2A <= aCh) and (aCh <= $0D39)) or
            (($0D60 <= aCh) and (aCh <= $0D61)) or
            (($0E01 <= aCh) and (aCh <= $0E2E)) or (aCh = $0E30) or
            (($0E32 <= aCh) and (aCh <= $0E33)) or
            (($0E40 <= aCh) and (aCh <= $0E45)) or
            (($0E81 <= aCh) and (aCh <= $0E82)) or (aCh = $0E84) or
            (($0E87 <= aCh) and (aCh <= $0E88)) or
            (aCh = $0E8A) or (aCh = $0E8D) or
            (($0E94 <= aCh) and (aCh <= $0E97)) or
            (($0E99 <= aCh) and (aCh <= $0E9F)) or
            (($0EA1 <= aCh) and (aCh <= $0EA3)) or
            (aCh = $0EA5) or (aCh = $0EA7) or
            (($0EAA <= aCh) and (aCh <= $0EAB)) or
            (($0EAD <= aCh) and (aCh <= $0EAE)) or (aCh = $0EB0) or
            (($0EB2 <= aCh) and (aCh <= $0EB3)) or (aCh = $0EBD) or
            (($0EC0 <= aCh) and (aCh <= $0EC4)) or
            (($0F40 <= aCh) and (aCh <= $0F47)) or
            (($0F49 <= aCh) and (aCh <= $0F69)) or
            (($10A0 <= aCh) and (aCh <= $10C5)) or
            (($10D0 <= aCh) and (aCh <= $10F6)) or (aCh = $1100) or
            (($1102 <= aCh) and (aCh <= $1103)) or
            (($1105 <= aCh) and (aCh <= $1107)) or (aCh = $1109) or
            (($110B <= aCh) and (aCh <= $110C)) or
            (($110E <= aCh) and (aCh <= $1112)) or
            (aCh = $113C) or (aCh = $113E) or (aCh = $1140) or
            (aCh = $114C) or (aCh = $114E) or (aCh = $1150) or
            (($1154 <= aCh) and (aCh <= $1155)) or (aCh = $1159) or
            (($115F <= aCh) and (aCh <= $1161)) or
            (aCh = $1163) or (aCh = $1165) or
            (aCh = $1167) or (aCh = $1169) or
            (($116D <= aCh) and (aCh <= $116E)) or
            (($1172 <= aCh) and (aCh <= $1173)) or
            (aCh = $1175) or (aCh = $119E) or
            (aCh = $11A8) or (aCh = $11AB) or
            (($11AE <= aCh) and (aCh <= $11AF)) or
            (($11B7 <= aCh) and (aCh <= $11B8)) or (aCh = $11BA) or
            (($11BC <= aCh) and (aCh <= $11C2)) or
            (aCh = $11EB) or (aCh = $11F0) or (aCh = $11F9) or
            (($1E00 <= aCh) and (aCh <= $1E9B)) or
            (($1EA0 <= aCh) and (aCh <= $1EF9)) or
            (($1F00 <= aCh) and (aCh <= $1F15)) or
            (($1F18 <= aCh) and (aCh <= $1F1D)) or
            (($1F20 <= aCh) and (aCh <= $1F45)) or
            (($1F48 <= aCh) and (aCh <= $1F4D)) or
            (($1F50 <= aCh) and (aCh <= $1F57)) or
            (aCh = $1F59) or (aCh = $1F5B) or (aCh = $1F5D) or
            (($1F5F <= aCh) and (aCh <= $1F7D)) or
            (($1F80 <= aCh) and (aCh <= $1FB4)) or
            (($1FB6 <= aCh) and (aCh <= $1FBC)) or (aCh = $1FBE) or
            (($1FC2 <= aCh) and (aCh <= $1FC4)) or
            (($1FC6 <= aCh) and (aCh <= $1FCC)) or
            (($1FD0 <= aCh) and (aCh <= $1FD3)) or
            (($1FD6 <= aCh) and (aCh <= $1FDB)) or
            (($1FE0 <= aCh) and (aCh <= $1FEC)) or
            (($1FF2 <= aCh) and (aCh <= $1FF4)) or
            (($1FF6 <= aCh) and (aCh <= $1FFC)) or (aCh = $2126) or
            (($212A <= aCh) and (aCh <= $212B)) or (aCh = $212E) or
            (($2180 <= aCh) and (aCh <= $2182)) or
            (($3041 <= aCh) and (aCh <= $3094)) or
            (($30A1 <= aCh) and (aCh <= $30FA)) or
            (($3105 <= aCh) and (aCh <= $312C)) or
            (($AC00 <= aCh) and (aCh <= $D7A3));
end;
{--------}
function XPIsChar(const aCh : TXpUcs4Char) : boolean;                  {!!.52}
begin
  {[2] Char ::= #x9 | #xA | #xD | [#x20-#xD7FF] |
                [#xE000-#xFFFD] | [#x10000-#x10FFFF] }
  Result := (aCh = 9) or (aCh = 10) or (aCh = 13) or
            (($20 <= aCh) and (aCh <= $D7FF)) or
            (($E000 <= aCh) and (aCh <= $FFFD)) or
            (($10000 <= aCh) and (aCh <= $10FFFF));
end;
{--------}
function XPIsCombiningChar(aCh : TXpUcs4Char) : boolean;
begin
  {[87] CombiningChar ::=
                     [#x0300-#x0345] | [#x0360-#x0361] | [#x0483-#x0486]
                     | [#x0591-#x05A1] | [#x05A3-#x05B9] | [#x05BB-#x05BD]
                     | #x05BF | [#x05C1-#x05C2] | #x05C4 | [#x064B-#x0652]
                     | #x0670 | [#x06D6-#x06DC] | [#x06DD-#x06DF]
                     | [#x06E0-#x06E4] | [#x06E7-#x06E8] | [#x06EA-#x06ED]
                     | [#x0901-#x0903] | #x093C | [#x093E-#x094C] | #x094D
                     | [#x0951-#x0954] | [#x0962-#x0963] | [#x0981-#x0983]
                     | #x09BC | #x09BE | #x09BF | [#x09C0-#x09C4]
                     | [#x09C7-#x09C8] | [#x09CB-#x09CD] | #x09D7
                     | [#x09E2-#x09E3] | #x0A02 | #x0A3C | #x0A3E | #x0A3F
                     | [#x0A40-#x0A42] | [#x0A47-#x0A48] | [#x0A4B-#x0A4D]
                     | [#x0A70-#x0A71] | [#x0A81-#x0A83] | #x0ABC
                     | [#x0ABE-#x0AC5] | [#x0AC7-#x0AC9] | [#x0ACB-#x0ACD]
                     | [#x0B01-#x0B03] | #x0B3C | [#x0B3E-#x0B43]
                     | [#x0B47-#x0B48] | [#x0B4B-#x0B4D] | [#x0B56-#x0B57]
                     | [#x0B82-#x0B83] | [#x0BBE-#x0BC2] | [#x0BC6-#x0BC8]
                     | [#x0BCA-#x0BCD] | #x0BD7 | [#x0C01-#x0C03]
                     | [#x0C3E-#x0C44] | [#x0C46-#x0C48] | [#x0C4A-#x0C4D]
                     | [#x0C55-#x0C56] | [#x0C82-#x0C83] | [#x0CBE-#x0CC4]
                     | [#x0CC6-#x0CC8] | [#x0CCA-#x0CCD] | [#x0CD5-#x0CD6]
                     | [#x0D02-#x0D03] | [#x0D3E-#x0D43] | [#x0D46-#x0D48]
                     | [#x0D4A-#x0D4D] | #x0D57 | #x0E31 | [#x0E34-#x0E3A]
                     | [#x0E47-#x0E4E] | #x0EB1 | [#x0EB4-#x0EB9]
                     | [#x0EBB-#x0EBC] | [#x0EC8-#x0ECD] | [#x0F18-#x0F19]
                     | #x0F35 | #x0F37 | #x0F39 | #x0F3E | #x0F3F
                     | [#x0F71-#x0F84] | [#x0F86-#x0F8B] | [#x0F90-#x0F95]
                     | #x0F97 | [#x0F99-#x0FAD] | [#x0FB1-#x0FB7] | #x0FB9
                     | [#x20D0-#x20DC] | #x20E1 | [#x302A-#x302F] | #x3099
                     | #x309A }
  Result := (($0300 <= aCh) and (aCh <= $0345)) or
            (($0360 <= aCh) and (aCh <= $0361)) or
            (($0483 <= aCh) and (aCh <= $0486)) or
            (($0591 <= aCh) and (aCh <= $05A1)) or
            (($05A3 <= aCh) and (aCh <= $05B9)) or
            (($05BB <= aCh) and (aCh <= $05BD)) or (aCh = $05BF) or
            (($05C1 <= aCh) and (aCh <= $05C2)) or (aCh = $05C4) or
            (($064B <= aCh) and (aCh <= $0652)) or (aCh = $0670) or
            (($06D6 <= aCh) and (aCh <= $06DC)) or
            (($06DD <= aCh) and (aCh <= $06DF)) or
            (($06E0 <= aCh) and (aCh <= $06E4)) or
            (($06E7 <= aCh) and (aCh <= $06E8)) or
            (($06EA <= aCh) and (aCh <= $06ED)) or
            (($0901 <= aCh) and (aCh <= $0903)) or (aCh = $093C) or
            (($093E <= aCh) and (aCh <= $094C)) or (aCh = $094D) or
            (($0951 <= aCh) and (aCh <= $0954)) or
            (($0962 <= aCh) and (aCh <= $0963)) or
            (($0981 <= aCh) and (aCh <= $0983)) or
            (aCh = $09BC) or (aCh = $09BE) or (aCh = $09BF) or
            (($09C0 <= aCh) and (aCh <= $09C4)) or
            (($09C7 <= aCh) and (aCh <= $09C8)) or
            (($09CB <= aCh) and (aCh <= $09CD)) or (aCh = $09D7) or
            (($09E2 <= aCh) and (aCh <= $09E3)) or
            (aCh = $0A02) or (aCh = $0A3C) or
            (aCh = $0A3E) or (aCh = $0A3F) or
            (($0A40 <= aCh) and (aCh <= $0A42)) or
            (($0A47 <= aCh) and (aCh <= $0A48)) or
            (($0A4B <= aCh) and (aCh <= $0A4D)) or
            (($0A70 <= aCh) and (aCh <= $0A71)) or
            (($0A81 <= aCh) and (aCh <= $0A83)) or
            (aCh = $0ABC) or (($0ABE <= aCh) and (aCh <= $0AC5)) or
            (($0AC7 <= aCh) and (aCh <= $0AC9)) or
            (($0ACB <= aCh) and (aCh <= $0ACD)) or
            (($0B01 <= aCh) and (aCh <= $0B03)) or (aCh = $0B3C) or
            (($0B3E <= aCh) and (aCh <= $0B43)) or
            (($0B47 <= aCh) and (aCh <= $0B48)) or
            (($0B4B <= aCh) and (aCh <= $0B4D)) or
            (($0B56 <= aCh) and (aCh <= $0B57)) or
            (($0B82 <= aCh) and (aCh <= $0B83)) or
            (($0BBE <= aCh) and (aCh <= $0BC2)) or
            (($0BC6 <= aCh) and (aCh <= $0BC8)) or
            (($0BCA <= aCh) and (aCh <= $0BCD)) or (aCh = $0BD7) or
            (($0C01 <= aCh) and (aCh <= $0C03)) or
            (($0C3E <= aCh) and (aCh <= $0C44)) or
            (($0C46 <= aCh) and (aCh <= $0C48)) or
            (($0C4A <= aCh) and (aCh <= $0C4D)) or
            (($0C55 <= aCh) and (aCh <= $0C56)) or
            (($0C82 <= aCh) and (aCh <= $0C83)) or
            (($0CBE <= aCh) and (aCh <= $0CC4)) or
            (($0CC6 <= aCh) and (aCh <= $0CC8)) or
            (($0CCA <= aCh) and (aCh <= $0CCD)) or
            (($0CD5 <= aCh) and (aCh <= $0CD6)) or
            (($0D02 <= aCh) and (aCh <= $0D03)) or
            (($0D3E <= aCh) and (aCh <= $0D43)) or
            (($0D46 <= aCh) and (aCh <= $0D48)) or
            (($0D4A <= aCh) and (aCh <= $0D4D)) or
            (aCh = $0D57) or (aCh = $0E31) or
            (($0E34 <= aCh) and (aCh <= $0E3A)) or
            (($0E47 <= aCh) and (aCh <= $0E4E)) or (aCh = $0EB1) or
            (($0EB4 <= aCh) and (aCh <= $0EB9)) or
            (($0EBB <= aCh) and (aCh <= $0EBC)) or
            (($0EC8 <= aCh) and (aCh <= $0ECD)) or
            (($0F18 <= aCh) and (aCh <= $0F19)) or
            (aCh = $0F35) or (aCh = $0F37) or (aCh = $0F39) or
            (aCh = $0F3E) or (aCh = $0F3F) or
            (($0F71 <= aCh) and (aCh <= $0F84)) or
            (($0F86 <= aCh) and (aCh <= $0F8B)) or
            (($0F90 <= aCh) and (aCh <= $0F95)) or (aCh = $0F97) or
            (($0F99 <= aCh) and (aCh <= $0FAD)) or
            (($0FB1 <= aCh) and (aCh <= $0FB7)) or (aCh = $0FB9) or
            (($20D0 <= aCh) and (aCh <= $20DC)) or (aCh = $20E1) or
            (($302A <= aCh) and (aCh <= $302F)) or
            (aCh = $3099) or (aCh = $309A);
end;
{--------}
function XPIsDigit(aCh : TXpUcs4Char) : boolean;
begin
  {[88] Digit ::=
             [#x0030-#x0039] | [#x0660-#x0669] | [#x06F0-#x06F9]
             | [#x0966-#x096F] | [#x09E6-#x09EF] | [#x0A66-#x0A6F]
             | [#x0AE6-#x0AEF] | [#x0B66-#x0B6F] | [#x0BE7-#x0BEF]
             | [#x0C66-#x0C6F] | [#x0CE6-#x0CEF] | [#x0D66-#x0D6F]
             | [#x0E50-#x0E59] | [#x0ED0-#x0ED9] | [#x0F20-#x0F29] }
  Result :=  (($30 <= aCh) and (aCh <= $39)) or
             (($660 <= aCh) and (aCh <= $669)) or
             (($6F0 <= aCh) and (aCh <= $6F9)) or
             (($966 <= aCh) and (aCh <= $96F)) or
             (($9E6 <= aCh) and (aCh <= $9EF)) or
             (($A66 <= aCh) and (aCh <= $A6F)) or
             (($AE6 <= aCh) and (aCh <= $AEF)) or
             (($B66 <= aCh) and (aCh <= $B6F)) or
             (($BE7 <= aCh) and (aCh <= $BEF)) or
             (($C66 <= aCh) and (aCh <= $C6F)) or
             (($CE6 <= aCh) and (aCh <= $CEF)) or
             (($D66 <= aCh) and (aCh <= $D6F)) or
             (($E50 <= aCh) and (aCh <= $E59)) or
             (($ED0 <= aCh) and (aCh <= $ED9)) or
             (($F20 <= aCh) and (aCh <= $F29));
end;
{--------}
function XPIsExtender(aCh : TXpUcs4Char) : boolean;
begin
  {[89] Extender ::=
                #x00B7 | #x02D0 | #x02D1 | #x0387 | #x0640 | #x0E46
                | #x0EC6 | #x3005 | [#x3031-#x3035] | [#x309D-#x309E]
                | [#x30FC-#x30FE] }
  Result := (aCh = $00B7) or (aCh = $02D0) or
            (aCh = $02D1) or (aCh = $0387) or
            (aCh = $0640) or (aCh = $0E46) or
            (aCh = $0EC6) or (aCh = $3005) or
            (($3031 <= aCh) and (aCh <= $3035)) or
            (($309D <= aCh) and (aCh <= $309E)) or
            (($30FC <= aCh) and (aCh <= $30FE));
end;
{--------}
function XPIsIdeographic(aCh : TXpUcs4Char) : boolean;
begin
  {[86] Ideographic ::= [#x4E00-#x9FA5] | #x3007 | [#x3021-#x3029] }
  Result := (($4E00 <= aCh) and (aCh <= $9FA5)) or
            (aCh = $3007) or
            (($3021 <= aCh) and (aCh <= $3029));
end;
{--------}
function XPIsLetter(aCh : TXpUcs4Char) : boolean;
begin
  {[84] Letter ::= BaseChar | Ideographic }
  Result := XPIsBaseChar(aCh) or XPIsIdeographic(aCh);
end;
{--------}
function XPIsNameChar(aCh : TXpUcs4Char) : boolean;
begin
  {[4] NameChar ::= Letter | Digit | '.' | '-' | '_' | ':'
                | CombiningChar | Extender }
  Result := XPIsLetter(aCh) or XPIsDigit(aCh) or
            (aCh = ord('.')) or (aCh = ord('-')) or
            (aCh = ord('_')) or (aCh = ord(':')) or
            XPIsCombiningChar(aCh) or XPIsExtender(aCh);
end;
{--------}
function XPIsNameCharFirst(aCh : TXpUcs4Char) : boolean;
begin
  {[5] Name ::= (Letter | '_' | ':') (NameChar)* }
  Result := XPIsLetter(aCh) or (aCh = ord('_')) or (aCh = ord(':'));
end;
{--------}
function XPIsPubidChar(aCh : TXpUcs4Char) : boolean;
begin
  {[13] PubidChar ::= #x20 | #xD | #xA | [a-zA-Z0-9]
                      | [-'()+,./:=?;!*#@$_%] }
  Result := (aCh = $20) or (aCh = 13) or (aCh = 10) or
            ((ord('a') <= aCh) and (aCh <= ord('z'))) or
            ((ord('A') <= aCh) and (aCh <= ord('Z'))) or
            ((ord('0') <= aCh) and (aCh <= ord('9'))) or
            (aCh = ord('-')) or (aCh = ord('''')) or
            (aCh = ord('(')) or (aCh = ord(')')) or
            (aCh = ord('+')) or (aCh = ord(',')) or
            (aCh = ord('.')) or (aCh = ord('/')) or
            (aCh = ord(':')) or (aCh = ord('=')) or
            (aCh = ord('?')) or (aCh = ord(';')) or
            (aCh = ord('!')) or (aCh = ord('*')) or
            (aCh = ord('#')) or (aCh = ord('@')) or
            (aCh = ord('$')) or (aCh = ord('_')) or
            (aCh = ord('%'));
end;
{--------}
function XPIsSpace(aCh : TXpUcs4Char) : Boolean;
begin
  {[3] S ::= (#x20 | #x9 | #xD | #xA)+ }
  Result := (aCh <= $20) and (AnsiChar(aCh) in [' ', #9, #13, #10]);
end;
{--------}
function XPIsSurrogate(aCh : DOMChar) : Boolean;                       {!!.52}
begin
  Result := (#$D800 <= aCh) and (aCh <= #$DFFF);
end;
{Begin !!.52 - Moved from XpParser }
{--------}
function XpIsValidXPathNumber(const aNumber : DOMString) : Boolean;
var
  i : Integer;
begin
  for i := 1 to Length(aNumber) do
    if (not (Char(aNumber[i]) in ['0'..'9', '.'])) then
      if ((i = 1) and
          (aNumber[1] = '-')) then
        Continue
      else begin
        Result := False;
        Exit;
      end;
  Result := True;
end;
{--------}
function XpValidName(const Name : DOMString) : Boolean;                {!!.57 - Added}
var
  i : Integer;
begin
  Result := True;
  for i := 1 to Length(Name) do
    if (not XpValidNameChar(i = 1, Name[i])) then begin
      Result := False;
      Exit;
    end;
end;
{--------}
function XpValidNameChar(const First : Boolean;
                         const Char  : DOMChar) : Boolean;
var
  BothUsed : Boolean;
  UCS4 : TXPUCS4Char;
begin
  { Naming rules -  from sect 2.3 of spec}
  { Names cannot be an empty string }
  { Names must begin with 1 letter or one of the following
    punctuation characters ['_',':']}
  { Names should not begin with 'XML' or any case derivitive}
  { Except for the first character, names can contain
    [letters, digits,'.', '-', '_', ':'}

  XPUtf16ToUcs4(DOMChar(PByteArray(@Char)^[0]),
                DOMChar(PByteArray(@Char)^[1]),
                UCS4,
                BothUsed);
  if not First then
    Result := XPIsNameChar(UCS4)
  else
    Result := XPIsNameCharFirst(UCS4);
end;
{End !!.52}
{====================================================================}

{==TXpMemoryStream===================================================}
procedure TXpMemoryStream.SetPointer(Ptr : Pointer; Size : Integer);
begin
  Assert(not Assigned(Memory));
  inherited;
end;
{====================================================================}

{===TXpFileStream====================================================}
constructor TXpFileStream.CreateEx(Mode : Word; const FileName : string);
begin
  inherited Create(FileName, Mode);
  FFileName := FileName;
end;
{====================================================================}

{===TXpComponent=====================================================}
function TXpComponent.xcGetVersion : string;
begin
  Result := Format('%5.4f', [XpVersionNumber / 10000.0]);
end;
{--------}
procedure TXpComponent.xcSetVersion(const Value : string);
begin
  {do nothing}
end;
{====================================================================}
end.
