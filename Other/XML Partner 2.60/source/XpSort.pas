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
{* XMLPartner Pro: XpSort.PAS                            *}
{*********************************************************}
{* XMLPartner Pro: Sort classes                          *}
{*********************************************************}

{$I XpDefine.inc}

unit XpSort;

interface

uses
  XpBase,
  XpDOM,
  XpHash;

type

  { forward declarations }
  TXpComparator = class;

{===Key-related classes==============================================}

  TXpSortDataType = (xpsdtText, xpsdtNumber);
    { Data types supported by xsl:sort. }

  TXpSortKeyDefinition = class
    { Contains the definition of a key as specified via an xsl:sort
      element. This class is a place-holder for values. The values are
      calculated in the implementation of xsl:sort.

      A key definition is used to determine which compare class to instantiate
      & how the keys are compared. }
  protected
    FAscending : Boolean;
    FComparator : TXpComparator;
    FDataType: TXpSortDataType;
    FLang : DOMString;
    FSelect : DOMString;
    FUpperFirst : Boolean;
  public
    procedure SetComparator;
      { Obtains a comparator for the key definition. Call this method only
        after the DataType and Lang properties have been set. }

    property Ascending : Boolean
      read FAscending
      write FAscending;

    property Comparator : TXpComparator
      read FComparator;

    property DataType : TXpSortDataType
      read FDataType
      write FDataType;

    property Lang : DOMString
      read FLang
      write FLang;

    property Select : DOMString
      read FSelect
      write FSelect;

    property UpperFirst : Boolean
      read FUpperFirst
      write FUpperFirst;

  end;

  TXpSortKeyDefinitions = class
  protected
    FList : TXpPointerList;
      { List of key definitions. }

    function GetDefinition(const wInx : Integer) : TXpSortKeyDefinition;

  public
    constructor Create;
    destructor Destroy; override;

    procedure Add(oKeyDef : TXpSortKeyDefinition);

    function Count : Longint;

    property Definitions[const wInx : Integer] : TXpSortKeyDefinition
      read GetDefinition;
  end;

  TXpSortKey = class
    { Contains one or more keys for a specific node. }
  protected
    FList : TXpPointerList;
      { Pointer to the list of key values. When the element is first created,
        only the first key is specified. The sort algorithm will generate
        the additional keys as necessary. }
    FNode : TXpNode;

    function GetKeyValue(const wInx : Integer) : DOMString;
  public
    constructor Create(oNode : TXpNode; const sKeyValue : DOMString); virtual;
    destructor Destroy; override;

    procedure Add(const sValue : DOMString);
    function Count : Longint;

    property KeyValues[const wInx : Integer] : DOMString
      read GetKeyValue;

    property Node : TXpNode
      read FNode;
  end;

  TXpSortKeys = class
    { This class represents a list of generated keys. }
  protected
    FList : TXpPointerList;
      { List of keys. }

    function GetKey(const wInx : Integer) : TXpSortKey;
    procedure SetKey(const wInx : Integer; oKey : TXpSortKey);

  public
    constructor Create;
    destructor Destroy; override;

    procedure Add(oNode : TXpNode; const sKeyValue : DOMString);

    function Count : Longint;

    property Keys[const wInx : Integer] : TXpSortKey
      read GetKey write SetKey;
  end;
{====================================================================}

{===Comparison classes===============================================}
  TXpComparator = class
    { Base class for key comparisons. XMLPartner Professional is delivered
      with a default class that supports binary order comparisons. Additional
      comparators may be created by TurboPower or end-users.

      Comparators must be created via the Register class method in the
        Initialization section of a unit (see this unit for an example).
      Comparators must be freed by calling the Unregister class method
        in the Finalization section of a unit (see this unit for an example).

      The sort engine will find comparators as necessary using the
        FindComparator class method.
    }
  protected
    FAscending : Boolean;
    FDataType : TXpSortDataType;
    FLang : DOMString;
    FUpperFirst : Boolean;
  public
    class procedure Register(const oDataType : TXpSortDataType;
                             const sLang : DOMString); virtual;
      { Creates an instance of this object and adds it to the list of
        registered comparators. }

    class procedure Unregister;
      { Removes all instances of this class type from the list of
        registered comparators. }

    class function FindComparator(const oDataType : TXpSortDataType;
                                  const sLang : DOMString) : TXpComparator;
      { Searchs the list of registered comparators for a comparator that
        handles the specified data type and language. }

    function Compare(oKeyA, oKeyB : TXpSortKey;
                     wKeyInx : Integer;
               const bAscending, bUpperFirst : Boolean) : integer; virtual;
      { Compares two keys. The comparison must take into account the
        bAscending and bUpperFirst parameters when performing the comparison.
        Since each key may be a composite key, parameter wKeyInx indicates
        which key value is being compared.
        Values returned:
          -1 if key A is less than key B
           0 if key A is equivalent to key B
           1 if key A is greater than key B }

    procedure Initialize; virtual;
      { This method is called after the object is instantiated via the
        Register class method. The default implementation of this method
        stores the specified parameters for use by the Compare method. }

    property DataType : TXpSortDataType
      read FDataType;

    property Lang : DOMString
      read FLang;

  end;

  TXpNumberComparator = class(TXpComparator)
    { Sorts numbers as though they were doubles. }
  public
    function Compare(oKeyA, oKeyB : TXpSortKey;
                     wKeyInx : Integer;
               const bAscending, bUpperFirst : Boolean) : integer; override;
  end;

  TXpTextComparator = class(TXpComparator)
    { Sorts characters in ASCII order. Characters in the range 192-255
      (i.e., Codepage 1252 used by Windows) are converted to the corresponding
      characters without the accents. }
  protected
    function AccentedLowercase(const sStr : DOMString;
                                 var bModified : Boolean) : DOMString;
      { Converts characters 65..90 to lowercase equivalent. If any character
        was converted then param bModified is set to True. }

    function ComparePrim(const sStrA, sStrB : DOMString;
                         const bIgnorePunct,
                               bConsiderUpper,
                               bUpperFirst : Boolean) : Integer;

    function UnaccentedLowercase(const sStr : DOMString;
                                   var bModified : Boolean) : DOMString;
      { Converts CP1252 192..255 to unaccented equivalent. Converts characters
        65..90 to lowecase equivalent. If any character was converted then
        param bModified is set to True. }
  public
    function Compare(oKeyA, oKeyB : TXpSortKey;
                     wKeyInx : Integer;
               const bAscending, bUpperFirst : Boolean) : integer; override;
  end;

{====================================================================}

{===Sort classes=====================================================}

  TXpNodeSorter = class
  protected
    procedure SortPrim(oKeys : TXpSortKeys;
                       oKeyDefinitions : TXpSortKeyDefinitions;
                       wKeyInx, wRangeStart, wRangeEnd : Integer);
      { Implements quick sort algorithm. If there are any duplicate key
        values after the sort has completed and there is at least one
        unused key definition, this method calls itself requesting a sort
        on the range of duplicate key values using the next key definition. }

    procedure SwapKeys(oKeys : TXpSortKeys;
                 const wInx1, wInx2 : Longint);
      { Swap two nodes within the list. }

  public
    function Sort(oNodeList : TXpNodeList;
                  oKeyDefinitions : TXpSortKeyDefinitions) : TXpNodeList; virtual;
      { Sorts the specified nodelist using the given key definitions.
        Returns a new nodelist containing the nodes in the appropriate order.
        The caller is responsible for freeing parameter oNodeList. }
  end;
{====================================================================}



implementation

uses
  SysUtils;

const
  xpsDefault = 'default';

var
  _Comparators : TXpPointerList;

{===TXpSortKeyDefinition=============================================}
procedure TXpSortKeyDefinition.SetComparator;
begin
  FComparator := TXpComparator.FindComparator(FDataType, FLang);
  if FComparator = nil then
    FComparator := TXpComparator.FindComparator(FDataType, xpsDefault);
end;
{====================================================================}

{===TXpSortKeyDefinitions============================================}
constructor TXpSortKeyDefinitions.Create;
begin
  inherited Create;
  FList := TXpPointerList.Create;
end;
{--------}
destructor TXpSortKeyDefinitions.Destroy;
var
  wInx : Integer;
begin
  for wInx := Pred(FList.Count) downto 0 do
    TXpSortKeyDefinition(FList.Pointers[wInx]).Free;
  FList.Free;
  inherited;
end;
{--------}
procedure TXpSortKeyDefinitions.Add(oKeyDef : TXpSortKeyDefinition);
begin
  FList.Append(oKeyDef);
end;
{--------}
function TXpSortKeyDefinitions.GetDefinition(const wInx : Integer) : TXpSortKeyDefinition;
begin
  Result := TXpSortKeyDefinition(FList.Pointers[wInx]);
end;
{--------}
function TXpSortKeyDefinitions.Count : Integer;
begin
  Result := FList.Count;
end;
{====================================================================}

{===TXpSortKey=======================================================}

{ The following class is used to hold DOMStrings within TXpSortKey. }
type
  TXpSortKeyValue = class
  public
    KeyValue : DOMString;
  end;

constructor TXpSortKey.Create(oNode : TXpNode; const sKeyValue : DOMString);
var
  oKeyValue : TXpSortKeyValue;
begin
  inherited Create;
  FList := TXpPointerList.Create;
  oKeyValue := TXpSortKeyValue.Create;
  oKeyValue.KeyValue := sKeyValue;
  FList.Append(Pointer(Longint(oKeyValue)));
  FNode := oNode;
  FNode.AddRef;
end;
{--------}
destructor TXpSortKey.Destroy;
var
  wInx : Integer;
begin
  for wInx := Pred(FList.Count) downto 0 do
    TXpSortKeyValue(FList.Pointers[wInx]).Free;
  FList.Free;
  FNode.Release;
  inherited;
end;
{--------}
procedure TXpSortKey.Add(const sValue : DOMString);
var
  oKeyValue : TXpSortKeyValue;
begin
  oKeyValue := TXpSortKeyValue.Create;
  oKeyValue.KeyValue := sValue;
  FList.Append(Pointer(Longint(oKeyValue)));
end;
{--------}
function TXpSortKey.Count : Longint;
begin
  Result := FList.Count;
end;
{--------}
function TXpSortKey.GetKeyValue(const wInx : Integer) : DOMString;
var
  oKeyValue : TXpSortKeyValue;
begin
  Result := '';
  oKeyValue := FList.Pointers[wInx];
  if oKeyValue <> nil then
    Result := oKeyValue.KeyValue;
end;
{====================================================================}

{===TXpSortKeys======================================================}
constructor TXpSortKeys.Create;
begin
  inherited Create;
  FList := TXpPointerList.Create;
end;
{--------}
destructor TXpSortKeys.Destroy;
var
  wInx : Integer;
begin
  for wInx := Pred(FList.Count) downto 0 do
    TXpSortKey(FList.Pointers[wInx]).Free;
  FList.Free;
  inherited;
end;
{--------}
procedure TXpSortKeys.Add(oNode : TXpNode; const sKeyValue : DOMString);
var
  oKey : TXpSortKey;
begin
  oKey := TXpSortKey.Create(oNode, sKeyValue);
  FList.Append(oKey);
end;
{--------}
function TXpSortKeys.Count : Integer;
begin
  Result := FList.Count;
end;
{--------}
function TXpSortKeys.GetKey(const wInx : Integer) : TXpSortKey;
begin
  Result := TXpSortKey(FList.Pointers[wInx]);
end;
{--------}
procedure TXpSortKeys.SetKey(const wInx : Integer; oKey : TXpSortKey);
begin
  FList.Pointers[wInx] := oKey;
end;
{====================================================================}

{===TXpComparator====================================================}
class procedure TXpComparator.Register(const oDataType : TXpSortDataType;
                                       const sLang : DOMString);
var
  oComparator : TXpComparator;
begin
  oComparator := Create;
  try
    oComparator.Initialize;
    _Comparators.Append(oComparator);
  except
    oComparator.Free;
  end;
  oComparator.FDataType := oDataType;
  oComparator.FLang := sLang;
end;
{--------}
class procedure TXpComparator.Unregister;
var
  wInx : Integer;
begin
  if _Comparators = nil then
    Exit;
  { Free every instance of this class. }
  for wInx := Pred(_Comparators.Count) downto 0 do
    with TXpComparator(_Comparators.Pointers[wInx]) do
{Begin !!.55}
      if (ClassType = Self) then begin
        Free;
        _Comparators.RemoveAt(wInx);
      end;
{End !!.55}
end;
{--------}
class function TXpComparator.FindComparator(const oDataType : TXpSortDataType;
                                            const sLang : DOMString) : TXpComparator;
var
  wInx : Integer;
begin
  Result := nil;
  for wInx := 0 to Pred(_Comparators.Count) do
    with TXpComparator(_Comparators.Pointers[wInx]) do
      if (FDataType = oDataType) and
         (FLang = sLang) then begin
        Result := _Comparators.Pointers[wInx];
        Break;
      end;
end;
{--------}
function TXpComparator.Compare(oKeyA, oKeyB : TXpSortKey;
                               wKeyInx : Integer;
                         const bAscending, bUpperFirst : Boolean) : integer;
begin
  { Descendant classes must override this method in order to implement
    comparison logic. }
  Result := 0;
end;
{--------}
procedure TXpComparator.Initialize;
begin
  { Descendant classes may override this method for custom initialization. }
end;
{====================================================================}

{===TXpNumberComparator==============================================}
function TXpNumberComparator.Compare(oKeyA, oKeyB : TXpSortKey;
                                     wKeyInx : Integer;
                               const bAscending, bUpperFirst : Boolean) : integer;
var
  bANaN,
  bBNaN : Boolean;
  wKeyA,
  wKeyB : Double;
begin
  bANaN := False;
  bBNaN := False;
  wKeyA := 0;
  wKeyB := 0;

  try
    wKeyA := StrToFloat(oKeyA.KeyValues[wKeyInx]);
  except
    bANaN := True;
  end;

  try
    wKeyB := StrToFloat(oKeyB.KeyValues[wKeyInx]);
  except
    bBNaN := True;
  end;

  if bANaN then begin
    if bBNaN then
      Result := 0
    else
      Result := -1;
  end
  else if bBNaN then
    Result := 1
  else if wKeyA = wKeyB then
    Result := 0
  else if wKeyA < wKeyB then
    Result := -1
  else
    Result := 1;

  if (not bAscending) and (Result <> 0) then
    Result := -Result;
end;
{====================================================================}

{===TXpTextComparator================================================}
function TXpTextComparator.AccentedLowercase(const sStr : DOMString;
                                               var bModified : Boolean) : DOMString;
var
  wChar,
  wInx : Integer;
begin
  Result := sStr;
  for wInx := 1 to Length(Result) do begin
    wChar := Ord(Result[wInx]);
    if wChar in [65..90] then begin
      Result[wInx] := WideChar(wChar + 32);
      bModified := True;
    end;
  end;
end;
{--------}
function TXpTextComparator.Compare(oKeyA, oKeyB : TXpSortKey;
                                   wKeyInx : Integer;
                             const bAscending, bUpperFirst : Boolean) : integer;
var
  sKeyA,
  sKeyB : DOMString;
  bModA,
  bModB : Boolean;
    { Set to True if conversion routine actually had to modify the input
      string. }
begin
  { This algorithm performs a character-by-character comparison using the
    guidelines presented in Section 15.7 of The Unicode Standard Version 3.0,
    pg. 137.

    There are 3 stages to the comparison:
    1 - Convert accented characters (CP1252, range 192 - 255) to their
        unaccented equivalents and compare. See the end of this routine
        for a map of characters 192 - 255.
    2 - If the strings compare equal then re-compare using accents.
    3 - If the strings still compare equal then re-compare using case.

    The acute accent and hyphen characters are treated as having no primary
    difference.
  }

  { Stage 1: Convert to unaccented, lowercase equivalent. }
  bModA := False;
  bModB := False;
  sKeyA := UnaccentedLowercase(oKeyA.KeyValues[wKeyInx], bModA);
  sKeyB := UnaccentedLowercase(oKeyB.KeyValues[wKeyInx], bModB);
  Result := ComparePrim(sKeyA, sKeyB, True, False, False);

  { Stage 2: Convert to lowercase equivalent & compare, if stage 1 compared
    equivalent and at least one character was modified in the stage 1
    conversion. }
  if (Result = 0) and (bModA or bModB) then begin
    bModA := False;
    bModB := False;
    sKeyA := AccentedLowercase(oKeyA.KeyValues[wKeyInx], bModA);
    sKeyB := AccentedLowercase(oKeyB.KeyValues[wKeyInx], bModB);
    Result := ComparePrim(sKeyA, sKeyB, False, False, False);
  end;

  { Stage 3: Take character case into account, if stage 2 compared equivalent
    & at least one character was modified in the stage 2 conversion. }
  if (Result = 0) and (bModA or bModB) then
    Result := ComparePrim(oKeyA.KeyValues[wKeyInx],
                          oKeyB.KeyValues[wKeyInx], False, True, bUpperFirst);

  if (Result <> 0) and (not bAscending) then
    Result := -Result;

{ Map of CP1252 characters 192 - 255 to their Unicode equivalent. First column
  is the CP1252 character code in hex. Second column is the Unicode equivalent
  in hex. Third column is the Unicode name following a # character.
  This map obtained from
  ftp://ftp.informatik.uni-erlangen.de/pub/doc/ISO/charsets/ucs-map-cp1252

  ====================================================
  0xc0	0x00c0	#LATIN CAPITAL LETTER A WITH GRAVE
  0xc1	0x00c1	#LATIN CAPITAL LETTER A WITH ACUTE
  0xc2	0x00c2	#LATIN CAPITAL LETTER A WITH CIRCUMFLEX
  0xc3	0x00c3	#LATIN CAPITAL LETTER A WITH TILDE
  0xc4	0x00c4	#LATIN CAPITAL LETTER A WITH DIAERESIS
  0xc5	0x00c5	#LATIN CAPITAL LETTER A WITH RING ABOVE
  0xc6	0x00c6	#LATIN CAPITAL LIGATURE AE
  0xc7	0x00c7	#LATIN CAPITAL LETTER C WITH CEDILLA
  0xc8	0x00c8	#LATIN CAPITAL LETTER E WITH GRAVE
  0xc9	0x00c9	#LATIN CAPITAL LETTER E WITH ACUTE
  0xca	0x00ca	#LATIN CAPITAL LETTER E WITH CIRCUMFLEX
  0xcb	0x00cb	#LATIN CAPITAL LETTER E WITH DIAERESIS
  0xcc	0x00cc	#LATIN CAPITAL LETTER I WITH GRAVE
  0xcd	0x00cd	#LATIN CAPITAL LETTER I WITH ACUTE
  0xce	0x00ce	#LATIN CAPITAL LETTER I WITH CIRCUMFLEX
  0xcf	0x00cf	#LATIN CAPITAL LETTER I WITH DIAERESIS
  0xd0	0x00d0	#LATIN CAPITAL LETTER ETH
  0xd1	0x00d1	#LATIN CAPITAL LETTER N WITH TILDE
  0xd2	0x00d2	#LATIN CAPITAL LETTER O WITH GRAVE
  0xd3	0x00d3	#LATIN CAPITAL LETTER O WITH ACUTE
  0xd4	0x00d4	#LATIN CAPITAL LETTER O WITH CIRCUMFLEX
  0xd5	0x00d5	#LATIN CAPITAL LETTER O WITH TILDE
  0xd6	0x00d6	#LATIN CAPITAL LETTER O WITH DIAERESIS
  0xd7	0x00d7	#MULTIPLICATION SIGN
  0xd8	0x00d8	#LATIN CAPITAL LETTER O WITH STROKE
  0xd9	0x00d9	#LATIN CAPITAL LETTER U WITH GRAVE
  0xda	0x00da	#LATIN CAPITAL LETTER U WITH ACUTE
  0xdb	0x00db	#LATIN CAPITAL LETTER U WITH CIRCUMFLEX
  0xdc	0x00dc	#LATIN CAPITAL LETTER U WITH DIAERESIS
  0xdd	0x00dd	#LATIN CAPITAL LETTER Y WITH ACUTE
  0xde	0x00de	#LATIN CAPITAL LETTER THORN
  0xdf	0x00df	#LATIN SMALL LETTER SHARP S
  0xe0	0x00e0	#LATIN SMALL LETTER A WITH GRAVE
  0xe1	0x00e1	#LATIN SMALL LETTER A WITH ACUTE
  0xe2	0x00e2	#LATIN SMALL LETTER A WITH CIRCUMFLEX
  0xe3	0x00e3	#LATIN SMALL LETTER A WITH TILDE
  0xe4	0x00e4	#LATIN SMALL LETTER A WITH DIAERESIS
  0xe5	0x00e5	#LATIN SMALL LETTER A WITH RING ABOVE
  0xe6	0x00e6	#LATIN SMALL LIGATURE AE
  0xe7	0x00e7	#LATIN SMALL LETTER C WITH CEDILLA
  0xe8	0x00e8	#LATIN SMALL LETTER E WITH GRAVE
  0xe9	0x00e9	#LATIN SMALL LETTER E WITH ACUTE
  0xea	0x00ea	#LATIN SMALL LETTER E WITH CIRCUMFLEX
  0xeb	0x00eb	#LATIN SMALL LETTER E WITH DIAERESIS
  0xec	0x00ec	#LATIN SMALL LETTER I WITH GRAVE
  0xed	0x00ed	#LATIN SMALL LETTER I WITH ACUTE
  0xee	0x00ee	#LATIN SMALL LETTER I WITH CIRCUMFLEX
  0xef	0x00ef	#LATIN SMALL LETTER I WITH DIAERESIS
  0xf0	0x00f0	#LATIN SMALL LETTER ETH
  0xf1	0x00f1	#LATIN SMALL LETTER N WITH TILDE
  0xf2	0x00f2	#LATIN SMALL LETTER O WITH GRAVE
  0xf3	0x00f3	#LATIN SMALL LETTER O WITH ACUTE
  0xf4	0x00f4	#LATIN SMALL LETTER O WITH CIRCUMFLEX
  0xf5	0x00f5	#LATIN SMALL LETTER O WITH TILDE
  0xf6	0x00f6	#LATIN SMALL LETTER O WITH DIAERESIS
  0xf7	0x00f7	#DIVISION SIGN
  0xf8	0x00f8	#LATIN SMALL LETTER O WITH STROKE
  0xf9	0x00f9	#LATIN SMALL LETTER U WITH GRAVE
  0xfa	0x00fa	#LATIN SMALL LETTER U WITH ACUTE
  0xfb	0x00fb	#LATIN SMALL LETTER U WITH CIRCUMFLEX
  0xfc	0x00fc	#LATIN SMALL LETTER U WITH DIAERESIS
  0xfd	0x00fd	#LATIN SMALL LETTER Y WITH ACUTE
  0xfe	0x00fe	#LATIN SMALL LETTER THORN
  0xff	0x00ff	#LATIN SMALL LETTER Y WITH DIAERESIS
  ====================================================
}
end;
{--------}
function TXpTextComparator.ComparePrim(const sStrA, sStrB : DOMString;
                                       const bIgnorePunct,
                                             bConsiderUpper,
                                             bUpperFirst : Boolean) : Integer;
var
  wCharA,
  wInxA,
  wInxB,
  wLenA,
  wLenB : Integer;
begin
  Result := 0;
  wInxA := 0;
  wInxB := 0;
  wLenA := Length(sStrA);
  wLenB := Length(sStrB);

  while True do begin
    { Have we reached the end of both strings simultaneously? }
    if ((wInxA = wLenA) and (wInxB = wLenB)) then
      { Yes. They must be equal. }
      Break;

    { Is string A shorter than string B? }
    if (wInxA = wLenA) then begin
      { Yes. String A is less than string B. }
      Result := -1;
      Break;
    end
    { Is string B shorter than string A? }
    else if (wInxB = wLenB) then begin
      { Yes. String A is greater than string B. }
      Result := 1;
      Break;
    end;

    inc(wInxA);
    inc(wInxB);

    { Skip punctuation characters? }
    if bIgnorePunct then begin
      { Yes. Move to next non-punctuation character. }
      while (wInxA <= wLenA) and (Ord(sStrA[wInxA]) in [45, 180]) do
        inc(wInxA);
      while (wInxB <= wLenB) and (Ord(sStrB[wInxB]) in [45, 180]) do
        inc(wInxB);
    end;

    { Considering uppercase? }
    if bConsiderUpper then begin
      { Yes. We reach this point at the 3rd stage. The characters are
        equal when case-insensitive. So the only time we reach this spot
        is when we are comparing something like ('a', 'A') or ('A', 'a'). }
      wCharA := Ord(sStrA[wInxA]);
      if sStrA[wInxA] <> sStrB[wInxB] then begin
        if bUpperFirst then begin
          if wCharA in [65..90] then
            Result := -1
          else
            Result := 1;
        end
        else begin
          if wCharA in [97..122] then
            Result := -1
          else
            Result := 1;
        end;
        Break;
      end;
    end
    else begin
      { No. }
      if sStrA[wInxA] < sStrB[wInxB] then begin
        Result := -1;
        Break;
      end
      else if sStrA[wInxA] > sStrB[wInxB] then begin
        Result := 1;
        Break;
      end;
    end;
  end;  { while }

end;
{--------}
function TXpTextComparator.UnaccentedLowercase(const sStr : DOMString;
                                                 var bModified : Boolean) : DOMString;
const
  cUnaccented : DOMString =
    'aaaaaaaceeeeiiii[nooooo*ouuuuy\saaaaaaaceeeeiiii]nooooo/ouuuuy^y';
    { This string is used to map character codes 192 - 255 to their lowercase,
      unaccented equivalents. Character 1 in this string corresponds to the
      unaccented equivalent of character 192. The eth and thorn characters are
      converted to characters such that they are sorted in binary order. }
var
  wChar,
  wInx : Integer;
begin
  Result := sStr;
  for wInx := 1 to Length(Result) do begin
    wChar := Ord(Result[wInx]);
    if wChar in [192..255] then begin
      Result[wInx] := cUnaccented[wChar - 191];
      bModified := True;
    end
    else if wChar in [65..90] then begin
      Result[wInx] := WideChar(wChar + 32);
      bModified := True;
    end;
  end;
end;
{====================================================================}

{===TXpNodeSorter====================================================}
function TXpNodeSorter.Sort(oNodeList : TXpNodeList;
                            oKeyDefinitions : TXpSortKeyDefinitions) : TXpNodeList;
var
  oKeys : TXpSortKeys;
  oNode : TXpNode;
  sKeyValue,
  sSelect : DOMString;
  wInx : Integer;
begin
{Begin !!.55}
  if (oNodeList = nil) or (oNodeList.Length = 0) then begin
    Result := TXpNodeList.Create;
    Exit;
  end;
{End !!.55}

  oKeys := TXpSortKeys.Create;
  try
    { Build the first key for each node. }
    sSelect := oKeyDefinitions.Definitions[0].Select;
    for wInx := 0 to Pred(oNodeList.Length) do begin
      oNode := oNodeList.Item(wInx);
      sKeyValue := oNode.SelectString(sSelect);
      oKeys.Add(oNode, sKeyValue);
    end;
    { Quicksort on the first key definition. SortPrim will perform additional
      sorts using subsequent keys as necessary. }
    SortPrim(oKeys, oKeyDefinitions, 0, 0, Pred(oKeys.Count));

    { Build the result nodelist. }
    Result := TXpNodeList.Create;
    for wInx := 0 to Pred(oKeys.Count) do
      Result.Add(oKeys.Keys[wInx].Node);
  finally
    oKeys.Free;
  end;
end;
{--------}
procedure TXpNodeSorter.SortPrim(oKeys : TXpSortKeys;
                                 oKeyDefinitions : TXpSortKeyDefinitions;
                                 wKeyInx, wRangeStart, wRangeEnd : Integer);
const
  MedianThreshold = 16;
  StackSize = 32;
type
  Stack = array[0..StackSize - 1] of Longint;
var
  bDupFound : Boolean;
  L : Longint;            { The left edge, base zero. }
  R : Longint;            { The right edge, base zero. }
  Pl : Longint;           { Left edge within current partition, base zero. }
  Pr : Longint;           { Right edge within current partition, base zero. }
  Pm : Longint;           { Mid-point of current partition. }
  PLen : Longint;         { The size of the current partition. }
  StackP : integer;       { Stack pointer. }
  LStack : Stack;         { Pending partitions, left edge. }
  RStack : Stack;         { Pending partitions, right edge. }
  oKeyDef : TXpSortKeyDefinition;
  oPivotKey : TXpSortKey;
  oComparator : TXpComparator;
  sSelect : DOMString;
begin
  { NOTE: This is a recursive method. After the specified range of nodes is
    sorted, it will look for duplicate key values and call itself on the
    range of duplicates. }

  oKeyDef := oKeyDefinitions.Definitions[wKeyInx];
  oComparator := oKeyDef.Comparator;

  { Initialize the stack. }
  StackP := 0;
  LStack[0] := wRangeStart;
  RStack[0] := wRangeEnd;

  { Repeatedly take top partition from the stack. }
  repeat
    { Pop the stack. }
    L := LStack[StackP];
    R := RStack[StackP];
    Dec(StackP);

    { Sort the current partition. }
    repeat
      Pl := L;
      Pr := R;
      PLen := Pr - Pl + 1;

      { Calculate the pivot element. }
      Pm := Pl + (PLen shr 1);
      if PLen >= MedianThreshold then begin
        { Sort elements P1, Pm, & Pr. }
        if oComparator.Compare(oKeys.Keys[Pm], oKeys.Keys[Pl], wKeyInx,
                               oKeyDef.Ascending, oKeyDef.UpperFirst) < 0 then
          SwapKeys(oKeys, Pm, Pl);
        if oComparator.Compare(oKeys.Keys[Pr], oKeys.Keys[Pl], wKeyInx,
                               oKeyDef.Ascending, oKeyDef.UpperFirst) < 0 then
          SwapKeys(oKeys, Pr, Pl);
        if oComparator.Compare(oKeys.Keys[Pr], oKeys.Keys[Pm], wKeyInx,
                               oKeyDef.Ascending, oKeyDef.UpperFirst) < 0 then
          SwapKeys(oKeys, Pr, Pm);
        { Exchange Pm with Pr - 1 but use Pm's value as the pivot. }
        SwapKeys(oKeys, Pm, Pr - 1);
        Pm := Pr - 1;

        { Reduce range of swapping now that Pl and Pr are in the right
          spots. }
        inc(Pl);
        dec(Pr, 2);
      end;

      { Save the pivot element. }
      oPivotKey := oKeys.Keys[Pm];

      { Swap items in sort order around the pivot. }
      repeat
        while oComparator.Compare(oKeys.Keys[Pl], oPivotKey, wKeyInx,
                                  oKeyDef.Ascending, oKeyDef.UpperFirst) < 0 do
          inc(Pl);
        while oComparator.Compare(oPivotKey, oKeys.Keys[Pr], wKeyInx,
                                  oKeyDef.Ascending, oKeyDef.UpperFirst) < 0 do
          dec(Pr);

        { Have we reached the pivot? }
        if Pl = Pr then begin
          Inc(Pl);
          Dec(Pr);
        end
        else if Pl < Pr then begin
          { No. Swap elements around the pivot. }
          SwapKeys(oKeys, Pl, Pr);
          inc(Pl);
          dec(Pr);
        end;
      until Pl > Pr;

      { Decide which partition to sort next. Which partition is bigger? }
      if (Pr - L) < (R - Pl) then begin
        { Left partition is bigger. }
        if Pl < R then begin
          { Stack the request for sorting right partition. }
          inc(StackP);
          LStack[StackP] := Pl;
          RStack[StackP] := R;
        end;
        { Continue sorting left partion. }
        R := Pr;
      end
      else begin
        { Right partition is bigger. }
        if L < Pr then begin
          { Stack the request for sorting left partition. }
          inc(StackP);
          LStack[StackP] := L;
          RStack[StackP] := Pr;
        end;
        { Continue sorting right partition. }
        L := Pl;
      end;
    until L >= R;
  until StackP < 0;

  { Look for duplicate key values. If find two or more duplicates then
    sort the duplicates on the next key value. }
  if wKeyInx < Pred(oKeyDefinitions.Count) then begin
    L := wRangeStart;                                                  {!!.55}
    R := L;                                                            {!!.55}
    repeat
      oPivotKey := oKeys.Keys[L];
      inc(R);
      bDupFound := False;
      while R < (wRangeEnd + 1) do begin                               {!!.55}
        if oKeys.Keys[R].KeyValues[wKeyInx] <> oPivotKey.KeyValues[wKeyInx] then
          Break
        else begin
          bDupFound := True;
          inc(R);
        end;
      end;
      if bDupFound then begin
        { Prep the key values. }
        sSelect := oKeyDefinitions.Definitions[wKeyInx + 1].Select;
        for Pm := L to Pred(R) do
          oKeys.Keys[Pm].Add(oKeys.Keys[Pm].Node.SelectString(sSelect));

        { Sort the range. }
        SortPrim(oKeys, oKeyDefinitions, wKeyInx + 1, L, R - 1);
      end;

      { By this time, R will be pointing at the next key after the last
        duplicate. Move L to the start of the next range. }
      L := R;
    until L >= (wRangeEnd + 1);                                        {!!.55}
  end;
end;
{--------}
procedure TXpNodeSorter.SwapKeys(oKeys : TXpSortKeys;
                           const wInx1, wInx2 : Longint);
var
  oKey : TXpSortKey;
begin
  oKey := oKeys.Keys[wInx1];
  oKeys.Keys[wInx1] := oKeys.Keys[wInx2];
  oKeys.Keys[wInx2] := oKey;
end;
{====================================================================}

initialization
  _Comparators := TXpPointerList.Create;

  { Register comparators. }
  TXpTextComparator.Register(xpsdtText, xpsDefault);
  TXpNumberComparator.Register(xpsdtNumber, xpsDefault);

finalization
  { Unregister comparators. }
  TxpNumberComparator.Unregister;
  TXpTextComparator.Unregister;

  _Comparators.Free;
    { Assumption: Units registering comparator classes will also unregister
      them. }
  _Comparators := nil;

end.
