{                                                                              }
{                             XML Functions v3.07                              }
{                                                                              }
{             This unit is copyright © 2000-2004 by David J Butler             }
{                                                                              }
{                  This unit is part of Delphi Fundamentals.                   }
{                 Its original file name is cXMLFunctions.pas                  }
{       The latest version is available from the Fundamentals home page        }
{                     http://fundementals.sourceforge.net/                     }
{                                                                              }
{                I invite you to use this unit, free of charge.                }
{        I invite you to distibute this unit, but it must be for free.         }
{             I also invite you to contribute to its development,              }
{             but do not distribute a modified copy of this file.              }
{                                                                              }
{          A forum is available on SourceForge for general discussion          }
{             http://sourceforge.net/forum/forum.php?forum_id=2117             }
{                                                                              }
{                                                                              }
{ Revision history:                                                            }
{   11/05/2000  1.01  Created cXML from cInternetStandards.                    }
{   14/02/2002  2.02  Fixed bug in xmlValidEncName. Thanks to Thomas Jensen    }
{                     <tjensen@xs4all.nl> for finding it.                      }
{   17/04/2002  2.03  Created cXMLFunctions from cXML.                         }
{   26/04/2002  2.04  Unicode support.                                         }
{   07/09/2003  3.05  Revised for Fundamentals 3.                              }
{   21/02/2004  3.06  Added xmlResolveEntityReference function.                }
{   01/04/2004  3.07  Compilable with FreePascal-1.92/Win32.                   }
{                                                                              }

{$INCLUDE ..\cDefines.inc}
unit cXMLFunctions;

interface

uses
  { Delphi }
  SysUtils,

  { Fundamentals }
  cUnicodeCodecs;

const
  UnitName      = 'cXMLFunctions';
  UnitVersion   = '3.07';
  UnitDesc      = 'XML functions';
  UnitCopyright = 'Copyright (c) 2000-2004 David J Butler';



{                                                                              }
{ Constants                                                                    }
{                                                                              }
const
  xmlVersion = '1.0';



{                                                                              }
{ Exception                                                                    }
{                                                                              }
type
  Exml = class(Exception);



{                                                                              }
{ Decoding                                                                     }
{                                                                              }
function  xmlValidChar(const Ch: Char): Boolean; overload;
function  xmlValidChar(const Ch: UCS4Char): Boolean; overload;
function  xmlValidChar(const Ch: WideChar): Boolean; overload;

function  xmlIsSpaceChar(const Ch: WideChar): Boolean;
function  xmlIsLetter(const Ch: WideChar): Boolean;
function  xmlIsDigit(const Ch: WideChar): Boolean;

function  xmlIsNameStartChar(const Ch: WideChar): Boolean;
function  xmlIsNameChar(const Ch: WideChar): Boolean;
function  xmlIsPubidChar(const Ch: WideChar): Boolean;
function  xmlValidName(const Text: WideString): Boolean;

const
  xmlSpace = [#$20, #$9, #$D, #$A];

function  xmlSkipSpace(var P: PWideChar): Boolean;
function  xmlSkipEq(var P: PWideChar): Boolean;
function  xmlExtractQuotedText(var P: PWideChar; var S: WideString): Boolean;

{ xmlGetEntityEncoding                                                         }
{   Buf is a pointer to the first part of an xml entity. It must include       }
{   the xml declaration. HeaderSize returns the number of bytes offset in      }
{   Buf to the actual xml.                                                     }
function  xmlGetEntityEncoding(const Buf: Pointer; const BufSize: Integer;
          var HeaderSize: Integer): TUnicodeCodecClass;

function  xmlResolveEntityReference(const RefName: WideString): WideChar;



{                                                                              }
{ Encoding                                                                     }
{                                                                              }
function  xmlTag(const Tag: WideString): WideString;
function  xmlEndTag(const Tag: WideString): WideString;
function  xmlAttrTag(const Tag: WideString;
          const Attr: WideString = ''): WideString;
function  xmlEmptyTag(const Tag, Attr: WideString): WideString;
procedure xmlSafeTextInPlace(var Txt: WideString);
function  xmlSafeText(const Txt: WideString): WideString;
function  xmlSpaceIndent(const IndentLength: Integer;
          const IndentLevel: Integer): WideString;
function  xmlTabIndent(const IndentLevel: Integer): WideString;
function  xmlComment(const Comment: WideString): WideString;



{                                                                              }
{ Test cases                                                                   }
{                                                                              }
procedure SelfTest;



implementation

uses
  { Fundamentals }
  cUtils,
  cUnicodeChar,
  cUnicode;



{                                                                              }
{ Decoding                                                                     }
{                                                                              }

{   [2]   Char ::=  #x9 | #xA | #xD | [#x20-#xD7FF] | [#xE000-#xFFFD] |        }
{                   [#x10000-#x10FFFF]                                         }
function xmlValidChar(const Ch: Char): Boolean;
begin
  Result := Ch in [#$9, #$A, #$D, #$20..#$FF];
end;

function xmlValidChar(const Ch: UCS4Char): Boolean;
begin
  if Ch <= $D7FF then
    if Ch >= $20 then
      Result := True
    else
      Result := Ch in [$9, $A, $D]
  else
    if Ch > $10FFFF then
      Result := False
    else
      Case Ch of
        $D800..$DFFF,
        $FFFE..$FFFF : Result := False;
      else
        Result := True;
      end;
end;

function xmlValidChar(const Ch: WideChar): Boolean;
begin
  if Ord(Ch) <= $D7FF then
    if Ord(Ch) >= $20 then
      Result := True
    else
      Result := Byte(Ord(Ch)) in [$9, $A, $D]
  else
    Case Ch of
      #$D800..#$DFFF,
      #$FFFE..#$FFFF : Result := False;
    else
      Result := True;
    end;
end;

{   [3]   S ::=  (#x20 | #x9 | #xD | #xA)+                                     }
function xmlIsSpaceChar(const Ch: WideChar): Boolean;
begin
  if Ord(Ch) > $20 then
    Result := False
  else
    Result := Byte(Ch) in [$20, $09, $0D, $0A];
end;

{   [84]  Letter ::=  BaseChar | Ideographic                                   }
function xmlIsLetter(const Ch: WideChar): Boolean;
begin
  Result := IsLetter(Ch);
end;

{   [88]  Digit ::=  [#x0030-#x0039] | [#x0660-#x0669] | [#x06F0-#x06F9] |     }
{                    [#x0966-#x096F] | [#x09E6-#x09EF] | [#x0A66-#x0A6F] |     }
{                    [#x0AE6-#x0AEF] | [#x0B66-#x0B6F] | [#x0BE7-#x0BEF] |     }
{                    [#x0C66-#x0C6F] | [#x0CE6-#x0CEF] | [#x0D66-#x0D6F] |     }
{                    [#x0E50-#x0E59] | [#x0ED0-#x0ED9] | [#x0F20-#x0F29]       }
function xmlIsDigit(const Ch: WideChar): Boolean;
begin
  Result := IsDecimalDigit(Ch);
end;


{   [4]   NameChar ::=  Letter | Digit | '.' | '-' | '_' | ':' |               }
{                 CombiningChar | Extender                                     }
function xmlIsNameChar(const Ch: WideChar): Boolean;
begin
  Result := xmlIsLetter(Ch) or xmlIsDigit(Ch);
  if Result then
    exit;
  Case Ch of
    '.', '-', '_', ':' : Result := True;
  end;
end;

{   [5]   Name ::=  (Letter | '_' | ':') (NameChar)*                           }
function xmlIsNameStartChar(const Ch: WideChar): Boolean;
begin
  Result := xmlIsLetter(Ch) or (Ch = '_') or (Ch = ':');
end;

{   [13]  PubidChar ::=  #x20 | #xD | #xA | [a-zA-Z0-9] |                      }
{                        [-'()+,./:=?;!*#@$_%]                                 }
function xmlIsPubidChar(const Ch: WideChar): Boolean;
begin
  Case Ch of
    ' ', #$D, #$A, 'A'..'Z', 'a'..'z', '0'..'9',
    '-', '''', '(', ')', '+', ',', '.', '/', ':',
    '=', '?', ';', '!', '*', '#', '@', '$', '_', '%' :
      Result := True;
  else
    Result := False;
  end;
end;

function xmlValidName(const Text: WideString): Boolean;
var P : PWideChar;
    L : Integer;
begin
  Result := False;
  P := Pointer(Text);
  L := Length(Text);
  if not Assigned(P) or (L = 0) then
    exit;
  if not xmlIsNameStartChar(P^) then
    exit;
  Inc(P);
  Dec(L);
  Result := WidePMatchChars(xmlIsNameChar, P, L) = L;
end;

{   [3]   S ::=  (#x20 | #x9 | #xD | #xA)+                                     }
function xmlSkipSpace(var P: PWideChar): Boolean;
begin
  Assert(Assigned(P));
  Result := WideZSkipChars(xmlIsSpaceChar, P) > 0;
end;

{   [25]  Eq ::=  S? '=' S?                                                    }
function xmlSkipEq(var P: PWideChar): Boolean;
var Q : PWideChar;
begin
  Q := P;
  xmlSkipSpace(Q);
  Result := Ord(Q^) = Ord('=');
  if not Result then
    exit;
  Inc(Q);
  xmlSkipSpace(Q);
  P := Q;
end;

{   [15]  Comment ::=  '<!--' ((Char - '-') | ('-' (Char - '-')))* '-->'       }
function xmlSkipComment(var P: PWideChar): Boolean;
var I : Integer;
begin
  Result := WidePMatchAnsiStr('<!--', P, True);
  if not Result then
    exit;
  I := WideZPosAnsiStr('-->', P, True);
  Result := I >= 0;
  if not Result then
    exit;
  Inc(P, I + 3);
end;

{   [..]  QuotedText  ::=  ' Text ' | " Text "                                 }
function xmlExtractQuotedText(var P: PWideChar; var S: WideString): Boolean;
begin
  Assert(Assigned(P));
  Result := WideZExtractAnsiCharQuoted('''', P, S);
  if Result then
    exit;
  Result := WideZExtractAnsiCharQuoted('"', P, S);
end;

{ Entity encoding                                                              }
{   [23]  XMLDecl ::=  '<?xml' VersionInfo EncodingDecl? SDDecl? S? '?>'       }
{   [24]  VersionInfo ::=  S 'version' Eq (' VersionNum ' | " VersionNum ")    }
{   [26]  VersionNum ::=  ([a-zA-Z0-9_.:] | '-')+                              }
{   [80]  EncodingDecl ::=  S 'encoding' Eq ('"' EncName '"' |                 }
{         "'" EncName "'" )                                                    }
function xmlGetEntityEncoding(const Buf: Pointer; const BufSize: Integer;
    var HeaderSize: Integer): TUnicodeCodecClass;
var S    : WideString;
    R    : PChar;
    P, Q : PWideChar;
    L    : Integer;
begin
  R := Buf;
  L := BufSize;
  // Detect Unicode markings
  Result := DetectUTFEncoding(R, L, HeaderSize);
  if Assigned(Result) then
    begin
      Inc(R, HeaderSize);
      Dec(L, HeaderSize);
    end
  else
    begin
      HeaderSize := 0;
      Result := TUTF8Codec;
      // Check if first character is a likely XML UTF-16 character
      if L >= 2 then
        begin
          P := Pointer(R);
          Case Ord(P^) of
            $0009, $000A, $000D, $0020, $003C : Result := TUTF16BECodec;
            $0900, $0A00, $0D00, $2000, $3C00 : Result := TUTF16LECodec;
          end;
        end;
    end;
  if L < 16 then
    exit;
  // Decode
  S := EncodingToUTF16(Result, R, MinI(L, 4096));
  // Locate XML declaration
  P := Pointer(S);
  While xmlSkipSpace(P) or xmlSkipComment(P) do ;
  if not WidePSkipAnsiStr('<?xml', P, False) then
    exit;
  if not xmlSkipSpace(P) then
    exit;
  // Locate encoding attribute
  While (Ord(P^) <> 0) and (P^ <> '>') do
    begin
      if xmlIsSpaceChar(P^) then
        begin
          Q := P;
          Inc(Q);
          if WidePSkipAnsiStr('encoding', Q, False) then
            if xmlSkipEq(Q) then
              begin
                // Extract encoding
                if not xmlExtractQuotedText(Q, S) then
                  if not WideZExtractBeforeAnsiCharSet(['>', #0] + xmlSpace, Q, S) then
                    S := '';
                WideTrimInPlace(S, IsWhiteSpace);
                if S <> '' then
                  // Get codec type from encoding name
                  Result := GetCodecClassByAlias(S);
                // Found encoding attribute
                exit;
              end;
        end;
      // Next character
      Inc(P);
    end;
end;

function xmlResolveEntityReference(const RefName: WideString): WideChar;
begin
  if WideEqualAnsiStr('amp', RefName, True) then
    Result := '&' else
  if WideEqualAnsiStr('lt', RefName, True) then
    Result := '<' else
  if WideEqualAnsiStr('gt', RefName, True) then
    Result := '>' else
  if WideEqualAnsiStr('quot', RefName, True) then
    Result := '"' else
  if WideEqualAnsiStr('apos', RefName, True) then
    Result := '''' 
  else
    Result := WideChar(#0);
end;



{                                                                              }
{ Encoding                                                                     }
{                                                                              }
function xmlTag(const Tag: WideString): WideString;
begin
  Result := '<' + Tag + '>';
end;

function xmlAttrTag(const Tag: WideString; const Attr: WideString): WideString;
begin
  if Attr = '' then
    Result := xmlTag(Tag)
  else
    Result := '<' + Tag + ' ' + Attr + '>';
end;

function xmlEndTag(const Tag: WideString): WideString;
begin
  Result := '</' + Tag + '>';
end;

function xmlEmptyTag(const Tag, Attr: WideString): WideString;
begin
  if Attr = '' then
    Result := '<' + Tag + '/>'
  else
    Result := '<' + Tag + ' ' + Attr + '/>';
end;

procedure xmlSafeTextInPlace(var Txt: WideString);
begin
  if WidePosAnsiCharSet([#0, '&', '''', '"', '>', '<'], Txt) <= 0 then
    exit;
  WideReplaceChar(#0, '', Txt);
  WideReplaceChar('&', '&amp;', Txt);
  WideReplaceChar('''', '&apos;', Txt);
  WideReplaceChar('"', '&quot;', Txt);
  WideReplaceChar('>', '&gt;', Txt);
  WideReplaceChar('<', '&lt;', Txt);
end;

function xmlSafeText(const Txt: WideString): WideString;
begin
  Result := Txt;
  xmlSafeTextInPlace(Result);
end;

function xmlSpaceIndent(const IndentLength: Integer; const IndentLevel: Integer): WideString;
begin
  Result := WideDup(#32, IndentLevel * IndentLength);
end;

function xmlTabIndent(const IndentLevel: Integer): WideString;
begin
  Result := WideDup(#9, IndentLevel);
end;

{   [15]  Comment ::=  '<!--' ((Char - '-') | ('-' (Char - '-')))* '-->'       }
function xmlComment(const Comment: WideString): WideString;
begin
  Assert(WidePos('--', Comment) = 0, 'Invalid sequence in comment');
  Result := '<!--' + Comment + '-->';
end;



{                                                                              }
{ Test cases                                                                   }
{                                                                              }
{$ASSERTIONS ON}
procedure SelfTest;
var S : String;
    I : Integer;
begin
  Assert(xmlValidChar(WideChar(#32)), 'xmlValidChar');
  Assert(xmlValidChar(WideChar(#13)), 'xmlValidChar');
  Assert(not xmlValidChar(WideChar(#0)), 'xmlValidChar');
  Assert(not xmlValidChar(WideChar(#11)), 'xmlValidChar');
  Assert(xmlValidName('_Test_0'), 'xmlValidName');
  Assert(not xmlValidName('X '), 'xmlValidName');
  Assert(not xmlValidName('X$'), 'xmlValidName');
  Assert(not xmlValidName('5X'), 'xmlValidName');
  Assert(xmlTag('Test') = '<Test>', 'xmlTag');
  Assert(xmlComment('Test') = '<!--Test-->', 'xmlComment');
  Assert(xmlSafeText('(abc) [123]') = '(abc) [123]', 'xmlSafeText');
  Assert(xmlSafeText('a&<b>') = 'a&amp;&lt;b&gt;', 'xmlSafeText');
  Assert(xmlIsSpaceChar(#32), 'xmlIsSpaceChar');
  Assert(xmlIsSpaceChar(#13), 'xmlIsSpaceChar');
  Assert(not xmlIsSpaceChar('_'), 'xmlIsSpaceChar');
  S := '<?xml version="1.0" encoding="ascii">';
  Assert(xmlGetEntityEncoding(Pointer(S), Length(S), I) = TUSASCIICodec, 'xmlGetEntityEncoding');
  {$IFNDEF FREEPASCAL} // GetCodecClassByAlias not working under FreePascal
  S := '  <?xml  attr  encoding=ISO-8859-10 >  ';
  Assert(xmlGetEntityEncoding(Pointer(S), Length(S), I) = TISO8859_10Codec, 'xmlGetEntityEncoding');
  {$ENDIF}
end;



end.

