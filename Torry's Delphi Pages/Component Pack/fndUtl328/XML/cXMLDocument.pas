{                                                                              }
{                              XML Document v3.09                              }
{                                                                              }
{             This unit is copyright © 2000-2004 by David J Butler             }
{                                                                              }
{                  This unit is part of Delphi Fundamentals.                   }
{                 Its original file name is cXMLDocument.pas                   }
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
{   11/05/2000  0.01  Created cXML from cInternetStandards.                    }
{   08/05/2001  0.02  Complete revision.                                       }
{   11/05/2001  2.03  Added DTD classes.                                       }
{   15/01/2002  2.04  Bug fixes and 'Pretty Printer' by Laurent Baudrillard.   }
{   17/04/2002  2.05  Created cXMLDocument from cXML.                          }
{   26/04/2002  2.06  Revised for Unicode support.                             }
{                     Merged base classes into AxmlType.                       }
{                     Refactor PrettyPrinter to general printer.               }
{   07/09/2003  3.07  Revised for Fundamentals 3.                              }
{   21/02/2004  3.08  Improvements.                                            }
{   01/04/2004  3.09  Compilable with FreePascal-1.92/Win32.                   }
{                                                                              }

{$INCLUDE ..\cDefines.inc}
unit cXMLDocument;

interface

uses
  { Fundamentals }
  cUtils,
  cWriters,
  cUnicodeChar,
  cUnicode,
  cXMLFunctions;

const
  UnitName      = 'cXMLDocument';
  UnitVersion   = '3.09';
  UnitDesc      = 'XML document';
  UnitCopyright = 'Copyright (c) 2000-2004 David J Butler';



{                                                                              }
{ XML Printer                                                                  }
{   AxmlPrinter is the base class for custom XML representations.              }
{   Implementations must override PrintToken and PrintSpecial.                 }
{                                                                              }
type
  TxmlPrintOptions = Set of (
      xmloNoFormatting,     // Don't use formatting characters
      xmloNoIndent,         // Don't use indentation characters
      xmloNoEOL,            // Don't use EOL characters
      xmloUseTabIndent,     // Use tab instead of space to indent
      xmloUseDoubleQuotes,  // Use double quotes instead of single quotes
      xmloForceQuoteType);  // Don't select quote type based on content

const
  xmlDefaultPrintOptions = [];
  xmlDefaultIndentLength = 2;

type
  TxmlPrintToken = (
      xmltDefault,
      xmltText,
      xmltName,
      xmltTagName,
      xmltAttrName,
      xmltSymbol,
      xmltComment);

  TxmlPrintSpecialSymbol = (
      xmlsSpace,
      xmlsTab,
      xmlsEOL);

  AxmlPrinter = class
  protected
    FOptions      : TxmlPrintOptions;
    FIndentLength : Integer;

    procedure PrintToken(const TokenType: TxmlPrintToken;
              const Txt: WideString); virtual; abstract;
    procedure PrintSpecial(const SpecialSymbol: TxmlPrintSpecialSymbol;
              const Count: Integer); virtual; abstract;

  public
    constructor Create(const Options: TxmlPrintOptions = xmlDefaultPrintOptions;
                const IndentLength: Integer = xmlDefaultIndentLength);

    property  Options: TxmlPrintOptions read FOptions write FOptions;
    property  IndentLength: Integer read FIndentLength write FIndentLength;

    function  GetQuoteChar(const Txt: WideString): WideChar;

    procedure PrintEOL;
    procedure PrintSpace(const Count: Integer = 1);
    procedure PrintTab(const Count: Integer = 1);
    procedure PrintIndent(const IndentLevel: Integer);

    procedure PrintDefault(const Txt: WideString);
    procedure PrintText(const Txt: WideString);
    procedure PrintSymbol(const Txt: WideString);
    procedure PrintQuoteStr(const Txt: WideString);
    procedure PrintSafeQuotedText(const Txt: WideString);

    procedure PrintName(const Txt: WideString);
    procedure PrintTagName(const Txt: WideString);
    procedure PrintAttrName(const Txt: WideString);

    procedure PrintComment(const Txt: WideString);
  end;



{                                                                              }
{ XML String Printer                                                           }
{   AxmlPrinter implementation that stores the XML text as a WideString.       }
{                                                                              }
type
  TxmlStringPrinter = class(AxmlPrinter)
  protected
    FxmlWideString : TWideStringWriter;

    procedure PrintToken(const TokenType: TxmlPrintToken;
              const Txt: WideString); override;
    procedure PrintSpecial(const SpecialSymbol: TxmlPrintSpecialSymbol;
              const Count: Integer); override;

    function  GetXMLWideString: WideString;
    function  GetXMLUTF8String: String;

  public
    constructor Create(const Options: TxmlPrintOptions = xmlDefaultPrintOptions;
                const IndentLength: Integer = xmlDefaultIndentLength);
    destructor Destroy; override;

    property  XMLWideString: WideString read GetXMLWideString;
    property  XMLUTF8String: String read GetXMLUTF8String;
  end;



{                                                                              }
{ XML object representation                                                    }
{                                                                              }
type
  {                                                                            }
  { AxmlType                                                                   }
  {   Common base class for XML data structures.                               }
  {                                                                            }
  TxmlMarkupDeclarationList = class; // forward
  CxmlType = class of AxmlType;
  AxmlType = class
  protected
    procedure Init; virtual;

    function  GetName: WideString; virtual;
    function  GetNameSpace: WideString;
    function  GetLocalName: WideString;

    function  GetChildCount: Integer; virtual;
    function  GetChildByIndex(const Idx: Integer): AxmlType; virtual;

    function  GetChildByName(const Name: WideString): AxmlType;
    function  PosNext(var C: AxmlType; const ClassType: CxmlType = nil;
              const PrevPos: Integer = -1): Integer; overload;
    function  PosNext(var C: AxmlType; const Name: WideString;
              const ClassType: CxmlType = nil;
              const PrevPos: Integer = -1): Integer; overload;
    function  Find(const ClassType: CxmlType): AxmlType; overload;
    function  Find(const Name: WideString;
              const ClassType: CxmlType = nil): AxmlType; overload;

  public
    constructor Create;

    property  Name: WideString read GetName;  // Name ::= <NameSpace> ':' <LocalName>
    property  NameSpace: WideString read GetNameSpace;
    property  LocalName: WideString read GetLocalName;

    function  IsName(const Name: WideString): Boolean;
    function  IsAsciiName(const Name: AnsiString;
              const CaseSensitive: Boolean = True): Boolean;

    property  ChildCount: Integer read GetChildCount;
    property  ChildByIndex[const Idx: Integer]: AxmlType read GetChildByIndex;
    property  ChildByName[const Name: WideString]: AxmlType read GetChildByName; default;
    function  GetChildCountByClass(const ClassType: CxmlType = nil): Integer;
    function  GetNames(const ClassType: CxmlType = nil): WideStringArray;

    procedure AddChild(const Child: AxmlType); virtual;
    procedure AddChildren(const Children: Array of AxmlType);
    function  AddAssigned(const Child: AxmlType): Boolean;

    function  TextContent(const Declarations: TxmlMarkupDeclarationList = nil): WideString; virtual;

    procedure Print(const D: AxmlPrinter; const IndentLevel: Integer = 0); virtual;
    function  AsWideString(const Options: TxmlPrintOptions = xmlDefaultPrintOptions;
              const IndentLength: Integer = xmlDefaultIndentLength): WideString;
    function  AsUTF8String(const Options: TxmlPrintOptions = xmlDefaultPrintOptions;
              const IndentLength: Integer = xmlDefaultIndentLength): String;
  end;
  ExmlType = class(Exml);

  {                                                                            }
  { TxmlTypeList                                                               }
  {   Ordererd list of XML data structures.                                    }
  {                                                                            }
  AxmlTypeArray = Array of AxmlType;
  TxmlTypeList = class(AxmlType)
  protected
    FChildren : AxmlTypeArray;

    function  GetChildCount: Integer; override;
    function  GetChildByIndex(const Idx: Integer): AxmlType; override;

  public
    constructor Create(const Children: Array of AxmlType); reintroduce; overload;
    destructor Destroy; override;

    procedure AddChild(const Child: AxmlType); override;

    procedure Print(const D: AxmlPrinter; const IndentLevel: Integer = 0); override;
    function  TextContent(const Declarations: TxmlMarkupDeclarationList = nil): WideString; override;
  end;



  {                                                                            }
  { CHARACTER DATA                                                             }
  {                                                                            }

  { TxmlCharData                                                               }
  {   [..]  CharData ::=  [^<&]*                                               }
  TxmlCharData = class(AxmlType)
  protected
    FData : WideString;

  public
    constructor Create(const Data: WideString);

    procedure Print(const D: AxmlPrinter; const IndentLevel: Integer = 0); override;
    function  TextContent(const Declarations: TxmlMarkupDeclarationList = nil): WideString; override;

    property  Data: WideString read FData;
  end;

  { TxmlCDSect                                                                 }
  {   [18]  CDSect ::=  CDStart CData CDEnd                                    }
  {   [19]  CDStart ::=  '<![CDATA['                                           }
  {   [20]  CData ::=  (Char* - (Char* ']]>' Char*))                           }
  {   [21]  CDEnd ::=  ']]>'                                                   }
  TxmlCDSect = class(TxmlCharData)
  public
    constructor Create(const Data: WideString);

    procedure Print(const D: AxmlPrinter; const IndentLevel: Integer = 0); override;
  end;



  {                                                                            }
  { REFERENCES                                                                 }
  {                                                                            }

  { AxmlReference                                                              }
  {   Base class for Reference entities                                        }
  {   [67]  Reference ::=  EntityRef | CharRef                                 }
  AxmlReference = class(AxmlType);

  { TxmlGeneralEntityRef                                                       }
  {   [68]  EntityRef ::=  '&' Name ';'                                        }
  TxmlGeneralEntityRef = class(AxmlReference)
  protected
    FRefName : WideString;

  public
    constructor Create(const RefName: WideString);

    procedure Print(const D: AxmlPrinter; const IndentLevel: Integer = 0); override;
    function  TextContent(const Declarations: TxmlMarkupDeclarationList = nil): WideString; override;

    property  RefName: WideString read FRefName;
  end;

  { TxmlCharRef                                                                }
  {   [66]  CharRef ::=  '&#' [0-9]+ ';' | '&#x' [0-9a-fA-F]+ ';'              }
  TxmlCharRef = class(AxmlReference)
  protected
    FHex    : Boolean;
    FNumber : UCS4Char;

  public
    constructor Create(const Number: UCS4Char; const Hex: Boolean = True);

    procedure Print(const D: AxmlPrinter; const IndentLevel: Integer = 0); override;
    function  TextContent(const Declarations: TxmlMarkupDeclarationList = nil): WideString; override;

    property  Hex: Boolean read FHex write FHex;
    property  Number: UCS4Char read FNumber write FNumber;
  end;

  { TxmlPEReference                                                            }
  {   [69]  PEReference ::=  '%' Name ';'                                      }
  TxmlPEReference = class(AxmlType)
  protected
    FRefName : WideString;

  public
    constructor Create(const RefName: WideString);

    procedure Print(const D: AxmlPrinter; const IndentLevel: Integer = 0); override;
    function  TextContent(const Declarations: TxmlMarkupDeclarationList = nil): WideString; override;

    property  RefName: WideString read FRefName;
  end;

  {   [..]  ReferenceText = (CharData | Reference | PEReference)*              }
  TxmlReferenceText = class(TxmlTypeList);



  {                                                                            }
  { FORMATTING TOKENS                                                          }
  {                                                                            }

  { TxmlLiteralFormatting                                                      }
  {   Represents a literal formatting string.                                  }
  TxmlLiteralFormatting = class(AxmlType)
  protected
    FText : WideString;

  public
    constructor Create(const Text: WideString);

    procedure Print(const D: AxmlPrinter; const IndentLevel: Integer = 0); override;

    property  Text: WideString read FText;
  end;

  { TxmlLiteralFormattingList                                                  }
  {   Represents a list of formatting tokens.                                  }
  TxmlLiteralFormattingList = class(TxmlTypeList)
    procedure Add(const Text: WideString); reintroduce; overload;
  end;

  { TxmlSpace                                                                  }
  {   [3]   S ::=  (#x20 | #x9 | #xD | #xA)+                                   }
  TxmlSpace = class(TxmlLiteralFormatting)
    constructor Create(const Text: WideString);
  end;

  {   [..]  QuotedReferenceText ::=  "'" ReferenceText "'" |                   }
  {                                  '"' ReferenceText '"'                     }
  TxmlQuotedReferenceText = class(TxmlReferenceText)
    procedure Print(const D: AxmlPrinter; const IndentLevel: Integer = 0); override;
  end;
  CxmlQuotedReferenceText = class of TxmlQuotedReferenceText;

  { TxmlQuotedText                                                             }
  {   [..]  QuotedText ::=  "'" Text "'" | '"' Text '"'                        }
  TxmlQuotedText = class(TxmlLiteralFormatting)
    public
    procedure Print(const D: AxmlPrinter; const IndentLevel: Integer = 0); override;
    function  IntegerContent(const DefaultValue: Int64 = -1): Int64;
  end;



  {                                                                            }
  { MISC TOKENS                                                                }
  {                                                                            }

  { TxmlComment                                                                }
  {   [15]  Comment ::=  '<!--' ((Char - '-') | ('-' (Char - '-')))* '-->'     }
  TxmlComment = class(AxmlType)
  protected
    FText : WideString;

  public
    constructor Create(const Text: WideString);

    procedure Print(const D: AxmlPrinter; const IndentLevel: Integer = 0); override;
    property  Text: WideString read FText;
  end;

  { TxmlProcessingInstruction                                                  }
  {   [16]  PI ::=  '<?' PITarget (S (Char* - (Char* '?>' Char*)))? '?>'       }
  TxmlProcessingInstruction = class(AxmlType)
  protected
    FText     : WideString;
    FPITarget : WideString;

  public
    constructor Create(const PITarget, Text: WideString);

    procedure Print(const D: AxmlPrinter; const IndentLevel: Integer = 0); override;

    property  PITarget: WideString read FPITarget;
    property  Text: WideString read FText;
  end;

  { TxmlMiscList                                                               }
  {   List of Misc entities.                                                   }
  {   [27]  Misc ::=  Comment | PI |  S                                        }
  TxmlMiscList = class(TxmlTypeList)
  public
    function  FirstComment: WideString;
    function  Comments: WideStringArray;

    function  FirstProcessingInstruction(const PITarget: WideString): WideString;
    function  ProcessingInstructions(const PITarget: WideString): WideStringArray;
  end;



  {                                                                            }
  { ATTRIBUTES                                                                 }
  {                                                                            }

  { TxmlAttValue                                                               }
  {   [10]  AttValue ::=  '"' ([^<&"] | Reference)* '"'                        }
  {                    |  "'" ([^<&'] | Reference)* "'"                        }
  TxmlAttValue = class(TxmlQuotedReferenceText)
  public
    function  IntegerContent(const Declarations: TxmlMarkupDeclarationList = nil;
              const DefaultValue: Int64 = -1): Int64;
    function  FloatContent(const Declarations: TxmlMarkupDeclarationList = nil;
              const DefaultValue: Extended = 0.0): Extended;
  end;

  { TxmlAttribute                                                              }
  {   [41]  Attribute ::=  Name Eq AttValue                                    }
  TxmlAttribute = class(AxmlType)
  protected
    FName  : WideString;
    FValue : TxmlAttValue;

    function  GetName: WideString; override;

  public
    constructor Create(const Name: WideString; const Value: TxmlAttValue); overload;

    procedure Print(const D: AxmlPrinter; const IndentLevel: Integer = 0); override;

    property  Value: TxmlAttValue read FValue;
    function  TextContent(const Declarations: TxmlMarkupDeclarationList = nil): WideString; override;
    function  IntegerContent(const Declarations: TxmlMarkupDeclarationList = nil; const DefaultValue: Int64 = -1): Int64;
    function  FloatContent(const Declarations: TxmlMarkupDeclarationList = nil; const DefaultValue: Extended = 0.0): Extended;

    function  IsNameSpaceDeclaration: Boolean;
    function  GetNameSpaceDeclaration(var NameSpace, URI: WideString): Boolean;
  end;

  { AxmlAttributeList                                                          }
  {   Abstraction of a list of attributes.                                     }
  AxmlAttributeList = class(AxmlType)
  protected
    procedure InitList(const List: TxmlTypeList); virtual;
    function  GetAttrCount: Integer; virtual; abstract;
    function  GetAttrNames: WideStringArray; virtual; abstract;

  public
    constructor Create(const List: TxmlTypeList);

    property  AttrCount: Integer read GetAttrCount;
    property  AttrNames: WideStringArray read GetAttrNames;
    function  HasAttribute(const Name: WideString): Boolean; virtual; abstract;
    function  FindNextAttr(var A: TxmlAttribute; const Idx: Integer = -1): Integer; virtual; abstract;

    function  AttrAsText(const Name: WideString;
              const Declarations: TxmlMarkupDeclarationList = nil;
              const DefaultValue: WideString = ''): WideString; virtual; abstract;
    function  AttrAsInteger(const Name: WideString;
              const Declarations: TxmlMarkupDeclarationList = nil;
              const DefaultValue: Int64 = -1): Int64; virtual;
    function  AttrAsFloat(const Name: WideString;
              const Declarations: TxmlMarkupDeclarationList = nil;
              const DefaultValue: Extended = 0.0): Extended; virtual;

    function  GetNameSpaceURI(const NameSpace: WideString): WideString;
  end;

  { TxmlAttributeList                                                          }
  {   [..]  (S Attribute)*                                                     }
  TxmlAttributeList = class(AxmlAttributeList)
  protected
    FList : TxmlTypeList;

    procedure InitList(const List: TxmlTypeList); override;
    function  GetAttrCount: Integer; override;
    function  GetAttrNames: WideStringArray; override;

  public
    destructor Destroy; override;

    procedure Print(const D: AxmlPrinter; const IndentLevel: Integer = 0); override;
    function  HasAttribute(const Name: WideString): Boolean; override;
    function  FindNextAttr(var A: TxmlAttribute; const Idx: Integer = -1): Integer; override;
    function  AttrAsText(const Name: WideString; const Declarations: TxmlMarkupDeclarationList = nil; const DefaultValue: WideString = ''): WideString; override;
    function  AttrAsInteger(const Name: WideString; const Declarations: TxmlMarkupDeclarationList = nil; const DefaultValue: Int64 = -1): Int64; override;
    function  AttrAsFloat(const Name: WideString; const Declarations: TxmlMarkupDeclarationList = nil; const DefaultValue: Extended = 0.0): Extended; override;
  end;

  {   [..]  TextAttribute ::=  Name Eq QuotedText                              }
  { TxmlTextAttribute                                                          }
  TxmlTextAttribute = class(AxmlType)
  protected
    FName    : WideString;
    FValue   : TxmlQuotedText;

    function  GetName: WideString; override;

  public
    constructor Create(const Name: WideString; const Value: TxmlQuotedText); overload;
    constructor Create(const Name, TextValue: WideString); overload;

    procedure Print(const D: AxmlPrinter; const IndentLevel: Integer = 0); override;
    property  Value: TxmlQuotedText read FValue;
    function  ValueText: WideString;
    function  IntegerContent(const DefaultValue: Int64 = -1): Int64;
  end;



  {                                                                            }
  { DECLARATIONS                                                               }
  {                                                                            }

  { TxmlXMLDecl                                                                }
  {   [23]  XMLDecl ::=  '<?xml' VersionInfo EncodingDecl? SDDecl? S? '?>'     }
  {   [24]  VersionInfo ::=  S 'version' Eq (' VersionNum ' | " VersionNum ")  }
  {   [25]  Eq ::=  S? '=' S?                                                  }
  {   [26]  VersionNum ::=  ([a-zA-Z0-9_.:] | '-')+                            }
  {   [80]  EncodingDecl ::=  S 'encoding' Eq                                  }
  {                           ('"' EncName '"' |  "'" EncName "'" )            }
  {   [81]  EncName ::=  [A-Za-z] ([A-Za-z0-9._] | '-')*                       }
  {   [32]  SDDecl ::=  S 'standalone' Eq (("'" ('yes' | 'no') "'") |          }
  {                    ('"' ('yes' | 'no') '"'))                               }
  TxmlOptionalBoolean = (obUnspecified, obFalse, obTrue);
  TxmlXMLDecl = class(TxmlTypeList)
  protected
    function  GetVersionNum: String;
    function  GetEncodingName: String;
    function  GetStandalone: TxmlOptionalBoolean;

  public
    procedure Print(const D: AxmlPrinter; const IndentLevel: Integer = 0); override;
    property  VersionNum: String read GetVersionNum;
    property  EncodingName: String read GetEncodingName;
    property  Standalone: TxmlOptionalBoolean read GetStandalone;
  end;

  { AxmlDeclaration                                                            }
  {   Base class for declarations.                                             }
  AxmlDeclaration = class(TxmlTypeList)
  protected
    FName : WideString;

  public
    constructor Create(const Name: WideString);

    procedure Print(const D: AxmlPrinter; const IndentLevel: Integer = 0); override;
    property  Name: WideString read FName;
  end;

  { TxmlChildrenElementContentSpec                                             }
  {   [48]  cp ::=  (Name | choice | seq) ('?' | '*' | '+')?                   }
  TxmlChildSpecNumerator = (csnOne, csnOptional, csnAny, csnAtLeastOne);
  AxmlChildSpec = class(AxmlType)
    Numerator : TxmlChildSpecNumerator;

    procedure Print(const D: AxmlPrinter; const IndentLevel: Integer = 0); override;
  end;
  {   [..]  namechildspec ::=  Name ('?' | '*' | '+')?                         }
  TxmlNameChildSpec = class(AxmlChildSpec)
  protected
    FName : WideString;

  public
    constructor Create(const Name: WideString);
    procedure Print(const D: AxmlPrinter; const IndentLevel: Integer = 0); override;
    property  Name: WideString read FName;
  end;
  {   [..]  listchildspec ::=  childspec* ('?' | '*' | '+')?                   }
  AxmlListChildSpec = class(AxmlChildSpec)
    List : TxmlTypeList;

    function  PosNextChildSpec(var C: AxmlChildSpec; const PrevPos: Integer = -1): Integer;
    procedure Print(const D: AxmlPrinter; const IndentLevel: Integer = 0); override;
  end;
  {   [49]  choice ::=  '(' S? cp ( S? '|' S? cp )* S? ')'                     }
  {   [..]  choicechildspec ::=  choice ('?' | '*' | '+')?                     }
  TxmlChoiceChildSpec = class(AxmlListChildSpec);
  {   [50]  seq ::=  '(' S? cp ( S? ',' S? cp )* S? ')'                        }
  {   [..]  seqchildspec ::=  seq ('?' | '*' | '+')?                           }
  TxmlSeqChildSpec = class(AxmlListChildSpec);
  {   [47]  children ::=  (choice | seq) ('?' | '*' | '+')?                    }
  TxmlChildrenElementContentSpec = AxmlListChildSpec;

  { AxmlContentSpec                                                            }
  {   [46]  contentspec ::=  'EMPTY' | 'ANY' | Mixed | children                }
  {   [51]  Mixed ::=  '(' S? '#PCDATA' (S? '|' S? Name)* S? ')*'              }
  {                  | '(' S? '#PCDATA' S? ')'                                 }
  AxmlContentSpec = class(AxmlType);
  TxmlEmptyContentSpec = class(AxmlContentSpec)
    procedure Print(const D: AxmlPrinter; const IndentLevel: Integer = 0); override;
  end;
  TxmlAnyContentSpec = class(AxmlContentSpec)
    procedure Print(const D: AxmlPrinter; const IndentLevel: Integer = 0); override;
  end;
  TxmlMixedContentSpec = class(AxmlContentSpec)
  protected
    FList : TxmlTypeList;

    function  GetAllowedNames: WideStringArray;

  public
    procedure Print(const D: AxmlPrinter; const IndentLevel: Integer = 0); override;
    property  List: TxmlTypeList read FList write FList;
    property  AllowedNames: WideStringArray read GetAllowedNames;
  end;
  TxmlChildrenContentSpec = class(AxmlContentSpec)
  protected
    FChildrenSpec : TxmlChildrenElementContentSpec;

  public
    procedure Print(const D: AxmlPrinter; const IndentLevel: Integer = 0); override;
    property  ChildrenSpec: TxmlChildrenElementContentSpec read FChildrenSpec write FChildrenSpec;
  end;

  { TxmlElementDeclaration                                                     }
  {   [45]  elementdecl ::=  '<!ELEMENT' S Name S contentspec S? '>'           }
  TxmlElementContentSpec = (ecsEmpty, ecsAny, ecsMixed, ecsChildren);
  TxmlElementDeclaration = class(AxmlDeclaration)
  protected
    FContentSpec : AxmlContentSpec;

  public
    constructor Create(const Name: WideString;
                const ContentSpecType: TxmlElementContentSpec);

    procedure Print(const D: AxmlPrinter; const IndentLevel: Integer = 0); override;
    property  ContentSpec: AxmlContentSpec read FContentSpec;
  end;

  { TxmlAttListDecl                                                            }
  {   [53]  AttDef ::=  S Name S AttType S DefaultDecl                         }
  {   [55]  StringType ::=  'CDATA'                                            }
  {   [56]  TokenizedType ::=  'ID' | 'IDREF' | 'IDREFS' | 'ENTITY'            }
  {                          | 'ENTITIES' | 'NMTOKEN' | 'NMTOKENS'             }
  {   [57]  EnumeratedType ::=  NotationType | Enumeration                     }
  {   [58]  NotationType ::=  'NOTATION' S '(' S? Name (S? '|' S? Name)*       }
  {                           S? ')'                                           }
  {   [59]  Enumeration ::=  '(' S? Nmtoken (S? '|' S? Nmtoken)* S? ')'        }
  {   [60]  DefaultDecl ::=  '#REQUIRED' | '#IMPLIED'                          }
  {                        | (('#FIXED' S)? AttValue)                          }
  {   [54]  AttType ::=  StringType | TokenizedType | EnumeratedType           }
  {   [10]  AttValue ::=  '"' ([^<&"] | Reference)* '"'                        }
  {                    |  "'" ([^<&'] | Reference)* "'"                        }
  TxmlAttType = (atNone,
                 atStringType,
                 atEnumeratedNotationType, atEnumeratedEnumerationType,
                 atTokenizedTypeID, atTokenizedTypeIDREF, atTokenizedTypeIDREFS,
                 atTokenizedTypeENTITY, atTokenizedTypeENTITIES, atTokenizedTypeNMTOKEN,
                 atTokenizedTypeNMTOKENS);
  TxmlDefaultType = (dtRequired, dtImplied, dtFixed, dtValue);
  TxmlAttDef = class(AxmlType)
  protected
    FName               : WideString;
    FAttType            : TxmlAttType;
    FNames              : TxmlTypeList;
    FDefaultType        : TxmlDefaultType;
    FDefaultValue       : TxmlAttValue;

  public
    constructor Create(const Name: WideString; const AttType: TxmlAttType;
                const Names: TxmlTypeList; const DefaultType: TxmlDefaultType;
                const DefaultValue: TxmlAttValue);

    procedure Print(const D: AxmlPrinter; const IndentLevel: Integer = 0); override;
    property  Name: WideString read FName;
    property  AttType: TxmlAttType read FAttType;
    property  Names: TxmlTypeList read FNames;
    property  DefaultType: TxmlDefaultType read FDefaultType;
    property  DefaultValue: TxmlAttValue read FDefaultValue;
  end;
  {   [52]  AttlistDecl ::=  '<!ATTLIST' S Name AttDef* S? '>'                 }
  TxmlAttListDecl = class(AxmlDeclaration)
    procedure Print(const D: AxmlPrinter; const IndentLevel: Integer = 0); override;
  end;

  { TxmlExternalID                                                             }
  {   [75]  ExternalID ::=  'SYSTEM' S SystemLiteral                           }
  {                       | 'PUBLIC' S PubidLiteral S SystemLiteral            }
  TxmlExternalIDType = (eidSystem, eidPublic);
  TxmlExternalID = class(AxmlType)
  protected
    FIDType   : TxmlExternalIDType;
    FSystemID : TxmlQuotedText;
    FPublicID : TxmlQuotedText;

  public
    constructor CreateSystemID(const SystemID: TxmlQuotedText);
    constructor CreatePublicID(const PublicID, SystemID: TxmlQuotedText);

    procedure Print(const D: AxmlPrinter; const IndentLevel: Integer = 0); override;
    property  IDType: TxmlExternalIDType read FIDType;
    property  SystemID: TxmlQuotedText read FSystemID;
    property  PublicID: TxmlQuotedText read FPublicID;
  end;
  CxmlExternalID = class of TxmlExternalID;

  {   [..]  ExternalIDNData ::=  (ExternalID NDataDecl?)                       }
  {   [76]  NDataDecl ::=  S 'NDATA' S Name                                    }
  TxmlExternalIDNData = class(TxmlExternalID)
  protected
    FNData : WideString;

  public
    procedure Print(const D: AxmlPrinter; const IndentLevel: Integer = 0); override;
    property  NData: WideString read FNData write FNData;
  end;

  {   [70]  EntityDecl ::=  GEDecl | PEDecl                                    }
  {   [71]  GEDecl ::=  '<!ENTITY' S Name S EntityDef S? '>'                   }
  {   [72]  PEDecl ::=  '<!ENTITY' S '%' S Name S PEDef S? '>'                 }
  {   [73]  EntityDef ::=  EntityValue | (ExternalID NDataDecl?)               }
  {   [74]  PEDef ::=  EntityValue | ExternalID                                }
  {   [9]   EntityValue ::=  '"' ([^%&"] | PEReference | Reference)* '"'       }
  {                       |  "'" ([^%&'] | PEReference | Reference)* "'"       }
  TxmlEntityDeclaration = class(AxmlDeclaration)
  protected
    FPEDeclaration  : Boolean;
    FDefinition     : AxmlType;

  public
    constructor Create(const PEDeclaration: Boolean; const Name: WideString;
                       const Definition: AxmlType);
    property  Definition: AxmlType read FDefinition;
    procedure Print(const D: AxmlPrinter; const IndentLevel: Integer = 0); override;
  end;

  {   [29]  markupdecl ::=  elementdecl | AttlistDecl | EntityDecl |           }
  {                         NotationDecl | PI | Comment                        }
  TxmlMarkupDeclarationList = class(TxmlMiscList)
  protected
    FParentDeclarationList : TxmlMarkupDeclarationList;

  public
    constructor Create(const ParentDeclarationList: TxmlMarkupDeclarationList = nil);

    property  ParentDeclarationList: TxmlMarkupDeclarationList read FParentDeclarationList;
    function  FindEntityDeclaration(const Name: WideString): TxmlEntityDeclaration;
    function  ResolveEntityReference(const RefName: WideString; var Value: WideString): Boolean;
    function  ResolveParseEntityReference(const RefName: WideString; var Value: WideString): Boolean;
  end;

  {   [82]  NotationDecl ::=  '<!NOTATION' S Name S                            }
  {                           (ExternalID | PublicID) S? '>'                   }
  {   [83]  PublicID ::=  'PUBLIC' S PubidLiteral                              }
  TxmlNotationDeclaration = class(AxmlDeclaration)
  protected
    FExternalID : TxmlExternalID;

  public
    constructor Create(const Name: WideString; const ExternalID: TxmlExternalID);
    property ExternalID: TxmlExternalID read FExternalID;
  end;

  { TxmlDocTypeDecl                                                            }
  {   [28]  doctypedecl ::=  '<!DOCTYPE' S Name (S ExternalID)? S?             }
  {                        ('[' (markupdecl | PEReference | S)* ']' S?)? '>'   }
  TxmlDocTypeDeclarationList = class(TxmlMarkupDeclarationList)
    procedure Print(const D: AxmlPrinter; const IndentLevel: Integer = 0); override;
  end;

  TxmlDocTypeDecl = class(TxmlTypeList)
  protected
    FName : WideString;

    function  GetName: WideString; override;

    function  GetExternalID: TxmlExternalID;
    function  GetDeclarations: TxmlDocTypeDeclarationList;
    function  GetURI: WideString;

  public
    constructor Create(const Name: WideString);

    procedure Print(const D: AxmlPrinter; const IndentLevel: Integer = 0); override;
    property  ExternalID: TxmlExternalID read GetExternalID;
    property  Declarations: TxmlDocTypeDeclarationList read GetDeclarations;

    property  URI: WideString read GetURI;
  end;



  {                                                                            }
  { TAGS                                                                       }
  {                                                                            }

  { AxmlTag / AxmlTagWithAttr / ATxmlTag / ATxmlTagWithAttr                    }
  {   Base classes for tag tokens.                                             }
  AxmlTag = class(AxmlType);
  AxmlTagWithAttr = class(AxmlTag)
  protected
    function  GetAttributes: AxmlAttributeList; virtual; abstract;
    function  GetAttrCount: Integer; virtual; abstract;
    function  GetAttrNames: WideStringArray; virtual; abstract;

  public
    property  Attributes: AxmlAttributeList read GetAttributes;
    property  AttrCount: Integer read GetAttrCount;
    property  AttrNames: WideStringArray read GetAttrNames;
    function  HasAttribute(const Attr: WideString): Boolean; virtual; abstract;
    function  AttrAsText(const Attr: WideString;
              const Declarations: TxmlMarkupDeclarationList = nil;
              const DefaultValue: WideString = ''): WideString; virtual; abstract;
    function  AttrAsInteger(const Attr: WideString;
              const Declarations: TxmlMarkupDeclarationList = nil;
              const DefaultValue: Integer = -1): Integer; virtual;
    function  AttrAsFloat(const Attr: WideString;
              const Declarations: TxmlMarkupDeclarationList = nil;
              const DefaultValue: Extended = 0.0): Extended; virtual;
  end;
  ATxmlTag = class(AxmlTag)
  protected
    FName : WideString;

    function  GetName: WideString; override;

  public
    constructor Create(const Name: WideString);
  end;
  ATxmlTagWithAttr = class(AxmlTagWithAttr)
  protected
    FName       : WideString;
    FAttributes : AxmlAttributeList;

    function  GetName: WideString; override;
    function  GetAttributes: AxmlAttributeList; override;
    function  GetAttrNames: WideStringArray; override;

  public
    constructor Create(const Name: WideString;
                const Attributes: AxmlAttributeList = nil);
    destructor Destroy; override;

    property  Attributes: AxmlAttributeList read FAttributes;
    function  HasAttribute(const Name: WideString): Boolean; override;
    function  AttrAsText(const Name: WideString;
              const Declarations: TxmlMarkupDeclarationList = nil;
              const DefaultValue: WideString = ''): WideString; override;
  end;

  { TxmlStartTag                                                               }
  {   [40]  STag ::=  '<' Name (S Attribute)* S? '>'                           }
  TxmlStartTag = class(ATxmlTagWithAttr)
    procedure Print(const D: AxmlPrinter; const IndentLevel: Integer = 0); override;
  end;

  { TxmlEndTag                                                                 }
  {   [42]  ETag ::=  '</' Name S? '>'                                         }
  TxmlEndTag = class(ATxmlTag)
    procedure Print(const D: AxmlPrinter; const IndentLevel: Integer = 0); override;
  end;

  { TxmlEmptyElementTag                                                        }
  {   [44]  EmptyElemTag ::=  '<' Name (S Attribute)* S? '/>'                  }
  TxmlEmptyElementTag = class(ATxmlTagWithAttr)
    procedure Print(const D: AxmlPrinter; const IndentLevel: Integer = 0); override;
  end;



  {                                                                            }
  { ELEMENTS                                                                   }
  {                                                                            }

  { AxmlElement                                                                }
  {   [39]  element ::=  EmptyElemTag | STag content ETag                      }
  TxmlElementContent = class; // forward
  AxmlElement = class;
  AxmlElementArray = Array of AxmlElement;
  AxmlElement = class(AxmlType)
  protected
    function  GetName: WideString; override;

    function  GetTag: AxmlTagWithAttr; virtual; abstract;
    function  GetAttributes: AxmlAttributeList; virtual;
    function  GetContent: TxmlElementContent; virtual; abstract;
    function  GetChildContent(const Path: WideString): TxmlElementContent; virtual;
    function  GetChildContentText(const Path: WideString): WideString;

  public
    // Tag
    property  Tag: AxmlTagWithAttr read GetTag;
    property  Attributes: AxmlAttributeList read GetAttributes;

    // Content
    property  Content: TxmlElementContent read GetContent;
    function  TextContent(const Declarations: TxmlMarkupDeclarationList = nil): WideString; override;

    // Attributes
    function  AttrNames: WideStringArray;
    function  HasAttribute(const Name: WideString): Boolean;
    function  AttrAsText(const Name: WideString; const Declarations: TxmlMarkupDeclarationList = nil; const DefaultValue: WideString = ''): WideString;
    function  AttrAsInteger(const Name: WideString; const Declarations: TxmlMarkupDeclarationList = nil; const DefaultValue: Integer = -1): Integer;
    function  AttrAsFloat(const Name: WideString; const Declarations: TxmlMarkupDeclarationList = nil; const DefaultValue: Extended = 0.0): Extended;

    // Child Elements
    property  ChildContent[const Path: WideString]: TxmlElementContent read GetChildContent;
    property  ChildContentText[const Path: WideString]: WideString read GetChildContentText; default;
    function  FirstElement: AxmlElement;
    function  ElementNames: WideStringArray;
    function  ElementByName(const Path: WideString): AxmlElement;
    function  ElementsByName(const Path: WideString): AxmlElementArray;
    function  PosNextElement(var C: AxmlElement; const PrevPos: Integer = -1): Integer;
    function  PosNextElementByName(var C: AxmlElement; const Name: WideString; const PrevPos: Integer = -1): Integer;
    function  ElementCount: Integer;
    function  ElementCountByName(const Name: WideString): Integer;
  end;

  { TxmlEmptyElement                                                           }
  TxmlEmptyElement = class(AxmlElement)
  protected
    FTag : TxmlEmptyElementTag;

    function GetTag: AxmlTagWithAttr; override;
    function GetContent: TxmlElementContent; override;

  public
    constructor Create(const Tag: TxmlEmptyElementTag);
    destructor Destroy; override;

    procedure Print(const D: AxmlPrinter; const IndentLevel: Integer = 0); override;
  end;

  { TxmlElement                                                                }
  TxmlElement = class(AxmlElement)
  protected
    FStartTag : TxmlStartTag;
    FEndTag   : TxmlEndTag;
    FContent  : TxmlElementContent;

    function GetTag: AxmlTagWithAttr; override;
    function GetContent: TxmlElementContent; override;

  public
    constructor Create(const StartTag: TxmlStartTag; const EndTag: TxmlEndTag;
                const Content: TxmlElementContent = nil);
    destructor Destroy; override;

    property  EndTag: TxmlEndTag read FEndTag;
    procedure Print(const D: AxmlPrinter; const IndentLevel: Integer = 0); override;
  end;

  { TxmlElementContent                                                         }
  {   [43]  content ::=  (element | CharData | Reference | CDSect |            }
  {                       PI | Comment)*                                       }
  {   Path parameters can contain '/'-delimited recursive names.               }
  TxmlElementContent = class(TxmlTypeList)
  protected
    function  ResolveElementPath(const Path: WideString; var Name: WideString): TxmlElementContent;

  public
    function  PosNextElement(var C: AxmlElement; const PrevPos: Integer = -1): Integer;
    function  PosNextElementByName(var C: AxmlElement; const Name: WideString; const PrevPos: Integer = -1): Integer;
    function  ElementCount: Integer;
    function  ElementCountByName(const Name: WideString): Integer;

    function  FirstElement: AxmlElement;
    function  FirstElementName: WideString;
    function  ElementNames: WideStringArray;
    function  ElementByName(const Path: WideString): AxmlElement;
    function  ElementsByName(const Path: WideString): AxmlElementArray;
    function  ElementContentByName(const Path: WideString): TxmlElementContent;

    function  ElementAttributeNames(const ElementName: WideString): WideStringArray;
    function  ElementAttributeValues(const ElementName, AttributeName: WideString;
              const Declarations: TxmlMarkupDeclarationList = nil; const DefaultValue: WideString = ''): WideStringArray;

    function  ElementTextContent(const ElementName: WideString; const Declarations: TxmlMarkupDeclarationList = nil): WideStringArray; overload;
    function  ElementTextContent(const ElementName, AttributeName, AttributeValue: WideString; const Declarations: TxmlMarkupDeclarationList = nil): WideStringArray; overload;
  end;



  {                                                                            }
  { DOCUMENT                                                                   }
  {                                                                            }

  { TxmlProlog                                                                 }
  {   [22]  prolog ::=  XMLDecl? Misc* (doctypedecl Misc*)?                    }
  TxmlProlog = class(TxmlTypeList)
  protected
    function  GetXMLDecl: TxmlXMLDecl;
    function  GetDocTypeDecl: TxmlDocTypeDecl;

  public
    property  XMLDecl: TxmlXMLDecl read GetXMLDecl;
    property  DocTypeDecl: TxmlDocTypeDecl read GetDocTypeDecl;
  end;

  { TxmlDocument                                                               }
  {   [1]  document ::=  prolog element Misc*                                  }
  TxmlDocument = class(TxmlTypeList)
  protected
    function  GetProlog: TxmlProlog;
    function  GetRootElement: AxmlElement;
    function  GetElementContentText(const RelPath: WideString): WideString;

  public
    constructor Create(const Prolog: TxmlProlog; const RootElement: AxmlElement);

    function  TextContent(const Declarations: TxmlMarkupDeclarationList = nil): WideString; override;

    property  Prolog: TxmlProlog read GetProlog;
    property  RootElement: AxmlElement read GetRootElement;

    function  DocTypeDecl: TxmlDocTypeDecl;
    function  DocTypeName: WideString;
    function  DocTypeURI: WideString;

    function  RootElementName: WideString;
    function  RootElementLocalName: WideString;
    function  RootElementNameSpace: WideString;
    function  RootElementNameSpaceURI: WideString;
    function  RootElementDefaultNameSpaceURI: WideString;
    function  IsRootElementName(const Name: WideString): Boolean;
    function  IsRootElementAsciiName(const Name: AnsiString;
              const CaseSensitive: Boolean = True): Boolean;

    function  ElementByName(const RelPath: WideString): AxmlElement;
    property  ElementContentText[const RelPath: WideString]: WideString read GetElementContentText; default;
  end;



implementation

uses
  { Delphi }
  SysUtils,

  { Fundamentals }
  cStrings,
  cUnicodeCodecs;



{                                                                              }
{ AxmlPrinter                                                                  }
{                                                                              }
constructor AxmlPrinter.Create(const Options: TxmlPrintOptions; const IndentLength: Integer);
begin
  inherited Create;
  FOptions := Options;
  FIndentLength := IndentLength;
end;

procedure AxmlPrinter.PrintEOL;
begin
  if [xmloNoFormatting, xmloNoEOL] * FOptions <> [] then
    exit;
  PrintSpecial(xmlsEOL, 1);
end;

procedure AxmlPrinter.PrintSpace(const Count: Integer);
var C : Integer;
begin
  if (xmloNoFormatting in FOptions) and (Count > 1) then
    C := 1 else
    C := Count;
  PrintSpecial(xmlsSpace, C);
end;

procedure AxmlPrinter.PrintTab(const Count: Integer);
var C : Integer;
begin
  if (xmloNoFormatting in FOptions) and (Count > 1) then
    C := 1 else
    C := Count;
  PrintSpecial(xmlsTab, C);
end;

procedure AxmlPrinter.PrintIndent(const IndentLevel: Integer);
begin
  if [xmloNoFormatting, xmloNoIndent] * FOptions <> [] then
    exit;
  if xmloUseTabIndent in FOptions then
    PrintSpecial(xmlsTab, IndentLength * IndentLevel)
  else
    PrintSpecial(xmlsSpace, IndentLength * IndentLevel);
end;

procedure AxmlPrinter.PrintDefault(const Txt: WideString);
begin
  PrintToken(xmltDefault, Txt);
end;

procedure AxmlPrinter.PrintText(const Txt: WideString);
begin
  PrintToken(xmltText, Txt);
end;

procedure AxmlPrinter.PrintSymbol(const Txt: WideString);
begin
  PrintToken(xmltSymbol, Txt);
end;

function AxmlPrinter.GetQuoteChar(const Txt: WideString): WideChar;
begin
  if xmloForceQuoteType in Options then
    begin
      if xmloUseDoubleQuotes in Options then
        Result := WideChar('"') else
        Result := WideChar('''');
    end else
    if xmloUseDoubleQuotes in Options then
      begin
        if WidePosChar(WideChar('"'), Txt) > 0 then
          Result := WideChar('''') else
          Result := WideChar('"');
      end else
      if WidePosChar(WideChar(''''), Txt) > 0 then
        Result := WideChar('"') else
        Result := WideChar('''');
end;

procedure AxmlPrinter.PrintQuoteStr(const Txt: WideString);
var Q : WideChar;
begin
  Q := GetQuoteChar(Txt);
  PrintSymbol(Q);
  PrintDefault(Txt);
  PrintSymbol(Q);
end;

procedure AxmlPrinter.PrintSafeQuotedText(const Txt: WideString);
begin
  PrintQuoteStr(xmlSafeText(Txt));
end;

procedure AxmlPrinter.PrintName(const Txt: WideString);
begin
  PrintToken(xmltName, Txt);
end;

procedure AxmlPrinter.PrintTagName(const Txt: WideString);
begin
  PrintToken(xmltTagName, Txt);
end;

procedure AxmlPrinter.PrintAttrName(const Txt: WideString);
begin
  PrintToken(xmltAttrName, Txt);
end;

procedure AxmlPrinter.PrintComment(const Txt: WideString);
begin
  PrintToken(xmltComment, Txt);
end;



{                                                                              }
{ TxmlStringPrinter                                                            }
{                                                                              }
constructor TxmlStringPrinter.Create(const Options: TxmlPrintOptions; const IndentLength: Integer);
begin
  inherited Create(Options, IndentLength);
  FxmlWideString := TWideStringWriter.Create;
end;

destructor TxmlStringPrinter.Destroy;
begin
  FreeAndNil(FxmlWideString);
  inherited Destroy;
end;

procedure TxmlStringPrinter.PrintToken(const TokenType: TxmlPrintToken;
    const Txt: WideString);
begin
  FxmlWideString.WriteWideStr(Txt);
end;

procedure TxmlStringPrinter.PrintSpecial(const SpecialSymbol: TxmlPrintSpecialSymbol;
    const Count: Integer);
var I : Integer;
begin
  Case SpecialSymbol of
    xmlsSpace : For I := 1 to Count do
                  FxmlWideString.WriteWord(32);
    xmlsTab   : For I := 1 to Count do
                  FxmlWideString.WriteWord(9);
    xmlsEOL   : FxmlWideString.WriteWideStr(WideCRLF);
  end;
end;

function TxmlStringPrinter.GetXMLWideString: WideString;
begin
  Result := FxmlWideString.AsWideString;
end;

function TxmlStringPrinter.GetXMLUTF8String: String;
begin
  Result := WideStringToUTF8String(GetXMLWideString);
end;



{                                                                              }
{ AxmlType                                                                     }
{                                                                              }
constructor AxmlType.Create;
begin
  inherited Create;
  Init;
end;

procedure AxmlType.Init;
begin
end;

function AxmlType.GetName: WideString;
begin
  Result := '';
end;

function AxmlType.GetNameSpace: WideString;
var I : Integer;
begin
  Result := GetName;
  I := WidePosChar(WideChar(':'), Result);
  if I <= 0 then
    Result := ''
  else
    Result := Copy(Result, 1, I - 1);
end;

function AxmlType.GetLocalName: WideString;
var I : Integer;
begin
  Result := GetName;
  I := WidePosChar(WideChar(':'), Result);
  if I <= 0 then
    exit;
  Result := WideCopyFrom(Result, I + 1);
end;

function AxmlType.IsName(const Name: WideString): Boolean;
begin
  Result := Name = GetName;
end;

function AxmlType.IsAsciiName(const Name: AnsiString; const CaseSensitive: Boolean): Boolean;
begin
  Result := WideEqualAnsiStr(Name, GetName, CaseSensitive);
end;

function AxmlType.TextContent(const Declarations: TxmlMarkupDeclarationList): WideString;
begin
  Result := '';
end;

procedure AxmlType.Print(const D: AxmlPrinter; const IndentLevel: Integer);
begin
  raise ExmlType.Create(ClassName + '.Print not implemented');
end;

function AxmlType.GetChildCount: Integer;
begin
  Result := 0;
end;

function AxmlType.GetChildByIndex(const Idx: Integer): AxmlType;
begin
  raise ExmlType.Create(ClassName + '.GetChildByIndex not implemented');
end;

function AxmlType.PosNext(var C: AxmlType; const ClassType: CxmlType;
    const PrevPos: Integer): Integer;
var I : Integer;
begin
  For I := MaxI(PrevPos + 1, 0) to ChildCount - 1 do
    begin
      C := ChildByIndex[I];
      if (ClassType = nil) or (C is ClassType) then
        begin
          Result := I;
          exit;
        end;
    end;
  Result := -1;
  C := nil;
end;

function AxmlType.PosNext(var C: AxmlType; const Name: WideString;
    const ClassType: CxmlType; const PrevPos: Integer): Integer;
var I : Integer;
begin
  For I := MaxI(PrevPos + 1, 0) to ChildCount - 1 do
    begin
      C := ChildByIndex[I];
      if ((ClassType = nil) or (C is ClassType)) and
         ((Name = '') or
          ((Name <> '') and (Name = C.Name))) then
        begin
          Result := I;
          exit;
        end;
    end;
  Result := -1;
  C := nil;
end;

function AxmlType.GetChildByName(const Name: WideString): AxmlType;
var C : AxmlType;
begin
  if PosNext(C, Name, AxmlType) >= 0 then
    Result := AxmlType(C)
  else
    Result := nil;
end;

function AxmlType.Find(const ClassType: CxmlType): AxmlType;
begin
  PosNext(Result, ClassType);
end;

function AxmlType.Find(const Name: WideString; const ClassType: CxmlType): AxmlType;
begin
  PosNext(Result, Name, ClassType);
end;

procedure AxmlType.AddChild(const Child: AxmlType);
begin
  raise ExmlType.Create(ClassName + '.AddChild not implemented');
end;

procedure AxmlType.AddChildren(const Children: Array of AxmlType);
var I : Integer;
begin
  For I := Low(Children) to High(Children) do
    AddChild(Children[I]);
end;

function AxmlType.AddAssigned(const Child: AxmlType): Boolean;
begin
  Result := Assigned(Child);
  if Result then
    AddChild(Child);
end;

function AxmlType.GetChildCountByClass(const ClassType: CxmlType = nil): Integer;
var I : Integer;
    C : AxmlType;
begin
  Result := 0;
  I := PosNext(C, ClassType);
  While I >= 0 do
    begin
      Inc(Result);
      I := PosNext(C, ClassType, I);
    end;
end;

function AxmlType.GetNames(const ClassType: CxmlType = nil): WideStringArray;
var I : Integer;
    C : AxmlType;
begin
  Result := nil;
  I := PosNext(C, ClassType);
  While I >= 0 do
    begin
      WideAppend(Result, C.Name);
      I := PosNext(C, ClassType, I);
    end;
end;

function AxmlType.AsWideString(const Options: TxmlPrintOptions;
    const IndentLength: Integer): WideString;
var P : TxmlStringPrinter;
begin
  P := TxmlStringPrinter.Create(Options, IndentLength);
  try
    Print(P);
    Result := P.XMLWideString;
  finally
    P.Free;
  end;
end;

function AxmlType.AsUTF8String(const Options: TxmlPrintOptions;
    const IndentLength: Integer): String;
begin
  Result := WideStringToUTF8String(AsWideString(Options, IndentLength));
end;



{                                                                              }
{ TxmlTypeList                                                                 }
{                                                                              }
constructor TxmlTypeList.Create(const Children: Array of AxmlType);
var I : Integer;
begin
  inherited Create;
  For I := 0 to High(Children) do
    AddChild(Children[I]);
end;

destructor TxmlTypeList.Destroy;
begin
  FreeObjectArray(FChildren);
  inherited Destroy;
end;

function TxmlTypeList.GetChildCount: Integer;
begin
  Result := Length(FChildren);
end;

function TxmlTypeList.GetChildByIndex(const Idx: Integer): AxmlType;
begin
  Result := FChildren[Idx];
end;

procedure TxmlTypeList.Print(const D: AxmlPrinter; const IndentLevel: Integer);
var I : Integer;
begin
  For I := 0 to ChildCount - 1 do
    ChildByIndex[I].Print(D, IndentLevel);
end;

function TxmlTypeList.TextContent(const Declarations: TxmlMarkupDeclarationList): WideString;
var I : Integer;
    C : AxmlType;
begin
  Result := '';
  I := PosNext(C, AxmlType);
  While I >= 0 do
    begin
      Result := Result + C.TextContent(Declarations);
      I := PosNext(C, AxmlType, I);
    end;
end;

procedure TxmlTypeList.AddChild(const Child: AxmlType);
begin
  Append(ObjectArray(FChildren), Child);
end;



{                                                                              }
{ CHARACTER DATA                                                               }
{                                                                              }

{ TxmlCharData                                                                 }
constructor TxmlCharData.Create(const Data: WideString);
begin
  inherited Create;
  FData := Data;
end;

procedure TxmlCharData.Print(const D: AxmlPrinter; const IndentLevel: Integer);
begin
  D.PrintText(xmlSafeText(FData));
end;

function TxmlCharData.TextContent(const Declarations: TxmlMarkupDeclarationList): WideString;
begin
  Result := FData;
end;

{ TxmlQuotedText                                                               }
procedure TxmlQuotedText.Print(const D: AxmlPrinter; const IndentLevel: Integer);
begin
  D.PrintQuoteStr(FText);
end;

function TxmlQuotedText.IntegerContent(const DefaultValue: Int64): Int64;
begin
  Result := StrToInt64Def(FText, DefaultValue);
end;

{ TxmlCDSect                                                                   }
constructor TxmlCDSect.Create(const Data: WideString);
begin
  Assert(WidePos(']]>', Data) = 0, 'CData contains an invalid sequence');
  inherited Create(Data);
end;

procedure TxmlCDSect.Print(const D: AxmlPrinter; const IndentLevel: Integer);
begin
  D.PrintIndent(IndentLevel);
  D.PrintSymbol('<![CDATA[');
  D.PrintText(FData);
  D.PrintSymbol(']]>');
  D.PrintEOL;
end;



{                                                                              }
{ REFERENCES                                                                   }
{                                                                              }

{ TxmlGeneralEntityRef                                                         }
constructor TxmlGeneralEntityRef.Create(const RefName: WideString);
begin
  Assert(xmlValidName(RefName), 'Invalid Entity Reference name');
  inherited Create;
  FRefName := RefName;
end;

procedure TxmlGeneralEntityRef.Print(const D: AxmlPrinter; const IndentLevel: Integer);
begin
  D.PrintSymbol('&');
  D.PrintName(FRefName);
  D.PrintSymbol(';');
end;

function TxmlGeneralEntityRef.TextContent(const Declarations: TxmlMarkupDeclarationList): WideString;
var Ch : WideChar;
begin
  Ch := xmlResolveEntityReference(FRefName);
  if Ch <> WideChar(#0) then
    Result := Ch else
    if not Assigned(Declarations) or
       not Declarations.ResolveEntityReference(FRefName, Result) then
      Result := '&' + FRefName + ';';
end;

{ TxmlCharRef                                                                  }
constructor TxmlCharRef.Create(const Number: UCS4Char; const Hex: Boolean);
begin
  Assert(Number <= $10FFFF, 'Invalid character reference');
  inherited Create;
  FNumber := Number;
  FHex := Hex;
end;

procedure TxmlCharRef.Print(const D: AxmlPrinter; const IndentLevel: Integer);
begin
  if FHex then
    begin
      D.PrintSymbol('&#x');
      D.PrintDefault(LongWordToHex(FNumber, 1));
    end else
    begin
      D.PrintSymbol('&#');
      D.PrintDefault(LongWordToStr(FNumber));
    end;
  D.PrintSymbol(';');
end;

function TxmlCharRef.TextContent(const Declarations: TxmlMarkupDeclarationList): WideString;
begin
  Result := WideChar(FNumber);
end;

{ TxmlPEReference                                                                }
constructor TxmlPEReference.Create(const RefName: WideString);
begin
  Assert(xmlValidName(RefName), 'Invalid PE Reference name');
  inherited Create;
  FRefName := RefName;
end;

procedure TxmlPEReference.Print(const D: AxmlPrinter; const IndentLevel: Integer);
begin
  D.PrintSymbol('%');
  D.PrintName(FRefName);
  D.PrintSymbol(';');
end;

function TxmlPEReference.TextContent(const Declarations: TxmlMarkupDeclarationList): WideString;
begin
  if not Assigned(Declarations) or
     not Declarations.ResolveParseEntityReference(FRefName, Result) then
    Result := '%' + FRefName + ';'
end;

{ TxmlQuotedReferenceText                                                      }
{   [..]  QuotedReferenceText ::=  "'" ReferenceText "'" |                     }
{                                  '"' ReferenceText '"'                       }
procedure TxmlQuotedReferenceText.Print(const D: AxmlPrinter; const IndentLevel: Integer);
var Q : WideString;
begin
  Q := D.GetQuoteChar('');
  D.PrintSymbol(Q);
  inherited Print(D, IndentLevel);
  D.PrintSymbol(Q);
end;



{                                                                              }
{ FORMATTING TOKENS                                                            }
{                                                                              }

{ TxmlLiteralFormatting                                                        }
constructor TxmlLiteralFormatting.Create(const Text: WideString);
begin
  inherited Create;
  FText := Text;
end;

procedure TxmlLiteralFormatting.Print(const D: AxmlPrinter; const IndentLevel: Integer);
begin
  D.PrintDefault(FText);
end;

{ TxmlFormattingList                                                           }
procedure TxmlLiteralFormattingList.Add(const Text: WideString);
begin
  AddChild(TxmlLiteralFormatting.Create(Text));
end;

{ TxmlSpace                                                                    }
constructor TxmlSpace.Create(const Text: WideString);
begin
  inherited Create(Text);
end;



{                                                                              }
{ ATTRIBUTES                                                                   }
{                                                                              }

{ TxmlAttValue                                                                 }
{   [10]  AttValue ::=  '"' ([^<&"] | Reference)* '"'                          }
{                    |  "'" ([^<&'] | Reference)* "'"                          }
function TxmlAttValue.IntegerContent(const Declarations: TxmlMarkupDeclarationList; const DefaultValue: Int64): Int64;
begin
  Result := StrToInt64Def(TextContent(Declarations), DefaultValue);
end;

function TxmlAttValue.FloatContent(const Declarations: TxmlMarkupDeclarationList; const DefaultValue: Extended): Extended;
begin
  Result := StrToFloatDef(TextContent(Declarations), DefaultValue);
end;

{ TxmlAttribute                                                                }
{   [41]  Attribute ::=  Name Eq AttValue                                      }
constructor TxmlAttribute.Create(const Name: WideString; const Value: TxmlAttValue);
begin
  Assert(Assigned(Value));
  inherited Create;
  FName := Name;
  FValue := Value;
end;

function TxmlAttribute.GetName: WideString;
begin
  Result := FName;
end;

procedure TxmlAttribute.Print(const D: AxmlPrinter; const IndentLevel: Integer);
begin
  D.PrintSpace;
  D.PrintAttrName(FName);
  D.PrintSymbol('=');
  FValue.Print(D, IndentLevel);
end;

function TxmlAttribute.TextContent(const Declarations: TxmlMarkupDeclarationList): WideString;
begin
  Result := FValue.TextContent(Declarations);
end;

function TxmlAttribute.IntegerContent(const Declarations: TxmlMarkupDeclarationList; const DefaultValue: Int64): Int64;
begin
  Result := FValue.IntegerContent(Declarations, DefaultValue);
end;

function TxmlAttribute.FloatContent(const Declarations: TxmlMarkupDeclarationList; const DefaultValue: Extended): Extended;
begin
  Result := FValue.FloatContent(Declarations, DefaultValue);
end;

function TxmlAttribute.IsNameSpaceDeclaration: Boolean;
begin
  Result := (FName = 'xmlns') or
            WideMatchLeftAnsiStr('xmlns:', FName, True);
end;

function TxmlAttribute.GetNameSpaceDeclaration(var NameSpace, URI: WideString): Boolean;
begin
  Result := WideMatchLeftAnsiStr('xmlns:', FName, True);
  if Result then
    begin
      NameSpace := WideCopyFrom(FName, 7);
      URI := TextContent(nil);
    end else
    if FName = 'xmlns' then
      begin
        NameSpace := '';
        URI := TextContent(nil);
        Result := True;
      end
    else
      begin
        NameSpace := '';
        URI := '';
      end
end;

{ AxmlAttributeList                                                            }
constructor AxmlAttributeList.Create(const List: TxmlTypeList);
begin
  inherited Create;
  InitList(List);
end;

procedure AxmlAttributeList.InitList(const List: TxmlTypeList);
begin
end;

function AxmlAttributeList.AttrAsInteger(const Name: WideString; const Declarations: TxmlMarkupDeclarationList; const DefaultValue: Int64): Int64;
begin
  Result := StrToInt64Def(AttrAsText(Name, Declarations, ''), DefaultValue);
end;

function AxmlAttributeList.AttrAsFloat(const Name: WideString; const Declarations: TxmlMarkupDeclarationList; const DefaultValue: Extended): Extended;
begin
  Result := StrToFloatDef(AttrAsText(Name, Declarations, ''), DefaultValue);
end;

function AxmlAttributeList.GetNameSpaceURI(const NameSpace: WideString): WideString;
var I    : Integer;
    A    : TxmlAttribute;
    N, U : WideString;
begin
  I := -1;
  Repeat
    I := FindNextAttr(A, I);
    if Assigned(A) and A.IsNameSpaceDeclaration then
      if A.GetNameSpaceDeclaration(N, U) then
        if N = NameSpace then
          begin
            Result := U;
            exit;
          end;
  Until I < 0;
end;

{ TxmlAttributeList                                                            }
procedure TxmlAttributeList.InitList(const List: TxmlTypeList);
begin
  inherited InitList(List);
  FList := List;
end;

destructor TxmlAttributeList.Destroy;
begin
  FreeAndNil(FList);
  inherited Destroy;
end;

procedure TxmlAttributeList.Print(const D: AxmlPrinter; const IndentLevel: Integer);
begin
  FList.Print(D, IndentLevel);
end;

function TxmlAttributeList.GetAttrCount: Integer;
begin
  Result := FList.GetChildCountByClass(TxmlAttribute);
end;

function TxmlAttributeList.GetAttrNames: WideStringArray;
begin
  Result := FList.GetNames(TxmlAttribute);
end;

function TxmlAttributeList.HasAttribute(const Name: WideString): Boolean;
begin
  Result := Assigned(FList.Find(Name, TxmlAttribute));
end;

function TxmlAttributeList.FindNextAttr(var A: TxmlAttribute; const Idx: Integer): Integer;
begin
  Result := FList.PosNext(AxmlType(A), TxmlAttribute, Idx);
end;

function TxmlAttributeList.AttrAsText(const Name: WideString; const Declarations: TxmlMarkupDeclarationList; const DefaultValue: WideString): WideString;
var A : TxmlAttribute;
begin
  A := TxmlAttribute(FList.Find(Name, TxmlAttribute));
  if Assigned(A) then
    Result := A.TextContent(Declarations) else
    Result := DefaultValue;
end;

function TxmlAttributeList.AttrAsInteger(const Name: WideString; const Declarations: TxmlMarkupDeclarationList; const DefaultValue: Int64): Int64;
var A : TxmlAttribute;
begin
  A := TxmlAttribute(FList.Find(Name, TxmlAttribute));
  if Assigned(A) then
    Result := A.IntegerContent(Declarations, DefaultValue) else
    Result := DefaultValue;
end;

function TxmlAttributeList.AttrAsFloat(const Name: WideString; const Declarations: TxmlMarkupDeclarationList; const DefaultValue: Extended): Extended;
var A : TxmlAttribute;
begin
  A := TxmlAttribute(FList.Find(Name, TxmlAttribute));
  if Assigned(A) then
    Result := A.FloatContent(Declarations, DefaultValue) else
    Result := DefaultValue;
end;

{ TxmlTextAttribute                                                            }
{   [..]  TextAttribute ::=  Name Eq QuotedText                                }
constructor TxmlTextAttribute.Create(const Name: WideString; const Value: TxmlQuotedText);
begin
  Assert(Assigned(Value));
  inherited Create;
  FName := Name;
  FValue := Value;
end;

constructor TxmlTextAttribute.Create(const Name, TextValue: WideString);
begin
  inherited Create;
  FName := Name;
  FValue := TxmlQuotedText.Create(TextValue);
end;

function TxmlTextAttribute.GetName: WideString;
begin
  Result := FName;
end;

procedure TxmlTextAttribute.Print(const D: AxmlPrinter; const IndentLevel: Integer);
begin
  D.PrintSpace;
  D.PrintAttrName(FName);
  D.PrintSymbol('=');
  FValue.Print(D, IndentLevel);
end;

function TxmlTextAttribute.ValueText: WideString;
begin
  Result := FValue.TextContent;
end;

function TxmlTextAttribute.IntegerContent(const DefaultValue: Int64 = -1): Int64;
begin
  Result := FValue.IntegerContent(DefaultValue);
end;

{ TxmlComment                                                                  }
constructor TxmlComment.Create(const Text: WideString);
begin
  Assert(Pos('--', Text) = 0, 'Comment may not contain ''--'' sequence');
  inherited Create;
  FText := Text;
end;

procedure TxmlComment.Print(const D: AxmlPrinter; const IndentLevel: Integer);
begin
  D.PrintIndent(IndentLevel);
  D.PrintSymbol('<--');
  D.PrintComment(FText);
  D.PrintSymbol('-->');
  D.PrintEOL;
end;

{ TxmlProcessingInstruction                                                    }
constructor TxmlProcessingInstruction.Create(const PITarget, Text: WideString);
begin
  Assert(Pos('?>', Text) = 0, 'PI Text may not contain ''?>'' sequence');
  inherited Create;
  FPITarget := PITarget;
  FText := Text;
end;

procedure TxmlProcessingInstruction.Print(const D: AxmlPrinter; const IndentLevel: Integer);
begin
  D.PrintIndent(IndentLevel);
  D.PrintSymbol('<?');
  D.PrintName(FPITarget);
  D.PrintSpace;
  D.PrintText(FText);
  D.PrintSymbol('?>');
  D.PrintEOL;
end;

{ TxmlMiscList                                                        }
function TxmlMiscList.FirstComment: WideString;
var C : TxmlComment;
begin
  C := TxmlComment(Find(TxmlComment));
  if Assigned(C) then
    Result := C.Text else
    Result := '';
end;

function TxmlMiscList.Comments: WideStringArray;
var I : Integer;
    C : AxmlType;
begin
  SetLength(Result, 0);
  I := PosNext(C, TxmlComment);
  While I >= 0 do
    begin
      WideAppend(Result, TxmlComment(C).Text);
      I := PosNext(C, TxmlComment, I);
    end;
end;

function TxmlMiscList.FirstProcessingInstruction(const PITarget: WideString): WideString;
var I : Integer;
    C : AxmlType;
begin
  I := PosNext(C, TxmlProcessingInstruction);
  While I >= 0 do
    begin
      if PITarget = TxmlProcessingInstruction(C).PITarget then
        begin
          Result := TxmlProcessingInstruction(C).Text;
          exit;
        end;
      I := PosNext(C, TxmlProcessingInstruction, I);
    end;
  Result := '';
end;

function TxmlMiscList.ProcessingInstructions(const PITarget: WideString): WideStringArray;
var I : Integer;
    C : AxmlType;
begin
  SetLength(Result, 0);
  I := PosNext(C, TxmlProcessingInstruction);
  While I >= 0 do
    begin
      if PITarget = TxmlProcessingInstruction(C).PITarget then
        WideAppend(Result, TxmlProcessingInstruction(C).Text);
      I := PosNext(C, TxmlProcessingInstruction, I);
    end;
end;

{ TxmlXMLDecl                                                                  }
function TxmlXMLDecl.GetVersionNum: String;
var C : AxmlType;
begin
  if PosNext(C, 'version', TxmlTextAttribute) >= 0 then
    Result := TxmlTextAttribute(C).TextContent else
    Result := '';
end;

function TxmlXMLDecl.GetEncodingName: String;
var C : AxmlType;
begin
  if PosNext(C, 'encoding', TxmlTextAttribute) >= 0 then
    Result := TxmlTextAttribute(C).TextContent else
    Result := '';
end;

function TxmlXMLDecl.GetStandalone: TxmlOptionalBoolean;
var C : AxmlType;
    S : String;
begin
  if PosNext(C, 'standalone', TxmlTextAttribute) < 0 then
    Result := obUnspecified else
    begin
      S := TxmlTextAttribute(C).TextContent;
      if S = 'yes' then
        Result := obTrue else
      if S = 'no' then
        Result := obFalse else
        Result := obUnspecified;
    end;
end;

procedure TxmlXMLDecl.Print(const D: AxmlPrinter; const IndentLevel: Integer);
begin
  D.PrintIndent(IndentLevel);
  D.PrintSymbol('<?xml');
  inherited Print(D, IndentLevel);
  D.PrintSymbol('?>');
  D.PrintEOL;
end;

{ AxmlDeclaration                                                              }
constructor AxmlDeclaration.Create(const Name: WideString);
begin
  Assert(xmlValidName(Name), 'Invalid Name');
  inherited Create;
  FName := Name;
end;

procedure AxmlDeclaration.Print(const D: AxmlPrinter; const IndentLevel: Integer);
begin
  D.PrintSpace;
  D.PrintDefault(FName);
end;

{ TxmlChildrenElementContentSpec                                               }
{   [47]  children ::=  (choice | seq) ('?' | '*' | '+')?                      }
{   [48]  cp ::=  (Name | choice | seq) ('?' | '*' | '+')?                     }
{   [49]  choice ::=  '(' S? cp ( S? '|' S? cp )* S? ')'                       }
{   [50]  seq ::=  '(' S? cp ( S? ',' S? cp )* S? ')'                          }
procedure AxmlChildSpec.Print(const D: AxmlPrinter; const IndentLevel: Integer);
begin
  Case Numerator of
    csnOne        : ;
    csnOptional   : D.PrintSymbol('?');
    csnAny        : D.PrintSymbol('*');
    csnAtLeastOne : D.PrintSymbol('+');
  end;
end;

constructor TxmlNameChildSpec.Create(const Name: WideString);
begin
  inherited Create;
  FName := Name;
end;

procedure TxmlNameChildSpec.Print(const D: AxmlPrinter; const IndentLevel: Integer);
begin
  D.PrintDefault(Name);
  inherited Print(D, IndentLevel);
end;

function AxmlListChildSpec.PosNextChildSpec(var C: AxmlChildSpec; const PrevPos: Integer): Integer;
var D : AxmlType;
begin
  Result := List.PosNext(D, AxmlChildSpec, PrevPos);
  C := AxmlChildSpec(D);
end;

procedure AxmlListChildSpec.Print(const D: AxmlPrinter; const IndentLevel: Integer);
begin
  D.PrintSymbol('(');
  List.Print(D, IndentLevel);
  D.PrintSymbol(')');
end;

{ AxmlContentSpec                                                              }
procedure TxmlEmptyContentSpec.Print(const D: AxmlPrinter; const IndentLevel: Integer);
begin
  D.PrintDefault('EMPTY');
end;

procedure TxmlAnyContentSpec.Print(const D: AxmlPrinter; const IndentLevel: Integer);
begin
  D.PrintDefault('ANY');
end;

function TxmlMixedContentSpec.GetAllowedNames: WideStringArray;
var I : Integer;
    C : AxmlType;
begin
  SetLength(Result, 0);
  I := FList.PosNext(C, TxmlLiteralFormatting);
  While I >= 0 do
    begin
      WideAppend(Result, TxmlLiteralFormatting(C).FText);
      I := FList.PosNext(C, TxmlLiteralFormatting, I);
    end;
end;

procedure TxmlMixedContentSpec.Print(const D: AxmlPrinter; const IndentLevel: Integer);
begin
  { Not implemented }
end;

procedure TxmlChildrenContentSpec.Print(const D: AxmlPrinter; const IndentLevel: Integer);
begin
  ChildrenSpec.Print(D, IndentLevel);
end;

{ TxmlElementDeclaration                                                       }
{   [45]  elementdecl ::=  '<!ELEMENT' S Name S contentspec S? '>'             }
{   [51]  Mixed ::=  '(' S? '#PCDATA' (S? '|' S? Name)* S? ')*'                }
{                  | '(' S? '#PCDATA' S? ')'                                   }
constructor TxmlElementDeclaration.Create(const Name: WideString;
    const ContentSpecType: TxmlElementContentSpec);
begin
  inherited Create(Name);
  Case ContentSpecType of
    ecsEmpty    : FContentSpec := TxmlEmptyContentSpec.Create;
    ecsAny      : FContentSpec := TxmlAnyContentSpec.Create;
    ecsMixed    : FContentSpec := TxmlMixedContentSpec.Create;
    ecsChildren : FContentSpec := TxmlChildrenContentSpec.Create;
  end;
end;

procedure TxmlElementDeclaration.Print(const D: AxmlPrinter; const IndentLevel: Integer);
begin
  D.PrintIndent (IndentLevel);
  D.PrintSymbol('<!ELEMENT');
  inherited Print(D, IndentLevel);
  D.PrintSpace;
  ContentSpec.Print(D, IndentLevel);
  D.PrintSymbol('>');
  D.PrintEOL;
end;

{ TxmlAttListDecl                                                              }
{   [52]  AttlistDecl ::=  '<!ATTLIST' S Name AttDef* S? '>'                   }
{   [53]  AttDef ::=  S Name S AttType S DefaultDecl                           }
{   [54]  AttType ::=  StringType | TokenizedType | EnumeratedType             }
{   [55]  StringType ::=  'CDATA'                                              }
{   [56]  TokenizedType ::=  'ID' | 'IDREF' | 'IDREFS' | 'ENTITY'              }
{                          | 'ENTITIES' | 'NMTOKEN' | 'NMTOKENS'               }
{   [57]  EnumeratedType ::=  NotationType | Enumeration                       }
{   [58]  NotationType ::=  'NOTATION' S '(' S? Name (S? '|' S? Name)*         }
{                           S? ')'                                             }
{   [59]  Enumeration ::=  '(' S? Nmtoken (S? '|' S? Nmtoken)* S? ')'          }
{   [60]  DefaultDecl ::=  '#REQUIRED' | '#IMPLIED'                            }
{                        | (('#FIXED' S)? AttValue)                            }
{   [10]  AttValue ::=  '"' ([^<&"] | Reference)* '"'                          }
{                    |  "'" ([^<&'] | Reference)* "'"                          }
constructor TxmlAttDef.Create(const Name: WideString; const AttType: TxmlAttType;
    const Names: TxmlTypeList; const DefaultType: TxmlDefaultType;
    const DefaultValue: TxmlAttValue);
begin
  inherited Create;
  FName := Name;
  FAttType := AttType;
  FNames := Names;
  FDefaultType := DefaultType;
  FDefaultValue := DefaultValue;
end;

procedure TxmlAttDef.Print(const D: AxmlPrinter; const IndentLevel: Integer);
begin
  D.PrintSpace;
  D.PrintName(Name);
  D.PrintSpace;
  Case AttType of
    atStringType                : D.PrintDefault('CDATA');
    atTokenizedTypeID           : D.PrintDefault('ID');
    atTokenizedTypeIDREF        : D.PrintDefault('IDREF');
    atTokenizedTypeIDREFS       : D.PrintDefault('IDREFS');
    atTokenizedTypeENTITY       : D.PrintDefault('IDENTITY');
    atTokenizedTypeENTITIES     : D.PrintDefault('IDENTITIES');
    atTokenizedTypeNMTOKEN      : D.PrintDefault('IDNMTOKEN');
    atTokenizedTypeNMTOKENS     : D.PrintDefault('IDNMTOKENS');
    atEnumeratedNotationType    :
      begin
        D.PrintDefault('NOTATION');
        D.PrintSpace;
        D.PrintSymbol('(');
        Names.Print(D, IndentLevel);
        D.PrintSymbol(')');
      end;
    atEnumeratedEnumerationType :
      begin
        D.PrintSymbol('(');
        Names.Print(D, IndentLevel);
        D.PrintSymbol(')');
      end;
  end;
  D.PrintSpace;
  Case DefaultType of
    dtRequired : D.PrintDefault('#REQUIRED');
    dtImplied  : D.PrintDefault('#IMPLIED');
    dtFixed    :
      begin
        D.PrintDefault('#FIXED');
        D.PrintSpace;
        DefaultValue.Print(D, IndentLevel);
      end;
    dtValue    : DefaultValue.Print(D, IndentLevel);
  end;
end;

procedure TxmlAttListDecl.Print(const D: AxmlPrinter; const IndentLevel: Integer);
begin
  D.PrintIndent(IndentLevel);
  D.PrintSymbol('<!ATTLIST');
  inherited Print(D, IndentLevel);
  D.PrintSymbol('>');
  D.PrintEOL;
end;

{ TxmlMarkupDeclarationList                                                    }
constructor TxmlMarkupDeclarationList.Create(const ParentDeclarationList: TxmlMarkupDeclarationList);
begin
  inherited Create;
  FParentDeclarationList := ParentDeclarationList;
end;

function TxmlMarkupDeclarationList.FindEntityDeclaration(const Name: WideString): TxmlEntityDeclaration;
begin
  Result := TxmlEntityDeclaration(Find(Name, TxmlEntityDeclaration));
end;

function TxmlMarkupDeclarationList.ResolveEntityReference(const RefName: WideString; var Value: WideString): Boolean;
var D : TxmlEntityDeclaration;
begin
  D := FindEntityDeclaration(RefName);
  Result := Assigned(D);
  if Result and Assigned(D.Definition) then
    Value := D.Definition.TextContent(nil)
  else
    Value := '';
end;

function TxmlMarkupDeclarationList.ResolveParseEntityReference(const RefName: WideString; var Value: WideString): Boolean;
var D : TxmlEntityDeclaration;
begin
  D := FindEntityDeclaration(RefName);
  Result := Assigned(D);
  if Result and Assigned(D.Definition) then
    Value := D.Definition.TextContent(nil)
  else
    Value := '';
end;

constructor TxmlNotationDeclaration.Create(const Name: WideString; const ExternalID: TxmlExternalID);
begin
  inherited Create(Name);
  FExternalID := ExternalID;
end;

{ TxmlExternalID                                                               }
{   [75]  ExternalID ::=  'SYSTEM' S SystemLiteral                             }
{                       | 'PUBLIC' S PubidLiteral S SystemLiteral              }
constructor TxmlExternalID.CreateSystemID(const SystemID: TxmlQuotedText);
begin
  inherited Create;
  FIDType := eidSystem;
  FSystemID := SystemID;
end;

constructor TxmlExternalID.CreatePublicID(const PublicID, SystemID: TxmlQuotedText);
begin
  inherited Create;
  FIDType := eidPublic;
  FPublicID := PublicID;
  FSystemID := SystemID;
end;

procedure TxmlExternalID.Print(const D: AxmlPrinter; const IndentLevel: Integer);
begin
  if IDType = eidSystem then
    begin
      D.PrintDefault('SYSTEM');
      D.PrintSpace;
      SystemID.Print(D, IndentLevel);
    end else
    begin
      D.PrintDefault('PUBLIC');
      D.PrintSpace;
      PublicID.Print(D, IndentLevel);
    end;
end;

procedure TxmlExternalIDNData.Print(const D: AxmlPrinter; const IndentLevel: Integer);
begin
  inherited Print(D, IndentLevel);
  D.PrintSpace;
  D.PrintDefault('NDATA');
  D.PrintSpace;
  D.PrintDefault(FNData);
end;

constructor TxmlEntityDeclaration.Create(const PEDeclaration: Boolean; const Name: WideString; const Definition: AxmlType);
begin
  inherited Create(Name);
  FPEDeclaration := PEDeclaration;
  FDefinition := Definition;
end;

procedure TxmlEntityDeclaration.Print(const D: AxmlPrinter; const IndentLevel: Integer);
begin
  D.PrintIndent(IndentLevel);
  D.PrintSymbol('<!ENTITY');
  inherited Print(D, IndentLevel);
  D.PrintSpace;
  FDefinition.Print(D, IndentLevel);
  D.PrintSymbol('>');
  D.PrintEOL;
end;

{ TxmlDocTypeDeclarationList                                                   }
procedure TxmlDocTypeDeclarationList.Print(const D: AxmlPrinter; const IndentLevel: Integer);
begin
  D.PrintSpace;
  D.PrintSymbol('[');
  D.PrintEOL;
  inherited Print(D, IndentLevel + 1);
  D.PrintIndent(IndentLevel);
  D.PrintSymbol(']');
end;

{ TxmlDocTypeDecl                                                              }
{   [28]  doctypedecl ::=  '<!DOCTYPE' S Name (S ExternalID)? S?               }
{                        ('[' (markupdecl | PEReference | S)* ']' S?)? '>'     }
constructor TxmlDocTypeDecl.Create(const Name: WideString);
begin
  Assert(xmlValidName(Name), 'Invalid Name');
  inherited Create;
  FName := Name;
end;

function TxmlDocTypeDecl.GetExternalID: TxmlExternalID;
begin
  Result := TxmlExternalID(Find(TxmlExternalID));
end;

function TxmlDocTypeDecl.GetDeclarations: TxmlDocTypeDeclarationList;
begin
  Result := TxmlDocTypeDeclarationList(Find(TxmlDocTypeDeclarationList));
end;

procedure TxmlDocTypeDecl.Print(const D: AxmlPrinter; const IndentLevel: Integer);
begin
  D.PrintIndent(IndentLevel);
  D.PrintSymbol('<!DOCTYPE');
  D.PrintSpace;
  D.PrintName(FName);
  D.PrintSpace;
  inherited Print(D, IndentLevel);
  D.PrintSymbol('>');
  D.PrintEOL;
end;

function TxmlDocTypeDecl.GetName: WideString;
begin
  Result := FName;
end;

function TxmlDocTypeDecl.GetURI: WideString;
var I : TxmlExternalID;
begin
  I := GetExternalID;
  if not Assigned(I) then
    Result := '' else
    Result := I.SystemID.Text;
end;



{                                                                              }
{ TAGS                                                                         }
{                                                                              }

{ AxmlTagWithAttr                                                              }
function AxmlTagWithAttr.AttrAsInteger(const Attr: WideString; const Declarations: TxmlMarkupDeclarationList; const DefaultValue: Integer): Integer;
begin
  Result := StrToIntDef(AttrAsText(Attr, Declarations, ''), DefaultValue);
end;

function AxmlTagWithAttr.AttrAsFloat(const Attr: WideString; const Declarations: TxmlMarkupDeclarationList; const DefaultValue: Extended): Extended;
begin
  Result := StrToFloatDef(AttrAsText(Attr, Declarations, ''), DefaultValue);
end;

{ ATxmlTag                                                                     }
constructor ATxmlTag.Create(const Name: WideString);
begin
  inherited Create;
  FName := Name;
end;

function ATxmlTag.GetName: WideString;
begin
  Result := FName;
end;

{ ATxmlTagWithAttr                                                             }
constructor ATxmlTagWithAttr.Create(const Name: WideString; const Attributes: AxmlAttributeList);
begin
  inherited Create;
  FName := Name;
  FAttributes := Attributes;
end;

destructor ATxmlTagWithAttr.Destroy;
begin
  FreeAndNil(FAttributes);
  inherited Destroy;
end;

function ATxmlTagWithAttr.GetName: WideString;
begin
  Result := FName;
end;

function ATxmlTagWithAttr.GetAttributes: AxmlAttributeList;
begin
  Result := FAttributes;
end;

function ATxmlTagWithAttr.HasAttribute(const Name: WideString): Boolean;
begin
  Result := Assigned(FAttributes) and FAttributes.HasAttribute(Name);
end;

function ATxmlTagWithAttr.GetAttrNames: WideStringArray;
begin
  if Assigned(FAttributes) then
    Result := FAttributes.AttrNames else
    SetLength(Result, 0);
end;

function ATxmlTagWithAttr.AttrAsText(const Name: WideString; const Declarations: TxmlMarkupDeclarationList; const DefaultValue: WideString): WideString;
begin
  if Assigned(FAttributes) then
    Result := FAttributes.AttrAsText(Name, Declarations, DefaultValue) else
    Result := DefaultValue;
end;

{ TxmlStartTag                                                                 }
procedure TxmlStartTag.Print(const D: AxmlPrinter; const IndentLevel: Integer);
begin
  D.PrintSymbol('<');
  D.PrintTagName(Name);
  if Assigned(FAttributes) then
    FAttributes.Print(D, IndentLevel);
  D.PrintSymbol('>');
end;

{ TxmlEndTag                                                                   }
procedure TxmlEndTag.Print(const D: AxmlPrinter; const IndentLevel: Integer);
begin
  D.PrintSymbol('</');
  D.PrintTagName(Name);
  D.PrintSymbol('>');
end;

{ TxmlEmptyElementTag                                                          }
procedure TxmlEmptyElementTag.Print(const D: AxmlPrinter; const IndentLevel: Integer);
begin
  D.PrintSymbol('<');
  D.PrintTagName(Name);
  if Assigned(FAttributes) then
    FAttributes.Print(D, IndentLevel);
  D.PrintSymbol('/>');
 end;

{ AxmlElement                                                                  }
function AxmlElement.GetName: WideString;
begin
  Result := GetTag.Name;
end;

function AxmlElement.GetAttributes: AxmlAttributeList;
begin
  Result := GetTag.Attributes;
end;

function AxmlElement.AttrNames: WideStringArray;
begin
  Result := GetTag.AttrNames;
end;

function AxmlElement.HasAttribute(const Name: WideString): Boolean;
begin
  Result := GetTag.HasAttribute(Name);
end;

function AxmlElement.AttrAsText(const Name: WideString; const Declarations: TxmlMarkupDeclarationList; const DefaultValue: WideString): WideString;
begin
  Result := GetTag.AttrAsText(Name, Declarations, DefaultValue);
end;

function AxmlElement.AttrAsInteger(const Name: WideString; const Declarations: TxmlMarkupDeclarationList; const DefaultValue: Integer): Integer;
begin
  Result := GetTag.AttrAsInteger(Name, Declarations, DefaultValue);
end;

function AxmlElement.AttrAsFloat(const Name: WideString; const Declarations: TxmlMarkupDeclarationList; const DefaultValue: Extended): Extended;
begin
  Result := GetTag.AttrAsFloat(Name, Declarations, DefaultValue);
end;

function AxmlElement.TextContent(const Declarations: TxmlMarkupDeclarationList): WideString;
var C : TxmlElementContent;
begin
  C := GetContent;
  if not Assigned(C) then
    Result := '' else
    Result := C.TextContent(Declarations);
end;

function AxmlElement.GetChildContent(const Path: WideString): TxmlElementContent;
var C : TxmlElementContent;
begin
  C := GetContent;
  if not Assigned(C) then
    Result := nil else
    Result := C.ElementContentByName(Path);
end;

function AxmlElement.GetChildContentText(const Path: WideString): WideString;
var C : TxmlElementContent;
begin
  C := GetChildContent(Path);
  if not Assigned(C) then
    Result := '' else
    Result := C.TextContent(nil);
end;

function AxmlElement.FirstElement: AxmlElement;
var C : TxmlElementContent;
begin
  C := GetContent;
  if not Assigned(C) then
    Result := nil else
    Result := C.FirstElement;
end;

function AxmlElement.ElementNames: WideStringArray;
var C : TxmlElementContent;
begin
  C := GetContent;
  if not Assigned(C) then
    Result := nil else
    Result := C.ElementNames;
end;

function AxmlElement.ElementByName(const Path: WideString): AxmlElement;
var C : TxmlElementContent;
begin
  C := GetContent;
  if not Assigned(C) then
    Result := nil else
    Result := C.ElementByName(Path);
end;

function AxmlElement.ElementsByName(const Path: WideString): AxmlElementArray;
var C : TxmlElementContent;
begin
  C := GetContent;
  if not Assigned(C) then
    Result := nil else
    Result := C.ElementsByName(Path);
end;

function AxmlElement.PosNextElement(var C: AxmlElement; const PrevPos: Integer): Integer;
var N : TxmlElementContent;
begin
  N := GetContent;
  if not Assigned(N) then
    Result := -1 else
    Result := N.PosNextElement(C, PrevPos);
end;

function AxmlElement.PosNextElementByName(var C: AxmlElement; const Name: WideString;
    const PrevPos: Integer): Integer;
var N : TxmlElementContent;
begin
  N := GetContent;
  if not Assigned(N) then
    Result := -1 else
    Result := N.PosNextElementByName(C, Name, PrevPos);
end;

function AxmlElement.ElementCount: Integer;
var N : TxmlElementContent;
begin
  N := GetContent;
  if not Assigned(N) then
    Result := 0 else
    Result := N.ElementCount;
end;

function AxmlElement.ElementCountByName(const Name: WideString): Integer;
var N : TxmlElementContent;
begin
  N := GetContent;
  if not Assigned(N) then
    Result := 0 else
    Result := N.ElementCountByName(Name);
end;

{ TxmlEmptyElement                                                             }
constructor TxmlEmptyElement.Create(const Tag: TxmlEmptyElementTag);
begin
  Assert(Assigned(Tag), 'Tag required');
  inherited Create;
  FTag := Tag;
end;

destructor TxmlEmptyElement.Destroy;
begin
  FreeAndNil(FTag);
  inherited Destroy;
end;

function TxmlEmptyElement.GetTag: AxmlTagWithAttr;
begin
  Result := FTag;
end;

function TxmlEmptyElement.GetContent: TxmlElementContent;
begin
  Result := nil;
end;

procedure TxmlEmptyElement.Print(const D: AxmlPrinter; const IndentLevel: Integer);
begin
  D.PrintIndent(IndentLevel);
  FTag.Print(D, IndentLevel);
  D.PrintEOL;
end;

{ TxmlElement                                                                  }
constructor TxmlElement.Create(const StartTag: TxmlStartTag; const EndTag: TxmlEndTag; const Content: TxmlElementContent);
begin
  Assert(Assigned(StartTag) and Assigned(EndTag), 'Start and End tags required');
  Assert(StartTag.Name = EndTag.Name, 'Start and End Tag names mismatch');
  inherited Create;
  FStartTag := StartTag;
  FEndTag := EndTag;
  FContent := Content;
end;

destructor TxmlElement.Destroy;
begin
  FreeAndNil(FEndTag);
  FreeAndNil(FContent);
  FreeAndNil(FStartTag);
  inherited Destroy;
end;

function TxmlElement.GetTag: AxmlTagWithAttr;
begin
  Result := FStartTag;
end;

function TxmlElement.GetContent: TxmlElementContent;
begin
  Result := FContent;
end;

procedure TxmlElement.Print(const D: AxmlPrinter; const IndentLevel: Integer);
var R : Boolean;
begin
  D.PrintIndent(IndentLevel);
  FStartTag.Print(D, IndentLevel);
  if Assigned(FContent) then
    begin
      R := FContent.FirstElement <> nil;
      if R then
        D.PrintEOL;
      FContent.Print(D, IndentLevel + 1);
      if R then
        D.PrintIndent(IndentLevel);
    end;
  FEndTag.Print(D, IndentLevel);
  D.PrintEOL;
end;

{ TxmlElementContent                                                           }
function TxmlElementContent.PosNextElement(var C: AxmlElement; const PrevPos: Integer): Integer;
var S : AxmlType;
begin
  Result := PosNext(S, AxmlElement, PrevPos);
  C := AxmlElement(S);
end;

function TxmlElementContent.PosNextElementByName(var C: AxmlElement; const Name: WideString; const PrevPos: Integer): Integer;
begin
  if Name <> '' then
    begin
      Result := PosNextElement(C, PrevPos);
      While Result >= 0 do
        begin
          if Name = C.Name then
            exit;
          Result := PosNextElement(C, Result);
        end;
    end;
  C := nil;
  Result := -1;
end;

function TxmlElementContent.ElementCount: Integer;
var I : Integer;
    C : AxmlElement;
begin
  Result := 0;
  I := -1;
  Repeat
    I := PosNextElement(C, I);
    if I >= 0 then
      Inc(Result);
  Until I < 0;
end;

function TxmlElementContent.ElementCountByName(const Name: WideString): Integer;
var I : Integer;
    C : AxmlElement;
begin
  Result := 0;
  I := -1;
  Repeat
    I := PosNextElementByName(C, Name, I);
    if I >= 0 then
      Inc(Result);
  Until I < 0;
end;

function TxmlElementContent.FirstElement: AxmlElement;
begin
  PosNextElement(Result);
end;

function TxmlElementContent.FirstElementName: WideString;
var C : AxmlElement;
begin
  PosNextElement(C);
  if not Assigned(C) then
    Result := '' else
    Result := C.Name;
end;

function TxmlElementContent.ElementNames: WideStringArray;
var I : Integer;
    C : AxmlElement;
begin
  SetLength(Result, 0);
  I := PosNextElement(C);
  While I >= 0 do
    begin
      WideAppend(Result, C.Name);
      I := PosNextElement(C, I);
    end;
end;

function TxmlElementContent.ResolveElementPath(const Path: WideString;
    var Name: WideString): TxmlElementContent;
var I, J : Integer;
    E    : AxmlElement;
begin
  Name := '';
  I := 1;
  Result := self;
  Repeat
    J := WidePosChar(WideChar('/'), Path, I);
    if J > 0 then
      begin
        Result.PosNextElementByName(E, WideCopyRange(Path, I, J - 1));
        if not Assigned(E) then
          begin
            Result := nil;
            exit;
          end;
        Result := E.Content;
        if not Assigned(Result) then
          exit;
        I := J + 1;
      end;
  Until J = 0;
  Name := WideCopyFrom(Path, I);
end;

function TxmlElementContent.ElementByName(const Path: WideString): AxmlElement;
var N : WideString;
    C : TxmlElementContent;
begin
  C := ResolveElementPath(Path, N);
  if Assigned(C) then
    C.PosNextElementByName(Result, N)
  else
    Result := nil;
end;

function TxmlElementContent.ElementsByName(const Path: WideString): AxmlElementArray;
var N : WideString;
    C : TxmlElementContent;
    E : AxmlElement;
    I, J, L : Integer;
begin
  Result := nil;
  C := ResolveElementPath(Path, N);
  if Assigned(C) then
    begin
      L := ElementCountByName(N);
      if L > 0 then
        begin
          SetLengthAndZero(ObjectArray(Result), L);
          J := 0;
          I := -1;
          Repeat
            I := C.PosNextElementByName(E, N, I);
            if I >= 0 then
              begin
                Assert(J < L);
                Result[J] := E;
                Inc(J);
              end;
          Until I < 0;
          Assert(J = L);
        end;
    end;
end;

function TxmlElementContent.ElementContentByName(const Path: WideString): TxmlElementContent;
var C : AxmlElement;
begin
  C := ElementByName(Path);
  if not Assigned(C) then
    Result := nil else
    Result := C.Content;
end;

function TxmlElementContent.ElementAttributeNames(const ElementName: WideString): WideStringArray;
var I : Integer;
    C : AxmlElement;
begin
  SetLength(Result, 0);
  I := PosNextElementByName(C, ElementName);
  While I >= 0 do
    begin
      WideAppendWideStringArray(Result, C.Tag.AttrNames);
      I := PosNextElementByName(C, ElementName, I);
    end;
end;

function TxmlElementContent.ElementAttributeValues(const ElementName, AttributeName: WideString; const Declarations: TxmlMarkupDeclarationList; const DefaultValue: WideString): WideStringArray;
var I : Integer;
    C : AxmlElement;
begin
  SetLength(Result, 0);
  I := PosNextElementByName(C, ElementName);
  While I >= 0 do
    begin
      WideAppend(Result, C.Tag.AttrAsText(AttributeName, Declarations, DefaultValue));
      I := PosNextElementByName(C, ElementName, I);
    end;
end;

function TxmlElementContent.ElementTextContent(const ElementName: WideString; const Declarations: TxmlMarkupDeclarationList): WideStringArray;
var I : Integer;
    C : AxmlElement;
begin
  SetLength(Result, 0);
  I := PosNextElementByName(C, ElementName);
  While I >= 0 do
    begin
      WideAppend(Result, C.TextContent(Declarations));
      I := PosNextElementByName(C, ElementName, I);
    end;
end;

function TxmlElementContent.ElementTextContent(const ElementName, AttributeName, AttributeValue: WideString; const Declarations: TxmlMarkupDeclarationList): WideStringArray;
var I : Integer;
    C : AxmlElement;
begin
  SetLength(Result, 0);
  I := PosNextElementByName(C, ElementName);
  While I >= 0 do
    begin
      if C.AttrAsText(AttributeName, Declarations) = AttributeValue then
        WideAppend(Result, C.TextContent(Declarations));
      I := PosNextElementByName(C, ElementName, I);
    end;
end;



{                                                                              }
{ DOCUMENT                                                                     }
{                                                                              }

{ TxmlProlog                                                                   }
function TxmlProlog.GetXMLDecl: TxmlXMLDecl;
begin
  Result := TxmlXMLDecl(Find(TxmlXMLDecl));
end;

function TxmlProlog.GetDocTypeDecl: TxmlDocTypeDecl;
begin
  Result := TxmlDocTypeDecl(Find(TxmlDocTypeDecl));
end;

{ TxmlDocument                                                                 }
{   [1]  document ::=  prolog element Misc*                                    }
constructor TxmlDocument.Create(const Prolog: TxmlProlog; const RootElement: AxmlElement);
begin
  Assert(Assigned(RootElement), 'RootElement required');
  inherited Create;
  if Assigned(Prolog) then
    AddChild(Prolog);
  AddChild(RootElement);
end;

function TxmlDocument.GetProlog: TxmlProlog;
begin
  Result := TxmlProlog(Find(TxmlProlog));
end;

function TxmlDocument.GetRootElement: AxmlElement;
begin
  Result := AxmlElement(Find(AxmlElement));
end;

function TxmlDocument.TextContent(const Declarations: TxmlMarkupDeclarationList): WideString;
begin
  Result := RootElement.TextContent(Declarations);
end;

function TxmlDocument.DocTypeDecl: TxmlDocTypeDecl;
var P : TxmlProlog;
begin
  P := GetProlog;
  if not Assigned(P) then
    Result := nil else
    Result := P.DocTypeDecl;
end;

function TxmlDocument.DocTypeName: WideString;
var D : TxmlDocTypeDecl;
begin
  D := DocTypeDecl;
  if not Assigned(D) then
    Result := '' else
    Result := D.Name;
end;

function TxmlDocument.DocTypeURI: WideString;
var D : TxmlDocTypeDecl;
begin
  D := DocTypeDecl;
  if not Assigned(D) then
    Result := '' else
    Result := D.URI;
end;

function TxmlDocument.RootElementName: WideString;
begin
  Result := RootElement.Name;
end;

function TxmlDocument.RootElementLocalName: WideString;
begin
  Result := RootElement.LocalName;
end;

function TxmlDocument.RootElementNameSpace: WideString;
begin
  Result := RootElement.NameSpace;
end;

function TxmlDocument.RootElementNameSpaceURI: WideString;
begin
  Result := RootElement.Attributes.GetNameSpaceURI(RootElementNameSpace);
end;

function TxmlDocument.RootElementDefaultNameSpaceURI: WideString;
begin
  Result := RootElement.Attributes.GetNameSpaceURI('');
end;

function TxmlDocument.IsRootElementName(const Name: WideString): Boolean;
begin
  Result := RootElement.IsName(Name);
end;

function TxmlDocument.IsRootElementAsciiName(const Name: AnsiString;
    const CaseSensitive: Boolean): Boolean;
begin
  Result := RootElement.IsAsciiName(Name, CaseSensitive);
end;

function TxmlDocument.ElementByName(const RelPath: WideString): AxmlElement;
begin
  Result := RootElement.ElementByName(RelPath);
end;

function TxmlDocument.GetElementContentText(const RelPath: WideString): WideString;
begin
  Result := RootElement.ChildContentText[RelPath];
end;


end.

